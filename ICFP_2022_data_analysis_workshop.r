## -------------------------------- 
## Setup
## --------------------------------
library(ipumsr)
library(tidyverse)
library(srvyr)
library(survey)
library(gtsummary)

# load pma longitudinal samples (wide format, female respondents only)
dat <- read_ipums_micro(
  ddi = here::here("data/pma_00114.xml"),
  data = here::here("data/pma_00114.dat.gz")
)

# keep only panel members in the de facto population 
dat <- dat %>% 
  mutate(COUNTRY = COUNTRY %>% as_factor %>% as.character) %>% 
  filter(
    RESULTFQ_1 == 1 & RESULTFQ_2 == 1,
    RESIDENT_1 %in% c(11, 22) & RESIDENT_2 %in% c(11, 22)
  )

# keep only samples from BF and KE 
dat <- dat %>% filter(COUNTRY %in% c("Burkina Faso", "Kenya"))

## --------------------------------
## Unmet Need by Country at Phase 1
## --------------------------------
dat %>% 
  as_survey_design(
    weight = PANELWEIGHT, 
    id = EAID_1, 
    strata = STRATA_1
  ) %>% 
  mutate(across(
    c(UNMETNEED_1, UNMETYN_1), 
    ~.x %>%  as_factor %>% fct_drop
  )) %>% 
  tbl_svysummary(
    by = COUNTRY, 
    include = c(UNMETNEED_1, UNMETYN_1), 
    statistic = list(everything() ~ "{p}%")
  ) %>% 
  modify_header(
    update = list(
      label ~ " ", 
      stat_1 ~ "Burkina Faso", 
      stat_2 ~ "Kenya"
    )
  ) %>% 
  modify_footnote(
    c(stat_1, stat_2) ~ "Weighted Percent"
  ) %>% 
  modify_caption("## Unmet Need at Phase 1")

## --------------------------------
## Unmet Need by Country at Phase 2
## --------------------------------
dat %>% 
  as_survey_design(
    weight = PANELWEIGHT, 
    id = EAID_1, 
    strata = STRATA_1
  ) %>% 
  mutate(across(
    c(UNMETNEED_2, UNMETYN_2), 
    ~.x %>%  as_factor %>% fct_drop
  )) %>% 
  tbl_svysummary(
    by = COUNTRY, 
    include = c(UNMETNEED_2, UNMETYN_2), 
    statistic = list(everything() ~ "{p}%")
  ) %>% 
  modify_header(
    update = list(
      label ~ " ", 
      stat_1 ~ "Burkina Faso", 
      stat_2 ~ "Kenya"
    )
  ) %>% 
  modify_footnote(
    c(stat_1, stat_2) ~ "Weighted Percent"
  ) %>% 
  modify_caption("## Unmet Need at Phase 2")

## --------------------------------
## Null Models
## --------------------------------
null_models <- dat %>% 
  mutate(across(matches("UNMETYN"), ~.x == 1)) %>% 
  group_by(COUNTRY) %>% 
  summarise(
    glm = cur_data() %>% 
      as_survey_design(
        weight = PANELWEIGHT, 
        id = EAID_1, 
        strata = STRATA_1
      ) %>% 
      svyglm(
        UNMETYN_2 ~ UNMETYN_1, 
        design = ., 
        family = "quasibinomial"
      ) %>% 
      list()
  )

null_models$glm %>% 
  map2(null_models$COUNTRY,
       ~.x %>% 
         tbl_regression(
           exponentiate = TRUE,
           show_single_row = where(is.logical),
           pvalue_fun = ~style_pvalue(.x, digits = 2),
           label = list(UNMETYN_1 ~ "Phase 1 Unmet Need")
         ) %>%
         add_significance_stars(hide_se = TRUE) %>% 
         modify_header(update = list(
           label ~ " ", 
           estimate = .y
         )) %>% 
         modify_footnote(estimate ~ NA, abbreviation = TRUE) 
  ) %>% 
  tbl_merge(tab_spanner = FALSE) %>% 
  modify_caption("## Odds Ratios for Phase 2 Unmet Need")

## --------------------------------
## Graphic 
## --------------------------------
dat %>% 
  filter(UNMETYN_1 == 1) %>% 
  group_by(COUNTRY) %>% 
  summarise(across(
    c(starts_with("FPYNOT") & ends_with("1"), PREGNANT_1),
    ~cur_data() %>% summarise(prop = weighted.mean(.x == 1, PANELWEIGHT))
  )) %>% 
  pivot_longer(!COUNTRY, names_to = "var_name") %>% 
  unnest(value) %>% 
  left_join(
    dat %>% 
      select(starts_with("FPYNOT") & ends_with("1"), PREGNANT_1) %>% 
      ipums_var_info() %>% 
      select(var_name, var_label) 
  ) %>% 
  mutate(
    var_label = var_label %>% 
      str_remove("Reason not using FP: ") %>% 
      str_to_title() %>% 
      str_replace("Pregnancy Status", "Currently Pregnant") %>% 
      fct_reorder(prop, mean)
  ) %>% 
  ggplot(aes(x = COUNTRY, y = var_label, fill = prop)) + 
  geom_tile() + 
  geom_text(aes(
    label = prop %>% scales::percent(1), 
    color = prop > 0.11
  )) +
  theme_minimal() %+replace% 
  theme(
    legend.position = "none",
    text = element_text(size = 13),
    panel.grid = element_blank(),
    axis.title = element_blank()
  ) + 
  scale_fill_gradient(
    na.value = "transparent",
    low =  "#00263A05",
    high = "#00263A"
  ) + 
  scale_color_manual(values = c("black", "white")) +
  scale_x_discrete(position = "top") 

## --------------------------------
## Recoding 
## --------------------------------

dat <- dat %>% 
  mutate(
    across(
      c(starts_with("UNMETYN"), starts_with("FPYNOT"), starts_with("PREGNANT")),
      ~.x == 1
    ),
    FPYOPPOSED = if_any(c(FPYNOTRELIG_1, FPYNOTOPPF_1, FPYNOTFATE_1,
                          FPYNOTOPPH_1, FPYNOTOPPO_1, FPYNOTSIDEF_1,
                          FPYNOTSDHLTH_1,  FPYNOTCONV_1, FPYNOTBODY_1)),
    FPYMETHOD = if_any(c(FPYNOTFAR_1, FPYNOTCOST_1, FPYNOTKNO_1,
                         FPYNOTSRC_1, FPYNOTAVAIL_1, FPYNOTAVAILP_1, 
                         PREGNANT_1)),
    FPYLOWRISK = if_any(c(FPYNOTBSTFD_1, FPYNOTHSBAWAY_1, FPYNOTMENO_1,
                          FPYNOTAMEN_1, FPYNOTNOSEX_1, FPYNOTMAR_1, FPYNOTINF_1
    )),
    FPYOTHER = UNMETYN_1 & !FPYOPPOSED & !FPYMETHOD & !FPYLOWRISK,
    across(
      c(FPYOPPOSED, FPYMETHOD, FPYLOWRISK),
      ~if_else(UNMETYN_1, .x, FALSE)
    )
  )    

## --------------------------------
## Recoded Reasons by Country
## --------------------------------

dat %>% 
  filter(UNMETYN_1) %>% 
  as_survey_design(
    weight = PANELWEIGHT, 
    id = EAID_1, 
    strata = STRATA_1
  ) %>% 
  tbl_svysummary(
    by = COUNTRY, 
    include = c(
      FPYOPPOSED, 
      FPYMETHOD, 
      FPYLOWRISK, 
      FPYOTHER
    ), 
    label = list(
      FPYOPPOSED ~ "Opposition or prohibition", 
      FPYMETHOD ~ "Method access",
      FPYLOWRISK ~ "Low risk of becoming pregnant",
      FPYOTHER ~ "Other / Unknown Reason"
    ),
    statistic = list(everything() ~ "{p}%")
  ) %>% 
  modify_header(update = list(
    label ~ " ", 
    stat_1 ~ "Burkina Faso", 
    stat_2 ~ "Kenya"
  )) %>% 
  modify_footnote(
    c(stat_1, stat_2) ~ "Weighted Percent; 
    Women could list multiple reasons"
  ) %>% 
  modify_caption("## Phase 1 Reasons for Unmet Need")

## --------------------------------
## Models 
## --------------------------------
models <- dat %>% 
  group_by(COUNTRY) %>% 
  summarise(
    glm = cur_data() %>%  
      as_survey_design(
        weight = PANELWEIGHT, 
        id = EAID_1, 
        strata = STRATA_1
      ) %>% 
      svyglm(
        formula = UNMETYN_2 ~ 
          FPYOPPOSED + FPYMETHOD + FPYLOWRISK + UNMETYN_1,
        family = "quasibinomial",
        design = .
      ) %>% 
      list()
  ) 

models$glm %>% 
  map2(models$COUNTRY,
       ~.x %>% 
         tbl_regression(
           exp = TRUE, show_single_row = where(is.logical),
           pvalue_fun = ~style_pvalue(.x, digits = 2),
           label = list(
             FPYOPPOSED ~ "Opposition or prohibition", 
             FPYMETHOD ~ "Method access",
             FPYLOWRISK ~ "Low risk of becoming pregnant",
             UNMETYN_1 ~ "Phase 1 Unmet Need"
           )
         ) %>%
         add_significance_stars(hide_se = TRUE) %>% 
         modify_header(update = list(
           label ~ " ", 
           estimate = .y
         )) %>% 
         modify_footnote(estimate ~ NA, abbreviation = TRUE) 
  ) %>% 
  tbl_merge(tab_spanner = FALSE) %>% 
  modify_caption("## Odds Ratios for Phase 2 Unmet Need")

