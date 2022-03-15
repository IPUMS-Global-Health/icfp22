# load libraries and source functions 
source(here::here("r/functions.R")) 
setup_icfp()

shapes <- here("data/shapes") %>% 
  st_read() %>% 
  filter(CNTRY_NAME %in% c(
    "Burkina Faso",
    "Congo, DRC",
    "Côte d'Ivoire", 
    "Kenya",
    "Uganda"
  )) %>% 
  st_make_valid() %>%               # close overlapping lines 
  filter(ADMIN_NAME != "Other Congo, DRC") %>% 
  mutate(SAMPLE = if_else(          # separate plots for non-national samples
    CNTRY_NAME == "Congo, DRC",
    paste("DRC -", ADMIN_NAME),
    CNTRY_NAME
  )) %>% 
  count(SAMPLE) %>%                 # remove subnational boundaries 
  st_simplify(dTolerance = 500)     # smooth to 500 meter tolerance 

write_rds(shapes, here("data/top_shapes.rds.gz"), compress = "gz")
