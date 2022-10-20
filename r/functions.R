
setup_icfp <- function(tidylog = FALSE, spatial = FALSE){
  
  # Load R packages  ----
  suppressPackageStartupMessages({
    library(here)
    library(ipumsr)
    library(tidyverse)
    library(srvyr)
    library(survey)
    library(gtsummary)
    library(broom)
    library(sysfonts)
    library(showtext)
    if(tidylog){library(tidylog)}
    if(spatial){
      library(ggspatial)
      library(sf)
      library(terra)
    }
  }) 
  
  # Graphics / Console Output Helpers ----
  sysfonts::font_add(
    family = "cabrito", 
    regular = "../../fonts/cabritosansnormregular-webfont.ttf"
  )
  update_geom_defaults("text", list(family = "cabrito", size = 3))
  showtext::showtext_auto()
  options(tibble.print_min = 35)
}