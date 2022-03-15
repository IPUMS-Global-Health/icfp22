# Load R packages  ----
setup_icfp <- function(tidylog = FALSE){
  suppressPackageStartupMessages({
    library(here)
    library(gtsummary)
    library(ggspatial)
    library(tmap)
    library(ipumsr)
    library(broom)
    library(sf)
    library(terra)
    library(tidyverse)
    library(progress)
    library(sysfonts)
    library(showtext)
    if(tidylog){library(tidylog)}
  }) 
  
  sysfonts::font_add(
    family = "cabrito", 
    regular = "../../fonts/cabritosansnormregular-webfont.ttf"
  )
  showtext::showtext_auto()
  options(tibble.print_min = 20)
}