gps <- map_df(
  c("burkina_faso/PMA_BF_GPS_v1_21Dec2020.csv",
    "cote_d_ivoire/PMA_CI_GPS_v1_19May2021.csv",
    "dr_congo/PMA_CD_GPS_v1_21Dec2020.csv",
    "kenya/PMA_KE_GPS_v1_21Dec2020.csv",
    "uganda/PMA_UG_GPS_v1_19May2021.csv"),
  ~here("data/gps", .x) %>% read_csv()
)

gps <- gps %>% 
  st_as_sf(coords = c("GPSLONG", "GPSLAT"), crs = 4326) %>% 
  rename(EAID = EA_ID) %>% 
  select(PMACC, EAID)

write_rds(gps, here("data/gps.rds.gz"), compress = "gz")
