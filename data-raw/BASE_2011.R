# Create the 2011 base dataset

library(dplyr)
library(sf)
library(readxl)

url <- "https://opendata.arcgis.com/datasets/e6f40847dfb04d3da01adda23dc0d334_0.geojson"

file <- tempfile()
download.file(url, file)

lk <- sf::st_read(file, stringsAsFactors = FALSE) %>%
  filter(CTY15CDO != 0) %>%
  sf::st_drop_geometry() %>%
  select(ends_with("CD"), ends_with("NM"))

names(lk) <- gsub("15", "11", names(lk))

unlink(file)

lk <- lk %>%
  mutate(LAD11CD = case_when(LAD11CD == "E07000240" ~ "E07000100",
                             LAD11CD == "E07000241" ~ "E07000104",
                             LAD11CD == "E06000057" ~ "E06000048",
                             LAD11CD == "E07000242" ~ "E07000097",
                             LAD11CD == "E07000243" ~ "E07000101",
                             LAD11CD == "E08000037" ~ "E08000020",
                             TRUE ~ as.character(LAD11CD)))

url <- "https://opendata.arcgis.com/datasets/4b9e1318dd854d9fac5f4eda66db7cd5_0.geojson"

file <- tempfile()
download.file(url, file)

dat <- sf::st_read(file, stringsAsFactors = FALSE) %>%
  sf::st_drop_geometry() %>%
  select(ends_with("CD"), ends_with("NM")) %>%
  filter(substr(lad11cd,1,1) == "E")

names(dat) <- toupper(names(dat))

unlink(file)

joined <- left_join(dat, lk, by = c("LAD11CD", "LAD11NM"))

BASE_2011 <-  joined %>%
  rename(UTLA11CD = CTY11CD,
         UTLA11NM = CTY11NM) %>%
  mutate(UTLA11CD = ifelse(substr(LAD11CD,1,3) != "E07", LAD11CD, UTLA11CD),
         UTLA11NM = ifelse(substr(LAD11CD,1,3) != "E07", LAD11NM, UTLA11NM))

summary(as.vector(is.na(BASE_2011)))

LEA_2011 <- read_excel("data-raw/Copy of nlac-2011.xls")

BASE_2011 <- left_join(BASE_2011, LEA_2011, by = c("UTLA11CD" = "New LA Code")) %>%
  rename("LEA11CD" = "Old LA Code") %>%
  mutate(LEA11NM = UTLA11NM) %>%
  select(-"LA Name")

usethis::use_data(BASE_2011, overwrite = TRUE)
