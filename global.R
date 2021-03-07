
# library, data, source ---- 
library(tidyverse)
library(wpp2019)
library(sf)
library(leaflet)
library(leaflet.minicharts)



source("./modules/counter.R")
source("./modules/sliderYear.R")

data("popF") # Daten weiblich
data("popM") # Daten männlich
data("UNlocations") # Schlüsselliste




# WRANGLE ----
# Populationdata Male/Female

# gemergte Daten für männlich und weiblich, dazu join von Kontinenten und Gruppierungen nach UN Definition
popFemale <- popF %>% mutate(gender = "F")


data_popFM <- popM %>% 
  mutate(gender = "M") %>% 
  bind_rows(popFemale) %>% 
  # join country code
  left_join(
    select(
      UNlocations,
      country_code,
      reg_code,
      reg_name,
      area_code,
      area_name),
    by = "country_code") %>% 
  mutate(country_code = stringr::str_pad(country_code, 3, pad = "0")) %>% 
  mutate(across(where(is.numeric), ~ .*1000))



# im long format
data_popFM_long <- data_popFM %>% 
  pivot_longer(
    # es werden die Spalten ent-pivotiert, die 4 Zahlen beinhalten (also bspw. 1950, 2015), also die yeares Spalten 
    cols = matches("[0-9]{4}"),
    values_to = "population",
    names_to = "year") %>% 
  mutate(
    year = as.numeric(year),
    age = factor(
      age,
      ordered = TRUE,
      levels = c(
        "0-4",
        "5-9",
        "10-14",
        "15-19",
        "20-24",
        "25-29",
        "30-34",
        "35-39",
        "40-44",
        "45-49",
        "50-54",
        "55-59",
        "60-64",
        "65-69",
        "70-74",
        "75-79",
        "80-84",
        "85-89",
        "90-94",
        "95-99",
        "100+")))


# Pop Pyramid

popPyData <- data_popFM_long %>% 
  # filtert Kontinente und Länder Gruppen heraus
  filter(!country_code >= 900) %>% 
  group_by(
    year,
    name,
    gender) %>% 
  mutate(
    percPop_country_gender = round(population/sum(population)*100,1)) %>% 
  ungroup() %>% 
  mutate(perc_MF = ifelse(gender == "F", percPop_country_gender*-1, percPop_country_gender)) %>% 
  filter(
    year == "2015"
    & name == "Germany")



# Liniendiagramm ----

data_popFM %>% 
  filter(
    name == "Germany"
    | name == "France") %>% 
  select(
    name,
    gender,
    age,
    contains("19"),
    contains("20")) %>% 
  pivot_longer(
    cols = -c(age, gender, name),
    values_to = "Anzahl",
    names_to = "Jahr") %>% 
  group_by(
    name,
    gender,
    Jahr) %>% 
  summarize(Anzahl = sum(Anzahl))

# Map ----
centroids <- rworldmap::getMap(resolution="low") %>% 
  rgeos::gCentroid(byid=TRUE) %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column("name")


tilesURL <- "http://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}"

data_EUmap <- st_as_sf(rnaturalearth::countries110) %>%  
  select(
    country_code = iso_n3,
    geometry) %>% 
  drop_na()

data_map_europe <- data_popFM_long %>% 
  filter(area_name == "Europe") %>% 
  select(
    year,
    name,
    country_code,
    age,
    gender,
    population) %>% 
  group_by(
    year,
    name,
    country_code,
    age) %>% 
  summarize(population = sum(population)) %>% 
  group_by(
    year,
    name,
    country_code) %>% 
  mutate(
    pop_total = sum(population)) %>% 
  ungroup() %>% 
  filter(
    age == "0-4"
    | age == "30-34"
    | age == "60-64") %>% 
  group_by(
    year,
    name,
    country_code,
    age) %>% 
  mutate(age_perc = round(population/sum(pop_total)*100,1)) %>% 
  ungroup()
  
map_polygon_EU <- sf::st_as_sf(data_map_europe %>%   
  left_join(data_EUmap) %>% 
  drop_na()) %>% 
  sf::st_transform(., 4326) %>% filter(year == 2020)

map_centroid_EU <- data_map_europe %>% 
  select(
    -population,
    -pop_total) %>% 
  pivot_wider(
    names_from = age,
    values_from = age_perc) %>% 
  left_join(centroids) %>% 
  drop_na() %>% filter(year == 2020)




# input data ----
wppYear <- popM %>% 
  select(-c("country_code", "name", "age")) %>% 
  colnames()

wppName <- popM %>% 
  filter(!country_code >= 900) %>% 
  select("name") %>% 
  distinct()

wppAge <- popM %>% 
  select("age") %>% 
  distinct()
