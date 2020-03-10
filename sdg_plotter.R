library(janitor)
library(dplyr)
library(sf)
library(readr)

glob_inds <- readr::read_csv("data/global_indicators.csv")

world <-
  janitor::clean_names(rnaturalearth::ne_countries(scale = "medium", returnclass = "sf"))

world$iso_a3[world$admin == "France"] <- "FRA"
world$iso_a3[world$admin == "Indian Ocean Territory"] <- "IOT"
world$iso_a3[world$admin == "Kosovo"] <- "RKS"
world$iso_a3[world$admin == "Norway"] <- "NOR"

world_sf <- world %>%
  dplyr::select(iso_a3, region_un, continent, pop_est, gdp_md_est) %>%
  dplyr::mutate(code = iso_a3, region_un = as.factor(region_un)) %>%
  left_join(., glob_inds, by = "code")

sdg_plotter <- function(goal_view = 1:17){
  
  num_ind_countries <- world_sf %>%
    dplyr::filter(goal %in% {{goal_view}} &
             region_un != "Antarctica" &
             region_un != "Seven seas (open ocean)") %>% 
    dplyr::group_by(entity, indicator) %>%
    dplyr::filter(latest_data_year == max(latest_data_year) |
        is.na(latest_data_year)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(total_inds = n_distinct(indicator)) %>% 
    dplyr::group_by(entity) %>%
    dplyr::summarise(sum_ind = sum(!is.na(latest_data_year)), prop_ind = 100*(sum_ind/unique(total_inds))) %>% 
    mutate(prop_ind = replace(prop_ind, prop_ind == 0, NA))
  
  num_ind_countries <- sf::as_Spatial(num_ind_countries)
  
  bins <- c(1,seq(20,100, 20))
  
  pal <-
    leaflet::colorBin("YlOrRd", domain = num_ind_countries$prop_ind, bins = bins)
  
  labels <- sprintf(
    "<strong>%s</strong><br/>%g indicators",
    num_ind_countries$entity,
    num_ind_countries$sum_ind
  ) %>% lapply(htmltools::HTML)
  
  leaflet::leaflet(num_ind_countries) %>%
    #clearBounds() %>% 
    setView(0, 30, 2) %>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(
      fillColor = ~ pal(prop_ind),
      weight = 2,
      opacity = 1,
      color = "white",
      dashArray = "3",
      fillOpacity = 0.7,
      highlight = highlightOptions(
        weight = 5,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.7,
        bringToFront = TRUE
      ),
      label = labels,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto"
      )
    ) %>%
    addLegend(
      pal = pal,
      values = ~ prop_ind,
      opacity = 0.7,
      title = "% Indicators\n with Data",
      position = "bottomleft"
    )
  
  
}

