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

sdg_plotter <- function(goal_view = 1:17, var_in = "num_indicators"){
  
  if(var_in == "num_indicators"){
  num_ind_countries <- world_sf %>%
    dplyr::filter(goal %in% {{goal_view}} &
             region_un != "Antarctica" &
             region_un != "Seven seas (open ocean)") %>% 
    dplyr::mutate(total_inds = n_distinct(indicator)) %>% 
    dplyr::group_by(entity) %>%
    dplyr::summarise(sum_ind = sum(!is.na(latest_data_year)), prop_ind = 100*(sum_ind/unique(total_inds))) %>% 
    mutate(disp_val = replace(prop_ind, prop_ind == 0, NA))

  bins <- c(1,seq(20,100, 20))
 
  labels <- sprintf(
    "<strong>%s</strong><br/>%g indicators",
    num_ind_countries$entity,
    num_ind_countries$sum_ind
  ) %>% lapply(htmltools::HTML)
  
   map_out <- sf::as_Spatial(num_ind_countries)
  
   }
  
 
  if(var_in == "mean_age"){
    
    mean_yr_countries <- world_sf %>%
      dplyr::filter(goal %in% {{goal_view}} &
                      region_un != "Antarctica" &
                      region_un != "Seven seas (open ocean)") %>% 
      filter(!is.na(latest_data_year)) %>%
      group_by(entity) %>%
      summarise(mean_year = mean(latest_data_year), disp_val = mean_year)
    
    if (min(mean_yr_countries$disp_val) < 2014){
      bins <- round(seq(min(mean_yr_countries$disp_val),2020, length.out = 6))
    }else{
      bins <- seq(min(mean_yr_countries$disp_val),2020)
    }
    
    labels <- sprintf(
      "<strong>%s</strong><br/>Average year of data availability: %g",
      mean_yr_countries$entity,
      mean_yr_countries$disp_val
    ) %>% lapply(htmltools::HTML)
    
    map_out <- sf::as_Spatial(mean_yr_countries)
  }
  
  
   pal <-
    leaflet::colorBin("YlOrRd", domain = map_out$disp_val, bins = bins)
  
  leaflet::leaflet(map_out) %>%
    setView(0, 30, 2) %>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(
      fillColor = ~ pal(disp_val),
      weight = 1,
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
      values = ~ disp_val,
      opacity = 0.7,
      title = ifelse(var_in == "num_indicators","% Indicators\n with Data", "Date of Most Recent Data"),   #should change to switch
      position = "bottomleft",
      labFormat = labelFormat(big.mark = "")
    )
  
  
}

