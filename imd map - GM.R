# IMD 

# install.packages("readxl")
# install.packages("sf")
# install.packages("stringr")
# install.packages("dplyr")
# install.packages("leaflet")
# install.packages("janitor")
# install.packages("leaflet.extras")
# install.packages("htmlwidgets")
# install.packages("htmltools")
# install.packages("glue")
# install.packages("htmltools")
# install.packages("htmlwidgets")

library(readxl)
library(sf)
library(stringr)
library(dplyr)
library(leaflet)
library(janitor)
library(leaflet.extras)
library(htmlwidgets)
library(htmltools)
library(glue)
library(htmltools)
library(htmlwidgets)

# setup variables ################################################################################

desired_areas <- c("Bolton", "Bury", "Oldham", "Rochdale", "Tameside", 
                   "Stockport", "Manchester", "Trafford", "Salford", "Wigan")

# desired_areas <- c("Bolton")

# imd data ########################################################################################

# imd 2010
# https://www.gov.uk/government/statistics/english-indices-of-deprivation-2010
  imd2010 <- read_excel("Copy of 1871524.xls", sheet = 2) %>%
    clean_names()
  names(imd2010)[7] <- "imd_rank_1_deprived"
  imd2010 <- imd2010 %>%
    mutate(total_lsoas = n(), 
           imd_decile_1_deprived = cut(imd_rank_1_deprived, breaks = 10, labels = 1:10, ordered_result = TRUE) 
           )
 
  # # check cuts have worked - not included on the download on this one
  # imd2010 %>%
  #   group_by(imd_decile_1_deprived) %>%
  #   summarise(n())

# imd 2015
  # https://www.gov.uk/government/statistics/english-indices-of-deprivation-2015
  imd2015 <- read_excel("File_1_ID_2015_Index_of_Multiple_Deprivation.xlsx", sheet = 2) %>%
    clean_names()
  names(imd2015)[5:6] <- c("imd_rank_1_deprived", "imd_decile_1_deprived")
  imd2015 <- imd2015 %>%
    mutate(imd_decile_1_deprived = as.factor(imd_decile_1_deprived),
           total_lsoas = n())

# imd 2019 
  # https://www.gov.uk/government/statistics/english-indices-of-deprivation-2019
  imd2019 <- read_excel("File_1_-_IMD2019_Index_of_Multiple_Deprivation.xlsx", sheet = 2) %>%
    clean_names()
  names(imd2019)[5:6] <- c("imd_rank_1_deprived", "imd_decile_1_deprived")
  imd2019 <- imd2019 %>%
    mutate(imd_decile_1_deprived = as.factor(imd_decile_1_deprived),
           total_lsoas = n())
  
# geographical data ##################################################################################

  # LSOA boundaries 2011 (current)
  # https://geoportal.statistics.gov.uk/datasets/lower-layer-super-output-areas-december-2011-generalised-clipped-boundaries-in-england-and-wales  
    lsoas_2011 <- st_read("G:\\Mapping Data\\R\\map\\Lower_Layer_Super_Output_Areas_December_2011_Generalised_Clipped__Boundaries_in_England_and_Wales/Lower_Layer_Super_Output_Areas_December_2011_Generalised_Clipped__Boundaries_in_England_and_Wales.shp")
  
    # add boroughs variable from LSOA name
    lsoas_2011 <- lsoas_2011 %>%
      mutate(borough = str_sub(lsoa11nm, 1, nchar(as.character(lsoa11nm))-5)) %>%
      st_transform(crs = 4326) # transforms to lat/ long from OSGB36

  # lsoa boundaries 2001 (for IMD 2010)
  # https://geoportal.statistics.gov.uk/datasets/lower-layer-super-output-areas-december-2001-full-clipped-boundaries-in-england-and-wales
    lsoas_2001 <- st_read("G:\\Mapping Data\\R\\map\\lsoa2001/Lower_Layer_Super_Output_Areas_December_2001_Generalised_Clipped_Boundaries_in_England_and_Wales.shp")
    # add boroughs variable from LSOA name
    lsoas_2001 <- lsoas_2001 %>%
      mutate(borough = str_sub(lsoa01nm, 1, nchar(as.character(lsoa01nm))-5)) %>%
      st_transform(crs = 4326) # transforms to lat/ long from OSGB36
    
  # wards
    # https://www.ordnancesurvey.co.uk/business-government/products/boundaryline
    wards <- st_read("G:\\Mapping Data\\R\\map\\OS boundary file\\Data\\GB\\district_borough_unitary_ward.TAB")
    wards <- wards %>%
      mutate(borough = str_replace_all(File_Name, "_", " "),
             borough = str_replace_all(borough, "  ", " "),
             borough = str_remove(borough, " \\(B\\)"), # () are special characters, need to escape them. 
             borough = str_remove(borough, " DISTRICT"),
             borough = str_to_title(borough)) %>%
      st_transform(crs = 4326) # transforms to lat/ long from OSGB36
    
  # boroughs
    boroughs <- st_read("G:\\Mapping Data\\R\\map\\OS boundary file\\Data\\GB\\district_borough_unitary.TAB")
    boroughs <- boroughs %>%
      mutate(borough = str_remove(Name, " District \\(B\\)")) %>%
      st_transform(crs = 4326) # transforms to lat/ long from OSGB36  
    
# filter areas ###############################################  
  
  # filter lsoas 2011 GM only
    GM_lsoas_2011 <- filter(lsoas_2011, borough %in% desired_areas)
    # plot(st_geometry(GM_lsoas)) # check areas look right
  
  # merge in IMD info & lsoa boundaries
    GM_lsoas_imd_2015 <- left_join(GM_lsoas_2011, imd2015, by = c("lsoa11cd" = "lsoa_code_2011"))  # add in Imd data
    GM_lsoas_imd_2019 <- left_join(GM_lsoas_2011, imd2019, by = c("lsoa11cd" = "lsoa_code_2011"))
 
  # filter GM 2001 lsoas only
    GM_lsoas_2001 <- filter(lsoas_2001, borough %in% desired_areas) 
    # plot(st_geometry(GM_lsoas_2001)) # check areas look right

    # merge in IMD info & lsoa boundaries  
    GM_lsoas_imd_2010 <- left_join(GM_lsoas_2001, imd2010, by = c("lsoa01cd" = "lsoa_code"))
    
  # filter wards GM only
    GM_wards <- filter(wards, borough %in% desired_areas)
   # plot(st_geometry(GM_wards)) # check areas look right
  
  # filter boroughs GM only
    GM_boroughs <- filter(boroughs, borough %in% desired_areas)
    # plot(st_geometry(GM_boroughs)) # check areas look right 
  
# map ########################################################################################
  
  my_bbox <- as.vector(st_bbox(GM_wards)) # boundary box around selected areas for to get corners for default map view 
  
  ward_labels = (glue("<b>{GM_wards$borough}</b> ward: <br>{str_sub(GM_wards$Name, 1, nchar(as.character(GM_wards$Name)) - 5)}"))
  
  borough_labels = (glue("<b>{GM_boroughs$borough}</b>")) 
  
  imd_2019_labels = (glue(
    "<b> IMD 2019</b><br>",
    "{GM_lsoas_imd_2019$lsoa11nmw}; <b>Decile {GM_lsoas_imd_2019$imd_decile_1_deprived}</b><br>", 
    "Rank: {prettyNum(GM_lsoas_imd_2019$imd_rank_1_deprived, big.mark=',')} of {prettyNum(GM_lsoas_imd_2019$total_lsoas, big.mark=',')} <br>", 
    "(ranked in the top <b>{round(GM_lsoas_imd_2019$imd_rank_1_deprived/ GM_lsoas_imd_2019$total_lsoas*100)}%</b> in England)<br>",
    "<a href=https://www.gov.uk/government/statistics/english-indices-of-deprivation-2019 target=_blank>find out more about IMD</a>")) # opens link in new window breaks with iframe if same 
 
  imd_2015_labels = (glue(
      "<b> IMD 2015</b><br>",
      "{GM_lsoas_imd_2015$lsoa11nmw}; <b>Decile {GM_lsoas_imd_2015$imd_decile_1_deprived}</b><br>", 
      "Rank: {prettyNum(GM_lsoas_imd_2015$imd_rank_1_deprived, big.mark=',')} of {prettyNum(GM_lsoas_imd_2015$total_lsoas, big.mark=',')} <br>", 
      "(ranked in the top <b>{round(GM_lsoas_imd_2015$imd_rank_1_deprived/ GM_lsoas_imd_2015$total_lsoas*100)}%</b> in England)<br>",
      "<a href=https://www.gov.uk/government/statistics/english-indices-of-deprivation-2015 target=_blank>find out more about IMD</a>")) # opens link in new window breaks with iframe if same 

  imd_2010_labels = (glue(
      "<b>IMD 2010</b><br>",
      "{GM_lsoas_imd_2010$lsoa01nmw}; <b>Decile {GM_lsoas_imd_2010$imd_decile_1_deprived}</b><br>", 
      "Rank: {prettyNum(GM_lsoas_imd_2010$imd_rank_1_deprived, big.mark=',')} of {prettyNum(GM_lsoas_imd_2010$total_lsoas, big.mark=',')} <br>", 
      "(ranked in the top <b>{round(GM_lsoas_imd_2010$imd_rank_1_deprived/ GM_lsoas_imd_2010$total_lsoas*100)}%</b> in England)<br>",
      "<a href=https://www.gov.uk/government/statistics/english-indices-of-deprivation-2010 target=_blank>find out more about IMD</a>")) # opens link in new window breaks with iframe if same 
  
  my_title <- tags$h1(tags$style("h1 {color: black; font-size:16px}"),
                      tags$b("Indices of Multiple Deprivation (IMD)"),
                      tags$p(tags$style("p {color:black; font-size:12px}"),
                             tags$p("Click on an area to find out more | Turn layers on and off to compare")))
 
  # make colour palatte 
  imd_decile_colours <- colorFactor(
    palette = c("#B30000", "#418FDE"), 
        levels = c(1, 10), 
    na.color = "white")
 
  imd_map <-  GM_lsoas_2011 %>%
    leaflet() %>%
    addResetMapButton() %>%
    fitBounds(lng1 = my_bbox[1], lat1 = my_bbox[2], lng2 = my_bbox[3], lat2= my_bbox[4]) %>%
    addProviderTiles("Stamen.TonerLite") %>%
    addPolygons(data = GM_boroughs, weight = 5, color = "#008142",
                fillColor = "white", fillOpacity = 0, group = "Boroughs",
                highlight = highlightOptions(weight = 5, color = "#666", bringToFront = FALSE),
                popup = ~borough_labels,
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px"),
                  textsize = "15px",
                  direction = "auto")) %>%
    addPolygons(data = GM_wards, weight = 2, color = "#008142",
                fillColor = "white", fillOpacity = 0, group = "Wards",
                highlight = highlightOptions(weight = 3, color = "#666", bringToFront = TRUE),
                popup = ~ward_labels,
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px"),
                  textsize = "15px",
                  direction = "auto")) %>%
    addPolygons(data = GM_lsoas_imd_2010, weight = 0.75, color = "lightgrey",
                fillColor = ~imd_decile_colours(imd_decile_1_deprived), fillOpacity = 0.5, group = "IMD 2010 decile",
                highlight = highlightOptions(weight = 3, color = "#666", bringToFront = TRUE),
                popup = ~imd_2010_labels,
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px"),
                  textsize = "15px",
                  direction = "auto")) %>%
    addPolygons(data = GM_lsoas_imd_2015, weight = 0.75, color = "lightgrey",
                fillColor = ~imd_decile_colours(imd_decile_1_deprived), fillOpacity = 0.5, group = "IMD 2015 decile",
                highlight = highlightOptions(weight = 3, color = "#666", bringToFront = TRUE),
                popup = ~imd_2015_labels,
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px"),
                  textsize = "15px",
                  direction = "auto")) %>%
    addPolygons(data = GM_lsoas_imd_2019, weight = 0.75, color = "lightgrey",
                fillColor = ~imd_decile_colours(imd_decile_1_deprived), fillOpacity = 0.5, group = "IMD 2019 decile",
                highlight = highlightOptions(weight = 3, color = "#666", bringToFront = TRUE),
                popup = ~imd_2019_labels,
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px"),
                  textsize = "15px",
                  direction = "auto")) %>%
    addLayersControl(overlayGroups  = c("Boroughs", "Wards", "IMD 2010 decile", "IMD 2015 decile", "IMD 2019 decile"), position = "topleft") %>%
    addLegend(
      position = "topleft",
      colors = c("#B30000", "#418FDE"),
      labels = c("Most deprived 10%", "Least deprived 10%"),
      opacity = 1,
      title = "Rank in England") %>%
    addControl(my_title, position = "topright")
  
  # save map 
  # htmltools::save_html(imd_map, "imd_map.html") # smaller file size but in a fixed window size
  saveWidget(imd_map, "imd_map.html") # larger file size but window size adjusts properly
  
  ################################## working copy for making adjustments on below #############
  
 
  