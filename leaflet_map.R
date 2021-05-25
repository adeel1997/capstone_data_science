source("geom_label.R")
eq_map <- function(data, annot_col) {
    
    # note that annot_col in a character string
    leaflet::leaflet() %>%
        leaflet::addTiles() %>%
        leaflet::addCircleMarkers(data = data, radius = ~ Mag*1.5,opacity = 0.5,weight = 5,
                                  lng = ~ Longitude, lat = ~ Latitude,
                                  popup = ~ data[[annot_col]])
}


eq_create_label <- function(data) {
    labelled_data <- data %>%
        dplyr::mutate(popup_text = paste("<b>Location:</b>", Location, "<br />",
                                         "<b>Magnitude:</b>", Mag, "<br />",
                                         "<b>Total Deaths:</b>", Deaths, "<br />"),
                      popup_text = ifelse(is.na(Location) | is.na(Mag) | is.na(Deaths),
                                          paste("<b>No Data Available</b>"), popup_text))
    labelled_data$popup_text
}



data_clean <- data %>%
    eq_clean_data()%>%
    dplyr::filter(Country == "India"|Country == "Pakistan" & lubridate::year(date) >= 2000)
eq_map(data = data_clean, annot_col = "date")


