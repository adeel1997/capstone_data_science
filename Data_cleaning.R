library(stringr);library(dplyr)
data <- read.delim("Data/earthquakes-2021-05-16_17-33-35_+0530.tsv")
head(data)
eq_clean_data <- function(data){
    ## Converting date into date column
    data <- data %>% mutate(date = as.Date(paste0(Year,"-",Mo,"-",Dy),format=
                                               "%Y-%m-%d"))
    ## Converting Lat Long into numeric
    data$Latitude <- as.numeric(data$Latitude)
    data$Longitude <- as.numeric(data$Longitude)
    ## Getting the location name and splitting with colon 
    location_name <- strsplit(data$Location.Name,split = ":")
    ## Removing the country and Selecting the region
    location_name <- sapply(location_name,"[",2)
    ## Removing extra white space
    location_name <- str_trim(location_name)
    ## Converting All caps to title case
    location_name <- str_to_title(location_name)
    ## Converting into a vector
    location_name <- as.vector(unlist(location_name))
    data$Location_Name <- location_name
    return(data)
                            
}

clean_data <- eq_clean_data(data = data)
head(clean_data)
