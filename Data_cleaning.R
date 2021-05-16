library(stringr);library(dplyr)
data <- read.delim("Data/earthquakes-2021-05-16_23-42-17_+0530.tsv")
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
    ## Country is first in the list and second is region
    Country <- sapply(location_name,"[",1)
    ## Converting All caps to Title case
    Country <- str_to_title(Country)
    data$Country <- Country
    ## Removing the country and Selecting the region
    region <- sapply(location_name,"[",2)
    ## Removing extra white space
    region <- str_trim(region)
    ## Converting All caps to title case
    region <- str_to_title(region)
    ## Converting into a vector
    region <- as.vector(unlist(region))
    data$Location_Name <- region
    return(data)
                            
}
