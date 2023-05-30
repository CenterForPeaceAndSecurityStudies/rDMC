# Create missingness dataframe
# Updated 8/14/2019
# run this function before running missingness_plot.R

library(tidyverse)

missingness_df <- function(){
  cow_states <- c("Afghanistan","Albania","Algeria","Angola","Antigua & Barbuda","Argentina",
                  "Armenia","Australia","Austria","Azerbaijan","Bahamas","Bahrain","Bangladesh",
                  "Barbados","Belarus","Belgium","Belize","Benin",
                  "Bolivia","Bosnia and Herzegovina","Botswana","Brazil","Brunei","Bulgaria",
                  "Burkina Faso","Burundi","Cape Verde","Cambodia","Cameroon","Canada",
                  "Central African Republic","Chad","Chile","China","Colombia","Congo",
                  "Costa Rica","Ivory Coast","Croatia","Cuba","Cyprus","Czechoslovakia",
                  "Czech Republic","Democratic Republic of the Congo","Denmark","Djibouti",
                  "Dominican Republic","Ecuador","Egypt","El Salvador","Equatorial Guinea",
                  "Eritrea","Estonia","Ethiopia","Fiji","Finland","France","Gabon","Gambia",
                  "Georgia","Germany","Ghana","Greece",
                  "Grenada","Guatemala","Guinea","Guinea-Bissau","Guyana","Haiti","Honduras",
                  "Hungary","Iceland","India","Indonesia","Iran","Iraq","Ireland","Israel",
                  "Italy","Jamaica","Japan","Jordan","Kazakhstan","Kenya","North Korea",
                  "Kuwait","Kyrgyzstan","Laos","Latvia","Lebanon","Lesotho",
                  "Liberia","Libya","Lithuania","Luxembourg","Madagascar","Malawi","Malaysia",
                  "Mali","Malta","Mauritania","Mauritius","Mexico",
                  "Mongolia","Montenegro","Morocco","Mozambique","Myanmar","Namibia","Nepal",
                  "Netherlands","New Zealand","Nicaragua","Niger","Nigeria","Norway","Oman",
                  "Pakistan","Panama","Papua New Guinea","Paraguay","Peru",
                  "Philippines","Poland","Portugal","Qatar","South Korea","Moldova",
                  "Romania","Russia","Rwanda","Sao Tome and Principe","Saudi Arabia",
                  "Senegal","Seychelles","Sierra Leone","Singapore","Slovakia","Slovenia",
                  "Solomon Islands","Somalia","South Africa","South Sudan","Spain","Sri Lanka",
                  "Sudan","Suriname","Sweden","Switzerland","Syria","Taiwan","Tajikistan",
                  "Thailand","Macedonia","Togo","Tonga","Trinidad and Tobago","Tunisia","Turkey",
                  "Turkmenistan","Uganda","Ukraine","United Arab Emirates","United Kingdom",
                  "Tanzania","United States","Uruguay","Uzbekistan","Venezuela","Vietnam",
                  "Yemen","Yugoslavia",
                  "Zambia","Zimbabwe")

  #iiss_countries
  iiss_supp <- googlesheets4::read_sheet('1yf-dGAiicIGNScouedvOCET4s34GGl3OelXrgOPWq8g')
  iiss_country <- googlesheets4::read_sheet('1yf-dGAiicIGNScouedvOCET4s34GGl3OelXrgOPWq8g', sheet = "iiss_countries")

  states <- read.csv(file = paste0(here::here(), '/data/states2016.csv'))
  states[which(states$statenme=="United States of America"),] <- "United States"

  iiss <- readRDS(file = paste0(here::here(), '/data/01b_clean.rds'))
  iiss$iiss <- NULL

  all_years <- unique(1970:2014)
  all_countries <- sort(states$statenme)
  countries <- c("countries",all_countries)

  ############################### Missingness Plot! ##############################

  list <- list()
  count <- 1
  for(a_year in all_years){
    for(country in all_countries){
      value <- 0

      stemp <- states %>% filter(states$statenme==country)
      if(nrow(stemp)>0){
        if(!(a_year>=stemp$styear & a_year<=stemp$endyear)){
          value <- -1
        }
      }

      temp <- iiss_country %>% filter(iiss_country$year==a_year)
      if(country=="Antigua & Barbuda"){country <- "Antigua and Barbuda"}
      if(country=="Bosnia and Herzegovina"){country <- "Bosnia-Herzegovina"}
      if(country=="Sao Tome and Principe"){country <- "São Tomé and Principe"}
      temp <- temp %>% filter(temp$iiss_country_name==country)
      if(nrow(temp) > 0){
        value <- 1
      }

      iiss_temp <- iiss %>% filter(iiss$year==a_year)
      temp_country <- tolower(country)
      if(temp_country=="united states"){temp_country <- "us"}
      if(temp_country=="antigua & barbuda"){temp_country <- "antigua and barbuda"}
      if(temp_country=="bosnia and herzegovina"){temp_country <- "bosnia-herzegovina"}
      if(temp_country=="north korea"){temp_country <- "korea, dpr of"}
      if(temp_country=="south korea"){temp_country <- "korea, republic of"}

      iiss_temp <- iiss_temp %>% filter(iiss_temp$country==temp_country)
      if(nrow(iiss_temp) > 0 ){
        value <- 2
      }

      year <- c(a_year,country,value)
      list[[count]] <- year
      count <- count + 1
    }
  }

  missingness <- rbind(list[[1]],list[[2]])
  for(j in 3:length(list)){
    missingness <- rbind(missingness,list[[j]])
  }

  colnames(missingness) <- c("year","country","value")

  missingness <- as.data.frame(missingness)

  return(missingness)
}
