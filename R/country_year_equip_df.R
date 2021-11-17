#' Creates a dataframe for equipment unit counts per country/year.
#'
#' Takes the IISS dataset and creates a new data frame that is the
#' aggregate count of unit counts by equipment_type for each country
#' year.
#'
#' @param iiss IISS dataframe.
#' @return df The equipment unit count dataset.

country_year_equip_df <- function(iiss){

  iiss$iiss <- NULL

  iiss <- iiss %>% dplyr::filter(!is.na(iiss$unit_count))

  df <- data.table::setkey(data.table::setDT(iiss),country,year,equipment_type)[,list(count=sum(unit_count,na.rm = T)),
                                                         by=list(country,year,equipment_type)]
  df
  return(df)
}
