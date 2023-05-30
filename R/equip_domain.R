#' Generates a new column in the IISS dataframe that identifies the domain of each
#' country's equipment_type.
#'
#' Takes the IISS dataframe, and based on each country's equipment_type, it identifies
#' which domain that equipment_type belongs to, and adds it to a new column, "domain".
#'
#' @param df IISS dataframe.

equip_domain <- function(df){
  domain <- c("land", "air", "sea", "cyber and space")

  df$equip_domain <- ifelse(df$equipment_type=="radars",      "cyber and space",

                        ifelse(df$equipment_type=="armoured fighting vehicles" |
                                 df$equipment_type=="artillery" |
                                 df$equipment_type=="engineering and maintenance vehicles" |
                                 df$equipment_type=="mine warfare" |
                                 df$equipment_type=="anti-tank/anti-infrastructure" |
                                 df$equipment_type=="surface-to-surface missile launchers",           "land",

                               ifelse(df$equipment_type=="patrol and coastal combatants" |
                                        df$equipment_type=="amphibious" |
                                        df$equipment_type=="submarines" |
                                        df$equipment_type=="principal surface combatants" |
                                        df$equipment_type=="coastal defence" |
                                        df$equipment_type=="logistics and support",               "sea",

                                      ifelse(df$equipment_type=="air defence" |
                                               df$equipment_type=="helicopters" |
                                               df$equipment_type=="aircraft" |
                                               df$equipment_type=="unmanned aerial vehicles" |
                                               df$equipment_type=="air-launched missiles" |
                                               df$equipment_type=="bombs" |
                                               df$equipment_type=="ballistic missiles",    "air",

                                                           NA  )))) # all other values map to NA

  return(df)
}
