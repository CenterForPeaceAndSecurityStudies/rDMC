#' Coerces all incorrect or misspelled entries in the IISS dataset.
#'
#' Takes the IISS dataframe, and, using multiple if-statements,
#' coerces all incorrect or misspelled entries. The output
#' contains all correct parent-child relationships.
#'
#' @param iiss The IISS dataframe.

iiss_wrangling <- function(iiss){


library(magrittr)
# Surface-to-surface missile launchers
#missiles>cruisers>na>na>salish / shaddock
  iiss$equipment_name[iiss$equipment_type == "missiles" &
                        iiss$equipment_subtype == "cruisers" &
                        (iiss$unit_name == "Salish" |
                           iiss$unit_name == "Shaddock")] <- "AShM"
  iiss$equipment_subtype[iiss$equipment_type == "missiles" &
                        iiss$equipment_subtype == "cruisers" &
                        iiss$equipment_name == "AShM" &
                        (iiss$unit_name == "Salish" |
                           iiss$unit_name == "Shaddock")] <- "missiles"
  iiss$equipment_type[iiss$equipment_type == "missiles" &
                           iiss$equipment_subtype == "missiles" &
                           iiss$equipment_name == "AShM" &
                           (iiss$unit_name == "Salish" |
                              iiss$unit_name == "Shaddock")] <- "surface-to-surface missile launchers"
# missiles>strategic/na>abm>na
iiss$equipment_subname[iiss$equipment_type == "missiles" &
                         (iiss$equipment_subtype == "strategic" |
                            is.na(iiss$equipment_subtype)) &
                         iiss$equipment_name == "ABM"] <- "ABM"
iiss$equipment_name[iiss$equipment_type == "missiles" &
                      (iiss$equipment_subtype == "strategic" |
                         is.na(iiss$equipment_subtype)) &
                         iiss$equipment_name == "ABM" &
                         iiss$equipment_subname == "ABM"] <- "SAM"
iiss$equipment_subtype[iiss$equipment_type == "missiles" &
                         (iiss$equipment_subtype == "strategic" |
                            is.na(iiss$equipment_subtype)) &
                      iiss$equipment_name == "SAM" &
                      iiss$equipment_subname == "ABM"] <- "missiles"
iiss$equipment_type[iiss$equipment_type == "missiles" &
                         iiss$equipment_subtype == "missiles" &
                         iiss$equipment_name == "SAM" &
                         iiss$equipment_subname == "ABM"] <- "air defence"
# launcher>na>na>na
iiss$equipment_name[iiss$equipment_type == "launcher" &
                        is.na(iiss$equipment_name)] <- "SSM"
iiss$equipment_subtype[iiss$equipment_type == "launcher" &
                        iiss$equipment_name == "SSM"] <- "missiles"
iiss$equipment_type[iiss$equipment_type == "launcher" &
                        iiss$equipment_subtype == "missiles" &
                           iiss$equipment_name == "SSM"] <- "surface-to-surface missile launchers"
#stsml>missiles>AIM
iiss$equipment_name[iiss$equipment_type == "surface-to-surface missile launchers" &
                      iiss$equipment_name == "AIM"] <- "AAM"
iiss$equipment_type[iiss$equipment_type == "surface-to-surface missile launchers" &
                      iiss$equipment_name == "AAM"] <- "air-launched missiles"
#stsml>na>ashm>na
iiss$equipment_subtype[iiss$equipment_type == "surface-to-surface missile launchers" &
                         is.na(iiss$equipment_subtype) &
                         iiss$equipment_name == "AShM" &
                         is.na(iiss$equipment_subname)] <- "missiles"
#stsml>na>asm>na
iiss$equipment_subtype[iiss$equipment_type == "surface-to-surface missile launchers" &
                         is.na(iiss$equipment_subtype) &
                         iiss$equipment_name == "ASM" &
                         is.na(iiss$equipment_subname)] <- "missiles"
#stsml>na>srbm>na
iiss$equipment_subname[iiss$equipment_type == "surface-to-surface missile launchers" &
                         is.na(iiss$equipment_subtype) &
                         iiss$equipment_name == "SRBM" &
                         is.na(iiss$equipment_subname)] <- "tactical"
iiss$equipment_subtype[iiss$equipment_type == "surface-to-surface missile launchers" &
                         is.na(iiss$equipment_subtype) &
                         iiss$equipment_name == "SRBM" &
                         iiss$equipment_subname == "tactical"] <- "missiles"
iiss$equipment_type[iiss$equipment_type == "surface-to-surface missile launchers" &
                      iiss$equipment_subtype == "missiles" &
                      iiss$equipment_name == "SRBM" &
                      iiss$equipment_subname == "tactical"] <- "ballistic missiles"
#stsml>na>ssm>na
iiss$equipment_subtype[iiss$equipment_type == "surface-to-surface missile launchers" &
                         is.na(iiss$equipment_subtype) &
                         iiss$equipment_name == "SSM" &
                         is.na(iiss$equipment_subname)] <- "missiles"
#stsml>strat>icbm>na
iiss$equipment_subname[iiss$equipment_type == "surface-to-surface missile launchers" &
                         iiss$equipment_subtype == "strategic" &
                         iiss$equipment_name == "ICBM" &
                         is.na(iiss$equipment_subname)] <- "strategic"
iiss$equipment_subtype[iiss$equipment_type == "surface-to-surface missile launchers" &
                         iiss$equipment_subtype == "strategic" &
                         iiss$equipment_name == "ICBM" &
                         iiss$equipment_subname == "strategic"] <- "missiles"
iiss$equipment_type[iiss$equipment_type == "surface-to-surface missile launchers" &
                      iiss$equipment_subtype == "missiles" &
                      iiss$equipment_name == "ICBM" &
                      iiss$equipment_subname == "strategic"] <- "ballistic missiles"
#stsml>strat>irbm>na
iiss$equipment_subname[iiss$equipment_type == "surface-to-surface missile launchers" &
                         iiss$equipment_subtype == "strategic" &
                         iiss$equipment_name == "IRBM" &
                         is.na(iiss$equipment_subname)] <- "strategic"
iiss$equipment_subtype[iiss$equipment_type == "surface-to-surface missile launchers" &
                         iiss$equipment_subtype == "strategic" &
                         iiss$equipment_name == "IRBM" &
                         iiss$equipment_subname == "strategic"] <- "missiles"
iiss$equipment_type[iiss$equipment_type == "surface-to-surface missile launchers" &
                      iiss$equipment_subtype == "missiles" &
                      iiss$equipment_name == "IRBM" &
                      iiss$equipment_subname == "strategic"] <- "ballistic missiles"
#stsml>strat>mrbm>na
iiss$equipment_subname[iiss$equipment_type == "surface-to-surface missile launchers" &
                         iiss$equipment_subtype == "strategic" &
                         iiss$equipment_name == "MRBM" &
                         is.na(iiss$equipment_subname)] <- "strategic"
iiss$equipment_subtype[iiss$equipment_type == "surface-to-surface missile launchers" &
                         iiss$equipment_subtype == "strategic" &
                         iiss$equipment_name == "MRBM" &
                         iiss$equipment_subname == "strategic"] <- "missiles"
iiss$equipment_type[iiss$equipment_type == "surface-to-surface missile launchers" &
                      iiss$equipment_subtype == "missiles" &
                      iiss$equipment_name == "MRBM" &
                      iiss$equipment_subname == "strategic"] <- "ballistic missiles"
#stsml>tac>ashm>na
iiss$equipment_subname[iiss$equipment_type == "surface-to-surface missile launchers" &
                         iiss$equipment_subtype == "tactical" &
                         iiss$equipment_name == "AShM" &
                         is.na(iiss$equipment_subname)] <- "tactical"
iiss$equipment_subtype[iiss$equipment_type == "surface-to-surface missile launchers" &
                         iiss$equipment_subtype == "tactical" &
                         iiss$equipment_name == "AShM" &
                         iiss$equipment_subname == "strategic"] <- "missiles"
iiss$equipment_type[iiss$equipment_type == "surface-to-surface missile launchers" &
                      iiss$equipment_subtype == "missiles" &
                      iiss$equipment_name == "AShM" &
                      iiss$equipment_subname == "strategic"] <- "air-launched missiles"
#stsml>tac>asm>na
iiss$equipment_subname[iiss$equipment_type == "surface-to-surface missile launchers" &
                         iiss$equipment_subtype == "tactical" &
                         iiss$equipment_name == "ASM" &
                         is.na(iiss$equipment_subname)] <- "tactical"
iiss$equipment_subtype[iiss$equipment_type == "surface-to-surface missile launchers" &
                         iiss$equipment_subtype == "tactical" &
                         iiss$equipment_name == "ASM" &
                         iiss$equipment_subname == "tactical"] <- "missiles"
iiss$equipment_type[iiss$equipment_type == "surface-to-surface missile launchers" &
                      iiss$equipment_subtype == "missiles" &
                      iiss$equipment_name == "ASM" &
                      iiss$equipment_subname == "strategic"] <- "air-launched missiles"
#stsml>tac>srbm>na
iiss$equipment_subname[iiss$equipment_type == "surface-to-surface missile launchers" &
                         iiss$equipment_subtype == "tactical" &
                         iiss$equipment_name == "SRBM" &
                         is.na(iiss$equipment_subname)] <- "tactical"
iiss$equipment_subtype[iiss$equipment_type == "surface-to-surface missile launchers" &
                         iiss$equipment_subtype == "tactical" &
                         iiss$equipment_name == "SRBM" &
                         iiss$equipment_subname == "tactical"] <- "missiles"
iiss$equipment_type[iiss$equipment_type == "surface-to-surface missile launchers" &
                      iiss$equipment_subtype == "missiles" &
                      iiss$equipment_name == "SRBM" &
                      iiss$equipment_subname == "tactical"] <- "ballistic missiles"
#stsml>tac>ssm>na
iiss$equipment_subname[iiss$equipment_type == "surface-to-surface missile launchers" &
                         iiss$equipment_subtype == "tactical" &
                         iiss$equipment_name == "SSM" &
                         is.na(iiss$equipment_subname)] <- "tactical"
iiss$equipment_subtype[iiss$equipment_type == "surface-to-surface missile launchers" &
                         iiss$equipment_subtype == "tactical" &
                         iiss$equipment_name == "SSM" &
                         iiss$equipment_subname == "tactical"] <- "missiles"
#stsml>tactical>ashm>tactical
iiss$equipment_subtype[iiss$equipment_type == "surface-to-surface missile launchers" &
                         iiss$equipment_subtype == "tactical" &
                         iiss$equipment_name == "AShM" &
                         iiss$equipment_subname == "tactical"] <- "missiles"
#stsml>tactical>msl>asm
iiss$equipment_subname[iiss$equipment_type == "surface-to-surface missile launchers" &
                         iiss$equipment_subtype == "tactical" &
                         iiss$equipment_name == "MSL" &
                         iiss$equipment_subname == "ASM"] <- "tactical"
iiss$equipment_name[iiss$equipment_type == "surface-to-surface missile launchers" &
                      iiss$equipment_subtype == "tactical" &
                      iiss$equipment_name == "MSL" &
                      iiss$equipment_subname == "tactical"] <- "ASM"
iiss$equipment_subtype[iiss$equipment_type == "surface-to-surface missile launchers" &
                         iiss$equipment_subtype == "tactical" &
                         iiss$equipment_name == "ASM" &
                         iiss$equipment_subname == "tactical"] <- "missiles"
iiss$equipment_type[iiss$equipment_type == "surface-to-surface missile launchers" &
                      iiss$equipment_subtype == "missiles" &
                      iiss$equipment_name == "ASM" &
                      iiss$equipment_subname == "tactical"] <- "air-launched missiles"
#stsml>strategic>na>na
iiss$equipment_subname[iiss$equipment_type == "surface-to-surface missile launchers" &
                         iiss$equipment_subtype == "strategic" &
                         is.na(iiss$equipment_name) &
                         is.na(iiss$equipment_subname)] <- "strategic"
iiss$equipment_name[iiss$equipment_type == "surface-to-surface missile launchers" &
                      iiss$equipment_subtype == "strategic" &
                      is.na(iiss$equipment_name) &
                      iiss$equipment_subname == "strategic"] <- "SLBM"
iiss$equipment_subtype[iiss$equipment_type == "surface-to-surface missile launchers" &
                         iiss$equipment_subtype == "strategic" &
                         iiss$equipment_name == "SLBM" &
                         iiss$equipment_subname == "strategic"] <- "missiles"
iiss$equipment_type[iiss$equipment_type == "surface-to-surface missile launchers" &
                      iiss$equipment_subtype == "missiles" &
                      iiss$equipment_name == "SLBM" &
                      iiss$equipment_subname == "strategic"] <- "ballistic missiles"
#stsml>stategic>srbm>na
iiss$equipment_subname[iiss$equipment_type == "surface-to-surface missile launchers" &
                         iiss$equipment_subtype == "strategic" &
                         iiss$equipment_name == "SRBM" &
                         is.na(iiss$equipment_subname)] <- "strategic"
iiss$equipment_subtype[iiss$equipment_type == "surface-to-surface missile launchers" &
                         iiss$equipment_subtype == "strategic" &
                         iiss$equipment_name == "SRBM" &
                         iiss$equipment_subname == "strategic"] <- "missiles"
iiss$equipment_type[iiss$equipment_type == "surface-to-surface missile launchers" &
                      iiss$equipment_subtype == "missiles" &
                      iiss$equipment_name == "SRBM" &
                      iiss$equipment_subname == "strategic"] <- "ballistic missiles"
#stsml>tactical>mrmb>na
iiss$equipment_subname[iiss$equipment_type == "surface-to-surface missile launchers" &
                         iiss$equipment_subtype == "tactical" &
                         iiss$equipment_name == "MRMB" &
                         is.na(iiss$equipment_subname)] <- "tactical"
iiss$equipment_name[iiss$equipment_type == "surface-to-surface missile launchers" &
                      iiss$equipment_subtype == "tactical" &
                      iiss$equipment_name == "MRMB" &
                      iiss$equipment_subname == "tactical"] <- "MRBM"
iiss$equipment_subtype[iiss$equipment_type == "surface-to-surface missile launchers" &
                         iiss$equipment_subtype == "tactical" &
                         iiss$equipment_name == "MRBM" &
                         iiss$equipment_subname == "tactical"] <- "missiles"
iiss$equipment_type[iiss$equipment_type == "surface-to-surface missile launchers" &
                      iiss$equipment_subtype == "missiles" &
                      iiss$equipment_name == "MRBM" &
                      iiss$equipment_subname == "tactical"] <- "ballistic missiles"
#stsml>tactical>arm>na
iiss$equipment_subname[iiss$equipment_type == "surface-to-surface missile launchers" &
                         iiss$equipment_subtype == "tactical" &
                         iiss$equipment_name == "ARM" &
                         is.na(iiss$equipment_subname)] <- "ARM"
iiss$equipment_name[iiss$equipment_type == "surface-to-surface missile launchers" &
                      iiss$equipment_subtype == "tactical" &
                      iiss$equipment_name == "ARM" &
                      iiss$equipment_subname == "ARM"] <- NA
iiss$equipment_subtype[iiss$equipment_type == "surface-to-surface missile launchers" &
                         iiss$equipment_subtype == "tactical" &
                         is.na(iiss$equipment_name) &
                         iiss$equipment_subname == "ARM"] <- "missiles"
#stsml>strategic>lacm>na
iiss$equipment_subname[iiss$equipment_type == "surface-to-surface missile launchers" &
                         iiss$equipment_subtype == "strategic" &
                         iiss$equipment_name == "LACM" &
                         is.na(iiss$equipment_subname)] <- "strategic"
iiss$equipment_subtype[iiss$equipment_type == "surface-to-surface missile launchers" &
                         iiss$equipment_subtype == "strategic" &
                         iiss$equipment_name == "LACM" &
                         iiss$equipment_subname == "strategic"] <- "missiles"
#stsml>na>ascm>na
iiss$equipment_subtype[iiss$equipment_type == "surface-to-surface missile launchers" &
                         is.na(iiss$equipment_subtype) &
                         iiss$equipment_name == "ASCM" &
                         is.na(iiss$equipment_subname)] <- "missiles"
#stsml>na>asmp>na
iiss$equipment_name[iiss$equipment_type == "surface-to-surface missile launchers" &
                      is.na(iiss$equipment_subtype) &
                      iiss$equipment_name == "ASMP" &
                      is.na(iiss$equipment_subname)] <- "ASM"
iiss$equipment_subtype[iiss$equipment_type == "surface-to-surface missile launchers" &
                         is.na(iiss$equipment_subtype) &
                         iiss$equipment_name == "ASM" &
                         is.na(iiss$equipment_subname)] <- "missiles"
iiss$equipment_type[iiss$equipment_type == "surface-to-surface missile launchers" &
                      iiss$equipment_subtype == "missiles" &
                      iiss$equipment_name == "ASM" &
                      is.na(iiss$equipment_subname)] <- "air-launched missiles"
#stsml>na>assm>na
iiss$equipment_name[iiss$equipment_type == "surface-to-surface missile launchers" &
                      is.na(iiss$equipment_subtype) &
                      iiss$equipment_name == "ASSM" &
                      is.na(iiss$equipment_subname)] <- "AShM"
iiss$equipment_subtype[iiss$equipment_type == "surface-to-surface missile launchers" &
                         is.na(iiss$equipment_subtype) &
                         iiss$equipment_name == "AShM" &
                         is.na(iiss$equipment_subname)] <- "missiles"
#stsml>na>asraam>na
iiss$equipment_name[iiss$equipment_type == "surface-to-surface missile launchers" &
                      is.na(iiss$equipment_subtype) &
                      iiss$equipment_name == "ASRAAM" &
                      is.na(iiss$equipment_subname)] <- "AAM"
#stsml>na>icbm>nuclear
iiss$equipment_subtype[iiss$equipment_type == "surface-to-surface missile launchers" &
                         is.na(iiss$equipment_subtype) &
                         iiss$equipment_name == "ICBM" &
                         (iiss$equipment_subname == "nuclear" |
                            is.na(iiss$equipment_subname))] <- "missiles"
iiss$equipment_type[iiss$equipment_type == "surface-to-surface missile launchers" &
                      iiss$equipment_subtype == "missiles" &
                      iiss$equipment_name == "ICBM" &
                      iiss$equipment_subname == "nuclear"] <- "ballistic missiles"
#stsml>na>lacm>na
iiss$equipment_subtype[iiss$equipment_type == "surface-to-surface missile launchers" &
                         is.na(iiss$equipment_subtype) &
                         iiss$equipment_name == "LACM" &
                         is.na(iiss$equipment_subname)] <- "missiles"
#stsml>na>srbm>conventional
iiss$equipment_subtype[iiss$equipment_type == "surface-to-surface missile launchers" &
                         is.na(iiss$equipment_subtype) &
                         iiss$equipment_name == "SRBM" &
                         iiss$equipment_subname == "conventional"] <- "missiles"
iiss$equipment_type[iiss$equipment_type == "surface-to-surface missile launchers" &
                      iiss$equipment_subtype == "missiles" &
                      iiss$equipment_name == "SRBM" &
                      iiss$equipment_subname == "conventional"] <- "ballistic missiles"
#stsml>na>msl>ssm
iiss$equipment_subtype[iiss$equipment_type == "surface-to-surface missile launchers" &
                         is.na(iiss$equipment_subtype) &
                         iiss$equipment_name == "MSL" &
                         iiss$equipment_subname == "SSM"] <- "missiles"
iiss$equipment_name[iiss$equipment_type == "surface-to-surface missile launchers" &
                      iiss$equipment_subtype == "missiles" &
                      iiss$equipment_name == "MSL" &
                      iiss$equipment_subname == "SSM"] <- "SSM"
iiss$equipment_subname[iiss$equipment_type == "surface-to-surface missile launchers" &
                         iiss$equipment_subtype == "missiles" &
                         iiss$equipment_name == "SSM" &
                         iiss$equipment_subname == "SSM"] <- NA
#stsml>na>tactical>na
iiss$equipment_subname[iiss$equipment_type == "surface-to-surface missile launchers" &
                         is.na(iiss$equipment_subtype) &
                         iiss$equipment_name == "TACTICAL" &
                         is.na(iiss$equipment_subname)] <- "tactical"
iiss$equipment_name[iiss$equipment_type == "surface-to-surface missile launchers" &
                      is.na(iiss$equipment_subtype) &
                      iiss$equipment_name == "TACTICAL" &
                      iiss$equipment_subname == "tactical"] <- "AShM"
iiss$equipment_subtype[iiss$equipment_type == "surface-to-surface missile launchers" &
                         is.na(iiss$equipment_subtype) &
                         iiss$equipment_name == "AShM" &
                         iiss$equipment_subname == "tactical"] <- "missiles"
#stsml>na>scalp eg>na
iiss$equipment_name[iiss$equipment_type == "surface-to-surface missile launchers" &
                      is.na(iiss$equipment_subtype) &
                      iiss$equipment_name == "SCALP EG" &
                      is.na(iiss$equipment_subname)] <- "LACM"
iiss$equipment_subtype[iiss$equipment_type == "surface-to-surface missile launchers" &
                         is.na(iiss$equipment_subtype) &
                         iiss$equipment_name == "LACM" &
                         is.na(iiss$equipment_subname)] <- "missiles"
#stsml>na>aam>asm
iiss$equipment_name[iiss$equipment_type == "surface-to-surface missile launchers" &
                      is.na(iiss$equipment_subtype) &
                      iiss$equipment_name == "AAM" &
                      iiss$equipment_subname == "ASM"] <- "ASM"
iiss$equipment_subtype[iiss$equipment_type == "surface-to-surface missile launchers" &
                         is.na(iiss$equipment_subtype) &
                         iiss$equipment_name == "ASM" &
                         iiss$equipment_subname == "ASM"] <- "missiles"
iiss$equipment_subname[iiss$equipment_type == "surface-to-surface missile launchers" &
                         iiss$equipment_subtype == "missiles" &
                         iiss$equipment_name == "ASM" &
                         iiss$equipment_subname == "ASM"] <- NA
iiss$equipment_subname[iiss$equipment_type == "surface-to-surface missile launchers" &
                         is.na(iiss$equipment_subtype) &
                         iiss$equipment_name == "ASM" &
                         iiss$equipment_subname %in% c("57mm", "80mm")] <- NA
#stsml>na>na>na>unit_name
iiss$equipment_subtype[(iiss$unit_name == "SCUD" |
                          iiss$unit_name == "FROG")] <- "missiles"
iiss$equipment_name[(iiss$unit_name == "SCUD" |
                       iiss$unit_name == "FROG") &
                      iiss$equipment_subtype == "missiles"] <- "SSM"
iiss$equipment_subname[(iiss$unit_name == "SCUD" |
                          iiss$unit_name == "FROG") &
                         iiss$equipment_subtype == "missiles" &
                         iiss$equipment_name == "SSM"] <- "tactical"
#stsml>na>ad>sam
iiss$equipment_name[iiss$equipment_type == "air defence" &
                      iiss$equipment_name == "AD" &
                      iiss$equipment_subname == "SAM"] <- "SAM"
iiss$equipment_subname[iiss$equipment_type == "air defence" &
                      iiss$equipment_name == "SAM" &
                      iiss$equipment_subname == "SAM"] <- NA
iiss$equipment_subtype[iiss$equipment_type == "air defence" &
                         is.na(iiss$equipment_subtype) &
                         iiss$equipment_name == "SAM"] <- "missiles"
#ALM
iiss$equipment_subtype[iiss$equipment_type == "surface-to-surface missile launchers" &
                         (is.na(iiss$equipment_subname) |
                            iiss$equipment_subname == "Tactical - AAM" |
                            iiss$equipment_subname %in% c("IR", "OR") |
                            iiss$equipment_subname == "IIR" |
                            iiss$equipment_subname == "ARH" |
                            iiss$equipment_subname == "AAM" |
                            iiss$equipment_subname == "SARH"|
                            iiss$equipment_subname == "AShM"|
                            iiss$equipment_subname == "ARM"|
                            iiss$equipment_subname == "PRH"|
                            iiss$equipment_subname == "IIR/ARH"|
                            iiss$equipment_subname == "IR/SARH"|
                            iiss$equipment_subname == "IR/ARH") &
                         iiss$unit_name %like any% c("AA-%", "AIM-%", "%AIM%", "AAM-%",
                                                     "%AGM-%", "AM-%", "%ALARM%", "AS%",
                                                     "%Derby%", "IRIS%", "%Iris%", "Kh%",
                                                     "MICA%", "Mica%", "R-%", "PL-%",
                                                     "%Meteor%","Python%", "Shafrir%",
                                                     "Super 530%", "YJ-91", "U-Darter",
                                                     "Armat", "%Magic%", "MAR-1", "R530",
                                                     "RB-%", "S5K%", "S8KP%", "Sky Flash",
                                                     "Sky Sword%", "V3C%")] <- "missiles"
#AD
iiss$equipment_subtype[iiss$equipment_type == "surface-to-surface missile launchers" &
                         iiss$equipment_name == "AAM" |
                         (is.na(iiss$equipment_subname) |
                            iiss$equipment_subname == "Tactical - AAM" |
                            iiss$equipment_subname %in% c("IR", "OR") |
                            iiss$equipment_subname == "IIR" |
                            iiss$equipment_subname == "ARH" |
                            iiss$equipment_subname == "AAM" |
                            iiss$equipment_subname == "SARH"|
                            iiss$equipment_subname == "AShM") &
                         iiss$unit_name %like any% c("%Apside%", "%Roland%")] <- "missiles"
iiss$equipment_type[iiss$equipment_type == "surface-to-surface missile launchers" &
                      iiss$equipment_name == "AAM" |
                      (is.na(iiss$equipment_subname) |
                         iiss$equipment_subname == "Tactical - AAM" |
                         iiss$equipment_subname %in% c("IR", "OR") |
                         iiss$equipment_subname == "IIR" |
                         iiss$equipment_subname == "ARH" |
                         iiss$equipment_subname == "AAM" |
                         iiss$equipment_subname == "SARH"|
                         iiss$equipment_subname == "AShM") &
                      iiss$unit_name %like any% c("%Apside%", "%Roland%")] <- "air defence"
#stsml>na>mrbm/irbm>na
iiss$equipment_name[iiss$equipment_name == "MRMB"] <- "MRBM"
iiss$equipment_subname[iiss$equipment_name == "MRBM" |
                         iiss$equipment_name == "IRBM"] <- "strategic"
iiss$equipment_subtype[(iiss$equipment_name == "MRBM" |
                          iiss$equipment_name == "IRBM") &
                         iiss$equipment_subname == "strategic"] <- "missiles"
iiss$equipment_type[iiss$equipment_subtype == "missiles" &
                      (iiss$equipment_name == "MRBM" |
                         iiss$equipment_name == "IRBM") &
                      iiss$equipment_subname == "strategic"] <- "ballistic missiles"
#stsml>na>na>na>slbm
iiss$equipment_name[iiss$equipment_type == "surface-to-surface missile launchers" &
                      iiss$unit_name == "SLBM"] <- "SLBM"
iiss$equipment_subtype[iiss$equipment_type == "surface-to-surface missile launchers" &
                         iiss$equipment_name == "SLBM" &
                         iiss$unit_name == "SLBM"] <- "missiles"
iiss$equipment_subname[iiss$equipment_type == "surface-to-surface missile launchers" &
                         iiss$subtype == "missiles" &
                         iiss$equipment_name == "SLBM" &
                         iiss$unit_name == "SLBM"] <- "strategic"
iiss$equipment_type[iiss$equipment_type == "surface-to-surface missile launchers" &
                      iiss$subtype == "missiles" &
                      iiss$equipment_name == "SLBM" &
                      iiss$equipment_subname == "strategic" &
                      iiss$unit_name == "SLBM"] <- "ballistic missiles"
iiss$unit_name[iiss$equipment_type == "ballistic missiles" &
                 iiss$subtype == "missiles" &
                 iiss$equipment_name == "SLBM" &
                 iiss$equipment_subname == "strategic" &
                 iiss$unit_name == "SLBM"] <- NA
# sarh
iiss$equipment_type[iiss$equipment_type == "surface-to-surface missile launchers" &
                      iiss$equipment_subtype == "missiles" &
                      iiss$equipment_name == "SARH"] <- "air defence"
iiss$equipment_subname[iiss$equipment_type == "air defence" &
                         iiss$equipment_subtype == "missiles" &
                         iiss$equipment_name == "SARH"] <- "SARH"
iiss$equipment_name[iiss$equipment_type == "air defence" &
                      iiss$equipment_subtype == "missiles" &
                      iiss$equipment_name == "SARH" &
                      iiss$equipment_subname == "SARH"] <- "AAM"
#stsml>ad
iiss$equipment_subtype[iiss$equipment_type == "surface-to-surface missile launchers" &
                         iiss$equipment_subtype == "AD"] <- "missiles"
#stsml>asm
iiss$equipment_type[iiss$equipment_type == "surface-to-surface missile launchers" &
                      iiss$equipment_name == "ASM"] <- "air-launched missiles"
#stsml>na>bombs/conventional
iiss$equipment_type[iiss$equipment_type == "surface-to-surface missile launchers" &
                      (iiss$equipment_name == "BOMBS" |
                         iiss$equipment_name == "conventional")] <- "bombs"
#stsml>na>icbm/slbm
iiss$equipment_type[iiss$equipment_type == "surface-to-surface missile launchers" &
                      (iiss$equipment_name == "ICBM" |
                         iiss$equipment_name == "SLBM")] <- "ballistic missiles"
#stsml>na>mbt
iiss$equipment_type[iiss$equipment_type == "surface-to-surface missile launchers" &
                      iiss$equipment_name == "MBT"] <- "armoured fighting vehicles"
#stsml>na>asm
iiss$equipment_type[iiss$equipment_type == "surface-to-surface missile launchers" &
                      iiss$equipment_name == "MSL" &
                      iiss$equipment_subname == "ASM"] <- "air-launched missiles"
#stsml>na>arh/ir/sarh
iiss$equipment_type[iiss$equipment_type == "surface-to-surface missile launchers" &
                      iiss$equipment_subtype == "missiles" &
                      is.na(iiss$equipment_name) &
                      (iiss$equipment_subname == "ARH" |
                         iiss$equipment_subname == "IR/SARH")] <- "air defence"
#poland>stsml>missiles>na>na
iiss$equipment_type[iiss$country == "poland" &
                      iiss$equipment_type == "surface-to-surface missile launchers" &
                      iiss$equipment_subtype == "missiles" &
                      is.na(iiss$equipment_name) &
                      is.na(iiss$equipment_subname)] <- "air defence"
#stsml>na>sam>na
iiss$equipment_type[iiss$equipment_type == "surface-to-surface missile launchers" &
                      is.na(iiss$equipment_subtype) &
                      iiss$equipment_name == "SAM"] <- "air defence"
#stsml>na>na>na
iiss$equipment_subtype[iiss$equipment_type == "surface-to-surface missile launchers" &
                         is.na(iiss$equipment_subtype) &
                         is.na(iiss$equipment_name) &
                         is.na(iiss$equipment_subname)] <- "missiles"
iiss$equipment_name[iiss$equipment_type == "surface-to-surface missile launchers" &
                      iiss$equipment_subtype == "missiles" &
                      is.na(iiss$equipment_name) &
                      is.na(iiss$equipment_subname)] <- "SSM"
#ad>tac>ssm
iiss$equipment_type[iiss$equipment_type == "air defence" &
                      iiss$equipment_subtype == "tac" &
                      iiss$equipment_name == "SSM"] <- "surface-to-surface missile launchers"
iiss$equipment_subname[iiss$equipment_type == "surface-to-surface missile launchers" &
                         iiss$equipment_subtype == "tac"] <- "tactical"
iiss$equipment_subtype[iiss$equipment_type == "surface-to-surface missile launchers" &
                         iiss$equipment_subtype == "tac" &
                         iiss$equipment_subname == "tactical"] <- "missiles"
# add missiles
iiss$equipment_subtype[iiss$equipment_type == "surface-to-surface missile launchers" &
                         iiss$equipment_name == "SAM"] <- "missiles"
# AD GUNS
iiss$equipment_name[iiss$equipment_type == "air defence" &
                      iiss$equipment_name == "AD GUNS"] <- "GUNS"
# stsml>SSM>na>na
iiss$equipment_name[iiss$equipment_type == "surface-to-surface missile launchers" &
                      iiss$equipment_subtype == "SSM"] <- "SSM"
iiss$equipment_subtype[iiss$equipment_type == "surface-to-surface missile launchers" &
                      iiss$equipment_subtype == "SSM" &
                        iiss$equipment_name == "SSM"] <- "missiles"
# stsml>na>atgw>na
iiss$equipment_type[iiss$equipment_type == "surface-to-surface missile launchers" &
                      iiss$equipment_name == "ATGW"] <- "anti-tank/anti-infrastructure"
# stsml>na>msl>tactical
iiss$equipment_name[iiss$equipment_type == "surface-to-surface missile launchers" &
                      iiss$equipment_name == "MSL" &
                      iiss$equipment_subname == "tactical"] <- "SSM"
# artillery>na>ashm>na
iiss$equipment_type[iiss$equipment_type == "artillery" &
                      iiss$equipment_name == "AShM"] <- "surface-to-surface missile launchers"
# tactical ssm
iiss$equipment_subname[iiss$equipment_type == "surface-to-surface missile launchers" &
                         iiss$equipment_name == "Tactical SSM"] <- "tactical"
iiss$equipment_name[iiss$equipment_type == "surface-to-surface missile launchers" &
                         iiss$equipment_name == "Tactical SSM" &
                         iiss$equipment_subname == "tactical"] <- "SSM"
# ssm in subname
iiss$equipment_name[iiss$equipment_type == "surface-to-surface missile launchers" &
                      iiss$equipment_subname == "SSM"] <- "SSM"
iiss$equipment_subname[iiss$equipment_type == "surface-to-surface missile launchers" &
                         iiss$equipment_name == "SSM" &
                      iiss$equipment_subname == "SSM"] <- NA
# unknown missiles
iiss$equipment_subtype[iiss$equipment_type == "surface-to-surface missile launchers" &
                         iiss$equipment_name == "MSL"] <- "missiles"
iiss$equipment_name[iiss$equipment_type == "surface-to-surface missile launchers" &
                      iiss$equipment_subtype == "missiles" &
                         iiss$equipment_name == "MSL"] <- NA
# anti-ship missile
iiss$equipment_name[iiss$equipment_type == "anti-ship missile" &
                      iiss$unit_name == "KN-01"] <- "AShM"
iiss$equipment_subtype[iiss$equipment_type == "anti-ship missile" &
                         iiss$equipment_name == "AShM" &
                      iiss$unit_name == "KN-01"] <- "missiles"
iiss$equipment_type[iiss$equipment_type == "anti-ship missile" &
                         iiss$equipment_name == "AShM" &
                      iiss$equipment_subtype == "missiles" &
                         iiss$unit_name == "KN-01"] <- "surface-to-surface missile launchers"
#all subtypes should be missile
iiss$equipment_subtype[iiss$equipment_type == "surface-to-surface missile launchers"] <- "missiles"
#coastal
iiss$equipment_subname[iiss$equipment_type == "surface-to-surface missile launchers" &
                         iiss$equipment_subname == "Coastal"] <- "coastal"
#HEL in name, move to helicopters
iiss$equipment_type[iiss$equipment_type == "surface-to-surface missile launchers" &
                      iiss$equipment_name == "HEL"] <- "helicopters"
iiss$equipment_subtype[iiss$equipment_type == "helicopters" &
                         iiss$equipment_subtype == "missiles"] <- NA
#Coastal regional defence forces
iiss$subservice[iiss$equipment_type == "Coastal regional defence forces"] <- "Coastal regional defence forces"
iiss$equipment_type[iiss$equipment_type == "Coastal regional defence forces" &
                      iiss$equipment_name == "SSM"] <- "surface-to-surface missile launchers"
iiss$equipment_type[iiss$equipment_type == "Coastal regional defence forces" &
                      iiss$equipment_name == "GUNS"] <- "artillery"
# arm in subname
iiss$equipment_name[iiss$equipment_type == "surface-to-surface missile launchers" &
                      iiss$equipment_subname == "ARM"] <- "ARM"
iiss$equipment_subname[iiss$equipment_type == "surface-to-surface missile launchers" &
                      iiss$equipment_name == "ARM" &
                      iiss$equipment_subname == "ARM"] <- NA
#surface to surface missile launcher
iiss$equipment_type[iiss$equipment_type == "surface to surface missile launcher"] <- "surface-to-surface missile launchers"



# Ballistic Missiles
# warheads in equipment_type
iiss$equipment_type[iiss$equipment_type == "warheads"] <- "ballistic missiles"
#missiles>strategic>icbm>liquid-fueled / solid-fueled
iiss$equipment_subname[iiss$equipment_type == "missiles" &
                         (iiss$equipment_subname == "liquid-fueled" |
                            iiss$equipment_subname == "solid-fueled")] <- NA
#bm>na>msl>strategic
iiss$equipment_name[iiss$equipment_type == "ballistic missiles" &
                      iiss$equipment_name == "MSL" &
                      iiss$unit_name == "Jericho 1 SRBM/Jericho 2 IRBM"]
#msl
iiss$equipment_name[iiss$equipment_type == "ballistic missiles" &
                      iiss$equipment_name == "MSL"] <- "IRBM"
#missiles>strategic>irbm/mrbm>na
iiss$equipment_subname[iiss$equipment_type == "missiles" &
                         iiss$equipment_subtype == "strategic" &
                         iiss$equipment_name == "IRBM/MRBM"] <- "strategic"
iiss$equipment_subtype[iiss$equipment_type == "missiles" &
                         iiss$equipment_subtype == "strategic" &
                         iiss$equipment_name == "IRBM/MRBM" &
                         iiss$equipment_subname == "strategic"] <- "missiles"
iiss$equipment_type[iiss$equipment_type == "missiles" &
                         iiss$equipment_subtype == "missiles" &
                         iiss$equipment_name == "IRBM/MRBM" &
                         iiss$equipment_subname == "strategic"] <- "ballistic missiles"




# Air defence

#missiles>sam>na>na
iiss$equipment_name[iiss$equipment_type == "missiles" &
                         iiss$equipment_subtype == "SAM"] <- "SAM"
iiss$equipment_subtype[iiss$equipment_type == "missiles" &
                      iiss$equipment_subtype == "SAM" &
                        iiss$equipment_name == "SAM"] <- "missiles"
iiss$equipment_type[iiss$equipment_type == "missiles" &
                         iiss$equipment_subtype == "SAM" &
                         iiss$equipment_name == "SAM"] <- "air defence"
#missiles>na>sam>na
iiss$equipment_subtype[iiss$equipment_type == "missiles" &
                      iiss$equipment_name == "SAM"] <- "missiles"
iiss$equipment_type[iiss$equipment_type == "missiles" &
                         iiss$equipment_subtype == "missiles" &
                         iiss$equipment_name == "SAM"] <- "air defence"
#missiles>aam>na>na
iiss$equipment_name[iiss$equipment_type == "missiles" &
                      iiss$equipment_subtype == "AAM"] <- "AAM"
iiss$equipment_subtype[iiss$equipment_type == "missiles" &
                         iiss$equipment_subtype == "AAM" &
                         iiss$equipment_name == "AAM"] <- "missiles"
iiss$equipment_type[iiss$equipment_type == "missiles" &
                      iiss$equipment_subtype == "AAM" &
                      iiss$equipment_name == "AAM"] <- "air defence"
# ad>msl>manpad>na
iiss$equipment_subname[iiss$equipment_type == "air defence" &
                         iiss$equipment_subtype == "missiles" &
                         iiss$equipment_name == "MANPAD" &
                         is.na(iiss$equipment_subname)] <- "MANPAD"
iiss$equipment_name[iiss$equipment_type == "air defence" &
                      iiss$equipment_subtype == "missiles" &
                      iiss$equipment_name == "MANPAD" &
                      iiss$equipment_subname == "MANPAD"] <- "SAM"
#ad>na>sam>manpad
iiss$equipment_subtype[iiss$equipment_type == "air defence" &
                         is.na(iiss$equipment_subtype) &
                         iiss$equipment_name == "SAM" &
                         iiss$equipment_subname == "MANPAD"] <- "missiles"
#ad>na>sam>NA
iiss$equipment_subtype[iiss$equipment_type == "air defence" &
                         is.na(iiss$equipment_subtype) &
                         iiss$equipment_name == "SAM" &
                         is.na(iiss$equipment_subname)] <- "missiles"
#ad>na>sam>sp
iiss$equipment_subtype[iiss$equipment_type == "air defence" &
                         is.na(iiss$equipment_subtype) &
                         iiss$equipment_name == "SAM" &
                         iiss$equipment_subname == "SP"] <- "missiles"
#ad>na>sam>towed
iiss$equipment_subtype[iiss$equipment_type == "air defence" &
                         is.na(iiss$equipment_subtype) &
                         iiss$equipment_name == "SAM" &
                         iiss$equipment_subname == "TOWED"] <- "missiles"
#ad>na>sam>static
iiss$equipment_subtype[iiss$equipment_type == "air defence" &
                         is.na(iiss$equipment_subtype) &
                         iiss$equipment_name == "SAM" &
                         iiss$equipment_subname == "STATIC"] <- "missiles"
iiss$equipment_subname[iiss$equipment_type == "air defence" &
                         iiss$equipment_subtype == "missiles" &
                         iiss$equipment_name == "SAM" &
                         iiss$equipment_subname == "STATIC"] <- "STATIC/SHELTER"
#ad>na>sam-manpad>na
iiss$equipment_subname[iiss$equipment_type == "air defence" &
                         is.na(iiss$equipment_subtype) &
                         iiss$equipment_name == "SAM-MANPAD" &
                         is.na(iiss$equipment_subname)] <- "MANPAD"
iiss$equipment_name[iiss$equipment_type == "air defence" &
                      is.na(iiss$equipment_subtype) &
                      iiss$equipment_name == "SAM-MANPAD" &
                      iiss$equipment_subname == "MANPAD"] <- "SAM"
iiss$equipment_subtype[iiss$equipment_type == "air defence" &
                         is.na(iiss$equipment_subtype) &
                         iiss$equipment_name == "SAM" &
                         iiss$equipment_subname == "MANPAD"] <- "missiles"
#ad>na>sam-sp>na
iiss$equipment_subname[iiss$equipment_type == "air defence" &
                         is.na(iiss$equipment_subtype) &
                         iiss$equipment_name == "SAM-SP" &
                         is.na(iiss$equipment_subname)] <- "SP"
iiss$equipment_name[iiss$equipment_type == "air defence" &
                      is.na(iiss$equipment_subtype) &
                      iiss$equipment_name == "SAM-SP" &
                      iiss$equipment_subname == "SP"] <- "SAM"
iiss$equipment_subtype[iiss$equipment_type == "air defence" &
                         is.na(iiss$equipment_subtype) &
                         iiss$equipment_name == "SAM" &
                         iiss$equipment_subname == "SP"] <- "missiles"
#ad>na>sam-towed>long-range
iiss$equipment_subname[iiss$equipment_type == "air defence" &
                         is.na(iiss$equipment_subtype) &
                         iiss$equipment_name == "SAM-TOWED" &
                         (iiss$equipment_subname == "long-range" |
                            iiss$equipment_subname == "long range")] <- "TOWED"
iiss$equipment_name[iiss$equipment_type == "air defence" &
                      is.na(iiss$equipment_subtype) &
                      iiss$equipment_name == "SAM-TOWED" &
                      iiss$equipment_subname == "TOWED"] <- "SAM"
iiss$equipment_subtype[iiss$equipment_type == "air defence" &
                         is.na(iiss$equipment_subtype) &
                         iiss$equipment_name == "SAM" &
                         iiss$equipment_subname == "TOWED"] <- "missiles"
#ad>sam>manpad>na
iiss$equipment_subname[iiss$equipment_type == "air defence" &
                         iiss$equipment_subtype == "SAM" &
                         iiss$equipment_name == "MANPAD" &
                         is.na(iiss$equipment_subname)] <- "MANPAD"
iiss$equipment_name[iiss$equipment_type == "air defence" &
                      iiss$equipment_subtype == "SAM" &
                      iiss$equipment_name == "MANPAD" &
                      iiss$equipment_subname == "MANPAD"] <- "SAM"
iiss$equipment_subtype[iiss$equipment_type == "air defence" &
                         iiss$equipment_subtype == "SAM" &
                         iiss$equipment_name == "SAM" &
                         iiss$equipment_subname == "MANPAD"] <- "missiles"
#ad>sam>manpads>na
iiss$equipment_subname[iiss$equipment_type == "air defence" &
                         iiss$equipment_subtype == "SAM" &
                         iiss$equipment_name == "MANPADS" &
                         is.na(iiss$equipment_subname)] <- "MANPAD"
iiss$equipment_name[iiss$equipment_type == "air defence" &
                      iiss$equipment_subtype == "SAM" &
                      iiss$equipment_name == "MANPADS" &
                      iiss$equipment_subname == "MANPAD"] <- "SAM"
iiss$equipment_subtype[iiss$equipment_type == "air defence" &
                         iiss$equipment_subtype == "SAM" &
                         iiss$equipment_name == "SAM" &
                         iiss$equipment_subname == "MANPAD"] <- "missiles"
#ad>sam>na>na
iiss$equipment_name[iiss$equipment_type == "air defence" &
                      iiss$equipment_subtype == "SAM" &
                      is.na(iiss$equipment_name)  &
                      is.na(iiss$equipment_subname)] <- "SAM"
iiss$equipment_subtype[iiss$equipment_type == "air defence" &
                         iiss$equipment_subtype == "SAM" &
                         iiss$equipment_name == "SAM" &
                         is.na(iiss$equipment_subname)] <- "missiles"
#ad>sp>na>na
iiss$equipment_subname[iiss$equipment_type == "air defence" &
                         iiss$equipment_subtype == "SAM" &
                         iiss$equipment_name == "SP" &
                         is.na(iiss$equipment_subname)] <- "SP"
iiss$equipment_name[iiss$equipment_type == "air defence" &
                      iiss$equipment_subtype == "SAM" &
                      iiss$equipment_name == "SP" &
                      iiss$equipment_subname == "SP"] <- "SAM"
iiss$equipment_subtype[iiss$equipment_type == "air defence" &
                         iiss$equipment_subtype == "SAM" &
                         iiss$equipment_name == "SAM" &
                         iiss$equipment_subname == "SP"] <- "missiles"
#ad>sam>towed>na
iiss$equipment_subname[iiss$equipment_type == "air defence" &
                         iiss$equipment_subtype == "SAM" &
                         iiss$equipment_name == "TOWED" &
                         is.na(iiss$equipment_subname)] <- "TOWED"
iiss$equipment_name[iiss$equipment_type == "air defence" &
                      iiss$equipment_subtype == "SAM" &
                      iiss$equipment_name == "TOWED" &
                      iiss$equipment_subname == "TOWED"] <- "SAM"
iiss$equipment_subtype[iiss$equipment_type == "air defence" &
                         iiss$equipment_subtype == "SAM" &
                         iiss$equipment_name == "SAM" &
                         iiss$equipment_subname == "TOWED"] <- "missiles"
#ad>sam>static>na
iiss$equipment_subname[iiss$equipment_type == "air defence" &
                         iiss$equipment_subtype == "SAM" &
                         iiss$equipment_name == "STATIC" &
                         is.na(iiss$equipment_subname)] <- "STATIC/SHELTER"
iiss$equipment_name[iiss$equipment_type == "air defence" &
                      iiss$equipment_subtype == "SAM" &
                      iiss$equipment_name == "STATIC" &
                      iiss$equipment_subname == "STATIC/SHELTER"] <- "SAM"
iiss$equipment_subtype[iiss$equipment_type == "air defence" &
                         iiss$equipment_subtype == "SAM" &
                         iiss$equipment_name == "SAM" &
                         iiss$equipment_subname == "STATIC/SHELTER"] <- "missiles"
#ad>msl>ssm>na
iiss$equipment_type[iiss$equipment_type == "air defence" &
                      iiss$equipment_subtype == "missiles" &
                      iiss$equipment_name == "SSM" &
                      is.na(iiss$equipment_subname)] <- "surface-to-surface missile launchers"
#ad>msl>sp>na
iiss$equipment_subname[iiss$equipment_type == "air defence" &
                         iiss$equipment_subtype == "missiles" &
                         iiss$equipment_name == "SP" &
                         is.na(iiss$equipment_subname)] <- "SP"
iiss$equipment_name[iiss$equipment_type == "air defence" &
                      iiss$equipment_subtype == "missiles" &
                      iiss$equipment_name == "SP" &
                      iiss$equipment_subname == "SP"] <- "SAM"
#ad>sam>manpats>na
iiss$equipment_subname[iiss$equipment_type == "air defence" &
                         iiss$equipment_subtype == "SAM" &
                         iiss$equipment_name == "MANPATS" &
                         is.na(iiss$equipment_subname)] <- "MANPATS"
iiss$equipment_name[iiss$equipment_type == "air defence" &
                      iiss$equipment_subtype == "SAM" &
                      iiss$equipment_name == "MANPATS" &
                      iiss$equipment_subname == "MANPATS"] <- NA
iiss$equipment_subtype[iiss$equipment_type == "air defence" &
                         iiss$equipment_subtype == "SAM" &
                         is.na(iiss$equipment_name) &
                         iiss$equipment_subname == "MANPATS"] <- "missiles"
iiss$equipment_type[iiss$equipment_type == "air defence" &
                      iiss$equipment_subtype == "missiles" &
                      is.na(iiss$equipment_name) &
                      iiss$equipment_subname == "MANPATS"] <- "anti-tank/anti-infrastructure"
#ad>sam>towed>mim-23
iiss$equipment_subname[iiss$equipment_type == "air defence" &
                         iiss$equipment_subtype == "SAM" &
                         iiss$equipment_name == "TOWED" &
                         iiss$equipment_subname == "MIM-23"] <- "TOWED"
iiss$equipment_name[iiss$equipment_type == "air defence" &
                      iiss$equipment_subtype == "SAM" &
                      iiss$equipment_name == "TOWED" &
                      iiss$equipment_subname == "TOWED"] <- "SAM"
iiss$equipment_subtype[iiss$equipment_type == "air defence" &
                         iiss$equipment_subtype == "SAM" &
                         iiss$equipment_name == "SAM" &
                         iiss$equipment_subname == "TOWED"] <- "missiles"
#ad>sam>sp>na
iiss$equipment_subname[iiss$equipment_type == "air defence" &
                         iiss$equipment_subtype == "SAM" &
                         iiss$equipment_name == "SP" &
                         is.na(iiss$equipment_subname)] <- "SP"
iiss$equipment_name[iiss$equipment_type == "air defence" &
                      iiss$equipment_subtype == "SAM" &
                      iiss$equipment_name == "SP" &
                      iiss$equipment_subname == "SP"] <- "SAM"
iiss$equipment_subtype[iiss$equipment_type == "air defence" &
                         iiss$equipment_subtype == "SAM" &
                         iiss$equipment_name == "SAM" &
                         iiss$equipment_subname == "SP"] <- "missiles"
#ad>sam>static/shelter>na
iiss$equipment_subname[iiss$equipment_type == "air defence" &
                         iiss$equipment_subtype == "SAM" &
                         iiss$equipment_name == "STATIC/SHELTER" &
                         is.na(iiss$equipment_subname)] <- "STATIC/SHELTER"
iiss$equipment_name[iiss$equipment_type == "air defence" &
                      iiss$equipment_subtype == "SAM" &
                      iiss$equipment_name == "STATIC/SHELTER" &
                      iiss$equipment_subname == "STATIC/SHELTER"] <- "SAM"
iiss$equipment_subtype[iiss$equipment_type == "air defence" &
                         iiss$equipment_subtype == "SAM" &
                         iiss$equipment_name == "SAM" &
                         iiss$equipment_subname == "STATIC/SHELTER"] <- "missiles"
#ad>sam>spaagm>na
iiss$equipment_subtype[iiss$equipment_type == "air defence" &
                         iiss$equipment_subtype == "SAM" &
                         iiss$equipment_name == "SPAAGM" &
                         is.na(iiss$equipment_subname)] <- "SPAAGM"
iiss$equipment_name[iiss$equipment_type == "air defence" &
                      iiss$equipment_subtype == "SPAAGM" &
                      iiss$equipment_name == "SPAAGM" &
                      is.na(iiss$equipment_subname)] <- NA
#ad>sam>naval>na
iiss$equipment_subname[iiss$equipment_type == "air defence" &
                         iiss$equipment_subtype == "SAM" &
                         iiss$equipment_name == "NAVAL" &
                         is.na(iiss$equipment_subname)] <- "NAVAL"
iiss$equipment_name[iiss$equipment_type == "air defence" &
                      iiss$equipment_subtype == "SAM" &
                      iiss$equipment_name == "NAVAL" &
                      iiss$equipment_subname == "NAVAL"] <- "SAM"
iiss$equipment_subtype[iiss$equipment_type == "air defence" &
                         iiss$equipment_subtype == "SAM" &
                         iiss$equipment_name == "SAM" &
                         iiss$equipment_subname == "NAVAL"] <- "missiles"
#ad>sp>na>20mm
iiss$equipment_name[iiss$equipment_type == "air defence" &
                      iiss$equipment_subtype == "SP" &
                      is.na(iiss$equipment_name) &
                      iiss$equipment_subname == "20mm"] <- "SP"
iiss$equipment_subtype[iiss$equipment_type == "air defence" &
                         iiss$equipment_subtype == "SP" &
                         iiss$equipment_name == "SP" &
                         iiss$equipment_subname == "20mm"] <- "GUNS"
#ad>sp>na>40mm
iiss$equipment_name[iiss$equipment_type == "air defence" &
                      iiss$equipment_subtype == "SP" &
                      is.na(iiss$equipment_name) &
                      iiss$equipment_subname == "40mm"] <- "SP"
iiss$equipment_subtype[iiss$equipment_type == "air defence" &
                         iiss$equipment_subtype == "SP" &
                         iiss$equipment_name == "SP" &
                         iiss$equipment_subname == "40mm"] <- "GUNS"
#ad>sp>na>na
iiss$equipment_name[iiss$equipment_type == "air defence" &
                      iiss$equipment_subtype == "SP" &
                      is.na(iiss$equipment_name) &
                      is.na(iiss$equipment_subname)] <- "SAM"
iiss$equipment_subtype[iiss$equipment_type == "air defence" &
                         iiss$equipment_subtype == "SP" &
                         iiss$equipment_name == "SAM" &
                         is.na(iiss$equipment_subname)] <- "missiles"
#ad>manpad>na>na
iiss$equipment_subname[iiss$equipment_type == "air defence" &
                         iiss$equipment_subtype == "MANPAD" &
                         is.na(iiss$equipment_name) &
                         is.na(iiss$equipment_subname)] <- "MANPAD"
iiss$equipment_name[iiss$equipment_type == "air defence" &
                      iiss$equipment_subtype == "MANPAD" &
                      is.na(iiss$equipment_name) &
                      iiss$equipment_subname == "MANPAD"] <- "SAM"
iiss$equipment_subtype[iiss$equipment_type == "air defence" &
                         iiss$equipment_subtype == "MANPAD" &
                         iiss$equipment_name == "SAM" &
                         iiss$equipment_subname == "MANPAD"] <- "missiles"
#ad>na>na>na
iiss$equipment_subtype[iiss$equipment_type == "air defence" &
                         is.na(iiss$equipment_subtype) &
                         is.na(iiss$equipment_name) &
                         is.na(iiss$equipment_subname)] <- "missiles"
#ad>na>sam>manpd
iiss$equipment_subname[iiss$equipment_type == "air defence" &
                         is.na(iiss$equipment_subtype) &
                         iiss$equipment_name == "SAM" &
                         iiss$equipment_subname == "MANPD"] <- "MANPAD"
iiss$equipment_subtype[iiss$equipment_type == "air defence" &
                         is.na(iiss$equipment_subtype) &
                         iiss$equipment_name == "SAM" &
                         iiss$equipment_subname == "MANPAD"] <- "missiles"
iiss$equipment_subname[iiss$equipment_subname == "MANPD"] <- "MANPAD"
#ad>na>sam-towed>na
iiss$equipment_subname[iiss$equipment_type == "air defence" &
                         is.na(iiss$equipment_subtype) &
                         iiss$equipment_name == "SAM-TOWED" &
                         is.na(iiss$equipment_subname)] <- "TOWED"
iiss$equipment_name[iiss$equipment_type == "air defence" &
                      is.na(iiss$equipment_subtype) &
                      iiss$equipment_name == "SAM-TOWED" &
                      iiss$equipment_subname == "TOWED"] <- "SAM"
iiss$equipment_subtype[iiss$equipment_type == "air defence" &
                         is.na(iiss$equipment_subtype) &
                         iiss$equipment_name == "SAM" &
                         iiss$equipment_subname == "TOWED"] <- "missiles"
#ad>na>sp>na
iiss$equipment_subname[iiss$equipment_type == "air defence" &
                         is.na(iiss$equipment_subtype) &
                         iiss$equipment_name == "SP" &
                         is.na(iiss$equipment_subname)] <- "SP"
iiss$equipment_name[iiss$equipment_type == "air defence" &
                      is.na(iiss$equipment_subtype) &
                      iiss$equipment_name == "SP" &
                      iiss$equipment_subname == "SP"] <- "SAM"
iiss$equipment_subtype[iiss$equipment_type == "air defence" &
                         is.na(iiss$equipment_subtype) &
                         iiss$equipment_name == "SAM" &
                         iiss$equipment_subname == "SP"] <- "missiles"
#ad>na>sp sam>na
iiss$equipment_subname[iiss$equipment_type == "air defence" &
                         is.na(iiss$equipment_subtype) &
                         iiss$equipment_name == "SP SAM" &
                         is.na(iiss$equipment_subname)] <- "SP"
iiss$equipment_name[iiss$equipment_type == "air defence" &
                      is.na(iiss$equipment_subtype) &
                      iiss$equipment_name == "SP SAM" &
                      iiss$equipment_subname == "SP"] <- "SAM"
iiss$equipment_subtype[iiss$equipment_type == "air defence" &
                         is.na(iiss$equipment_subtype) &
                         iiss$equipment_name == "SAM" &
                         iiss$equipment_subname == "SP"] <- "missiles"
#ad>na>spaagm>na
iiss$equipment_subtype[iiss$equipment_type == "air defence" &
                         is.na(iiss$equipment_subtype) &
                         iiss$equipment_name == "SPAAGM" &
                         is.na(iiss$equipment_subname)] <- "SPAAGM"
iiss$equipment_name[iiss$equipment_type == "air defence" &
                      iiss$equipment_subtype == "SPAAGM" &
                      iiss$equipment_name == "SPAAGM" &
                      is.na(iiss$equipment_subname)] <- NA
#ad>na>manpad>na
iiss$equipment_subname[iiss$equipment_type == "air defence" &
                         is.na(iiss$equipment_subtype) &
                         iiss$equipment_name == "MANPAD" &
                         is.na(iiss$equipment_subname)] <- "MANPAD"
iiss$equipment_name[iiss$equipment_type == "air defence" &
                      is.na(iiss$equipment_subtype) &
                      iiss$equipment_name == "MANPAD" &
                      iiss$equipment_subname == "MANPAD"] <- "SAM"
iiss$equipment_subtype[iiss$equipment_type == "air defence" &
                         is.na(iiss$equipment_subtype) &
                         iiss$equipment_name == "SAM" &
                         iiss$equipment_subname == "MANPAD"] <- "missiles"
#ad>na>sam>static/shelter
iiss$equipment_subtype[iiss$equipment_type == "air defence" &
                         is.na(iiss$equipment_subtype) &
                         iiss$equipment_name == "SAM" &
                         iiss$equipment_subname == "STATIC/SHELTER"] <- "missiles"
#ad>na>sam-static>na
iiss$equipment_subname[iiss$equipment_type == "air defence" &
                         is.na(iiss$equipment_subtype) &
                         iiss$equipment_name == "SAM-STATIC" &
                         is.na(iiss$equipment_subname)] <- "STATIC/SHELTER"
iiss$equipment_name[iiss$equipment_type == "air defence" &
                      is.na(iiss$equipment_subtype) &
                      iiss$equipment_name == "SAM-STATIC" &
                      iiss$equipment_subname == "STATIC/SHELTER"] <- "SAM"
iiss$equipment_subtype[iiss$equipment_type == "air defence" &
                         is.na(iiss$equipment_subtype) &
                         iiss$equipment_name == "SAM" &
                         iiss$equipment_subname == "STATIC/SHELTER"] <- "missile"
#ad>na>static>na
iiss$equipment_subname[iiss$equipment_type == "air defence" &
                         is.na(iiss$equipment_subtype) &
                         iiss$equipment_name == "STATIC" &
                         is.na(iiss$equipment_subname)] <- "STATIC/SHELTER"
iiss$equipment_name[iiss$equipment_type == "air defence" &
                      is.na(iiss$equipment_subtype) &
                      iiss$equipment_name == "STATIC" &
                      iiss$equipment_subname == "STATIC/SHELTER"] <- "SAM"
iiss$equipment_subtype[iiss$equipment_type == "air defence" &
                         is.na(iiss$equipment_subtype) &
                         iiss$equipment_name == "SAM" &
                         iiss$equipment_subname == "STATIC/SHELTER"] <- "missiles"
#ad>na>towed>na
iiss$equipment_subname[iiss$equipment_type == "air defence" &
                         is.na(iiss$equipment_subtype) &
                         iiss$equipment_name == "TOWED" &
                         is.na(iiss$equipment_subname)] <- "TOWED"
iiss$equipment_name[iiss$equipment_type == "air defence" &
                      is.na(iiss$equipment_subtype) &
                      iiss$equipment_name == "TOWED" &
                      iiss$equipment_subname == "TOWED"] <- "SAM"
iiss$equipment_subtype[iiss$equipment_type == "air defence" &
                         is.na(iiss$equipment_subtype) &
                         iiss$equipment_name == "SAM" &
                         iiss$equipment_subname == "TOWED"] <- "missiles"
#ad>abm>na>na
iiss$equipment_subname[iiss$equipment_type == "air defence" &
                         iiss$equipment_subtype == "ABM" &
                         is.na(iiss$equipment_name) &
                         is.na(iiss$equipment_subname)] <- "ABM"
iiss$equipment_name[iiss$equipment_type == "air defence" &
                      iiss$equipment_subtype == "ABM" &
                      is.na(iiss$equipment_name) &
                      iiss$equipment_subname == "ABM"] <- "SAM"
iiss$equipment_subtype[iiss$equipment_type == "air defence" &
                         iiss$equipment_subtype == "ABM" &
                         iiss$equipment_name == "SAM" &
                         iiss$equipment_subname == "ABM"] <- "missiles"
#ad>na>abm>na
iiss$equipment_subname[iiss$equipment_type == "air defence" &
                         is.na(iiss$equipment_subtype) &
                         iiss$equipment_name == "ABM" &
                         is.na(iiss$equipment_subname)] <- "ABM"
iiss$equipment_name[iiss$equipment_type == "air defence" &
                      is.na(iiss$equipment_subtype) &
                      iiss$equipment_name  == "ABM" &
                      iiss$equipment_subname == "ABM"] <- "SAM"
iiss$equipment_subtype[iiss$equipment_type == "air defence" &
                         is.na(iiss$equipment_subtype) &
                         iiss$equipment_name == "SAM" &
                         iiss$equipment_subname == "ABM"] <- "missiles"
#ad>ad>na>na
iiss$equipment_name[iiss$equipment_type == "air defence" &
                      iiss$equipment_subtype == "AD" &
                      is.na(iiss$equipment_name) &
                      is.na(iiss$equipment_subname)] <- "SAM"
iiss$equipment_subtype[iiss$equipment_type == "air defence" &
                         iiss$equipment_subtype == "AD" &
                         iiss$equipment_name == "SAM" &
                         is.na(iiss$equipment_subname)] <- "missiles"
#ad>na>sam>long-range
iiss$equipment_subname[iiss$equipment_type == "air defence" &
                         is.na(iiss$equipment_subtype) &
                         iiss$equipment_name == "SAM" &
                         iiss$equipment_subname == "long-range"] <- NA
iiss$equipment_subtype[iiss$equipment_type == "air defence" &
                         is.na(iiss$equipment_subtype) &
                         iiss$equipment_name == "SAM" &
                         is.na(iiss$equipment_subname)] <- "missiles"
#ad>long-range>sam-sp>na
iiss$equipment_subname[iiss$equipment_type == "air defence" &
                         iiss$equipment_subtype == "long-range" &
                         iiss$equipment_name == "SAM-SP" &
                         is.na(iiss$equipment_subname)] <- "SP"
iiss$equipment_name[iiss$equipment_type == "air defence" &
                      iiss$equipment_subtype == "long-range" &
                      iiss$equipment_name == "SAM-SP" &
                      iiss$equipment_subname == "SP"] <- "SAM"
iiss$equipment_subtype[iiss$equipment_type == "air defence" &
                         iiss$equipment_subtype == "long-range" &
                         iiss$equipment_name == "SAM" &
                         iiss$equipment_subname == "SP"] <- "missiles"
#ad>na>sam>short-range
iiss$equipment_subname[iiss$equipment_type == "air defence" &
                         is.na(iiss$equipment_subtype) &
                         iiss$equipment_name == "SAM" &
                         iiss$equipment_subname == "short-range"] <- NA
iiss$equipment_subtype[iiss$equipment_type == "air defence" &
                         is.na(iiss$equipment_subtype) &
                         iiss$equipment_name == "SAM" &
                         is.na(iiss$equipment_subname)] <- "missiles"
#ad>na>sam-manpad>point-defence
iiss$equipment_subname[iiss$equipment_type == "air defence" &
                         is.na(iiss$equipment_subtype) &
                         iiss$equipment_name %in% c("SAM-MANPAD", "MANPAD") &
                         iiss$equipment_subname == "point-defence"] <- "MANPAD"
iiss$equipment_name[iiss$equipment_type == "air defence" &
                      is.na(iiss$equipment_subtype) &
                      iiss$equipment_name == "SAM-MANPAD" &
                      iiss$equipment_subname == "MANPAD"] <- "SAM"
iiss$equipment_subtype[iiss$equipment_type == "air defence" &
                         is.na(iiss$equipment_subtype) &
                         iiss$equipment_name == "SAM" &
                         iiss$equipment_subname == "MANPAD"] <- "missiles"
#ad>na>sam-sp>point-defence
iiss$equipment_subname[iiss$equipment_type == "air defence" &
                         is.na(iiss$equipment_subtype) &
                         iiss$equipment_name == "SAM-SP" &
                         iiss$equipment_subname == "point-defence"] <- "SP"
iiss$equipment_name[iiss$equipment_type == "air defence" &
                      is.na(iiss$equipment_subtype) &
                      iiss$equipment_name == "SAM-SP" &
                      iiss$equipment_subname == "SP"] <- "SAM"
iiss$equipment_subtype[iiss$equipment_type == "air defence" &
                         is.na(iiss$equipment_subtype) &
                         iiss$equipment_name == "SAM" &
                         iiss$equipment_subname == "SP"] <- "missiles"
#ad>na>sam>point-defence
iiss$equipment_subname[iiss$equipment_type == "air defence" &
                         is.na(iiss$equipment_subtype) &
                         iiss$equipment_name == "SAM" &
                         iiss$equipment_subname == "point-defence"] <- NA
iiss$equipment_subtype[iiss$equipment_type == "air defence" &
                         is.na(iiss$equipment_subtype) &
                         iiss$equipment_name == "SAM" &
                         is.na(iiss$equipment_subname)] <- "missiles"
#ad>na>towed sam>na
iiss$equipment_subname[iiss$equipment_type == "air defence" &
                         is.na(iiss$equipment_subtype) &
                         iiss$equipment_name == "TOWED SAM" &
                         is.na(iiss$equipment_subname)] <- "TOWED"
iiss$equipment_name[iiss$equipment_type == "air defence" &
                      is.na(iiss$equipment_subtype) &
                      iiss$equipment_name == "TOWED SAM" &
                      iiss$equipment_subname == "TOWED"] <- "SAM"
iiss$equipment_subtype[iiss$equipment_type == "air defence" &
                         is.na(iiss$equipment_subtype) &
                         iiss$equipment_name == "SAM" &
                         iiss$equipment_subname == "TOWED"] <- "missiles"
#ad>na>sam towed>na
iiss$equipment_subname[iiss$equipment_type == "air defence" &
                         is.na(iiss$equipment_subtype) &
                         iiss$equipment_name == "SAM TOWED" &
                         is.na(iiss$equipment_subname)] <- "TOWED"
iiss$equipment_name[iiss$equipment_type == "air defence" &
                      is.na(iiss$equipment_subtype) &
                      iiss$equipment_name == "SAM TOWED" &
                      iiss$equipment_subname == "TOWED"] <- "SAM"
iiss$equipment_subtype[iiss$equipment_type == "air defence" &
                         is.na(iiss$equipment_subtype) &
                         iiss$equipment_name == "SAM" &
                         iiss$equipment_subname == "TOWED"] <- "missiles"
#ad>na>manpads>na
iiss$equipment_subname[iiss$equipment_type == "air defence" &
                         is.na(iiss$equipment_subtype) &
                         iiss$equipment_name == "MANPADS" &
                         is.na(iiss$equipment_subname)] <- "MANPAD"
iiss$equipment_name[iiss$equipment_type == "air defence" &
                      is.na(iiss$equipment_subtype) &
                      iiss$equipment_name == "MANPADS" &
                      iiss$equipment_subname == "MANPAD"] <- "SAM"
iiss$equipment_subtype[iiss$equipment_type == "air defence" &
                         is.na(iiss$equipment_subtype) &
                         iiss$equipment_name == "SAM" &
                         iiss$equipment_subname == "MANPAD"] <- "missiles"
# rader/raders
iiss$equipment_name[iiss$equipment_name == "RADER" |
                      iiss$equipment_name == "RADERS" ] <- "RADARS"
#ad>na>sam>radar
iiss$equipment_subtype[iiss$equipment_type == "air defence" &
                         is.na(iiss$equipment_subtype) &
                         iiss$equipment_name == "SAM" &
                         iiss$equipment_subname == "RADAR"] <- "missiles"
#ad>na>na>radar
iiss$equipment_name[iiss$equipment_type == "air defence" &
                      is.na(iiss$equipment_subtype) &
                      is.na(iiss$equipment_name) &
                      iiss$equipment_subname == "RADAR"] <- "SAM"
iiss$equipment_subtype[iiss$equipment_type == "air defence" &
                         is.na(iiss$equipment_subtype) &
                         iiss$equipment_name == "SAM" &
                         iiss$equipment_subname == "RADAR"] <- "missiles"
#ad>lr>samsp>na
iiss$equipment_subname[iiss$equipment_type == "air defence" &
                         iiss$equipment_subtype == "long-range" &
                         iiss$equipment_name == "SAM-SP" &
                         is.na(iiss$equipment_subname)] <- "SP"
iiss$equipment_name[iiss$equipment_type == "air defence" &
                      iiss$equipment_subtype == "long-range" &
                      iiss$equipment_name == "SAM-SP" &
                      iiss$equipment_subname == "SP"] <- "SAM"
iiss$equipment_subtype[iiss$equipment_type == "air defence" &
                         iiss$equipment_subtype == "long-range" &
                         iiss$equipment_name == "SAM" &
                         iiss$equipment_subname == "SP"] <- "missiles"
#ad>na>na>na
iiss$equipment_subname[iiss$equipment_type == "air defence" &
                         is.na(iiss$equipment_subtype) &
                         is.na(iiss$equipment_name) &
                         is.na(iiss$equipment_subname)] <- "SP"
iiss$equipment_name[iiss$equipment_type == "air defence" &
                      is.na(iiss$equipment_subtype) &
                      is.na(iiss$equipment_name) &
                      iiss$equipment_subname == "SP"] <- "SAM"
iiss$equipment_subtype[iiss$equipment_type == "air defence" &
                         is.na(iiss$equipment_subtype) &
                         iiss$equipment_name == "SAM" &
                         iiss$equipment_subname == "SP"] <- "missiles"
#missiles>strategic>icbm>na
iiss$equipment_subname[iiss$equipment_type == "missiles" &
                         iiss$equipment_subtype == "strategic" &
                         iiss$equipment_name == "ICBM" &
                         is.na(iiss$equipment_subname)] <- "strategic"
iiss$equipment_subtype[iiss$equipment_type == "missiles" &
                         iiss$equipment_subtype == "strategic" &
                         iiss$equipment_name == "ICBM" &
                         iiss$equipment_subname == "strategic"] <- "missiles"
iiss$equipment_type[iiss$equipment_type == "missiles" &
                      iiss$equipment_subtype == "missiles" &
                      iiss$equipment_name == "ICBM" &
                      iiss$equipment_subname == "strategic"] <- "ballistic missiles"
#ad>land>gbi>na
iiss$equipment_subname[iiss$equipment_type == "air defence" &
                         iiss$equipment_subtype == "land" &
                         iiss$equipment_name == "ground-based interceptors" &
                         is.na(iiss$equipment_subname)] <- "ground-based interceptors"
iiss$equipment_name[iiss$equipment_type == "air defence" &
                      iiss$equipment_subtype == "land" &
                      iiss$equipment_name == "ground-based interceptors" &
                      is.na(iiss$equipment_subname)] <- "SAM"
iiss$equipment_subtype[iiss$equipment_type == "strategic defenses - missile defenses" &
                         iiss$equipment_subtype == "land" &
                         iiss$equipment_name == "SAM" &
                         iiss$equipment_subname == "ground-based interceptor"] <- "missile"
#bm>ad
iiss$equipment_subtype[iiss$equipment_type == "ballistic missiles" &
                         iiss$equipment_subtype == "AD"] <- "missiles"
# RL
iiss$equipment_type[iiss$equipment_type == "air defence" &
                      iiss$equipment_subtype == "RL"] <- "anti-tank/anti-infrastructure"
# cleaning
iiss$equipment_name[iiss$equpment_type == "air defence" &
                      iiss$equipment_subtype == "missiles" &
                      is.na(iiss$equipment_name) &
                      iiss$unit_name %like% "SA%"] <- "SAM"
# misc
iiss$equipment_name[iiss$equipment_type == "air defence" &
                      iiss$equipment_subtype == "missiles" &
                      iiss$equipment_name == "AAM" &
                      iiss$equipment_subname == "AShM"] <- "AShM"
iiss$equipment_subname[iiss$equipment_type == "air defence" &
                         iiss$equipment_subtype == "missiles" &
                         iiss$equipment_name == "AShM" &
                         iiss$equipment_subname == "AShM"] <- NA
iiss$equipment_subname[iiss$equipment_type == "air defence" &
                         iiss$equipment_subtype == "missiles" &
                         iiss$equipment_name == "SAM" &
                         iiss$equipment_subname == "NAVAL"] <- NA
#manpad>""
iiss$equipment_subname[iiss$equipment_type == "air defence" &
                         iiss$equipment_subtype == "missiles" &
                         iiss$equipment_name == "MANPAD"] <- "MANPAD"
iiss$equipment_name[iiss$equipment_type == "air defence" &
                      iiss$equipment_subtype == "missiles" &
                      iiss$equipment_name == "MANPAD"] <- "SAM"
#misc
iiss$equipment_subname[iiss$equipment_type == "air defence" &
                         iiss$equipment_name == "ARM"] <- "ARM"
iiss$equipment_name[iiss$equipment_type == "air defence" &
                      iiss$equipment_name == "ARM" &
                      iiss$equipment_subname == "ARM"] <- "AAM"
#ad>civil defence>AC/NA
iiss$equipment_type[iiss$equipment_type == "air defence" &
                      iiss$equipment_subtype == "civil defence"] <- "aircraft"
#ad>coastal defence>na>pci
iiss$equipment_type[iiss$equipment_type == "air defence" &
                      iiss$equipment_subtype == "coastal defence" &
                      iiss$equipment_subname == "PCI"] <- "patrol and coastal combatants"
# GUN
iiss$equipment_subtype[iiss$equipment_subtype == "GUN"] <- "GUNS"
# IN STORE
iiss$unit_count[iiss$equipment_subtype == "IN STORE"] <- 0
iiss$equipment_subtype[iiss$equipment_subtype == "IN STORE"] <- NA
# THADD
iiss$unit_name[iiss$unit_name == "THADD"] <- "THAAD"
iiss$equipment_subname[iiss$unit_name == "THAAD"] <- "TOWED"
iiss$equipment_name[iiss$unit_name == "THAAD"] <- "SAM"
iiss$equipment_subtype[iiss$unit_name == "THAAD"] <- "missiles"
#ad>medium-range
iiss$equipment_subname[iiss$unit_name == "MIM-104F Patriot PAC-3/PAC-3 MSE"] <- "medium-range"
iiss$equipment_name[iiss$unit_name == "MIM-104F Patriot PAC-3/PAC-3 MSE"] <- "SAM"
iiss$equipment_subtype[iiss$unit_name == "MIM-104F Patriot PAC-3/PAC-3 MSE"] <- "missiles"
#ad>na>sam>short range
iiss$equipment_subname[iiss$unit_name == "NASAMS"] <- "short-range"
iiss$equipment_subtype[iiss$unit_name == "NASAMS"] <- "missiles"
#ad>na>sam>long range
iiss$equipment_subname[iiss$unit_name == "MIM-104D/E Patriot PAC-2 GEM/GEM-T"] <- "long-range"
iiss$equipment_subtype[iiss$unit_name == "MIM-104D/E Patriot PAC-2 GEM/GEM-T"] <- "missiles"
#missile
iiss$equipment_subtype[iiss$equipment_subtype == "missile"] <- "missiles"
#ad>tac>ssm>na
iiss$equipment_type[iiss$equipment_type == "air defence" &
                      iiss$equipment_subtype == "tac" &
                      iiss$equipment_name == "SSM"] <- "surface-to-surface missile launchers"
#AIM
iiss$equipment_name[iiss$equipment_name == "AIM"] <- "AAM"
#MISSILES
iiss$equipment_subtype[iiss$equipment_name == "MISSILES"] <- "missiles"
iiss$equipment_name[iiss$equipment_name == "MISSILES"] <- NA
#SP SAM
iiss$equipment_subname[iiss$equipment_type == "air defence" &
                         iiss$equipment_subtype == "missiles" &
                         iiss$equipment_name == "SP SAM"] <- "SP"
iiss$equipment_name[iiss$equipment_type == "air defence" &
                         iiss$equipment_subtype == "missiles" &
                         iiss$equipment_name == "SP SAM" &
                         iiss$equipment_subname == "SP"] <- "SAM"
#ad>na>sbs>ig1 and s2
iiss$equipment_subtype[iiss$equipment_name == "SPACE BASED SYSTEMS" &
                       iiss$equipment_subname == "Imagery 1"] <- "space based systems"
iiss$equipment_name[iiss$equipment_subtype == "space based systems" &
                         iiss$equipment_name == "SPACE BASED SYSTEMS" &
                         iiss$equipment_subname == "Imagery 1"] <- "Imagery 1"
iiss$equipment_name[iiss$equipment_subtype == "space based systems" &
                      iiss$equipment_name == "Imagery 1" &
                      iiss$equipment_subname == "Imagery 1"] <- NA

iiss$equipment_subtype[iiss$equipment_name == "SPACE BASED SYSTEMS" &
                         iiss$equipment_subname == "Satellites 2"] <- "space based systems"
iiss$equipment_name[iiss$equipment_subtype == "space based systems" &
                      iiss$equipment_name == "SPACE BASED SYSTEMS" &
                      iiss$equipment_subname == "Satellites 2"] <- "Satellites 2"
iiss$equipment_name[iiss$equipment_subtype == "space based systems" &
                      iiss$equipment_name == "Satellites 2" &
                      iiss$equipment_subname == "Satellites 2"] <- NA

iiss$equipment_name[iiss$equipment_subname == "Imagery 1"] <- "Imagery 1"
iiss$equipment_subname[iiss$equipment_name == "Imagery 1"] <- NA

iiss$equipment_name[iiss$equipment_subname == "Satellites 2"] <- "Satellites 2"
iiss$equipment_subname[iiss$equipment_name == "Satellites 2"] <- NA
# tactical in subtype
iiss$equipment_subname[iiss$equipment_type == "air defence" &
                         iiss$equipment_subtype == "tactical"] <- "tactical"
iiss$equipment_subtype[iiss$equipment_type == "air defence" &
                         iiss$equipment_subtype == "tactical" &
                         iiss$equipment_subname == "tactical"] <- NA
# MSL>AAM
iiss$equipment_subtype[iiss$equipment_type == "air-launched missiles" &
                        iiss$equipment_name == "MSL" &
                        iiss$equipment_subname == "AAM"] <- "missiles"
iiss$equipment_name[iiss$equipment_type == "air-launched missiles" &
                        iiss$equipment_subtype == "missiles" &
                        iiss$equipment_name == "MSL" &
                        iiss$equipment_subname == "AAM"] <- "AAM"
iiss$equipment_subname[iiss$equipment_type == "air-launched missiles" &
                      iiss$equipment_subtype == "missiles" &
                      iiss$equipment_name == "AAM" &
                      iiss$equipment_subname == "AAM"] <- NA
#coastal defence
iiss$subservice[iiss$equipment_type == "air defence" &
               iiss$equipment_subtype == "coastal defence"] <- "coastal defence"
iiss$equipment_subtype[iiss$equipment_type == "air defence" &
                         iiss$subservice == "coastal defence" &
                  iiss$equipment_subtype == "coastal defence"] <- NA
#ad>sam>ad>manpad or na
iiss$equipment_name[iiss$equipment_type == "air defence" &
                      iiss$equipment_subtype == "SAM" &
                      iiss$equipment_name == "AD"] <- "SAM"
iiss$equipment_subtype[iiss$equipment_type == "air defence" &
                      iiss$equipment_subtype == "SAM" &
                      iiss$equipment_name == "SAM"] <- "missiles"
#ssm>tactical
iiss$equipment_type[iiss$equipment_type == "air defence" &
                      iiss$equipment_name == "SSM" &
                      iiss$equipment_subtype == "tactical"] <- "surface-to-surface missile launchers"
# atk in name
iiss$equipment_type[iiss$equipment_type == "air defence" &
                      iiss$equipment_name == "ATK"] <- "anti-tank/anti-infrastructure"
# ssm in name
iiss$equipment_type[iiss$equipment_type == "air defence" &
                      iiss$equipment_name == "SSM" &
                      iiss$equipment_subname == "tactical"] <- "surface-to-surface missile launchers"
#9K35Strela-10 and 9K32Strela-2 in subname
iiss$equipment_subname[iiss$equipment_subname == "9K35Strela-10" |
                         iiss$equipment_subname == "9K32Strela-2"] <- NA
#air wing in equipment_subtype
iiss$subsubservice[iiss$equipment_type == "air defence" &
                     iiss$equipment_subtype == "AIR WING"] <- "air wing"
iiss$equipment_subtype[iiss$equipment_type == "air defence" &
                     iiss$equipment_subtype == "AIR WING"] <- NA
# other cleaning
iiss$equipment_subname <- gsub(" ", "", iiss$equipment_subname, fixed=TRUE)
iiss$equipment_subname[iiss$equipment_subname == "35mmand85mmand100mm"] <- "35mm/85mm/100mm"
iiss$equipment_subname[iiss$equipment_subname == "MANDPD"] <- "MANPAD"
# launcher
iiss$equipment_subname[iiss$equipment_type == "air defence" &
                      iiss$equipment_name == "LAUNCHER"] <- "MANPAD"
iiss$equipment_name[iiss$equipment_type == "air defence" &
                         iiss$equipment_name == "LAUNCHER" &
                      iiss$equipment_subname == "MANPAD"] <- "SAM"
# surv in subtype
iiss$equipment_type[iiss$equipment_type == "air defence" &
                      iiss$equipment_subtype == "surv"] <- "radars"
# COERCE RADAR-RELATED AIR DEFENCE TO equipment_tpye RADARS
iiss$equipment_type[iiss$equipment_type == "air defence" &
                      (iiss$equipment_name == "CONTROL" |
                         iiss$equipment_name == "SYSTEMS" |
                         iiss$equipment_name == "RADARS")] <- "radars"
iiss$equipment_type[iiss$equipment_type == "air defence" &
                      iiss$equipment_subname == "RADARS"] <- "radars"
#People's regional force in equipment_type
iiss$subservice[iiss$equipment_type == "people's regional force"] <- "people's regional force"
iiss$equipment_type[iiss$equipment_type == "people's regional force"] <- "radars"
#PCO in equipment_name
iiss$equipment_subname[iiss$equipment_type == "air defence" &
                      iiss$equipment_name == "PCO" &
                      iiss$equipment_subname == "76mm"] <- NA
iiss$equipment_type[iiss$equipment_type == "air defence" &
                      iiss$equipment_name == "PCO"] <- "patrol and coastal combatants"
#TRG in equipment_name
iiss$equipment_type[iiss$equipment_type == "air defence" &
                      iiss$equipment_name == "TRG"] <- "aircraft"
#asm in subtype
iiss$equipment_name[iiss$equipment_type == "air defence" &
                      iiss$equipment_subtype == "ASM"] <- "ASM"
iiss$equipment_subtype[iiss$equipment_type == "air defence" &
                      iiss$equipment_subtype == "ASM" &
                        iiss$equipment_name == "ASM"] <- NA
#AA guns
iiss$equipment_subtype[iiss$equipment_type == "air defence" &
                         (iiss$equipment_name == "AA GUNS" |
                            iiss$equipment_name == "AD arty")] <- "GUNS"
iiss$equipment_name[iiss$equipment_type == "air defence" &
                         iiss$equipment_subtype == "GUNS" &
                         (iiss$equipment_name == "AA GUNS" |
                            iiss$equipment_name == "AD arty")] <- NA
#ssm in equipment name
iiss$equipment_type[iiss$equipment_type == "air defence" &
                      iiss$equipment_name == "SSM"] <- "surface-to-surface missile launchers"
#HEL
iiss$equipment_type[iiss$equipment_type == "air defence" &
                      (iiss$equipment_name == "HEL" |
                         iiss$equipment_name == "SPT")] <- "helicopters"
#ATF
iiss$equipment_type[iiss$equipment_type == "air defence" &
                      iiss$equipment_name == "ATF"] <- "logistics and support"
#SP
iiss$equipment_name[iiss$equipment_type == "air defence" &
                      iiss$equipment_subtype == "missiles" &
                      iiss$equipment_name == "SP"] <- "SAM"
# PCI in subname
iiss$equipment_type[iiss$equipment_type == "air defence" &
                      iiss$equipment_subname == "PCI"] <- "patrol and coastal combatants"
iiss$equipment_name[iiss$equipment_type == "patrol and coastal combatants" &
                      iiss$equipment_subname == "PCI"] <- "PCI"
iiss$equipment_name[iiss$equipment_type == "patrol and coastal combatants" &
                      iiss$equipment_name == "PCI" &
                      iiss$equipment_subname == "PCI"] <- NA
#arm in subname
iiss$equipment_name[iiss$equipment_type == "air defence" &
                      iiss$equipment_subname == "ARM"] <- NA
#mco
iiss$equipment_name[iiss$equipment_type == "air defence" &
                      iiss$equipment_name == "MCO"] <- "SAM"
#air-defence
iiss$equipment_type[iiss$equipment_type == "air-defence"] <- "air defence"
#amraam
iiss$unit_name[iiss$equipment_type == "air defence" &
                 iiss$equipment_name == "AMRAAM"] <- "AMRAAM"
iiss$equipment_name[iiss$equipment_type == "air defence" &
                 iiss$equipment_name == "AMRAAM"] <- "AAM"
#ftr
iiss$equipment_type[iiss$equipment_type == "air defence" &
                      iiss$equipment_name == "FTR"] <- "aircraft"
#sidewinder
iiss$unit_name[iiss$equipment_type == "air defence" &
                 iiss$equipment_name == "Sidewinder"] <- "Sidewinder"
iiss$equipment_name[iiss$equipment_type == "air defence" &
                 iiss$equipment_name == "Sidewinder"] <- "AAM"
#sparrow
iiss$unit_name[iiss$equipment_type == "air defence" &
                 iiss$equipment_name == "Sparrow"] <- "Sidewinder"
iiss$equipment_name[iiss$equipment_type == "air defence" &
                      iiss$equipment_name == "Sparrow"] <- "AAM"
# corvettes in subtype
iiss$equipment_subtype[iiss$equipment_type == "air defence" &
                          iiss$equipment_subtype == "corvettes" &
                          (iiss$unit_name == "Barak VLS" |
                             iiss$unit_name == "Selenia")] <- "missiles"





# Anti-tank/anti-infrastructure

#gun>AT>RL>67mm
iiss$equipment_subtype[iiss$equipment_type == "gun" &
                         iiss$equipment_name == "RL"] <- "RL"
iiss$equipment_name[iiss$equipment_type == "gun" &
                      iiss$equipment_subtype == "RL" &
                         iiss$equipment_name == "RL"] <- NA
iiss$equipment_type[iiss$equipment_type == "gun" &
                      iiss$equipment_subtype == "RL" &
                      is.na(iiss$equipment_name)] <- "anti-tank/anti-infrastructure"
#atai>msl>manpats>na
iiss$equipment_subname[iiss$equipment_type == "anti-tank/anti-infrastructure" &
                         iiss$equipment_subtype == "missiles" &
                         iiss$equipment_name == "MANPATS" &
                         is.na(iiss$equipment_subname)] <- "MANPATS"
iiss$equipment_name[iiss$equipment_type == "anti-tank/anti-infrastructure" &
                      iiss$equipment_subtype == "missiles" &
                      iiss$equipment_name == "MANPATS" &
                      iiss$equipment_subname == "MANPATS"] <- NA
#atai>msl>mlr>na
iiss$equipment_subname[iiss$equipment_type == "anti-tank/anti-infrastructure" &
                         iiss$equipment_subtype == "missiles" &
                         iiss$equipment_name == "MLR" &
                         is.na(iiss$equipment_subname)] <- "MLR"
iiss$equipment_name[iiss$equipment_type == "anti-tank/anti-infrastructure" &
                      iiss$equipment_subtype == "missiles" &
                      iiss$equipment_name == "MLR" &
                      iiss$equipment_subname == "MLR"] <- NA
#atai>msl>sp>na
iiss$equipment_subname[iiss$equipment_type == "anti-tank/anti-infrastructure" &
                         iiss$equipment_subtype == "missiles" &
                         iiss$equipment_name == "SP" &
                         is.na(iiss$equipment_subname)] <- "SP"
iiss$equipment_name[iiss$equipment_type == "anti-tank/anti-infrastructure" &
                      iiss$equipment_subtype == "missiles" &
                      iiss$equipment_name == "SP" &
                      iiss$equipment_subname == "SP"] <- NA
#atai>msl>tow>na
iiss$equipment_name[iiss$equipment_type == "anti-tank/anti-infrastructure" &
                         iiss$equipment_subtype == "missiles" &
                         iiss$equipment_name == "TOW" &
                         is.na(iiss$equipment_subname)] <- "TOWED"
iiss$equipment_name[iiss$equipment_type == "anti-tank/anti-infrastructure" &
                      iiss$equipment_subtype == "missiles" &
                      iiss$equipment_name == "TOWED" &
                      iiss$equipment_subname == "TOWED"] <- NA
#atai>msl>towed>na
iiss$equipment_subname[iiss$equipment_type == "anti-tank/anti-infrastructure" &
                         iiss$equipment_subtype == "missiles" &
                         iiss$equipment_name == "TOWED" &
                         is.na(iiss$equipment_subname)] <- "TOWED"
iiss$equipment_name[iiss$equipment_type == "anti-tank/anti-infrastructure" &
                      iiss$equipment_subtype == "missiles" &
                      iiss$equipment_name == "TOWED" &
                      iiss$equipment_subname == "TOWED"] <- NA
#atai>na>msl>manpats
iiss$equipment_name[iiss$equipment_type == "anti-tank/anti-infrastructure" &
                      is.na(iiss$equipment_subtype) &
                      iiss$equipment_name == "MSL" &
                      iiss$equipment_subname == "MANPATS"] <- NA
iiss$equipment_subtype[iiss$equipment_type == "anti-tank/anti-infrastructure" &
                         is.na(iiss$equipment_subtype) &
                         is.na(iiss$equipment_name) &
                         iiss$equipment_subname == "MANPATS"] <- "missiles"
#atai>na>msl>sp
iiss$equipment_name[iiss$equipment_type == "anti-tank/anti-infrastructure" &
                      is.na(iiss$equipment_subtype) &
                      iiss$equipment_name == "MSL" &
                      iiss$equipment_subname == "SP"] <- NA
iiss$equipment_subtype[iiss$equipment_type == "anti-tank/anti-infrastructure" &
                         is.na(iiss$equipment_subtype) &
                         is.na(iiss$equipment_name) &
                         iiss$equipment_subname == "SP"] <- "missiles"
#atai>na>sam-manpad>na
iiss$equipment_subname[iiss$equipment_type == "anti-tank/anti-infrastructure" &
                         is.na(iiss$equipment_subtype) &
                         iiss$equipment_name == "SAM-MANPAD" &
                         is.na(iiss$equipment_subname)] <- "MANPAD"
iiss$equipment_name[iiss$equipment_type == "anti-tank/anti-infrastructure" &
                      is.na(iiss$equipment_subtype) &
                      iiss$equipment_name == "SAM-MANPAD" &
                      iiss$equipment_subname == "MANPAD"] <- "SAM"
iiss$equipment_subtype[iiss$equipment_type == "anti-tank/anti-infrastructure" &
                         is.na(iiss$equipment_subtype) &
                         iiss$equipment_name == "SAM" &
                         iiss$equipment_subname == "MANPAD"] <- "missiles"
iiss$equipment_type[iiss$equipment_type == "anti-tank/anti-infrastructure" &
                      iiss$equipment_subtype == "missiles" &
                      iiss$equipment_name == "SAM" &
                      iiss$equipment_subname == "MANPAD"] <- "air defence"
#atai>sam>na>na
iiss$equipment_name[iiss$equipment_type == "anti-tank/anti-infrastructure" &
                      iiss$equipment_subtype == "SAM" &
                      is.na(iiss$equipment_name) &
                      is.na(iiss$equipment_subname)] <- "SAM"
iiss$equipment_subtype[iiss$equipment_type == "anti-tank/anti-infrastructure" &
                         iiss$equipment_subtype == "SAM" &
                         iiss$equipment_name == "SAM" &
                         is.na(iiss$equipment_subname)] <- "missiles"
iiss$equipment_type[iiss$equipment_type == "anti-tank/anti-infrastructure" &
                      iiss$equipment_subtype == "missiles" &
                      iiss$equipment_name == "SAM" &
                      is.na(iiss$equipment_subname)] <- "air defence"
#atai>msl>strategic>na
iiss$equipment_subname[iiss$equipment_type == "anti-tank/anti-infrastructure" &
                         iiss$equipment_subtype == "missiles" &
                         iiss$equipment_name == "STRATEGIC" &
                         is.na(iiss$equipment_subname)] <- "strategic"
iiss$equipment_name[iiss$equipment_type == "anti-tank/anti-infrastructure" &
                      iiss$equipment_subtype == "missiles" &
                      iiss$equipment_name == "STRATEGIC" &
                      iiss$equipment_subname == "strategic"] <- "ICBM"
iiss$equipment_type[iiss$equipment_type == "anti-tank/anti-infrastructure" &
                      iiss$equipment_subtype == "missiles" &
                      iiss$equipment_name == "ICBM" &
                      iiss$equipment_subname == "strategic"] <- "ballistic missiles"
#atai>msl>manpat/manpats/rcl/rl/rl94mm
iiss$equipment_subtype[iiss$equipment_type == "anti-tank/anti-infrastructure" &
                         iiss$equipment_subtype == "missiles" &
                         (iiss$equipment_name == "MANPAT" |
                            iiss$equipment_name == "MANPATS" |
                            iiss$equipment_name == "RCL" |
                            iiss$equipment_name == "RL" |
                            iiss$equipment_name == "RL 94mm" |
                            iiss$equipment_name == "MANPAT")] <- NA
#atai>na>msl>na
iiss$equipment_name[iiss$equipment_type == "anti-tank/anti-infrastructure" &
                      is.na(iiss$equipment_subtype) &
                      iiss$equipment_name == "MSL" &
                      is.na(iiss$equipment_subname)] <- "ATGW"
#atai>na>sam-sp>na
iiss$equipment_subname[iiss$equipment_type == "anti-tank/anti-infrastructure" &
                         is.na(iiss$equipment_subtype) &
                         (iiss$equipment_name == "SAM-SP" |
                            iiss$equipment_name == "SP") &
                         is.na(iiss$equipment_subname)] <- "SP"
iiss$equipment_name[iiss$equipment_type == "anti-tank/anti-infrastructure" &
                      is.na(iiss$equipment_subtype) &
                      (iiss$equipment_name == "SAM-SP" |
                         iiss$equipment_name == "SP") &
                      iiss$equipment_subname == "SP"] <- "SAM"
iiss$equipment_subtype[iiss$equipment_type == "anti-tank/anti-infrastructure" &
                         is.na(iiss$equipment_subtype) &
                         iiss$equipment_name == "SAM" &
                         iiss$equipment_subname == "SP"] <- "missiles"
iiss$equipment_type[iiss$equipment_type == "anti-tank/anti-infrastructure" &
                      iiss$equipment_subtype == "missiles" &
                      iiss$equipment_name == "SAM" &
                      iiss$equipment_subname == "SP"] <- "air defence"
#atai>na>sam>na
iiss$equipment_subtype[iiss$equipment_type == "anti-tank/anti-infrastructure" &
                         is.na(iiss$equipment_subtype) &
                         iiss$equipment_name == "SAM" &
                         is.na(iiss$equipment_subname)] <- "missiles"
iiss$equipment_type[iiss$equipment_type == "anti-tank/anti-infrastructure" &
                      iiss$equipment_subtype == "missiles" &
                      iiss$equipment_name == "SAM" &
                      is.na(iiss$equipment_subname)] <- "air defence"
#atai>na>manpad>na
iiss$equipment_subtype[iiss$equipment_type == "anti-tank/anti-infrastructure" &
                         is.na(iiss$equipment_subtype) &
                         iiss$equipment_name == "MANPAD" &
                         is.na(iiss$equipment_subname)] <- "missiles"
iiss$equipment_type[iiss$equipment_type == "anti-tank/anti-infrastructure" &
                      iiss$equipment_subtype == "missiles" &
                      iiss$equipment_name == "MANPAD" &
                      is.na(iiss$equipment_subname)] <- "air defence"
#atai>na>manpat/manpats
iiss$equipment_subname[iiss$equipment_type == "anti-tank/anti-infrastructure" &
                         is.na(iiss$equipment_subtype) &
                         (iiss$equipment_name == "MANPAT" |
                            iiss$equipment_name == "MANPATS" |
                            iiss$equipment_name == "MANPADS")] <- "MANPATS"
iiss$equipment_name[iiss$equipment_type == "anti-tank/anti-infrastructure" &
                      is.na(iiss$equipment_subtype) &
                      (iiss$equipment_name == "MANPAT" |
                         iiss$equipment_name == "MANPATS" |
                         iiss$equipment_name == "MANPADS") &
                      iiss$equipment_subname == "MANPATS"] <- NA
iiss$equipment_subtype[iiss$equipment_type == "anti-tank/anti-infrastructure" &
                         is.na(iiss$equipment_subtype) &
                         is.na(iiss$equipment_name) &
                         iiss$equipment_subname == "MANPATS"] <- "missiles"
# misc
iiss$equipment_subtype[iiss$equipment_type == "anti-tank/anti-infrastructure" &
                         iiss$equipment_name == "RCL"] <- "RCL"
iiss$equipment_name[iiss$equipment_type == "anti-tank/anti-infrastructure" &
                      iiss$equipment_subtype == "RCL" &
                      iiss$equipment_name == "RCL"] <- NA
#rl
iiss$equipment_subtype[iiss$equipment_type == "anti-tank/anti-infrastructure" &
                         iiss$equipment_name == "RL"] <- "RL"
iiss$equipment_name[iiss$equipment_type == "anti-tank/anti-infrastructure" &
                      iiss$equipment_subtype == "RL" &
                      iiss$equipment_name == "RL"] <- NA
#rcl
iiss$equipment_subtype[iiss$equipment_type == "artillery" &
                         is.na(iiss$equipment_subtype) &
                         iiss$equipment_name == "RCL"] <- "RCL"
iiss$equipment_name[iiss$equipment_type == "artillery" &
                      iiss$equipment_subtype == "RCL" &
                      iiss$equipment_name == "RCL"] <- NA
#mlr
iiss$equipment_name[iiss$equipment_type == "anti-tank/anti-infrastructure" &
                      iiss$equipment_subtype == "missiles" &
                      is.na(iiss$equipment_name) &
                      iiss$equipment_subname == "MLR"] <- "MLR"
iiss$equipment_subname[iiss$equipment_type == "anti-tank/anti-infrastructure" &
                         iiss$equipment_subtype == "missiles" &
                         iiss$equipment_name == "MLR" &
                         iiss$equipment_subname == "MLR"] <- NA
#atgw
iiss$equipment_name[iiss$equipment_type == "anti-tank/anti-infrastructure" &
                      iiss$equipment_subtype == "ATGW"] <- "ATGW"
iiss$equipment_subtype[iiss$equipment_type == "anti-tank/anti-infrastructure" &
                      iiss$equipment_subtype == "ATGW" &
                    iiss$equipment_name == "ATGW"] <- NA
#ad>guns
iiss$equipment_subtype[iiss$equipment_type == "anti-tank/anti-infrastructure" &
                         iiss$equipment_subtype == "AD"] <- "GUNS"
iiss$equipment_name[iiss$equipment_type == "anti-tank/anti-infrastructure" &
                      iiss$equipment_subtype == "GUNS" &
                      iiss$equipment_name == "GUNS"] <- NA
#atai>atk>guns
iiss$equipment_subtype[iiss$equipment_type == "anti-tank/anti-infrastructure" &
                         iiss$equipment_subtype == "ATK"] <- "GUNS"
iiss$equipment_subtype[iiss$equipment_type == "anti-tank/anti-infrastructure" &
                         iiss$equipment_subtype == "GUNS" &
                         iiss$equipment_name == "GUNS"] <- NA
#atai>guns>atk
iiss$equipment_name[iiss$equipment_type == "anti-tank/anti-infrastructure" &
                         iiss$equipment_subtype == "GUNS" &
                         iiss$equipment_name == "ATK"] <- "SP"
#atai>na>rl94mm>na
iiss$equipment_subtype[iiss$equipment_type == "anti-tank/anti-infrastructure" &
                         is.na(iiss$equipment_subtype) &
                         iiss$equipment_name == "RL 94mm"] <- "RL"
iiss$equipment_subname[iiss$equipment_type == "anti-tank/anti-infrastructure" &
                         iiss$equipment_subtype == "RL" &
                         iiss$equipment_name == "RL 94mm"] <- "94mm"
iiss$equipment_name[iiss$equipment_type == "anti-tank/anti-infrastructure" &
                         iiss$equipment_subtype == "RL" &
                         iiss$equipment_name == "RL 94mm"] <- NA
#atai>na>ssm>na
iiss$equipment_type[iiss$equipment_type == "anti-tank/anti-infrastructure" &
                      iiss$equipment_name == "SSM"] <- "surface-to-surface missile launchers"
#atai>na>aslt gun>na
iiss$equipment_subtype[iiss$equipment_type == "anti-tank/anti-infrastructure" &
                         iiss$equipment_name == "ASLT GUN"] <- "GUNS"
iiss$equipment_name[iiss$equipment_type == "anti-tank/anti-infrastructure" &
                         iiss$equipment_subtype == "GUNS" &
                         iiss$equipment_name == "ASLT GUN"] <- "SP"
iiss$equipment_name[iiss$equipment_type == "anti-tank/anti-infrastructure" &
                      iiss$equipment_subtype == "GUNS" &
                      iiss$equipment_name == "ATK GUNS"] <- NA
#atai>na>msl>tow
iiss$equipment_subtype[iiss$equipment_type == "anti-tank/anti-infrastructure" &
                         is.na(iiss$equipment_subtype) &
                         iiss$equipment_name == "MSL" &
                         iiss$equipment_subname == "TOW"] <- "missiles"
iiss$equipment_name[iiss$equipment_type == "anti-tank/anti-infrastructure" &
                         iiss$equipment_subtype == "missiles" &
                         iiss$equipment_name == "MSL" &
                         iiss$equipment_subname == "TOW"] <- "TOWED"
iiss$equipment_subname[iiss$equipment_type == "anti-tank/anti-infrastructure" &
                      iiss$equipment_subtype == "missiles" &
                      iiss$equipment_name == "TOWED" &
                      iiss$equipment_subname == "TOW"] <- NA
iiss$equipment_name[iiss$equipment_type == "anti-tank/anti-infrastructure" &
                      iiss$equipment_name == "ATGW" &
                      iiss$equipment_subname == "TOW"] <- "TOWED"
iiss$equipment_subname[iiss$equipment_type == "anti-tank/anti-infrastructure" &
                      iiss$equipment_name == "TOWED" &
                      iiss$equipment_subname == "TOW"] <- NA
#manpats
iiss$equipment_subname[iiss$equipment_type == "anti-tank/anti-infrastructure" &
                         iiss$equipment_name == "MANPATS"] <- "MANPATS"
iiss$equipment_name[iiss$equipment_type == "anti-tank/anti-infrastructure" &
                         iiss$equipment_name == "MANPATS" &
                      iiss$equipment_subname == "MANPATS"] <- NA
#atk guns in eqpt subtype
iiss$equipment_subtype[iiss$equipment_type == "anti-tank/anti-infrastructure" &
                         iiss$equipment_subtype == "ATK GUNS"] <- "GUNS"
#atk guns in eqpt name
iiss$equipment_subtype[iiss$equipment_type == "anti-tank/anti-infrastructure" &
                         iiss$equipment_name == "ATK GUNS"] <- "GUNS"
iiss$equipment_name[iiss$equipment_type == "anti-tank/anti-infrastructure" &
                         iiss$equipment_subtype == "GUNS" &
                         iiss$equipment_name == "ATK GUNS"] <- NA

#gun in subname
iiss$equipment_subname[iiss$equipment_type == "anti-tank/anti-infrastructure" &
                         iiss$equipment_subname == "GUN"] <- NA
# pco in equipment_name
iiss$equipment_subname[iiss$equipment_type == "air defence" &
                         iiss$equipment_name == "PCO" &
                         iiss$equipment_subname == "76mm"] <- NA
iiss$equipment_type[iiss$equipment_type == "air defence" &
                         iiss$equipment_name == "PCO"] <- "patrol and coastal combatants"



# Air-launched missiles
#alm>aam
iiss$equipment_name[iiss$equipment_type == "air-launched missiles" &
                         iiss$equipment_subtype == "AAM"] <- "AAM"
iiss$equipment_subtype[iiss$equipment_type == "air-launched missiles" &
                      iiss$equipment_subtype == "AAM" &
                        iiss$equipment_name == "AAM"] <- "missiles"
#msl in equipment_type
iiss$equipment_type[iiss$equipment_type == "msl" &
                      iiss$equipment_subtype == "AAM"] <- "air-launched missiles"
#missiles>na>asm>na
iiss$equipment_subtype[iiss$equipment_type == "missiles" &
                         is.na(iiss$equipment_subtype) &
                         (iiss$equipment_name == "ASM" |
                            iiss$equipment_name == "ASM/ALCM")] <- "missiles"
iiss$equipment_type[iiss$equipment_type == "missiles" &
                         iiss$equipment_subtype == "missiles" &
                         (iiss$equipment_name == "ASM" |
                            iiss$equipment_name == "ASM/ALCM")] <- "air-launched missiles"
#missiles>na>aam>na
iiss$equipment_subtype[iiss$equipment_type == "missiles" &
                         is.na(iiss$equipment_subtype) &
                         iiss$equipment_name == "AAM"] <- "missiles"
iiss$equipment_type[iiss$equipment_type == "missiles" &
                         iiss$equipment_subtype == "missiles" &
                         iiss$equipment_name == "AAM"] <- "air-launched missiles"
#alm>na>aam>arh
iiss$equipment_subtype[iiss$equipment_type == "air-launched missiles" &
                         is.na(iiss$equipment_subtype) &
                         iiss$equipment_name == "AAM" &
                         iiss$equipment_subname == "ARH"] <- "missiles"
#alm>na>aam>iir
iiss$equipment_subname[iiss$equipment_type == "air-launched missiles" &
                         is.na(iiss$equipment_subtype) &
                         iiss$equipment_name == "AAM" &
                         iiss$equipment_subname == "IIR"] <- "ARH/IIR"
iiss$equipment_subtype[iiss$equipment_type == "air-launched missiles" &
                         is.na(iiss$equipment_subtype) &
                         iiss$equipment_name == "AAM" &
                         iiss$equipment_subname == "ARH/IIR"] <- "missiles"
#alm>na>aam>ir
iiss$equipment_subtype[iiss$equipment_type == "air-launched missiles" &
                         is.na(iiss$equipment_subtype) &
                         iiss$equipment_name == "AAM" &
                         iiss$equipment_subname == "IR"] <- "missiles"
#alm>na>arm>na
iiss$equipment_subtype[iiss$equipment_type == "air-launched missiles" &
                         is.na(iiss$equipment_subtype) &
                         iiss$equipment_name == "ARM" &
                         is.na(iiss$equipment_subname)] <- "missiles"
#alm>na>ashm>na
iiss$equipment_subtype[iiss$equipment_type == "air-launched missiles" &
                         is.na(iiss$equipment_subtype) &
                         iiss$equipment_name == "AShM" &
                         is.na(iiss$equipment_subname)] <- "missiles"
#alm>na>asm>na
iiss$equipment_subtype[iiss$equipment_type == "air-launched missiles" &
                         is.na(iiss$equipment_subtype) &
                         iiss$equipment_name == "ASM" &
                         is.na(iiss$equipment_subname)] <- "missiles"
#alm>na>asm>tactical
iiss$equipment_subname[iiss$equipment_type == "air-launched missiles" &
                         is.na(iiss$equipment_subtype) &
                         iiss$equipment_name == "ASM" &
                         iiss$equipment_subname == "Tactical"] <- "tactical"
iiss$equipment_subtype[iiss$equipment_type == "air-launched missiles" &
                         is.na(iiss$equipment_subtype) &
                         iiss$equipment_name == "ASM" &
                         iiss$equipment_subname == "tactical"] <- "missiles"
#alm>na>aam>asm
iiss$equipment_subname[iiss$equipment_type == "air-launched missiles" &
                         is.na(iiss$equipment_subtype) &
                         iiss$equipment_name == "AAM" &
                         iiss$equipment_subname == "ASM"] <- NA
iiss$equipment_name[iiss$equipment_type == "air-launched missiles" &
                      is.na(iiss$equipment_subtype) &
                      iiss$equipment_name == "AAM" &
                      is.na(iiss$equipment_subname)] <- "ASM"
iiss$equipment_subtype[iiss$equipment_type == "air-launched missiles" &
                         is.na(iiss$equipment_subtype) &
                         iiss$equipment_name == "ASM" &
                         is.na(iiss$equipment_subname)] <- "missiles"
#alm>na>aam>na
iiss$equipment_subtype[iiss$equipment_type == "air-launched missiles" &
                         is.na(iiss$equipment_subtype) &
                         iiss$equipment_name == "AAM" &
                         is.na(iiss$equipment_subname)] <- "missiles"
#alm>na>aam>sarh
iiss$equipment_subtype[iiss$equipment_type == "air-launched missiles" &
                         is.na(iiss$equipment_subtype) &
                         iiss$equipment_name == "AAM" &
                         iiss$equipment_subname == "SARH"] <- "missiles"
#alm>na>alcm>nuclear
iiss$equipment_name[iiss$equipment_type == "air-launched missiles" &
                      is.na(iiss$equipment_subtype) &
                      iiss$equipment_name == "ALCM" &
                      (iiss$equipment_subname == "nuclear" | iiss$equipment_subname == "Nuclear")] <- "LACM"
iiss$equipment_subtype[iiss$equipment_type == "air-launched missiles" &
                         is.na(iiss$equipment_subtype) &
                         iiss$equipment_name == "LACM" &
                         (iiss$equipment_subname == "nuclear" | iiss$equipment_subname == "Nuclear")] <- "missiles"
#alm>na>alcm>conventional
iiss$equipment_name[iiss$equipment_type == "air-launched missiles" &
                      is.na(iiss$equipment_subtype) &
                      iiss$equipment_name == "ALCM" &
                      iiss$equipment_subname == "conventional"] <- "LACM"
iiss$equipment_subtype[iiss$equipment_type == "air-launched missiles" &
                         is.na(iiss$equipment_subtype) &
                         iiss$equipment_name == "LACM" &
                         iiss$equipment_subname == "conventional"] <- "missiles"
#alm>na>alcm>tactical
iiss$equipment_name[iiss$equipment_type == "air-launched missiles" &
                      is.na(iiss$equipment_subtype) &
                      iiss$equipment_name == "ALCM" &
                      (iiss$equipment_subname == "tactical" | iiss$equipment_subname == "Tactical")] <- "LACM"
iiss$equipment_subtype[iiss$equipment_type == "air-launched missiles" &
                         is.na(iiss$equipment_subtype) &
                         iiss$equipment_name == "LACM" &
                         (iiss$equipment_subname == "tactical" | iiss$equipment_subname == "Tactical")] <- "missiles"
#alm>na>arh>na
iiss$equipment_subname[iiss$equipment_type == "air-launched missiles" &
                         is.na(iiss$equipment_subtype) &
                         iiss$equipment_name == "ARH" &
                         is.na(iiss$equipment_subname)] <- "ARH"
iiss$equipment_name[iiss$equipment_type == "air-launched missiles" &
                      is.na(iiss$equipment_subtype) &
                      iiss$equipment_name == "ARH" &
                      iiss$equipment_subname == "ARH"] <- "AAM"
iiss$equipment_subtype[iiss$equipment_type == "air-launched missiles" &
                         is.na(iiss$equipment_subtype) &
                         iiss$equipment_name == "AAM" &
                         iiss$equipment_subname == "ARH"] <- "missiles"
#alm>na>asm>strategic
iiss$equipment_name[iiss$equipment_type == "air-launched missiles" &
                      is.na(iiss$equipment_subtype) &
                      iiss$equipment_name == "ASM" &
                      (iiss$equipment_subname == "strategic" | iiss$equipment_subname == "Strategic")] <- "LACM"
iiss$equipment_subtype[iiss$equipment_type == "air-launched missiles" &
                         is.na(iiss$equipment_subtype) &
                         iiss$equipment_name == "LACM" &
                         (iiss$equipment_subname == "strategic" | iiss$equipment_subname == "Strategic")] <- "missiles"
#alm>na>ew>na
iiss$equipment_subtype[iiss$equipment_type == "air-launched missiles" &
                         is.na(iiss$equipment_subtype) &
                         iiss$equipment_name == "EW" &
                         is.na(iiss$equipment_subname)] <- "missiles"
#alm>na>iir>na
iiss$equipment_subname[iiss$equipment_type == "air-launched missiles" &
                         is.na(iiss$equipment_subtype) &
                         iiss$equipment_name == "IIR" &
                         is.na(iiss$equipment_subname)] <- "ARH/IIR"
iiss$equipment_name[iiss$equipment_type == "air-launched missiles" &
                      is.na(iiss$equipment_subtype) &
                      iiss$equipment_name == "IIR" &
                      iiss$equipment_subname == "ARH/IIR"] <- "AAM"
iiss$equipment_subtype[iiss$equipment_type == "air-launched missiles" &
                         is.na(iiss$equipment_subtype) &
                         iiss$equipment_name == "AAM" &
                         iiss$equipment_subname == "ARH/IIR"] <- "missiles"
#alm>na>lacm>na
iiss$equipment_subtype[iiss$equipment_type == "air-launched missiles" &
                         is.na(iiss$equipment_subtype) &
                         iiss$equipment_name == "LACM" &
                         is.na(iiss$equipment_subname)] <- "missiles"
#alm>na>manpad>na
iiss$equipment_name[iiss$equipment_type == "air-launched missiles" &
                      is.na(iiss$equipment_subtype) &
                      iiss$equipment_name == "MANPAD" &
                      is.na(iiss$equipment_subname)] <- "AAM"
iiss$equipment_subtype[iiss$equipment_type == "air-launched missiles" &
                         is.na(iiss$equipment_subtype) &
                         iiss$equipment_name == "AAM" &
                         is.na(iiss$equipment_subname)] <- "missiles"
#alm>na>sarh>na
iiss$equipment_subname[iiss$equipment_type == "air-launched missiles" &
                         is.na(iiss$equipment_subtype) &
                         iiss$equipment_name == "SARH" &
                         is.na(iiss$equipment_subname)] <- "SARH"
iiss$equipment_name[iiss$equipment_type == "air-launched missiles" &
                      is.na(iiss$equipment_subtype) &
                      iiss$equipment_name == "SARH" &
                      iiss$equipment_subname == "SARH"] <- "AAM"
iiss$equipment_subtype[iiss$equipment_type == "air-launched missiles" &
                         is.na(iiss$equipment_subtype) &
                         iiss$equipment_name == "AAM" &
                         iiss$equipment_subname == "SARH"] <- "missiles"
#alm>na>na>nuclear
iiss$equipment_name[iiss$equipment_type == "air-launched missiles" &
                      is.na(iiss$equipment_subtype) &
                      is.na(iiss$equipment_name) &
                      iiss$equipment_subname == "nuclear"] <- "LACM"
iiss$equipment_subtype[iiss$equipment_type == "air-launched missiles" &
                         is.na(iiss$equipment_subtype) &
                         iiss$equipment_name == "LACM" &
                         iiss$equipment_subname == "nuclear"] <- "missiles"
#alm>alm
iiss$equipment_subtype[iiss$equipment_type == "air-launched missiles" &
                         iiss$equipment_subtype == "air-launched missiles"] <- "missiles"
#misc
iiss$equipment_subname[iiss$equipment_subname == "IIR"] <- "IIR"
iiss$equipment_subname[iiss$equipment_name == "IIR"] <- "IIR"
iiss$equipment_name[iiss$equipment_subname == "IIR"] <- "AAM"
iiss$equipment_subname[iiss$equipment_name == "IR"] <- "IR"
iiss$equipment_name[iiss$equipment_subname == "IR"] <- "AAM"

iiss$equipment_subname[iiss$equipment_name == "ARH"] <- "ARH"
iiss$equipment_name[iiss$equipment_name == "ARH"] <- "AAM"

iiss$equipment_subname[iiss$equipment_name == "IR/SARH"] <- "IR/SARH"
iiss$equipment_name[iiss$equipment_name == "IR/SARH" &
                      iiss$equipment_subname == "IR/SARH"] <- "AAM"

iiss$equipment_subname[iiss$equipment_name == "SARH"] <- "SARH"
iiss$equipment_name[iiss$equipment_name == "SARH" &
                      iiss$equipment_subname == "SARH"] <- "AAM"
#alm>ad
iiss$equipment_subtype[iiss$equipment_type == "air-launched missiles" &
                         iiss$equipment_subtype == "AD"] <- "missiles"
#alm>na>msl>asm
iiss$equipment_subtype[iiss$equipment_type == "air-launched missiles" &
                         is.na(iiss$equipment_subtype) &
                         iiss$equipment_name == "MSL" &
                         iiss$equipment_subname == "ASM"] <- "missiles"
iiss$equipment_name[iiss$equipment_type == "air-launched missiles" &
                      iiss$equipment_subtype == "missiles" &
                      iiss$equipment_name == "MSL" &
                      iiss$equipment_subname == "ASM"] <- "ASM"
iiss$equipment_subname[iiss$equipment_type == "air-launched missiles" &
                         iiss$equipment_subtype == "missiles" &
                         iiss$equipment_name == "ASM" &
                         iiss$equipment_subname == "ASM"] <- NA
#OR
iiss$equipment_subname[iiss$equipment_subname == "OR"] <- "IR"
#Tactical-AAM
iiss$equipment_subname[iiss$equipment_subname == "Tactical-AAM"] <- "tactical"
#add missing missiles
iiss$equipment_subtype[iiss$equipment_type == "air-launched missiles" &
                         is.na(iiss$equipment_subtype) &
                         (iiss$equipment_name == "AAM" |
                            iiss$equipment_name == "ASMP")] <- "missiles"
#MANPAD
iiss$equipment_type[iiss$equipment_type == "air-launched missiles" &
                      iiss$equipment_name == "MANPAD"] <- "air defence"
#move ashm
iiss$equipment_name[iiss$equipment_type == "air-launched missiles" &
                      iiss$equipment_subname == "AShM"] <- "AShM"
iiss$equipment_subname[iiss$equipment_type == "air-launched missiles" &
                         iiss$equipment_name == "AShM" &
                      iiss$equipment_subname == "AShM"] <- NA
#move asm
iiss$equipment_name[iiss$equipment_type == "air-launched missiles" &
                      iiss$equipment_subname == "ASM"] <- "ASM"
iiss$equipment_subname[iiss$equipment_type == "air-launched missiles" &
                         iiss$equipment_name == "ASM" &
                         iiss$equipment_subname == "ASM"] <- NA
#remove tactical
iiss$equipment_subtype[iiss$equipment_type == "air-launched missiles" &
                      iiss$equipment_subtype == "tactical"] <- NA
#sam
iiss$equipment_type[iiss$equipment_type == "air-launched missiles" &
                      iiss$equipment_name == "SAM"] <- "air defence"
# air-to-air missiles in equipment_name
iiss$equipment_type[iiss$equipment_type == "air-to-air missiles"] <- "air-launched missiles"





# GUNS

iiss$equipment_name[iiss$equipment_type == "air defence" &
                      is.na(iiss$equipment_subtype) &
                      iiss$equipment_name == "GUNS-TOWED"] <- "TOWED"
iiss$equipment_subtype[iiss$equipment_type == "air defence" &
                         is.na(iiss$equipment_subtype) &
                         iiss$equipment_name == "TOWED"] <- "GUNS"
# ad>na>guns-sp
iiss$equipment_name[iiss$equipment_type == "air defence" &
                      is.na(iiss$equipment_subtype) &
                      iiss$equipment_name == "GUNS-SP"] <- "SP"
iiss$equipment_subtype[iiss$equipment_type == "air defence" &
                         is.na(iiss$equipment_subtype) &
                         iiss$equipment_name == "SP"] <- "GUNS"
# ad>na>guns
iiss$equipment_subtype[iiss$equipment_type == "air defence" &
                         is.na(iiss$equipment_subtype) &
                         iiss$equipment_name == "GUNS"] <- "GUNS"
iiss$equipment_name[iiss$equipment_type == "air defence" &
                      iiss$equipment_subtype == "GUNS" &
                      iiss$equipment_name == "GUNS"] <- NA
# ad>ad guns>towed
iiss$equipment_subtype[iiss$equipment_type == "air defence" &
                         iiss$equipment_subtype == "AD GUNS" &
                         iiss$equipment_name == "TOWED"] <- "GUNS"
# ad>guns>ad guns
iiss$equipment_name[iiss$equipment_type == "air defence" &
                      iiss$equipment_subtype == "GUNS" &
                      iiss$equipment_name == "AD GUNS"] <- NA
# ad>at>guns
iiss$equipment_name[iiss$equipment_type == "air defence" &
                      iiss$equipment_subtype == "AT" &
                      iiss$equipment_name == "GUNS"] <- "TOWED"
iiss$equipment_subtype[iiss$equipment_type == "air defence" &
                         iiss$equipment_subtype == "AT" &
                         iiss$equipment_name == "TOWED"] <- "GUNS"
# ad>towed>na
iiss$equipment_name[iiss$equipment_type == "air defence" &
                      iiss$equipment_subtype == "TOWED" &
                      is.na(iiss$equipment_name)] <- "TOWED"
iiss$equipment_subtype[iiss$equipment_type == "air defence" &
                         iiss$equipment_subtype == "TOWED" &
                         iiss$equipment_name == "TOWED"] <- "GUNS"
# ad>guns>arv
iiss$equipment_subtype[iiss$equipment_type == "air defence" &
                         iiss$equipment_subtype == "GUNS" &
                         iiss$equipment_name == "ARV"] <- NA
iiss$equipment_type[iiss$equipment_type == "air defence" &
                      is.na(iiss$equipment_subtype) &
                      iiss$equipment_name == "ARV"] <- "engineering and maintenance vehicles"
# towed to TOWED
iiss$equipment_name[iiss$equipment_name == "towed"] <- "TOWED"
# GDF
iiss$equipment_subname[iiss$equipment_subname == "GDF"] <- NA
# guns>ad
iiss$equipment_name[iiss$equipment_type == "air defence" &
                      iiss$equipment_subtype == "GUNS" &
                      iiss$equipment_name == "AD"] <- NA
# ad>ad>guns
iiss$equipment_subtype[iiss$equipment_type == "air defence" &
                         iiss$equipment_subtype == "AD" &
                         iiss$equipment_name == "GUNS"] <- NA
# TOWED and SP in subname
iiss$equipment_name[grepl("TOWED", iiss$equipment_subname)] <- "TOWED"
iiss$equipment_subname <- gsub("TOWED", '', iiss$equipment_subname)

iiss$equipment_name[grepl("SP", iiss$equipment_subname)] <- "SP"
iiss$equipment_subname <- gsub("SP", '', iiss$equipment_subname)
# getting rid of SAMs
iiss$equipment_subtype[iiss$equipment_type == "air defence" &
                         iiss$equipment_subtype == "GUNS" &
                         iiss$equipment_name == "SAM"] <- NA
#ad>na>guns>sp25mm
iiss$equipment_subtype[iiss$equipment_type == "air defence" &
                         (iiss$equipment_name == "GUNS" |
                            iiss$equipment_name == "GUNS-TOWED" |
                            iiss$equipment_name == "TOWED" |
                            iiss$equipment_name == "GUNS-SP")] <- "GUNS"
# misc
iiss$equipment_subtype[iiss$equipment_type == "anti-tank/anti-infrastructure" &
                         is.na(iiss$equipment_subtype) &
                         iiss$equipment_name == "GUNS"] <- "GUNS"
iiss$equipment_name[iiss$equipment_type == "anti-tank/anti-infrastructure" &
                      iiss$equipment_subtype == "GUNS" &
                      iiss$equipment_name == "GUNS"] <- NA
iiss$equipment_subtype[iiss$equipment_type == "anti-tank/anti-infrastructure" &
                         is.na(iiss$equipment_subtype) &
                         iiss$equipment_name == "TOWED"] <- "GUNS"
#r530
iiss$equipment_subname[iiss$equipment_subname == "R530"] <- NA




# Armoured fighting vehicles
#LT VEH 10
iiss$equipment_name[iiss$equipment_name == "LT VEH 10"] <- "LT VEH"
# remove subtypes
iiss$equipment_subtype[iiss$equipment_type == "armoured fighting vehicles" &
                         (iiss$equipment_subtype == "TK" |
                            iiss$equipment_subtype == "XA Series")] <- NA
# light tanks
iiss$equipment_name[iiss$equipment_type == "armoured fighting vehicles" &
                      is.na(iiss$equipment_subtype) &
                      (iiss$equipment_name == "Light Tanks" |
                         iiss$equipment_name == "LIGHT TANK" |
                         iiss$equipment_name == "LIGHT TANKS")] <- "LT TK"
# tk mbt
iiss$equipment_name[iiss$equipment_type == "armoured fighting vehicles" &
                      is.na(iiss$equipment_subtype) &
                      iiss$equipment_name == "TK MBT"] <- "MBT"
# apc w
iiss$equipment_subname[iiss$equipment_type == "armoured fighting vehicles" &
                         is.na(iiss$equipment_subtype) &
                         (iiss$equipment_name == "APC (W)" |
                            iiss$equipment_name == "APC (w)")] <- "APC (W)"
iiss$equipment_name[iiss$equipment_type == "armoured fighting vehicles" &
                      is.na(iiss$equipment_subtype) &
                      (iiss$equipment_name == "APC (W)" |
                         iiss$equipment_name == "APC (w)") &
                      iiss$equipment_subname == "APC (W)"] <- "APC"
# apc t
iiss$equipment_subname[iiss$equipment_type == "armoured fighting vehicles" &
                         is.na(iiss$equipment_subtype) &
                         iiss$equipment_name == "APC (T)"] <- "APC (T)"
iiss$equipment_name[iiss$equipment_type == "armoured fighting vehicles" &
                      is.na(iiss$equipment_subtype) &
                      iiss$equipment_name == "APC (T)" &
                      iiss$equipment_subname == "APC (T)"] <- "APC"
# afv>na>ppv>na
iiss$equipment_subname[iiss$equipment_type == "armoured fighting vehicles" &
                         is.na(iiss$equipment_subtype) &
                         iiss$equipment_name == "PPV" &
                         (iiss$equipment_subname == "APC (W)" |
                            is.na(iiss$equipment_subname))] <- "PPV"
iiss$equipment_name[iiss$equipment_type == "armoured fighting vehicles" &
                      is.na(iiss$equipment_subtype) &
                      iiss$equipment_name == "PPV" &
                      iiss$equipment_subname == "PPV"] <- "APC"
#misc
iiss$equipment_name[iiss$equipment_name == "APC" &
                      iiss$equipment_subname == "AAV"] <- "AAV"
iiss$equipment_subname[iiss$equipment_name == "AAV" &
                         iiss$equipment_subname == "AAV"] <- NA
#aslt gun
iiss$equipment_name[iiss$equipment_type == "armoured fighting vehicles" &
                      iiss$equipment_name == "ASLT GUN"] <- "ASLT"
#reece
iiss$equipment_name[iiss$equipment_name == "REECE"] <- "RECCE"
#apc(w) in eqpt subtype
iiss$equipment_name[iiss$equipment_type == "armoured fighting vehicles" &
                      iiss$equipment_subtype == "APC(W)"] <- "APC (W)"
iiss$equipment_subtype[iiss$equipment_type == "armoured fighting vehicles" &
                      iiss$equipment_subtype == "APC(W)" &
                      iiss$equipment_name == "APC (W)"] <- NA
#recce in eqpt subtype
iiss$equipment_name[iiss$equipment_type == "armoured fighting vehicles" &
                      iiss$equipment_subtype == "RECCE"] <- "RECCE"
iiss$equipment_subtype[iiss$equipment_type == "armoured fighting vehicles" &
                         iiss$equipment_subtype == "RECCE" &
                         iiss$equipment_name == "RECCE"] <- NA
#recce in subname
iiss$equipment_name[iiss$equipment_type == "armoured fighting vehicles" &
                      iiss$equipment_subname == "RECCE"] <- "RECCE"
iiss$equipment_subname[iiss$equipment_type == "armoured fighting vehicles" &
                         iiss$equipment_name == "RECCE" &
                      iiss$equipment_subname == "RECCE"] <- NA
#lfv in subname
iiss$equipment_name[iiss$equipment_type == "armoured fighting vehicles" &
                      iiss$equipment_subname == "LFV"] <- "LFV"
iiss$equipment_subname[iiss$equipment_type == "armoured fighting vehicles" &
                      iiss$equipment_name == "LFV" &
                      iiss$equipment_subname == "LFV"] <- NA
#flip l-a-l and subname
iiss$equipment_subname[iiss$equipment_type == "armoured fighting vehicles" &
                         iiss$equipment_name == "L-A-L" &
                         iiss$equipment_subname == "AIFV/APC"] <- "L-A-L"
iiss$equipment_name[iiss$equipment_type == "armoured fighting vehicles" &
                         iiss$equipment_name == "L-A-L" &
                         iiss$equipment_subname == "L-A-L"] <- "AIFV/APC"
#apc in subname
iiss$equipment_name[iiss$equipment_type == "armoured fighting vehicles" &
                      iiss$equipment_name == "AIFV" &
                      iiss$equipment_subname == "APC"] <- "AIFV/APC"
iiss$equipment_subname[iiss$equipment_type == "armoured fighting vehicles" &
                         iiss$equipment_name == "AIFV/APC" &
                         iiss$equipment_subname == "APC"] <- NA
#mbt in subname
iiss$equipment_name[iiss$equipment_type == "armoured fighting vehicles" &
                      iiss$equipment_subname == "MBT"] <- "MBT"
iiss$equipment_subname[iiss$equipment_type == "armoured fighting vehicles" &
                         iiss$equipment_name == "MBT" &
                         iiss$equipment_subname == "MBT"] <- NA
#ltlk in subname
iiss$equipment_name[iiss$equipment_type == "armoured fighting vehicles" &
                      iiss$equipment_subname == "LTLK"] <- "LT TK"
iiss$equipment_subname[iiss$equipment_type == "armoured fighting vehicles" &
                         iiss$equipment_name == "LT TK" &
                         iiss$equipment_subname == "LTLK"] <- NA
# coerce apc(t) and apc(w)
iiss$equipment_subname[iiss$equipment_subname == "APT(T)-4K4"] <- "APC (T)"
iiss$equipment_subname[iiss$equipment_subname == "APT(T)-Ulan"] <- "APC (T)"
iiss$equipment_subname[iiss$equipment_subname == "APT(W)VAB"] <- "APC (W)"

#type variants
iiss$equipment_subname[iiss$equipment_type == "armoured fighting vehicles" &
                         iiss$equipment_subname == "TYPEVARIANTS"] <- "L-A-L"
#aifv/apc (w) and recce/aifv/apc (w)
iiss$equipment_name[iiss$equipment_type == "armoured fighting vehicles" &
                      iiss$equipment_name == "AIFV/APC (W)"] <- "AIFV/APC"
iiss$equipment_name[iiss$equipment_type == "armoured fighting vehicles" &
                      iiss$equipment_name == "RECCE/AIFV/APC (W)"] <- "AIFV/APC"
#apapc (w)
iiss$equipment_subname[iiss$equipment_type == "armoured fighting vehicles" &
                         iiss$equipment_name == "APAPC (W)"] <- "APC(W)"
iiss$equipment_name[iiss$equipment_type == "armoured fighting vehicles" &
                         iiss$equipment_name == "APAPC (W)" &
                      iiss$equipment_subname == "APC(W)"] <- "APC"
#apc(w)
iiss$equipment_subname[iiss$equipment_type == "armoured fighting vehicles" &
                         iiss$equipment_name == "APC(W)"] <- "APC(W)"
iiss$equipment_name[iiss$equipment_type == "armoured fighting vehicles" &
                      iiss$equipment_name == "APC(W)" &
                      iiss$equipment_subname == "APC(W)"] <- "APC"
#lt name tk subname
iiss$equipment_name[iiss$equipment_type == "armoured fighting vehicles" &
                      iiss$equipment_name == "LT" &
                      iiss$equipment_subname == "TK"] <- "LT TK"
iiss$equipment_subname[iiss$equipment_type == "armoured fighting vehicles" &
                      iiss$equipment_name == "LT TK" &
                      iiss$equipment_subname == "TK"] <- NA
#lt name veh subname
iiss$equipment_name[iiss$equipment_type == "armoured fighting vehicles" &
                      iiss$equipment_name == "LT" &
                      iiss$equipment_subname == "VEH"] <- "LT VEH"
iiss$equipment_subname[iiss$equipment_type == "armoured fighting vehicles" &
                         iiss$equipment_name == "LT VEH" &
                         iiss$equipment_subname == "VEH"] <- NA
#sp 122mm move
iiss$equipment_type[iiss$equipment_type == "armoured fighting vehicles" &
                      iiss$equipment_name == "SP" &
                      iiss$equipment_subname == "122mm"] <- "artillery"
#apc(t)-4k4 and apc(t)-ulan
iiss$equipment_subname[iiss$equipment_type == "armoured fighting vehicles" &
                      (iiss$equipment_subname == "APC(T)-4K4" |
                         iiss$equipment_subname == "APC(T)-Ulan")] <- "APC(T)"
#APC(W)VAB
iiss$equipment_name[iiss$equipment_type == "armoured fighting vehicles" &
                       iiss$equipment_subname == "APC(W)VAB"] <- "APC(W)"
#VCTP(inclvarients)
iiss$equipment_name[iiss$equipment_type == "armoured fighting vehicles" &
                      iiss$equipment_subname == "VCTP(inclvarients)"] <- "VCTP"
iiss$equipment_subname[iiss$equipment_type == "armoured fighting vehicles" &
                      iiss$equipment_name == "VCTP" &
                      iiss$equipment_subname == "VCTP(inclvarients)"] <- NA
#matra in subname
iiss$equipment_subname[iiss$equipment_type == "air-launched missiles" &
                         iiss$equipment_subname == "Matra"] <- NA
#BRM-1k
iiss$unit_name[iiss$equipment_type == "armoured fighting vehicles" &
                 iiss$equipment_name == "AIFV" &
                 iiss$equipment_subname == "BRM-1K"] <- "BRM-1K"
iiss$equipment_subname[iiss$equipment_type == "armoured fighting vehicles" &
                 iiss$equipment_name == "AIFV" &
                 iiss$equipment_subname == "BRM-1K"] <- NA
# ATGW
iiss$equipment_type[iiss$equipment_type == "armoured fighting vehicles" &
                      iiss$equipment_name == "ATGW"] <- "anti-tank/anti-infrastructure"
# remove subnames
iiss$equipment_subname[iiss$equipment_type == "armoured fighting vehicles" &
                         (iiss$equipment_subname == "PPC" |
                            iiss$equipment_subname == "TAC" |
                            iiss$equipment_subname == "PRC" |
                            iiss$equipment_subname == "PVP" |
                            iiss$equipment_subname == "VBL")]






# Aircraft
#remove A-37BDragonfly/OA-37BDragonfly
iiss$equipment_subname[iiss$equipment_type == "aircraft" &
                         iiss$equipment_subname == "A-37BDragonfly/OA-37BDragonfly"] <- NA
#communications
iiss$equipment_name[iiss$equipment_type == "aircraft" &
                      (iiss$equipment_name == "Communications" |
                         iiss$equipment_name == "COMMS")] <- "communications"
#Trg/COIN
iiss$equipment_name[iiss$equipment_type == "aircraft" &
                      iiss$equipment_name == "Trg/COIN"] <- "TRG/COIN"
#MP ASW
iiss$equipment_name[iiss$equipment_type == "aircraft" &
                      iiss$equipment_name == "MP ASW"] <- "MP/ASW"
#MP/Surv
iiss$equipment_name[iiss$equipment_type == "aircraft" &
                      iiss$equipment_name == "MP/Surv"] <- "MP/SURV"
#aicraft
iiss$equipment_type[iiss$equipment_type == "aicraft"] <- "aircraft"
# tiltrotor
iiss$equipment_subtype[iiss$equipment_subtype == "tiltrotor"] <- NA
# special freighter
iiss$equipment_subtype[iiss$equipment_subtype == "special freighter"] <- NA
#acrft>tpt>light
iiss$equipment_subname[iiss$equipment_subtype == "TPT" &
                         iiss$equipment_name == "light"] <- "light"
iiss$equipment_name[iiss$equipment_subtype == "TPT" &
                      iiss$equipment_name == "light" &
                      iiss$equipment_subname == "light"] <- "TPT"
iiss$equipment_subtype[iiss$equipment_subtype == "TPT" &
                         iiss$equipment_name == "TPT" &
                         iiss$equipment_subname == "light"] <- NA
#aewandcontrol
iiss$equipment_subtype[iiss$equipment_subtype == "AEW AND CONTROL"] <- "AEWandC"
#acrft>aew
iiss$equipment_name[iiss$equipment_type == "aircraft" &
                      iiss$equipment_subtype == "AEWandC"] <- "AEWandC"
iiss$equipment_subtype[iiss$equipment_type == "aircraft" &
                         iiss$equipment_subtype == "AEWandC" &
                         iiss$equipment_name == "AEWandC"] <- NA
#acrft>ftr
iiss$equipment_name[iiss$equipment_type == "aircraft" &
                      iiss$equipment_subtype == "FTR"] <- "FTR"
iiss$equipment_subtype[iiss$equipment_type == "aircraft" &
                         iiss$equipment_subtype == "FTR" &
                         iiss$equipment_subtype == "FTR"] <- NA
#acrft>tpt
iiss$equipment_name[iiss$equipment_type == "aircraft" &
                      iiss$equipment_subtype == "TPT"] <- "TPT"
iiss$equipment_subtype[iiss$equipment_type == "aircraft" &
                         iiss$equipment_subtype == "TPT" &
                         iiss$equipment_subtype == "TPT"] <- NA
#acrft>trg
iiss$equipment_name[iiss$equipment_type == "aircraft" &
                      iiss$equipment_subtype == "TRG"] <- "TRG"
iiss$equipment_subtype[iiss$equipment_type == "aircraft" &
                         iiss$equipment_subtype == "TRG" &
                         iiss$equipment_subtype == "TRG"] <- NA

#pax
iiss$equipment_subname[iiss$equipment_type == "aircraft" &
                         iiss$equipment_name == "PAX"] <- "PAX"
iiss$equipment_name[iiss$equipment_type == "aircraft" &
                      iiss$equipment_name == "PAX" &
                      iiss$equipment_subname == "PAX"] <- NA

#elint
iiss$equipment_name[iiss$equipment_type == "aircraft" &
                      iiss$equipment_name == "EW" &
                      (iiss$equipment_subname == "ELINT" |
                         iiss$equipment_subname == "Elint")] <- "ELINT"
iiss$equipment_subname[iiss$equipment_type == "aircraft" &
                         iiss$equipment_name == "ELINT" &
                         (iiss$equipment_subname == "ELINT" |
                            iiss$equipment_subname == "Elint")] <- NA
#elint
iiss$equipment_name[iiss$equipment_type == "aircraft" &
                       iiss$equipment_name == "Elint"] <- "ELINT"
#ac>civil defence>ac
iiss$equipment_name[iiss$equipment_type == "aircraft" &
                      iiss$equipment_subtype == "civil defence" &
                      (iiss$equipment_name == "AC" |
                         is.na(iiss$equipment_name))] <- "TPT"
iiss$equipment_subtype[iiss$equipment_type == "aircraft" &
                      iiss$equipment_subtype == "civil defence" &
                      iiss$equipment_name == "TPT"] <- NA
#remove helicopters
iiss$equipment_type[iiss$equipment_type == "aircraft" &
                      (iiss$equipment_name == "ATTACK HEL" |
                         iiss$equipment_name == "ATTACK HELICOPTERS" |
                         iiss$equipment_name == "ARMED HEL")] <- "helicopters"
iiss$equipment_type[iiss$equipment_type == "aircraft" &
                      iiss$equipment_name == "HELICOPTERS"] <- "helicopters"
iiss$equipment_type[iiss$equipment_type == "aircraft" &
                      iiss$equipment_name == "HEL"] <- "helicopters"
#misc
iiss$unit_name[iiss$equipment_name == "Su-22M-4 (Su-17M-4) Fitter K Su-17M Fitter C FGA/Su-22UM-3K (Su-17UM-3) Fitter G Su-17U trg"] <- "Su-22M-4 (Su-17M-4) Fitter K Su-17M Fitter C FGA/Su-22UM-3K (Su-17UM-3) Fitter G Su-17U trg"
iiss$equipment_name[iiss$equipment_name == "Su-22M-4 (Su-17M-4) Fitter K Su-17M Fitter C FGA/Su-22UM-3K (Su-17UM-3) Fitter G Su-17U trg"] <- NA
#AIRCRAFT
iiss$equipment_name[iiss$equipment_type == "aircraft" &
                      iiss$equipment_name == "AIRCRAFT"] <- "TPT"
#Coerce TPT
iiss$equipment_name[iiss$equipment_type == "aircraft" &
                      (iiss$equipment_name == "T" |
                         iiss$equipment_name == "tpt" |
                         iiss$equipment_name == "Transport" |
                         iiss$equipment_name == "TRANSPORT")] <- "TPT"
#Coerce TRG
iiss$equipment_name[iiss$equipment_type == "aircraft" &
                      (iiss$equipment_name == "TRAINING" |
                         iiss$equipment_name == "TR" |
                         iiss$equipment_name == "trg" |
                         iiss$equipment_name == "Training"|
                         iiss$equipment_name == "training" |
                         iiss$equipment_name == "TRG CENTRE")] <- "TRG"
#Coerce TT
iiss$equipment_name[iiss$equipment_type == "aircraft" &
                      (iiss$equipment_name == "TandT" |
                         iiss$equipment_name == "TEST" |
                         iiss$equipment_name == "TRIALS")] <- "TT"
#Coerce STRIKE/FGA
iiss$equipment_name[iiss$equipment_type == "aircraft" &
                      iiss$equipment_name == "Strike/FGA"] <- "STRIKE/FGA"
#Coerce FTR
iiss$equipment_name[iiss$equipment_type == "aircraft" &
                      (iiss$equipment_name == "Fighter" |
                         iiss$equipment_name == "FIGHTER" |
                         iiss$equipment_name == "FRG" |
                         iiss$equipment_name == "FTG" |
                         iiss$equipment_name == "FTR/BOMBERS")] <- "FTR"
#tankers
iiss$equipment_name[iiss$equipment_type == "aircraft" &
                      iiss$equipment_name == "TANKERS"] <- "TKR"
#survey
iiss$equipment_name[iiss$equipment_type == "aircraft" &
                      (iiss$equipment_name == "SURVEY" |
                         iiss$equipment_name == "survey")] <- "SURV"
#multi-role
iiss$equipment_name[iiss$equipment_type == "aircraft" &
                      iiss$equipment_name == "MULTI-ROLE"] <- "MR"
#AEW
iiss$equipment_name[iiss$equipment_type == "aircraft" &
                      iiss$equipment_name == "AEW"] <- "AEWandC"
#EW
iiss$equipment_name[iiss$equipment_type == "aircraft" &
                      iiss$equipment_name == "EWS"] <- "EW"
#LACM
iiss$equipment_type[iiss$equipment_type == "aircraft" &
                      iiss$equipment_name == "LACM"] <- "surface-to-surface missile launchers"
#AC
iiss$equipment_name[iiss$equipment_type == "aircraft" &
                      iiss$equipment_name == "AC"] <- "UTL"
#CandC
iiss$equipment_name[iiss$equipment_type == "aircraft" &
                      iiss$equipment_name == "CandC"] <- "C2"
#In Store/trials
iiss <- iiss %>% dplyr::mutate(notes = ifelse(iiss$equipment_type == "aircraft" &
                                                (iiss$equipment_name == "In Store" |
                                                   iiss$equipment_name == "IN STORE" |
                                                   iiss$equipment_name == "STORAGE"), paste0(unit_count, " in store"), notes))
iiss <- iiss %>% dplyr::mutate(notes = ifelse(iiss$equipment_type == "aircraft" &
                                                iiss$equipment_name == "TRIALS AND TEST", paste0(unit_count, " test and trials"), notes))
iiss$equipment_name[iiss$equipment_type == "aircraft" &
                      (iiss$equipment_name == "In Store" |
                         iiss$equipment_name == "IN STORE" |
                         iiss$equipment_name == "STORAGE")] <- NA
#LCA/LCM/LCU
iiss$equipment_type[iiss$equipment_type == "aircraft" &
                      (iiss$equipment_name == "LCU" |
                         iiss$equipment_name == "LCA" |
                         iiss$equipment_name == "LCM")] <- "amphibious"
#MULTI ROLE
iiss$equipment_name[iiss$equipment_type == "aircraft" &
                      iiss$equipment_name == "Multi Role"] <- "MR"
# move unit names around
#iiss <- iiss %>% dplyr::mutate(unit_name = ifelse(iiss$equipment_type == "aircraft" &
 #                                                   (iiss$equipment_subname == "C-130" |
  #                                                     iiss$equipment_subname == "F-5" |
   #                                                    iiss$equipment_subname == "L-410" |
    #                                                   iiss$equipment_subname == "PC-6" |
     #                                                  iiss$equipment_subname == "RC-12" |
      #                                                 iiss$equipment_subname == "SAAB" |
       #                                                iiss$equipment_subname == "SC.73MSkyvan2" |
        #                                               iiss$equipment_subname == "TP-100A"), equipment_subname, unit_name),
         #                      equipment_subname = ifelse(iiss$equipment_type == "aircraft" &
          #                                          (iiss$equipment_subname == "C-130" |
           #                                            iiss$equipment_subname == "F-5" |
            #                                           iiss$equipment_subname == "L-410" |
             #                                          iiss$equipment_subname == "PC-6" |
              #                                         iiss$equipment_subname == "RC-12" |
               #                                        iiss$equipment_subname == "SAAB" |
                #                                       iiss$equipment_subname == "SC.73MSkyvan2" |
                 #                                      iiss$equipment_subname == "TP-100A"), NA, equipment_subname))
iiss$equipment_subname[iiss$equipment_type == "aircraft" &
                         iiss$equipment_subname == "ARL"] <- NA
iiss$equipment_subname[iiss$equipment_type == "aircraft" &
                         iiss$equipment_subname == "7.0"] <- NA
#TPT and TRG in subname
iiss$equipment_name[iiss$equipment_type == "aircraft" &
                      iiss$equipment_subname == "TRG"] <- "TRG"
iiss$equipment_subname[iiss$equipment_type == "aircraft" &
                      iiss$equipment_name == "TRG" &
                      iiss$equipment_subname == "TRG"] <- NA

iiss$equipment_name[iiss$equipment_type == "aircraft" &
                      iiss$equipment_subname == "TPT"] <- "TPT"
iiss$equipment_subname[iiss$equipment_type == "aircraft" &
                         iiss$equipment_name == "TPT" &
                         iiss$equipment_subname == "TPT"] <- NA
#long-range
iiss$equipment_subname[iiss$equipment_type == "aircraft" &
                         iiss$equipment_subname == "longrange"] <- "long-range"
#medium
iiss$equipment_subname[iiss$equipment_type == "aircraft" &
                         iiss$equipment_subname == "medium"] <- "medium-range"
#capitalize tpt
iiss$equipment_name[iiss$equipment_name == "ELINT/Tpt"] <- "ELINT/TPT"
iiss$equipment_name[iiss$equipment_name == "EW/tpt"] <- "EW/TPT"
iiss$equipment_name[iiss$equipment_name == "EW/Tpt"] <- "EW/TPT"
iiss$equipment_name[iiss$equipment_name == "CBT TRG"] <- "CBT/TRG"
#ac in subtype
iiss$equipment_name[iiss$equipment_type == "aircraft" &
                         iiss$equipment_subtype == "AC"] <- "UTL"
iiss$equipment_subtype[iiss$equipment_type == "aircraft" &
                         iiss$equipment_subtype == "AC"] <- NA
#mlight
iiss$equipment_subname[iiss$equipment_subname == "mlight"] <- "light"
#Bomber
iiss$equipment_name[iiss$equipment_type == "aircraft" &
                      (iiss$equipment_name == "Bomber" |
                         iiss$equipment_name == "BOMBERS" |
                         iiss$equipment_name == "LT BBR" |
                         iiss$equipment_name == "MED BBR")] <- "BBR"
#attack/recce
iiss$equipment_name[iiss$equipment_type == "aircraft" &
                      iiss$equipment_name == "ATTACK/RECCE"] <- "ATK/RECCE"
#armed
iiss$equipment_name[iiss$equipment_type == "aircraft" &
                      (iiss$equipment_name == "armed" |
                         iiss$equipment_name == "ATTACK" |
                         iiss$equipment_name == "MARITIME ATTACK")] <- "ATK"
#Liaison
iiss$equipment_name[iiss$equipment_type == "aircraft" &
                      iiss$equipment_name == "Liaison"] <- "LIAISON"
#light attack/trg
iiss$equipment_name[iiss$equipment_type == "aircraft" &
                      (iiss$equipment_name == "LIGHT ATTACK/TRG" |
                         iiss$equipment_name == "COMBAT TRG")] <- "ATK/TRG"
#mr(mp)
iiss$equipment_name[iiss$equipment_type == "aircraft" &
                      iiss$equipment_name == "MR(MP)"] <- "MR/MP"
#mr/attack
iiss$equipment_name[iiss$equipment_type == "aircraft" &
                      iiss$equipment_name == "MR/ATTACK"] <- "MR/ATK"
#presidential
iiss$equipment_name[iiss$equipment_type == "aircraft" &
                      (iiss$equipment_name == "PRESIDENTIAL" |
                         iiss$equipment_name == "PRESIDENTIAL FLT")] <- "TPT"
#acwg
iiss$equipment_subname[iiss$equipment_type == "aircraft" &
                         iiss$equipment_subname == "ACWG"] <- NA
#ew>aew
iiss$equipment_name[iiss$equipment_type == "aircraft" &
                      iiss$equipment_name == "EW" &
                      iiss$equipment_subname == "AEW"] <- "AEW"
iiss$equipment_subname[iiss$equipment_type == "aircraft" &
                      iiss$equipment_name == "AEW" &
                      iiss$equipment_subname == "AEW"] <- NA
#at-3 in subname
iiss$equipment_subname[iiss$equipment_type == "aircraft" &
                         iiss$equipment_subname == "AT-3"] <- NA
#trials
iiss$equipment_subname[iiss$equipment_type == "aircraft" &
                         (iiss$equipment_name == "TRIALS" |
                            iiss$equipment_name == "TT" |
                            iiss$equipment_name == "TEST AND TRIALS")] <- "TT"
#cessna in subname
#iiss$unit_name <- ifelse(iiss$equipment_type == "aircraft" &
 #                iiss$equipment_subname == "Cessna",
  #               paste(iiss$equipment_subname, iiss$unit_name, sep = " "), iiss$unit_name)
#hel in subname
iiss$equipment_subname[iiss$equipment_type == "aircraft" &
                         iiss$equipment_subname == "hel"] <- NA
#tpt>mr/sar
iiss$equipment_name[iiss$equipment_type == "aircraft" &
                      iiss$equipment_name == "TPT" &
                      iiss$equipment_subname == "MR/SAR"] <- "MR/SAR"
iiss$equipment_subname[iiss$equipment_type == "aircraft" &
                      iiss$equipment_name == "MR/SAR" &
                      iiss$equipment_subname == "MR/SAR"] <- NA
#schweizer in subname
#iiss$unit_name <- ifelse(iiss$equipment_type == "aircraft" &
 #                          iiss$equipment_subname == "Schweizer",
  #                       paste(iiss$equipment_subname, iiss$unit_name, sep = " "), iiss$unit_name)
#su-27 in subname
#iiss$unit_name <- ifelse(iiss$equipment_type == "aircraft" &
 #                          iiss$equipment_subname == "Su-27",
  #                       paste(iiss$equipment_subname, iiss$unit_name, sep = " "), iiss$unit_name)
# remove subnames
iiss$equipment_subname[iiss$equipment_type == "aircraft" &
                         (iiss$equipment_subname == "(6withSLAR)" |
                            iiss$equipment_subname == "2sqn" |
                            iiss$equipment_subname == "Bandeirante" |
                            iiss$equipment_subname == "beech" |
                            iiss$equipment_subname == "C-130" |
                            iiss$equipment_subname == "Cessna" |
                            iiss$equipment_subname == "EC-93" |
                            iiss$equipment_subname == "EMB-111(P-54/P-95B)" |
                            iiss$equipment_subname == "EU-93" |
                            iiss$equipment_subname == "F-5" |
                            iiss$equipment_subname == "L-410" |
                            iiss$equipment_subname == "PC-6" |
                            iiss$equipment_subname == "R-99B" |
                            iiss$equipment_subname == "RC-12" |
                            iiss$equipment_subname == "SAAB" |
                            iiss$equipment_subname == "SC.73MSkyvan2" |
                            iiss$equipment_subname == "Schweizer" |
                            iiss$equipment_subname == "Su-27" |
                            iiss$equipment_subname == "TP-100A" |
                            iiss$equipment_subname == "OBS" |
                            iiss$equipment_subname == "(HB-315BGaviao)" |
                            iiss$equipment_subname == "AlouetteIII\n/SA-319\nAlouetteIII" |
                            iiss$equipment_subname == "TwinOtter" |
                            iiss$equipment_subname == "EU-93A")] <- NA
#aircraft>na>mp>asw
iiss$equipment_name[iiss$equipment_type == "aircraft" &
                      iiss$equipmemt_name == "MP" &
                      iiss$equipment_subname == "ASW"] <- "MP/ASW"
iiss$equipment_subname[iiss$equipment_type == "aircraft" &
                      iiss$equipmemt_name == "MP/ASW" &
                      iiss$equipment_subname == "ASW"] <- NA
#airship
iiss$equipment_type[iiss$equipment_type == "airships"] <- "aircraft"
#arm
iiss$equipment_name[iiss$equipment_type == "aircraft" &
                      iiss$equipment_name == "ARM"] <- "FGA"
#air arm
iiss$equipment_name[iiss$equipment_type == "aircraft" &
                      iiss$equipment_name == "AIR ARM"] <- "UTL"
#asmp
iiss$equipment_name[iiss$equipment_type == "aircraft" &
                      iiss$equipment_name == "ASMP"] <- "FGA"
#vvv
iiss$equipment_name[iiss$equipment_type == "aircraft" &
                      iiss$equipment_name == "vvv"] <- "MR"
#FGA/SWN
iiss$equipment_name[iiss$equipment_type == "aircraft" &
                      (iiss$equipment_name == "FGA/SWN" |
                         iiss$equipment_name == "FGA/STRIKE")] <- "FGA"
#CCT
iiss$equipment_name[iiss$equipment_type == "aircraft" &
                      iiss$equipment_name == "CCT"] <- "TRG"
#FF
#iiss$equipment_name[iiss$equipment_type == "aircraft" &
 #                     iiss$equipment_name == "FF"] <- "UTL"
#mrh
iiss$equipment_type[iiss$equipment_type == "aircraft" &
                      (iiss$equipment_name == "MRH" |
                         iiss$equipment_name == "hel")] <- "helicopters"
#adf
iiss$equipment_type[iiss$equipment_type == "aircraft" &
                      iiss$equipment_name == "ADF"] <- "air defence"
iiss$equipment_name[iiss$equipment_type == "air defence" &
                      iiss$equipment_name == "ADF"] <- "SAM"
#LIAISON and TBT
iiss$equipment_name[iiss$equipment_type == "aircraft" &
                      (iiss$equipment_name == "LIAISON" |
                         iiss$equipment_name == "LIAISON/TRG" |
                         iiss$equipment_name == "TBT")] <- "TPT"
#LIFT and OCU
iiss$equipment_name[iiss$equipment_type == "aircraft" &
                      (iiss$equipment_name == "LIFT" |
                         iiss$equipment_name == "OCU")] <- "TRG"
#CONTROL
iiss$equipment_type[iiss$equipment_type == "aircraft" &
                      iiss$equipment_name == "CONTROL"] <- "radars"
#OBS
iiss$equipment_name[iiss$equipment_type == "aircraft" &
                      iiss$equipment_name == "OBS"] <- "RECCE"
#ac
iiss$equipment_name[iiss$equipment_type == "aircraft" &
                      iiss$equipment_name == "ac"] <- "FGA"
#VIP
iiss$equipment_name[iiss$equipment_type == "aircraft" &
                      iiss$equipment_name == "VIP"] <- "TPT"
#SF
iiss$equipment_type[iiss$equipment_type == "aircraft" &
                      iiss$equipment_name == "SF"] <- "amphibious"
#unit name coercion
iiss$equipment_name[iiss$equipment_type == "aircraft" &
                      is.na(iiss$equipment_name) &
                      iiss$unit_name %in% c("A-7", "A-7E", "A-7H", "A-7H/TA-7H", "Alpha Jet", "AMX", "Entendard IV P", "FGA",
                                            "Harrier", "Hunter F9", "Jaguar", "Jaguar-A", "Jastreb", "Kraguj", "L-29", "L-29 Delfin",
                                            "MiG-15 U7", "MiG-17", "MiG-21", "MiG-21 BIS/UM","MiG-21 Lancer B (two seat trainers)",
                                            "MiG-21MF Fishbed J Mi-G 21/MiG-21UB Mongol Mongil A trg;", "MiG-21MF/UB", "MiG-21R", "MiG-21U",
                                            "MiG-23", "MiG-23B/E", "MiG-25", "MiG-27", "MiG-29", "MiG-29 Fulcrum/MiG-29UB Fulcrum Mig-29U",
                                            "MiG-29/Su-22/Su-24/Su-27", "MiG-29/UB", "MiG-31", "Mirage 2000", "Mirage 2000 (EG (FGA)/BG (trg))",
                                            "Mirage 2000B", "Mirage 2000C", "Mirage 2000N", "Mirage 2000N (ASMP", "Mirage 5", "Mirage 5D/E", "Mirage EL3",
                                            "Mirage F-1 EG (ftr)", "Mirage M-2000D", "Orao", "Phantom", "Su-17", "Su-20", "Su-22",
                                            "Su-22M-4 (Su-17M-4) Fitter K Su-17M Fitter C FGA/Su-22UM-3K (Su-17UM-3) Fitter G Su-17U trg",
                                            "Su-22M4/UM3K", "Su-24", "Su-25", "Su-25K Frogfoot A FGA/Su-25UBK Frogfoot B trg", "Su-25K/UBK",
                                            "Su-27", "Su-7B/U", "Super Etendard", "Super Etendard", "TA-7H", "TH-7P", "TF-104G", "Tornado",
                                            "Typhoon/Tornado")] <- "FGA"

iiss$equipment_name[iiss$equipment_type == "aircraft" &
                      is.na(iiss$equipment_name) &
                      iiss$unit_name %in% c("A310-300", "Aero Commander", "An-2", "An-21", "An-24", "AN-24", "An-24 Coke",
                                            "An-26", "AN-26", "An-26 Curl", "AN-26 Curl", "An-28", "An-28RM", "An-72",
                                            "BAe125", "BAe146 Mk2", "BN-2", "BN-2 Islander", "BN-2A Islander", "BN-2T",
                                            "Boeing-707", "Boeing-707-320", "Boeing 707", "Boeing 707-320 (VIP)",
                                            "C-12A", "C-130", "C-130B (tpt)", "C-130H", "C-130H-30", "C-130H (tpt)",
                                            "C-130H Hercules", "C-130H/J", "C-160", "C-160 Astarte", "C-160 Gabriel", "C-160AG",
                                            "C-160G-2", "C-160H", "C-160NG", "C-17A", "C-212", "C-212A Aviocar", "C-212B Aviovar", "
                                            C-47 (tpt)", "Cessna", "Cessna-U206", "Cessna 206C", "Cessna 337", "Cessna 402", "Cessna Caravan II",
                                            "CF-5-A", "CF-5-D", "CN-235", "Commander", "Commander 500B", "CV-22A Osprey", "CV-22A/B Osprey",
                                            "DC-10-30", "DC-3", "DC-3 (C-47 Skytrain)", "DC-8", "DC-8E", "DC-8F", "DC9-32", "DH-6", "DHC-6",
                                            "DHC-6 Twin Otter", "DHC-7R", "Do-128", "Do-228", "Do-27", "Do-28 (tpt)", "EMB-121", "Falcon 10MER",
                                            "Falcon 20", "Falcon 20C", "Falcon 50", "Falcon 50MER", "Falcon 900", "G-222", "G-4 Super Galeb", "Galeb",
                                            "Gulfstream G-III", "Gulfstram I", "Gulfstream I/V (VIP tpt)", "Gulfstream III", "Hercules C-130",
                                            "HFB-320", "HS-748", "Il-18", "IL-28", "Il-62", "Il-76", "Il-76M/MD", "IL 62", "Islander CC2/2A",
                                            "King Air 200\r\n(leased)", "L-19", "L-410", "L-410-S", "L-410 Turbolet", "Let L-410", "MH-1521",
                                            "Navajo", "NC-212", "O-1", "O-1E", "P-149D", "P-166", "P-180", "PA-31T", "PA-NAVAJO", "PC-6", "PC-7",
                                            "PC-9", "PC-9M", "PD-808", "Pzl-104", "Reims-Cessna 406", "Shrike Commander", "SIAI-208", "SM-1019", "SM-92",
                                            "Super King Air", "SW-111 Merlin (VIP)", "TBM-700", "Tristar", "Tu-128", "Tu-134", "Tu-154", "U-17A",
                                            "VFW-614", "YAK-40", "YS-11-200 (tpt)")] <- "TPT"

iiss$equipment_name[is.na(iiss$equipment_type) &
                      is.na(iiss$equipment_name) &
                      iiss$unit_name %in% c("Gulfstream II")] <- "TPT"

iiss$equipment_type[is.na(iiss$equipment_type) &
                      is.na(iiss$equipment_name) &
                      iiss$unit_name %in% c("Gulfstream II")] <- "aircraft"

iiss$equipment_name[iiss$equipment_type == "aircraft" &
                      is.na(iiss$equipment_name) &
                      iiss$unit_name %in% c("AT-802F Air Tractor")] <- "SPT"

iiss$equipment_name[iiss$equipment_type == "aircraft" &
                      is.na(iiss$equipment_name) &
                      iiss$unit_name %in% c("RF-10", "RF-104G", "RF-35", "RF-4E", "RF-5A", "RU-21")] <- "RECCE"

iiss$equipment_name[iiss$equipment_type == "aircraft" &
                      is.na(iiss$equipment_name) &
                      iiss$unit_name %in% c("OV-1D", "PA-22", "RV-1D", "U-9E")] <- "UTL"

iiss$equipment_name[iiss$equipment_type == "aircraft" &
                      is.na(iiss$equipment_name) &
                      iiss$unit_name %in% c("KC-135", "VC10")] <- "TKR"

iiss$equipment_name[iiss$equipment_type == "aircraft" &
                      is.na(iiss$equipment_name) &
                      iiss$unit_name %in% c("Atlantic (MR)", "MR-2", "Nimrod")] <- "MR"

iiss$equipment_name[iiss$equipment_type == "aircraft" &
                      is.na(iiss$equipment_name) &
                      iiss$unit_name %in% c("B-1B", "B-2", "B-2A Spirit", "B-52", "B-52G Stratofortress", "B-52H Stratofortress",
                                            "Tu-16", "Tu-22", "Tu-22M", "Tu-26")] <- "BBR"

iiss$equipment_name[iiss$equipment_type == "aircraft" &
                      is.na(iiss$equipment_name) &
                      iiss$unit_name %in% c("HU-16B", "SA-360")] <- "SAR"

iiss$equipment_name[iiss$equipment_type == "aircraft" &
                      is.na(iiss$equipment_name) &
                      iiss$unit_name %in% c("Be-12 Mail", "P-3", "P-3C", "P-3N Orion", "P-3P", "S-2T Tracker")] <- "ASW"

iiss$equipment_name[iiss$equipment_type == "aircraft" &
                      is.na(iiss$equipment_name) &
                      iiss$unit_name %in% c("E-3D Sentry", "Saab 340H Erieye", "ZDK-03")] <- "AEW"

iiss$equipment_name[iiss$equipment_type == "aircraft" &
                      is.na(iiss$equipment_name) &
                      iiss$unit_name %in% c("CAP-231")] <- "TT"

iiss$equipment_name[iiss$equipment_type == "aircraft" &
                      is.na(iiss$equipment_name) &
                      iiss$unit_name %in% c("CAP 10", "Chipmunk", "CM-170", "Dominie TI (trg) plus 2 in reserve", "Epsilon", "Fouga",
                                            "Hawk", "Jetstream", "L-39", "MB- 339CD", "MB-326", "MB-339", "MB-339CD", "MFI-15", "MS-760",
                                            "Mystere-Falcon", "N-262", "Nord-262", "Rallye 880", "SAAB T-17", "SF-260M", "Super Galeb",
                                            "T-2", "T-2C", "T-2E (trg)", "T-3", "T-33", "T-33A", "T-37", "T-37B", "T-37B/C (trg)", "T-38", "T-38A Talon",
                                            "T-4/T-8", "T-41", "T-41D", "T-6A", "T-6B", "TS-11", "Tucano (trg) (plus 52 in reserve)",
                                            "Tutor (trg)", "UTVA", "Utva-75", "UTVA-75", "Xingu", "Z-143L", "Zlin-242")] <- "TRG"

iiss$equipment_name[iiss$equipment_type == "aircraft" &
                      is.na(iiss$equipment_name) &
                      iiss$unit_name %in% c("Crusader", "F-104", "F-16", "F-16-A", "F-16-B", "F-16A/B", "F-16AM Fighting Falcon",
                                            "F-16BM Fighting Falcon", " F16CG (FGA)/DG (trg)", "F-27", "F-27M", "F-28 (VIP)", "F-3", "F-4",
                                            "F-4E/RF-4E", "F-4F Phantom II", "F-5", "F-50", "F-5A/B", "F-60", "F104G", "G-91", "NF- 5B",
                                            "NF-5A", "NF-5B", "Su-15")] <- "FTR"


iiss$equipment_name[is.na(iiss$equipment_type) &
                      is.na(iiss$equipment_name) &
                      iiss$unit_name %in% c("F-5")] <- "FTR"

iiss$equipment_type[is.na(iiss$equipment_type) &
                      is.na(iiss$equipment_name) &
                      iiss$unit_name %in% c("F-5")] <- "aircraft"

iiss$equipment_name[iiss$equipment_type == "aircraft" &
                      is.na(iiss$equipment_name) &
                      iiss$unit_name %in% c("CL-215", "CL-415")] <- "SPT"

iiss$equipment_name[iiss$equipment_type == "aircraft" &
                      is.na(iiss$equipment_name) &
                      iiss$unit_name %in% c("II-20")] <- "EW"

iiss$equipment_type[iiss$equipment_type == "aircraft" &
                      is.na(iiss$equipment_name) &
                      iiss$unit_name %in% c("Bell UH-1H/M", "Bo-105m", "IAR-316", "IAR-330", "IAR-93", "Ka-25", "Mi-24", "Mi-8", "Mi-8 Hip")] <- "helicopters"
#uav
iiss$equipment_type[iiss$equipment_type == "aircraft" &
                       iiss$equipment_name == "UAV"] <- "unmanned aerial vehicles"
#Naval Aviation
iiss$equipment_type[iiss$equipment_type == "Naval Aviation"] <- "aircraft"
#F-15A/B and F-15C/D and F-16A/B
iiss$unit_name[iiss$equipment_type == "aircraft" &
                 iiss$equipment_name == "F-15A/B"] <- "F-15A/B"
iiss$unit_name[iiss$equipment_type == "aircraft" &
                 iiss$equipment_name == "F-15C/D"] <- "F-15C/D"
iiss$unit_name[iiss$equipment_type == "aircraft" &
                 iiss$equipment_name == "F-16A/B"] <- "F-16A/B"
iiss$equipment_name[iiss$equipment_type == "aircraft" &
                      (iiss$equipment_name == "F-15A/B" |
                         iiss$equipment_name == "F-15C/D" |
                         iiss$equipment_name == "F-16A/B")] <- "FTR"
# TT, ASW, ac in subname
iiss$equipment_subname[iiss$equipment_type == "aircraft" &
                         (iiss$equipment_subname == "TT" |
                          iiss$equipment_subname == "ASW" |
                            iiss$equipment_subname == "ac")] <- NA






# Amphibious
# LS>LSM
iiss$equipment_name[iiss$equipment_type == "amphibious" &
                      iiss$equipment_name == "LS" &
                      iiss$equipment_subname == "LSM"] <- "LSM"
iiss$equipment_subname[iiss$equipment_type == "amphibious" &
                      iiss$equipment_name == "LSM" &
                      iiss$equipment_subname == "LSM"] <- NA
#LCM in subtype
iiss$equipment_name[iiss$equipment_type == "amphibious" &
                      iiss$equipment_subtype == "LCM"] <- "LCM"
iiss$equipment_subtype[iiss$equipment_type == "amphibious" &
                      iiss$equipment_subtype == "LCM" &
                      iiss$equipment_name == "LCM"] <- NA
#LCAC
iiss$equipment_subtype[iiss$equipment_name == "LC" &
                         iiss$equipment_subname == "LCAC"] <- "landing craft"
iiss$equipment_name[iiss$equipment_subtype == "landing craft" &
                      iiss$equipment_name == "LC" &
                      iiss$equipment_subname == "LCAC"] <- "LCAC"
iiss$equipment_subname[iiss$equipment_subtype == "landing craft" &
                         iiss$equipment_name == "LCAC" &
                         iiss$equipment_subname == "LCAC"] <- NA
# craft
iiss$equipment_subtype[iiss$equipment_type == "amphibious" &
                         iiss$equipment_subtype == "CRAFT"] <- "landing craft"
#LACV/LCAC/LSLH/RRC
iiss$equipment_name[iiss$equipment_type == "amphibious" &
                      iiss$equipment_subname == "LACV"] <- "LACV"
iiss$equipment_subname[iiss$equipment_type == "amphibious" &
                      iiss$equipment_name == "LACV" &
                      iiss$equipment_subname == "LACV"] <- NA
iiss$equipment_name[iiss$equipment_type == "amphibious" &
                      iiss$equipment_subname == "LCAC"] <- "LCAC"
iiss$equipment_subname[iiss$equipment_type == "amphibious" &
                         iiss$equipment_name == "LCAC" &
                         iiss$equipment_subname == "LCAC"] <- NA
iiss$equipment_name[iiss$equipment_type == "amphibious" &
                      iiss$equipment_subname == "LSLH"] <- "LSLH"
iiss$equipment_subname[iiss$equipment_type == "amphibious" &
                         iiss$equipment_name == "LSLH" &
                         iiss$equipment_subname == "LSLH"] <- NA
iiss$equipment_name[iiss$equipment_type == "amphibious" &
                      iiss$equipment_subname == "RRC"] <- "RRC"
iiss$equipment_subname[iiss$equipment_type == "amphibious" &
                         iiss$equipment_name == "RRC" &
                         iiss$equipment_subname == "RRC"] <- NA
#sf in equipment_subtype
iiss$equipment_subtype[iiss$equipment_type == "amphibious" &
                         iiss$equipment_subtype == "SF"] <- "special forces"
#amphib in name
iiss$equipment_name[iiss$equipment_type == "amphibious" &
                      iiss$equipment_name == "amphibious"] <- NA
#lst in subname
iiss$equipment_name[iiss$equipment_type == "amphibious" &
                      iiss$equipment_subname == "LST"] <- "LST"
iiss$equipment_subname[iiss$equipment_type == "amphibious" &
                         iiss$equipment_name == "LST" &
                      iiss$equipment_subname == "LST"] <- NA
#utilitycraftaircushion
iiss$equipment_subname[iiss$equipment_subname == "utilitycraftaircushion"] <- "UCAC"
#aslt craft
iiss$equipment_subtype[iiss$equipment_type == "amphibious" &
                         iiss$equipment_subtype == "ASLT CRAFT"] <- "assault craft"
#landing craft
iiss$equipment_subtype[iiss$equipment_type == "amphibious" &
                         (iiss$equipment_subtype == "landing craft" |
                            iiss$equipment_subtype == "landing ships")] <- "landing ships"
#principal amphibious ships
iiss$equipment_subtype[iiss$equipment_type == "amphibious" &
                         (iiss$equipment_subtype == "principal amphibious vessels" |
                            iiss$equipment_subtype == "principal amphibious ships")] <- "principal amphibious ships"
#craft
iiss$equipment_type[iiss$subservice == "amphibious" &
                      iiss$equipment_type == "craft"] <- "amphibious"
iiss$equipment_type[iiss$subservice == "coast guard" &
                      iiss$equipment_type == "craft"] <- "patrol and coastal combatants"
#lsl in subname
iiss$equipment_subname[iiss$equipment_type == "amphibious" &
                         iiss$equipment_subname == "LSL"] <- NA
#AMPHBIBIOUS
iiss$equipment_type[iiss$equipment_type == "patrol and coastal combatants" &
                      iiss$equipment_name == "AMPHIBIOUS"] <- "amphibious"
iiss$equipment_name[iiss$equipment_type == "amphibious" &
                      iiss$equipment_name == "AMPHIBIOUS" &
                      iiss$unit_name == "LC"] <- "LCU"
#avn
iiss$equipment_type[iiss$equipment_type == "patrol and coastal combatants" &
                      iiss$equipment_name == "AVN"] <- "aircraft"
iiss$equipment_name[iiss$equipment_type == "aircraft" &
                      iiss$equipment_name == "AVN"] <- "TPT"






# Artillery
# mine countermeasures
iiss$equipment_type[iiss$equipment_type == "artillery" &
                      iiss$equipment_subtype == "mine countermeasures"] <- "mine warfare"
# remove arty
iiss$equipment_subtype[iiss$equipment_type == "artillery" &
                         iiss$equipment_subtype == "ARTY"] <- NA
# ssm
iiss$equipment_type[iiss$equipment_type == "artillery" &
                      iiss$equipment_subtype == "SSM"] <- "surface-to-surface missile launchers"
iiss$equipment_type[iiss$equipment_type == "artillery" &
                      iiss$equipment_name == "SSM"] <- "surface-to-surface missile launchers"
# sam
iiss$equipment_type[iiss$equipment_type == "artillery" &
                      iiss$equipment_subtype == "SAM"] <- "air defence"
iiss$equipment_type[iiss$equipment_type == "artillery" &
                      iiss$equipment_name == "SAM"] <- "air defence"
# ad in name
iiss$equipment_type[iiss$equipment_type == "artillery" &
                      iiss$equipment_name == "AD"] <- "air defence"
# coerce arty variants
iiss$equipment_name[iiss$equipment_name == "arty/MRL/mors" |
                      iiss$equipment_name == "MRL/MORs" |
                      iiss$equipment_name == "MRL/mor"] <- "MRL/MOR"
iiss$equipment_name[iiss$equipment_name == "Coastal" |
                      iiss$equipment_name == "COASTAL"] <- "coastal"
# msl
iiss$equipment_type[iiss$equipment_type == "artillery" &
                      iiss$equipment_name == "MSL"] <- "anti-tank/anti-infrastructure"
# atk guns
iiss$equipment_subtype[iiss$equipment_type == "artillery" &
                         iiss$equipment_name == "ATK GUNS"] <- "GUNS"
iiss$equipment_name[iiss$equipment_type == "artillery" &
                      iiss$equipment_subtype == "GUNS" &
                         iiss$equipment_name == "ATK GUNS"] <- "SP"
iiss$equipment_subtype[iiss$equipment_type == "artillery" &
                         iiss$equipment_name == "GUNS"] <- "GUNS"
iiss$equipment_name[iiss$equipment_type == "artillery" &
                      iiss$equipment_subtype == "GUNS" &
                         iiss$equipment_name == "GUNS"] <- NA
# mr;
iiss$equipment_name[iiss$equipment_name == "MR;"] <- "MRL"
# gun mor etc.
iiss$equipment_subtype[iiss$equipment_name == "Combined GUN/MOR"] <- "GUNS/MOR"
iiss$equipment_name[iiss$equipment_subtype == "GUNS/MOR" &
                      iiss$equipment_name == "Combined GUN/MOR"] <- NA

iiss$equipment_subtype[iiss$equipment_name == "GUN"] <- "GUNS"
iiss$equipment_name[iiss$equipment_subtype == "GUNS" &
                      iiss$equipment_name == "GUN"] <- NA

iiss$equipment_subtype[iiss$equipment_name == "GUN/MOR"] <- "GUNS/MOR"
iiss$equipment_name[iiss$equipment_subtype == "GUNS/MOR" &
                      iiss$equipment_name == "GUN/MOR"] <- NA

iiss$equipment_subtype[iiss$equipment_type == "artillery" &
                         iiss$equipment_name == "MOR"] <- "MOR"
iiss$equipment_name[iiss$equipment_type == "artillery" &
                         iiss$equipment_subtype == "MOR" &
                         iiss$equipment_name == "MOR"] <- NA

iiss$equipment_subtype[iiss$equipment_type == "artillery" &
                         (iiss$equipment_name == "MORTAR" |
                            iiss$equipment_name == "MORTARS")] <- "MOR"
iiss$equipment_name[iiss$equipment_type == "artillery" &
                      iiss$equipment_subtype == "MOR" &
                      (iiss$equipment_name == "MORTAR" |
                         iiss$equipment_name == "MORTARS")] <- NA
# mor-sp/towed
iiss$equipment_subtype[iiss$equipment_type == "artillery" &
                         (iiss$equipment_name == "MOR-SP" |
                            iiss$equipment_name == "MOR * SP")] <- "MOR"
iiss$equipment_name[iiss$equipment_type == "artillery" &
                         iiss$equipment_subtype == "MOR" &
                         (iiss$equipment_name == "MOR-SP" |
                            iiss$equipment_name == "MOR * SP")] <- "SP"

iiss$equipment_subtype[iiss$equipment_type == "artillery" &
                         iiss$equipment_name == "MOR-TOWED"] <- "MOR"
iiss$equipment_name[iiss$equipment_type == "artillery" &
                      iiss$equipment_subtype == "MOR" &
                      iiss$equipment_name == "MOR-TOWED"] <- "TOWED"
# mrl-towed
iiss$equipment_name[iiss$equipment_type == "artillery" &
                      iiss$equipment_name == "MRL-TOWED"] <- "TOWED"
# mlr
iiss$equipment_name[iiss$equipment_type == "artillery" &
                      iiss$equipment_name == "MLR"] <- "MRL"
#towed arty
iiss$equipment_name[iiss$equipment_type == "artillery" &
                      iiss$equipment_name == "TOWED ARTY"] <- "TOWED"
# ad arty
iiss$equipment_type[iiss$equipment_type == "artillery" &
                      iiss$equipment_name == "AD arty"] <- "air defence"
#bm21grad
iiss$unit_name[iiss$equipment_type == "artillery" &
                 iiss$equipment_subname == "BM-21Grad"] <- "BM-21 Grad"
iiss$equipment_subname[iiss$equipment_type == "artillery" &
                 iiss$equipment_subname == "BM-21Grad" &
                   iiss$unit_name == "BM-21 Grad"] <- NA
#at49k111
iiss$unit_name[iiss$equipment_type == "artillery" &
                 iiss$equipment_subname == "AT49K111"] <- "AT49K111 Spigot"
iiss$equipment_subname[iiss$equipment_type == "artillery" &
                         iiss$equipment_subname == "AT49K111" &
                         iiss$unit_name == "AT49K111 Spigot"] <- NA
#at4
iiss$unit_name[iiss$equipment_type == "artillery" &
                 iiss$equipment_subname == "AT4"] <- "AT4 Spigot"
iiss$equipment_subname[iiss$equipment_type == "artillery" &
                         iiss$equipment_subname == "AT4" &
                         iiss$unit_name == "AT4 Spigot"] <- NA
#mor
iiss$equipment_subtype[iiss$equipment_type == "artillery" &
                         (iiss$equipment_subname == "MOR" |
                            iiss$equipment_subname == "MOR120mm")] <- "MOR"
iiss$equipment_subname[iiss$equipment_type == "artillery" &
                         iiss$equipment_subtype == "MOR" &
                         iiss$equipment_subname == "MOR"] <- NA
iiss$equipment_subname[iiss$equipment_type == "artillery" &
                         iiss$equipment_subtype == "MOR" &
                         iiss$equipment_subname == "MOR120mm"] <- "120mm"
#r120mm and r81mm
iiss$equipment_subname[iiss$equipment_subname == "R120mm"] <- "120mm"
iiss$equipment_subname[iiss$equipment_subname == "R81mm"] <- "81mm"
iiss$equipment_subname[iiss$equipment_subname == "-107mm"] <- "107mm"
#rl
iiss$equipment_subtype[iiss$equipment_type == "artillery" &
                         iiss$equipment_name == "RL"] <- "RL"
iiss$equipment_name[iiss$equipment_type == "artillery" &
                         iiss$equipment_subtype == "RL" &
                         iiss$equipment_name == "RL"] <- NA
#AD arty
iiss$equipment_type[iiss$equipment_type == "artillery" &
                      (iiss$equipment_name == "AD arty" |
                      iiss$equipment_name == "AD")] <- "air defence"
#ARTY
iiss$equipment_name[iiss$equipment_type == "artillery" &
                      iiss$equipment_name == "ARTY"] <- NA
#ASLT GUNS
iiss$equipment_subtype[iiss$equipment_type == "artillery" &
                         iiss$equipment_name == "ASLT GUNS"] <- "GUNS"
iiss$equipment_name[iiss$equipment_type == "artillery" &
                         iiss$equipment_subtype == "GUNS" &
                         iiss$equipment_name == "ASLT GUNS"] <- NA
#coastal
iiss$equipment_subtype[iiss$equipment_type == "artillery" &
                         iiss$equipment_name == "coastal"] <- "GUNS/MOR"
#tow
iiss$equipment_name[iiss$equipment_type == "artillery" &
                      iiss$equipment_name == "TOW"] <- "TOWED"
#KIFV SPAAG TOWED / KIFV SPAAG SP
iiss$equipment_name[iiss$equipment_type == "artillery" &
                      iiss$equipment_name == "KIFV SPAAG SP"] <- "SP"
iiss$equipment_name[iiss$equipment_type == "artillery" &
                      iiss$equipment_name == "KIFV SPAAG TOWED"] <- "TOWED"
#MPR
iiss$equipment_name[iiss$equipment_type == "artillery" &
                      iiss$equipment_name == "MPR"] <- "MOR"
#AA GUNS / AD
iiss$equipment_type[iiss$equipment_type == "artillery" &
                      iiss$equipment_name == "AA GUNS"] <- "air defence"
iiss$equipment_type[iiss$equipment_type == "artillery" &
                      (iiss$equipment_name == "AD" |
                         iiss$equipment_name == "AD arty")] <- "air defence"
#guns
iiss$equipment_subtype[iiss$equipment_type == "artillery" &
                      iiss$equipment_name == "guns"] <- "GUNS"
iiss$equipment_name[iiss$equipment_type == "artillery" &
                      iiss$equipment_subtype == "GUNS" &
                         iiss$equipment_name == "guns"] <- NA
#mine layer
iiss$equipment_type[iiss$equipment_type == "artillery" &
                         iiss$equipment_subtype == "mine layers"] <- "mine warfare"
#missiles
iiss$equipment_type[iiss$equipment_type == "artillery" &
                      iiss$equipment_subtype == "missiles"] <- "anti-tank/anti-infrastructure"
#coastal
iiss$equipment_name[iiss$equipment_type == "artillery" &
                      iiss$equipment_name == "coastal"] <- NA
#FORTRESS
iiss$equipment_subtype[iiss$equipment_type == "artillery" &
                         iiss$equipment_name == "FORTRESS"] <- "GUNS"
iiss$equipment_name[iiss$equipment_type == "artillery" &
                         iiss$equipment_name == "FORTRESS"] <- NA
#aifv
iiss$equipment_type[iiss$equipment_type == "artillery" &
                      (iiss$equipment_name == "AIFV" |
                         iiss$equipment_name == "APC" |
                         iiss$equipment_name == "MBT")] <- "armoured fighting vehicles"
#light
iiss$equipment_subname[iiss$equipment_type == "artillery" &
                         iiss$equipment_subname == "light"] <- NA



# Bombs

iiss$equipment_name[iiss$equipment_type == "bombs" &
                      iiss$equipment_subtype == "conventional"] <- "conventional"
iiss$equipment_subtype[iiss$equipment_type == "bombs" &
                         iiss$equipment_subtype == "conventional" &
                         iiss$equipment_name == "conventional"] <- NA

iiss$equipment_name[iiss$equipment_type == "bombs" &
                      iiss$equipment_subtype == "laser-guided"] <- "laser-guided"
iiss$equipment_subtype[iiss$equipment_type == "bombs" &
                         iiss$equipment_subtype == "laser-guided" &
                         iiss$equipment_name == "laser-guided"] <- NA

iiss$equipment_name[iiss$equipment_type == "bombs" &
                      iiss$equipment_subtype == "INS/GPS-guided"] <- "INS/GPS-guided"
iiss$equipment_subtype[iiss$equipment_type == "bombs" &
                         iiss$equipment_subtype == "INS/GPS-guided" &
                         iiss$equipment_name == "INS/GPS-guided"] <- NA
# laser-guided in subname
iiss$equipment_name[iiss$equipment_type == "bombs" &
                      iiss$equipment_subname == "Laser-guided"] <- "laser-guided"
iiss$equipment_subname[iiss$equipment_type == "bombs" &
                         iiss$equipment_name == "laser-guided" &
                      iiss$equipment_subname == "Laser-guided"] <- NA
# INS/GPS/GLONASS guided in subtype
iiss$equipment_name[iiss$equipment_type == "bombs" &
                      iiss$equipment_subtype == "INS/GPS/GLONASS guided"] <- "INS/GPS/GLONASS-guided"
iiss$equipment_subtype[iiss$equipment_type == "bombs" &
                         iiss$equipment_subtype == "INS/GPS/GLONASS guided" &
                         iiss$equipment_name == "INS/GPS/GLONASS-guided"] <- NA
# PGM JSOW
iiss$unit_name[iiss$equipment_type == "bombs" &
                      iiss$equipment_name == "JSOW" &
                 iiss$unit_name == "AGM-145"] <- "AGM-154 JSOW"
iiss$unit_name[iiss$equipment_type == "bombs" &
                    iiss$equipment_name == "JSOW" &
                    iiss$unit_name == "AGM-154 JSOW"] <- "conventional"
iiss$equipment_name[iiss$equipment_type == "bombs" &
                      iiss$equipment_subtype == "PGM"] <- "PGM"
iiss$equipment_subtype[iiss$equipment_type == "bombs" &
                      iiss$equipment_name == "PGM" &
                      iiss$equipment_subtype == "PGM"] <- NA




# Engineering and Maintenance Vehicles
iiss$equipment_name[iiss$equipment_type == "engineering and maintenance vehicles" &
                      iiss$equipment_subtype == "AEV"] <- "AEV"
iiss$equipment_subtype[iiss$equipment_type == "engineering and maintenance vehicles" &
                         iiss$equipment_subtype == "AEV"] <- NA
iiss$equipment_name[iiss$equipment_type == "engineering and maintenance vehicles" &
                      iiss$equipment_subtype == "ARV"] <- "ARV"
iiss$equipment_subtype[iiss$equipment_type == "engineering and maintenance vehicles" &
                         iiss$equipment_subtype == "ARV"] <- NA
iiss$equipment_name[iiss$equipment_type == "engineering and maintenance vehicles" &
                      iiss$equipment_subtype == "VLB"] <- "VLB"
iiss$equipment_subtype[iiss$equipment_type == "engineering and maintenance vehicles" &
                         iiss$equipment_subtype == "VLB"] <- NA
# RL
iiss$equipment_type[iiss$equipment_type == "engineering and maintenance vehicles" &
                      iiss$equipment_subtype == "RL"] <- "anti-tank/anti-infrastructure"
#aam in equipment_name
iiss$equipment_type[iiss$equipment_type == "engineering and maintenance vehicles" &
                      iiss$equipment_name == "AAM"] <- "air-launched missiles"




# Helicopters
#MRH/Tpt
iiss$equipment_name[iiss$equipment_name == "MRH/Tpt"] <- "MRH/TPT"
#hellicopters
iiss$equipment_type[iiss$equipment_type == "hellicopters"] <- "helicopters"
# armd
iiss$equipment_subtype[iiss$equipment_type == "helicopters" &
                         iiss$equipment_subtype == "armd"] <- NA
# helicopters
iiss$equipment_subtype[iiss$equipment_type == "helicopters" &
                         iiss$equipment_subtype == "helicopters"] <- NA
# ATK
iiss$equipment_name[iiss$equipment_type == "helicopters" &
                      iiss$equipment_subtype == "ATK"] <- "ATK"
iiss$equipment_subtype[iiss$equipment_type == "helicopters" &
                      iiss$equipment_subtype == "ATK" &
                      iiss$equipment_name == "ATK"] <- NA
# TPT
iiss$equipment_name[iiss$equipment_type == "helicopters" &
                      iiss$equipment_subtype == "TPT"] <- "TPT"
iiss$equipment_subtype[iiss$equipment_type == "helicopters" &
                         iiss$equipment_subtype == "TPT" &
                         iiss$equipment_name == "TPT"] <- NA
# TRG
iiss$equipment_name[iiss$equipment_type == "helicopters" &
                      iiss$equipment_subtype == "TRG"] <- "TRG"
iiss$equipment_subtype[iiss$equipment_type == "helicopters" &
                         iiss$equipment_subtype == "TRG" &
                         iiss$equipment_name == "TRG"] <- NA
# UTL
iiss$equipment_name[iiss$equipment_type == "helicopters" &
                      iiss$equipment_subtype == "UTL"] <- "UTL"
iiss$equipment_subtype[iiss$equipment_type == "helicopters" &
                         iiss$equipment_subtype == "UTL" &
                         iiss$equipment_name == "UTL"] <- NA
# ATK
iiss$equipment_name[iiss$equipment_type == "helicopters" &
                      (iiss$equipment_name == "ATTACK HEL" |
                         iiss$equipment_name == "ATTACK HELICPTERS" |
                         iiss$equipment_name == "ATTACK" |
                         iiss$equipment_name == "ATTACK HEL" |
                         iiss$equipment_name == "ARMED HEL" |
                         iiss$equipment_name == "TAK HEL")] <- "ATK"
# AMPH ASLT
iiss$equipment_name[iiss$equipment_type == "helicopters" &
                      iiss$equipment_name == "AMPH ASLT"] <- "ASLT"
# Bomber
iiss$equipment_type[iiss$equipment_type == "helicopters" &
                      iiss$equipment_name == "Bomber"] <- "aircraft"
# cbt
iiss$equipment_name[iiss$equipment_type == "helicopters" &
                      (iiss$equipment_name == "CBT-SPT" |
                         iiss$equipment_name == "CBT SPT")] <- "CBT/SPT"
# guns
iiss$equipment_type[iiss$equipment_type == "helicopters" &
                      (iiss$equipment_name == "GUNS-TOWED" |
                         iiss$equipment_name == "GUNS-SP")] <- "air defence"
# hel helicopters
iiss$equipment_name[iiss$equipment_type == "helicopters" &
                      (iiss$equipment_name == "HEL" |
                         iiss$equipment_name == "HELICOPTERS")] <- NA
#spt hel
iiss$equipment_name[iiss$equipment_type == "helicopters" &
                      iiss$equipment_name == "SPT HEL"] <- "SPT"
#tpt
iiss$equipment_name[iiss$equipment_type == "helicopters" &
                      (iiss$equipment_name == "tpt" &
                         iiss$equipment_name == "TPt" &
                         iiss$equipment_name == "Transport")] <- "TPT"
#trg
iiss$equipment_name[iiss$equipment_type == "helicopters" &
                      (iiss$equipment_name == "Traning" |
                         iiss$equipment_name == "TRAINING")] <- "TRG"
#ULT
iiss$equipment_name[iiss$equipment_type == "helicopters" &
                      iiss$equipment_name == "ULT"] <- "UTL"
#uav
iiss$equipment_type[iiss$equipment_type == "helicopters" &
                      iiss$equipment_name == "UAV"] <- "unmanned aerial vehicles"
#Bell206
iiss$equipment_subname[iiss$equipment_type == "helicopters" &
                      (iiss$equipment_subname == "BELL212" |
                         iiss$equipment_subname == "BELL206" |
                         iiss$equipment_subname == "OH-58" |
                         iiss$equipment_subname == "S-70" |
                         iiss$equipment_subname == "SA-319AlouetteIII")] <- NA
#ad
iiss$equipment_type[iiss$equipment_type == "helicopters" &
                      iiss$equipment_name == "AD"] <- "air defence"
#attack helicopters
iiss$equipment_name[iiss$equipment_type == "helicopters" &
                      iiss$equipment_name == "ATTACK HELICOPTERS"] <- "ATK"
# transport and tpt and TPt
iiss$equipment_name[iiss$equipment_type == "helicopters" &
                      (iiss$equipment_name == "Transport" |
                         iiss$equipment_name == "tpt" |
                         iiss$equipment_name == "TPt")] <- "TPT"
# Training
iiss$equipment_name[iiss$equipment_type == "helicopters" &
                      iiss$equipment_name == "Training"] <- "TRG"
# MRh
iiss$equipment_name[iiss$equipment_type == "helicopters" &
                      iiss$equipment_name == "MRh"] <- "MRH"
#ASuW
iiss$equipment_name[iiss$equipment_name == "ASuW"] <- "ASUW"
#"MAR SPT"
iiss$equipment_name[iiss$equipment_name == "MAR SPT"] <- "MAR/SPT"
#COMMS(SAR)
iiss$equipment_name[iiss$equipment_type == "helicopters" &
                      iiss$equipment_name == "COMMS(SAR)"] <- "COMMS/SAR"
# remove reserves
b = which(iiss$equipment_type == "helicopters" &
            iiss$equipment_name == "Reserves")
iiss <<- iiss[-b, ]
#utility
iiss$equipment_name[iiss$equipment_type == "helicopters" &
                      iiss$equipment_name == "utility"] <- "UTL"
#pci
iiss$equipment_type[iiss$equipment_type == "helicopters" &
                      iiss$equipment_name == "PCI"] <- "patrol and coastal combatants"
#ac
iiss$equipment_type[iiss$equipment_type == "helicopters" &
                      iiss$equipment_name == "AC"] <- "aircraft"
# clean up subnames
iiss$equipment_subname[iiss$equipment_type == "helicopters" &
                         (iiss$equipment_subname == "(armed)" |
                            iiss$equipment_subname == "(HB-315BGaviao)" |
                            iiss$equipment_subname == "AlouetteIII\n/SA-319\nAlouetteIII" |
                            iiss$equipment_subname == "Bell205" |
                            iiss$equipment_subname == "EMB-111(P-54/P-95B)" |
                            iiss$equipment_subname == "S-76" |
                            iiss$equipment_subname == "TAC" |
                            iiss$equipment_subname == "PAX")] <- NA
#utl>sar
iiss$equipment_name[iiss$equipment_type == "helicopters" &
                      iiss$equipment_name == "UTL" &
                      iiss$equipment_subname == "SAR"] <- "SAR"
iiss$equipment_subname[iiss$equipment_type == "helicopters" &
                      iiss$equipment_name == "SAR" &
                      iiss$equipment_subname == "SAR"] <- NA
#apt
iiss$equipment_name[iiss$equipment_type == "helicopters" &
                      iiss$equipment_name == "APT"] <- "SPT"
#asuw
iiss$equipment_name[iiss$equipment_type == "helicopters" &
                      (iiss$equipment_name == "ASW/ASuW" |
                         iiss$equipment_name == "ASW/ASUV")] <- "ASW/ASUW"
#liaison and VIP
iiss$equipment_name[iiss$equipment_type == "helicopters" &
                      (iiss$equipment_name == "LIAISON" |
                         iiss$equipment_name == "LIAISON/RECCE" |
                         iiss$equipment_name == "VIP" |
                         iiss$equipment_name == "VIP TPT")] <- "TPT"
# unit names
iiss$equipment_name[iiss$equipment_type == "helicopters" &
                      is.na(iiss$equipment_name) &
                      grepl("Hughes", iiss$unit_name)] <- "MRH"
iiss$equipment_name[iiss$equipment_type == "helicopters" &
                      is.na(iiss$equipment_name) &
                      grepl("Bell", iiss$unit_name)] <- "MRH"
iiss$equipment_name[iiss$equipment_type == "helicopters" &
                      is.na(iiss$equipment_name) &
                      grepl("Mi-", iiss$unit_name)] <- "UTL"
iiss$equipment_name[iiss$equipment_type == "helicopters" &
                      is.na(iiss$equipment_name) &
                      grepl("S-5", iiss$unit_name)] <- "UTL"
iiss$equipment_name[iiss$equipment_type == "helicopters" &
                      is.na(iiss$equipment_name) &
                      grepl("Mi-", iiss$unit_name)] <- "UTL"
iiss$equipment_name[iiss$equipment_type == "helicopters" &
                      is.na(iiss$equipment_name) &
                      grepl("A-1", iiss$unit_name)] <- "UTL"
iiss$equipment_name[iiss$equipment_type == "helicopters" &
                      is.na(iiss$equipment_name) &
                      grepl("SAR", iiss$unit_name)] <- "SAR"
iiss$equipment_name[iiss$equipment_type == "helicopters" &
                      is.na(iiss$equipment_name) &
                      grepl("AB-206", iiss$unit_name)] <- "TPT"
iiss$equipment_name[iiss$equipment_type == "helicopters" &
                      is.na(iiss$equipment_name) &
                      grepl("AB-205", iiss$unit_name)] <- "UTL"
iiss$equipment_name[iiss$equipment_type == "helicopters" &
                      is.na(iiss$equipment_name) &
                      grepl("AB-212", iiss$unit_name)] <- "UTL"
iiss$equipment_name[iiss$equipment_type == "helicopters" &
                      is.na(iiss$equipment_name) &
                      grepl("AH-", iiss$unit_name)] <- "ATK"
iiss$equipment_name[iiss$equipment_type == "helicopters" &
                      is.na(iiss$equipment_name) &
                      grepl("AS-", iiss$unit_name)] <- "UTL"
iiss$equipment_name[iiss$equipment_type == "helicopters" &
                      is.na(iiss$equipment_name) &
                      grepl("Lynx", iiss$unit_name)] <- "ASW"
iiss$equipment_name[iiss$equipment_type == "helicopters" &
                      is.na(iiss$equipment_name) &
                      grepl("ouette", iiss$unit_name)] <- "UTL"
iiss$equipment_name[iiss$equipment_type == "helicopters" &
                      is.na(iiss$equipment_name) &
                      grepl("Dauphin", iiss$unit_name)] <- "UTL"
iiss$equipment_name[iiss$equipment_type == "helicopters" &
                      is.na(iiss$equipment_name) &
                      grepl("AB-412", iiss$unit_name)] <- "UTL"
iiss$equipment_name[iiss$equipment_type == "helicopters" &
                      is.na(iiss$equipment_name) &
                      grepl("AB-47", iiss$unit_name)] <- "TRG"
iiss$equipment_name[iiss$equipment_type == "helicopters" &
                      is.na(iiss$equipment_name) &
                      grepl("SA-3", iiss$unit_name)] <- "MRH"
iiss$equipment_name[iiss$equipment_type == "helicopters" &
                      is.na(iiss$equipment_name) &
                      grepl("UH-", iiss$unit_name)] <- "TPT"
iiss$equipment_name[iiss$equipment_type == "helicopters" &
                      is.na(iiss$equipment_name) &
                      grepl("Ecureuil", iiss$unit_name)] <- "TPT"
iiss$equipment_name[iiss$equipment_type == "helicopters" &
                      is.na(iiss$equipment_name) &
                      grepl("Puma", iiss$unit_name)] <- "SPT"
iiss$equipment_name[iiss$equipment_type == "helicopters" &
                      is.na(iiss$equipment_name) &
                      grepl("Z-", iiss$unit_name)] <- "MRH"
iiss$equipment_name[iiss$equipment_type == "helicopters" &
                      is.na(iiss$equipment_name) &
                      grepl("NH-", iiss$unit_name)] <- "UTL"
iiss$equipment_name[iiss$equipment_type == "helicopters" &
                      is.na(iiss$equipment_name) &
                      grepl("Bo-", iiss$unit_name)] <- "UTL"
iiss$equipment_name[iiss$equipment_type == "helicopters" &
                      is.na(iiss$equipment_name) &
                      grepl("BO-", iiss$unit_name)] <- "UTL"
iiss$equipment_name[iiss$equipment_type == "helicopters" &
                      is.na(iiss$equipment_name) &
                      grepl("CH-", iiss$unit_name)] <- "TPT"
iiss$equipment_name[iiss$equipment_type == "helicopters" &
                      is.na(iiss$equipment_name) &
                      grepl("Ka-", iiss$unit_name)] <- "ATK"
iiss$equipment_name[iiss$equipment_type == "helicopters" &
                      is.na(iiss$equipment_name) &
                      grepl("PZL-", iiss$unit_name)] <- "MRH"
iiss$equipment_name[iiss$equipment_type == "helicopters" &
                      is.na(iiss$equipment_name) &
                      grepl("Sea King", iiss$unit_name)] <- "ASW"
iiss$equipment_name[iiss$equipment_type == "helicopters" &
                      is.na(iiss$equipment_name) &
                      grepl("IAR-", iiss$unit_name)] <- "SPT"
iiss$equipment_name[iiss$equipment_type == "helicopters" &
                      is.na(iiss$equipment_name) &
                      grepl("S-70", iiss$unit_name)] <- "TPT"
iiss$equipment_name[iiss$equipment_type == "helicopters" &
                      is.na(iiss$equipment_name) &
                      grepl("MD", iiss$unit_name)] <- "MRH"
iiss$equipment_name[iiss$equipment_type == "helicopters" &
                      is.na(iiss$equipment_name) &
                      grepl("NH-", iiss$unit_name)] <- "TRG"
iiss$equipment_name[iiss$equipment_type == "helicopters" &
                      is.na(iiss$equipment_name) &
                      iiss$unit_name == "AEW-2"] <- "AEW"
iiss$equipment_name[iiss$equipment_type == "helicopters" &
                      is.na(iiss$equipment_name) &
                      iiss$unit_name == "EH-101 Merlin Mk1"] <- "ASW"
iiss$equipment_name[iiss$equipment_type == "helicopters" &
                      is.na(iiss$equipment_name) &
                      iiss$unit_name == "H-34"] <- "ASW"
iiss$equipment_name[iiss$equipment_type == "helicopters" &
                      is.na(iiss$equipment_name) &
                      iiss$unit_name == "HC-4"] <- "ATK"
iiss$equipment_name[iiss$equipment_type == "helicopters" &
                      is.na(iiss$equipment_name) &
                      iiss$unit_name == "HH-3F"] <- "SAR"
iiss$equipment_name[iiss$equipment_type == "helicopters" &
                      is.na(iiss$equipment_name) &
                      iiss$unit_name == "Mo-24"] <- "UTL"
iiss$equipment_name[iiss$equipment_type == "helicopters" &
                      is.na(iiss$equipment_name) &
                      iiss$unit_name == "OH-13HS"] <- "TRG"
iiss$equipment_name[iiss$equipment_type == "helicopters" &
                      is.na(iiss$equipment_name) &
                      iiss$unit_name == "SE-3130"] <- "OCU"
iiss$equipment_name[iiss$equipment_type == "helicopters" &
                      is.na(iiss$equipment_name) &
                      (iiss$unit_name == "SE3160" |
                         iiss$unit_name == "Wessex")] <- "UTL"
iiss$equipment_name[iiss$equipment_type == "helicopters" &
                      is.na(iiss$equipment_name) &
                      (iiss$unit_name == "SH-3D" |
                         iiss$unit_name == "Shabaviz" |
                         iiss$unit_name == "W-3" |
                         iiss$unit_name == "W-3 Sokol")] <- "TPT"

iiss$equipment_type[iiss$equipment_type == "helicopters" &
                      grepl("Tu-", iiss$unit_name)] <- "aircraft"
iiss$equipment_type[iiss$equipment_type == "helicopters" &
                      is.na(iiss$equipment_name) &
                      (iiss$unit_name == "MiG-29" |
                         iiss$unit_name == "Su-17" |
                         iiss$unit_name == "Su-25")] <- "aircraft"
# subnames
iiss$equipment_subname[iiss$equipment_type == "helicopters" &
                         (iiss$equipment_subname == "hel" |
                            iiss$equipment_subname == "SA-319" |
                            iiss$equipment_subname == "SA-330" |
                            iiss$equipment_subname == "PZL")] <- NA
#med
iiss$equipment_subname[iiss$equipment_subname == "med"] <- "medium"
#lt
iiss$equipment_subname[iiss$equipment_subname == "lt"] <- "light"
#asw>RL
iiss$equipment_name[iiss$equipment_type == "helicopters" &
                      iiss$equipment_name == "ASW"] <- "PCI"
iiss$equipment_subname[iiss$equipment_type == "helicopters" &
                      iiss$equipment_name == "ASW"] <- NA
iiss$equipment_type[iiss$equipment_type == "helicopters" &
                      iiss$equipment_subname == "ASW"] <- "patrol and coastal combatants"





# Logistics and support
#CRAFT in subtype
iiss$equipment_subtype[iiss$equipment_type == "logistics and support" &
                       iiss$equipment_subtype == "CRAFT"] <- NA
#naval ship asset
iiss$equipment_name[iiss$equipment_type == "naval ship asset"] <- "naval ship asset"
iiss$equipment_type[iiss$equipment_type == "naval ship asset" &
                      iiss$equipment_name == "naval ship asset"] <- "logistics and support"
iiss$unit_name[iiss$equipment_type == "logistics and support" &
                 iiss$equipment_name == "naval ship asset"] <- "floating dock"

iiss$equipment_name[iiss$equipment_type == "logistics and support" &
                      iiss$equipment_name == "ALR"] <- "AKR"

iiss$equipment_name[iiss$equipment_type == "logistics and support" &
                      iiss$equipment_name == "AX" &
                      iiss$equipment_subname == "AXS"] <- "AXS"
iiss$equipment_subname[iiss$equipment_type == "logistics and support" &
                         iiss$equipment_name == "AXS" &
                         iiss$equipment_subname == "AXS"] <- NA

iiss$equipment_name[iiss$equipment_type == "logistics and support" &
                      iiss$equipment_name == "AX" &
                      iiss$equipment_subname == "AXL"] <- "AXL"
iiss$equipment_subname[iiss$equipment_type == "logistics and support" &
                         iiss$equipment_name == "AXL" &
                         iiss$equipment_subname == "AXL"] <- NA
#move helicopter
iiss$equipment_type[iiss$equipment_type == "logistics and support" &
                      iiss$equipment_subtype == "helicopters"] <- "helicopters"
#remove subtype
iiss$equipment_subtype[iiss$equipment_type == "logistics and support" &
                         (iiss$equipment_subtype == "TRG" |
                            iiss$equipment_subtype == "surv" |
                            iiss$equipment_subtype == "support")] <- NA
#AAM should be ARH
iiss$equipment_name[iiss$equipment_type == "logistics and support" &
                      iiss$equipment_name == "AAM"] <- "ARH"
#move MSL out of logistics and support
iiss$equipment_type[iiss$equipment_type == "logistics and support" &
                      iiss$equipment_name == "MSL" &
                      iiss$equipment_subname == "AAM"] <- "air-launched missiles"
#Coerce tkr
iiss$equipment_name[iiss$equipment_type == "logistics and support" &
                      (iiss$equipment_name == "TANKER LIGHT" |
                         iiss$equipment_name == "tkr")] <- "TKR"
#Coerce tpt
iiss$equipment_name[iiss$equipment_type == "logistics and support" &
                      (iiss$equipment_name == "tpt" |
                         iiss$equipment_name == "Tpt")] <- "TPT"
#Coerce trg
iiss$equipment_name[iiss$equipment_type == "logistics and support" &
                      (iiss$equipment_name == "TRG (sail)" |
                         iiss$equipment_name == "TRG 2 (sail)" |
                         iiss$equipment_name == "sail trg")] <- "TRG"
#arh in subname
iiss$equipment_subname[iiss$equipment_type == "logistics and support" |
                         iiss$equipment_subname == "ARH"] <- NA
#recce arl
iiss$equipment_name[iiss$equipment_type == "logistics and support" &
                      iiss$equipment_name == "RECCE"] <- "ARL"
iiss$equipment_subname[iiss$equipment_type == "logistics and support" &
                      iiss$equipment_name == "ARL" &
                        iiss$equipment_subname == "ARL"] <- NA
#tak(breakbulk)
iiss$equipment_name[iiss$equipment_type == "logistics and support" &
                      iiss$equipment_name == "AK" &
                      (iiss$equipment_subname == "T-AK(breakbulk)" |
                         iiss$equipment_subname == "T-AK(heavylift)")] <- "T-AK"
iiss$equipment_subname[iiss$equipment_type == "logistics and support" &
                      iiss$equipment_name == "AK" &
                      (iiss$equipment_subname == "T-AK(breakbulk)" |
                         iiss$equipment_subname == "T-AK(heavylift)")] <- NA
#upper case
iiss$equipment_name[iiss$equipment_type == "logistics and support" &
                      iiss$equipment_name == "AX (trg)"] <- "AX/TRG"
iiss$equipment_name[iiss$equipment_type == "logistics and support" &
                      iiss$equipment_name == "diving tender"] <- "YDT"
iiss$equipment_name[iiss$equipment_type == "logistics and support" &
                      (iiss$equipment_name == "Diving tender/spt" |
                         iiss$equipment_name == "YDT/spt")] <- "YDT/SPT"
iiss$equipment_name[iiss$equipment_type == "logistics and support" &
                      iiss$equipment_name == "spt"] <- "SPT"
# Royal Yacht
iiss$equipment_name[iiss$equipment_type == "logistics and support" &
                      iiss$equipment_name == "RY Royal Yacht"] <- "RY"
#trg
iiss$equipment_name[iiss$equipment_type == "logistics and support" &
                      iiss$equipment_name == "trg"] <- "TRG"
#ae (ammo)
iiss$equipment_name[iiss$equipment_type == "logistics and support" &
                      iiss$equipment_name == "AE (AMMO)"] <- "AE/AMMO"
#AGI (INT)
iiss$equipment_name[iiss$equipment_type == "logistics and support" &
                      iiss$equipment_name == "AGI (INT)"] <- "AGI/INT"
#AGHS (SVY)
iiss$equipment_name[iiss$equipment_type == "logistics and support" &
                      iiss$equipment_name == "AGHS (SVY)"] <- "AGHS/SVY"
#AH (MED)
iiss$equipment_name[iiss$equipment_type == "logistics and support" &
                      iiss$equipment_name == "AH (MED)"] <- "AH/MED"
#T-AOE (RAS)
iiss$equipment_name[iiss$equipment_type == "logistics and support" &
                      iiss$equipment_name == "T-AOE (RAS)"] <- "T-AOE/RAS"
#TRG AXS
iiss$equipment_name[iiss$equipment_type == "logistics and support" &
                      iiss$equipment_name == "TRG AXS"] <- "TRG/AXS"
#tpt/trg
iiss$equipment_name[iiss$equipment_type == "logistics and support" &
                      iiss$equipment_name == "tpt/trg"] <- "TPT/TRG"
#Craft in name
iiss$equipment_name[iiss$equipment_type == "logistics and support" &
                      iiss$equipment_name == "Craft" &
                      iiss$unit_name == "LCU"] <- "LCU"
iiss$unit_name[iiss$equipment_type == "logistics and support" &
                      iiss$equipment_name == "LCU" &
                      iiss$unit_name == "LCU"] <- NA
#spt
iiss$equipment_name[iiss$equipment_type == "logistics and support" &
                      iiss$equipment_name == "diving spt"] <- "SPT"
iiss$equipment_name[iiss$equipment_type == "logistics and support" &
                      iiss$equipment_name == "spt tkr"] <- "SPT/TKR"
iiss$equipment_name[iiss$equipment_type == "logistics and support" &
                      iiss$equipment_name == "spt/cmd"] <- "SPT/CMD"
#tug
iiss$equipment_name[iiss$equipment_type == "logistics and support" &
                      (iiss$equipment_name == "ocean tug" |
                         iiss$equipment_name == "ocean tugs")] <- "tug"
#trial
iiss$equipment_name[iiss$equipment_type == "logistics and support" &
                      (iiss$equipment_name == "Trial" |
                         iiss$equipment_name == "Trial Ship")] <- "trial"
#tkr/tender
iiss$equipment_name[iiss$equipment_type == "logistics and support" &
                      (iiss$equipment_name == "Water Tender" |
                         iiss$equipment_name == "water tkr")] <- "TKR"
#support and miscellaneous
iiss$equipment_type[iiss$equipment_type == "Support and Miscellaneous"] <- "logistics and support"
#TRIAL
iiss$equipment_name[iiss$equipment_type == "logistics and support" &
                      iiss$equipment_name == "TRIAL"] <- "trial"
#Craft
iiss$equipment_name[iiss$equipment_type == "logistics and support" &
                      iiss$equipment_name == "Craft"] <- NA
# move subtypes
iiss$equipment_subtype[iiss$equipment_type == "logistics and support" &
                         iiss$equipment_subtype == "landing craft"] <- NA

iiss$equipment_name[iiss$equipment_type == "logistics and support" &
                         iiss$equipment_subtype == "coastal"] <- "AOT"
iiss$equipment_subtype[iiss$equipment_type == "logistics and support" &
                      iiss$equipment_subtype == "coastal" &
                      iiss$equipment_name == "AOT"] <- NA

iiss$unit_name[iiss$equipment_type == "logistics and support" &
                 iiss$equipment_subtype == "coastal tug"] <- "coastal tug"
iiss$equipment_subtype[iiss$equipment_type == "logistics and support" &
                 iiss$equipment_subtype == "coastal tug"] <- NA

iiss$unit_name[iiss$equipment_type == "logistics and support" &
                 iiss$equipment_subtype == "ocean tug"] <- "ocean tug"
iiss$equipment_subtype[iiss$equipment_type == "logistics and support" &
                         iiss$equipment_subtype == "ocean tug"] <- NA

iiss$unit_name[iiss$equipment_type == "logistics and support" &
                 iiss$equipment_subtype == "repair"] <- "repair"
iiss$equipment_subtype[iiss$equipment_type == "logistics and support" &
                         iiss$equipment_subtype == "repair"] <- NA
# marine
iiss$equipment_type[iiss$equipment_type == "marine"] <- "logistics and support"














# Patrol and coastal combatants
#MISC in subtype
iiss$equipment_subtype[iiss$equipment_type == "patrol and coastal combatants" &
                         iiss$equipment_subtype == "MISC"] <- "misc boats/craft"
#PBR in subtype
iiss$equipment_name[iiss$equipment_type == "patrol and coastal combatants" &
                      iiss$equipment_subtype == "PCR"] <- "PCR"
iiss$equipment_subtype[iiss$equipment_type == "patrol and coastal combatants" &
                      iiss$equipment_subtype == "PCR" &
                      iiss$equipment_name == "PCR"] <- NA
#PCR in subtype
iiss$equipment_name[iiss$equipment_type == "patrol and coastal combatants" &
                      iiss$equipment_subtype == "PBR"] <- "PBR"
iiss$equipment_subtype[iiss$equipment_type == "patrol and coastal combatants" &
                         iiss$equipment_subtype == "PBR" &
                         iiss$equipment_name == "PBR"] <- NA
# patrol craft in equipment_type
iiss$equipment_type[iiss$equipment_type == "patrol, inshore" |
                      iiss$equipment_type == "patrol craft, inshore"] <- "patrol and coastal combatants"
# mis-spelled
iiss$equipment_type[iiss$equipment_type == "patrol and coastal combatancts" |
                      iiss$equipment_type == "patrol and costal combatants" |
                      iiss$equipment_type == "postal and costal combatants"] <- "patrol and coastal combatants"
# move to mine warfare
iiss$equipment_type[iiss$equipment_type == "patrol and coastal combatants" &
                      iiss$equipment_subtype == "mine countermeasures"] <- "mine warfare"

iiss$equipment_subname[iiss$equipment_type == "patrol and coastal combatants" &
                         iiss$equipment_subname == "IR/SARH"] <- NA
#corvettes
iiss$equipment_subtype[iiss$equipment_type == "patrol and coastal combatants" &
                         iiss$equipment_name == "Corvettes"] <- "corvettes"
iiss$equipment_name[iiss$equipment_type == "patrol and coastal combatants" &
                      (iiss$equipment_subtype == "corvettes" |
                         iiss$equipment_name == "Corvettes")] <- NA
#40mm + 76mm
iiss$equipment_type[iiss$equipment_type == "patrol and coastal combatants" &
                      (iiss$equipment_subname == "76mm" |
                         iiss$equipment_subname == "40mm")] <- "air defence"
#1
iiss$equipment_subname[iiss$equipment_type == "patrol and coastal combatants" &
                         iiss$equipment_subname == "1.0"] <- NA
#base koper pci
iiss$equipment_name[iiss$equipment_type == "patrol and coastal combatants" &
                      iiss$equipment_name == "BASE" &
                      iiss$equipment_subname == "Koper" &
                      iiss$unit_name == "PCI"] <- "PCI"
iiss$equipment_subname[iiss$equipment_type == "patrol and coastal combatants" &
                      iiss$equipment_name == "PCI" &
                      iiss$equipment_subname == "Koper" &
                      iiss$unit_name == "PCI"] <- NA
iiss$unit_name[iiss$equipment_type == "patrol and coastal combatants" &
                      iiss$equipment_name == "PCI" &
                      is.na(iiss$equipment_subname) &
                      iiss$unit_name == "PCI"] <- "PCI"
#inshore
iiss$equipment_subtype[iiss$equipment_type == "patrol and coastal combatants" &
                         (iiss$equipment_name == "patrol inshore" |
                            iiss$equipment_name == "patrol, coastal/inshore" |
                            iiss$equipment_name == "patrol, inshore" |
                            iiss$equipment_subtype == "Patrol, Inshore" |
                            iiss$equipment_subtype == "inshore patrol")] <- "inshore"
iiss$equipment_name[iiss$equipment_type == "patrol and coastal combatants" &
                         iiss$equipment_subtype == "inshore" &
                         (iiss$equipment_name == "patrol inshore" |
                            iiss$equipment_name == "patrol, coastal/inshore" |
                            iiss$equipment_name == "patrol, inshore")] <- NA
#offshore
iiss$equipment_subtype[iiss$equipment_type == "patrol and coastal combatants" &
                         (iiss$equipment_name == "patrol coastal" |
                            iiss$equipment_subtype == "coastal patrol")] <- "offshore"
iiss$equipment_name[iiss$equipment_type == "patrol and coastal combatants" &
                      iiss$equipment_subtype == "offshore" &
                      iiss$equipment_name == "patrol coastal"] <- NA
#misc boats
iiss$equipment_subtype[iiss$equipment_type == "patrol and coastal combatants" &
                         (iiss$equipment_name == "MISC Boats/craft" |
                            iiss$equipment_name == "MISC BOATS/CRAFTS" |
                            iiss$equipment_name == "Misc Boats/Craft"|
                            iiss$equipment_name == "boats/craft" |
                            iiss$equipment_name == "misc boats/craft")] <- "misc boats/craft"
iiss$equipment_name[iiss$equipment_type == "patrol and coastal combatants" &
                         iiss$equipment_subtype == "misc boats/craft" &
                         (iiss$equipment_name == "MISC Boats/craft" |
                            iiss$equipment_name == "MISC BOATS/CRAFTS" |
                            iiss$equipment_name == "Misc Boats/Craft"|
                            iiss$equipment_name == "boats/craft" |
                            iiss$equipment_name == "misc boats/craft")] <- NA
#ice patrol
iiss$equipment_subtype[iiss$equipment_type == "patrol and coastal combatants" &
                         iiss$equipment_name == "ICE PATROL"] <- "ice patrol"
iiss$equipment_name[iiss$equipment_type == "patrol and coastal combatants" &
                      iiss$equipment_subtype == "ice patrol" &
                         iiss$equipment_name == "ICE PATROL"] <- NA
#missile craft
iiss$equipment_subtype[iiss$equipment_type == "patrol and coastal combatants" &
                         iiss$equipment_name == "missile craft"] <- "missile craft"
iiss$equipment_name[iiss$equipment_type == "patrol and coastal combatants" &
                      iiss$equipment_subtype == "missile craft" &
                      iiss$equipment_name == "missile craft"] <- NA
#torpedo craft
iiss$equipment_subtype[iiss$equipment_type == "patrol and coastal combatants" &
                         iiss$equipment_name == "torpedo craft"] <- "torpedo craft"
iiss$equipment_name[iiss$equipment_type == "patrol and coastal combatants" &
                      iiss$equipment_subtype == "torpedo craft" &
                      iiss$equipment_name == "torpedo craft"] <- NA
iiss$equipment_subtype[iiss$equipment_type == "patrol and coastal combatants" &
                         iiss$equipment_subtype == "Torpedo craft"] <- "torpedo craft"
#PC1
iiss$equipment_name[iiss$equipment_type == "patrol and coastal combatants" &
                      (iiss$equipment_name == "PC1<" |
                         iiss$equipment_name == "PCI<")] <- "PCI"
#coastal defence
iiss$equipment_subtype[iiss$equipment_type == "patrol and coastal combatants" &
                         (iiss$equipment_subtype == "coastal defence" |
                            iiss$equipment_subtype == "Patrol craft")] <- "patrol craft"
iiss$equipment_name[iiss$equipment_type == "patrol and coastal combatants" &
                      iiss$equipment_subname == "PCI"] <- "PCI"
iiss$equipment_subname[iiss$equipment_type == "patrol and coastal combatants" &
                         iiss$equipment_name == "PCI" &
                      iiss$equipment_subname == "PCI"] <- NA
#toppedo
iiss$equipment_subtype[iiss$equipment_type == "patrol and coastal combatants" &
                         (iiss$equipment_name == "TOPPEDO" |
                            iiss$equipment_subtype == "Torpedo Craft")] <- "torpedo craft"
iiss$equipment_name[iiss$equipment_type == "patrol and coastal combatants" &
                         iiss$equipment_subtype == "torpedo craft" &
                         iiss$equipment_name == "TOPPEDO"] <- NA
#boats/craft
iiss$equipment_subtype[iiss$equipment_subtype == "boats/craft"] <- "misc boats/craft"
#miscboats/craft in subname
iiss$equipment_subtype[iiss$equipment_type == "patrol and coastal combatants" &
                        iiss$equipment_subname == "miscboats/craft"] <- "misc boats/craft"
iiss$equipment_subname[iiss$equipment_type == "patrol and coastal combatants" &
                        iiss$equipment_subtype == "misc boats/craft" &
                        iiss$equipment_subname == "miscboats/craft"] <- NA
#river in subname
iiss$equipment_subtype[iiss$equipment_type == "patrol and coastal combatants" &
                         iiss$equipment_subname == "river"] <- "riverine"
iiss$equipment_subname[iiss$equipment_type == "patrol and coastal combatants" &
                         iiss$equipment_subtype == "riverine" &
                         iiss$equipment_subname == "river"] <- NA
#PCC in stsml
iiss$equipment_type[iiss$equipment_type == "surface-to-surface missile launchers" &
                      iiss$equipment_name == "PCC"] <- "patrol and coastal combatants"
iiss$equipment_subtype[iiss$equipment_type == "patrol and coastal combatants" &
                         iiss$equipment_subtype == "missiles" &
                         iiss$equipment_name == "PCC"] <- NA
#FSG FF move to psc
iiss$equipment_type[iiss$equipment_type == "patrol and coastal combatants" &
                      iiss$equipment_name == "FSG" &
                      iiss$equipment_subname == "FF"] <- "principal surface combatants"
# Fix typo for IUW
iiss$equipment_name[iiss$equipment_name == "IUW"] <- "MIUW"
#Missile Craft
iiss$equipment_subtype[iiss$equipment_type == "patrol and coastal combatants" &
                      iiss$equipment_subtype == "Missile Craft"] <- "missile craft"
iiss$equipment_name[iiss$equipment_type == "patrol and coastal combatants" &
                      iiss$equipment_name == "PCI-10"] <- "PCI"
# marine in subname
iiss$equipment_subtype[iiss$equipment_type == "patrol and coastal combatants" &
                         iiss$equipment_name == "marine"] <- "misc boats/craft"
iiss$equipment_name[iiss$equipment_type == "patrol and coastal combatants" &
                      iiss$equipment_subtype == "misc boats/craft" &
                         iiss$equipment_name == "marine"] <- NA
#pacc>patrol craft>inshore
iiss$equipment_subtype[iiss$equipment_type == "patrol and coastal combatants" &
                         iiss$equipment_subtype == "patrol craft" &
                         iiss$equipment_name == "Inshore"] <- "inshore"
iiss$equipment_subtype[iiss$equipment_type == "patrol and coastal combatants" &
                         iiss$equipment_subtype == "inshore" &
                         iiss$equipment_name == "Inshore"] <- "PCI"
#pacc>patrol craft>riverine
iiss$equipment_subtype[iiss$equipment_type == "patrol and coastal combatants" &
                         iiss$equipment_subtype == "patrol craft" &
                         iiss$equipment_name == "Riverine"] <- "riverine"
iiss$equipment_subtype[iiss$equipment_type == "patrol and coastal combatants" &
                         iiss$equipment_subtype == "riverine" &
                         iiss$equipment_name == "Riverine"] <- "PCR"
# SAM
a = which(iiss$equipment_type == "patrol and coastal combatants" &
                      iiss$equipment_name == "SAM")
iiss <<- iiss[-a, ]
#pco/pCC/pci
iiss$equipment_name[iiss$equipment_type == "patrol and coastal combatants" &
                      iiss$equipment_name == "pco"] <- "PCO"
iiss$equipment_name[iiss$equipment_type == "patrol and coastal combatants" &
                      iiss$equipment_name == "pCC"] <- "PCC"
iiss$equipment_name[iiss$equipment_type == "patrol and coastal combatants" &
                      iiss$equipment_name == "pci"] <- "PCI"
#27mm/32mmm/6.0
iiss$equipment_subname[iiss$equipment_type == "patrol and coastal combatants" &
                         (iiss$equipment_subname == "27mm" |
                            iiss$equipment_subname == "32mm" |
                            iiss$equipment_subname == "6.0")] <- NA
#coastal
iiss$equipment_subtype[iiss$equipment_type == "patrol and coastal combatants" &
                         iiss$equipment_subtype == "coastal"] <- "patrol craft"
#pacc>pci>inshore
iiss$equipment_subtype[iiss$equipment_type == "patrol and coastal combatants" &
                         iiss$equipment_subtype == "PCI"] <- "inshore"
iiss$equipment_name[iiss$equipment_type == "patrol and coastal combatants" &
                         iiss$equipment_subtype == "inshore" &
                         iiss$equipment_name == "Inshore"] <- "PCI"
#pacc>patrol craft>coastal
iiss$equipment_subtype[iiss$equipment_type == "patrol and coastal combatants" &
                         iiss$equipment_subtype == "patrol craft" &
                         iiss$equipment_name == "coastal"] <- "inshore"
iiss$equipment_name[iiss$equipment_type == "patrol and coastal combatants" &
                         iiss$equipment_subtype == "inshore" &
                         iiss$equipment_name == "coastal"] <- "PCI"
#pacc>destroyers
iiss$equipment_type[iiss$equipment_type == "patrol and coastal combatants" &
                      (iiss$equipment_name == "destroyers" |
                         iiss$equipment_name == "frigates")] <- "principal surface combatants"
# PFC
iiss$equipment_type[iiss$equipment_name == "PFC"] <- "patrol and coastal combatants"
iiss$equipment_subtype[iiss$equipment_name == "PFC"] <- NA
#FSU
iiss$equipment_type[iiss$equipment_name == "FSU"] <- "mine warfare"
iiss$equipment_subtype[iiss$equipment_name == "FSU"] <- "mine countermeasures"
iiss$equipment_name[iiss$equipment_type == "mine warfare" &
                      iiss$equipment_subtype == "mine countermeasures" &
                      iiss$equipment_name == "FSU"] <- "MSO"
#ssm
iiss$equipment_type[iiss$equipment_type == "patrol and coastal combatants" &
                      iiss$equipment_name == "SSM" &
                      iiss$unit_name == "MM-38 Exocet"] <- "surface-to-surface missile launchers"
iiss$equipment_subtype[iiss$equipment_type == "patrol and coastal combatants" &
                         iiss$equipment_name == "SSM"] <- "missile craft"
iiss$equipment_name[iiss$equipment_type == "patrol and coastal combatants" &
                      iiss$equipment_subtype == "missile craft" &
                         iiss$equipment_name == "SSM"] <- "PFM"
#ob
iiss$equipment_name[iiss$equipment_type == "patrol and coastal combatants" &
                      iiss$equipment_name == "OB"] <- "PB"
#pcd
iiss$equipment_name[iiss$equipment_type == "patrol and coastal combatants" &
                      iiss$equipment_name == "PCD"] <- "PCC"
#phsc
iiss$equipment_name[iiss$equipment_type == "patrol and coastal combatants" &
                      iiss$equipment_name == "PHSC"] <- "PCO"
#prc
iiss$equipment_name[iiss$equipment_type == "patrol and coastal combatants" &
                      iiss$equipment_name == "PRC"] <- "PFM"
#mcm and msi
iiss$equipment_type[iiss$equipment_type == "patrol and coastal combatants" &
                      (iiss$equipment_name == "MCM" |
                         iiss$equipment_name == "MSI")] <- "mine warfare"
iiss$equipment_subtype[iiss$equipment_type == "mine warfare" &
                      (iiss$equipment_name == "MCM" |
                         iiss$equipment_name == "MSI")] <- "mine countermeasures"
# frigates and destroyers
iiss$equipment_type[iiss$equipment_type == "patrol and coastal combatants" &
                      (iiss$equipment_subtype == "frigates" |
                         iiss$equipment_subtype == "destroyers")] <- "principal surface combatants"
# unit name coercion
iiss$equipment_subtype[iiss$equipment_type == "patrol and coastal combatants" &
                      is.na(iiss$equipment_name) &
                      iiss$unit_name %in% c("armed boats", "aux", "boat", "boats", "coverted civilian craft", "converted fishing boat",
                                            "coverted fishing boats", "fishing craft", "chartered", "hovercraft", "less than 100 tones",
                                            "other", "patrol craft", "ramped lighters", "tugs", "Tjeld", "Victoria")] <- "misc boats/craft"

iiss$equipment_subtype[is.na(iiss$equipment_type) &
                         is.na(iiss$equipment_name) &
                         iiss$unit_name %in% c("hovercraft")] <- "misc boats/craft"

iiss$equipment_type[is.na(iiss$equipment_type) &
                         is.na(iiss$equipment_name) &
                         iiss$unit_name %in% c("hovercraft")] <- "patrol and coastal combatants"

iiss$equipment_name[iiss$equipment_type == "patrol and coastal combatants" &
                         is.na(iiss$equipment_name) &
                         iiss$unit_name %in% c("Cabo Blanco (US Swift 65)", "CG27", "CG29", "Nisr", "Patra", "Shanghai II", "Swiftships",
                                               "Vosper 75")] <- "PB"

iiss$equipment_name[iiss$equipment_type == "patrol and coastal combatants" &
                      is.na(iiss$equipment_name) &
                      iiss$unit_name %in% c("Grisha-II", "Grisha-III")] <- "FS"

iiss$equipment_name[iiss$equipment_type == "patrol and coastal combatants" &
                      is.na(iiss$equipment_name) &
                      iiss$unit_name %in% c("Turk")] <- "PCC"

iiss$equipment_name[iiss$equipment_type == "patrol and coastal combatants" &
                      is.na(iiss$equipment_name) &
                      iiss$unit_name %in% c("PCH")] <- "PCH"

iiss$equipment_name[iiss$equipment_type == "patrol and coastal combatants" &
                      is.na(iiss$equipment_name) &
                      iiss$unit_name %in% c("PCR", "Piyavka", "Vosh", "Yaz")] <- "PCR"

iiss$equipment_name[iiss$equipment_type == "patrol and coastal combatants" &
                      is.na(iiss$equipment_name) &
                      iiss$unit_name %in% c("Osa", "Osa-I", "Osa/Komar", "Dvora", "ex-Sov Turya", "Sov Turya", "Tuima (Sov Osa-II)")] <- "PFI"

iiss$equipment_name[is.na(iiss$equipment_type) &
                      iiss$equipment_name == "other" &
                      iiss$unit_name %in% c("Dvora")] <- "PFI"

iiss$equipment_name[iiss$equipment_type == "patrol and coastal combatants" &
                         is.na(iiss$equipment_name) &
                         iiss$unit_name %in% c("Krivak-III", "Kronshtadt", "T-58")] <- "PCO"

iiss$equipment_name[iiss$equipment_type == "patrol and coastal combatants" &
                      is.na(iiss$equipment_name) &
                      iiss$unit_name %in% c("Pauk")] <- "PCM"

iiss$equipment_name[iiss$equipment_type == "patrol and coastal combatants" &
                      is.na(iiss$equipment_name) &
                      iiss$unit_name %in% c("ex-Sw Coast Guard PCI", "PCI<", "Sw Coast Guard PCI<", "Independence/Sovereignty", "Kandor II", "Kano", "Kondor",
                                            "ex-GDR Kondor-I", "P1903", "SO-1", "Sov-SO1", "Svetlyak", "T-43", "Yunnan", "Zhuk")] <- "PCI"

iiss$equipment_name[iiss$equipment_type == "patrol and coastal combatants" &
                      is.na(iiss$equipment_name) &
                      iiss$unit_name %in% c("Mukha", "Muravey")] <- "PHT"

iiss$equipment_name[iiss$equipment_type == "patrol and coastal combatants" &
                      is.na(iiss$equipment_name) &
                      iiss$unit_name %in% c("Shmel", "US PBR")] <- "PBR"

iiss$equipment_name[iiss$equipment_type == "patrol and coastal combatants" &
                      is.na(iiss$equipment_name) &
                      iiss$unit_name %in% c("Sov Bogomol", "Stenka")] <- "PBF"

iiss$equipment_subtype[iiss$equipment_type == "patrol and coastal combatants" &
                      is.na(iiss$equipment_name) &
                      iiss$unit_name %in% c("HWT", "Kralj", "Storion")] <- "missile craft"
#MISC
iiss$equipment_type[iiss$equipment_type == "MISC" |
                      iiss$equipment_type == "patrol vessels"] <- "patrol and coastal combatants"
# PCF and PCI
iiss$equipment_name[iiss$equipment_name == "PCF and PCI"] <- "PCF/PCI"
# river patrol craft
iiss$equipment_name[iiss$equipment_name == "RIVER PATROL CRAFT"|
                      iiss$equipment_name == "riverine"] <- "PCR"
#sam
iiss$equipment_type[iiss$equipment_type == "patrol and coastal combatants" &
                      iiss$equipment_name == "SAM"] <- "air defence"






# Principal surface comabatants
#DDG as equipment_name
iiss$equipment_type[iiss$equipment_type == "DDG"] <- "principal surface combatants"
#frigates and cruisers
iiss$equipment_subtype[iiss$equipment_name == "FFH" |
                         iiss$equipment_name == "FFGHM"] <- "frigates"
iiss$equipment_name[iiss$equipment_subtype == "frigates" &
                      is.na(iiss$equipment_name)] <- "FFGHM"
iiss$equipment_subname[iiss$equipment_type == "principal surface combatants" &
                         iiss$equipment_subname == "CRUISERS"] <- "cruisers"
# mine countermeasures
iiss$equipment_type[iiss$equipment_type == "principal surface combatants" &
                      iiss$equipment_subtype == "mine countermeasures"] <- "mine warfare"
# missile craft
iiss$equipment_type[iiss$equipment_type == "principal surface combatants" &
                      iiss$equipment_subtype == "missile craft"] <- "patrol and coastal combatants"
# CRUISERS
iiss$equipment_subtype[iiss$equipment_type == "principal surface combatants" &
                         iiss$equipment_subtype == "CRUISERS"] <- "cruisers"
# carriers
iiss$equipment_subtype[iiss$equipment_type == "principal surface combatants" &
                         iiss$equipment_subtype == "carriers"] <- "aircraft carriers"
#battleship
iiss$equipment_subtype[iiss$equipment_type == "principal surface combatants" &
                      iiss$equipment_subtype == "battleship"] <- "battleships"
#frigate in equipment type
iiss$equipment_subtype[iiss$equipment_type == "frigates" |
                         iiss$equipment_type == "Frigates"] <- "frigates"
iiss$equipment_type[(iiss$equipment_type == "frigates" |
                       iiss$equipment_type == "Frigates") &
                      iiss$equipment_subtype == "frigates"] <- "principal surface combatants"
#MCM in equipment type
iiss$equipment_name[iiss$equipment_type == "MCM"] <- "MCM"
iiss$equipment_type[iiss$equipment_type == "MCM" &
                      iiss$equipment_name == "MCM"] <- "patrol and coastal combatants"
#NA in Timor leste
iiss$equipment_type[iiss$equipment_type == "NA"] <- NA
iiss$rawtext[iiss$country == "timor leste" &
               iiss$rawtext == "NA"] <- "No equipment information given"
#corvette in subtype
iiss$equipment_type[iiss$equipment_type == "principal surface combatants" &
                      iiss$equipment_subtype == "corvettes"] <- "patrol and coastal combatants"
#pcc
iiss$equipment_type[iiss$equipment_type == "principal surface combatants" &
                      iiss$equipment_name == "PCC"] <- "patrol and coastal combatants"
iiss$equipment_subtype[iiss$equipment_type == "patrol and coastal combatants" &
                      iiss$equipment_name == "PCC"] <- "coastal"
#pbr
iiss$equipment_type[iiss$equipment_type == "principal surface combatants" &
                      iiss$equipment_name == "PBR"] <- "patrol and coastal combatants"
iiss$equipment_subtype[iiss$equipment_type == "patrol and coastal combatants" &
                         iiss$equipment_name == "PBR"] <- "riverine"
#pci
iiss$equipment_type[iiss$equipment_type == "principal surface combatants" &
                      iiss$equipment_name == "PCI"] <- "patrol and coastal combatants"
iiss$equipment_subtype[iiss$equipment_type == "patrol and coastal combatants" &
                         iiss$equipment_name == "PCI"] <- "inshore"
#mco
iiss$equipment_type[iiss$equipment_type == "principal surface combatants" &
                      iiss$equipment_name == "MCO"] <- "mine warfare"
iiss$equipment_subtype[iiss$equipment_type == "mine warfare" &
                         iiss$equipment_name == "MCO"] <- "mine countermeasures"
#PCC
iiss$equipment_type[iiss$equipment_type == "principal surface combatants" &
                      iiss$equipment_name == "PCC"] <- "patrol and coastal combatants"
# unit name coercion
iiss$equipment_name[iiss$equipment_type == "principal surface combatants" &
                      is.na(iiss$equipment_name) &
                      iiss$unit_name %in% c("Petya III", "Uzhgorod (Petya II)")] <- "FF"
iiss$equipment_subtype[iiss$equipment_type == "principal surface combatants" &
                      is.na(iiss$equipment_name) &
                      iiss$unit_name %in% c("Petya III", "Uzhgorod (Petya II)")] <- "frigates"

iiss$equipment_name[iiss$equipment_type == "principal surface combatants" &
                      is.na(iiss$equipment_name) &
                      iiss$unit_name %in% c("Hetman")] <- "FFHM"
iiss$equipment_subtype[iiss$equipment_type == "principal surface combatants" &
                         is.na(iiss$equipment_name) &
                         iiss$unit_name %in% c("Hetman")] <- "frigates"

iiss$equipment_name[iiss$equipment_type == "principal surface combatants" &
                      is.na(iiss$equipment_name) &
                      iiss$unit_name %in% c("Lutsk")] <- "FS"
iiss$equipment_subtype[iiss$equipment_type == "principal surface combatants" &
                         is.na(iiss$equipment_name) &
                         iiss$unit_name %in% c("Lutka")] <- "corvettes"
#fsg in subname
iiss$equipment_subname[iiss$equipment_type == "principal surface combatants" &
                         (iiss$equipment_subname == "FF" |
                            iiss$equipment_subname == "FGA" |
                            iiss$equipment_subname == "trg")] <- NA







# Radars

iiss$equipment_subname[iiss$equipment_type == "radars" &
                         iiss$equipment_subtype == "land"] <- "land"
iiss$equipment_subtype[iiss$equipment_type == "radars" &
                         iiss$equipment_subtype == "land" &
                         iiss$equipment_subname == "land"] <- NA

iiss$equipment_subname[iiss$equipment_type == "radars" &
                         iiss$equipment_subtype == "tac"] <- "tactical"
iiss$equipment_subtype[iiss$equipment_type == "radars" &
                         iiss$equipment_subtype == "tac" &
                         iiss$equipment_subname == "tactical"] <- NA

iiss$equipment_subname[iiss$equipment_type == "radars" &
                         iiss$equipment_subtype == "long-range"] <- "long range"
iiss$equipment_subtype[iiss$equipment_type == "radars" &
                         iiss$equipment_subtype == "long-range" &
                         iiss$equipment_subname == "long range"] <- NA

iiss$equipment_subname[iiss$equipment_type == "radars" &
                         iiss$equipment_subtype == "strategic"] <- "strategic"
iiss$equipment_name[iiss$equipment_type == "radars" &
                      iiss$equipment_name == "strategic" &
                      iiss$equipment_subname == "strategic"] <- NA

iiss$equipment_subtype[iiss$equipment_type == "radars" &
                         iiss$equipment_subtype == "strategic" &
                         iiss$equipment_subname == "strategic"] <- NA

iiss$equipment_subtype[iiss$equipment_type == "radars" &
                       (iiss$equipment_subname == "land" |
                         iiss$equipment_subname == "strategic" |
                         iiss$equipment_subname == "tactical" |
                         iiss$equipment_subname == "long range")] <- "AD"
# strategic
iiss$equipment_subname[iiss$equipment_type == "radars" &
                         iiss$equipment_name == "strategic"] <- "strategic"
iiss$equipment_name[iiss$equipment_type == "radars" &
                      iiss$equipment_name == "strategic"] <- NA

iiss$equipment_name[iiss$equipment_type == "radars" &
                      iiss$equipment_name == "AShM"] <- NA
iiss$equipment_subtype[iiss$equipment_type == "radars" &
                         iiss$equipment_name == "AD RADARS"] <- "AD"
iiss$equipment_name[iiss$equipment_type == "radars" &
                      iiss$equipment_name == "AD RADARS"] <- NA

iiss$equipment_subname[iiss$equipment_type == "radars" &
                         iiss$equipment_subname == "35mm"] <- NA
# shortrange and longrange
iiss$equipment_subname[iiss$equipment_subname == "longrange"] <- "long-range"
iiss$equipment_subname[iiss$equipment_subname == "shortrange"] <- "short-range"
#land and tactical and strategic in subtype
iiss$equipment_subname[iiss$equipment_type == "radars" &
                         iiss$equipment_subtype == "LAND"] <- "land"
iiss$equipment_subtype[iiss$equipment_type == "radars" &
                         iiss$equipment_subtype == "LAND" &
                         iiss$equipment_subname == "land"] <- NA
iiss$equipment_subname[iiss$equipment_type == "radars" &
                         iiss$equipment_subtype == "STRATEGIC"] <- "strategic"
iiss$equipment_subtype[iiss$equipment_type == "radars" &
                         iiss$equipment_subtype == "STRATEGIC" &
                         iiss$equipment_subname == "strategic"] <- NA
iiss$equipment_subname[iiss$equipment_type == "radars" &
                         iiss$equipment_subtype == "tactical"] <- "tactical"
iiss$equipment_subtype[iiss$equipment_type == "radars" &
                         iiss$equipment_subtype == "tactical" &
                         iiss$equipment_subname == "tactical"] <- NA
#ad raders
iiss$equipment_subtype[iiss$equipment_type == "radars" &
                      iiss$equipment_name == "AD RADER"] <- "AD"
iiss$equipment_name[iiss$equipment_type == "radars" &
                         iiss$equipment_subtype == "AD" &
                         iiss$equipment_name == "AD RADER"] <- NA
#ssm
iiss$equipment_type[iiss$equipment_type == "radars" &
                      iiss$equipment_name == "SSM"] <- "surface-to-surface missile launchers"
#space based systems in name
iiss$equipment_subtype[iiss$equipment_type == "radars" &
                         iiss$equipment_name == "Space Based Systems" &
                         iiss$equipment_subname == "satellites"] <- "space based systems"
iiss$equipment_name[iiss$equipment_type == "radars" &
                         iiss$equipment_subtype == "space based systems" &
                         iiss$equipment_name == "Space Based Systems" &
                         iiss$equipment_subname == "satellites"] <- "satellites"
iiss$equipment_subname[iiss$equipment_type == "radars" &
                      iiss$equipment_subtype == "space based systems" &
                      iiss$equipment_name == "satellites" &
                      iiss$equipment_subname == "satellites"] <- NA
# AD in subtype
iiss$equipment_subtype[iiss$equipment_type == "radars" &
                         iiss$equipment_subtype == "AD"] <- "surv"
# imagery 1
iiss$equipment_name[iiss$equipment_type == "radars" &
                      iiss$equipment_name == "Imagery 1"] <- "imagery"
# land in name
iiss$equipment_subname[iiss$equipment_type == "radars" &
                         iiss$equipment_name == "land"] <- "land"
iiss$equipment_name[iiss$equipment_type == "radars" &
                         iiss$equipment_name == "land" &
                      iiss$equipment_name == "land"] <- NA
#RADARS
iiss$equipment_name[iiss$equipment_type == "radars" &
                      iiss$equipment_name == "RADARS"] <- NA
# satellites 2
iiss$equipment_name[iiss$equipment_type == "radars" &
                      iiss$equipment_name == "Satellites 2"] <- "satellites"
# SURV in name
iiss$equipment_subtype[iiss$equipment_type == "radars" &
                         iiss$equipment_name == "SURV"] <- "surv"
iiss$equipment_name[iiss$equipment_type == "radars" &
                      iiss$equipment_subtype == "surv" &
                         iiss$equipment_name == "SURV"] <- NA
# satellites in name
iiss$equipment_name[iiss$equipment_type == "radars" &
                      iiss$equipment_name == "satellites"] <- NA
# space surveillance
iiss$equipment_subtype[iiss$equipment_type == "radars" &
                         (iiss$equipment_name == "space surveillance" |
                            iiss$equipment_name == "spacetrack system")] <- "space based systems"
#ground based electro optical deep space surveillance system (GEODSS)
iiss$equipment_subname[iiss$equipment_type == "radars" &
                         iiss$equipment_name == "ground based electro optical deep space surveillance system (GEODSS)"] <- "land"
iiss$equipment_subtype[iiss$equipment_type == "radars" &
                         iiss$equipment_name == "ground based electro optical deep space surveillance system (GEODSS)"] <- "surv"
# ABM RADAR
iiss$equipment_name[iiss$equipment_type == "radars" &
                      iiss$equipment_name == "ABM RADAR"] <- "ABM"
# missiles
iiss$equipment_subtype[iiss$equipment_type == "radars" &
                         (iiss$equipment_subtype == "missiles" |
                            iiss$equipment_subtype == "surv")] <- "land based systems"
# remove radars from subname
iiss$equipment_subname[iiss$equipment_type == "radars" &
                         iiss$equipment_subname == "RADARS"] <- NA
# rename SAM
iiss$equipment_name[iiss$equipment_type == "radars" &
                      iiss$equipment_name == "SAM"] <- "SAM RADARS"
# ABM in unit_name
iiss$equipment_name[iiss$equipment_type == "radars" &
                      iiss$unit_name == "ABM"] <- "ABM"
iiss$equipment_name[iiss$equipment_type == "radars" &
                      iiss$equipment_name == "ABM" &
                      iiss$unit_name == "ABM"] <- NA
# OTH-B
iiss$equipment_name[iiss$equipment_type == "radars" &
                      iiss$unit_name == "OTH-B"] <- "OTH-B"
iiss$equipment_name[iiss$equipment_type == "radars" &
                      iiss$equipment_name == "OTH-B" &
                      iiss$unit_name == "OTH-B"] <- NA
#ad>space based systems
iiss$equipment_type[iiss$equipment_type == "air defence" &
                      iiss$equipment_subtype == "space based systems"] <- "radars"
#space under aircraft
iiss$equipment_type[iiss$equipment_type == "aircraft" &
                      iiss$equipment_name == "SPACE"] <- "radars"
iiss$equipment_subtype[iiss$equipment_name == "radars" &
                         iiss$equipment_name == "SPACE"] <- "space based systems"
#icbm stuff
iiss$equipment_name[iiss$equipment_type == "radars" &
                      iiss$equipment_subtype == "ICBM/SLBM launch-detection capability"] <- "ICBM/SLBM launch-detection capability"
iiss$equipment_subtype[iiss$equipment_type == "radars" &
                      iiss$equipment_subtype == "ICBM/SLBM launch-detection capability" &
                        iiss$equipment_name == "ICBM/SLBM launch-detection capability"] <- "space based systems"
#civil defence
iiss$equipment_type[iiss$equipment_type == "radars" &
                      iiss$equipment_subtype == "civil defence"] <- "aircraft"
# ground-based interceptors
iiss$equipment_subtype[iiss$equipment_type == "radars" &
                         iiss$equipment_subtype == "ground-based interceptors"] <- "land based systems"
#aegis cruisers
iiss$equipment_name[iiss$equipment_type == "radars" &
                      iiss$equipment_subtype == "aegis cruisers and destroyers"] <- "aegis cruisers and destroyers"
iiss$equipment_subtype[iiss$equipment_type == "radars" &
                         iiss$equipment_subtype == "aegis cruisers and destroyers" &
                         iiss$equipment_name == "aegis cruisers and destroyers"] <- "land based systems"
#defensive
iiss$equipment_type[iiss$equipment_type == "defensive"] <- "radars"
#surv
iiss$equipment_type[iiss$equipment_type == "air defence" &
                      iiss$equipment_name == "SURV"] <- "radars"
#uav
iiss$equipment_type[iiss$equipment_type == "air defence" &
                      iiss$equipment_name == "UAV"] <- "unmanned aerial vehicles"
#AC
iiss$equipment_type[iiss$equipment_type == "radars" &
                      iiss$equipment_name == "AC"] <- "aircraft"
# TPQ-37 Firefinder
iiss$equipment_subtype[iiss$equipment_type == "radars" &
                         iiss$unit_name == "TPQ-37 Firefinder"] <- "land based systems"
iiss$equipment_subname[iiss$equipment_type == "radars" &
                         iiss$unit_name == "TPQ-37 Firefinder"] <- "land"

iiss$equipment_subname[iiss$equipment_type == "radars" &
                         iiss$unit_name == "AN-TPQ-36/-37"] <- "land"
#RECCE
iiss$equipment_subtype[iiss$equipment_type == "radars" &
                         iiss$equipment_name == "RECCE"] <- "land based systems"
iiss$equipment_subname[iiss$equipment_type == "radars" &
                         iiss$equipment_name == "RECCE"] <- "land"
#SYSTEMS
iiss$equipment_subtype[iiss$equipment_type == "radars" &
                         iiss$equipment_name == "SYSTEMS"] <- "land based systems"
iiss$equipment_subname[iiss$equipment_type == "radars" &
                         iiss$equipment_name == "SYSTEMS"] <- "land"
#1,800 S-300 (SA-10 Grumble)
iiss$equipment_type[iiss$unit_name == "1,800 S-300 (SA-10 Grumble)"] <- "air defence"
iiss$equipment_name[iiss$unit_name == "1,800 S-300 (SA-10 Grumble)"] <- "SAM"
#"53T6 (ABM-3 Gazelle)" / "9M96 (S-400)/Sh-08 Gazelle)" / ABM
iiss$equipment_type[iiss$unit_name == "53T6 (ABM-3 Gazelle)" |
                      iiss$unit_name == "9M96 (S-400)/Sh-08 Gazelle)" |
                      iiss$unit_name == "SH-08 Gazelle"  |
                      iiss$unit_name == "ABM" |
                      iiss$unit_name == "SH-11 Gorgon"] <- "air defence"
iiss$equipment_name[iiss$unit_name == "53T6 (ABM-3 Gazelle)" |
                      iiss$unit_name == "9M96 (S-400)/Sh-08 Gazelle)"  |
                      iiss$unit_name == "SH-08 Gazelle"   |
                      iiss$unit_name == "ABM" |
                      iiss$unit_name == "SH-11 Gorgon"] <- "SAM"
iiss$equipment_subname[iiss$unit_name == "53T6 (ABM-3 Gazelle)" |
                         iiss$unit_name == "9M96 (S-400)/Sh-08 Gazelle)"  |
                         iiss$unit_name == "SH-08 Gazelle"   |
                         iiss$unit_name == "ABM" |
                         iiss$unit_name == "SH-11 Gorgon"] <- "ABM"
#1,800 S-300 (SA-10 Grumble)
iiss$equipment_type[iiss$unit_name ==  "96 S-400 (SA-21 Growler)"] <- "air defence"
iiss$equipment_name[iiss$unit_name ==  "96 S-400 (SA-21 Growler)"] <- "SAM"
#"AN/TPS-43"
iiss$equipment_subtype[iiss$equipment_type == "radars" &
                         iiss$unit_name == "AN/TPS-43"] <- "land based systems"
iiss$equipment_subname[iiss$equipment_type == "radars" &
                         iiss$equipment_name == "AN/TPS-43"] <- "land"
#"BPS-1000"
iiss$equipment_subtype[iiss$equipment_type == "radars" &
                         iiss$unit_name == "BPS-1000"] <- "land based systems"
iiss$equipment_subname[iiss$equipment_type == "radars" &
                         iiss$equipment_name == "BPS-1000"] <- "land"
#FPS-117 / goldhaube
iiss$equipment_subtype[iiss$equipment_type == "radars" &
                         (iiss$unit_name == "FPS-117" |
                            iiss$unit_name == "Goldhaube" |
                            iiss$unit_name == "Land" |
                            iiss$unit_name == "LUR" |
                            iiss$unit_name == "MSTAR" |
                            iiss$unit_name == "NBR"|
                            iiss$unit_name == "PRV-11" |
                            iiss$unit_name == "PSZNR-5B" |
                            iiss$unit_name == "radar bde" |
                            iiss$unit_name == "S-600" |
                            iiss$unit_name == "SABER M60"|
                            iiss$unit_name == "sites"|
                            iiss$unit_name == "SNAR-10 Big Fred (SZNAR-10)"|
                            iiss$unit_name == "TPQ-37 Firefinder")] <- "land based systems"
iiss$equipment_subname[iiss$equipment_type == "radars" &
                         (iiss$unit_name == "FPS-117" |
                            iiss$unit_name == "Goldhaube" |
                            iiss$unit_name == "Land" |
                            iiss$unit_name == "LUR" |
                            iiss$unit_name == "MSTAR" |
                            iiss$unit_name == "NBR" |
                            iiss$unit_name == "PRV-11"|
                            iiss$unit_name == "PSZNR-5B"|
                            iiss$unit_name == "radar bde" |
                            iiss$unit_name == "S-600"|
                            iiss$unit_name == "SABER M60"|
                            iiss$unit_name == "sites"|
                            iiss$unit_name == "SNAR-10 Big Fred (SZNAR-10)"|
                            iiss$unit_name == "TPQ-37 Firefinder")] <- "land"
#C-212 MR
iiss$equipment_name[iiss$equipment_type == "radars" &
                      iiss$unit_name == "C-212 MR"] <- "TPT"
iiss$equipment_type[iiss$equipment_type == "radars" &
                      iiss$equipment_name == "TPT" &
                      iiss$unit_name == "C-212 MR"] <- "aircraft"
#M-113 Land
iiss$equipment_name[iiss$equipment_type == "radars" &
                      iiss$unit_name == "M-113 Land"] <- "APC"
iiss$equipment_type[iiss$equipment_type == "radars" &
                      iiss$equipment_name == "APC" &
                      iiss$unit_name == "M-113 Land"] <- "armoured fighting vehicles"
#operational satellites
iiss$equipment_subtype[iiss$equipment_type == "radars" &
                         iiss$unit_name == "operational satellites"] <- "space based systems"
iiss$equipment_subname[iiss$equipment_type == "radars" &
                         iiss$equipment_name == "operational satellites"] <- "space"







# Mine warfare

iiss$equipment_subtype[iiss$equipment_type == "mine warfare" &
                         iiss$equipment_name == "MCCS"] <- "mine countermeasures"
#MCMV
iiss$equipment_name[iiss$equipment_type == "mine warfare" &
                      iiss$equipment_subtype == "MCMV"] <- "MCMV"
iiss$equipment_subtype[iiss$equipment_type == "mine warfare" &
                         iiss$equipment_subtype == "MCMV" &
                         iiss$equipment_name == "MCMV"] <- NA
#ML(I)
iiss$equipment_name[iiss$equipment_type == "mine warfare" &
                      (iiss$equipment_name == "ML(1)" |
                         iiss$equipment_name == "ML(I)")] <- "MLI"
#amphibious
iiss$equipment_type[iiss$equipment_type == "mine warfare" &
                      iiss$equipment_subtype == "mine countermeasures" &
                      iiss$equipment_name == "amphibious"] <- "amphibious"
iiss$equipment_subtype[iiss$equipment_type == "amphibious" &
                         iiss$equipment_subtype == "mine countermeasures" &
                         iiss$equipment_name == "amphibious"] <- NA
iiss$equipment_subtype[iiss$equipment_type == "amphibious" &
                         is.na(iiss$equipment_subtype) &
                         iiss$equipment_name == "amphibious"] <- NA
#mcm spt
iiss$equipment_name[iiss$equipment_type == "mine warfare" &
                      iiss$equipment_name == "MCM SPT"] <- "MCM/SPT"
# T in subname
iiss$equipment_name[iiss$equipment_type == "mine warfare" &
                      iiss$equipment_name == "SP" &
                      iiss$equipment_subname == "T"] <- "SPT"
iiss$equipment_subname[iiss$equipment_type == "mine warfare" &
                      iiss$equipment_name == "SPT" &
                      iiss$equipment_subname == "T"] <- NA
#MCO
iiss$equipment_type[iiss$equipment_type == "principal surface combatants" &
                      iiss$equipment_name == "MCO"] <- "mine warfare"
iiss$equipment_type[iiss$equipment_type == "principal surface combatants" &
                      iiss$equipment_subtype == "aircraft carriers" &
                      iiss$equipment_name == "MCO"] <- "mine countermeasures"







# Satellites
iiss$equipment_name[iiss$equipment_type == "satellites" &
                      iiss$equipment_subtype == "communications"] <- "communications"
iiss$equipment_subtype[iiss$equipment_type == "satellites" &
                         iiss$equipment_subtype == "communications" &
                         iiss$equipment_name == "communications"] <- NA

iiss$equipment_name[iiss$equipment_type == "satellites" &
                      iiss$equipment_subtype == "ISR"] <- "ISR"
iiss$equipment_subtype[iiss$equipment_type == "satellites" &
                         iiss$equipment_subtype == "ISR" &
                         iiss$equipment_name == "ISR"] <- NA
# imagery in subname
iiss$equipment_name[iiss$equipment_type == "satellites" &
                      iiss$equipment_subname == "Imagery"] <- "imagery"
iiss$equipment_subname[iiss$equipment_type == "satellites" &
                      iiss$equipment_name == "imagery" &
                      iiss$equipment_subname == "Imagery"] <- NA
# infared/tv
iiss$equipment_name[iiss$equipment_type == "satellites" &
                      iiss$equipment_subname == "Infared/TV"] <- "infrared/TV"
iiss$equipment_subname[iiss$equipment_type == "satellites" &
                         iiss$equipment_name == "infrared/TV" &
                         iiss$equipment_subname == "Infared/TV"] <- NA
# coerce imagery
iiss$equipment_name[iiss$equipment_type == "satellites" &
                      (iiss$equipment_name == "Imagery" |
                         iiss$equipment_name == "IMAGERY" |
                         iiss$equipment_name == "Imagery 1")] <- "imagery"
# shift everything over to radars
iiss$equipment_type[iiss$equipment_type == "satellites"] <- "radars"




# Submarines

iiss$equipment_subname[iiss$equipment_type == "submarines" &
                         iiss$equipment_subtype == "tac"] <- "tactical"
iiss$equipment_subtype[iiss$equipment_type == "submarines" &
                         iiss$equipment_subtype == "tac"] <- NA

iiss$equipment_subname[iiss$equipment_type == "submarines" &
                         iiss$equipment_subtype == "suppport"] <- "support"
iiss$equipment_subtype[iiss$equipment_type == "submarines" &
                         iiss$equipment_subtype == "support"] <- NA

iiss$equipment_subname[iiss$equipment_type == "submarines" &
                         iiss$equipment_subtype == "strategic"] <- "strategic"
iiss$equipment_subtype[iiss$equipment_type == "submarines" &
                         iiss$equipment_subtype == "strategic"] <- NA
# tactical in subtype
iiss$equipment_subname[iiss$equipment_type == "submarines" &
                         iiss$equipment_subtype == "tactical"] <- "tactical"
iiss$equipment_subtype[iiss$equipment_type == "submarines" &
                         iiss$equipment_subtype == "tactical" &
                         iiss$equipment_subname == "tactical"] <- NA

iiss$equipment_subname[iiss$equipment_type == "submarines" &
                         (iiss$equipment_subname == "M45 SLBM" |
                            iiss$equipment_subname == "M51 SLBM")] <-NA

iiss$equipment_name[iiss$equipment_type == "submarines" &
                      iiss$equipment_subtype == "SDV"] <- "SDV"
iiss$equipment_subtype[iiss$equipment_type == "submarines" &
                         iiss$equipment_subtype == "SDV" &
                         iiss$equipment_name == "SDV"] <- NA
iiss$equipment_subname[iiss$equipment_type == "submarines" &
                         iiss$equipment_subname == "tac"] <- "tactical"
#offensive
iiss$equipment_type[iiss$equipment_type == "offensive" &
                      iiss$equipment_subtype == "submarines"] <- "submarines"
iiss$equipment_subtype[iiss$equipment_type == "submarines" &
                         iiss$equipment_subtype == "submarines"] <- NA
iiss$equipment_type[iiss$equipment_type == "offensive" &
                      iiss$equipment_name == "ICBM"] <- "ballistic missiles"
# other roles
iiss$equipment_name[iiss$equipment_type == "submarines" &
                      iiss$equipment_name == "Other Roles"] <- "SS"
# unit names
iiss$equipment_name[iiss$equipment_type == "submarines" &
                      (iiss$unit_name == "Dolfijn" |
                         iiss$unit_name == "Whiskey" |
                         iiss$unit_name == "Zeeleeuw")] <- "SSK"
iiss$equipment_subtype[iiss$equipment_type == "submarines" &
                      (iiss$unit_name == "Katsonis" |
                         iiss$unit_name == "Toti" |
                         iiss$unit_name == "W-class" |
                         iiss$unit_name == "Zwaardvis")] <- "attack"
iiss$equipment_subtype[iiss$equipment_type == "submarines" &
                         iiss$unit_name == "R-class"] <- "other"






# Miscellaneous / remaining cases

# Unmanned Aerial Vehicles
iiss$equipment_subtype[iiss$equipment_type == "unmanned aerial vehicles" &
                         iiss$equipment_subtype == "missiles"] <- NA
iiss$equipment_subname[iiss$equipment_type == "unmanned aerial vehicles" &
                         iiss$equipment_name == "medium"] <- "medium"
iiss$equipment_name[iiss$equipment_type == "unmanned aerial vehicles" &
                      iiss$equipment_name == "medium" &
                      iiss$equipment_subname == "medium"] <- NA
iiss$equipment_subname[iiss$equipment_type == "unmanned aerial vehicles" &
                         iiss$equipment_name == "heavy"] <- "heavy"
iiss$equipment_name[iiss$equipment_type == "unmanned aerial vehicles" &
                      iiss$equipment_name == "heavy" &
                      iiss$equipment_subname == "heavy"] <- NA
# tactical in subtype
iiss$equipment_subname[iiss$equipment_type == "unmanned aerial vehicles" &
                         iiss$equipment_subtype == "tactical"] <- "tactical"
iiss$equipment_subtype[iiss$equipment_type == "unmanned aerial vehicles" &
                         iiss$equipment_subtype == "tactical" &
                         iiss$equipment_subname == "tactical"] <- NA
# cbt isr
iiss$equipment_name[iiss$equipment_type == "unmanned aerial vehicles" &
                      iiss$equipment_name == "Cbt ISR"] <- "CISR"
# light
iiss$equipment_subname[iiss$equipment_type == "unmanned aerial vehicles" &
                      iiss$equipment_name == "light"] <- "light"
iiss$equipment_name[iiss$equipment_type == "unmanned aerial vehicles" &
                         iiss$equipment_name == "light" &
                      iiss$equipment_subname == "light"] <- "ISR"
# TAC
iiss$equipment_subname[iiss$equipment_type == "unmanned aerial vehicles" &
                         iiss$equipment_subname == "TAC"] <- "tactical"
# TACTICAL
iiss$equipment_subname[iiss$equipment_type == "unmanned aerial vehicles" &
                         iiss$equipment_name == "TACTICAL"] <- "tactical"
iiss$equipment_name[iiss$equipment_type == "unmanned aerial vehicles" &
                         iiss$equipment_name == "TACTICAL" &
                      iiss$equipment_subname == "tactical"] <- NA
# TPT (Bo-105 is a helicopter)
iiss$equipment_type[iiss$equipment_type == "unmanned aerial vehicles" &
                      iiss$equipment_name == "TPT" &
                      iiss$unit_name == "Bo-105"] <- "helicopters"
# uav in subname
iiss$equipment_name[iiss$equipment_type == "unmanned aerial vehicles" &
                      iiss$equipment_subname == "UAV"] <- "UAV"
iiss$equipment_subname[iiss$equipment_type == "unmanned aerial vehicles" &
                      iiss$equipment_subname == "UAV"] <- NA
#recce/atl
iiss$equipment_name[iiss$equipment_type == "unmanned aerial vehicles" &
                      iiss$equipment_name == "RECCE/ATL"] <- "RECCE/ATK"





# NA in equipment_type


# use equipment_name to coerce equipments into correct types
# air defence
iiss$equipment_type[is.na(iiss$equipment_type) &
                      (iiss$equipment_name == "AA GUNS" |
                         iiss$equipment_name == "AD" |
                         iiss$equipment_name == "AD GUNS" |
                         iiss$unit_name == "SA-7 Grail" |
                         iiss$equipment_name == "SAM" |
                         iiss$equipment_name == "SP" |
                         iiss$equipment_subtype == "SAM" |
                         iiss$equipment_name == "AD ARTY" |
                         iiss$unit_name == "Rh-202" |
                         iiss$unit_name == "L/60")] <- "air defence"
# air-launched missiles
iiss$equipment_type[is.na(iiss$equipment_type) &
                      (iiss$equipment_name == "AAM" |
                         iiss$equipment_name == "ASM")] <- "air-launched missiles"
# engineering and maintenance vehicles
iiss$equipment_type[is.na(iiss$equipment_type) &
                      iiss$equipment_name == "ACV"] <- "engineering and maintenance vehicles"
# logistics and support
iiss$equipment_type[is.na(iiss$equipment_type) &
                      (iiss$equipment_name == "AG" |
                       iiss$equipment_name == "YDT" |
                         iiss$equipment_name == "YFRT" |
                         iiss$equipment_name == "YTM")] <- "logistics and support"
# armoured fighting vehicles
iiss$equipment_type[is.na(iiss$equipment_type) &
                      (iiss$equipment_name == "AIFV" |
                       iiss$equipment_name == "APC" |
                         iiss$equipment_name == "LT" |
                         iiss$equipment_name == "LT TK" |
                         iiss$equipment_name == "MBT" |
                         iiss$equipment_name == "RECCE" |
                         iiss$equipment_name == "APC(T)" |
                         iiss$equipment_name == "APC (W)" |
                         (iiss$service == "Joint Forces Command (JFC)" &
                            iiss$equipment_name == "RCL") |
                         iiss$equipment_subtype == "RECCE" |
                         iiss$equipment_subtype == "APC(W)" |
                         iiss$equipment_subname == "MBT" |
                         iiss$equipment_subname == "RECCE" |
                         iiss$equipment_subname == "LTLK")] <- "armoured fighting vehicles"
# helicopters
iiss$equipment_type[is.na(iiss$equipment_type) &
                      (iiss$equipment_name == "ARMED HEL" |
                         iiss$equipment_name == "HEL" |
                         iiss$equipment_name == "TAK HEL") |
                      (is.na(iiss$equipment_subtype) &
                               iiss$equipment_name == "ASW" |
                         iiss$equipment_name == "ASW/ASUV") |
                      iiss$unit_name == "AS332B Super Puma"] <- "helicopters"
# anti-tank/anti-infrastructure
iiss$equipment_type[is.na(iiss$equipment_type) &
                         (iiss$equipment_name == "ASLT" |
                         iiss$equipment_name == "ATGW" |
                         iiss$equipment_name == "ATK GUNS" |
                         iiss$equipment_name == "GUNS" |
                         iiss$equipment_name == "MANPATS" |
                         iiss$equipment_name == "RL" |
                           iiss$equipment_subtype == "RL" |
                         iiss$equipment_subtype == "ATK GUNS" |
                           iiss$unit_name == "RPG-22" |
                           iiss$unit_name == "M-72 LAW")] <- "anti-tank/anti-infrastructure"
# patrol and coastal combatants
iiss$equipment_type[is.na(iiss$equipment_type) &
                      (iiss$equipment_name == "AVN" |
                      iiss$equipment_name == "PC" |
                      iiss$equipment_name == "PCC" |
                      iiss$equipment_name == "PCI" |
                      iiss$equipment_name == "PCH" |
                      iiss$equipment_name == "PCO" |
                      iiss$equipment_name == "PCI-10" |
                        iiss$equipment_name == "TOPPEDO" |
                        iiss$equipment_name == "marine")] <- "patrol and coastal combatants"
#principal surface combatants
iiss$equipment_type[is.na(iiss$equipment_type) &
                      (iiss$equipment_subtype == "command ships")] <- "principal surface combatants"
# aircraft
iiss$equipment_type[is.na(iiss$equipment_type) &
                      (iiss$equipment_name == "BBR" |
                         iiss$equipment_name == "FGA" |
                         iiss$equipment_name == "CONTROL" |
                         iiss$equipment_name == "Fighter" |
                         iiss$equipment_name == "FTR" |
                         iiss$equipment_name == "MR" |
                         iiss$equipment_name == "TANKERS" |
                         (iiss$service == "air force" &
                            iiss$equipment_name == "TBT") |
                         iiss$equipment_name == "TKR" |
                         iiss$equipment_name == "TPT" |
                         iiss$equipment_name == "FGA/ISR" |
                         iiss$equipment_name == "MRH" |
                         iiss$equipment_name == "MP" |
                         iiss$equipment_name == "training" |
                         iiss$equipment_name == "Training" |
                         iiss$equipment_name == "Transport" |
                         iiss$equipment_name == "TRG" |
                         iiss$equipment_name == "SURVEY" |
                         iiss$equipment_subname == "TRG" |
                         iiss$unit_name == "JJ-5/-6" |
                         iiss$unit_name == "Y-8" |
                         iiss$unit_name == "Hawker 800XP (EU-93A)" |
                           iiss$unit_name == "Learjet 35A" |
                           iiss$unit_name == "Kobalt M")] <- "aircraft"
# artillery
iiss$equipment_type[is.na(iiss$equipment_type) &
                      (iiss$subservice == "coastal arty" &
                       iiss$equipment_name == "GUN" |
                         iiss$equipment_name == "GUNS") |
                      iiss$equipment_name == "guns" |
                      (iiss$equipment_name == "MOR" |
                      iiss$equipment_name == "MRL" |
                        iiss$equipment_name == "TOWED" |
                        iiss$equipment_subtype == "ARTY MOR" |
                      iiss$equipment_name == "ARTY" |
                      iiss$equipment_name == "RCL" |
                      iiss$equipment_subtype == "GUNS" |
                        iiss$unit_name == "540 AT-4" |
                        iiss$equipment_subname == "105mm" |
                        iiss$equipment_subname == "122mm"  |
                        iiss$equipment_subname == "130mm"  |
                        iiss$equipment_subname == "155mm")] <- "artillery"
iiss$equipment_type[iiss$subservice == "costal defence" &
                      is.na(iiss$equipment_type) &
                      (iiss$equipment_subname == "105mm" |
                      iiss$equipment_subname == "120mm" |
                        iiss$equipment_subname == "127mm" |
                        iiss$equipment_subname == "150mm" |
                        iiss$equipment_subname == "75mm")] <- "artillery"
iiss$subservice[iiss$service == "costal defence"] <- "coastal defence"
# ballistic missiles
iiss$equipment_type[is.na(iiss$equipment_type) &
                      (iiss$equipment_name == "ICBM" |
                      iiss$unit_name == "Jericho 1 SRBM/Jericho 2 IRBM")] <- "ballistic missiles"
# surface-to-surface missile launchers
iiss$equipment_type[is.na(iiss$equipment_type) &
                      (iiss$equipment_name == "MSL" &
                      (iiss$unit_name == "SS-C-1B Sepal" |
                         iiss$unit_name == "SS-C-3 Styx" |
                         is.na(iiss$unit_name)) |
                         iiss$equipment_name == "SSM" |
                        iiss$equipment_name == "MSL" |
                         iiss$equipment_name == "Tactical SSM") |
                      iiss$equipment_subname == "SSM"] <- "surface-to-surface missile launchers"
# unmanned aerial vehicles
iiss$equipment_type[is.na(iiss$equipment_type) &
                      (iiss$equipment_name == "UAV")] <- "unmanned aerial vehicles"
# mine warfare
iiss$equipment_type[is.na(iiss$equipment_type) &
                      (iiss$equipment_name == "MINE" |
                         iiss$unit_name == "Fushun MSI") |
                      iiss$unit_name == "Fushun MSI"] <- "mine warfare"
# amphibious
iiss$equipment_type[is.na(iiss$equipment_type) &
                      (iiss$equipment_name == "LCT")] <- "amphibious"
# radars
iiss$equipment_type[is.na(iiss$equipment_type) &
                      iiss$equipment_name == "SURV"] <- "radars"

iiss$equipment_type[is.na(iiss$equipment_type) &
                      iiss$unit_name == "9M120 Ataka (AT-12 Swinger)"] <- "anti-tank/anti-infrastructure"

iiss$equipment_type[is.na(iiss$equipment_type) &
                      (iiss$unit_name == "AB-212" |
                         iiss$unit_name == "Allouette II" |
                         iiss$unit_name == "Mi-17" |
                         iiss$unit_name == "Mi-35" |
                         iiss$unit_name == "SA-315" |
                         iiss$unit_name == "SA-319")] <- "helicopters"

iiss$equipment_type[is.na(iiss$equipment_type) &
                      (iiss$unit_name == "Albatros" |
                      iiss$unit_name == "Albatross" |
                        iiss$unit_name == "C-160" |
                        iiss$unit_name == "Do-27" |
                        iiss$unit_name == "Falcon 20" |
                        iiss$unit_name == "Jaguar" |
                        iiss$unit_name == "MiG-21 BIS/UM" |
                        iiss$unit_name == "MiG-23B/E" |
                        iiss$unit_name == "Mirage 2000" |
                        iiss$unit_name == "Mirage 5D/E" |
                        iiss$unit_name == "P-149D" |
                        iiss$unit_name == "P-3" |
                        iiss$unit_name == "Su-27" |
                        iiss$unit_name == "Tu-154")] <- "aircraft"

iiss$equipment_type[is.na(iiss$equipment_type) &
                      (iiss$unit_name == "AML" |
                         iiss$unit_name == "AMX-30" |
                         iiss$unit_name == "AMX-VTT" |
                         iiss$unit_name == "BTR-80" |
                         iiss$unit_name == "Fiat 6616" |
                         iiss$unit_name == "PSZH" |
                         iiss$unit_name == "TM-170" |
                         iiss$unit_name == "VCC2")] <- "armoured fighting vehicles"

iiss$equipment_type[is.na(iiss$equipment_type) &
                      (iiss$unit_name == "cable mine")] <- "mine warfare"

iiss$equipment_type[is.na(iiss$equipment_type) &
                      (iiss$unit_name == "guns" |
                         iiss$unit_name == "M-106" |
                         iiss$unit_name == "M-113" |
                         iiss$unit_name == "torpedo bty")] <- "artillery"

iiss$equipment_type[is.na(iiss$equipment_type) &
                      (iiss$unit_name == "CG27" |
                         iiss$unit_name == "CG29" |
                         iiss$unit_name == "converted fishing boat" |
                         iiss$unit_name == "fishing craft" |
                         iiss$unit_name == "P1903" |
                         iiss$unit_name == "Patra" |
                         iiss$unit_name == "patrol craft" |
                         iiss$unit_name == "PCH" |
                         iiss$unit_name == "PCI<" |
                         iiss$unit_name == "Tjeld" |
                         iiss$unit_name == "Vosper 75" |
                         iiss$unit_name == "Dvora" |
                         iiss$unit_name == "Shimrit (US Flagstaff 2)")] <- "patrol and coastal combatants"

iiss$equipment_type[is.na(iiss$equipment_type) &
                      (iiss$unit_name == "Hunter")] <- "unmanned aerial vehicles"

iiss$equipment_type[is.na(iiss$equipment_type) &
                      (iiss$unit_name == "LCM" |
                         iiss$unit_name == "Yudao LSM")] <- "amphibious"

iiss$equipment_type[is.na(iiss$equipment_type) &
                      (iiss$unit_name == "SA-13" |
                         iiss$unit_name == "SA-2" |
                         iiss$unit_name == "SA-3/3A")] <- "air defence"

# AG fixes 2020-05-04 to sort later
## Fix typo for IUW
iiss$equipment_name[iiss$equipment_name == "IUW"] <- "MIUW"

## All FFs are frigates
iiss$equipment_subtype[iiss$equipment_type == "patrol and coastal combatants" &
                         iiss$equipment_name == "FF"] <- "frigates"

## corvettes are patrol and coastal, not principal surface combatants
iiss$equipment_type[iiss$equipment_type == "principal surface combatants" &
                         iiss$equipment_subtype == "corvettes"] <- "patrol and coastal combatants"

## All FFHM should be frigates
iiss$equipment_subtype[iiss$equipment_type == "principal surface combatants" &
                         iiss$equipment_name == "FFHM"] <- "frigates"

## All PSO (offshore patrol vessels) should be patrol and coastal combatants _ offshore
iiss$equipment_type[iiss$equipment_type == "principal surface combatants" &
                         iiss$equipment_name == "PSO"] <- "patrol and coastal combatants"

iiss$equipment_subtype[iiss$equipment_type == "principal surface combatants" &
                      iiss$equipment_name == "PSO"] <- "offshore"

## Radars
iiss$equipment_name[iiss$equipment_type == "radars" &
                      is.na(iiss$equipment_subname) &
                         is.na(iiss$equipment_name)] <- "early warning"

iiss$equipment_type[is.na(iiss$equipment_type) &
                      iiss$equipment_name == "early warning"] <- "radars"

# Submarines with NA NA
iiss$equipment_name[iiss$equipment_type == "submarines" &
                      is.na(iiss$equipment_subname) &
                      is.na(iiss$equipment_name)] <- "other"

# Fix NA_NA_NAs where we only have unit names
## A-340 Airbus
iiss$equipment_name[iiss$unit_name == "A-340 Airbus"] <- "TPT"

iiss$equipment_type[iiss$unit_name == "A-340 Airbus"] <- "aircraft"

# BN-2A Maritime Defender
iiss$equipment_name[iiss$unit_name == "BN-2A Maritime Defender"] <- "TPT"

iiss$equipment_type[iiss$unit_name == "BN-2A Maritime Defender"] <- "aircraft"

## Cessna 172
iiss$equipment_name[iiss$unit_name == "Cessna 172"] <- "TPT"

iiss$equipment_type[iiss$unit_name == "Cessna 172"] <- "aircraft"

## M-1938
iiss$equipment_subtype[iiss$unit_name == "M-1938"] <- "MOR"

iiss$equipment_type[iiss$unit_name == "M-1938"] <- "artillery"

## M-198
iiss$equipment_name[iiss$unit_name == "M-198"] <- "TOWED"

iiss$equipment_type[iiss$unit_name == "M-198"] <- "artillery"

# AG fixes 2020-05-11 to sort later
## Fix MIUW
iiss$equipment_type[iiss$equipment_name == "MIUW"] <- "patrol and coastal combatants"
iiss$equipment_subtype[iiss$equipment_name == "MIUW"] <- "inshore patrol"

## Fix corvettes
iiss$equipment_type[iiss$equipment_subtype == "corvettes"] <- "patrol and coastal combatants"
iiss$equipment_type[iiss$equipment_name == "FS" |
                      iiss$equipment_name == "FSG"] <- "patrol and coastal combatants"
iiss$equipment_subtype[iiss$equipment_name == "FS" |
                      iiss$equipment_name == "FSG"] <- "corvettes"

## Fix armoured fighting vehicles without subtypes or names
iiss$equipment_name[iiss$unit_name == "AML"] <- "RECCE"
iiss$equipment_name[iiss$unit_name == "BLR"] <- "APC"
iiss$equipment_name[iiss$unit_name == "BMD"] <- "AIFV"
iiss$equipment_name[iiss$unit_name == "BTR APC (W)/M-113A APT (T)"] <- "APC"
iiss$equipment_name[iiss$unit_name == "BTR-70"] <- "APC"
iiss$equipment_name[iiss$unit_name == "BTR-80"] <- "APC"
iiss$equipment_name[iiss$unit_name == "Fiat 6616"] <- "RECCE"
iiss$equipment_name[iiss$unit_name == "LVTP-7 AAV"] <- "AAV"
iiss$equipment_name[iiss$unit_name == "M-60PB"] <- "APC"
iiss$equipment_name[iiss$unit_name == "PSZH"] <- "APC"
iiss$equipment_name[iiss$unit_name == "Scorpion lt tk"] <- "LT TK"
iiss$equipment_name[iiss$unit_name == "TM-170"] <- "APC"
iiss$equipment_name[iiss$unit_name == "VCC2"] <- "APC"

iiss$equipment_type[iiss$unit_name == "AMX-VTT"] <- "artillery"
iiss$equipment_name[iiss$unit_name == "AMX-VTT"] <- "SP"
iiss$equipment_subname[iiss$unit_name == "AMX-VTT"] <- "81mm"

iiss$equipment_type[iiss$unit_name == "M998 HMMVW"] <- "air defence"
iiss$equipment_subtype[iiss$unit_name == "M998 HMMVW"] <- "missiles"
iiss$equipment_name[iiss$unit_name == "M998 HMMVW"] <- "SAM"

## Fix things wrongly made aircraft
iiss$equipment_type[iiss$equipment_name == "PCI"] <- "patrol and coastal combatants"
iiss$equipment_subtype[iiss$equipment_name == "PCI"] <- "inshore"

iiss$equipment_type[iiss$equipment_name == "UAV"] <- "unmanned aerial vehicles"

## Fix things wrongly made helicopters
iiss$equipment_type[iiss$equipment_name == "ALCM"] <- "air-launched missiles"
iiss$equipment_subtype[iiss$equipment_name == "ALCM"] <- "missiles"

## Fix fake hel equipment_name
iiss$equipment_name[iiss$unit_name == "Mi-24"] <- "ATK"
iiss$equipment_name[iiss$unit_name == "Mi-26"] <- "TPT"
iiss$equipment_name[iiss$unit_name == "Mi-8 Hip"] <- "TPT"

iiss$equipment_type[iiss$unit_name == "MiG-21"] <- "aircraft"
iiss$equipment_name[iiss$unit_name == "MiG-21"] <- "FTR"

iiss$equipment_type[iiss$unit_name == "Su-7"] <- "aircraft"
iiss$equipment_name[iiss$unit_name == "Su-7"] <- "FGA"

iiss$equipment_type[iiss$unit_name == "Su-7/-20"] <- "aircraft"
iiss$equipment_name[iiss$unit_name == "Su-7/-20"] <- "FGA"

## Fix PCI errors in heli
iiss$equipment_name[iiss$equipment_type == "helicopters" &
                      iiss$equipment_name == "PCI"] <- "ASW"

iiss$equipment_type[iiss$equipment_type == "helicopters" &
                      (iiss$unit_name == "Alize" |
                      iiss$unit_name == "AP-3C Orion" |
                      iiss$unit_name == "Atlantique 2" |
                      iiss$unit_name == "Be-12 Mail" |
                      iiss$unit_name == "Be-6 Madge" |
                      iiss$unit_name == "BR1150 Atlantic" |
                      iiss$unit_name == "C-212" |
                      iiss$unit_name == "C-295ASW Persuader" |
                      iiss$unit_name == "CP-121" |
                      iiss$unit_name == "CP-121 (modified) Tracker" |
                      iiss$unit_name == "CS2F-3 Tracker" |
                      iiss$unit_name == "Dornier 228" |
                      iiss$unit_name == "Dornier 228-212" |
                      iiss$unit_name == "EMB-111A" |
                      iiss$unit_name == "ES-3A" |
                      iiss$unit_name == "F-27" |
                      iiss$unit_name == "F-27 Maritime Enforcer (ESM/ELINT)" |
                      iiss$unit_name == "F-27 MPA" |
                      iiss$unit_name == "Fokker F.27 Mk 200" |
                      iiss$unit_name == "Il-38 May" |
                      iiss$unit_name == "N-24A Nomad" |
                      iiss$unit_name == "P-1" |
                      iiss$unit_name == "P-3" |
                      iiss$unit_name == "P-3 Orion" |
                      iiss$unit_name == "P-3 Orion (CP-140 Aurora)" |
                      iiss$unit_name == "P-3A Orion" |
                      iiss$unit_name == "P-3A Orion (P-3T)" |
                      iiss$unit_name == "P-3ACH Orion" |
                      iiss$unit_name == "P-3AM Orion" |
                      iiss$unit_name == "P-3B Orion" |
                      iiss$unit_name == "P-3C" |
                      iiss$unit_name == "P-3C Orion" |
                      iiss$unit_name == "P-3CK Orion" |
                      iiss$unit_name == "P-3F Orion" |
                      iiss$unit_name == "P-3K Orion" |
                      iiss$unit_name == "P-3K2 Orion" |
                      iiss$unit_name == "P-3M Orion" |
                      iiss$unit_name == "P-3MP Orion" |
                      iiss$unit_name == "P-3N Orion" |
                      iiss$unit_name == "P-3T Orion" |
                      iiss$unit_name == "P-81 Poseidon" |
                      iiss$unit_name == "P-8A Poseidon" |
                      iiss$unit_name == "S-2" |
                      iiss$unit_name == "S-2A" |
                      iiss$unit_name == "S-2E" |
                      iiss$unit_name == "S-2E Tracker" |
                      iiss$unit_name == "S-2F" |
                      iiss$unit_name == "S-2G" |
                      iiss$unit_name == "S-2G Tracker" |
                      iiss$unit_name == "S-2T Tracjer" |
                      iiss$unit_name == "S-2T Tracker" |
                      iiss$unit_name == "S-3B Viking" |
                      iiss$unit_name == "UP-\r3T (UP-3A) Orion" |
                      iiss$unit_name == "UP-3T")] <- "aircraft"

iiss$unit_name[iiss$unit_name == "S-2T Tracjer"] <- "S-2T Tracker"

iiss$equipment_type[iiss$unit_name == "Rihtniemi"] <- "patrol and coastal combatants"
iiss$equipment_subtype[iiss$unit_name == "Rihtniemi"] <- "PB"

iiss$equipment_type[iiss$unit_name == "Ruissalo"] <- "patrol and coastal combatants"
iiss$equipment_subtype[iiss$unit_name == "Ruissalo"] <- "corvettes"

iiss$equipment_name[iiss$equipment_type == "helicopters" &
                      iiss$unit_name == "HSS-2B"] <- "TRG"

iiss$equipment_name[iiss$equipment_type == "helicopters" &
                      iiss$unit_name == "N-SH-60B Seahawk"] <- "TRG"

iiss$equipment_name[iiss$equipment_type == "helicopters" &
                      iiss$unit_name == "SH-60J"] <- "ASW"

iiss$equipment_name[iiss$equipment_type == "helicopters" &
                      iiss$unit_name == "U-36A"] <- "UTL"

iiss$equipment_name[iiss$equipment_type == "helicopters" &
                      iiss$unit_name == "UP-2J"] <- "ASW"

iiss$equipment_name[iiss$equipment_name == "TPE/ECM"] <- "TPT/ECM"

## Drop non-teks for sri lanka
iiss <- iiss[!grepl("Reserves", iiss$equipment_name), ]

## Fix coastal and patrol
iiss$equipment_name[iiss$equipment_name == "GDR"] <- "PC"
iiss$equipment_subname[iiss$equipment_name == "GDR"] <- NA

# AG fixes 2020-05-12 to sort later
## Aircraft miscoded as carrier
iiss <- iiss %>%
  dplyr::filter(!(country == "india" & equipment_name == "CV" & unit_name == "Sea Harrier (Sea Eagle ASM)"))

iiss <- iiss %>%
  dplyr::filter(!(country == "india" & equipment_name == "CV" & unit_name == "Ka-27 Helix/ Sea King Mk24B"))

## If equipment_subtype = carriers and equipment_subname is AEW, ASW, or FGA, then drop row
iiss <- iiss %>%
  dplyr::filter(!(equipment_subtype == "carriers" & equipment_name == "AEW"))

iiss <- iiss %>%
  dplyr::filter(!(equipment_subtype == "carriers" & equipment_name == "ASW"))

iiss <- iiss %>%
  dplyr::filter(!(equipment_subtype == "carriers" & equipment_name == "FGA"))

## Finland patrol boats
iiss$equipment_name[iiss$unit_name == "Rihtniemi" |
                      iiss$unit_name == "Ruissalo"] <- "PB"

## Vietnam space-based radar drop. People were counted
iiss <- iiss %>%
  dplyr::filter(!(country == "vietnam" & equipment_type == "radars" & equipment_name == "early warning"))

## Vietnam aircraft_inshore fix
iiss$equipment_subtype[iiss$unit_name == "Be-12 Mail"] <- NA
iiss$equipment_name[iiss$unit_name == "Be-12 Mail"] <- "MR"

## aircraft_NA
iiss$equipment_name[iiss$unit_name == "AC 1 Cessa U206G" |
                      iiss$unit_name == "BN-2T" |
                      iiss$unit_name == "Helio 269" |
                      iiss$unit_name == "Helio 391" |
                      iiss$unit_name == "OV-1D" |
                      iiss$unit_name == "PC-12" |
                      iiss$unit_name == "RU-21" |
                      iiss$unit_name == "RV-1D" |
                      iiss$unit_name == "short skyvan"] <- "UTL"

iiss$equipment_name[iiss$unit_name == "Albatros" |
                      iiss$unit_name == "Albatross"] <- "TRG"

iiss$equipment_name[iiss$unit_name == "AN-2" |
                      iiss$unit_name == "B-707" |
                      iiss$unit_name == "B-727" |
                      iiss$unit_name == "B-737-200" |
                      iiss$unit_name == "C-47 (tpt)" |
                      iiss$unit_name == "Cessna 402C" |
                      iiss$unit_name == "DC-9" |
                      iiss$unit_name == "Falcon 20 (civil registration)" |
                      iiss$unit_name == "Gulfstream (C-20)" |
                      iiss$unit_name == "Gulfstream I" |
                      iiss$unit_name == "Gulfstream II" |
                      iiss$unit_name == "Gulfstream III (C-20E)" |
                      iiss$unit_name == "Gulfstream IV (C-20F)" |
                      iiss$unit_name == "Gulfstream V(C-37A)" |
                      iiss$unit_name == "Gulfstream(C-20)" |
                      iiss$unit_name == "TMB-700" |
                      iiss$unit_name == "Tu-154 Careless" |
                      iiss$unit_name == "Yak-40"] <- "TPT"

iiss$equipment_name[iiss$unit_name == "F-16CG (FGA)/DG (trg)" |
                      iiss$unit_name == "Mirage 2000N (ASMP)" |
                      iiss$unit_name == "Super Entendard"] <- "FGA"

iiss$equipment_name[iiss$unit_name == "MiG-21MF Fishbed J Mi-G 21/MiG-21UB Mongol Mongol A trg;" |
                      iiss$unit_name == "Mirage F-1 CG (ftr)" |
                      iiss$unit_name == "TA-7P"] <- "FTR"

## helicopters_NA
iiss$equipment_type[iiss$unit_name == "Ka-25 Hormone"] <- "helicopters"
iiss$equipment_name[iiss$unit_name == "Ka-25 Hormone"] <- "ASW"
iiss$equipment_subtype[iiss$unit_name == "Ka-25 Hormone"] <- NA

iiss$equipment_name[iiss$unit_name == "KV-107"] <- "SAR"

iiss$equipment_name[iiss$unit_name == "SE-3160"] <- "UTL"

iiss$equipment_name[iiss$unit_name == "Shabaviz 2061 and 2-75" |
                      iiss$unit_name == "V-107"] <- "TPT"

# SET NEW DATA
iiss <<- iiss


}
