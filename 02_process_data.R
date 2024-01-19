library(here)
library(vroom)
library(tidyverse)
library(conflicted)
library(janitor)

conflicts_prefer(dplyr::filter)

cip_noc_diff <- readxl::read_excel(here("out","cip_noc_differences2.xlsx"),
                                   col_types = c(rep("text",4), rep("numeric", 77)))|>
  select(-starts_with("noc-cip"))

census_dat <- vroom(here("raw_data","stats_can","data_donnees_2021_ind_v2.csv"),
                    na = c("88888888", "99999999"),
                    col_select = c("NOC21",
                                   "CIP2021",
                                   "EmpIn",
                                   "AGEGRP",
                                   "HDGREE",
                              #    "WEIGHT",
                                   "WRKACT",
                                   "PR",
                                   "COW",
                                   "HLMOSTEN",
                                   "Gender",
                                   "DPGRSUM"))|>
  mutate(NOC21=as.character(NOC21),
         CIP2021=as.character(CIP2021)
         )

filtering_info <- list(`Census sample size`=nrow(census_dat))

#filter the data-------------------------

filtered <- census_dat|>
  filter(WRKACT==11)# worked full time full year

filtering_info$`Worked full time full year` <- nrow(filtered)

filtered <- filtered|>
  filter(! EmpIn %in% c(88888888, 99999999)) #employment income not-not available/applicable

filtering_info$`Employment Income not missing` <- nrow(filtered)

filtered <- filtered|>
  filter(EmpIn > 30*40*15) # income should be larger than 30 hours/week*40 weeks/year*$15/hour

filtering_info$`Employment income > $18,000` <- nrow(filtered)

filtered <- filtered|>
  filter(PR==59) #BC

filtering_info$`Live in B.C.` <- nrow(filtered)

filtered <- filtered|>
  filter(COW==1) #employee, not self employed

filtering_info$`Employee, not self employed` <- nrow(filtered)

filtered <- filtered|>
  filter(AGEGRP %in% c(8:17, 88)) #ages between 20 and 69

filtering_info$`Age between 20 and 69 or unknown` <- nrow(filtered)

filtered <- filtered|>
  filter(HDGREE>2) #some education beyond high school

filtering_info$`Education beyond high school or unknown` <- nrow(filtered)

filtered <- filtered|>
  filter(! CIP2021 %in% c(12,13,88,99)) #person had a field of study

filtering_info$`Reported a field of study` <- nrow(filtered)

filtered <- filtered|>
  filter(! NOC21 %in% c(1,88)) #Do not have skill data for senior admin and legislators

filtering_info$`Occupation other than senior administrators and legislators` <- nrow(filtered)

no_distance <- filtered|>
  remove_constant()|>
  mutate(`Age vs. 20-24:`=factor(AGEGRP, labels=c("20 to 24 years",
                                        "25 to 29 years",
                                        "30 to 34 years",
                                        "35 to 39 years",
                                        "40 to 44 years",
                                        "45 to 49 years",
                                        "50 to 54 years",
                                        "55 to 59 years",
                                        "60 to 64 years",
                                        "65 to 69 years",
                                        "Unknown"
                                        )), .keep = "unused")|>
   mutate(`Degree vs. non-apprentice:`=factor(HDGREE,labels=c("Non-apprenticeship trades, or certificate",
                                        "Apprenticeship certificate",
                                        "Less than 1 year College",
                                        "1-2 years of College",
                                        "More than 2 years of College",
                                        "University certificate or diploma",
                                        "Bachelor's degree",
                                        "University diploma above bachelor level",
                                        "Medicine, dentistry, veterinary, optometry",
                                        "Master's degree",
                                        "PhD",
                                        "Unknown"
                                        )), .keep = "unused")|>
  mutate(`Language vs. not english:`=factor(HLMOSTEN, labels=c("Not English", "English", "Unknown")), .keep = "unused")|>
  mutate(`Gender vs. Woman+:`=factor(Gender, labels = c("Woman+", "Man+")), .keep = "unused")|>
  mutate(`Ethnicity vs. White:`=factor(DPGRSUM, labels =c("White",
                                           "South Asian",
                                           "Chinese",
                                           "Black",
                                           "Filipino",
                                           "Arab",
                                           "Latin American",
                                           "Southeast Asian",
                                           "West Asian",
                                           "Korean",
                                           "Japanese",
                                           "Other population groups, n.i.e.",
                                           "Other multiple population groups",
                                           "Indigenous peoples",
                                           "Unknown"
                                           )), .keep = "unused")|>
  mutate(log_income=log(EmpIn), .after = EmpIn)

filtered <- no_distance|>
  left_join(cip_noc_diff, by = c("NOC21"= "census_noc_code", "CIP2021"="census_cip_code"))|>
  rename(`NOC vs. Admin and financ...:`="census_noc_description",
         `CIP vs. Agriculture...:`="census_cip_description")|>
  select(-NOC21, -CIP2021, -EmpIn)

filtering_info <- enframe(filtering_info)

write_rds(filtering_info, here("out","filtering_info.rds"))
write_rds(filtered, here("out","filtered.rds"))
write_rds(no_distance, here("out","no_distance.rds"))

#create dataset for logit regression (work full year full time vs not)

filtering_info2 <-  list(`Census sample size`=nrow(census_dat))

filtered2 <- census_dat|>
  filter(WRKACT %in% 3:12)|>
  mutate(full_time_full_year=if_else(WRKACT==11, 1, 0))|>
  select(-WRKACT)

filtering_info2$`Employed in 2020` <- nrow(filtered2)

filtered2 <- filtered2|>
  filter(PR==59) #BC

filtering_info2$`Live in B.C.` <- nrow(filtered2)

filtered2 <- filtered2|>
  filter(COW==1) #employee, not self employed

filtering_info2$`Employee, not self employed` <- nrow(filtered2)

filtered2 <- filtered2|>
  filter(AGEGRP %in% c(8:17, 88)) #ages between 20 and 69 or unknown

filtering_info2$`Age between 20 and 69 or unknown` <- nrow(filtered2)

filtered2 <- filtered2|>
  filter(HDGREE>2) #some education beyond high school

filtering_info2$`Education beyond high school or unknown` <- nrow(filtered2)

filtered2 <- filtered2|>
  filter(! CIP2021 %in% c(12,13,88,99)) #person had a field of study

filtering_info2$`Reported a field of study` <- nrow(filtered2)

filtered2 <- filtered2|>
  filter(! NOC21 %in% c(1,88)) #Do not have skill data for senior admin and legislators

filtering_info2$`Occupation other than senior administrators and legislators` <- nrow(filtered2)

filtered2 <- filtered2|>
  remove_constant()|>
  mutate(`Age vs. 20-24:`=factor(AGEGRP, labels=c("20 to 24 years",
                                                  "25 to 29 years",
                                                  "30 to 34 years",
                                                  "35 to 39 years",
                                                  "40 to 44 years",
                                                  "45 to 49 years",
                                                  "50 to 54 years",
                                                  "55 to 59 years",
                                                  "60 to 64 years",
                                                  "65 to 69 years",
                                                  "Unknown"
  )), .keep = "unused")|>
  mutate(`Degree vs. non-apprentice:`=factor(HDGREE,labels=c("Non-apprenticeship trades, or certificate",
                                                             "Apprenticeship certificate",
                                                             "Less than 1 year College",
                                                             "1-2 years of College",
                                                             "More than 2 years of College",
                                                             "University certificate or diploma",
                                                             "Bachelor's degree",
                                                             "University diploma above bachelor level",
                                                             "Medicine, dentistry, veterinary, optometry",
                                                             "Master's degree",
                                                             "PhD",
                                                             "Unknown"
  )), .keep = "unused")|>
  mutate(`Language vs. not english:`=factor(HLMOSTEN, labels=c("Not English", "English", "Unknown")), .keep = "unused")|>
  mutate(`Gender vs. Woman+:`=factor(Gender, labels = c("Woman+", "Man+")), .keep = "unused")|>
  mutate(`Ethnicity vs. White:`=factor(DPGRSUM, labels =c("White",
                                                          "South Asian",
                                                          "Chinese",
                                                          "Black",
                                                          "Filipino",
                                                          "Arab",
                                                          "Latin American",
                                                          "Southeast Asian",
                                                          "West Asian",
                                                          "Korean",
                                                          "Japanese",
                                                          "Other population groups, n.i.e.",
                                                          "Other multiple population groups",
                                                          "Indigenous peoples",
                                                          "Unknown"
  )), .keep = "unused")|>
  left_join(cip_noc_diff, by = c("NOC21"= "census_noc_code", "CIP2021"="census_cip_code"))|>
  rename(`NOC vs. Admin and financ...:`="census_noc_description",
         `CIP vs. Agriculture...:`="census_cip_description")|>
  select(-NOC21, -CIP2021, -EmpIn)

filtering_info2 <- enframe(filtering_info2)

write_rds(filtering_info2, here("out","filtering_info2.rds"))
write_rds(filtered2, here("out","filtered2.rds"))









