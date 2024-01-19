#' This script does the following...
#' 1) creates a skill/work activities profile for each census field of study (CIP)
#' 2) calculates distance between the skill/work activities profile for each CIP/NOC combination.
#' 3) it also calculates the NOC-CIP difference in skills/work activities for each combo.
#libraries------------------------------------
library(tidyverse)
library(readxl)
library(here)
library(janitor)
library(tidytext) # for reorder_within
library(conflicted)
conflicts_prefer(dplyr::filter)
#functions------------------
cut_off <- .01
read_data <- function(file_name){
  #' Input: a file name.
  #' Output: a wide format dataframe.
  read_excel(here("raw_data", "onet", file_name))%>%
    clean_names()%>%
    select(o_net_soc_code, element_name, scale_name, data_value)%>%
    pivot_wider(names_from = scale_name, values_from = data_value)%>% #make it wider
    select(-Importance) #using the level data
}
add_weights <- function(tbbl){
  #' Input: a tibble with columns noc and count (derived from a row of the cip/noc table)
  #' Output: the input tibble with columns weight and weight1 added.
  tbbl|>
    mutate(weight=count/sum(count), #using all the counts as weights
           weight1=if_else(weight>=cut_off, weight, 0), #replacing all weights less than cut_off with 0
           weight1=weight1/sum(weight1), .after="count" #weights, conditional on being greater than cut_off (conditional weights sum to 1)
    )|>
    arrange(desc(weight1))
}

add_weights2 <- function(tbbl){
  #' Input: a tibble with columns noc and count2
  #' Output: the input tibble with columns weight and weight1 added.
  tbbl|>
    mutate(weight=count2/sum(count2), #using all the counts as weights
           weight1=if_else(weight>=.01, weight, 0), #replacing all weights less than .01 with 0
           weight1=weight1/sum(weight1), .after="count2" #weights, conditional on being greater than .01 (conditional weights sum to 1)
    )|>
    arrange(desc(weight1))
}

get_weighted_mean <- function(tbbl){
  #' Input: a tibble with columns noc, count, weight and weight1
  #' Output: a long format tibble with columns thing and level
  #browser()
  inner_join(tbbl, onet_long)|> #joins the weights to the skills data
    group_by(thing)|> #group by the skill/work activity
    mutate(weighted1=Level*weight1)|> #multiply skill/work activity Level times the weights
    summarize(Level=round(sum(weighted1),2)) #add up all the weighted values
}
get_weighted_mean2 <- function(tbbl){
  #' Input: a wide tibble with columns count2, weight, weight1 plus all skills and activities
  #' Output: a wide tibble with the weighted average skills and activities.
  tbbl|>
    select(-count2,-weight)|>
    pivot_longer(cols=-weight1)|>
    mutate(value=value*weight1)|>
    group_by(name)|>
    summarise(value=sum(value))|>
    pivot_wider(names_from = name, values_from = value)
}
get_difference <- function(noc, cip){
  #' Inputs: one row tibbles with all the skills/work activities as columns
  #' Output: a tibble with columns thing (i.e. skills/work activities) and difference
  noc <- enframe(t(noc)) #transposes tibble
  colnames(noc) <- c("thing", "noc")
  cip <- enframe(t(cip))
  colnames(cip) <- c("thing", "cip")
  temp <- inner_join(noc,cip, by = join_by(thing))|>
    mutate(difference=round(noc-cip, 2))|>
    select(thing, difference)|>
    as.data.frame()
  colnames(temp) <- c("thing", "difference")
  return(temp)
}
# we need to map from onet's soc to our noc-----------------------
mapping <- read_excel(here("raw_data","mapping", "onet2019_soc2018_noc2016_noc2021_crosswalk.xlsx"))%>%
  mutate(noc2021=str_pad(noc2021, "left", pad="0", width=5))%>%
  select(noc=noc2021, description= noc2021_title, o_net_soc_code = onetsoc2019)%>%
  distinct()
#the onet data-----------------------------------
onet_long <- tibble(file=c("Skills.xlsx", "Work Activities.xlsx"))%>%
  mutate(data=map(file, read_data))%>%
  select(-file)%>%
  unnest(data)%>%
  pivot_wider(id_cols = o_net_soc_code, names_from = element_name, values_from = Level)%>%
  inner_join(mapping)%>%
  ungroup()%>%
  select(-o_net_soc_code)%>%
  select(noc, everything())%>%
  group_by(noc, description)%>%
  summarise(across(where(is.numeric), \(x) round(mean(x, na.rm = TRUE),2)))%>% #mapping from SOC to NOC is not one to one: mean give one value per NOC
  mutate(across(where(is.numeric), ~ if_else(is.na(.), mean(., na.rm=TRUE), .)))|> #for 11 occupations and 4 variables replace missing values with the mean
  pivot_longer(cols=-c(noc, description), names_to = "thing", values_to = "Level")
#same as above but with long names-----------------
onet_long_name <- onet_long|>
  unite(noc, "noc", "description", sep=": ")

cip2_noc5 <- vroom::vroom(here("raw_data","stats_can","cip2_noc5.csv"), skip = 15, n_max = 512, col_names = FALSE)
colnames(cip2_noc5) <- vroom::vroom(here("raw_data","stats_can","cip2_noc5.csv"), skip = 10, n_max = 1, col_names = FALSE)
colnames(cip2_noc5)[1] <- "noc"
cip2_noc5 <- cip2_noc5|>
  mutate(noc=str_sub(noc, end=5))

#long format cip_noc table--------------------
cip2_long <- cip2_noc5%>%
  pivot_longer(cols=-"noc", names_to="Field of Study", values_to = "count")|>
  full_join(read_csv(here("raw_data","mapping","cip2_mapping.csv")), by=c("Field of Study"="cip"))


#nested dataframe with noc counts for each cip--------------------
noc_counts_by_cip2 <- cip2_long|>
  group_by(`Field of Study`, census_code)|>
  mutate(total=sum(count), #how many employed in each noc/cip combo (just so we can arrange by)
         num_nocs= sum(count>0), #just for interest, number of destination NOCs associated with each CIP
  )|>
  group_by(census_description=`Field of Study`, census_code, total, num_nocs)|> #grouping by FOS... total and num_nocs would be dropped if not included.
  nest()|>
  arrange(desc(total))

#get the weighted mean skills for each cip---------------------
skills_by_cip2 <- noc_counts_by_cip2|>
  mutate(data=map(data, add_weights),
         skills=map(data, get_weighted_mean)
  )|>
  ungroup()|>
  select(-data, -total, -num_nocs)|>
  unnest(skills)|>
  pivot_wider(names_from = thing, values_from = Level)|>
  mutate(id=paste("cip", census_description, census_code, sep=": "), .before = everything())|>
  select(-census_description, -census_code)

#we need to aggregate the onet data down to 26 NOC groups available in the census data.
#start by aggregating to 2 digit level---------------

noc2_skills <- cip2_long|>
  ungroup()|>
  group_by(noc)|>
  summarize(count=sum(count))|>
  mutate(noc2=str_sub(noc, end=2), .after = "noc")|>
  group_by(noc2)|>
  mutate(count2=sum(count))|>
  group_by(noc2, count2)|>
  nest()|>
  arrange(desc(count2))|>
  mutate(data=map(data, add_weights),
         skills=map(data, get_weighted_mean))

noc2_mapping <- read_csv(here("raw_data","mapping","noc2_mapping.csv"))|>
  mutate(noc2=str_pad(as.character(noc2), side="left",pad = "0", width=2),
         census_description=str_remove_all(census_description, "[:digit:]"),
         census_description=str_remove_all(census_description, "-"),
         census_description=trimws(census_description)
  )

skills_by_census_noc <- noc2_skills|>
  full_join(noc2_mapping)|>
  ungroup()|>
  select(-data, -noc2)|>
  unnest(skills)|>
  pivot_wider(names_from = thing, values_from = Level)|>
  ungroup()|>
  group_by(census_description, census_code)|>
  nest()|>
  mutate(data=map(data, add_weights2),
         skills=map(data, get_weighted_mean2)
  )|>
  select(-data)|>
  unnest(skills)|>
  mutate(id=paste("noc",census_description, census_code, sep=": "), .before = everything())|>
  ungroup()|>
  select(-census_description, -census_code)

#bind the NOC and CIP data into single tibble (full_join safer than row_bind)
scaled_onet2 <- full_join(skills_by_census_noc, skills_by_cip2)|>
  column_to_rownames("id")|>
  na.omit()|>
  scale()
#principal components of scaled data (distance metrics do not work well in 76D space)-----------------
pca_onet2 <- prcomp(scaled_onet2)
factoextra::fviz_eig(pca_onet2) #looks like first 5 should do it
first_five2 <- pca_onet2$x[,1:5]
#Euclidean distance between all NOCs and CIPs using
dist_mat2 <- dist(first_five2)|>
  as.matrix(nrow=nrows(first_five2))
#use distance matrix to create a long dataframe with columns NOC, CIP and distance
distance2 <- dist_mat2|>
  as.data.frame()|>
  rownames_to_column("id1")|>
  pivot_longer(cols=-id1, names_to = "id2", values_to = "distance")|>
  filter(str_detect(id1, "noc"),
         str_detect(id2, "cip"))|>
  rename(NOC=id1,
         CIP=id2)|>
  mutate(distance=round(distance, 2))|>
  arrange(distance)

write_csv(distance2, here("out","distance2.csv"))

#join in the scores----------------------------
noc2_nested <- skills_by_census_noc|>
  nest(noc_data=-id)
cip2_nested <- skills_by_cip2|>
  nest(cip_data=-id)
#get the differences in skill/work activities--------------------
all_the_data2 <- distance2|>
  inner_join(noc2_nested, by=c("NOC"="id"))|>
  inner_join(cip2_nested, by=c("CIP"="id"))|>
  mutate(level_diff=map2(noc_data, cip_data, get_difference))
#prepare the file for writting to excel------------------
unnested2 <- all_the_data2|>
  select(-noc_data, -cip_data)|>
  unnest(level_diff)|>
  pivot_wider(names_from = thing, values_from = difference, names_prefix = "noc-cip ")|>
  mutate(NOC=str_remove_all(NOC, "noc: "),
         CIP=str_remove_all(CIP, "cip: "))|>
  separate(NOC, into=c("census_noc_description","census_noc_code"), sep=": ")|>
  separate(CIP, into=c("census_cip_description","census_cip_code"), sep=": ")

openxlsx::write.xlsx(unnested2, here("out", "cip_noc_differences2.xlsx"))

cip_onet2 <- skills_by_cip2|>
  mutate(id=str_remove_all(id, "cip: "))|>
  separate(id, into=c("census_cip_description","census_cip_code"), sep=": ")
colnames(cip_onet2) <- c(colnames(cip_onet2)[1:2], paste0("cip: ", colnames(cip_onet2)[-c(1,2)]))

openxlsx::write.xlsx(cip_onet2, here("out", "cip_onet2.xlsx"))


