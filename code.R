#' This script does the following... slowly:
#' 1) creates a skill/work activities profile for each field of study (CIP)
#' 2) calculates distance between the skill/work activities profile for each CIP/NOC combination with + count.
#' 3) it also calculates the NOC-CIP difference in skills/work activities for each combo.
#libraries------------------------------------
library(tidyverse)
library(readxl)
library(here)
library(janitor)
library(tidytext) # reorder_within
#functions------------------
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
           weight1=if_else(weight>=.01, weight, 0), #replacing all weights less than .01 with 0
           weight1=weight1/sum(weight1), #weights, conditional on being greater than .01 (conditional weights sum to 1)
    )
}
get_weighted_mean <- function(tbbl){
  #' Input: a tibble with columns noc, count, weight and weight1
  #' Output: a long format tibble with columns thing and level
  inner_join(tbbl, onet_long)|> #joins the weights to the skills data
    group_by(thing)|> #group by the skill/work activity
    mutate(weighted1=Level*weight1)|> #multiply skill/work activity Level times the weights
    summarize(Level=round(sum(weighted1),2)) #add up all the weighted values
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
#cip by noc table-------------------
#' the skill/work abilities score for a CIP is a weighted average of the NOC scores, where the
#' weights are based on the employment counts in the NOC/CIP table.
cip_noc <- vroom::vroom(here("raw_data","stats_can", "cip_noc.csv"), skip= 13, n_max = 436) #garbage before and after data
colnames(cip_noc)[1] <- "Field of Study" #missing column name
cip_noc <- cip_noc[-1,] #garbage in first row
#long format cip_noc table--------------------
cip_long <- cip_noc%>%
  select(!contains("..."))%>%# columns containing ... are garbage.
  pivot_longer(cols=-"Field of Study", names_to="noc", values_to = "count")%>%
  mutate(count=as.numeric(str_replace_all(count,",","")), #counts strings with commas
         noc=str_sub(noc, 1 , 5) #discard description part of noc (could differ from other files?)
  )
#nested dataframe with noc counts for each cip--------------------
noc_counts_by_cip <- cip_long|>
  group_by(`Field of Study`)|>
  mutate(total=sum(count), #how many employed in each noc/cip combo (just so we can arrange by)
         num_nocs= sum(count>0), #just for interest, number of destination NOCs associated with each CIP
         )|>
  group_by(`Field of Study`, total, num_nocs)|> #grouping by FOS... total and num_nocs would be dropped if not included.
  nest()|>
  arrange(desc(total))
#get the weighted mean skills for each cip---------------------
skills_by_cip <- noc_counts_by_cip|>
  mutate(data=map(data, add_weights),
         skills=map(data, get_weighted_mean)
        )|>
  select(-data)
#we are going to calculate the euclidean distance between the skill/work activity profiles of each NOC and CIP----------
#the skill/work activities profile by NOC----------------
noc_onet <- onet_long_name|>
  pivot_wider(id_cols = noc, names_from = thing, values_from = Level)|>
  rename(id=noc)
assertthat::assert_that(all(str_detect(noc_onet$id, ": ")), msg = "Every NOC has to be separated by :") #code below depends on it

#the skill/work activities profile by CIP--------------------------
cip_onet <- skills_by_cip|>
  ungroup()|>
  select(`Field of Study`, skills)|>
  unnest(skills)|>
  pivot_wider(id_cols = `Field of Study`, names_from = thing, values_from = Level)|>
  rename(id=`Field of Study`)
assertthat::assert_that(!all(str_detect(cip_onet$id, ": ")), msg = "CIP cannot include : ") #code below depends on it


#row_bind the NOC and CIP data into single tibble (full_join safer than row_bind)
scaled_onet <- full_join(noc_onet, cip_onet)|>
  column_to_rownames("id")|>
  na.omit()|>
  scale()
#principal components of scaled data (distance metrics do not work well in 76D space)-----------------
pca_onet <- prcomp(scaled_onet)
factoextra::fviz_eig(pca_onet) #looks like first 5 should do it
first_five <- pca_onet$x[,1:5]
#Euclidean distance between all NOCs and CIPs using
dist_mat <- dist(first_five)|>
  as.matrix(nrow=nrows(first_five))
#use distance matrix to create a long dataframe with columns NOC, CIP and distance
distance <- dist_mat|>
  as.data.frame()|>
  rownames_to_column("id1")|>
  pivot_longer(cols=-id1, names_to = "id2", values_to = "distance")|>
  filter(str_detect(id1, ": "), #assumes that NOCs have a : separating the number and description
         !str_detect(id2, ": "))|> #assumes that CIPs do not have a : in their name.
  rename(NOC=id1,
         CIP=id2)|>
  mutate(distance=round(distance, 2))|>
  arrange(distance)
#add in the counts from CIP/NOC table...
counts <- mapping|>
  select(-o_net_soc_code)|>
  distinct()|>
  inner_join(cip_long)|>
  rename(CIP=`Field of Study`)|>
  unite(NOC, noc, description, sep=": ")
with_counts <- inner_join(distance, counts)|>
  arrange(desc(count))
#filter to reduce file size--------------------------
only_positive_counts <- with_counts|>
  filter(count>0)
#join in the scores----------------------------
noc_nested <- noc_onet|>
   nest(noc_data=-id)
cip_nested <- cip_onet|>
   nest(cip_data=-id)
#get the differences in skill/work activities (takes a while)--------------------
all_the_data <- only_positive_counts|>
  inner_join(noc_nested, by=c("NOC"="id"))|>
  inner_join(cip_nested, by=c("CIP"="id"))|>
  mutate(level_diff=map2(noc_data, cip_data, get_difference))
#prepare the file for writting to excel------------------
unnested <- all_the_data|>
  select(-noc_data, -cip_data)|>
  unnest(level_diff)|>
  pivot_wider(names_from = thing, values_from = difference, names_prefix = "noc-cip ")
openxlsx::write.xlsx(unnested, here("out", "cip_noc_differences.xlsx"))
openxlsx::write.xlsx(cip_onet, here("out", "cip_onet.xlsx"))

#exploration-------------
major_groups <- c("Legislative and senior management occupations",
                  "Business, finance and administration occupations",
                  "Natural and applied sciences and related occupations",
                  "Health occupations",
                  "Occupations in education, law and social, community and government services",
                  "Occupations in art, culture, recreation and sport",
                  "Sales and service occupations",
                  "Trades, transport and equipment operators and related occupations",
                  "Natural resources, agriculture and related production occupations",
                  "Occupations in manufacturing and utilities")
noc1 <- as.character(0:9)

noc_names <- tibble(major_groups=major_groups, noc1=noc1)


with_noc_groups <- unnested|>
  mutate(noc1=str_sub(NOC, 1, 1),
         noc2=str_sub(NOC, 2, 2), .after=NOC)|>
  left_join(noc_names)|>
  relocate(major_groups, .after="noc1")

with_noc_groups|>
  group_by(major_groups, noc2)|>
  mutate(weight=count/sum(count),
         dist_times_weight=distance*weight
         )|>
  summarize(ave_distance=sum(dist_times_weight))|>
  mutate(noc2=reorder_within(noc2, ave_distance, major_groups, mean))|>
  ggplot(aes(ave_distance, noc2))+
  geom_col(alpha=.5)+
  labs(x="CIP/NOC weighted average distance",
       y="TEER",
       title="CIP/NOC Matching is worst in Manufacturing and Utilities, best in Health.",
       subtitle="CIP/NOC Matching is worst in TEER 0 (Management) and TEER 5 (occupations usually require short-term work demonstration and no formal education)"
       )+
    facet_wrap(~fct_reorder(major_groups, ave_distance, .desc=TRUE), scales = "free_y")+
  scale_y_reordered()+
  theme_minimal()






