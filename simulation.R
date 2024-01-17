library(tidyverse)
library(here)

no_distance <- read_rds(here("out","no_distance.rds"))|>
  mutate(NOC21=as.integer(NOC21),
         CIP2021=as.integer(CIP2021)
  )

cip_noc_diff <- readxl::read_excel(here("out","cip_noc_differences2.xlsx"),
                                   col_types = c(rep("text",4), rep("numeric", 77)))|>
  select(-starts_with("noc-cip"))|>
  mutate(census_noc_code=as.integer(census_noc_code),
         census_cip_code=as.integer(census_cip_code)
         )

#split the data
train_and_test <- no_distance|>
  mutate(set=sample(c("train", "test"), size=nrow(no_distance), replace = TRUE, prob = c(.9, .1)))

train <- train_and_test|>
  filter(set=="train")|>
  select(-set)|>
  left_join(cip_noc_diff, by = c("NOC21"= "census_noc_code", "CIP2021"="census_cip_code"))|>
  select(-NOC21, -CIP2021, -EmpIn)

test <- train_and_test|>
  filter(set=="test")|>
  select(-set)|>
  rowid_to_column("id")

test_stripped <- test|>
  select(id, NOC21, CIP2021)


#SIMULATION--------------------------------

set.seed(123)
num_sim <- 100000 #social planner gives up after this many tries
cutoff <- .5 #only swap occupations if proportional improvement is greater than this
results <- tibble(sim = 1:num_sim, #initialize a dataframe to store simulation results
                  nocs = vector(length = num_sim, mode = "list"),
                  mean_distance = NA_real_)

for(i in 1:nrow(results)){
  if(i==1){#in the first iteration
    original <- test_stripped|> # test stripped contains only id, NOC and CIP
      left_join(cip_noc_diff,
                by = c("NOC21"= "census_noc_code", "CIP2021"="census_cip_code"))|> #adds in the distances
      mutate(prob=distance/sum(distance))#used below for drawing two observations to swap occupations
  }else{
    original <- test_stripped|>
      select(-NOC21)|> #get rid of the original allocation of NOCs
      bind_cols(NOC21=results$nocs[[i-1]])|> #add in the NOCs from the previous iteration
      left_join(cip_noc_diff,
                by = c("NOC21"= "census_noc_code",
                       "CIP2021"="census_cip_code"))|>#add distances (using last iterations NOCs)
      mutate(prob=distance/sum(distance)) #used below for drawing two observations to swap occupations
  }
    changepoints <- sample(original$id,
                           size=2,
                           replace = FALSE,
                           prob = original$prob) #two observations where we will swap NOCs
    new <- original|> #take  the original (for this iteration) data, then
      mutate(NOC21= replace(NOC21,
                            changepoints,
                            NOC21[rev(changepoints)]))|> #swap the NOCs for two of the observations
      select(-distance)|> #distance is now wrong given the swap
      left_join(cip_noc_diff,
                by = c("NOC21"= "census_noc_code",
                       "CIP2021"="census_cip_code"))#add in the correct distances
    proportion_improvement <- (sum(original$distance)-sum(new$distance))/mean(original$distance)
    if(proportion_improvement>cutoff){#if significant reduction in distance
      results$nocs[[i]] <- new$NOC21 #save the swapped NOCs for use in next iteration
      results$mean_distance[[i]] <- mean(new$distance) #save the new average distance
    }else{#if the swap did not reduce the mean distance
      results$nocs[[i]] <- original$NOC21 #save the unchanged NOCs for use in next iteration
      results$mean_distance[[i]] <- mean(original$distance) #save the original distance
    }
  print(paste(scales::percent(i/num_sim, accuracy = 1), "complete")) #how much longer do I have to wait?
}


test_original <- test|>
  select(-id)|>
  left_join(cip_noc_diff, by = c("NOC21"= "census_noc_code", "CIP2021"="census_cip_code"))|>
  select(-NOC21, -CIP2021, -EmpIn)

test_swapped <- test|>
  select(-NOC21, -id)|>
  bind_cols(NOC21=results$nocs[[num_sim]])|>
  left_join(cip_noc_diff, by = c("NOC21"= "census_noc_code", "CIP2021"="census_cip_code"))|>
  select(-NOC21, -CIP2021, -EmpIn)

#the income model----------------
mod1 <- lm(log_income ~ . , data = train)

predict_original <- tibble(data="original", predictions=predict(mod1, newdata = test_original))
predict_swapped <- tibble(data="swapped", predictions=predict(mod1, newdata = test_swapped))

noc_change <- bind_cols(original=test_original$census_noc_description, swapped=test_swapped$census_noc_description)|>
  mutate(unchanged=if_else(original==swapped, "same", "changed"))

compare_swap <- bind_rows(predict_original, predict_swapped)

write_rds(compare_swap, here("out", "compare_swap.rds"))
write_rds(results, here("out","simulation results.rds"))
write_rds(noc_change, here("out","noc_change.rds"))

#take a look------------------------

table(noc_change$unchanged)/nrow(noc_change)

compare_swap|>
  group_by(data)|>
  summarise(mean_income=scales::dollar(mean(exp(predictions))),
            sd_income=scales::dollar(sd(exp(predictions))))


ggplot(results, aes(sim, mean_distance))+
  geom_line()+
  scale_x_continuous(labels=scales::comma)+
  labs(x="Simulation Number",
       y="Average skill gap distance",
       title="Shuffling occupations can reduce the average skill gap."
  )






