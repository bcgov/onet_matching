library(tidyverse)
library(here)
set.seed(123)

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

num_sim <- 100000 #social planner gives up after this many tries
results <- tibble(sim = 1:num_sim, #initialize a dataframe to store simulation results
                  nocs = vector(length = num_sim, mode = "list"),
                  mean_distance = NA_real_)

for(i in 1:nrow(results)){
  if(i==1){#in the first iteration
    original <- test_stripped|> # test stripped contains only id, NOC and CIP
      left_join(cip_noc_diff,
                by = c("NOC21"= "census_noc_code", "CIP2021"="census_cip_code"))|> #adds in the distances
      mutate(prob=distance/sum(distance))#used below for drawing two observations to swap occupations
    original_distance <- mean(original$distance) #baseline for comparison
    changepoints <- sample(original$id,
                           size=2,
                           replace = FALSE,
                           prob = original$prob) #the two observations we are going to swap NOCs
    new <- original|> #take the original allocation then
      mutate(NOC21= replace(NOC21,
                            changepoints,
                            NOC21[rev(changepoints)]))|>#swap the NOCs of the two observations
      select(-distance)|> #git rid of the old distance measure (it is wrong post-swap)
      left_join(cip_noc_diff,
                by = c("NOC21"= "census_noc_code",
                       "CIP2021"="census_cip_code"))#adds the correct distances (given the swap)
    new_distance <- mean(new$distance)#calculate the new mean distance
    if(new_distance<original_distance){#if the swap reduced the mean distance
      results$nocs[[i]] <- new$NOC21#new NOCs (starting point in the next iteration)
      results$mean_distance[[i]] <- new_distance#save the new distance
    }else{#if our swapping two NOCs did not reduce the mean distance
      results$nocs[[i]] <- original$NOC21 #keep original NOCs (better than the swap)
      results$mean_distance[[i]] <- original_distance #keep original distance (better than the swap)
    }
  }else{#for all subsequent iterations
    original <- test_stripped|>
      select(-NOC21)|> #get rid of the original allocation of NOCs
      bind_cols(NOC21=results$nocs[[i-1]])|> #add in the NOCs from the previous iteration
      left_join(cip_noc_diff,
                by = c("NOC21"= "census_noc_code",
                       "CIP2021"="census_cip_code"))|>#add distances (using last iterations NOCs)
      mutate(prob=distance/sum(distance)) #used below for drawing two observations to swap occupations
    original_distance <- mean(original$distance) #baseline for comparison
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
    new_distance <- mean(new$distance)#calculate the mean distance (given the swap)
    if(new_distance<original_distance){#if the swap reduced the mean distance
      results$nocs[[i]] <- new$NOC21 #save the swapped NOCs for use in next iteration
      results$mean_distance[[i]] <- new_distance #save the new average distance
    }else{#if the swap did not reduce the mean distance
      results$nocs[[i]] <- original$NOC21 #save the unchanged NOCs for use in next iteration
      results$mean_distance[[i]] <- original_distance #save the original distance
    }
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

compare_swap <- bind_rows(predict_original, predict_swapped)

write_rds(compare_swap, here("out", "compare_swap.rds"))
write_rds(results, here("out","simulation results.rds"))



