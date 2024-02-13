set.seed(123)
my_dt <- function(tbbl) {
  DT::datatable(tbbl,
                extensions = "Buttons",
                rownames = FALSE,
                options = list(
                  columnDefs = list(list(className = "dt-center", targets = "_all")),
                  paging = TRUE,
                  scrollX = TRUE,
                  scrollY = TRUE,
                  searching = TRUE,
                  ordering = TRUE,
                  dom = "Btip",
                  buttons = list(
                    list(extend = "csv", filename = "some_file_name"),
                    list(extend = "excel", filename = "some_file_name")
                  ),
                  pageLength = 20,
                  lengthMenu = c(3, 5)
                )
  )
}

#simulation results----------------------
simulation_results <- read_rds(here("out","simulation results.rds"))
compare_swap <- read_rds(here("out","compare_swap.rds"))
noc_change <- read_rds(here("out","noc_change.rds"))
#for the heatmap------------------
distance2 <- read_csv(here("out","distance2.csv"))
dist_mat <- distance2|>
  mutate(NOC=str_sub(NOC, start=6),
         CIP=str_sub(CIP, start=6)
  )|>
  separate(NOC, into=c("NOC","NOC_number"), sep=": ")|>
  separate(CIP, into=c("CIP","CIP_number"), sep=": ")|>
  select(-NOC_number,-CIP_number)|>
  mutate(CIP=str_trunc(CIP, 50),
         NOC=str_trunc(NOC, 50))|>
  pivot_wider(names_from = CIP, values_from = distance)|>
  column_to_rownames("NOC")|>
  as.matrix()

#for proportion table-------------------
cip2_noc5 <- vroom::vroom(here("raw_data","stats_can","cip2_noc5.csv"), skip = 15, n_max = 512, col_names = FALSE)
colnames(cip2_noc5) <- vroom::vroom(here("raw_data","stats_can","cip2_noc5.csv"), skip = 10, n_max = 1, col_names = FALSE)
colnames(cip2_noc5)[1] <- "noc"
cip2_noc5 <- cip2_noc5|>
  mutate(noc=str_sub(noc, start=7))|>
  select(noc, `Health and related fields`)|>
  arrange(desc(`Health and related fields`))|>
  mutate(Proportion=`Health and related fields`/sum(`Health and related fields`),
         `Truncated Proportion`=if_else(Proportion>.01, Proportion, 0),
         `Adjusted Proportion`=`Truncated Proportion`/sum(`Truncated Proportion`)
  )|>
  slice_head(n=20)|>
  mutate(across(`Proportion`:`Adjusted Proportion`, ~ round(.x, 4)))
#for the income regression-------------------------
filtered <- read_rds(here("out","filtered.rds"))
filtering_info <- read_rds(here("out","filtering_info.rds"))
#for the logistic regression--------------------------
filtered2 <- read_rds(here("out","filtered2.rds"))
filtering_info2 <- read_rds(here("out","filtering_info2.rds"))

#for text---------------------
num_occupations <- length(unique(filtered$`NOC vs. Admin and financ...:`))
num_cip <- length(unique(filtered$`CIP vs. Agriculture...:`))
#split the data
train_and_test <- filtered|>
  mutate(set=sample(c("train", "test"), size=nrow(filtered), replace = TRUE, prob = c(.9, .1)))
train <- train_and_test|>
  filter(set=="train")|>
  select(-set)
test <- train_and_test|>
  filter(set=="test")|>
  select(-set)

train_and_test2 <- filtered2|>
  mutate(set=sample(c("train", "test"), size=nrow(filtered2), replace = TRUE, prob = c(.9, .1)))
train2 <- train_and_test2|>
  filter(set=="train")|>
  select(-set)

null_prob <- table(train2$full_time_full_year)[2]/nrow(train2)
test2 <- train_and_test2|>
  filter(set=="test")|>
  select(-set)

#the income model----------------
mod1 <- lm(log_income ~ . , data = train)
#the logit model-------------------------------
mod2 <- glm(full_time_full_year ~ . , family=binomial(link='logit'), data=train2)

margins_mod2 <- margins(mod2)|> #cant interpret logit (log odds ratio): margins translates to probabilities
  summary()|>
  separate(factor, into=c("variable", "level"), sep=":")

#assess accuracy-----------------
rmse_in_sample <- sqrt(mean(mod1$residuals^2))
test_w_pred <- test|>
  mutate(prediction= predict(mod1, newdata = test))
rmse_out_of_sample <- test_w_pred|>
  mutate(error_squared=(log_income-prediction)^2)|>
  summarize(mean_squared_error=mean(error_squared))|>
  pull()|>
  sqrt()

#rmse_in_sample2 <- sqrt(mean(mod2$residuals^2))

test_w_pred2 <- test2|>
  mutate(prediction= predict(mod2, newdata = test2),
         prediction=  round(exp(prediction)/(1+exp(prediction))), #inverse logit, then round to 0 or 1
         null_prediction = sample(c(0,1), size=nrow(test2), replace=TRUE, prob=c(1-null_prob, null_prob))
  )

confusion <- with(test_w_pred2, table(full_time_full_year, prediction))
prediction_accuracy <- (confusion[1,1]+confusion[2,2])/sum(confusion)

null_confusion <- with(test_w_pred2, table(full_time_full_year, null_prediction))
null_prediction_accuracy <- (null_confusion[1,1]+null_confusion[2,2])/sum(null_confusion)

test_logit <- glance(with(test_w_pred2, chisq.test(full_time_full_year, prediction)))
null_test_logit <- glance(with(test_w_pred2, chisq.test(full_time_full_year, null_prediction)))

# Adjust standard errors for stargazer
cov1         <- vcovHC(mod1, type = "HC1")
robust_se1    <- sqrt(diag(cov1))
cov2        <- vcovHC(mod2, type = "HC1")
robust_se2    <- sqrt(diag(cov2))

# Robust standard errors for plot ---------------
mod1rse <- coeftest(mod1, cov1)
mod1_coef <- broom::tidy(mod1rse, conf.int = TRUE)|>
  separate(term, into=c("variable","level"),sep=":`")|>
  mutate(variable=str_remove_all(variable, "`"),
         level= if_else(is.na(level), variable, level),
         level=str_trunc(level, 50)
  )|>
  filter(!variable %in% c("(Intercept)",
                          "Language vs. not english",
                          "Gender vs. Woman+"
  ))|>
  arrange(variable, level)|>
  mutate(estimate=exp(estimate)-1,
         conf.low=exp(conf.low)-1,
         conf.high=exp(conf.high)-1
  )

mod2rse <- coeftest(mod2, cov2)
mod2_coef <- broom::tidy(mod2rse, conf.int = TRUE)|>
  separate(term, into=c("variable","level"),sep=":`")|>
  mutate(variable=str_remove_all(variable, "`"),
         level= if_else(is.na(level), variable, level),
         level=str_trunc(level, 50)
  )|>
  filter(!variable %in% c("(Intercept)",
                          "Language vs. not english",
                          "Gender vs. Woman+"
  ))|>
  arrange(variable, level)|>
  mutate(estimate=exp(estimate)-1,
         conf.low=exp(conf.low)-1,
         conf.high=exp(conf.high)-1
  )

#for text--------------------------
distance_effect <- mod1_coef$estimate[mod1_coef$variable=="distance"]
dist_quant <- quantile(filtered$distance, probs = c(.1,.9), na.rm = TRUE)
economic_effect <- (dist_quant[2]-dist_quant[1])*distance_effect

distance_effect2 <- margins_mod2$AME[margins_mod2$variable=="distance"]
dist_quant2 <- quantile(filtered2$distance, probs = c(.1,.9), na.rm = TRUE)
economic_effect2 <- (dist_quant2[2]-dist_quant2[1])*distance_effect2



