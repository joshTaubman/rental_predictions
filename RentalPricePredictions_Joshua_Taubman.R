#The goal of this project is to estimate the rental price of property using the
#other columns as predictors

#Ensure the csv file is in your wd
dat<- read.csv("House_Rent_Dataset.csv")
dat

max(dat$Size)
library(caret)
library(tidyverse)
#Create Training and Test Sets
test_indexes <- createDataPartition(y = dat$Rent,times = 1, 0.1, list = FALSE)
test_data <- dat[test_indexes,]
train_data <- dat[-test_indexes,]
test_data
length(test_data$Rent)
length(train_data$Rent)

#Try out the most basic linear model using only the size as a predictor
linear_fit <- lm(Rent~ Size, data = train_data)
y_hat_lm <- predict(linear_fit, newdata = test_data)

#RMSE 
rmse<- function(prediction, true){
  sqrt(mean(prediction-true)^2)
  }
rmse(y_hat_lm, test_data$Rent)
#Essentially a linear model is out by a bit (2160 in rent), 
#so we have to come up with something smarter

#Could break down the size of the property into levels and then predict based
#off the average rent accross that size bracket 

#Assign level based upon int
#Plot with hist

hist(train_data$Size, breaks = 20)
level_lower_bound <- cut(train_data$Size, breaks = seq(0,8100,100),
                        labels = sapply(seq(0,8000,100), toString))
df_l <- data.frame(level_lower_bound)
colnames(df_l) <- "level_lwer_bound"
train_data <- train_data %>% cbind(df_l, .)
names(train_data)

#Replicate process but this time for test data
level_lower_bound_test <- cut(test_data$Size, breaks = seq(0,8100,100),
                              labels = sapply(seq(0,8000,100), toString))

df_l_t <- data.frame(level_lower_bound_test)
colnames(df_l_t) <- "level_lwer_bound"
test_data <- test_data %>% cbind(df_l_t, .)

#Do I seperate with a tune grid to optimise the selection?
# <- train(tuneGrid = seq(0,8000, 10))

#Include the average for all property rent 'mu' so that 
#we are only looking at the bias 
mu <- mean(train_data$Rent)

#Average across floor size brackets
level_averages <- train_data %>%
  group_by(level_lwer_bound) %>%
  summarise(level_avg = mean(Rent-mu))
level_averages
train_data$level_lwer_bound
level_averages$level_lwer_bound

train_data <- train_data %>%
  left_join(level_averages, by = "level_lwer_bound") 

#We attach the level averages from the train data to the test data
test_data <- test_data %>% 
  left_join(level_averages, by = "level_lwer_bound") 
#Make predictions based upon the property size bracket 

#This prediction is very unstable and sometimes NAs are returned 
#Could have a method for dealing with NAs 
#Nas come from when the test stratum has a value but there is no corresponding
#average from the train data

#If we have an NA we can take the slope from the linear regression model and
#multiply it by the size of the property to get an estimate to replace the NA


for (i in 1:length(test_data$level_avg)) {
  test_data$level_avg[i] <- ifelse(is.na(test_data$level_avg[i] == TRUE),
         (predict(linear_fit, newdata = test_data))[i]
         ,test_data$level_avg[i])
}

y_hat_size_strata <- mu + test_data$level_avg 
(test_data$level_avg)

#Test size strata method and compare to using linear model
linear_model<- rmse(y_hat_lm, test_data$Rent)
size_strata <- rmse(y_hat_size_strata, test_data$Rent)


rmse_results <- data.frame(method = "Linear Regression on Size", 
                           RMSE = linear_model)

rmse_results <- bind_rows(rmse_results,
                          data.frame(method= "Property Size Brackets",
                                     RMSE = size_strata))
rmse_results %>% knitr::kable()
rmse_results
#Need a way of addressing NAs 

#Ok then what else can we do?
#Average across other categories then add these predictors as well
names(train_data)
#Start with City
#How am I going to do weighting? Do I do a subtraction based model as in before
#so I just have the bias for each factor?
names(train_data)
city_averages <- train_data %>%
  group_by(City) %>%
  summarise(city_avgs = mean(Rent - level_avg- mu))
  

city_averages %>%
  ggplot(aes(City,city_avgs))+
  geom_point()

train_data <- train_data %>%
  left_join(city_averages, by ="City")

#Model taking into account biases of city and size strata
test_data <- test_data %>% 
  left_join(city_averages, by = "City")

y_hat_3 <- mu + test_data$city_avgs + test_data$level_avg
strat_cit <- rmse(y_hat_3, test_data$Rent)

rmse_results <- bind_rows(rmse_results,
                          data.frame(method= "Size Bracket and City",
                                     RMSE = strat_cit))

rmse_results %>% knitr::kable()

#It's actually worse when including city bias!
#I think this means that regularisation is very important as the graph implies
#very high levels of seperation within city_avgs (high sd)

#Regularisation:

#Add residuals 
#Based off template in machine learning component of course
names(train_data)
lambdas <- seq(0, 10^4, 50)
rmses <- sapply(lambdas, function(l){
  mu <- mean(train_data$Rent)
  level_average_reg <- train_data %>%
#Can't use group by either
    group_by(level_lwer_bound) %>%
    summarize(level_avg_reg = sum(Rent - mu)/(n()+l))

#left join will be problematic
#can't left join on a list but what exactly does this bug mean?

#colnames(level_average_reg) <- level_avg_reg 
  city_average_reg <- train_data %>% 
    group_by(level_lwer_bound) %>%
    left_join(level_average_reg, by = "level_lwer_bound") %>%
    ungroup() %>%
    group_by(City) %>%
    summarize(city_avg_reg = sum(Rent - level_avg_reg - mu)/(n()+l))
  predicted_rent <- 
    test_data %>% 
    group_by(level_lwer_bound) %>%
    left_join(level_average_reg, by = "level_lwer_bound") %>%
    ungroup() %>%
    group_by(City)%>%
    left_join(city_average_reg, by = "City") %>%
    mutate(pred = mu + level_avg_reg + city_avg_reg) %>%
    .$pred
  
  #Replace the few NAs with linear fit
  for (i in 1:length(predicted_rent)) {
    predicted_rent[i] <- ifelse(is.na(predicted_rent[i] == TRUE |
                                      predicted_rent[i] <= 0),
                                (predict(linear_fit, newdata = test_data))[i]
                                ,predicted_rent[i])
  }
  return(rmse(predicted_rent, test_data$Rent))
})
rmses
qplot(lambdas, rmses)  
#lambda approaches infinity gives the best result
#i.e. penalise the square terms as much as possible
# l<- 0
# level_average_reg <- train_data %>%
#   group_by(level_lwer_bound) %>%
#   summarise(level_avg_reg = sum(Rent - mu)/(n()+l))
# train_data %>% 
#   left_join(level_avg_reg, by = "level_lwer_bound")
# 
# length(train_data$Size)
# level_avg_reg
# bind_rows(level_average_reg, train_data)
# names(level_average_reg$level_avg_reg)
#  
# train_data %>% 
#   group_by(level_lwer_bound) %>%
#   left_join(level_average_reg, by = "level_lwer_bound") %>%
#   ungroup()
# 
# 
# city_average_reg <- train_data %>% 
#   group_by(level_lwer_bound) %>%
#   left_join(level_average_reg, by = "level_lwer_bound") %>%
#   ungroup() %>%
#   group_by(City) %>%
#   summarize(city_avg_reg = sum(Rent - level_avg_reg - mu)/(n()+l))

# thinks there is an issue with level_lwer_bound being a list... why now again?
# names(test_data)
# test_data %>% 
#   group_by(level_lwer_bound) %>%
#   left_join(level_average_reg, by = "level_lwer_bound")
# #This section is good now
# city_average_reg
# 
# predicted_rent <- 
#   test_data %>% 
#   group_by(level_lwer_bound) %>%
#   left_join(level_average_reg, by = "level_lwer_bound") %>%
#   ungroup() %>%
#   group_by(City)%>%
#   left_join(city_average_reg, by = "City") %>%
#   mutate(pred = mu + level_avg_reg + city_avg_reg) %>%
#   .$pred
# #Remove Nas like we did for level_avg
# predicted_rent
# for (i in 1:length(predicted_rent)) {
#   predicted_rent[i] <- ifelse(is.na(predicted_rent[i] == TRUE | 
#                                       predicted_rent[i] <= 0 ),
#                                    (predict(linear_fit, newdata = test_data))[i]
#                                    ,predicted_rent[i])
#   
# }
ind<- which.min(rmses)
ind
lambda_best <- (ind-1) *50
lambda_best
#rmse(predicted_rent, test_data$Rent)
#lamda = 0 is the most effective here, but wouldn't this just be the city+
#size strata model then?... 

# rmse(predicted_rent[1:476],test_data$Rent[1:476])
# length(predicted_rent)
# length(test_data$Rent)
# predicted_rent


rmse_results <- bind_rows(rmse_results,
                          data.frame(method= paste("Regularisation, lambda =", 
                                     lambda_best),
                                     RMSE = rmses[ind]))
rmse_results %>% knitr::kable()

just_mu <- rmse(mu, test_data$Rent)

rmse_results <- bind_rows(rmse_results,
                          data.frame(method= "Just using average rent",
                                     RMSE = just_mu))

rmse_results %>% knitr::kable()
#predicted_rent

#There is no consistent choice of the best lambda, as it varies based upon
#The partition chosen for test/train data

#Also the average being so effective, though occasionally it is not 
#All this implies that I need to bootstrap my sample

#To improve I think bootstrapping is the most important thing I can do
#How is this going to work...

#Need to investigate resampling process
indexes_boot <- createResample(train_data$Rent, 10)
indexes_boot
#What we had was we were splitting the data based on columns not rows
length(train_data)
#Test out bootstrapping, just calculating mu

mu_boot <- sapply(indexes_boot, function(ind){
  rent_boot<- train_data$Rent[ind]
  mean(rent_boot)
})
mu_boot
mean(mu_boot)
mean(mu_boot)-mu
#Why are mu and mean(mu_boot) so different???
train_data$Rent
mu
y_hat_mu_boot <- mean(mu_boot)
rmse_mu_boot <- rmse(y_hat_mu_boot, test_data$Rent)
rmse_mu_boot

rmse_results <- bind_rows(rmse_results,
                          data.frame(method= "Bootstrap using just Mu",
                                     RMSE = rmse_mu_boot))

rmse_results %>% knitr::kable()
train_data$level_avg
levels_boot <- sapply(indexes_boot, function(ind){
  level_boot<- train_data$level_avg[ind]
  rent_l_boot <- mu - level_boot
  mean(rent_l_boot)
})
y_hat_mu_l_boot <- mean(levels_boot)
rmse_mu_l_boot <- rmse(y_hat_mu_l_boot, test_data$Rent)

rmse_results <- bind_rows(rmse_results,
                          data.frame(method= "Bootstrap with Size Level",
                                     RMSE = rmse_mu_l_boot))

rmse_results %>% knitr::kable()

#Now we do the same for regularisation
#We will need to still take a value for Mu (do we select the median?)
rmses
#Resampled indexes
indexes_boot


######## rmse function using the relevant lambdas
#lambdas only go up to 10^3 to reduce run time
lambdas<- seq(0, 10^3, 50)
rmse_boot<- sapply(indexes_boot, function(ind){
    sapply(lambdas, function(l){
  mu <- mean(train_data$Rent[ind])
  #To select the rows based upon index we need to turn our train and test data
  # into data frames
  train_df <- train_data %>% data.frame(.)
  test_df <- test_data %>% data.frame(.)
  train_df_boot <- train_df[ind,]
  test_df_boot <- test_df[-ind,]
  
  level_average_reg <- train_df_boot %>%
    #Can't use group by either
    group_by(level_lwer_bound) %>%
    summarize(level_avg_reg = sum(Rent - mu)/(n()+l))
  
  #left join will be problematic
  #can't left join on a list but what exactly does this bug mean?
  
  #colnames(level_average_reg) <- level_avg_reg 
  city_average_reg <- train_df_boot %>% 
    group_by(level_lwer_bound) %>%
    left_join(level_average_reg, by = "level_lwer_bound") %>%
    ungroup() %>%
    group_by(City) %>%
    summarize(city_avg_reg = sum(Rent - level_avg_reg - mu)/(n()+l))
  predicted_rent <- 
    test_df_boot %>% 
    group_by(level_lwer_bound) %>%
    left_join(level_average_reg, by = "level_lwer_bound") %>%
    ungroup() %>%
    group_by(City)%>%
    left_join(city_average_reg, by = "City") %>%
    mutate(pred = mu + level_avg_reg + city_avg_reg) %>%
    .$pred
  
  #Replace the few NAs with linear fit
  for (i in 1:length(predicted_rent)) {
    predicted_rent[i] <- ifelse(is.na(predicted_rent[i] == TRUE |
                                        predicted_rent[i] <= 0),
                                (predict(linear_fit, newdata = test_data))[i]
                                ,predicted_rent[i])
  }
  #predicted_rent_avg<- mean(predicted_rent)
  print(predicted_rent)
  predicted_rent_avg<- mean(predicted_rent)
  return(rmse(predicted_rent_avg, test_data$Rent))
})
})
#I want to return the predicted_rent that I get from inside the function
#Why is predicted rent not found?????

rmse_boot_mat<- as.matrix(rmse_boot)
average_accross_boots <- rowMeans(rmse_boot_mat)
average_accross_boots

ind_b<- which.min(average_accross_boots)
ind_b
lambda_best_b <- (ind_b-1) *50
lambda_best_b

rmse_results <- bind_rows(rmse_results,
                          data.frame(method= paste("Reg with Boot, lambda =", 
                                                   lambda_best_b),
                                     RMSE = average_accross_boots[ind_b]))
rmse_results %>% knitr::kable()

rmse_results

#Wait for this one have I just got a more accurate evaluation of the error?
#It does also help select a lambda that's true

#There is still no consistency as to which is the best method...

