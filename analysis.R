#### PART 1 - read in data ####
load(url("https://github.com/ms-soil/ds-submission-soil/raw/main/eusoil.Rdata"))

#### PART 2- load packages ####
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")

library(caret)
library(tidyverse)
library(randomForest)

# taking a first look at the dataset
names(eusoil) # variable names
dim(eusoil) # data set dimensions
as_tibble(eusoil) # overview of data structure

#### PART 3 - data preparation ####
# creating landuse categories from the LC1 variable and deleting the LC1 variable afterwards
# this info (data-info.pdf) can be found at https://github.com/ms-soil/ds-submission-soil
eusoil <- eusoil %>% mutate(landuse = case_when(str_detect(LC1, "A") ~ "artif_land",
                                                str_detect(LC1, "B") ~ "cropland",
                                                str_detect(LC1, "C") ~ "woodland",
                                                str_detect(LC1, "D") ~ "shrubland",
                                                str_detect(LC1, "E") ~ "grassland",
                                                str_detect(LC1, "F") ~ "bare_land",
                                                str_detect(LC1, "G") ~ "water",
                                                str_detect(LC1, "H") ~ "wetland")) %>% 
  select(-LC1)

# remove observations without organic carbon data
table(is.na(eusoil$OC)) # organic carbon data is available in all observations

# distribution of landuses
table(eusoil$landuse)

# factorize landuse
eusoil <- eusoil %>% mutate(landuse = as.factor(landuse))

#### PART 4 - variable selection ####
#### Variable selection I: choose variables which are available in most observations ####

# print availability for every variable
res <- for (i in 1:length(names(eusoil))) { 
  print(table(!is.na(eusoil[,i])))
  print(names(eusoil[i]))
}
# texture (the silt, sand and clay variables) are only available in a fraction of cases
# see which fraction of the dataset misses texture 
condition <- !is.na(eusoil$Clay)
table(condition)[[1]]/(table(condition)[[1]] + table(condition)[[2]])
# texture is missing in 80.5% of cases

#### variable selection II: remove variables that are often NA, not commonly measured or not clearly defined ####
eusoil <- eusoil %>% select(-Clay, -Sand, -Silt, -EC, -pH.CaCl2., -Soil_Stones, -LC1_Desc) %>% 
  select(OC, N, P, K, CaCO3, pH.H2O., Elevation, landuse)

#### variable selection III: influential variables ####
names(eusoil)
## a) numerical variables that correlate with organic carbon but not with each other
relationships <- plot(eusoil[1:2000,]) # plotting the first 2000 observations
relationships
# P (phosphorus), K (potassium) with no clear relationship to organic carbon
# CaCO3 and pH co-dependend -> take pH as it is more common and can represent both
# remaining: N, pH, & elevation

## b) categorical variables that show differences in organic carbon
# landuseis the only factor now
# overview of OC (organic carbon) in landuses
qplot(landuse, OC, data = eusoil, fill = landuse) + geom_boxplot()  + ylab("organic carbon mg C / g soil")
# remaining overall now: N, pH, elevation and landuse

# give some easier names and select the chosen variables
eusoil <- eusoil %>% mutate(pH = pH.H2O., elev = Elevation) %>% 
  select(OC, N, P, pH, elev, landuse)
dim(eusoil)


#### PART 5 - train set / test set / validation set####
#### create training, test and hold-out test set (validation) ####

# create validation set
# I used 10% so I have a large amount of data for training
set.seed(1, sample.kind = "Rounding")
valindex <- createDataPartition(eusoil$OC, p = 0.1, list = F)
temp <- eusoil[-valindex,]
validation <- eusoil[valindex,]

# create test and train sets
# experience from the movielens project tells me to train with a large
# proportion of the data set, so I take 80%
set.seed(1, sample.kind = "Rounding")
testindex <- createDataPartition(temp$OC, p = 0.2, list = F)
trainset <- temp[-testindex,]
testset <- temp[testindex,]

# see how many observations are in each
nrow(trainset); nrow(testset); nrow(validation)

#### PART 6 - RMSE function ####
# created so that the models can be evaluated thoughout the project
RMSE <- function(observed_values, predicted_values){
  sqrt(mean((observed_values - predicted_values)^2))
}

#### PART 7 - Linear models (LM) ####

#### guessing model ####
mu <- mean(trainset$OC)
head(mu)

# here I plot observations vs. predictions
qplot(testset$OC, mu)
rmse_mu <- RMSE(testset$OC, mu)
rmse_mu

# create an RMSE table to always add the RMSEs
rmses <- data.frame(model = "mean", rmse = rmse_mu)
rmses

#### LM with N (Nitrogen) ####

m1 <- lm(OC ~ N, data = trainset) 

# the base will be the testset as a dataframe to predict on
base <- data.frame(testset)
pred1 <- predict(m1, base)
head(pred1)

# view predictions
qplot(testset$OC, pred1, col = testset$landuse)

# get RMSE
rmse1 <- RMSE(testset$OC, pred1)
rmse1

# add RMSE to RMSE table
rmses <- rbind(rmses, 
               data.frame(model = "lm with N", rmse = rmse1))
rmses

#### LM with N, pH #### 
# for comments on the steps, see above

m2 <- lm(OC ~ N + pH, data = trainset)
summary(m2)

base <- data.frame(testset)

pred2 <- predict(m2, base)
head(pred2)

qplot(testset$OC, pred2)
rmse2 <- RMSE(testset$OC, pred2)

rmses <- rbind(rmses, 
               data.frame(model = "lm with N + pH", rmse = rmse2))
rmses


#### LM with N, pH, elevation ####

m3 <- lm(OC~N + pH + elev, data = trainset)
summary(m3)

base <- data.frame(testset)

pred3 <- predict(m3, base)
head(pred3)

qplot(testset$OC, pred3)
rmse3 <- RMSE(testset$OC, pred3)

rmses <- rbind(rmses, 
               data.frame(model = "lm with N + pH + elev.", rmse = rmse3))
rmses


#### lm with N, pH, elevation, landuse ####

m_all <- lm(OC ~ ., data = trainset)
summary(m_all)

base <- data.frame(testset)

pred4 <- predict(m_all, base)
head(pred4)

qplot(testset$OC, pred4)
rmse4 <- RMSE(testset$OC, pred4)
rmse4

rmses <- rbind(rmses, 
               data.frame(model = "lm with all variables", rmse = rmse4))
rmses


#### INTERMDEDITATE RESULT: include all variables in fancier algorithms ####

#### PART 8 - KNN ####
#### KNN [caret] with N, pH, elevation, landuse ####
# as a rule of thumb, sqrt of observations:
sqrt(nrow(trainset)) # = 125

# after some trials, the following range of ks were found useful
ks <- seq(1,15,1)
ks

# checking which is the optimal k
result <- lapply(ks, function(i){
  fit_knn <- knnreg(OC ~ ., data = trainset, k = i)
  pred <-predict(fit_knn, base)
  RMSE(testset$OC, pred)
})
data.frame(result)[1,]

plot(ks, result)

best_k <- ks[which.min(data.frame(result)[1,])] #31
best_k # this is 3
# it appears a better k is lower but I don't want to depend on too few points as this
# may lead to overtraining

best_k <- 10

## using best k

fit_knn <- knnreg(OC ~ ., data = trainset, k = best_k)
pred6 <-predict(fit_knn, base)

qplot(testset$OC, pred6)
rmse6 <- RMSE(testset$OC, pred6)
rmse6

rmses <- rbind(rmses, 
               data.frame(model = "knn with all variables", rmse = rmse6))
rmses
# this is clearly not good
# elevation showed to be the problem


#### KNN [caret] all variables / excl. elevation ####

sqrt(nrow(trainset)) 

ks <- seq(10,30,1)
ks

# checking which is the optimal k
result <- lapply(ks, function(i){
  fit_knn2 <- knnreg(OC ~ N + pH + landuse, data = trainset, k = i)
  pred <-predict(fit_knn2, base)
  RMSE(testset$OC, pred)
})
data.frame(result)[1,]

plot(ks, result)

best_k <- ks[which.min(data.frame(result)[1,])] #31
best_k # this is 17

## using best k

fit_knn2 <- knnreg(OC ~ N + pH + landuse, data = trainset, k = best_k)
pred7 <-predict(fit_knn2, base)

qplot(testset$OC, pred7)
rmse7 <- RMSE(testset$OC, pred7)
rmse7

rmses <- rbind(rmses, 
               data.frame(model = "knn excluding elev.", rmse = rmse7))
rmses

# without elevation, the KNN model works fine, It was not immediately clear why


#### INTERMEDIATE RESULT ####
# The same parameters may not be the best for every model type



#### PART 9 - Random forest ####
#### Random forest [caret] with all variables ####

set.seed(1)

fit_rf <- randomForest(
  OC ~ ., data = trainset, 
  ntree = 250
)

# going from 10 to 100 trees in the only-N model improved the RMSE by ~0.4
# and another 0.2 for 250 trees, hardly then anymore for 500 trees

pred8 <- predict(fit_rf, base)
head(pred8)

qplot(testset$OC, pred8)
rmse8 <- RMSE(testset$OC, pred8)
rmse8

rmses <- rbind(rmses, 
               data.frame(model = "rf with all variables", rmse = rmse8))
rmses

## in this case all variables improve the model





#### PART 10 - ENSEMBLE ####
# use ensemble to predict 
# that is, using the best 2 models and taking a mean of their prediction
pred_ensemble <- (pred7 + pred8)/2 # knn & random forest 

qplot(testset$OC, pred_ensemble)
rmse_ens1 <- RMSE(testset$OC, pred_ensemble)
rmse_ens1

rmses <- rbind(rmses, 
               data.frame(model = "ensemble of knn & rf", rmse = rmse_ens1))
rmses


#### PART 11 - VALIDATION #### 
# final validation on hold-out validation set

# incorporate the mean prediction of knn and rf since they are the best ones

fin_predict_a <- predict(fit_knn2, validation)
fin_predict_b <- predict(fit_rf, validation)

fin_predict <- (fin_predict_a + fin_predict_b) / 2

qplot(validation$OC, fin_predict, col = validation$landuse) +
  geom_vline(xintercept = 150, lty = 2) + geom_vline(xintercept = 400, lty = 2) 

fin_rmse <- RMSE(validation$OC, fin_predict)
fin_rmse

rmses <- rbind(rmses, 
               data.frame(model = "FINAL RMSE OVERALL", rmse = fin_rmse))
rmses

#### CONCLUSION I ####
# not every parameter fits into every model
# model specifications such as k (knn) or ntree (randomforest) are important
# ensembles can improve a prediction

#### PERFORMANCE in ranges of organic carbon ####
# There were ranges of good and less good performances of the model:
# Wow well does the model perform in a certain range, so I divide
# set into 0-200 / 200-400 / 400+ (mg per g organic carbon)

# I put the observations, predictions and land uses in a data frame
df <- data.frame(landuse = validation$landuse,
                 observation = validation$OC, prediction = fin_predict)

# in the following, I divide this set and observe the model performance on
# data ranges

# range 0-150
obs  <- df$observation[df$observation < 150]
pred <- df$prediction[df$observation < 150]

fin_rmse_0_150 <- RMSE(obs, pred)
fin_rmse_0_150

# how many observations are in that range
length(eusoil$OC[eusoil$OC < 150]) / 
  length(eusoil$OC[eusoil$OC])
# it is 95% of observations

# range 150-400
obs  <- df$observation[df$observation >= 150 & df$observation <= 400]
pred <- df$prediction[df$observation >= 150 & df$observation <= 400]

fin_rmse_150_400 <- RMSE(obs, pred)
fin_rmse_150_400

# range 400+
obs  <- df$observation[df$observation > 400]
pred <- df$prediction[df$observation > 400]

fin_rmse_400_plus <- RMSE(obs, pred)
fin_rmse_400_plus


rmses <- rbind( 
               data.frame(model = "range <150 mg/g org. C", rmse = fin_rmse_0_150),
               data.frame(model = "range 150-400 mg/g org. C", rmse = fin_rmse_150_400),
               data.frame(model = "range 400+ mg/g org. C", rmse = fin_rmse_400_plus))
rmses


#### Absolute error per landuse in the range <150 mg/g carbon ####
df %>% filter(observation < 150) %>% mutate(error = observation - prediction) %>%
  ggplot(aes(landuse, error, col = landuse)) +
  geom_hline(yintercept = 0)+ geom_jitter(alpha = 0.5) +
  theme_bw() + ylab("Absolute error in predicted organic carbon in mg C/g soil") +
  xlab("Land use") + theme(legend.position = "none") +
  annotate(geom = "text", label = "underestimating", x = 1.5, y = 75, col = "darkgreen") +
  annotate(geom = "text", label = "overestimating", x = 1.5, y = -75, col = "darkgreen") 
  
#### CONCLUSION II ####
# For the range of organic C of 0-150 mg/g C, we can predict with
# a typical error of 10.2 mg/g C, done through an ensemble of random forest
# and knn predictions. 
# We used total P, pH, elevation and landuse for this prediction.




