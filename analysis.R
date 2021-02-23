#### read in data ####

# eusoil <- read.csv("P:\\r\\projects\\ex-data\\LUCAS2015_topsoildata_20200323\\LUCAS_Topsoil_2015_20200323.csv")
# as_tibble(eusoil)
# names(eusoil)
# unique(eusoil$LC0_Desc)
# eusoil <- eusoil %>% select(Clay, Sand, Silt, pH.CaCl2., pH.H2O., EC, OC, CaCO3, P, N, K, Elevation, Soil_Stones, LC1, LC1_Desc)
# save(eusoil, file="ds-pt-9-capstone/b-data/eusoil.Rdata")

#### load & inspect data ####
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")

library(caret)
library(tidyverse)

load("b-soildata/data/eusoil.Rdata")

names(eusoil)
dim(eusoil)
str(eusoil)

# generalize landuses
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

# check what variables are left
names(eusoil)

#### variable selection I: choose variables which are available in >80% of cases ####

res <- for (i in 1:length(names(eusoil))) { # for all variables
  print(table(!is.na(eusoil[,i])))
  print(names(eusoil[i]))
}
# texture (the silt, sand and clay variables) are only available in a fraction of cases
# Note: either all texture classes are available or none
condition <- !is.na(eusoil$Clay)
table(condition)[[1]]/(table(condition)[[1]] + table(condition)[[2]])
# Texture is missing in 80.5% of cases

#### variable selection II: remove variables that are often NA, not common or not clearly defined ####
eusoil <- eusoil %>% select(-Clay, -Sand, -Silt, -EC, -pH.CaCl2., -Soil_Stones, -LC1_Desc) %>% 
  select(OC, N, P, K, CaCO3, pH.H2O., Elevation, landuse)

#### variable selection III: incluential variables ####
names(eusoil)
str(eusoil)
## a) numerical variables that correlate with OC but not with each other
relationships <- plot(eusoil[1:2000,]) # all relationships, takes time to run
relationships
# P, K with no clear relationship
# CaCO3 and pH co-dependend -> take pH as it is most common
# remaining: N, pH & elevation


## b) categorical variables that show differences in OC
# landuseis the only factor now
# overview of OC (organic carbon) in landuses)
qplot(landuse, OC, data = eusoil, col = landuse) + geom_boxplot()
range(eusoil$OC)
# remaining overall: N, pH, elevation and landuse

dim(eusoil)

eusoil <- eusoil %>% mutate(pH = pH.H2O., elev = Elevation) %>% 
  select(OC, N, P, pH, elev, landuse)

dim(eusoil)

#### create training, test and hold-out test set (validation) ####

# create validation set
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

nrow(trainset); nrow(testset); nrow(validation)

#### RMSE function ####
RMSE <- function(observed_values, predicted_values){
  sqrt(mean((observed_values - predicted_values)^2))
}

#### guessing model ####
mu <- mean(trainset$OC)
head(mu)

qplot(testset$OC, mu)
rmse_mu <- RMSE(testset$OC, mu)
rmse_mu

rmses <- data.frame(model = "mean", rmse = rmse_mu)
rmses

#### lm with N ####

m1 <- lm(OC ~ N, data = trainset) 

base <- data.frame(testset)

pred1 <- predict(m1, base)
head(pred1)

qplot(testset$OC, pred1, col = testset$landuse)


# get rmse
rmse1 <- RMSE(testset$OC, pred1)
rmse1

rmses <- rbind(rmses, 
               data.frame(model = "lm with N", rmse = rmse1))
rmses

#### lm with N, pH #### 

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



#### lm with N, pH, elevation ####

m3 <- lm(OC~N + pH + elev, data = trainset)
summary(m3)

base <- data.frame(testset)

pred3 <- predict(m3, base)
head(pred3)

qplot(testset$OC, pred3)
rmse3 <- RMSE(testset$OC, pred3)

rmses <- rbind(rmses, 
               data.frame(model = "lm with N + pH + Elev.", rmse = rmse3))
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


#### INTERLUDE: finding the outliers at the large OC values ####
plot_all_landuses <- testset %>% mutate(prediction = pred4) %>% 
  ggplot(aes(OC, prediction)) +
  geom_point(aes(col = landuse)) + 
  scale_color_brewer()
plot_all_landuses

# excluding water and wetland 
plot_test <- testset %>% mutate(prediction = pred4) %>% 
  filter(!(landuse %in% c("water", "wetland"))) %>% 
  ggplot(aes(OC, prediction)) +
  geom_point(aes(col = landuse)) + 
  scale_color_brewer()
plot_test

## appears to have little effect

#### INTERMDEDITATE RESULT: include all variables in fancier algorithms ####

#### knn [caret] all variables ####
# as a rule of thumb, sqrt of observations:
sqrt(nrow(trainset)) # = 125

ks <- seq(10,30,1)
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
best_k # this is 10
# it appears a better k is lower but I don't want to depend on too few points as this
# may lead to overtraining


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


#### knn [caret] all variables / excl. elevation ####
# as a rule of thumb, sqrt of observations:
sqrt(nrow(trainset)) # = 125

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

# short exploration why elevation messes things up:
qplot(landuse, elev, data = trainset)
qplot(elev, OC, data = trainset)
# no very clear reason immediately obvious
# try once more a linear model excluding elevation:

#### lm with N, pH, elevation, landuse ####

m5 <- lm(OC ~ N + pH + landuse, data = trainset)
summary(m5)

base <- data.frame(testset)

pred5 <- predict(m5, base)
head(pred5)

qplot(testset$OC, pred5)
rmse5 <- RMSE(testset$OC, pred5)
rmse5

rmses <- rbind(rmses, 
               data.frame(model = "lm excluding elev.", rmse = rmse5))
rmses

## we keep elevation in the linear model but took it out for knn

#### INTERMEDIATE RESULT ####
# The same parameters may not be the best for every model type



#### random forest regression with all variables ####

library(randomForest)

set.seed(1)

fit_rf <- randomForest(
  OC ~ ., data = trainset, 
  ntree = 250
)

# going from 10 to 100 trees in the only-N model improved the RMSE by ~0.4
# and another 0.2 for 250 trees, hardly then anymore for 500 trees

pred8 <- predict(fit_rf, base)
head(pred8)

rmse8 <- RMSE(testset$OC, pred8)
rmse8

rmses <- rbind(rmses, 
               data.frame(model = "rf with all variables", rmse = rmse8))
rmses

## in this case all variables improve the model





#### use ensemble to predict ####

pred_ensemble <- (pred7 + pred8)/2 # knn and random forest with N, pH and landuse

qplot(testset$OC, pred_ensemble)
rmse_ens1 <- RMSE(testset$OC, pred_ensemble)
rmse_ens1

rmses <- rbind(rmses, 
               data.frame(model = "ensemble knn rf", rmse = rmse_ens1))
rmses


#### hold-out testset #### # final validation

# incorporate the mean prediction of knn and rf since they are the best ones


fin_predict_a <- predict(fit_knn2, validation)
fin_predict_b <- predict(fit_rf, validation)

fin_predict <- (fin_predict_a + fin_predict_b) / 2

qplot(validation$OC, fin_predict, col = validation$landuse)

fin_rmse <- RMSE(validation$OC, fin_predict)
fin_rmse

#### CONCLUSIONS ####
# not every parameter fits into every model
# model specifications such as k (knn) or ntree (randomforest) are important
# ensembles may improve the model

#### APPDX ####
# how well does the model perform in a certain range, optically divide
# set into 0-200 / 200-400 / 400+ (mg per g organic carbon)

df <- data.frame(landuse = validation$landuse,
                 observation = validation$OC, prediction = fin_predict)


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


rmses <- rbind(rmses, 
               data.frame(model = "final rmse overall", rmse = fin_rmse),
               data.frame(model = "final_rmse range <150 mg/g org. C", rmse = fin_rmse_0_150),
               data.frame(model = "final_rmse range 150-400 mg/g org. C", rmse = fin_rmse_150_400),
               data.frame(model = "final_rmse range 400+ mg/g org. C", rmse = fin_rmse_400_plus))
rmses


#### Relative error in range <150 mg/g carbon ####
df %>% filter(observation < 150) %>% mutate(error = observation - prediction) %>%
  mutate(error_relative_pc = abs(observation - prediction) / observation * 100) %>% 
  summarize(relative_mean_error = sum(error_relative_pc)/n())
# this is 35%

#### Error per landuse in the range <150 mg/g carbon ####
as_tibble(df)
df %>% filter(observation < 150) %>% mutate(error = observation - prediction) %>%
  mutate(observationclass = round(observation, -1)) %>% # rounding to nearest 10 
  ggplot(aes(landuse, error, col = landuse, alpha = observationclass)) +
  geom_hline(yintercept = 0)+ geom_point() + geom_jitter(width = 0.3) +
  theme_bw()


#### Relative error per landuse in the range <150 mg/g carbon ####
as_tibble(df)
df %>% filter(observation < 150) %>% mutate(error = observation - prediction) %>%
  mutate(error_relative_pc = (observation - prediction) / observation * 100) %>% 
  mutate(observationclass = round(observation, -1)) %>% # rounding to nearest 10 
  ggplot(aes(landuse, error_relative_pc, col = landuse, alpha = observationclass)) +
  geom_hline(yintercept = 0)+ geom_point() + geom_jitter(width = 0.3) +
  theme_bw() 

df %>% filter(observation < 150) %>% mutate(error = observation - prediction) %>%
  mutate(error_relative_pc = (observation - prediction) / observation * 100) %>% 
  mutate(observationclass = round(observation, -1)) %>% # rounding to nearest 10 
  ggplot(aes(landuse, error_relative_pc, col = landuse, alpha = observationclass)) +
  geom_hline(yintercept = 0)+ geom_point() + geom_jitter(width = 0.3) +
  theme_bw() + ylim(c(-100,100))
# removed 108 points of
nrow(df) # 2187
# points

#### CONCLUSION ####
# For the range of organic C of 0-150 mg/g C, we can predict with
# a typical error of 10.2 mg/g C, done through an ensemble of random forest
# and knn predictions. This is error is 35% of the observed organic C on average.
# We used total P, pH, elevation and landuse for this prediction.




