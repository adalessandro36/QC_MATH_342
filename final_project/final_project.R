
setwd("C:/Users/Antonio/Desktop/650W/final_project") 
housing_data<- read.csv("housing_data_2016_2017.csv", stringsAsFactors = FALSE) 

library(data.table)
library(dplyr)
library(ggplot2)
library(randomForest)
library(rpart)
library(rpart.plot)
library(readr)
library(skimr)
library(ggmap)
library(magrittr)
library(googleway)

# replace "api_key" with your API key
register_google(key = '')

#load as data.table
housing_data <- data.table(housing_data)
#throw out garbage columns related to MTurk
housing_data <- housing_data[,29:54]
#review data type of each of the remaining relevant columns
str(housing_data)
skim(housing_data)
#D is going to be rows 1:529
#model_type looks like a junk column, either the data is duplicated elsewhere or not relevant so we drop it
housing_data[, model_type:=NULL] 

# maintenance_cost -> convert from $ABC to double
# parking charges same as costs/ common charges
# sale_price need to convert to double 
# total_taxes convert to double
# listing price convert to double 
# convert common charges from $ABC to double
housing_data[,c('common_charges','maintenance_cost','parking_charges','sale_price','total_taxes','listing_price_to_nearest_1000')] <- lapply(housing_data[,c('common_charges','maintenance_cost','parking_charges','sale_price','total_taxes','listing_price_to_nearest_1000')], parse_number)


# Factorize the character predictors - that have a reasonable number of unique levels. 
cols <- c('cats_allowed','coop_condo', 'dining_room_type','dogs_allowed','fuel_type', 'garage_exists','kitchen_type')
housing_data[, (cols):= lapply(.SD, factor), .SDcols=cols]

#check levels
levels(housing_data$cats_allowed)
levels(housing_data$coop_condo)
levels(housing_data$dining_room_type)
levels(housing_data$dogs_allowed)
levels(housing_data$fuel_type)
levels(housing_data$garage_exists)
levels(housing_data$kitchen_type)

#some problems with the unique levels need to be cleaned up particularly: garage_exists, kitchen_type, cats_allowed, dogs_allowed
housing_data$dogs_allowed[which(housing_data$dogs_allowed == "yes89")] <- 'yes'
housing_data$cats_allowed[which(housing_data$cats_allowed == "y")]  <- 'yes'
housing_data$fuel_type[which(housing_data$fuel_type =='Other')] <- 'other'

housing_data$garage_exists[which(housing_data$garage_exists ==1 | housing_data$garage_exists =='eys' | housing_data$garage_exists =='UG' | housing_data$garage_exists =='Underground' | housing_data$garage_exists =='Yes')] <- 'yes' 

housing_data$garage_exists <- as.character(housing_data$garage_exists)
housing_data$garage_exists[which(is.na(housing_data$garage_exists))] <- 'NA'


housing_data$kitchen_type[which(housing_data$kitchen_type == 1955)] <- NA
housing_data$kitchen_type[which(housing_data$kitchen_type == 'Combo')] <- 'combo'
housing_data$kitchen_type[which(housing_data$kitchen_type == 'Eat in' | housing_data$kitchen_type == 'Eat In' | housing_data$kitchen_type == 'eatin' )] <- 'eat in'
housing_data$kitchen_type[which(housing_data$kitchen_type == 'efficiemcy' | housing_data$kitchen_type == 'efficiency kitchen' | housing_data$kitchen_type == 'efficiency kitchene' | housing_data$kitchen_type == 'efficiency ktchen')] <- 'efficiency'

housing_data[, (cols):= lapply(.SD, factor), .SDcols=cols]


#deal with date of sale 
housing_data$date_of_sale <- as.numeric(as.Date(housing_data$date_of_sale, "%m/%d/%Y"))
# if we need to convert back to date objects use as.Date(housing_data$date_of_sale,"1970-01-01")


# full_address_or_zip_code has a lot of useful information, extract just the zip code first 
# extract zip code from address column vector
address <- housing_data$full_address_or_zip_code
result = substr(address,(nchar(address)+1)-18,nchar(address))
zipcodes <-parse_number(result)

# looks like there was a problem with rows 2,600,1189,1322,1347 lets fix those now
address[c(2,600,1189,1322,1347)]
zipcodes[c(2,600,1189,1322,1347)] <- c(11354,11375,11375,11418,11418)

# parse_number returned some incomplete zips 
probs <- which(nchar(zipcodes) < 5)
zipcodes[probs]
address[probs]

# appears to be data missing in the source file, we could try and impute or just look up the correct zipcodes and correct manually
zipcodes[probs] <- c(11355,NA,11369,11372,11372,11375,11364,11427,11355,NA,11369,11372,11375,11364,11427)
housing_data<-cbind(housing_data,zipcodes)
rm(result,probs,address,zipcodes)

# we can use google maps API to convert the full addresses to lat/long data (numeric) and save to a seperate csv file. I will leave the code in the comments below
# and will provide the csv file to prevent having to query the API every time the code is run 

# housing_data$lon <- NA
# housing_data$lat <- NA
# 
# address_as_char <- as.character(housing_data$full_address_or_zip_code)
# 
# b <- matrix(NA,ncol=2,nrow=nrow(housing_data))
# 
# colnames(b) <- c('lat','lon')
# for (i in 1:nrow(housing_data)){
#   
#   b[i,2] <- as.numeric(geocode(address_as_char[i])[1])
#   b[i,1] <- as.numeric(geocode(address_as_char[i])[2])
# 
# }
# write.matrix(b,file="latlong.csv")
#
#Take these lines when ready to attached lat lon - wait until D_train is created and we add more features
#b <- read.csv("latlong2.csv", stringsAsFactors = FALSE) 
#housing_data$lat <- b[,1]
#housing_data$lon <- b[,2]


# High levels of missing in the data set will be dropped out right num_half_bathrooms has 93% of the rows missing 
housing_data<-housing_data[,num_half_bathrooms:=NULL]
# We will use the lat / lon and zipcode features in place of the full address
housing_data<-housing_data[,full_address_or_zip_code:=NULL]


# We want to record missing-ness and try and impute the rest of the missing values before moving on.
M <- data.table(apply(is.na(housing_data), 2, as.numeric))
colnames(M) <- paste("is_missing_", colnames(housing_data), sep = "")
pacman::p_load(missForest)
X_imp <- missForest(data.frame(housing_data), sampsize = rep(400, ncol(housing_data)))$ximp
M <- M[1:529,]

#write.csv(M, file='M_missing.csv')
#write.csv(X_imp, file='X_imputed.csv')



# #number of train stations within a 1 mile radius
# num_train <- c()
# 
# for(j in 1:529){
# 
#   df_places <- google_places(
#     location = b[j,],
#     radius = 1700,
#     place_type = 'train_station',
#     key = key)
# 
#   num_train <- append(num_train,length(df_places$results$name))
# 
# }
# 
# #number of liquor stores within a 1 mile radius
# num_liq <- c()
# 
# for(j in 1:529){
#   
#   df_places <- google_places(
#     location = b[j,],
#     radius = 1700,
#     place_type = 'liquor_store',
#     key = key)
#   
#   num_liq <- append(num_liq,length(df_places$results$name))
#   
# }
# 
# 
# #total fast food locations within 1 mile radius?
# 
# #we want to add in crime statistics gather from historic NYPD data. First we need to connect each address via lat lon to it's corresponding police precinct
# police_prec <- c()
# for(j in 1:529){
#   
#   df_places <- google_places( 
#     location = b[j,],   
#     radius = 5000,
#     keyword = 'police precinct',
#     key = key)
#   
#   police_prec <- append(police_prec,df_places$results$name[1])
#   
# }
# 
#  
# unique(police_prec)
# #some of the lat / lon returned police precincts outside of queens (midtown south and 75th). They are cases where south Queens boarders BK or North Queens with Midtown this needs to be corrected before attaching. 
# which(police_prec=="Midtown Precinct South")
# which(police_prec=="New York City Police Department - 75th Precinct")
# police_prec[which(police_prec=="Midtown Precinct South")] <- 'New York City Police Department - 108th Precinct'
# police_prec[which(police_prec=="New York City Police Department - 75th Precinct")] <- 'New York City Police Department - 106th Precinct'
# 
# 
# #police_prec is actually a character vector, we just require the number.
# matches <- regmatches(police_prec, gregexpr("[[:digit:]]+", police_prec))
# prec_num <- as.numeric(unlist(matches))


#Create D
Dat <- X_imp[1:529,]
Dat$lat <- b[,1]
Dat$lon <- b[,2]
Dat$prec_num <- prec_num
Dat$num_train <- num_train
Dat$num_liq <- num_liq
#load historical crime data
#crime_dat3 <- read.csv("crime_qns.csv", stringsAsFactors = FALSE) 
#left join the crime data 
Dat <- merge(Dat, crime_dat3, by = "prec_num", all.x = TRUE)
Dat$listing_price_to_nearest_1000 <- Dat$listing_price_to_nearest_1000 / 1000
#save to D to csv
#write.csv(Dat, file='D_final.csv')


#Create D_train and hold-out set
n <- nrow(Dat)
K <- 5 
test_indices <- sample(1:n, 1 / K * n)
train_indices <- setdiff(1:n, test_indices)

D_train <- Dat[train_indices, ]
y_train <- Dat[train_indices, "sale_price"]

D_test <- Dat[test_indices, ]
y_test <- Dat[test_indices,"sale_price"]
D_test$sale_price <- NULL

#dim(D_train)
#dim(D_test)
#length(y_train)
#length(y_test)

#fit the different models

#build tree mod
tree_mod1 <- rpart(sale_price ~ . , data=D_train, method = 'anova')
#summary(tree_mod)
#printcp(tree_mod)
#rsq.rpart(tree_mod)
#prp(tree_mod1, digits = 4, extra = 1)

#check on the hold out
yhat_oos <- predict(tree_mod1, D_test)
tree_oos_residuals <- y_test - yhat_oos
tree_rsq <- 1 - sum(tree_oos_residuals^2) / sum((y_test - mean(y_test))^2)
tree_rmse <- sd(tree_oos_residuals)
tree_med <- median(abs((y_test - yhat_oos)/y_test))


#build tree without listing
tree_mod2 <- rpart(sale_price ~ ., data=D_train[,-24], method = 'anova')
yhat_oos <- predict(tree_mod2, D_test)
tree2_oos_residuals <- y_test - yhat_oos
tree2_rsq <- 1 - sum(tree2_oos_residuals^2) / sum((y_test - mean(y_test))^2)
tree2_rmse <- sd(tree2_oos_residuals)
tree2_med <- median(abs((y_test - yhat_oos)/y_test))
#prp(tree_mod2, digits = 4, extra = 1)


#stock linear model based on CART1
linear_mod_cart1 <- lm(formula = sale_price ~ listing_price_to_nearest_1000  + sq_footage, data = D_train)
summary(linear_mod_cart1)$sigma
summary(linear_mod_cart1)$r.squared

#how well does it do on the hold out?
yhat_oos <- predict(linear_mod_cart1, D_test)
lmc1_oos_residuals <- y_test - yhat_oos
lmc1_rsq <- 1 - sum(lmc1_oos_residuals^2) / sum((y_test - mean(y_test))^2)
lmc1_rmse <-sd(lmc1_oos_residuals)
lmc1_med <- median(abs((y_test - yhat_oos)/y_test))


#linear model based on CART2
linear_mod_cart2 <- lm(formula = sale_price ~ num_full_bathrooms + coop_condo + maintenance_cost + sq_footage + 
                         common_charges + pct_tax_deductibl + total_taxes + parking_charges + lat + zipcodes, data = D_train)
summary(linear_mod_cart2)$sigma
summary(linear_mod_cart2)$r.squared

#how well does it do on the hold out?
yhat_oos <- predict(linear_mod_cart2, D_test)
lmc2_oos_residuals <- y_test - yhat_oos
lmc2_rsq <- 1 - sum(lmc2_oos_residuals^2) / sum((y_test - mean(y_test))^2)
lmc2_rmse <- sd(lmc2_oos_residuals)
lmc2_med <- median(abs((y_test - yhat_oos)/y_test))


#use forward step-wise to do feature selection for lm
# fitall <- lm(sale_price ~ ., data=D_train)
# fit_start <- lm(sale_price ~ 1 , data=D_train)
# step(fit_start, direction='forward', scope=formula(fitall))

#take the result of the forward step-wise 
linear_mod <- lm(formula = sale_price ~ listing_price_to_nearest_1000 + common_charges + 
                   date_of_sale + zipcodes + maintenance_cost + total_taxes + 
                   approx_year_built + prec_num + dogs_allowed + num_train + 
                   sq_footage + walk_score + num_full_bathrooms, data = D_train)
summary(linear_mod)$sigma
summary(linear_mod)$r.squared

#how well does it do on the hold out?
yhat_oos <- predict(linear_mod, D_test)
linear_oos_residuals <- y_test - yhat_oos
linear_rsq <- 1 - sum(linear_oos_residuals^2) / sum((y_test - mean(y_test))^2)
linear_rmse <- sd(linear_oos_residuals)
linear_med <- median(abs((y_test - yhat_oos)/y_test))


#run random forest
rf_mod <- randomForest(sale_price ~ ., data=D_train, ntree=2000, nodesize=1)
#rf_mod
#sqrt(rf_mod$mse)
#print(rf_mod)

#check on the hold out
yhat_oos <- predict(rf_mod, D_test)
rf_oos_residuals <- y_test - yhat_oos
rf_rsq <- 1 - sum(rf_oos_residuals^2) / sum((y_test - mean(y_test))^2)
rf_rmse <- sd(rf_oos_residuals)
rf_med <- median(abs((y_test - yhat_oos)/y_test))

# median percent error
tree_med
tree2_med
lmc1_med
lmc2_med
linear_med
rf_med

# OUT OF SAMPLE RMSE
tree_rmse
tree2_rmse
lmc1_rmse
lmc2_rmse
linear_rmse
rf_rmse


# did we beat zillow?
k <- abs((y_test - yhat_oos)/y_test)
length(which(k < .05)) / length(y_test)
length(which(k < .1)) / length(y_test)
length(which(k < .2)) / length(y_test)


