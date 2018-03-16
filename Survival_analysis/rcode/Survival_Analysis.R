# 1. Preparation of data set
# Load relevant packages
library(data.table)
require(data.table)
library(aod)
library(ggplot2)
library(Rcpp)
library(MASS)
library(foreign)
library(car)
library(lmtest)
library(zoo)
library(gplots)
library(tseries)
library(nortest)
library(lattice)
library(Hmisc)
library(moments)
library(formatR)
library(survival)
library(survminer)
library(survAUC)
library(lubridate)
library(rms)
library(ROCR)
library(rpart)
library(OIsurv)
library("dplyr")
library(rpart)
library(rpart.plot)
library(OIsurv)
library("partykit")


# Loading data and saving in the variable
data = fread("C:/Informationsystems/Data/new_data_big.csv")


# Name columns
colnames(data)[colnames(data) == "V1"]         = "vendor_id"
colnames(data)[colnames(data) == "V2"]         = "guid"
colnames(data)[colnames(data) == "V3"]         = "first_registered"
colnames(data)[colnames(data) == "V4"]         = "first_activated"
colnames(data)[colnames(data) == "V5"]         = "customer_type"
colnames(data)[colnames(data) == "V6"]         = "availability_after_order"
colnames(data)[colnames(data) == "V7"]         = "images_number"
colnames(data)[colnames(data) == "V8"]         = "mileage"
colnames(data)[colnames(data) == "V9"]         = "price"
colnames(data)[colnames(data) == "V10"]        = "dealer_price_filled"
colnames(data)[colnames(data) == "V11"]        = "power"
colnames(data)[colnames(data) == "V12"]        = "previous_owners"
colnames(data)[colnames(data) == "V13"]        = "asking_price_filled"
colnames(data)[colnames(data) == "V14"]        = "body_color_id_filled"
colnames(data)[colnames(data) == "V15"]        = "body_color_original_filled"
colnames(data)[colnames(data) == "V16"]        = "body_type_id_filled"
colnames(data)[colnames(data) == "V17"]        = "co2_emissions"
colnames(data)[colnames(data) == "V18"]        = "cylinders_filled"
colnames(data)[colnames(data) == "V19"]        = "description_filled"
colnames(data)[colnames(data) == "V20"]        = "displacement_filled"
colnames(data)[colnames(data) == "V21"]        = "efficiency_class_id"
colnames(data)[colnames(data) == "V22"]        = "electric_consumption_mixed_filled"
colnames(data)[colnames(data) == "V23"]        = "emission_pollution_class_id"
colnames(data)[colnames(data) == "V24"]        = "enviromental_sticker_id"
colnames(data)[colnames(data) == "V25"]        = "equipment_id_amount"
colnames(data)[colnames(data) == "V26"]        = "fuel_consumption_city_filled"
colnames(data)[colnames(data) == "V27"]        = "fuel_consumption_country_filled"
colnames(data)[colnames(data) == "V28"]        = "fuel_consumption_mixed_filled"
colnames(data)[colnames(data) == "V29"]        = "fuel_id"
colnames(data)[colnames(data) == "V30"]        = "gears"
colnames(data)[colnames(data) == "V31"]        = "interior_color_id_filled"
colnames(data)[colnames(data) == "V32"]        = "last_belt_service_filled"
colnames(data)[colnames(data) == "V33"]        = "date"
colnames(data)[colnames(data) == "V34"]        = "is_featured_ad"
colnames(data)[colnames(data) == "V35"]        = "is_featured_ad_plus"
colnames(data)[colnames(data) == "V36"]        = "is_top_insertion"
colnames(data)[colnames(data) == "V37"]        = "make_id"
colnames(data)[colnames(data) == "V38"]        = "model_id"
colnames(data)[colnames(data) == "V39"]        = "next_inspection_date"
colnames(data)[colnames(data) == "V40"]        = "number_of_doors_filled"
colnames(data)[colnames(data) == "V41"]        = "paint_type_id_filled"
colnames(data)[colnames(data) == "V42"]        = "seal_ids_amount"
colnames(data)[colnames(data) == "V43"]        = "tier"
colnames(data)[colnames(data) == "V44"]        = "transmission_id"
colnames(data)[colnames(data) == "V45"]        = "upholstery_id_filled"
colnames(data)[colnames(data) == "V46"]        = "usage_state_id"
colnames(data)[colnames(data) == "V47"]        = "version_description_filled"
colnames(data)[colnames(data) == "V48"]        = "videos"
colnames(data)[colnames(data) == "V49"]        = "warranty_filled"
colnames(data)[colnames(data) == "V50"]        = "list_result_page_views"
colnames(data)[colnames(data) == "V51"]        = "sent_contact_emails"
colnames(data)[colnames(data) == "V52"]        = "app_hits"
colnames(data)[colnames(data) == "V53"]        = "detail_hits"
colnames(data)[colnames(data) == "V54"]        = "app_list_result_page_views"
colnames(data)[colnames(data) == "V55"]        = "bookmarks"
colnames(data)[colnames(data) == "V56"]        = "print_page_views"
colnames(data)[colnames(data) == "V57"]        = "mobile_calls"



# Format date data as dates
data$first_registered    <- ymd(data$first_registered)
data$first_activated      <- ymd(data$first_activated)
data$date                 <- ymd(data$date)
data$next_inspection_date      <- ymd(data$next_inspection_date)


# Restricting the data frame
data_filter <- select(data, -tier, -electric_consumption_mixed_filled)
data_filter <- (data_filter[which(data_filter$date != "2017-07-31")])


# Build a table that shows for each guid and each day if the guid was online that day
## Set up grid for all
cols <- unique(sort(data_filter$date))
guids <- unique(data_filter$guid)
big_grid <- as.data.frame(matrix(0, nrow = NROW(guids), ncol = 1 + NROW(cols)))
colnames(big_grid) <- c("guid", as.character(cols))
big_grid$guid <- guids


## Fill grid for all
for(i in 1 : nrow(data_filter)) {
  big_grid[big_grid$guid == data_filter$guid[i],
           which(colnames(big_grid) == as.character(data_filter$date[i]))] <- 1}


## Death date table
ddt <- data.frame(big_grid$guid, apply(big_grid, 1, function(x) colnames(big_grid)[min(which(x == 0))]))
colnames(ddt) <- c("guid", "earliest_ddt")
ddt$earliest_ddt <- ymd(ddt$earliest_ddt)


# Merging the data frames
data_final <- merge(x = data, y = ddt, by = "guid", all = TRUE)


# Calculate additional features part I
data_final$age_of_listing <- data_final$earliest_ddt - data_final$first_activated
data_final$time_to_next_inspection_static <- data_final$next_inspection_date - data_final$first_activated
data_final$time_to_next_inspection_dynamic <- data_final$next_inspection_date - data_final$date
data_final$age_of_car <- data_final$date - data_final$first_registered


# Preparing the final data frame
## Converting features to integers, set status, 0 = alive later and 1 = dead
data_final$age_of_car <- as.integer(data_final$age_of_car)
data_final$age_of_listing <- as.integer(data_final$age_of_listing)

data_final$asking_price_filled[which(data_final$asking_price_filled == "false")] <- 0
data_final$asking_price_filled[which(data_final$asking_price_filled == "true")] <- 1
data_final$asking_price_filled[which(is.na(data_final$asking_price_filled))] <- 0
data_final$asking_price_filled <- as.integer(data_final$asking_price_filled)

data_final$date_integer <- as.integer(data_final$date)
data_final$date_integer <- as.integer(data_final$date-17378)
##17378 für ab 01.08 und 17353 für ab 01.01.

data_final$is_featured_ad[which(data_final$is_featured_ad == "false")] <- 0
data_final$is_featured_ad[which(data_final$is_featured_ad == "true")] <- 1
data_final$is_featured_ad <- as.integer(data_final$is_featured_ad)

data_final$is_featured_ad_plus[which(data_final$is_featured_ad_plus == "false")] <- 0
data_final$is_featured_ad_plus[which(data_final$is_featured_ad_plus == "true")] <- 1
data_final$is_featured_ad_plus <- as.integer(data_final$is_featured_ad_plus)

data_final$is_top_insertion[which(data_final$is_top_insertion == "false")] <- 0
data_final$is_top_insertion[which(data_final$is_top_insertion == "true")] <- 1
data_final$is_top_insertion <- as.integer(data_final$is_top_insertion)

data_final$status[which(data_final$date - data_final$earliest_ddt < -1)] <- 1 
data_final$status[which(data_final$date - data_final$earliest_ddt == -1)] <- 2 
data_final$status[which(data_final$date - data_final$earliest_ddt > -1)] <- 3
data_final$end_date <- as.Date("2018-03-05")
data_final$status[which(data_final$date == data_final$end_date)] <- 4


## Grouping previous owners
data_final$previous_owners[which(data_final$previous_owners == 0)] <- 0 
data_final$previous_owners[which(data_final$previous_owners == 1)] <- 1 
data_final$previous_owners[which(data_final$previous_owners == 2)] <- 2 
data_final$previous_owners[which(data_final$previous_owners > 2)] <- 3 


## Setting up make_id groups
## 9 = audi, 13 = BMW, 28 = Fiat, 29 = Ford, 33 = Hyundai, 31 = Honda, 37 = Jaguar, 39 = Kia, 46 = Mazda, 47 = Mercedes
## 52 = Nissan, 55 = Peugeot, 57 = Porsche, 68 = Suzuki, 70 = Toyota, 73 = Volvo, 74 = Volkswagen
data_final$make_group[which(data_final$make_id == 9)] <- 3 
data_final$make_group[which(data_final$make_id == 13)] <- 3 
data_final$make_group[which(data_final$make_id == 28)] <- 1 
data_final$make_group[which(data_final$make_id == 29)] <- 2 
data_final$make_group[which(data_final$make_id == 33)] <- 1 
data_final$make_group[which(data_final$make_id == 31)] <- 2 
data_final$make_group[which(data_final$make_id == 37)] <- 3 
data_final$make_group[which(data_final$make_id == 39)] <- 1 
data_final$make_group[which(data_final$make_id == 46)] <- 1 
data_final$make_group[which(data_final$make_id == 47)] <- 3 
data_final$make_group[which(data_final$make_id == 52)] <- 2 
data_final$make_group[which(data_final$make_id == 55)] <- 2 
data_final$make_group[which(data_final$make_id == 57)] <- 3 
data_final$make_group[which(data_final$make_id == 68)] <- 1 
data_final$make_group[which(data_final$make_id == 70)] <- 2 
data_final$make_group[which(data_final$make_id == 73)] <- 2 
data_final$make_group[which(data_final$make_id == 74)] <- 2 
data_final$make_group[which(is.na(data_final$make_group))] <- 0 


## Controlling for Seal_ids
data_final$seal_ids_filled[which(data_final$seal_ids_amount == 0)] <- 0 
data_final$seal_ids_filled[which(data_final$seal_ids_amount == 1)] <- 1 
data_final$seal_ids_filled[which(data_final$seal_ids_amount == 2)] <- 1 


## Controlling for Images numbers
## 0 = no images, 1 = 1 to 5 images, 2 = 6 to 10 images, 3 = more than 10 images
data_final$images_number_group[which(data_final$images_number == 0)] <- 0 
data_final$images_number_group[which(data_final$images_number == 1)] <- 1 
data_final$images_number_group[which(data_final$images_number == 2)] <- 1 
data_final$images_number_group[which(data_final$images_number == 3)] <- 1 
data_final$images_number_group[which(data_final$images_number == 4)] <- 1 
data_final$images_number_group[which(data_final$images_number == 5)] <- 1 
data_final$images_number_group[which(data_final$images_number == 6)] <- 2 
data_final$images_number_group[which(data_final$images_number == 7)] <- 2 
data_final$images_number_group[which(data_final$images_number == 8)] <- 2 
data_final$images_number_group[which(data_final$images_number == 9)] <- 2 
data_final$images_number_group[which(data_final$images_number == 10)] <- 2 
data_final$images_number_group[which(data_final$images_number > 10)] <- 3 


## Controlling for Price groups
## 0 = below 5000, 1 = 5001 to 15000, 3 = 15001 to 35000, 4 = greater than 35000
data_final$price_groups[which(data_final$price < 5001)] <- 0 
data_final$price_groups[which(data_final$price > 5000 & data_final$price < 15001)] <- 1 
data_final$price_groups[which(data_final$price > 15000 & data_final$price < 35001)] <- 2  
data_final$price_groups[which(data_final$price > 35000)] <- 3


## Controlling for Mileage
## 0 = below 5000, 1 = 5001 to 45000, 3 = 45001 to 105000, 4 = greater than 105001
data_final$mileage_groups[which(data_final$mileage < 5001)] <- 0 
data_final$mileage_groups[which(data_final$mileage > 5000 & data_final$price < 45001)] <- 1 
data_final$mileage_groups[which(data_final$mileage > 45000 & data_final$price < 105001)] <- 2  
data_final$mileage_groups[which(data_final$mileage > 105000)] <- 3


## Controlling for Age groups
## 0 = below 5000, 1 = 5001 to 15000, 3 = 15001 to 35000, 4 = greater than 35000
data_final$age_groups[which(data_final$age_of_car < 365)] <- 0 
data_final$age_groups[which(data_final$age_of_car > 364 & data_final$age_of_car < 1092)] <- 1 
data_final$age_groups[which(data_final$age_of_car > 1091 & data_final$age_of_car < 3640)] <- 2  
data_final$age_groups[which(data_final$age_of_car > 3639)] <- 3

## Controlling for outliers
data_final$time_to_next_inspection_static[which(data_final$time_to_next_inspection_static < 0)] <- 0
data_final$time_to_next_inspection_static <- as.integer(data_final$time_to_next_inspection_static)
data_final$time_to_next_inspection_dynamic[which(data_final$time_to_next_inspection_dynamic < 0)] <- 0
data_final$time_to_next_inspection_dynamic <- as.integer(data_final$time_to_next_inspection_dynamic)
data_final <- data_final[which(data_final$transmission_id != 0)]


## Generate aggregates of performance KPIs
### grit with hits
grid_hits <- as.data.frame(aggregate(data_final$detail_hits+data_final$app_hits, by=list(guid=data_final$guid),FUN=sum))
colnames(grid_hits)[colnames(grid_hits) == "x"]         = "detail_and_app_hits"

### pageviews
grid_pageviews <- as.data.frame(aggregate(data_final$list_result_page_views+data_final$app_list_result_page_views, by=list(guid=data_final$guid),FUN=sum))
colnames(grid_pageviews)[colnames(grid_pageviews) == "x"]         = "detail_and_app_result_page_views"

### saved or contacted
grid_saved_or_contacted <- as.data.frame(aggregate(data_final$bookmarks+data_final$sent_contact_emails+data_final$mobile_calls+data_final$print_page_views, by=list(guid=data_final$guid),FUN=sum))
colnames(grid_saved_or_contacted)[colnames(grid_saved_or_contacted) == "x"]         = "saved_or_contacted"


## Merging the aggregates with data_final
data_final <- merge(x = data_final, y = grid_pageviews, by = "guid", all = TRUE)
data_final <- merge(x = data_final, y = grid_hits, by = "guid", all = TRUE)
data_final <- merge(x = data_final, y = grid_saved_or_contacted, by = "guid", all = TRUE)


## Reducing the data set further to eliminate left censored data
data_final <- (data_final[which(data_final$status != 3)])


## Reduce the set further by eliminating all days of listings when they were active
data_final <- (data_final[which(data_final$status != 1)])


## Changing back to boolean
data_final$status[which(data_final$status == 4)] <- 0
data_final$status[which(data_final$status == 2)] <- 1


## Analyzing and getting rid of biased deaths (never contacted or saved)
data_final_all <- data_final
nrow(data_final[which(data_final$saved_or_contacted < 1 & data_final$status == 1 & data_final$detail_and_app_hits < 5)])
nrow(data_final[which(data_final$detail_and_app_hits < 5 & data_final$status == 1)])
data_bias <- (data_final[which(data_final$saved_or_contacted < 1 & data_final$status == 1 & data_final$detail_and_app_hits < 5)])
data_final <- data_final[!data_bias]


# Calculate surviors after period of 216 days
t <- ymd("2017-08-01")
t2 <- ymd("2018-03-05")
t2 - t
nrow(data_final[which(data_final$status == 0)])


# 2. Start of Survival Analysis Part
## Create a survival element by Surv function
days_of_survival <- with (data_final, Surv(data_final$date_integer, data_final$status==1), data=data_final)
print(days_of_survival)


## K A P L A N - M E I E R
### Produce and plot Kaplan-Meier estimator by survfit function
model_fit_simple <- survfit(Surv(data_final$date_integer, data_final$status==1) ~ 1, data=data_final) 
autoplot(model_fit_simple) +
  labs(x = "\n Survival Time (days) ", y = "Survival Probabilities \n",
       title = "Survival Time of \n Automobile Listing \n") +
  theme(plot.title = element_text(hjust=0.5),
        axis.title.x = element_text(face = "bold", color = "#FF7A33", size = 12),
        axis.title.y = element_text(face = "bold", color = "#FF7A33", size = 12),
        legend.title = element_text(face = "bold", size = 10))


model_fit_age_of_car_groups <- survfit(Surv(data_final$date_integer, data_final$status==1) ~ data_final$age_groups, data=data_final) 
autoplot(model_fit_age_of_car_groups) +
  labs(x = "\n Survival Time (days) ", y = "Survival Probabilities \n",
       title = "Survival Time of \n Automobile Listing based on age of the car \n") +
  theme(plot.title = element_text(hjust=0.5),
        axis.title.x = element_text(face = "bold", color = "#FF7A33", size = 12),
        axis.title.y = element_text(face = "bold", color = "#FF7A33", size = 12),
        legend.title = element_text(face = "bold", size = 10))


model_fit_body_color_id_filled <- survfit(Surv(data_final$date_integer, data_final$status==1) ~ data_final$body_color_id_filled, data=data_final) 
autoplot(model_fit_body_color_id_filled) +
  labs(x = "\n Survival Time (days) ", y = "Survival Probabilities \n",
       title = "Survival Time of Automobile Listing with and \n without the information on color of the car \n") +
  theme(plot.title = element_text(hjust=0.5),
        axis.title.x = element_text(face = "bold", color = "#FF7A33", size = 12),
        axis.title.y = element_text(face = "bold", color = "#FF7A33", size = 12),
        legend.title = element_text(face = "bold", size = 10))

#nrow(data_final[which(data_final$status == 1 & data_final$body_color_id_filled == 0)])


model_fit_body_type_id_filled <- survfit(Surv(data_final$date_integer, data_final$status==1) ~ data_final$body_type_id_filled, data=data_final) 
autoplot(model_fit_body_type_id_filled) +
  labs(x = "\n Survival Time (days) ", y = "Survival Probabilities \n",
       title = "Survival Time of Automobile Listing with and \n without the information on body type of the car \n") +
  theme(plot.title = element_text(hjust=0.5),
        axis.title.x = element_text(face = "bold", color = "#FF7A33", size = 12),
        axis.title.y = element_text(face = "bold", color = "#FF7A33", size = 12),
        legend.title = element_text(face = "bold", size = 10))

#nrow(data_final[which(data_final$status == 1 & data_final$body_type_id_filled == 0)])


model_fit_customer_type <- survfit(Surv(data_final$date_integer, data_final$status==1) ~ data_final$customer_type, data=data_final) 
autoplot(model_fit_customer_type) +
  labs(x = "\n Survival Time (days) ", y = "Survival Probabilities \n",
       title = "Survival Time of Automobile Listing differentiated by \n private and professional customers \n") +
  theme(plot.title = element_text(hjust=0.5),
        axis.title.x = element_text(face = "bold", color = "#FF7A33", size = 12),
        axis.title.y = element_text(face = "bold", color = "#FF7A33", size = 12),
        legend.title = element_text(face = "bold", size = 10))


model_fit_fuel_type <- survfit(Surv(data_final$date_integer, data_final$status==1) ~ data_final$fuel_id, data=data_final) 
autoplot(model_fit_fuel_type) +
  labs(x = "\n Survival Time (days) ", y = "Survival Probabilities \n",
       title = "Survival Time of Automobile Listing differentiated by \n fuel type \n") +
  theme(plot.title = element_text(hjust=0.5),
        axis.title.x = element_text(face = "bold", color = "#FF7A33", size = 12),
        axis.title.y = element_text(face = "bold", color = "#FF7A33", size = 12),
        legend.title = element_text(face = "bold", size = 10))


model_fit_images_number_group <- survfit(Surv(data_final$date_integer, data_final$status==1) ~ data_final$images_number_group, data=data_final) 
autoplot(model_fit_images_number_group) +
  labs(x = "\n Survival Time (days) ", y = "Survival Probabilities \n",
       title = "Survival Time of Automobile Listing differentiated by \n amount of images uploaded \n") +
  theme(plot.title = element_text(hjust=0.5),
        axis.title.x = element_text(face = "bold", color = "#FF7A33", size = 12),
        axis.title.y = element_text(face = "bold", color = "#FF7A33", size = 12),
        legend.title = element_text(face = "bold", size = 10))


model_fit_interior_color_id_filled <- survfit(Surv(data_final$date_integer, data_final$status==1) ~ data_final$interior_color_id_filled, data=data_final) 
autoplot(model_fit_interior_color_id_filled) +
  labs(x = "\n Survival Time (days) ", y = "Survival Probabilities \n",
       title = "Survival Time of Automobile Listing with and \n without the information on interior color of the car \n") +
  theme(plot.title = element_text(hjust=0.5),
        axis.title.x = element_text(face = "bold", color = "#FF7A33", size = 12),
        axis.title.y = element_text(face = "bold", color = "#FF7A33", size = 12),
        legend.title = element_text(face = "bold", size = 10))

#nrow(data_final[which(data_final$status == 1 & data_final$interior_color_id_filled == 0)])


model_fit_last_belt_service_filled <- survfit(Surv(data_final$date_integer, data_final$status==1) ~ data_final$last_belt_service_filled, data=data_final) 
autoplot(model_fit_last_belt_service_filled) +
  labs(x = "\n Survival Time (days) ", y = "Survival Probabilities \n",
       title = "Survival Time of Automobile Listing with and \n without the information on last belt service the car \n") +
  theme(plot.title = element_text(hjust=0.5),
        axis.title.x = element_text(face = "bold", color = "#FF7A33", size = 12),
        axis.title.y = element_text(face = "bold", color = "#FF7A33", size = 12),
        legend.title = element_text(face = "bold", size = 10))


model_fit_make_group <- survfit(Surv(data_final$date_integer, data_final$status==1) ~ data_final$make_group, data=data_final) 
autoplot(model_fit_make_group) +
  labs(x = "\n Survival Time (days) ", y = "Survival Probabilities \n",
       title = "Survival Time of Automobile Listing differentiated by \n brand groups \n") +
  theme(plot.title = element_text(hjust=0.5),
        axis.title.x = element_text(face = "bold", color = "#FF7A33", size = 12),
        axis.title.y = element_text(face = "bold", color = "#FF7A33", size = 12),
        legend.title = element_text(face = "bold", size = 10))


model_fit_mileage_group <- survfit(Surv(data_final$date_integer, data_final$status==1) ~ data_final$mileage_group, data=data_final) 
autoplot(model_fit_mileage_group) +
  labs(x = "\n Survival Time (days) ", y = "Survival Probabilities \n",
       title = "Survival Time of Automobile Listing based on \n the mileage of the car \n") +
  theme(plot.title = element_text(hjust=0.5),
        axis.title.x = element_text(face = "bold", color = "#FF7A33", size = 12),
        axis.title.y = element_text(face = "bold", color = "#FF7A33", size = 12),
        legend.title = element_text(face = "bold", size = 10))


model_fit_number_of_doors_filled <- survfit(Surv(data_final$date_integer, data_final$status==1) ~ data_final$number_of_doors_filled, data=data_final) 
autoplot(model_fit_number_of_doors_filled) +
  labs(x = "\n Survival Time (days) ", y = "Survival Probabilities \n",
       title = "Survival Time of Automobile Listing with and \n without the information on number of doors of the car \n") +
  theme(plot.title = element_text(hjust=0.5),
        axis.title.x = element_text(face = "bold", color = "#FF7A33", size = 12),
        axis.title.y = element_text(face = "bold", color = "#FF7A33", size = 12),
        legend.title = element_text(face = "bold", size = 10))


model_fit_previous_owners <- survfit(Surv(data_final$date_integer, data_final$status==1) ~ data_final$previous_owners, data=data_final) 
autoplot(model_fit_previous_owners) +
  labs(x = "\n Survival Time (days) ", y = "Survival Probabilities \n",
       title = "Survival Time of Automobile Listing differentiated by \n amount of previous owners of the car \n") +
  theme(plot.title = element_text(hjust=0.5),
        axis.title.x = element_text(face = "bold", color = "#FF7A33", size = 12),
        axis.title.y = element_text(face = "bold", color = "#FF7A33", size = 12),
        legend.title = element_text(face = "bold", size = 10))


model_fit_price_groups <- survfit(Surv(data_final$date_integer, data_final$status==1) ~ data_final$price_groups, data=data_final) 
autoplot(model_fit_price_groups) +
  labs(x = "\n Survival Time (days) ", y = "Survival Probabilities \n",
       title = "Survival Time of Automobile Listing differentiated by \n price groups \n") +
  theme(plot.title = element_text(hjust=0.5),
        axis.title.x = element_text(face = "bold", color = "#FF7A33", size = 12),
        axis.title.y = element_text(face = "bold", color = "#FF7A33", size = 12),
        legend.title = element_text(face = "bold", size = 10))


model_fit_seals <- survfit(Surv(data_final$date_integer, data_final$status==1) ~ data_final$seal_ids_filled, data=data_final) 
autoplot(model_fit_seals) +
  labs(x = "\n Survival Time (days) ", y = "Survival Probabilities \n",
       title = "Survival Time of Automobile Listing based on whether \n the seller has seals that proof the quality of the offered car \n") +
  theme(plot.title = element_text(hjust=0.5),
        axis.title.x = element_text(face = "bold", color = "#FF7A33", size = 12),
        axis.title.y = element_text(face = "bold", color = "#FF7A33", size = 12),
        legend.title = element_text(face = "bold", size = 10))


model_fit_usage_type <- survfit(Surv(data_final$date_integer, data_final$status==1) ~ data_final$usage_state_id, data=data_final) 
autoplot(model_fit_usage_type) +
  labs(x = "\n Survival Time (days) ", y = "Survival Probabilities \n",
       title = "Survival Time of Automobile Listing differentiated by \n usage status (new vs. used) \n") +
  theme(plot.title = element_text(hjust=0.5),
        axis.title.x = element_text(face = "bold", color = "#FF7A33", size = 12),
        axis.title.y = element_text(face = "bold", color = "#FF7A33", size = 12),
        legend.title = element_text(face = "bold", size = 10))


model_fit_videos <- survfit(Surv(data_final$date_integer, data_final$status==1) ~ data_final$videos, data=data_final) 
autoplot(model_fit_videos) +
  labs(x = "\n Survival Time (days) ", y = "Survival Probabilities \n",
       title = "Survival Time of Automobile Listings that have videos \n  and those which only have images \n") +
  theme(plot.title = element_text(hjust=0.5),
        axis.title.x = element_text(face = "bold", color = "#FF7A33", size = 12),
        axis.title.y = element_text(face = "bold", color = "#FF7A33", size = 12),
        legend.title = element_text(face = "bold", size = 10))


model_fit_warranty <- survfit(Surv(data_final$date_integer, data_final$status==1) ~ data_final$warranty_filled, data=data_final) 
autoplot(model_fit_warranty) +
  labs(x = "\n Survival Time (days) ", y = "Survival Probabilities \n",
       title = "Survival Time of Automobile Listings based on  \n the fact that the dealer offers warranty or not \n") +
  theme(plot.title = element_text(hjust=0.5),
        axis.title.x = element_text(face = "bold", color = "#FF7A33", size = 12),
        axis.title.y = element_text(face = "bold", color = "#FF7A33", size = 12),
        legend.title = element_text(face = "bold", size = 10))


### Performing the Mantel-Haenzel test (all that are commented out are failing)
survdiff(Surv(data_final$date_integer, data_final$status==1) ~ data_final$age_groups, data=data_final)
survdiff(Surv(data_final$date_integer, data_final$status==1) ~ data_final$availability_after_order, data=data_final)
survdiff(Surv(data_final$date_integer, data_final$status==1) ~ data_final$body_color_id_filled, data=data_final)
survdiff(Surv(data_final$date_integer, data_final$status==1) ~ data_final$body_type_id_filled, data=data_final)
survdiff(Surv(data_final$date_integer, data_final$status==1) ~ data_final$customer_type, data=data_final)
survdiff(Surv(data_final$date_integer, data_final$status==1) ~ data_final$fuel_id, data=data_final)
survdiff(Surv(data_final$date_integer, data_final$status==1) ~ data_final$gears, data=data_final)
survdiff(Surv(data_final$date_integer, data_final$status==1) ~ data_final$images_number_group, data=data_final)
survdiff(Surv(data_final$date_integer, data_final$status==1) ~ data_final$interior_color_id_filled, data=data_final)
survdiff(Surv(data_final$date_integer, data_final$status==1) ~ data_final$last_belt_service_filled, data=data_final)
survdiff(Surv(data_final$date_integer, data_final$status==1) ~ data_final$make_group, data=data_final)
survdiff(Surv(data_final$date_integer, data_final$status==1) ~ data_final$mileage_groups, data=data_final)
survdiff(Surv(data_final$date_integer, data_final$status==1) ~ data_final$number_of_doors_filled, data=data_final)
survdiff(Surv(data_final$date_integer, data_final$status==1) ~ data_final$previous_owners, data=data_final)
survdiff(Surv(data_final$date_integer, data_final$status==1) ~ data_final$price_groups, data=data_final)
survdiff(Surv(data_final$date_integer, data_final$status==1) ~ data_final$seal_ids_amount, data=data_final)
survdiff(Surv(data_final$date_integer, data_final$status==1) ~ data_final$usage_state_id, data=data_final)
survdiff(Surv(data_final$date_integer, data_final$status==1) ~ data_final$videos, data=data_final)
survdiff(Surv(data_final$date_integer, data_final$status==1) ~ data_final$warranty_filled, data=data_final)
survdiff(Surv(data_final$date_integer, data_final$status==1) ~ data_final$time_to_next_inspection_static, data=data_final)


### Using Survminer package, simple curve (not used):
ggsurvplot(model_fit_usage_type, data = data_final)

ggsurv <- ggsurvplot(model_fit_fuel_type, data = data_final, title = "Survival Time of \n Diesel and Petrol Cars \n",
                     legend.title = "Fuel Type",
                     legend.labs = c("Diesel", "Petrol"),
                     xlab = "\n Survival Time (days) ",
                     ylab = "Survival Probabilities \n",
                     risk.table = TRUE,
                     risk.table.title = "Risk Table",
                     conf.int = TRUE)
ggsurv


# Sampling, first time
## Selecting 50%  from initial population
sample <- sample.int(n = NROW(data_final), size = floor(.5*NROW(data_final)), replace = F)
train <- data_final[sample,]
test  <- data_final[-sample,]
test <- test[-11697]


# Testing varies Cox PH models
cox_many <- coxph(Surv(date_integer,status == 1 ) ~ fuel_id + price_groups + previous_owners + customer_type + images_number_group + mileage_groups + power + co2_emissions + equipment_id_amount + age_groups + usage_state_id + warranty_filled + time_to_next_inspection_static, data = data_final)
print(summary(cox_many))

cox_few <- coxph(Surv(date_integer,status == 1 ) ~ fuel_id + price_groups + customer_type + images_number_group + mileage_groups + power + equipment_id_amount + age_groups + usage_state_id + time_to_next_inspection_static, data = data_final)
print(summary(cox_few))

cox_many_2 <- coxph(Surv(date_integer,status == 1 ) ~ fuel_id + price_groups + customer_type + images_number_group + equipment_id_amount + availability_after_order + dealer_price_filled + asking_price_filled + body_color_id_filled + body_type_id_filled + cylinders_filled + fuel_consumption_city_filled + fuel_consumption_country_filled + fuel_consumption_mixed_filled + interior_color_id_filled + last_belt_service_filled + number_of_doors_filled + transmission_id + videos + seal_ids_amount, data = data_final)
print(summary(cox_many_2))

cox_many_3 <- coxph(Surv(date_integer,status == 1 ) ~ fuel_id + price_groups + customer_type + images_number_group + equipment_id_amount + availability_after_order + dealer_price_filled + asking_price_filled + body_color_id_filled + body_type_id_filled + cylinders_filled + fuel_consumption_city_filled + fuel_consumption_mixed_filled + interior_color_id_filled + last_belt_service_filled + number_of_doors_filled + transmission_id + videos + seal_ids_amount, data = data_final)
print(summary(cox_many_3))

cox_many_4 <- coxph(Surv(date_integer,status == 1 ) ~ fuel_id + price_groups + customer_type + images_number_group + equipment_id_amount + power + age_groups + usage_state_id + time_to_next_inspection_static + body_color_id_filled + body_type_id_filled + cylinders_filled + fuel_consumption_city_filled + fuel_consumption_country_filled + last_belt_service_filled + number_of_doors_filled + transmission_id + videos, data = data_final)
print(summary(cox_many_4))

cox_mini <- coxph(Surv(date_integer,status == 1 ) ~ fuel_id + price_groups + customer_type + images_number_group + equipment_id_amount, data = data_final)
print(summary(cox_mini))

cox_performance <- coxph(Surv(date_integer,status == 1 ) ~ detail_and_app_hits + detail_and_app_result_page_views + saved_or_contacted, data = data_final)
print(summary(cox_performance))

cox_mixed_mini <- coxph(Surv(date_integer,status == 1 ) ~ detail_and_app_result_page_views + make_group, data = data_final)
print(summary(cox_mixed_mini))

cox_mixed_few <- coxph(Surv(date_integer,status == 1 ) ~ fuel_id + price_groups + customer_type + images_number_group + equipment_id_amount + detail_and_app_result_page_views, data = data_final)
print(summary(cox_mixed_few))

cox_mixed_many <- coxph(Surv(date_integer,status == 1 ) ~ (mileage_group) + body_color_id_filled + body_type_id_filled + seal_ids_amount + number_of_doors_filled + fuel_id + price_groups + customer_type + images_number_group + equipment_id_amount + detail_and_app_result_page_views, data = data_final)
print(summary(cox_mixed_many))

cox_mixed_many_logs <- coxph(Surv(date_integer,status == 1 ) ~ (mileage_groups) + body_color_id_filled + body_type_id_filled + seal_ids_amount + number_of_doors_filled + fuel_id + log(price) + customer_type + images_number_group + equipment_id_amount + (detail_and_app_result_page_views), data = data_final)
print(summary(cox_mixed_many_logs))

cox_mixed_many_logs_2 <- coxph(Surv(date_integer,status == 1 ) ~ time_to_next_inspection_static + usage_state_id + seal_ids_amount + last_belt_service_filled + gears + age_of_car + warranty_filled + availability_after_order + (mileage_groups) + body_color_id_filled + body_type_id_filled + seal_ids_amount + number_of_doors_filled + fuel_id + log(price) + customer_type + images_number_group + equipment_id_amount + (detail_and_app_result_page_views), data = data_final)
print(summary(cox_mixed_many_logs_2))

cox_mixed_many_logs_3 <- coxph(Surv(date_integer,status == 1 ) ~ saved_or_contacted + last_belt_service_filled + (mileage_groups) + body_color_id_filled + body_type_id_filled + seal_ids_amount + fuel_id + log(price) + customer_type + images_number_group + (detail_and_app_result_page_views), data = data_final)
print(summary(cox_mixed_many_logs_3))

cox_mixed_many_logs_4 <- coxph(Surv(date_integer,status == 1 ) ~ saved_or_contacted + detail_and_app_hits + last_belt_service_filled + (mileage_groups) + body_color_id_filled + body_type_id_filled + seal_ids_amount + fuel_id + (price) + customer_type + images_number_group + (detail_and_app_result_page_views), data = data_final)
print(summary(cox_mixed_many_logs_4))

cox_mixed_many_logs_5 <- coxph(Surv(date_integer,status == 1 ) ~ time_to_next_inspection_static + usage_state_id + seal_ids_amount + gears + age_of_car + (mileage_groups) + body_type_id_filled + number_of_doors_filled + fuel_id + log(price) + customer_type + images_number_group + equipment_id_amount + (detail_and_app_result_page_views) + saved_or_contacted + detail_and_app_hits, data = data_final)
print(summary(cox_mixed_many_logs_5))

cox_mixed_many_logs_6 <- coxph(Surv(date_integer,status == 1 ) ~ time_to_next_inspection_static + usage_state_id + gears + age_of_car + (mileage_groups) + body_type_id_filled + number_of_doors_filled + fuel_id + log(price) + images_number_group + equipment_id_amount + (detail_and_app_result_page_views) + saved_or_contacted + detail_and_app_hits, data = data_final)
print(summary(cox_mixed_many_logs_6))

cox_initial <- coxph(Surv(date_integer,status == 1 ) ~ time_to_next_inspection_static + (mileage_groups) + fuel_id + log(price) + images_number_group + equipment_id_amount + (detail_and_app_result_page_views) + saved_or_contacted + detail_and_app_hits + make_group, data = data_final)
print(summary(cox_initial))

data_final.cox <-  coxph(Surv(date_integer,status == 1 ) ~ saved_or_contacted + (mileage_groups) + body_color_id_filled + body_type_id_filled + fuel_id + log(price) + images_number_group + (detail_and_app_result_page_views) + make_group, data = train)
print(summary(data_final.cox))


# Testing Accelarated Time To Failure Models
#atf_model_weibull <- survreg(Surv(date_integer,status == 1 ) ~ saved_or_contacted + (mileage_groups) + body_color_id_filled + body_type_id_filled + fuel_id + log(price) + images_number_group + (detail_and_app_result_page_views) + make_group -1, data = train, dist='weibull')
#summary(atf_model_weibull)

#atf_model_exponential <- survreg(Surv(date_integer,status == 1 ) ~ saved_or_contacted + (mileage_groups) + body_color_id_filled + fuel_id + log(price) + images_number_group + (detail_and_app_result_page_views) + make_group -1, data = train, dist='exponential')
#summary(atf_model_exponential)

#atf_model_log <- survreg(Surv(date_integer,status == 1 ) ~ saved_or_contacted + (mileage_groups) + body_color_id_filled + body_type_id_filled + fuel_id + log(price) + images_number_group + (detail_and_app_result_page_views) + make_group -1, data = train, dist='loglogistic')
#summary(atf_model_log)

atf_model_weibull <- survreg(Surv(date_integer,status == 1 ) ~ (detail_and_app_result_page_views), data = train, dist='weibull')
summary(atf_model_weibull)

atf_model_exponential <- survreg(Surv(date_integer,status == 1 ) ~ (detail_and_app_result_page_views), data = train, dist='exponential')
summary(atf_model_exponential)

atf_model_log <- survreg(Surv(date_integer,status == 1 ) ~ (detail_and_app_result_page_views), data = train, dist='loglogistic')
summary(atf_model_log)


## Testing to modify for NAs and making sets equally strong
train <- data_final[sample,]
#train_2 <- train[1:10621] 
train_2 <- train[1:14023] 


## Receive AUC and ROC plot for all ATF models
par(mfrow = c(2, 2))
pred_atf <- prediction(predict(atf_model_weibull), train_2$status==0)
perf_atf <- performance(pred_atf,"tpr","fpr")
plot(perf_atf)
abline(a=0, b= 1)
auc.perf_atf_weibull = performance(pred_atf, measure = "auc")
auc.perf_atf_weibull@y.values

pred_exponential <- prediction(predict(atf_model_exponential), train_2$status==0)
perf_exponential <- performance(pred_exponential,"tpr","fpr")
plot(perf_exponential)
abline(a=0, b= 1)
auc.perf_atf_exponential = performance(pred_exponential, measure = "auc")
auc.perf_atf_exponential@y.values

pred_log <- prediction(predict(atf_model_log), train_2$status==0)
perf_log <- performance(pred_log,"tpr","fpr")
plot(perf_log)
abline(a=0, b= 1)
auc.perf_atf_log = performance(pred_log, measure = "auc")
auc.perf_atf_log@y.values


# Loading additional packages
if(!require("KMsurv")) install.packages("KMsurv"); library("KMsurv")
if(!require("survAUC")) install.packages("survAUC"); library("survAUC")
if(!require("survival")) install.packages("survival"); library("survival")


# Clean Data Set to not include nulls
data_final_clean <- data_final[!is.na(data_final$saved_or_contacted)
                 & !is.na(data_final$last_belt_service_filled)
                 & !is.na(data_final$mileage_groups)
                 & !is.na(data_final$body_color_id_filled)
                 & !is.na(data_final$fuel_id)
                 & !is.na(data_final$price)
                 & !is.na(data_final$customer_type)
                 & !is.na(data_final$detail_and_app_result_page_views)
                 & !is.na(data_final$images_number_group)
                 & !is.na(data_final$make_group)                 
                 ,]


# Sampling second time
## Selecting 75%  from initial population
sample_clean <- sample.int(n = NROW(data_final_clean), size = floor(.75*NROW(data_final_clean)), replace = F)
train_clean <- data_final_clean[sample_clean,]
test_clean  <- data_final_clean[-sample_clean,]


# Constructing a final cox ph model
data_final_clean.cox <-  coxph(Surv(date_integer,status == 1 ) ~ saved_or_contacted + (mileage_groups) + body_color_id_filled  + fuel_id + log(price) + images_number_group + (detail_and_app_result_page_views) + make_group, data = train_clean)
summary(data_final_clean.cox)


# Building the GBM Model
gbm_cox = gbm(data_final_clean.cox,
             data = train,
             distribution = "coxph",
             n.trees = 2500,
             shrinkage = 0.02,
             n.minobsinnode = 4)

print(summary(gbm_cox))


# 3. Benchmark and Prediction
## How to analyze and access a single subject to predict the odds of its individual survival
curves <- survfit(data_final_clean.cox, data_final_clean)
curves[10]
curves[100]
curves[200]
par(mfrow = c(2, 2))
plot(curves[10], xlab = "Days", ylab="Survival Probability")
plot(curves[100], xlab = "Days", ylab="Survival Probability")
plot(curves[200], xlab = "Days", ylab="Survival Probability")


## ROC - Receiver operating characteristic
## Generate predictions of cox_model and performance variable and plot the ROC
pred <- prediction(predict(data_final_clean.cox), train_clean$status)
perf <- performance(pred,"tpr","fpr")
plot(perf)
abline(a=0, b= 1)

#Sensitivity: Probability that a person with the disease will have a positive test result.
#Specificity: Probability that a person without the disease will have a negative test result.

#At every cutoff, the TPR and FPR are calculated and plotted. The smoother the graph, the more cutoffs the predictions have.
#We also plotted a 45-degree line, which represents, on average, the performance of a Uniform(0, 1) random variable. 
#The further away (towards (0,1) from the diagonal line, the better. Overall, we see that we see gains in sensitivity 
#(true positive rate, (> 80%)), trading off a false positive rate (1- specificity), up until about 25% FPR. 
#After an FPR of 25%, we don't see significant gains in TPR for a tradeoff of increased FPR.


## AUC - Area under the curve
auc.perf = performance(pred, measure = "auc")
auc.perf@y.values


# 4 Generate and Benchmark the models
## Training new data sets
### Selecting 33%, 66% and 100%  from initial population
sample <- sample.int(n = NROW(data_final_clean), size = floor(.33*NROW(data_final_clean)), replace = F)
train_1_3 <- data_final_clean[sample,]
test_2_3  <- data_final_clean[-sample,]
sample <- sample.int(n = NROW(data_final_clean), size = floor(.66*NROW(data_final_clean)), replace = F)
train_2_3 <- data_final_clean[sample,]
test_1_3  <- data_final_clean[-sample,]


## Building the trained proportional hazard cox models
data_final_clean.cox_1_3 <- coxph(Surv(date_integer,status == 1 ) ~ saved_or_contacted + last_belt_service_filled + (mileage_groups) + body_color_id_filled + body_type_id_filled + seal_ids_amount + fuel_id + log(price) + customer_type + images_number_group + (detail_and_app_result_page_views), data = train_1_3)
data_final_clean.cox_2_3 <- coxph(Surv(date_integer,status == 1 ) ~ saved_or_contacted + last_belt_service_filled + (mileage_groups) + body_color_id_filled + body_type_id_filled + seal_ids_amount + fuel_id + log(price) + customer_type + images_number_group + (detail_and_app_result_page_views), data = train_2_3)
data_final_clean.cox_3_3 <- coxph(Surv(date_integer,status == 1 ) ~ saved_or_contacted + last_belt_service_filled + (mileage_groups) + body_color_id_filled + body_type_id_filled + seal_ids_amount + fuel_id + log(price) + customer_type + images_number_group + (detail_and_app_result_page_views), data = data_final_clean)


## Building a gradient boosting model from the above mentioned cox model
gbm_1_3 = gbm(data_final_clean.cox_1_3,
                 data = train_1_3,
                 distribution = "coxph",
                 n.trees = 2500,
                 shrinkage = 0.02,
                 n.minobsinnode = 4)

gbm_2_3 = gbm(data_final_clean.cox_2_3,
                 data = train_2_3,
                 distribution = "coxph",
                 n.trees = 2500,
                 shrinkage = 0.02,
                 n.minobsinnode = 4)

gbm_3_3 = gbm(data_final_clean.cox_3_3,
                 data = data_final_clean,
                 distribution = "coxph",
                 n.trees = 2500,
                 shrinkage = 0.02,
                 n.minobsinnode = 4)


## Modelling the ATF models
atf_model_weibull_1_3 <- survreg(Surv(date_integer,status == 1 ) ~ saved_or_contacted + (mileage_groups) + fuel_id + (price) + customer_type + (detail_and_app_result_page_views), dist='weibull', data = train_1_3)
atf_model_weibull_2_3 <- survreg(Surv(date_integer,status == 1 ) ~ saved_or_contacted + (mileage_groups) + fuel_id + (price) + customer_type + (detail_and_app_result_page_views), dist='weibull', data = train_2_3)
atf_model_weibull_3_3 <- survreg(Surv(date_integer,status == 1 ) ~ saved_or_contacted + (mileage_groups) + fuel_id + (price) + customer_type + (detail_and_app_result_page_views), dist='weibull', data = data_final_clean)


atf_model_exponential_1_3 <- survreg(Surv(date_integer,status == 1 ) ~ saved_or_contacted + (mileage_groups) + fuel_id + (price) + customer_type + (detail_and_app_result_page_views), dist='exponential', data = train_1_3)
atf_model_exponential_2_3 <- survreg(Surv(date_integer,status == 1 ) ~ saved_or_contacted + (mileage_groups) + fuel_id + (price) + customer_type + (detail_and_app_result_page_views), dist='exponential', data = train_2_3)
atf_model_exponential_3_3 <- survreg(Surv(date_integer,status == 1 ) ~ saved_or_contacted + (mileage_groups) + fuel_id + (price) + customer_type + (detail_and_app_result_page_views), dist='exponential', data = data_final_clean)

atf_model_log_1_3 <- survreg(Surv(date_integer,status == 1 ) ~ saved_or_contacted + last_belt_service_filled + (mileage_groups) + body_color_id_filled + body_type_id_filled + seal_ids_amount + fuel_id + (price) + customer_type + images_number_group + (detail_and_app_result_page_views), dist='loglogistic', data = train_1_3)
atf_model_log_2_3 <- survreg(Surv(date_integer,status == 1 ) ~ saved_or_contacted + (mileage_groups) + fuel_id + (price) + customer_type + images_number_group + (detail_and_app_result_page_views), dist='loglogistic', data = train_2_3)
atf_model_log_3_3 <- survreg(Surv(date_integer,status == 1 ) ~ saved_or_contacted + (mileage_groups) + fuel_id + (price) + customer_type + images_number_group + (detail_and_app_result_page_views), dist='loglogistic', data = data_final_clean)


## Modelling the tree models
treefit_1_3 <- rpart(Surv(date_integer,status == 1 ) ~ saved_or_contacted + last_belt_service_filled + (mileage_groups) + body_color_id_filled 
                 + body_type_id_filled + seal_ids_amount + fuel_id + (price_groups) + images_number_group 
                 + (detail_and_app_result_page_views) + time_to_next_inspection_static, data = train_1_3)

treefit_2_3 <- rpart(Surv(date_integer,status == 1 ) ~ saved_or_contacted + last_belt_service_filled + (mileage_groups) + body_color_id_filled 
                     + body_type_id_filled + seal_ids_amount + fuel_id + (price_groups) + images_number_group 
                     + (detail_and_app_result_page_views) + time_to_next_inspection_static, data = train_2_3)

treefit_3_3 <- rpart(Surv(date_integer,status == 1 ) ~ saved_or_contacted + last_belt_service_filled + (mileage_groups) + body_color_id_filled 
                     + body_type_id_filled + seal_ids_amount + fuel_id + (price_groups) + images_number_group 
                     + (detail_and_app_result_page_views) + time_to_next_inspection_static, data = data_final_clean)


### plot the model
#### plot model 1 - not very readable
plot(treefit_1_3)
text(treefit_1_3)


#### plot model 2 -  change type from 0 to 3 to get different structure
rpart.plot(treefit_1_3,type=2,fallen.leaves=T)


#### plot model 3 - partykit - survival graphs
treefit_1_3_plot <- as.party(treefit_1_3)
treefit_2_3_plot <- as.party(treefit_2_3)
treefit_3_3_plot <- as.party(treefit_3_3)

plot(treefit_1_3_plot)
plot(treefit_2_3_plot)
plot(treefit_3_3_plot)


#### Partykit allows to check out median survival at each node
predict(treefit_1_3_plot, type = "response")[1]
predict(treefit_1_3_plot, type = "prob")[[1]]


### Generating ROC
par(mfrow = c(2, 2))
pred <- prediction(predict(treefit_1_3), train_1_3$status==1)
perf <- performance(pred,"tpr","fpr")
plot(perf)
abline(a=0, b= 1)

pred <- prediction(predict(treefit_2_3), train_2_3$status==1)
perf <- performance(pred,"tpr","fpr")
plot(perf)
abline(a=0, b= 1)

pred <- prediction(predict(treefit_3_3), data_final_clean$status==1)
perf <- performance(pred,"tpr","fpr")
plot(perf)
abline(a=0, b= 1)


#prediction test for tree model
prediction_test <-predict(treefit_1_3_plot,test_1_3)
table(prediction_test, predicted = prediction_test)
table(test_1_3$status, prediction_test)


# Receive AUCs for all models
## Generate predictions on all models
pred_coxph_1_3 <- prediction(predict(data_final_clean.cox_1_3), train_1_3$status)
pred_coxph_2_3 <- prediction(predict(data_final_clean.cox_2_3), train_2_3$status)
pred_coxph_3_3 <- prediction(predict(data_final_clean.cox_3_3), data_final_clean$status)

pred_gbm_1_3 = prediction(predict(object = gbm_1_3,
                                     newdata = train_1_3,
                                     n.trees = 1500,
                                     type = "response"), train_1_3$status)
pred_gbm_2_3 = prediction(predict(object = gbm_2_3,
                                     newdata = train_2_3,
                                     n.trees = 1500,
                                     type = "response"), train_2_3$status)
pred_gbm_3_3 = prediction(predict(object = gbm_3_3,
                                     newdata = data_final_clean,
                                     n.trees = 1500,
                                     type = "response"), data_final_clean$status)

pred_atf_model_weibull_1_3 <- prediction(predict(atf_model_weibull_1_3), train_1_3$status==0)
pred_atf_model_weibull_2_3 <- prediction(predict(atf_model_weibull_2_3), train_2_3$status==0)
pred_atf_model_weibull_3_3 <- prediction(predict(atf_model_weibull_3_3), data_final_clean$status==0)

pred_atf_model_exponential_1_3 <- prediction(predict(atf_model_exponential_1_3), train_1_3$status==0)
pred_atf_model_exponential_2_3 <- prediction(predict(atf_model_exponential_2_3), train_2_3$status==0)
pred_atf_model_exponential_3_3 <- prediction(predict(atf_model_exponential_3_3), data_final_clean$status==0)

pred_atf_model_log_1_3 <- prediction(predict(atf_model_log_1_3), train_1_3$status==0)
pred_atf_model_log_2_3 <- prediction(predict(atf_model_log_2_3), train_2_3$status==0)
pred_atf_model_log_3_3 <- prediction(predict(atf_model_log_3_3), data_final_clean$status==0)

pred_tree_model_1_3 <- prediction(predict(treefit_1_3), train_1_3$status)
pred_tree_model_2_3 <- prediction(predict(treefit_2_3), train_2_3$status)
pred_tree_model_3_3 <- prediction(predict(treefit_3_3), data_final_clean$status)


## Receive the AUC values for each model
auc.perf_coxph_1_3 = performance(pred_coxph_1_3, measure = "auc")
auc.perf_coxph_2_3 = performance(pred_coxph_2_3, measure = "auc")
auc.perf_coxph_3_3 = performance(pred_coxph_3_3, measure = "auc")

auc.perf_gbmpbc_1_3 = performance(pred_gbmpbc_1_3, measure = "auc")
auc.perf_gbmpbc_2_3 = performance(pred_gbmpbc_2_3, measure = "auc")
auc.perf_gbmpbc_3_3 = performance(pred_gbmpbc_3_3, measure = "auc")

auc.perf_atf_model_weibull_1_3 = performance(pred_atf_model_weibull_1_3, measure = "auc")
auc.perf_atf_model_weibull_2_3 = performance(pred_atf_model_weibull_2_3, measure = "auc")
auc.perf_atf_model_weibull_3_3 = performance(pred_atf_model_weibull_3_3, measure = "auc")

auc.perf_atf_model_exponential_1_3 = performance(pred_atf_model_exponential_1_3, measure = "auc")
auc.perf_atf_model_exponential_2_3 = performance(pred_atf_model_exponential_2_3, measure = "auc")
auc.perf_atf_model_exponential_3_3 = performance(pred_atf_model_exponential_3_3, measure = "auc")

auc.perf_atf_model_log_1_3 = performance(pred_atf_model_log_1_3, measure = "auc")
auc.perf_atf_model_log_2_3 = performance(pred_atf_model_log_2_3, measure = "auc")
auc.perf_atf_model_log_3_3 = performance(pred_atf_model_log_3_3, measure = "auc")

auc.perf_tree_1_3 = performance(pred_tree_model_1_3, measure = "auc")
auc.perf_tree_2_3 = performance(pred_tree_model_2_3, measure = "auc")
auc.perf_tree_3_3 = performance(pred_tree_model_3_3, measure = "auc")


## Print the values (example)
auc.perf_atf_model_weibull_3_3@y.values
auc.perf_atf_model_weibull_2_3@y.values
auc.perf_atf_model_weibull_1_3@y.values


## Calculate the Concordance Indices
### First always Predicting hazard ratios on testset
### Data for CI of cox is already included in the summary() of cox models.
### tree_1_3
treetrain = predict(object = treefit_1_3_plot,
                   newdata = train_1_3,
                   n.trees = 1500,
                   type = "response")


treetest = predict(object = treefit_1_3_plot,
                  newdata = test_2_3,
                  n.trees = 1500,
                  type = "response")

Survresptrain <- Surv(train_1_3$date_integer,train_1_3$status==1)
Survresptest <- Surv(test_2_3$date_integer,test_2_3$status == 1)
CI_tree_1_3 <- BeggC(Survresptrain, Survresptest, treetrain, treetest)
if(CI_tree_1_3<=0.5){
  CI_tree_1_3 =1-CI_tree_1_3
}
CI_tree_1_3


### tree_2_3
treetrain = predict(object = treefit_2_3_plot,
                    newdata = train_2_3,
                    n.trees = 1500,
                    type = "response")


treetest = predict(object = treefit_2_3_plot,
                   newdata = test_1_3,
                   n.trees = 1500,
                   type = "response")

Survresptrain <- Surv(train_2_3$date_integer,train_2_3$status==1)
Survresptest <- Surv(test_1_3$date_integer,test_1_3$status == 1)
CI_tree_2_3 <- BeggC(Survresptrain, Survresptest, treetrain, treetest)
if(CI_tree_2_3<=0.5){
  CI_tree_2_3 =1-CI_tree_2_3
}
CI_tree_2_3


### tree_3_3
treetrain = predict(object = treefit_3_3_plot,
                    newdata = data_final_clean,
                    n.trees = 1500,
                    type = "response")


treetest = predict(object = treefit_3_3_plot,
                   newdata = test_1_3,
                   n.trees = 1500,
                   type = "response")

Survresptrain <- Surv(data_final_clean$date_integer,data_final_clean$status==1)
Survresptest <- Surv(test_1_3$date_integer,test_1_3$status == 1)
CI_tree_3_3 <- BeggC(Survresptrain, Survresptest, treetrain, treetest)
if(CI_tree_3_3<=0.5){
  CI_tree_2_3 =1-CI_tree_3_3
}
CI_tree_3_3


### gbmpbc_1_3
gbmtrain = predict(object = gbm_1_3,
                      newdata = train_1_3,
                      n.trees = 1500,
                      type = "response")


gbmtest = predict(object = gbm_1_3,
                     newdata = test_2_3,
                     n.trees = 1500,
                     type = "response")


Survresptrain <- Surv(train_1_3$date_integer,train_1_3$status==1)
Survresptest <- Surv(test_2_3$date_integer,test_2_3$status == 1)
CI_gbm_1_3 <- BeggC(Survresptrain, Survresptest, gbmtrain, gbmtest)
if(CI_gbm_1_3<=0.5){
  CI_gbm_1_3 =1-CI_gbm_1_3
}
CI_gbm_1_3


### gbmpbc_2_3
gbmtrain = predict(object = gbm_2_3,
                      newdata = train_2_3,
                      n.trees = 1500,
                      type = "response")


gbmtest = predict(object = gbm_2_3,
                     newdata = test_1_3,
                     n.trees = 1500,
                     type = "response")


Survresptrain <- Surv(train_2_3$date_integer,train_2_3$status==1)
Survresptest <- Surv(test_1_3$date_integer,test_1_3$status == 1)
CI_gbm_2_3 <- BeggC(Survresptrain, Survresptest, gbmtrain, gbmtest)
if(CI_gbm_2_3<=0.5){
  CI_gbm_2_3 =1-CI_gbm_2_3
}
CI_gbm_2_3


### gbmpbc_3_3
gbmtrain = predict(object = gbm_3_3,
                   newdata = data_final_clean,
                   n.trees = 1500,
                   type = "response")


gbmtest = predict(object = gbm_3_3,
                  newdata = test_1_3,
                  n.trees = 1500,
                  type = "response")


Survresptrain <- Surv(data_final_clean$date_integer,data_final_clean$status==1)
Survresptest <- Surv(test_1_3$date_integer,test_1_3$status == 1)
CI_gbm_3_3 <- BeggC(Survresptrain, Survresptest, gbmtrain, gbmtest)
if(CI_gbm_3_3<=0.5){
  CI_gbm_3_3 =1-CI_gbm_3_3
}
CI_gbm_3_3


### atf_model_weibull_1_3
train = predict(object = atf_model_weibull_1_3,
                   newdata = train_1_3,
                   n.trees = 1500,
                   type = "response")


test = predict(object = atf_model_weibull_1_3,
                  newdata = test_2_3,
                  n.trees = 1500,
                  type = "response")


Survresptrain <- Surv(train_1_3$date_integer,train_1_3$status==1)
Survresptest <- Surv(test_2_3$date_integer,test_2_3$status == 1)
CI_atf_model_weibull_1_3 <- BeggC(Survresptrain, Survresptest, train, test)
if(CI_atf_model_weibull_1_3<=0.5){
  CI_atf_model_weibull_1_3 =1-CI_atf_model_weibull_1_3
}
CI_atf_model_weibull_1_3


### atf_model_weibull_2_3
train_atf = predict(object = atf_model_weibull_2_3,
                   newdata = train_2_3,
                   n.trees = 1500,
                   type = "response")


test_atf = predict(object = atf_model_weibull_2_3,
                  newdata = test_1_3,
                  n.trees = 1500,
                  type = "response")


Survresptrain <- Surv(train_2_3$date_integer,train_2_3$status==1)
Survresptest <- Surv(test_1_3$date_integer,test_1_3$status == 1)
CI_atf_model_weibull_2_3 <- BeggC(Survresptrain, Survresptest, train_atf, test_atf)
if(CI_atf_model_weibull_2_3<=0.5){
  CI_atf_model_weibull_2_3 =1-CI_atf_model_weibull_2_3
}
CI_atf_model_weibull_2_3


### atf_model_weibull_3_3
train = predict(object = atf_model_weibull_3_3,
                   newdata = data_final_clean,
                   n.trees = 1500,
                   type = "response")


test = predict(object = atf_model_weibull_3_3,
                  newdata = test_1_3,
                  n.trees = 1500,
                  type = "response")


Survresptrain <- Surv(data_final_clean$date_integer,data_final_clean$status==1)
Survresptest <- Surv(test_1_3$date_integer,test_1_3$status == 1)
CI_atf_model_weibull_3_3 <- BeggC(Survresptrain, Survresptest, train, test)
if(CI_atf_model_weibull_3_3<=0.5){
  CI_atf_model_weibull_3_3 =1-CI_atf_model_weibull_3_3
}
CI_atf_model_weibull_3_3


### atf_model_exponential_1_3
train_atf = predict(object = atf_model_exponential_1_3,
                   newdata = train_1_3,
                   n.trees = 1500,
                   type = "response")


test_atf = predict(object = atf_model_exponential_1_3,
                  newdata = test_2_3,
                  n.trees = 1500,
                  type = "response")


Survresptrain <- Surv(train_1_3$date_integer,train_1_3$status==1)
Survresptest <- Surv(test_2_3$date_integer,test_2_3$status == 1)
CI_atf_model_exponential_1_3 <- BeggC(Survresptrain, Survresptest, train_atf, test_atf)
if(CI_atf_model_exponential_1_3<=0.5){
  CI_atf_model_exponential_1_3 =1-CI_atf_model_exponential_1_3
}
CI_atf_model_exponential_1_3


### atf_model_exponential_2_3
train_atf = predict(object = atf_model_exponential_2_3,
                   newdata = train_2_3,
                   n.trees = 1500,
                   type = "response")


test_atf = predict(object = atf_model_exponential_2_3,
                  newdata = test_1_3,
                  n.trees = 1500,
                  type = "response")

#train_atf <- trainatf[which(trainatf!=Inf)]
#test_atf <- trainatf[which(testatf!=Inf)]

Survresptrain <- Surv(train_2_3$date_integer,train_2_3$status==1)
Survresptest <- Surv(test_1_3$date_integer,test_1_3$status == 1)
CI_atf_model_exponential_2_3 <- BeggC(Survresptrain, Survresptest, train_atf, test_atf)
if(CI_atf_model_exponential_2_3<=0.5){
  CI_atf_model_exponential_2_3 =1-CI_atf_model_exponential_2_3
}
CI_atf_model_exponential_2_3


### atf_model_exponential_3_3
train_atf = predict(object = atf_model_exponential_3_3,
                   newdata = data_final_clean,
                   n.trees = 1500,
                   type = "response")


test_atf = predict(object = atf_model_exponential_3_3,
                  newdata = test_1_3,
                  n.trees = 1500,
                  type = "response")


Survresptrain <- Surv(data_final_clean$date_integer,data_final_clean$status==1)
Survresptest <- Surv(test_1_3$date_integer,test_1_3$status == 1)
CI_atf_model_exponential_3_3 <- BeggC(Survresptrain, Survresptest, train_atf, test_atf)
if(CI_atf_model_exponential_3_3<=0.5){
  CI_atf_model_exponential_3_3 =1-CI_atf_model_exponential_3_3
}
CI_atf_model_exponential_3_3


### atf_model_log_1_3
train_atf = predict(object = atf_model_log_1_3,
                   newdata = train_1_3,
                   n.trees = 1500,
                   type = "response")


test_atf = predict(object = atf_model_log_1_3,
                  newdata = test_2_3,
                  n.trees = 1500,
                  type = "response")


Survresptrain <- Surv(train_1_3$date_integer,train_1_3$status==1)
Survresptest <- Surv(test_2_3$date_integer,test_2_3$status == 1)
CI_atf_model_log_1_3 <- BeggC(Survresptrain, Survresptest, train_atf, test_atf)
if(CI_atf_model_log_1_3<=0.5){
  CI_atf_model_log_1_3 =1-CI_atf_model_log_1_3
}
CI_atf_model_log_1_3


### atf_model_log_2_3
train_atf = predict(object = atf_model_log_2_3,
                   newdata = train_2_3,
                   n.trees = 1500,
                   type = "response")


test_atf = predict(object = atf_model_log_2_3,
                  newdata = test_1_3,
                  n.trees = 1500,
                  type = "response")


Survresptrain <- Surv(train_2_3$date_integer,train_2_3$status==1)
Survresptest <- Surv(test_1_3$date_integer,test_1_3$status == 1)
CI_atf_model_log_2_3 <- BeggC(Survresptrain, Survresptest, train_atf, test_atf)
if(CI_atf_model_log_2_3<=0.5){
  CI_atf_model_log_2_3 =1-CI_atf_model_log_2_3
}
CI_atf_model_log_2_3


### atf_model_log_3_3
train_atf = predict(object = atf_model_log_3_3,
                   newdata = data_final_clean,
                   n.trees = 1500,
                   type = "response")


test_atf = predict(object = atf_model_log_3_3,
                  newdata = test_1_3,
                  n.trees = 1500,
                  type = "response")


Survresptrain <- Surv(data_final_clean$date_integer,data_final_clean$status==1)
Survresptest <- Surv(test_1_3$date_integer,test_1_3$status ==1)
CI_atf_model_log_3_3 <- BeggC(Survresptrain, Survresptest, train_atf, test_atf)
if(CI_atf_model_log_3_3<=0.5){
  CI_atf_model_log_3_3 =1-CI_atf_model_log_3_3
}
CI_atf_model_log_3_3


# 5.Compare the different groups
## Generating new data frames for make groups 1, 2 and 3
data_make_group_1 <- (data_final_clean[which(data_final_clean$make_group == 1)])
data_make_group_2 <- (data_final_clean[which(data_final_clean$make_group == 2)])
data_make_group_3 <- (data_final_clean[which(data_final_clean$make_group == 3)])


## Plotting Kaplan-Meier Curves for different brands
model_fit_make_group <- survfit(Surv(data_final_clean$date_integer, data_final_clean$status==1) ~ data_final_clean$make_group, data=data_final_clean) 
autoplot(model_fit_make_group) +
  labs(x = "\n Survival Time (days) ", y = "Survival Probabilities \n",
       title = "Survival Time of Automobile Listing differentiated by \n brand groups \n") +
  theme(plot.title = element_text(hjust=0.5),
        axis.title.x = element_text(face = "bold", color = "#FF7A33", size = 12),
        axis.title.y = element_text(face = "bold", color = "#FF7A33", size = 12),
        legend.title = element_text(face = "bold", size = 10))

model_fit_price_groups_make_1 <- survfit(Surv(data_make_group_1$date_integer, data_make_group_1$status==1) ~ data_make_group_1$price_groups, data=data_make_group_1) 
autoplot(model_fit_price_groups_make_1) +
  labs(x = "\n Survival Time (days) ", y = "Survival Probabilities \n",
       title = "Survival Time of Automobile Listing in make group 1\n  differentiated by price groups \n") +
  theme(plot.title = element_text(hjust=0.5),
        axis.title.x = element_text(face = "bold", color = "#FF7A33", size = 12),
        axis.title.y = element_text(face = "bold", color = "#FF7A33", size = 12),
        legend.title = element_text(face = "bold", size = 10))

model_fit_price_groups_make_2 <- survfit(Surv(data_make_group_2$date_integer, data_make_group_2$status==1) ~ data_make_group_2$price_groups, data=data_make_group_2) 
autoplot(model_fit_price_groups_make_2) +
  labs(x = "\n Survival Time (days) ", y = "Survival Probabilities \n",
       title = "Survival Time of Automobile Listing in make group 2\n  differentiated by price groups \n") +
  theme(plot.title = element_text(hjust=0.5),
        axis.title.x = element_text(face = "bold", color = "#FF7A33", size = 12),
        axis.title.y = element_text(face = "bold", color = "#FF7A33", size = 12),
        legend.title = element_text(face = "bold", size = 10))

model_fit_price_groups_make_3 <- survfit(Surv(data_make_group_3$date_integer, data_make_group_3$status==1) ~ data_make_group_3$price_groups, data=data_make_group_3) 
autoplot(model_fit_price_groups_make_3) +
  labs(x = "\n Survival Time (days) ", y = "Survival Probabilities \n",
       title = "Survival Time of Automobile Listing in make group 3\n  differentiated by price groups \n") +
  theme(plot.title = element_text(hjust=0.5),
        axis.title.x = element_text(face = "bold", color = "#FF7A33", size = 12),
        axis.title.y = element_text(face = "bold", color = "#FF7A33", size = 12),
        legend.title = element_text(face = "bold", size = 10))

model_fit_mileage_groups_make_1 <- survfit(Surv(data_make_group_1$date_integer, data_make_group_1$status==1) ~ data_make_group_1$mileage_groups, data=data_make_group_1) 
autoplot(model_fit_mileage_groups_make_1) +
  labs(x = "\n Survival Time (days) ", y = "Survival Probabilities \n",
       title = "Survival Time of Automobile Listing in make group 1\n  differentiated by mileage groups \n") +
  theme(plot.title = element_text(hjust=0.5),
        axis.title.x = element_text(face = "bold", color = "#FF7A33", size = 12),
        axis.title.y = element_text(face = "bold", color = "#FF7A33", size = 12),
        legend.title = element_text(face = "bold", size = 10))

model_fit_mileage_groups_make_2 <- survfit(Surv(data_make_group_2$date_integer, data_make_group_2$status==1) ~ data_make_group_2$mileage_groups, data=data_make_group_2) 
autoplot(model_fit_mileage_groups_make_2) +
  labs(x = "\n Survival Time (days) ", y = "Survival Probabilities \n",
       title = "Survival Time of Automobile Listing in make group 2\n  differentiated by mileage groups \n") +
  theme(plot.title = element_text(hjust=0.5),
        axis.title.x = element_text(face = "bold", color = "#FF7A33", size = 12),
        axis.title.y = element_text(face = "bold", color = "#FF7A33", size = 12),
        legend.title = element_text(face = "bold", size = 10))

model_fit_mileage_groups_make_3 <- survfit(Surv(data_make_group_3$date_integer, data_make_group_3$status==1) ~ data_make_group_3$mileage_groups, data=data_make_group_3) 
autoplot(model_fit_mileage_groups_make_3) +
  labs(x = "\n Survival Time (days) ", y = "Survival Probabilities \n",
       title = "Survival Time of Automobile Listing in make group 3\n  differentiated by mileage groups \n") +
  theme(plot.title = element_text(hjust=0.5),
        axis.title.x = element_text(face = "bold", color = "#FF7A33", size = 12),
        axis.title.y = element_text(face = "bold", color = "#FF7A33", size = 12),
        legend.title = element_text(face = "bold", size = 10))


## Calculating tree models for the three groups
treefit_group_1 <- rpart(Surv(date_integer,status == 1 ) ~ saved_or_contacted + last_belt_service_filled + (mileage_groups) + body_color_id_filled 
                         + body_type_id_filled + seal_ids_amount + fuel_id + (price_groups) + images_number_group 
                         + (detail_and_app_result_page_views) + time_to_next_inspection_static, data = data_make_group_1)

treefit_group_2 <- rpart(Surv(date_integer,status == 1 ) ~ saved_or_contacted + last_belt_service_filled + (mileage_groups) + body_color_id_filled 
                         + body_type_id_filled + seal_ids_amount + fuel_id + (price_groups) + images_number_group 
                         + (detail_and_app_result_page_views) + time_to_next_inspection_static, data = data_make_group_2)

treefit_group_3 <- rpart(Surv(date_integer,status == 1 ) ~ saved_or_contacted + last_belt_service_filled + (mileage_groups) + body_color_id_filled 
                         + body_type_id_filled + seal_ids_amount + fuel_id + (price_groups) + images_number_group 
                         + (detail_and_app_result_page_views) + time_to_next_inspection_static, data = data_make_group_3)


## Plotting the tree models for the three groups
treefit_group_1_plot <- as.party(treefit_group_1)
treefit_group_2_plot <- as.party(treefit_group_2)
treefit_group_3_plot <- as.party(treefit_group_3)

plot(treefit_group_1_plot)
plot(treefit_group_2_plot)
plot(treefit_group_3_plot)


# 6.Product Feature
## Calculate the set size and receiving the uplift from product feature
number_of_customers <- unique(data_final_clean$vendor_id[which(data_final_clean$price_groups==1)]) ##3853
number_of_listings <- unique(data_final_clean$guid[which(data_final_clean$price_groups==1)])
potential_listings <- 0.33 * number_of_listings
average_page_views <- median(data_final_clean$detail_and_app_result_page_views[which(data_final_clean$price_groups==1)])
average_page_views_group_2_users <- 0.33 * median(data_final_clean$detail_and_app_result_page_views[which(data_final_clean$price_groups==1)])


## Generate new data frame with only mean values for price group 1
data_final_clean_age <- data_final_clean[!is.na(data_final_clean$age_of_listing)]
average_survival_time <- mean(data_final_clean_age$age_of_listing[which(data_final_clean_age$price_groups==1)])

data_final_numeric_only <- as.data.frame(data_final_clean_age[, c(5,7,8,9,11,12,13,29,36,59,61,66,68,70,71,73)], drop=false)
meansofcol <- as.data.frame(colMeans(data_final_numeric_only),drop=false)


## To add entirely new data and see its performance, we have to enrich it. 
## First plot the mean data survival curve and then the survival curve of the product feature with 20% higher page views
plot(survfit(data_final_clean.cox, newdata = data.frame(saved_or_contacted=25,mileage_groups=1.7,body_color_id_filled=1,fuel_id=0.53,price=17535,images_number_group=2.6,detail_and_app_result_page_views=6583,make_group=1.65)),
     xlab = "Days", ylab="Survival")

plot(survfit(data_final_clean.cox, newdata = data.frame(saved_or_contacted=25,mileage_groups=1.7,body_color_id_filled=1,fuel_id=0.53,price=17535,images_number_group=2.6,detail_and_app_result_page_views=7900,make_group=1.65)),
     xlab = "Days", ylab="Survival")                                                                                                                                                                                                                           ```