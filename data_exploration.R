# Data Exploration Analysis & Features Preparation.
# 2022.06
# Jun

# NUMBER OF DAYS IT WAS RUN

# packages
library(tidyverse)
library(lubridate)
library(caret)
library(matrixStats)
library(fastDummies)

# read in data
daily_admin = read_csv("./data/dailyAdmission2022.csv")
daily_visit = read_csv("./data/dailyVisitation2022.csv")
df_events = readxl::read_xlsx("./data/events2022.xlsx")
df_meta = readxl::read_xlsx("./data/MetaData2022.xlsx")

# check variable types
glimpse(daily_admin)
glimpse(daily_visit)

# correct date format
daily_admin = daily_admin %>% 
  mutate(date_new = as.Date(record_date, "%d-%m-%Y"))
daily_visit$date_new = as.Date(daily_visit$record_date, "%d-%m-%Y")
df_meta$start_date = as.Date(df_meta$`Start Date`, "%d/%m/%Y")
df_meta$end_date = as.Date(df_meta$EndDate, "%d/%m/%Y")
names(df_events) = tolower(str_replace_all(names(df_events), " ", "_"))
df_events$from_date = as.Date(df_events$valid_from, "%d-%m-%Y")
df_events$to_date = as.Date(df_events$valid_through, "%d-%m-%Y")
# Note: School holidays are multiple holidays, others are single date events.

# check basic data info
summary(daily_admin)
sapply(daily_admin, function(x) sum(is.na(x)))
# note that the price per ticket is either NA or 0
summary(daily_visit)
sapply(daily_visit, function(x) sum(is.na(x)))
length(unique(daily_visit$id)) #id has no meaning here.

daily_admin2 = daily_admin %>% filter(sold > 0) 
#Note: Under the Ice and Under The Ice solved by this too

df_meta$end_date - df_meta$start_date # all positive no quality issue with meta info.

# change the name of necessary variable for df_meta
names(df_meta)[1] = "Product"
date_validation = daily_admin2 %>% 
  group_by(Product) %>% 
  summarise(min_date = min(date_new),
            max_date = max(date_new)) %>% 
  left_join(df_meta %>% select(Product, start_date, end_date), by = "Product")
#' Note: no information about Under The Ice in the meta data, so here I
#' decided to ignore this product.
#' Also, there is tickets sold prior than the start date as well as after the end date!
#' Assumption 1: those tickets sold prior the start date and after the end date are bad data.
#' Assumption 2: those dates has the wrong format need to interchange month and day
#' based on the examination of events data (i.e. school holidays), Assumption 2 is more feasible.

df_events$to_date - df_events$from_date # negative duration, data quality issue with inconsistent date format.

# Correct date

daily_admin3 = daily_admin2 %>% select(date_new, record_date, Product, TicketType, sold) %>% 
  filter(Product != "Under The Ice") %>% 
  left_join(df_meta %>% select(Product, start_date, end_date), by = "Product")

daily_admin3$date_pot = daily_admin3$date_new

daily_admin3$date_pot[which(daily_admin3$date_new < daily_admin3$start_date)] = 
  as.Date(daily_admin3$record_date[which(daily_admin3$date_new < daily_admin3$start_date)], "%m-%d-%Y")

daily_admin3$date_pot[which(daily_admin3$date_new > daily_admin3$end_date)] = 
  as.Date(daily_admin3$record_date[which(daily_admin3$date_new > daily_admin3$end_date)], "%m-%d-%Y")

# note some of the dates are not allowed to do this correction, so change it back to date_new
daily_admin3$date_pot[which(is.na(daily_admin3$date_pot))] = 
  daily_admin3$date_new[which(is.na(daily_admin3$date_pot))]

summary(daily_admin3$date_pot)

## df events dates.
df_events = df_events %>% mutate(diff = to_date - from_date)
df_events %>% filter(diff < 0) %>% 
  mutate(from_date_alt = as.Date(valid_from, "%m-%d-%Y"),
         to_date_alt = as.Date(valid_through, "%m-%d-%Y"))

df_events$from_date[which(df_events$diff < 0)[c(1,3,5,6)]] <- 
  as.Date(df_events$valid_from[which(df_events$diff < 0)[c(1,3,5,6)]], "%m-%d-%Y")
df_events$to_date[which(df_events$diff < 0)[c(2,4)]] <- 
  as.Date(df_events$valid_through[which(df_events$diff < 0)[c(2,4)]], "%m-%d-%Y")

df_events = df_events %>% mutate(diff = to_date - from_date)

# re-validate:
daily_admin3 %>% 
  group_by(Product) %>% 
  summarise(min_date = min(date_pot),
            max_date = max(date_pot)) %>% 
  left_join(df_meta %>% select(Product, start_date, end_date), by = "Product") %>% print()

#' Improved a lot, but still have observations outside of the events duration.
#' Use the strategy of Assumption 1 above to filter out those events.
#' Also Assumption 3: tickets sold is the sum of tickets sold across all the categories.
#' multi-visit, family-tickets, complimentary tickets are not examined separately here.


daily_admin3 %>% 
  filter(date_pot >= start_date, date_pot <= end_date) %>% 
  group_by(date_pot) %>% 
  summarise(tickets_sold = sum(sold)) %>% ggplot() +
  geom_line(aes(date_pot, tickets_sold))

# spread the tickets product 
daily_admin4 = daily_admin3 %>% 
  filter(date_pot >= start_date, date_pot <= end_date) %>%
  mutate(product = str_replace_all(Product, "[[:punct:]]", " ")) %>% 
  mutate(product = str_replace_all(product, " ", "_")) %>%
  group_by(date_pot, product) %>% 
  summarise(tickets_sold = sum(sold)) %>% 
  ungroup() %>% 
  spread(product, tickets_sold)

# Data Creation

## create date sequence
dates = seq(min(df_meta$start_date), max(df_meta$end_date), 1)
df = data.frame(dates)

## join tickets sold per day
df = df %>% left_join(daily_admin3 %>% 
  filter(date_pot >= start_date, date_pot <= end_date) %>%
  group_by(date_pot) %>% 
  summarise(total_tickets_sold = sum(sold)), by = c("dates" = "date_pot"))

## join visitors per day
df = df %>% 
  left_join(daily_visit %>% select(date_new, visitors), by = c("dates" = "date_new"))

## create number of products per day (some days have multiple exhibition)
df_meta = df_meta %>% 
  mutate(product = str_replace_all(Product, "[[:punct:]]", " ")) %>% 
  mutate(product = str_replace_all(product, " ", "_"))

df[,df_meta$product] <- 0

duration_check = function(x){
  product = df_meta$product[x]
  find_obs = which(df$dates >= df_meta$start_date[x] & df$dates <= df_meta$end_date[x])
  return(find_obs)
}

for(i in 1:length(df_meta$product)){
  df[,df_meta$product[i]][duration_check(i)] = 1
}

df$n_prod = df %>% select(df_meta$product) %>% rowSums()

## add floor info
df_meta$floor = word(df_meta$Floor) %>% tolower()
### reset products' columns
df[,df_meta$product] <- NA
for(i in 1:length(df_meta$product)){
  df[,df_meta$product[i]][duration_check(i)] = df_meta$floor[i]
}

df = df %>% add_column(lower = 0,
                  upper = 0,
                  ground = 0)

for(i in 1:dim(df)[1]){
  df$lower[i] = sum(str_count(df[,df_meta$product][i,], "lower"), na.rm = T)
  df$upper[i] = sum(str_count(df[,df_meta$product][i,], "upper"), na.rm = T)
  df$ground[i] = sum(str_count(df[,df_meta$product][i,], "ground"), na.rm = T)
}

## add location
df_meta$location = df_meta %>% pull(Location) %>% str_replace_all(" ", "_") %>% tolower()
df[,df_meta$product] <- NA

for(i in 1:length(df_meta$product)){
  df[,df_meta$product[i]][duration_check(i)] = df_meta$location[i]
}

df = df %>% add_column(major_exhibition_space = 0,
                       ice_gallery_1 = 0,
                       ice_gallery_2 = 0,
                       ice_gallery_3 = 0)


for(i in 1:dim(df)[1]){
  df$major_exhibition_space[i] = sum(str_count(df[,df_meta$product][i,], "major_exhibition_space"), na.rm = T)
  df$ice_gallery_1[i] = sum(str_count(df[,df_meta$product][i,], "ice_gallery_1"), na.rm = T)
  df$ice_gallery_2[i] = sum(str_count(df[,df_meta$product][i,], "ice_gallery_2"), na.rm = T)
  df$ice_gallery_3[i] = sum(str_count(df[,df_meta$product][i,], "ice_gallery_3"), na.rm = T)
}

## add marketing budget
df[,df_meta$product] <- 0

for(i in 1:length(df_meta$product)){
  df[,df_meta$product[i]][duration_check(i)] = df_meta$`Marketing Budget`[i]
}

df$budget = df %>% select(df_meta$product) %>% rowSums()

## add adult ticket price, simply add them together won't make any sense here,
## as an alternative use avg_adult_ticket_price if multi exhibition happens on the same day.
df[,df_meta$product] <- NA

for(i in 1:length(df_meta$product)){
  df[,df_meta$product[i]][duration_check(i)] = df_meta$`Adult Ticket Price`[i]
}

df$avg_price = df %>% select(df_meta$product) %>% as.matrix() %>% rowMins(na.rm = TRUE)

## classification
df[,df_meta$product] <- NA

for(i in 1:length(df_meta$product)){
  df[,df_meta$product[i]][duration_check(i)] = df_meta$Classification[i]
}

df = df %>% add_column(enthusiast = 0,
                       generalist = 0,
                       niche = 0)

for(i in 1:dim(df)[1]){
  df$enthusiast[i] = sum(str_count(df[,df_meta$product][i,], "Enthusiast"), na.rm = T)
  df$generalist[i] = sum(str_count(df[,df_meta$product][i,], "Generalist"), na.rm = T) +
    sum(str_count(df[,df_meta$product][i,], "Genralist"), na.rm = T) #correct the typo from the data.
  df$niche[i] = sum(str_count(df[,df_meta$product][i,], "Niche"), na.rm = T)
}

## Blockbuster; NA treated as -1.
df[,df_meta$product] <- 0
df_meta$blockbuster = 0
df_meta$blockbuster[which(df_meta$Blockbuster == "Y")] = 1
df_meta$blockbuster[which(df_meta$Blockbuster == "N/A")] = -1

for(i in 1:length(df_meta$product)){
  df[,df_meta$product[i]][duration_check(i)] = df_meta$blockbuster[i]
}


df$blockbuster = df %>% select(df_meta$product) %>% rowSums()

## Focus
df[,df_meta$product] <- NA

for(i in 1:length(df_meta$product)){
  df[,df_meta$product[i]][duration_check(i)] = df_meta$Focus[i]
}

df = df %>% add_column(group = 0,
                       single = 0)
for(i in 1:dim(df)[1]){
  df$group[i] = sum(str_count(df[,df_meta$product][i,], "Group"), na.rm = T)
  df$single[i] = sum(str_count(df[,df_meta$product][i,], "Single"), na.rm = T)
}

## Audience
df_meta$young_only = 0
df_meta$young_only[which(df_meta$Audience == "Young Culturals")] = 1

df[,df_meta$product] <- 0

for(i in 1:length(df_meta$product)){
  df[,df_meta$product[i]][duration_check(i)] = df_meta$young_only[i]
}

df$young_only = df %>% select(df_meta$product) %>% rowSums()

## duration
df[,df_meta$product] <- NA

for(i in 1:dim(df_meta)[1]){
  df[,df_meta$product[i]][which(df$dates >= df_meta$start_date[i] & df$dates <= df_meta$end_date[i])] =
    1:(as.numeric(df_meta$end_date[i] - df_meta$start_date[i])+1)
}

### minimum duration
df$duration_min = df %>% select(df_meta$product) %>% as.matrix() %>% rowMins(na.rm = TRUE)
### maximum duration
df$duration_max = df %>% select(df_meta$product) %>% as.matrix() %>% rowMaxs(na.rm = TRUE)

## drop the dummy variables:
df = df %>% select(-c(df_meta$product))

## extra features engineering around dates

df = df %>% mutate(year = year(dates),
              month = month(dates),
              day = day(dates),
              day_of_week = weekdays(dates, abbreviate = F)) %>% 
  mutate(dow = factor(day_of_week, 
                      levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"),
                      ordered = TRUE) %>% as.numeric())


### create dummy variables for day of week.
df = df %>% dummy_cols("day_of_week")

### cyclic encode month, week and day
df = df %>% mutate(month_cos = cos(2*pi*month/max(month)),
              month_sin = sin(2*pi*month/max(month)),
              week_cos = cos(2*pi*dow/max(dow)),
              week_sin =sin(2*pi*dow/max(dow)),
              day_cos = cos(2*pi*day/max(day)),
              day_sin = sin(2*pi*day/max(day)))

## add events
school_holidays = df_events %>% filter(type == "School Holidays")
public_holidays = df_events %>% filter(type == "Public Holidays")
regional_holidays = df_events %>% filter(type == "Regional")
special_holidays = df_events %>% filter(type == "Special")

df = df %>% add_column(school_holidays = 0,
                       public_holidays = 0,
                       regional_holidays = 0,
                       special_holidays = 0,
                       impact_of_competing_events = -1,
                       people_in_area_due_to_competing_events = -1)

### school holidays
for(i in 1:dim(school_holidays)[1]){
  index = which(df$dates >= school_holidays$from_date[i] &
                  df$dates <= school_holidays$to_date[i])
  df$school_holidays[index] = 1
}

### public holidays
for(i in 1:dim(public_holidays)[1]){
  index = which(df$dates == public_holidays$from_date[i])
  df$public_holidays[index] = 1
}

### regional holidays
for(i in 1:dim(regional_holidays)[1]){
  index = which(df$dates == regional_holidays$from_date[i])
  df$regional_holidays[index] = 1
}

### special holidays
for(i in 1:dim(special_holidays)[1]){
  index = which(df$dates == special_holidays$from_date[i])
  df$special_holidays[index] = 1
  df$impact_of_competing_events[index] = special_holidays$impact_of_competing_event[i]
  df$people_in_area_due_to_competing_events[index] = special_holidays$people_in_area_due_to_competing_events[i]
}

# Check distribution and missing values.
names(df)
df$avg_price[which(df$avg_price == Inf)] = NA
sapply(df, function(x) sum(is.na(x)))

## use mean value per month for visitors to impute the missing visitors.
estimated_visitors = df %>% 
  filter(is.na(visitors)) %>% 
  left_join(df %>% group_by(month) %>% summarise(mean_visitors = floor(mean(visitors, na.rm = T))), "month") %>% 
  pull(mean_visitors)

df$visitors[which(is.na(df$visitors))] = estimated_visitors

## we only care about tickets sold, so if tickets sold is na, we ignore them
df_sold = df %>% filter(!is.na(total_tickets_sold))
sapply(df_sold, function(x) sum(is.na(x)))

# data output

## data for our final predictions, the goal of this project
df_predict = df %>% filter(dates >= df_meta$start_date[1] & dates <= df_meta$end_date[1]) %>% 
  select(-c(dates, day, dow, month, day_of_week))
sapply(df_predict, function(x) sum(is.na(x)))
## 0.8 training, 0.2 testing split.
df_split = df_sold %>% select(-c(dates, day, dow, month, day_of_week)) %>% 
  mutate(log_tickets_sold = log(total_tickets_sold)) %>% 
  select(-total_tickets_sold)

set.seed(123)
trainIndex <- createDataPartition(df_split$log_tickets_sold, p = .8,
                                  list = FALSE,
                                  times = 1)
train <- df_split[trainIndex, ]
test <- df_split[-trainIndex, ]

write_csv(train, "./data/train_data.csv")
write_csv(test, "./data/test_data.csv")
write_csv(df_predict, "./data/predict_data.csv")

# # output final prediction for the targeted exhibition
# pred_goal = read_csv("./data/pred_goal.csv")
# df_output = df %>% 
#   filter(dates >= df_meta$start_date[1] & dates <= df_meta$end_date[1]) %>% 
#   select(-c(day, dow, month, day_of_week)) %>%
#   bind_cols(pred_goal) %>%
#   mutate(total_tickets_sold = round(exp(pred_goal)))
# 
# write_csv(df_output, "./data/Robert_Falcon_Scott_predictions.csv")
