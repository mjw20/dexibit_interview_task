# analysis the random forest results
# 2022.06
# Jun

# this script has to be run after data_exploration.R loaded.

library(tidyverse)

# read in results
pred_test = read_csv("./data/pred_test.csv")
pred_goal = read_csv("./data/pred_goal.csv")
feature_import = read_csv("./data/feature_import.csv")

# convert back from log scale
pred_test = pred_test %>% mutate(tickets_pred = round(exp(pred_test)))
pred_goal = pred_goal %>% mutate(tickets_pred = round(exp(pred_goal)))

pred_test$total_tickets_sold = df_sold[-trainIndex, "total_tickets_sold"]
pred_test$log_tickets_sold = test$log_tickets_sold

# Mean Absolute Error after converting back from log scale
pred_test %>%
  mutate(error = abs(tickets_pred - total_tickets_sold)) %>% 
  summarise(mae = round(mean(error))) %>%
  pull(mae)

# 242

pred_test %>% 
  mutate(error = abs(tickets_pred - total_tickets_sold)) %>% 
  mutate(mape = 100*error/total_tickets_sold) %>% 
  summarise(accuracy = 100 - mean(mape))
# 59.8

# log scale comparison
pred_test %>%
  mutate(error = abs(pred_test - log_tickets_sold)) %>% 
  summarise(mae = (mean(error))) %>%
  pull(mae)

pred_test %>% 
  mutate(error = abs(pred_test - log_tickets_sold)) %>% 
  mutate(mape = 100*error/log_tickets_sold) %>% 
  summarise(accuracy = 100 - mean(mape))

# feature importance
feature_import %>%
  ggplot() +
  geom_bar(aes(features, weight = importance), fill = "blue") +
  scale_x_discrete(limits = feature_import$features[order(feature_import$importance)]) +
  coord_flip() +
  labs(y = "relative importance")

# analysis of the top important features
## budget
df_budget = df_sold %>%
  mutate(budget = as.character(budget)) %>% 
  group_by(budget) %>% 
  summarise(all_tickets = sum(total_tickets_sold),
            n_days = n()) %>% 
  mutate(tickets_per_day = all_tickets/n_days)

df_budget %>% 
  ggplot() +
  geom_bar(aes(budget, weight = tickets_per_day), fill = "blue") +
  scale_x_discrete(limits = as.character(unique(df_sold$budget)[order(unique(df_sold$budget))])) +
  ylab("tickets_per_day") +
  coord_flip()

df_sold %>%
  mutate(budget = as.character(budget)) %>% 
  ggplot() + geom_boxplot(aes(budget, total_tickets_sold)) +
  coord_flip() +
  scale_x_discrete(limits = as.character(unique(df_sold$budget)[order(unique(df_sold$budget))]))

## visitors
df_sold %>% 
  ggplot() +
  geom_point(aes(visitors, total_tickets_sold))

cor(df_sold$visitors, df_sold$total_tickets_sold)

# df_sold %>% 
#   mutate(ratio = total_tickets_sold/visitors) %>% 
#   ggplot() +
#   geom_point(aes(ratio, total_tickets_sold))
# 
# ratio = df_sold %>% mutate(ratio = total_tickets_sold/visitors) %>% pull(ratio)
# cor(ratio, df_sold$total_tickets_sold)

## group
df_sold %>% 
  mutate(group = as.character(group)) %>% 
  ggplot() + geom_boxplot(aes(group, total_tickets_sold)) +
  coord_flip()

## duration min/duration max
df_sold %>% 
  mutate(duration_min = as.character(duration_min)) %>% 
  group_by(duration_min) %>% summarise(n = sum(total_tickets_sold)) %>% 
  arrange(desc(n))

df_sold %>% 
  mutate(duration_max = as.character(duration_max)) %>% 
  group_by(duration_max) %>% summarise(n = sum(total_tickets_sold)) %>% 
  arrange(desc(n))

df_meta$end_date - df_meta$start_date

df_sold %>% filter(duration_min == 8) %>%
  select(total_tickets_sold, duration_min, duration_max) %>% 
  arrange(desc(total_tickets_sold))

df_sold %>% filter(duration_min == 9) %>%
  select(total_tickets_sold, duration_min, duration_max) %>% 
  arrange(desc(total_tickets_sold))

# niche
df_sold %>% 
  mutate(niche = as.character(niche)) %>% 
  ggplot() + geom_boxplot(aes(niche, total_tickets_sold)) +
  coord_flip()

# day of week
df_sold %>% 
  ggplot() + geom_boxplot(aes(day_of_week, total_tickets_sold)) +
  coord_flip()
