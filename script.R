library(tidyverse)
a=read_csv("test.csv")
b=a %>% 
  mutate(sexuality = tolower(sexuality)) %>% 
  mutate(
    sexuality_superset="other"
  ) %>% 
  mutate(sexuality_superset=ifelse(
    sexuality=="bisexual", "bisexual", sexuality_superset
  )) %>%
  mutate(sexuality_superset=ifelse(
    sexuality=="lesbian", "lesbian", sexuality_superset
  )) %>% 
  mutate(sexuality_superset=ifelse(
    sexuality=="gay", "gay", sexuality_superset
  )) %>% 
  mutate(sexuality_superset=ifelse(
    sexuality=="queer", "queer", sexuality_superset
  )) 

write_csv(b, "holly_data_with_supersets.csv")

d=b %>% 
  mutate(
    gender_superset=as_factor(gender_superset),
    sexuality_superset=as_factor(sexuality_superset)
  ) %>% select(-c(gender,sexuality))



mod1=lm(data=d,
        outcome ~ 
          anxiety + avoidance + identity_centrality + identity_superiority + identity_negative + 
          gender_superset + sexuality_superset) 

contrasts(d$sexuality_superset) = "contr.sum"

mod2=lm(data=d,
        outcome ~ 
          anxiety + avoidance + identity_centrality + identity_superiority + identity_negative + 
          gender_superset + sexuality_superset) 
