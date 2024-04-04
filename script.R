suppressPackageStartupMessages(library(tidyverse))
library(standardize)
library(effects)
library(ggeffects)
library(see)

a=read_csv("holly_qualtrics_subset.csv", show_col_types = F)

a=a %>% mutate(gender=tolower(gender)) %>% 
  mutate(gender_superset="other") %>% 
  mutate(gender_superset=ifelse(gender=="cis woman", "cis woman", gender_superset)) %>% 
  mutate(gender_superset=ifelse(gender=="cis man", "cis man", gender_superset)) %>% 
  mutate(gender_superset=ifelse(gender=="trans man", "trans man", gender_superset)) %>% 
  mutate(gender_superset=ifelse(gender=="non-binary,trans man", "non-binary_trans man", gender_superset)) %>% 
  mutate(gender_superset=ifelse(gender=="non-binary", "non-binary", gender_superset)) %>% 
  mutate(gender_superset=ifelse(gender=="trans man", "trans man", gender_superset)) %>% 
  mutate(gender_superset=ifelse(gender=="trans woman", "trans woman", gender_superset))

b=a %>% 
  mutate(sexuality = tolower(sexuality)) %>% 
  mutate(sexuality_superset = "other") %>%
  mutate(sexuality_superset = ifelse(sexuality == "bisexual", "bisexual", sexuality_superset)) %>%
  mutate(sexuality_superset = ifelse(sexuality == "lesbian", "lesbian", sexuality_superset)) %>%
  mutate(sexuality_superset = ifelse(sexuality == "gay", "gay", sexuality_superset)) %>%
  mutate(sexuality_superset = ifelse(sexuality == "queer", "queer", sexuality_superset)) 

c=b %>%  
  select(-c(gender, sexuality)) %>% 
  rename(centrality=identity_centrality) %>% 
  rename(superiority=identity_superiority) %>% 
  rename(negative=identity_negative) %>% 
  rename(gender  = gender_superset) %>% 
  rename(sexuality = sexuality_superset) %>% 
  mutate(gender=str_replace_all(gender, pattern=" ", replace="_"))

write_csv(c, "holly_data_with_supersets.csv")



mod1=lm(data=d,
        outcome ~ 
          anxiety + avoidance + identity_centrality + identity_superiority + identity_negative + 
          gender_superset + sexuality_superset) 

#named_contr_sum(x, scale = 1, return_contr = TRUE)
#contrasts(d$sexuality_superset) = "contr.sum"
contrasts(d$sexuality_superset) = named_contr_sum(d$sexuality_superset)
#contrasts(d$gender_superset) = "contr.sum"
contrasts(d$gender_superset) = named_contr_sum(d$gender_superset)


mod2=lm(data=d,
        outcome ~ 
          anxiety + avoidance + identity_centrality + identity_superiority + identity_negative + 
          gender_superset + sexuality_superset) 

summary(mod1) %>%  print()
summary(mod2) %>% print()

plot(predictorEffects(mod1))
plot(predictorEffects(mod2) )



mydf <- predict_response(mod1, terms=c("gender_superset"))
ggplot(mydf, aes(x, predicted)) + stat_summary()
  


mydf=predict_response(mod2, terms=c("anxiety" , "avoidance", "identity_centrality" , "identity_superiority" , "identity_negative",   "gender_superset" , "sexuality_superset"))
plot(mydf)


mydf=predict_response(mod2, terms=c("anxiety" , "avoidance", "identity_centrality" , "identity_superiority" , "identity_negative"))
plot(mydf)
