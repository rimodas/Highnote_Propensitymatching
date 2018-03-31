#install.packages("MatchIt")
library(MatchIt)
#install.packages("dplyr")
library(dplyr)
library(ggplot2)
#install.packages("tidyverse")
library(purrr)
library(psych)
setwd("/Users/rimo/Documents/Materials")
getwd()

#Summary statistics
highnote <- read.csv("HighNote Data Midterm-2.csv")
highnote %>% split(.$adopter) %>% map(describe)


#Analyze the differences in the mean values of the variables, comparing the adopter and non-adapter subsamples

lapply(highnote[,c('age','male' , 'friend_cnt' , 'avg_friend_male' ,'avg_friend_age', 
             'friend_country_cnt'  , 'songsListened' , 'lovedTracks' , 
             'posts' , 'playlists' ,'shouts' , 'tenure' ,'good_country', 'subscriber_friend_cnt')], function(i) t.test(i ~ highnote$adopter))

#DATA VISUALIZATION
#demographics
#age
ggplot(highnote,aes(x=age,group=adopter,fill=adopter))+
  geom_histogram(position="identity",binwidth=0.5)+theme_classic()

#male
ggplot(highnote,aes(x=male,group=adopter,fill=adopter))+
  geom_bar(position="dodge")+theme_classic()

#good_country
ggplot(highnote,aes(x=good_country,group=adopter,fill=adopter))+
  geom_bar(position="dodge")+theme_classic()

#PEER INFLUENCE
#friend_cnt
friend_cnt<-highnote %>%
  group_by(adopter)%>%
  summarise(friend_cnt=mean(friend_cnt))
ggplot(friend_cnt,aes(x = adopter,y=friend_cnt)) +
  geom_bar(stat="identity",position=position_identity(), fill="navyblue")+theme_classic()

#friend age
ggplot(highnote,aes(x=avg_friend_age,group=adopter,fill=adopter))+
  geom_histogram(position="identity",binwidth=0.5)+theme_classic()

#friend country count
ggplot(highnote,aes(x=friend_country_cnt,group=adopter,fill=adopter))+
  geom_histogram(position="identity",binwidth=0.5)+theme_classic()

#subscriber friend count
subscriber_friend_cnt<-highnote %>%
  group_by(adopter)%>%
  summarise(subscriber_friend_cnt=mean(subscriber_friend_cnt))
ggplot(subscriber_friend_cnt,aes(x = adopter,y=subscriber_friend_cnt)) +
  geom_bar(stat="identity",position=position_identity(), fill="navyblue")+theme_classic()

#avg friend male
avg_friend_male<- highnote %>%
  group_by(adopter)%>%
  summarise(avg_friend_male=mean(avg_friend_male))
ggplot(avg_friend_male,aes(x = adopter,y=avg_friend_male)) +
  geom_bar(stat="identity",position=position_identity(), fill="navyblue")+theme_classic()


#USER ENGAGEMENT
#songs listened
songsListened<- highnote %>%
  group_by(adopter)%>%
  summarise(songsListened=mean(songsListened))
ggplot(songsListened,aes(x = adopter,y=songsListened)) +
  geom_bar(stat="identity",position=position_identity(), fill="navyblue")+theme_classic()

#loved tracks
lovedTracks<- highnote %>%
  group_by(adopter)%>%
  summarise(lovedTracks=mean(lovedTracks))
ggplot(lovedTracks,aes(x = adopter,y=lovedTracks)) +
  geom_bar(stat="identity",position=position_identity(), fill="navyblue")+theme_classic()

#posts
posts<-highnote %>%
  group_by(adopter)%>%
  summarise(posts=mean(posts))
ggplot(posts,aes(x = adopter,y=posts)) +
  geom_bar(stat="identity",position=position_identity(), fill="navyblue")+theme_classic()

#playlists
playlists<-highnote %>%
  group_by(adopter)%>%
  summarise(playlists=mean(playlists))
ggplot(playlists,aes(x = adopter,y=playlists)) +
  geom_bar(stat="identity",position=position_identity(), fill="navyblue")+theme_classic()

#shouts
shouts<-highnote %>%
  group_by(adopter)%>%
  summarise(shouts=mean(shouts))
ggplot(shouts,aes(x = adopter,y=shouts)) +
  geom_bar(stat="identity",position=position_identity(), fill="navyblue")+theme_classic()

#tenure
tenure<- highnote %>%
  group_by(adopter)%>%
  summarise(tenure=mean(tenure))
ggplot(tenure,aes(x = adopter,y=tenure)) +
  geom_bar(stat="identity",position=position_identity(), fill="navyblue")+theme_classic()


#grouping subscriber friend count. 1 if friend is > 0 (treatment)  0 if 0 friends (control)
highnote$subscriber_friend_cnt <- ifelse(highnote$subscriber_friend_cnt >0,1,0)

#with(highnote, t.test(subscriber_friend_cnt ~ adopter))


#calculating mean of covariates after grouping with subscriber friend count
highnote_cov <- c('age', 'male', 'good_country', 'friend_cnt', 'avg_friend_age', 'avg_friend_male', 'friend_country_cnt', 'songsListened', 'lovedTracks', 'posts', 'playlists', 'shouts', 'tenure' )
highnote %>%
  group_by(adopter) %>%
  select(one_of(hn_cov)) %>%
  summarise_all(funs(mean(., na.rm = T)))

#matching for propensity score
m_ps <- glm(subscriber_friend_cnt ~ age + male + good_country + 
              friend_cnt + avg_friend_age + avg_friend_male + friend_country_cnt + 
              songsListened + lovedTracks + posts + playlists + shouts + tenure,
            family = binomial(), data = highnote)
summary(m_ps)


prs_df <- data.frame(pr_score = predict(m_ps, type = "response"),
                     subscriber_friend_cnt = m_ps$model$subscriber_friend_cnt)
head(prs_df)

#examining region of common support
labs <- paste("Type of User:", c("Premium", "Non-Premium"))
prs_df %>%
  mutate(adopter = ifelse(subscriber_friend_cnt == 1, labs[1], labs[2])) %>%
  ggplot(aes(x = pr_score)) +
  geom_histogram(color="navyblue", fill="lightblue") +
  facet_wrap(~subscriber_friend_cnt) +
  xlab("Probability of Treatment (Having >1 Subscriber)") +
  theme_classic() 

#install.packages("MatchIt")
library(MatchIt)

#find pairs of observations with similar propensity scores

#ecls_nomiss <- highnote %>%  # MatchIt does not allow missing values
  #select(subscriber_friend_cnt, adopter, one_of(highnote_cov)) %>%
  #na.omit()

mod_match <- matchit(subscriber_friend_cnt ~ age + male + good_country + friend_cnt + avg_friend_age + avg_friend_male + friend_country_cnt + songsListened + lovedTracks + posts + playlists + shouts + tenure,
                     method = "nearest", data = highnote)

#information about matching success
summary(mod_match)
plot(mod_match)

#create dataframe using only matched observations
dta_m <- match.data(mod_match)
dim(dta_m)


#original data
originaldata_mean  <- highnote%>%
  group_by(subscriber_friend_cnt) %>%
  select(one_of(highnote_cov)) %>%
  summarise_all(funs(mean))

head(originaldata_mean)

#matched data
matcheddata_mean  <- dta_m%>%
  group_by(subscriber_friend_cnt) %>%
  select(one_of(highnote_cov)) %>%
  summarise_all(funs(mean))

View(matcheddata_mean)
write.csv(matcheddata_mean, file = "matchedmean.csv", row.names=FALSE)

#Visual Inspection

fn_bal <- function(dta, variable) {
  dta$variable <- dta[, variable]
  dta$subscriber_friend_cnt <- as.factor(dta$subscriber_friend_cnt)
  support <- c(min(dta$variable), max(dta$variable))
  ggplot(dta, aes(x = distance, y = variable, color = subscriber_friend_cnt)) +
    geom_point(alpha = 0.2, size = 1.3) +
    geom_smooth(method = "loess", se = F) +
    xlab("Propensity score") +
    ylab(variable) +
    theme_bw() +
    ylim(support)
}


#install.packages("gridExtra")

library(gridExtra)

grid.arrange(
  fn_bal(dta_m, "age"),
  fn_bal(dta_m, "male") + theme(legend.position = "none"),
  fn_bal(dta_m, "good_country"),
  fn_bal(dta_m, "friend_cnt") + theme(legend.position = "none"),
  fn_bal(dta_m, "avg_friend_age"),
  fn_bal(dta_m, "avg_friend_male") + theme(legend.position = "none"),
  fn_bal(dta_m, "friend_country_cnt"),
  fn_bal(dta_m, "songsListened") + theme(legend.position = "none"),
  fn_bal(dta_m, "lovedTracks"),
  fn_bal(dta_m, "posts") + theme(legend.position = "none"),
  fn_bal(dta_m, "playlists"),
  fn_bal(dta_m, "shouts") + theme(legend.position = "none"),
  fn_bal(dta_m, "tenure"),
  nrow = 7, widths = c(1, 0.8)
)


#OLS with and without covariates
#without
lm_treat1 <- lm(adopter ~ subscriber_friend_cnt, data = dta_m)
summary(lm_treat1)

#with
lm_treat2 <- lm(adopter ~ subscriber_friend_cnt +  age + male + good_country + friend_cnt + avg_friend_age + avg_friend_male + friend_country_cnt + songsListened + lovedTracks + posts + playlists + shouts + tenure, data = dta_m)
summary(lm_treat2)

#Logistic Regression

result <- glm(adopter ~ male + age + subscriber_friend_cnt + friend_cnt + avg_friend_age + friend_country_cnt + songsListened + lovedTracks + good_country + playlists + tenure + shouts + posts + avg_friend_male,
            family = binomial(), data = highnote)
summary(result)

#run another regression with only significant variables
final_result <- glm(adopter ~ age + male  + avg_friend_age+friend_country_cnt+subscriber_friend_cnt+
              songsListened+lovedTracks+playlists+
              tenure+good_country,
            family = binomial(), data = highnote)
summary(final_result)

exp(final_result$coefficients)-1
