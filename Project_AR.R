library(readxl)
library(dplyr)
getwd()
setwd("/Users/adhirajan/Desktop/SDM Project/Data")

#Importing Chicago crime data
Crime_data = read_excel("Crime_data.xlsx", sheet = "Crime_data")
str(Crime_data)

#number of public schools by community
public_schools = read_excel("Chicago_Public_Schools.xlsx", sheet = "Chicago_Public_Schools")
colnames(public_schools)=tolower(make.names(colnames(public_schools)))
public_schools <- public_schools %>% group_by(community.area.number) %>% summarise(no_of_public_schools = n())
str(public_schools)

#number of health_support_agencies by community
Health_Support_Services = read_excel("Family_and_Support_Services_Delegate_Agencies.xlsx", sheet = "Health")
colnames(Health_Support_Services)=tolower(make.names(colnames(Health_Support_Services)))
Health_Support_Services <- Health_Support_Services %>% group_by(community.area.number) %>% summarise(health_support_agencies = n())
str(Health_Support_Services)

#number of homeless_support_agencies by community
Homeless_Support_Services = read_excel("Family_and_Support_Services_Delegate_Agencies.xlsx", sheet = "Homeless")
colnames(Homeless_Support_Services)=tolower(make.names(colnames(Homeless_Support_Services)))
Homeless_Support_Services <- Homeless_Support_Services %>% group_by(community.area.number) %>% summarise(homeless_support_agencies = n())
str(Homeless_Support_Services)

#number of employment_support_agencies by community
Employment_Support_Services = read_excel("Family_and_Support_Services_Delegate_Agencies.xlsx", sheet = "employment")
colnames(Employment_Support_Services)=tolower(make.names(colnames(Employment_Support_Services)))
Employment_Support_Services <- Employment_Support_Services %>% group_by(community.area.number) %>% summarise(employment_support_agencies = n())
str(Employment_Support_Services)


#police sentiment scores at district level
Sentiment_Scores = read_excel("Police_Sentiment_Scores.xlsx", sheet = "Police_Sentiment_Scores")
colnames(Sentiment_Scores)=tolower(make.names(colnames(Sentiment_Scores)))
Sentiment_Scores <- Sentiment_Scores %>% group_by(district) %>% summarise(sentiment_score = mean(safety))
str(Sentiment_Scores)

#SSL scores at district level
SSL_Scores = read_excel("AA_ data_combined.xlsx", sheet = "SSL Score")
colnames(SSL_Scores)=tolower(make.names(colnames(SSL_Scores)))
SSL_Scores <- SSL_Scores %>% group_by(year,district) %>% summarise(ssl_score = mean(ssl.score))
str(SSL_Scores)

#liquor moratorium status of each ward
Liquor_Status = read_excel("AA_ data_combined.xlsx", sheet = "Liquor moratorium")
colnames(Liquor_Status)=tolower(make.names(colnames(Liquor_Status)))
str(Liquor_Status)

#percent of owner occupied at each community
owner_occupied = read_excel("percentowneroccupied.xlsx", sheet = "Sheet2")
colnames(owner_occupied)=tolower(make.names(colnames(owner_occupied)))
str(owner_occupied)

#percent of population below poverty at each community
below_poverty = read_excel("POVERTY_AR2.xlsx", sheet = "Sheet1")
colnames(below_poverty)=tolower(make.names(colnames(below_poverty)))
str(below_poverty)

#percent of population by race
per_race = read_excel("Races.xlsx", sheet = "Sheet1")
colnames(per_race)=tolower(make.names(colnames(per_race)))
str(per_race)

#Population by community
population = read_excel("POPULATION.xlsx", sheet = "Sheet1")
colnames(population)=tolower(make.names(colnames(population)))
str(population)

#Percentage of non high school graduates
nhsg = read_excel("perc_nhsg.xlsx", sheet = "Sheet1")
colnames(nhsg)=tolower(make.names(colnames(nhsg)))
str(nhsg)


Crime_data = merge(Crime_data,public_schools,by.x="community_area",by.y="community.area.number", all.x=TRUE)
Crime_data = merge(Crime_data,Health_Support_Services,by.x="community_area",by.y="community.area.number", all.x=TRUE)
Crime_data = merge(Crime_data,Homeless_Support_Services,by.x="community_area",by.y="community.area.number", all.x=TRUE)
Crime_data = merge(Crime_data,Employment_Support_Services,by.x="community_area",by.y="community.area.number", all.x=TRUE)
Crime_data[is.na(Crime_data)] <- 0
Crime_data = merge(Crime_data,Sentiment_Scores,by.x="district",by.y="district", all.x=TRUE)
Crime_data = merge(Crime_data,SSL_Scores, by.x=c("district", "year"), by.y=c("district", "year"), all.x=TRUE)
Crime_data = merge(Crime_data,Liquor_Status, by.x=c("ward"), by.y=c("ward"), all.x=TRUE)
Crime_data = merge(Crime_data,owner_occupied, by.x=c("community_area", "year"), by.y=c("community", "year"), all.x=TRUE)
Crime_data = merge(Crime_data,below_poverty, by.x=c("community_area", "year"), by.y=c("community.area", "year"), all.x=TRUE)
Crime_data = merge(Crime_data,per_race, by.x=c("community_area", "year"), by.y=c("community", "year"), all.x=TRUE)
Crime_data = merge(Crime_data,population, by.x=c("community_area", "year"), by.y=c("community.area", "year"), all.x=TRUE)
Crime_data = merge(Crime_data,nhsg, by.x=c("community_area", "year"), by.y=c("community_area", "year"), all.x=TRUE)

#normalizing data with population
Crime_data$no_of_public_schools_per10k_pop <- (Crime_data$no_of_public_schools/Crime_data$population)*10000
Crime_data$health_support_agencies_per10k_pop <- (Crime_data$health_support_agencies/Crime_data$population)*10000
Crime_data$homeless_support_agencies_per10k_pop <- (Crime_data$homeless_support_agencies/Crime_data$population)*10000
Crime_data$employment_support_agencies_per10k_pop <- (Crime_data$employment_support_agencies/Crime_data$population)*10000

# Crime_data$percent.owner.occupied[is.na(Crime_data$percent.owner.occupied )]<-mean(Crime_data$percent.owner.occupied ,na.rm=TRUE)
# Crime_data$sentiment_score[is.na(Crime_data$sentiment_score )]<-mean(Crime_data$sentiment_score ,na.rm=TRUE)
# Crime_data$ssl_score[is.na(Crime_data$ssl_score )]<-mean(Crime_data$ssl_score ,na.rm=TRUE)
# Crime_data$per.below.poverty.level[is.na(Crime_data$per.below.poverty.level )]<-mean(Crime_data$per.below.poverty.level ,na.rm=TRUE)
# Crime_data$asian.percent[is.na(Crime_data$asian.percent )]<-mean(Crime_data$asian.percent ,na.rm=TRUE)
# Crime_data$baa.percent[is.na(Crime_data$baa.percent )]<-mean(Crime_data$baa.percent ,na.rm=TRUE)
# Crime_data$hl.percent[is.na(Crime_data$hl.percent )]<-mean(Crime_data$hl.percent ,na.rm=TRUE)
# Crime_data$white.percent[is.na(Crime_data$white.percent )]<-mean(Crime_data$white.percent ,na.rm=TRUE)
# Crime_data$population[is.na(Crime_data$population )]<-mean(Crime_data$population ,na.rm=TRUE)
# Crime_data$perc_nhsg[is.na(Crime_data$perc_nhsg )]<-mean(Crime_data$perc_nhsg ,na.rm=TRUE)

str(Crime_data)
colSums(is.na(Crime_data))   

col <- c("year", "month","ward","district", "community_area", "primary_type","liquor_moratorium_status")
Crime_data[col] <- lapply(Crime_data[col], factor)
str(Crime_data)

df <- Crime_data[complete.cases(Crime_data), ]
str(df)
#' Free up memory by removing unnecessary big data dataframes
rm(public_schools,population,Crime_data,nhsg,Health_Support_Services,Homeless_Support_Services,Employment_Support_Services,Sentiment_Scores,SSL_Scores,Liquor_Status,owner_occupied,below_poverty,per_race)                        

#write.csv(df,"/Users/adhirajan/Desktop/SDM Project/finaldata.csv", row.names = FALSE)

library(lattice)
bwplot(incidents ~ year, data=df)
histogram(df$incidents)


d <- df[, c(8,9,10,11,12,13,15,16,17,18,19,20,22)]
str(d)
library(PerformanceAnalytics)
#chart.Correlation(d)      
cor(d) 

library(lme4)
library(MASS) 

m1 <- glm(incidents ~ year + month + no_of_public_schools_per10k_pop + health_support_agencies_per10k_pop
             + percent.owner.occupied + perc_nhsg  + asian.percent  + ssl_score,
             family=poisson, data=df)

summary(m1)

m2 <- glmer(incidents ~  year + month + no_of_public_schools_per10k_pop + health_support_agencies_per10k_pop
             + percent.owner.occupied + perc_nhsg  + asian.percent  + ssl_score
            + (1|community_area) + (1|ward), family=poisson, data=df)

summary(m2)

m3 <- glmer(incidents ~ year + month + employment_support_agencies_per10k_pop + homeless_support_agencies_per10k_pop + 
              baa.percent + hl.percent  + per.below.poverty.level +
              (1|community_area) , family=poisson, data=df)
summary(m3)

m4 <- glmer(incidents ~ asian.percent + baa.percent + hl.percent + white.percent +
              (1|community_area) , family=poisson, data=df)
summary(m4)

library(stargazer)
stargazer(m1, m2, m3, type="text", single.row=TRUE)

# library(AER)
# dispersiontest(m1) 

#vif test
library("car")
vif(m2) 

#fixed effects
fixef(m2)
#random effects
community_area <- data.frame(c(ranef(m2)$community_area))
community_area[order(community_area$X.Intercept),]

ranef(m2)$community_area

