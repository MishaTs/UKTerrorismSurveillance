library(readxl)
SimpJPDat <- read_excel("Desktop/SimpJPDat.xlsx")
View(SimpJPDat)
library(dplyr)
library(ggplot2)
library(arsenal)
library(ggthemes)
library(plm)

posdat <- SimpJPDat %>% filter(Level == 1)

ggplot(data=posdat, aes(x=Date, y=Total, group=Subj)) +
  geom_line()+
  geom_point()

sdat <- SimpJPDat %>% filter(Subj == 1)
tdat <- SimpJPDat %>% filter(Subj == 0)

summary(comparedf(sdat,tdat))

colnames(tdat) <- paste("t", colnames(tdat), sep = ".")
colnames(tdat)[29] <- "Date"


compdat <- merge(sdat, tdat, by="Date")

rel <- lm(Total ~ t.Total, data = compdat)

JPSurveyData <- read_excel("Desktop/JPSurveyData.xlsx")
View(JPSurveyData)

sdatF <- JPSurveyData %>% filter(Subj == 1)
tdatF <- JPSurveyData %>% filter(Subj == 0)
colnames(tdatF) <- paste("t", colnames(tdatF), sep = ".")
colnames(tdatF)[29] <- "Date"
colnames(tdatF)[2] <- "Level"
compdatF <- merge(sdatF, tdatF, by=c("Date", "Level"))
rel <- lm(Total ~ t.Total, data = compdatF)

#too few data points on terrorism in the UK and EUrope between Jan 2015 and March 2016
#first differences of the expanded dataset (w/ column coding for terrorism and surveillance) --> only look at Total 
#bc total is weighted of category breakdowns

colnames(sdat) <- paste("s", colnames(sdat), sep = ".")
colnames(sdat)[29] <- "Date"

p.sdat <- sdat %>% filter(s.Level == 1)
n.sdat <- sdat %>% filter(s.Level == 0)
p.tdat <- tdat %>% filter(t.Level == 1)
n.tdat <- tdat %>% filter(t.Level == 0)

colnames(p.sdat) <- paste("p", colnames(p.sdat), sep = ".")
colnames(p.sdat)[29] <- "Date"
colnames(n.sdat) <- paste("n", colnames(n.sdat), sep = ".")
colnames(n.sdat)[29] <- "Date"
colnames(p.tdat) <- paste("p", colnames(p.tdat), sep = ".")
colnames(p.tdat)[29] <- "Date"
colnames(n.tdat) <- paste("n", colnames(n.tdat), sep = ".")
colnames(n.tdat)[29] <- "Date"

temp.compdat1 <- merge(p.sdat, n.sdat, by="Date")
temp.compdat2 <- merge(p.tdat, n.tdat, by="Date")
big.compdat <- merge(temp.compdat1, temp.compdat2, by="Date")

big.compdat$n.s.Total[1] <- 49

ggplot(data=big.compdat, aes(x=Date)) +
  geom_line(aes(y=p.s.Total), colour="red")+ geom_point(aes(y=p.s.Total), colour="red") +
  geom_line(aes(y=n.s.Total), colour="dark green")+geom_point(aes(y=n.s.Total), colour="dark green") +
  geom_line(aes(y=p.t.Total), colour="blue")+geom_point(aes(y=p.t.Total), colour="blue") +
  geom_line(aes(y=n.t.Total), colour="orange")+ geom_point(aes(y=n.t.Total), colour="orange") + 
  ggtitle("UK Citizen Concern about Surveillance and Terrorism") + ylab("% Concerned") + xlab("") +
  theme(legend.position = c(0.95, 0.95), legend.justification = c("right", "top")) +
  theme_economist()
  

graphdat <- big.compdat %>% select("p.s.Total", "n.s.Total", "p.t.Total", "n.t.Total", "Date") %>% 
  tidyr::gather("id", "value", 1:4)

graphdat$id <- as.factor(graphdat$id)

levels(graphdat$id)[levels(graphdat$id)=="p.s.Total"] <- "Surveillance: concerned"
levels(graphdat$id)[levels(graphdat$id)=="n.s.Total"] <- "Surveillance: unconcerned"
levels(graphdat$id)[levels(graphdat$id)=="p.t.Total"] <- "Terrorism: concerned"
levels(graphdat$id)[levels(graphdat$id)=="n.t.Total"] <- "Terrorism: unconcerned"
names(graphdat)[names(graphdat)=="id"]  <- "Opinion"

head(graphdat)

ggplot(data=graphdat, aes(x=Date, y = value, colour = Opinion)) + geom_point() + geom_line() +
  ggtitle("Who's the enemy?", subtitle = "Britons fear terrorism more than surveillance") + ylab("") + xlab("") +
  scale_color_discrete(name="") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  theme_fivethirtyeight()

#geom_point(aes(color = Subj, shape = Pro))


fd.mod <- plm(Total ~ t.Total + as.factor(Date), data = compdatF, index = c("Date", "Level"), model = "fd")
summary(fd.mod)

####next steps


graphdat <- big.compdat %>% select("p.s.18to24", "n.s.18to24", "p.t.18to24", "n.t.18to24", "Date") %>% 
  tidyr::gather("id", "value", 1:4)

graphdat$id <- as.factor(graphdat$id)

levels(graphdat$id)[levels(graphdat$id)=="p.s.18to24"] <- "Surveillance: concerned"
levels(graphdat$id)[levels(graphdat$id)=="n.s.18to24"] <- "Surveillance: unconcerned"
levels(graphdat$id)[levels(graphdat$id)=="p.t.18to24"] <- "Terrorism: concerned"
levels(graphdat$id)[levels(graphdat$id)=="n.t.18to24"] <- "Terrorism: unconcerned"
names(graphdat)[names(graphdat)=="id"]  <- "Opinion"

head(graphdat)

ggplot(data=graphdat, aes(x=Date, y = value, colour = Opinion)) + geom_point() + geom_line() +
  ggtitle("What if phones but too much?", subtitle = "British youth are substantially more concerned about surveillance") + ylab("") + xlab("") +
  scale_color_discrete(name="") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  theme_fivethirtyeight()

graphdat <- big.compdat %>% select("p.s.London", "n.s.London", "p.t.London", "n.t.London", "Date") %>% 
  tidyr::gather("id", "value", 1:4)

graphdat$id <- as.factor(graphdat$id)

levels(graphdat$id)[levels(graphdat$id)=="p.s.London"] <- "Surveillance: concerned"
levels(graphdat$id)[levels(graphdat$id)=="n.s.London"] <- "Surveillance: unconcerned"
levels(graphdat$id)[levels(graphdat$id)=="p.t.London"] <- "Terrorism: concerned"
levels(graphdat$id)[levels(graphdat$id)=="n.t.London"] <- "Terrorism: unconcerned"
names(graphdat)[names(graphdat)=="id"]  <- "Opinion"

head(graphdat)

ggplot(data=graphdat, aes(x=Date, y = value, colour = Opinion)) + geom_point() + geom_line() +
  ggtitle("Capital anxiety", subtitle = "Londoners are especially anxious about both terrorism and surveillance") + ylab("") + xlab("") +
  scale_color_discrete(name="") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  theme_fivethirtyeight()

HateCrimeDataUK <- read_excel("Desktop/Data/WWS TF/HateCrimeDataUK.xlsx")
View(HateCrimeDataUK)

names(HateCrimeDataUK)[names(HateCrimeDataUK)=="bias"]  <- "Motivation"

library(lubridate)
bruno <- mutate(HateCrimeDataUK, year  = (ymd(yr)))
bruno <- bruno %>% mutate(tm = as.integer(year(bruno$yr)))
head(bruno)
#HateCrimeDataUK$yr <- as.Date(HateCrimeDataUK$yr, origin="2009-01-01")

#lim <- as.POSIXct(c("2017-01-01"),  origin = "2009-01-01")

ggplot(bruno, aes(x=tm, y=val, fill=Motivation)) + 
  geom_area() + theme_economist() +
  ggtitle("Colouring hate", subtitle = "Incrasing racism continually motivates the majority of hate crimes") + ylab("Hate Crimes") + xlab("") +
  scale_x_continuous(breaks = c(2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017)) + scale_fill_brewer(palette="YlOrRd")
  

hatecrimefilt <- bruno %>% filter(HateCrimeDataUK$Motivation != "Racism and xenophobia")
View(hatecrimefilt)
ggplot(hatecrimefilt, aes(x=tm, y=val, fill=Motivation)) + 
  geom_area() + theme_economist() +
  ggtitle("Colouring hate", subtitle = "Racism increasingly motivates a growing share of hate crimes") + ylab("# Hate Crimes") + xlab("") +
  scale_x_continuous(breaks = c(2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017))