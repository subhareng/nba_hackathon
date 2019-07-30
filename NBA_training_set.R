# load the necessary packages
library(plyr)
library(dplyr)
library(tm)
library(stringr)
library(tidytext)
library(ggplot2)
library(anytime)
library(NLP)
library(tm)
library(lubridate)
library(xgboost)
install.packages('xgboost')
stopwords("en")
NBA_train <- read_csv("/Users/srengar/Box Sync/Downloads/training_set.csv")

max(NBA_train$Created)

subset(NBA_train, NBA_train$Created == min(NBA_train$Created))
NBA_train
separate(NBA_train, "Created", into=c("date", 'time'), sep=" ")
NBA_train$datetimes<- anytime(NBA_train$Created)
NBA_train$dates<- anydate(NBA_train$Created)

NBA_train$day_of_week <- weekdays(NBA_train$dates)
NBA_train$hr<- format(as.POSIXct(NBA_train$datetimes), '%H:%M:%S')
content <- NBA_train$Description
NBA_train$hr
LeBron James @kingjames, 
Stephen Curry @stephencurry30
Dwyane Wade, @dwyanewade
Kyrie Irving, @kyrieirving
Russell Westbrook, @russwest44
Chris Paul, @cp3
Carmelo Anthony, @carmeloanthony
James Harden, @jharden13
Paul George, @ygtrece
Klay Thompson, @klaythompson
Draymond Green, @money23green
Giannis Annetukumpo, @giannis_an34

Golden State Warriors, @warriors
Cleveland Cavaliers, @cavs
Los Angeles Lakers, @lakers
Chicago Bulls, @chicagobulls
Miami Heat, @miamiheat
Oklahoma City Thunder, @okcthunder
Los Angeles Clippers, @laclippers
San Antonio Spurs, @spurs
Houston Rockets, @houstonrockets
New York Knicks, @nyknicks
Milwaukee Bucks, @bucks
Boston Celtics, @celtics 
Dallas Mavericks, @dallasmavs
Toronto Raptors, @raptors
Philadephia 76ers, @sixers
Brooklyn Nets, @brooklynnets
Atlanta Hawks, @atlhawks 
Charlotte Hornets, @hornets
Denver Nuggets, @nuggets
Detroit Pistons, @detroitpistons
Indiana Pacers, @pacers
Memphis Grizzlies, @memgrizz
Minnesota Timberwolves, @timberwolves
New Orleans Pelicans, @pelicansnba 
Orlando Magic, @orlandomagic
Phoenix Suns, @suns
Portland Trail Blazers, @trailblazers
Sacramento Kings, @sacramentokings
San Antonio Spurs, @spurs
Utah Jazz, @utahjazz
Washington Wizards, @washwizards
#buzzerbeater, WIN, #NBAonABC


stop_words <- stopwords("en")

content <- gsub("<code>.*?</code>","",content)
content <- gsub("href\\S+", "", content)
content <- gsub("http\\S+", "", content)
content <- gsub("([#@<>-])|[[:punct:]]", "\\1", content) # remove punctuation except "-/</>" 
content <- gsub("<.*?>", "", content)
content <- gsub("\n", " ", content)  # replace newline with a space
#content <- gsub("(\\S+\\d+|\\d+)\\S+", "", content) # remove words having any digits
content <- gsub("<a", "", content)
content <- gsub("%\xfc\xbe\x8d\xa3\xa4\xbc", "", content)
gsub(x = content, pattern = regex("\xfc\xbe\x8d\xa3\xa4\xbc"), replacement = "'")

content <- gsub("[^[:print:]]", "", content)

content
docs <- Corpus(VectorSource(content))

corpus = tm_map(docs, removeWords, c(unique(stopwords("english"), "were")))

corpus = tm_map(docs, stemDocument)

stopwords("english")
frequencies = DocumentTermMatrix(corpus)
frequencies
sparse = removeSparseTerms(frequencies, 0.995)
text_desc = as.data.frame(as.matrix(sparse))

descdist(NBA_train$Engagements/NBA_train$`Followers at Posting`, discrete = FALSE)

full_data <- cbind(NBA_train, text_desc)
names(full_data)


drops <- c("Description")
full_data <- full_data[ , !(names(full_data) %in% drops)]
full_data$month_of_post <- month(full_data$Created)
full_data$hour_of_post <- hour(full_data$Created)
library(fastDummies)
names(full_data)
dta <- fastDummies::dummy_cols(full_data, select_columns = c("hour_of_post", 'month_of_post', 'Type', 'day_of_week'))
summary(dta)

Video = subset(dta2, dta2$Type_Video ==1)
Album = subset(dta2, dta2$Type_Album ==1)
Photo = subset(dta2, dta2$Type_Photo ==1)

drops <- c("datetimes", 'Created', 'day_of_week',"hour_of_post", 'month_of_post', 'Type', 'day_of_week')
dta <- dta[ , !(names(dta) %in% drops)]
dta
sapply(dta$dates, class)
dta$primetime<-ifelse(dta$dates>=as.Date('2019-04-13'),"playoffs",
                    ifelse(dta$dates>=as.Date('2019-02-18') & dta$dates<as.Date('2019-04-13'),"regular",
                           ifelse(dta$dates>=as.Date('2019-02-15') & dta$dates<as.Date('2019-02-18'),"allstar",
                                  ifelse(dta$dates>=as.Date('2018-10-18') & dta$dates<as.Date('2019-02-15'),"regular",
                                         ifelse(dta$dates>=as.Date('2018-06-09') & dta$dates<as.Date('2018-10-18'),"off-season",
                                            ifelse(dta$dates>=as.Date('2018-04-14') & dta$dates<as.Date('2018-06-09'),"playoffs",
                                                   ifelse(dta$dates>=as.Date('2018-02-19') & dta$dates<as.Date('2018-04-14'),"regular",
                                                          ifelse(dta$dates>=as.Date('2018-02-16') & dta$dates<as.Date('2018-02-19'),"allstar",
                                                                 ifelse(dta$dates>=as.Date('2018-10-17') & dta$dates<as.Date('2018-02-16'),"regular","unknown"
                                                                        
                                                                 
                                                      
                                                          
                                  )))))))))

dta2 <- fastDummies::dummy_cols(dta, select_columns = c("primetime"))
dta2
yvec <- dta$Engagements

## RESTRICT TO FIRST 30 words 

drops <- c("dates", 'primetime', 'hr')
dta2 <- dta2[ , !(names(dta2) %in% drops)]
video_y_vec= Video$Engagements
album_y_vec= Album$Engagements
photo_y_vec= Photo$Engagements

Video <- Video[ , !(names(Video) %in% drops)]
Album <- Album[ , !(names(Album) %in% drops)]
Photo <- Photo[ , !(names(Photo) %in% drops)]

vid_mat<- as.matrix(Video)
alb_mat<- as.matrix(Album) 
pho_mat<- as.matrix(Photo)


nrow(yvec)

drops <- c('Engagements')
dta2 <- dta2[ , !(names(dta2) %in% drops)]
library(ggplot2)

install.packages("beeswarm")
library("beeswarm")
beeswarm(Engagements ~ Type, data = NBA_train, method="swarm")
ggplot(NBA_train$Engagements) +  geom_beeswarm(aes(x=1,y=yvec)) + ggtitle("Beeswarm") + theme(axis.title.x=element_blank(),
                                                                                 axis.text.x=element_blank(),
                                                                               axis.ticks.x=element_blank())

NBA_train
xmatrix<- as.matrix(dta2)
xmatrix
nrow(xmatrix)
colnames(xmatrix)
rownames(xmatrix)
params <- list(booster = "gbtree", eval.metric = "error", eval.metric = "logloss", eval.metric ='rmse', eta=0.2, gamma=0, max_depth=10, min_child_weight=1, subsample=1, colsample_bytree=1)

install.packages("fitdistrplus")
install.packages("logspline")
library(fitdistrplus)
library(logspline)

descdist(yvec, discrete = TRUE)
dta2$Engagements <-yvec
dumb<- lm(Engagements~. , data = dta2)
summary(dumb)


hist(yvec)
xgb.cv.results <- xgb.cv(params = params, data=pho_mat, label = photo_y_vec, verbose=T, nrounds=200, nfold=10)
best.round = c(1:length(xgb.cv.results$evaluation_log$test_rmse_mean))[xgb.cv.results$evaluation_log$test_rmse_mean==min(xgb.cv.results$evaluation_log$test_rmse_mean)]; min(xgb.cv.results$evaluation_log$test_rmse_mean); best.round
xgb.model <- xgboost(params = params, data=xmatrix, label = yvec, verbose=T, nrounds=as.numeric(best.round[1]))
importance_matrix <- xgb.importance(feature_names = colnames(xmatrix),model = xgb.model)
top10_imp_mat <- importance_matrix[1:10,]
var_import = data.frame(Feature=top10_imp_mat$Feature,Gain=top10_imp_mat$Gain)

