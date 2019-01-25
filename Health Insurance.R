install.packages("Rcpp")
install.packages('Rcpp', dependencies=TRUE, repos='http://cran.rstudio.com/')
install.packages("dplyr",dependencies=TRUE,lib= "/opt/microsoft/mlserver/9.2.1/libraries/RServer")

install.packages("magrittr",dependencies=TRUE,lib= "https://www.rdocumentation.org/packages/magrittr")

install.packages("dplyr")
#importing dataSset
myDataSource <- read.csv(file.choose(),header = T)

#to get number of users and number of classes" variables"

#to view first 3 rows of datset
head(myDataSource,2)

#characteristics of the data
str(myDataSource)

#summary statisitic about whole dataset
summary(myDataSource)
cor(myDataSource)

#discover categories in column and to know how many categories we have in state
categories <- unique(myDataSource$X_STATE) 
numberOfCategories <- length(categories)
categories

#count how many records in each category in state
length(which(myDataSource$STATE == "9"))
length(which(myDataSource$STATE == "34"))
length(which(myDataSource$STATE == "36"))


#discover categories in column and to know how many categories we have in PHYSHLTH
categoriesPHYSHLTH <- unique(myDataSource$PHYSHLTH) 
numberOfCategoriesPHYSHLTH <- length(categoriesPHYSHLTH)
categoriesPHYSHLTH

#subset from dataset contains data from last month only
library(dplyr)
library(magrittr)

myNewDataSource1 <- subset(myDataSource, PHYSHLTH>1 & PHYSHLTH<31,  select= STATE:PHYSHLTH)
View(myNewDataSource1)
newdata <- myNewDataSource1[order(myNewDataSource1$PHYSHLTH),] 
View(newdata)


#each state with number of bad phyiscal days
library(dplyr)
library(magrittr)
State9Group <- myNewDataSource1 %>% filter(STATE ==9 )
State34Group <- myNewDataSource1 %>% filter(STATE ==34 )
State36Group <- myNewDataSource1 %>% filter(STATE ==36 )

#each state with average number of bad phyiscal days
library(dplyr)
library(magrittr)

State9Group %>%
  group_by(STATE) %>%
  summarize(mean_PHYSHLTH = mean(PHYSHLTH, na.rm = TRUE))

State34Group %>%
  group_by(STATE) %>%
  summarize(mean_PHYSHLTH = mean(PHYSHLTH, na.rm = TRUE))

State36Group %>%
  group_by(STATE) %>%
  summarize(mean_PHYSHLTH = mean(PHYSHLTH, na.rm = TRUE))



#Starting with normal distrbution for State 9

State9StandardDeviation =10.98881
State9Mean =11.8
X= 1.5

#Draw the normal distribution
range =seq(State9Mean-4*State9StandardDeviation, State9Mean+4*State9StandardDeviation,0.01)
y=dnorm(range,State9Mean,State9StandardDeviation)
#plot(dnorm, from = -4, to = 4)
plot(range,y,main =, type='l',ylim=c(0,max(y)+0.01),axes=TRUE)
axis(1,at=seq(State9Mean-3*State9StandardDeviation,State9Mean+3*State9StandardDeviation,State9StandardDeviation))

#add area to the left of X
cord.a= c(0,seq(min(range),X,0.01),X)
cord.b= c(0,dnorm(seq(min(range),X,0.01),State9Mean,State9StandardDeviation),0)
polygon(cord.a,cord.b,col="blue")

#Add area to the right of x
#cord.c=c(X,seq(X,max(range),0.01),cord.b)
#cord.d=c(0,dnorm(seq(X,max(range),0.01),State9Mean,State9StandardDeviation),0)
#polygon(cord.c,cord.d,col=grey(0.90))

#p(X<x) area to the left of x
pnorm(X,State9Mean,State9StandardDeviation)

#p(X>x) area to the left of x
1-pnorm(X,State9Mean,State9StandardDeviation)
pnorm(3,11.8,10.98881)-pnorm(1.5,11.8,10.98881)

#90th percentile
qnorm(0.9,State9Mean,State9StandardDeviation)

sd(State36Group$PHYSHLTH)

#Starting with normal distrbution for State 34

State34StandardDeviation =11.17737
State34Mean =13.5
X= 1.5
#Draw the normal distribution
range =seq(State34Mean-4*State34StandardDeviation, State34Mean+4*State34StandardDeviation,0.01)
y=dnorm(range,State34Mean,State34StandardDeviation)
#plot(dnorm, from = -4, to = 4)
plot(range,y,main =, type='l',ylim=c(0,max(y)+0.01),axes=TRUE)
axis(1,at=seq(State34Mean-3*State34StandardDeviation,State34Mean+3*State34StandardDeviation,State34StandardDeviation))

#add area to the left of X
cord.a= c(0,seq(min(range),X,0.01),X)
cord.b= c(0,dnorm(seq(min(range),X,0.01),State34Mean,State34StandardDeviation),0)
polygon(cord.a,cord.b,col="blue")


#p(X<x) area to the left of x
pnorm(X,State34Mean,State34StandardDeviation)

#p(X>x) area to the left of x
1-pnorm(X,State34Mean,State34StandardDeviation)
prnom(3,13.5,11.17737)-prnom(1.5,13.5,11.17737)

#90th percentile
qnorm(0.9,State34Mean,State34StandardDeviation)

###############################################
#Starting with normal distrbution for State 36#
###############################################
State36StandardDeviation =10.82889
State36Mean =12.3
X= 1.5
#Draw the normal distribution
range =seq(State36Mean-4*State36StandardDeviation, State36Mean+4*State36StandardDeviation,0.01)
y=dnorm(range,State36Mean,State36StandardDeviation)
#plot(dnorm, from = -4, to = 4)
plot(range,y,main =, type='l',ylim=c(0,max(y)+0.01),axes=TRUE)
axis(1,at=seq(State36Mean-3*State36StandardDeviation,State36Mean+3*State36StandardDeviation,State36StandardDeviation))

#add area to the left of X
cord.a= c(0,seq(min(range),X,0.01),X)
cord.b= c(0,dnorm(seq(min(range),X,0.01),State36Mean,State36StandardDeviation),0)
polygon(cord.a,cord.b,col="blue")

#p(X<x) area to the left of x
pnorm(X,State36Mean,State36StandardDeviation)

#p(X>x) area to the left of x
1-pnorm(X,State36Mean,State36StandardDeviation)
pnorm(3,12.3,10.82889)-prnom(1.5,12.3,10.82889)

#90th percentile
qnorm(0.9,State36Mean,State36StandardDeviation)
names(myDataSource)

###################################################################
###################################################################
#to determine whether demographic, behaviors, and previous illness
#have impact/dependant of general health or not. The result will be
# introduced using Cross Tabulation and Chi Square test
###################################################################
###################################################################


#checking employee current status and its impact of general health "Demographic"

#find the sum and the percentage of missings in employee column
sum(is.na(myDataSource$EMPLOY1))
mean(is.na(myDataSource$EMPLOY1))

# function to remove the missings

df2 <- subset(myDataSource, select = c( GENHLTH, EMPLOY1))
df2New <- na.omit(df2)
View(df2New)

#create contingency table
df2New.tab <- table(df2New$GENHLTH,df2New$EMPLOY1)
df2New.tab
#can get marginal frequencies
margin.table(df2New.tab,1) #row marginal frequencies
margin.table(df2New.tab,2)#column marginal frequencies
#call also get cell, row, and column %
#with rounding to get just 2 decimal places
round(prop.table(df2New.tab),2) #cell%
round(prop.table(df2New.tab,1),2)#row%
round(prop.table(df2New.tab,2),2)#column%

#chi squared test
chisq.test(df2New.tab,correct = FALSE)

############################################################
############################################################


#checking person physical activities and its impact of general health "Behaviors"

#find the sum and the percentage of missings in employee column
sum(is.na(myDataSource$EXERANY2))
mean(is.na(myDataSource$EXERANY2))

# function to remove the missings

df3 <- subset(myDataSource, select = c( GENHLTH, EXERANY2))
df3New <- na.omit(df3)
View(df3New)

#create contingency table
df3New.tab <- table(df3New$GENHLTH,df3New$EXERANY2)
df3New.tab
#can get marginal frequencies
margin.table(df3New.tab,1) #row marginal frequencies
margin.table(df3New.tab,2)#column marginal frequencies
#call also get cell, row, and column %
#with rounding to get just 2 decimal places
round(prop.table(df3New.tab),2) #cell%
round(prop.table(df3New.tab,1),2)#row%
round(prop.table(df3New.tab,2),2)#column%

#chi squared test
chisq.test(df3New.tab,correct = FALSE)

##########################################################
##########################################################

#checking person didnot take medication because of cost during
# last 12 months and its impact of general health "Previous illness"


#find the sum and the percentage of missings in MEDSCOST column
sum(is.na(myDataSource$MEDSCOST))
mean(is.na(myDataSource$MEDSCOST))


# function to remove the missings
df4 <- subset(myDataSource, select = c( GENHLTH,MEDSCOST))
df4New <- na.omit(df4)
View(df4New)

#create contingency table
df4New.tab <- table(df4New$GENHLTH,df4New$MEDSCOST)
df4New.tab
#can get marginal frequencies
margin.table(df4New.tab,1) #row marginal frequencies
margin.table(df4New.tab,2)#column marginal frequencies
#call also get cell, row, and column %
#with rounding to get just 2 decimal places
round(prop.table(df4New.tab),2) #cell%
round(prop.table(df4New.tab,1),2)#row%
round(prop.table(df4New.tab,2),2)#column%

#chi squared test
chisq.test(df4New.tab,correct = FALSE)




