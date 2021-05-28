
setwd("/Users/Utsav/Desktop/SDM")

library(readxl)
# Reading file in a dataframe
nba_f <- read_excel("Power_Ranking_Final_Data.xlsx")
attach(nba_f)

# Making week as a factor
# nba_f$Week <- as.factor(nba_f$Week)

# Checking correlations of variables 
m <- cor(cbind(nba_f[, 2:9]))
# Correlation Matrix
library(corrplot)
corrplot(m, method="number")

# Plotting boxplot

boxplot(PR~Team,data=nba_f, main="Power Ranking Distribution",col=(c("gold","darkgreen")),
        xlab="NBA Teams", ylab="Power Ranking")

boxplot(OR~Team,data=nba_f, main="Offensive Rating Distribution",col=(c("red","blue")),
        xlab="NBA Teams", ylab="Offensive Rating")

boxplot(DR~Team,data=nba_f, main="Defensive Rating Distribution",col=(c("green","navyblue")),
        xlab="NBA Teams", ylab="Defensive Rating")

# Creating a win% column
nba_f$winp= round((nba_f$W/(nba_f$W + nba_f$L))*100,1)

# Creating sequence to make lags
n <- nrow(nba_f)

nba_f$PRLag <- c(NA, nba_f$PR[1:n-1])
nba_f$SOS1Lag <- c(NA,nba_f$SOS1[1:n-1])
  
# Linear model
ols_base <- lm(PR ~ OR+DR+PRLag+SOS1, data = nba_f)
summary(ols_base)
plot(ols_base)
# The residual plot shows that the model is violating linearity and 
# independence assumptions. The residual plot is also heteroskedastic.


library(plm)
# Panel Data Analysis
# Fixed Effect Model With Lag
fixed_1 <- plm(PR ~ OR + DR + PRLag + SOS1, data=nba_f, 
              index=c("Team","Week"), model="within")
summary(fixed_1)
df_nba<- fixef(fixed_1)

# Checking assumptions for auto correlation and heteroskedasticity

library(lmtest)
pdwtest(fixed_1)

# Checking for 
pbgtest(fixed_1)

# The test shows that our model has auto correlation and heteroskedasticity.
# We will have to correct for these in order to do a better prediction.


# Random Effect Model with Lag and SOS

random_1 <- plm(PR ~ OR + DR + PRLag + SOS1, data=nba_f, 
               index=c("Team"), model="random")
summary(random_1)
df_nba_rn<- ranef(random_1)

View(df_nba_rn)
library(stargazer)

stargazer(ols_base,fixed_1,random_1,type = "text")

# Hausman Test
phtest(fixed_1, random_1) # Null hypothesis is random effects model is consistent

# but since p-value is less than 0.05, we can reject the null hypothesis
# and say that fixed effects model is better.








