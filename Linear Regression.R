# Importing Data #
df <- read.csv("E:/My Dictionary/Using R/Intermediate R/House_Price.csv")
View(df)
str(df)

############################## Preprocessing Data ############################################
summary(df)
hist(df$crime_rate)
pairs(~price+crime_rate+n_hot_rooms+rainfall, data = df)
barplot(table(df$airport))
barplot(table(df$waterbody))
barplot(table(df$bus_ter))

# Interpretation : n_hot_rooms and rainfall have outliers, n_hos_beds has missing values, Crime_rate has some other functional relationship with price so we need to transform it first so that these variabels have linier relationship 

# Outlier Treatment #
summary(df$n_hot_rooms)
quantile(df$n_hot_rooms, 0.99) #P99
uv = 3*quantile(df$n_hot_rooms, 0.99) ; uv #3*P99
df$n_hot_rooms[df$n_hot_rooms > uv] <- uv #replace all values above 3*P99 with 3*P99, it is used for higher outlier
summary(df$n_hot_rooms)

summary(df$rainfall)
lv = 0.3*quantile(df$rainfall, 0.01) ; lv 
df$rainfall[df$rainfall<lv] <-lv ##replace all values above 0.3*P1 with 0.3*P1, it is used for lower outlier
summary(df$rainfall)

# Handling Missing Value #
mean(df$n_hos_beds)
mean(df$n_hos_beds,na.rm = TRUE) #without NA value
which(is.na(df$n_hos_beds)) #Mencari letak NA
df$n_hos_beds[is.na(df$n_hos_beds)] <- mean(df$n_hos_beds,na.rm = TRUE) #Imputing mean
summary(df$n_hos_beds)
which(is.na(df$n_hos_beds))

# Variable Transformation #
pairs(~price+crime_rate,data = df) #matrix plot
plot(df$price,df$crime_rate) #scatter plot
df$crime_rate=log(1+df$crime_rate) #algoritma for transformation based on plot
df$avg_dist = (df$dist1+df$dist2+df$dist3+df$dist4)/4 #dimension reduction
df2 <- df[,-7:-10] #because we have had new variable svg_dist then we remove variable dist
df <- df2
rm(df2)
df <-df[,-14] #because variabel 14 has 1 category so it is useless

# Dummy Variable #
install.packages("dummies")
library(dummies)
df <- dummy.data.frame(df)
View(df)
df <- df[,-9] #if the variable has 2 categories, there will be 1 dummy variable, because -> the number of dummy variable = the number of categories - 1
df <- df[,-14] #if the variable has 2 categories, there will be 1 dummy variable, because -> the number of dummy variable = the number of categories - 1

# Correlation matrix #
cor(df)
round(cor(df),2)
df <- df[,-16] #because variable parks is highly correlated with ai_qual, we need to remove one of the two so we can avoid multicolinearity

#################################### Linear Regression Modeling ###################################
simple_model <- lm(price~room_num, data=df) #which price is dependent variable
simple_model
summary(simple_model)
plot(df$room_num,df$price)
abline(simple_model)
#variable room_num is significantly affecting the price because pvalue < 0.05
#simple model : price = -34.6592 + 9.0997 room_num

multiple_model <- lm(price~.,data = df) #beside variable price, the other variables are independent variable
summary(multiple_model)
#there is one or more variable that affects price significant
#variable air_qual, room_num, teachers, poor_prop, airportYES, n_hos_beds, avg_dist are significantly affecting the price because pvalue < 0.05

################################## Modelling with SPLIT DATA ######################################
install.packages("caTools")
library(caTools)
set.seed(0)
split = sample.split(df,SplitRatio = 0.8) #80% training data, 20% testing data
training_set = subset(df,split == TRUE)
test_set = subset(df, split == FALSE)
lm_a = lm(price~.,data=training_set) #modelling
train_a = predict(lm_a,training_set) #predict training dataset
test_a = predict(lm_a,test_set) #predict testing dataset using training model
mean((training_set$price-train_a)^2)
mean((test_set$price-test_a)^2)

################################# Find BEST MODEL ##########################################
install.packages("leaps")
library(leaps)
lm_best = regsubsets(price~.,data = df, nvmax = 15) #variable selection using best subset
summary(lm_best)
summary(lm_best)$adjr2
which.max(summary(lm_best)$adjr2)
coef(lm_best,8)

lm_forward = regsubsets(price~.,data = df, nvmax = 15, method = "forward") #variable selection using forward selection
summary(lm_forward)
summary(lm_forward)$adjr2
which.max(summary(lm_forward)$adjr2)
coef(lm_forward,8)

lm_backward = regsubsets(price~.,data = df, nvmax = 15, method = "backward") #variable selection using backward elimination
summary(lm_backward)
summary(lm_backward)$adjr2
which.max(summary(lm_backward)$adjr2)
coef(lm_backward,8)