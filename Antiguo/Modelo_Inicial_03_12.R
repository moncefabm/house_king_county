#Librerias
library(tidyr)
library(date)
library(zoo)
library(tidyverse)#--> para funciones de fechas#
library(ggplot2)
library(corrplot)
library(caret)
library(dplyr)
library(olsrr)
library(fastDummies)
library(pracma)
library(geosphere)
library(readxl)
library(glmnet)

#Cargamos los datos y definimos una muestra train y otra test
data = as.data.frame(read_excel("~/house_king_county/kc_house_data_vfinal.xlsx"))

dt = sort(sample(nrow(data), nrow(data)*.7))
train<-data[dt,]
test<-data[-dt,]

#Creamos algunas variables a partir de otras en la BBDD

train$sale_date = substr(train$date, 1, 8)
train = transform(train, sale_date = as.Date(as.character(sale_date), "%Y%m%d"))
train$sale_year = as.numeric(format(train$sale_date, format="%Y"))
train$sale_month = format(train$sale_date, format="%m")
train$sale_quarter = as.yearqtr(train$sale_date)
train$sale_quarter = str_sub(train$sale_quarter,-2,-1)
train$sale_tenure =  as.numeric(train$sale_year) - train$yr_built
train$yr_since_last_renovated = ifelse(train$sale_year - train$yr_renovated > 2013, 0, train$sale_year - train$yr_renovated)
train$price_m2 = train$price/train$sqft_living

#Tranfomación de variables

train$waterfront = as.factor(train$waterfront)
train$view = as.factor(train$view)
train$zipcode = as.factor(train$zipcode)


#Creamos una variable que mide la distnacia de cada casa a la venta con respecto al centro ecónomico de la ciudad: Seattle
seattle = c(-122.335167, 47.608013)
train = mutate(train, Dist_from_seattle=distHaversine(cbind(long, lat),seattle))


s
#POr otro lado, se calcula la distancia de cada casa con respecto al resto, agrupando después casas cercanas en clústers.
long= train$long
lat = train$lat
DataMat<-as.matrix(cbind(long, lat))
DataMat[is.na(DataMat)]<-0

my_data <- as_tibble(DataMat)
#my_data=my_data %>% slice(1:100)
my_data = as.data.frame(my_data)

Dist_Mat<-distm(my_data,my_data,fun=distHaversine)/1000
hclustfunc <- function(x) hclust(x, method="complete")
distfunc <- function(x) as.dist((1-cor(t(x)))/2)
d <- distfunc(Dist_Mat)
fit <- hclustfunc(d)
my_data$Clusters<- cutree(fit, h=0.25)
train = merge(x = train, y = my_data, by = c("lat", "long") , all.x = TRUE)
train$Clusters = as.factor(train$Clusters)
bwplot(Clusters~price, data = train, horizontal= TRUE)


#Convertimos las variables ctageoricas en dummies para su uso en el modelo.

train<- dummy_cols(train, select_columns = c('sale_quarter','waterfront','view','Clusters'),remove_most_frequent_dummy = TRUE,
                   remove_selected_columns = TRUE)


#Incluimos en c() las variables que NO queremos usar en la regresión.
train_v2 = select(train,-c(id,price,date,zipcode,sqft_basement,lat,sqft_lot15,long,date,sale_date,
                           sale_year,yr_built, yr_renovated,sale_month,
                           yr_since_last_renovated))


model <- lm(log(price_m2) ~. -sqft_living15, data = train_v2)
summary(model)

res <- resid(model)
plot(fitted(model), res)

#create Q-Q plot for residuals
qqnorm(res)

#add a straight diagonal line to the plot
qqline(res) 

#Add
plot(density(res))



#LASSO

train_v2_nona <- na.omit(train)

x <- model.matrix(price~., train)[,-1]
y <- train_v2_nona$price

grid <- 10^seq(10,-2,length=100)

lasso_reg = glmnet(x, y, alpha = 1, lambda = grid)
plot(lasso_reg)

set.seed(1234)
cv_result <- cv.glmnet(x,y,alpha=1)
plot(cv_result)


best_lam <- cv_result$lambda.min
out <- glmnet(x,y,alpha=1,lambda = best_lam)
lasso_coef <- predict(out,type="coefficients",s=best_lam)[1:20,]
lasso_coef[lasso_coef!=0]


first_eight <- max(which(cv_result$nzero == 6))
my_lam <- cv_result$lambda[first_eight]
out_eight <- glmnet(x,y,alpha=1,lambda = first_eight)
lasso_coef_eight <- predict(out_eight,type="coefficients")[1:20,]
lasso_coef_eight[lasso_coef_eight!=0]


