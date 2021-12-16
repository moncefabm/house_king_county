#Librerias
library(MASS)
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
library(emmeans)
library(stargazer)


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
densityplot(log(train$price_m2))

#Tranfomación de variables

train$waterfront = as.factor(train$waterfront)
train$view = as.factor(train$view)
train$zipcode = as.factor(train$zipcode)


#Creamos una variable que mide la distnacia de cada casa a la venta con respecto al centro ecónomico de la ciudad: Seattle
seattle = c(-122.335167, 47.608013)
train = mutate(train, Dist_from_seattle=distHaversine(cbind(long, lat),seattle))

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




#specify_decimal
specify_decimal <- function(x, k) format(round(x, k), nsmall=k)

#beautifying summary.lm
new_summary  <- function(lmcoef, digits) {
  
  coefs <- as.data.frame(lmcoef)
  coefs[] <- lapply(coefs, function(x) specify_decimal(x, digits))
  coefs
  
}


write_csv(train, file = "C:/Users/Moncef/Documents/house_king_county/train.csv")

#Convertimos las variables ctageoricas en dummies para su uso en el modelo.

train_v1<- dummy_cols(train, select_columns = c('waterfront','view','Clusters','sale_quarter'),remove_most_frequent_dummy = TRUE,
                   remove_selected_columns = TRUE)

train_v1$sqft_lot_log = log(train_v1$sqft_lot)
train_v1$sqft_above_log = log(train_v1$sqft_above)
train_v1$sqft_living_log = log(train_v1$sqft_living)
train_v1$Dist_from_seattle_log = log(train_v1$Dist_from_seattle)
#train_v1$yr_renovated_log = log(train_v1$yr_renovated)
#train_v1$sale_tenure_log = log(train_v1$sale_tenure)


#Incluimos en c() las variables que NO queremos usar en la regresión.
train_v2 = dplyr::select(train_v1,-c(id,sale_tenure,yr_renovated,sqft_above,sqft_lot,sale_tenure,yr_renovated,sqft_living,Dist_from_seattle,price,date,zipcode,sqft_basement,lat,sqft_lot15,long,date,sale_date,
                              sale_year,yr_built,sale_month,yr_since_last_renovated,sqft_living15))

summarize(train_v2)

model <- lm(log(price_m2) ~ ., data = train_v2)
summary(model)
new_summary(summary(model)$coefficients, 5)

plot(model, 1)
res <- resid(model)
plot(fitted(model), res)
plot(model, which=1, col=c("blue"))
plot(model, 2)

require(nortest)  # Se debe haber instalado nortest
lillie.test(model$residuals)



#Analisis de variables candidatas 
step_model <- stepAIC(model, trace = TRUE, direction= "both")
stargazer(model, step_model, type = "text")


#create Q-Q plot for residuals
qqnorm(res)

#add a straight diagonal line to the plot
qqline(res) 

#Add
plot(density(res))



sigma(model)*100/mean(log(train_v2$price_m2))



#LASSO

train_v2_nona <- na.omit(train_v2)

x <- model.matrix(log(train_v2_nona$price_m2)~., train_v2_nona )[,-1]
y <- log(train_v2_nona$price_m2)


#######
data_lasso <- train_v2
lambdas <- model.matrix(log(price_m2) ~., data = data_lasso)
y <- log(train_v2$price_m2)

# Funciones de error por variable
models_lasso <- glmnet(x = lambdas, y = y, alpha = 1)
plot(models_lasso, xvar = "lambda", label = TRUE)


set.seed(737)

# Ajuste de la función de error
cv_lasso <- cv.glmnet(x = lambdas, y = y, alpha = 1)
plot(cv_lasso)


out_eleven <- glmnet(lambdas,y,alpha=1,lambda = cv_lasso$lambda.1se)
out_eleven
lasso_coef_eleven <- predict(out_eleven, type="coefficients")[1:27,]
lasso_coef_eleven
cv_lasso

#Incluimos en c() las variables que NO queremos usar en la regresión.


model_lasso <- lm(log(price_m2) ~ bedrooms + bathrooms + floors + condition
                  + grade + waterfront_1 + view_1 + view_2 + view_3 + view_4 + Clusters_1 + Clusters_3 
                  + Clusters_4+ Clusters_5 + Clusters_6 + Clusters_7  + Clusters_8 
                  + Clusters_9 + Clusters_10 + Clusters_11 + Clusters_12 + Clusters_13
                  + sale_quarter_Q1 + sale_quarter_Q3 +sale_quarter_Q4, data = train_v2)
summary(model_lasso)
new_summary(summary(  model_lasso)$coefficients, 5)




residuals =  model_lasso$residuals
autoplot(model_lasso)
abline(model_lasso <- lm(log(price_m2) ~ bedrooms + bathrooms + floors + condition
                         + grade + waterfront_1 + view_1 + view_2 + view_3 + view_4 + Clusters_1 + Clusters_3 
                         + Clusters_4+ Clusters_5 + Clusters_6 + Clusters_7  + Clusters_8 
                         + Clusters_9 + Clusters_10 + Clusters_11, Clusters_12, Clusters_13
                         + sale_quarter_Q1 + sale_quarter_Q3 +sale_quarter_Q4, data = train_v2))


