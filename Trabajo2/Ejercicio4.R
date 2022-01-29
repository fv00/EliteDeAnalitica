#Lectura de librerías
library(ISLR)
library(corrplot)
library(gam)

#Lectura de datos
data(Credit)

#Validacion cruzada
set.seed(123)
size = ceiling(nrow(Credit)*0.8)
training = sample(1:nrow(Credit), size)
test = which(!1:nrow(Credit) %in% training)

#Variable respuesta:
y_train <- training$Balance
y_train <- scale(y_train)

#Distribucion variable respuesta:
hist(y_train)

#Grafica de correlacion:
cor = cor(training[c(2:6, 12)])
corrplot(cor)


#Diagramas de dispersion:
pairs(training[c(2:6, 12)])

#Boxplots:
par(mfrow = c(2,2))

# boxplot(training$Balance ~ training$Gender)
boxplot(training$Balance ~ training$Education)
boxplot(training$Balance ~ training$Married)
boxplot(training$Balance ~ training$Ethnicity)
boxplot(training$Balance ~ training$Student)

#Modelamiento:
#Balance en base a las covariables sin usar splines
mod_gam1 <- gam(Balance ~ Limit + Rating +  Student,
                family = Gamma(link = "identity"),
                data = Credit,
                subset = training)
#Balance en base a las covariables usasndo splines en una covariable
mod_gam2 <- gam(Balance ~ s(Limit) + Rating + Student,
                family = Gamma(link = "identity"),
                data = Credit,
                subset = training)
#Balance en base a las covariables usando loess
mod_gam3 <- gam(Balance ~ limit + lo(Rating),
                family = Gamma(link = "identity"),
                data = Credit,
                subset = training)
#Balance en base a las covariables usando loess y splines
mod_gam4 <- gam(Balance ~ s(Limit) + lo(Rating),
                family = Gamma(link = "identity"),
                data = Credit,
                subset = training)

#Familia de modelos gamma   