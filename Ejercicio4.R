library(ISLR)
data(Credit)

#https://cran.r-project.org/web/packages/ISLR/ISLR.pdf
Credit <- subset(Credit, select = -c(ID))

attach(Credit)

# Definicion Balance:
#--The balance in your bank account is the amount of money you have in it.
#--Cantidad de dinero que tiene una persona en su cuenta bancaria

## Preprocesamiento basico
Cards <- as.factor(Cards)
Education <- as.factor(Education)

## Varibles categoricas
#plot(Gender, Balance)
plot(Student, Balance) # Se observa desbalanceo en los datos
#plot(Married, Balance)
#plot(Ethnicity, Balance)
#plot(Education, Balance)

barplot(Cards)
barplot(Education)

## Variables numericas
plot(Income, Balance)
plot(Limit, Balance) # Relacion obvia
plot(Rating, Balance)
plot(Age)

