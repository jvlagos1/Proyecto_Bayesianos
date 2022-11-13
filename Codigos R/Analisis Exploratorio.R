library(readr)

########### Braemar Hotels & Resorts ##############

BHR <- read_csv("C:/Users/56979/Desktop/uc/Metodos Bayesianos II/Proyecto/BHR.csv")
View(B_19)
dim(B_19)
B =as.matrix(BHR)
B_19 = B[1:252,]
B_20 = B[253:315,]

## Retorno 2019

retorno_bhr.19 = vector(mode="numeric", length = dim(B_19)[1]-1)
for (i in 1:251) {
  retorno_bhr.19[i] = (as.numeric(B_19[i+1,2])-as.numeric(B_19[i,2]))/(as.numeric(B_19[i,2]))
  
}
plot(retorno_bhr.19,type = "l")

## Retorno 2020

retorno_bhr.20 = vector(mode="numeric", length = dim(B_20)[1]-1)
for (i in 1:62) {
  retorno_bhr.20[i] = as.numeric(B_20[i+1,2])-as.numeric(B_20[i,2])
  
}
plot(retorno_bhr.20,type = "l")

################## Delta Airlines ##############

delta <- read_csv("C:/Users/56979/Desktop/uc/Metodos Bayesianos II/Proyecto/Delta Airlines.csv")
View(delta)
dim(Del)
Del = as.matrix(delta)
del.19 = Del[1:252,]
del.20 = Del[253:315,]


## Retorno 2019 

retorno_del.19 = vector(mode="numeric", length = dim(del.19)[1]-1)
for (i in 1:251) {
  retorno_del.19[i] = as.numeric(del.19[i+1,2])-as.numeric(del.19[i,2])
  
}
plot(retorno_del.19,type = "l")

## Retorno 2020

retorno_del.20 = vector(mode="numeric", length = dim(del.20)[1]-1)
for (i in 1:61) {
  retorno_del.20[i] = as.numeric(del.20[i+1,2])-as.numeric(del.20[i,2])
  
}
plot(retorno_del.20,type = "l")

################# Caesars Casino ###############

caesars <- read_csv("C:/Users/56979/Desktop/uc/Metodos Bayesianos II/Proyecto/Caesars casino.csv")
caes = as.matrix(caesars)
View(caes)
caes.19 = caes[1:252,]
caes.20 = caes[253:315,]

## Retorno 2019

retorno_caes.19 = vector(mode="numeric", length = dim(caes.19)[1]-1)

for (i in 1:251) {
  retorno_caes.19[i] = as.numeric(caes.19[i+1,2])-as.numeric(caes.19[i,2])
  
}
plot(retorno_caes.19,type = "l")

## Retorno 2020

retorno_caes.20 = vector(mode="numeric", length = dim(caes.20)[1]-1)

for (i in 1:61) {
  retorno_caes.20[i] = as.numeric(caes.20[i+1,2])-as.numeric(caes.20[i,2])
  
}
plot(retorno_caes.20,type = "l")

################## ARCA biopharma ######################

ABIO <- read_csv("C:/Users/56979/Desktop/uc/Metodos Bayesianos II/Proyecto/ABIO.csv")
View(ABIO)
bio = as.matrix(ABIO)
dim(bio)
bio.19 = bio[1:252,]
bio.20 = bio[253:315,]

## Retorno 2019

retorno_bio.19 = vector(mode="numeric", length = dim(bio.19)[1]-1)

for (i in 1:251) {
  retorno_bio.19[i] = as.numeric(bio.19[i+1,2])-as.numeric(bio.19[i,2])
  
}
plot(retorno_bio.19,type = "l")

## Retorno 2020

retorno_bio.20 = vector(mode="numeric", length = dim(bio.20)[1]-1)

for (i in 1:61) {
  retorno_bio.20[i] = as.numeric(bio.20[i+1,2])-as.numeric(bio.20[i,2])
  
}

plot(retorno_bio.20,type = "l")

###### Precio Accion 

plot(log(CZR$Close), type="l", col="red", ylim = c(0,5.2), lwd=2, main="Precio Acciones en Bolsa",
     ylab = "Log($)", xlab="")
lines(log(ABIO$Close), type = "l", col="green",lwd=2)
lines(log(BHR$Close),type = "l", lwd=2)
lines(log(Delta$Close), type="l", col="blue", lwd=2)
legend(x="topright", legend = c("Caesars Palace", "Abio","BHR","Delta"),
       col = c("red","green","black","blue"),pch=16, cex=0.8)
plot(log(AMZN$Close), type = "l")


