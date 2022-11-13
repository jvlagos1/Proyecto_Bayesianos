##### Creacion tablas  

######### GDP ###########

gdp = as.data.frame(Table_1_[6:33,])
colnames(gdp)=gdp[1,]
gdp = gdp[-1,]
gdp.us = gdp[2,]

nose=seq(g[1],g[1],length=3)
for (j in 2:length(g)) {
  m = seq(g[j],g[j],length=3)
  nose=c(nose,m)
  
}

gdp.us=as.vector(gdp[2,3:33])
colnames(gdp.us)=NULL
g =as.vector(as.numeric(gdp.us))
gdp_us=as.vector(as.numeric(gdp_us))
gdp_us[1:100]
V1<-seq(as.Date("15-01-01", format=c("%y-%m-%d")), as.Date("22-07-01", format=c("%y-%m-%d")), by="3 month")
gd=data.frame(as.Date(V1, format="%y-%m-%d"),g)
colnames(gd)=c("Date","GDP")
gdp_us=vector(mode="numeric",length = length(Coke_ajustado$Date))
gdp_us[1]=gd$GDP[1]


k=2
for (i in 2:length(Coke_ajustado$Date)) {
  if(format(as.Date(Coke_ajustado$Date, format="%d/%m/%Y"),"%m")[i]==
     format(as.Date(Coke_ajustado$Date, format="%d/%m/%Y"),"%m")[i-1]) {
    gdp_us[i]=gdp_us[i-1]
  }
  else{
    gdp_us[i]=nose[k]
    k=k+1
  }
}

######## GNI #########

library(readr)
api <- read_csv("C:/Users/56979/Desktop/uc/Metodos Bayesianos II/Proyecto/API_NY.GNP.PCAP.KD.ZG_DS2_en_csv_v2_4676257.csv")
View(api)
wrld.gni = as.vector(as.numeric(api[264,60:66]))
us.gni = as.vector(as.numeric(api[256,60:66]))
plot(x=fec, y=us.gni, type="l", col="red",lwd=2)
lines(x=fec,y=wrld.gni,col="blue",lwd=2)
fec = seq(2015,2021,1)

### Stocks traded #######

stock.trade <- read_csv("C:/Users/56979/Desktop/uc/Metodos Bayesianos II/Proyecto/API_CM.MKT.TRAD.GD.ZS_DS2_en_csv_v2_4530698.csv")
View(stock.trade)
stock.trade_wrld = as.vector(as.numeric(stock.trade[264,60:66]))
stock.trade_us = as.vector(as.numeric(stock.trade[256,60:66]))

######### Broad Money Growth #######

broad.money <- read_csv("C:/Users/56979/Desktop/uc/Metodos Bayesianos II/Proyecto/API_FM.LBL.BMNY.ZG_DS2_en_csv_v2_4538248.csv")
broad.money.us = as.vector(as.numeric(broad.money[256,60:66]))


####### Coca cola ###########

Coke_ajustado$Date <- as.Date(Coke_ajustado$Date, format =c("%d-%m-%y"))
dif.precio.coke = vector(mode="numeric",length = length(Coke_ajustado$Date))

for (i in 2:length(Coke_ajustado$Date)) {
  dif.precio.coke[i]=((Coke_ajustado$Close[i]-Coke_ajustado$Close[i-1])/Coke_ajustado$Close[i-1])*100
}

cc =cbind(Coke_ajustado,gdp_us, dif.precio.coke)
colnames(cc) = c("Fecha","Precio Cierre","Volumen","Inflacion Acumulada","Precio Ajustado","PIB US",
                 "Retorno")

####### Delta Airlines ##########

Delta_ajustado$Date = as.Date(Delta_ajustado$Date, format =c("%d-%m-%y"))
dif.precio.delta =  vector(mode="numeric",length = length(Delta_ajustado$Date))

for (i in 2:length(Delta_ajustado$Date)) {
  dif.precio.delta[i]=((Delta_ajustado$Close[i]-Delta_ajustado$Close[i-1])/Delta_ajustado$Close[i-1])*100
}

dd =cbind(Delta_ajustado,gdp_us, dif.precio.delta)
colnames(dd) = c("Fecha","Precio Cierre","Volumen","Inflacion Acumulada","Precio Ajustado","PIB US",
                 "Retorno")

######### Amazon ##########

Amazon_ajustado$Date = as.Date(Amazon_ajustado$Date, format =c("%d-%m-%y"))
dif.precio.amzn =  vector(mode="numeric",length = length(Amazon_ajustado$Date))

for (i in 2:length(Amazon_ajustado$Date)) {
  dif.precio.amzn[i]=((Amazon_ajustado$Close[i]-Amazon_ajustado$Close[i-1])/Amazon_ajustado$Close[i-1])*100
}

aa =cbind(Amazon_ajustado,gdp_us, dif.precio.amzn)
colnames(aa) = c("Fecha","Precio Cierre","Volumen","Inflacion Acumulada","Precio Ajustado","PIB US",
                 "Retorno")

########## ABIO Pharma ########

ABIO_ajustado$Date = as.Date(ABIO_ajustado$Date, format =c("%d-%m-%y"))
dif.precio.abio =vector(mode="numeric",length = length(ABIO_ajustado$Date))

for (i in 2:length(ABIO_ajustado$Date)) {
  dif.precio.abio[i]=((ABIO_ajustado$Close[i]-ABIO_ajustado$Close[i-1])/ABIO_ajustado$Close[i-1])*100
}

abab =cbind(ABIO_ajustado,gdp_us, dif.precio.abio)
colnames(abab) = c("Fecha","Precio Cierre","Inflacion Acumulada","Precio Ajustado","PIB US",
                 "Retorno")

############ BH Resorts & Hotels #############

BHR_ajustado$Date = as.Date(BHR_ajustado$Date, format =c("%d-%m-%y"))
dif.precio.bhr =vector(mode="numeric",length = length(BHR_ajustado$Date))

for (i in 2:length(BHR_ajustado$Date)) {
  dif.precio.bhr[i]=((BHR_ajustado$Close[i]-BHR_ajustado$Close[i-1])/BHR_ajustado$Close[i-1])*100
}

bb =cbind(BHR_ajustado,gdp_us, dif.precio.bhr)
colnames(bb) = c("Fecha","Precio Cierre","Inflacion Acumulada","Precio Ajustado","PIB US",
                   "Retorno")

####### Caesars Casinos ############3

CZR_ajustado$Date = as.Date(CZR_ajustado$Date, format =c("%d-%m-%y"))
dif.precio.czr =vector(mode="numeric",length = length(CZR_ajustado$Date))

for (i in 2:length(CZR_ajustado$Date)) {
  dif.precio.czr[i]=((CZR_ajustado$Close[i]-CZR_ajustado$Close[i-1])/CZR_ajustado$Close[i-1])*100
}

czcz =cbind(CZR_ajustado,gdp_us, dif.precio.czr)
colnames(czcz) = c("Fecha","Precio Cierre","Volumen","Inflacion Acumulada","Precio Ajustado","PIB US",
                 "Retorno")

####### Colgate-Palmolive

infl = CZR_ajustado$inflacion
Colgate_Palmolive <- read_csv("C:/Users/56979/Desktop/uc/Metodos Bayesianos II/Proyecto/Colgate-Palmolive.csv")
col_pal = cbind(Colgate_Palmolive,infl)
View(col_pal)
precio.aj.Col =col_pal$Close*(1+col_pal$infl)
Colg_Palm_ajustado = cbind(col_pal,precio.aj.Col)

Colg_Palm_ajustado$Date = as.Date(Colg_Palm_ajustado$Date, format =c("%d-%m-%y"))
dif.precio.col =vector(mode="numeric",length = length(Colg_Palm_ajustado$Date))

for (i in 2:length(Colg_Palm_ajustado$Date)) {
  dif.precio.col[i]=((Colg_Palm_ajustado$Close[i]-Colg_Palm_ajustado$Close[i-1])/Colg_Palm_ajustado$Close[i-1])*100
}

cp =cbind(Colg_Palm_ajustado,gdp_us, dif.precio.col)
colnames(cp) = c("Fecha","Precio Cierre","Volumen","Inflacion Acumulada","Precio Ajustado","PIB US",
                   "Retorno")


