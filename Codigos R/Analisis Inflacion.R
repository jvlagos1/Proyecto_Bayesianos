
################# Arreglo Inflacion #############

########## Inflacion EE.UU

infl = c(-0.1,0.0,-0.1,-0.2,0.0,0.1,0.2,0.2,0.0,0.2,0.5,0.7,
         1.4,1.0,0.9,1.1,1.0,1.0,0.8,1.1,1.5,1.6,1.7,2.1,
         2.5,2.7,2.4,2.2,1.9,1.6,1.7,1.9,2.2,2.0,2.2,2.1,
         2.1,2.2,2.4,2.5,2.8,2.9,2.9,2.7,2.3,2.5,2.2,1.9,
         1.6,1.5,1.9,2.0,1.8,1.6,1.8,1.7,1.7,1.8,2.1,2.3,
         2.5,2.3,1.5,0.3,0.1,0.6,1.0,1.3,1.4,1.2,1.2,1.4)


a15=seq(from=2015,to=2015,length=12)
a16=seq(from=2016,to=2016,length=12)
a17=seq(from=2017,to=2017,length=12)
a18=seq(from=2018,to=2018,length=12)
a19=seq(from=2019,to=2019,length=12)
a20=seq(from=2020,to=2020,length=12)
anios =c(a15,a16,a17,a18,a19,a20)
mesess = seq(1:12)
meses1 = c(mesess,mesess,mesess,mesess,mesess,mesess)

dat= data.frame(anios,meses1)
fecha =paste(dat$anios,dat$meses1,sep="-")
inflacion =data.frame(cbind(fecha,infl)) 


##### Inflacion acumulada #######

infl.ac = vector(mode = "numeric", length = 72)
for (i in 1:72) {
  infl.ac[i]=sum(infl[1:i])/100
  
}

inflacion$infl=as.numeric(infl.ac)
plot(inflacion$infl, type="l")

### inflacion 2015 

en.15=cbind(CZR[1:20,],inflacion$infl[1])
colnames(en.15)[4]="inflacion"
feb.15=cbind(CZR[21:39,],inflacion$infl[2])
colnames(feb.15)[4]="inflacion"
mar.15 = cbind(CZR[40:61,],inflacion$infl[3])
colnames(mar.15)[4]="inflacion"
abr.15=cbind(CZR[62:82,],inflacion$infl[4])
colnames(abr.15)[4]="inflacion"
may.15 = cbind(CZR[83:102,],inflacion$infl[5])
colnames(may.15)[4]="inflacion"
jun.15=cbind(CZR[103:124,],inflacion$infl[6])
colnames(jun.15)[4]="inflacion"
jul.15=cbind(CZR[125:146,],inflacion$infl[7])
colnames(jul.15)[4]="inflacion"
ag.15=cbind(CZR[147:167,],inflacion$infl[8])
colnames(ag.15)[4]="inflacion"
sep.15=cbind(CZR[168:188,],inflacion$infl[9])
colnames(sep.15)[4]="inflacion"
oct.15=cbind(CZR[189:210,],inflacion$infl[10])
colnames(oct.15)[4]="inflacion"
nov.15=cbind(CZR[211:230,],inflacion$infl[11])
colnames(nov.15)[4]="inflacion"
dic.15=cbind(CZR[231:252,],inflacion$infl[12])
colnames(dic.15)[4]="inflacion"
infl.2015 =rbind(en.15,feb.15,mar.15,abr.15,may.15,jun.15,jul.15,ag.15,sep.15,oct.15,nov.15,dic.15)

View(infl.2015)

### Inflacion 2016

i.2016 =CZR[253:504,]
infl.16 = inflacion$infl[13:24]
en.16=cbind(i.2016[1:19,],infl.16[1])
colnames(en.16)[4]="inflacion"
feb.16=cbind(i.2016[20:39,],infl.16[2])
colnames(feb.16)[4]="inflacion"
mar.16 = cbind(i.2016[40:61,],infl.16[3])
colnames(mar.16)[4]="inflacion"
abr.16=cbind(i.2016[62:82,],infl.16[4])
colnames(abr.16)[4]="inflacion"
may.16 = cbind(i.2016[83:103,],infl.16[5])
colnames(may.16)[4]="inflacion"
jun.16=cbind(i.2016[104:125,],infl.16[6])
colnames(jun.16)[4]="inflacion"
jul.16=cbind(i.2016[126:145,],infl.16[7])
colnames(jul.16)[4]="inflacion"
ag.16=cbind(i.2016[146:168,],infl.16[8])
colnames(ag.16)[4]="inflacion"
sep.16=cbind(i.2016[169:189,],infl.16[9])
colnames(sep.16)[4]="inflacion"
oct.16=cbind(i.2016[190:210,],infl.16[10])
colnames(oct.16)[4]="inflacion"
nov.16=cbind(i.2016[211:231,],infl.16[11])
colnames(nov.16)[4]="inflacion"
dic.16=cbind(i.2016[232:252,],infl.16[12])
colnames(dic.16)[4]="inflacion"
infl.2016 =rbind(en.16,feb.16,mar.16,abr.16,may.16,jun.16,jul.16,ag.16,sep.16,oct.16,nov.16,dic.16)

## Inflacion 2017

i.2017 =CZR[505:755,]
infl.17 = inflacion$infl[25:36]
en.17=cbind(i.2017[1:20,],infl.17[1])
colnames(en.17)[4]="inflacion"
feb.17=cbind(i.2017[21:39,],infl.17[2])
colnames(feb.17)[4]="inflacion"
mar.17 = cbind(i.2017[40:62,],infl.17[3])
colnames(mar.17)[4]="inflacion"
abr.17=cbind(i.2017[63:81,],infl.17[4])
colnames(abr.17)[4]="inflacion"
may.17 = cbind(i.2017[82:103,],infl.17[5])
colnames(may.17)[4]="inflacion"
jun.17=cbind(i.2017[104:125,],infl.17[6])
colnames(jun.17)[4]="inflacion"
jul.17=cbind(i.2017[126:145,],infl.17[7])
colnames(jul.17)[4]="inflacion"
ag.17=cbind(i.2017[146:168,],infl.17[8])
colnames(ag.17)[4]="inflacion"
sep.17=cbind(i.2017[169:188,],infl.17[9])
colnames(sep.17)[4]="inflacion"
oct.17=cbind(i.2017[189:210,],infl.17[10])
colnames(oct.17)[4]="inflacion"
nov.17=cbind(i.2017[211:231,],infl.17[11])
colnames(nov.17)[4]="inflacion"
dic.17=cbind(i.2017[232:251,],infl.17[12])
colnames(dic.17)[4]="inflacion"
infl.2017 =rbind(en.17,feb.17,mar.17,abr.17,may.17,jun.17,jul.17,ag.17,sep.17,oct.17,nov.17,dic.17)

### Inflacion 2018

i.2018 =CZR[756:1006,]
View(i.2018)
infl.18 = inflacion$infl[37:48]
en.18=cbind(i.2018[1:21,],infl.18[1])
colnames(en.18)[4]="inflacion"
feb.18=cbind(i.2018[22:40,],infl.18[2])
colnames(feb.18)[4]="inflacion"
mar.18 = cbind(i.2018[41:61,],infl.18[3])
colnames(mar.18)[4]="inflacion"
abr.18=cbind(i.2018[62:82,],infl.18[4])
colnames(abr.18)[4]="inflacion"
may.18 = cbind(i.2018[83:104,],infl.18[5])
colnames(may.18)[4]="inflacion"
jun.18=cbind(i.2018[105:125,],infl.18[6])
colnames(jun.18)[4]="inflacion"
jul.18=cbind(i.2018[126:146,],infl.18[7])
colnames(jul.18)[4]="inflacion"
ag.18=cbind(i.2018[147:169,],infl.18[8])
colnames(ag.18)[4]="inflacion"
sep.18=cbind(i.2018[170:188,],infl.18[9])
colnames(sep.18)[4]="inflacion"
oct.18=cbind(i.2018[189:211,],infl.18[10])
colnames(oct.18)[4]="inflacion"
nov.18=cbind(i.2018[212:232,],infl.18[11])
colnames(nov.18)[4]="inflacion"
dic.18=cbind(i.2018[233:251,],infl.18[12])
colnames(dic.18)[4]="inflacion"
infl.2018 =rbind(en.18,feb.18,mar.18,abr.18,may.18,jun.18,jul.18,ag.18,sep.18,oct.18,nov.18,dic.18)

## Inflacion 2019 

i.2019 =CZR[1007:1258,]
View(CZR[1007:1258,])
infl.19 = inflacion$infl[49:60]
en.19=cbind(i.2019[1:21,],infl.19[1])
colnames(en.19)[4]="inflacion"
feb.19=cbind(i.2019[22:40,],infl.19[2])
colnames(feb.19)[4]="inflacion"
mar.19 = cbind(i.2019[41:61,],infl.19[3])
colnames(mar.19)[4]="inflacion"
abr.19=cbind(i.2019[62:82,],infl.19[4])
colnames(abr.19)[4]="inflacion"
may.19 = cbind(i.2019[83:104,],infl.19[5])
colnames(may.19)[4]="inflacion"
jun.19=cbind(i.2019[105:124,],infl.19[6])
colnames(jun.19)[4]="inflacion"
jul.19=cbind(i.2019[125:146,],infl.19[7])
colnames(jul.19)[4]="inflacion"
ag.19=cbind(i.2019[147:168,],infl.19[8])
colnames(ag.19)[4]="inflacion"
sep.19=cbind(i.2019[169:188,],infl.19[9])
colnames(sep.19)[4]="inflacion"
oct.19=cbind(i.2019[189:211,],infl.19[10])
colnames(oct.19)[4]="inflacion"
nov.19=cbind(i.2019[212:231,],infl.19[11])
colnames(nov.19)[4]="inflacion"
dic.19=cbind(i.2019[232:252,],infl.19[12])
colnames(dic.19)[4]="inflacion"
infl.2019 =rbind(en.19,feb.19,mar.19,abr.19,may.19,jun.19,jul.19,ag.19,sep.19,oct.19,nov.19,dic.19)
View(infl.2019)

## Inflacion 2020

i.2020 =CZR[1259:1321,]
View(CZR[1259:1322,])
infl.20 = inflacion$infl[61:64]
en.20=cbind(i.2020[1:21,],infl.20[1])
colnames(en.20)[4]="inflacion"
feb.20=cbind(i.2020[22:40,],infl.20[2])
colnames(feb.20)[4]="inflacion"
mar.20 = cbind(i.2020[41:62,],infl.20[3])
colnames(mar.20)[4]="inflacion"
abr.20=cbind(i.2020[63,],infl.20[4])
colnames(abr.20)[4]="inflacion"

infl.2020 =rbind(en.20,feb.20,mar.20,abr.20)
CZR.infl = rbind(infl.2015,infl.2016,infl.2017,infl.2018,infl.2019,infl.2020)
inflacion.total = CZR.infl$inflacion

delta.infl = cbind(Delta,inflacion.total)
bhr.infl = cbind(BHR,inflacion.total)
abio.infl = cbind(ABIO,inflacion.total)
Am.infl = cbind(AMZN,inflacion.total)
coke.infl = cbind(COKE,inflacion.total)
  
write_csv(CZR.infl,file="CZR.inflacion.csv")
write_csv(delta.infl,file="Delta.inflacion.csv")
write_csv(bhr.infl,file="BHR.inflacion.csv")
write_csv(abio.infl,file="ABIO.inflacion.csv")
write_csv(Am.infl,file="Amazon.inflacion.csv")

precio.aj.CZR =CZR.infl$Close*(1+CZR.infl$inflacion)
precio.aj.BHR =bhr.infl$Close*(1+bhr.infl$inflacion)
precio.aj.delta = delta.infl$Close*(1+delta.infl$inflacion.total)
precio.aj.abio = abio.infl$Close*(1+abio.infl$inflacion.total)
precio.aj.am= Am.infl$Close*(1+Am.infl$inflacion.total)
precio.aj.coke = coke.infl$Close*(1+coke.infl$inflacion.total)
CZR.aj = cbind(CZR.infl,precio.aj.CZR)
BHR.aj = cbind(bhr.infl,precio.aj.BHR)
Delta.aj=cbind(delta.infl,precio.aj.delta)
ABIO.aj = cbind(abio.infl,precio.aj.abio)
AMZN.aj = cbind(Am.infl,precio.aj.am)
Coke.aj = cbind(coke.infl,precio.aj.coke)

### Graficos Precios

plot(log(AMZN.aj$precio.aj.am),type = "l", lwd=1, ylim=c(0.5,8.5))
lines(log(CZR.aj$precio.aj.CZR), col="green", lwd=1)
lines(log(BHR.aj$precio.aj.BHR), lwd=1, col="red")
lines(log(Delta.aj$precio.aj.delta), col="blue")
lines(log(ABIO.aj$precio.aj.abio),col="magenta")
lines(log(Coke.aj$precio.aj.coke),col="turquoise")



write_csv(CZR.aj,file="CZR.ajustado.csv")
write_csv(Delta.aj,file="Delta.ajustado.csv")
write_csv(BHR.aj,file="BHR.ajustado.csv")
write_csv(ABIO.aj,file="ABIO.ajustado.csv")
write_csv(AMZN.aj,file="Amazon.ajustado.csv")
write_csv(Coke.aj,file = "Coke.ajustado.csv")
