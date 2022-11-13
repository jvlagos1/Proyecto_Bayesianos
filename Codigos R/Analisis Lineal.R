### Analisis lineal ###

library(mvtnorm)

######### Cocacola ############

mod_coke = lm(cc$`Precio Cierre`~cc$Volumen+cc$`Inflacion Acumulada`+cc$`PIB US`)
summary(mod_coke)
Y_coke = cc$`Precio Cierre`[1:1258]
X_c = cbind(1,cc$Volumen,cc$`Inflacion Acumulada`,cc$`PIB US`)
X_coke = X_c[1:1258,]
X_coke.nuevo = X_c[1259:1321,]
betas.gorro = solve(t(X_coke)%*%X_coke)%*%t(X_coke)%*%Y_coke

n=length(Y_coke)
p=length(betas.gorro)
m=dim(X_coke.nuevo)[1]
s2_gorro.coke =as.numeric((t(Y_coke- X_coke%*%betas.gorro)%*%(Y_coke- X_coke%*%betas.gorro))/(n-p))
var.cov_coke = as.numeric(s2_gorro.coke)*(solve(t(X_coke)%*%X_coke))

### Verosimilitud ###

I_m =diag(x=1,nrow=m,ncol=m)
fac1 =I_m +X_coke.nuevo%*%solve(t(X_coke)%*%X_coke)%*%t(X_coke.nuevo)
t_3 = s2_gorro.coke*fac1
t_2 = X_coke.nuevo%*%betas.gorro
t_1 = n-p

y_coke.n =rmvnorm(m,mean=t_2,sigma=t_3)


modelo_pred.coke<- "model{

  # Verosimilitud 

  for (i in 1:N) {
    mu=X_coke.nuevo%*%beta
    sy = sigma2%*%I_m
    Y[i]~dmvnorm(m,mu,sy)
    
  }

  # Prioris
  B_n = solve(sigma2^(-1)*t(X_coke)%*%X_coke)
  b_n = B_n%*%(sigma2^(-1)*t(X_coke)%*%Y_coke)
  v_n = n-p
  vn_s2n =-p*s2_gorro.coke+
  as.numeric(t(Y_coke- X_coke%*%betas.gorro)%*%(Y_coke- X_coke%*%betas.gorro))+
  t(beta-beta.gorro)%*%t(X_coke)%*%X_coke%*%(beta-beta.gorro)
  
  beta ~ dmvnorm(p,b_n,sigma2*B_n)
  sigma2 ~ dgamma(v_n,vn_s2n)

}"                  
