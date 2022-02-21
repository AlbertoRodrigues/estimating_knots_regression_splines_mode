K=c(3,1,1)
alpha1=c(2,1,0)
lambda1=c(0,0,0)

#K=c(1)
#alpha1=c(1)
#lambda1=c(0)

cria_f1=function(alpha, lambda, p=1, X)
{
  cont=1
  fcost2<<-""
  valores_iniciais<<-c()
  for(i in 1:p)
  {
    for(l in 1:K[i])
    {
      fcost2<<-paste(fcost2,"theta[",cont,"]*x[,",i,"]**",l,"+")
      cont=cont+1
    }
    valores_iniciais<<-append(valores_iniciais,rep(0,K[i]))
    if(alpha[i]>0)
    {
      for(j in 1:alpha[i])
      {
        fcost2<<-paste(fcost2,"theta[",cont,"]*((x[,",i,"]-theta[",cont+1,"])**",K[i],")*ifelse(x[,",i,"]>theta[",cont+1,"],1,0) +")
        cont=cont+2
        valores_iniciais<<-append(valores_iniciais,as.vector(c(0,quantile(X[,i],j/(alpha[i]+1)))))
      }
    }
    
    
  }
  valores_iniciais<<-append(valores_iniciais,0)
  
  fcost2<<-paste(fcost2, "theta[",cont,"]")
  f_cost=paste("mean((",fcost2,"-y)**2)")
  ind_param_pen<<-c()

  if(sum(alpha!=0)>0)
  {
    ind_param_pen<<-list()
    cont=0
    for(j in 1:p)
    {
      if(alpha[j]>0)
      {
        ind_param_pen[[j]]<<-seq(K[j]+1+cont,2*alpha[j]+K[j]+cont,2)
        f_cost=paste(f_cost,"+lambda[",j,"]*alpha[",j,"]*sum(abs(theta[indices[[",j,"]]]))")
      }
      cont=cont+2*alpha[j]+K[j]
    }
  }
  return(f_cost)
}


cria_df1=function(alpha, lambda, p=1)
{
  dtheta=c()
  cont=1
  for(i in 1:p)
  {
    for(l in 1:K[i])
    {
      dtheta[cont]=paste("2*mean((",fcost2,"-y)*x[,",i,"]**",l,")") 
      cont=cont+1
      #print(cont)
    }
    if(alpha[i]>0)
    {  
      for(j in 1:alpha[i])
      {
        dtheta[cont]=paste("2*mean((",fcost2,"-y)*(((x[,",i,"]-theta[",cont+1,"])**",K[i],")*ifelse(x[,",i,"]>theta[",cont+1,"],1,0)))+alpha[",i,"]*lambda[",i,"]*ifelse(theta[",cont,"]>=0,ifelse(theta[",cont,"]>0,1,0),-1)")
        dtheta[cont+1]=paste("2*mean((",fcost2,"-y)*theta[",cont,"]*ifelse(x[,",i,"]>theta[",cont+1,"],",-K[i],"*(x[,",i,"]-theta[",cont+1,"])**",(K[i]-1),",0))")  
        cont=cont+2
        #print(cont)
      }
    }
  }
  #print(cont)
  dtheta[cont]=paste("2*mean(",fcost2,"-y)")
  return(dtheta)
}



f=function(theta)
{
  f1 = parse(text = f_cost)
  return(eval(f1, envir=list(theta=theta,x=X_real,y=y,alpha=alpha1,lambda=lambda1,indices=ind_param_pen)))
}
f(valores_iniciais)


dfparam=function(theta)
{
  dtheta2=c()
  for(i in 1:length(dtheta))
  {
    f2 = parse(text = dtheta[i])
    dtheta2[i]=eval(f2, envir=list(theta=theta,x=X_real,y=y,lambda=lambda1, alpha=alpha1))
  }
  return(dtheta2)
}

param_n=matrix(0,nrow=3,ncol=1)
#param_n=matrix(0,nrow=1, ncol=1)
var_ruido=1
knots2 = list(c(18,76.5), c(84.4), c(90))
theta2 = list(c(0.2,4.1, 5,1,-1.5,2.3),c(0.2,1.1, 5,1,-5.5),c(0.2,1.1, 5,1,-5.5))
K2=c(3,1,1)
#knots2 = list(c(84.4))
#theta2 = list(c(0.2,1.1, 5,1,-5.5))
#K2=c(1)

n2=c(200, 400, 600)
#n=300
#Ajeitar as funções desse codigo
#Talvez oproblema seja as variaveis globais
for(n in n2)
{
  set.seed(7)
  x1=sample(seq(0, 100, length.out = n))
  x2=sample(seq(0, 105, length.out = n))
  x3=sample(seq(0, 70,  length.out = n))
  
  X_real = cbind(x1, x2, x3)
  #X_real = matrix(x2, ncol=1)
  y_simulado=gera_y_simulado(n, X_real, knots2, theta2, K2, var_ruido)
  param=matrix(0,nrow=1,ncol=3)
  #param=matrix(0,nrow=1,ncol=1)
  #b=1
  for(b in 1:3)
  {
    set.seed(b)
    y=y_simulado+rnorm(n, 0, var_ruido)
    
    f_cost=cria_f1(alpha1, lambda1, p, X_real)
    dtheta=cria_df1(alpha1, lambda1, p)
    modelo=optim(valores_iniciais ,fn=f,gr=dfparam, method="BFGS", control=list(trace=TRUE,maxit=150))
    param=rbind(param, modelo$par[c(5, 7, 10)])
    #param=rbind(param, modelo$par[3])
  }
  param=param[-1,]
  param_n=cbind(param_n, t(t(round(colMeans(param),2))))
  #param_n=cbind(param_n, t(t(round(mean(param),2))))
}
param_n=param_n[,-1]
param_n
param_vector
