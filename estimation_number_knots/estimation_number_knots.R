knots_analysis=function(knots, theta, K, quantidade_simulacoes, ix)
{
require(xtable)
library(splines)
library(data.table)
cria_f1=function(alpha, lambda)
{
  cont=1
  fcost2<<-""
  valores_iniciais<<-c()
  for(i in 1:p)
  {
    for(l in 1:K)
    {
      fcost2<<-paste(fcost2,"theta[",cont,"]*x[,",i,"]**",l,"+")
      cont=cont+1
    }
    valores_iniciais<<-append(valores_iniciais,rep(0,K))
    if(alpha[i]>0)
    {
      for(j in 1:alpha[i])
      {
        fcost2<<-paste(fcost2,"theta[",cont,"]*((x[,",i,"]-theta[",cont+1,"])**",K,")*ifelse(x[,",i,"]>theta[",cont+1,"],1,0) +")
        cont=cont+2
        valores_iniciais<<-append(valores_iniciais,as.vector(c(0,quantile(X[,i],j/(alpha[i]+1)))))
      }
    }
    
    
  }
  valores_iniciais<<-append(valores_iniciais,0)
  
  fcost2<<-paste(fcost2, "theta[",cont,"]")
  f_cost=paste("mean((",fcost2,"-y)**2)")
  ind_param_pen<<-c()
  
  if(p==1 & alpha>0)
  {
    ind_param_pen<<-seq(K+1,2*alpha+K,2)
    f_cost=paste(f_cost,"+lambda*alpha*sum(abs(theta[indices]))")
  }
  
  
  if(p>1 & sum(alpha!=0)>0)
  {
    ind_param_pen<<-list()
    cont=0
    for(j in 1:p)
    {
      if(alpha[j]>0)
      {
        ind_param_pen[[j]]<<-seq(K+1+cont,2*alpha[j]+K+cont,2)
        f_cost=paste(f_cost,"+lambda[",j,"]*alpha[",j,"]*sum(abs(theta[indices[[",j,"]]]))")
      }
      cont=cont+2*alpha[j]+K
    }
  }
  return(f_cost)
}

cria_df1=function(alpha, lambda)
{
  dtheta=c()
  cont=1
  for(i in 1:p)
  {
    for(l in 1:K)
    {
      dtheta[cont]=paste("2*mean((",fcost2,"-y)*x[,",i,"]**",l,")") 
      cont=cont+1
    }
    if(alpha[i]>0)
    {  
      for(j in 1:alpha[i])
      {
        dtheta[cont]=paste("2*mean((",fcost2,"-y)*(((x[,",i,"]-theta[",cont+1,"])**",K,")*ifelse(x[,",i,"]>theta[",cont+1,"],1,0)))+alpha[",i,"]*lambda[",i,"]*ifelse(theta[",cont,"]>=0,ifelse(theta[",cont,"]>0,1,0),-1)")
        dtheta[cont+1]=paste("2*mean((",fcost2,"-y)*theta[",cont,"]*ifelse(x[,",i,"]>theta[",cont+1,"],",-K,"*(x[,",i,"]-theta[",cont+1,"])**",(K-1),",0))")  
        cont=cont+2
      }
    }
  }
  dtheta[cont]=paste("2*mean(",fcost2,"-y)")
  return(dtheta)
}

cria_f2=function(alpha, lambda)
{
  cont=1
  fcost2<<-""
  valores_iniciais<<-c()
  for(i in 1:p)
  {
    for(l in 1:K)
    {
      fcost2<<-paste(fcost2,"theta[",cont,"]*x[,",i,"]**",l,"+")
      cont=cont+1
    }
    valores_iniciais<<-append(valores_iniciais,rep(0,K))
    if(alpha[i]>0)
    {
      for(j in 1:alpha[i])
      {
        fcost2<<-paste(fcost2,"theta[",cont,"]*((x[,",i,"]-theta[",cont+1,"])**",K,")*ifelse(x[,",i,"]>theta[",cont+1,"],1,0) +")
        cont=cont+2
        valores_iniciais<<-append(valores_iniciais,as.vector(c(0,quantile(X[,i],j/(alpha[i]+1)))))
      }
    }
    
    
  }
  valores_iniciais<<-append(valores_iniciais,0)
  
  fcost2<<-paste(fcost2, "theta[",cont,"]")
  f_cost=paste("mean((",fcost2,"-y)**2)+lambda*sum(alpha)/log(n)")
  
  return(f_cost)
}

cria_df2=function(alpha, lambda)
{
  dtheta=c()
  cont=1
  for(i in 1:p)
  {
    for(l in 1:K)
    {
      dtheta[cont]=paste("2*mean((",fcost2,"-y)*x[,",i,"]**",l,")") 
      cont=cont+1
    }
    if(alpha[i]>0)
    {  
      for(j in 1:alpha[i])
      {
        dtheta[cont]=paste("2*mean((",fcost2,"-y)*(((x[,",i,"]-theta[",cont+1,"])**",K,")*ifelse(x[,",i,"]>theta[",cont+1,"],1,0)))")
        dtheta[cont+1]=paste("2*mean((",fcost2,"-y)*theta[",cont,"]*ifelse(x[,",i,"]>theta[",cont+1,"],",-K,"*(x[,",i,"]-theta[",cont+1,"])**",(K-1),",0))")  
        cont=cont+2
      }
    }
  }
  dtheta[cont]=paste("2*mean(",fcost2,"-y)")
  return(dtheta)
}

cria_f=function(alpha)
{
  cont=1
  fcost<<-""
  valores_iniciais<<-c()
  for(i in 1:p)
  {
    for(l in 1:K)
    {
      fcost<<-paste(fcost,"theta[",cont,"]*x[,",i,"]**",l,"+")
      cont=cont+1
    }
    valores_iniciais<<-append(valores_iniciais,rep(0,K))
    if(alpha[i]>0)
    {
      for(j in 1:alpha[i])
      {
        fcost<<-paste(fcost,"theta[",cont,"]*((x[,",i,"]-theta[",cont+1,"])**",K,")*ifelse(x[,",i,"]>theta[",cont+1,"],1,0) +")
        cont=cont+2
        valores_iniciais<<-append(valores_iniciais,as.vector(c(0,quantile(X[,i],j/(alpha[i]+1)))))
      }
    }
    
    
  }
  valores_iniciais<<-append(valores_iniciais,0)
  
  fcost<<-paste(fcost, "theta[",cont,"]")
  f_cost=paste("mean((",fcost,"-y)**2)")
  
  return(f_cost)
}

cria_df=function(alpha)
{
  dtheta=c()
  cont=1
  for(i in 1:p)
  {
    for(l in 1:K)
    {
      dtheta[cont]=paste("2*mean((",fcost,"-y)*x[,",i,"]**",l,")") 
      cont=cont+1
    }
    if(alpha[i]>0)
    {  
      for(j in 1:alpha[i])
      {
        dtheta[cont]=paste("2*mean((",fcost,"-y)*(((x[,",i,"]-theta[",cont+1,"])**",K,")*ifelse(x[,",i,"]>theta[",cont+1,"],1,0)))")
        dtheta[cont+1]=paste("2*mean((",fcost,"-y)*theta[",cont,"]*ifelse(x[,",i,"]>theta[",cont+1,"],",-K,"*(x[,",i,"]-theta[",cont+1,"])**",(K-1),",0))")  
        cont=cont+2
      }
    }
  }
  dtheta[cont]=paste("2*mean(",fcost,"-y)")
  return(dtheta)
}

f_y=function(x)
{
  if(is.null(dim(x)))
  {
    x=matrix(x,ncol=p)
  }
  f_est = parse(text = fcost)
  return(eval(f_est, envir=list(theta=param,x=x)))
}

genSpline = function(x, knots, degree, theta) {

basis = bs(x = x, knots = knots, degree = degree,
           Boundary.knots = c(0,100), intercept = T)

y.spline = basis %*% theta

dt = data.table(x, y.spline = as.vector(y.spline))

return(list(dt = dt, basis = basis, knots = knots))

}
x = seq(0, 100, length.out = 800)

alpha=c(length(knots))
sdata = genSpline(x, knots, 3, theta)

y_spline=sdata$dt$y.spline

X=matrix(x,ncol=1)
p=ncol(X)
y=y_spline

f_cost=cria_f(alpha)
dtheta=cria_df(alpha)

f=function(theta)
{
f1 = parse(text = f_cost)
return(eval(f1, envir=list(theta=theta,x=X,y=y,alpha=alpha)))
}
dfparam=function(theta)
{
dtheta2=c()
for(i in 1:length(dtheta))
{
  f2 = parse(text = dtheta[i])
  dtheta2[i]=eval(f2, envir=list(theta=theta,x=X,y=y,alpha=alpha))
}
return(dtheta2)
}

modelo=optim(valores_iniciais ,fn=f,gr=dfparam, method="BFGS", control=list(trace=TRUE,maxit=150))
param=modelo$par

list_alpha=list(c(0,1,2,3,4,5))
list_lambda=c(0.1,0.3,1,3)
n=nrow(X)

loss1=list()
loss2=list()
for(k in 1:length(list_lambda))
{
  loss1[[k]]=matrix(0,quantidade_simulacoes,length(list_alpha[[1]]))
  loss2[[k]]=matrix(0,quantidade_simulacoes,length(list_alpha[[1]]))
  lambda=list_lambda[k]
  for(b in 1:quantidade_simulacoes)
  {
    set.seed(b)
    ruido=rnorm(dim(X)[1],0,2)
    y=f_y(X)+ruido
    for(m in 1:length(list_alpha[[1]]))
    {
      alpha=list_alpha[[1]][m]
      
      f_cost=cria_f1(alpha, lambda)
      
      f=function(theta)
      {
        f1 = parse(text = f_cost)
        return(eval(f1, envir=list(theta=theta,x=X,y=y,alpha=alpha,lambda=lambda,indices=ind_param_pen)))
      }
      
      dtheta=cria_df1(alpha, lambda)
      
      dfparam=function(theta)
      {
        dtheta2=c()
        for(i in 1:length(dtheta))
        {
          f2 = parse(text = dtheta[i])
          dtheta2[i]=eval(f2, envir=list(theta=theta,x=X,y=y,alpha=alpha,lambda=lambda))
        }
        return(dtheta2)
      }
      modelo1=optim(valores_iniciais ,fn=f,gr=dfparam, method="BFGS", control=list(trace=TRUE,maxit=150))
      
      loss1[[k]][b,m]=modelo1$value
      
      f_cost=cria_f2(alpha, lambda)
      
      f=function(theta)
      {
        f1 = parse(text = f_cost)
        return(eval(f1, envir=list(theta=theta,x=X,y=y,alpha=alpha, lambda=lambda, n=nrow(X))))
      }
      
      dtheta=cria_df2(alpha, lambda)
      
      dfparam=function(theta)
      {
        dtheta2=c()
        for(i in 1:length(dtheta))
        {
          f2 = parse(text = dtheta[i])
          dtheta2[i]=eval(f2, envir=list(theta=theta,x=X,y=y))
        }
        return(dtheta2)
      }
      
      modelo2=optim(valores_iniciais ,fn=f,gr=dfparam, method="BFGS", control=list(trace=TRUE,maxit=150))
      loss2[[k]][b,m]=modelo2$value
    }
  }
  
}
loss1
loss2

j1=rbind.data.frame(colMeans(loss1[[1]]), colMeans(loss1[[2]]), colMeans(loss1[[3]]), colMeans(loss1[[4]]))

j2=rbind.data.frame(colMeans(loss2[[1]]), colMeans(loss2[[2]]), colMeans(loss2[[3]]), colMeans(loss2[[4]]))
colnames(j1)=list_alpha[[1]]
colnames(j2)=list_alpha[[1]]
rownames(j1)=list_lambda
rownames(j2)=list_lambda

sink(file = paste("\\Users\\Alberto\\Desktop\\ime-usp\\dissertacao_ime_mestrado\\estnumberknots\\R_output_", ix, ".txt"))

print(j1)
print(j2)
print(xtable(j1, digits=3))
print(xtable(j2, digits=3))

sink(file = NULL)
}

knots_analysis(c(84.4), c(0.2,1.1, 5,1,-5.5), 1, 100, 1)
knots_analysis(c(18,76.5), c(0.2,4.1, 5,1,-1.5,2.3), 1, 100, 3)
knots_analysis(c(15,35,66.5), c(0.2,4.1, 5,1,-1.5,10.3,-7.4), 1, 100, 5)
knots_analysis(c(84.4), c(0.2,1.1, 5,1,-5.5), 3, 100, 2)
knots_analysis(c(18,76.5), c(0.2,4.1, 5,1,-1.5,2.3), 3, 100, 4)
knots_analysis(c(15,35,66.5), c(0.2,4.1, 5,1,-1.5,10.3,-7.4), 3, 100, 6)