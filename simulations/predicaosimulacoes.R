library(splines)
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

cria_f2=function(alpha, lambda, p=1, X)
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
  f_cost=paste("mean((",fcost2,"-y)**2)+lambda*sum(alpha)/log(n)")
  
  return(f_cost)
}

cria_df2=function(alpha, lambda, p=1)
{
  dtheta=c()
  cont=1
  for(i in 1:p)
  {
    for(l in 1:K[i])
    {
      dtheta[cont]=paste("2*mean((",fcost2,"-y)*x[,",i,"]**",l,")") 
      cont=cont+1
    }
    if(alpha[i]>0)
    {  
      for(j in 1:alpha[i])
      {
        dtheta[cont]=paste("2*mean((",fcost2,"-y)*(((x[,",i,"]-theta[",cont+1,"])**",K[i],")*ifelse(x[,",i,"]>theta[",cont+1,"],1,0)))")
        dtheta[cont+1]=paste("2*mean((",fcost2,"-y)*theta[",cont,"]*ifelse(x[,",i,"]>theta[",cont+1,"],",-K[i],"*(x[,",i,"]-theta[",cont+1,"])**",(K[i]-1),",0))")  
        cont=cont+2
      }
    }
  }
  dtheta[cont]=paste("2*mean(",fcost2,"-y)")
  return(dtheta)
}

lc <- function(n, k) {
  stopifnot(k > 0 && n > 0)
  aux <- n
  nk <- k
  i <- vector("numeric", k)
  while (nk > 0) {
    i[nk] <- round(aux/nk)
    aux <- aux - i[nk]
    nk <- nk - 1
  }
  return(i)
}

kFold <- function(dados, k = 2, seed = NULL) {
  if (!is.null(seed))
    set.seed(seed)
  n <- dim(dados)[1]
  interval <- lc(n, k)
  res <- vector("raw", k)
  for (i in 1:k) {
    temp <- sample(dim(dados)[1], interval[i], replace = FALSE)
    res[i] <- list(dados[temp, ])
    dados <- dados[-temp, ]
  }
  names(res) <- 1:k
  return(res)
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

f_y2=function(x)
{
  if(is.null(dim(x)))
  {
    x=matrix(x,ncol=p)
  }
  f_est = parse(text = fcost2)
  return(eval(f_est, envir=list(theta=param2,x=x)))
}



num_alpha_max=3

list_lambda1=list(c(0, 0.01,0.03,0.1,0.3,1,3, 10, 30),c(0, 0.01,0.03,0.1,0.3,1,3, 10, 30),c(0, 0.01,0.03,0.1,0.3,1,3, 10, 30))
list_lambda2=c(0, 0.01,0.03,0.1,0.3,1,3, 10, 30)
list_K=list(c(1,2,3),c(1,2,3),c(1,2,3))
n2=c(300, 900, 1500, 3000)
var_ruido=c(1, 2, 3)
quant_random_search=30

erros=list()

#b=1  
for(b in 1:length(var_ruido))
{ 
  erros[[b]]=matrix(0,ncol=1, nrow=3)

#n=n2[1]  
for(n in n2)
{
    treino=read.csv(paste("\\Users\\Alberto\\Desktop\\ime-usp\\dissertacao_ime_mestrado\\simulations\\data\\treino", n, var_ruido[b],".csv",sep="_"))
    teste=read.csv(paste("\\Users\\Alberto\\Desktop\\ime-usp\\dissertacao_ime_mestrado\\simulations\\data\\teste", n, var_ruido[b],".csv",sep="_"))
    X_treino = treino[,c(1,2,3)]
    X_teste = teste[,c(1,2,3)]
    y_treino = treino$y
    y_teste = teste$y
    p=ncol(X_treino)
    
#loop da CV
folds=kFold(data.frame(X_treino),k=5)
#k=1


alpha1_est=list()
alpha2_est=list()

lambda1_est=list()
lambda2_est=c()
K1_est=list()
K2_est=list()



y=y_treino
#k=1
for(k in 1:quant_random_search)
{
  # VER DIREITO OS INDICES DAS VARIAVEIS EM SEGUIDA!
  lambda2=sample(list_lambda2,1)
  lambda2_est[k]=lambda2
  K1_est[[k]]=rep(-1,p)
  K2_est[[k]]=rep(-1,p)
  alpha1_est[[k]]=rep(-1,p)
  alpha2_est[[k]]=rep(-1,p)
  lambda1_est[[k]]=rep(-1,p)
  #j=1
  for(j in 1:p)
  {
    loss1=c()
    loss2=c()
    
    lambda1=sample(list_lambda1[[j]],1)
    lambda1_est[[k]][j]=lambda1
    
    
    K=sample(list_K[[j]],1)
    K1_est[[k]][j]=K
    #m=2
    for(m in 0:num_alpha_max)
    {
    alpha1=m
    f_cost=cria_f1(alpha1, lambda1, 1, matrix(X_treino[,j]))
    
    f=function(theta)
    {
      f1 = parse(text = f_cost)
      return(eval(f1, envir=list(theta=theta,x=matrix(X_treino[,j]),y=y,alpha=alpha1,lambda=lambda1,indices=ind_param_pen)))
    }
    
    dtheta=cria_df1(alpha1, lambda1, 1)
    
    dfparam=function(theta)
    {
      dtheta2=c()
      for(i in 1:length(dtheta))
      {
        f2 = parse(text = dtheta[i])
        dtheta2[i]=eval(f2, envir=list(theta=theta,x=matrix(X_treino[,j]),y=y,lambda=lambda1, alpha=alpha1))
      }
      return(dtheta2)
    }
    
    
    modelo1=optim(valores_iniciais ,fn=f,gr=dfparam, method="BFGS", control=list(trace=F,maxit=150))
    loss1[m+1]=modelo1$value
    alpha1_est[[k]][j]=m
    
    if(modelo1$value>min(loss1))
    {
      alpha1_est[[k]][j]=m-1
      break
    }
  }
    
    K=sample(list_K[[j]],1)
    K2_est[[k]][j]=K
    #m=1
    for(m in 0:num_alpha_max)
     {
        alpha2=m
        f_cost=cria_f2(alpha2, lambda2, 1, matrix(X_treino[,j]))
        
        f=function(theta)
        {
          f1 = parse(text = f_cost)
          return(eval(f1, envir=list(theta=theta,x=matrix(X_treino[,j]),y=y,alpha=alpha2, lambda=lambda2, n=nrow(matrix(X_treino[,j])))))
        }
        
        dtheta=cria_df2(alpha2, lambda2, 1)
        
        dfparam=function(theta)
        {
          dtheta2=c()
          for(i in 1:length(dtheta))
          {
            f2 = parse(text = dtheta[i])
            dtheta2[i]=eval(f2, envir=list(theta=theta,x=matrix(X_treino[,j]),y=y, lambda=lambda2))
          }
          return(dtheta2)
        }
        
        modelo2=optim(valores_iniciais ,fn=f,gr=dfparam, method="BFGS", control=list(trace=F,maxit=150))
        loss2[m+1]=modelo2$value
        alpha2_est[[k]][j]=m
        
        if(modelo2$value>min(loss2))
        {
          alpha2_est[[k]][j]=m-1
          break
        }
    }
  }
}
alpha1_est
alpha2_est

lambda1_est
lambda2_est
K1_est
K2_est

erro_treino1=c()
erro_val1=c()
dp_treino1=c()
dp_val1=c()

erro_treino2=c()
erro_val2=c()
dp_treino2=c()
dp_val2=c()
#k=1
for(k in 1:quant_random_search)
{
  erro_treino_cv1=c()
  erro_val_cv1=c()
  dp_treino_cv1=c()
  dp_val_cv1=c()
  
  erro_treino_cv2=c()
  erro_val_cv2=c()
  dp_treino_cv2=c()
  dp_val_cv2=c()
  
  alpha1=alpha1_est[[k]]
  alpha2=alpha2_est[[k]]
  
  lambda1=lambda1_est[[k]]
  lambda2=lambda2_est[k]
  #s=1
  for(s in 1:length(folds))
  {
    # verificar os indices de treino e teste, muito de XTreino para X2
    ind_val2=as.numeric(rownames(as.data.frame(folds[s])))
    ind_treino2=setdiff(1:nrow(X_treino),ind_val2)
    X_treino2=X_treino[ind_treino2,]
    X_val2=X_treino[ind_val2,]
    y_simulado_treino_cv=y_treino[ind_treino2]
    y_simulado_val_cv=y_treino[ind_val2]
    
    y=y_treino[ind_treino2]
    
    K=K1_est[[k]]
    #alpha1=c(1,1)
    f_cost=cria_f1(alpha1, lambda1,p, X_treino2)
    
    f=function(theta)
    {
      f1 = parse(text = f_cost)
      return(eval(f1, envir=list(theta=theta,x=X_treino2,y=y,alpha=alpha1,lambda=lambda1,indices=ind_param_pen)))
    }
    
    #f(valores_iniciais)
    dtheta=cria_df1(alpha1, lambda1, p)
    
    dfparam=function(theta)
    {
      dtheta2=c()
      #i=5
      for(i in 1:length(dtheta))
      {
        f2 = parse(text = dtheta[i])
        dtheta2[i]=eval(f2, envir=list(theta=theta,x=X_treino2,y=y,lambda=lambda1, alpha=alpha1))
      }
      return(dtheta2)
    }
    
    modelo1=optim(valores_iniciais ,fn=f,gr=dfparam, method="BFGS", control=list(trace=F,maxit=150))
    param2=modelo1$par
    
    y_treino_pred=f_y2(X_treino2)
    y_val_pred=f_y2(X_val2)
    
    diff1=(y_treino_pred-y_simulado_treino_cv)**2
    diff2=(y_val_pred-y_simulado_val_cv)**2
    
    erro_treino_cv1[s]=mean(diff1)
    erro_val_cv1[s]=mean(diff2)
    
    dp_treino_cv1[s]=sd(diff1)
    dp_val_cv1[s]=sd(diff2)
    
    
    K=K2_est[[k]]
    f_cost=cria_f2(alpha2, lambda2, p, X_treino2)
    
    f=function(theta)
    {
      f1 = parse(text = f_cost)
      return(eval(f1, envir=list(theta=theta,x=X_treino2,y=y,alpha=alpha2, lambda=lambda2, n=nrow(X_treino2))))
    }
    
    dtheta=cria_df2(alpha2, lambda2, p)
    
    dfparam=function(theta)
    {
      dtheta2=c()
      for(i in 1:length(dtheta))
      {
        f2 = parse(text = dtheta[i])
        dtheta2[i]=eval(f2, envir=list(theta=theta,x=X_treino2,y=y, lambda=lambda2))
      }
      return(dtheta2)
    }
    
    modelo2=optim(valores_iniciais ,fn=f,gr=dfparam, method="BFGS", control=list(trace=F,maxit=150))
    
    param2=modelo2$par
    
    y_treino_pred=f_y2(X_treino2)
    y_val_pred=f_y2(X_val2)
    
    diff1=(y_treino_pred-y_simulado_treino_cv)**2
    diff2=(y_val_pred-y_simulado_val_cv)**2
    
    erro_treino_cv2[s]=mean(diff1)
    erro_val_cv2[s]=mean(diff2)
    
    dp_treino_cv2[s]=sd(diff1)
    dp_val_cv2[s]=sd(diff2)
    
  }
  erro_treino1[k]=mean(erro_treino_cv1)
  erro_val1[k]=mean(erro_val_cv1)
  dp_treino1[k]=mean(dp_treino_cv1)
  dp_val1[k]=mean(dp_val_cv1)
  
  erro_treino2[k]=mean(erro_treino_cv2)
  erro_val2[k]=mean(erro_val_cv2)
  dp_treino2[k]=mean(dp_treino_cv2)
  dp_val2[k]=mean(dp_val_cv2)
}
  
# Modelo 1 final

erro_treino1
erro_val1
dp_treino1
dp_val1

#sink(file = paste("\\Users\\Alberto\\Desktop\\ime-usp\\dissertacao_ime_mestrado\\simulations\\hiperparametros\\hiper",n,var_ruido[b],1,".txt",sep="_"))
#print(K1_est[[order(erro_val1)[1]]])
#print(lambda1_est[[order(erro_val1)[1]]])
#print(alpha1_est[[order(erro_val1)[1]]])
#sink(file = NULL)

alpha1=alpha1_est[[order(erro_val1)[1]]]
lambda1=lambda1_est[[order(erro_val1)[1]]]
K=K1_est[[order(erro_val1)[1]]]

f_cost=cria_f1(alpha1, lambda1, p, X_treino)


f=function(theta)
{
  f1 = parse(text = f_cost)
  return(eval(f1, envir=list(theta=theta,x=X_treino,y=y,alpha=alpha1,lambda=lambda1,indices=ind_param_pen)))
}

dtheta=cria_df1(alpha1, lambda1, p)
dfparam=function(theta)
{
  dtheta2=c()
  for(i in 1:length(dtheta))
  {
    f2 = parse(text = dtheta[i])
    dtheta2[i]=eval(f2, envir=list(theta=theta,x=X_treino,y=y,lambda=lambda1, alpha=alpha1))
  }
  return(dtheta2)
}
y=y_treino
modelo1=optim(valores_iniciais ,fn=f,gr=dfparam, method="BFGS", control=list(trace=F,maxit=150))

param2=modelo1$par
y_pred_test=f_y2(X_teste)
diff1=(y_pred_test-y_teste)**2
mean(diff1)
sd(diff1)



# Modelo 2 final
erro_treino2
erro_val2
dp_treino2
dp_val2

#sink(file = paste("\\Users\\Alberto\\Desktop\\ime-usp\\dissertacao_ime_mestrado\\simulations\\hiperparametros\\hiper",n,var_ruido[b],2,".txt",sep="_"))
#print(K2_est[[order(erro_val2)[1]]])
#print(lambda2_est[order(erro_val2)[1]])
#print(alpha2_est[[order(erro_val2)[1]]])
#sink(file = NULL)

alpha2=alpha2_est[[order(erro_val2)[1]]]
lambda2=lambda2_est[order(erro_val2)[1]]
K=K2_est[[order(erro_val2)[1]]]

f_cost=cria_f2(alpha2, lambda2, p, X_treino)

f=function(theta)
{
  f1 = parse(text = f_cost)
  return(eval(f1, envir=list(theta=theta,x=X_treino,y=y,alpha=alpha2, lambda=lambda2, n=nrow(X_treino))))
}

dtheta=cria_df2(alpha2, lambda2, p)

dfparam=function(theta)
{
  dtheta2=c()
  for(i in 1:length(dtheta))
  {
    f2 = parse(text = dtheta[i])
    dtheta2[i]=eval(f2, envir=list(theta=theta,x=X_treino,y=y, lambda=lambda2))
  }
  return(dtheta2)
}
y=y_treino
modelo2=optim(valores_iniciais ,fn=f,gr=dfparam, method="BFGS", control=list(trace=F,maxit=150))

param2=modelo2$par
y_pred_test=f_y2(X_teste)

diff2=(y_pred_test-y_teste)**2
mean(diff2)
sd(diff2)

#B Splines

#Random search

erro1=c()
erro2=c()
dp1=c()
dp2=c()

alpha=c(0,1,2,3)
degree=c(1,2,3)
alpha2=list()
degree2=list()

#k=1
for(k in 1:quant_random_search)
{
  # CV
  erro_cv1=c()
  erro_cv2=c()
  dp_cv1=c()
  dp_cv2=c()
  
  #s=1
  alpha2[[k]]=sample(alpha, p, replace = T)
  degree2[[k]]=sample(degree, p, replace = T)
  
  for(s in 1:length(folds))
  {
    ind_val2=as.numeric(rownames(as.data.frame(folds[s])))
    ind_treino2=setdiff(1:nrow(X_treino),ind_val2)
    X_treino2=X_treino[ind_treino2,]
    X_val2=X_treino[ind_val2,]
    y_simulado_treino_cv=y_treino[ind_treino2]
    y_simulado_val_cv=y_treino[ind_val2]
    
    X_bsplines_treino=matrix(0,nrow=length(ind_treino2))
    X_bsplines_teste=matrix(0,nrow=length(ind_val2))
    
    
    #j=1
    for(j in 1:p)
    {
      knot=as.vector(quantile(X_treino2[,j],seq(1,alpha2[[k]][j])/(alpha2[[k]][j]+1)))
      if(alpha2[[k]][j]==0)
      {
        knot=NULL
      }
      X_bsplines_treino=cbind(X_bsplines_treino, bs(X_treino2[,j],knots=knot , degree=degree2[[k]][j]))
      X_bsplines_teste=cbind(X_bsplines_teste, bs(X_val2[,j],knots=knot , degree=degree2[[k]][j]))
      colnames(X_bsplines_treino)=seq(0,ncol(X_bsplines_treino)-1)
    }
    X_bsplines_treino=X_bsplines_treino[,-1]
    X_bsplines_teste=X_bsplines_teste[,-1]
    
    model=lm(y_simulado_treino_cv~X_bsplines_treino)
    ypred=as.vector(cbind(rep(1, nrow(X_bsplines_teste)),X_bsplines_teste)%*%t(t(model$coefficients)))
    diff=(y_simulado_val_cv-ypred)**2
    erro_cv1[s]=mean(diff)
    dp_cv1[s]=sd(diff)
    
  }
  erro1[k]=mean(erro_cv1)
  dp1[k]=mean(dp_cv1)
}
# Modelo final
erro1
dp1
alpha2
degree2

alpha_final=alpha2[[order(erro1)[1]]]
degree_final=degree2[[order(erro1)[1]]]
#Exportando todos os hiperparametros
sink(file = paste("\\Users\\Alberto\\Desktop\\ime-usp\\dissertacao_ime_mestrado\\simulations\\hiperparametros\\hiper_R",n,var_ruido[b],".txt",sep="_"))

print(K1_est[[order(erro_val1)[1]]])
print(lambda1_est[[order(erro_val1)[1]]])
print(alpha1_est[[order(erro_val1)[1]]])

print(K2_est[[order(erro_val2)[1]]])
print(lambda2_est[order(erro_val2)[1]])
print(alpha2_est[[order(erro_val2)[1]]])

print(alpha_final)
print(degree_final)
sink(file = NULL)

X_bsplines_treino=matrix(0,nrow=nrow(X_treino))
X_bsplines_teste=matrix(0,nrow=nrow(X_teste))


#j=1
for(j in 1:p)
{
  knot=as.vector(quantile(X_treino[,j],seq(1,alpha_final[j])/(alpha_final[j]+1)))
  if(alpha_final[j]==0)
  {
    knot=NULL
  }
  X_bsplines_treino=cbind(X_bsplines_treino, bs(X_treino[,j],knots=knot , degree=degree_final[j]))
  X_bsplines_teste=cbind(X_bsplines_teste, bs(X_teste[,j],knots=knot , degree=degree_final[j]))
  colnames(X_bsplines_treino)=seq(0,ncol(X_bsplines_treino)-1)
}
X_bsplines_treino=X_bsplines_treino[,-1]
X_bsplines_teste=X_bsplines_teste[,-1]

model=lm(y_treino~X_bsplines_treino)
ypred=as.vector(cbind(rep(1, nrow(X_bsplines_teste)),X_bsplines_teste)%*%t(t(model$coefficients)))
diff3=(y_teste-ypred)**2
mean(diff3)
sd(diff3)

erros[[b]]=cbind(erros[[b]], c(mean(diff1), mean(diff2), mean(diff3)), c(sd(diff1), sd(diff2), sd(diff3)))
print(n)
  }
  
}

erros[[1]]=erros[[1]][,-1]
erros[[2]]=erros[[2]][,-1]
erros[[3]]=erros[[3]][,-1]
erros[[1]]=data.frame(erros[[1]])
erros[[2]]=data.frame(erros[[2]])
erros[[3]]=data.frame(erros[[3]])
rownames(erros[[1]])=c("1","2","B Splines")
rownames(erros[[2]])=c("1","2","B Splines")
rownames(erros[[3]])=c("1","2","B Splines")
colnames(erros[[1]])=rep(c("EQM", "DP"), 4)
colnames(erros[[2]])=rep(c("EQM", "DP"), 4)
colnames(erros[[3]])=rep(c("EQM", "DP"), 4)
erros
