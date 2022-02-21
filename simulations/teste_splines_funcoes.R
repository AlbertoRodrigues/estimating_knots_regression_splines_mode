#B Splines
X_bsplines=matrix(0,nrow=length(ind_treino))
X_teste=matrix(0,nrow=nrow(X2)-length(ind_treino))

alpha=c(0,1,2,3)
degree=c(1,2,3)
alpha2=c()
degree2=c()
for(j in 1:ncol(X2))
{
  alpha2[j]=sample(alpha, 1)
  knot=as.vector(quantile(X2[,j],seq(1,alpha2[j])/(alpha2[j]+1)))
  if(alpha2[j]==0)
  {
    knot=NULL
  }
  degree2[j]=sample(degree, 1)
  X_bsplines=cbind(X_bsplines, bs(X2[ind_treino,j],knots=knot , degree=degree2[j]))
  X_teste=cbind(X_teste, bs(X2[-ind_treino,j],knots=knot , degree=degree2[j]))
  colnames(X_bsplines)=seq(0,ncol(X_bsplines)-1)
}
X_bsplines=X_bsplines[,-1]
X_teste=X_teste[,-1]
dim(X_bsplines)
dim(X_teste)
degree2
alpha2
#bs(X2[ind_treino,1], knots = c(10, 50), degree=1)

model=lm(y_simulado[ind_treino]~X_bsplines)
summary(model)
ypred=as.vector(cbind(rep(1, nrow(X_teste)),X_teste)%*%t(t(model$coefficients)))
diff=(y_simulado[-ind_treino]-ypred)**2
mean(diff)
sd(diff)



#Natural Splines
X_nsplines=matrix(0,nrow=length(ind_treino))
X_teste=matrix(0,nrow=nrow(X2)-length(ind_treino))

alpha=c(0,1,2,3)
degree=c(1,2,3)
alpha2=c()
degree2=c()
for(j in 1:ncol(X2))
{
  alpha2[j]=sample(alpha, 1)
  knot=as.vector(quantile(X2[,j],seq(1,alpha2[j])/(alpha2[j]+1)))
  if(alpha2[j]==0)
  {
    knot=NULL
  }
  degree2[j]=sample(degree, 1)
  X_nsplines=cbind(X_nsplines, ns(X2[ind_treino,j],knots=knot , degree=degree2[j]))
  X_teste=cbind(X_teste, ns(X2[-ind_treino,j],knots=knot , degree=degree2[j]))
  colnames(X_nsplines)=seq(0,ncol(X_bsplines)-1)
}
X_nsplines=X_nsplines[,-1]
X_teste=X_teste[,-1]
dim(X_nsplines)
dim(X_teste)
degree2
alpha2
#bs(X2[ind_treino,1], knots = c(10, 50), degree=1)

model=lm(y_simulado[ind_treino]~X_nsplines)
summary(model)
ypred=as.vector(cbind(rep(1, nrow(X_teste)),X_teste)%*%t(t(model$coefficients)))
diff=(y_simulado[-ind_treino]-ypred)**2
mean(diff)
sd(diff)

#Random search

erro1=c()
erro2=c()
dp1=c()
dp2=c()

alpha=c(0,1,2,3)
degree=c(1,2,3)
alpha2=list()
degree2=list()


quant_random_search=4
#k=1
for(k in 1:quant_random_search)
  {
  # CV
  erro_cv1=c()
  erro_cv2=c()
  dp_cv1=c()
  dp_cv2=c()
  
  #s=1
  alpha2[[k]]=sample(alpha, p)
  degree2[[k]]=sample(degree, p)
  
  for(s in 1:length(folds))
  {
    ind_val2=as.numeric(rownames(as.data.frame(folds[s])))
    ind_treino2=setdiff(1:nrow(X2[ind_treino,]),ind_val2)
    X_treino2=X2[ind_treino,][ind_treino2,]
    X_val2=X2[ind_treino,][ind_val2,]
    y_simulado_treino_cv=y_simulado[ind_treino][ind_treino2]
    y_simulado_val_cv=y_simulado[ind_treino][ind_val2]
    
    X_bsplines=matrix(0,nrow=length(ind_treino2))
    X_teste=matrix(0,nrow=length(ind_val2))
    
    
    #j=1
    for(j in 1:ncol(X2))
    {
      knot=as.vector(quantile(X_treino2[,j],seq(1,alpha2[[k]][j])/(alpha2[[k]][j]+1)))
      if(alpha2[[k]][j]==0)
      {
        knot=NULL
      }
      X_bsplines=cbind(X_bsplines, bs(X_treino2[,j],knots=knot , degree=degree2[[k]][j]))
      X_teste=cbind(X_teste, bs(X_val2[,j],knots=knot , degree=degree2[[k]][j]))
      colnames(X_bsplines)=seq(0,ncol(X_bsplines)-1)
    }
    X_bsplines=X_bsplines[,-1]
    X_teste=X_teste[,-1]
    
    model=lm(y_simulado_treino_cv~X_bsplines)
    ypred=as.vector(cbind(rep(1, nrow(X_teste)),X_teste)%*%t(t(model$coefficients)))
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

X_bsplines=matrix(0,nrow=length(ind_treino))
X_teste=matrix(0,nrow=nrow(X2)-length(ind_treino))


#j=1
for(j in 1:ncol(X2))
{
  knot=as.vector(quantile(X2[ind_treino,j],seq(1,alpha_final[j])/(alpha_final[j]+1)))
  if(alpha_final[j]==0)
  {
    knot=NULL
  }
  X_bsplines=cbind(X_bsplines, bs(X2[ind_treino,j],knots=knot , degree=degree_final[j]))
  X_teste=cbind(X_teste, bs(X2[-ind_treino,j],knots=knot , degree=degree_final[j]))
  colnames(X_bsplines)=seq(0,ncol(X_bsplines)-1)
}
X_bsplines=X_bsplines[,-1]
X_teste=X_teste[,-1]

model=lm(y_simulado[ind_treino]~X_bsplines)
ypred=as.vector(cbind(rep(1, nrow(X_teste)),X_teste)%*%t(t(model$coefficients)))
diff=(y_simulado[-ind_treino]-ypred)**2
mean(diff)
sd(diff)
