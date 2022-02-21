gera_y_simulado=function(n, X_pred, knots2, theta2, K2)
{
library(splines)
library(data.table)
library(ggplot2)

cria_f=function(alpha)
{
  cont=1
  fcost<<-""
  valores_iniciais<<-c()

  for(l in 1:K)
  {
    fcost<<-paste(fcost,"theta[",cont,"]*x[,",i,"]**",l,"+")
    cont=cont+1
  }
  valores_iniciais<<-append(valores_iniciais,rep(0,K))
  if(alpha>0)
  {
    for(j in 1:alpha)
    {
      fcost<<-paste(fcost,"theta[",cont,"]*((x[,",i,"]-theta[",cont+1,"])**",K,")*ifelse(x[,",i,"]>theta[",cont+1,"],1,0) +")
      cont=cont+2
      valores_iniciais<<-append(valores_iniciais,as.vector(c(0,knots[j])))
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
  for(l in 1:K)
  {
    dtheta[cont]=paste("2*mean((",fcost,"-y)*x[,",i,"]**",l,")") 
    cont=cont+1
  }
  if(alpha>0)
  {  
    for(j in 1:alpha)
    {
      dtheta[cont]=paste("2*mean((",fcost,"-y)*(((x[,",i,"]-theta[",cont+1,"])**",K,")*ifelse(x[,",i,"]>theta[",cont+1,"],1,0)))")
      dtheta[cont+1]=paste("2*mean((",fcost,"-y)*theta[",cont,"]*ifelse(x[,",i,"]>theta[",cont+1,"],",-K,"*(x[,",i,"]-theta[",cont+1,"])**",(K-1),",0))")  
      cont=cont+2
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

#Dados para gerar os parametros e nao mudar as estimativas independente do n
n2=4000
set.seed(7)
x1=sample(seq(0, 100, length.out = n2))
x2=sample(seq(0, 105, length.out = n2))
x3=sample(seq(0, 70,  length.out = n2))

X2 = cbind(x1, x2, x3)
#X2 = matrix(x2, ncol=1)

p<<-ncol(X2)
y_simulado=rep(0, n)
set.seed(7)
ind_treino<<-sample.int(n,0.7*n)
#y=matrix(0,length(ind_treino),p)
param_vector<<-list()
ypred=list()
#k=1
for(k in 1:p)
{
  #Pelo motivo da função acima e eu fazer por variavel
  i=1
  #x = X2[,k]
  genSpline = function(x, knots, degree, theta) 
  {
    basis = bs(x = x, knots = knots, degree = degree,
               Boundary.knots = c(0,max(x)), intercept = T)
    
    y.spline = basis %*% theta
    
    dt = data.table(x, y.spline = as.vector(y.spline))
    
    return(list(dt = dt, basis = basis, knots = knots))
  }
  
  knots = knots2[[k]]
  theta = theta2[[k]]
  
  alpha=c(length(knots))
  K=K2[k]
  #V=V2[k]
  
  X=matrix(X2[,k],ncol=1)
  sdata = genSpline(X, knots, 3, theta)
  y=sdata$dt$y.spline

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
      dtheta2[i]=eval(f2, envir=list(theta=theta,x=X,y=y))
    }
    return(dtheta2)
  }
  
  modelo=optim(valores_iniciais ,fn=f,gr=dfparam, method="BFGS", control=list(trace=TRUE,maxit=150))
  assign(paste("param",k,sep="_"), modelo$par)
  param_text=parse(text= paste("param",k,sep="_"))
  param_vector[[k]]<<-eval(param_text)
  
  assign(paste("f_y",k,sep="_"), function(x)
  {
    if(is.null(dim(x)))
    {
      x=matrix(x,ncol=1)
    }
    f_est = parse(text = fcost)
    return(eval(f_est, envir=list(theta=param_vector[[k]],x=x)))
  })
  
  f_y_text=paste(paste("f_y",k,sep="_"),"(x)",sep="")
  f_y_exp=parse(text= f_y_text)
  
  #ypred[[k]]=eval(f_y_exp, envir = list(x=X_treino))
  #y_simulado[ind_treino]=y_simulado[ind_treino]+eval(f_y_exp, envir = list(x=X_pred[ind_treino,]))
  #y_simulado[-ind_treino]=y_simulado[-ind_treino]+eval(f_y_exp, envir = list(x=X_pred[ind_treino,]))
  y_simulado=y_simulado+eval(f_y_exp, envir = list(x=X_pred))
  
  #grafico[k]=(ggplot()+
  #    geom_point(aes(x=X_treino, y = eval(f_y_exp, envir = list(x=X_treino))+rnorm(140,0,1)), color = "black")  +
  #    geom_point(aes(x=X_val, y = eval(f_y_exp, envir = list(x=X_val))+rnorm(60,0,1)), color = "darkgreen")  +
  #    #stat_function(fun = f_y,color="#d11141",lwd=1.3)+
  #    labs(x="x",y="y")+
  #    scale_x_continuous(breaks=round(param[ind_param_pen],2))+
      #geom_vline(xintercept = round(param[ind_param_pen],2),linetype="dashed")+
  #    theme_test())
  
}

#y_simulado=y_simulado
return(y_simulado)
}


n2=c(300, 900, 1500, 3000)
var_ruido=c(1, 2, 3)

knots2 = list(c(18,76.5), c(84.4), c(90))
theta2 = list(c(0.2,4.1, 5,1,-1.5,2.3),c(0.2,1.1, 5,1,-5.5),c(0.2,1.1, 5,1,-5.5))
K2=c(3,1,1)
y_simulado=gera_y_simulado(n, X_real, knots2, theta2, K2)
param_vector
#n=300
for(n in n2)
{
  set.seed(7)
  x1=sample(seq(0, 100, length.out = n))
  x2=sample(seq(0, 105, length.out = n))
  x3=sample(seq(0, 70,  length.out = n))
  
  X_real = cbind(x1, x2, x3)
  for(var in var_ruido)
  {
    y_simulado=gera_y_simulado(n, X_real, knots2, theta2, K2)
    set.seed(7)
    y_simulado=y_simulado+rnorm(length(y_simulado),0, var)
    
    treino=cbind.data.frame(X_real[ind_treino,], y_simulado[ind_treino])
    teste=cbind.data.frame(X_real[-ind_treino,], y_simulado[-ind_treino])
    colnames(treino)[4]="y"
    colnames(teste)[4]="y"
    
    #Exportação para treino dos modelos em python
    write.csv(treino, file=paste("\\Users\\Alberto\\Desktop\\ime-usp\\dissertacao_ime_mestrado\\simulations\\data\\treino", n, var,".csv",sep="_"),row.names = F)
    write.csv(teste, file=paste("\\Users\\Alberto\\Desktop\\ime-usp\\dissertacao_ime_mestrado\\simulations\\data\\teste", n, var,".csv",sep="_"),row.names = F)
  }
}

y_simulado
summary(y_simulado)
#param_1
#param_2
#param_3

ggplot()+aes(x1 , y_simulado)+geom_point()
ggplot()+aes(x2 , y_simulado)+geom_point()
ggplot()+aes(x3 , y_simulado)+geom_point()


#Dads de treino e teste

#(ggplot()+
#    geom_point(aes(x=X_treino, y = f_y_1(X_treino)+rnorm(length(X_treino),0,1)), color = "black")  +
#    geom_point(aes(x=X_val, y = +rnorm(length(X_val),0,1)), color = "darkgreen")  +
#    stat_function(fun = f_y_3,color="#d11141",lwd=1.3)+
#    labs(x="x",y="y")+
#    scale_x_continuous(breaks=round(param[ind_param_pen],2))+
#    geom_vline(xintercept = round(param[ind_param_pen],2),linetype="dashed")+
#    theme_test())

