gera_res=function(knots, theta, K, quantidade_simulacoes=100, ix)
{
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
          valores_iniciais<<-append(valores_iniciais,as.vector(c(0,quantile(X_treino[,i],j/(alpha[i]+1)))))
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
          valores_iniciais<<-append(valores_iniciais,as.vector(c(0,quantile(X_treino[,i],j/(alpha[i]+1)))))
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
  
  genSpline = function(x, knots, degree, theta) {
    
    basis = bs(x = x, knots = knots, degree = degree,
               Boundary.knots = c(0,100), intercept = T)
    
    y.spline = basis %*% theta
    
    dt = data.table(x, y.spline = as.vector(y.spline))
    
    return(list(dt = dt, basis = basis, knots = knots))
    
  }
  x = seq(0, 100, length.out = 500)
  
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
      dtheta2[i]=eval(f2, envir=list(theta=theta,x=X,y=y))
    }
    return(dtheta2)
  }
  
  modelo=optim(valores_iniciais ,fn=f,gr=dfparam, method="BFGS", control=list(trace=FALSE,maxit=150))
  param=modelo$par
  
  list_lambda=c(0,0.01,0.03,0.1,0.3,1,3,10,30)
  num_alpha_max=5
  n2=c(300, 900, 1500, 3000)
  number_knots1_prop=list()
  lambda_knots1_prop=list()
  
  number_knots2_prop=list()
  lambda_knots2_prop=list()
  
  erro_simulacoes_geral1=c()
  dp_simulacoes_geral1=c()
  
  erro_simulacoes_geral2=c()
  dp_simulacoes_geral2=c()
  
  erro_simulacoes_geral3=c()
  dp_simulacoes_geral3=c()
  for(v in 1:length(n2))
  {
    number_knots1=c()
    lambda_knots1=c()
    
    number_knots2=c()
    lambda_knots2=c()
    
    n=n2[v]
    x = seq(0, 100, length.out = n)
    X=matrix(x,ncol=1)
    set.seed(v)
    ind_treino=sample.int(nrow(X),0.7*nrow(X))
    X_treino=matrix(X[ind_treino,])
    X_val=matrix(X[-ind_treino,])
    
    erro_simulacoes1=c()
    dp_simulacoes1=c()
    
    erro_simulacoes2=c()
    dp_simulacoes2=c()
    
    erro_simulacoes3=c()
    dp_simulacoes3=c()

    for(b in 1:quantidade_simulacoes)
    {
      set.seed(b)
      ruido_treino=rnorm(dim(X_treino)[1],0,2)
      ruido_val=rnorm(dim(X_val)[1],0,2)
      
      y_treino_real=f_y(X_treino)+ruido_treino
      y_val_real=f_y(X_val)+ruido_val
      
      dados1=data.frame("alpha"=0,"lambda"=0,"loss"=0,"erro_cv"=0,"dp_cv"=0)
      dados2=data.frame("alpha"=0,"lambda"=0,"loss"=0,"erro_cv"=0,"dp_cv"=0)

      folds=kFold(cbind.data.frame(X_treino,X_treino+1),k=5)

      for(k in 1:length(list_lambda))
      {
        lambda=list_lambda[k]
        y=y_treino_real
        
        vetor_lambda1=c()
        list_alpha_iter1=c()
        loss1=c()
        
        vetor_lambda2=c()
        list_alpha_iter2=c()
        loss2=c()

        for(m in 0:num_alpha_max)
        {
          alpha=m
          
          f_cost=cria_f1(alpha, lambda)
          
          f=function(theta)
          {
            f1 = parse(text = f_cost)
            return(eval(f1, envir=list(theta=theta,x=X_treino,y=y,alpha=alpha,lambda=lambda,indices=ind_param_pen)))
          }
          
          dtheta=cria_df1(alpha, lambda)
          
          dfparam=function(theta)
          {
            dtheta2=c()
            for(i in 1:length(dtheta))
            {
              f2 = parse(text = dtheta[i])
              dtheta2[i]=eval(f2, envir=list(theta=theta,x=X_treino,y=y,lambda=lambda, alpha=alpha))
            }
            return(dtheta2)
          }
          
          
          modelo1=optim(valores_iniciais ,fn=f,gr=dfparam, method="BFGS", control=list(trace=FALSE,maxit=150))
          
          loss1=append(loss1, modelo1$value) 
          vetor_lambda1=append(vetor_lambda1, lambda) 
          list_alpha_iter1=append(list_alpha_iter1,alpha) 
          if(modelo1$value>min(loss1))
          {
            break
          }
        }

      for(m in 0:num_alpha_max)
        {
            alpha=m
            f_cost=cria_f2(alpha, lambda)
          
            f=function(theta)
            {
              f1 = parse(text = f_cost)
              return(eval(f1, envir=list(theta=theta,x=X_treino,y=y,alpha=alpha, lambda=lambda, n=nrow(X_treino))))
            }
            
            dtheta=cria_df2(alpha, lambda)
            
            dfparam=function(theta)
            {
              dtheta2=c()
              for(i in 1:length(dtheta))
              {
                f2 = parse(text = dtheta[i])
                dtheta2[i]=eval(f2, envir=list(theta=theta,x=X_treino,y=y))
              }
              return(dtheta2)
            }
            
            modelo2=optim(valores_iniciais ,fn=f,gr=dfparam, method="BFGS", control=list(trace=FALSE,maxit=150))
            
            loss2=append(loss2, modelo2$value) 
            vetor_lambda2=append(vetor_lambda2, lambda) 
            list_alpha_iter2=append(list_alpha_iter2,alpha) 
            if(modelo2$value>min(loss2))
            {
              break
            }
        }
        
        dados1[k,]=cbind.data.frame(list_alpha_iter1, vetor_lambda1, loss1,0,0)[order(loss1),][1,]
        dados2[k,]=cbind.data.frame(list_alpha_iter2, vetor_lambda2, loss2,0,0)[order(loss2),][1,]
        
        erro_treino_cv1=c()
        erro_val_cv1=c()
        dp_treino_cv1=c()
        dp_val_cv1=c()
        alpha1=dados1[k,"alpha"]
        
        erro_treino_cv2=c()
        erro_val_cv2=c()
        dp_treino_cv2=c()
        dp_val_cv2=c()
        alpha2=dados2[k,"alpha"]
        for(s in 1:length(folds))
        {
          ind_val=as.numeric(rownames(as.data.frame(folds[s])))
          ind_treino=setdiff(1:nrow(X_treino),ind_val)
          
          y_treino_real_cv=y_treino_real[ind_treino]
          y_val_real_cv=y_treino_real[ind_val]
          
          X_val2=as.matrix(X_treino[ind_val,])
          X_treino2=as.matrix(X_treino[ind_treino,])
          y=y_treino_real[ind_treino]
          
          f_cost=cria_f1(alpha1, lambda)
          f=function(theta)
          {
            f1 = parse(text = f_cost)
            return(eval(f1, envir=list(theta=theta,x=X_treino2,y=y,alpha=alpha1,lambda=lambda,indices=ind_param_pen)))
          }
          
          dtheta=cria_df1(alpha1, lambda)
          dfparam=function(theta)
          {
            dtheta2=c()
            for(i in 1:length(dtheta))
            {
              f2 = parse(text = dtheta[i])
              dtheta2[i]=eval(f2, envir=list(theta=theta,x=X_treino2,y=y, lambda=lambda, alpha=alpha1))
            }
            return(dtheta2)
          }
          
          modelo1=optim(valores_iniciais ,fn=f,gr=dfparam, method="BFGS", control=list(trace=FALSE,maxit=150))
          param2=modelo1$par
          
          y_treino_pred=f_y2(X_treino2)
          y_val_pred=f_y2(X_val2)
          
          diff1=(y_treino_pred-y_treino_real_cv)**2
          diff2=(y_val_pred-y_val_real_cv)**2
          
          erro_treino_cv1[s]=mean(diff1)
          erro_val_cv1[s]=mean(diff2)
          
          dp_treino_cv1[s]=sd(diff1)
          dp_val_cv1[s]=sd(diff2)
          
          f_cost=cria_f2(alpha2, lambda)
          f=function(theta)
          {
            f1 = parse(text = f_cost)
            return(eval(f1, envir=list(theta=theta,x=X_treino2,y=y,alpha=alpha2, lambda=lambda, n=nrow(X_treino2))))
          }
          
          dtheta=cria_df2(alpha2, lambda)
          dfparam=function(theta)
          {
            dtheta2=c()
            for(i in 1:length(dtheta))
            {
              f2 = parse(text = dtheta[i])
              dtheta2[i]=eval(f2, envir=list(theta=theta,x=X_treino2,y=y))
            }
            return(dtheta2)
          }
          
          modelo2=optim(valores_iniciais ,fn=f,gr=dfparam, method="BFGS", control=list(trace=FALSE,maxit=150))
          
          param2=modelo2$par
          
          y_treino_pred=f_y2(X_treino2)
          y_val_pred=f_y2(X_val2)
          
          diff1=(y_treino_pred-y_treino_real_cv)**2
          diff2=(y_val_pred-y_val_real_cv)**2
          
          erro_treino_cv2[s]=mean(diff1)
          erro_val_cv2[s]=mean(diff2)
          
          dp_treino_cv2[s]=sd(diff1)
          dp_val_cv2[s]=sd(diff2)
          
        }
        dados1[k,"erro_cv"]=mean(erro_val_cv1)
        dados1[k,"dp_cv"]=mean(dp_val_cv1)
        
        dados2[k,"erro_cv"]=mean(erro_val_cv2)
        dados2[k,"dp_cv"]=mean(dp_val_cv2)
      }
      number_knots1[b]=dados1[order(dados1$erro_cv),"alpha"][1]
      lambda_knots1[b]=dados1[order(dados1$erro_cv),"lambda"][1]
      alpha1=number_knots1[b]
      lambda1=lambda_knots1[b]
      
      f_cost=cria_f1(alpha1, lambda1)
      f=function(theta)
      {
        f1 = parse(text = f_cost)
        return(eval(f1, envir=list(theta=theta,x=X_treino,y=y,alpha=alpha1,lambda=lambda1,indices=ind_param_pen)))
      }
      
      dtheta=cria_df1(alpha1, lambda1)
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
      
      y=y_treino_real
      modelo1=optim(valores_iniciais ,fn=f,gr=dfparam, method="BFGS", control=list(trace=FALSE,maxit=150))
      
      param2=modelo1$par
      y_pred_test=f_y2(X_val)
      
      diff=(y_pred_test-y_val_real)**2
      erro_simulacoes1[b]=mean(diff)
      dp_simulacoes1[b]=sd(diff)
      
      number_knots2[b]=dados2[order(dados2$erro_cv),"alpha"][1]
      lambda_knots2[b]=dados2[order(dados2$erro_cv),"lambda"][1]
      alpha2=number_knots2[b]
      lambda2=lambda_knots2[b]
      
      f_cost=cria_f2(alpha2, lambda2)
      f=function(theta)
      {
        f1 = parse(text = f_cost)
        return(eval(f1, envir=list(theta=theta,x=X_treino,y=y,alpha=alpha2, lambda=lambda2, n=nrow(X_treino))))
      }
      
      dtheta=cria_df2(alpha2, lambda2)
      dfparam=function(theta)
      {
        dtheta2=c()
        for(i in 1:length(dtheta))
        {
          f2 = parse(text = dtheta[i])
          dtheta2[i]=eval(f2, envir=list(theta=theta,x=X_treino,y=y))
        }
        return(dtheta2)
      }
      
      y=y_treino_real
      modelo2=optim(valores_iniciais ,fn=f,gr=dfparam, method="BFGS", control=list(trace=FALSE,maxit=150))
      
      param2=modelo2$par
      y_pred_test=f_y2(X_val)
      
      diff=(y_pred_test-y_val_real)**2
      erro_simulacoes2[b]=mean(diff)
      dp_simulacoes2[b]=sd(diff)
      
      y_pred_test=f_y(X_val)
      
      diff=(y_pred_test-y_val_real)**2
      erro_simulacoes3[b]=mean(diff)
      dp_simulacoes3[b]=sd(diff)
    }
    
    number_knots1_prop[[v]]=table(number_knots1)/quantidade_simulacoes
    lambda_knots1_prop[[v]]=table(lambda_knots1)/quantidade_simulacoes
    
    number_knots2_prop[[v]]=table(number_knots2)/quantidade_simulacoes
    lambda_knots2_prop[[v]]=table(lambda_knots2)/quantidade_simulacoes
    
    erro_simulacoes_geral1[v]=mean(erro_simulacoes1)
    dp_simulacoes_geral1[v]=mean(dp_simulacoes1)
    
    erro_simulacoes_geral2[v]=mean(erro_simulacoes2)
    dp_simulacoes_geral2[v]=mean(dp_simulacoes2)
    
    erro_simulacoes_geral3[v]=mean(erro_simulacoes3)
    dp_simulacoes_geral3[v]=mean(dp_simulacoes3)
  }
  
  sink(file = paste("/home/ubuntu/work/R",ix,".txt",sep="_"))
  print(number_knots1_prop)
  print(lambda_knots1_prop)
  
  print(number_knots2_prop)
  print(lambda_knots2_prop)
  
  print(erro_simulacoes_geral1)
  print(dp_simulacoes_geral1)
  
  
  print(erro_simulacoes_geral2)
  print(dp_simulacoes_geral2)
  
  print(erro_simulacoes_geral3)
  print(dp_simulacoes_geral3)
  sink(file = NULL)
}
gera_res(c(84.4),c(0.2,1.1, 5,1,-5.5),1,100, 1)
gera_res(c(18,76.5),c(0.2,4.1, 5,1,-1.5,2.3),1,100,2)
gera_res(c(15,35,66.5),c(0.2,4.1, 5,1,-1.5,10.3,-7.4),1,100,3)
gera_res(c(84.4),c(0.2,1.1, 5,1,-5.5),3,100,4)
gera_res(c(18,76.5),c(0.2,4.1, 5,1,-1.5,2.3),3,100,5)
gera_res(c(15,35,66.5),c(0.2,4.1, 5,1,-1.5,10.3,-7.4),3,100,6)