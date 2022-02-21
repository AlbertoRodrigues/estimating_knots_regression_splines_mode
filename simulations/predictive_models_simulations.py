import numpy as np
import pandas as pd
from sklearn.model_selection import KFold, RandomizedSearchCV
from sklearn.linear_model import Ridge
from sklearn.ensemble import RandomForestRegressor, AdaBoostRegressor
from sklearn.neural_network import MLPRegressor
from scipy.stats import uniform
#j=1
for j in [1, 2, 3]:
    
    treino_nomes=["treino_300_"+str(j)+"_.csv", "treino_900_"+str(j)+"_.csv", "treino_1500_"+str(j)+"_.csv", "treino_3000_"+str(j)+"_.csv"]
    teste_nomes=["teste_300_"+str(j)+"_.csv", "teste_900_"+str(j)+"_.csv", "teste_1500_"+str(j)+"_.csv", "teste_3000_"+str(j)+"_.csv"]

    metricas=np.zeros((4,1))
    #i=0
    for i in range(len(treino_nomes)):
        #Data
        train=pd.read_csv(treino_nomes[i])
        test=pd.read_csv(teste_nomes[i])
        
        y_train=train["y"].values
        X_train=train.values[:,[0,1,2]]
        
        y_test=test["y"].values
        X_test=test.values[:,[0,1,2]]
        
        
        #CV
        cv=KFold(n_splits=5, random_state=0, shuffle=True)
        
        #Models
        eqm=[]
        dp=[]
        hiper=[]
        #Regressão Ridge
        modelo=Ridge()
        hiperp = {"alpha":uniform(loc=0,scale=10)}
        otim = RandomizedSearchCV(modelo, hiperp,cv=cv, n_iter = 30
                                        ,scoring="neg_mean_squared_error", random_state=0)
        otim.fit(X_train, y_train)
        hiper.append(otim.best_params_)
        modelo=Ridge(**otim.best_params_)
        modelo.fit(X_train, y_train)
        
        diff=(y_test - modelo.predict(X_test))**2
        eqm.append(np.mean(diff))
        dp.append(np.std(diff))
        
        
        #Random Forest
        modelo=RandomForestRegressor()
        hiperp = {"n_estimators":[50, 80, 100, 150],
                  "max_depth":[2,3,4],
                  "min_samples_split":[10,15,20,30],
                  "min_samples_leaf":[5,10,15,20,25,30,50]}
        otim = RandomizedSearchCV(modelo, hiperp,cv=cv, n_iter = 30
                                        ,scoring="neg_mean_squared_error", random_state=0)
        otim.fit(X_train, y_train)
        hiper.append(otim.best_params_)
        modelo=RandomForestRegressor(**otim.best_params_)
        modelo.fit(X_train, y_train)
        
        diff=(y_test - modelo.predict(X_test))**2
        eqm.append(np.mean(diff))
        dp.append(np.std(diff))
        
        #AdaBoost
        modelo=AdaBoostRegressor()
        hiperp = {"n_estimators":[50, 80, 100, 150],
                  "learning_rate":uniform(loc=0.9,scale=1.5)}
        otim = RandomizedSearchCV(modelo, hiperp,cv=cv, n_iter = 30
                                        ,scoring="neg_mean_squared_error", random_state=0)
        otim.fit(X_train, y_train)
        hiper.append(otim.best_params_)
        modelo=AdaBoostRegressor(**otim.best_params_)
        modelo.fit(X_train, y_train)
        
        diff=(y_test - modelo.predict(X_test))**2
        eqm.append(np.mean(diff))
        dp.append(np.std(diff))
        
        # Redes Neurais
        
        modelo=MLPRegressor()
        hiperp = {"hidden_layer_sizes":[(10),(30),(50),(10, 10),(30, 30), (50, 50)],
                  "activation":["identity","relu"],
                  "solver":["lbfgs"],
                  "learning_rate_init":[0.01, 0.03, 0.1, 0.3]}
        
        otim = RandomizedSearchCV(modelo, hiperp,cv=cv, n_iter = 30
                                        ,scoring="neg_mean_squared_error", random_state=0)
        otim.fit(X_train, y_train)
        hiper.append(otim.best_params_)
        modelo=MLPRegressor(**otim.best_params_)
        modelo.fit(X_train, y_train)
        
        diff=(y_test - modelo.predict(X_test))**2
        eqm.append(np.mean(diff))
        dp.append(np.std(diff))
        
        metricas=np.column_stack((metricas, eqm, dp))
        
        with open("\\Users\\Alberto\\Desktop\\ime-usp\\dissertacao_ime_mestrado\\simulations\\hiperparametros\\hiper_python_"+treino_nomes[i][7:13]+".txt", "w") as output:
            output.write(str(hiper))
    metricas=metricas[:,np.arange(1,9)]
    metricas=pd.DataFrame(metricas
                , index=["Ridge","Random Forest", "AdaBoost", "MLP"]
                     , columns=["EQM","DP","EQM","DP","EQM","DP","EQM","DP"])
    
    metricas.to_csv("\\Users\\Alberto\\Desktop\\ime-usp\\dissertacao_ime_mestrado\\simulations\\metricas\\metricas_"+str(j)+".csv")
    print(j)