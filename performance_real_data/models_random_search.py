import numpy as np
import pandas as pd
from sklearn.model_selection import KFold, RandomizedSearchCV
from sklearn.linear_model import Ridge
from sklearn.ensemble import RandomForestRegressor, AdaBoostRegressor
from sklearn.neural_network import MLPRegressor
from scipy.stats import uniform


treino_nomes=["airfoil_treino.csv", "auto_mpg_treino.csv", "concrete_treino.csv", "folds_treino.csv", "qsar_treino.csv", "valuation_treino.csv"]
teste_nomes=["airfoil_teste.csv", "auto_mpg_teste.csv", "concrete_teste.csv", "folds_teste.csv", "qsar_teste.csv", "valuation_teste.csv"]

metricas=np.zeros((4,1))
#i=0
for i in range(len(treino_nomes)):
    #Data
    train=pd.read_csv(treino_nomes[i])
    test=pd.read_csv(teste_nomes[i])
    
    y_train=train["y"].values
    X_train=train.drop(["y"], axis=1).values
    
    y_test=test["y"].values
    X_test=test.drop(["y"], axis=1).values
    
    
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
    
    with open("\\Users\\Alberto\\Desktop\\ime-usp\\dissertacao_ime_mestrado\\performance_real_data\\hiperparametros\\hiper_python_"+treino_nomes[i].split("_")[0]+".txt", "w") as output:
            output.write(str(hiper))
    print(i)

metricas            
metricas=metricas[:,np.arange(1,13)]
metricas=pd.DataFrame(metricas
            , index=["Ridge","Random Forest", "AdaBoost", "MLP"]
                 , columns=["EQM","DP","EQM","DP","EQM","DP","EQM",
                            "DP","EQM","DP","EQM","DP"])

metricas.to_csv("\\Users\\Alberto\\Desktop\\ime-usp\\dissertacao_ime_mestrado\\performance_real_data\\metricas\\metricas_python.csv")