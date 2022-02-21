#Auto mpg
dados=read.csv("\\Users\\Alberto\\Desktop\\ime-usp\\dissertacao_ime_mestrado\\performance_real_data\\data\\auto_mpg.csv")
nrow(dados[!complete.cases(dados),])/nrow(dados)
colnames(dados)
dados=dados[,c("mpg", "displacement", "horsepower", "weight", "acceleration")]
colnames(dados)[1]="y"
set.seed(7)
ind=sample(nrow(dados), 0.7*nrow(dados))
treino=dados[ind,]
teste=dados[-ind,]

write.csv(treino, file="\\Users\\Alberto\\Desktop\\ime-usp\\dissertacao_ime_mestrado\\performance_real_data\\data\\auto_mpg_treino.csv",row.names = F)
write.csv(teste, file="\\Users\\Alberto\\Desktop\\ime-usp\\dissertacao_ime_mestrado\\performance_real_data\\data\\auto_mpg_teste.csv",row.names = F)

#Rever como selecionar as variaveis
#Concrete
dados=read.csv("\\Users\\Alberto\\Desktop\\ime-usp\\dissertacao_ime_mestrado\\performance_real_data\\data\\concrete.csv")
nrow(dados[!complete.cases(dados),])/nrow(dados)
colnames(dados)[9]="y"
set.seed(7)
ind=sample(nrow(dados), 0.7*nrow(dados))
treino=dados[ind,]
teste=dados[-ind,]

write.csv(treino, file="\\Users\\Alberto\\Desktop\\ime-usp\\dissertacao_ime_mestrado\\performance_real_data\\data\\concrete_treino.csv",row.names = F)
write.csv(teste, file="\\Users\\Alberto\\Desktop\\ime-usp\\dissertacao_ime_mestrado\\performance_real_data\\data\\concrete_teste.csv",row.names = F)

#Folds
dados=read.csv("\\Users\\Alberto\\Desktop\\ime-usp\\dissertacao_ime_mestrado\\performance_real_data\\data\\folds.csv")
nrow(dados[!complete.cases(dados),])/nrow(dados)
colnames(dados)[5]="y"
set.seed(7)
ind=sample(nrow(dados), 0.7*nrow(dados))
treino=dados[ind,]
teste=dados[-ind,]

write.csv(treino, file="\\Users\\Alberto\\Desktop\\ime-usp\\dissertacao_ime_mestrado\\performance_real_data\\data\\folds_treino.csv",row.names = F)
write.csv(teste, file="\\Users\\Alberto\\Desktop\\ime-usp\\dissertacao_ime_mestrado\\performance_real_data\\data\\folds_teste.csv",row.names = F)

#qsar
dados=read.csv("\\Users\\Alberto\\Desktop\\ime-usp\\dissertacao_ime_mestrado\\performance_real_data\\data\\qsar.csv", sep = ";", header = F)
nrow(dados[!complete.cases(dados),])/nrow(dados)
summary(dados)
dados=dados[,c("V1", "V2", "V4", "V5", "V6", "V9")]
colnames(dados)[6]="y"
set.seed(7)
ind=sample(nrow(dados), 0.7*nrow(dados))
treino=dados[ind,]
teste=dados[-ind,]

write.csv(treino, file="\\Users\\Alberto\\Desktop\\ime-usp\\dissertacao_ime_mestrado\\performance_real_data\\data\\qsar_treino.csv",row.names = F)
write.csv(teste, file="\\Users\\Alberto\\Desktop\\ime-usp\\dissertacao_ime_mestrado\\performance_real_data\\data\\qsar_teste.csv",row.names = F)

#Real state valuation
dados=read.csv("\\Users\\Alberto\\Desktop\\ime-usp\\dissertacao_ime_mestrado\\performance_real_data\\data\\valuation.csv")
nrow(dados[!complete.cases(dados),])/nrow(dados)
dados=dados[,-c(1,2)]
colnames(dados)[6]="y"
set.seed(7)
ind=sample(nrow(dados), 0.7*nrow(dados))
treino=dados[ind,]
teste=dados[-ind,]

write.csv(treino, file="\\Users\\Alberto\\Desktop\\ime-usp\\dissertacao_ime_mestrado\\performance_real_data\\data\\valuation_treino.csv",row.names = F)
write.csv(teste, file="\\Users\\Alberto\\Desktop\\ime-usp\\dissertacao_ime_mestrado\\performance_real_data\\data\\valuation_teste.csv",row.names = F)

#Yatch
#Muitos dados faltantes, nao usar por enquanto
dados=read.csv("\\Users\\Alberto\\Desktop\\ime-usp\\dissertacao_ime_mestrado\\performance_real_data\\data\\yacht.csv", header=F, sep=" ")
nrow(dados[!complete.cases(dados),])/nrow(dados)
set.seed(7)
ind=sample(nrow(dados), 0.7*nrow(dados))
treino=dados[ind,]
teste=dados[-ind,]

write.csv(treino, file="\\Users\\Alberto\\Desktop\\ime-usp\\dissertacao_ime_mestrado\\performance_real_data\\data\\yatch_treino.csv",row.names = F)
write.csv(teste, file="\\Users\\Alberto\\Desktop\\ime-usp\\dissertacao_ime_mestrado\\performance_real_data\\data\\yatch_teste.csv",row.names = F)


# airfoil
dados=read.delim("\\Users\\Alberto\\Desktop\\ime-usp\\dissertacao_ime_mestrado\\performance_real_data\\data\\airfoil_self_noise.txt", header=F)
nrow(dados[!complete.cases(dados),])/nrow(dados)
colnames(dados)[6]="y"
set.seed(7)
ind=sample(nrow(dados), 0.7*nrow(dados))
treino=dados[ind,]
teste=dados[-ind,]

write.csv(treino, file="\\Users\\Alberto\\Desktop\\ime-usp\\dissertacao_ime_mestrado\\performance_real_data\\data\\airfoil_treino.csv",row.names = F)
write.csv(teste, file="\\Users\\Alberto\\Desktop\\ime-usp\\dissertacao_ime_mestrado\\performance_real_data\\data\\airfoil_teste.csv",row.names = F)

