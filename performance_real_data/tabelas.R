require(xtable)

#Variancia 1
metricas=read.csv("\\Users\\Alberto\\Desktop\\ime-usp\\dissertacao_ime_mestrado\\performance_real_data\\metricas\\metricas_python.csv")
metricas
rownames(metricas)=metricas[,1]
metricas=metricas[,-1]
colnames(metricas)=colnames(erros)
metricas

metricas2=round(rbind.data.frame(erros, metricas),2);metricas2
metricas_finais=data.frame(cbind(paste(metricas2[,1],"(", metricas2[,2],")", sep=""), 
                                 paste(metricas2[,3],"(", metricas2[,4],")", sep=""), 
                                 paste(metricas2[,5],"(", metricas2[,6],")", sep=""), 
                                 paste(metricas2[,7],"(", metricas2[,8],")", sep=""),
                                 paste(metricas2[,9],"(", metricas2[,10],")", sep=""),
                                 paste(metricas2[,11],"(", metricas2[,12],")", sep="")))
colnames(metricas_finais)=c("airfoil", "auto_mpg", "concrete", "folds", "qsar", "valuation")
rownames(metricas_finais)=rownames(metricas2)
metricas_finais
xtable(metricas_finais)
