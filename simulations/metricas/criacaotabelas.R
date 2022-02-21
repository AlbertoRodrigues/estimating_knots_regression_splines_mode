require(xtable)

#Variancia 1
metricas=read.csv("\\Users\\Alberto\\Desktop\\ime-usp\\dissertacao_ime_mestrado\\simulations\\metricas\\metricas_1.csv")
metricas
rownames(metricas)=metricas[,1]
metricas=metricas[,-1]
colnames(metricas)=colnames(erros[[1]])
metricas

metricas2=round(rbind.data.frame(erros[[1]], metricas),2);metricas2
metricas_finais=data.frame(cbind(paste(metricas2[,1],"(", metricas2[,2],")", sep=""), paste(metricas2[,3],"(", metricas2[,4],")", sep=""), paste(metricas2[,5],"(", metricas2[,6],")", sep=""), paste(metricas2[,7],"(", metricas2[,8],")", sep="")))
colnames(metricas_finais)=c("300", "900", "1500", "3000")
rownames(metricas_finais)=rownames(metricas2)
metricas_finais
xtable(metricas_finais)

#Variancia 2
metricas=read.csv("\\Users\\Alberto\\Desktop\\ime-usp\\dissertacao_ime_mestrado\\simulations\\metricas\\metricas_2.csv")
metricas
rownames(metricas)=metricas[,1]
metricas=metricas[,-1]
colnames(metricas)=colnames(erros[[2]])
metricas

metricas2=round(rbind.data.frame(erros[[2]], metricas),2);metricas2
metricas_finais=data.frame(cbind(paste(metricas2[,1],"(", metricas2[,2],")", sep=""), paste(metricas2[,3],"(", metricas2[,4],")", sep=""), paste(metricas2[,5],"(", metricas2[,6],")", sep=""), paste(metricas2[,7],"(", metricas2[,8],")", sep="")))
colnames(metricas_finais)=c("300", "900", "1500", "3000")
rownames(metricas_finais)=rownames(metricas2)
metricas_finais
xtable(metricas_finais)

#Variancia 3
metricas=read.csv("\\Users\\Alberto\\Desktop\\ime-usp\\dissertacao_ime_mestrado\\simulations\\metricas\\metricas_3.csv")
metricas
rownames(metricas)=metricas[,1]
metricas=metricas[,-1]
colnames(metricas)=colnames(erros[[3]])
metricas

metricas2=round(rbind.data.frame(erros[[3]], metricas),2);metricas2
metricas_finais=data.frame(cbind(paste(metricas2[,1],"(", metricas2[,2],")", sep=""), paste(metricas2[,3],"(", metricas2[,4],")", sep=""), paste(metricas2[,5],"(", metricas2[,6],")", sep=""), paste(metricas2[,7],"(", metricas2[,8],")", sep="")))
colnames(metricas_finais)=c("300", "900", "1500", "3000")
rownames(metricas_finais)=rownames(metricas2)
metricas_finais
xtable(metricas_finais)
