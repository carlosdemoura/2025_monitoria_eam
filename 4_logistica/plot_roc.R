## predictors : fitted values
## gold       : observed values
plot.roc <- function(predictors,gold){
   sorted.predictors<-sort(unique(predictors))
   ll<-length(sorted.predictors)
   num.pos<-length(predictors[gold==1])
   num.neg<-length(predictors[gold==0])
   tnf<-rep(0,ll)
   tpf<-rep(0,ll)
   for(i in 1:ll){
     tnf[i]<-length(predictors[gold==0&predictors<=sorted.predictors[i]])/num.neg
     tpf[i]<-length(predictors[gold==1&predictors> sorted.predictors[i]])/num.pos 
   }
   plot(1-tnf,tpf,type="l",xlab="False Positive Fraction",ylab="True Positive Fraction",xlim=c(0,1),ylim=c(0,1),lwd=6,cex.axis=1.75,cex.lab=1.5)
}

# Funçao para geraçao dos elementos da curva ROC para dados binarios ("0" ou "1")
# [S, E, tau] = roc(y,out)
# Entrada:
#   y   : saida desejada ("0" ou "1")
#   out : saida do modelo (valor entre 0 e 1)
# Saida
#     S : Vetor de sensibilidade
#     E : Vetor de Especificidade
#   tau : Vetor de criterio de corte

roc <- function(y,out){
    tau <- sort(out, index.return=TRUE)
    n   <- length(tau$x);
    s   <- c()
    e   <- c()
    for(c in 1:n){
        aux = as.numeric(out >= tau$x[c]);
        s[c] = sum(y*aux)/sum(y);
        e[c] = sum(as.numeric(!y)*as.numeric(!aux))/sum(as.numeric(!y));
    }
    return( list(s=s,e=e,tau=tau$x) )
}