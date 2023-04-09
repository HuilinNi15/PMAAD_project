library(readr)
library(dplyr)
library(cluster)
library(readxl)

path <- './Datasets/'
dd <- read_excel(paste0(path,"DadesMimmi.xlsx"))

attach(dd)

#obtenim els datasets per cada cluster
df2 <- subset(dd, select = -c(Crash_ID, Crash_Year, Crash_Time, Crash_Month, Crash_Date, Hour_of_Day, Charge))
#aplicar factor.R  
dist_mat <- daisy(df2, metric = "gower", stand=TRUE)
dist_mat <- dist_mat^2
hclust_avg <- hclust(dist_mat, method = 'ward.D2')
cut_avg <- cutree(hclust_avg, k = 6)
seeds_df_cl <- mutate(df2, cluster = cut_avg)
#Create data subsets
cluster1 <- data.frame(dd[which(seeds_df_cl$cluster==1), ])
cluster2 <- data.frame(dd[which(seeds_df_cl$cluster==2), ])
cluster3 <- data.frame(dd[which(seeds_df_cl$cluster==3), ])
cluster4 <- data.frame(dd[which(seeds_df_cl$cluster==4), ])
cluster5 <- data.frame(dd[which(seeds_df_cl$cluster==5), ])
cluster6 <- data.frame(dd[which(seeds_df_cl$cluster==6), ])


#Calcula els valor test de la variable Xnum per totes les modalitats del factor P
ValorTestXnum <- function(Xnum,P){
  #freq dis of fac
  nk <- as.vector(table(P));
  n <- sum(nk);
  #mitjanes x grups
  xk <- tapply(Xnum,P,mean);
  #valors test
  txk <- (xk-mean(Xnum))/(sd(Xnum)*sqrt((n-nk)/(n*nk)));
  #p-values
  pxk <- pt(txk,n-1,lower.tail=F);
  for(c in 1:length(levels(as.factor(P)))){if (pxk[c]>0.5){pxk[c]<-1-pxk[c]}}
  return (pxk)
}


ValorTestXquali <- function(P,Xquali){
  taula <- table(P,Xquali);
  n <- sum(taula); 
  pk <- apply(taula,1,sum)/n;
  pj <- apply(taula,2,sum)/n;
  pf <- taula/(n*pk);
  pjm <- matrix(data=pj,nrow=dim(pf)[1],ncol=dim(pf)[2], byrow=TRUE);
  dpf <- pf - pjm; 
  dvt <- sqrt(((1-pk)/(n*pk))%*%t(pj*(1-pj))); 
  #i hi ha divisions iguals a 0 dona NA i no funciona
  zkj <- dpf
  zkj[dpf!=0]<-dpf[dpf!=0]/dvt[dpf!=0]; 
  pzkj <- pnorm(zkj,lower.tail=F);
  for(c in 1:length(levels(as.factor(P)))){for (s in 1:length(levels(Xquali))){if (pzkj[c,s]> 0.5){pzkj[c,s]<-1- pzkj[c,s]}}}
  return (list(rowpf=pf,vtest=zkj,pval=pzkj))
}



# dades <- subset(cluster1, select = -c(Crash_ID,Person_Injury_Severity,Crash_Year, Crash_Time, Crash_Month, Crash_Date, Hour_of_Day, Charge))
# #dades <- subset(cluster2, select = -c(Crash_ID,Person_Injury_Severity,Crash_Year, Crash_Time, Crash_Month, Crash_Date, Hour_of_Day, Charge))
# #dades <- subset(cluster3, select = -c(Crash_ID,Person_Injury_Severity,Crash_Year, Crash_Time, Crash_Month, Crash_Date, Hour_of_Day, Charge))
dades <- subset(cluster4, select = -c(Crash_ID,Person_Injury_Severity,Crash_Year, Crash_Time, Crash_Month, Crash_Date, Hour_of_Day, Charge))
# #dades <- subset(cluster5, select = -c(Crash_ID,Person_Injury_Severity,Crash_Year, Crash_Time, Crash_Month, Crash_Date, Hour_of_Day, Charge))
# #dades <- subset(cluster6, select = -c(Crash_ID,Person_Injury_Severity,Crash_Year, Crash_Time, Crash_Month, Crash_Date, Hour_of_Day, Charge))


dim(dades)
View(dades)
K<-dim(dades)[2] #num de variables
par(ask=TRUE)


#P must contain the class variable
P<-dades[,7] #severity
nameP<-"Severity"

nc<-length(levels(factor(P)))
nc # numero de modalitats
pvalk <- matrix(data=0,nrow=nc,ncol=K, dimnames=list(levels(P),names(dades)))
n<-dim(dades)[1] #num de files

#dd[5] <- as.Date(unlist(dd[5]), format = "%Y-%m-%d") #transformem

for(k in 1:K){
  if (is.numeric(dades[,k])){ 
      print(paste("An�lisi per classes de la Variable:", names(dades)[k]))
      
      paleta<-rainbow(length(levels(as.factor(P))))
      boxplot(dades[,k]~P, main=paste("Boxplot of", names(dades)[k], "vs", nameP ), horizontal=FALSE,xlab=nameP,ylab=names(dades)[k], col=paleta,cex.axis = 0.8)
      
      barplot(tapply(dades[[k]], P, mean),main=paste("Means of", names(dades)[k], "by", nameP ),horizontal=TRUE,xlab=nameP,ylab=names(dades)[k],col=paleta,cex.axis = 0.7)
      abline(h=mean(dades[[k]]))
      legend(0,mean(dades[[k]]),"global mean",bty="n")
      print("Estad�stics per grups:")
      for(s in levels(as.factor(P))) {print(summary(dades[P==s,k]))}
      o<-oneway.test(dades[,k]~P)
      print(paste("p-valueANOVA:", o$p.value))
      kw<-kruskal.test(dades[,k]~P)
      print(paste("p-value Kruskal-Wallis:", kw$p.value))
      pvalk[,k]<-ValorTestXnum(dades[,k], P)
      print("p-values ValorsTest: ")
      print(pvalk[,k])
      }else{
        if(any(class(dd[, k])=="Date")){
          print(summary(dd[,k]))
          print(sd(dd[,k]))
          #decide breaks: weeks, months, quarters...
          hist(dd[,k],breaks="months",xlab=names(dades)[k])
        }else{#qualitatives
      print(paste("Variable", names(dades)[k]))
      table<-table(P,dades[,k])
      rowperc<-prop.table(table,1)

   colperc<-prop.table(table,2)

   dades[,k]<-as.factor(dades[,k])
   
   marg <- table(as.factor(P))/n
   print(append("Categories=",levels(as.factor(dades[,k]))))

   #from next plots, select one of them according to your practical case
   plot(marg,type="l",ylim=c(0,1),main=paste("Prop. of Severity by",names(dades)[k]),cex.axis = 0.8)
   paleta<-rainbow(length(levels(dades[,k])))
   for(c in 1:length(levels(dades[,k]))){lines(colperc[,c],col=paleta[c]) }
   
   #legend
   plot(marg,type="l",ylim=c(0,1),main=paste("Prop. of Severity by",names(dades)[k]),cex.axis = 0.8)
   for(c in 1:length(levels(dades[,k]))){lines(colperc[,c],col=paleta[c]) }
   legend("topright", levels(dades[,k]), col=paleta, lty=2, cex=0.6)
   
   #condicionades a classes
   plot(marg,type="n",ylim=c(0,1),main=paste("Prop. of Severity by",names(dades)[k]),cex.names=0.8)
   paleta<-rainbow(length(levels(dades[,k])))
   for(c in 1:length(levels(dades[,k]))){lines(rowperc[,c],col=paleta[c]) }
   
   #legend
   plot(marg,type="n",ylim=c(0,1),main=paste("Prop. of Severity by",names(dades)[k]),cex.names=0.8)
   for(c in 1:length(levels(dades[,k]))){lines(rowperc[,c],col=paleta[c]) }
   legend("topright", levels(dades[,k]), col=paleta, lty=2, cex=0.6)
   
   #amb variable en eix d'abcisses
   marg <-table(dades[,k])/n
   print(append("Categories=",levels(dades[,k])))
   plot(marg,type="l",ylim=c(0,1),main=paste("Prop. of Severity by",names(dades)[k]), las=3)
   paleta<-rainbow(length(levels(as.factor(P))))
   for(c in 1:length(levels(as.factor(P)))){lines(rowperc[c,],col=paleta[c])}

   #legend
   plot(marg,type="l",ylim=c(0,1),main=paste("Prop. of Severity by",names(dades)[k]), las=3)
   for(c in 1:length(levels(as.factor(P)))){lines(rowperc[c,],col=paleta[c])}
   legend("topright", levels(as.factor(P)), col=paleta, lty=2, cex=0.6)
   
   #condicionades a columna 
   plot(marg,type="n",ylim=c(0,1),main=paste("Prop. of Severity by",names(dades)[k]), las=3)
   paleta<-rainbow(length(levels(as.factor(P))))
   for(c in 1:length(levels(as.factor(P)))){lines(colperc[c,],col=paleta[c])}
   legend("topright", levels(as.factor(P)), col=paleta, lty=2, cex=0.6)
  
   #legend
   plot(marg,type="n",ylim=c(0,1),main=paste("Prop. of Severity by",names(dades)[k]), las=3)
   for(c in 1:length(levels(as.factor(P)))){lines(colperc[c,],col=paleta[c])}
   legend("topright", levels(as.factor(P)), col=paleta, lty=2, cex=0.6)
   
      table<-table(dades[,k],P)
      print("Cross Table:")
      print(table)
      print("Distribucions condicionades a columnes:")
      print(colperc)

      #diagrames de barres apilades                                         
      
      paleta<-rainbow(length(levels(dades[,k])))
      barplot(table(dades[,k], as.factor(P)), beside=FALSE,col=paleta,cex.names=0.8 )
      legend("topright",levels(as.factor(dades[,k])),pch=1,cex=0.5, col=paleta)
      
      #diagrames de barres adosades
      barplot(table(dades[,k], as.factor(P)), beside=TRUE,col=paleta,cex.names=0.8)
      legend("topright",levels(as.factor(dades[,k])),pch=1,cex=0.5, col=paleta)
   
      print("Test Chi quadrat: ")
      print(chisq.test(dades[,k], as.factor(P)))
   
      print("valorsTest:")
      print( ValorTestXquali(P,dades[,k]))
   }
      }
}#endfor


#descriptors de les classes m�s significatius. Afegir info qualits
for (c in 1:length(levels(as.factor(P)))) {
  if(!is.na(levels(as.factor(P))[c])){
    print(paste("P.values per class:",levels(as.factor(P))[c]));
    print(sort(pvalk[c,]), digits=3) 
  }
}

