library(dplyr)
library(factoextra)
library(ggplot2)
library(ggpubr)
library(cluster)
library(tidyverse)
data<-read.csv("C:\\Users\\Lenovo\\Documents\\PA PROJECT\\Research paper 1.csv")
k<-data[-c(1:2)]

print(k)
results1<-kmeans(k,2)
results1
results1$size
results1$cluster
sil <- silhouette(results1$cluster, dist(k))
fviz_silhouette(sil)
species<-data[-c(1:2),12]
while(TRUE){
  print(cat("\n1.Which social platforms people use mostly\n2.Does level of impact of your well being and hours spend in social media affects number of social media platforms or not\n3.Positive impact about social media\n"))
  choice=readline(prompt="Enter the choice")
  if(choice==1){
    analyse1<-data[-c(1:2),c(5:11)]
    colnames(analyse1)<-c("Whatsapp ","Facebook","LinkedIn","Telegram","Youtube","Twitter","Instagram")
    print(analyse1)
   
    res.pca <- prcomp(analyse1, scale = TRUE)
    print(res.pca)
    summary(res.pca)
    plot(res.pca,type="l")
    
    biplot(res.pca,scale=0)
    print(fviz_eig(res.pca))
    eig.val <- get_eigenvalue(res.pca)
    
    
    eig.val
    res.pca$sdev^2 / sum(res.pca$sdev^2)
    str(res.pca)
    
    res.pca$x
    
    iris2=cbind(analyse1,res.pca$x[,1:2],species)
    
    head(iris2)
    print(iris2)
    
    library(ggplot2)
    
    print(ggplot(iris2,aes(PC1,PC2,col=species,fill=species))+
      
      stat_ellipse(geom="polygon",col="black",alpha=0.5)+
      
    geom_point(shape=21,col="black"))
  }
  else if(choice==2){
      analyse1<-data[-c(1:2),4]
      species<-na.omit(data[-c(1:2),12])
      gender<-na.omit(data[-c(1:2),2])
      hours<-na.omit(data[-c(1:2),4])
      level<-na.omit(data[-c(1:2),41])
      print(level)
      final<-as.data.frame(cbind(species,gender,hours,level))
      final<-na.omit(final)
     
      final2<-read.csv("C:\\Users\\Lenovo\\Documents\\PA PROJECT\\written2.csv")
      print(final2)
      result<-manova(cbind(hours,level)~species,data=final2)
      print(summary(result))
  
    
  }
  else if(choice==3){
    analyse4<-(data[-c(1:2),c(27:31)])
    species<-na.omit(data[-c(1:2),12])
    final4<-as.data.frame(cbind(analyse4,species))
    
    final5<-read.csv("C:\\Users\\Lenovo\\Documents\\PA PROJECT\\written3.csv")
    print(final5)
    library(olsrr)
    model<-lm(species~X1.3+X2.3+X3.3+X4.2+X5.2, data=final5)
    print(model)
    K=ols_step_all_possible(model)
    print(ols_step_forward_p(model,penter=0.05))

  }
   
  else{
    
    print("Thank you")
    break
    
  }
  
}
