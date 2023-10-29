library(dplyr)
library(matlib)
#main data
export<-X2018_2010_export
import<-X2018_2010_import

# Combine export and import datasets
export$HSCode <- as.character(export$HSCode)

# Add a 'Type' column to distinguish between import and export data
export$Type <- "Export"
import$Type <- "Import"

# Combine the datasets
combined_data <- bind_rows(export, import)

# View the combined dataset
head(combined_data)





export$value[is.na(export$value)]=0
import$value[is.na(import$value)]=0

E_Regression<-function(x)
{
  scalingfactor<-1000
  y<-x$total_value/scalingfactor
  x<-x$`export$year`
  n<-length(x)
  #LINEAR REGRESSION
  
   X<-matrix(c(rep(1,n),x),ncol=2,byrow = FALSE)
   Y<-matrix(y,ncol=1,byrow=FALSE)
   B=inv(t(X)%*%X)%*%t(X)%*%Y
   Beta0=B[1][1]
   Beta1=B[2][1]
   cat("The Line of fit is y=",Beta0," + ",Beta1,"*x")
   plot(x,y,main="EXPORT VS YEAR SIMPLE LINEAR REGRESSION",xlim=c(1980,max(x)))
   abline(a=Beta0,b=Beta1)
  
  #NON LINEAR REGRESSION
  
  #QUADRATIC
  
  X<-matrix(c(rep(1,n),x,x^2),ncol=3,byrow = FALSE)
  Y<-matrix(y,ncol=1,byrow=FALSE)
  B=inv(t(X)%*%X)%*%t(X)%*%Y
  Beta0=B[1][1]/scalingfactor
  Beta1=B[2][1]/scalingfactor
  Beta2=B[3][1]/scalingfactor
  cat("The Line of fit is y=",Beta0," + ",Beta1,"*x + ",Beta2," *x^2")
  plot(x,y,main="EXPORT VS YEAR 2 nd ORDER POLYNOMIAL REGRESSION",xlim=c(0,max(x)),ylim=c(min(y),max(y)),xlab="years",ylab=)
  qgraph<-function(x,Beta0,Beta1,Beta2)
  {
    Beta0+Beta1*x+Beta2*x^2
  }
  curve(qgraph(x,Beta0,Beta1,Beta2),from=0,to=max(x),col="blue",add=TRUE)
  
  
  #EXPONENTIAL
  
  X<-matrix(c(rep(1,n),x),ncol=2,byrow = FALSE)
  Y<-matrix(log(y),ncol=1,byrow=FALSE)
  B=inv(t(X)%*%X)%*%t(X)%*%Y
  Beta0=B[1][1]
  Beta1=B[2][1]
  cat("The Line of fit is y= e^(",Beta0," + ",Beta1,"*x ) ")
  expgraph<-function(x)
  {
    exp(Beta0+Beta1*x)
  }
  plot(x,y,main="EXPORT VS YEAR EXPONENTIAL REGRESSION",xlim=c(1940,max(x)))
  curve(expgraph,from=0,to=max(x),col="blue",add=TRUE)
  
}

I_Regression<-function(x)
{
  scalingfactor<-1000
  y<-x$total_value/scalingfactor
  x<-x$`import$year`
  n<-length(x)
  #LINEAR REGRESSION
  
  X<-matrix(c(rep(1,n),x),ncol=2,byrow = FALSE)
  Y<-matrix(y,ncol=1,byrow=FALSE)
  B=inv(t(X)%*%X)%*%t(X)%*%Y
  Beta0=B[1][1]
  Beta1=B[2][1]
  cat("The Line of fit is y=",Beta0," + ",Beta1,"*x")
  plot(x,y,main="IMPORT VS YEAR SIMPLE LINEAR REGRESSION",xlim=c(2005,max(x)))
  abline(a=Beta0,b=Beta1)
  
  #NON LINEAR REGRESSION
  
  #QUADRATIC
  
  X<-matrix(c(rep(1,n),x,x^2),ncol=3,byrow = FALSE)
  Y<-matrix(y,ncol=1,byrow=FALSE)
  B=inv(t(X)%*%X)%*%t(X)%*%Y
  Beta0=B[1][1]/scalingfactor
  Beta1=B[2][1]/scalingfactor
  Beta2=B[3][1]/scalingfactor
  cat("The Line of fit is y=",Beta0," + ",Beta1,"*x + ",Beta2," *x^2")
  plot(x,y,main="IMPORT VS YEAR 2 nd ORDER POLYNOMIAL REGRESSION",xlim=c(0,max(x)),ylim=c(0,max(y)),xlab="years",ylab=)
  qgraph<-function(x,Beta0,Beta1,Beta2)
  {
    Beta0+Beta1*x+Beta2*x^2
  }
  curve(qgraph(x,Beta0,Beta1,Beta2),from=0,to=max(x),col="blue",add=TRUE)
  
  
  #EXPONENTIAL
  
  X<-matrix(c(rep(1,n),x),ncol=2,byrow = FALSE)
  Y<-matrix(log(y),ncol=1,byrow=FALSE)
  B=inv(t(X)%*%X)%*%t(X)%*%Y
  Beta0=B[1][1]
  Beta1=B[2][1]
  cat("The Line of fit is y= e^(",Beta0," + ",Beta1,"*x ) ")
  expgraph<-function(x)
  {
    exp(Beta0+Beta1*x)
  }
  plot(x,y,main="IMPORT VS YEAR EXPONENTIAL REGRESSION",xlim=c(1940,max(x)))
  curve(expgraph,from=0,to=max(x),col="blue",add=TRUE)
}


grouped_export_yearw= export%>% group_by(export$year) %>%summarise(total_value = sum(value))
grouped_export_yearw

grouped_import_yearw= import%>% group_by(import$year) %>%summarise(total_value = sum(value))
grouped_import_yearw

E_Regression(grouped_export_yearw)
I_Regression(grouped_import_yearw)
