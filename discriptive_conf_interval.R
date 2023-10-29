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

summary(export)
summary(import)

#boxplot and histogram of imported stocks

qqnorm(import$value,main="Import Value")
qqline(import$value)
boxplot(import$value,main="Import Value")
hist(import$value,main="Import Value")

#boxplot and histogram of exported stocks

qqnorm(export$value,main="Export Value")
qqline(export$value)
boxplot(export$value,main="Export Value")
hist(export$value,main="Export Value")

grouped_export_yearw= export%>% group_by(export$year) %>%summarise(total_value = sum(value))
grouped_export_yearw

grouped_import_yearw= import%>% group_by(import$year) %>%summarise(total_value = sum(value))
grouped_import_yearw

#CONFIDENCE INTERVAL (95% confidence level)

#import data

#Assuming distribution to be normal

mean_sample<-mean(grouped_import_yearw$total_value)
n=length(grouped_import_yearw$total_value)
error<-sd(grouped_import_yearw$total_value)*qnorm(0.975)/sqrt(n)
UL=mean_sample+error
LL=mean_sample-error
curve(dnorm(x,0,1),lwd=2,main="95% Confidence interval for import data(normal data)",xlim=c(-4,4),col="yellow")
abline(v=c(qnorm(0.025),qnorm(0.975)),lwd=5,col="red")

#Assuming t-distribution

mean_sample<-mean(grouped_import_yearw$total_value)
n=length(grouped_import_yearw$total_value)
error<-sd(grouped_import_yearw$total_value)*qt(0.975,n-1)/sqrt(n-1)
UL=mean_sample+error
LL=mean_sample-error
curve(dnorm(x,0,1),lwd=2,main="95% Confidence interval for import data(t data)",xlim=c(-4,4),col="yellow")
abline(v=c(qt(0.025,n-1),qt(0.975,n-1)),lwd=5,col="red")

# export data

# Assuming normal

mean_sample<-mean(grouped_export_yearw$total_value)
n=length(grouped_export_yearw$total_value)
error<-sd(grouped_export_yearw$total_value)*qnorm(0.975)/sqrt(n)
UL=mean_sample+error
LL=mean_sample-error
curve(dnorm(x,0,1),lwd=2,main="95% Confidence interval for export data(normal data)",xlim=c(-4,4),col="yellow")
abline(v=c(qnorm(0.025),qnorm(0.975)),lwd=5,col="red")

#Assuming t-distribution

mean_sample<-mean(grouped_export_yearw$total_value)
n=length(grouped_export_yearw$total_value)
error<-sd(grouped_export_yearw$total_value)*qt(0.975,n-1)/sqrt(n-1)
UL=mean_sample+error
LL=mean_sample-error
curve(dnorm(x,0,1),lwd=2,main="95% Confidence interval for export data(t data)",xlim=c(-4,4),col="yellow")
abline(v=c(qt(0.025,n-1),qt(0.975,n-1)),lwd=5,col="red")
