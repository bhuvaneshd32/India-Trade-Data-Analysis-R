library(dplyr)
#main data
export<-X2018_2010_export
import<-X2018_2010_import
#1)VISUALISATION
#barplot(export$value,width=1,xlab=)
#Equating NA values to 0
export$value[is.na(export$value)]=0
import$value[is.na(import$value)]=0
#Grouping Export data in accordance to commodity and year 
grouped_commodity_export= export%>% group_by(Commodity,export$year) %>%summarise(total_value = sum(value), 
            .groups = 'drop')
max_export=max(grouped_commodity_export$total_value)
#grouped_commodity_export$total_value[grouped_commodity_export$Commodity=="MINERAL FUELS, MINERAL OILS AND PRODUCTS OF THEIR DISTILLATION; BITUMINOUS SUBSTANCES; MINERAL WAXES."]
#Finding the major export commodities by comparing maximum export value
major_export_chunk=grouped_commodity_export$Commodity[grouped_commodity_export$total_value==max_export]
cat("Major Export Chunk: ",major_export_chunk)
#Grouping Import data in accordance to commodity and year
grouped_commodity_import=import%>%group_by(Commodity,import$year) %>%summarise(total_value=sum(value), .groups='drop')
max_import=max(grouped_commodity_import$total_value)
#Finding the major export commodities by comparing maximum import value
major_import_chunk=grouped_commodity_import$Commodity[grouped_commodity_import$total_value==max_import]
#grouped_commodity_import$total_value[grouped_commodity_import$Commodity=="MINERAL FUELS, MINERAL OILS AND PRODUCTS OF THEIR DISTILLATION; BITUMINOUS SUBSTANCES; MINERAL WAXES."]
cat("Major Import Chunk: ",major_import_chunk)
#merging import and export data
##export_import=cbind(grouped_commodity_export,grouped_commodity_import)
##export_import=export_import%>% group_by(Commodity,export_import$year) %>%summarise(total_value = sum(value), 
##

# R program to illustrate
# taking input from the user

# string input
country = readline(prompt = "Enter your name : "); 
print(country)


