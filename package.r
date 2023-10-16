library(dplyr)
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

#pie(export$value,labels = export$country)


#3)GROWTH RATE CALCULATION 
# string input
country = readline(prompt = "Enter The Country for import growth rate calculation(Compound Annual Growth Rate (CAGR)) : ");
#country <- scan(what = character(), nmax = 1, prompt = "Enter The Country: ")

print(country)
#AVERAGE GROWTH RATE = (Ending Value/Begining value)^1/n -1)
grouped_commodity_import_by_country=import%>%group_by(import$year,import$country) %>%summarise(total_value=sum(value), .groups='drop')

BV=grouped_commodity_import_by_country$total_value[grouped_commodity_import_by_country$`import$year`==2010 & grouped_commodity_import_by_country$`import$country`==country]

EV=grouped_commodity_import_by_country$total_value[grouped_commodity_import_by_country$`import$year`==2018 & grouped_commodity_import_by_country$`import$country`==country]

Growth_rate=((EV/BV)^(1.0/9.0))-1

cat("Growth Rate of import between  ",country,"is",Growth_rate)

country1 = readline(prompt = "Enter The Country for export growth rate calculation(Compound Annual Growth Rate (CAGR)) : ");

print(country1)

grouped_commodity_export_by_country=export%>%group_by(export$year,export$country) %>%summarise(total_value=sum(value), .groups='drop')

BV1=grouped_commodity_export_by_country$total_value[grouped_commodity_export_by_country$`export$year`==2010 & grouped_commodity_export_by_country$`export$country`==country1]

EV1=grouped_commodity_export_by_country$total_value[grouped_commodity_export_by_country$`export$year`==2018 & grouped_commodity_export_by_country$`export$country`==country1]

Growth_rate1=((EV1/BV1)^(1.0/9.0))-1

cat("Growth Rate of export between  ",country,"is",Growth_rate1)



