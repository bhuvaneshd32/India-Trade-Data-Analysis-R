library(dplyr)
#main data
export<-X2018_2010_export
import<-X2018_2010_import

export$value[is.na(export$value)] = 0
export$value[is.nan(export$value)] = 0
import$value[is.na(import$value)] = 0
import$value[is.nan(import$value)] = 0

sample = export$value[sample(1:length(export$value), 100)]

#grp = export %>% group_by(country,export$year) %>% summarise(total_value = sum(value))
grp_country_export = export %>% group_by(country) %>% summarise(total_value = sum(value))
grp_country_export

grp_year_export = export %>% group_by(year) %>% summarise(total_value = sum(value))
grp_year_export

grp_country_import = import %>% group_by(country) %>% summarise(total_value = sum(value))
grp_country_import

grp_year_import = import %>% group_by(year) %>% summarise(total_value = sum(value))
grp_year_import

paste("Goodness of fit test")
paste("Ho: Export and import year-wise totals are statistically independent")
paste("Ha: Export and import year-wise totals are statistically dependent")

exponential <- function(x, lambda) {
  fx = lambda * exp(-lambda * x)
  return(fx)
}

lambda = 1 / mean(grp_year$total_value)
