library(dplyr)
#main data
export<-X2018_2010_export
import<-X2018_2010_import

export$value[is.na(export$value)] = 0
export$value[is.nan(export$value)] = 0
import$value[is.na(import$value)] = 0
import$value[is.nan(import$value)] = 0

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

df = data.frame(Year=grp_year_export$year,
                Export=grp_year_export$total_value,
                Import=grp_year_import$total_value,
                Total=grp_year_export$total_value + grp_year_import$total_value)
df = rbind(df,data.frame(Year=NA,
                         Export=sum(grp_year_export$total_value),
                         Import=sum(grp_year_import$total_value),
                         Total=sum(df$Total)))
df

df_expected = data.frame(Export=df$Total[1:9] * df$Export[10] / df$Total[10],
                         Import=df$Total[1:9] * df$Import[10] / df$Total[10])
df_expected

chi_sq = 0
for(i in 1:9) {
  chi_sq = chi_sq + (df$Export[i] - df_expected$Export[i]) ** 2 / df_expected$Export[i]
  chi_sq = chi_sq + (df$Import[i] - df_expected$Import[i]) ** 2 / df_expected$Import[i]
}

if(chi_sq < qchisq(0.95, df=(9-1)*(2-1))) {
  print("Accept Ho")
  print("Export and import year-wise totals are statistically independent")
} else {
  print("Reject Ho")
  print("Export and import year-wise totals are statistically dependent")
}

paste("Comparing two population variance")
paste("Ho: Var(Export) = Var(Import)")
paste("Ha: Var(Export) != Var(Import)")
F_test = sd(df$Import) ** 2 / sd(df$Export) ** 2
paste("P-value: ",pf(F_test,9-1,9-1))
print("At 5% level of significance or higher, we reject Ho")
print("At 4% level of significance or lower, we accept Ho")

sample_import = export$value[sample(1:length(export$value), 10000*2)]
sample_export = import$value[sample(1:length(import$value), 10000*2)]

print("Paired T-test")
t.test(sample_export,sample_import,alternative = "two.sided", paired = TRUE)
