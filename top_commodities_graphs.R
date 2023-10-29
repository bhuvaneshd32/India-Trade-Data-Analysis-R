library("av")
library("tidyr")
library("dplyr")
library("sjmisc")
library("GGally")
library("cowplot")
library("ggplot2")
library("ggExtra")
library("gapminder")
library("gridExtra")
library("gganimate")
library("ggcorrplot")
library("RColorBrewer")

export<-X2018_2010_export
import<-X2018_2010_import
attach(export)
attach(import)


#ggplot
data1 %>% count( HSCode ) %>% arrange( -n ) %>% head(5) %>% 
  ggplot( aes( reorder( HSCode, n ), n ) ) + geom_col( fill = "thistle1" ) + xlab( "COMMODITY HSCODE" ) + ylab( "COUNT" ) + ggtitle( "TOP 5 CMMODITY IN THE EXPORT SECTION" )


data2 %>% count( HSCode ) %>% arrange( -n ) %>% head(5) %>%
  ggplot( aes( reorder( HSCode, n ), n ) ) + geom_col( fill = "tan4" ) + xlab( "COMMODITY HSCODE" ) + ylab( "COUNT" ) + ggtitle( "TOP 5 COMMODITY IN THE IMPORT SECTION" )

data2 %>% filter( year ==  2010 ) %>% count( HSCode ) %>% arrange(-n) %>% head(5) %>%
  ggplot( aes( reorder( HSCode, n ), n ) ) + geom_col( fill = "orangered" ) + xlab( "COMMODITY HSCODE" ) + ylab( "COUNT" ) + ggtitle( "TOP 5 COMMODITY IN THE IMPORT SECTION IN 2010" )

topComImport = data2 %>% filter( HSCode %in% ( data2 %>% count( HSCode ) %>% arrange(-n) %>% head(5) %>% pull( HSCode ) ) )

tb1 = with( topComImport, table( year, HSCode ) )
tb1 = as.data.frame( tb1 )

ggplot( tb1, aes( HSCode, Freq, fill = year) ) + geom_col( position = 'dodge' ) + xlab( "COMMODITY HSCODE" ) + ylab( "COUNT" ) + ggtitle( "5 Top COMMODITIES IN IMPORT SECTION FOR ALL YEARS NEXT TO EACH OTHER" )

