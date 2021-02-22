library(readxl)
library(dplyr)
library(readr)


#data Processing
csv <- read.csv("C:/Users/mccan/Documents/R/Repositorios/ProgrammingAssignment5-2/repdata_data_StormData.csv", na.strings = "NA")

df <- as.data.frame(csv)
head(df)

dim(df)

colnames(df)

summary(df)


#Crear nueva columna 


df <- df %>%
  mutate(PROPDMGEXP= as.numeric(df$PROPDMGEXP))

df$PROPDMGEXP[df$PROPDMGEXP == "H"] <- 2
df$PROPDMGEXP[df$PROPDMGEXP == "K"] <- 3
df$PROPDMGEXP[df$PROPDMGEXP == "M"] <- 6
df$PROPDMGEXP[df$PROPDMGEXP == "B"] <- 9

df <- df %>%  
  mutate(CROPDMGEXP= as.numeric(df$CROPDMGEXP))

df$CROPDMGEXP[df$CROPDMGEXP == "K"] <- 3   
df$CROPDMGEXP[df$CROPDMGEXP == "M"] <- 6   
df$CROPDMGEXP[df$CROPDMGEXP == "B"] <- 9   



df <- df %>%
  mutate(CostoProp = df$PROPDMG*(10^df$PROPDMGEXP)) %>%
  mutate(CostCrop = df$CROPDMG*(10^df$CROPDMGEXP))


#primera pregunta
df_agrupado <- df %>%
  group_by(EVTYPE) %>%
  summarise(MeanFATALITIES = mean(FATALITIES), MeanINJURIES = mean(INJURIES) , Count = n()) %>%
  mutate(TotalDMG = MeanFATALITIES*0.75 + MeanINJURIES*0.25) %>%
  arrange( desc(TotalDMG), by_group = TRUE)

df_agrupado  


#segunda pregunta
df_agrupado2 <- df %>%
  group_by(EVTYPE) %>%
  summarise(MeanPropCost = mean(CostoProp), MeanCropCost = mean(CostCrop) , Count = n()) %>%
  mutate(TotalCost = MeanPropCost + MeanCropCost) %>%
  arrange( desc(TotalCost), by_group = TRUE)

df_agrupado2 
