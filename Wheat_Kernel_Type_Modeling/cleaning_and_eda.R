# paper link
# https://home.agh.edu.pl/~kulpi/publ/Charytanowicz_Niewczas_Kulczycki_Kowalski_Lukasik_Zak_-_Information_Technologies_in_Biomedicine_-_2010.pdf


############################### read in data and format ####################################
seed = read.csv("~/Documents/Winter22/STA223/proj2/Seed_Data.csv")
View(seed)
# y = seed type
names(seed) = c("area", "perimeter", "compactness", "length_kernal", "width_kernal",
                "asymCoef", "grove_length", "kernalType")

seed = seed[,c(8,1:7)]

unique(seed$kernalType)
table(seed$kernalType)

seed[1:70,"kernalType"] = "Kama"
seed[71:140, "kernalType"] = "Rosa"
seed[141:210, "kernalType"] = "Canadian"
seed$kernalType = as.factor(seed$kernalType)
str(seed)

############################### data cleaning ####################################

## check neg vals
checkNegVals = function(Cname){
  freq = length(which(seed$Cname < 0))
  return(freq)
}
sapply(seed,checkNegVals)

### check repeats/duplicates rows
library(data.table)
which(duplicated(seed) == TRUE)

range(seed$compactness)
sapply(seed[2:8], range)

######################### correlations
library(ggplot2)
library(corrplot)

correlationVals = cor(seed[,2:8])
corrplot(correlationVals, method = "number")

#take out length and width
which(names(seed) %in% c("width_kernal", 'length_kernal'))
seed2 = seed[,-which(names(seed) %in% c("width_kernal", 'length_kernal'))]

correlationVals = cor(seed2[,2:6])
corrplot(correlationVals, method = "number")

############ investigate
ggplot(seed)+
  geom_point(aes(x=area, y=length_kernal))

area_freq = data.frame(table(seed2$area))
names(area_freq) = c("area", "freq")
View(area_freq)
#repeating areas
repAreas = area_freq[which(area_freq$freq >1),]
repAreas$area = as.numeric(as.character(repAreas$area))
## looking at the original data we see some repeating values but different y 
which(seed2$area == 11.23)
seed2[c(62,189,207),] #rm these pts, same area diff y 

seed3 = seed2[-c(62,189,207),]
#reset index
rownames(seed3) = NULL


##################### verify if we can use mulinomial ########################
library(gridExtra)
library(ggpubr)
library(grid)

areaHist <- ggplot(seed3, aes(x=area, fill=kernalType)) +
  geom_histogram(binwidth=.5, alpha=.5, position="identity")+
  xlab("Area") + ylab("Frequency")+ggtitle("Histogram of Area by Kernel Type")+
  theme_bw()+theme(legend.position='none')+
  theme(text = element_text(size = 16))  
areaHist

compactHist <- ggplot(seed3, aes(x=compactness, fill=kernalType)) +
  geom_histogram(position="identity", alpha = 0.5)+
  xlab("Compactness") + ylab("Frequency")+ ggtitle("Histogram of Compactness\n by Kernel Type")+
  theme_bw()+theme(legend.position='none')+
  theme(text = element_text(size = 16))

asymCoefHist<- ggplot(seed3, aes(x=asymCoef, fill=kernalType)) +
  geom_histogram(binwidth=.5, alpha=.5, position="identity")+
  xlab("Asymmetry") + ylab("Frequency")+
  ggtitle("Histogram of Asymmetry by Kernel Type")+
  labs(fill = "Type of Wheat Kernel")+
  theme_bw()+theme(legend.position="bottom")+
  theme(text = element_text(size = 16))

grid.arrange(areaHist,compactHist, asymCoefHist, ncol = 2,
             top=textGrob("Histograms"))

## notice only the green overlaps for area so we have to condition on area. 
# Multinomial will not work, if we do fit a multinomial we will get a lack of fit


