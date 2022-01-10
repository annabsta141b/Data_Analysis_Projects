
  
  
knitr::opts_chunk$set(echo = TRUE)
cl_apartments = readRDS("/Users/aboeriu/Documents/UC Davis/Year 3/STA 141A/assignment 3/cl_apartments.rds")
library(ggplot2)

#looking at price

sort(cl_apartments$price,decreasing = T)
cl_apartments[which.max(cl_apartments$price=="34083742"),]
cl_apartments$price[15872] = 3742
cl_apartments[which.max(cl_apartments$price=="9951095"),]
cl_apartments$price[4515]=1092
range(cl_apartments$price,na.rm = T)



# removing 5-7 bedroom apartments

cl_apartments=cl_apartments[cl_apartments$bedrooms<5,]
cl_apartments
#remove all but CA
```{r}
cl_apartments=cl_apartments[which(cl_apartments$state=="CA"),]
```
## looking at states

table(cl_apartments$state)
which(cl_apartments$state=="CT")
cl_apartments[3657,]
which(cl_apartments$state=="FL")

which(cl_apartments$state=="MD")


which(cl_apartments$state=="NC")


which(cl_apartments$state=="NV")

which(cl_apartments$state=="OH")
#set ohio entries to NA b/c apartments are not in California.
cl_apartments$state[6032]=NA
cl_apartments$state[15111]=NA

which(cl_apartments$state=="UT")

which(cl_apartments$state=="VA")
#set entry 8248 to NA b/c appt is in tijuana mexico

which(cl_apartments$state=="WA")






# of bedrooms

table(cl_apartments$bedrooms)
which(cl_apartments$bedrooms==5)
cl_apartments[439,]
cl_apartments[977,]
cl_apartments[4887,]
cl_apartments[7262,]
cl_apartments[7284,]
cl_apartments[9018,]
cl_apartments[10485 ,]
cl_apartments[10996,]
cl_apartments[11397,]
cl_apartments[12317,]
cl_apartments[12989,]
cl_apartments[14230,]
cl_apartments[14231,]
```

# looking at # bathrooms

which(cl_apartments$bathrooms==6)
#change entry 7618 to a 2 bath 
cl_apartments$bathrooms[7618]=2

which(cl_apartments$bathrooms==5)
#change entry 959 to be 2.5 baths
cl_apartments$bathrooms[950]=2.5

which(cl_apartments$bathrooms==4.5)
#change entry 6192 to 2.5 bath
cl_apartments$bathrooms[6192]=2.5

which(cl_apartments$bathrooms==4)

which(cl_apartments$bathrooms==0)
cl_apartments$bathrooms[11455]=1
cl_apartments$bathrooms[7582]=2
cl_apartments$bathrooms[11324]=2
cl_apartments$bathrooms[21105]=NA
cl_apartments$bathrooms[2096]=1
cl_apartments$bathrooms[20112]=NA
cl_apartments$bathrooms[4105]=NA
cl_apartments$bathrooms[4178]=NA
```
#where bath > bedrooms
ind = which(cl_apartments$bedrooms > 0 & cl_apartments$bedrooms < cl_apartments$bathrooms)

cl_apartments$bathrooms[ind] = NA

---
  title: "HW 3 craiglist"
author: "Ana Boeriu"
date: "10/19/2018"
output: html_document
---
  

#2.1.2

unique(cl_apartments$state)
table(cl_apartments$state)
which(cl_apartments$state=="CA")
cl_apartments[which(cl_apartments$state=="CA"),]
cl_apartments[which(cl_apartments$state=="WA"),]

table(cl_apartments$state[is.na(cl_apartments$city)])
#out of these states _ have missing city info
sum(cl_apartments$state=="WA",na.rm=T)

identifying top 10 major cities

#which(cl_apartments$state=="CA")
sort(table(cl_apartments$city[cl_apartments$state=="CA"]),decreasing=T)
head(sort(table(cl_apartments$city[cl_apartments$state=="CA"]),decreasing=T),10)
top10_cities<-tail(sort(table(cl_apartments$city[cl_apartments$state=="CA"])),10)
top10_cities
cl_apartments$city%in%names(top10_cities)

#choosing suburbs

suburbs=c(" Walnut Creek ","Belmont"," Danville", #sf
          "Beverly Hills","Manhattan Beach", "Long Beach",#LA
          "Chula Vista","Lemon Grove","Santee", #SD
          "Citrus Heights","Davis","Elk Grove", #sacramento
          "Los Gatos","Morgan Hill","Saratoga", #san jose
          "Calistoga" ,"Sebastopol","St. Helena", #SR
          "Palo Alto","Atherton","Sunnyvale", #MV
          "Hillsborough","Burlingame","Milbrae", #SM
          "Ross","San Anselmo","Sausalito", #SR
          "ALbany","Alameda","Lafayette") #oakland

major_9_cities = c("San Francisco",
                    "Los Angeles",
                    "San Diego",
                    "Sacramento",
                    "San Jose",
                    "Santa Rosa",
                    "Mountain View",
                    "San Mateo",
                    "San Rafael",
                    "Oakland")

city_and_suburbs = data.frame(major_9_cities, suburbs)

#table(cl_apartments$city=="San Fransisco"[cl_apartments$pets])
my_city=cl_apartments$city %in% major_10_cities
organize_city=cl_apartments$city[my_city]
display_only_my_city=droplevels(organize_city,na.rm=T)
table(cl_apartments$bedrooms[my_city],display_only_my_city)
#table(cl_apartments$state[cl_apartments$bedrooms])
table(cl_apartments$pets[my_city],display_only_my_city)
sum(cl_apartments$bedrooms[my_city],display_only_my_city,na.rm=T)
table(cl_apartments$bedrooms,cl_apartments$pets)

my_suburbs=cl_apartments$city %in% suburbs
organize_suburbs=cl_apartments$city[my_suburbs]
display_only_my_suburbs=droplevels(organize_suburbs)
a=table(cl_apartments$bedrooms[my_suburbs],display_only_my_suburbs)
plot(a)
sum(cl_apartments$bedrooms[my_suburbs],display_only_my_suburbs,na.rm=T)
sum(cl_apartments$bedrooms[my_suburbs]>3,display_only_my_suburbs,na.rm=T)
mean(cl_apartments$bedrooms[my_suburbs],display_only_my_suburbs,na.rm=T)

ggplot(cl_apartments[cl_apartments$city %in% suburbs,])+
  geom_boxplot(aes(x=bedrooms,y=sqft, group=bedrooms))
# do the same for city
```


##prices 34083742   9951095
```{r}
sort(cl_apartments$price,decreasing = TRUE)
cl_apartments[which.max(cl_apartments$price=="34083742"),]
cl_apartments$price[15961] = 3742
cl_apartments[which.max(cl_apartments$price=="9951095"),]
cl_apartments$price[4531]=1092
```

#2.2  
```{r}
unique(cl_apartments$bathrooms,na.rm=T)
unique(cl_apartments$bedrooms)
appt_bed_bath.model=lm(price ~ bedrooms + bathrooms, data = cl_apartments)
appt_bed_bath.model$coefficients
ei=appt_bed_bath.model$residuals
fitted= ei=appt_bed_bath.model$fitted.values
qqnorm(ei)


par(mfrow = c(2, 2))
plot(appt_bed_bath.model)
all.sum = summary(appt_bed_bath.model)
HT = all.sum$coefficients
HT.b1 = HT[2,]
HT.b1



aggregate(price~bedrooms+bathrooms, data=cl_apartments,mean)






#2.3 
```{r}
by_group=split(cl_apartments$longitude,cl_apartments$latitude)
by_group
sapply(by_group~cl_apartments$city,data=cl_apartments,summary,na.rm=T)
```


##plots 
ggplot(cl_apartments[cl_apartments$city %in% suburbs, ])+
  geom_bar(aes(x=bedrooms))


ggplot(cl_apartments[cl_apartments$city %in% major_10_cities, ])+
  geom_bar(aes(x=bedrooms))




#3. 

#which(cl_apartments$sqft > 150000)
#cl_apartments[1446, ]
ggplot(data= cl_apartments[-1446,])+
  geom_point(aes(x=sqft,y=price,color=bathrooms))
#ylim(0,10000)




# plotting barplot for #bedrooms and 10 citites in CA
library(ggplot2)
ggplot(cl_apartments[cl_apartments$city %in% major_10_cities, ])+
  geom_bar(aes(x=bedrooms))+
  facet_wrap(city~.)

ggplot(cl_apartments[cl_apartments$city %in% c("Sacramento","Mountain View", "San Francisco"), ])+
  geom_bar(aes(x=bedrooms))+
  facet_grid(city~.)
#SF has the most studios and all of them have lots of 

ggplot(cl_apartments[cl_apartments$city %in% suburbs, ])+
  geom_bar(aes(x=bedrooms))+
  facet_wrap(city~.,ncol = 3)

#family friendly variable
cl_apartments$family_friendly = (cl_apartments$bedrooms>1 & 
  cl_apartments$pets!='none' & 
  cl_apartments$parking!="none" &
    !(is.na(cl_apartments$parking) | is.na(cl_apartments$pets)))


ggplot(cl_apartments[cl_apartments$city %in% major_10_cities, ])+
  geom_bar(aes(x=family_friendly))+
  facet_wrap(city~.)

ggplot(cl_apartments[cl_apartments$city %in% suburbs, ])+
  geom_bar(aes(x=family_friendly))+
  facet_wrap(city~.)

