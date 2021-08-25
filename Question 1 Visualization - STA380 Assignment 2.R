rm(list=ls())


Data = read.csv('greenbuildings.csv')
attach(Data)

Data$green_rating <- as.factor(Data$green_rating)
Data$cluster <- as.factor(Data$cluster) # 693 clusters numbered randomly
Data$renovated <- as.factor(Data$renovated)
Data$class_a <- as.factor(Data$class_a)
Data$class_b <- as.factor(Data$class_b)
Data$LEED <- as.factor(Data$LEED)
Data$Energystar <- as.factor(Data$Energystar)
Data$green_rating <- as.factor(Data$green_rating)
Data$net <- as.factor(Data$net)
Data$amenities <- as.factor(Data$amenities)

```{r 1.1, echo = FALSE, warning = FALSE, message = FALSE}
library(readr)
library(knitr)
library(ggplot2)
library(gridExtra)
library(mosaic)
library(MatchIt)
library(cowplot)
library(corrplot)
options(scipen=999)

# Drop those with leasing rates < 10%
Data = Data[Data$leasing_rate >= 10,]
# Define revenue per square foot measure
Data$RevPSF = Data$Rent * Data$leasing_rate / 100
ggplot(Data, aes(x = as.factor(green_rating), y = RevPSF), fill = green_rating) + 
  geom_boxplot() + 
  xlab("Green Rating") + 
  ylab("Revenue per Square Foot") +
  labs(title = "Comparing Revenue per Square Foot of Not Green v Green Buildings") +
  scale_x_discrete(labels = c("Not Green", "Green")) +
  coord_flip() +
  stat_summary(aes(label=round(..y..,2)), fun.y = "mean", geom = "point", shape = 8, size = 2, color = "red")
```

The medians (black lines on the boxplots) and means (the red stars) of green and non-green building revenue per square foot show that green buildings have higher revenue. 
The nogreen buildings have a lowe mean than the green buildings   

##Visualizations##


```{r ,out.width=c('50%', '50%'), fig.show='hold', echo=FALSE}
ggplot(data=Data) + 
  geom_point(mapping=aes(x=cluster_rent, y=Rent, colour=green_rating)) +
  labs(x="Cluster Rent", y='Rent', title = 'Green buildings: Rent vs. Cluster Rent',
       color='Green building')
ggplot(data=Data) + 
  geom_point(mapping=aes(x=age, y=Rent, colour=green_rating))+
  labs(x="Age", y='Rent', title = 'Green buildings: Rent vs. Age of the Building',
       color='Green building')
ggplot(data=Data) + 
  geom_point(mapping=aes(x=size, y=Rent, colour=green_rating)) +
  labs(x="Size", y='Rent', title = 'Green buildings: Rent vs. Size of the Rental space in the building (square foot)',
       color='Green building')
ggplot(data=Data) + 
  geom_point(mapping=aes(x=age, y=Rent, colour=class_a))+
  labs(x="Age", y='Rent', title = 'Class A: Age of the building vs. Rent',
       color='Class A building')
```
##Inference##
  
* There is a correlation between rent and the cluster rent
* The size of the rental space in the building is also correlated with the Rent
* A Class buildings appear to be younger
* Age does not not seem to have a high correlation with rent 
* Class A buildings have higher rent  


```{r out.width=c('50%', '50%'), fig.show='hold', echo=FALSE}
g = ggplot(Data, aes(x=age))
g + geom_density(aes(fill=factor(green_rating)), alpha=0.4)+
  labs(x="Age", y='Density', title = 'Age Distribution',
       fill='Green building')
ggplot(Data, aes(class_a, ..count..)) + geom_bar(aes(fill = green_rating), position = "dodge")+
  labs(x="Class a", y='Number of buildings', title = 'Class A vs Green Buildings',
       fill='Green building')
g = ggplot(Data, aes(x=size))
g + geom_density(aes(fill=factor(green_rating)), alpha=0.4)+
  labs(x="Size", y='Density', title = 'Size Distribution',
       fill='Green building')
medians <- aggregate(Rent ~  class_a, Data, median)
ggplot(data=Data, aes(x=factor(class_a), y=Rent, fill=class_a)) + geom_boxplot()+
  stat_summary(fun.y=median, colour="darkred", geom="point", 
               shape=18, size=3,show.legend = FALSE) + 
  geom_text(data = medians, aes(label = Rent, y = Rent - 20)) +
  labs(x="Class A", y='Rent', title = 'Class A vs Rent',
       fill='Class A')

-------
  

  
