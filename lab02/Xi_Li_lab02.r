#' ---
#' title: "Lab 02"
#' author: "Li Xi"
#' date: "Sep 25, 2017"
#' ---  

```{r include = FALSE}
library(MASS)
```

The code below is lab2. 

## Question 1
```{r price}
df <- Cars93
View(Cars93)
cheapest <- tapply(Cars93$Min.Price,Cars93$Type,min)
cheapest
```

##### the manufacturer and model for cars with the max fuel efficiency
```{r fuel}
max_fuel <- tapply(Cars93$MPG.highway,Cars93$Type,max)
max_fuel
which(df$MPG.highway==36&df$Type=="Compact",arr.ind=TRUE) 
which(df$MPG.highway==28&df$Type=="Large",arr.ind=TRUE) 
which(df$MPG.highway==31&df$Type=="Midsize",arr.ind=TRUE)
which(df$MPG.highway==50&df$Type=="Small",arr.ind=TRUE)
which(df$MPG.highway==36&df$Type=="Sporty",arr.ind=TRUE)
which(df$MPG.highway==24&df$Type=="Van",arr.ind=TRUE)
df[c(12,7,20,30,71,77,6,69,39,40,56),c(1,2,3,8)]
```

##### the manufacturer and model for the cheapest cars
```{r cheap}
which(df$Min.Price==8.5,arr.ind=TRUE) #ROW 12
which(df$Min.Price==17.5,arr.ind=TRUE) #Row 30
which(df$Min.Price==12.4,arr.ind=TRUE) #Row 47
which(df$Min.Price==6.7,arr.ind=TRUE) #Row 39
which(df$Min.Price==9.1,arr.ind=TRUE) #Row 46
which(df$Min.Price==13.6,arr.ind=TRUE) #Row 26
df[c(12,30,47,46,26),c(1:4)]
```

## Question 2
##### horsepower
```{r q2}
Cars93.hp <- tapply(Cars93$Horsepower,Cars93$Type,mean)
Cars93.hp
Cars93.compact <- which(Cars93$Type=="Compact")
Cars93.large <- which(Cars93$Type=="Large")
Cars93.midsize <- which(Cars93$Type=="Midsize")
Cars93.small <- which(Cars93$Type=="Small")
Cars93.sporty <- which(Cars93$Type=="Sporty")
Cars93.van <- which(Cars93$Type=="Van")  
```

##### difference
```{r difference}
dif.compact <- Cars93$Horsepower[c(Cars93.compact)] - Cars93.hp[1]
dif.large <- Cars93$Horsepower[c(Cars93.large)] - Cars93.hp[2]
dif.midsize <- Cars93$Horsepower[c(Cars93.midsize)] - Cars93.hp[3]
dif.small <- Cars93$Horsepower[c(Cars93.small)] - Cars93.hp[4]
dif.sporty <- Cars93$Horsepower[c(Cars93.sporty)] - Cars93.hp[5]
dif.van <- Cars93$Horsepower[c(Cars93.van)] - Cars93.hp[6]
```

##### calculate skewness and kurtosis
```{r skewness}
library(moments)
skewness(c(dif.compact,dif.large,dif.midsize,dif.small,dif.sporty,dif.van))
kurtosis(c(dif.compact,dif.large,dif.midsize,dif.small,dif.sporty,dif.van))
```

## Question 3
```{r cars}
USA.cars <- which(Cars93$Origin=="USA")
NonUSA.cars <- which(Cars93$Origin=="non-USA")
new_df1 <- data.frame(df[c(USA.cars),])
new_df2 <- data.frame(df[c(NonUSA.cars),])
```

## Question 4
```{r csv}
write.csv(new_df1,file="USA Cars")
usa_cars <- read.csv(file="USA Cars")
str(usa_cars)
```