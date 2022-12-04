library(MASS)

## Question 1
df <- Cars93
ind <- sample(1:nrow(df),size=40)


#### split data
train <- df[ind,]
test <- df[-ind,]


#### write into csv files
write.csv(train,"training_set",row.names=FALSE)
write.csv(test,"testing_set",row.names=FALSE)


#### number of USA cars and Non-USA cars
t_USA <- which(train$Origin=="USA")
t_nonUSA <- which(train$Origin=="non-USA")
as.data.frame(t_USA) 
as.data.frame(t_nonUSA) 

## Question 2
#### 5 girls and 5 boys coming out from 10 births
set.seed(121)
sample(c("boy","girl"),size = 10,replace = TRUE,prob = c(0.515,0.485))


#### 5222 boys and 4778 girls
set.seed(222)
x <- sample(c("boy","girl"),size = 10000,replace = TRUE,prob = c(0.515,0.485))
sample(x[x=="boy"]) 
sample(x[x=="girl"])

## Question 3
#### 3 boys out of 10 births
dbinom(3,size=10,prob=0.515)

#### density function plot
n <- 10
x <- 0:n
y <- dbinom(x,size=n,prob=0.515)
plot(x,y,type="b",pch=16,col="black")


#### 8 or more boys out of 10 births
dbinom(8,10,prob=0.515) + dbinom(9,10,prob=0.515) + dbinom(10,10,prob=0.515)
pbinom(7,10,prob=0.515,lower.tail = FALSE)

#### cumulative probability function plot
n <- 10
x <- 0:n
y1 <- pbinom(x,size=n,prob=0.515)
plot(x,y1,type="b",pch=14,col="red")

## Question 4
#### 4 cars every 10 seconds & >5 cars every 10 seconds
dpois(4,lambda=5)
ppois(5,lambda=5,lower.tail = FALSE)

#### plot
n <- 20
x <- 0:n
y <- dpois(x,lambda=5)
plot(x,y,type="b",pch=15,col="blue")
