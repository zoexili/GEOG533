# lab 5
# question 1
sd <- 2
q95 <- qnorm(0.975)
std.err <- 0.2
n <- round((sd*q95/std.err)^2,digits = 2)

# question 3
n <- 50
mean <- 18.5
sd <- 7
pollutant <- mvrnorm(n=50,mu=18.5,Sigma=sd^2,empirical=TRUE)
result <- t.test(pollutant,mu=16,conf.level = 0.95) #[16.51, 20.48]
if(result$p.value<=0.05){
  print('we reject the null hypothesis')
}else{
  print("we cannot reject the null hypothesis")
}
findInterval(16,result$conf.int) # 0, so not included

curve(dnorm(x,mean,sd),from=-2.5,to=39.5)
cord.x <- c(16.51,seq(16.51,20.49,by=0.01),20.49)
cord.y <- c(0,dnorm(seq(16.51,20.49, by=0.01),mean,sd),0)
polygon(cord.x,cord.y,col='skyblue')

# question 5
result <- prop.test(0.24*50,50,0.165,conf.level=0.9) # [0.15 0.36]
if(result$p.value<=0.05){
  print('we reject the null hypothesis')
}else{
  print('we cannot reject the null hypothesis')
}

# question 7
### a
n1 <- 20
n2 <- 16
var_1 <- 14.3
var_2 <- 12
x1 <- 4.1
x2 <- 3.1
X1 <- mvrnorm(n1,x1,var_1,empirical=TRUE)
X2 <- mvrnorm(n2,x2,var_2,empirical=TRUE)
result1 <- t.test(X1,X2,var.equal = TRUE)
if (result1$p.value<=0.05){
  print("we reject the null hypothesis")
} else{
  print("we cannot reject the null hypothesis")
}

### b
result2 <- t.test(X1,X2,alternative='two.sided',var.equal = FALSE)
if (result2$p.value<=0.05){
  print("we reject the null hypothesis")
} else{
  print("we cannot reject the null hypothesis")
}

### c
#### part a's p value
p.a <- result1$p.value

#### part b's p value
p.b <- result2$p.value

### d
t.test(X1,X2,var.equal = TRUE,conf.level = 0.95) # [-1.48, 3.48]

### e
n1 <- 24
n2 <- 12
X1 <- mvrnorm(n1,x1,var_1,empirical=TRUE)
X2 <- mvrnorm(n2,x2,var_2,empirical=TRUE)

##### var equal
result3 <- t.test(X1,X2,var.equal = TRUE)
if (result3$p.value<=0.05){
  print("we reject the null hypothesis")
} else{
  print("we cannot reject the null hypothesis")
}

##### var not equal
result4 <- t.test(X1,X2,var.equal = FALSE)
if (result4$p.value<=0.05){
  print("we reject the null hypothesis")
} else{
  print("we cannot reject the null hypothesis")
}

##### p value
p_a <- result3$p.value
p_b <- result4$p.value

# question 9
X <- mvrnorm(n=17,mu = 6.4,Sigma = 4.4^2,empirical=TRUE)
result <- t.test(X,mu=4.2,alternative = 'greater')
if (result$p.value<=0.05){
  print("we reject the null hypothesis")
} else{
  print("we cannot reject the null hypothesis")
}
##### p value
p_value <- result$p.value


# question 13
x1 <- 3.4
x2 <- 2.8
s1 <- 1.1
s2 <- 0.8
n1 <- 52
n2 <- 62
# H0: the mean lengths of unemployment in two communities are the same.
# H1 <- the mean lengths of unemployment in two communities are different.
library(MASS)
X1 <- mvrnorm(n1,x1,s1^2,empirical = TRUE)
X2 <- mvrnorm(n2,x2,s2^2,empirical = TRUE)
result <- t.test(X1,X2,alternative = "two.sided",var.equal=FALSE)
if(result$p.value<=0.05){
  print("we reject the null hypothesis")
} else{
  print("we cannot reject the null hypothesis")
}

# question 15
v <- c(100,426,322,466,112,155,388,1155,234,324,556,221,18,133,177,441)
mean.v <- mean(v)
sd.v <- sd(v)
length.v <- length(v)
x <- mvrnorm(length.v,mean.v,sd.v^2,empirical = TRUE)
t.test(x,conf.level=0.90) # [208.7856 444.7144]
t.test(x,conf.level=0.95) # [183.3228 470.1772]

# question 16
result <- prop.test(x=c(0.3*50,0.22*40),n=c(50,40),conf.level=0.9)
if(result$p.value<=0.01){
  print("we reject the null hypothesis")
} else{
  print("we cannot reject the null hypothesis")
}

# question 17
x1 <- 12.4
x2 <- 14.4
s1 <- 3
s2 <- 4
n <- 15
X1 <- mvrnorm(n,x1,s1^2,empirical=TRUE)
X2 <- mvrnorm(n,x2,s2^2,empirical=TRUE)

### variance equal
result <- t.test(X1,X2,alternative='two.sided',var.equal = TRUE)
if (result$p.value<=0.05){
  print("we reject the null hypothesis")
} else{
  print("we cannot reject the null hypothesis")
}
### variance not equal
result <- t.test(X1,X2,alternative='two.sided',var.equal = FALSE)
if (result$p.value<=0.05){
  print("we reject the null hypothesis")
} else{
  print("we cannot reject the null hypothesis")
}

# question 20
result <- prop.test(0.15*50,50,0.1)
if (result$p.value<=0.05){
  print('we reject the null hypothesis')
}else{
  print('we cannot reject the null hypothesis')
}