library(chron)
library(ggplot2)
###############
## LOAD DATA ##
###############

Data <- read.table(file = 'A1_temp_data.tsv', sep = '\t', header = TRUE)
Data$Timestamp <- strptime(x=paste(Data$Date,Data$Time),format="%d-%m-%Y %H:%M:%S")
Data$TimeDiff <- difftime(Data$Timestamp,Data$Timestamp[1])
Est <- Data[1:500,]
Comp <- Data[-1:-500,]


##########
## EX 1 ##
##########
p <- ggplot(data = Est,aes(x = TimeDiff , y= Temperature)) +geom_line()
  #qplot(Est$TimeDiff,Est$Temperature)

##########
## EX 2 ##
##########
g_mean <- mean(Est$Temperature)
g_var <- var(Est$Temperature)
#p <- p + geom_hline(aes(yintercept=g_mean),col="blue")
#p <- p + geom_hline(aes(yintercept=g_mean-g_var),col="blue",linetype="dashed")
#p <- p + geom_hline(aes(yintercept=g_mean+g_var),col="blue",linetype="dashed")

##########
## EX 3 ##
##########
GLM <- lm(Est$Temperature~Est$TimeDiff)
print(GLM)
#p <- p + stat_smooth(data = Est, aes( x = TimeDiff, y = Temperature),method = "lm",col="red",se = FALSE)

##########
## EX 4 ##
##########
Est$Y_lcmm = Est$Temperature #new variable representing the local constant mean model
lambda = 0.8
for (i in 2:500){
  #equation (3.74) 
  Est$Y_lcmm[i] = (1-lambda)*Est$Temperature[i-1] + lambda*Est$Y_lcmm[i-1]
} 

# Computing the 2 hour prediction
pred_lcmm <- numeric(4)
pred_lcmm[1] <- (1-lambda)*Est$Temperature[500] + lambda*Est$Y_lcmm[500]
for(i in 2:4){
  pred_lcmm[i] <- (1-lambda)*Est$Temperature[500] + lambda*pred_lcmm[i-1]
}
#p <- p + geom_line(data = Est,aes(x = TimeDiff, y = Y_lcmm),linetype="dashed",col="blue")

##########
## EX 5 ##
##########
# Initializing the values
lambda = 0.8
Est$Y_lltm = Est$Temperature
L = rbind(c(1,0),c(1, 1))
F = rbind(1,0) %*% cbind(1,0)
h = rbind(1,0)*Est$Temperature[1]
# Loop over the data
for (i in 1:499){
  # Updating F, h and theta
  F = F + lambda^(i) * rbind(1,-i) %*% cbind(1,-i)
  h = lambda * solve(L) %*% h + rbind(1,0) * Est$Temperature[i]
  theta = solve(F) %*% h
  # computing the one step estimate
  Est$Y_lltm[i+1] = cbind(1,1)%*%theta
}

# 2 hours prediction:
F = F + lambda^(500) * rbind(1,-500) %*% cbind(1,-500)
h = lambda * solve(L) %*% h + rbind(1,0) * Est$Temperature[500]
theta = solve(F) %*% h

pred_lltm <- numeric(4)
for(i in 1:4){
  pred_lltm[i] <- cbind(1,i)%*%theta
}

p <- p + geom_line(data = Est,aes( x = TimeDiff, y = Y_lltm),linetype="dashed",col="green")


##########
## EX 6 ##
########## 
lambdatest <- function(lambda){
  Est$Y_lltm = Est$Temperature
  L = rbind(c(1,0),c(1, 1))
  F = rbind(1,0) %*% cbind(1,0)
  h = rbind(1,0)*Est$Temperature[1]
  # Loop over the data
  for (i in 1:499){
    # Updating F, h and theta
    F = F + lambda^(i) * rbind(1,-i) %*% cbind(1,-i)
    h = lambda * solve(L) %*% h + rbind(1,0) * Est$Temperature[i]
    theta = solve(F) %*% h
    # computing the one step estimate
    Est$Y_lltm[i+1] = cbind(1,1)%*%theta
  }
  mean((Est$Y_lltm[101:500]-Est$Temperature[101:500])^2)
}

c = 0.01
for (i in 1:100){
  #print(c(c,lambdatest(c)))
  c = c +0.01
}

##########
## EX 7 ##
##########
lambdatest2 <- function(lambda){
  Est$Y_lltm = Est$Temperature
  L = rbind(c(1,0),c(1, 1))
  F = rbind(1,0) %*% cbind(1,0)
  h = rbind(1,0)*Est$Temperature[1]
  # Loop over the data
  for (i in 1:496){
    # Updating F, h and theta
    F = F + lambda^(i) * rbind(1,-i) %*% cbind(1,-i)
    h = lambda * solve(L) %*% h + rbind(1,0) * Est$Temperature[i]
    theta = solve(F) %*% h
    # computing the one step estimate
    Est$Y_lltm[i+4] = cbind(1,4)%*%theta
  }
  mean((Est$Y_lltm[101:500]-Est$Temperature[101:500])^2)
}
c = 0.01
for (i in 1:100){
  #print(c(c,lambdatest2(c)))
  c = c +0.01
}

##########
## EX 8 ##
##########

q <- ggplot(data = Data[450:504,],aes(x = TimeDiff , y= Temperature)) +geom_line()
q <- q + geom_line(data = Est[450:500,],aes(x = TimeDiff, y = Y_lcmm),linetype="dashed",col="blue")
q <- q + geom_line(data = Est[450:500,],aes( x = TimeDiff, y = Y_lltm),linetype="dashed",col="green")
q = q + geom_line(data =Data[501:504,], aes(x=TimeDiff, y = pred_lcmm),col="blue")
q = q + geom_line(data =Data[501:504,], aes(x=TimeDiff, y = pred_lltm),col="green")

print(p)
print(q)
