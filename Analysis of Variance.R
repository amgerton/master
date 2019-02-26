 #Problem 1: 
data1 = read.table('/Users/alanna/Downloads/CH16PR12.txt', header=F)

names(data1) = c('response', 'factor_level', 'j')
fit1 = aov(response ~ factor(factor_level), data = data1)
summary(fit1)

#a) Obtain the residuals prepare aligned residual dot plots by agent. 

#aligned residual dot plot:
par(mfrow = c(1,1))
plot(fit1$fitted, fit1$residuals, main = "Residual Plot", xlab= "fitted", ylab = "residual")
abline(h = 0)
plot(fit1$fitted.values, rij, main = "Residual Plot", xlab = "Fitted", ylab = "studentized residuals")
abline(h = 0)

#b)Prepare a normal probability plot of the residuals.

 #Studentized Residuals for Normal Prob. Plot:
si = tapply(fit1$residuals, data1$factor_level, sd)
newseij = sqrt(si^2*(ni-1)/ni)
newrij = fit1$residuals/rep(newseij, ni)

# Normal Q-Q Plot
par(mfrow = c(1,1))
qqnorm(fit1$residuals); qqline(fit1$residuals)
qqnorm(newrij);qqline(newrij)

# (c) Prepare residual sequence plots and interpret them

# d*) Obtain the studentized residuals and check how many fall outside the interval[−3, 3].
#studentized residual
nT = length(data1$response)
ni = tapply(data1$response, data1$factor_level, length)
r = 5
MSE = sum(fit1$residuals^2)/(nT - r)
seij = sqrt(MSE*(ni-1)/(ni))
rij = fit1$residuals/rep(seij, ni)
rij
seij

  # Problem 2:

library(car) #for boxCox()
library(MASS) #for boxcox()
library(knitr) #for kable() which organizing table (not necessary)

#Perform ANOVA
df = read.table('/Users/alanna/Downloads/CH18PR17 (1).txt', header = F)
names(df) = c("values", "i", "j")
anova.model = aov(values ~ as.factor(i), data = df)
summary(anova.model)

# (a) Obtain the fitted values and residuals of the ANOVA model.
df_values = cbind(df$i, df$values, anova.model$fitted.values, anova.model$residuals)
colnames(df_values) = c("Factor level", "Y", "Fitted", "Residual")
knitr::kable(df_values)

#(b) Prepare suitable residual plots to study whether or not the error variances are equal for the four winding speeds.
#Residual plot
plot(anova.model$fitted.values, anova.model$residuals,
     xlab = 'Fitted Value', ylab = "Residual", main = "Residuals vs. Fitted Values")
abline(h = 0, col="red")  #no the variances are not same

# (c) Test by means of Brown-Forsythe test whether or not the treatment error variances are equal, at α = 0.05.
Y = df$values
nT = length(Y)
ymedian = tapply(df$values, df$i, median)
ni = tapply(df$values, df$i, length)
dij = abs(df$values - rep(ymedian, ni))
d_data = cbind(dij, df)
dbar = tapply(d_data$dij, d_data$i, mean)
dtotalmean = mean(d_data$dij)
SSTRd = sum(ni*(dbar - dtotalmean)^2)
MSTRd = SSTRd/2
SSTd = sum((dij - dtotalmean)^2)
SSEd = SSTd - SSTRd
MSEd = SSEd/(sum(ni)-3)
Fbf = MSTRd/MSEd

alpha=0.05
1- pf(Fbf, 3, 59) #pf(Fbf, 2, 21, lower.tail = F)
anova_d = aov(dij ~ as.factor(i), data = d_data)
summary(anova_d)

# (d*) Perform Hartley’s test to see whether or not the treatment error variances are equal, at α = 0.05.

###carry out the Hartley's test for equality of treatment group variances. 
s.sq = tapply(df$values, df$i,var)
H = max(s.sq)/min(s.sq)
#look at B.10 for H(1-0.05, 4, 64-1) = H(1-alpha, r, n-1)
H.crit = 1.85 #approximately
H > H.crit #TRUE: reject H0 if it's H > H.crit

# (e*) For each winding speed, calculate Y i· and si

ybar = tapply(df$values, df$i, mean) #vector of Y_ibar
s.sq = tapply(df$values, df$i, var) #vector of s_i
ratiotable = rbind(s.sq/ybar, sqrt(s.sq)/ybar, sqrt(s.sq)/ybar^2)
rownames(ratiotable) = c('s^2/ybar', 's/ybar', 's/ybar^2')
knitr::kable(ratiotable) #kable function returns the organized form of table. You can return ratiotable only either.

#plot the ratio
plot(s.sq/ybar,ylim=c(0,1.8), ylab = "", type ="b", xlab = "Factor level")
lines(sqrt(s.sq)/ybar,col=2,ylim=c(0,2), type = "b") 
lines(sqrt(s.sq)/ybar^2,col=3,ylim=c(0,2), type = "b")

#(f*) Use the Box-Cox procedure to find an appropriate power transformation of Y .

library(car)
Y1 <- Y + 1 #strictly positive
bc = car::boxCox(Y1 ~ as.factor(df$i), lambda=seq(-2,2,by=.1))

lambda = bc$x[which.max(bc$y)] # find which maximizing log-likelihood = minimizing SSE
lambda

###################SSE for the best lambda by boxcox ###########
# lecture note notation
K2 = prod(Y1)^(1/nT)
K1 = 1/(lambda*K2^(lambda-1))
Y.star = K1*(Y1^lambda-1)
plot(Y1, Y.star) #this almost looks like the sqrt curve

new.model=aov(Y.star~as.factor(df$i))
summary(new.model)

SSE=sum(new.model$residual^2)
SSE ###### best SSE=90.28356

##########SSE for the lambda given in table 18.6########
l = seq(-1,1, by = 0.2)
# or c(-1.0 ,-.80 ,-.60 ,-.40, -.20 ,-.10,0,0.1,0.2,0.4,0.6,0.8,1)
# repeat the sse calculation with different lambda in l
sse_table = sapply(1:length(l), function(i){
  lambda = l[i] # updating lamdba
  K2=prod(Y1)^(1/nT)
  K1=1/(lambda*K2^(lambda-1))
  ##calculate it separately for lambda = 0
  if(lambda != 0) Y.star = K1*(Y1^lambda-1) else Y.star=K2*log(Y1)
  n.model = aov(Y.star ~ as.factor(df$i))
  c(l[i], sum(n.model$residual^2)) #return lambda, sse
})
rownames(sse_table) = c("lambda", "SSE")
knitr::kable(t(sse_table)) #knitr::kable(sse_table)
