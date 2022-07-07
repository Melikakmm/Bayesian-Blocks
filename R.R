
#-------------------------------------------------------------------------------
#packages
install.packages("svMisc")
#-------------------------------------------------------------------------------
#libraries
library(svMisc)
library('lubridate')



#Juilia's data:
#producing some fake data to see the result for it:
data = c(rnorm(1000, mean = 0), rcauchy(1000, 5))
data = data[data> -5 & data<10]

typeof(data)


#George's data

data <- read.table("/Users/melikakeshavarz/Desktop/adv/spectrum.dat")
data = data$V1


#------------------------------------------------------------------------------
#defining the fitness function:
logf <- function(N_k, T_k){
  return(N_k * log(N_k/T_k))
}


#------------------------------------------------------------------------------
#The fitness function = Cash#

BB <- function(x) {
  
  #for atomic data vectors(1D):
  if (class(x) == 'numeric' | class(x) == 'matrix' ) {
    x_sorted = sort(x)
    x_unique <- c(x_sorted[1])
    x_weight <- c(1)
    for(i in 2:length(x_sorted)) {
      #adding weight if it is repeated:
      if(x_sorted[i] == x_sorted[i-1]) {
        x_weight[length(x_weight)] <- x_weight[length(x_weight)] + 1
      }
      else {
        x_unique <- c(x_unique, x_sorted[i])
        x_weight <- c(x_weight, 1)
      }
    }    
  }#---------------------------------------------------------- 
  
  #For histogram data:        
  else if (class(x) == 'histogram') {
    eg <- x$breaks
    x_unique <- c(eg[1],x$mids,eg[length(eg)])
    x_weight <- c(1,x$counts,1)
  }        
  else {
    stop('The data type is not supported! Please input numeric vectors or histograms')
    
  }#------------------------------------------------------------ 
  
  
  #Final number of data points:       
  N = length(x_unique) 
  
  
  
  
  
  
  
  #defining a prior dis function:
  logp <- function(prior) {
    if(prior == 'uniform') {
      return(0)
    }
    else if(prior == 'gamma') {
      #gamma = as.numeric(readline('input the parameter gamma: '))
      gamma = 0.01
      return(-log(gamma))
    }
    else if(prior == 'p0') {
      #p0 = as.numeric(readline('input the parameter p0: '))
      p0 = 0.01
      return( log(73.53 * p0 * N^(-0.478)) - 4)
      
    }
    else {
      stop('No Prior Distribution input!')
    }
    
  }
  
  x <- x_unique
  
  #input one of these:
  #1.uniform           2.gamma              3.p0
  prior_input = readline('Please input the type of prior distribution: ')
  
  #calculating the prior:
  ncp_prior = logp(prior = prior_input)
  
  
  #array of all possible edges:
  edges <- c(x[1], 0.5*(x[1:(length(x)-1)] + x[2:length(x)]), x[length(x)])
  
  
  best = c()
  last = c()
  
  
  F <- function(r, k){
    return(logf(sum(x_weight[r:k]), edges[k+1] - edges[r]) + ncp_prior)
  } 
  
  for (k in 1:N){
    A <- c()
    for (r in 1:k){
      A <- c(A, F(r, k)+ if(r==1) 0 else best[r-1])
    }
    last <- c(last, which.max(A))
    best <- c(best, max(A, na.rm=TRUE))
    progress(k, N, progress.bar=TRUE)
    if (k == N) message("Done!")
  }
  
  
  
  
  cp <- c()
  i <- N + 1
  while(i!=0){
    cp <- c(cp, i)
    i = (if(i == 1) 0 else last[i - 1])
  }
  
  y <- c()
  for (j in cp[length(cp):-1:1]){
    y <- c(y, edges[j])
  }
  return(y)
  
  
}
#-------------------------------------------------------------------------------
hist(data)
breaks = BB(data)



N <- length(data)

col1 = scales::alpha(2,.55)
col2 = scales::alpha(4,.55)

# sqrt(N)
hist(data,breaks=sqrt(N),freq=FALSE,col=col1,border=F, panel.first=grid(),density=200,
     xlab='x',ylab='Intensity?',main=('Histogram'))
# Bayesian Blocks
hist(data,breaks=breaks,density=200, col=col2,border=F,add=T)

text_legend <- c(sprintf("sqrt(N) blocks = %3i",floor(sqrt(N))), sprintf("Bayesian Blocks = %3i",length(breaks)))

legend("topright", 
       legend=text_legend,
       fill=c(col1, col2),
       text.width = strwidth(text_legend)[2]/2)
box()

#----------------------------------------------------- --------------------------
#Stock market


par(mfrow=c(1,1))
options(repr.plot.width=20, repr.plot.height=8)



SP = read.csv('/Users/melikakeshavarz/Desktop/adv/SPX_HistoricalData.csv')
SP = head(SP, 1000)
sort(unique(SP$Date))
Date = as.character(SP$Date)

#year/month/day
SP$Date = mdy(Date)
Date = SP$Date
Date


#plotting 

plot(Date, SP$High, type = 'l', col = 'forestgreen', xlab = 'Dates', ylab = 'High', main = 'S&P', panel.first = grid())



#######How to make it a historam:
#approximation in oder to get round data:
SP$High<-as.integer(SP$High)
SP

######The matrix of date destribution w.r.t Highest price:
MatData <- matrix(0,nrow=sum(SP$High),ncol=1)

k <- 1


for(i in 1:dim(SP)[1]){
  L <- SP$High[i]
  if(L!=0){
    MatData[k:(k+L-1)] <- rep(SP$Date[i],L)
    k <- k + L
  }
  else{
    pass
  }
}

#omitting zero values:
MatData[MatData==0] <- NA
na.omit(MatData)


#Checking%
sort(unique(MatData))

#Converting number of days to dates:
MData = as.Date(MatData, origin = '1970-01-01')
MData[0:100]



breaks = 600
hist(MData,breaks=breaks,freq=FALSE,col=scales::alpha('salmon2',.55),border=F, panel.first=grid(),density=200,
     xlab='Dates',ylab='Noise of the high price', ylim = c(0, 0.002))

box()

#-------------------------------------------------------------------------------
#applying the Bayesin Blocks to the stock market data:
BB_breaks <- BB(MatData)
BB_breaks
myhist <- hist(MatData, breaks= 600, plot = F)
BBhist <- hist(MatData , breaks= BB_breaks, plot= F)


par(mfrow=c(1,1))
options(repr.plot.width=20, repr.plot.height=8)
#myhist plot:
plot(x=myhist$breaks[1:length(myhist$breaks)], y=c(myhist$density,tail(myhist$density,n=1)), log="y", panel.first=grid(),
     col='forestgreen',lwd=1.5, type='s', xlab='dates in number (origin = 1970-01-01)',ylab='Highest price in a day',
     main="Histogram of S&P 500 index during Covid19")
# Bayesian Blocks plot:
lines(x=BBhist$breaks[1:length(BBhist$breaks)], y=c(BBhist$density,tail(BBhist$density,n=1)), type='s', lwd=1.5,col='tomato')

legend("topright", inset=+0,legend=c(sprintf("Fixed number of edges: %3i",length(myhist$breaks)),sprintf("Bayesian Blocks: %3i",length(BBhist$breaks))),
       col=c("forestgreen","tomato"),lty=c(1,1), lwd=c(1.5, 1.5), cex=0.4,box.lty=0, border=F)

