install.packages("smfsb") 

rm(list = ls())

library(smfsb)

stepLVc(c(x1=50,x2=100),0,1)

data(LVdata)

plot(LVnoise10,plot.type="single",col=c(2,4))

pfMLLik <- function (n, simx0, t0, stepFun, dataLik, data) 
{
  times = c(t0, as.numeric(rownames(data)))
  deltas = diff(times)
  return(function(...) {
    xmat = simx0(n, t0, ...)
    ll = 0
    for (i in 1:length(deltas)) {
      xmat = t(apply(xmat, 1, stepFun, t0 = times[i], deltat = deltas[i], ...))
      w = apply(xmat, 1, dataLik, t = times[i + 1], y = data[i,], log = FALSE, ...)
      if (max(w) < 1e-20) {
        warning("Particle filter bombed")
        return(-1e+99)
      }
      ll = ll + log(mean(w))
      rows = sample(1:n, n, replace = TRUE, prob = w)
      xmat = xmat[rows, ]
    }
    ll
  })
}


# set up data likelihood
noiseSD=10
dataLik <- function(x,t,y,log=TRUE,...)
{
  ll=sum(dnorm(y,x,noiseSD,log=TRUE))
  if (log)
    return(ll)
  else
    return(exp(ll))
}
# now define a sampler for the prior on the initial state
simx0 <- function(N,t0,...)
{
  mat=cbind(rpois(N,50),rpois(N,100))
  colnames(mat)=c("x1","x2")
  mat
}

# convert the time series to a timed data matrix
LVdata=as.timedData(LVnoise10)

# create marginal log-likelihood functions, based on a particle filter
mLLik=pfMLLik(100,simx0,0,stepLVc,dataLik,LVdata)

iters=1000
tune=0.01
thin=10
th=c(th1 = 1, th2 = 0.005, th3 = 0.6)
p=length(th)
ll=-1e99
thmat=matrix(0,nrow=iters,ncol=p)
colnames(thmat)=names(th)
# Main pMCMC loop
for (i in 1:iters) {
  message(paste(i,""),appendLF=FALSE)
  for (j in 1:thin) {
    thprop=th*exp(rnorm(p,0,tune))
    llprop=mLLik(thprop)
    if (log(runif(1)) < llprop - ll) {
      th=thprop
      ll=llprop
    }
  }
  thmat[i,]=th
}
message("Done!")
# Compute and plot some basic summaries
mcmcSummary(thmat)



