# Import libraries
library(ChainLadder)

n <- 7

# Claim dataset Thefirst column holds the origin year, 
# the second column the development
# year and the third column has the incremental payments / transactions.
Claims <- data.frame(originf = factor(rep(2007:2013, n:1)), 
                     dev=sequence(n:1),
                     inc.paid= c(3511, 3215, 2266, 1712, 1059, 587,340, 
                                 4001, 3702, 2278, 1180, 956,629, 4355, 3932, 
                                 1946, 1522, 1238,4295, 3455, 2023, 1320, 4150, 
                                 3747,2320, 5102, 4548, 6283))

# To present the data in a triangle format, we can use the matrix function
(inc.triangle <- with(Claims, {M <- matrix(nrow=n, ncol=n,dimnames=list(origin=levels(originf), dev=1:n))
M[cbind(originf, dev)] <- inc.paid
M
}))

# It is the objective of a reserving exercise to forecast the future 
# claims development in the bottom right corner of the triangle and 
# potential further developments beyond development age 7

# cumulative development of claims
(cum.triangle <- t(apply(inc.triangle, 1, cumsum)))

# latest cumulative paid position of all origin years
(latest.paid <- cum.triangle[row(cum.triangle) == n - col(cum.triangle) + 1])

# add the cumulative paid data as a column to the data frame
Claims$cum.paid <- cum.triangle[with(Claims, cbind(originf, dev))]

# To start the reserving analysis, we plot the data:
op <- par(fig=c(0,0.5,0,1), cex=0.8, oma=c(0,0,0,0))
with(Claims, {interaction.plot(x.factor=dev, trace.factor=originf, response=inc.paid,fun=sum, type="b", bty='n', legend=FALSE); axis(1, at=1:n)
  par(fig=c(0.45,1,0,1), new=TRUE, cex=0.8, oma=c(0,0,0,0))
  interaction.plot(x.factor=dev, trace.factor=originf, response=cum.paid,
                   fun=sum, type="b", bty='n'); axis(1,at=1:n)
})
mtext("Incremental and cumulative claims development",side=3, outer=TRUE, line=-3, cex = 1.1, font=2)
par(op)

library(lattice)
xyplot(cum.paid ~ dev | originf, data=Claims, t="b", layout=c(4,2),as.table=TRUE, main="Cumulative claims development")

# Chain-Ladder Algorithm: oldest method or algorithm for estimating reserves is
# the so-called chain-ladder method or loss development factor (LDF) method.
# The classical chain-ladder method is a deterministic algorithm to forecast claims based
# on historical data. It assumes that the proportional developments of claims from one development
# period to the next is the same for all origin periods.

# first step, the age-to-age link ratios fk are calculated as the volume
# weighted average development ratios of a cumulative loss development triangle from one
# age period to the next
f <- sapply((n-1):1, function(i) {
  sum( cum.triangle[1:i, n-i+1] ) / sum( cum.triangle[1:i, n-i] )
})

# Initially we expect no further development after year 7. Hence, we set the last link ratio
# (often called the tail factor) to 1
tail <- 1
(f <- c(f, tail))

# The squaring of the claims triangle
full.triangle <- cum.triangle
for(k in 1:(n-1)){
  full.triangle[(n-k+1):n, k+1] <- full.triangle[(n-k+1):n,k]*f[k]
}
full.triangle

# The last column contains the forecast ultimate loss cost
(ultimate.paid <- full.triangle[,n])

# The cumulative products of the age-to-age development ratios provide the loss development
# factors for the latest cumulative paid claims for each row to ultimate
(ldf <- rev(cumprod(rev(f))))

# The inverse of the loss development factor estimates the proportion of claims developed to
# date for each origin year, often also called the gross up factors or growth curve
(dev.pattern <- 1/ldf)

# The total estimated outstanding loss reserve with this method is
(reserve <- sum (latest.paid * (ldf - 1)))
