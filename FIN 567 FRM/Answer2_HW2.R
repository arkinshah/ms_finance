require(tidyverse)
# Import csv file
returns <- read_csv("returns4symbols.2004-2018(2).csv")
returns <- tibble(returns)

# Stock Holdings
s <- 5000000
x <- 3000000
m <- 1000000
a <- 1000000

# Get 1% quantile
p <- -qnorm(0.01)

# Make table of returns from just 2008 to 2009
snipped_returns <- returns[1007:1511,]

# Initialise lists for sd's and covariances
sd_spy <- vector("list", 505)
sd_xlf <- vector("list", 505)
sd_msft <- vector("list", 505)
sd_aapl <- vector("list", 505)

# Initialise lists for covariances
cov_sx <- vector("list", 505) 
cov_sm <- vector("list", 505)
cov_sa <- vector("list", 505)
cov_xm <- vector("list", 505)
cov_xa <- vector("list", 505)
cov_ma <- vector("list", 505)

# Initialize list for DNVaR
DNVaR <- vector("list", 505)

it <- c(1:505)

# For loop to calculate the sd's and covariances
for (i in c(1:length(snipped_returns$date))){
  
  sd_spy[[i]] <- sqrt(sum(returns[1:1005+i,2]^2) / (1005 + i))

  sd_xlf[[i]] <- sqrt(sum(returns[1:1005+i,3]^2) / (1005 + i))

  sd_msft[[i]] <- sqrt(sum(returns[1:1005+i,4]^2) / (1005 + i))

  sd_aapl[[i]] <- sqrt(sum(returns[1:1005+i,5]^2) / (1005 + i))
  
  cov_sx[[i]] <- cov(returns[1:1005+i,2], returns[1:1005+i,3]) 
  cov_sm[[i]] <- cov(returns[1:1005+i,2], returns[1:1005+i,4])
  cov_sa[[i]] <- cov(returns[1:1005+i,2], returns[1:1005+i,5])
  cov_xm[[i]] <- cov(returns[1:1005+i,3], returns[1:1005+i,4])
  cov_xa[[i]] <- cov(returns[1:1005+i,3], returns[1:1005+i,5])
  cov_ma[[i]] <- cov(returns[1:1005+i,4], returns[1:1005+i,5])
  
  DNVaR[[i]] <- p * sqrt( 
                          ((s*sd_spy[[i]])^2) +  
                          ((x*sd_xlf[[i]])^2) +
                          ((m*sd_msft[[i]])^2) +
                          ((a*sd_aapl[[i]])^2) +
                          (2*s*x*cov_sx[[i]]*sd_spy[[i]]*sd_xlf[[i]]) +
                          (2*s*m*cov_sm[[i]]*sd_spy[[i]]*sd_msft[[i]]) + 
                          (2*s*a*cov_sa[[i]]*sd_spy[[i]]*sd_aapl[[i]]) +
                          (2*x*m*cov_xm[[i]]*sd_xlf[[i]]*sd_msft[[i]]) +
                          (2*x*a*cov_xa[[i]]*sd_xlf[[i]]*sd_aapl[[i]]) +
                          (2*m*a*cov_ma[[i]]*sd_msft[[i]]*sd_aapl[[i]])
                        )
}

# Coerce as double to make it easier to work with
snipped_returns$sd_spy <- as.double(sd_spy)
snipped_returns$sd_xlf <- as.double(sd_xlf)
snipped_returns$sd_msft <- as.double(sd_msft)
snipped_returns$sd_aapl <- as.double(sd_aapl)

snipped_returns$cov_sx <- as.double(cov_sx)
snipped_returns$cov_sm <- as.double(cov_sm)
snipped_returns$cov_sa <- as.double(cov_sa)
snipped_returns$cov_xm <- as.double(cov_xm)
snipped_returns$cov_xa <- as.double(cov_xa)
snipped_returns$cov_ma <- as.double(cov_ma)

snipped_returns$DNVaR <- as.double(DNVaR)

snipped_returns$days_since_2008 <- it

# Remove intermediate variables
rm(i, sd_aapl, sd_msft, sd_spy, sd_xlf, 
   cov_sx, cov_sm, cov_sa, cov_xm, cov_xa, cov_ma, DNVaR,
   s, x, m, a, p, it)

# Round off current values for neatness
round(snipped_returns$sd_spy, 6)
round(snipped_returns$sd_xlf, 6)
round(snipped_returns$sd_msft, 6)
round(snipped_returns$sd_aapl, 6)
round(snipped_returns$cov_sx, 8)
round(snipped_returns$cov_sm, 8)
round(snipped_returns$cov_sa, 8)
round(snipped_returns$cov_xm, 8)
round(snipped_returns$cov_xa, 8)
round(snipped_returns$cov_ma, 8)
round(snipped_returns$DNVaR, 2)

plot(snipped_returns$days_since_2008, snipped_returns$DNVaR, color = "blue", type = "l")
?cov.wt
