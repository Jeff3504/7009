install.packages("pracma")
install.packages("Sim.DiffProc")
install.packages("vtable")

library(dplyr)
library(ggplot2)
library(pracma)
library(Sim.DiffProc)
library(vtable)
library(qwraps2)


# parameters to be specified  **********************************************************************************************************************

annualized_coupon = 0.24       # coupon rate of the product
time_to_maturity = 120         # in days
up_barrier = 1.1               # up&out multiplier
down_barrier = 0.9             # down&out multiplier
rr = 0.05                      # risk-free rate
start_price = 100              # current price of the underlying
volatility = 0.14              # volatility of the underlying
number_of_simulation = 500     # number of simulations


# define functions  ********************************************************************************************************************************

# calculate coupon at each date
get_coupon <- function(coupon_rate = annualized_coupon, principle = start_price,
                       t = time_to_maturity){
  linspace(coupon_rate* principle/ 365,
           coupon_rate* principle/ 365* t, (t-1))
}

# calculate discount factor at each date
get_df <- function(theta = rr, t = time_to_maturity){
  
  discount_factor <- c()
  for (i in 1:t){
    discount_factor[i] <- exp(-i/ 365* theta) 
  }
  discount_factor
}

# generate a list in which the 1st order index indicates a simulated path, and
# the 2nd order index indicates a daily stock price in that path
get_price_data <- function(sim = path1, sim_num = number_of_simulation, 
                           t = time_to_maturity){
  obs_price <- list()
  for (i in (1:sim_num)){
    each_sim <- c()
    
    for (j in 1:t){
      each_sim[j] <- sim[((i-1)* (t+1)+ j)] 
    }
    each_sim <- each_sim[!is.na(each_sim)]
    obs_price[[i]] <- each_sim
  }
  obs_price
}

# once knocked out, the contract is terminated and investors receive a higher coupon rate
# or they just receive risk-free rate at maturity
get_value <- function(price_data = price_data1, 
                      rate_of_return = annualized_coupon,
                      t = time_to_maturity,
                      rf = rr,
                      coupon = coupon1,
                      df = df1,
                      initial_value = start_price,
                      up_out_barrier = up_barrier,
                      down_out_barrier = down_barrier
){
  snowball_value = list()
  for (i in 1:length(price_data)){
    n <- 0
    for (j in price_data[[i]]){
      n <- n + 1
      if (j > up_out_barrier* initial_value){
        snowball_value[[i]] <-  coupon[n]* df[n] # contract ends immediately when knocked-out
        break
      }
      else if (j < down_out_barrier* initial_value){
        snowball_value[[i]] <-  coupon[n]* df[n] # contract ends immediately when knocked-out
        break 
      }
      else if (n == length(price_data[[i]])){
        snowball_value[[i]] <- 0 # receive rf rate at maturity
      } 
    }
  }
  snowball_value
#  mean(unlist(snowball_value)) gives the price of the product, 
#  yet NA occurs more often as the number of simulations increases,
#  though I should have eliminated them beforehand :/
}


path1 <- GBM(N = time_to_maturity, M = number_of_simulation , T = 1 , t0 = 0, 
             x0 = start_price, theta = rr, sigma = volatility)  # generate simulation paths
plot(path1)
coupon1 <- get_coupon()                                                                            
df1 <- get_df()
price_data1 <- get_price_data()

product_price <- get_value()
product_price 
hist(unlist(product_price), breaks = 50, main = ("Histogram of the Squash Option" ), xlab = "Price of the Squash Option")
mean(unlist(product_price))