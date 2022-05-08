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

annualized_coupon <- 0.24       # coupon rate of the product
time_to_maturity <- 120         # in days
up_barrier <- 1.1               # up&out multiplier
down_barrier <- 0.9             # down&out multiplier
rr <- 0.03                      # risk-free rate
start_price <- 100              # current price of the underlying
volatility <- 0.14              # volatility of the underlying
number_of_simulation <- 1000    # number of simulations


# define functions  ********************************************************************************************************************************

# Calculate coupon at each date
get_coupon <- function(coupon_rate = annualized_coupon, principal = start_price,
                       t = time_to_maturity){
  linspace(coupon_rate* principal/ 365,
           coupon_rate* principal/ 365* t, (t-1))
}

# Calculate discount factor at each date
get_df <- function(theta = rr, t = time_to_maturity){
  
  discount_factor <- c()
  for (i in 1:t){
    discount_factor[i] <- exp(-i/ 365* theta) 
  }
  discount_factor
}

# Generate a list in which the 1st order index indicates a simulated path, and
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

# Once knocked out, the contract is terminated and investors receive a higher coupon rate
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
  squash_value = list()
  for (i in 1:length(price_data)){
    n <- 0
    for (j in price_data[[i]]){
      n <- n + 1
      if (j > up_out_barrier* initial_value){
        squash_value[[i]] <-  coupon[n]* df[n] 
        # contract ends immediately when knocked-out
        break
      }
      else if (j < down_out_barrier* initial_value){
        squash[[i]] <-  coupon[n]* df[n] 
        # contract ends immediately when knocked-out
        break 
      }
      else if (n == length(price_data[[i]])){
        squash_value[[i]] <- 0 
        # receive zero at maturity
      } 
    }
  }
  squash_value
  #  mean(unlist(squash_value), na.rm = TRUE) gives the price of the product, 
  #  yet NA occurs more often as the number of simulations increases,
  #  though I should have eliminated them beforehand :/
}


# calculate price  ********************************************************************************************************************************

# Generate simulation paths

path1 <- GBM(N = time_to_maturity, 
             M = number_of_simulation , 
             T = 1 , 
             t0 = 0, 
             x0 = start_price, 
             theta = rr, 
             sigma = volatility)

# Calculate relevant inputs, i.e., coupon payments, discount factors.
coupon1 <- get_coupon()                                                                            
df1 <- get_df()
price_data1 <- get_price_data()

product_price <- get_value()

# Product_price 
mean(unlist(product_price), na.rm = TRUE)

# Visualization
hist(unlist(product_price), breaks = 50, main = ("Monte Carlo Simulation of the Squash Option"), xlab = "Payoff of the Squash Option")

df <- data.frame(Mean = c(2.8706, 2.7325, 2.8293, 2.8626, 2.8408),  
                 sd = c(0.5194, 0.2765, 0.1290, 0.0430, 0.0205),
                 Simu_num = c("A", "B", "C", "D", "E"))  # A:10, B:100, C:500, D:1000, E:10000
p <- ggplot(df, aes(x = Simu_num, y = Mean)) + 
  geom_point() +
  geom_errorbar(aes(ymin = Mean - sd, ymax = Mean + sd), width = 0.2)
p  # Simulation times and accuracy