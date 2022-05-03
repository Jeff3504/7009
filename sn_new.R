install.packages("pracma")
install.packages("Sim.DiffProc")
install.packages("vtable")

library(pracma)
library(Sim.DiffProc)
library(ggplot2)
library(dplyr)
library(tidyquant)
library(vtable)

# download price data using tidyquant  *************************************************************************************************************

etf <- tq_get("2801.HK",from = '2000-01-01', to = Sys.Date(), get = "stock.prices")
etf %>%  ggplot(aes(x = date, y = adjusted)) +  geom_line() +  ggtitle("ETF") +  labs(x = "Date", "Price") +  
  scale_x_date(date_breaks = "years", date_labels = "%Y") +  labs(x = "Date", y = "Adjusted Price") +  theme_bw()

etf_daily_returns <- etf %>%  tq_transmute(select = adjusted,  mutate_fun = periodReturn,  period = "daily",  col_rename = "etf_returns") 
etf_daily_returns <- etf_daily_returns[which(rowSums(etf_daily_returns==0)==0),]

st(etf_daily_returns) #statistical description



# parameters to be specified  **********************************************************************************************************************

annualized_coupon = 0.24       # coupon rate of the product
time_to_maturity = 120         # in days
start_price = 100              # current price 
up_barrier = 1.1               # up&out multiplier
down_barrier = 0.9             # down&out multiplier
rr = 0.05                      # risk-free rate
volatility = 0.16              # volatility of the index
number_of_simulation = 10      # number of MC simulation used



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
        snowball_value[[i]] <- (j-initial_value)* coupon[n]* df[n] # contract ends immediately when knocked-out
        break
      }
      else if (j < down_out_barrier* initial_value){
        snowball_value[[i]] <- (initial_value-j)* coupon[n]* df[n] # contract ends immediately when knocked-out
        break 
      }
      else if (n == length(price_data[[i]])){
        snowball_value[[i]] <- abs(initial_value - dplyr::last(price_data[[i]]))*
          rf* t/ 365* dplyr::last(df)                              # receive rf rate at maturity
      } 
    }
  }
  mean(unlist(snowball_value))
}


path1 <- GBM(N = time_to_maturity, M = number_of_simulation , T = 1 , t0 = 0, 
             x0 = start_price, theta = rr, sigma = volatility)  # generate simulation paths
coupon1 <- get_coupon()                                                                            
df1 <- get_df()
price_data1 <- get_price_data()


sn_value <- get_value()
sn_value

