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
time_to_maturity = 132         # in days
start_price = 100              # current price 
out_price = 110                # knock-out price
in_price = 90                  # knock-in price
rr = 0.05                      # risk-free rate
volatility = 0.16              # volatility of the index
number_of_simulation = 10      # number of MC simulation used



# define functions  ********************************************************************************************************************************

# calculate coupon at each observation date
get_coupon <- function(coupon_rate = annualized_coupon, principle = start_price,
                       t = time_to_maturity){
  linspace(coupon_rate*principle/365,
           coupon_rate*principle/365*t, (t-1))
}

# calculate discount factor at each observation date
get_df <- function(theta = rr, t = time_to_maturity){
  
  discount_factor <- c()
  for (i in 1:t){
    discount_factor[i] <- exp(-i/365*theta) 
  }
  discount_factor
}


get_price_data <- function(sim = path1, sim_num = number_of_simulation, 
                           t = time_to_maturity){
  obs_price <- list()
  for (i in (1:sim_num)){
    each_sim <- c()
    
    for (j in 1:t){
      each_sim[j] <- sim[((i-1)*(t+1)+j)] 
    }
    each_sim <- each_sim[!is.na(each_sim)]
    obs_price[[i]] <- each_sim
  }
  obs_price
}

# calculate product price 
get_value <- function(price_data = price_data1, 
                      t = time_to_maturity,
                      rf = rr,
                      coupon = coupon1,
                      df = df1,
                      initial_value = start_price,
                      out_barrier = out_price,
                      in_barrier = in_price
){
  snowball_value = list()
  for (i in 1:length(price_data)){
    n <- 0
    for (j in price_data[[i]]){
      n <- n + 1
      if (j > out_barrier){
        snowball_value[[i]] <- (j-initial_value)*coupon[n]*df[n]  # contract ends immediately when knocked-out
        break
      }
      else if (j < in_barrier){
        snowball_value[[i]] <- (initial_value-j)*coupon[n]*df[n] # contract ends immediately when knocked-in
        break 
      }
      else if (n == length(price_data[[i]])){
        snowball_value[[i]] <- abs(initial_value - dplyr::last(price_data[[i]]))*rf*t/365
      } 
    }
  }
  snowball_value
  # mean(unlist(snowball_value))
}


path1 <- GBM(N = time_to_maturity, M = number_of_simulation , T = 1 , t0 = 0, 
             x0 = start_price, theta = rr, sigma = volatility)  # generate simulation paths
coupon1 <- get_coupon()                                                                            
df1 <- get_df()
price_data1 <- get_price_data()


sn_value <- get_value()
sn_value

