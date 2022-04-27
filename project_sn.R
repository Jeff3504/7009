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
value_within_limit = 106.67    # principle + fixed coupon at maturity
out_price = 110                # knock-out price
in_price = 90                  # knock-in price
obs_date = c("2022-5-25","2022-6-25","2022-7-25")  #observation date before maturity
rr = 0.05                      # risk-free rate
volatility = 0.16              # volatility of the index
number_of_simulation = 10      # number of MC simulation used



# define functions  ********************************************************************************************************************************

# calculate coupon at each observation date
get_coupon <- function(annualized = annualized_coupon, to_maturity = time_to_maturity, call_obs = obs_date){
  linspace(annualized/365,
           annualized*to_maturity, length(call_obs))
}

# calculate discount factor at each observation date
get_df <- function(theta = rr, call_obs = obs_date){
  issue_date <- Sys.Date()
  call_obs_date <- c()
  for (i in call_obs){
    call_obs_date[i] <- as.Date(i) - issue_date  #calculate time interval
  }
  
  discount_factor <- c()
  for (i in 1:(length(call_obs))){
    discount_factor[i] <- exp(-call_obs_date[i]/365*theta) 
  }
  discount_factor
}

# use index to retrieve price at each observation date from the simulation results
get_index <- function(call_obs = obs_date){
  issue_date <- Sys.Date()
  call_obs_date <- c()
  for (i in call_obs){
    call_obs_date[i] <- as.Date(i) - issue_date
  }
  call_obs_date
}

# calculate price at each observation date
get_price_data <- function(sim = path1, sim_num = number_of_simulation, 
                           price_index = index1, t = time_to_maturity){
  obs_price <- list()
  for (i in (1:sim_num)){
    each_sim <- c()
    
    for (j in price_index){
      each_sim[j] <- sim[((i-1)*(t+1)+j)] # may see the structure of the simulation results
    }
    each_sim <- each_sim[!is.na(each_sim)]
    obs_price[[i]] <- each_sim
  }
  obs_price
}

# calculate product price 
get_value <- function(price_data = price_data1, 
                      coupon = coupon1,
                      df = df1,
                      initial_value = start_price,
                      end_value = value_within_limit, 
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
        snowball_value[[i]] <- end_value  # get princlple + fixed coupon if no early exercise
      } 
    }
  }
  mean(unlist(snowball_value))
}


path1 <- GBM(N = time_to_maturity, M = number_of_simulation , T = 1 , t0 = 0, 
            x0 = start_price, theta = rr, sigma = volatility)  # generate simulation paths
coupon1 <- get_coupon()                                                                            
df1 <- get_df()
index1 <- get_index()
price_data1 <- get_price_data()


sn_value <- get_value()
sn_value

