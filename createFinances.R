#This file creates the financial data for a service member.

source("helpers.R")

create_finances <- function(birth_date, life_expectancy, basd, 
                            career_path, date_tsp_switch, tsp_type,
                            retirement_year, tsp_withdrawl,
                            market_return, tsp_cont, continuation_pay,
                            pay_to_tsp_cont){
  
  #We are going to set up some vairables to use later 
  c <- career_path
  eol <- as.Date(birth_date) + life_expectancy*365.25 #this is the date of end of life
  param.r <- (market_return/12.0) #returns are calculated by month
  rvs_needed <- as.integer((as.Date(eol) -as.Date(date_tsp_switch))/30.461)
  r <- rnorm(rvs_needed, param.r, sd = .05)
  total_months_in_service <- retirement_year * 12
  retire_date <- as.Date(basd) + retirement_year*365.25 #R treats dates as days from an origin
  COLA_rate <- .015 #1.2% per year increase to pension for cost of living
  withdrawl_date <- as.Date(birth_date) + tsp_withdrawl*365.25
  
  #this is the month to start from. we will start doing the financial analysis at this month.
  #In essence we are only trying to make a decision about what would happen from the point you
  #make a decsion to switch retirement plans.
  current_service_month <- as.integer(as.numeric(as.Date(date_tsp_switch) - as.Date(basd))/30.461)
  
  #We are going to create lists that contain the data we need. Then we will put those lists into a df.
  
  #initialize the values for the lists.
  month_idx <- c(current_service_month) #this is an index month.
  rank <- c(get_rank(current_service_month,c)) #the current rank
  base_pay <- c(get_base_pay(pay_chart, rank, month_to_year(current_service_month)))
  
  i<-1 #this is the index of each list item
  #in order to get the pension amount, we need to pass a df that represents the service member's career
  #so we will need to create a seperate loop that ends when the service member retires.

  #Calculate the tsp value for the first month
  member_tsp <- c(base_pay * tsp_cont) #the amount the service member contributes
  tsp_match <- c(base_pay * get_tsp_match(tsp_cont, tsp_type)) #the amount the government matches
  market_returns <- c((member_tsp+tsp_match) * r[i]) #the market returns for the month
  total_month_tsp <- c(member_tsp + tsp_match + market_returns) #the total tsp amount for the month
  cum_tsp <- c(total_month_tsp) #this is the cumulative tsp value
  pension_amount <- c(0) #initialize a list for the pension amount
  
  #create vectors for the time on active duty
  if (continuation_pay == "N"){
  while (current_service_month < total_months_in_service ){
    i=i+1 #this is a direct index for the length of the vectors above
    current_service_month <- current_service_month +1

    month_idx <- append(month_idx,
                        current_service_month) #add one month to this index
    rank <- append(rank,
                   get_rank(current_service_month,c)) #get the rank for the current month
    base_pay <- append(base_pay, 
                       get_base_pay(pay_chart, #get the base pay for the current month
                                              get_rank(current_service_month,c),
                                              month_to_year(current_service_month)))
    member_tsp <- append(member_tsp,
                         base_pay[i]* tsp_cont) #calculate the tsp contribution for the month
    tsp_match <- append(tsp_match,
                        base_pay[i] * get_tsp_match(tsp_cont, tsp_type))
    market_returns <- append(market_returns,
                             (member_tsp[i]+tsp_match[i]) * r[i]) #the market retuns for the current month
    
    account_balance_returns <- cum_tsp[i-1] * r[i] #gets the market returns for the account balance
    
    #adds the member's contribution, government match, market returns for the current month, and the
    #market returns for the cumulative account balance
    total_month_tsp <- append(total_month_tsp,
                              member_tsp[i] + tsp_match[i] + market_returns[i] +account_balance_returns)
    
    cum_tsp <- append(cum_tsp, 
                      cum_tsp[i-1] + total_month_tsp[i])
    
    pension_amount <- append(pension_amount, 0) #this is 0 by defenition - SM is on active duty
    } #ends while statement
    
  } else { #the service member will take continuation pay
    #we assume they take continuation pay at 12 years - or when they switch if after 12 years
    if (current_service_month <= 144){
      print(current_service_month)
      continuation_entitlement_month = 145 #the service member gets continuation pay at 12 years
    } else {

      continuation_entitlement_month = current_service_month+1 #the service member takes continuation pay immeadeately
    }
    while (current_service_month < total_months_in_service ){
    
    i=i+1 #this is a direct index for the length of the vectors above
    current_service_month <- current_service_month +1
    month_idx <- append(month_idx,
                        current_service_month) #add one month to this index
    rank <- append(rank,
                   get_rank(current_service_month,c)) #get the rank for the current month
    base_pay <- append(base_pay, 
                       get_base_pay(pay_chart, #get the base pay for the current month
                                    get_rank(current_service_month,c),
                                    month_to_year(current_service_month)))
    #cont_pay is the amount of continuation pay contributed to the tsp account

    cont_pay <- get_continuation_pay(base_pay[i],month_idx[i],continuation_entitlement_month) * pay_to_tsp_cont
    member_tsp <- append(member_tsp,
                         (base_pay[i]* tsp_cont + cont_pay)) #calculate the tsp contribution for the month
    tsp_match <- append(tsp_match,
                        base_pay[i] * get_tsp_match(tsp_cont, tsp_type))
    market_returns <- append(market_returns,
                             (member_tsp[i]+tsp_match[i]) * r[i]) #the market retuns for the current month
    
    account_balance_returns <- cum_tsp[i-1] * r[i] #gets the market returns for the account balance
    
    #adds the member's contribution, government match, market returns for the current month, and the
    #market returns for the cumulative account balance
    total_month_tsp <- append(total_month_tsp,
                              member_tsp[i] + tsp_match[i] + market_returns[i] +account_balance_returns)
    
    cum_tsp <- append(cum_tsp, 
                      cum_tsp[i-1] + total_month_tsp[i])
    
    pension_amount <- append(pension_amount, 0) #this is 0 by defenition - SM is on active duty
    } #ends while loop
  } #ends else
  
  career_df <- data.frame(month_idx, rank) #this is the data frame of a career, needed to get the pension
  initial_pension <- get_pension_amount(pay_chart, total_months_in_service, career_df, tsp_type)
  
  #now we create a loop to add data for the tsp performance before the withdrawl date
  #we also need to account for the increase in the pension - COLA adjustments
  
  #this is the number of months that the tsp account will grow
  num_months_tsp_grows <- as.integer((withdrawl_date - retire_date)/30.461)
  num_pension_months <- 0

  #this is a loop to create the financial data from retirement until the person withdraws tsp
  while (num_pension_months < num_months_tsp_grows){
    num_pension_months <- num_pension_months+1
    i<-i+1
    current_service_month <- current_service_month +1
    month_idx <- append(month_idx, current_service_month)
    rank <- append(rank, "None")
    base_pay <- append(base_pay, 0)
    member_tsp <- append(member_tsp, 0) #the member no longer contributes to tsp
    tsp_match <- append(tsp_match, 0) #the member cannot get matching after retiring
    market_returns <- append(market_returns, 0) #there are no market returns for current month
    
    account_balance_returns <- cum_tsp[i-1] * r[i] #gets the market returns for the account balance
    total_month_tsp <- append(total_month_tsp, member_tsp[i-1] + 
                                tsp_match[i-1] + 
                                market_returns[i-1] +
                                account_balance_returns)
    
    cum_tsp <- append(cum_tsp, cum_tsp[i-1] + total_month_tsp[(i)])
    
    pension_and_COLA <- initial_pension + (initial_pension * 
                                             (month_to_year(num_pension_months) - 1) *COLA_rate)
    pension_amount <- append(pension_amount, pension_and_COLA) 
    
  } #ends while loop
  
  num_months_to_eol <- as.integer((eol - withdrawl_date)/30.461) #this is the number of months from tsp withdrawl to end of life
  num_months_on_pension <- 0

  #this is a loop to create the financial data from tsp withdrawl until death
  while(num_months_on_pension < num_months_to_eol){
    num_months_on_pension <- num_months_on_pension+1
    i<-i+1
    current_service_month <- current_service_month +1
    month_idx <- append(month_idx, current_service_month)
    rank <- append(rank, "None")
    base_pay <- append(base_pay, 0)
    member_tsp <- append(member_tsp, 0) #the member no longer contributes to tsp
    tsp_match <- append(tsp_match, 0) #the member cannot get matching after retiring
    market_returns <- append(market_returns, 0) #there are no market returns for current month
    
    account_balance_returns <- 0 #the member has withdrawn money from tsp
    total_month_tsp <- append(total_month_tsp, member_tsp[i-1] + 
                                tsp_match[i-1] + 
                                market_returns[i-1] +
                                account_balance_returns)
    
    cum_tsp <- append(cum_tsp, cum_tsp[i-1] + total_month_tsp[(i)])
    
    pension_and_COLA <- initial_pension + (initial_pension * 
                                             (month_to_year(num_pension_months + num_months_on_pension) - 1) *COLA_rate)
    
    pension_amount <- append(pension_amount, pension_and_COLA)
  }#ends while loop
  
  cum_pension <- cumsum(pension_amount)
  
  #create a dataframe of the cumulative vectors
  # full.df <- data.frame( month_idx,
  #                        cum_tsp,
  #                        pension_amount)
  # full.df$cum_pension <- cumsum(pension_amount)
  
  return(list(cum_tsp, cum_pension))
  } #closes create finances function

sim_finances <- function(num_sims, birth_date, life_expectancy, basd, 
                         career_path, date_tsp_switch, tsp_type,
                         retirement_year, tsp_withdrawl,
                         market_return, tsp_cont, continuation_pay,
                         pay_to_tsp_cont){
  
  #initialize a list of results
  finances <- create_finances(birth_date, life_expectancy, basd, 
                              career_path, date_tsp_switch, tsp_type,
                              retirement_year, tsp_withdrawl,
                              market_return, tsp_cont, continuation_pay,
                              pay_to_tsp_cont) #call the create finances function and store results
  tsp_data <- c(finances[1])
  pension_data <- c(finances[2])
  
  for (i in 1:(num_sims-1)){
    finances <- create_finances(birth_date, life_expectancy, basd, 
                    career_path, date_tsp_switch, tsp_type,
                    retirement_year, tsp_withdrawl,
                    market_return, tsp_cont, continuation_pay,
                    pay_to_tsp_cont) #call the create finances function and store results
    
    tsp_data <- append(tsp_data, finances[1]) #parse the results into two separate lists
    pension_data <- append(pension_data, finances[2]) #this creates two lists of length i
    
  }#ends for loop
  
  return(list(tsp_data, pension_data))
}#closes sim function

make_pension_df <- function(result_list, num_sims){
  pension_data <- result_list[2]
  pension.df <- data.frame(matrix(unlist(pension_data), nrow = num_sims, byrow=T))
return(pension.df)
  }

make_tsp_df <- function(result_list, num_sims){
  tsp_data <- result_list[1]
  tsp.df <- data.frame(matrix(unlist(tsp_data), nrow = num_sims, byrow=T))
  return(tsp.df)
}