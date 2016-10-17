#This file contains the helper functions that process pay amounts for a service member.
#These functions are used to make the code for creating a the financial outputs more
#readable and manageable.

#process the 2016 military pay chart by rank
pay_chart <- read.csv("pay_chart.csv")
row.names(pay_chart) <- pay_chart$y.Gr

#set up some variables - these variables will become reactive later
birth_date = "1982-08-18"
life_expectancy = 75
basd = "2004-05-24" #basic active service date
date_tsp_switch = "2016-07-30" #the date you would switch tsp types
career_path = "O" #Choices are O,E,W
#year_of_service_tsp_switch = 12 #the year you can switch from traditional to full tsp
retirement_year = 20 #the year you plan to retire
tsp_withdrawl = 65 #the age tsp contributions are withdrawn
tsp_cont = .05 # the amount of your base pay you plan to donate to tsp
market_return = .06 #annual expected market return
pay_to_tsp_cont = .25 #the percentage of continuation pay to contribute to tsp
continuation_pay = "Y" #this variable represents if the SM will take continuation pay
tsp_type <- "full"

#set up some variables that are not reactive
tsp_types = c("full","partial")

#start helper functions. These functions do specific things to help create the career finances.

#We are going to model each month as a step in a loop. So most of our inputs for the
#functions will take a month as an input.

#this function takes a month and converts to a year. Some functions will need a year.
#We need a way to convert an integer to a year, where 1 - 12 corresponds to year 1;
#13 - 24 corresponds to year 2, and so on...
month_to_year <- function(month){
  #retuns the integer year for a given month. Year counting starts at 1.
  if (month%%12 == 0){
    year <- as.integer(month/12)
  } else {
    year <- as.integer(month/12) +1
  }
  return(year)
}

#this is a function to get the rank of someone in service along a normal career path
get_rank <- function(months_in_service, career_path){
  c <- career_path
  m <- months_in_service
  if (c=="O") {
    #officer promotion timeline
    if (m<18){
      rank <- "O-1"
    } else if (m<49){
      rank <- "O-2"
    } else if (m<121){
      rank <- "O-3"
    } else if (m<193){
      rank <- "O-4"
    } else if (m<264){
      rank <- "O-5"
    } else {
      rank <- "O-6"
    }
  } else if (c == "E"){
    #enlisted promotion timeline
    if (m<12){
      rank <- "E-1"
    } else if (m<24){
      rank <- "E-2"
    } else if (m<36){
      rank <- "E-3"
    } else if (m<40){
      rank <- "E-4"
    } else if (m<50){
      rank <- "E-5"
    } else if (m<96) {
      rank <- "E-6"
    } else if (m<163) {
      rank <- "E-7"
    } else if (m<204) {
      rank <- "E-8"
    } else {
      rank <- "E-9"
    }
  } else if (c == "W"){
    #Warrant Officer timeline
    if (m<18){
      rank <- "W-1"
    } else if (m<72){
      rank <- "W-2"
    } else if (m<144) {
      rank <- "W-3"
    } else if (m<240) {
      rank <- "W-4"
    } else {
      rank <- "W-5"
    }
  }
  
  return(rank)
}

#this function returns the monthly base pay given a rank and the number of years in service
get_base_pay <- function(pay_chart, rank, years){
  #returns the base pay for a rank and the number of years of service
  #there are only 40 years in the pay chart, so we need to accoutn for people in over 40
  if (years <= 2){
    col_name = "X2_or_less"
  } else if (years > 40) {
    years <- 40
    col_name <- paste("Over_",years, sep="")
  } else {
    col_name <- paste("Over_",years, sep="")
  }
  result <- pay_chart[rownames(pay_chart) == rank, col_name]
  return(result)
}

#a function to return a 1 if on active duty, 0 if not on active duty
is_active_duty <- function(current_month_in_service, retirement_year) {
  retirement_month <- 12*retirement_year
  if (current_month_in_service <= retirement_month){
    return(1)
  } else{
    return(0)
  }
}

#this function gets the retirement multiplier for each year of service
retirement_multiplier <- function (tsp_type){
  #if a memeber is a full tsp memeber - new retirement system they only recieve 2% per year
  if (tsp_type == "full"){
    return(2.0) 
  } else {
    return(2.5)
  }
}

#this function gets the base pay amount used to calculate pension values
get_high_3 <- function (pay_chart, rank1, rank2, rank3, year1, year2, year3){
  base_pay1 <- get_base_pay(pay_chart, rank1, year1)
  base_pay2 <- get_base_pay(pay_chart, rank2, year2)
  base_pay3 <- get_base_pay(pay_chart, rank3, year3)
  sum <- base_pay1 + base_pay2 + base_pay3
  avg <- sum /3
  return (avg)
}

#this function gets the months a person expects to stay on a pension
get_months_on_pension <- function(birth_date, life_expectancy, basd, retirement_year){
  death <- as.Date(birth_date) + life_expectancy*365.25
  retirement <- as.Date(basd) + retirement_year*365.25
  num<- as.numeric(death-retirement)/30.4167
  months <- as.integer(num)
  return(months)
}

#this function returns the amount of money that the government matches
get_tsp_match <- function(tsp_cont, tsp_type){
  if (tsp_type == "partial"){
    return(0)
  } else if (tsp_type == "full"){
    automatic_match <- .01 #the government will always contribute 1% to the account
    if (tsp_cont >= .05){
      match <- .04
    } else if (tsp_cont <= .03){
      match <- tsp_cont #the government matches 1-1 for the first 3%
    } else {
      exact_match <- .03
      remaining <- tsp_cont - exact_match
      add_match <- remaining*.5 #the government matches 1/2 of the 3-5 percent contributions
      match <- exact_match + add_match
    }
    total_match <- automatic_match + match
    return(total_match)
  }
}

#this function returns the amount of continuation pay a Service member recieves
get_continuation_pay_amount <- function(base_pay){
  continuation_multiplier <- 2.5
  continuation_pay <- base_pay * continuation_multiplier
  return(continuation_pay)
}

#this function returns continuation pay for each month 0, or an amount
get_continuation_pay <- function(base_pay, current_month, entitlement_month){
  if(current_month == entitlement_month){
    return(get_continuation_pay_amount(base_pay))
  } else {return(0)}
}

#required for get_pension_amount.  This function encapsualtes the retirement multipliers
#that are defined in the 2016 NDAA.
retirement_multiplier <- function (tsp_type){
  #if a memeber is a full tsp memeber - new retirement system they only recieve 2% per year
  if (tsp_type == "full"){
    return(2.0) 
  } else {
    return(2.5)
  }
}

#required for get_pension_amount.  This function returns the average of the 'high 3'
#base pay years for a service member.
get_high_3 <- function (pay_chart, rank1, rank2, rank3, year1, year2, year3){
  base_pay1 <- get_base_pay(pay_chart, rank1, year1)
  base_pay2 <- get_base_pay(pay_chart, rank2, year2)
  base_pay3 <- get_base_pay(pay_chart, rank3, year3)
  sum <- base_pay1 + base_pay2 + base_pay3
  avg <- sum /3
  return (avg)
}

#a function to get the monthly pension amount.  This is the inital pension amount
#for a person at retirement under the 'high3' retirement system
get_pension_amount <- function(pay_chart, months_in_service, career_df, tsp_type) {
  career <- career_df # this a career data frame inside the create_finances function
  last_month <- max(career_df$month_idx) #the month of retirement
  last_id <- length(row.names(career_df))
  month1 <- last_month - 12
  month1_id <- last_id- 12
  month2 <- last_month - 24
  month2_id <- last_id- 24
  month3 <- last_month - 36
  month3_id <- last_id- 36
  year1 <- as.integer(month_to_year(month1))
  year2 <- as.integer(month_to_year(month2))
  year3 <- as.integer(month_to_year(month3))
  rank1 <- as.character(career$rank[month1_id])
  rank2 <- as.character(career$rank[month2_id])
  rank3 <- as.character(career$rank[month3_id])
  years_in_service <- as.integer(month_to_year(last_month))
  
  high3 <- get_high_3(pay_chart, rank1, rank2 , rank3 , year1 , year2, year3)
  multiplier <- retirement_multiplier(tsp_type)
  retirement_percentage <- years_in_service * multiplier /100
  
  if (months_in_service < 240){
    pension_amt <- 0 } else{
      pension_amt <- high3 * retirement_percentage
    }
  return(pension_amt)
  
}

#This is a function to change integers into $1,000,000
num_to_dollar <- function(x){
  paste("$",format(x, big.mark=","),sep="")
}