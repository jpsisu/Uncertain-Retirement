library(ggplot2)
library(reshape2)
library(scales)

plot_results <- function (a, birth_date, life_expectancy, date_tsp_switch){
  
  eol <- as.Date(birth_date) + life_expectancy*365.25 #this is the date of end of life
  m<-seq(as.Date(date_tsp_switch), as.Date(eol), by = "months") #this is a sequence of months to plot the x-axis
  months <- length(a) #this is going to be the length x-axis
  m<- m[1:months]
  num_rows <- length(row.names(a)) #each row will become one plotted simulation
  myColor = "#ccffdd"
  average <- apply(a,2,median) #get the average values for all simulations for each month
  
  p <- plot(m,a[1,], type = "l", col = myColor, xlab="Date", ylab="$", yaxt="n")
  for (i in 2:num_rows){
    lines(m,a[i,], type = "l", col = myColor)
  }
  
  lines(m, average, type = "l")
#   
#   myTicks = axTicks(2) #get the location of the Ticks
#   
#   newTicks = c(0,as.integer(average[months]),as.integer(max(myTicks)))
# #   axis(2, at = newTicks, labels=FALSE)
# 
#   axis(2, at = newTicks,labels=format(newTicks, scientific=FALSE), las=2) #never return scientific notation
#   
#   par(ps=4) #set the font size
  return(p) 
}

plot_trad_hist <- function(df){
  last_month <- paste("X",length(df), sep="" )
  num_bins <- length(row.names(df)) / 4
  myColor = "#47d147"
  
  p <- ggplot(df, aes_string(last_month, fill = last_month)) + geom_histogram(bins = num_bins, fill = myColor)
  p <- p + labs(x = "Total TSP Value at Date of Withdrawl", y="")
  p <- p + theme(axis.text.y = element_blank())
  p <- p + scale_x_continuous(labels = comma)
  
  return(p)
}

plot_blended_hist <- function(df){
  last_month <- paste("X",length(df), sep="" )
  num_bins <- length(row.names(df)) / 4
  myColor = "#3333ff"
  
  p <- ggplot(df, aes_string(last_month, fill = last_month)) + geom_histogram(bins = num_bins, fill = myColor)
  p <- p + labs(x = "Total TSP Value at Date of Withdrawl", y="")
  p <- p + theme(axis.text.y = element_blank())
  p <- p + scale_x_continuous(labels = comma)
  
  return(p)
}


plot_difference <- function(full_tsp_df, full_pension_df, partial_tsp_df, partial_pension_df){
  
  pension.full <- apply(full_pension_df,2,median)
  tsp.full <- apply(full_tsp_df,2,median)
  full.total <- pension.full + tsp.full
  
  pension.partial <- apply(partial_pension_df,2,median)
  tsp.partial <- apply(partial_tsp_df,2,median)
  partial.total <- pension.partial + tsp.partial
  months <- c(1:length(pension.full))
  
  full.total.df <- as.data.frame(cbind(months, partial.total, full.total))
  
  partial_total_amt <- max(full.total.df$partial.total)
  full_total_amt <- max(full.total.df$full.total)
  
  if(partial_total_amt > full_total_amt){
    full.total.df$diff <- partial.total - full.total
    
    p <- ggplot(full.total.df, aes(x = months, y = diff)) +
      geom_bar(stat = "identity", position = position_dodge(), fill = "#47d147")
    p <- p + xlab("Time")
    p <- p + ylab("Cumulative $")
    p <- p + scale_y_continuous(labels = comma)
    p <- p + theme(legend.position = "bottom",
                   axis.text.x=element_blank())  
  } else{
    full.total.df$diff <- full.total - partial.total
    
    p <- ggplot(full.total.df, aes(x = months, y = diff)) +
      geom_bar(stat = "identity", position = position_dodge(), fill = "#3333ff")
    p <- p + xlab("Time")
    p <- p + ylab("Cumulative $ Difference")
    p <- p + scale_y_continuous(labels = comma)
    p <- p + theme(legend.position = "bottom",
                   axis.text.x=element_blank()) 
  }

  return(p)
  
}