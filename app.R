#    http://shiny.rstudio.com/
#    author: kristofer fosmoe

library(shiny)
library(shinythemes)
source("createFinances.R", local = TRUE)
source("plotting.R", local = TRUE)

# Define UI for application
ui <- shinyUI(fluidPage(theme = shinytheme("spacelab"), 
   
   # Application title
   titlePanel("Should I change from a traditional retirement pension to the Full TSP retirement?"),
   
   h4("The controls on the left describe the conditions of your retirement scenario in order to
      provide support for a potential decision to change retirement plans.  After you have selected 
      the options that best describe your scenario click the SIMULATE button.  For 500 simulations allow 
      up to one minute."),
   hr(),
   
   # Sidebar with a slider for input variables 
   sidebarLayout(
      sidebarPanel(  width = 3,
                     radioButtons("path", label = NULL,
                                  choices = list("Officer" = "O", "Enlisted" = "E", "Warrant Officer" = "W"), 
                                  selected = "O"),
                     dateInput("birth_date",
                               "Enter Your Birth Date:",
                               value = "1982-08-01"),
                     dateInput("basd",
                               "Basic Active Service Date:",
                               value = "2004-05-01"),
                     dateInput("date_tsp_switch",
                               "The date when you are considering switching to a full tsp member:",
                               value = "2016-08-01"),
                     sliderInput("retirement_year",
                                 "The number of years you plan to stay in service:",
                                 min = 5,
                                 max = 30,
                                 value = 20),
                     sliderInput("tsp_withdraw",
                                 "When will you begin withdrawing TSP?:",
                                 min = 59.5,
                                 max = 70,
                                 step = .5,
                                 value = 59.5),
                     sliderInput("life_exp",
                                 "What is your life expectancy?:",
                                 min = 50,
                                 max = 100,
                                 value = 72),
                     sliderInput("tsp_cont",
                                 "What percentage of your base pay will you contribute to TSP?:",
                                 min = 0,
                                 max = .15,
                                 step = .01,
                                 value = .05),
                     sliderInput("market_return",
                                 "Average annual market return:",
                                 min = .01,
                                 max = .12,
                                 step = .01,
                                 value = .06),
                     sliderInput("cont_pay_tsp",
                                 "The percentage of continuation pay invested in tsp:",
                                 min = 0,
                                 max = 1,
                                 step = .01,
                                 value = 0),
                     sliderInput("num_sims",
                                 "Number of simulations. Allow up to 1 min for 500 sims:",
                                 min = 50,
                                 max = 500,
                                 step = 50,
                                 value = 50),
                     
                     actionButton("go_sim", label = "Simulate")
      ), #closes sidebarPanel
      
      # Show a plot of the generated distribution
      mainPanel(width = 9,
                
                fluidRow(
                  h3("Total valuation comparison"),
                  textOutput("best_option"),
                                 tags$head(tags$style("#best_option{color: black;
                                 font-size: 25px;
                                 font-style: bold;
                                 }"
                                 )),
                  hr(),
                  h3("Over time the difference in the total value is plotted below"),
                  plotOutput("diff_plot")

                ),
                fluidRow(
                  column(6,                             
                         
                         h5("Simulated Cumulative Values of TSP in a BLENDED system"),
                         plotOutput("exp_full_tsp_amount")
                  ),
                  column(6,
                         
                         h5("Simulated Cumulative Values of TSP in a TRADITIONAL system"),
                         plotOutput("exp_partial_tsp_amount")
                  )
                  
                  
                  
                )
                
      ) #closes main panel
   ), #closes sidebarLayout
   
   h3("Model Assumptions", class = "panel panel-info", class = "panel-heading", class = "panel-title"),
   
   h4("There are a large number of factors to consider when deciding to switch from the traditional
      military pension plan to participation in the blended or 'full' TSP plan.  The principal monetary 
      trade-off is up to 5% matching TSP contributions.  In exchange, service members defined benefit
      pension is reduced from 2.5% per month to 2% month.  As an example at 20 years of service, a service
      memebers pension would be reduced from 50% of their base pay to 40%.", class = "panel-body")
)) #closes fluid page #closes UI

server <- shinyServer(function(input, output) {

  full_sim_results <- reactive({ #this function returns a df with the 'full' tsp results
    input$go_sim
    full_sim_results <- isolate(sim_finances(input$num_sims, input$birth_date, input$life_exp,
                                     input$basd, input$path, input$date_tsp_switch, "full",
                                     input$retirement_year, input$tsp_withdraw, input$market_return,
                                     input$tsp_cont, "Y", input$cont_pay_tsp))
    full_sim_results
  })#close full_tsp_sim
  
  full_tsp_df <- reactive ({ #returns a data frame of the full tsp results
    input$go_sim
    full.tsp.df <- isolate(make_tsp_df(full_sim_results(), input$num_sims))
    full.tsp.df
  }) #closes full_tsp_sim
  
  full_pension_df <- reactive ({ #returns a data frame of the full pension results
    input$go_sim
    full.pension.df <- isolate(make_pension_df(full_sim_results(), input$num_sims))
    full.pension.df
  }) #closes full_pension_sim
  
  partial_sim_results <- reactive({ #this function returns a df with the 'partial' tsp results
    input$go_sim
    partial_sim_results <- isolate(sim_finances(input$num_sims, input$birth_date, input$life_exp,
                                             input$basd, input$path, input$date_tsp_switch, "partial",
                                             input$retirement_year, input$tsp_withdraw, input$market_return,
                                             input$tsp_cont, "Y", input$cont_pay_tsp))
    partial_sim_results
  })#close partial_tsp_sim
  
  partial_tsp_df <- reactive ({ #returns a data frame of the full tsp results
    input$go_sim
    partial.tsp.df <- isolate(make_tsp_df(partial_sim_results(), input$num_sims))
    partial.tsp.df
  }) #closes full_tsp_sim
  
  partial_pension_df <- reactive ({ #returns a data frame of the full pension results
    input$go_sim
    partial.pension.df <- isolate(make_pension_df(partial_sim_results(), input$num_sims))
    partial.pension.df
  }) #closes full_pension_sim
  
  total_full <- reactive({
    input$go_sim
    last_month <- paste("X",length(full_tsp_df()), sep="" )
    total_full <- isolate(median(full_tsp_df()[,last_month]) + median(full_pension_df()[,last_month]))
    total_full
  })#closes best case - the total amount under the full tsp system - a single value
  
  total_partial <- reactive({
    input$go_sim
    last_month <- paste("X",length(partial_tsp_df()), sep="" )
    total_partial <- isolate(median(partial_tsp_df()[,last_month]) + median(partial_pension_df()[,last_month]))
    total_partial
  })#closes best case - the total amount under the partial tsp system - a single value
  
  output$best_option <- renderText({
    if (total_partial() > total_full()){
      amt <- total_partial() - total_full()
      print(paste("The TRADITIONAL retirement system is expected to result in more total income. 
            The total difference between the choices is:", num_to_dollar(round(amt,-3))))
#       num_to_dollar(round(amt,-3))
    } else {
      amt <- total_full() - total_partial()
      print(paste("The BLENDED retirement system is expected to result in more total income. 
            The total difference between the choices is:", num_to_dollar(round(amt,-3))))
      }#closes else statement
  })#closes out$best_option - displays text of the best option and the dollar difference

  output$exp_full_tsp_amount <- renderPlot({
    input$go_sim
    plot <- isolate(plot_blended_hist(full_tsp_df()))
    plot

  })

  output$exp_partial_tsp_amount <- renderPlot({
    input$go_sim
    plot <- isolate(plot_trad_hist(partial_tsp_df()))
    plot
    
  })

  output$diff_plot <- renderPlot({
    input$go_sim
    plot<- isolate(plot_difference(full_tsp_df(), full_pension_df(), partial_tsp_df(), partial_pension_df()))
    plot
  })#closes diff_plot



}) #closes shinyServer function

# Run the application 
shinyApp(ui = ui, server = server)

