
RegData <- read.csv("CaseStudy_data.csv", stringsAsFactors = FALSE)

library(shiny)
library(shinythemes)

ui <- fluidPage(
  
  titlePanel("Linear Regression Analysis on Energy Efficiency in Buildings"),
  
  h4("By Giovanni Mannino"),
  
  sidebarLayout(
    sidebarPanel(
      
      h3("Description of App", style = "font-family: 'times'; font-si16pt"),
      
      p("With the Outcome dropdown, you can select the dependent variable you are interested in. The Outcome variables you can choose from
        are either 'Heating Load' or 'Cooling Load'. These apply to how well the building can be heated or cooled, respectively.
        With the Explanatory Variable dropdown, you can select the independent variables.
        There are a variety of variables to choose from. We have 'Relative Compactness', which is a ratio that compares the 
        overall volume of the building compared to the surface of the building. There is the 'Surface Area', which is the 
        total surface area of the building. There is the 'Wall Area', which is the total wall area of the building. And 
        lastly, there is the 'Roof Area', which is the area of the roof of the building. This data was  collected in hopes 
        of finding a correlation between the efficiency of applying heat or cooling to buildings in relation with these different building 
        factors. After selecting the variables, select the tab where you want to see the desired type of statistical analysis and data. 
        Within these tabs, I will explain the statistical analysis occuring within the tab. In the last tab, I will discuss how this app
        can help businesses.", style = "font-family: 'times'; font-si16pt"),
      
      
      selectInput("outcome", label = h4("Outcome", style = "font-family: 'times'; font-si16pt"),
                  
                  choices = list("Heating.Load" = "Heating.Load",
                                 "Cooling.Load" = "Cooling.Load"), selected = 1),
      
      selectInput("indepvar", label = h4("Explanatory Variable", style = "font-family: 'times'; font-si16pt"),
                  choices = list("Relative.Compactness" = "Relative.Compactness",
                                 "Surface.Area" = "Surface.Area",
                                 "Wall.Area" = "Wall.Area",
                                 "Roof.Area" = "Roof.Area"), selected = 1)
      
    ),
    
    mainPanel(

      
      em("Select the Desired Tab for the Desired Statistical Data", style = "font-family: 'times'; font-si16pt"),
      
      tabsetPanel(
        
                  tabPanel("Scatterplot", 
                           plotOutput("scatterplot"), # Plot
                           h4("Description of Regression and Scatter Plot", style = "font-family: 'times'; font-si16pt"),
                           
                           p("Regression is a common way to see any trends within your data between a single dependent 
                             variable and one or more independent variables. The dependent variable,
                             as it sounds is the response variable while the independent variables are the predictor
                             variables. The type of regression we have here is linear regression. In this app, we utilize
                             linear regression to estimate future values of the response variable based on a linear function
                             produced from the regression analysis. In this case, the response variables we are interested in 
                             are heat loading and cooling loading while the independent variables are relative compactness,
                             surface area, wall area, and roof area. We are using linear regression to be able to predict 
                             the efficiency of providing heat or cooling to building based on the factor just mentioned.", style = "font-family: 'times'; font-si16pt"
                             ),
                           
                           br(),
                           
                             p("To see if a linear relationship is applicable
                             to our data set, we use a scatter plot. For some relationships in this app, this is true and for
                             others it is not. With this particular set of data, there are many identical response variable 
                            values per the selected dependent variable. This is because during the trials, the same amount of 
                               heat or cooling was applied to the building while the different independent variables were manipulated and changed.
                               In this app, the blue line is a line that follows the average response value per the designated
                               independent value. The red line is the linear line produced from the linear regression. 
                               ", style = "font-family: 'times'; font-si16pt") #Resume editing here
                           ),
                  
                  
                  tabPanel("Distribution", # Plots of distributions
                           fluidRow(
                             column(6, plotOutput("distribution1")),
                             column(6, plotOutput("distribution2"))),
                           
                           h4("Description of Histograms", style = "font-family: 'times'; font-si16pt"),
                           p("Histograms are helpful, as they allow us to see how the data for each variable is distributed within the data set. More specifically
                             we can see the frequency of each value and can see how skewed the data is. Additionally, more than anything else,
                             this is a call back to some of what we learned previously in the class. Up to this point, we have used R studio
                             to do extraordinary things with data analysis. And some of those things included plotting data and creating 
                             histograms out of data sets, so I figured these histograms would a provide a good call back to what we have done 
                             in the past. Obviously, other major topics such as cleaning and filtering data within data sets are also important.
                             Within this project, however, this wasn't as necessary since values within the data sets are all numberical and are
                             already clean.", style = "font-family: 'times'; font-si16pt")
                           
                  ),
                  tabPanel("Model Summary", verbatimTextOutput("summary"), #Regression Output 
                           h4("Description of Regression Analysis", style = "font-family: 'times'; font-si16pt"), # Description of Regression output
                           p("Besides providing a linear function, linear regression analysis also provides additional details about the data.
                             As mentioned early, regression analysis utilizes residuals. And just to reiterate, residuals are the differences 
                             between the observed values in the data and the predicted values provided by the linear function. In this tab, we see 
                             a summary of the regression analysis applied to the selected variables in the slider panel. The 'Call' is just the code 
                             used within the program to conduct the initial regression analysis. In the 'Residuals' section, we see 'Min, 1Q, Median, 3Q, 
                             and Max'. These are the minimal, first quartile average, median, third quartile average, and max values of residuals provided
                             between the selected variables. This gives us a detailed look of how well the linear function fits the data.
                             
                             
                             With 'Coefficients', under 'Estimate', and next to 'Intercept' and 'NA', these are the values used to fit the linear function. 
                             The 'Intercept', as it sounds, is the y-intercept of the linear function that is produced. The 'NA' value is the estimated effect that 
                             that independent varabile has on the dependent variable. These values are produced in hopes of creating the lowest residual values. The 
                             'Standard Error' values are the values of the standard error between the observed values and estimated values. Under 't value', these values are estimates of the difference between 
                             population mean and the hypothesized value. The greater the value, the greater the difference.
                             The 'Pr(>|t|)' is the p-value, which is the probability of finding the given t-statistic if the null hypothesis of no relationship were true.
                             The final few lines are a few more statistical details about the model. The most important statistical details are the Multiple R-squared
                             vales and Adjusted R-squared values, which describes how well the line produced is fitting the data. The closer the values are to 1, the better our
                             fit is. Another important value is the p-value, which again, tells us how the model is fitting the data.", style = "font-family: 'times'; font-si16pt"),
                           
                           br(),
                           
                           p("There are many pros when applying linear regression analysis to data. The first of which is representation of the data. The regression function within R studio
                             does a great job at explaining the statistical importance of the data and how well our model is fitting the data. In our case, we used linear modeling, but there are other 
                             types of modeling that can better explain certain trends within data. Regression analysis also allows for more than just one variable to be analyzed. We have the option 
                             of comparing multiple dependent variables with multiple independent variables. This allows use to easily see trends and relationships within our data quickly and efficiently. 
                             Additionally, regression allows us to see the impact a variable can have on another. This allows us optimize or minimize impact within the variable we are interested in.", style = "font-family: 'times'; font-si16pt"), 
                           br(),
                           
                           p("Despite these pros, there are also cons within regression anlysis. The program does not easily work if there are any missing values, or specifically, if there are any zeros within the data.
                             The program also assumes that each variable has linear relationship. In our case, this is obviously not true for some relationships between variables. For others, however, it is. 
                             Discontinutiy, or step functions, are not helpful within this analysis as they disrupt linearity. Additionally, distinct and discrete values are not helpful as well.", style = "font-family: 'times'; font-si16pt"),
                           br(),
                           
                           p("Linear regression analysis, still holds itself as a premier machine learning tool. With any data, it could give us key statistical details of the relationships within the desired data set. 
                             In this app, I use a data set that can't be changed or manipulated. Code within this app, however, can be added to allow for the user to import their own data.
                             Naturally, if this machine learning tool was given to a business, they would either import new data or add data into the initial data set and the regression analysis would be performed.", style = "font-family: 'times'; font-si16pt")), 
                 
                   tabPanel("Data Product", #Data Product Discussion
                           h4("Data Products and How they Can Impact Businesses"), style = "font-family: 'times'; font-si16pt",
                           p("Within any data product, the primary considerations and reasons for this product are the business problems and goals. We want to create a data product in order to improve our overall business.
                             To do this, we create a data product that can find trends and patterns within the data we're interested in. In our case, our business goal was to find parameters of buildings that provide the optimum 
                             levels of heating and cooling efficiency. To achieve this, our data product uses linear regression analysis to try and find trends and relationships within the data. This is just one example of many 
                             data products being fueled by their business goals or problems", style = "font-family: 'times'; font-si16pt"),
                           
                           
                           br(),
                           
                           p("With any data product, there is a life cycle that the product goes through before it is finally operationalized. The phases within the life cycles are 'Discovery, Data Prep, Model Planning, Model Building, 
                              Communicating Results, and Operationalize'. The discover phase consists of realizing our business domain and goals. So, as mentioned earlier, this is really just the business realization phase and motivation
                              for the data product. Here, we additionally understand the problem we want to address, and figure out what methods we want to use to solve the problem (within our technological capabilities of course). Within 'Data Prep', it is pretty much what it sounds like,
                              we want to prepare our data with whatever methods we're using. So for example, if we were using linear regression analysis, we would want to make sure that there were no missing values within our date set. 
                              We additionally identify specific tools that might be useful for our data, such as visualization tools. The 'Model Planning' phase is where we determine the environment and applications that can be used for the data.
                              We additionally try to form a prototype model to satisfy our initial hypotheses of the problem. In the 'Model Building' phase, we begin to build the product within the environment we determine. The 'Communicate Results' 
                              phase is exactly as it sounds, we interpret the results of the product to the stakeholders. We want to effectively communicate the business value of this product, and summarize its findings and benefits. Once the stakeholders
                              approve, we 'Operationalize' the product. This equates to running a pilot of the product and eventually delivering the final deliverables of the project. We additionally define the process of how to use the product 
                              and allow for maintenance and updating of the product. ", style = "font-family: 'times'; font-si16pt"),
                           
                           br(),
                           
                           p("Overall, this project was made within the same life cycle. I first acquired this data set, and then determined the business domain of this project. I then determined how I could use the data acquired to benefit related businesses. 
                              Next, I needed to go within the data, and identified what kind of data analysis I wanted to perform. After seeing the data, I realized that a regression analysis would be helpful. 
                              As a result, I needed to make sure to clean the data and remove any missing values. Next, I needed to determine the environment I wanted/was capable of applying the algorithm to. In this case, I knew that R studio 
                              would be able to perform the analysis, but I also knew that it would be an application, so I needed to adapt my code in order to be ran as an application. After realizing this, I then began to build the code within 
                              the application. Once that was done, I began to communicate the results, which is what I am doing now! Lastly, I needed to operationalize the product, which is what I have done
                              by using a preliminary  data set as a pilot. All in all, this was a great project to finish the course off with!", style = "font-family: 'times'; font-si16pt")),
                  
                  
                  
                  tabPanel("Data", DT::dataTableOutput('tbl'), # Data as datatable
                           h4("Description of Data Table", style = "font-family: 'times'; font-si16pt"),
                           
                           p("The primary reason why I selected this data was because my IQP was related to the heating and cooling efficiencies of buildings. I also figured that there would be some trends within the data. 
                             I found out later that although there are certainly trends within the data, there are not any linear trends except for one. Overall, the regression analysis gave some great statistical details about the relationships
                             between variables in the data. However, the trends within data are not linear. Another problem with the data was the fact that the distributions are not uniform. You can see by the histograms that there is no uniformity
                             within the data. Overall, the data was still suitable for regression analysis.", style = "font-family: 'times'; font-si16pt"))
                  
                  
                           
                  
      )
    )
  ))



# SERVER
server <- function(input, output) {
  
  # Regression output
  output$summary <- renderPrint({
    fit <- lm(RegData[,input$outcome] ~ RegData[,input$indepvar])
    names(fit$coefficients) <- c("Intercept", input$var2)
    summary(fit)
  })
  
  # Data output
  output$tbl = DT::renderDataTable({
    DT::datatable(RegData, options = list(lengthChange = FALSE))
  })
  
  
  # Scatterplot output
  output$scatterplot <- renderPlot({
    plot(RegData[,input$indepvar], RegData[,input$outcome], main="Scatterplot",
         xlab=input$indepvar, ylab=input$outcome, pch=19)
    abline(lm(RegData[,input$outcome] ~ RegData[,input$indepvar]), col="red")
    lines(lowess(RegData[,input$indepvar],RegData[,input$outcome]), col="blue")
  }, height=400)
  
  
  # Histogram output var 1
  output$distribution1 <- renderPlot({
    hist(RegData[,input$outcome], main="", xlab=input$outcome)
  }, height=300, width=300)
  
  # Histogram output var 2
  output$distribution2 <- renderPlot({
    hist(RegData[,input$indepvar], main="", xlab=input$indepvar)
  }, height=300, width=300)
}

shinyApp(ui = ui, server = server)
