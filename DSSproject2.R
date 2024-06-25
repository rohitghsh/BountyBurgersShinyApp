#Problem 1

# Install libraries
install.packages("readxl")
install.packages("shiny")
install.packages("shinydashboard")

# Load libraries
library(readxl)
library(shiny)
library(shinydashboard)

# Read the data
visits <- read_excel("/Users/ghoshdarnit/Desktop/Semester 4/Adaptive Business Intelligence/BurgerBounty.xlsx", sheet = "Visits")
prices <- read_excel("/Users/ghoshdarnit/Desktop/Semester 4/Adaptive Business Intelligence/BurgerBounty.xlsx", sheet = "Prices")
sales <- read_excel("/Users/ghoshdarnit/Desktop/Semester 4/Adaptive Business Intelligence/BurgerBounty.xlsx", sheet = "Sales")

# Merged dataFrame
BBDF <- merge(visits, prices, by = "Date", all.x = TRUE)
BBDF <- merge(BBDF, sales, by = "Date", all.x = TRUE)

attach(BBDF)

# linear regression model
model1 <- lm(`Bounty Hunter.y`~`Bounty Hunter.x`+Town+Time+Precipitation+Temperature+Event+Weekend)
model2 <- lm(`Classic Cheeseburger.y`~`Classic Cheeseburger.x`+Town+Time+Precipitation+Temperature+Event+Weekend)
model3 <- lm(`Spicy Mutiny.y`~`Spicy Mutiny.x`+Town+Time+Precipitation+Temperature+Event+Weekend)
model4 <- lm(`Nature Bounty.y`~`Nature Bounty.x`+Town+Time+Precipitation+Temperature+Event+Weekend)
model5 <- lm(BEC.y~BEC.x+Town+Time+Precipitation+Temperature+Event+Weekend)
model6 <- lm(`Double Veggie.y`~`Double Veggie.x`+Town+Time+Precipitation+Temperature+Event+Weekend)

# Display regression summary
print(summary(model1))
print(summary(model2))
print(summary(model3))
print(summary(model4))
print(summary(model5))
print(summary(model6))

# Predict sales for the example scenerio
scenerio <- data.frame(
  `Bounty Hunter.x` = 9,
  Town = "Downtown Hartford",
  Time = 2,
  Precipitation = 0.2,
  Temperature = 70,
  Event = "Yes",
  Weekend = "No"
)
colnames(scenerio) = c(colnames(BBDF)[9], "Town", "Time", "Precipitation", "Temperature", "Event", "Weekend")

predicted_sales <- predict(model1,scenerio)

# Predicted Revenue
price <- scenerio$`Bounty Hunter.x`
predicted_revenue <- predicted_sales * price

# Results
cat("Predicted Sales (Bounty Hunter):", predicted_sales, "\n")
cat("Predicted Revenue (Bounty Hunter):", predicted_revenue , "\n")

#problem 2
library(shiny)
library(shiny)
ui = fluidPage(
  titlePanel("Burger Bounty Recommendations"),
  sidebarLayout(
    sidebarPanel(
      numericInput("hrs", label = "Hours to be spent at the location", value = 0),
      sliderInput("avgprec", label = "Average Precipitation", min= 0, max = 1, step = 0.1, value = 0.5),
      sliderInput("avgtemp", label = "Average Temperature", min= 0, max = 100, step = 1, value = 50),
      selectInput("wknd", label = "Weekend?", choices = c("Yes", "No"), selected = "No"),
      checkboxInput("dt", "Event in Downtown Hartford?"),
      checkboxInput("eh", "Event in East Hartford?"),
      checkboxInput("gb", "Event in Glastonbury?"),
      checkboxInput("mc", "Event in Manchester?"),
      checkboxInput("nb", "Event in New Britain?"),
      checkboxInput("wh", "Event in West Hartford?"),
      checkboxInput("wf", "Event in Wethersfield?"),
      sliderInput("priceBH", "Selling Price for Bounty Hunter:", value = 9, min = 1, max = 30),
      sliderInput("priceCC", "Selling Price for Classic Cheeseburger:", value = 9, min = 1, max = 30),
      sliderInput("priceSM", "Selling Price for Spicy Mutiny:", value = 9, min = 1, max = 30),
      sliderInput("priceNB", "Selling Price for Nature Bounty:", value = 9, min = 1, max = 30),
      sliderInput("priceBEC", "Selling Price for BEC:", value = 9, min = 1, max = 30),
      sliderInput("priceDV", "Selling Price for Double Veggie:", value = 9, min = 1, max = 25),
      actionButton("button", label = "Recommendations")
    ),
    mainPanel(
      # Output table
      tableOutput("result")
    )
  )
  
)

server = function(input, output){
  observeEvent(input$button, 
               {
                 #Downtown
                 downtown <- data.frame(
                   Bounty.Hunter.x = input$priceBH,
                   Classic.Cheeseburger.x = input$priceCC,
                   Spicy.Mutiny.x = input$priceSM,
                   Nature.Bounty.x = input$priceNB,
                   BEC.x = input$priceBEC,
                   Double.Veggie.x = input$priceDV,
                   Town = "Downtown Hartford",
                   Time = input$hrs,
                   Precipitation = input$avgprec,
                   Temperature = input$avgtemp,
                   Event = ifelse(input$dt==TRUE, "Yes","No"),
                   Weekend = input$wknd
                 ) 
                 colnames(downtown) <- colnames(BBDF)[c(9:14, 3:8)]
                 #predictions                 
                 BB_downtown <- round(predict(model1, downtown),2)
                 CC_downtown <- round(predict(model2, downtown),2)
                 SM_downtown <- round(predict(model3, downtown), 2)
                 NB_downtown <- round(predict(model4, downtown), 2)
                 BEC_downtown <- round(predict(model5, downtown), 2)
                 DV_downtown <- round(predict(model6, downtown), 2)
                 
                 revenue.downtown <- round(BB_downtown*input$priceBH 
                                           + CC_downtown*input$priceCC 
                                           + SM_downtown*input$priceSM 
                                           + NB_downtown*input$priceNB 
                                           + BEC_downtown*input$priceBEC 
                                           + DV_downtown*input$priceDV, 2)
                 
                 downtown.predictions <- c("Downtown", BB_downtown, CC_downtown, 
                                           SM_downtown, NB_downtown, BEC_downtown, 
                                           DV_downtown, revenue.downtown)
                 
                 
                 #West Hartford
                 westhartford <- data.frame(
                   Bounty.Hunter.x = input$priceBH,
                   Classic.Cheeseburger.x = input$priceCC,
                   Spicy.Mutiny.x = input$priceSM,
                   Nature.Bounty.x = input$priceNB,
                   BEC.x = input$priceBEC,
                   Double.Veggie.x = input$priceDV,
                   Town = "West Hartford",
                   Time = input$hrs,
                   Precipitation = input$avgprec,
                   Temperature = input$avgtemp,
                   Event = ifelse(input$wh==TRUE, "Yes","No"),
                   Weekend = input$wknd
                 )
                 colnames(westhartford) <-colnames(BBDF)[c(9:14, 3:8)]
                 #predictions
                 BB_westhartford <- round(predict(model1, westhartford), 2)
                 CC_westhartford <- round(predict(model2, westhartford), 2)
                 SM_westhartford <- round(predict(model3, westhartford), 2)
                 NB_westhartford <- round(predict(model4, westhartford), 2)
                 BEC_westhartford <- round(predict(model5, westhartford), 2)
                 DV_westhartford <- round(predict(model6, westhartford), 2)
                 
                 revenue.westhartford <- round(BB_westhartford*input$priceBH 
                                               + CC_westhartford*input$priceCC 
                                               + SM_westhartford*input$priceSM 
                                               + NB_westhartford*input$priceNB 
                                               + BEC_westhartford*input$priceBEC 
                                               + DV_westhartford*input$priceDV, 2)
                 
                 westhartford.predictions <- c("West Hartford", BB_westhartford, CC_westhartford,
                                               SM_westhartford, NB_westhartford, BEC_westhartford, 
                                               DV_westhartford, revenue.westhartford)
                 
                 #East Hartford
                 easthartford <- data.frame(
                   Bounty.Hunter.x = input$priceBH,
                   Classic.Cheeseburger.x = input$priceCC,
                   Spicy.Mutiny.x = input$priceSM,
                   Nature.Bounty.x = input$priceNB,
                   BEC.x = input$priceBEC,
                   Double.Veggie.x = input$priceDV,
                   Town = "East Hartford",
                   Time = input$hrs,
                   Precipitation = input$avgprec,
                   Temperature = input$avgtemp,
                   Event = ifelse(input$eh==TRUE, "Yes","No"),
                   Weekend = input$wknd
                 ) 
                 colnames(easthartford) <- colnames(BBDF)[c(9:14, 3:8)]
                 #predictioms
                 BB_easthartford <- round(predict(model1, easthartford), 2)
                 CC_easthartford <- round(predict(model2, easthartford), 2)
                 SM_easthartford <- round(predict(model3, easthartford), 2)
                 NB_easthartford <- round(predict(model4, easthartford), 2)
                 BEC_easthartford <- round(predict(model5, easthartford), 2)
                 DV_easthartford <- round(predict(model6, easthartford), 2)
                 
                 revenue.easthartford <- round(BB_easthartford*input$priceBH 
                                               + CC_easthartford*input$priceCC 
                                               + SM_easthartford*input$priceSM 
                                               + NB_easthartford*input$priceNB 
                                               + BEC_easthartford*input$priceBEC 
                                               + DV_easthartford*input$priceDV, 2)
                 
                 easthartford.predictions <- c("East Hartford", BB_easthartford, CC_easthartford, 
                                               SM_easthartford, NB_easthartford, BEC_easthartford, 
                                               DV_easthartford, revenue.easthartford)
                 
                 #Glastonbury
                 glastonbury <- data.frame(
                   Bounty.Hunter.x = input$priceBH,
                   Classic.Cheeseburger.x = input$priceCC,
                   Spicy.Mutiny.x = input$priceSM,
                   Nature.Bounty.x = input$priceNB,
                   BEC.x = input$priceBEC,
                   Double.Veggie.x = input$priceDV,
                   Town = "Glastonbury",
                   Time = input$hrs,
                   Precipitation = input$avgprec,
                   Temperature = input$avgtemp,
                   Event = ifelse(input$gb==TRUE, "Yes","No"),
                   Weekend = input$wknd
                 )
                 colnames(glastonbury) <- colnames(BBDF)[c(9:14, 3:8)]
                 #predictions
                 BB_glastonbury <- round(predict(model1, glastonbury), 2)
                 CC_glastonbury <- round(predict(model2, glastonbury), 2)
                 SM_glastonbury <- round(predict(model3, glastonbury), 2)
                 NB_glastonbury <- round(predict(model4, glastonbury), 2)
                 BEC_glastonbury <- round(predict(model5, glastonbury), 2)
                 DV_glastonbury <- round(predict(model6, glastonbury), 2)
                 
                 revenue.glastonbury <- round(BB_glastonbury*input$priceBH 
                                              + CC_glastonbury*input$priceCC 
                                              + SM_glastonbury*input$priceSM 
                                              + NB_glastonbury*input$priceNB 
                                              + BEC_glastonbury*input$priceNB 
                                              + DV_glastonbury*input$priceDV, 2)
                 glastonbury.predictions <- c("Glastonbury", BB_glastonbury, CC_glastonbury, 
                                              SM_glastonbury, NB_glastonbury, BEC_glastonbury, 
                                              DV_glastonbury, revenue.glastonbury)
                 
                 #Manchester
                 manchester <- data.frame(
                   Bounty.Hunter.x = input$priceBH,
                   Classic.Cheeseburger.x = input$priceCC,
                   Spicy.Mutiny.x = input$priceSM,
                   Nature.Bounty.x = input$priceNB,
                   BEC.x = input$priceBEC,
                   Double.Veggie.x = input$priceDV,
                   Town = "Manchester",
                   Time = input$hrs,
                   Precipitation = input$avgprec,
                   Temperature = input$avgtemp,
                   Event = ifelse(input$mc==TRUE, "Yes","No"),
                   Weekend = input$wknd
                 )
                 colnames(manchester) <- colnames(BBDF)[c(9:14, 3:8)]
                 #predicions
                 BB_manchester <- round(predict(model1, manchester), 2)
                 CC_manchester <- round(predict(model2, manchester), 2)
                 SM_manchester <- round(predict(model3, manchester), 2)
                 NB_manchester <- round(predict(model4, manchester), 2)
                 BEC_manchester <- round(predict(model5, manchester), 2)
                 DV_manchester <- round(predict(model6, manchester), 2)
                 
                 
                 
                 revenue.manchester <- round(BB_manchester*input$priceBH 
                                             +CC_manchester*input$priceCC 
                                             + SM_manchester*input$priceSM 
                                             + NB_manchester*input$priceNB 
                                             + BEC_manchester*input$priceBEC 
                                             + DV_manchester*input$priceDV, 2)
                 
                 manchester.predictions <- c("Manchester", BB_manchester, CC_manchester, 
                                             SM_manchester, NB_manchester, BEC_manchester, 
                                             DV_manchester, revenue.manchester)
                 
                 #New Britain
                 newbritain <- data.frame(
                   Bounty.Hunter.x = input$priceBH,
                   Classic.Cheeseburger.x = input$priceCC,
                   Spicy.Mutiny.x = input$priceSM,
                   Nature.Bounty.x = input$priceNB,
                   BEC.x = input$priceBEC,
                   Double.Veggie.x = input$priceDV,
                   Town = "New Britain",
                   Time = input$hrs,
                   Precipitation = input$avgprec,
                   Temperature = input$avgtemp,
                   Event = ifelse(input$nb==TRUE, "Yes","No"),
                   Weekend = input$wknd
                 )
                 colnames(newbritain) <- colnames(BBDF)[c(9:14, 3:8)]
                 #predictions
                 BB_newbritain <- round(predict(model1, newbritain), 2)
                 CC_newbritain <- round(predict(model2, newbritain), 2)
                 SM_newbritain <- round(predict(model3, newbritain), 2)
                 NB_newbritain <- round(predict(model4, newbritain), 2)
                 BEC_newbritain <- round(predict(model5, newbritain), 2)
                 DV_newbritain <- round(predict(model6, newbritain), 2)
                 
                 revenue.newbritain <- round(BB_newbritain*input$priceBH 
                                             +CC_newbritain*input$priceCC 
                                             + SM_newbritain*input$priceSM 
                                             + NB_newbritain*input$priceNB 
                                             + BEC_newbritain*input$priceBEC 
                                             + DV_newbritain*input$priceDV, 2)
                 
                 newbritain.predictions <- c("New Britain", BB_newbritain, CC_newbritain, 
                                             SM_newbritain, NB_newbritain, BEC_newbritain, 
                                             DV_newbritain, revenue.newbritain)
                 
                 
                 #Wethersfield
                 wethersfield <- data.frame(
                   Bounty.Hunter.x = input$priceBH,
                   Classic.Cheeseburger.x = input$priceCC,
                   Spicy.Mutiny.x = input$priceSM,
                   Nature.Bounty.x = input$priceNB,
                   BEC.x = input$priceBEC,
                   Double.Veggie.x = input$priceDV,
                   Town = "Wethersfield",
                   Time = input$hrs,
                   Precipitation = input$avgprec,
                   Temperature = input$avgtemp,
                   Event = ifelse(input$wf==TRUE, "Yes","No"),
                   Weekend = input$wknd
                 )
                 
                 
                 colnames(wethersfield) <- colnames(BBDF)[c(9:14, 3:8)]
                 #predictions
                 BB_wethersfield <- round(predict(model1, wethersfield), 2)
                 CC_wethersfield <- round(predict(model2, wethersfield), 2)
                 SM_wethersfield <- round(predict(model3, wethersfield), 2)
                 NB_wethersfield <- round(predict(model4, wethersfield), 2)
                 BEC_wethersfield <- round(predict(model5, wethersfield), 2)
                 DV_wethersfield <- round(predict(model6, wethersfield), 2)
                 
                 revenue.wethersfield <- round(BB_wethersfield*input$priceBH 
                                               +CC_wethersfield*input$priceCC 
                                               + SM_wethersfield*input$priceSM
                                               + NB_wethersfield*input$priceNB 
                                               + BEC_wethersfield*input$priceBEC 
                                               + DV_wethersfield*input$priceDV, 2)
                 
                 wethersfield.predictions <- c("wethersfield", BB_wethersfield, CC_wethersfield, 
                                               SM_wethersfield, NB_wethersfield, BEC_wethersfield, 
                                               DV_wethersfield, revenue.wethersfield)
                 
                 
                 final_table <- data.frame(downtown.predictions, easthartford.predictions, 
                                           westhartford.predictions, manchester.predictions, 
                                           newbritain.predictions, glastonbury.predictions, 
                                           wethersfield.predictions)
                 
                 final_table <- t(final_table)
                 colnames(final_table) <- c("Town", "Bounty Hunter", "Classic Cheeseburger", 
                                            "Spicy Mutiny", "Nature Bounty", "BEC", "Double Veggie", 
                                            "Predicted Revenues")
                 
                 output$result <- renderTable({
                   final_table
                 })
                 
               })
  
  
}
shinyApp(ui, server)



















