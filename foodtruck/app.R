library(shiny)
library(lpSolveAPI)
library(lpSolveAPI)
library(markdown)
library(ggplot2)
library(gridExtra)
library(ggfortify)


ui <- navbarPage("FoodTruck",
                 
                 tabPanel("summary",
                          titlePanel("Résumé des données"),
                          mainPanel(
                            width = 100,
                            verbatimTextOutput("sum")
                          )
                 ),
                 
                 tabPanel("Compostion des menus",
                          titlePanel("Composition optimal des menus"),
                          sidebarLayout(
                            # Sidebar panel for inputs ----
                            sidebarPanel(
                              h2(textOutput("variableTitle", container = span)),
                              # Input: Numeric entry for number of obs to view ----
                              
                              numericInput(inputId = "nbDays",
                                           label = "Number of days",
                                           value = 7),
                              numericInput(inputId = "sla",
                                           label = "Respect of the SLA (%)",
                                           value = 90),
                              numericInput(inputId = "pricePasta",
                                           label = "Price of pasta",
                                           value = 2),
                              numericInput(inputId = "priceRice",
                                           label = "Price of rice",
                                           value = 2.75),
                              numericInput(inputId = "priceVegetable",
                                           label = "Price of vegetable",
                                           value = 8),
                              numericInput(inputId = "priceFish",
                                           label = "Price of fish",
                                           value = 15)
                            ),
                            
                            mainPanel(
                              verbatimTextOutput("comp"),
                              plotOutput("plotMenu")
                            )
                          )
                 ),
                 
                 tabPanel("Correlation des variables",
                          titlePanel("Correlation des variables"),
                          mainPanel(
                            width = 100,
                            verbatimTextOutput("corrs")
                            
                          )
                 ),
                 tabPanel("ANOVA",
                          titlePanel("Simple ANOVA TEST"),
                          sidebarLayout(
                            sidebarPanel(
                              radioButtons("plotType", "Menu",
                                           c("BlueLake"="b", "Surprise"="s", "Earth"="e")
                              )
                            ),
                            mainPanel(
                              verbatimTextOutput("anova"),
                              width = 100,
                              plotOutput("plot")
                            )
                          )
                 ),
                 tabPanel("Model",
                          titlePanel("Multivariate regression model"),
                          sidebarLayout(
                            sidebarPanel(
                              radioButtons("plotTypeMenu", "Menu",
                                           c("BlueLake"="b", "Surprise"="s", "Earth"="e", "all"="a")
                              ),
                              radioButtons("resi", "Residual",
                                           c("Normality"="1", "Homoscedesticity"="2", "Linearity"="3")
                              ),
                              "Variables for prediction (seulement les variables présentes dans lm() sont prise en compte)",
                              selectInput("week_day", "Week day:",
                                          c("Monday" = "Monday",
                                            "Tuesday" = "Tuesday",
                                            "Wednesday" = "Wednesday",
                                            "Thursday" = "Thursday",
                                            "Friday" = "Friday",
                                            "Saturday" = "Saturday",
                                            "Sunday" = "Sunday")),
                              selectInput("location", "Location",
                                          c("1" = "1",
                                            "2" = "2",
                                            "3" = "3",
                                            "4" = "4")),
                              numericInput(inputId = "min_temp",
                                           label = "Min. temp.",
                                           value = 6),
                              numericInput(inputId = "max_temp",
                                           label = "Max. temp",
                                           value = 18),
                              numericInput(inputId = "rainfal",
                                           label = "Rainfall",
                                           value = 0.7),
                              numericInput(inputId = "wind",
                                           label = "Wind",
                                           value = 13),
                              numericInput(inputId = "passerby",
                                           label = "Passerby",
                                           value = 220)
                            ),
                            mainPanel(
                              verbatimTextOutput("model"),
                              width = 100,
                              plotOutput("plotModel")
                            )
                          )
                 ),
                 tabPanel("AIC",
                          titlePanel("AIC"),
                          sidebarLayout(
                            sidebarPanel(
                              radioButtons("plotTypeMenuAIC", "Menu",
                                           c("BlueLake"="b", "Surprise"="s", "Earth"="e", "all"="a")
                              ),
                              radioButtons("AICType", "Type",
                                           c("Poly"="p", "I()"="i")
                              ),
                              "Variables for prediction (seulement les variables présentes dans lm() sont prise en compte)",
                              selectInput("week_dayAIC", "Week day:",
                                          c("Monday" = "Monday",
                                            "Tuesday" = "Tuesday",
                                            "Wednesday" = "Wednesday",
                                            "Thursday" = "Thursday",
                                            "Friday" = "Friday",
                                            "Saturday" = "Saturday",
                                            "Sunday" = "Sunday")),
                              selectInput("locationAIC", "Location",
                                          c("1" = "1",
                                            "2" = "2",
                                            "3" = "3",
                                            "4" = "4")),
                              numericInput(inputId = "min_tempAIC",
                                           label = "Min. temp.",
                                           value = 6),
                              numericInput(inputId = "max_tempAIC",
                                           label = "Max. temp",
                                           value = 18),
                              numericInput(inputId = "rainfalAIC",
                                           label = "Rainfall",
                                           value = 0.7),
                              numericInput(inputId = "windAIC",
                                           label = "Wind",
                                           value = 13),
                              numericInput(inputId = "passerbyAIC",
                                           label = "Passerby",
                                           value = 220)
                            ),
                            mainPanel(
                              verbatimTextOutput("AIC")
                            )
                          )
                 ),
                 tabPanel("CSV test",
                          titlePanel("Predictions based on foodtruck_test.csv (best model)"),
                          mainPanel(
                            verbatimTextOutput("test")
                          ))
)




# Define server logic ----
server <- function(input, output) {
  
  data <- read.csv(file="goodfood_group_7.csv",sep=";")
  data$week_day <- factor(data$week_day, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday","Saturday", "Sunday"))
  data$location <- as.factor(data$location)
  data$sumSoldes <- data$BlueLake + data$Earth + data$Surprise
  
  model <- eventReactive(list(input$priceRice, input$priceFish, input$nbDays, input$sla, input$pricePasta, input$priceVegetable ), {
    menu.label <- c("Earth","Suprise", "BlueLake")
    menu.value <- c(8, 10, 15)
    menu.volume <- c(0.0005, 0.0008, 0.0012)
    menu.weight <- c(0.4, 0.6, 0.5)
    ingredient.label <- c("Pasta","Rice","Vegetable", "Fish")
    
    ingredient.cost <- c(input$pricePasta, input$priceRice,input$priceVegetable,input$priceFish)
    
    nbvar <- length(ingredient.label)*length(menu.label)
    mymodel <- make.lp(0, nbvar)
    mycoef <- c(ingredient.cost[1], ingredient.cost[2], ingredient.cost[3], ingredient.cost[4],
                ingredient.cost[1], ingredient.cost[2], ingredient.cost[3], ingredient.cost[4],
                ingredient.cost[1], ingredient.cost[2], ingredient.cost[3], ingredient.cost[4])
    
    set.objfn(mymodel, mycoef)
    mysense <- "min"
    lp.control(mymodel,sense=mysense)
    
    # set weight sur le menu 1
    add.constraint(mymodel, c(1,1,1,1,
                              0,0,0,0,
                              0,0,0,0), "=", menu.weight[1])
    
    # set weight sur le menu 2
    add.constraint(mymodel, c(0,0,0,0,
                              1,1,1,1,
                              0,0,0,0), "=", menu.weight[2]) 
    
    # set weight sur le menu 3
    add.constraint(mymodel, c(0,0,0,0,
                              0,0,0,0,
                              1,1,1,1), "=", menu.weight[3])
    
    # menu earth a least 50% of pasta (solve x1 >= (x1 + y1 + z1 + w1) * 0.50)
    add.constraint(mymodel, c(0.5,-0.5,-0.5,-0.5,
                              0,0,0,0,
                              0,0,0,0), ">=", 0)
    
    # menu earth a least 10% of Vegetable (solve z1 >= (x1 + y1 + z1 + w1) * 0.10)
    add.constraint(mymodel, c(-0.1,-0.1,0.9,-0.1,
                              0,0,0,0,
                              0,0,0,0), ">=", 0)
    
    # menu suprise a least 30% of Rice (solve y2 >= (x2 + y2 + z2 + w2) * 0.30)
    add.constraint(mymodel, c(0,0,0,0,
                              -0.3,0.7,-0.3,-0.3,
                              0,0,0,0), ">=", 0)
    
    # menu suprise a least 40% of Vegetable (solve z2 >= (x2 + y2 + z2 + w2) * 0.40)
    add.constraint(mymodel, c(0,0,0,0,
                              -0.4,-0.4,0.6,-0.4,
                              0,0,0,0), ">=", 0)
    
    # menu suprise a least 20% of Fish (solve w2 >= (x2 + y2 + z2 + w2) * 0.20)
    add.constraint(mymodel, c(0,0,0,0,
                              -0.2,-0.2,-0.2,0.8,
                              0,0,0,0), ">=", 0)
    
    # menu BlueLake a least 20% of Vegetable (solve z3 >= (x3 + y3 + z3 + w3) * 0.20)
    add.constraint(mymodel, c(0,0,0,0,
                              0,0,0,0,
                              -0.2,-0.2,0.8,-0.2), ">=", 0)
    
    # menu BlueLake a least 60% of Fish (solve w3 >= (x3 + y3 + z3 + w3) * 0.40)
    add.constraint(mymodel, c(0,0,0,0,
                              0,0,0,0,
                              -0.6,-0.6,-0.6,0.4), ">=", 0)
    
    # solve the LPA
    solve(mymodel)
    mymodel.obj <- get.objective(mymodel)
    mymodel.var <- get.variables(mymodel)
    
    nbvar <- length(menu.label)
    endModel <- make.lp(0, nbvar)
    mycoef <- c(menu.value[1] - ((ingredient.cost[1] * mymodel.var[1]) + (ingredient.cost[2] * mymodel.var[2]) + (ingredient.cost[3] * mymodel.var[3]) + (ingredient.cost[4] * mymodel.var[4])),
                menu.value[2] - ((ingredient.cost[1] * mymodel.var[5]) + (ingredient.cost[2] * mymodel.var[6]) + (ingredient.cost[3] * mymodel.var[7]) +  (ingredient.cost[4] * mymodel.var[8])),
                menu.value[3] - ((ingredient.cost[1] * mymodel.var[9]) + (ingredient.cost[2] * mymodel.var[10]) + (ingredient.cost[3] * mymodel.var[11]) + (ingredient.cost[4] * mymodel.var[12])))
    
    surprise <- data$Surprise
    earth <- data$Earth
    Bluelake <- data$BlueLake
    
    # Volum of the fridge
    add.constraint(endModel, c(menu.volume[1], menu.volume[2], menu.volume[3]), "<=", 2)
    
    
    qEarth <- floor(quantile(earth,c((input$sla/100)))[[1]])
    qSurprise <- floor(quantile(surprise,c(input$sla/100))[[1]])
    qBl <- floor(quantile(Bluelake,c(input$sla/100))[[1]])
    c <- c(qEarth*input$nbDays, qSurprise*input$nbDays, qBl*input$nbDays)
    
    # Contraint of SLA
    add.constraint(endModel, c(1,0,0), ">=", qEarth*input$nbDays)
    add.constraint(endModel, c(0,1,0), ">=", qSurprise*input$nbDays)
    add.constraint(endModel, c(0,0,1), ">=", qBl*input$nbDays)
    
    add.constraint(endModel, c(1,0,0), "<=", qEarth*input$nbDays)
    add.constraint(endModel, c(0,1,0), "<=", qSurprise*input$nbDays)
    add.constraint(endModel, c(0,0,1), "<=", qBl*input$nbDays)
    
    set.objfn(endModel, mycoef)
    mysense <- "max"
    lp.control(endModel,sense=mysense)
    
    set.type(endModel, 1, "integer")
    set.type(endModel, 2, "integer")
    set.type(endModel, 3, "integer")
    
    # solve the LPA
    solve(endModel)
    
    endModel.obj <- get.objective(endModel)
    endModel.var <- get.variables(endModel)
    
    menuEarthDF <- data.frame (
      ingredient <- c("Pasta","Rice","Vegetable", "Fish"),
      value <- c(mymodel.var[1], mymodel.var[2],mymodel.var[3], mymodel.var[4])
    )
    
    bp <- ggplot(menuEarthDF, aes(x="Kg", y=value, fill=ingredient))+
      geom_bar(width = 1, stat = "identity")
    pie <- bp + coord_polar("y", start=0) + ggtitle("Menu Earth")
    
    menuSurpriseDF <- data.frame (
      ingredient2 <- c("Pasta","Rice","Vegetable", "Fish"),
      value2 <- c(mymodel.var[5], mymodel.var[6],mymodel.var[7], mymodel.var[8])
    )
    
    bp2 <- ggplot(menuSurpriseDF, aes(x="Kg", y=value2, fill=ingredient2))+
      geom_bar(width = 1, stat = "identity")
    pie2 <- bp2 + coord_polar("y", start=0) + ggtitle("Menu Surprise")
    
    menuBluelakeDF <- data.frame (
      ingredient3 <- c("Pasta","Rice","Vegetable", "Fish"),
      value3 <- c(mymodel.var[9], mymodel.var[10],mymodel.var[11], mymodel.var[12])
    )
    
    bp3 <- ggplot(menuBluelakeDF, aes(x="Kg", y=value3, fill=ingredient3))+
      geom_bar(width = 1, stat = "identity")
    pie3 <- bp3 + coord_polar("y", start=0) + ggtitle("Menu Bluelake")
    
    
    
    menuCost <- c((ingredient.cost[1] * mymodel.var[1]) + (ingredient.cost[2] * mymodel.var[2]) + (ingredient.cost[3] * mymodel.var[3]) + (ingredient.cost[4] * mymodel.var[4]),
                  (ingredient.cost[1] * mymodel.var[5]) + (ingredient.cost[2] * mymodel.var[6]) + (ingredient.cost[3] * mymodel.var[7]) +  (ingredient.cost[4] * mymodel.var[8]),
                  (ingredient.cost[1] * mymodel.var[9]) + (ingredient.cost[2] * mymodel.var[10]) + (ingredient.cost[3] * mymodel.var[11]) + (ingredient.cost[4] * mymodel.var[12]))
    
    
    # return all object as a list
    list(plotEarth = pie, plotSurprise = pie2, plotBluelake = pie3, modelCompositionMenu = mymodel.var, modelPrice = endModel.var, modelBenef = endModel.obj ,menuCost = menuCost,  nbMenu  = c)
  })
  
  output$comp <- renderPrint({
    model <- model()$modelCompositionMenu
    modelPrice <- model()$modelPrice
    nbMenu <- model()$nbMenu
    modelCost <- model()$menuCost
    
    modelBenef <- model()$modelBenef
    
    
    print("menu 1")
    print(paste(model[1], "kg of pasta", ", ", model[2], "kg of rice", ", ", model[3], "kg of Vegetable", ", ", model[4], "kg of Fish",  sep=""))
    print("menu 2")
    print(paste(model[5], "kg of pasta", ", ", model[6], "kg of rice", ", ", model[7], "kg of Vegetable", ", ", model[8], "kg of Fish",  sep=""))
    print("menu 3")
    print(paste(model[9], "kg of pasta", ", ", model[10], "kg of rice", ", ", model[11], "kg of Vegetable", ", ", model[12], "kg of Fish",  sep=""))
    print("Number of menus"); print(nbMenu)
    print("SLA")
    print(nbMenu)
    print("cost");print(modelCost[1]*nbMenu[1] + modelCost[2]*nbMenu[2] + modelCost[3]*nbMenu[3])
    
    print("CA");print((modelPrice[1]*8) + (modelPrice[2]*10) + (modelPrice[3]*15))
    print("Benef");print(modelBenef)
  })
  
  output$plotMenu <- renderPlot({
    
    e <- model()$plotEarth
    s <- model()$plotSurprise
    b <- model()$plotBluelake
    
    ptlist <- list(e, s, b)
    grid.arrange(grobs=ptlist, ncol=3)
    
  })
  
  output$sum <- renderPrint({
    print(summary(data))
    print(str(data))
  })
  
  output$corrs <-renderPrint({
    library(dplyr)
    library(reshape2)
    
    data2 <- read.csv(file="goodfood_group_7.csv",sep=";")
    datacorr <- as.data.frame(data2)
    datacorr$week_day <- as.numeric(data$week_day)
    datacorr$day <-as.numeric(data$day)
    
    print("Correlation entre variable")
    cor_value <- cor(datacorr, use="pairwise.complete.obs")
    abs(cor_value)
    
  })
  
  output$anova <- renderPrint({
    
    if(input$plotType == "b")
      anova_test <- aov(BlueLake ~ week_day, data = data)
    else if (input$plotType == "e")
      anova_test <- aov(Earth ~ week_day, data = data)
    else if (input$plotType == "s")
      anova_test <- aov(Surprise ~ week_day, data = data)
    
    summary(anova_test)
  })
  
  output$plot <- renderPlot({
    if(input$plotType == "b")
      ggplot(data=data, aes(x=week_day, y=BlueLake, group=week_day, fill=week_day)) +
      geom_boxplot()
    else if (input$plotType == "e")
      ggplot(data=data, aes(x=week_day, y=Earth, group=week_day, fill=week_day)) +
      geom_boxplot()
    else if (input$plotType == "s")
      ggplot(data=data, aes(x=week_day, y=Surprise, group=week_day, fill=week_day)) +
      geom_boxplot()
    
  })
  
  output$model <- renderPrint({
    
    if(input$plotTypeMenu == "b")
      fit_lm <- lm(data$BlueLake ~ week_day + location + max_temp + wind + rainfall , data)
    else if (input$plotTypeMenu == "e")
      fit_lm <- lm(data$Earth ~ week_day + location + max_temp + wind + rainfall , data)
    else if (input$plotTypeMenu == "s")
      fit_lm <- lm(data$Surprise ~ week_day + location + max_temp + wind + rainfall , data)
    else if (input$plotTypeMenu == "a")
      fit_lm <- lm(data$sumSoldes ~ week_day + location + max_temp + wind + rainfall , data)
    
    sum <- summary(fit_lm)
    x_new <- data.frame(week_day = input$week_day, location = input$location, rainfall = input$rainfal, wind = input$wind, max_temp = input$max_temp, min_temp = input$min_temp, passerby = input$passerby)
    p <- predict(fit_lm, newdata = x_new, interval = "predict", level = 0.95)
    print("prediction")
    print(p)
    print(sum)
    
    
    
  })
  
  output$plotModel <- renderPlot({
    
    if(input$plotTypeMenu == "b"){
      fit_lm <- lm(data$BlueLake ~ week_day + location + max_temp + wind + rainfall, data)
      d_f <- data.frame(residual = residuals(fit_lm), BlueLake = data$BlueLake)
      plot3 <- ggplot(d_f, aes(x=BlueLake, y=residual)) + geom_point()
    }
    else if (input$plotTypeMenu == "e"){
      fit_lm <- lm(data$Earth ~ week_day + location + max_temp + wind + rainfall, data)
      d_f <- data.frame(residual = residuals(fit_lm), Earth = data$Earth)
      plot3 <- ggplot(d_f, aes(x=Earth, y=residual)) + geom_point()
    }
    else if (input$plotTypeMenu == "s"){
      fit_lm <- lm(data$Surprise ~ week_day + location + max_temp + wind + rainfall, data)
      d_f <- data.frame(residual = residuals(fit_lm), Surprise = data$Surprise)
      plot3 <- ggplot(d_f, aes(x=Surprise, y=residual)) + geom_point()
    }
    else if (input$plotTypeMenu == "a"){
      fit_lm <- lm(data$sumSoldes ~ week_day + location + max_temp + wind + rainfall , data)
      d_f <- data.frame(residual = residuals(fit_lm), sumSoldes = data$sumSoldes)
      plot3 <- ggplot(d_f, aes(x=sumSoldes, y=residual)) + geom_point()
    }
    
    if(input$resi == "1")
      autoplot(fit_lm, which=2, label.size=3)
    else if (input$resi == "2")
      autoplot(fit_lm, which=1, label.size=3)
    else if (input$resi == "3")
      plot3
    
  })
  
  output$AIC <- renderPrint({
    
    require(MASS)
    if(input$AICType == "i"){
      if(input$plotTypeMenuAIC == "b")
        fit_lm = lm(BlueLake ~ week_day + location + I(max_temp^2) + I(max_temp^3) + I(min_temp^2) +
                      I(min_temp^3) + I(rainfall^2) + I(rainfall^3) + I(passerby^2) + I(passerby^3) + I(wind^2) + I(wind^3), data=data)
      else if (input$plotTypeMenuAIC == "e")
        fit_lm = lm(Earth ~ week_day + location + I(max_temp^2) + I(max_temp^3) + I(min_temp^2) +
                      I(min_temp^3) + I(rainfall^2) + I(rainfall^3) + I(passerby^2) + I(passerby^3) + I(wind^2) + I(wind^3), data=data)
      else if (input$plotTypeMenuAIC == "s")
        fit_lm = lm(Surprise ~ week_day + location + I(max_temp^2) + I(max_temp^3) + I(min_temp^2) +
                      I(min_temp^3) + I(rainfall^2) + I(rainfall^3) + I(passerby^2) + I(passerby^3) + I(wind^2) + I(wind^3), data=data)
      else if (input$plotTypeMenuAIC == "a")
        fit_lm = lm(sumSoldes ~ week_day + location + I(max_temp^2) + I(max_temp^3) + I(min_temp^2) +
                      I(min_temp^3) + I(rainfall^2) + I(rainfall^3) + I(passerby^2) + I(passerby^3) + I(wind^2) + I(wind^3), data=data)
    } else {
      if(input$plotTypeMenuAIC == "b")
        fit_lm <- lm(BlueLake ~ week_day + location + poly(max_temp,3) + poly(min_temp,3) + poly(rainfall,3) + poly(passerby,3) + 
                       poly(wind,3), data = data)
      else if (input$plotTypeMenuAIC == "e")
        fit_lm <- lm(Earth ~ week_day + location + poly(max_temp,3) + poly(min_temp,3) + poly(rainfall,3) + poly(passerby,3) + 
                       poly(wind,3), data = data)
      else if (input$plotTypeMenuAIC == "s")
        fit_lm <- lm(Surprise ~ week_day + location + poly(max_temp,3) + poly(min_temp,3) + poly(rainfall,3) + poly(passerby,3) + 
                       poly(wind,3), data = data)
      else if (input$plotTypeMenuAIC == "a")
        fit_lm <- lm(sumSoldes ~ week_day + location + poly(max_temp,3) + poly(min_temp,3) + poly(rainfall,3) + poly(passerby,3) + 
                       poly(wind,3), data = data)
    }
    
    
    x_new <- data.frame(week_day = input$week_dayAIC, location = input$locationAIC, rainfall = input$rainfalAIC, wind = input$windAIC, max_temp = input$max_tempAIC, min_temp = input$min_tempAIC, passerby = input$passerbyAIC)
    
    p <- predict(stepAIC(fit_lm, trace=FALSE), newdata = x_new, interval = "predict", level = 0.95)
    print("Prediction (from best model)")
    print(p)
    print(stepAIC(fit_lm))
    
  })
  
  output$test <- renderPrint({
    require(MASS)
    data_test <- read.csv(file="goodfood_test.csv",sep=";")
    data_test$week_day <- factor(data_test$week_day, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday","Saturday", "Sunday"))
    
    data_test$location <- as.factor(data_test$location)
    
    df <- data.frame(week_day = data_test[[1,2]], location = data_test[[1,7]], rainfall = data_test[[1,5]], wind = data_test[[1,6]], max_temp = data_test[[1,3]], min_temp = data_test[[1,4]], passerby = data_test[[1,8]])
    df2 <- data.frame(week_day = data_test[[2,2]], location = data_test[[2,7]], rainfall = data_test[[2,5]], wind = data_test[[2,6]], max_temp = data_test[[2,3]], min_temp = data_test[[2,4]], passerby = data_test[[2,8]])
    df3 <- data.frame(week_day = data_test[[3,2]], location = data_test[[3,7]], rainfall = data_test[[3,5]], wind = data_test[[3,6]], max_temp = data_test[[3,3]], min_temp = data_test[[3,4]], passerby = data_test[[3,8]])
    
    fit_lm_b <- lm(BlueLake ~ week_day + location + I(max_temp^2) + I(max_temp^3) + I(min_temp^2) +
                     I(min_temp^3) + I(rainfall^2) + I(rainfall^3) + I(passerby^2) + I(passerby^3) + I(wind^2) + I(wind^3), data=data)
    fit_lm_e <- lm(Earth ~ week_day + location + I(max_temp^2) + I(max_temp^3) + I(min_temp^2) +
                     I(min_temp^3) + I(rainfall^2) + I(rainfall^3) + I(passerby^2) + I(passerby^3) + I(wind^2) + I(wind^3), data=data)
    fit_lm_s <- lm(Surprise ~ week_day + location + I(max_temp^2) + I(max_temp^3) + I(min_temp^2) +
                     I(min_temp^3) + I(rainfall^2) + I(rainfall^3) + I(passerby^2) + I(passerby^3) + I(wind^2) + I(wind^3), data=data)
    
    print("data")
    print(df);print(df2);print(df3)
    
    p1b <- predict(stepAIC(fit_lm_b, trace=FALSE), newdata = df, interval = "predict", level = 0.95)
    p2b <- predict(stepAIC(fit_lm_b, trace=FALSE), newdata = df2, interval = "predict", level = 0.95)
    p3b <- predict(stepAIC(fit_lm_b, trace=FALSE), newdata = df3, interval = "predict", level = 0.95)
    
    p1e <- predict(stepAIC(fit_lm_e, trace=FALSE), newdata = df, interval = "predict", level = 0.95)
    p2e <- predict(stepAIC(fit_lm_e, trace=FALSE), newdata = df2, interval = "predict", level = 0.95)
    p3e <- predict(stepAIC(fit_lm_e, trace=FALSE), newdata = df3, interval = "predict", level = 0.95)
    
    p1s <- predict(stepAIC(fit_lm_s, trace=FALSE), newdata = df, interval = "predict", level = 0.95)
    p2s <- predict(stepAIC(fit_lm_s, trace=FALSE), newdata = df2, interval = "predict", level = 0.95)
    p3s <- predict(stepAIC(fit_lm_s, trace=FALSE), newdata = df3, interval = "predict", level = 0.95)
    
    print("Prediction Bluelake")
    print(p1b);print(p2b);print(p3b);
    
    print("Prediction Earth")
    print(p1e);print(p2e);print(p3e);
    
    print("Prediction surprise")
    print(p1s);print(p2s);print(p3s);
    
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)