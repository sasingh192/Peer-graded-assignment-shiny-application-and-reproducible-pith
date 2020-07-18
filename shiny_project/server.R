library(shiny)
shinyServer(function(input, output) {
  min.RSS <- function(data, par) {
    breakpt1 <- par[1]
    data$mpgsp <- ifelse(data$mpg - breakpt1 > 0, data$mpg - breakpt1, 0)
    model <- lm(hp ~ mpgsp + mpg, data = data)
    data$predicted <- predict(model)
    with(data, sum((hp - predicted)^2))
  }
  result <- optimize(min.RSS, c(20, 30), data=mtcars)
  breakpt <- round(result$minimum, 2)
  output$breakpt <- renderText({breakpt})
  mtcars$mpgsp <- ifelse(mtcars$mpg - breakpt > 0, mtcars$mpg - breakpt, 0)
  model1 <- lm(hp ~ mpg, data = mtcars)
  model2 <- lm(hp ~ mpgsp + mpg, data = mtcars)
  
  model1pred <- reactive({
    mpgInput <- input$sliderMPG
    predict(model1, newdata = data.frame(mpg = mpgInput))
  })
  
  model2pred <- reactive({
    mpgInput <- input$sliderMPG
    predict(model2, newdata = 
              data.frame(mpg = mpgInput,
                         mpgsp = ifelse(mpgInput - breakpt > 0,
                                        mpgInput - breakpt, 0)))
  })
  
  output$plot1 <- renderPlot({
    mpgInput <- input$sliderMPG
    
    plot(mtcars$mpg, mtcars$hp, xlab = "Miles Per Gallon", 
         ylab = "Horsepower", bty = "n", pch = 16,
         xlim = c(10, 35), ylim = c(50, 350))
    if(input$showModel1){
      abline(model1, col = "red", lwd = 2)
    }
    if(input$showModel2){
      model2lines <- predict(model2, newdata = data.frame(
        mpg = 10:35, mpgsp = ifelse(10:35 - breakpt > 0, 10:35 - breakpt, 0)
      ))
      lines(10:35, model2lines, col = "blue", lwd = 2)
    }
    legend(25, 250, c("Model 1 Prediction", "Model 2 Prediction"), pch = 16, 
           col = c("red", "blue"), bty = "n", cex = 1.2)
    points(mpgInput, model1pred(), col = "red", pch = 16, cex = 2)
    points(mpgInput, model2pred(), col = "blue", pch = 16, cex = 2)
    abline(v = breakpt, col = 'blue', lwd=3)
    text(breakpt+2.4, 325, paste("break_point = \n", as.character(breakpt)), col = 'blue')
  })
  
  output$pred1 <- renderText({
    round(model1pred(), 2)
  })
  
  output$pred2 <- renderText({
    round(model2pred(), 2)
  })
})