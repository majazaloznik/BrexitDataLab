function(input, output, session) {
  load("AllReady.Rdata")
  source("04-Functions.R")
  
  active.table <- all.6age.groups[,3:5] 
  
  active.table <- reactive({
    active.table <- data.frame(
      registered.prop = 100*c(input$R1, input$R2,input$R3,input$R4,input$R5,input$R6),
      turnout.prop=100*c(input$T1, input$T2,input$T3,input$T4,input$T5,input$T6),
      remain.prop= 100*c(input$V1, input$V2,input$V3,input$V4,input$V5,input$V6)) 
    return(active.table)
  })
  
  FunTable <- function(){
    all.6age.groups[,3:5] <- active.table()
    active.table <- rbind(all.6age.groups[,3:5] , FunCalculateResult(all.6age.groups, base = input$base)[,c(4,5,8)])
    rownames(active.table) <- c(all.6age.groups$age.group, "total")
    colnames(active.table) <- c("Registered", "Turnout", "Remain")
    active.table
  }
  
    
  output$table <- renderTable({ FunTable()})
  output$plot <- renderPlot({
    all.6age.groups$turnout.prop[1:6] <- 
      c(input$T1, input$T2,input$T3,input$T4,input$T5,input$T6)
    all.6age.groups$registered.prop[1:6] <- 
      c(input$R1, input$R2,input$R3,input$R4,input$R5,input$R6)
    all.6age.groups$remain.prop[1:6] <- 
      c(input$V1, input$V2,input$V3,input$V4,input$V5,input$V6)
    
  #  all.6age.groups[,3:5] <-active.table()/100
    FunBestPlot(all.6age.groups, base=input$base)
  })

  output$resetable_registration <- renderUI({
    times <- input$reset_reg
    div(id=letters[(times %% length(letters)) + 1],
        sliderInput("R6", "Registration over 65", min = 0,max = 1, step=0.001,
                    value = all.6age.groups$registered.prop[6]),
        sliderInput("R5", "Registration 55-65", min = 0,max = 1, step=0.001,
                    value = all.6age.groups$registered.prop[5]),
        sliderInput("R4", "Registration 45-54", min = 0,max = 1, step=0.001,
                    value = all.6age.groups$registered.prop[4]),
        sliderInput("R3", "Registration 35-44", min = 0,max = 1, step=0.001,
                    value = all.6age.groups$registered.prop[3]),
        sliderInput("R2", "Registration 25-34", min = 0,max = 1, step=0.001,
                    value = all.6age.groups$registered.prop[2]),
        sliderInput("R1", "Registration 18-24", min = 0,max = 1, step=0.001,
                    value = all.6age.groups$registered.prop[1])
    )})
  
  output$resetable_turnout <- renderUI({
    times <- input$reset_to
    div(id=letters[(times %% length(letters)) + 1],
        sliderInput("T6", "Turnout over 65", min = 0,max = 1, step=0.001,
                    value = all.6age.groups$turnout.prop[6]),
        sliderInput("T5", "Turnout 55-65", min = 0,max = 1, step=0.001,
                    value = all.6age.groups$turnout.prop[5]),
        sliderInput("T4", "Turnout 45-54", min = 0,max = 1, step=0.001,
                    value = all.6age.groups$turnout.prop[4]),
        sliderInput("T3", "Turnout 35-44", min = 0,max = 1, step=0.001,
                    value = all.6age.groups$turnout.prop[3]),
        sliderInput("T2", "Turnout 25-34", min = 0,max = 1, step=0.001,
                    value = all.6age.groups$turnout.prop[2]),
        sliderInput("T1", "Turnout 18-24", min = 0,max = 1, step=0.001,
                    value = all.6age.groups$turnout.prop[1])
    )
  })
  
  output$resetable_voting <- renderUI({
    times <- input$reset_vote
    div(id=letters[(times %% length(letters)) + 1],
        sliderInput("V6", "Remain over 65", min = 0,max = 1, step=0.001,
                    value = all.6age.groups$remain.prop[6]),
        sliderInput("V5", "Remain 55-65", min = 0,max = 1, step=0.001,
                    value = all.6age.groups$remain.prop[5]),
        sliderInput("V4", "Remain 45-54", min = 0,max = 1, step=0.001,
                    value = all.6age.groups$remain.prop[4]),
        sliderInput("V3", "Remain 35-44", min = 0,max = 1, step=0.001,
                    value = all.6age.groups$remain.prop[3]),
        sliderInput("V2", "Remain 25-34", min = 0,max = 1, step=0.001,
                    value = all.6age.groups$remain.prop[2]),
        sliderInput("V1", "Remain 18-24", min = 0,max = 1, step=0.001,
                    value = all.6age.groups$remain.prop[1])
    )
  })
  
}