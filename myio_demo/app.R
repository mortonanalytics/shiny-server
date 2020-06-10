library(shiny)
library(shinydashboard)
library(htmlwidgets)
library(jsonlite)
library(dplyr)
library(tibble)
library(broom)
library(viridis)
library(myIO)

# Define UI
ui <- navbarPage(
  title = "myIO",
  windowTitle = "myIO | Morton Analytics",
  tabPanel("Home",
           fluidRow(
           box(status = "primary",myIOOutput("home_page")),
           box(h1("Welcome!"),
               p("This is the myIO project! The project is dedicated to helping R programmers get the most out of JavaScript plotting
                 via d3.js."),
               h3("How can I contribute?"),
               p("Easy. Go to the Morton Analytics GitHub page for the project. Decide whether you want to contribute R or JavaScript or both.
                 Fork the project and start coding! If you think somoething adds to the project, send a pull request."),
               uiOutput("project_page"),
               h3("What should I contibute?"),
               p("Anything! Certainly, R users tend to like statistical plotting functions, but I'll take JavaScript based
                 solutions as well."),
               h3("Who will benefit?"),
               p("The R community and those they serve. In particular, this project is meant to help Shiny application developers
                 present their work on the web using a fantastic, data-driven JavaScript API."))),
           br(),
           fluidRow(img(src = "myIOsticker.png", width = "200px", height = "200px", style = "margin:10px", align = "center"))
           
           ),
  tabPanel("ScatterPlot",
           sidebarLayout(
             sidebarPanel(uiOutput("scatter_ui"),
                          #uiOutput("scatter_model_ui"),
                          uiOutput("hexbin_ui"),
                          uiOutput("anscombe_ui"),
                          width = 2),
             mainPanel(
               tabsetPanel(
                 tabPanel("Scatterplot",box(myIOOutput("scatterplot", width = "100%"), width = "100%")),
                 #tabPanel("With Model",box(myIOOutput("scatter_model", width = "100%"), width = "100%")),
                 tabPanel("Hex-bin", 
                          box(myIOOutput("hexbin", width = "600px", height = "400px"), width = "400px", height = "400px"),
                          box(myIOOutput("hexpoints", width = "600px", height = "400px"),width = "400px", height = "400px")),
                 tabPanel("Anscombe Quartet",
                          box(myIOOutput("anscombe", width = "100%"), width = "100%"),
                          box(tableOutput("anscombe_model"),
                              tableOutput("anscombe_stat")))
               ))
           )
  ),
  tabPanel("Line Chart",
           sidebarLayout(
             sidebarPanel(uiOutput("linechart_ui"), width = 2),
             mainPanel(
               tabsetPanel(
                 tabPanel("Time Series", box(myIOOutput("linechart", width = "100%"), width = "100%")),
                 tabPanel("Regression Line", 
                          box(myIOOutput("regLine", width = "100%"), width = "100%"),
                          box(tableOutput("dragPoints")))
                 )
             )
           )
  ),
  tabPanel("Bar Chart",
           sidebarLayout(
             sidebarPanel(
               sliderInput("bar_ui", "Slide to alter donut", min = 1, max = 15, value = 10, step = 1), width = 2),
             mainPanel(
               tabsetPanel(
                 tabPanel("Bullet Bar Chart", 
                          box(myIOOutput("bullet", width = "100%"), width = "100%")
                          ),
                 tabPanel("Grouped Bar Chart", 
                          box(myIOOutput("grouped_bar", width = "100%"), width = "100%"),
                          )
               )
             )
           )
  ),
  tabPanel("Proportions",
           sidebarLayout(
             sidebarPanel(
               uiOutput("treemap_ui"),
               sliderInput("donut", "Slide to alter donut", min = 1, max = 10, value = 5, step = 0.2),
               width = 2
               ),
             mainPanel(
               tabsetPanel(
                 tabPanel("Treemap",
                          box(myIOOutput("treemap", width = "100%"), width = "100%")
                          ),
                 tabPanel("Donut/Pie Chart",
                          box(myIOOutput("donut_chart", width = "100%"), width = "100%")
                          )
               )
               
             )
            )
           )
               )

# Define server logic
server <- function(input, output,session) {
  
  output$home_page <- renderMyIO({
    df <- airquality[airquality$Month ==7, ]
    
    myIO() %>%
      addIoLayer(
        type = "line",
        color= "steelblue",
        label = "Temp",
        data = df,
        mapping = list(
          x_var = "Day",
          y_var = "Temp"
        )
      ) %>%
      addIoLayer(
        type = "line",
        color = "orange",
        label = "Ozone",
        data = df,
        mapping = list(
          x_var = "Day",
          y_var = "Ozone"
        )
      )%>%
      addIoLayer(
        type = "line",
        color = "lightgreen",
        label = "Wind",
        data = df,
        mapping = list(
          x_var = "Day",
          y_var = "Wind"
        )
      )
  })
  
  output$project_page <- renderUI({
    url <- a("Project GitHub Page", href="https://github.com/mortonanalytics/myIO")
    tagList("URL link:", url)
  })
  
  output$scatter_ui <- renderUI({
    selectInput("scatter_ui", "Scatterplot Variable",choices = colnames(airquality),selected = "Ozone")
  })
  
  output$scatter_model_ui <- renderUI({
    selectInput("scatter_model_ui", "Model variable", choices = colnames(mtcars), selected = "wt")
  })
  
  output$hexbin_ui <- renderUI({
    numericInput(inputId = "hexbin_ui", label = "Hexbin Std Dev", value = 2, min = 0, max = 10, step = 1)
  })
  
  output$anscombe_ui <- renderUI({
    selectInput("anscombe_ui", "Anscombe Quartet", choices = c("First", "Second", "Third", "Fourth"), selected = "First")
  })
  
  output$linechart_ui <- renderUI({
    selectInput("linechart_ui", "Select a dataset", choices = c("First", "Second", "Third", "Fourth"), selected = "First")
  })
  
  output$treemap_ui <- renderUI({
    selectInput("treemap_ui", "Select a Nesting Column", choices = c("vs", "am", "gear", "carb"), selected = "carb")
  })
  
  output$scatterplot <- renderMyIO({
    req(input$scatter_ui) 
    myIO() %>%
      addIoLayer(type = "point",
                 color = "steelblue",
                 label = "5",
                 data = airquality %>% filter(Month == 5),
                 mapping = list(x_var = "Day",
                                y_var = input$scatter_ui)) %>%
      suppressLegend()
  })
  
  # output$scatter_model <- renderMyIO({
  #   req(input$scatter_model_ui)
  #   df <- mtcars
  #   myIO() %>%
  #     addIoLayer(type = 'point',
  #                color = 'steelblue',
  #                label = 'mypoints',
  #                data = df[order(df[[input$scatter_model_ui]]),],
  #                mapping = list(x_var = input$scatter_model_ui,
  #                               y_var = "mpg")) %>%
  #     addIoStatLayer(type = "lm",
  #                    color = "red",
  #                    label = "LinearModel",
  #                    data = df[order(df[[input$scatter_model_ui]]),],
  #                    mapping = list(x_var = input$scatter_model_ui,
  #                                   y_var = "mpg")
  #     )
  # })
  
  anscombe_df <- reactive({
    req(input$anscombe_ui)
    df_1 <- with(anscombe, data.frame(xVal=c(x1),yVal=c(y1), group = "First", stringsAsFactors = FALSE))
    df_2 <- with(anscombe, data.frame(xVal=c(x2),yVal=c(y2), group = "Second", stringsAsFactors = FALSE))
    df_3 <- with(anscombe, data.frame(xVal=c(x3),yVal=c(y3), group = "Third", stringsAsFactors = FALSE))
    df_4 <- with(anscombe, data.frame(xVal=c(x4),yVal=c(y4), group = "Fourth", stringsAsFactors = FALSE))
    
    final_df <- list(df_1, df_2, df_3, df_4)
    final_df <- do.call("rbind", final_df)
    final_df <- final_df %>% filter(group == input$anscombe_ui)
  })
  
  output$anscombe <- renderMyIO({
    req(input$anscombe_ui)
    df <- anscombe_df()
    
    myIO() %>%
      addIoLayer(type = 'point',
                 color = 'steelblue',
                 label = input$anscombe_ui,
                 data = df,
                 mapping = list(x_var = "xVal",
                                y_var = "yVal")) %>%
      dragPoints()
    
  })
  
  output$anscombe_model <- renderTable({
    req(input$anscombe_ui)
    
    df <- anscombe_df()
    
    fit <- lm(yVal ~ xVal, data = df)
    
    final_df <- tidy(fit) %>%
      mutate(Group = input$anscombe_ui)
    
  })
  
  output$anscombe_stat <- renderTable({
    req(input$anscombe_ui)
    
    df <- anscombe_df()
    
    fit <- lm(yVal ~ xVal, data = df)
    
    new_df <- glance(fit)
    
    return(new_df)
    
  })
  
  random_df <- reactive({
    req(input$hexbin_ui)
    df <- data.frame(x =rnorm(2000, 10,input$hexbin_ui), y = rnorm(2000,10,input$hexbin_ui))
  })
  
  output$hexbin <- renderMyIO({
    req(input$hexbin_ui)
    df <- random_df()
    
    myIO() %>%
      addIoLayer(type = "hexbin",
                 color = "steelblue",
                 label = "Points",
                 data = df,
                 mapping = list(x_var = "x",
                                y_var = "y",
                                radius = 20))
  })
  
  output$hexpoints <- renderMyIO({
    req(input$hexbin_ui)
    df <- random_df()
    
    myIO() %>%
      addIoLayer(type = "point",
                 color = "steelblue",
                 label = "Points",
                 data = df,
                 mapping = list(x_var = "x",
                                y_var = "y"))
  })
  
  output$linechart <- renderMyIO({
    req(input$linechart_ui)
    
    if(input$linechart_ui == "First") {
      df <- airquality[airquality$Month == 5, ]
    } else if(input$linechart_ui == "Second"){
        df <- airquality[airquality$Month == 6, ]
    } else if(input$linechart_ui == "Third") {
      df <- airquality[airquality$Month == 7, ]
    } else {
      df <- airquality[airquality$Month == 8, ]
        }
    
    myIO() %>%
      addIoLayer(
        type = "line",
        color= "steelblue",
        label = "Temp",
        data = df,
        mapping = list(
          x_var = "Day",
          y_var = "Temp"
        )
      ) %>%
      addIoLayer(
        type = "line",
        color = "orange",
        label = "Ozone",
        data = df,
        mapping = list(
          x_var = "Day",
          y_var = "Ozone"
        )
      )%>%
      addIoLayer(
        type = "line",
        color = "red",
        label = "Wind",
        data = df,
        mapping = list(
          x_var = "Day",
          y_var = "Wind"
        )
      )%>%
      addIoLayer(
        type = "line",
        color = "green",
        label = "Solar",
        data = df,
        mapping = list(
          x_var = "Day",
          y_var = "Solar.R"
        )
      )
  })
  
  output$regLine <- renderMyIO({
    myIO() %>%
      addIoLayer(type = 'point',
                 color = 'steelblue',
                 label = 'MPG',
                 data = mtcars[order(mtcars$mpg),],
                 mapping = list(x_var = "wt",
                                y_var = "mpg")) %>%
      dragPoints()
  })
  
    output$dragPoints <- renderTable({
      req(input$myIOpointsX)
      data.frame(x = input$myIOpointsX,
                 y = input$myIOpointsY,
                 estimate = input$myIOpointsEst)
      })
  
  output$treemap <- renderMyIO({
    req(input$treemap_ui)
    df <- mtcars %>% mutate(cars = rownames(.))
    class(df$cyl) <- "character"
    class(df$vs) <- "character"
    class(df$am) <- "character"
    class(df$gear) <- "character"
    class(df$carb) <- "character"
    myIO() %>%
      addIoLayer(
        type = "treemap",
        color = c("steelblue", "red", "orange", "green" , "gold", "brown", "purple"),
        label = "treemap",
        data = df,
        mapping = list(
          level_2 = "cyl",
          level_1 = input$treemap_ui,
          x_var = "cars",
          y_var = "mpg"
        )
      )
    
  })
  
  output$donut_chart <- renderMyIO({
    
    rando <- rnorm(3, mean = input$donut, sd = input$donut / 3)
    
    df_donut <- data.frame(x = c("First", "Second", "Third"),
                           y = c(10, 9,8) *rando,
                           stringsAsFactors = FALSE)
    
    myIO() %>%
      addIoLayer(
        type = "donut",
        color = c("steelblue", "red", "orange"),
        label = "donut",
        data = df_donut,
        mapping = list(
          x_var = "x",
          y_var = "y"
        )
      )
  })
  
  ##### bar charts ####
  df <- datasets::airquality %>%
    mutate(Month = paste0("This Is the Month of ", Month),
           Temp_low = Temp * c(0.8,0.9,0.75),
           Temp_high = Temp * c(1.2,1.1,1.3)) %>%
    group_by(Day) %>%
    mutate(Percent = Temp/sum(Temp)) %>%
    ungroup() %>%
    mutate(Day2 = Day * c(1.02, 1.03, 1.01, 1.05, 1.1, 1.08))%>%
    arrange(Day)
  
  colors <- substr(viridis(5), 1, 7)
  
  output$bullet <- renderMyIO({
    myIO() %>%
      addIoLayer(type = "bar",
                 color = colors[4],
                 label = "This Is the Month of 5",
                 data = df %>% filter(Month == "This Is the Month of 5" & Day <= input$bar_ui ),
                 mapping = list(
                   x_var = "Day",
                   y_var = "Temp"
                 ))%>%
      addIoLayer(type = "bar",
                 color = colors[5],
                 label = "This Is the Month of 7",
                 data = df %>% filter(Month == "This Is the Month of 7" & Day <= input$bar_ui ),
                 mapping = list(
                   x_var = "Day",
                   y_var = "Temp"
                 ),
                 options = list(barSize = "small")
      )%>%
      setAxisLimits(ylim = list(min = "0"))
  })
  
  output$grouped_bar <- renderMyIO({
    myIO()%>%
      addIoLayer(type = "groupedBar",
                 color = colors,
                 label = "stuff",
                 data = df[df$Day <= input$bar_ui,] ,
                 mapping = list(
                   x_var = "Day",
                   y_var = "Temp",
                   # low_y = "Temp_low",
                   # high_y = "Temp_high",
                   group = "Month"
                 )) %>%
      setToggle(newY = "Percent", newScaleY = ".0%") %>%
      setAxisFormat(xAxis = ".0f",yAxis = ".0f") %>%
      setAxisLimits(xlim = list(max = paste0(input$bar_ui + 1)),ylim = list(min="0"))
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

