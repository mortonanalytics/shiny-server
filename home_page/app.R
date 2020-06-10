library(shiny)
library(shinydashboard)
library(mailR)

# Define UI for application using prepopulated
ui <- navbarPage(
  title = "Morton Analytics",
  tabPanel("Welcome",
           HTML(readLines("./www/welcome_page.html"))),
  tabPanel("What We Do",
           HTML(readLines("./www/what_we_do.html"))),
  tabPanel("About Us",
           HTML(readLines("./www/about_us.html")),
           tags$div(style = "margin: 5%;",
                    textAreaInput("form", "Contact Us", width = "500px", height = "150px"),
                    textInput("email", "Your Email"),
                    actionButton("submit_btn", "Submit")),
           tags$head(
             tags$meta(charset="UTF-8"),
             tags$meta(name = "description", content = "Data Analytics and Data Science for the everday business professional"),
             tags$meta(name = "keywords", content = "Data Analytics, Data Science, Dashboard, Consultant, Big Data, R, SQL, JavaScript, Missoula, Montana, Morton Analytics, myIO")
           ),
           br(),
           br(),
           br(),
           br(),
           br(),
           br(),
           br(),
           br())
)

# Define server logic
server <- function(input, output,session) {
  observe({
    if(is.null(input$submit_btn) || input$submit_btn ==0) return(NULL)
    to <- "morton@myMA.us"
    from <- isolate(input$email)
    msg <- isolate(input$form)
    
    mailR::send.mail(from = from,
                     to = to,
                     subject = "Contact Form Submission",
                     body = msg,
                     smtp = list(host.name = "smtp.gmail.com", port = 465),
                     authenticate = FALSE,
                     send = TRUE)
    updateTextAreaInput(session, "form", value = "")
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

