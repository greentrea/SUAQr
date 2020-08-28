#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# from CRAN


library(shiny)
library(shinyMobile)
shiny::shinyApp(
    ui = f7Page(
        title = "Orangutanapp",
        f7TabLayout(
            navbar = f7Navbar(
                title = "Follow",
                hairline = FALSE,
                shadow = TRUE
            ),
            toolbar = f7Toolbar(
                position = "bottom",
                f7Link(label = "Link 1", src = "https://www.google.com"),
                f7Link(label = "Link 2", src = "https://www.google.com", external = TRUE)
            ),
            # main content
            f7Card(verbatimTextOutput("infos")),
            f7Card(
                f7Text(inputId = "text", label = "Text"),
                f7Slider(inputId = "range1", label = "Range", min = 0, max = 2, value = 1, step = 0.1),
                f7Stepper(inputId = "stepper1", label = "Stepper", min = 0, max = 10, value = 5),
                verbatimTextOutput("lastChanged")
            )
        )
    ),
    server = function(input, output) {
        output$infos <- renderPrint(input$shinyInfo)
        output$lastChanged <- renderPrint(input$lastInputChanged)
    }
)
