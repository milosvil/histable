body_css <- stringr::str_glue("body {{background-color: {pal[2]};}}")

fluidPage(
  
  tags$head(
    tags$link(href = "style.css", rel = "stylesheet", type = "text/css"),
    tags$style(body_css)
  ),
  
  fluidRow(
    tags$div(align = "center",
             tags$p(
               tags$span("H", style = "font-size: 25px; "),
               tags$span("I", style = "font-size: 36px; "),
               tags$span("S", style = "font-size: 50px; "),
               tags$span("T", style = "font-size: 80px; "),
               tags$span("A", style = "font-size: 60px; "),
               tags$span("B", style = "font-size: 40px; "),
               tags$span("L", style = "font-size: 36px; "),
               tags$span("E", style = "font-size: 25px; ")
             ),
             tags$p("Please select column."), 
             tags$br(),
             selectInput(inputId = "anim_type",
                         label = "Animation Type",
                         choices = c("STATIC" = 1,
                                     "SLIDE | LEFT TO RIGHT" = 2,
                                     "SLIDE | RIGHT TO LEFT" = 3,
                                     " STEP | LEFT TO RIGHT" = 4,
                                     " STEP | RIGHT TO LEFT" = 5),
                         selected = 1,
                         width = 250)
    )
  ),
  
  fluidRow(
    tags$div(style = "width: 1100px; margin: 0 auto;",
             DT::DTOutput("table")
    )
  )
)
