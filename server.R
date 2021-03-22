
server <- function(input, output, session) {
  
  loginpage <- div(
    id = "loginpage",
    style = "width: 500px; max-width: 100%; height: 550px; max-height: 100%; margin: 0 auto; padding: 80px;",
    wellPanel(
      tags$h2("LOGIN", class = "text-center", style = "padding-top: 0;color:#000; font-weight:600;"),
      textInput("userName", placeholder="Username", label = tagList(icon("user"), "Username")),
      passwordInput("passwd", placeholder="Password", label = tagList(icon("unlock-alt"), "Password")),
      br(),
      div(
        style = "text-align: center;",
        actionButton("login", "SIGN IN",
                     style = "color: white; background-color:#3c8dbc; padding: 10px 15px; width: 150px; 
                     cursor: pointer; font-size: 18px; font-weight: 600;"),
        shinyjs::hidden(
          div(id = "nomatch",
              tags$p("Incorrect username or password. ",
                     style = "color: red; font-weight: 600; padding-top: 5px;font-size:16px;", 
                     class = "text-center")))
      )
    )
  )
  
  login <- FALSE
  USER <- reactiveValues(login = login)
  
  observe({ 
    if (USER$login == FALSE) {
      if (!is.null(input$login)) {
        if (input$login > 0) {
          Username <- isolate(input$userName)
          Password <- isolate(input$passwd)
          
          if (length(which(credentials$username_id == Username)) == 1) {
            pw_match  <- credentials["password"][which(credentials$username_id == Username), ]
            pw_verify <- password_verify(pw_match, Password)
            
            if (pw_verify) {
              USER$login <- TRUE
              
            } else {
              shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
              shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
            }
            
          } else {
            shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
            shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
          }
        } 
      }
    }    
  })
  
  output$logoutbtn <- renderUI({
    req(USER$login)
    tags$li(
      a(icon("sign-out-alt"), "Logout", 
        href="javascript:window.location.reload(true)"),
      class = "dropdown", 
      style = "background-color: #eee !important; border: 0; font-weight: bold; margin: 5px; padding: 10px;"
    )
  })
  
  output$sidebarpanel <- renderUI({
    if (USER$login == TRUE ){ 
      if (credentials[, "permission"][which(credentials$username_id == input$userName)] == "advanced") {
        fluidRow(
          column(12, selectInput("select", label = "Select", choices = c(1, 2, 3, 4, 5), selected = 1, multiple = TRUE)),
          column(12, actionButton("button", label = "Button", icon = icon("button"))),
          column(12, downloadButton("download", label = "Download"))
        )
      } else {
        sidebarMenu(
          menuItem("Main Page", tabName = "dashboard", icon = icon("dashboard")),
          menuItem("About Page", tabName = "About", icon = icon("th"))
        )
      }
    }
  })
  
  output$body <- renderUI({
    if (USER$login == TRUE ) {
      if (credentials[, "permission"][which(credentials$username_id == input$userName)] == "advanced") {
        fluidRow(
          textOutput("text")
        )
      } else {
        tabItems(
          tabItem(
            tabName = "dashboard",
            class = "active",
            fluidRow(
              DT::dataTableOutput("table")
            )
          ),
          tabItem(
            tabName = "About",
            h2("This is void. ")
          )
        )
      }
      
    } else {
      loginpage
    }
  })
  
  output$text <- renderText({
    "Text. "
  })
  
  output$table <- DT::renderDataTable({
    datatable(
      data.frame(x = c(1, 2, 3),
                 y = c(2, 3, 4)),
      options = list(
        autowidth = TRUE,
        searching = FALSE
      )
    )
  })
}
