
ui<-dashboardPage(
  dashboardHeader(title = "Login Page", uiOutput("logoutbtn")),
  
  dashboardSidebar(uiOutput("sidebarpanel")),
  
  dashboardBody(shinyjs::useShinyjs(), uiOutput("body")),
  
  skin = "blue"
)
