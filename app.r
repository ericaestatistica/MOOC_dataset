## app.R ##
library(shinydashboard)
require(data.table)
#setwd("C:\\Users\\EricaCastilho\\Dropbox\\Recrutamentos\\Github\\MOOC Dataset\\App_Predicao")


load('Dados_novos.Rdata')
niveis_anos<-dados_novos[,.(media_yob=mean(predito)),by=YoB][,YoB]

ui <- dashboardPage(
  
  dashboardHeader(title =span(img(src = "logo_havard.svg", height = 350), "MOOC Havard")
                  ),
  dashboardSidebar( 
    sidebarMenu(
      
      
      
      
      menuItem("Quantitative variables", tabName = "quantitative", icon = icon("th")),
      
      
      
      menuItem("Qualitative variables", tabName = "qualitative", icon = icon("th"))
      
  
      
      
    )   
  ),
  dashboardBody(
    
  tabItems(
    tabItem(tabName = "quantitative",
    # Boxes need to be put in a row (or column)
    fluidRow(
      box(
        title = "Parameters",
        sliderInput("nevents", "Number of events:", 0, 1000, 1000),
        sliderInput("grade", "Grade:", 0, 1, 1000),
        sliderInput("nplay_video", "Number of times playing video:", 0, 1000, 1000),
        sliderInput("ndays_act", "Number of active days:", 0, 1000, 1000),
        sliderInput("nchapters", "Number of chapters:", 0, 1000, 1000),
        sliderInput("nforum_posts", "Number of forum posts:", 0, 1000, 1000)
        
       ),
      valueBoxOutput("probabilidade_nevents"),
      valueBoxOutput("probabilidade_grade"),
      valueBoxOutput("probabilidade_nplay_video"),
      valueBoxOutput("probabilidade_ndays_act"),
      valueBoxOutput("probabilidade_nchapters"),
      valueBoxOutput("probabilidade_nforum_posts")
     
    )
  ),
  tabItem(tabName = "qualitative",
         
            box(       title = "Parameters",
              selectInput('gender','Gender:',choices=c('Female'='f','Male'='m')),
              selectInput('degree','Degree:', choices=levels(dados_novos$LoE_DI)[levels(dados_novos$LoE_DI)!="NA"]),
              selectInput('country','Degree:', choices=levels(dados_novos$final_cc_cname_DI )[levels(dados_novos$final_cc_cname_DI )!="NA"]),
              selectInput('YoB','Year of Birth:', choices=sort(niveis_anos))
              
               ),
        
          valueBoxOutput("probabilidade_gender"),
          valueBoxOutput("probabilidade_degree"),
          valueBoxOutput("probabilidade_country"),
          valueBoxOutput("probabilidade_YoB")
          )
  )
  
))

server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)
  
  output$probabilidade_nevents<- renderValueBox({

    valueBox(
      
      value = round(dados_novos[which.min(abs(nevents-input$nevents)),predito],7)  ,
      subtitle = 'Number of events',
      icon = icon("list"),
      color ="red" 
    )
    
    
  })
  
  
  output$probabilidade_grade<- renderValueBox({
    
    valueBox(
      
      value = round(dados_novos[which.min(abs(grade-input$grade)),predito],7)  ,
      subtitle = 'Grade',
      icon = icon("list"),
      color ="blue" 
    )
    
    
  })
  
  output$probabilidade_nplay_video<- renderValueBox({
    
    valueBox(
      
      value = round(dados_novos[which.min(abs(nplay_video-input$nplay_video)),predito],7)  ,
      subtitle = 'Video',
      icon = icon("list"),
      color ="green" 
    )
    
    
  })
  
  output$probabilidade_ndays_act<- renderValueBox({
    
    valueBox(
      
      value = round(dados_novos[which.min(abs(nplay_video-input$ndays_act)),predito],7)  ,
      subtitle = 'Days active',
      icon = icon("list"),
      color ="yellow" 
    )
    
    
  })
  
  output$probabilidade_ndays_act<- renderValueBox({
    
    valueBox(
      
      value = round(dados_novos[which.min(abs(nplay_video-input$ndays_act)),predito],7)  ,
      subtitle = 'Days active',
      icon = icon("list"),
      color ="orange" 
    )
    
    
  })
  
  output$probabilidade_nchapters<- renderValueBox({
    
    valueBox(
      
      value = round(dados_novos[which.min(abs(nplay_video-input$nchapters)),predito],7)  ,
      subtitle = 'Number of chapters',
      icon = icon("list"),
      color ="navy" 
    )
    
    
  })
  
  output$probabilidade_nforum_posts <- renderValueBox({
    
    valueBox(
      
      value = round(dados_novos[which.min(abs(nplay_video-input$nforum_posts )),predito],7)  ,
      subtitle = 'Number of forum posts',
      icon = icon("list"),
      color ="olive" 
    )
    
    
  })
  output$probabilidade_gender<- renderValueBox({
  
    
    valueBox(
      
      value = round( mean(dados_novos[gender==input$gender,predito]),7)  ,
      subtitle = 'Gender',
      icon = icon("list"),
      color ="red" 
    )
    
    
  })
  
  
  output$probabilidade_degree<- renderValueBox({
    
    
    valueBox(
      
      value = round( mean(dados_novos[LoE_DI==input$degree,predito],na.rm=T),7)  ,
      subtitle = 'Degree',
      icon = icon("list"),
      color ="fuchsia" 
    )
    
    
  })
  
  output$probabilidade_country<- renderValueBox({
    
    
    valueBox(
      
      value = round( mean(dados_novos[final_cc_cname_DI==input$country,predito],na.rm=T),7)  ,
      subtitle = 'Country',
      icon = icon("list"),
      color ="aqua" 
    )
    
    
  })
  
  output$probabilidade_YoB<- renderValueBox({
    
    
    valueBox(
      
      value = round(mean(dados_novos[YoB==input$YoB,predito],na.rm=T),7)  ,
      subtitle = 'Year of Birth',
      icon = icon("list"),
      color ="purple" 
    )
    
    
  })
  
  
}

shinyApp(ui, server)