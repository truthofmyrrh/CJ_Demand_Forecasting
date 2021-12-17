library(shiny)
library(shinythemes)
library(dplyr)
library(shinyjs)
library(fresh)

library(ggplot2)
require(methods)

jsResetCode <- "shinyjs.reset = function() {history.go(0)}"

ui <- fluidPage(
  use_googlefont("Jua"),
  use_theme(create_theme(
    theme = "simplex",
    bs_vars_font(
      family_sans_serif = "'Jua'"
    )
  )),

  titlePanel(span(strong("수요 예측 시각화"), style=" color:red"),windowTitle = "Demand forecasting"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput(inputId = "file",label =  "파일 데이터 선택하기(csv)", buttonLabel = "내PC 검색",placeholder = "선택된 파일 없음"),
      checkboxInput(inputId = 'header', label = "header 존재", value = TRUE),
      radioButtons('sep',"Separator",c(Comma=',',Semicolen=';',Tab='\t'),','),
      selectInput('item_qty',label=h4("상품 수량"), choices = NULL),
      selectInput('item_cd',label=h4("상품 종류(열 이름 선택)"), choices = NULL),
      selectInput('shpr_cd',label=h4("고객사(열 이름 선택)"), choices = NULL),
      selectInput('date',label=h4("기준 날짜(열 이름 선택)"), choices = NULL),
      br(),
      hr(),
      sliderInput('coln', "미리보기 행 개수:",min=1,max=30,value=5),
      selectInput('shpr',label=h4("고객사 선택"), choices = NULL),
      sliderInput('bins', "축 간격:",min=1,max=50,value=30),
      radioButtons('color','그래프 색상',choices = c("blue","red","grey"),'blue'),
      downloadButton(outputId = "download",label = "시각화 저장(pdf)"),
      hr(),
      
      actionButton("git","", icon = icon("fab fa-github","fa-x"),color = "success"),
      uiOutput("ui_open_tab"),
      useShinyjs(),                                           
      extendShinyjs(text = jsResetCode, functions = "reset"), 
      actionButton("reset_button","", icon = icon("fas fa-redo-alt","fa-x"))
    ),
      
    mainPanel(
      uiOutput("tb")
    )
  )
)

server <- function(input, output,session) {
  # 새로고침 버튼
  observeEvent(input$reset_button, {js$reset()})
  
  # 소스코드 링크 이동 버튼
  output$ui_open_tab <- renderUI({
    req(input$git > 0)
    tags$script(paste0("window.open('https://github.com/truthofmyrrh/CJ_Demand_Forecasting', '_blank')"))
  })
  
  # 데이터 저장
  data <- reactive({
    file1 <- input$file
    if(is.null(file1)){return()}
    dataset <- read.csv(file = file1$datapath, sep = input$sep, header = input$header)
    
    updateSelectInput(session, "item_qty", choices = colnames(dataset))
    updateSelectInput(session, "item_cd", choices = colnames(dataset))
    updateSelectInput(session, "shpr_cd", choices = colnames(dataset))
    updateSelectInput(session, "date", choices = colnames(dataset))
    
    dataset$SHPR_CD <- factor(dataset$SHPR_CD)
    updateSelectInput(session, "shpr", choices = levels(dataset$SHPR_CD))
    
    dataset
  })
  
  # 결과 화면
  output$tb <- renderUI({
    if(is.null(data()))
      h5("데이터가 존재하지 않습니다.")
    else
      tabsetPanel(type = "tabs",
                  tabPanel("Summary",
                           tableOutput("table"),
                           verbatimTextOutput("summary")
                           ),
                  tabPanel("Visualization",
                           plotOutput("histogram")
                           ),
                  tabPanel("Analysis",
                           h4("모델별 학습 결과"),
                           img(src="models.png",height=500,width=800)
                           )
                  
      )
  })
  
  ## summery tab
  output$table <- renderTable({
    if(is.null(data())){return(print("데이터가 없습니다."))}
    d = data()
    sel_data = d %>% filter(d$SHPR_CD %in% input$shpr)
    head(sel_data,input$coln)
  })
  output$summary <- renderPrint({
    d = data()
    sel_data = d %>% filter(d$SHPR_CD %in% input$shpr)
    summary(sel_data)
  })
  
  ## visualization tab
  output$histogram <- renderPlot({
    d = data()
    sel_data = d %>% filter(d$SHPR_CD %in% input$shpr)
    
  })
  
  
  
  output$download <- downloadHandler(
    filename = function(){paste("histogram",input$item_qty,".pdf",sep="")},
    content = function(file){
      pdf(file)
      # p<-ggplot(data(),aes(x,y)) + geom_point()
      # print(p)
      x<-data()[,input$item_qty]
      hist(x,breaks=input$bins,col=input$color,border='white')
      dev.off()
    }
  )
}

shinyApp(ui, server)

