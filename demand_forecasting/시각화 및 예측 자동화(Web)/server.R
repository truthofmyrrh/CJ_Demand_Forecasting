library(shiny)
library(shinythemes)
library(dplyr)
library(shinyjs)
library(fresh)

library(ggplot2)
require(methods)
library(RColorBrewer)

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
    if(is.null(data())){
      h4(paste("데이터가 존재하지 않습니다.","\n","왼쪽 사이드바에서 주문 데이터를 불러와 주세요. :)"), style=" color:grey")
    }
    else
      tabsetPanel(type = "tabs",
                  tabPanel("Summary",
                           tableOutput("table"),
                           verbatimTextOutput("summary")
                  ),
                  tabPanel("Visualization",
                           h4("STEP1. 데이터 탐색"),
                           h5("- 상품 주문량(item_qty) 분포"),
                           plotOutput("histogram"),
                           plotOutput("piechart"),
                           h4("STEP2. 인기상품 선정: 최근 3개월 간 가장 많이 팔린 상품 TOP10"),
                           tableOutput("items"),
                           h4("STEP3. 인기상품 주문 패턴 파악"),
                           plotOutput("linegraph")
                           
                  ),
                  tabPanel("Analysis",
                           h4("변수 간 상관관계 분석"),
                           img(src="corr.png",height=600,width=600),
                           h4("모델별 학습 결과"),
                           img(src="models.png",height=500,width=800),
                           h5("< MLP >", style=" color:red"),
                           h5("==================================="),
                           h5("Train rmse: 1.2982780765289341"),
                           h5("Validation rmse: 1.7146908649661337", style=" color:red"),
                           br(),
                           h5("< CNN >"),
                           h5("==================================="),
                           h5("Train rmse: 1.326183962490449"),
                           h5("Validation rmse: 1.7535316249128579"),
                           br(),
                           h5("< LSTM >"),
                           h5("==================================="),
                           h5("Train rmse: 1.4041929906164117"),
                           h5("Validation rmse: 1.7535316249128579"),
                           br(),
                           h5("< CNN-LSTM >"),
                           h5("==================================="),
                           h5("Train rmse: 1.2729274476673684"),
                           h5("Validation rmse: 1.9009386292215869"),
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
    x <- data()[,input$item_qty]
    hist(x,breaks = input$bins, col= input$color,border='white')
  })
  
  output$piechart <- renderPlot({
    d = data()
    sel_data = d %>% filter(d$SHPR_CD %in% input$shpr)
    
    sel_data$ITEM_QTY = ifelse(sel_data$ITEM_QTY>=2, 2, 1)
    d<-sel_data %>% group_by(ITEM_QTY) %>% summarise(n=n()) %>% filter(!is.na(ITEM_QTY))
    d$ITEM_QTY <- factor(d$ITEM_QTY)
    col <- ifelse(input$color=="blue", "Blues", ifelse(input$color=="red", "Reds", "Greys"))
    p<-ggplot(d,aes(x="",y=n,fill=ITEM_QTY))+
      geom_bar(width = 1, stat="identity")+
      coord_polar("y",start=0)+
      geom_text(aes(label=paste0(round(n/sum(n)*100,1),"%")),
                position = position_stack(vjust = 0.5)) +
      scale_fill_brewer(palette = col, direction = -1)
    
    print(p)
    
  })
  output$items <- renderTable({
    d = data()
    g_data = d %>% filter(d$SHPR_CD %in% input$shpr)%>% 
      group_by(ITEM_CD)  %>% 
      summarise(Total_Quantity = sum(ITEM_QTY))
    if(nrow(g_data)<10){return(print("데이터가 없습니다."))}
    head(g_data[order(-g_data$Total_Quantity), ],10)
  })
  output$linegraph <- renderPlot({
    d = data()
    sel_data = d %>% filter(d$SHPR_CD %in% input$shpr)
    
    g_data = sel_data %>% 
      group_by(ITEM_CD)  %>% 
      summarise(Total_Quantity = sum(ITEM_QTY))
    if(nrow(g_data)<10){return(print("데이터가 없습니다."))}
    top10 = head(g_data[order(-g_data$Total_Quantity), ],10)['ITEM_CD'] %>% dplyr::pull(1)
    
    a = sel_data %>% filter(sel_data$ITEM_CD %in% top10)%>%
      group_by(날짜,ITEM_CD)%>% 
      summarise(Total_Quantity = sum(ITEM_QTY))
    
    
    ggplot(a, aes(x=날짜, y=Total_Quantity, group=ITEM_CD,col=ITEM_CD)) + 
      geom_line() + 
      ylim(0, max(a$날짜)) 
  })
  
  
  
  output$download <- downloadHandler(
    filename = function(){paste("histogram",input$item_qty,".pdf",sep="")},
    content = function(file){
      pdf(file)
      # p<-ggplot(data(),aes(x,y)) + geom_point()
      # print(p)
      x<-data()[,input$item_qty]
      hist(x,breaks=input$bins,col=input$color,border='white',ylim=30)
      dev.off()
    }
  )
}