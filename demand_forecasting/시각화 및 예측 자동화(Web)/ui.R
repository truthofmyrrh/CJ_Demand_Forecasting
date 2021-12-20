library(shiny)
library(shinythemes)
library(dplyr)
library(shinyjs)
library(fresh)

library(tidyverse) 
library(showtext)


font_add_google("Jua", "Jua") 
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
  h4("안녕하세요. 미디어프로젝트 28조 박예리, 유시은입니다."),
  h4("이 페이지는 '고객사별 상품 수요 예측 자동화 프로세스'를 위해 제작되었습니다."),
  sidebarLayout(
    sidebarPanel(
      fileInput(inputId = "file",label =  "파일 데이터 선택하기(csv,xlsx,txt 등)", buttonLabel = "내PC 검색",placeholder = "선택된 파일 없음"),
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
      sliderInput('bins', "축 간격:",min=1,max=50,value=20),
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