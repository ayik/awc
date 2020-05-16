suppressMessages(library(tidyverse))
suppressMessages(library(shiny))
suppressMessages(library(shinyWidgets))
suppressMessages(library(tm))
suppressMessages(library(wordcloud))
suppressMessages(library(ggthemes))
suppressMessages(library(RColorBrewer))
suppressMessages(library(lubridate))




ui <- navbarPage("Whastapp Vizualizer",
                 tabPanel("Import",
                          tags$a(href="https://awc.netlify.app/", tags$h1("Ayik Apps")) ,
                          sidebarPanel(fileInput("file", "Unggah File WA",
                                                 multiple = TRUE,
                                                 accept = c(".txt",
                                                            ".zip")),
                                       "Riwayat chat dapat di ekspor dari opsi Chat WA di HP anda. File hasil ekspor berupa format txt.
                                       File yang diunggah tidak disimpan diserver.",br(),br(),
                                       "Lanjutkan klik tab Analisis setelah menggunggah file.")),
                 
                 tabPanel("Analyze",conditionalPanel(condition = "output.Choose_Users", 
                          titlePanel("Analisis Whastapp Chat"),
                          sidebarPanel(uiOutput("Choose_Users"),"pilih obyek untuk dianalisis"),
                          titlePanel(""),
                          mainPanel(tabsetPanel(type = "tabs",
                                                tabPanel("Wordcloud",h1("Word Cloud dari kata yang digunakan saat chat"), plotOutput("plot")),
                                                tabPanel("When - Distribusi sepanjang hari", plotOutput("overDayPlot"),br(),plotOutput("overHourPlot"),div("Terakhir jam 4 sore", align = "center")),
                                                tabPanel("What - panjang dan jumlah chat",htmlOutput("lengthtext")),
                                                tabPanel("Table", dataTableOutput("table"))
                                                )
                                    )
                          )
                 ))


server <- function(input, output, session) {
  whatsapp_chat <- eventReactive(input$file,{
    rwhatsapp::rwa_read(input$file$datapath)})
  output$Choose_Users <- renderUI(
    pickerInput("users","select users",choices = whatsapp_chat() %>% distinct(author) %>% drop_na() %>% pull() %>% as.character(),
                multiple = TRUE, options = list(`actions-box` = TRUE)))
  source("cloud_prep.R")
  avglen <- reactive({lengthprep(whatsapp_chat(),input$users)})
  output$data <- renderText(input$variable)
  output$plot <- renderPlot({
    df <- cloud_prep(whatsapp_chat())
    wordcloud(words = df$word,
              freq = df$freq,
              min.freq = 2,
              scale=c(4.5,.25),
              max.words=200,
              random.order=FALSE,
              rot.per=0.35,
              colors=brewer.pal(8, "Dark2")
              )
    })
  
  output$lengthtext <- renderUI({
    emg <- avglen()
    HTML(emg)
  })
  
  output$lengthPlot <- renderPlot({
    whatsapp_chat() %>% drop_na() %>% filter(author %in% input$users) %>% 
    ggplot(., aes(x = hour(time), fill = author)) + 
      stat_count(position = "stack", show.legend = TRUE) + 
      ggtitle("Chat per Jam") + ylab("# of messages") + 
      xlab("time") + 
      theme(plot.title = element_text(face = "italic"))+ 
      scale_x_continuous(breaks=seq(0,23,1))
  })
  
  output$overDayPlot <- renderPlot({
    whatsapp_chat() %>% drop_na() %>% filter(author %in% input$users) %>% 
    ggplot(., aes(x = mday(time), fill = author)) + 
      stat_count(position = "stack", show.legend = TRUE) + 
      ggtitle("Rata2 Chat per Hari") + ylab("# of messages") + 
      xlab("Date") + 
      theme(plot.title = element_text(face = "italic"))+ 
      scale_x_continuous(breaks=seq(0,31,1))
  })
  
  output$overHourPlot <- renderPlot({
    box_data <- whatsapp_chat() %>% drop_na() %>% filter(author %in% input$users) %>% mutate(hour=hour(time)) %>% mutate(hour = ifelse(hour<4,hour+24,hour)) %>% group_by(author,hour) %>% tally()
    ggplot(box_data)+ 
      geom_boxplot(aes(x=author,y=hour,fill=author)) + 
      geom_point(aes(x=author,y=hour,size=n),color="grey40") +
      ylab("Jam per Hari") +scale_y_continuous(breaks=seq(0,28,1))+ 
      xlab("Obyek")+theme(axis.text.x = element_text(angle = 45))
  })
  
  output$table <- renderDataTable(whatsapp_chat() %>% drop_na() %>% filter(author %in% input$users) %>% select(-source) %>% mutate(time = as.character(time)))
  outputOptions(output, "Choose_Users", suspendWhenHidden = FALSE)
}

shinyApp(ui, server)