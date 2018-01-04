
# shiny app
library(shiny)
library(ggplot2)


dat <- read.csv('../data/cleandata/cleanscores.csv')
dat <- dat[,-1]

dat$Grade <- factor(dat$Grade,levels=c('A+','A','A-',
                                       'B+','B','B-',
                                       'C+','C','C-','D','F'))
f <- table(dat$Grade)
p <- prop.table(f)
a <- data.frame(header=FALSE)
    for (i in 1:11){
      a[i,1] <- names(f[i])
      a[i,2] <- sprintf('%s',f[[i]])
      a[i,3] <- sprintf('%.2f',p[[i]])    
    }
  colnames(a) <- c('Grade','Freq','Prop')
  

ui <- fluidPage(titlePanel('Grade Visualizer',windowTitle = 'F'),
   sidebarLayout(
     
      sidebarPanel(
        conditionalPanel(
          
          condition="input.tabselected==1",
          h3('Grades Distribution'),
          verbatimTextOutput('fp')
        ),
    
       conditionalPanel(
         
         condition="input.tabselected==2",
             selectInput('xcol', 'X-axis variable', names(dat),
                selected=NULL),      
             sliderInput("bin",
                  label = "Bin Width:",
                  min = 1,
                  max = 10,
                 value = 10)
       ),
       
       conditionalPanel(
         condition="input.tabselected==3",
                        
          selectInput('xco', 'X-axis variable', names(dat),
                             selected=NULL),
          selectInput('ycol', 'Y-axis variable', names(dat),
                             selected=NULL),
          sliderInput("op",
                              label = "Opacity:",
                              min = 0,
                              max = 1,
                              value = 0.5),
                   
          radioButtons("l",
                             label = "Show line:",
                             c('none'='NA',
                            'lm'='lm',
                            'loess'='loess'))
       )
                        
     ),
                        
    
      mainPanel( 
        tabsetPanel(
        
               tabPanel("Barchart",fluid=TRUE,
               value=1,
               plotOutput("bar")
               
               ), 
        
               tabPanel("Histogram", fluid=TRUE,
               value=2,
               plotOutput("hist"),
               print("Summary Statistics"),
               verbatimTextOutput("sum")),
        
               tabPanel("Scatterplot", fluid=TRUE,
               value=3, 
               plotOutput("scat"),
               print('Correlation:'),
               verbatimTextOutput("summary")), 
               
               id = "tabselected"
        )
       

    )
)
)
 

          

server <- function(input, output) {
  
    
  output$bar <- renderPlot({
    ggplot(dat,aes(x=dat$Grade))+
      geom_histogram(stat='count',colour='lightblue',fill='lightblue')+
      xlab('Grade')+
      ylab('frequency')+
      scale_y_continuous(breaks=seq(0,60,by=10))+
      theme_bw()
  })

    
  output$hist <- renderPlot({
   ggplot(dat,aes(x=dat[[input$xcol]]))+
      geom_histogram(binwidth=input$bin)+
      scale_x_continuous(breaks=seq(-10,110,by=10))+
      scale_y_continuous(breaks=seq(0,140,by=20))+
      ylab('count')+
      xlab(input$xcol)+
      theme_bw()
      
  })

  
  output$scat <- renderPlot({
    ggplot(dat,aes(x=dat[[input$xco]],y=dat[[input$ycol]]))+
      geom_point(alpha=input$op)+
      geom_smooth(method=input$l)+
      scale_x_continuous(breaks=seq(0,105,by=10))+
      scale_y_continuous(breaks=seq(0,105,by=10))+
      xlab(input$xco)+
      ylab(input$ycol)+
      theme_bw()
    
  })
  
  output$fp <- renderPrint({
    
    print.data.frame(a,right= F,row.names = FALSE)
  })
  output$summary <- renderText({
   cor(dat[[input$xco]],dat[[input$ycol]])
  })
  
  output$sum <- renderPrint({
   sum <- summary_stats(dat[[input$xcol]]) 
   a <- data.frame(header=FALSE)
   
     for (i in 1:length(sum)){
     a[i,1] <- names(sum[i])
     a[i,2] <- c(':')
     a[i,3] <- sprintf('%.4f',sum[[i]])    
   }
    
   names(a) <- NULL
   print.data.frame(a,right=F,row.names = FALSE)
  })
  
}



# Run the application 
shinyApp(ui = ui, server = server)













