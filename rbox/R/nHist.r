#' A Histogram Charting Function
#' 
#' This function is supposed to make charting basic histograms really quick for Birchbox DSAI-ers.
#' The only required argument is your data frame. The function will chart an interactive histogram that will let you choose which column you want to chart along with the top 'N' you would like to see in the histogram.
#' There is one optional argument to decide how you would like to view the chart. The default is the rstudio pane. The other options include a pop up dialog or browser.
#' @param df This is your data frame. It is required. Note: You can chain from dplyr into the chart!
#' @param vwr This is an optional argument. Defaults to 'pane'. Other options include 'dialog' and 'browser'
#' @keywords charting histogram hist rbox topn
#' @examples nHist(dataframe)
#' @examples nHist(dataframe,vwr='dialog')
#' @examples nHist(dataframe,vwr='browser')
#' @import ggplot2
#' @import shiny
#' @import miniUI
#' @import dplyr
#' nHist()

nHist <- function(df,vwr='pane') {
  
  if(!require(ggplot2)){
    install.packages("ggplot2")
    library(ggplot2)
  }
  
  if(!require(shiny)){
    install.packages("shiny")
    library(shiny)
  }
  
  if(!require(miniUI)){
    install.packages("miniUI")
    library(miniUI)
  }
  
  if(!require(dplyr)){
    install.packages("dplyr")
    library(dplyr)
  }
  
  ui <- miniPage(
    gadgetTitleBar("rbox - Top N Histogram"),
    miniContentPanel(
      plotOutput('histogram_chart'),
      fluidRow(
        column(width=3, offset=1,
               selectInput('x_choice',label='X Variable',choices=unique(names(df)))),
        column(width=3, offset=0,
               sliderInput('top_n_slider',label='Top N',min=1,max=30,value=15))
      )
    )
  )
  
  server <- function(input, output, session) {
    data <- reactive({
      x_spot <- which(names(df)==input$x_choice)
      fill_spot <- which(names(df)==input$fill_choice)
      df %>%
        select(
          x_set=x_spot
        )
    })
    
    output$histogram_chart <- renderPlot({
      data() %>% 
        count(x_set) %>% 
        arrange(desc(n)) %>%  
        head(input$top_n_slider) %>% 
        ggplot(aes(x = reorder(x_set, n), y = n)) +
        geom_bar(stat = "identity", width = 0.8) +
        theme(axis.title.y = element_blank()) +
        ggtitle(input$x_choice) +
        ylab(paste0("Number of ",input$x_choce)) + 
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
    })
    
    observeEvent(input$done, {
      stopApp()
    })
    
    observeEvent(input$cancel, {
      stopApp(NULL)
    })
  }
  
  ifelse(vwr=='pane',
         viewer<-paneViewer(minHeight=NULL),
         ifelse(vwr=='dialog',
                viewer<-dialogViewer('Top N Histogram',width=1000,height=1000),
                ifelse(vwr=='browser',viewer<-browserViewer(browser = getOption("browser"))
                )
         )
  )
  
  runGadget(ui, server, viewer = viewer,stopOnCancel = FALSE)
  
}