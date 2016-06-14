#' A Histogram Charting Function
#' 
#' nHIst takes advantage of Shiny Gadgets to make charting basic histograms quick and easy for Birchbox DSAI.
#' The only required argument is your data frame. The function will chart an interactive histogram that will let you choose which column you want to chart along with the top 'N' you would like to see in the histogram.
#' 
#' There is one optional argument to decide how you would like to view the chart. The default is the rstudio pane. The other options include a pop up dialog or browser.
#' 
#' To close the app, please click "DONE" in the upper right-hand corner!
#' 
#' @param df Your data frame. It is required. Note: You can chain from dplyr into the chart!
#' @param vwr An optional argument. Defaults to 'pane'. Other options include 'dialog' and 'browser'
#' 
#' @return An interactive histogram chart
#' 
#' @author Matt Negrin
#' 
#' @export

nHist <- function(df,vwr='pane') {
  
  if(!base::require(ggplot2)){
    utils::install.packages("ggplot2")
    base::library(ggplot2)
  }
  
  if(!base::require(shiny)){
    utils::install.packages("shiny")
    base::library(shiny)
  }
  
  if(!base::require(miniUI)){
    utils::install.packages("miniUI")
    base::library(miniUI)
  }
  
  if(!base::require(dplyr)){
    utils::install.packages("dplyr")
    base::library(dplyr)
  }
  
  if(!base::require(stats)){
    utils::install.packages("stats")
    base::library(stats)
  }
  
  if(!base::require(utils)){
    utils::install.packages("utils")
    base::library(utils)
  }
  
  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("rbox - Top N Histogram"),
    miniUI::miniContentPanel(
      shiny::plotOutput('histogram_chart'),
      shiny::fluidRow(
        shiny::column(width=3, offset=1,
               shiny::selectInput('x_choice',label='X Variable',choices=unique(names(df)))),
        shiny::column(width=3, offset=0,
               shiny::sliderInput('top_n_slider',label='Top N',min=1,max=30,value=15))
      )
    )
  )
  
  server <- function(input, output, session) {
    data <- shiny::reactive({
      x_spot <- which(names(df)==input$x_choice)
      dplyr::select(df,x_set=x_spot)
    })
    
    output$histogram_chart <- shiny::renderPlot({
      data() %>% 
        dplyr::count(x_set) %>% 
        dplyr::arrange(dplyr::desc(n)) %>%  
        utils::head(input$top_n_slider) %>% 
        ggplot2::ggplot(aes(x = stats::reorder(x_set, n), y = n)) +
        ggplot2::geom_bar(stat = "identity", width = 0.8) +
        ggplot2::theme(axis.title.y = ggplot2::element_blank()) +
        ggplot2::ggtitle(paste0("Counts of ",input$x_choice)) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust=1))
    })
    
    shiny::observeEvent(input$done, {
      shiny::stopApp()
    })
    
    shiny::observeEvent(input$cancel, {
      shiny::stopApp(NULL)
    })
  }
  
  ifelse(vwr=='pane',
         viewer<-shiny::paneViewer(minHeight=NULL),
         ifelse(vwr=='dialog',
                viewer<-shiny::dialogViewer('Top N Histogram',width=1000,height=1000),
                ifelse(vwr=='browser',viewer<-shiny::browserViewer(browser = getOption("browser"))
                )
         )
  )
  
  shiny::runGadget(ui, server, viewer = viewer,stopOnCancel = FALSE)
  
}