library(shiny)
library(gganimate)
library(tidyverse)
library(ggplot2)
library(gt)
library(fivethirtyeight)
library(plotly)
library(shinythemes)



data1 <- read_rds("income_jobs.rds")
data2 <- read_rds("religion_social.rds")
data3 <- read_rds("psych_social.rds")
data4 <- read_rds("dep_psych.rds")
data5 <- read_rds("dep_religion.rds")

ui <- fluidPage(
    
    theme = shinytheme("lumen"),
    
    imageOutput("cover_images/image_cross.jpg", width = "100%", height = "100%"), br(),
    
    
    navbarPage(
    "The Sacred and the Profane: The Role of Medicine and Religion on Mental Health",
    tabPanel("Model",
             fluidPage(
                 titlePanel("Employment of Psych + Clergy + Social Workers in MSA"),
                 sidebarLayout(
                     sidebarPanel(
                         selectInput(
                             "plot_type",
                             "Plot Type",
                             c("Option A" = "a", "Option B" = "b")
                         )),
                     sliderInput("income", "Income",
                                 min = 0, max = 150000,  value = 75000))),
                     mainPanel(
                         # Use imageOutput to place the image on the page
                         plotOutput("graph1"),
                         plotOutput("graph2"),
                         plotOutput("graph3"),
                         plotOutput("graph4"),
                         plotOutput("graph5"),
                        )),
                     
                 
 
    
    tabPanel("Discussion",
             titlePanel("Discussion Title"),
             h3("The following project attempts to see if the number of psychiatrists 
           per capita declines in counties where there is an abundant number of 
           clergy per capita. Additionally, the project attempts to look for patterns,
           correlates, and distributions of clergy and of psychiatrists per 
           capita throughout the United States.")),
    
    
    
    tabPanel("About", 
             titlePanel("About"),
             h3("This webapp is the final project for the GOV 1005."),
             p("The following project attempts to see if the number of psychiatrists per capita declines in counties where there is an abundant number of clergy per capita. Additionally, the project attempts to look for patterns, correlates, and distributions of clergy and of psychiatrists per capita throughout the United States. 

Research papers include:

“The clergy as a source of mental health assistance: What Americans Believe” by Christopher G. Ellison

“Patterns and Correlates of Contacting Clergy for Mental Disorders in the United States” by Philip S. Wang & Patricia A Berglund

‘Religiousness and mental health: a review’

“The role of religion and spirituality in mental health”

Key Counties to consider include Jefferson County, Alabama, where religious service attendance is 62.9% of the population, Graham County, Arizona, and Ouachita County Arkansas. 

Key links to look into include:

https://www.usatoday.com/story/news/2018/03/13/most-religious-counties-every-state-u-s/421946002/

https://www.jstor.org/stable/20058132?seq=1

https://www.ncbi.nlm.nih.gov/pmc/articles/PMC1360908/

https://journals.lww.com/co-psychiatry/Abstract/2014/09000/The_role_of_religion_and_spirituality_in_mental.9.aspx"),
             
             h3("About Me"),
             p("Benjamin Villa Wiesner, a student in the Master in Design 
               Engineering program at Harvard University.")),

tabPanel("Video", 
         titlePanel("Video"),
         h3("Coming Soon")
         
)
         )
)

server <- function(input, output, session) {
    output$myImage <- renderImage({
        # A temp file to save the output.
        # This file will be removed later by renderImage
        outfile <- tempfile(fileext = '.png')
        
        # Generate the PNG
        png(outfile, width = 400, height = 300)
        hist(rnorm(input$obs), main = "Generated in renderImage()")
        dev.off()
        
        # Return a list containing the filename
        list(src = "temp_graphs/plot.png",
             contentType = 'image/png',
             width = 1000,
             height = 1000,
             alt = "This is alternate text")
    }, deleteFile = FALSE)
    
    output$graph1 <- renderPlot({
       income_jobs %>%
            filter(income_2018.x<input$income) %>%
            ggplot(aes(jobs_1000_orig.x,jobs_1000_orig.y, color=income_2018.x))+ geom_point() + geom_smooth(method = "lm", se = FALSE)+
            scale_y_log10() + scale_x_log10() +
            labs(x= "Religion Employment per 1000 ", y = "Psych Employmnet per 1000", 
                 title = "Religion vs Psychiatrist Across Counties") +
            theme_classic()
    })
        
        output$graph2 <- renderPlot({
            religion_social %>%
                ggplot(aes(jobs_1000_orig.x,jobs_1000_orig.y,color=tot_emp.x))+ geom_point() + geom_smooth(method = "lm", se = FALSE)+
                scale_y_log10() + scale_x_log10()+
                labs(x= "Religion Employment per 1000 ", y = "Social Workers", 
                     title = "Religion vs Social Workers Across Counties") +
                theme_classic()
        }
        ) 
       
        output$graph3 <- renderPlot({
            psych_social %>%
                ggplot(aes(jobs_1000_orig.x,jobs_1000_orig.y,color=tot_emp.x))+ geom_point() + geom_smooth(method = "lm", se = FALSE)+
                scale_y_log10() + scale_x_log10()+
                labs(x= "Psych Employment per 1000 ", y = "Social Workers", 
                     title = "Psych vs Social Workers Across Counties") +
                theme_classic()
            
        
        }
        ) 
        
        output$graph4 <- renderPlot({
            dep_psych %>%
                ggplot(aes(jobs_1000_orig,yes))+ geom_point() + geom_smooth(method = "lm", se = FALSE)+
                scale_y_log10() + scale_x_log10()+
                labs(x= "Psych Employment per 1000 ", y = "Diagnosed Depression", 
                     title = "Psych vs Depression Across Counties") +
                theme_classic()
            
            
        }
        ) 
        
        output$graph5 <- renderPlot({
            dep_religion %>%
                ggplot(aes(jobs_1000_orig,yes))+ geom_point() + geom_smooth(method = "lm", se = FALSE)+
                scale_y_log10() + scale_x_log10()+
                labs(x= "Religion Employment per 1000 ", y = "Diagnosed Depression", 
                     title = "Religion vs Depression Across Counties") +
                theme_classic()
            
            
        }
        ) 
        
        
}

shinyApp(ui, server)
