options(rgl.useNULL=TRUE)
   
library(shiny)
library(shinydashboard)
library(scales)
library(shinyRGL)

#library(MASS)
#data(beav1)

dat <- read.csv("www/TNC_BommerCanyon_data.csv")
Control <- subset( dat, dat[,2] == "0" & dat[,3] == "0")
Tarweed_protection <- subset( dat, dat[,2] == "1" & dat[,3] == "0")
Mulch_protection <- subset( dat, dat[,2] == "0" & dat[,3] == "1")
Tarweed_and_mulch <- subset( dat, dat[,2] == "1" & dat[,3] == "1")


shinyUI(dashboardPage(
  dashboardHeader(title = "FARM project"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "introduction", icon = icon("cogs")),
      menuItem("Model design", tabName = "model_design", icon = icon("cogs")),
      menuItem("Methods", tabName = "methods", icon = icon("cogs")),
      menuItem("Results", tabName = "results_page", icon = icon("area-chart")),
      menuItem("Conclusions", tabName = "conclusions", icon = icon("area-chart")),
      menuItem("Citations", tabName = "citations", icon = icon("cogs"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "introduction",
              fluidRow(
                imageOutput("walking_water", height = 450, width = 400)
              ),
              
              tags$h1("HOW DO IDEAS AND TECHNOLOGY SPREAD?"),
              tags$h4(p("Created by", span(tags$a(href = "http://www.colorado.edu/eeb/tuff/Tuff_et_al./Ty_Tuff.html", "Ty Tuff, Bruno Vilela, and Carlos Botero at WUSTL.edu", style = "color:black"))  ), align="center"),
              ### tags$iframe(id="iframe1", src="http://player.vimeo.com/video/21172499?"),
              
              p("Cultural traits are known to spread through human societies using a number of different social, environmental, and evolutionary transmission mechanisms, however, no methods currently exist to link specific mechanisms to the spread of specific traits across broad spatial scales or deep historical time. To overcome this limitation, we developed a method for disentangling the relative contribution of two specific mechanisms of cultural information transfer, horizontal and vertical transmission.  Ideas and technologies that are horizontally transmitted are shared between societies, while ideas that are vertically transmitted are inherited from parents passing them to their offspring and only expand when populations displace neighboring societies. We describe the relative contributions each of these mechanisms make to the spread of ideas using both spatial and linguistic phylogeny data and competing those data against a series of simulations describing the possible outcomes under each transmission mode. When applied to the trait of agriculture, our results show a dominance of horizontal transmission with little contribution from the alternative. These results indicate that the idea of domesticating plants and animals was spread primarily through sharing between neighbors rather than the expansion of societies that became powerful through the development of agriculture.")
              
              
                
              ),
       
      tabItem(tabName = "model_design",    
               
              fluidRow(
                imageOutput("cow_race", height = 450, width = 400)
              ),
              tags$h3("Model design and engineering"),
              p("Enter text here describing the algorithm and add the schematic from early days"),
              br()
              
      ),
      # First tab content
      tabItem(tabName = "methods",
              fluidRow(
                imageOutput("man_pulling_cow", height = 500, width = 400)
              ),
              tags$h3("Using the model to answer scientific questions"), 
            p("Describe the process of using the model to answer questions about culture. Describe our choice of parameter values and describe our choice to implement the model in four combinations and compete those models against eachother. ")
              
       ),
      
   

        tabItem(tabName = "results_page",
            tabsetPanel(
                tabPanel("Results summary", 
                         
                         fluidRow(
                           imageOutput("mobile_data_visualization", height = 390, width = 400)
                         ),
                         
               			 tags$h3("Making scientific inferences by statistically comparing data"),

               			 p("Describe the analyses we produced and how those analyses compare to eachother
               			   ")
	

		),
                tabPanel("Maps vs. Trees", 
                         fluidRow(
                           imageOutput("measure_means", height = 375, width = 400)
                         ),
                         
                         p("This page will include a video of the maps and trees through time and a reactive element to let ppl toggle through simulations and timesteps themselves. 
                           ")
                   
                         
                               
                ), 
                tabPanel("Traits", 
                         
                         fluidRow(
                           imageOutput("anova_cartoon", height = 400, width = 400)
                         ),
                         
                         
                         tags$h3("How do traits compare to eachother?"),
                         
                         p("Show the binary division of each trait and the analysis that follows for each trait")
                         
                ), 



                tabPanel("Tree statistics through time", 
                         
                         fluidRow(
                           imageOutput("gm_regression", height = 400, width = 400)
                         ),
                         tags$h3("This is how we analyze trees"),
                         p("Show gamma et al. through time")
                )
                )   
      ),
        tabItem(tabName = "conclusions",
        
                fluidRow(
                  imageOutput("Big_Data_Traffic", height = 410, width = 400)
                ),
                
                mainPanel(
                  webGLOutput("myWebGL")
                ),
                
       			tags$h3("What conclusions have we gleaned from this?"),
       			p("This is were we need to start digesting our results
       			  ")
       			),
        tabItem(tabName = "citations",
                fluidRow(
                  imageOutput("graph_in_context", height = 310, width = 400)
                )  ,
                tags$h3("These are the citations we're currently using for this project"),
                p("Start building citations list here")
                
        )
      )
  ) 
)









  )