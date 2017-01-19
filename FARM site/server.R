options(rgl.useNULL=TRUE)

library(shiny)
library(shinydashboard)
library(scales)
library(shinyRGL)
library(rgl)


# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
  output$myWebGL <- renderWebGL({
   
    points3d(1:10, 1:10, 1:10, col="red")
    axes3d()
  })
  
  
  
 
  #### Photo loads
  
  
  output$man_pulling_cow <- renderImage({
    list(
      src = "www/JorgeBacelar_small.png",
      contentType = "image/png",
      alt = "Face"
    )}, deleteFile = FALSE)
  
  
    output$cow_race <- renderImage({
      list(
          src = "www/LiduanChen_small.png",
          contentType = "image/png",
          alt = "Face"
        )}, deleteFile = FALSE)
    
    output$walking_water <- renderImage({
      list(
        src = "www/KMAsad_small.png",
        contentType = "image/png",
        alt = "Face"
      )}, deleteFile = FALSE)

    output$brommer_panarama <- renderImage({
      list(
        src = "www/brommer panarama.png",
        contentType = "image/png",
        alt = "Face"
      )}, deleteFile = FALSE)   
    
    output$brommer_panarama_green <- renderImage({
      list(
        src = "www/brommer panarama green.png",
        contentType = "image/png",
        alt = "Face"
      )}, deleteFile = FALSE)  
    
    
    output$brommer_big_sign <- renderImage({
      list(
        src = "www/brommer_big_sign.png",
        contentType = "image/png",
        alt = "Face"
      )}, deleteFile = FALSE)  
    
    
     
    
    
    output$data_pipeline <- renderImage({
      list(
        src = "www/data_pipeline.png",
        contentType = "image/png",
        alt = "Face"
      )}, deleteFile = FALSE) 
    
    
    
    
    
    output$call_this_number <- renderImage({
      list(
        src = "www/call_this_number.png",
        contentType = "image/png",
        alt = "Face"
      )}, deleteFile = FALSE) 
    
    
    output$looking_hard_at_data <- renderImage({
      list(
        src = "www/looking_hard_at_data.png",
        contentType = "image/png",
        alt = "Face"
      )}, deleteFile = FALSE)
    
    output$binary_data_tweezers <- renderImage({
      list(
        src = "www/binary_data_tweezers.png",
        contentType = "image/png",
        alt = "Face"
      )}, deleteFile = FALSE)
    
    
    output$Big_Data_Traffic <- renderImage({
      list(
        src = "www/Big_Data_Traffic.png",
        contentType = "image/png",
        alt = "Face"
      )}, deleteFile = FALSE)
    
    
    output$mobile_data_visualization <- renderImage({
      list(
        src = "www/mobile_data_visualization.png",
        contentType = "image/png",
        alt = "Face"
      )}, deleteFile = FALSE)
    
    
    
    output$anova_cartoon <- renderImage({
      list(
        src = "www/anova_cartoon.png",
        contentType = "image/png",
        alt = "Face"
      )}, deleteFile = FALSE)
    
    
    output$measure_means <- renderImage({
      list(
        src = "www/measure_means.png",
        contentType = "image/png",
        alt = "Face"
      )}, deleteFile = FALSE)
    
    
    output$gm_regression <- renderImage({
      list(
        src = "www/gm_regression.png",
        contentType = "image/png",
        alt = "Face"
      )}, deleteFile = FALSE)
    
    
    output$d_linear_regression <- renderImage({
      list(
        src = "www/d_linear_regression.png",
        contentType = "image/png",
        alt = "Face"
      )}, deleteFile = FALSE)
    
    
    output$graph_in_context <- renderImage({
      list(
        src = "www/graph_in_context.png",
        contentType = "image/png",
        alt = "Face"
      )}, deleteFile = FALSE)
    
    
    
    output$graph_display <- renderImage({
      list(
        src = "www/graph_display copy.png",
        contentType = "image/png",
        alt = "Face"
      )}, deleteFile = FALSE)
    
    
    
    output$Tarweed_and_mulch <- renderImage({
      list(
        src = "www/Tarweed_and_mulch.png",
        contentType = "image/png",
        alt = "Face"
      )}, deleteFile = FALSE)
    
    
    output$bommer_canyon_trail <- renderImage({
      list(
        src = "www/bommer_canyon_trail.png",
        contentType = "image/png",
        alt = "Face"
      )}, deleteFile = FALSE)
    
    output$Bommer_canyon_sign <- renderImage({
      list(
        src = "www/Bommer_canyon_sign.png",
        contentType = "image/png",
        alt = "Face"
      )}, deleteFile = FALSE)
    
    
    output$R_topper <- renderImage({
      list(
        src = "www/R_topper.png",
        contentType = "image/png",
        alt = "Face"
      )}, deleteFile = FALSE)
    
    
    
    output$summary_plot_r <- renderPlot({
      plot(0,0,col="red")
      
    })
    
    
      #output$ttest_console_rich <- renderPrint({
      #  capture.output(t.test(x_Tarweed , x_Mulch))
      #})
      
     
    #output$anova_code_load <- renderText({
    #  "data(beaver1)"
    #})
    
    

    
    
    
})
    

    



#####

