options(rgl.useNULL=TRUE)

library(shiny)
library(shinydashboard)
library(scales)
library(shinyRGL)
library(rgl)

#library(MASS)
#data(beav1)

dat <- read.csv("www/TNC_BommerCanyon_data.csv", as.is = TRUE)
Control <- subset( dat, dat[,2] == "0" & dat[,3] == "0")
Controls <- subset( dat, dat[,2] == "0" & dat[,3] == "0")
Tarweed_protection <- subset( dat, dat[,2] == "1" & dat[,3] == "0")
Mulch_protection <- subset( dat, dat[,2] == "0" & dat[,3] == "1")
Tarweed_and_mulch <- subset( dat, dat[,2] == "1" & dat[,3] == "1")

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
  output$myWebGL <- renderWebGL({
   
    points3d(1:10, 1:10, 1:10, col="red")
    axes3d()
  })
  
  
  
 
  
  
  
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
    
    
    
    output$prelim_graph <- renderPlot({
      
    
      ####  Mine #########################
      #########################################
      
      # General definition: Apply methods from statistics or data mining as a way to discern patterns or place the data in a mathematical context. This means producing a few numbers that represent the full data set. These are called summary statistics and they can be calculated in a variety of ways depending on how you've structured your research question, the experimental design you used to test your question, the type of data you produced, and the scale of data you produced. 
      
      
     
      ### Let's start by looking at the data in graphical form:
      par(mfrow=c(2,5), mar=c(2,2,3,2)) 
      plot(0,0,type="n", xaxt="n",yaxt="n")
      mtext("Percent ground cover", 4, line=-5)
      # summary plots
      plot(c(0,3) , c(0,1), type="n", xaxt="n",yaxt="n")
      boxplot(Control$nativeCover , Control$exoticCover, main="Control", ylim=c(0,1), names=c("Native", "Exotic"), add=TRUE, at=c(1,2))
      points(rep(1,20) , Control$nativeCover, col="blue")
      points(rep(2,20), Control$exoticCover, col="blue")
      
      plot(c(0,3) , c(0,1), type="n", xaxt="n",yaxt="n")
      boxplot(Tarweed_protection$nativeCover , Tarweed_protection$exoticCover, main="Tarweed_protection", ylim=c(0,1), names=c("Native", "Exotic"), add=TRUE, at=c(1,2))
      points(rep(1,20) , Tarweed_protection $nativeCover, col="blue")
      points(rep(2,20), Tarweed_protection $exoticCover, col="blue")
      
      plot(c(0,3) , c(0,1), type="n", xaxt="n",yaxt="n")
      boxplot(Mulch_protection$nativeCover , Mulch_protection$exoticCover, main="Mulch_protection", ylim=c(0,1), names=c("Native", "Exotic"), add=TRUE, at=c(1,2))
      points(rep(1,20) , Mulch_protection $nativeCover, col="blue")
      points(rep(2,20), Mulch_protection $exoticCover, col="blue")
      
      plot(c(0,3) , c(0,1), type="n", xaxt="n",yaxt="n")
      boxplot(Tarweed_and_mulch$nativeCover , Tarweed_and_mulch$exoticCover, main="Tarweed_and_mulch", ylim=c(0,1), names=c("Native", "Exotic"), add=TRUE, at=c(1,2))
      points(rep(1,20) , Tarweed_and_mulch $nativeCover, col="blue")
      points(rep(2,20), Tarweed_and_mulch $exoticCover, col="blue")
      
      plot(0,0,type="n", xaxt="n",yaxt="n")
      mtext("Species Richness", 4, line=-5)
      
      plot(c(0,3) , c(0,16), type="n", xaxt="n",yaxt="n")
      boxplot(Control$nativeRich , Control$exoticRich, ylim=c(0,18), names=c("Native", "Exotic"), add=TRUE, at=c(1,2))
      points(rep(1,20) , Control$nativeRich, col="blue")
      points(rep(2,20), Control$exoticRich, col="blue")
      
      plot(c(0,3) , c(0,16), type="n", xaxt="n",yaxt="n")
      boxplot(Tarweed_protection$nativeRich , Tarweed_protection$exoticRich, ylim=c(0,18), names=c("Native", "Exotic"), add=TRUE, at=c(1,2))
      points(rep(1,20) , Tarweed_protection$nativeRich, col="blue")
      points(rep(2,20), Tarweed_protection$exoticRich, col="blue")
      
      plot(c(0,3) , c(0,16), type="n", xaxt="n",yaxt="n")
      boxplot(Mulch_protection$nativeRich , Mulch_protection$exoticRich,  ylim=c(0,18), names=c("Native", "Exotic"), add=TRUE, at=c(1,2))
      points(rep(1,20) , Mulch_protection$nativeRich, col="blue")
      points(rep(2,20), Mulch_protection$exoticRich, col="blue")
      
      plot(c(0,3) , c(0,16), type="n", xaxt="n",yaxt="n")
      boxplot(Tarweed_and_mulch$nativeRich , Tarweed_and_mulch$exoticRich,  ylim=c(0,18), names=c("Native", "Exotic"), add=TRUE, at=c(1,2))
      points(rep(1,20) , Tarweed_and_mulch$nativeRich, col="blue")
      points(rep(2,20), Tarweed_and_mulch$exoticRich, col="blue")
      
      ### Before we move on to doing statistics, can you look at these graphs and write down the biological story you think they represent?
      
      
      
    })
    
    output$pulled_controls_1 <- renderPlot({
     
        par(mfrow=c(2,1), mar=c(2,4,3,2)) 
      
      plot(c(0,3) , c(0,1), type="n", xaxt="n",yaxt="n", ylab="% Cover")
      boxplot(Control$nativeCover , Control$exoticCover, main="Control", ylim=c(0,1), names=c("Native", "Exotic"), add=TRUE, at=c(1,2),cex.axis=1)
      points(rep(1,20) , Control$nativeCover, col="blue")
      points(rep(2,20), Control$exoticCover, col="blue")
      
      plot(c(0,3) , c(0,16), type="n", xaxt="n",yaxt="n", ylab="Species Richness")
      boxplot(Control$nativeRich , Control$exoticRich, ylim=c(0,18), names=c("Native", "Exotic"), add=TRUE, at=c(1,2),cex.axis=1)
      points(rep(1,20) , Control$nativeRich, col="blue")
      points(rep(2,20), Control$exoticRich, col="blue")
      
      })
    
    output$pulled_controls_2 <- renderPlot({
      
        par(mfrow=c(2,1), mar=c(2,4,3,2)) 
        
        plot(c(0,3) , c(0,1), type="n", xaxt="n",yaxt="n", ylab="% Cover")
        boxplot(Tarweed_protection$nativeCover , Tarweed_protection$exoticCover, main="Tarweed_protection", ylim=c(0,1), names=c("Native", "Exotic"), add=TRUE, at=c(1,2))
        points(rep(1,20) , Tarweed_protection $nativeCover, col="blue")
        points(rep(2,20), Tarweed_protection $exoticCover, col="blue")
        
        plot(c(0,3) , c(0,16), type="n", xaxt="n",yaxt="n", ylab="Species Richness")
        boxplot(Tarweed_protection$nativeRich , Tarweed_protection$exoticRich, ylim=c(0,18), names=c("Native", "Exotic"), add=TRUE, at=c(1,2))
        points(rep(1,20) , Tarweed_protection$nativeRich, col="blue")
        points(rep(2,20), Tarweed_protection$exoticRich, col="blue")
      
    })
    
    output$pulled_controls_3 <- renderPlot({
      
        par(mfrow=c(2,1), mar=c(2,4,3,2)) 
        
        plot(c(0,3) , c(0,1), type="n", xaxt="n",yaxt="n", ylab="% Cover")
        boxplot(Mulch_protection$nativeCover , Mulch_protection$exoticCover, main="Mulch_protection", ylim=c(0,1), names=c("Native", "Exotic"), add=TRUE, at=c(1,2))
        points(rep(1,20) , Mulch_protection $nativeCover, col="blue")
        points(rep(2,20), Mulch_protection $exoticCover, col="blue")
        
        plot(c(0,3) , c(0,16), type="n", xaxt="n",yaxt="n", ylab="Species Richness")
        boxplot(Mulch_protection$nativeRich , Mulch_protection$exoticRich,  ylim=c(0,18), names=c("Native", "Exotic"), add=TRUE, at=c(1,2))
        points(rep(1,20) , Mulch_protection$nativeRich, col="blue")
        points(rep(2,20), Mulch_protection$exoticRich, col="blue")
      
    })
    
    output$pulled_controls_4 <- renderPlot({
      
        par(mfrow=c(2,1), mar=c(2,4,3,2)) 
        
        plot(c(0,3) , c(0,1), type="n", xaxt="n",yaxt="n", ylab="% Cover")
        boxplot(Tarweed_and_mulch$nativeCover , Tarweed_and_mulch$exoticCover, main="Tarweed_and_mulch", ylim=c(0,1), names=c("Native", "Exotic"), add=TRUE, at=c(1,2))
        points(rep(1,20) , Tarweed_and_mulch $nativeCover, col="blue")
        points(rep(2,20), Tarweed_and_mulch $exoticCover, col="blue")
        
        plot(c(0,3) , c(0,16), type="n", xaxt="n",yaxt="n", ylab="Species Richness")
        boxplot(Tarweed_and_mulch$nativeRich , Tarweed_and_mulch$exoticRich,  ylim=c(0,18), names=c("Native", "Exotic"), add=TRUE, at=c(1,2))
        points(rep(1,20) , Tarweed_and_mulch$nativeRich, col="blue")
        points(rep(2,20), Tarweed_and_mulch$exoticRich, col="blue")
      
    })
   
      output$pillars_inference <- renderImage({
      list(
        src = "www/five pillars.png",
        contentType = "image/png",
        alt = "Face"
      )}, deleteFile = FALSE)
    

   
      
    
    output$t_test_example <- renderPlot({
      
      if(input$t_test_first_compare == "controls" & input$t_test_first_compare_1 == "exotic"){x_Tarweed <-Control$exoticCover}
      if(input$t_test_first_compare == "controls" & input$t_test_first_compare_1 == "native"){x_Tarweed <- Control$nativeCover}
      if(input$t_test_first_compare == "tarweed_protection" & input$t_test_first_compare_1 == "exotic"){x_Tarweed <- Tarweed_protection$exoticCover}
      if(input$t_test_first_compare == "tarweed_protection" & input$t_test_first_compare_1 == "native"){x_Tarweed <- Tarweed_protection$nativeCover}
      if(input$t_test_first_compare == "mulch_protection" & input$t_test_first_compare_1 == "exotic"){x_Tarweed <- Mulch_protection$exoticCover}
      if(input$t_test_first_compare == "mulch_protection" & input$t_test_first_compare_1 == "native"){x_Tarweed <- Mulch_protection$nativeCover}
      if(input$t_test_first_compare == "tarweed_and_mulch" & input$t_test_first_compare_1 == "exotic"){x_Tarweed <- Tarweed_and_mulch$exoticCover}
      if(input$t_test_first_compare == "tarweed_and_mulch" & input$t_test_first_compare_1 == "native"){x_Tarweed <- Tarweed_and_mulch$nativeCover}
      
      if(input$t_test_second_compare == "controls" & input$t_test_second_compare_1 == "exotic"){x_Mulch <-Control$exoticCover}
      if(input$t_test_second_compare == "controls" & input$t_test_second_compare_1 == "native"){x_Mulch <- Control$nativeCover}
      if(input$t_test_second_compare == "tarweed_protection" & input$t_test_second_compare_1 == "exotic"){x_Mulch <- Tarweed_protection$exoticCover}
      if(input$t_test_second_compare == "tarweed_protection" & input$t_test_second_compare_1 == "native"){x_Mulch <- Tarweed_protection$nativeCover}
      if(input$t_test_second_compare == "mulch_protection" & input$t_test_second_compare_1 == "exotic"){x_Mulch <- Mulch_protection$exoticCover}
      if(input$t_test_second_compare == "mulch_protection" & input$t_test_second_compare_1 == "native"){x_Mulch <- Mulch_protection$nativeCover}
      if(input$t_test_second_compare == "tarweed_and_mulch" & input$t_test_second_compare_1 == "exotic"){x_Mulch <- Tarweed_and_mulch$exoticCover}
      if(input$t_test_second_compare == "tarweed_and_mulch" & input$t_test_second_compare_1 == "native"){x_Mulch <- Tarweed_and_mulch$nativeCover}
      
      output$t_test_code__percent_cover <- renderText({
        if(input$t_test_first_compare == "controls" & input$t_test_first_compare_1 == "exotic"){z_Tarweed <-"Controls$exoticCover"}
        if(input$t_test_first_compare == "controls" & input$t_test_first_compare_1 == "native"){z_Tarweed <- "Controls$nativeCover"}
        if(input$t_test_first_compare == "tarweed_protection" & input$t_test_first_compare_1 == "exotic"){z_Tarweed <- "Tarweed_protection$exoticCover"}
        if(input$t_test_first_compare == "tarweed_protection" & input$t_test_first_compare_1 == "native"){z_Tarweed <- "Tarweed_protection$nativeCover"}
        if(input$t_test_first_compare == "mulch_protection" & input$t_test_first_compare_1 == "exotic"){z_Tarweed <- "Mulch_protection$exoticCover"}
        if(input$t_test_first_compare == "mulch_protection" & input$t_test_first_compare_1 == "native"){z_Tarweed <- "Mulch_protection$nativeCover"}
        if(input$t_test_first_compare == "tarweed_and_mulch" & input$t_test_first_compare_1 == "exotic"){z_Tarweed <- "Tarweed_and_mulch$exoticCover"}
        if(input$t_test_first_compare == "tarweed_and_mulch" & input$t_test_first_compare_1 == "native"){z_Tarweed <- "Tarweed_and_mulch$nativeCover"}
        
        if(input$t_test_second_compare == "controls" & input$t_test_second_compare_1 == "exotic"){z_Mulch <-"Controls$exoticCover"}
        if(input$t_test_second_compare == "controls" & input$t_test_second_compare_1 == "native"){z_Mulch <- "Controls$nativeCover"}
        if(input$t_test_second_compare == "tarweed_protection" & input$t_test_second_compare_1 == "exotic"){z_Mulch <- "Tarweed_protection$exoticCover"}
        if(input$t_test_second_compare == "tarweed_protection" & input$t_test_second_compare_1 == "native"){z_Mulch <- "Tarweed_protection$nativeCover"}
        if(input$t_test_second_compare == "mulch_protection" & input$t_test_second_compare_1 == "exotic"){z_Mulch <- "Mulch_protection$exoticCover"}
        if(input$t_test_second_compare == "mulch_protection" & input$t_test_second_compare_1 == "native"){z_Mulch <- "Mulch_protection$nativeCover"}
        if(input$t_test_second_compare == "tarweed_and_mulch" & input$t_test_second_compare_1 == "exotic"){z_Mulch <- "Tarweed_and_mulch$exoticCover"}
        if(input$t_test_second_compare == "tarweed_and_mulch" & input$t_test_second_compare_1 == "native"){z_Mulch <- "Tarweed_and_mulch$nativeCover"}
        paste("t.test( ", z_Tarweed, ", " , z_Mulch," )", sep="")
      })
      
      output$ttest_console_native_cover <- renderPrint({
        capture.output(t.test(x_Tarweed , x_Mulch))
      })
      
      
      t_out <- t.test(x_Tarweed , x_Mulch)
      t_out[[5]][2]
      
      #x_Mulch <- Mulch_protection$exoticCover
      
      h_Mulch <- hist(x_Mulch, breaks=10)
      xfit_Mulch <- (seq(-1, max(x_Mulch)+1, length=80) ) 
      yfit_Mulch <- dnorm(xfit_Mulch, mean=mean(x_Mulch), sd=sd(x_Mulch))
      yfit_Mulch <- yfit_Mulch*diff(h_Mulch $mids[1:2])*length(x_Mulch)
      
      
      #x_Tarweed<- Mulch_protection $nativeCover
      
      h_Tarweed <- hist(x_Tarweed, breaks=10)
      xfit_Tarweed <- (seq(-1, max(x_Tarweed)+1, length=80) ) 
      yfit_Tarweed <- dnorm(xfit_Tarweed, mean=mean(x_Tarweed), sd=sd(x_Tarweed))
      yfit_Tarweed <- yfit_Tarweed*diff(h_Tarweed $mids[1:2])*length(x_Tarweed)
      
      
      
      
      par(mfrow=c(3,1), mar=c(2,4,0,0))
      plot(1,1, type="n",   xlab="", ylab="Frequency", bty="n", col="green", main="", ylim=c(0,7), xlim=c(-4,4))
      
      
      polygon(xfit_Tarweed-t_out[[5]][1] + t_out[[1]],yfit_Tarweed, col="dodgerblue", lwd=2)
      d_t <- density(x_Tarweed)
      #polygon(d_t, col="red", border="blue")
      #rect((t_out[[5]][1] - t_out[[4]][2])-t_out[[5]][1] + t_out[[1]], 12, (t_out[[5]][1] + t_out[[4]][2])-t_out[[5]][1] + t_out[[1]], -1, col=alpha("white",.8), border="white")
      polygon(xfit_Tarweed-t_out[[5]][1] + t_out[[1]],yfit_Tarweed, lwd=2)
      
      abline(v=t_out[[5]][1]-t_out[[5]][1] + t_out[[1]], lwd=2)
      #abline(v=t_out[[5]][1] + t_out[[4]][2]-t_out[[5]][1] + t_out[[1]], lty=2)
      #abline(v=t_out[[5]][1] - t_out[[4]][2]-t_out[[5]][1] + t_out[[1]], lty=2)
      
      
      abline(h=0, lwd=2)
      
      h_Tarweed$breaks<- c((h_Tarweed$breaks)-t_out[[5]][1] + t_out[[1]])
      if(input$hist_on_off == "1"){plot(h_Tarweed,  xaxt="n", yaxt="n", xlab="", ylab="", bty="n", col=alpha("orange",1), main="", add=TRUE)}
      
      #abline(v=0, lwd=1, col="green")
      #legend(.5, 7, paste("95% confidence interval "), col = alpha("dodgerblue",.5), text.col = "black",  pch = 15, bty="n", cex=1.5, pt.cex=4)
      if(input$t_test_first_compare_1 == "native"){choice_b1 <- "Native"}else{choice_b1 <- "Exotic"}
      
      if(input$t_test_first_compare == "controls"){choice_b2 <- "Control"}
      if(input$t_test_first_compare == "tarweed_protection"){choice_b2 <- "Tarweed protection"}
      if(input$t_test_first_compare == "mulch_protection"){choice_b2 <- "Mulch protection"}
      if(input$t_test_first_compare == "tarweed_and_mulch"){choice_b2 <- "Tarweed and Mulch protection"}
      
      legend(-4.5, 7, paste(choice_b1," ",choice_b2," data", sep=""), col = "white", text.col = "black",  pch = 1, bty="n", cex=1.5, pt.cex=4)
      
      
      plot(1,1, type="n",   xlab="", ylab="Frequency", bty="n", col="green", main="", ylim=c(0,7), xlim=c(-4,4))
      
      polygon(xfit_Mulch-t_out[[5]][2],yfit_Mulch, col="dodgerblue", lwd=2)
      d_m <- density(x_Mulch)
      
      #rect(t_out[[5]][2] - t_out[[4]][1]-t_out[[5]][2], 12, t_out[[5]][2] + t_out[[4]][1]-t_out[[5]][2], -1, col=alpha("white",.8), border="white")
      polygon(xfit_Mulch-t_out[[5]][2],yfit_Mulch, lwd=2)
      
      abline(v=t_out[[5]][2]-t_out[[5]][2], lwd=2)
      #abline(v=t_out[[5]][2] + t_out[[4]][1]-t_out[[5]][2], lty=2)
      #abline(v=t_out[[5]][2] - t_out[[4]][1]-t_out[[5]][2], lty=2)
      abline(h=0, lwd=2)
      #abline(v=0, lwd=1, col="green")
      
      h_Mulch $breaks <- c(h_Mulch $breaks-t_out[[4]][2])
      if(input$hist_on_off == "1"){plot(h_Mulch,  xaxt="n", yaxt="n", xlab="", ylab="", bty="n", col=alpha("orange",1), main="", add=TRUE)}
      
      #legend(.5, 7, paste("95% confidence interval ", sep=""), col = alpha("dodgerblue",.5), text.col = "black",  pch = 15, bty="n", cex=1.5, pt.cex=4)
      
      
      
      if(input$t_test_second_compare_1 == "native"){choice_a1 <- "Native"}else{choice_a1 <- "Exotic"}
      
      if(input$t_test_second_compare == "controls"){choice_a2 <- "Control"}
      if(input$t_test_second_compare == "tarweed_protection"){choice_a2 <- "Tarweed protection"}
      if(input$t_test_second_compare == "mulch_protection"){choice_a2 <- "Mulch protection"}
      if(input$t_test_second_compare == "tarweed_and_mulch"){choice_a2 <- "Tarweed and Mulch protection"}
      
      legend(-4.5, 6.5, paste(choice_a1," ",choice_a2," data", sep=""), col = "white", text.col = "black",  pch = 1, bty="n", cex=1.5, pt.cex=4)
      
      par(mar=c(4,4,0,0))
      d <- density(rt(10000000, df=t_out[[2]]))
      plot(d, main="", bty="n", ylim=c(0,.5),   xlab="Difference between means", ylab="Frequency", xlim=c(-4,4))
      
      polygon(d, col="firebrick3")
      
      rect( - t_out[[1]], 12,  t_out[[1]], -1, col=alpha("white",.8), border=alpha("white",0))
      polygon(d,lwd=2)
      abline(v=t_out[[1]], col="dodgerblue")
      abline(h=0, lwd=4)
      #abline(v=0, lwd=1, col="green")
      t_out
      
      legend(2, .45, paste("P-value = ",format(t_out[[3]], digit=3), sep=""), col = "firebrick3", text.col = "black",  pch = 15, bty="n", cex=1.5, pt.cex=4)
      legend(-4.5, .45, paste("t statistic = ",format(t_out[[1]], digit=3), sep=""), col = "white", text.col = "black",  pch = 1, bty="n", cex=1.5, pt.cex=4)
      
    })
    
    
    
    
    
    output$relative_t_test_example <- renderPlot({
      
      x_Tarweed <- NA
      x_mulch <- NA
      if(input$relative_t_test_first_compare == "controls" & input$relative_t_test_first_compare_1 == "exotic"){x_Tarweed <-Controls$exoticRich}
      if(input$relative_t_test_first_compare == "controls" & input$relative_t_test_first_compare_1 == "native"){x_Tarweed <- Controls$nativeRich}
      if(input$relative_t_test_first_compare == "tarweed_protection" & input$relative_t_test_first_compare_1 == "exotic"){x_Tarweed <- Tarweed_protection$exoticRich}
      if(input$relative_t_test_first_compare == "tarweed_protection" & input$relative_t_test_first_compare_1 == "native"){x_Tarweed <- Tarweed_protection$nativeRich}
      if(input$relative_t_test_first_compare == "mulch_protection" & input$relative_t_test_first_compare_1 == "exotic"){x_Tarweed <- Mulch_protection$exoticRich}
      if(input$relative_t_test_first_compare == "mulch_protection" & input$relative_t_test_first_compare_1 == "native"){x_Tarweed <- Mulch_protection$nativeRich}
      if(input$relative_t_test_first_compare == "tarweed_and_mulch" & input$relative_t_test_first_compare_1 == "exotic"){x_Tarweed <- Tarweed_and_mulch$exoticRich}
      if(input$relative_t_test_first_compare == "tarweed_and_mulch" & input$relative_t_test_first_compare_1 == "native"){x_Tarweed <- Tarweed_and_mulch$nativeRich}
      
      if(input$relative_t_test_second_compare == "controls" & input$relative_t_test_second_compare_1 == "exotic"){x_Mulch <-Controls$exoticRich}
      if(input$relative_t_test_second_compare == "controls" & input$relative_t_test_second_compare_1 == "native"){x_Mulch <- Controls$nativeRich}
      if(input$relative_t_test_second_compare == "tarweed_protection" & input$relative_t_test_second_compare_1 == "exotic"){x_Mulch <- Tarweed_protection$exoticRich}
      if(input$relative_t_test_second_compare == "tarweed_protection" & input$relative_t_test_second_compare_1 == "native"){x_Mulch <- Tarweed_protection$nativeRich}
      if(input$relative_t_test_second_compare == "mulch_protection" & input$relative_t_test_second_compare_1 == "exotic"){x_Mulch <- Mulch_protection$exoticRich}
      if(input$relative_t_test_second_compare == "mulch_protection" & input$relative_t_test_second_compare_1 == "native"){x_Mulch <- Mulch_protection$nativeRich}
      if(input$relative_t_test_second_compare == "tarweed_and_mulch" & input$relative_t_test_second_compare_1 == "exotic"){x_Mulch <- Tarweed_and_mulch$exoticRich}
      if(input$relative_t_test_second_compare == "tarweed_and_mulch" & input$relative_t_test_second_compare_1 == "native"){x_Mulch <- Tarweed_and_mulch$nativeRich}
      
      output$t_test_code_rich <- renderText({
        if(input$relative_t_test_first_compare == "controls" & input$relative_t_test_first_compare_1 == "exotic"){z_Tarweed <-"Controls$exoticRich"}
        if(input$relative_t_test_first_compare == "controls" & input$relative_t_test_first_compare_1 == "native"){z_Tarweed <- "Controls$nativeRich"}
        if(input$relative_t_test_first_compare == "tarweed_protection" & input$relative_t_test_first_compare_1 == "exotic"){z_Tarweed <- "Tarweed_protection$exoticRich"}
        if(input$relative_t_test_first_compare == "tarweed_protection" & input$relative_t_test_first_compare_1 == "native"){z_Tarweed <- "Tarweed_protection$nativeRich"}
        if(input$relative_t_test_first_compare == "mulch_protection" & input$relative_t_test_first_compare_1 == "exotic"){z_Tarweed <- "Mulch_protection$exoticRich"}
        if(input$relative_t_test_first_compare == "mulch_protection" & input$relative_t_test_first_compare_1 == "native"){z_Tarweed <- "Mulch_protection$nativeRich"}
        if(input$relative_t_test_first_compare == "tarweed_and_mulch" & input$relative_t_test_first_compare_1 == "exotic"){z_Tarweed <- "Tarweed_and_mulch$exoticRich"}
        if(input$relative_t_test_first_compare == "tarweed_and_mulch" & input$relative_t_test_first_compare_1 == "native"){z_Tarweed <- "Tarweed_and_mulch$nativeRich"}
        
        if(input$relative_t_test_second_compare == "controls" & input$relative_t_test_second_compare_1 == "exotic"){z_Mulch <- "Controls$exoticRich"}
        if(input$relative_t_test_second_compare == "controls" & input$relative_t_test_second_compare_1 == "native"){z_Mulch <- "Controls$nativeRich"}
        if(input$relative_t_test_second_compare == "tarweed_protection" & input$relative_t_test_second_compare_1 == "exotic"){z_Mulch <- "Tarweed_protection$exoticRich"}
        if(input$relative_t_test_second_compare == "tarweed_protection" & input$relative_t_test_second_compare_1 == "native"){z_Mulch <- "Tarweed_protection$nativeRich"}
        if(input$relative_t_test_second_compare == "mulch_protection" & input$relative_t_test_second_compare_1 == "exotic"){z_Mulch <- "Mulch_protection$exoticRich"}
        if(input$relative_t_test_second_compare == "mulch_protection" & input$relative_t_test_second_compare_1 == "native"){z_Mulch <- "Mulch_protection$nativeRich"}
        if(input$relative_t_test_second_compare == "tarweed_and_mulch" & input$relative_t_test_second_compare_1 == "exotic"){z_Mulch <- "Tarweed_and_mulch$exoticRich"}
        if(input$relative_t_test_second_compare == "tarweed_and_mulch" & input$relative_t_test_second_compare_1 == "native"){z_Mulch <- "Tarweed_and_mulch$nativeRich"}
        
        paste("t.test( ", z_Tarweed, ", " , z_Mulch," )", sep="")
      })
      
      output$ttest_console_rich <- renderPrint({
        capture.output(t.test(x_Tarweed , x_Mulch))
      })
      
      t_out <- t.test(x_Tarweed , x_Mulch)
      t_out[[5]][2]
      
      #x_Mulch <- Mulch_protection$exoticCover
      
      h_Mulch <- hist(x_Mulch, breaks=10)
      xfit_Mulch <- (seq(-4, max(x_Mulch)+4, length=80) ) 
      yfit_Mulch <- dnorm(xfit_Mulch, mean=mean(x_Mulch), sd=sd(x_Mulch))
      yfit_Mulch <- yfit_Mulch*diff(h_Mulch $mids[1:2])*length(x_Mulch)
      
      
      #x_Tarweed<- Mulch_protection $nativeCover
      
      h_Tarweed <- hist(x_Tarweed, breaks=10)
      xfit_Tarweed <- (seq(-4, max(x_Tarweed)+4, length=80) ) 
      yfit_Tarweed <- dnorm(xfit_Tarweed, mean=mean(x_Tarweed), sd=sd(x_Tarweed))
      yfit_Tarweed <- yfit_Tarweed*diff(h_Tarweed $mids[1:2])*length(x_Tarweed)
      
      
      
      
      par(mfrow=c(3,1), mar=c(2,4,0,0))
      plot(1,1, type="n",   xlab="", ylab="Frequency", bty="n", col="green", main="", ylim=c(0,7), xlim=c(-10,10))
      
      
      polygon(xfit_Tarweed-t_out[[5]][1] + t_out[[1]],yfit_Tarweed, col="dodgerblue", lwd=2)
      d_t <- density(x_Tarweed)
      #polygon(d_t, col="red", border="blue")
      #rect((t_out[[5]][1] - t_out[[4]][2])-t_out[[5]][1] + t_out[[1]], 12, (t_out[[5]][1] + t_out[[4]][2])-t_out[[5]][1] + t_out[[1]], -1, col=alpha("white",.8), border="white")
      polygon(xfit_Tarweed-t_out[[5]][1] + t_out[[1]],yfit_Tarweed, lwd=2)
      
      abline(v=t_out[[5]][1]-t_out[[5]][1] + t_out[[1]], lwd=2)
      #abline(v=t_out[[5]][1] + t_out[[4]][2]-t_out[[5]][1] + t_out[[1]], lty=2)
      #abline(v=t_out[[5]][1] - t_out[[4]][2]-t_out[[5]][1] + t_out[[1]], lty=2)
      
      
      abline(h=0, lwd=2)
      
      h_Tarweed$breaks<- c((h_Tarweed$breaks)-t_out[[5]][1] + t_out[[1]])
      if(input$relative_hist_on_off == "1"){plot(h_Tarweed,  xaxt="n", yaxt="n", xlab="", ylab="", bty="n", col=alpha("orange",1), main="", add=TRUE)}
      
      #abline(v=0, lwd=1, col="green")
      #legend(2, 7, paste("95% confidence interval "), col = alpha("dodgerblue",.5), text.col = "black",  pch = 15, bty="n", cex=1.5, pt.cex=4)
      if(input$relative_t_test_first_compare_1 == "native"){choice_b1 <- "Native"}else{choice_b1 <- "Exotic"}
      
      if(input$relative_t_test_first_compare == "controls"){choice_b2 <- "Control"}
      if(input$relative_t_test_first_compare == "tarweed_protection"){choice_b2 <- "Tarweed protection"}
      if(input$relative_t_test_first_compare == "mulch_protection"){choice_b2 <- "Mulch protection"}
      if(input$relative_t_test_first_compare == "tarweed_and_mulch"){choice_b2 <- "Tarweed and Mulch protection"}
      
      legend(-10.5, 7, paste(choice_b1," ",choice_b2," data", sep=""), col = "white", text.col = "black",  pch = 1, bty="n", cex=1.5, pt.cex=4)
      
      
      plot(1,1, type="n",   xlab="", ylab="Frequency", bty="n", col="green", main="", ylim=c(0,7), xlim=c(-10,10))
      
      polygon(xfit_Mulch-t_out[[5]][2],yfit_Mulch, col="dodgerblue", lwd=2)
      d_m <- density(x_Mulch)
      
      #rect(t_out[[5]][2] - t_out[[4]][1]-t_out[[5]][2], 12, t_out[[5]][2] + t_out[[4]][1]-t_out[[5]][2], -1, col=alpha("white",.8), border="white")
      polygon(xfit_Mulch-t_out[[5]][2],yfit_Mulch, lwd=2)
      
      abline(v=t_out[[5]][2]-t_out[[5]][2], lwd=2)
      #abline(v=t_out[[5]][2] + t_out[[4]][1]-t_out[[5]][2], lty=2)
      #abline(v=t_out[[5]][2] - t_out[[4]][1]-t_out[[5]][2], lty=2)
      abline(h=0, lwd=2)
      #abline(v=0, lwd=1, col="green")
      
      h_Mulch$breaks<- h_Mulch$breaks-t_out[[5]][1] + t_out[[1]]
      if(input$relative_hist_on_off == "1"){plot(h_Mulch,  xaxt="n", yaxt="n", xlab="", ylab="", bty="n", col=alpha("orange",1), main="", add=TRUE)}
      
      #legend(2, 7, paste("95% confidence interval ", sep=""), col = alpha("dodgerblue",.5), text.col = "black",  pch = 15, bty="n", cex=1.5, pt.cex=4)
      
      
      
      if(input$relative_t_test_second_compare_1 == "native"){choice_a1 <- "Native"}else{choice_a1 <- "Exotic"}
      
      if(input$relative_t_test_second_compare == "controls"){choice_a2 <- "Control"}
      if(input$relative_t_test_second_compare == "tarweed_protection"){choice_a2 <- "Tarweed protection"}
      if(input$relative_t_test_second_compare == "mulch_protection"){choice_a2 <- "Mulch protection"}
      if(input$relative_t_test_second_compare == "tarweed_and_mulch"){choice_a2 <- "Tarweed and Mulch protection"}
      
      legend(-10.5, 6.5, paste(choice_a1," ",choice_a2," data", sep=""), col = "white", text.col = "black",  pch = 1, bty="n", cex=1.5, pt.cex=4)
      
      par(mar=c(4,4,0,0))
      d <- density(rt(10000000, df=t_out[[2]]))
      plot(d, main="", bty="n", ylim=c(0,.5),  xlab="Distance between means", ylab="Frequency", xlim=c(-10,10))
      
      polygon(d, col="firebrick3")
      
      rect( - t_out[[1]], 12,  t_out[[1]], -1, col=alpha("white",.8), border=alpha("white",0))
      polygon(d,lwd=2)
      abline(v=t_out[[1]], col="dodgerblue")
      abline(h=0, lwd=4)
      #abline(v=0, lwd=1, col="green")
      t_out
      
      legend(4.5, .45, paste("P-value = ",format(t_out[[3]], digit=3), sep=""), col = "firebrick3", text.col = "black",  pch = 15, bty="n", cex=1.5, pt.cex=4)
      legend(-10.5, .45, paste("t statistic = ",format(t_out[[1]], digit=3), sep=""), col = "white", text.col = "black",  pch = 1, bty="n", cex=1.5, pt.cex=4)
      
    })
    
    output$anova_code_load <- renderText({
      "data(beaver1)"
    })
    
    output$lm_code <- renderText({
      "model1 <- lm(beaver1[,3]~beaver1[,2])"
    })
    
    output$lm_code_b <- renderText({
      "summary(model1)" 
    })
    
    output$lm_console <- renderPrint({
      capture.output(summary(model1 <- lm(beaver1[,3]~beaver1[,2])))
    })
    
    
    
    output$linear_match <- renderPlot({
      
     
      beaver1  <- beaver1
      plot(beaver1[,2],beaver1[,3], xlab="Explanatory variable = Seconds of sun exposure", ylab="Response variable = Beaver body temperature") 
      model1 <- lm(beaver1[,3]~beaver1[,2])
      #abline(model1)
      
      
      
      
      #lines(c(beaver1[1,2],0),c(beaver1[1,3],0), col="red")
      m <- input$m_slider  #1.21e-04
      b <-  input$b_slider #3.67e+01
      
      
      if(input$reset_linear == 1){
      updateSliderInput(session, "m_slider", value=1.21e-04)
      updateSliderInput(session, "b_slider", value=3.67e+01)
      }
      
      
      
      
      
      y_cat <- NA
      diff_height_catch <- NA
      for(i in 1:length(beaver1[,1])){
        y <- m*beaver1[i,2] + b
        
        if(input$m_slider == 1.21e-04 & input$b_slider == 3.67e+01){lines(c(beaver1[i,2],beaver1[i,2]),c(beaver1[i,3],y), col="green")}else{lines(c(beaver1[i,2],beaver1[i,2]),c(beaver1[i,3],y), col="red")}
        diff_height <- abs(y-beaver1[i,3])
        
        diff_height_catch <- c(diff_height_catch,diff_height)
        y_cat <- c(y_cat,y)
      }
      lines(beaver1[,2],y_cat[-1])
      points(beaver1[,2],beaver1[,3])
      
    })
 
    
    
    output$sum_of_squares_dist <- renderPlot({
      beaver1  <- beaver1
      model1 <- lm(beaver1[,3]~beaver1[,2])
      #abline(model1)
      
      
      m <- input$m_slider  #1.21e-04
      b <-  input$b_slider #3.67e+01
      
    
      y_cat <- NA
      diff_height_catch <- NA
      for(i in 1:length(beaver1[,1])){
        y <- m*beaver1[i,2] + b
        
        diff_height <- y-beaver1[i,3]
        
        diff_height_catch <- c(diff_height_catch,diff_height)
        y_cat <- c(y_cat,y)
      }
      diff_height_catch <- diff_height_catch[-1]
      
      
      h_Tarweed <- hist(diff_height_catch, breaks=10)
      xfit_Tarweed <- (seq(-10, max(diff_height_catch)+10, length=2000) ) 
      yfit_Tarweed <- dnorm(xfit_Tarweed, mean=mean(diff_height_catch), sd=sd(diff_height_catch))
      yfit_Tarweed <- yfit_Tarweed*diff(h_Tarweed $mids[1:2])*length(diff_height_catch)
      
      
      
      
      par(mar=c(0,0,0,0))
      d <- density(diff_height_catch)
      plot(d, main="", bty="n", ylim=c(0,30), xaxt="n", yaxt="n", xlab="", ylab="", xlim=c(-2,2), type="n")
     abline(v=0, lwd=2)
      polygon(xfit_Tarweed,yfit_Tarweed, col="dodgerblue", lwd=2)
      
     # if(input$slider_hist_on_off == "1"){plot(h_Tarweed,  xaxt="n", yaxt="n", xlab="", ylab="", bty="n", col=alpha("orange",1), main="", add=TRUE) }
      #rect((t_out[[5]][1] - t_out[[4]][2])-t_out[[5]][1] + t_out[[1]], 12, (t_out[[5]][1] + t_out[[4]][2])-t_out[[5]][1] + t_out[[1]], -1, col=alpha("white",.8), border="white")
      polygon(xfit_Tarweed,yfit_Tarweed, lwd=2)
      
    })
    
    
    
    output$anova_code <- renderText({
"nativeCover_AOV <-aov(nativeCover ~ tarweed + mulch + tarweed:mulch , data=dat)"
      })
    
    output$anova_code_b <- renderText({
      "summary(nativeCover_AOV)"
    })
    
    output$sum_it <- renderPlot({
      
      
      Element_1 <- Control$nativeCover
      Element_2 <-  Tarweed_protection$nativeCover
      Element_3 <-  Mulch_protection$nativeCover
      Element_4 <-  Tarweed_and_mulch$nativeCover
      
      y_lim_max<- 4.5
      standard_error_Element_1 <- sd(Element_1)/sqrt(length(Element_1))
      standard_error_Element_2 <- sd(Element_2)/sqrt(length(Element_2))
      standard_error_Element_3 <- sd(Element_3)/sqrt(length(Element_3))
      standard_error_Element_4 <- sd(Element_4)/sqrt(length(Element_4))
      
      
      
      par(mar=c(3,3,0,0))
      plot(c(max(Element_1),c(1,1,2,2), min(Element_1), max(Element_2), min(Element_2)), type="n", yaxt="n", xlim=c(0,1), ylab="", ylim=c(0.5,y_lim_max), xlab="Percentage of ground covered")
      
      axis(2, at=c(1,2,3,4),c("Control", "Tarweed protection", "Mulch protection", "Mulch and Tarweed"), las=3, tick=FALSE)
      
      rect((mean(Element_1)-(standard_error_Element_1 + input$control_variance_slider)) + input$control_mean_slider, 1.05, (mean(Element_1)+ (standard_error_Element_1 + input$control_variance_slider))+input$control_mean_slider, .95, col=alpha("red", 0.5), border="white")
      rect((mean(Element_2)-(standard_error_Element_2 + input$tarweed_variance_slider_slider )) + input$tarweed_mean_slider_slider, 2.05, (mean(Element_2)+ (standard_error_Element_2 + input$tarweed_variance_slider_slider ))+ input$tarweed_mean_slider_slider, 1.95, col= alpha("red", 0.5), border="white")
      rect((mean(Element_3)-(standard_error_Element_3 + input$mulch_variance_slider)) + input$mulch_mean_slider, 3.05, (mean(Element_3)+ (standard_error_Element_3 + input$mulch_variance_slider ))+ input$mulch_mean_slider, 2.95, col= alpha("red", 0.5), border="white")
      rect((mean(Element_4)-(standard_error_Element_4 + input$tarweed_and_mulch_variance_slider)) + input$tarweed_and_mulch_mean_slider, 4.05, (mean(Element_4)+ (standard_error_Element_4 + input$tarweed_and_mulch_variance_slider ))+ input$tarweed_and_mulch_mean_slider, 3.95, col= alpha("red", 0.5), border="white")
      
      if(input$control_mean_slider == 0 & input$tarweed_mean_slider_slider == 0 & input$tarweed_mean_slider_slider == 0 & input$tarweed_and_mulch_mean_slider == 0 
         & input$control_variance_slider == 0 & input$tarweed_variance_slider_slider == 0 & input$mulch_variance_slider == 0 & input$tarweed_and_mulch_variance_slider == 0){
        
      points(Element_1,rep(.9,20) ,  col=alpha("blue", 0.5), pch=15, cex=1.5)
      points(Element_2,rep(1.9,20),  col=alpha("blue", 0.5), pch=15, cex=1.5)
      points(Element_3,rep(2.9,20),  col=alpha("blue", 0.5), pch=15, cex=1.5)
      points(Element_4,rep(3.9,20),  col=alpha("blue", 0.5), pch=15, cex=1.5)
      
      
      }
      
      
      lines( rep(mean(Element_1)+ input$control_mean_slider,2), c(.75,1.25), lwd=2)
      lines( rep(mean(Element_2)+ input$tarweed_mean_slider_slider,2) , c(1.75,2.25), lwd=2)
      lines( rep(mean(Element_3)+ input$mulch_mean_slider,2), c(2.75,3.25), lwd=2)
      lines( rep(mean(Element_4)+ input$tarweed_and_mulch_mean_slider,2) , c(3.75,4.25), lwd=2)
      
     
      Element_1_b <- rnorm(20, mean= (mean(Element_1)+ input$control_mean_slider), sd = (sd(Element_1) + input$control_variance_slider))
      Element_2_b <- rnorm(20, mean= (mean(Element_2)+ input$tarweed_mean_slider_slider), sd = (sd(Element_2) + input$tarweed_variance_slider_slider))
      Element_3_b <- rnorm(20, mean= (mean(Element_3)+ input$mulch_mean_slider), sd = (sd(Element_3) + input$mulch_variance_slider))
      Element_4_b <- rnorm(20, mean= (mean(Element_4)+ input$tarweed_and_mulch_mean_slider), sd = (sd(Element_4) + input$tarweed_and_mulch_variance_slider))
      
      dater <- dat
      
      dat[1:20,4] <- Element_1_b
      dat[21:40,4] <- Element_2_b
      dat[41:60,4] <- Element_3_b
      dat[61:80,4] <- Element_4_b
    
      
      nativeCover_AOV <-aov(nativeCover ~ tarweed + mulch + tarweed:mulch , data=dater)
      nativeCover_AOV_2 <-aov(nativeCover ~ tarweed + mulch + tarweed:mulch , data=dat)
      #summary(nativeCover_AOV)
      
      output$anova_code_data <- renderTable({ 
        if(input$control_mean_slider == 0 & input$tarweed_mean_slider_slider == 0 & input$tarweed_mean_slider_slider == 0 & input$tarweed_and_mulch_mean_slider == 0 
           & input$control_variance_slider == 0 & input$tarweed_variance_slider_slider == 0 & input$mulch_variance_slider == 0 & input$tarweed_and_mulch_variance_slider == 0){as.matrix(dater[,c(4,2,3)], 3, dater[,c(4,2,3)]) }else{as.matrix(dat[,c(4,2,3)], 3, dat[,c(4,2,3)])} })
      
      
      output$console_native_cover <- renderPrint({
        if(input$control_mean_slider == 0 & input$tarweed_mean_slider_slider == 0 & input$tarweed_mean_slider_slider == 0 & input$tarweed_and_mulch_mean_slider == 0 
           & input$control_variance_slider == 0 & input$tarweed_variance_slider_slider == 0 & input$mulch_variance_slider == 0 & input$tarweed_and_mulch_variance_slider == 0){capture.output(summary(nativeCover_AOV))}else{capture.output(summary(nativeCover_AOV_2))}
        
      })
       
      
      grand_mean <- mean(dat[,4])
      abline(v=grand_mean, col= alpha("orange", .5), lwd=2)
      
      
    })
    
    output$anova_code_exotic_cover <- renderText({
      "exoticCover_AOV <-aov(exoticCover ~ tarweed + mulch + tarweed:mulch , data=dat)"
    })
    
    output$anova_code_exotic_cover_b <- renderText({
      "summary(exoticCover_AOV)"
    })
    
    output$sum_it_aa <- renderPlot({
      
      
      Element_1 <- Control$exoticCover
      Element_2 <-  Tarweed_protection$exoticCover
      Element_3 <-  Mulch_protection$exoticCover
      Element_4 <-  Tarweed_and_mulch$exoticCover
      
      y_lim_max<- 4.5
      standard_error_Element_1 <- sd(Element_1)/sqrt(length(Element_1))
      standard_error_Element_2 <- sd(Element_2)/sqrt(length(Element_2))
      standard_error_Element_3 <- sd(Element_3)/sqrt(length(Element_3))
      standard_error_Element_4 <- sd(Element_4)/sqrt(length(Element_4))
      
      
      
      par(mar=c(3,3,0,0))
      plot(c(max(Element_1),c(1,1,2,2), min(Element_1), max(Element_2), min(Element_2)), type="n", yaxt="n", xlim=c(0,1), ylab="", ylim=c(0.5,y_lim_max), xlab="Percentage of ground covered")
      
      axis(2, at=c(1,2,3,4),c("Control", "Tarweed protection", "Mulch protection", "Mulch and Tarweed"), las=3, tick=FALSE)
      
      rect((mean(Element_1)-(standard_error_Element_1 + input$control_variance_slider_aa)) + input$control_mean_slider_aa, 1.05, (mean(Element_1)+ (standard_error_Element_1 + input$control_variance_slider_aa))+input$control_mean_slider_aa, .95, col=alpha("red", 0.5), border="white")
      rect((mean(Element_2)-(standard_error_Element_2 + input$tarweed_variance_slider_slider_aa )) + input$tarweed_mean_slider_slider_aa, 2.05, (mean(Element_2)+ (standard_error_Element_2 + input$tarweed_variance_slider_slider_aa ))+ input$tarweed_mean_slider_slider_aa, 1.95, col= alpha("red", 0.5), border="white")
      rect((mean(Element_3)-(standard_error_Element_3 + input$mulch_variance_slider_aa)) + input$mulch_mean_slider_aa, 3.05, (mean(Element_3)+ (standard_error_Element_3 + input$mulch_variance_slider_aa ))+ input$mulch_mean_slider_aa, 2.95, col= alpha("red", 0.5), border="white")
      rect((mean(Element_4)-(standard_error_Element_4 + input$tarweed_and_mulch_variance_slider_aa)) + input$tarweed_and_mulch_mean_slider_aa, 4.05, (mean(Element_4)+ (standard_error_Element_4 + input$tarweed_and_mulch_variance_slider_aa ))+ input$tarweed_and_mulch_mean_slider_aa, 3.95, col= alpha("red", 0.5), border="white")
      
      if(input$control_mean_slider_aa == 0 & input$tarweed_mean_slider_slider_aa == 0 & input$tarweed_mean_slider_slider_aa == 0 & input$tarweed_and_mulch_mean_slider_aa == 0 
         & input$control_variance_slider_aa == 0 & input$tarweed_variance_slider_slider_aa == 0 & input$mulch_variance_slider_aa == 0 & input$tarweed_and_mulch_variance_slider_aa == 0){
      
      points(Element_1,rep(.9,20) ,  col=alpha("blue", 0.5), pch=15, cex=1.5)
      points(Element_2,rep(1.9,20),  col=alpha("blue", 0.5), pch=15, cex=1.5)
      points(Element_3,rep(2.9,20),  col=alpha("blue", 0.5), pch=15, cex=1.5)
      points(Element_4,rep(3.9,20),  col=alpha("blue", 0.5), pch=15, cex=1.5)
    }
      
      lines( rep(mean(Element_1)+ input$control_mean_slider_aa,2), c(.75,1.25), lwd=2)
      lines( rep(mean(Element_2)+ input$tarweed_mean_slider_slider_aa,2) , c(1.75,2.25), lwd=2)
      lines( rep(mean(Element_3)+ input$mulch_mean_slider_aa,2), c(2.75,3.25), lwd=2)
      lines( rep(mean(Element_4)+ input$tarweed_and_mulch_mean_slider_aa,2) , c(3.75,4.25), lwd=2)
      
      
      
      
      Element_1_b <- rnorm(20, mean= (mean(Element_1)+ input$control_mean_slider_aa), sd = (sd(Element_1) + input$control_variance_slider_aa))
      Element_2_b <- rnorm(20, mean= (mean(Element_2)+ input$tarweed_mean_slider_slider_aa), sd = (sd(Element_2) + input$tarweed_variance_slider_slider_aa))
      Element_3_b <- rnorm(20, mean= (mean(Element_3)+ input$mulch_mean_slider_aa), sd = (sd(Element_3) + input$mulch_variance_slider_aa))
      Element_4_b <- rnorm(20, mean= (mean(Element_4)+ input$tarweed_and_mulch_mean_slider_aa), sd = (sd(Element_4) + input$tarweed_and_mulch_variance_slider_aa))
      
      
     # exoticCover_AOV <-aov(exoticCover ~ tarweed + mulch + tarweed:mulch , data=dat)
     # summary(exoticCover_AOV )
      
      
      
      #output$console_exotic_cover <- renderPrint({
     #   capture.output(summary(exoticCover_AOV))
     # })
      
      dater <- dat
      dat[1:20,5] <- Element_1_b
      dat[21:40,5] <- Element_2_b
      dat[41:60,5] <- Element_3_b
      dat[61:80,5] <- Element_4_b
    
      exoticCover_AOV <-aov(exoticCover ~ tarweed + mulch + tarweed:mulch , data=dater)
      exoticCover_AOV_2 <-aov(exoticCover ~ tarweed + mulch + tarweed:mulch , data=dat)
      
      output$console_exotic_cover <- renderPrint({
        if(input$control_mean_slider_aa == 0 & input$tarweed_mean_slider_slider_aa == 0 & input$tarweed_mean_slider_slider_aa == 0 & input$tarweed_and_mulch_mean_slider_aa == 0 
           & input$control_variance_slider_aa == 0 & input$tarweed_variance_slider_slider_aa == 0 & input$mulch_variance_slider_aa == 0 & input$tarweed_and_mulch_variance_slider_aa == 0){capture.output(summary(exoticCover_AOV))}else{capture.output(summary(exoticCover_AOV_2))}
      })
      
     
      output$anova_code_data_percent_exotic <- renderTable({ 
        if(input$control_mean_slider_aa == 0 & input$tarweed_mean_slider_slider_aa == 0 & input$tarweed_mean_slider_slider_aa == 0 & input$tarweed_and_mulch_mean_slider_aa == 0 
           & input$control_variance_slider_aa == 0 & input$tarweed_variance_slider_slider_aa == 0 & input$mulch_variance_slider_aa == 0 & input$tarweed_and_mulch_variance_slider_aa == 0){as.matrix(dater[,c(5,2,3)], 3, dater[,c(5,2,3)]) }else{as.matrix(dat[,c(5,2,3)], 3, dat[,c(5,2,3)])} })
      
      grand_mean <- mean(dat[,5])
      abline(v=grand_mean, col= alpha("orange", .5), lwd=2)
      
      
    })
    
    
    output$anova_code_native_rich <- renderText({
      "nativeRich_AOV <-aov(nativeRich ~ tarweed + mulch + tarweed:mulch , data=dat)"
    })
    
    output$anova_code_native_rich_b <- renderText({
      "summary(nativeRich_AOV)"
    })
    
    output$sum_it_b <- renderPlot({
      
      
      Element_1 <- Control$nativeRich
      Element_2 <-  Tarweed_protection$nativeRich
      Element_3 <-  Mulch_protection$nativeRich
      Element_4 <-  Tarweed_and_mulch$nativeRich
      
      y_lim_max<- 4.5
      standard_error_Element_1 <- sd(Element_1)/sqrt(length(Element_1))
      standard_error_Element_2 <- sd(Element_2)/sqrt(length(Element_2))
      standard_error_Element_3 <- sd(Element_3)/sqrt(length(Element_3))
      standard_error_Element_4 <- sd(Element_4)/sqrt(length(Element_4))
      
      
      
      par(mar=c(3,3,0,0))
      plot(c(max(Element_1),c(1,1,2,2), min(Element_1), max(Element_2), min(Element_2)), type="n", yaxt="n", xlim=c(0,12), ylab="", ylim=c(0.5,y_lim_max), xlab="Percentage of ground covered")
      
      axis(2, at=c(1,2,3,4),c("Control", "Tarweed protection", "Mulch protection", "Mulch and Tarweed"), las=3, tick=FALSE)
      
      rect((mean(Element_1)-(standard_error_Element_1 + input$control_variance_slider_b)) + input$control_mean_slider_b, 1.05, (mean(Element_1)+ (standard_error_Element_1 + input$control_variance_slider_b))+input$control_mean_slider_b, .95, col=alpha("red", 0.5), border="white")
      rect((mean(Element_2)-(standard_error_Element_2 + input$tarweed_variance_slider_slider_b )) + input$tarweed_mean_slider_slider_b, 2.05, (mean(Element_2)+ (standard_error_Element_2 + input$tarweed_variance_slider_slider_b ))+ input$tarweed_mean_slider_slider_b, 1.95, col= alpha("red", 0.5), border="white")
      rect((mean(Element_3)-(standard_error_Element_3 + input$mulch_variance_slider_b)) + input$mulch_mean_slider_b, 3.05, (mean(Element_3)+ (standard_error_Element_3 + input$mulch_variance_slider_b ))+ input$mulch_mean_slider_b, 2.95, col= alpha("red", 0.5), border="white")
      rect((mean(Element_4)-(standard_error_Element_4 + input$tarweed_and_mulch_variance_slider_b)) + input$tarweed_and_mulch_mean_slider_b, 4.05, (mean(Element_4)+ (standard_error_Element_4 + input$tarweed_and_mulch_variance_slider_b ))+ input$tarweed_and_mulch_mean_slider_b, 3.95, col= alpha("red", 0.5), border="white")
      
      if(input$control_mean_slider_b == 0 & input$tarweed_mean_slider_slider_b == 0 & input$tarweed_mean_slider_slider_b == 0 & input$tarweed_and_mulch_mean_slider_b == 0 
         & input$control_variance_slider_b == 0 & input$tarweed_variance_slider_slider_b == 0 & input$mulch_variance_slider_b == 0 & input$tarweed_and_mulch_variance_slider_b == 0){
        
      points(Element_1,rep(.9,20) ,  col=alpha("blue", 0.5), pch=15, cex=1.5)
      points(Element_2,rep(1.9,20),  col=alpha("blue", 0.5), pch=15, cex=1.5)
      points(Element_3,rep(2.9,20),  col=alpha("blue", 0.5), pch=15, cex=1.5)
      points(Element_4,rep(3.9,20),  col=alpha("blue", 0.5), pch=15, cex=1.5)
      }
      
      lines( rep(mean(Element_1)+ input$control_mean_slider_b,2), c(.75,1.25), lwd=2)
      lines( rep(mean(Element_2)+ input$tarweed_mean_slider_slider_b,2) , c(1.75,2.25), lwd=2)
      lines( rep(mean(Element_3)+ input$mulch_mean_slider_b,2), c(2.75,3.25), lwd=2)
      lines( rep(mean(Element_4)+ input$tarweed_and_mulch_mean_slider_b,2) , c(3.75,4.25), lwd=2)
      
      
      
      Element_1_b <- rnorm(20, mean= (mean(Element_1)+ input$control_mean_slider_b), sd = (sd(Element_1) + input$control_variance_slider_b))
      Element_2_b <- rnorm(20, mean= (mean(Element_2)+ input$tarweed_mean_slider_slider_b), sd = (sd(Element_2) + input$tarweed_variance_slider_slider_b))
      Element_3_b <- rnorm(20, mean= (mean(Element_3)+ input$mulch_mean_slider_b), sd = (sd(Element_3) + input$mulch_variance_slider_b))
      Element_4_b <- rnorm(20, mean= (mean(Element_4)+ input$tarweed_and_mulch_mean_slider_b), sd = (sd(Element_4) + input$tarweed_and_mulch_variance_slider_b))
      
      dater <- dat
      dat[1:20,6] <- Element_1_b
      dat[21:40,6] <- Element_2_b
      dat[41:60,6] <- Element_3_b
      dat[61:80,6] <- Element_4_b
      
      nativeRich_AOV <-aov(nativeRich ~ tarweed + mulch + tarweed:mulch , data=dater)
      nativeRich_AOV_2 <-aov(nativeRich ~ tarweed + mulch + tarweed:mulch , data=dat)
      
      output$console_native_richness <- renderPrint({
        if(input$control_mean_slider_b == 0 & input$tarweed_mean_slider_slider_b == 0 & input$tarweed_mean_slider_slider_b == 0 & input$tarweed_and_mulch_mean_slider_b == 0 
        & input$control_variance_slider_b == 0 & input$tarweed_variance_slider_slider_b == 0 & input$mulch_variance_slider_b == 0 & input$tarweed_and_mulch_variance_slider_b == 0){capture.output(summary(nativeRich_AOV))}else{capture.output(summary(nativeRich_AOV_2))}
        
      })
      
      output$anova_code_data_native_rich <- renderTable({ 
        if(input$control_mean_slider_b == 0 & input$tarweed_mean_slider_slider_b == 0 & input$tarweed_mean_slider_slider_b == 0 & input$tarweed_and_mulch_mean_slider_b == 0 
           & input$control_variance_slider_b == 0 & input$tarweed_variance_slider_slider_b == 0 & input$mulch_variance_slider_b == 0 & input$tarweed_and_mulch_variance_slider_b == 0){as.matrix(dater[,c(6,2,3)], 3, dater[,c(6,2,3)]) }else{as.matrix(dat[,c(6,2,3)], 3, dat[,c(6,2,3)])} })
      
      
      grand_mean <- mean(dat[,6])
      abline(v=grand_mean, col= alpha("orange", .5), lwd=2)
      
    })
    
    output$anova_code_exotic_rich <- renderText({
      "exoticRich_AOV <-aov(exoticRich ~ tarweed + mulch + tarweed:mulch , data=dat)"
    })
    
    output$anova_code_exotic_rich_b <- renderText({
      "summary(exoticRich_AOV)"
    })
    
    output$sum_it_bb <- renderPlot({
      
      
      Element_1 <- Control$exoticRich
      Element_2 <-  Tarweed_protection$exoticRich
      Element_3 <-  Mulch_protection$exoticRich
      Element_4 <-  Tarweed_and_mulch$exoticRich
      
      y_lim_max<- 4.5
      standard_error_Element_1 <- sd(Element_1)/sqrt(length(Element_1))
      standard_error_Element_2 <- sd(Element_2)/sqrt(length(Element_2))
      standard_error_Element_3 <- sd(Element_3)/sqrt(length(Element_3))
      standard_error_Element_4 <- sd(Element_4)/sqrt(length(Element_4))
      
      
      
      par(mar=c(3,3,0,0))
      plot(c(max(Element_1),c(1,1,2,2), min(Element_1), max(Element_2), min(Element_2)), type="n", yaxt="n", xlim=c(0,12), ylab="", ylim=c(0.5,y_lim_max), xlab="Percentage of ground covered")
      
      axis(2, at=c(1,2,3,4),c("Control", "Tarweed protection", "Mulch protection", "Mulch and Tarweed"), las=3, tick=FALSE)
      
      rect((mean(Element_1)-(standard_error_Element_1 + input$control_variance_slider_bb)) + input$control_mean_slider_bb, 1.05, (mean(Element_1)+ (standard_error_Element_1 + input$control_variance_slider_bb))+input$control_mean_slider_bb, .95, col=alpha("red", 0.5), border="white")
      rect((mean(Element_2)-(standard_error_Element_2 + input$tarweed_variance_slider_slider_bb )) + input$tarweed_mean_slider_slider_bb, 2.05, (mean(Element_2)+ (standard_error_Element_2 + input$tarweed_variance_slider_slider_bb ))+ input$tarweed_mean_slider_slider_bb, 1.95, col= alpha("red", 0.5), border="white")
      rect((mean(Element_3)-(standard_error_Element_3 + input$mulch_variance_slider_bb)) + input$mulch_mean_slider_bb, 3.05, (mean(Element_3)+ (standard_error_Element_3 + input$mulch_variance_slider_bb ))+ input$mulch_mean_slider_bb, 2.95, col= alpha("red", 0.5), border="white")
      rect((mean(Element_4)-(standard_error_Element_4 + input$tarweed_and_mulch_variance_slider_bb)) + input$tarweed_and_mulch_mean_slider_bb, 4.05, (mean(Element_4)+ (standard_error_Element_4 + input$tarweed_and_mulch_variance_slider_bb ))+ input$tarweed_and_mulch_mean_slider_bb, 3.95, col= alpha("red", 0.5), border="white")
      
      if(input$control_mean_slider_bb == 0 & input$tarweed_mean_slider_slider_bb == 0 & input$tarweed_mean_slider_slider_bb == 0 & input$tarweed_and_mulch_mean_slider_bb == 0 
         & input$control_variance_slider_bb == 0 & input$tarweed_variance_slider_slider_bb == 0 & input$mulch_variance_slider_bb == 0 & input$tarweed_and_mulch_variance_slider_bb == 0){
        
      points(Element_1,rep(.9,20) ,  col=alpha("blue", 0.5), pch=15, cex=1.5)
      points(Element_2,rep(1.9,20),  col=alpha("blue", 0.5), pch=15, cex=1.5)
      points(Element_3,rep(2.9,20),  col=alpha("blue", 0.5), pch=15, cex=1.5)
      points(Element_4,rep(3.9,20),  col=alpha("blue", 0.5), pch=15, cex=1.5)
      }
      
      lines( rep(mean(Element_1)+ input$control_mean_slider_bb,2), c(.75,1.25), lwd=2)
      lines( rep(mean(Element_2)+ input$tarweed_mean_slider_slider_bb,2) , c(1.75,2.25), lwd=2)
      lines( rep(mean(Element_3)+ input$mulch_mean_slider_bb,2), c(2.75,3.25), lwd=2)
      lines( rep(mean(Element_4)+ input$tarweed_and_mulch_mean_slider_bb,2) , c(3.75,4.25), lwd=2)
      
      
      
      Element_1_b <- rnorm(20, mean= (mean(Element_1)+ input$control_mean_slider_bb), sd = (sd(Element_1) + input$control_variance_slider_bb))
      Element_2_b <- rnorm(20, mean= (mean(Element_2)+ input$tarweed_mean_slider_slider_bb), sd = (sd(Element_2) + input$tarweed_variance_slider_slider_bb))
      Element_3_b <- rnorm(20, mean= (mean(Element_3)+ input$mulch_mean_slider_bb), sd = (sd(Element_3) + input$mulch_variance_slider_bb))
      Element_4_b <- rnorm(20, mean= (mean(Element_4)+ input$tarweed_and_mulch_mean_slider_bb), sd = (sd(Element_4) + input$tarweed_and_mulch_variance_slider_bb))
      
      dater <- dat
      dat[1:20,7] <- Element_1_b
      dat[21:40,7] <- Element_2_b
      dat[41:60,7] <- Element_3_b
      dat[61:80,7] <- Element_4_b
      
      exoticRich_AOV <-aov(exoticRich ~ tarweed + mulch + tarweed:mulch , data=dater)
      exoticRich_AOV_2 <-aov(exoticRich ~ tarweed + mulch + tarweed:mulch , data=dat)
      
      output$console_exotic_richness <- renderPrint({
        if(input$control_mean_slider_bb == 0 & input$tarweed_mean_slider_slider_bb == 0 & input$tarweed_mean_slider_slider_bb == 0 & input$tarweed_and_mulch_mean_slider_bb == 0 
           & input$control_variance_slider_bb == 0 & input$tarweed_variance_slider_slider_bb == 0 & input$mulch_variance_slider_bb == 0 & input$tarweed_and_mulch_variance_slider_bb == 0){capture.output(summary(exoticRich_AOV))}else{capture.output(summary(exoticRich_AOV_2))}
          
        
      })
      
      output$anova_code_data_exotic_rich <- renderTable({ 
        if(input$control_mean_slider_bb == 0 & input$tarweed_mean_slider_slider_bb == 0 & input$tarweed_mean_slider_slider_bb == 0 & input$tarweed_and_mulch_mean_slider_bb == 0 
           & input$control_variance_slider_bb == 0 & input$tarweed_variance_slider_slider_bb == 0 & input$mulch_variance_slider_bb == 0 & input$tarweed_and_mulch_variance_slider_bb == 0){as.matrix(dater[,c(7,2,3)], 3, dater[,c(7,2,3)]) }else{as.matrix(dat[,c(7,2,3)], 3, dat[,c(7,2,3)])} })
      
      
      grand_mean <- mean(dat[,7])
      abline(v=grand_mean, col= alpha("orange", .5), lwd=2)
      
    })
    
    
    output$graph_demo_plot <- renderPlot({
      if(input$graph_element_1_a == "Controls" & input$graph_element_1_b == "exoticCover"){Element_1_pre <-Control$exoticCover}
      if(input$graph_element_1_a == "Controls" & input$graph_element_1_b == "nativeCover"){Element_1_pre <- Control$nativeCover}
      if(input$graph_element_1_a == "Tarweed_protection" & input$graph_element_1_b == "exoticCover"){Element_1_pre <- Tarweed_protection$exoticCover}
      if(input$graph_element_1_a == "Tarweed_protection" & input$graph_element_1_b == "nativeCover"){Element_1_pre <- Tarweed_protection$nativeCover}
      if(input$graph_element_1_a == "Mulch_protection" & input$graph_element_1_b == "exoticCover"){Element_1_pre <- Mulch_protection$exoticCover}
      if(input$graph_element_1_a == "Mulch_protection" & input$graph_element_1_b == "nativeCover"){Element_1_pre <- Mulch_protection$nativeCover}
      if(input$graph_element_1_a == "Tarweed_and_mulch" & input$graph_element_1_b == "exoticCover"){Element_1_pre <- Tarweed_and_mulch$exoticCover}
      if(input$graph_element_1_a == "Tarweed_and_mulch" & input$graph_element_1_b == "nativeCover"){Element_1_pre <- Tarweed_and_mulch$nativeCover}
      
      if(input$graph_element_1_a == "Controls" & input$graph_element_1_b == "exoticRich"){Element_1_pre <-Control$exoticRich}
      if(input$graph_element_1_a == "Controls" & input$graph_element_1_b == "nativeRich"){Element_1_pre <- Control$nativeRich}
      if(input$graph_element_1_a == "Tarweed_protection" & input$graph_element_1_b == "exoticRich"){Element_1_pre <- Tarweed_protection$exoticRich}
      if(input$graph_element_1_a == "Tarweed_protection" & input$graph_element_1_b == "nativeRich"){Element_1_pre <- Tarweed_protection$nativeRich}
      if(input$graph_element_1_a == "Mulch_protection" & input$graph_element_1_b == "exoticRich"){Element_1_pre <- Mulch_protection$exoticRich}
      if(input$graph_element_1_a == "Mulch_protection" & input$graph_element_1_b == "nativeRich"){Element_1_pre <- Mulch_protection$nativeRich}
      if(input$graph_element_1_a == "Tarweed_and_mulch" & input$graph_element_1_b == "exoticRich"){Element_1_pre <- Tarweed_and_mulch$exoticRich}
      if(input$graph_element_1_a == "Tarweed_and_mulch" & input$graph_element_1_b == "nativeRich"){Element_1_pre <- Tarweed_and_mulch$nativeRich}
      
      if(input$graph_element_2_a == "Controls" & input$graph_element_2_b == "exoticCover"){Element_2_pre <-Control$exoticCover}
      if(input$graph_element_2_a == "Controls" & input$graph_element_2_b == "nativeCover"){Element_2_pre <- Control$nativeCover}
      if(input$graph_element_2_a == "Tarweed_protection" & input$graph_element_2_b == "exoticCover"){Element_2_pre <- Tarweed_protection$exoticCover}
      if(input$graph_element_2_a == "Tarweed_protection" & input$graph_element_2_b == "nativeCover"){Element_2_pre <- Tarweed_protection$nativeCover}
      if(input$graph_element_2_a == "Mulch_protection" & input$graph_element_2_b == "exoticCover"){Element_2_pre <- Mulch_protection$exoticCover}
      if(input$graph_element_2_a == "Mulch_protection" & input$graph_element_2_b == "nativeCover"){Element_2_pre <- Mulch_protection$nativeCover}
      if(input$graph_element_2_a == "Tarweed_and_mulch" & input$graph_element_2_b == "exoticCover"){Element_2_pre <- Tarweed_and_mulch$exoticCover}
      if(input$graph_element_2_a == "Tarweed_and_mulch" & input$graph_element_2_b == "nativeCover"){Element_2_pre <- Tarweed_and_mulch$nativeCover}
      
      if(input$graph_element_2_a == "Controls" & input$graph_element_2_b == "exoticRich"){Element_2_pre <-Control$exoticRich}
      if(input$graph_element_2_a == "Controls" & input$graph_element_2_b == "nativeRich"){Element_2_pre <- Control$nativeRich}
      if(input$graph_element_2_a == "Tarweed_protection" & input$graph_element_2_b == "exoticRich"){Element_2_pre <- Tarweed_protection$exoticRich}
      if(input$graph_element_2_a == "Tarweed_protection" & input$graph_element_2_b == "nativeRich"){Element_2_pre <- Tarweed_protection$nativeRich}
      if(input$graph_element_2_a == "Mulch_protection" & input$graph_element_2_b == "exoticRich"){Element_2_pre <- Mulch_protection$exoticRich}
      if(input$graph_element_2_a == "Mulch_protection" & input$graph_element_2_b == "nativeRich"){Element_2_pre <- Mulch_protection$nativeRich}
      if(input$graph_element_2_a == "Tarweed_and_mulch" & input$graph_element_2_b == "exoticRich"){Element_2_pre <- Tarweed_and_mulch$exoticRich}
      if(input$graph_element_2_a == "Tarweed_and_mulch" & input$graph_element_2_b == "nativeRich"){Element_2_pre <- Tarweed_and_mulch$nativeRich}
      
      
     
      
      Element_1 <- Element_1_pre
      Element_2 <-  Element_2_pre
      Element_3 <-  Mulch_protection$nativeCover
      Element_4 <-  Tarweed_and_mulch$nativeCover
      
      Element_1_error <- qt(0.975,df=length(Element_1)-1)* sd(Element_1)/sqrt(length(Element_1))
      Element_1_top_CI <- mean(Element_1) - Element_1_error
      Element_1_bottom_CI <- mean(Element_1) + Element_1_error
      
      Element_2_error <- qt(0.975,df=length(Element_1)-2)* sd(Element_2)/sqrt(length(Element_1))
      Element_2_top_CI <- mean(Element_2) - Element_2_error
      Element_2_bottom_CI <- mean(Element_2) + Element_2_error
      
     
      plot(c(1,1,2,2),c(max(Element_1), min(Element_1), max(Element_2), min(Element_2)), type="n", xaxt="n", xlim=as.vector(input$x_lim_slider), xlab=as.character(input$x_axis_title_a), ylim=as.vector(input$y_lim_slider), ylab= as.character(input$y_axis_title_a), main= as.character(input$main_title_a))
      points(rep(1,20) , Element_1, col=as.character(input$Point_color), pch=input$shape_code_a)
      points(rep(2,20), Element_2, col=as.character(input$Point_color), pch=input$shape_code_a)
      lines(c(1 - input$Mean_line_length, 1 + input$Mean_line_length), rep(mean(Element_1), 2), col=as.character(input$CI_line_color), lty=as.numeric(input$CI_line_type))
      lines(c(2 - input$Mean_line_length, 2 + input$Mean_line_length), rep(mean(Element_2),2 ), col=as.character(input$CI_line_color), lty=as.numeric(input$CI_line_type))
      arrows(1, mean(Element_1), 1, Element_1_top_CI, angle=90, length=input$SE_line_length, col=as.character(input$CI_line_color), lty=as.numeric(input$CI_line_type_a))
      arrows(1, mean(Element_1), 1, Element_1_bottom_CI, angle=90, length=input$SE_line_length, col=as.character(input$CI_line_color), lty=as.numeric(input$CI_line_type_a))
      arrows(2, mean(Element_2), 2, Element_2_top_CI, angle=90, length=input$SE_line_length, col=as.character(input$CI_line_color), lty=as.numeric(input$CI_line_type_a))
      arrows(2, mean(Element_2), 2, Element_2_bottom_CI, angle=90, length=input$SE_line_length, col=as.character(input$CI_line_color), lty=as.numeric(input$CI_line_type_a))
      
      axis(1, at=c(1,2), labels= c(input$left_element_label, input$right_element_label))
      t_out <- t.test(Element_1_pre, Element_2_pre)
      
      
      #legend(-10.5, 7, paste(choice_b1," ",choice_b2," data", sep=""), col = "white", text.col = "black",  pch = 1, bty="n", cex=1.5, pt.cex=4)
      #legend(3.5, .45, paste("P-value = ",format(t_out[[3]], digit=3), sep=""), col = "firebrick3", text.col = "black",  pch = 15, bty="n", cex=1.5, pt.cex=2)
      #legend(3.5, .25, paste("t statistic = ",format(t_out[[1]], digit=3), sep=""), col = "white", text.col = "black",  pch = 1, bty="n", cex=1.5, pt.cex=2)
      #legend(3.5, .65, "Data points", col = "white", text.col = "black",  pch = 1, bty="n", cex=1.5, pt.cex=2)
      #legend(3.5, .85, "Data points", col = "white", text.col = "black",  pch = 1, bty="n", cex=1.5, pt.cex=2)
      #legend(3.5, 1.05, "Data points", col = "white", text.col = "black",  pch = 1, bty="n", cex=1.5, pt.cex=2)
      
    })
    
    
    output$format_graph_output_code <- renderText({paste("
Element_1 <- ", input$graph_element_1_a, "$", input$graph_element_1_b,"
Element_2 <- ", input$graph_element_2_a, "$", input$graph_element_2_b,"

?qt      
Element_1_confidence_interval <- qt(0.975,df=length(Element_1)-1) * 
  sd(Element_1)/sqrt(length(Element_1))

  Element_1_top_CI <- mean(Element_1) + Element_1_confidence_interval
  Element_1_bottom_CI <- mean(Element_1) - Element_1_confidence_interval
                                                           
Element_2_confidence_interval <- qt(0.975,df=length(Element_1)-1) * 
  sd(Element_2)/sqrt(length(Element_1))

  Element_2_top_CI <- mean(Element_2) + Element_2_confidence_interval
  Element_2_bottom_CI <- mean(Element_2) - Element_2_confidence_interval
 
plot(
  c(1,1,2,2),
  c(max(Element_1), min(Element_1), max(Element_2), min(Element_2)), 
  type='n', xaxt='n', 
  main= '",input$main_title_a, "' , 
  xlim= c(",input$x_lim_slider[[1]] ,"," ,input$x_lim_slider[[2]] , "), 
  ylim= c(",input$y_lim_slider[[1]] ,"," ,input$y_lim_slider[[2]] , "),  
  xlab= '", input$x_axis_title_a, "', 
  ylab= '", input$y_axis_title_a, "'
  )
      
  points(rep(1,20), Element_1, col= '",input$Point_color,"', pch= ", input$shape_code_a, ")
  points(rep(2,20), Element_2, col= '", input$Point_color, "', pch= ", input$shape_code_a, ")
      
  lines(c(1 - ", input$Mean_line_length ,", 1 + ", input$Mean_line_length, ") , rep(mean(Element_1), 2), col= '", input$CI_line_color ,"', lty= ", input$CI_line_type ,")
  lines(c(2 - ", input$Mean_line_length ,", 2 + ", input$Mean_line_length, ") , rep(mean(Element_2), 2), col= '", input$CI_line_color ,"', lty= ", input$CI_line_type, ")
      
  arrows(1, mean(Element_1), 1, Element_1_top_CI, 
      angle= 90, length= ", input$SE_line_length ,", col= '", input$CI_line_color ,"', lty= ", input$CI_line_type_a, ")
  arrows(1, mean(Element_1), 1, Element_1_bottom_CI, 
      angle= 90, length= ", input$SE_line_length ,", col= '", input$CI_line_color ,"', lty= ", input$CI_line_type_a ,")
  arrows(2, mean(Element_2), 2, Element_2_top_CI, 
      angle= 90, length= ", input$SE_line_length ,", col= '", input$CI_line_color ,"', lty= ", input$CI_line_type_a ,")
  arrows(2, mean(Element_2), 2, Element_2_bottom_CI, 
      angle= 90, length= ", input$SE_line_length ,", col= '", input$CI_line_color ,"', lty= ", input$CI_line_type_a ,")", sep="")
      })
    
    
    
    
    output$format_graph_output_code_2 <- renderText({paste("
Element_1 <-  ", input$graph_element_1_a, "$", input$graph_element_1_b,"
Element_2 <- ", input$graph_element_2_a, "$", input$graph_element_2_b,"
     
                                                    
Element_1_confidence_interval <- qt(0.975,df=length(Element_1)-1) * 
  sd(Element_1)/sqrt(length(Element_1))
                                                           
   Element_1_top_CI <- mean(Element_1) - Element_1_confidence_interval
   Element_1_bottom_CI <- mean(Element_1) + Element_1_confidence_interval
   
Element_2_confidence_interval <- qt(0.975,df=length(Element_1)-1) * 
sd(Element_2)/sqrt(length(Element_1))
   
   Element_2_top_CI <- mean(Element_2) - Element_2_confidence_interval
   Element_2_bottom_CI <- mean(Element_2) + Element_2_confidence_interval
                                                           
plot(
c(1,1,2,2),
c(max(Element_1), min(Element_1), max(Element_2), min(Element_2)), 
type='n', xaxt='n', 
main= '",input$main_title_a, "', 
xlim= c(",input$x_lim_slider[[1]] ,"," ,input$x_lim_slider[[2]] , "), 
ylim= c(",input$y_lim_slider[[1]] ,"," ,input$y_lim_slider[[2]] , "),  
xlab= '", input$x_axis_title_a, "', 
ylab= '", input$y_axis_title_a, "'
)
      
  points(rep(1,20), Element_1, col= '",input$Point_color,"', pch= ", input$shape_code_a, ")
  points(rep(2,20), Element_2, col= '", input$Point_color, "', pch= ", input$shape_code_a, ")
  
  lines(c(1 - ", input$Mean_line_length ,", 1 + ", input$Mean_line_length, ") , rep(mean(Element_1), 2), col= '", input$CI_line_color ,"', lty= ", input$CI_line_type ,")
  lines(c(2 - ", input$Mean_line_length ,", 2 + ", input$Mean_line_length, ") , rep(mean(Element_2), 2), col= '", input$CI_line_color ,"', lty= ", input$CI_line_type, ")
  
  arrows(1, mean(Element_1), 1, Element_1_top_CI, 
  angle= 90, length= ", input$SE_line_length ,", col= '", input$CI_line_color ,"', lty= ", input$CI_line_type_a, ")
  arrows(1, mean(Element_1), 1, Element_1_bottom_CI, 
  angle= 90, length= ", input$SE_line_length ,", col= '", input$CI_line_color ,"', lty= ", input$CI_line_type_a ,")
  arrows(2, mean(Element_2), 2, Element_2_top_CI, 
  angle= 90, length= ", input$SE_line_length ,", col= '", input$CI_line_color ,"', lty= ", input$CI_line_type_a ,")
  arrows(2, mean(Element_2), 2, Element_2_bottom_CI, 
  angle= 90, length= ", input$SE_line_length ,", col= '", input$CI_line_color ,"', lty= ", input$CI_line_type_a ,")
      
  axis(1, at=c(1,2), labels= c('", input$left_element_label,"','" ,input$right_element_label,"')) ", sep="")
    
    
    })
    
    
    
output$tester_tester <- renderText({ paste("pdf(file = '", input$Enter_pdf_file_name_for_save, ".pdf', width = 11, height = 8.5)", sep="") })
output$tester_tester_b <- renderText({ "dev.off()" })


output$tester_tester_2 <- renderText({ paste("pdf(file = '", input$Enter_pdf_file_name_for_save_2, ".pdf', width = 11, height = 8.5)", sep="") })
output$tester_tester_b_2 <- renderText({ "dev.off()" })
    
    
    
    
    
    
    
    
    
    
    output$full_panel_graph_demo <- renderPlot({
      
      if(input$Four_graph_element_1_a == "Controls" & input$Four_graph_element_1_b == "exoticCover"){Element_1_pre <-Control$exoticCover}
      if(input$Four_graph_element_1_a == "Controls" & input$Four_graph_element_1_b == "nativeCover"){Element_1_pre <- Control$nativeCover}
      if(input$Four_graph_element_1_a == "Tarweed_protection" & input$Four_graph_element_1_b == "exoticCover"){Element_1_pre <- Tarweed_protection$exoticCover}
      if(input$Four_graph_element_1_a == "Tarweed_protection" & input$Four_graph_element_1_b == "nativeCover"){Element_1_pre <- Tarweed_protection$nativeCover}
      if(input$Four_graph_element_1_a == "Mulch_protection" & input$Four_graph_element_1_b == "exoticCover"){Element_1_pre <- Mulch_protection$exoticCover}
      if(input$Four_graph_element_1_a == "Mulch_protection" & input$Four_graph_element_1_b == "nativeCover"){Element_1_pre <- Mulch_protection$nativeCover}
      if(input$Four_graph_element_1_a == "Tarweed_and_mulch" & input$Four_graph_element_1_b == "exoticCover"){Element_1_pre <- Tarweed_and_mulch$exoticCover}
      if(input$Four_graph_element_1_a == "Tarweed_and_mulch" & input$Four_graph_element_1_b == "nativeCover"){Element_1_pre <- Tarweed_and_mulch$nativeCover}
      
      if(input$Four_graph_element_1_a == "Controls" & input$Four_graph_element_1_b == "exoticRich"){Element_1_pre <-Control$exoticRich}
      if(input$Four_graph_element_1_a == "Controls" & input$Four_graph_element_1_b == "nativeRich"){Element_1_pre <- Control$nativeRich}
      if(input$Four_graph_element_1_a == "Tarweed_protection" & input$Four_graph_element_1_b == "exoticRich"){Element_1_pre <- Tarweed_protection$exoticRich}
      if(input$Four_graph_element_1_a == "Tarweed_protection" & input$Four_graph_element_1_b == "nativeRich"){Element_1_pre <- Tarweed_protection$nativeRich}
      if(input$Four_graph_element_1_a == "Mulch_protection" & input$Four_graph_element_1_b == "exoticRich"){Element_1_pre <- Mulch_protection$exoticRich}
      if(input$Four_graph_element_1_a == "Mulch_protection" & input$Four_graph_element_1_b == "nativeRich"){Element_1_pre <- Mulch_protection$nativeRich}
      if(input$Four_graph_element_1_a == "Tarweed_and_mulch" & input$Four_graph_element_1_b == "exoticRich"){Element_1_pre <- Tarweed_and_mulch$exoticRich}
      if(input$Four_graph_element_1_a == "Tarweed_and_mulch" & input$Four_graph_element_1_b == "nativeRich"){Element_1_pre <- Tarweed_and_mulch$nativeRich}
      
      if(input$Four_graph_element_2_a == "Controls" & input$Four_graph_element_1_b == "exoticCover"){Element_2_pre <-Control$exoticCover}
      if(input$Four_graph_element_2_a == "Controls" & input$Four_graph_element_1_b == "nativeCover"){Element_2_pre <- Control$nativeCover}
      if(input$Four_graph_element_2_a == "Tarweed_protection" & input$Four_graph_element_1_b == "exoticCover"){Element_2_pre <- Tarweed_protection$exoticCover}
      if(input$Four_graph_element_2_a == "Tarweed_protection" & input$Four_graph_element_1_b == "nativeCover"){Element_2_pre <- Tarweed_protection$nativeCover}
      if(input$Four_graph_element_2_a == "Mulch_protection" & input$Four_graph_element_1_b == "exoticCover"){Element_2_pre <- Mulch_protection$exoticCover}
      if(input$Four_graph_element_2_a == "Mulch_protection" & input$Four_graph_element_1_b == "nativeCover"){Element_2_pre <- Mulch_protection$nativeCover}
      if(input$Four_graph_element_2_a == "Tarweed_and_mulch" & input$Four_graph_element_1_b == "exoticCover"){Element_2_pre <- Tarweed_and_mulch$exoticCover}
      if(input$Four_graph_element_2_a == "Tarweed_and_mulch" & input$Four_graph_element_1_b == "nativeCover"){Element_2_pre <- Tarweed_and_mulch$nativeCover}
      
      if(input$Four_graph_element_2_a == "Controls" & input$Four_graph_element_1_b == "exoticRich"){Element_2_pre <-Control$exoticRich}
      if(input$Four_graph_element_2_a == "Controls" & input$Four_graph_element_1_b == "nativeRich"){Element_2_pre <- Control$nativeRich}
      if(input$Four_graph_element_2_a == "Tarweed_protection" & input$Four_graph_element_1_b == "exoticRich"){Element_2_pre <- Tarweed_protection$exoticRich}
      if(input$Four_graph_element_2_a == "Tarweed_protection" & input$Four_graph_element_1_b == "nativeRich"){Element_2_pre <- Tarweed_protection$nativeRich}
      if(input$Four_graph_element_2_a == "Mulch_protection" & input$Four_graph_element_1_b == "exoticRich"){Element_2_pre <- Mulch_protection$exoticRich}
      if(input$Four_graph_element_2_a == "Mulch_protection" & input$Four_graph_element_1_b == "nativeRich"){Element_2_pre <- Mulch_protection$nativeRich}
      if(input$Four_graph_element_2_a == "Tarweed_and_mulch" & input$Four_graph_element_1_b == "exoticRich"){Element_2_pre <- Tarweed_and_mulch$exoticRich}
      if(input$Four_graph_element_2_a == "Tarweed_and_mulch" & input$Four_graph_element_1_b == "nativeRich"){Element_2_pre <- Tarweed_and_mulch$nativeRich}
      
      if(input$Four_graph_element_3_a == "Controls" & input$Four_graph_element_1_b == "exoticCover"){Element_3_pre <-Control$exoticCover}
      if(input$Four_graph_element_3_a == "Controls" & input$Four_graph_element_1_b == "nativeCover"){Element_3_pre <- Control$nativeCover}
      if(input$Four_graph_element_3_a == "Tarweed_protection" & input$Four_graph_element_1_b == "exoticCover"){Element_3_pre <- Tarweed_protection$exoticCover}
      if(input$Four_graph_element_3_a == "Tarweed_protection" & input$Four_graph_element_1_b == "nativeCover"){Element_3_pre <- Tarweed_protection$nativeCover}
      if(input$Four_graph_element_3_a == "Mulch_protection" & input$Four_graph_element_1_b == "exoticCover"){Element_3_pre <- Mulch_protection$exoticCover}
      if(input$Four_graph_element_3_a == "Mulch_protection" & input$Four_graph_element_1_b == "nativeCover"){Element_3_pre <- Mulch_protection$nativeCover}
      if(input$Four_graph_element_3_a == "Tarweed_and_mulch" & input$Four_graph_element_1_b == "exoticCover"){Element_3_pre <- Tarweed_and_mulch$exoticCover}
      if(input$Four_graph_element_3_a == "Tarweed_and_mulch" & input$Four_graph_element_1_b == "nativeCover"){Element_3_pre <- Tarweed_and_mulch$nativeCover}
      
      if(input$Four_graph_element_3_a == "Controls" & input$Four_graph_element_1_b == "exoticRich"){Element_3_pre <-Control$exoticRich}
      if(input$Four_graph_element_3_a == "Controls" & input$Four_graph_element_1_b == "nativeRich"){Element_3_pre <- Control$nativeRich}
      if(input$Four_graph_element_3_a == "Tarweed_protection" & input$Four_graph_element_1_b == "exoticRich"){Element_3_pre <- Tarweed_protection$exoticRich}
      if(input$Four_graph_element_3_a == "Tarweed_protection" & input$Four_graph_element_1_b == "nativeRich"){Element_3_pre <- Tarweed_protection$nativeRich}
      if(input$Four_graph_element_3_a == "Mulch_protection" & input$Four_graph_element_1_b == "exoticRich"){Element_3_pre <- Mulch_protection$exoticRich}
      if(input$Four_graph_element_3_a == "Mulch_protection" & input$Four_graph_element_1_b == "nativeRich"){Element_3_pre <- Mulch_protection$nativeRich}
      if(input$Four_graph_element_3_a == "Tarweed_and_mulch" & input$Four_graph_element_1_b == "exoticRich"){Element_3_pre <- Tarweed_and_mulch$exoticRich}
      if(input$Four_graph_element_3_a == "Tarweed_and_mulch" & input$Four_graph_element_1_b == "nativeRich"){Element_3_pre <- Tarweed_and_mulch$nativeRich}
      
      if(input$Four_graph_element_4_a == "Controls" & input$Four_graph_element_1_b == "exoticCover"){Element_4_pre <-Control$exoticCover}
      if(input$Four_graph_element_4_a == "Controls" & input$Four_graph_element_1_b == "nativeCover"){Element_4_pre <- Control$nativeCover}
      if(input$Four_graph_element_4_a == "Tarweed_protection" & input$Four_graph_element_1_b == "exoticCover"){Element_4_pre <- Tarweed_protection$exoticCover}
      if(input$Four_graph_element_4_a == "Tarweed_protection" & input$Four_graph_element_1_b == "nativeCover"){Element_4_pre <- Tarweed_protection$nativeCover}
      if(input$Four_graph_element_4_a == "Mulch_protection" & input$Four_graph_element_1_b == "exoticCover"){Element_4_pre <- Mulch_protection$exoticCover}
      if(input$Four_graph_element_4_a == "Mulch_protection" & input$Four_graph_element_1_b == "nativeCover"){Element_4_pre <- Mulch_protection$nativeCover}
      if(input$Four_graph_element_4_a == "Tarweed_and_mulch" & input$Four_graph_element_1_b == "exoticCover"){Element_4_pre <- Tarweed_and_mulch$exoticCover}
      if(input$Four_graph_element_4_a == "Tarweed_and_mulch" & input$Four_graph_element_1_b == "nativeCover"){Element_4_pre <- Tarweed_and_mulch$nativeCover}
    
      if(input$Four_graph_element_4_a == "Controls" & input$Four_graph_element_1_b == "exoticRich"){Element_4_pre <-Control$exoticRich}
      if(input$Four_graph_element_4_a == "Controls" & input$Four_graph_element_1_b == "nativeRich"){Element_4_pre <- Control$nativeRich}
      if(input$Four_graph_element_4_a == "Tarweed_protection" & input$Four_graph_element_1_b == "exoticRich"){Element_4_pre <- Tarweed_protection$exoticRich}
      if(input$Four_graph_element_4_a == "Tarweed_protection" & input$Four_graph_element_1_b == "nativeRich"){Element_4_pre <- Tarweed_protection$nativeRich}
      if(input$Four_graph_element_4_a == "Mulch_protection" & input$Four_graph_element_1_b == "exoticRich"){Element_4_pre <- Mulch_protection$exoticRich}
      if(input$Four_graph_element_4_a == "Mulch_protection" & input$Four_graph_element_1_b == "nativeRich"){Element_4_pre <- Mulch_protection$nativeRich}
      if(input$Four_graph_element_4_a == "Tarweed_and_mulch" & input$Four_graph_element_1_b == "exoticRich"){Element_4_pre <- Tarweed_and_mulch$exoticRich}
      if(input$Four_graph_element_4_a == "Tarweed_and_mulch" & input$Four_graph_element_1_b == "nativeRich"){Element_4_pre <- Tarweed_and_mulch$nativeRich}
      
      
      
      
      Element_1 <- Element_1_pre
      Element_2 <-  Element_2_pre
      Element_3 <-  Element_3_pre
      Element_4 <-  Element_4_pre
      
      Element_1_error <- qt(0.975,df=length(Element_1)-1)* sd(Element_1)/sqrt(length(Element_1))
      Element_1_top_CI <- mean(Element_1) - Element_1_error
      Element_1_bottom_CI <- mean(Element_1) + Element_1_error
      
      Element_2_error <- qt(0.975,df=length(Element_2)-1)* sd(Element_2)/sqrt(length(Element_2))
      Element_2_top_CI <- mean(Element_2) - Element_2_error
      Element_2_bottom_CI <- mean(Element_2) + Element_2_error
      
      Element_3_error <- qt(0.975,df=length(Element_3)-1)* sd(Element_3)/sqrt(length(Element_3))
      Element_3_top_CI <- mean(Element_3) - Element_3_error
      Element_3_bottom_CI <- mean(Element_3) + Element_3_error
      
      Element_4_error <- qt(0.975,df=length(Element_4)-1)* sd(Element_4)/sqrt(length(Element_4))
      Element_4_top_CI <- mean(Element_4) - Element_4_error
      Element_4_bottom_CI <- mean(Element_4) + Element_4_error
      
      
      plot(c(1,1,2,2,3,3,4,4),c(max(Element_1), min(Element_1), max(Element_2), min(Element_2),max(Element_3), min(Element_3), max(Element_4), min(Element_4)), type="n", xaxt="n", xlim=as.vector(input$Four_x_lim_slider), xlab=as.character(input$Four_x_axis_title_a), ylim=as.vector(input$Four_y_lim_slider), ylab= as.character(input$Four_y_axis_title_a), main= as.character(input$Four_main_title_a))
      points(rep(1,20) , Element_1, col=as.character(input$Four_Point_color), pch=input$Four_shape_code_a)
      points(rep(2,20), Element_2, col=as.character(input$Four_Point_color), pch=input$Four_shape_code_a)
      points(rep(3,20) , Element_3, col=as.character(input$Four_Point_color), pch=input$Four_shape_code_a)
      points(rep(4,20), Element_4, col=as.character(input$Four_Point_color), pch=input$Four_shape_code_a)
      
      lines(c(1 - input$Four_Mean_line_length, 1 + input$Four_Mean_line_length), rep(mean(Element_1), 2), col=as.character(input$Four_CI_line_color), lty=as.numeric(input$Four_CI_line_type))
      lines(c(2 - input$Four_Mean_line_length, 2 + input$Four_Mean_line_length), rep(mean(Element_2),2 ), col=as.character(input$Four_CI_line_color), lty=as.numeric(input$Four_CI_line_type))
      lines(c(3 - input$Four_Mean_line_length, 3 + input$Four_Mean_line_length), rep(mean(Element_1), 2), col=as.character(input$Four_CI_line_color), lty=as.numeric(input$Four_CI_line_type))
      lines(c(4 - input$Four_Mean_line_length, 4 + input$Four_Mean_line_length), rep(mean(Element_2),2 ), col=as.character(input$Four_CI_line_color), lty=as.numeric(input$Four_CI_line_type))
      
      arrows(1, mean(Element_1), 1, Element_1_top_CI, angle=90, length=input$Four_SE_line_length, col=as.character(input$Four_CI_line_color), lty=as.numeric(input$Four_CI_line_type_a))
      arrows(1, mean(Element_1), 1, Element_1_bottom_CI, angle=90, length=input$Four_SE_line_length, col=as.character(input$Four_CI_line_color), lty=as.numeric(input$Four_CI_line_type_a))
      arrows(2, mean(Element_2), 2, Element_2_top_CI, angle=90, length=input$Four_SE_line_length, col=as.character(input$Four_CI_line_color), lty=as.numeric(input$Four_CI_line_type_a))
      arrows(2, mean(Element_2), 2, Element_2_bottom_CI, angle=90, length=input$Four_SE_line_length, col=as.character(input$Four_CI_line_color), lty=as.numeric(input$Four_CI_line_type_a))
      arrows(3, mean(Element_1), 3, Element_3_top_CI, angle=90, length=input$Four_SE_line_length, col=as.character(input$Four_CI_line_color), lty=as.numeric(input$Four_CI_line_type_a))
      arrows(3, mean(Element_1), 3, Element_3_bottom_CI, angle=90, length=input$Four_SE_line_length, col=as.character(input$Four_CI_line_color), lty=as.numeric(input$Four_CI_line_type_a))
      arrows(4, mean(Element_2), 4, Element_4_top_CI, angle=90, length=input$Four_SE_line_length, col=as.character(input$Four_CI_line_color), lty=as.numeric(input$Four_CI_line_type_a))
      arrows(4, mean(Element_2), 4, Element_4_bottom_CI, angle=90, length=input$Four_SE_line_length, col=as.character(input$Four_CI_line_color), lty=as.numeric(input$Four_CI_line_type_a))
      
      
      axis(1, at=c(1,2,3,4), label=c(input$Four_1_element_label,input$Four_2_element_label,input$Four_3_element_label,input$Four_4_element_label))
      
      
    })
    
    
    output$Four_format_graph_output_code <- renderText({paste("
Element_1 <- ", input$Four_graph_element_1_a, "$", input$Four_graph_element_1_b,"
Element_2 <- ", input$Four_graph_element_2_a, "$", input$Four_graph_element_1_b,"
Element_3 <- ", input$Four_graph_element_3_a, "$", input$Four_graph_element_1_b,"
Element_4 <- ", input$Four_graph_element_4_a, "$", input$Four_graph_element_1_b,"

Element_1_confidence_interval <- qt(0.975,df=length(Element_1)-1) * 
sd(Element_1)/sqrt(length(Element_1))

  Element_1_top_CI <- mean(Element_1) + Element_1_confidence_interval
  Element_1_bottom_CI <- mean(Element_1) - Element_1_confidence_interval

Element_2_confidence_interval <- qt(0.975,df=length(Element_2)-1) * 
sd(Element_2)/sqrt(length(Element_2))

  Element_2_top_CI <- mean(Element_2) + Element_2_confidence_interval
  Element_2_bottom_CI <- mean(Element_2) - Element_2_confidence_interval

Element_3_confidence_interval <- qt(0.975,df=length(Element_3)-1) * 
sd(Element_1)/sqrt(length(Element_3))

  Element_3_top_CI <- mean(Element_3) + Element_3_confidence_interval
  Element_3_bottom_CI <- mean(Element_3) - Element_3_confidence_interval

Element_4_confidence_interval <- qt(0.975,df=length(Element_4)-1) * 
sd(Element_4)/sqrt(length(Element_4))

  Element_4_top_CI <- mean(Element_4) + Element_2_confidence_interval
  Element_4_bottom_CI <- mean(Element_4) - Element_2_confidence_interval

plot(
c(1,1,2,2,3,3,4,4),
c(max(Element_1), min(Element_1), max(Element_2), min(Element_2), max(Element_3), min(Element_3), max(Element_4), min(Element_4)), 
type='n', xaxt='n', 
main= '",input$Four_main_title_a, "' , 
xlim= c(",input$Four_x_lim_slider[[1]] ,"," ,input$Four_x_lim_slider[[2]] , "), 
ylim= c(",input$Four_y_lim_slider[[1]] ,"," ,input$Four_y_lim_slider[[2]] , "),  
xlab= '", input$Four_x_axis_title_a, "', 
ylab= '", input$Four_y_axis_title_a, "'
)

points(rep(1,20), Element_1, col= '",input$Point_color,"', pch= ", input$shape_code_a, ")
points(rep(2,20), Element_2, col= '", input$Point_color, "', pch= ", input$shape_code_a, ")
points(rep(3,20), Element_3, col= '",input$Point_color,"', pch= ", input$shape_code_a, ")
points(rep(4,20), Element_4, col= '", input$Point_color, "', pch= ", input$shape_code_a, ")
      
lines(c(1 - ", input$Mean_line_length ,", 1 + ", input$Mean_line_length, ") , rep(mean(Element_1), 2), col= '", input$CI_line_color ,"', lty= ", input$CI_line_type ,")
lines(c(2 - ", input$Mean_line_length ,", 2 + ", input$Mean_line_length, ") , rep(mean(Element_2), 2), col= '", input$CI_line_color ,"', lty= ", input$CI_line_type, ")
lines(c(3 - ", input$Mean_line_length ,", 3 + ", input$Mean_line_length, ") , rep(mean(Element_3), 2), col= '", input$CI_line_color ,"', lty= ", input$CI_line_type ,")
lines(c(4 - ", input$Mean_line_length ,", 4 + ", input$Mean_line_length, ") , rep(mean(Element_4), 2), col= '", input$CI_line_color ,"', lty= ", input$CI_line_type, ")


arrows(1, mean(Element_1), 1, Element_1_top_CI, 
angle= 90, length= ", input$SE_line_length ,", col= '", input$CI_line_color ,"', lty= ", input$CI_line_type_a, ")
arrows(1, mean(Element_1), 1, Element_1_bottom_CI, 
angle= 90, length= ", input$SE_line_length ,", col= '", input$CI_line_color ,"', lty= ", input$CI_line_type_a ,")
arrows(2, mean(Element_2), 2, Element_2_top_CI, 
angle= 90, length= ", input$SE_line_length ,", col= '", input$CI_line_color ,"', lty= ", input$CI_line_type_a ,")
arrows(2, mean(Element_2), 2, Element_2_bottom_CI, 
angle= 90, length= ", input$SE_line_length ,", col= '", input$CI_line_color ,"', lty= ", input$CI_line_type_a ,")
      
arrows(3, mean(Element_3), 3, Element_3_top_CI, 
angle= 90, length= ", input$SE_line_length ,", col= '", input$CI_line_color ,"', lty= ", input$CI_line_type_a, ")
arrows(3, mean(Element_3), 3, Element_3_bottom_CI, 
angle= 90, length= ", input$SE_line_length ,", col= '", input$CI_line_color ,"', lty= ", input$CI_line_type_a ,")
arrows(4, mean(Element_4), 4, Element_4_top_CI, 
angle= 90, length= ", input$SE_line_length ,", col= '", input$CI_line_color ,"', lty= ", input$CI_line_type_a ,")
arrows(4, mean(Element_4), 4, Element_4_bottom_CI, 
angle= 90, length= ", input$SE_line_length ,", col= '", input$CI_line_color ,"', lty= ", input$CI_line_type_a ,")", sep="")
    
    })
    
    
    
    
    output$Four_format_graph_output_code_2 <- renderText({paste("
Element_1 <- ", input$Four_graph_element_1_a, "$", input$Four_graph_element_1_b,"
Element_2 <- ", input$Four_graph_element_2_a, "$", input$Four_graph_element_1_b,"
Element_3 <- ", input$Four_graph_element_3_a, "$", input$Four_graph_element_1_b,"
Element_4 <- ", input$Four_graph_element_4_a, "$", input$Four_graph_element_1_b,"

Element_1_confidence_interval <- qt(0.975,df=length(Element_1)-1) * 
sd(Element_1)/sqrt(length(Element_1))

Element_1_top_CI <- mean(Element_1) + Element_1_confidence_interval
Element_1_bottom_CI <- mean(Element_1) - Element_1_confidence_interval

Element_2_confidence_interval <- qt(0.975,df=length(Element_2)-1) * 
sd(Element_2)/sqrt(length(Element_2))

Element_2_top_CI <- mean(Element_2) + Element_2_confidence_interval
Element_2_bottom_CI <- mean(Element_2) - Element_2_confidence_interval

Element_3_confidence_interval <- qt(0.975,df=length(Element_3)-1) * 
sd(Element_1)/sqrt(length(Element_3))

Element_3_top_CI <- mean(Element_3) + Element_3_confidence_interval
Element_3_bottom_CI <- mean(Element_3) - Element_3_confidence_interval

Element_4_confidence_interval <- qt(0.975,df=length(Element_4)-1) * 
sd(Element_4)/sqrt(length(Element_4))

Element_4_top_CI <- mean(Element_4) + Element_2_confidence_interval
Element_4_bottom_CI <- mean(Element_4) - Element_2_confidence_interval

plot(
c(1,1,2,2,3,3,4,4),
c(max(Element_1), min(Element_1), max(Element_2), min(Element_2), max(Element_3), min(Element_3), max(Element_4), min(Element_4)), 
type='n', xaxt='n', 
main= '",input$Four_main_title_a, "' , 
xlim= c(",input$Four_x_lim_slider[[1]] ,"," ,input$Four_x_lim_slider[[2]] , "), 
ylim= c(",input$Four_y_lim_slider[[1]] ,"," ,input$Four_y_lim_slider[[2]] , "),  
xlab= '", input$Four_x_axis_title_a, "', 
ylab= '", input$Four_y_axis_title_a, "'
)

points(rep(1,20), Element_1, col= '",input$Four_Point_color,"', pch= ", input$Four_shape_code_a, ")
points(rep(2,20), Element_2, col= '", input$Four_Point_color, "', pch= ", input$Four_shape_code_a, ")
points(rep(3,20), Element_3, col= '",input$Four_Point_color,"', pch= ", input$Four_shape_code_a, ")
points(rep(4,20), Element_4, col= '", input$Four_Point_color, "', pch= ", input$Four_shape_code_a, ")

lines(c(1 - ", input$Four_Mean_line_length ,", 1 + ", input$Four_Mean_line_length, ") , rep(mean(Element_1), 2), col= '", input$Four_CI_line_color ,"', lty= ", input$Four_CI_line_type ,")
lines(c(2 - ", input$Four_Mean_line_length ,", 2 + ", input$Four_Mean_line_length, ") , rep(mean(Element_2), 2), col= '", input$Four_CI_line_color ,"', lty= ", input$Four_CI_line_type, ")
lines(c(3 - ", input$Four_Mean_line_length ,", 3 + ", input$Four_Mean_line_length, ") , rep(mean(Element_3), 2), col= '", input$Four_CI_line_color ,"', lty= ", input$Four_CI_line_type ,")
lines(c(4 - ", input$Four_Mean_line_length ,", 4 + ", input$Four_Mean_line_length, ") , rep(mean(Element_4), 2), col= '", input$Four_CI_line_color ,"', lty= ", input$Four_CI_line_type, ")


arrows(1, mean(Element_1), 1, Element_1_top_CI, 
angle= 90, length= ", input$SE_line_length ,", col= '", input$Four_CI_line_color ,"', lty= ", input$Four_CI_line_type_a, ")
arrows(1, mean(Element_1), 1, Element_1_bottom_CI, 
angle= 90, length= ", input$SE_line_length ,", col= '", input$Four_CI_line_color ,"', lty= ", input$Four_CI_line_type_a ,")
arrows(2, mean(Element_2), 2, Element_2_top_CI, 
angle= 90, length= ", input$SE_line_length ,", col= '", input$Four_CI_line_color ,"', lty= ", input$Four_CI_line_type_a ,")
arrows(2, mean(Element_2), 2, Element_2_bottom_CI, 
angle= 90, length= ", input$SE_line_length ,", col= '", input$Four_CI_line_color ,"', lty= ", input$Four_CI_line_type_a ,")

arrows(3, mean(Element_3), 3, Element_3_top_CI, 
angle= 90, length= ", input$SE_line_length ,", col= '", input$Four_CI_line_color ,"', lty= ", input$Four_CI_line_type_a, ")
arrows(3, mean(Element_3), 3, Element_3_bottom_CI, 
angle= 90, length= ", input$SE_line_length ,", col= '", input$Four_CI_line_color ,"', lty= ", input$Four_CI_line_type_a ,")
arrows(4, mean(Element_4), 4, Element_4_top_CI, 
angle= 90, length= ", input$SE_line_length ,", col= '", input$CI_line_color ,"', lty= ", input$Four_CI_line_type_a ,")
arrows(4, mean(Element_4), 4, Element_4_bottom_CI, 
angle= 90, length= ", input$SE_line_length ,", col= '", input$CI_line_color ,"', lty= ", input$CI_line_type_a ,")

axis(1, at=c(1,2,3,4), label=c('", input$Four_1_element_label,"','",input$Four_2_element_label,"','",input$Four_3_element_label,"','",input$Four_4_element_label, "'))
      ", sep="")
    
    
  })
    
    
    
})
    

    



#####

