


display_important_numbers <- function(sea_name, sea.T_name, sea.D_name, sea.DT_name){
	
	par(mar=c(0,0,0,0))

files.available <- c(sea_name, sea.D_name, sea.T_name, sea.DT_name )

##### parse file names to retrieve simulation parameter info ########################
  split.file.name <- strsplit(files.available, split = "_")   #split file name everywhere there is and underscore
  
  
  positions <- c(8, 10) # 
  data.result <- data.frame(matrix(ncol = 2, nrow = length(files.available)))
  
  
  for (i in 1:length(positions)) {
    data.result[, i + 1] <- sapply(split.file.name, "[", positions[i])
  }
head(data.result)

	blankplot(c(0,0), c(0,0))
	mtext("Results Dashboard", side=1, line=-15, adj=0)
	mtext("Developed by Ty Tuff, Bruno Vilela, ", side=1, line=-13, adj=0, col="darkgrey")
	mtext("and Carlos Botero at wustl.edu", side=1, line=-11.5, adj=0, col="darkgrey")
	mtext(paste0(format(Sys.time(), format="%d %B %Y")), side=1, line=-9.5, adj=0)
	mtext("1254      Number of tips", side=1, line=-7, adj=0)
	mtext(paste0(as.character(min(data.result[,3])), "        Replicates"), side=1, line=-5, adj=0)
	mtext(paste0(as.character(data.result[1,2]), "     Timesteps"), side=1, line=-3, adj=0)
	
	
	
	
	}
	
	
	
#display_important_numbers(sea_name, sea.T_name, sea.D_name, sea.DT_name)