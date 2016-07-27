# Model with figures hehehe Awesome!

RunSim.fig <- function(myWorld, P.extinction, P.speciation, 
                       P.diffusion, P.Arisal, P.TakeOver, nbs,
                       N.steps = 250, multiplier = 1.3, start = NULL) {
  # myWorld = The hexagonal world created with the function BuildWorld
  # P.extinction = Probability matrix of extinction
  # P.speciation = Probability matrix of speciation
  # P.diffusion = Probability matrix of diffusion
  # P.Arisal = Probability matrix of arisal
  # P.TakeOver = Probability matrix of takeover
  # N.steps = Number of steps in the model
  # multiplier = The number that will multiply the probabilities according 
  # to environmetal fitness. 
  # start = the point ID in 'myWorld' that will give risen to humans. 
  # (humans origin will be in one of the existing positions)
  
  
  
  world.size <- nrow(myWorld)
  # Initialize parameters we will use later to build the phylogeny
  rootnode <-  world.size + 1 # standard convention for root node number
  
  # set the seed for simulation
  if (is.null(start)) {
    start <- sample(1:world.size, 1)
  }
  
  myWorld[start, 4:6] <- c(0, 0, 1) # Setting root(0), time(0), ancestral(1, forager)
  
  # Keep track of the tip numbers for each position in myWorld (when colonized)
  NodeData <- matrix(c(rootnode, start), 1, 2)
  colnames(NodeData) <- c('Node', 'Tip') 
  
  mytree <- NULL # Empty tree 
  myT <- 0 # Time starts at zero
  
  # Common input and output for all the internal modules
  input <- list(P.speciation, P.Arisal, P.diffusion, P.extinction, P.TakeOver,
                myWorld, mytree, NodeData, myT, multiplier, nbs) 
  
  # Functions order to be randomized
  rand_order_func_run <- list("Extinction", "Diffusion", "SpeciationTakeOver", "Arisal")
  
  cat("0% [") # Time count
  
  for (steps in 1:N.steps) { # Starts the loop with 'n' steps
    
    if (steps %% (N.steps / 10) == 0) { # Time count
      cat('-') # Time count
    }# Time count
    if (steps == N.steps) { # Time count
      cat("] 100 %\n")# Time count
    }# Time count
    
    # Randomize functions order
    rand_order <- sample(rand_order_func_run)
    # Run the functions
    input <- do.call(rand_order[[1]], list(input = input))
    plot.fig(input)
    input <- do.call(rand_order[[2]], list(input = input))
    plot.fig(input)
    input <- do.call(rand_order[[3]], list(input = input))
    plot.fig(input)
    input <- do.call(rand_order[[4]], list(input = input))
    plot.fig(input)
    Sys.sleep(.5)  
  }
  # Trunsform the input/output into the final result and return it
  myWorld <- input[[6]]
  mytree <- input[[7]]
  NodeData <- input[[8]]
  return(list('mytree' = mytree, 'NodeData' = NodeData, 'myWorld' = myWorld))
}

plot.fig <- function(input) {
  myWorld <- input[[6]]
  coords <- myWorld[, 2:3]
  nbs <- input[[11]]
  myT <- floor(input[[9]])
  tiff(file = paste0("figures/", myT, "_", sample(1:100000, 1), ".tiff"))
  map()
  mtext(text = myT)
  plot(nbs, coords, add = TRUE, col = "gray80", lty = 3)
  col1 <- rgb(1, 0, 0, .5)
  col2 <- rgb(0, 0, 1, .5)
  points(coords, col = c("blue", "red")[myWorld[, 7]])
  points(coords, col = c(col2, col1)[myWorld[, 6]], pch = 20)
  dev.off()
}

sub <- sample(1:nrow(coords), 200) # subsample (remove when running for all)
myWorld <- BuildWorld(coords[sub, ], conds[sub, ])
nbs <- knn2nb(knearneigh(coords[sub, ], k = 7, longlat = TRUE),
              sym = TRUE) # 7 symmetric neighbors
map()
plot(nbs, coords[sub, ], add = TRUE, col = "gray80", lty = 3)
points(coords[sub, ], col = c("blue", "red")[myWorld[, 7]])
myOut <- RunSim.fig(myWorld, P.extinction, P.speciation, 
                    P.diffusion, P.Arisal, P.TakeOver, nbs,
                    N.steps = 250, multiplier = 2, start = NULL) 