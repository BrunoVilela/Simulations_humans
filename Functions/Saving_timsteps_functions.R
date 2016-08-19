# Run the simulation function skiping the erros and atributing NA if it occurs
RunSimUltimate2 <- function(myWorld, P.extinction, P.speciation,
                            P.diffusion, P.Arisal, P.TakeOver, nbs, independent,
                            N.steps = 250, multiplier = 1.3,
                            silent = TRUE, resolution = 100,
                            replicate_cycle, combo_number,
                            number_of_time_steps, prob_choose_a) {

  result <- try(RunSim2(myWorld, P.extinction, P.speciation,
                        P.diffusion, P.Arisal, P.TakeOver, nbs,
                        independent, N.steps,
                        multiplier, resolution = resolution,
                        replicate_cycle = replicate_cycle,
                        combo_number = combo_number,
                        number_of_time_steps = number_of_time_steps,
                        prob_choose_a = prob_choose_a), silent = silent)
  if (class(result) == "try-error") {
    result <- NA
  }
  return(result)
}



#==================================================================
RunSim2 <- function(myWorld, P.extinction, P.speciation,
                    P.diffusion, P.Arisal, P.TakeOver, nbs, independent,
                    N.steps = 250, multiplier = 1.3, start = NULL,
                    resolution = 100, replicate_cycle,
                    combo_number, number_of_time_steps,
                    prob_choose_a) {
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
  folder <- paste0("big world cluster outputs/bytime/myOut_replicate_",
                   formatC(replicate_cycle, width = 2,flag = 0),
                   "_combination_",
                   formatC(combo_number, width = 2,flag = 0),
                   "_","parameters", "_P.speciation_",
                   paste(P.speciation, collapse="_"), "_P.extinction_",
                   paste(P.extinction, collapse="_"), "_P.diffusion_",
                   paste(P.diffusion, collapse="_"), "_P.TakeOver_",
                   paste(P.TakeOver, collapse="_"),"_P.Arisal_",
                   prob_choose_a,
                   "_timesteps_", number_of_time_steps)
  dir.create("big world cluster outputs/bytime", showWarnings = FALSE)
  dir.create(folder)
  world.size <- nrow(myWorld)
  # Initialize parameters we will use later to build the phylogeny
  rootnode <-  world.size + 1 # standard convention for root node number

  # set the seed for simulation
  if (is.null(start)) {
    start <- sample(1:world.size, 1)
  }

  myWorld[start, 4:6] <- c(0, 0, 1) # Setting root(0), time(0), ancestral(1, forager)

  mytree <- TheOriginOfSpecies(world.size, start) # Empty tree
  myT <- 0 # Time starts at zero

  # Common input and output for all the internal modules
  input <- list(P.speciation, P.Arisal, P.diffusion, P.extinction, P.TakeOver,
                myWorld, mytree, myT, multiplier, nbs, independent)

  # Functions order to be randomized
  rand_order_func_run <- list("Extinction", "Diffusion", "SpeciationTakeOver", "Arisal")

  cat("0% [") # Time count

  for (steps in 1:N.steps) { # Starts the loop with 'n' steps

    if (steps %% round((N.steps / 10)) == 0) { # Time count
      cat('-') # Time count
    }# Time count
    if (steps == N.steps) { # Time count
      cat("] 100 %\n")# Time count
    }# Time count

    # Randomize functions order
    rand_order <- sample(rand_order_func_run)
    # Run the functions
    input <- do.call(rand_order[[1]], list(input = input))
    input <- do.call(rand_order[[2]], list(input = input))
    input <- do.call(rand_order[[3]], list(input = input))
    input <- do.call(rand_order[[4]], list(input = input))

    # Save
    if(steps %% resolution == 0) {
      myWorld <- as.data.frame(input[[6]])
      myWorld[, 8] <- paste0("t", myWorld[, 8])
      mytree <- makePhy(input[[7]])
      myOut <- list('mytree' = mytree, 'myWorld' = myWorld)
      save(myOut, file= paste0(folder,"/", steps, ".Rdata"))
    }
  }
  # Trunsform the input/output into the final result and return it
  myWorld <- as.data.frame(input[[6]])
  myWorld[, 8] <- paste0("t", myWorld[, 8])
  mytree <- makePhy(input[[7]])
  return(list('mytree' = mytree, 'myWorld' = myWorld))
}
