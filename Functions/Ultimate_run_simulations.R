# Run the simulation function skiping the erros and atributing NA if it occurs
RunSimUltimate <- function(myWorld, P.extinction, P.speciation, 
                           P.diffusion, P.Arisal, P.TakeOver, nbs,
                           N.steps = 250, multiplier = 1.3, 
                           silent = TRUE) {
  
  result <- try(RunSim(myWorld, P.extinction, P.speciation, 
                       P.diffusion, P.Arisal, P.TakeOver, nbs, N.steps,
                       multiplier), silent = silent)
  if (class(result) == "try-error") {
    result <- NA
  }
  return(result)
}
