#' Set seed, loads packages and detach variables
#' @return Nothing
#' @export
cmc_initialise <- function()
{
# set version and seed
set.seed(1)

# load functions
#require(maxLik)
#require(sandwich)
#require(numDeriv)
#require(randtoolbox)
#require(parallel)
#require(matrixStats)
#require(rgenoud)

# Detach everything that is not a package, tools, Autoloads or a couple more
doDetach <- !grepl("^(.GlobalEnv|package:|tools:|Autoloads|CheckExEnv|TempEnv)", search())
doDetach <- (search())[which(doDetach)]
if(length(doDetach)>0) for(i in 1:length(doDetach)) detach(pos=(which(doDetach[i]==search()))[1])
  
# switch off file writing if in use
if(sink.number()>0) sink()
}
