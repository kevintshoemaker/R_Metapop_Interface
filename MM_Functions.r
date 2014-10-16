
#### NOTES: 

###   KTS: how does predRMax interact with effConst??   Do we need a predRMax term???
####     maybe a ceiling model makes sense? if ferret density exceeds a threshold such that territoriality ensues, then limit reproduction
####     based on the number of reproductive ferrets that can produce given the available area. 


#### KTS: need to make sure we have a good ricker density dependence implementation for the metamodeling where vital rates are modified... 

################################
#      MODIFIERS (for quick reference)

#  ChangeAbund <<- FALSE
#  ChangeVital <<- FALSE
#  ChangeK     <<- FALSE   
#  ChangeDisp  <<- FALSE
#  Timestep    <<-  0   
#  Complete    <<- TRUE
#  Active      <<- TRUE
#  Abund       <<- array(0,dim=c(GlobalVars[[ID+1]]$nPopulations,GlobalVars[[ClientID+1]]$nStages))
#  Vital       <<- array(0,dim=c(GlobalVars[[ClientID+1]]$nStages,GlobalVars[[ClientID+1]]$nStages)) # array(0.1,dim=c(GlobalVars[[ClientID+1]]$nPopulations,GlobalVars[[ClientID+1]]$nStages,GlobalVars[[ClientID+1]]$nStages))
#  K           <<- rep(0,times=GlobalVars[[ClientID+1]]$nPopulations)
#  Disp        <<- numeric(GlobalVars[[ClientID+1]]$nPopulations)


###############################
#      STATE VARS (available for use in modifying Metapop parameters)

# STATE VARS, GLOBAL
# nPopulations: Number of populations (scalar) (must be read in each year)
# nStages: Number of stages (scalar)
# nYears: Number of years
# constraintsMat: Constraints matrix (matrix of dimension "nStages", "nStages")
# stageMass: Stage-specific average mass (vector of length "nStages")
# stageBreed: Stage-specific proportion breeding (vector of length "nStages")
# stageRelDisp: Stage-specific relative dispersal (vector of length "nStages")
# stageDD: Basis for density dependence (vector of length "nStages")
# nCatastrophes: Number of defined catastrophes (scalar)
# catType: Indicates whether catastrophe affects survival, abundance, or K (Boolean array of dimension "nCatastrophes", 4 [4 types of catastrophe])
# catStages: Stage-specific catastrophe multiplier (array of dimension "nCatastrophes","nStages")

# STATE VARS, POPULATION
# popAbundTot: Total abundance for each population (vector of dimension "nPopulations")
# popAbundFem: Total number of adult females (vector of dimension "nPopulations")
# popAbundMale: Total number of adult males (vector of dimension "nPopulations")
# PBM: percent breeders that have mates (vector of dimension "nPopulations") 
# disperserMat: Disperser matrix (array of dimension "nPopulations", "nPopulations", "nStages")
# dispersalMat: Dispersal matrix (matrix of dimension "nPopulations", "nPopulations")
# NDF: correlated standard normal deviate for fecundity (vector of length "nPopulations") 
# NDS: correlated standard normal deviate for survival (vector of length "nPopulations")
# NDK: correlated standard normal deviate for carrying capacity (vector of length "nPopulations")
# popEmigrants: Number of emigrants (array of dimension "nPopulations", "nStages")
# popImmigrants: Number of immigrants (array of dimension "nPopulations", "nStages")
# popCatStatus: Catastrophe status (array of dimension "nPopulations", 2)
# popTimeSince: Time since last catastrophe (array of dimension "nPopulations", 2)
# popCatProb: Catastrophe probability (vector of length "nPopulations")
# popRmax: Maximum growth rate (vector of length "nPopulations") 
# popHarvest: number of individuals harvested (array of dimension "nPopulations", "nStages")
# popSupplement:  number of individuals supplemented (array of dimension "nPopulations", "nStages")
# popK2: actual or realized K (with stochasticity).
# popStMat2: Realized transition matrices, including stochasticity …
# popAbundSt: Stage-specific abundances for each population (matrix of dimension "nPopulations","nStages")  [[modifications result in specification of "abundance modifier" ]]
# popStMat: Mean transition matrices for each population (array of dimension "nPopulations", "nStages", "nStages")   [[modifications result in specification of "vital rate modifier"]]
# popK: Mean population-specific carrying capacity (vector of length "nPopulations").  [[modifications result in specification of "K modifier"]]
# relDispersal: Relative dispersal rate (matrix of dimension "nPopulations", "nStages") [[modifications result in specification of "dispersal modifier"]]


##############  OTHER PARAMS
# CurTimeStep[ClientID+1]: Current Time Step for each active client model




#####################################
####################################
########    LOAD USER-DEFINED GLOBAL VARIABLES
## note: ultimately these variables will be defined in MP manager...
## should build test modules into mp manager to see if user-defined functions work, assess expected interactions at time 0, find errors, etc..

   # debug flag
DEBUG = TRUE     # if true, writes out lots of messages to the dump file

 # // PreyRmax (max growth rate) and PreyKcap are assumed to be those when there is no predation
PreyRmax <- 2.43      # Rmax for prey  (no predation)
PreyKcap <- 500000      #  K for prey (no predation)

PredRmax <- 1.9    # Rmax for predator population when glutted with prey resources. 
PredKcap <- 1000      # predator carrying capacity, when glutted with prey resources. 

stageMatrices <- list()    # initialize storage structure for all stage matrices
SMLambdas <- list()
 
#maxSurvival <- 0.99   # Maximum survival rate


##################################
#   PRE-COMPUTE A SERIES OF PLAUSIBLE STAGE MATRICES WITH LAMBDA VALS.... 
    

# PreyLambdas <- numeric(0)      # returns: a list of stage matrices along with corresponding lambdas...
# PreyStMats <- list()
# PredLambdas <- numeric(0)
# PredStMats <- list()

# SPECIFY MEAN, MIN, and MAX vital rates for predator and prey (prey=prairie dog and predator = black-footed ferret)

sink("MinPreyMat.csv")
cat("
	0.050,	0.100,	0.100,	0.100,	0.100,	0.100,	0.100,	0.100,	0,		0,		0,		0,		0,		0	
	0.500,	0,		0,		0,		0,		0,		0,		0,		0,		0,		0,		0,		0,		0
	0,		0.500,	0,		0,		0,		0,		0,		0,		0,		0,		0,		0,		0,		0
	0,		0,		0.500,	0,		0,		0,		0,		0,		0,		0,		0,		0,		0,		0
	0,		0,		0,		0.400,	0,		0,		0,		0,		0,		0,		0,		0,		0,		0
	0,		0,		0,		0,		0.300,	0,		0,		0,		0,		0,		0,		0,		0,		0
	0,		0,		0,		0,		0,		0.150,	0,		0,		0,		0,		0,		0,		0,		0
	0,		0,		0,		0,		0,		0,		0.100,	0,		0,		0,		0,		0,		0,		0
	0.040,	0.100,	0.100,	0.100,	0.100,	0.100,	0.100,	0.100,	0,		0,		0,		0,		0,		0
	0,		0,		0,		0,		0,		0,		0,		0,		0.400,	0,		0,		0,		0,		0
	0,		0,		0,		0,		0,		0,		0,		0,		0,		0.300,	0,		0,		0,		0
	0,		0,		0,		0,		0,		0,		0,		0,		0,		0,		0.300,	0,		0,		0
	0,		0,		0,		0,		0,		0,		0,		0,		0,		0,		0,		0.200,	0,		0
	0,		0,		0,		0,		0,		0,		0,		0,		0,		0,		0,		0,		0.100,	0
")
sink()
MinPreyMat <- as.matrix(read.csv("MinPreyMat.csv",header=F))

sink("BaselinePreyMat.csv")
cat("
	0.209,	0.418,	0.418,	0.418,	0.418,	0.418,	0.418,	0.418,	0,		0,		0,		0,		0,		0	
	0.729,	0,		0,		0,		0,		0,		0,		0,		0,		0,		0,		0,		0,		0
	0,		0.772,	0,		0,		0,		0,		0,		0,		0,		0,		0,		0,		0,		0
	0,		0,		0.723,	0,		0,		0,		0,		0,		0,		0,		0,		0,		0,		0
	0,		0,		0,		0.606,	0,		0,		0,		0,		0,		0,		0,		0,		0,		0
	0,		0,		0,		0,		0.451,	0,		0,		0,		0,		0,		0,		0,		0,		0
	0,		0,		0,		0,		0,		0.282,	0,		0,		0,		0,		0,		0,		0,		0
	0,		0,		0,		0,		0,		0,		0.127,	0,		0,		0,		0,		0,		0,		0
	0.189,	0.379,	0.379,	0.379,	0.379,	0.379,	0.379,	0.379,	0,		0,		0,		0,		0,		0
	0,		0,		0,		0,		0,		0,		0,		0,		0.528,	0,		0,		0,		0,		0
	0,		0,		0,		0,		0,		0,		0,		0,		0,		0.500,	0,		0,		0,		0
	0,		0,		0,		0,		0,		0,		0,		0,		0,		0,		0.428,	0,		0,		0
	0,		0,		0,		0,		0,		0,		0,		0,		0,		0,		0,		0.300,	0,		0
	0,		0,		0,		0,		0,		0,		0,		0,		0,		0,		0,		0,		0.127,	0
")
sink()
BaselinePreyMat <- as.matrix(read.csv("BaselinePreyMat.csv",header=F))

		 ### Mean prey vital rates, at low conspecific densities, few predators
sink("MaxPreyMat.csv")
cat("
	1.10,	2.35,	2.35,	2.35,	2.35,	2.35,	2.35,	2.35,	0,		0,		0,		0,		0,		0	
	0.850,	0,		0,		0,		0,		0,		0,		0,		0,		0,		0,		0,		0,		0
	0,		0.950,	0,		0,		0,		0,		0,		0,		0,		0,		0,		0,		0,		0
	0,		0,		0.94,	0,		0,		0,		0,		0,		0,		0,		0,		0,		0,		0
	0,		0,		0,		0.900,	0,		0,		0,		0,		0,		0,		0,		0,		0,		0
	0,		0,		0,		0,		0.750,	0,		0,		0,		0,		0,		0,		0,		0,		0
	0,		0,		0,		0,		0,		0.400,	0,		0,		0,		0,		0,		0,		0,		0
	0,		0,		0,		0,		0,		0,		0.150,	0,		0,		0,		0,		0,		0,		0
	1.0,	2.0,	2.0,	2.0,	2.0,	2.0,	2.0,	2.0,	0,		0,		0,		0,		0,		0
	0,		0,		0,		0,		0,		0,		0,		0,		0.900,	0,		0,		0,		0,		0
	0,		0,		0,		0,		0,		0,		0,		0,		0,		0.900,	0,		0,		0,		0
	0,		0,		0,		0,		0,		0,		0,		0,		0,		0,		0.800,	0,		0,		0
	0,		0,		0,		0,		0,		0,		0,		0,		0,		0,		0,		0.500,	0,		0
	0,		0,		0,		0,		0,		0,		0,		0,		0,		0,		0,		0,		0.150,	0
")
sink()
MaxPreyMat <- as.matrix(read.csv("MaxPreyMat.csv",header=F))


##########################################
### PREDATOR VITAL RATES...

sink("MinPredMat.csv")            # note: should match predRnoprey... that is, these parameters should reflect absence of prey
cat("
	0.05,	0.10,	0.10,	0.10,	0.000,	0,		0,		0,		0,		0		
	0.05,	0,		0,		0,		0,		0,		0,		0,		0,		0
	0,		0.1,	0,		0,		0,		0,		0,		0,		0,		0
	0,		0,		0.2,	0,		0,		0,		0,		0,		0,		0
	0,		0,		0,		0.2,	0,		0,		0,		0,		0,		0
	0.05,	0.10,	0.10,	0.10,	0.000,	0,		0,		0,		0,		0
	0,		0,		0,		0,		0,		0.05,	0,		0,		0,		0
	0,		0,		0,		0,		0,		0,		0.1,	0,		0,		0
	0,		0,		0,		0,		0,		0,		0,		0.2,	0,		0
	0,		0,		0,		0,		0,		0,		0,		0,		0.2,	0
")
sink()
MinPredMat <- as.matrix(read.csv("MinPredMat.csv",header=F))

sink("BaselinePredMat.csv")
cat("
	0.73,	1.25,	1.25,	1.25,	0.000,	0,		0,		0,		0,		0		
	0.390,	0,		0,		0,		0,		0,		0,		0,		0,		0
	0,		0.670,	0,		0,		0,		0,		0,		0,		0,		0
	0,		0,		0.670,	0,		0,		0,		0,		0,		0,		0
	0,		0,		0,		0.670,	0,		0,		0,		0,		0,		0
	0.73,	1.25,	1.25,	1.25,	0.000,	0,		0,		0,		0,		0
	0,		0,		0,		0,		0,		0.390,	0,		0,		0,		0
	0,		0,		0,		0,		0,		0,		0.670,	0,		0,		0
	0,		0,		0,		0,		0,		0,		0,		0.670,	0,		0
	0,		0,		0,		0,		0,		0,		0,		0,		0.670,	0
")
sink()
BaselinePredMat <- as.matrix(read.csv("BaselinePredMat.csv",header=F))

sink("MaxPredMat.csv")          # note: should match Rmax for predator...
cat("
	1.1,	1.9,	1.9,	1.9,	0.000,	0,		0,		0,		0,		0		
	0.50,	0,		0,		0,		0,		0,		0,		0,		0,		0
	0,		0.80,	0,		0,		0,		0,		0,		0,		0,		0
	0,		0,		0.75,	0,		0,		0,		0,		0,		0,		0
	0,		0,		0,		0.7,	0,		0,		0,		0,		0,		0
	1.1,	1.9,	1.9,	1.9,	0.000,	0,		0,		0,		0,		0
	0,		0,		0,		0,		0,		0.50,	0,		0,		0,		0
	0,		0,		0,		0,		0,		0,		0.80,	0,		0,		0
	0,		0,		0,		0,		0,		0,		0,		0.75,	0,		0
	0,		0,		0,		0,		0,		0,		0,		0,		0.7,	0
")
sink()
MaxPredMat <- as.matrix(read.csv("MaxPredMat.csv",header=F))
 
decimals <- 3    # note: takes a very long time with three decimal places...

global_env <-  .GlobalEnv  #parent.frame()

##################################
#  PREDATOR PREY INTERACTION PARAMETERS

PredRnoprey <- 0.01   # "Rmax" for predator (no prey available, must be less than 1 but greater than zero)

 # # // alpha: slope of the function at origin; the rate at which the prey population is made available
 # # //        to the predator population: the prey death rate due to predation cannot exceed alpha
alpha <-  0.4      # maximum proportion of the prey population that can be consumed (per time step) when predators far outnumber prey

 # # // htime: handling time; 1/asymptote
MaxKill   <- 1000     # asymptote of prey killed per predator (killed, but not necessarily eaten and used for reproduction)
htime <-  1 / MaxKill      #  "handling time": mean time spent between kills when prey abundance greatly exceeds predator abundance.    

                     # KTS: we should really turn this into a fecundity-related term. Literally, how many offspring can be generated
					        # at different consumption levels. Alternatively, we could generate a series of stage matrices
							# representing low and high survivals/fecundities representing various consumption levels.
EffConst <- 0.01   #   0.02  # this term translates consumption rates (functional response) to predator lambda. That is, converts the functional response to a numerical response.  

 # #Rows = Predator populations, Cols = Prey Populations, percent of that pred pop that feed on that prey pop
 # #PredFeed <- matrix(0, nrow=GlobalVars[[2]]$nPopulations, ncol=GlobalVars[[1]]$nPopulations)
PredFeed <- matrix(0,nrow=1,ncol=1)
PredFeed[1,1] <- 1
 #PredFeed[1,2] <- .6
 #PredFeed[2,1] <- 0
 # #PredFeed[2,2] <- 1

 # #Rows = Prey Populations, Cols = Predator populations, percent of that prey pop that pred pop has access to
 # #PreyAccess <- matrix(0, nrow=GlobalVars[[1]]$nPopulations, ncol=GlobalVars[[2]]$nPopulations)
PreyAccess <- matrix(0,nrow=1,ncol=1)
PreyAccess[1,1] <- 1
 # #PreyAccess[1,2] <- 0
 # #PreyAccess[2,1] <- .7
 # #PreyAccess[2,2] <- .3

# QuasiExtinction_prey <- 100    # not used at present
# QuasiExtinction_pred <- 15



####################################
#######  TOLERANCE PARAMETERS
exp_biggest     <- 15.0
pow_biggest     <- 2.6e10
tol             <- 1e-3   # how close to an integer do you need to be?


####################################
########  STORE KEY INPUT PARAMS TO FILE

datetime <- format(Sys.time(), "%a %b %d %Y %H%M")
filenameParams <- paste("Params_BFFTest1",datetime,".csv",sep="")
temp <- data.frame(
  PreyRmax = PreyRmax,     # Rmax for prey  (no predation)
  PreyKcap = PreyKcap,     #  K for prey (no predation) 
  PredRmax = PredRmax,  # Rmax for predator population when glutted with prey resources. 
  PredKcap = PredKcap,     # predator carrying capacity, when glutted with prey resources. 
  PredRnoprey = PredRnoprey,
  alpha = alpha,      # maximum proportion of the prey population that can be consumed (per time step) when predators far outnumber prey
  MaxKill = MaxKill,     # asymptote of prey killed per predator (killed, but not necessarily eaten and used for reproduction)
  EffConst = EffConst   #   0.02  # this term translates consumption rates (functional response) to predator lambda. That is, converts the functional response to a numerical response.  
)
suppressWarnings(write.table(temp,filenameParams,sep=",",row.names=FALSE,col.names=TRUE))

###########################################
#####    ANCILLARY FUNCTIONS (e.g., functional response equations that can be reused)

#################################
#  FUNCTIONAL RESPONSE

# // Ratio-Dependent Functional Response (based on Equation 1.14 in Arditi & Ginzburg 2012)
# // Functional response is the number of prey killed per predator in this time period (one year).

RDFuncResp <- function(PreyPop,PredPop,alpha,htime){
     if((PreyPop==0)|(PredPop==0)){       # if abundance is equal to zero for either predator or prey, then set # killed to zero
          FuncResp <- 0
     } else {
          FuncResp <- (alpha * PreyPop) / (PredPop + alpha * htime * PreyPop)
     }
     return(FuncResp)
}


###################################
########     BASIC ERROR CHECKING

# // CHECK PARAMETERS
# If any of the following is <0 then it is an error condition: 
# PreyPop, PredPop,      // population sizes at current time step
# PreyRmax, PreyKcap,    // prey density dependence (Rmax & K in the absence of predator)
# PredRmax, PredKcap,    // predator density dependence (not quite: Rmax is an optional maximum)
# alpha, htime,          // functional response parameters
# PredRnoprey, EffConst, // for the predator equation (see below)
# PreyMatrix, PREDmatrix // stage matrices for prey and predator; eigenvalues must be 1.0

# CONVENTION: for fatal errors, global env. var. "flag" = TRUE and an informative message gets appended to text string "ErrMsg"

# CONVENTION: for non-fatal errors (warnings), informative messages are appended to a file named "MMdump.txt" in the working directory

CheckParams1 <- function(){
     if((PreyRmax<0)|(PredRmax<0)|(PreyKcap<0)|(PredKcap<0)){
          flag <<- TRUE
          ErrMsg <<- paste(ErrMsg," : ","Key global Rmax or Kcap parameter is less than zero",sep=" ")
		  write(ErrMsg,file=Dumpfile,sep="\n",append=T)
     }
     if((alpha<0)|(htime<0)){
          flag <<- TRUE
          ErrMsg <<- paste(ErrMsg," : ","Key global functional response parameter is less than zero",sep=" ")
		  write(ErrMsg,file=Dumpfile,sep="\n",append=T)
     }
     if((PredRnoprey<0)|(EffConst<0)){
          flag <<- TRUE
          ErrMsg <<- paste(ErrMsg," : ","Key predator functional response parameter is less than zero",sep=" ")
		  write(ErrMsg,file=Dumpfile,sep="\n",append=T)
     }
     
     if(PredRnoprey >= 1){
          flag <<- TRUE
          ErrMsg <<- paste(ErrMsg," : ","predator pop must decline when there is no prey",sep="")
		  write(ErrMsg,file=Dumpfile,sep="\n",append=T)
     }
     
     if(PreyRmax <= 1){
          flag <<- TRUE
          ErrMsg <<- paste(ErrMsg," : "," prey Rmax is too low, DOOOOM",sep="")
		  write(ErrMsg,file=Dumpfile,sep="\n")
     }
     
     if(PredRmax <= 1){
          flag <<- TRUE
          ErrMsg <<- paste(ErrMsg," : "," predator Rmax is too low, DOOOOM",sep="")
		  write(ErrMsg,file=Dumpfile,sep="\n",append=T)
     }
     
     if(EffConst >=1){      ### This is a warning condition, but not necessarily an error, because there are some cases the predator has smaller body mass than the prey
            #flag <<- TRUE
          WarningMsg <<- paste(WarningMsg," : "," unlikely for numerical response to exceed functional response",sep="")
          write(WarningMsg,file=Dumpfile,sep="\n",append=T)		  
     }
}

CheckParams2 <- function(ID){
     if(length(which(PopVars[[ID+1]]$popAbundTot<0))>0){
          flag <<- TRUE
          ErrMsg <<- paste(ErrMsg," : ","abundance is less than zero for model ",ID,sep=" ")
		  write(ErrMsg,file=Dumpfile,sep="\n",append=T)
     }
     # if(length(which(abs(apply(PopVars[[ID+1]]$popStMat,1,function(t) as.numeric(eigen(t)$values[1]))-1)>tol))>0){    
          # flag <<- TRUE
          # ErrMsg <<- paste(ErrMsg," : ","eigenvalue of stage matrix is not equal to 1 for model ",ID,sep=" ")
		  # writeLines(ErrMsg,file=Dumpfile,sep="\n")
     #}
}


###########################################
#####   END-USER FUNCTIONS FOR MANIPULATING STATE VARIABLES VIA MODIFIER OBJECT

#####   convention: "Old" is the old modifier object, "New" is the returned modifier object    

########################################
#####        TEST: modify carrying capacity
#                           NOTE: K must be an integer- otherwise Metapop will fail

ModKTest1 <- function(Old, ID){
     New <- Old
     ######  TEST SCENARIO: INCREASE K BY 10 PER YEAR
     New$ChangeK <- TRUE
     newK <- 1000 + CurTimeStep[ID+1]*100
     New$K <- rep(newK,times=GlobalVars[[ID+1]]$nPopulations) 
     return(New)
}

ModKTest2 <- function(Old, ID){            # first attempt at real metamodeling (trivial example: modify K based on abundance of other species)
     New <- Old
     ######  TEST SCENARIO: INCREASE K BY 10 PER YEAR
     New$ChangeK <- TRUE
     newK <- floor(PopVars[[1]]$popAbundTot[1]/10)          # set predator carrying capacity nased on prey abundance in Population 1
     New$K <- rep(newK,times=GlobalVars[[ID+1]]$nPopulations) 
     return(New)
}


ModKPrey1 <- function(Old, ID){
     New <- Old
     ######  DO NOTHING TO THE PREY MODEL...  no modifier is turned on
     # New$ChangeK <- FALSE
     # newK <- 1000 + CurTimeStep[ID+1]*100
     # New$K <- rep(newK,times=GlobalVars[[ID+1]]$nPopulations) 
     return(New)
}

ModKPred1 <- function(Old, ID){            # first attempt at real metamodeling (trivial example: modify K based on abundance of other species)
     New <- Old
     New$ChangeK <- TRUE
     newK <- round(sum(PopVars[[1]]$popAbundTot)/766)          # set predator carrying capacity nased on prey abundance in Population 1
     New$K <- rep(newK,times=GlobalVars[[2]]$nPopulations) 
	 ExportDataToCSV(ID)
     return(New)
}

ModKTest3 <- function(Old, ID){  # prey pop with no modification
     New <- Old
     #New$ChangeK <- TRUE
	 New$ChangeK <- FALSE
     for(p in 1:GlobalVars[[ID+1]]$nPopulations) {
     	newK <- Old #1000000 #floor(sum(PopVars[[1]]$popAbundTot)+100)
     	New$K[p] <- newK #rep(newK,times=GlobalVars[[ID+1]]$nPopulations) 
     	PreyVital[p] <<- PopVars[[ID+1]]$popStMat[p, GlobalVars[[ID+1]]$nStages, GlobalVars[[ID+1]]$nStages] 
     }
     return(New)
}

ModKTest4 <- function(Old, ID) {    #attempt to modify pred K on basis of total prey abundance (1/766 of total PDog abund)
     New <- Old
     New$ChangeK <- TRUE
     for(p in 1:GlobalVars[[ID+1]]$nPopulations) {
     	newK <- floor(sum(PopVars[[1]]$popAbundTot)/766)
     	New$K[p] <- newK #rep(newK, times=GlobalVars[[ID+1]]$nPopulations)
    	PredVital[p] <<- PopVars[[ID+1]]$popStMat[p, GlobalVars[[ID+1]]$nStages, GlobalVars[[ID+1]]$nStages]      
     }     
     ExportDataToCSV(ID)     
     return(New)
}

########################################
##          TEST: modify vital rates
ModVitalTest1 <- function(Old, ID){
     New <- Old
     ######  TEST SCENARIO: INCREASE ADULT SURV LINEARLY OVER TIME (logit scale)
     New$ChangeVital <- TRUE 
     newAdultSurv <- plogis(1+0.5*CurTimeStep[ID+1]) 
     #######
     # DEBUG
     #setwd("C:\\Users\\Kevin\\Documents\\Employment\\Stony Brook\\MetaModels\\MetapopManager")
     #write.table(PopVars[[ID+1]]$popStMat2,file="debug.txt")
     ######
     New$Vital <- listToMat(PopVars[[ID+1]]$popStMat2) # convert back to matrix (RJSONIO converts to list...)  [[note: moved to StopTimeStep]]
     for(p in 1:GlobalVars[[ID+1]]$nPopulations){
          New$Vital[p,GlobalVars[[ID+1]]$nStages,GlobalVars[[ID+1]]$nStages] <- newAdultSurv 
     }
     return(New)
}



##########################################
########   MODIFY PREY VITAL RATES ACCORDING TO STATE OF PREDATOR AND PREY POPULATION

# // PreyRmax (max growth rate) and PreyKcap are assumed to be those when there is no predation

# // NOTE: This function assumes that the eigenvalue of the PREY stage matrix is 1.0,
# // The following loop checks if the user-specified Rmax is possible (when N is small, EV>=Rmax)
# // REMOVE or CHANGE the following loop if eigenvalue of the matrix (EV)=Rmax

ModVitalPrey1 <- function(Old, ID){
     #############
     #        INPUTS
     #   Old : Modifier object from the last year (to be updated)
     #   ID  : Client ID for the associated Metapop instance 
     
	New <- Old
	New$ChangeVital <- TRUE

		flag <<- FALSE                    # initialize warning flag: no warnings
		ErrMsg <<- "Error: "              # initialize error message
		WarningMsg <<- "Warning: "        # initialize warning message
	
	if(CurTimeStep[ID+1]==1){		  	  # at timestep 0, perform more checks

		Dumpfile <<- "MMdump.txt"         # initialize the dumpfile for storing warnings and errors

		CheckParams1()                    # check for initial errors.... only at timestep 0
	
		if(DEBUG) write("",file=Dumpfile,sep="\n",append=FALSE)
		
                                      
		#preyStMat <<- as.matrix(PopVars[[1]]$popStMat[1,,])         # set base stage matrix for prey population: baseline for further modifications

		if(nrow(as.matrix(MaxPreyMat))>1){
			# TotalSurv <- apply(as.matrix(preyStMat)*
                                       # as.matrix(GlobalVars[[ID+1]]$constraintsMat)*
                                       # PreyRmax,2,sum)              # total MAXIMUM survival rate for each stage   PopVars[[ID+1]]$popRmax[p]
			TotalSurv <- apply(as.matrix(MaxPreyMat)*
			                           as.matrix(GlobalVars[[ID+1]]$constraintsMat),2,sum)
									   
		}else{
			TotalSurv <- 0.5
		}

          # if(length(which(TotalSurv>(1+tol)))>0) {
            # target <- maxSurvival      # target survival rate (maximum allowable survival)
 		    # SurvMult <- ifelse(TotalSurv>1, target/TotalSurv, 1)
 		    # multMat <- matrix(rep(SurvMult, times = nrow(preyStMat)), nrow = nrow(preyStMat), ncol = ncol(preyStMat), byrow = TRUE) * as.matrix(GlobalVars[[ID+1]]$constraintsMat) + (1-as.matrix(GlobalVars[[ID+1]]$constraintsMat))
 		    # preyStMat <- as.matrix(preyStMat) * multMat
          # } 
         
		if(length(which(TotalSurv>(1+tol)))>0){     # throw error if total maximum survival rates for any stage are above 1 
			flag <<- TRUE
			ErrMsg <<- paste(ErrMsg," : "," survival above 1! ",sep="")
			write(ErrMsg,file=Dumpfile,sep="\n",append=T)
			#break
		}   
	}   # end loop through the actions for timestep 0
       
	CheckParams2(ID=ID)                        # check for other errors (repeats check for all time steps)
     
     # sum of all survivals from each stage when N is approx. 0 (when R=Rmax)
     
	for(p in 1:GlobalVars[[ID+1]]$nPopulations){     # LOOP THROUGH ALL PREY POPULATIONS
	
          
		if(PreyKcap<=0){				# if carrying capacity is zero...      
			lmult <- 0					# then set all vital rates to zero
		}else{
			lmult <- (-1*log(PreyRmax) / PreyKcap) * PopVars[[ID+1]]$popAbundTot[p]    # determine DD multiplier, log scale
			if(abs(lmult)<exp_biggest){                               # if exponentiation is within reason
				lmult <- exp(lmult)                                  # then exponentiate
			}else{
				if(lmult > 0){                                       # otherwise, use pre-set limits
					lmult <- pow_biggest                             
				}else{
					lmult <- 1/pow_biggest
				}
			}

               
				   # // At this point lmult~1 if N is small, and lmult=(1/Rmax) if N=K.  So:
				   # // REMOVE the following line if eigenvalue of the matrix (EV)=Rmax; 
				   # // KEEP it if EV=1.  These are the only two options.
			#EV <- as.numeric(eigen(as.matrix(preyStMat))$values[1])   # dominant eigenvalue of the matrix (should be 1!!)
			#if(abs(EV-1)<tol){

			lmult <- PreyRmax * lmult     # finally, multiply by Rmax to compute intended growth absent predator effect
			# } else{
				# flag <<- TRUE           # throw error if eigenvalue of stage matrix is not equal to 1
				# ErrMsg <<- paste(ErrMsg," : ","eigenvalue of stage matrix is not equal to 1 for model ",ID,sep=" ")
				# writeLines(ErrMsg,file=Dumpfile,sep="\n")
			# }

		}  # end if K <= 0
		
		#### PredMort is the per-capita prey mortality due to predation
		effectivePredPop <- as.vector(PopVars[[2]]$popAbundTot %*% (PredFeed[,p] * PreyAccess[p,]))  #Total number of pred pops with access to this prey pop
		PreyEffectivePredPop[p] <<- effectivePredPop

        if(DEBUG) write(paste("effective pred pop is ",PreyEffectivePredPop[p]," at time ",CurTimeStep[ID+1],sep=""),file=Dumpfile,sep="\n",append=T)
		
		if((PopVars[[1]]$popAbundTot[p]==0)|(effectivePredPop)==0){
			PredMort <- 0
		}else{              # NOTE: ID 1 is prey pop, ID 2 is predator pop
			PredMort <- RDFuncResp(PopVars[[1]]$popAbundTot[p],effectivePredPop,alpha,htime) *      # fraction of prey pop killed by predators 
			(effectivePredPop/PopVars[[1]]$popAbundTot[p])
		}

		if(DEBUG) write(paste("PredMort is",PredMort," at time ",CurTimeStep[ID+1],sep=""),file=Dumpfile,sep="\n",append=T)
		
		lmult <- lmult / exp(PredMort)         # finally, include predator effect
		
		if(DEBUG) write(paste("target prey R is ",lmult," at time ",CurTimeStep[ID+1],sep="", "prey abundance is ", sum(PopVars[[1]]$popAbundTot)),file=Dumpfile,sep="\n",append=T)
		
		#New$Vital[p,,] <- as.matrix(preyStMat) * lmult
		targ <- round(lmult,decimals)
		if(targ<min(PreyLambdas)){ 
			ndx <- 1
		}else if(targ>max(PreyLambdas)){
			ndx <- length(PreyLambdas)
		}else{
			ndx <- which.min(abs(PreyLambdas-targ))
		}
		 
		#if(length(ndx)>1) ndx <- ndx[1]
		if(length(ndx)==0){
			flag <<- TRUE           # throw error if eigenvalue of stage matrix is not equal to 1
			ErrMsg <<- paste(ErrMsg," : ","target Lambda does not match with any possible stage matrix for model ",ID,sep=" ")
			write(ErrMsg,file=Dumpfile,sep="\n",append=T)
		}
		
		New$Vital[p,,] <- PreyStMats[[ndx]]

		PreyVital[p] <<- New$Vital[p, GlobalVars[[ID+1]]$nStages, GlobalVars[[ID+1]]$nStages-1]   # store adult survival rate for CSV file
		PreyGrowth[p] <<- PreyLambdas[ndx]  #as.numeric(eigen(as.matrix(New$Vital[p, , ]))$values[1])
	}  ## end loop through populations
     
	if(flag==TRUE){     # if no errors, then return the new modifier object
		# return(New)
		# } else{
		New<-ErrMsg     # otherwise, return the relevant error message(s)
	}
	#if(flag==FALSE) PreyVital <<- New$Vital[1,GlobalVars[[ID+1]]$nStages, GlobalVars[[ID+1]]$nStages] 
	
	if(sum(PopVars[[2]]$popAbundTot)==0) ExportDataToCSV(ID)
	
	return(New)
}



##########################################
########   MODIFY PREDATOR VITAL RATES ACCORDING TO STATE OF PREY AND PREDATOR POPULATION

# // EffConst is the efficiency constant that converts number of prey eaten to number of predators;
# // it accounts for the trophic efficiency of energy transfer, as well as differences in body mass.
# // PredRnoprey is the predator growth rate (eigenvalue of the matrix) when there is no prey.

ModVitalPred1 <- function(Old, ID){

	#############
	#        INPUTS
	#   Old : Modifier object from the last year (to be updated)
	#   ID  : Client ID for the associated Metapop instance 

	New <- Old
	New$ChangeVital <- TRUE

	flag <<- FALSE   									# initialize warning flag: no warnings
	ErrMsg <<- "Error:"  # initialize error message

	if(CurTimeStep[ID+1]==1){      						# if the initial time step...
		#predStMat <<- as.matrix(PopVars[[2]]$popStMat[1,,])     # set base stage matrix for prey population. Should have lambda=1

		#CheckParams1()    								# check for initial parameters that might be off. 
		# EV <- as.numeric(eigen(as.matrix(predStMat))$values[1])   	# dominant eigenvalue of the matrix (should be 1!!)
		# if(abs(EV-1)>tol){
			# flag <<- TRUE           					# throw error if eigenvalue of stage matrix is not equal to 1
			# ErrMsg <<- paste(ErrMsg," : ","eigenvalue of stage matrix is not equal to 1 for model ",ID,sep=" ")
			# writeLines(ErrMsg,file=Dumpfile,sep="\n")
		# }

		if(nrow(as.matrix(MaxPredMat))>1){
			# TotalSurv <- apply(as.matrix(predStMat)*
				   # as.matrix(GlobalVars[[ID+1]]$constraintsMat)*
				   # PredRmax,2,sum)						# total maximum survival rate for each stage   PopVars[[ID+1]]$popRmax[p]
			TotalSurv <- apply(as.matrix(MaxPredMat)*
			                           as.matrix(GlobalVars[[ID+1]]$constraintsMat),2,sum)
		}else{
			TotalSurv <- 0.5   
		}

		if(any(TotalSurv>(1+tol))) {					# throw error if survival rates for any stage are above 1 
			flag <<- TRUE
			ErrMsg <<- paste(ErrMsg," : "," survival above 1! ",sep="")
			write(ErrMsg,file=Dumpfile,sep="\n",append=T)
			#break
		} 
	}

	CheckParams2(ID=ID)	    # checks that must be performed every year

	for(p in 1:GlobalVars[[ID+1]]$nPopulations){   ## loop through all predator populations        
		effectivePreyPop <- as.vector(PopVars[[1]]$popAbundTot %*% (PredFeed[p,] * PreyAccess[,p])) #Total number of prey this pred pop has access to
		PredEffectivePreyPop[p] <<- effectivePreyPop
		FuncResp <- RDFuncResp(effectivePreyPop,PopVars[[2]]$popAbundTot[p],alpha,htime)
		PreyConsumed[p] <<- FuncResp * PopVars[[2]]$popAbundTot[p]
		lmult <- exp(EffConst * FuncResp) * PredRnoprey    # compute expected lambda(dominant eigenvalue) for this year
        if(DEBUG) write(paste("target pred R is ",lmult," at time ",CurTimeStep[ID+1],sep=""),file=Dumpfile,sep="\n",append=T)
			# // from Pred_R := exp(EffConst * RDFuncResp(PreyPop,PredPop,alpha,htime)) / exp(mu);

			# // NOTE: This equation assumes that the eigenvalue of the PREDATOR stage matrix is 1.0,
			# // and all stage matrix elements are proportionally affected by predation.  
			# // Thus, lMult is the eigenvalue of the predator stage matrix at this time step.  
			# // If the user specified an Rmax, lMult should not be larger than Rmax (Rmax must be >1)

		if(lmult>PredRmax){
			lmult <- PredRmax        # can't exceed maximum rate of growth (note: this allows predators to kill more prey than are used for predator growth)
		}

		#New$Vital[p,,] <- as.matrix(predStMat)*lmult

		targ <- round(lmult,decimals)
		if(targ<min(PredLambdas)){ 
			ndx <- 1
		}else if(targ>max(PredLambdas)){
			ndx <- length(PredLambdas)
		}else{
			ndx <- which(abs(PredLambdas-targ)<=tol)
		}
		 
		if(length(ndx)>1) ndx <- ndx[1]
		if(length(ndx)==0){
			flag <<- TRUE           # throw error if eigenvalue of stage matrix is not equal to 1
			ErrMsg <<- paste(ErrMsg," : ","target Lambda does not match with any possible stage matrix for model ",ID,sep=" ")
			write(ErrMsg,file=Dumpfile,sep="\n",append=T)
		}
		
		New$Vital[p,,] <- PredStMats[[ndx]]


		#####           # // Finally, add ceiling-type density dependence (e.g., to simulate space limitation)  [assume it affects only fecundity]
		if(PopVars[[2]]$popAbundTot[p] > PredKcap){
			New$Vital[p,,] <- (as.matrix(New$Vital[p,,]) * (as.matrix(GlobalVars[[ID+1]]$constraintsMat))) + 
			                             ((as.matrix(New$Vital[p,,]) * (1-as.matrix(GlobalVars[[ID+1]]$constraintsMat)))*(PredKcap/PopVars[[2]]$popAbundTot[p]))       
		}

		PredVital[p] <<- New$Vital[p, GlobalVars[[ID+1]]$nStages, GlobalVars[[ID+1]]$nStages-1]        # for CSV file
		PredGrowth[p] <<- PredLambdas[ndx] #as.numeric(eigen(as.matrix(New$Vital[p, , ]))$values[1])    # for CSV file
		#PredProduced[p] <<- (lmult * PopVars[[2]]$popAbundTot[p]) - PopVars[[2]]$popAbundTot[p]
	}   # end loop through populations

	if(flag==TRUE){     # if no errors, then return the new modifier object
	# return(New)
	# } else{
		New <- ErrMsg     # otherwise, return the relevant error message(s)
	}

	#Stored[CurTimeStep[ID+1]+1,] <<- c(CurTimeStep[ID+1], sum(PopVars[[1]]$popAbundTot), sum(PopVars[[2]]$popAbundTot), 
	#                                   PreyVital, New$Vital[1,GlobalVars[[ID+1]]$nStages, GlobalVars[[ID+1]]$nStages])

	#write.csv(Stored,"PredPreyResults.csv",row.names=FALSE)

	ExportDataToCSV(ID)

	return(New)
}

ExportDataToCSV <- function(ID){
 
	if(CurTimeStep[ID+1]==1){
			#name the csv output files
		#filenamePop <<- paste("PredPreyResults_BFFTest1_Pops ",datetime,".csv",sep="")
		filenameTot <<- paste("PredPreyResults_BFFTest1_Totals ",datetime,".csv",sep="")

            # clear the csv files in preparation for inputs (only relevant if time not used in name...)
		#temp <- data.frame()
		#write.table(x=temp,file=filenamePop,sep=",",append=F,row.names=FALSE)  # clear previous file
        #write.table(x=temp,file=filenameTot,sep=",",append=F,row.names=FALSE)  # clear previous file		
	}
 
     #Store individual pred populations
     #for(p in 1:GlobalVars[[ID+1]]$nPopulations){

	# StoredPops <<- data.frame(
								# timestep   =rep(CurTimeStep[ID+1],times=GlobalVars[[ID+1]]$nPopulations), 
								# predpop    =paste("Predator ",c(1:GlobalVars[[ID+1]]$nPopulations),sep=" "),
								# predabund  =PopVars[[2]]$popAbundTot,
								# predsurv   =PredVital,
								# predgrowth = PredGrowth,
						  #PredEffectivePreyPop[p],
								# preykilled =PreyConsumed
						  #PredProduced[p]
						# )
						
     #}
     
     #Store totals
	StoredTotal <<- data.frame(
									timestep      = CurTimeStep[ID+1], 
                                    totabundprey  = sum(PopVars[[1]]$popAbundTot),       # mp abundance, prey  
                                    totabundpred  = sum(PopVars[[2]]$popAbundTot),       # mp abundance, pred
                                    meansurvprey  = mean(PreyVital),                     # adult survival, prey
                                    meansurvpred  = mean(PredVital),                      # adult survival, pred
									meanpreygrow  = mean(PreyGrowth),
									meanpredgrow  = mean(PredGrowth),
									Kprey         = sum(PopVars[[1]]$popK),              # total carrying capacity, prey
									Kpred         = sum(PopVars[[2]]$popK)               # carrying capacity, pred
								)
     #if(CurTimeStep[ID+1]+1 >= nYears) {
     #     StoredTotal[nYears+1,] <<- colMeans(StoredTotal, na.rm = TRUE)
     #}
     
     #Write csv files
	if(CurTimeStep[ID+1]==1){
		#write.table(StoredPops,filenamePop,sep=",",append=F,row.names=FALSE,col.names=TRUE)  
		write.table(StoredTotal,filenameTot,sep=",",append=F,row.names=FALSE,col.names=TRUE)
	}else{
		#write.table(StoredPops,filenamePop,sep=",",append=T,row.names=FALSE,col.names=FALSE)  
		write.table(StoredTotal,filenameTot,sep=",",append=T,row.names=FALSE,col.names=FALSE)			
	}
}



#############################
###########   END










