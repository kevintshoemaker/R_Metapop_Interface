

##################################
##############   RECEIVE OBJECT: MetapopStateVarsGlobal   (JSON RPC object)


  # Spec: Global state variables (read from Metapop, cannot be modified from
  #       outside but may be used in forging metamodel linkages). These data
  #       are always stored by Metapop Manager but only need to be read in
  #       once.

InitializeGlobalStateVars <- function(nPopulations,nStages) {
   
    MetapopStateVarsGlobal = list()               # GLOBAL STATE VARIABLES

					# Spec: Number of populations (scalar) (must be read in each year)
	MetapopStateVarsGlobal$nPopulations <- nPopulations	

					# Spec: Number of stages (scalar)
	MetapopStateVarsGlobal$nStages <- nStages	
	
						# Spec: Number of years (scalar)
	MetapopStateVarsGlobal$nYears <- 1	

					# Spec: Constraints matrix (matrix of dimension "nStages", "nStages")
	MetapopStateVarsGlobal$constraintsMat <- array(0,dim=c(MetapopStateVarsGlobal$nStages,MetapopStateVarsGlobal$nStages))

					# Spec: Stage-specific average mass (vector of length "nStages")
	MetapopStateVarsGlobal$stageMass <- numeric(MetapopStateVarsGlobal$nStages)	

					# Spec: Stage-specific proportion breeding (vector of length "nStages")
	MetapopStateVarsGlobal$stageBreed <- numeric(MetapopStateVarsGlobal$nStages)	

					# Spec: Stage-specific relative dispersal (vector of length "nStages")
	MetapopStateVarsGlobal$stageRelDisp <- numeric(MetapopStateVarsGlobal$nStages)	

					# Spec: Basis for density dependence (vector of length "nStages")
	MetapopStateVarsGlobal$stageDD	<- numeric(MetapopStateVarsGlobal$nStages)	

					# Spec: Number of defined catastrophes (scalar)
	MetapopStateVarsGlobal$nCatastrophes <- 1	

					# Spec: Indicates whether catastrophe affects survival, abundance, or K
    						#       (Boolean array of dimension "nCatastrophes", 4 [4 types of
    						#       catastrophe])
	MetapopStateVarsGlobal$catType <- array(FALSE,dim=c(MetapopStateVarsGlobal$nCatastrophes,4)) 		   


					# Spec: Stage-specific catastrophe multiplier (array of dimension
    						#       "nCatastrophes","nStages")
	MetapopStateVarsGlobal$catStages <-  array(0,dim=c(MetapopStateVarsGlobal$nCatastrophes,MetapopStateVarsGlobal$nStages))      


	    # WTR disabled nMP, since Metapop doesn't know this!
		#    # Spec: Number of Metapop instances being run simultaneously (scalar).
		#    nMP: Cardinal;
       return(MetapopStateVarsGlobal)
}	


##################################
##############   RECEIVE OBJECT: MetapopStateVarsPop   (JSON RPC object)

  	# Spec: Population state variables (read from Metapop, cannot be directly
  	#       modified from outside but are optionally available for use in forging
  	#       metamodel linkages). These variables are stored only if a user has
  	#       specified to do so.


InitializePopulationStateVars <- function(ID) {

    MetapopStateVarsPop <- list()

    	# Spec: Total abundance for each population (vector of dimension "nPopulations")
    MetapopStateVarsPop$popAbundTot <- numeric(GlobalVarsx[[ID]]$nPopulations)

    MetapopStateVarsPop$currentYear <- 0

    	# Spec: Total number of adult females (vector of dimension "nPopulations")
    MetapopStateVarsPop$popAbundFem<- numeric(GlobalVarsx[[ID]]$nPopulations)

   	 # Spec: Total number of adult males (vector of dimension "nPopulations")
    MetapopStateVarsPop$popAbundMale<- numeric(GlobalVarsx[[ID]]$nPopulations)

   	 # Spec: percent breeders that have mates (vector of dimension "nPopulations")
    MetapopStateVarsPop$PBM<- numeric(GlobalVarsx[[ID]]$nPopulations)

    	# Spec: Disperser matrix (array of dimension "nPopulations", "nPopulations", "nStages")
    MetapopStateVarsPop$disperserMat<- array(0,dim=c(GlobalVarsx[[ID]]$nPopulations,GlobalVarsx[[ID]]$nPopulations,GlobalVarsx[[ID]]$nStages))

    	# Spec: Dispersal matrix (matrix of dimension "nPopulations", "nPopulations")
    MetapopStateVarsPop$dispersalMat<- array(0,dim=c(GlobalVarsx[[ID]]$nPopulations,GlobalVarsx[[ID]]$nPopulations))

    	# Spec: correlated standard normal deviate for fecundity (vector of length "nPopulations")
    MetapopStateVarsPop$NDF<- numeric(GlobalVarsx[[ID]]$nPopulations)

    	# Spec: correlated standard normal deviate for survival (vector of length "nPopulations")
    MetapopStateVarsPop$NDS<- numeric(GlobalVarsx[[ID]]$nPopulations)

    	# Spec: correlated standard normal deviate for carrying capacity (vector of length "nPopulations")
    MetapopStateVarsPop$NDK<- numeric(GlobalVarsx[[ID]]$nPopulations)

    	# Spec: Number of emigrants (array of dimension "nPopulations", "nStages")
    MetapopStateVarsPop$popEmigrants<- array(0,dim=c(GlobalVarsx[[ID]]$nPopulations,GlobalVarsx[[ID]]$nStages))

    	# Spec: Number of immigrants (array of dimension "nPopulations", "nStages")
    MetapopStateVarsPop$popImmigrants<- array(0,dim=c(GlobalVarsx[[ID]]$nPopulations,GlobalVarsx[[ID]]$nStages))

    	# Spec: Catastrophe status (array of dimension "nPopulations", 2)
    MetapopStateVarsPop$popCatStatus<- array(TRUE,dim=c(GlobalVarsx[[ID]]$nPopulations,2))     # TRUE

    	# Spec: Time since last catastrophe (array of dimension "nPopulations", 2)
    MetapopStateVarsPop$popTimeSince<- array(0,dim=c(GlobalVarsx[[ID]]$nPopulations,2))

    	# Spec: Catastrophe probability (vector of length "nPopulations")
    MetapopStateVarsPop$popCatProb<- numeric(GlobalVarsx[[ID]]$nPopulations)

    	# Spec: Maximum growth rate (vector of length "nPopulations")
    MetapopStateVarsPop$popRmax<- numeric(GlobalVarsx[[ID]]$nPopulations)

    	# Spec: number of individuals harvested (array of dimension "nPopulations", "nStages")
    MetapopStateVarsPop$popHarvest<- array(0,dim=c(GlobalVarsx[[ID]]$nPopulations,GlobalVarsx[[ID]]$nStages))

    	# Spec: number of individuals supplemented (array of dimension "nPopulations", "nStages")
    MetapopStateVarsPop$popSupplement<- array(0,dim=c(GlobalVarsx[[ID]]$nPopulations,GlobalVarsx[[ID]]$nStages))

    	# Spec: actual or realized K (with stochasticity).
    MetapopStateVarsPop$popK2<- numeric(GlobalVarsx[[ID]]$nPopulations)

    	# Spec: Realized transition matrices, including stochasticity …
    MetapopStateVarsPop$popStMat2<- array(0,dim=c(GlobalVarsx[[ID]]$nPopulations,GlobalVarsx[[ID]]$nStages,GlobalVarsx[[ID]]$nStages))


    	#####
    	# Spec: Population state variables (read from Metapop: modifications to
    	#       these variables are translated into a set of multipliers stored in
    	#       the "Metapop modifier" object). These variables [sic]
    	#####

    	# Spec: Stage-specific abundances for each population (matrix of dimension
    	#       "nPopulations","nStages")  [[modifications result in specification
    	#       of "abundance modifier" ]]
    MetapopStateVarsPop$popAbundSt<- array(0,dim=c(GlobalVarsx[[ID]]$nPopulations,GlobalVarsx[[ID]]$nStages))

    	# Spec: Mean transition matrices for each population (array of dimension
    	#       "nPopulations", "nStages", "nStages")   [[modifications result in
    	#       specification of "vital rate modifier"]]
    MetapopStateVarsPop$popStMat<- array(0,dim=c(GlobalVarsx[[ID]]$nPopulations,GlobalVarsx[[ID]]$nStages,GlobalVarsx[[ID]]$nStages))

    	# Spec: Mean population-specific carrying capacity (vector of length
    	#       "nPopulations").  [[modifications result in specification of "K
    	#       modifier"]]
    MetapopStateVarsPop$popK<- numeric(GlobalVarsx[[ID]]$nPopulations)

    	# Spec: Relative dispersal rate (matrix of dimension "nPopulations",
    	#       "nStages") [[modifications result in specification of "dispersal
    	#       modifier"]]
    MetapopStateVarsPop$relDispersal<- array(0,dim=c(GlobalVarsx[[ID]]$nPopulations,GlobalVarsx[[ID]]$nStages))

    return(MetapopStateVarsPop)
}



#####################  INITIALIZE MODIFIER OBJECT

InitializeModifier <- function(ID){
  MetapopModifier = list()     # generate metapop modifier object based on state variables...

    MetapopModifier$ChangeAbund = FALSE
    MetapopModifier$ChangeVital = FALSE
    MetapopModifier$ChangeK     = FALSE    
    MetapopModifier$ChangeDisp  = FALSE


    	# Spec: Last year for which complete simulation data are available for all
    	#       component models (scalar integer).
    MetapopModifier$Timestep    <-  0    # need population state var to tell us what year it is.    #GlobalVarsx[[ID]]$

    	# Spec: Boolean indicator of whether this modifier object is complete and
    	#       ready to be used in a Metapop simulation (single Boolean).
    MetapopModifier$Complete    <- TRUE

    	# Spec: Boolean indicator of whether this modifier object is currently in
    	#       effect (single Boolean). [[for now, always TRUE]]
    MetapopModifier$Active      <- TRUE

    	# TODO: properly define and implement VarList
#    		# Spec: List of optional variables that can be monitored, and thereby used
#    		#       to modify any of the metamodel linkage options specified in Type
#    		#       (vector of Booleans of length "number of possible variables"- see
#    		#       Metapop Manager description below for a more detailed list of
#    		#       variables).
#    		VarList<-

    	# Spec: modified stage-specific abundances (matrix of dimension
    	#       "nPopulations", "nStages"). Replaces stage-specific abundances for
    	#       each population. Not compatible with catastrophes affecting
    	#       abundance.
    MetapopModifier$Abund       <- array(0,dim=c(GlobalVarsx[[ID]]$nPopulations,GlobalVarsx[[ID]]$nStages))

    	# Spec: modified vital rates (matrix of dimension "nPopulations",
    	#       "nStages", "nStages"). Replaces mean population-specific transition
    	#       matrix values. Not compatible with temporal changes in vital rates
    	#       or catastrophes that affect vital rates.
    MetapopModifier$Vital       <- array(0,dim=c(GlobalVarsx[[ID]]$nPopulations,GlobalVarsx[[ID]]$nStages,GlobalVarsx[[ID]]$nStages))

    	# Spec: modified mean population-specific carrying capacities (vector of
    	#       length "nPopulations"). Replaces mean population-specific K values.
    	#       Not compatible with "temporal change in K" or catastrophes that
    	#       affect K.
    MetapopModifier$K           <- rep(0,times=GlobalVarsx[[ID]]$nPopulations)

    	# Spec: dispersal rate modifier (vector of length "nPopulations"). Modifies
    	#       dispersal rates relative to the rate expected for a given stage and
    	#       population pairing. Not compatible with catastrophes that affect
    	#       dispersal. [[this should probably be tabled for now, until we can
    	#       clarify the details]]
    MetapopModifier$Disp  <- numeric(GlobalVarsx[[ID]]$nPopulations)
      #write.csv("StartTimeStep",file="TESTOUT.TXT",)  # test to see if functions are being called.

    a <- MetapopModifier  #list(result=as.logical(TRUE),modifier=MetapopModifier)
 
    return(a)

   # 
  
}


#### 

buildObjects <- function(nPopulations,nStages){
   GlobalVarsx <<- list()      # save to global environment
   PopVarsx <<- list()
   Modifier <<- list()
   GlobalVarsx[[1]] <<- InitializeGlobalStateVars(nPopulations,nStages)
   GlobalVarsx[[2]] <<- InitializeGlobalStateVars(nPopulations,nStages)
   PopVarsx[[1]] <<- InitializePopulationStateVars(1)
   PopVarsx[[2]] <<- InitializePopulationStateVars(2)

   Modifier[[1]] <<- InitializeModifier(1)
   Modifier[[2]] <<- InitializeModifier(2)
}

     # run through the RJSON "wash" (only for objects sent over from RAMAS)
washObjects <- function(){
  PopVars_temp <<- list()
  GlobalVars_temp <<- list()

  PopVars_temp[[1]] <<- toJSON(PopVarsx[[1]])    # asIs=TRUE
  PopVars_temp[[2]] <<- toJSON(PopVarsx[[2]])

  GlobalVars_temp[[1]] <<- toJSON(GlobalVarsx[[1]])
  GlobalVars_temp[[2]] <<- toJSON(GlobalVarsx[[2]])

  PopVars <<- list()
  GlobalVars <<- list()

  PopVars[[1]] <<- fromJSON(PopVars_temp[[1]])
  PopVars[[2]] <<- fromJSON(PopVars_temp[[2]])  

  GlobalVars[[1]] <<- fromJSON(GlobalVars_temp[[1]])
  GlobalVars[[2]] <<- fromJSON(GlobalVars_temp[[2]])   
}



################## END READ FUNCTIONS



################################
#######################
###################
#### POPULATE TEST OBJECTS

nPopulations=1
nStages=14
nmodels=2  # number of MP instances
predpop=2  # which is predator?
preypop=1  # which is prey?


buildObjects(nPopulations=nPopulations,nStages=nStages)      # initialize global and population state vars for two populations        
 # washObjects()                               # run through the JSON-RPC "washer"


m=1
for(m in 1:nmodels){
  
  p=1                                   # initialize abundance
  for(p in 1:GlobalVarsx[[m]]$nPopulations){
    if(m==preypop) PopVarsx[[m]]$popAbundTot[p] <- 100000   # prey   (all prey pops)
    if(m==predpop) PopVarsx[[m]]$popAbundTot[p] <- 100   # predator  (all pred pops)
  }
                                    # initialize stage matrix
 # PopVarsx[[m]]$popStMat <- listToMat(PopVarsx[[m]]$popStMat)
  for(p in 1:GlobalVarsx[[m]]$nPopulations){
    if(m==preypop) PopVarsx[[m]]$popStMat[p,,] <- BaselinePreyMat #2.5  # matrix(c(0,.5,.5,.75),nrow=2)  #matrix(1,nrow=1) #
    if(m==predpop) PopVarsx[[m]]$popStMat[p,,] <- BaselinePreyMat #1.5  # matrix(c(0,.5,.5,.75),nrow=2)  #matrix(1,nrow=1)  # 
  }

  #GlobalVarsx[[m]]$constraintsMat <- listToMat(GlobalVarsx[[m]]$constraintsMat)
  GlobalVarsx[[m]]$constraintsMat <- matrix(c(rep(0,times=nStages),rep(1,times=nStages*(nStages-1))),nrow=nStages,byrow=T) #matrix(c(0,1,0,1),nrow=2)
  GlobalVarsx[[m]]$constraintsMat[9,] <- rep(0,times=nStages)

}


############################
############# SET UP THE DATA ENVIRONMENT

                            # INITIALIZE ENVIRONMENT DATA 
GlobalVars <- list()
PopVars <- list() 
AllModifiers <- list() # collect modifier objects for all instances of Metapop and store in global environment
nInstances <- 0    
ClientIDs <- numeric(0)
CurTimeStep <- numeric(0)
ExtinctFlag <- logical(0)

ts = 10    # number of timesteps

Initialize(ClientID=0)
Initialize(ClientID=1)

StartSimulation(MetapopStateVarsGlobal=GlobalVarsx[[1]],ClientID=0)
StartSimulation(MetapopStateVarsGlobal=GlobalVarsx[[2]],ClientID=1)

GlobalVars[[1]] <- GlobalVarsx[[1]]
GlobalVars[[2]] <- GlobalVarsx[[2]]

GlobalVars

PopVars[[1]] <- PopVarsx[[1]]
PopVars[[2]] <- PopVarsx[[2]]

BaselinePredMat=BaselinePreyMat
MaxPredMat=MaxPreyMat
MinPredMat=MinPreyMat













