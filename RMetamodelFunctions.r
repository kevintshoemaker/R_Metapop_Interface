#########################################################################
######   DEFINE PROCEDURE CALLS INVOKED BY METAPOP/ METAPOP MANAGER
######
######   Kevin Shoemaker, May 2013
#############################################################


#####################################
#######  Load packages...

# {none currently}



######################################
##################################
########     LOAD GLOBAL STORAGE VARIABLES


GlobalVars <- list()
PopVars <- list() 
AllModifiers <- list() # collect modifier objects for all instances of Metapop and store in global environment
nInstances <- 0    

ClientIDs <- numeric(0)
CurTimeStep <- numeric(0)
ExtinctFlag <- logical(0)

####################################################
#                       DEFINE ALL METAPOP RPC PROCEDURES: Initialize, StartSimulation, StartTimeStep, StopTimeStep, StopSimulation, Finalize
##############################
#                       "INITIALIZE" routine: initiate this MP instance

# function Initialize(const ClientID: Cardinal): Boolean;
#  - return True to continue, return False to cancel

Initialize <- function(ClientID){
     nInstances <<- nInstances + 1  #  length(AllModifiers)   # record the number of MP instances
    
     # if(nInstances==1){
          # ClientIDs <<- ClientID
          # CurTimeStep <<- 0
          # ExtinctFlag <<- FALSE
     # }else{
          ClientIDs <<- c(ClientIDs,ClientID)
          CurTimeStep <<- c(CurTimeStep,0)  # set current time step to 0 for all instances (year 0)
          ExtinctFlag <<- c(ExtinctFlag,FALSE)	
     #}
     return(is.numeric(ClientID))
}


###################################################
##############################
#                       "StartSimulation" routine: get the storage structures ready for this instance of Metapop

# function StartSimulation(const MetapopStateVarsGlobal: TRpcMetapopStateVarsGlobal): Boolean;
#  - store values from metapopStateVarsGlobal as needed for simulation
#  - return True to continue, return False to cancel

StartSimulation <- function(MetapopStateVarsGlobal,ClientID){      # ClientID identifies the instance of Metapop
     GlobalVars[[ClientID+1]] <<- MetapopStateVarsGlobal   # save current global state vars to global workspace
     
     #################  Convert matrices and arrays back to original format... [shouldn't have to do this, but for now...]
     GlobalVars[[ClientID+1]]$constraintsMat <<- listToMat(GlobalVars[[ClientID+1]]$constraintsMat)
     
     ###### INITIALIZE MODIFIER IN PREP FOR TIME LOOP ######
     
     AllModifiers[[ClientID+1]] <<- list()     # generate metapop modifier object based on state variables...
     
     # Spec: Categorical indicator of what is to be modified: abundance, vital
     #       rates, carrying capacity or dispersal
     #       (ultimately, it should be possible to modify more than one;
     #       therefore, an array of Booleans may be most appropriate)
     
     AllModifiers[[ClientID+1]]$ChangeAbund <<- FALSE
     AllModifiers[[ClientID+1]]$ChangeVital <<- FALSE
     AllModifiers[[ClientID+1]]$ChangeK     <<- FALSE   #   FALSE   #   
     AllModifiers[[ClientID+1]]$ChangeDisp  <<- FALSE
     
     
     # Spec: Last year for which complete simulation data are available for all
     #       component models (scalar integer).
     AllModifiers[[ClientID+1]]$Timestep    <<-  as.integer(0)    # need population state var to tell us what year it is.    #GlobalVars[[ClientID+1]]$
     
     # Spec: Boolean indicator of whether this modifier object is complete and
     #       ready to be used in a Metapop simulation (single Boolean).
     AllModifiers[[ClientID+1]]$Complete    <<- TRUE
     
     # Spec: Boolean indicator of whether this modifier object is currently in
     #       effect (single Boolean). [[for now, always TRUE]]
     AllModifiers[[ClientID+1]]$Active      <<- TRUE
     
     # Spec: modified stage-specific abundances (matrix of dimension
     #       "nPopulations", "nStages"). Replaces stage-specific abundances for
     #       each population. Not compatible with catastrophes affecting
     #       abundance.
     AllModifiers[[ClientID+1]]$Abund       <<- array(0,dim=c(GlobalVars[[ClientID+1]]$nPopulations,GlobalVars[[ClientID+1]]$nStages))
     
     # Spec: modified vital rates (matrix of dimension "nPopulations",
     #       "nStages", "nStages"). Replaces mean population-specific transition
     #       matrix values. Not compatible with temporal changes in vital rates
     #       or catastrophes that affect vital rates.
     AllModifiers[[ClientID+1]]$Vital       <<- array(0,dim=c(GlobalVars[[ClientID+1]]$nPopulations,GlobalVars[[ClientID+1]]$nStages,GlobalVars[[ClientID+1]]$nStages)) # array(0.1,dim=c(GlobalVars[[ClientID+1]]$nPopulations,GlobalVars[[ClientID+1]]$nStages,GlobalVars[[ClientID+1]]$nStages))
     
     # Spec: modified mean population-specific carrying capacities (vector of
     #       length "nPopulations"). Replaces mean population-specific K values.
     #       Not compatible with "temporal change in K" or catastrophes that
     #       affect K.
     AllModifiers[[ClientID+1]]$K           <<- rep(0,times=GlobalVars[[ClientID+1]]$nPopulations)
     
     # Spec: dispersal rate modifier (vector of length "nPopulations"). Modifies
     #       dispersal rates relative to the rate expected for a given stage and
     #       population pairing. Not compatible with catastrophes that affect
     #       dispersal. [[this should probably be tabled for now, until we can
     #       clarify the details]]
     AllModifiers[[ClientID+1]]$Disp  <<- numeric(GlobalVars[[ClientID+1]]$nPopulations)



 
	###################################
	####### SET UP STORAGE VARIABLES 
	
	if(ClientID==1){      # if final model is being read in... then set up storage structure.

		datetime <<- format(Sys.time(), "%a %b %d %Y %H%M")
		nYears <<- GlobalVars[[ClientID+1]]$nYears

		PreyVital <<- array(0,dim=GlobalVars[[1]]$nPopulations)
		PredVital <<- array(0,dim=GlobalVars[[2]]$nPopulations)
		PreyGrowth <<- array(0,dim=GlobalVars[[1]]$nPopulations)
		PredGrowth <<- array(0,dim=GlobalVars[[2]]$nPopulations)
		PreyConsumed <<- array(0,dim=GlobalVars[[2]]$nPopulations)
		 #PredProduced <<- array(0,dim=2)

		PreyEffectivePredPop <<- array(0,dim=GlobalVars[[1]]$nPopulations)     # for each prey population, the abundance of predators feeding upon it
		PredEffectivePreyPop <<- array(0,dim=GlobalVars[[2]]$nPopulations)     # for each pred population, the abundance of prey available to it
		 
	}
     
     return(is.numeric(ClientID))
}


###############################################################
##############################
#                       "StartTimeStep" routine: make any changes and return modifier...when all information is ready     

#  function StartTimeStep: TModifierResult;
#  - fill TModifierResult.modifier  as desired to make Metapop change simulation data
#  - return True for TModifierResult.result to continue, return False to cancel

StartTimeStep <- function(ClientID){
     
     AllModifiers[[ClientID+1]]$Timestep <<- as.integer(CurTimeStep[ClientID+1])   # last time step for which complete information is available
     
     ############  TEST #1: simply modify predator K based on prey abundance:  
     #if(ClientID==0) AllModifiers[[ClientID+1]] <<- ModKTest1(AllModifiers[[ClientID+1]],ClientID)   # update the Modifier for this Client ID
     #if(ClientID==1) AllModifiers[[ClientID+1]] <<- ModKTest2(AllModifiers[[ClientID+1]],ClientID)  #ModVitalTest1(AllModifiers[[ClientID+1]],ClientID)
     ########################

	###################################
	####### SET UP STAGE MATRICES 	 

	if((ClientID==0)&(CurTimeStep[ClientID+1]==1)){      # if first model is being read in... then set up stage matrices.
		tempFileName <- paste("Stage_matrices_",decimals,".RData",sep="")
		if(file.exists(tempFileName)){
			load(tempFileName, envir = global_env)
			    #PreyLambdas <<- PreyLambdas
		}else{
			tempobj <- preComputeSMs(MinMat=MinPreyMat,BaselineMat=BaselinePreyMat,MaxMat=MaxPreyMat,decimals=decimals)   # for prey
			PreyLambdas <<- tempobj$Lambdas
			PreyStMats <<- tempobj$StMats
			if(is.null(PreyStMats[[1]])) PreyStMats[[1]] <<- MinPreyMat   # correct this better later...
			if(is.null(PreyStMats[[length(PreyLambdas)]])) PreyStMats[[length(PreyLambdas)]] <<- MaxPreyMat   # correct this better later...

			tempobj <- preComputeSMs(MinMat=MinPredMat,BaselineMat=BaselinePredMat,MaxMat=MaxPredMat,decimals=decimals)   # for prey
			PredLambdas <<- tempobj$Lambdas
			PredStMats <<- tempobj$StMats
			if(is.null(PredStMats[[1]])) PredStMats[[1]] <<- MinPredMat   # correct this better later...
			if(is.null(PredStMats[[length(PredLambdas)]])) PredStMats[[length(PredLambdas)]] <<- MaxPredMat   # correct this better later...

			save(MaxPredMat,MinPredMat,BaselinePredMat,MaxPreyMat,MinPreyMat,BaselinePreyMat,PreyLambdas,PredLambdas,PreyStMats,PredStMats,
			            envir = global_env, file=tempFileName)
		}
	}
     
     ############  TEST #2: implement Resit's suggested predator prey model
     if(ClientID==0) AllModifiers[[ClientID+1]] <<- ModVitalPrey1(AllModifiers[[ClientID+1]],ClientID)  # Update Prey
     if(ClientID==1) AllModifiers[[ClientID+1]] <<- ModVitalPred1(AllModifiers[[ClientID+1]],ClientID)  # Update Predator
     ############ 
     
     a <- list(result=is.numeric(ClientID),modifier=AllModifiers[[ClientID+1]])
     return(a)
}


########################################################
##############################
#                       "StopTimeStep" routine: read in population state variables

# function StopTimeStep(const MetapopStateVarsPop: TRpcMetapopStateVarsPop): Boolean;
#  - store values from metapopStateVarsPop as needed for next time step
#  - return True to continue, return False to cancel


StopTimeStep <- function(MetapopStateVarsPop,ClientID){
     
     PopVars[[ClientID+1]] <<- MetapopStateVarsPop   # save new pop state vars to workspace.
     
     #################  Convert matrices and arrays back to original format... [shouldn't have to do this, but for now...]
     PopVars[[ClientID+1]]$popStMat <<- listToMat(PopVars[[ClientID+1]]$popStMat) # convert back to matrix or array format (RJSONIO converts to list for some reason...)
     
     if(is.matrix(PopVars[[ClientID+1]]$popStMat)){    # deal with the 1-pop 1-stage case, which seems to be problematic
          PopVars[[ClientID+1]]$popStMat <<- array(PopVars[[ClientID+1]]$popStMat[1,1],dim=c(1,1,1))
     }
     
     CurTimeStep[ClientID+1] <<- CurTimeStep[ClientID+1] + 1    #  MetapopStateVarsPop$currentYear     # Record the year of the simulation
     return(is.numeric(ClientID))    
}


###########################################################
##############################
#                       "StopSimulation" routine

# function StopSimulation: Boolean;
#  - clean up after simulation
#  - return True to continue, return False to cancel
# (it usually doesn't make sense to cancel once the simulation has finished)
#  if extinction occurs, set abundances and most other population state vars to zero


StopSimulation <- function(ClientID){
     
     if(CurTimeStep[ClientID+1] < GlobalVars[[ClientID+1]]$nYears){
          ExtinctFlag[ClientID+1] <<- TRUE 
          
          ##################################################################
          # set population state variables to their final values....
          # Spec: Total abundance for each population (vector of dimension "nPopulations")
          PopVars[[ClientID+1]]$popAbundTot <<- numeric(GlobalVars[[ClientID+1]]$nPopulations)   # set to zero
          
          PopVars[[ClientID+1]]$currentYear <<- CurTimeStep[ClientID+1]    # set to year of extinction
          
          # Spec: Total number of adult females (vector of dimension "nPopulations")
          PopVars[[ClientID+1]]$popAbundFem <<- numeric(GlobalVars[[ClientID+1]]$nPopulations)
          
          # Spec: Total number of adult males (vector of dimension "nPopulations")
          PopVars[[ClientID+1]]$popAbundMale <<- numeric(GlobalVars[[ClientID+1]]$nPopulations)
          
          # Spec: percent breeders that have mates (vector of dimension "nPopulations")
          PopVars[[ClientID+1]]$PBM <<- numeric(GlobalVars[[ClientID+1]]$nPopulations)
          
          # Spec: Disperser matrix (array of dimension "nPopulations", "nPopulations", "nStages")
          PopVars[[ClientID+1]]$disperserMat <<- array(0,dim=c(GlobalVars[[ClientID+1]]$nPopulations,GlobalVars[[ClientID+1]]$nPopulations,GlobalVars[[ClientID+1]]$nStages))
          
          # Spec: Dispersal matrix (matrix of dimension "nPopulations", "nPopulations")
          PopVars[[ClientID+1]]$dispersalMat <<- array(0,dim=c(GlobalVars[[ClientID+1]]$nPopulations,GlobalVars[[ClientID+1]]$nPopulations))
          
          # Spec: correlated standard normal deviate for fecundity (vector of length "nPopulations")
          PopVars[[ClientID+1]]$NDF <<- numeric(GlobalVars[[ClientID+1]]$nPopulations)
          
          # Spec: correlated standard normal deviate for survival (vector of length "nPopulations")
          PopVars[[ClientID+1]]$NDS <<- numeric(GlobalVars[[ClientID+1]]$nPopulations)
          
          # Spec: correlated standard normal deviate for carrying capacity (vector of length "nPopulations")
          PopVars[[ClientID+1]]$NDK <<- numeric(GlobalVars[[ClientID+1]]$nPopulations)
          
          # Spec: Number of emigrants (array of dimension "nPopulations", "nStages")
          PopVars[[ClientID+1]]$popEmigrants <<- array(0,dim=c(GlobalVars[[ClientID+1]]$nPopulations,GlobalVars[[ClientID+1]]$nStages))
          
          # Spec: Number of immigrants (array of dimension "nPopulations", "nStages")
          PopVars[[ClientID+1]]$popImmigrants <<- array(0,dim=c(GlobalVars[[ClientID+1]]$nPopulations,GlobalVars[[ClientID+1]]$nStages))
          
          # Spec: Catastrophe status (array of dimension "nPopulations", 2)
          PopVars[[ClientID+1]]$popCatStatus <<- array(TRUE,dim=c(GlobalVars[[ClientID+1]]$nPopulations,2))     # TRUE
          
          # Spec: Time since last catastrophe (array of dimension "nPopulations", 2)
          PopVars[[ClientID+1]]$popTimeSince <<- array(0,dim=c(GlobalVars[[ClientID+1]]$nPopulations,2))
          
          # Spec: Catastrophe probability (vector of length "nPopulations")
          PopVars[[ClientID+1]]$popCatProb <<- numeric(GlobalVars[[ClientID+1]]$nPopulations)
          
          # Spec: Maximum growth rate (vector of length "nPopulations")
          #PopVars[[ClientID+1]]$popRmax <<- numeric(GlobalVars[[ClientID+1]]$nPopulations)
          
          # Spec: number of individuals harvested (array of dimension "nPopulations", "nStages")
          PopVars[[ClientID+1]]$popHarvest <<- array(0,dim=c(GlobalVars[[ClientID+1]]$nPopulations,GlobalVars[[ClientID+1]]$nStages))
          
          # Spec: number of individuals supplemented (array of dimension "nPopulations", "nStages")
          PopVars[[ClientID+1]]$popSupplement <<- array(0,dim=c(GlobalVars[[ClientID+1]]$nPopulations,GlobalVars[[ClientID+1]]$nStages))
          
          # Spec: actual or realized K (with stochasticity).
          #PopVars[[ClientID+1]]$popK2 <<- numeric(GlobalVars[[ClientID+1]]$nPopulations)
          
          # Spec: Realized transition matrices, including stochasticity …
          PopVars[[ClientID+1]]$popStMat2 <<- array(0,dim=c(GlobalVars[[ClientID+1]]$nPopulations,GlobalVars[[ClientID+1]]$nStages,GlobalVars[[ClientID+1]]$nStages))
          
          
          #####
          # Spec: Population state variables (read from Metapop: modifications to
          #       these variables are translated into a set of multipliers stored in
          #       the "Metapop modifier" object). These variables [sic]
          #####
          
          # Spec: Stage-specific abundances for each population (matrix of dimension
          #       "nPopulations","nStages")  [[modifications result in specification
          #       of "abundance modifier" ]]
          PopVars[[ClientID+1]]$popAbundSt <<- array(0,dim=c(GlobalVars[[ClientID+1]]$nPopulations,GlobalVars[[ClientID+1]]$nStages))
          
          # Spec: Mean transition matrices for each population (array of dimension
          #       "nPopulations", "nStages", "nStages")   [[modifications result in
          #       specification of "vital rate modifier"]]
          PopVars[[ClientID+1]]$popStMat <<- array(0,dim=c(GlobalVars[[ClientID+1]]$nPopulations,GlobalVars[[ClientID+1]]$nStages,GlobalVars[[ClientID+1]]$nStages))
          
          # Spec: Mean population-specific carrying capacity (vector of length
          #       "nPopulations").  [[modifications result in specification of "K
          #       modifier"]]
          #PopVars[[ClientID+1]]$popK <<- numeric(GlobalVars[[ClientID+1]]$nPopulations)
          
          # Spec: Relative dispersal rate (matrix of dimension "nPopulations",
          #       "nStages") [[modifications result in specification of "dispersal
          #       modifier"]]
          PopVars[[ClientID+1]]$relDispersal <<- array(0,dim=c(GlobalVars[[ClientID+1]]$nPopulations,GlobalVars[[ClientID+1]]$nStages))
     }
     
     return(is.numeric(ClientID)) 
}                                 # why do I need to return FALSE here- otherwise we have an endless loop...?????


#################################################
##############################
#                       "Finalize" routine 
# procedure Finalize;
# - clean up
# - no return value required

Finalize <- function(ClientID){
     return(is.numeric(ClientID))
}



######################################
#######   END DEFINITIONS Of RPC PROCEDURES






################################################
#######################   LOAD GENERIC ANCILLARY FUNCTIONS FOR METAMODELING WITH RAMAS...


      # function for truncating to a certain decimal place
trunc <- function(x, ..., prec = 0) base::trunc(x * 10^prec, ...) / 10^prec;

preComputeSMs <- function(MinMat,BaselineMat,MaxMat,decimals=1){
  returnobj <- list()
  minLambda <- as.numeric(eigen(MinMat)$values[1])
  maxLambda <- as.numeric(eigen(MaxMat)$values[1])
  baselineLambda <- as.numeric(eigen(BaselineMat)$values[1])    
  targets <- seq(trunc(minLambda,prec=decimals),round(maxLambda,decimals),by=(1/(10^decimals)))   # target lambda values...
  selected <- list()

  temp <- list()
  matrixgen <- length(targets)*10
  halfgen <- floor(matrixgen/2)  
  temp2 <- numeric(matrixgen)

  daddymat <- array(0,dim=c(nrow(MinMat),ncol(MinMat),matrixgen))  
      # determine the range of values for each matrix element
  for(i in 1:nrow(MinMat)){
    for(j in 1:ncol(MinMat)){
	  daddymat[i,j,1:halfgen] <- seq(from=MinMat[i,j],to=BaselineMat[i,j],length=halfgen)
	}
  }
  for(i in 1:nrow(MinMat)){
    for(j in 1:ncol(MinMat)){
	  daddymat[i,j,(halfgen+1):matrixgen] <- seq(from=BaselineMat[i,j],to=MaxMat[i,j],length=length((halfgen+1):matrixgen))
	}
  }
      # generate a bunch of plausible matrices for matching with the target lambda values
  for(m in 1:matrixgen){
    temp[[m]] <- daddymat[,,m]
    temp2[m] <- as.numeric(eigen(temp[[m]])$values[1])	
  }
 
      # select one matrix for each target lambda value
  for(t in 1:length(targets)){
     ndx <- which(abs(round(temp2,decimals)-targets[t])<(1/(decimals*10*2)))
	 selected[[t]] <- matrix()
	 if(length(ndx)>0){
	   select <- ndx[1]  #sample(ndx,1)
	   selected[[t]] <- temp[[select]]
	 }
	 if(is.null(nrow(selected[[t]]))){
		if(!is.null(nrow(selected[[t-1]]))){ 
			selected[[t]] <- selected[[t-1]]
		}else{
			if(!is.null(nrow(selected[[t-2]]))){ 
				selected[[t]] <- selected[[t-2]]	
			}
		}
	 }
  }  
  returnobj$StMats <- selected
  returnobj$Lambdas <- targets  
  return(returnobj)
}



#################################################









