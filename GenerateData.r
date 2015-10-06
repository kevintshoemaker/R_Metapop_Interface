
source("C:\\Users\\Kevin\\Documents\\Employment\\Stony Brook\\MetaModels\\MPM Repository\\R_JSON_Server\\Rserver4_noserver.r")
source("C:\\Users\\Kevin\\Documents\\Employment\\Stony Brook\\MetaModels\\MPM Repository\\R_JSON_Server\\RMetamodelFunctions.r")
source("C:\\Users\\Kevin\\Documents\\Employment\\Stony Brook\\MetaModels\\MPM Repository\\R_JSON_Server\\MM_Functions.r")
source("C:\\Users\\Kevin\\Documents\\Employment\\Stony Brook\\MetaModels\\MPM Repository\\R_JSON_Server\\GenerateData_functions.r")



###########################
#########  SIMULATE ALL RPC CALLS

#file.remove("Stage_matrices_3.RData")
StopTimeStep(MetapopStateVarsPop=PopVars[[1]],ClientID=0)   # prey
StopTimeStep(MetapopStateVarsPop=PopVars[[2]],ClientID=1)   # pred
t=1;client=1

system.time(
 for(t in 1:ts){
   for(client in 1:nmodels){
     StartTimeStep(ClientID=client-1)
     StopTimeStep(MetapopStateVarsPop=PopVars[[client]],ClientID=client-1)
   }
 }
)


file.remove("Stage_matrices_3.RData")

############################
##############  CALL THE "MOD" FUNCTION (from MM_Functions.r)

ModVitalPrey1(Old=AllModifiers[[1]],ID=0)
ModVitalPred1(Old =AllModifiers[[2]],ID=1)


######################  TEST UNITS


### test multidimensional array:

returnArray <- function(x){
  a<-x
  b <- array(a,dim=c(3,3,3))
  return(b)
}



######################  Change toJSON for lists

temp3 <- setMethod(f="toJSON", signature="list",
           definition=function (x, container = isContainer(x, asIs, .level), collapse = " ", 
    ..., .level = 1L, .withNames = length(x) > 0 && length(names(x)) > 0, .na = "null", .escapeEscapes = TRUE, 
          pretty = FALSE,asIs = NA) {
    browser()
    if (length(x) == 0) {
        return(if (is.null(names(x))) "[]" else "{}")
    }
    els = lapply(x, toJSON, ..., .na = .na,   # .level = .level + 1L, 
        .escapeEscapes = .escapeEscapes, asIs = asIs)
    if (all(sapply(els, is.name))) 
        names(els) = NULL
    if (missing(container) && is.na(asIs)) 
        container = TRUE
    if (!container) 
        return(els)
    els = unlist(els)
    if (.withNames) 
        paste(sprintf("{%s", collapse), paste(dQuote(names(x)), 
            els, sep = ": ", collapse = sprintf(",%s", collapse)), 
            sprintf("%s}", collapse))
    else paste(sprintf("[%s", collapse), paste(els, collapse = sprintf(",%s", 
        collapse)), sprintf("%s]", collapse))
}
  #,where="package:RJSONIO"
)


############## PLAY WITH toJSON


 cat(toJSON(list(a = 1, b = 2L, c = TRUE,
                  d = c(1, 3),
                  e = "abc"), asIs = TRUE))





#######################TEST



 GlobalVars[[ClientID+1]] <- MetapopStateVarsGlobal   # save current global state vars to global workspace
  
  #################  Convert matrices and arrays back to original format... [shouldn't have to do this, but for now...]
  GlobalVars[[ClientID+1]]$constraintsMat <- listToMat(GlobalVars[[ClientID+1]]$constraintsMat)
  
  
     ###### INITIALIZE MODIFIER IN PREP FOR TIME LOOP ######

  AllModifiers[[ClientID+1]] <- list()     # generate metapop modifier object based on state variables...
 
    	# Spec: Categorical indicator of what is to be modified: abundance, vital
    	#       rates, carrying capacity or dispersal
    	#       (ultimately, it should be possible to modify more than one;
    	#       therefore, an array of Booleans may be most appropriate)

  AllModifiers[[ClientID+1]]$ChangeAbund <- FALSE
  AllModifiers[[ClientID+1]]$ChangeVital <- FALSE
  AllModifiers[[ClientID+1]]$ChangeK     <- FALSE   #   FALSE   #   
  AllModifiers[[ClientID+1]]$ChangeDisp  <- FALSE


    	# Spec: Last year for which complete simulation data are available for all
    	#       component models (scalar integer).
  AllModifiers[[ClientID+1]]$Timestep    <-  0    # need population state var to tell us what year it is.    #GlobalVars[[ClientID+1]]$

    	# Spec: Boolean indicator of whether this modifier object is complete and
    	#       ready to be used in a Metapop simulation (single Boolean).
  AllModifiers[[ClientID+1]]$Complete    <- TRUE

    	# Spec: Boolean indicator of whether this modifier object is currently in
    	#       effect (single Boolean). [[for now, always TRUE]]
  AllModifiers[[ClientID+1]]$Active      <- TRUE

    	# Spec: modified stage-specific abundances (matrix of dimension
    	#       "nPopulations", "nStages"). Replaces stage-specific abundances for
    	#       each population. Not compatible with catastrophes affecting
    	#       abundance.
  AllModifiers[[ClientID+1]]$Abund       <- array(0,dim=c(GlobalVars[[ClientID+1]]$nPopulations,GlobalVars[[ClientID+1]]$nStages))

    	# Spec: modified vital rates (matrix of dimension "nPopulations",
    	#       "nStages", "nStages"). Replaces mean population-specific transition
    	#       matrix values. Not compatible with temporal changes in vital rates
    	#       or catastrophes that affect vital rates.
  AllModifiers[[ClientID+1]]$Vital       <- array(0,dim=c(GlobalVars[[ClientID+1]]$nPopulations,GlobalVars[[ClientID+1]]$nStages,GlobalVars[[ClientID+1]]$nStages)) # array(0.1,dim=c(GlobalVars[[ClientID+1]]$nPopulations,GlobalVars[[ClientID+1]]$nStages,GlobalVars[[ClientID+1]]$nStages))

    	# Spec: modified mean population-specific carrying capacities (vector of
    	#       length "nPopulations"). Replaces mean population-specific K values.
    	#       Not compatible with "temporal change in K" or catastrophes that
    	#       affect K.
  AllModifiers[[ClientID+1]]$K           <- rep(0,times=GlobalVars[[ClientID+1]]$nPopulations)

    	# Spec: dispersal rate modifier (vector of length "nPopulations"). Modifies
    	#       dispersal rates relative to the rate expected for a given stage and
    	#       population pairing. Not compatible with catastrophes that affect
    	#       dispersal. [[this should probably be tabled for now, until we can
    	#       clarify the details]]
  AllModifiers[[ClientID+1]]$Disp  <<- numeric(GlobalVars[[ClientID+1]]$nPopulations)




#################################
####   MODVITALRATEPREY



ModVitalPrey1 <- function(Old, ID){
     #############
     #        INPUTS
     #   Old : Modifier object from the last year (to be updated)
     #   ID  : Client ID for the associated Metapop instance 
     
     New <- Old
     New$ChangeVital <- TRUE
     
     if(CurTimeStep[ID+1]==0) preyStMat <- as.matrix(PopVars[[1]]$popStMat[1,,])     # set base stage matrix for prey population
     
     flag <- FALSE   # initialize warning flag: no warnings
     ErrMsg <- "Error:"  # initialize error message
     
     
     if(CurTimeStep[ID+1]==0) CheckParams1()    # check for initial errors.... 
     #CheckParams2(ID=ID)
     
     # sum of all survivals from each stage when N is approx. 0 (when R=Rmax)
     
     p=1
     for(p in 1:GlobalVars[[ID+1]]$nPopulations){
          if(nrow(as.matrix(preyStMat))>1){
               TotalSurv <- apply(as.matrix(preyStMat)*
                                       as.matrix(GlobalVars[[ID+1]]$constraintsMat)*
                                       PreyRmax,2,sum)              # total maximum survival rate for each stage   PopVars[[ID+1]]$popRmax[p]
          }else{
               TotalSurv <- 0.5
          }
          
          if(length(which(TotalSurv>(1+tol)))>0) {      #### THIS SEEMS STRANGE: high survival stages will routinely throw this error, right!
               flag <<- TRUE
               ErrMsg <<- paste(ErrMsg," : "," survival above 1! ",sep="")
               break
          } 
          
          # if(length(which(TotalSurv<0.999999))>0) {throw error} # KTS: I'm not sure why this is an error condition
          
          if(PreyKcap<=0){    # if carrying capacity is zero?     KTS: why is this not an error condition 
               lmult <- 0             # lmult is a multiplier for survival and fecundity based on density
          } else{
               lmult <- (-1*log(PreyRmax) / PreyKcap) * PopVars[[ID+1]]$popAbundTot[p]    # determine DD multiplier
               if(abs(lmult)<exp_biggest){                         # if exponentiation is within reason
                    lmult <- exp(lmult)                                  # then exponentiate
               } else{
                    if(lmult > 0){                                    # otherwise, set to limits
                         lmult <- pow_biggest    # what is pow_biggest 
                    } 	else{
                         lmult <- 1/pow_biggest
                    }
               }
               
               # // At this point lmult~1 if N is small, and lmult=(1/Rmax) if N=K.  So:
               # // REMOVE the following line if eigenvalue of the matrix (EV)=Rmax; 
               # // KEEP it if EV=1.  These are the only two options.
               EV <- eigen(as.matrix(preyStMat))$values[1]   # dominant eigenvalue of the matrix  
               if(EV == 1){
                    lmult <- PreyRmax * lmult     # finally, multiply by Rmax to compute multiplier absent predator effect
               }
          }  # end if K <= 0
          
          #### PredMort is the per-capita prey mortality due to predation
          effectivePredPop <- as.vector(PopVars[[2]]$popAbundTot %*% (PredFeed[,p] * PreyAccess[p,]))  #Total number of pred with access to this prey pop
          PreyEffectivePredPop[p] <- effectivePredPop
          if((PopVars[[1]]$popAbundTot[p]==0)|(effectivePredPop)==0){
               PredMort <- 0
          } else{              # NOTE: ID 1 is prey pop, ID 2 is predator pop
               PredMort <- RDFuncResp(PopVars[[1]]$popAbundTot[p],effectivePredPop,alpha,htime) * 
                    (effectivePredPop/PopVars[[1]]$popAbundTot[p])
          }
          
          lmult <- lmult / exp(PredMort)         # finally, include predator effect
          New$Vital[p,,] <- as.matrix(preyStMat) * lmult
          
          PreyVital[p] <- New$Vital[p, GlobalVars[[ID+1]]$nStages, GlobalVars[[ID+1]]$nStages] 
     }
     
     if(flag==TRUE){     # if no errors, then return the new modifier object
          # return(New)
          # } else{
          New<-ErrMsg     # otherwise, return the relevant error message(s)
     }
     #if(flag==FALSE) PreyVital <<- New$Vital[1,GlobalVars[[ID+1]]$nStages, GlobalVars[[ID+1]]$nStages] 
     #return(New)
}












###########################
##########   KTS: random debugging tests

#PopVarsx[[1]]
#a<-listToMat(fromJSON(toJSON(PopVarsx[[1]]$popStMat)))

#GlobalVarsx[[1]]$constraintsMat
#b<-fromJSON(toJSON(GlobalVarsx[[1]]$constraintsMat))


        ######### GENERATE "do rpc" calls
json_parser <- rjson::newJSONParser() 
JSONtext <- "\n{\"method\":\"Initialize\",\"params\":{\"ClientID\":0},\"id\":525,\"jsonrpc\":\"2.0\"}\n"
json_parser$addData(JSONtext)
rpc <- try( json_parser$getObject(), silent = TRUE)
rpc$params <- as.list( rpc$params )          
result <- try( do.call( rpc$method, rpc$params ), silent = TRUE )
rpc_result <- list(
      jsonrpc = "2.0",     # assemble result object
      result = result,
      id = as.integer(rpc$id)
)

ret <- toJSON( rpc_result, container=TRUE, collapse = " ",asIs=TRUE )    
ret <- gsub("\n","",ret)    
ret <- paste( ret, "\n", sep="" ) 

cat(ret)

        ####### complicated one...
json_parser <- rjson::newJSONParser()
JSONtext <- "{\"method\":\"StartTimeStep\",\"params\":{\"ClientID\":0},\"id\":528,\"jsonrpc\":\"2.0\"}"
json_parser$addData(JSONtext)
rpc <- try( json_parser$getObject(), silent = TRUE)
rpc$params <- as.list( rpc$params )          
result <- try( do.call( rpc$method, rpc$params ), silent = TRUE )
rpc_result <- list(
      jsonrpc = "2.0",     # assemble result object
      result = result,
      id = as.integer(rpc$id)
)

ret <- toJSON( rpc_result, container=TRUE, collapse = " ",asIs=TRUE )    
ret <- gsub("\n","",ret)    
ret <- paste( ret, "\n", sep="" ) 

cat(ret)














