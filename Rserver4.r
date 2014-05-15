
suppressWarnings(library( "rjson", quietly = TRUE))        # load rjson library
suppressWarnings(library( "utils", quietly = TRUE ))        # load utils library
suppressWarnings(suppressPackageStartupMessages(library( "RJSONIO", quietly= TRUE )))       # load the "other" json package
suppressWarnings(require( "stats", quietly = TRUE ))

##################################################################################################
######## FIX FUNCTIONS TO PROPERLY HANDLE ARRAYS AND MATRICES.... (modified from RJSONIO package)

options(useFancyQuotes = FALSE)
 
temp1 <- setMethod(f="toJSON", signature="matrix",
           definition=function(x, container =  isContainer(x, asIs, .level), collapse = " ", ...,   # was collapse = "\n"
                    .level = 1L, .withNames = length(x) > 0 && length(names(x)) > 0, .na = "null", .escapeEscapes = TRUE, pretty = FALSE, asIs = NA) {
			 #browser()
             tmp = paste(apply(x, 1, toJSON, .na = .na, ..., .escapeEscapes = .escapeEscapes), collapse = sprintf(",%s", collapse))
             if(!container)
               return(tmp)
              if(.withNames)
                paste("{", paste(dQuote(names(x)), tmp, sep = ": "), "}")                
              else
                paste("[", tmp, "]")
           }
		   #where="namespace:RJSONIO"
)
		   
                              # works for 3d arrays...
temp2 <- setMethod(f="toJSON", signature="array",
           definition=function(x, container =  isContainer(x, asIs, .level), collapse = " ", ...,   # was collapse = "\n"
                    .level = 1L, .withNames = length(x) > 0 && length(names(x)) > 0, .na = "null", .escapeEscapes = TRUE, pretty = FALSE, asIs = NA) {
			 #browser()
             tmp = paste(apply(x, 1, toJSON, .na = .na, ..., .escapeEscapes = .escapeEscapes), collapse = sprintf(",%s", collapse))
             if(!container)
               return(tmp)
              if(.withNames)
                paste("{", paste(dQuote(names(x)), tmp, sep = ": "), "}")                
              else
                paste("[", tmp, "]")
           }
		   #where=as.environment("namespace:RJSONIO")
)

temp3 <- setMethod(f="toJSON", signature="list",
           definition=function (x, container=FALSE, collapse = " ",    # container = isContainer(x, asIs, .level), 
    ..., .level = 1L, .withNames = length(x) > 0 && length(names(x)) > 0, .na = "null", .escapeEscapes = TRUE, 
          pretty = FALSE,asIs = NA) {
    #browser()
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

temp4 <- setMethod(f="toJSON", signature="integer",
           definition=function (x, container = FALSE, collapse = " ", 
    ..., .level = 1L, .withNames = length(x) > 0 && length(names(x)) > 
        0, .na = "null", .escapeEscapes = TRUE, pretty = FALSE, 
    asIs = NA) {
	#browser()
    if (any(nas <- is.na(x))) 
        x[nas] = .na
    if (container) {
        if (.withNames) 
            paste(sprintf("{%s", collapse), paste(dQuote(names(x)), 
                x, sep = ": ", collapse = sprintf(",%s", collapse)), 
                sprintf("%s}", collapse))
        else paste("", paste(x, collapse = ", "), "")
    } else as.character(x)
}

)


temp5 <- setMethod(f="toJSON", signature="logical",
           definition=function (x, container = FALSE, collapse = " ", 
    ..., .level = 1L, .withNames = length(x) > 0 && length(names(x)) > 
        0, .na = "null", .escapeEscapes = TRUE, pretty = FALSE, 
    asIs = NA) {
	#browser()
    tmp = ifelse(x, "true", "false")
    if (any(nas <- is.na(tmp))) 
        tmp[nas] = .na
    if (container) {
        if (.withNames) 
            paste(sprintf("{%s", collapse), paste(dQuote(names(x)), 
                tmp, sep = ": ", collapse = sprintf(",%s", collapse)), 
                sprintf("%s}", collapse))
        else paste("", paste(tmp, collapse = ", "), "")
    } else tmp
}
)

temp6 <- setMethod(f="toJSON", signature="character",
           definition=function (x, container = FALSE, collapse = " ", 
    ..., .level = 1L, .withNames = length(x) > 0 && length(names(x)) > 
        0, .na = "null", .escapeEscapes = TRUE, pretty = FALSE, 
    asIs = NA) {
	 #browser()
    .local <- function (x, container = FALSE, 
        collapse = "\n", digits = 5, ..., .level = 1L, .withNames = length(x) > 
            0 && length(names(x)) > 0, .na = "null", .escapeEscapes = TRUE, 
        pretty = FALSE, asIs = NA) 
    {
        tmp = x
        tmp = gsub("(\\\\)", "\\1\\1", tmp)
        if (.escapeEscapes) {
            tmp = gsub("\\t", "\\\\t", tmp)
            tmp = gsub("\\n", "\\\\n", tmp)
        }
        tmp = gsub("\"", "\\\\\"", tmp)
        tmp = dQuote(tmp)
        if (any(nas <- is.na(x))) 
            tmp[nas] = .na
        if (container) {
            if (.withNames) 
                paste(sprintf("{%s", collapse), paste(dQuote(names(x)), 
                  tmp, sep = ": ", collapse = sprintf(",%s", 
                    collapse)), sprintf("%s}", collapse))
            else paste("", paste(tmp, collapse = ", "), "")
        }else tmp
    }
    .local(x, container, collapse, ..., .level = .level, .withNames = .withNames, 
        .na = .na, .escapeEscapes = .escapeEscapes, pretty = pretty, 
        asIs = asIs)
}
)



####################################
### make function to convert lists back to matrices and arrays
###   converts outputs from JSONIO back to the original matrix or array format 

listToMat <- function(inp){
  l <- length(inp)
  if(is.list(inp[[1]])){        # if it's a 3-d array...
    l2 <- length(inp[[1]])
    l3 <- length(inp[[1]][[1]])
    result <- array(0,dim=c(l,l2,l3))
  } else{
    l2 <- length(inp[[1]])
    result <- array(0,dim=c(l,l2))
  }
  for(i in 1:l){
    if(is.list(inp[[i]])){
      for(j in 1:l2){
        result[i,j,]<-inp[[i]][[j]]
      }
    } else{
      result[i,] <- inp[[i]]
    }
  }
  return(result)
}



##################################
#########   READ IN SOURCE CODE FOR ALL METAMODEL PROCEDURES...

  # read in the functions to be used... (may be stored in separate files...)

source("C:\\Users\\Kevin\\Documents\\Employment\\Stony Brook\\MetaModels\\MPM Repository\\R_JSON_Server\\RMetamodelFunctions.r")
source("C:\\Users\\Kevin\\Documents\\Employment\\Stony Brook\\MetaModels\\MPM Repository\\R_JSON_Server\\MM_Functions.r")
#source(file.path(getwd() ,"R_JSON_Server\\MM_Functions.r"))
#source("C:\\DATA\\RAMAS\\R_JSON_Server\\RMetamodelFunctions.r")
#source("C:\\DATA\\RAMAS\\R_JSON_Server\\MM_Functions.r")


###########################################
#####################  DO.RPC is a function for implementing the method specified in the JSON-RPC "request" object
                                 #returns: a JSON RPC result object or encoded error message
do.rpc <- function( rpc ){
  #options(useFancyQuotes = TRUE)   #KTS: good change?
  rpc$params <- as.list( rpc$params )          
  result <- try( do.call( rpc$method, rpc$params ), silent = TRUE )    # "do.call" takes a function and applies the list as parameters to the function 
  if( class( result ) == "try-error" ) {
    rpc_result <- list(
      jsonrpc = "2.0",
      error = list( code = -32601, message = "Procedure not found.", data = as.character( result ) ),
      id = rpc$id
    )
  } else {    #RPC call suceeded
    rpc_result <- list(
      jsonrpc = "2.0",     # assemble result object
      result = result,
      id = as.integer(rpc$id)
    )
  }           
              ## return the JSON string
  #options(useFancyQuotes = FALSE)
  ret <- toJSON( rpc_result, container=TRUE, collapse = " ",asIs=TRUE )    # RJSONIO::  # converts the result of the function call to JSON interchange format...   removed: asIs=TRUE
  ret <- gsub("\n","",ret)    # stupid: RJSONIO adds a \n character in arrays
  ret <- paste( ret, "\n", sep="" )       # add newline character, useful for "cat" statement?
  return( ret )
}



#############################################
######################         INITIALIZE SERVER 

process_stdin <- file("stdin", blocking = T, open = "rb" )     	# define a read binary connection with "stdin"
	
json_parser <- rjson::newJSONParser()                          	# starts a new JSON parser structure ( list of functions used for parsing JSON objects)   


#############################################
                      # START SERVER

while( TRUE ) {         					# accept any number of requests, until the connection is broken 
                                                    

  s <- readBin(process_stdin, what = raw(), n = 1)  	# read in one raw byte at a time    
  if( length(s) == 0 )  break    			# if line entered with nothing in it, break out of while loop
  s <- rawToChar( s )           			# convert raw byte to character
  json_parser$addData(s)        			# add input to parser buffer, one character at a time
  while( s == "}" ) {      				# Optimization: JSON RPC objects MUST terminate with a `}' - no need to check if the object can be parsed otherwise (since it can't)                                                          
    rpc <- try( json_parser$getObject(), silent = TRUE)  # try to parse any JSON RPC objects
    if( class(rpc) == "try-error" ) {                     # if an error occurred in the parsing...
      cat( '{"jsonrpc": "2.0", "error": {"code": -32700, "message": "Parse error"}, "id": null}') 
      json_parser <- rjson::newJSONParser()    		# reset the json parser... clear anything on the input
      seek( process_stdin, where = 0, origin = "end") 	# not enough data is in the buffer to extract a complete JSON object
    } else {
      if( is.null(rpc) ) break
      ret <- do.rpc(rpc)                   		# a valid JSON object was extracted: perform the requested procedure
      cat(ret)                             		# then write to output             
    } 
  }   							# end script for JSON interpreting
}   # end WHILE loop

#q()
