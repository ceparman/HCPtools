#'Creates a directory in a namespace
#'\code{createDirectory}create a directory in a namespace
#'@param dirName name for the new directory
#'@param auth authorization string created with authString
#'@param namespace namespace to get object list from 
#'@param verbose  Print a messages default = FALSE
#'@return r
#'@export
#'@examples
#'\dontrun{}
#
createDir<-function(dirName,namespace,auth,verbose=FALSE)
{
  #create a directory
  curl =  RCurl::getCurlHandle()
  k = RCurl::basicTextGatherer()
  RCurl::curlPerform(
    url = paste(namespace,"/rest/",dirName,"?type=directory",sep=""),
    curl=curl,
    ssl.verifypeer = FALSE, 
    ssl.verifyhost = FALSE,
    put = TRUE,
    httpheader = auth,
    headerfunction=k$update,
    verbose=verbose)
  
  
  if(strsplit(k$value(),"\r")[[1]][1] =="HTTP/1.1 201 Created")
  { return("Directory Created")
  }
  else return( paste("Error",strsplit(k$value(),"\r")[[1]][1]))
   
}

