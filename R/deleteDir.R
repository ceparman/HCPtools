#'Deletes an empty directory in a namespace
#'\code{deleteDirectory} delete a directory in a namespace
#'@param dirName name of the directory to delete
#'@param auth authorization string created with authString
#'@param namespace namespace to get object list from 
#'@param verbose  Print a messages default = FALSE
#'@return Upon success "Directory sucessfully deleted", else erroe message
#'@export
#'@examples
#'\dontrun{deleteDir("testdir3",namespace,auth)}
#
deleteDir<-function(dirName,namespace,auth,verbose=FALSE)
{
  #create a directory
  curl =  RCurl::getCurlHandle()
  k = RCurl::basicTextGatherer()
  RCurl::curlPerform(
    url = paste(namespace,"/rest/",dirName,sep=""),
    curl=curl,
    ssl.verifypeer = FALSE, 
    ssl.verifyhost = FALSE,
    customrequest = "Delete",
    httpheader = auth,
    headerfunction=k$update,
    verbose=verbose)
  
  
  if(strsplit(k$value(),"\r")[[1]][1] =="HTTP/1.1 200 OK")
  { return("Directory sucessfully deleted")
  }
  else return( paste("Error",strsplit(k$value(),"\r")[[1]][1]))
   
}

