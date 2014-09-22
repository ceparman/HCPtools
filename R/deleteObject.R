#'Deletes an object in a namespace
#'\code{deleteObject} delete an object in a namespace
#'@param ObjectName name of the directory to delete
#'@param auth authorization string created with authString
#'@param namespace namespace to get object list from 
#'@param verbose  Print a messages default = FALSE
#'@return Upon success "Object sucessfully deleted", else erroe message
#'@export
#'@examples
#'\dontrun{deleteObject("myFile",namespace,auth)}
#
deleteObject<-function(objectName,namespace,auth,verbose=FALSE)
{
  curl =  RCurl::getCurlHandle()
  k = RCurl::basicTextGatherer()
  RCurl::curlPerform(
    url = paste(namespace,"/rest/",objectName,sep=""),
    curl=curl,
    ssl.verifypeer = FALSE, 
    ssl.verifyhost = FALSE,
    customrequest = "Delete",
    httpheader = auth,
    headerfunction=k$update,
    verbose=verbose)
  
  
  if(strsplit(k$value(),"\r")[[1]][1] =="HTTP/1.1 200 OK")
  { return(paste(objectName,"sucessfully deleted"))
  }
  else return( paste("Error",strsplit(k$value(),"\r")[[1]][1]))
   
}

