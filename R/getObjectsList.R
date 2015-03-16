#'get current objects a directory
#'\code{authString} CReturns list of current object in a directory
#'@param auth authorization string created with authString
#'@param namespace namespace to get object list from 
#'@param verbose  Print a messages default = FALSE
#'@return returns charactor array of object names if sucessful, 
#'Otherwise returms "Error" followed by the returned http headers.
#'@export
#'@examples
#'\dontrun{getObjectsList(namespace,auth)}
#

getObjectsList<-function(namespace,auth,verbose=FALSE)
{
  #get current objects a directory
  
  
  h = RCurl::basicTextGatherer()
  k = RCurl::basicTextGatherer()
  
  curl = RCurl::getCurlHandle()
  RCurl::curlPerform(url = paste(namespace,"/rest",sep=""),
              curl=curl,
            #  ssl.verifypeer = FALSE, 
            #  ssl.verifyhost = FALSE,
              writefunction=h$update,
              headerfunction=k$update,
              httpheader = auth,
              verbose = verbose)
  
  
  if(strsplit(k$value(),"\r")[[1]][1] =="HTTP/1.1 200 OK")
  {
    
    doc<-XML::xmlInternalTreeParse(h$value())
    
    objects<-unlist(XML::xpathApply(doc, "//entry[@urlName]", XML::xmlGetAttr, "urlName"))
    return(objects)
  }
  else return( paste("Error",strsplit(k$value(),"\r")[[1]][1]))
}


#getObjectsList(namespace,auth,verbose=TRUE) 




