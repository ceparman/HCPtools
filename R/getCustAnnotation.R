#'gets a custom annotation of an object from HCP
#'\code{getCustAnnotation} gets a object (file) a namespace
#'@param objectName path to file to be uploaded
#'@param annotationName name of stored custome annotation
#'@param saveName name for annotation
#'@param namespace namespace to get object list from
#'@param auth authentication string
#'@param verbose  Print a messages default = FALSE
#'@return returns "annotation saved in ..." if sucessful, "Error" followed by the returned http headers.
#'@export
#'@examples
#'\dontrun{getCustAnnotation(objectName,annotationName,saveName,namespace,auth,verbose=True)}
#
getCustAnnotation<-function(objectName,annotationName,saveName,namespace,auth,verbose=FALSE) 
{
 
  k = RCurl::basicTextGatherer()
  #get a file
  f<-RCurl::CFILE(saveName ,mode="wb")
      
    RCurl::curlPerform(url =paste(namespace,"/rest/",objectName,"?type=custom-metadata&annotation=",annotationName,sep=""),
                       httpheader = auth, 
                       writedata = f@ref,   
                       ssl.verifyhost = FALSE,
                       ssl.verifypeer = FALSE, 
                       headerfunction=k$update,
                       verbose=verbose    
       )
    
  RCurl::close(f)

  if(verbose)print(strsplit(k$value(),"\r\n")  )
  
   if(strsplit(k$value(),"\r\n")[[1]][1] !="HTTP/1.1 200 OK")
   {
   return( paste("Error ",strsplit(k$value(),"\r\n")[[1]][3]))
   }
      
    
}



   
     