#'adds a custom annotation to an object
#'\code{addCustannotation} puts a file to a namespace
#'@param filename name for object in namespace
#'@param namespace namespace to get object list from
#'@param auth authentication string
#'@param filePath path to annotation file
#'@param annotationName name for the custom annotation slot
#'@param verbose  Print a messages default = FALSE
#'@return returns character array of object names if sucessful, Otherwise returms "Error" followed by the returned http headers.
#'@export
#'@examples
#'\dontrun{addCustannotation(namespace,auth)}
#
addCustAnnotation<-function(fileName,namespace,auth,filePath,annotationName,verbose=FALSE) 
{
    k = RCurl::basicTextGatherer()
  #put a file
  f<-RCurl::CFILE(filePath,mode="rb")
  RCurl::curlPerform(url =paste(namespace,"/rest/",fileName,"?type=custom-metadata&annotation=",annotationName,sep=""),
              httpheader = auth, 
              upload=TRUE,     
              infilesize = file.info(filePath)[1, "size"],
              ssl.verifyhost = FALSE,
              ssl.verifypeer = FALSE, 
              headerfunction=k$update,
              readdata = f@ref,
              verbose=verbose 
  )
  RCurl::close(f)
  

  
   if(strsplit(k$value(),"\r\n")[[1]][3] =="HTTP/1.1 201 Created")   
     {

      return("HTTP/1.1 201 Created")
    }
   else return( paste("Error ",strsplit(k$value(),"\r\n")[[1]][3]))
  
}


