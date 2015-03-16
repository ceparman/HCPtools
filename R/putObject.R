#'puts a file to HCP
#'\code{putFile} puts a file to a namespace
#'@param filePath path to file to be uploaded
#'@param filename name for object in namespace
#'@param namespace namespace to get object list from
#'@param auth authentication string
#'@param compress use gzip compression for upload default = FALSE 
#'@param verbose  Print a messages default = FALSE
#'@return returns charactor array of object names if sucessful, Otherwise returms "Error" followed by the returned http headers.
#'@export
#'@examples
#'\dontrun{getObjectsList(namespace,auth)}
#
putFile<-function(filePath,fileName,namespace,auth,compress=FALSE,verbose=FALSE) 
{
  if(compress) encoding = "gzip" else encoding = "none"
  k = RCurl::basicTextGatherer()
  #put a file
  f<-RCurl::CFILE(filePath,mode="rb")
  RCurl::curlPerform(url =paste(namespace,"/rest/",fileName,sep=""),
              httpheader = auth, 
              upload=TRUE,     
              infilesize = file.info(filePath)[1, "size"],
              ssl.verifyhost = FALSE,
              ssl.verifypeer = FALSE, 
              headerfunction=k$update,
              readdata = f@ref,
              encoding=encoding,
              verbose=verbose 
  )
  RCurl::close(f)
  
  
  if(strsplit(k$value(),"\r\n")[[1]][3] =="HTTP/1.1 201 Created")
  {
    
    return("HTTP/1.1 201 Created")
  }
  else return( paste("Error ",strsplit(k$value(),"\r\n")[[1]][3]))
  
}


#putFile(filePath,fileName,namespace,auth,verbose=FALSE) 

