#'gets a object (file) from HCP without annotations
#'\code{authString} gets a object (file) a namespace
#'@param objectName path to file to be uploaded
#'@param saveName name for object in namespace
#'@param namespace namespace to get object list from
#'@param auth authentication string
#'@param compress use gzip compression for upload default = FALSE
#'@param validate validate file checksum 
#'@param verbose  Print a messages default = FALSE
#'@return returns "File saved" if sucessful, "Error" followed by the returned http headers.
#'@export
#'@examples
#'\dontrun{getOject(objectName,saveName,namespace,auth,compress=TRUE,verbose=True)}
#
getObject<-function(objectName,saveName,namespace,auth,compress=FALSE,validate=TRUE,verbose=FALSE) 
{
 
  k = RCurl::basicTextGatherer()
  #get a file
  f<-RCurl::CFILE(saveName ,mode="wb")
  
  if(compress){
  
    RCurl::curlPerform(url = paste(namespace,"/rest/",objectName,sep=""),
              httpheader = auth, 
              writedata = f@ref,   
              ssl.verifyhost = FALSE,
              ssl.verifypeer = FALSE, 
              headerfunction=k$update,
              encoding="gzip",
              verbose=verbose 
  )
  }  else {
    
    RCurl::curlPerform(url =paste(namespace,"/rest/",objectName,sep=""),
                       httpheader = auth, 
                       writedata = f@ref,   
                       ssl.verifyhost = FALSE,
                       ssl.verifypeer = FALSE, 
                       headerfunction=k$update,
                       verbose=verbose 
    
    )
    
  }
  
  
  
  
  
  RCurl::close(f)
  if(verbose)print(strsplit(k$value(),"\r\n")  )
  
   if(strsplit(k$value(),"\r\n")[[1]][1] !="HTTP/1.1 200 OK")
   {
   return( paste("Error ",strsplit(k$value(),"\r\n")[[1]][3]))
   }
      
   if(validate)
   {
     x<- readBin(saveName,raw(),file.info(saveName)$size) 
     s<-toupper(digest(x,algo="sha256",serialize=FALSE))
     if (s != strsplit (  strsplit(k$value(),"\r\n")[[1]][16]," ")[[1]][3]){
      file.remove(saveName)   
     return("checksum error, destination file removed")
      }
   }
     return("HTTP/1.1 200 OK") 
}


#getObject("IMG_20140629_181735.jpg","temp.jpg",namespace,auth,compress=TRUE,validate=TRUE,verbose=TRUE)




   
     