#'queryies HCP with an XML file
#'\code{xmlFileQuery} gets a object (file) a namespace
#'@param xmlFile file path to xml query file
#'@param namespace namespace to get object list from
#'@param auth authentication string
#'@param verbose  Print a messages default = FALSE
#'@return returns XML object if sucessful or "Error" followed by the returned http headers.
#'@export
#'@examples
#'\dontrun{xmlFileQuery(xmlFile,saveName,namespace,auth,verbose=True)}
#
xmlFileQuery<-function(xmlFile,namespace,auth,verbose=FALSE) 
{
  #for testing
 # verbose <-TRUE
#  xmlFile<-"tests/testthat/testfiles/query1.xml"
  
  #query file
  f<-RCurl::CFILE(xmlFile,mode="rb")
  #gathers results
  j<-RCurl::basicTextGatherer()
  #rather returned header
  k<-RCurl::basicTextGatherer()
  
  #add content types to header
  header<-c(auth,"Content-Type: application/xml","Accept: application/xml")
  
 
  RCurl::curlPerform(url = paste(namespace,"/query?prettyprint",sep=""),
                     ssl.verifyhost = FALSE,
                     ssl.verifypeer = FALSE, 
                     post = 1L,
                     httpheader = header,
                     
                     upload=TRUE,
                     customrequest= "POST",
                     infilesize = file.info(xmlFile)[1, "size"],
                     writefunction=j$update,
                     readdata = f@ref,
                     
                     
                     headerfunction=k$update,
                     #encoding="gzip",
                     verbose=verbose 
  )
  RCurl::close(f) 
  
  

  if(verbose)print(strsplit(k$value(),"\r\n")  )
  
   if(strsplit(k$value(),"\r\n")[[1]][3] !="HTTP/1.1 200 OK")
   {
   return( paste("Error ",strsplit(k$value(),"\r\n")[[1]][3]))
   }
      
  XML::xmlInternalTreeParse(j$value())   
}

#xpathSApply(xmlFileQuery(xmlFile,namespace,auth),"//object",xmlGetAttr,"urlName")

   
     