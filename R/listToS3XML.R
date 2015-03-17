#'Create  XML from a list object for a custom HCP annotation that is comaptable with S3 browers
#'\code{listToS3XML} Creates encoded authorization string using base64enc and digest packages
#'\code{listToS3XML} reate  XML from a list object for a custom HCP annotation that is comaptable with S3 browers
#'@param node name of root node in XML
#'@param list object to convert
#'@export
#'@examples
#'@example
#'\dontrun{listToS3XML(list(field1="a",field2="b"))}
#
listToS3XML <- function(sublist){
  metapairs <- newXMLNode("metapairs")
  for(i in 1:length(sublist)){
    child <- newXMLNode(paste("meta-",names(sublist)[i],sep=""), parent=metapairs);
    
    if (typeof(sublist[[i]]) == "list"){
      listToXML(child, sublist[[i]])
    }
    else{
      xmlValue(child) <- paste(sublist[[i]],sep="")
      #xmlValue(child) <- paste("[!CDATA[",sublist[[i]],"]",sep="")
    }
  } 
  metapairs}




