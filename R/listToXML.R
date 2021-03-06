#'Create  XML from a list object for a custom HCP annotation
#'\code{listToXML} Creates encoded authorization string using base64enc and digest packages
#'\code{listToXML} Creates  XML from a list object for a custom HCP annotation
#'\code{authString} Creates encoded authorization string using base64enc and digest packages
#'@param node name of root node in XML
#'@param list object to convert
#'@return XML object
#'@export



listToXML <- function(rootname, sublist){
  node<- newXMLNode(rootname)
  for(i in 1:length(sublist)){
    child <- newXMLNode(names(sublist)[i], parent=node);
    
    if (typeof(sublist[[i]]) == "list"){
      listToXML(child, sublist[[i]])
    }
    else{
      xmlValue(child) <- sublist[[i]]
    }
   } 
 node}


