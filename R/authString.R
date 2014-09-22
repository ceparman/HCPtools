#'Create authorization string for HCP access
#'\code{authString} Creates encoded authorization string using base64enc and digest packages
#'@param username username authorized for namespace
#'@param password password for username
#'@return returns authorization text string to be included Http header
#'@export
#'@examples
#'authString(readline("username - "),readline("password - "))
#

authString<-function(username,password)
{
  
auth<-paste("Authorization: HCP ",base64enc::base64encode(charToRaw(username)),
      ":",digest::digest(password,algo="md5",serialize=FALSE),sep="" )

return(auth)  
}
  
 