
if(file.exists("testfiles/cred"))
  { load("testfiles/cred")
  
  } else
  {

     
  credentials<-list(user=readline("enter user?"),pass=readline("enter password"))

  }

auth<-paste("Authorization: HCP ",base64enc::base64encode(charToRaw(credentials[["user"]])),":"
            ,digest::digest(credentials[["pass"]],algo="md5",serialize=FALSE),sep="" )

namespace<-"http://Rtest.parman.hcpdemo.gssd.hds.com"