context("Test for creating and deleting a directory in a namespace ")


test_that("Creates a directory",
          {
            
          auth<-paste("Authorization: HCP ",base64enc::base64encode(charToRaw("s3test")),":",digest::digest("cepcepcep3",algo="md5",serialize=FALSE),sep="" )
          namespace<-"https://s3acl.s3-test.hcp-demo.hcpdomain.com"
          tempDir<-"testdir12344321"
          result<-createDir(tempDir,namespace,auth)     
          expect_that(result, equals("Directory Created"))
})

test_that("deletes a directory",
{
  auth<-paste("Authorization: HCP ",base64enc::base64encode(charToRaw("s3test")),":",digest::digest("cepcepcep3",algo="md5",serialize=FALSE),sep="" )
  namespace<-"https://s3acl.s3-test.hcp-demo.hcpdomain.com"
  tempDir<-"testdir12344321"
  result<-deleteDir(tempDir,namespace,auth)     
  expect_that(result, equals("Directory sucessfully deleted"))
  
})

