context("Test forgetting list of objects from exixting namespace, ")
test_that("Returns object list",
          {
          auth<-paste("Authorization: HCP ",base64enc::base64encode(charToRaw("s3test")),":",digest::digest("cepcepcep3",algo="md5",serialize=FALSE),sep="" )
          namespace<-"https://s3acl.s3-test.hcp-demo.hcpdomain.com"
          
          result<-getObjectsList(namespace,auth)     
          expect_that(result[1], equals(".lost%2bfound"))
          expect_that(length(result),equals(6))
})

test_that("Returns return error with wrong password",
{
  auth<-paste("Authorization: HCP ",base64enc::base64encode(charToRaw("s3test")),":",digest::digest("cepcepcep",algo="md5",serialize=FALSE),sep="" )
  namespace<-"https://s3acl.s3-test.hcp-demo.hcpdomain.com"
  
  result<-getObjectsList(namespace,auth)     
  expect_that(result, equals("Error HTTP/1.1 403 Forbidden"))

})