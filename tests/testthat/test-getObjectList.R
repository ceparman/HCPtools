context("Test for getting list of objects from existing namespace, ")

test_that("Returns object list",
          {
            if( !(exists("credentials") & exists("namespace"))) source("testAuth.R")

#Assumes starting with a empty namespace

          result<-getObjectsList(namespace,auth)     
          expect_that(result[1], equals(".lost%2bfound"))
          expect_that(length(result),equals(1))
})

test_that("Returns return error with wrong password",
{
 #if !(exists("credentials") & exists("namespace")) source("testAuth.R")
  
  auth<-paste("Authorization: HCP ",base64enc::base64encode(charToRaw("testuser")),":",
              digest::digest("mypasswor",algo="md5",serialize=FALSE),sep="" )
  result<-getObjectsList(namespace,auth)     
  expect_that(result, equals("Error HTTP/1.1 403 Forbidden"))

})