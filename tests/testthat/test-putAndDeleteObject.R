context("Test for uploading, annotating,and deleting a file into an existing namespace, ")

test_that("uploading a file",
          {
          source("testAuth.R")
          fileName="lab9.xml"
          filePath="testfiles/lab9.r.xml"
          result<-putFile(filePath,fileName,namespace,auth,verbose=FALSE)  
          expect_that(result, equals("HTTP/1.1 201 Created"))
         
})

test_that("Returns error with name conflict",
{
         source("testAuth.R")
         fileName="lab9.xml"
          filePath="testfiles/lab9.r.xml"
          result<-putFile(filePath,fileName,namespace,auth,verbose=FALSE)  
          expect_that(result, equals("Error  HTTP/1.1 409 Conflict"))
})

test_that("add annotation to object we just created",
{
  source("testAuth.R")
  fileName="lab9.xml"
  filePath="testfiles/ann.xml"
  annotationName="TestAnn"   
  result<-addCustAnnotation(fileName,namespace,auth,filePath,annotationName,verbose=FALSE) 
  expect_that(result, equals("HTTP/1.1 201 Created"))
})


test_that("get the annotation to object we just created",
{
  source("testAuth.R")
  fileName="lab9.xml"
  filePath="testfiles/ann.xml"
  annotationName="TestAnn"
  saveName<-"save.xml"
  result<-getCustAnnotation<-function(objectName,annotationName,saveName,namespace,auth,verbose=FALSE)
  expect_that(result, equals("HTTP/1.1 200 OK"))
})




test_that("Deletes object we just created",
{
  source("testAuth.R")
  fileName="lab9.xml"
  result<-deleteObject(fileName,namespace,auth,verbose=FALSE)  
  expect_that(result, equals("lab9.xml sucessfully deleted"))
})

test_that("Error when deleting already deleted object",
{
  source("testAuth.R")
  fileName="lab9.xml"
  result<-deleteObject(fileName,namespace,auth,verbose=FALSE)  
  expect_that(result, equals("Error HTTP/1.1 404 Not Found"))
})
