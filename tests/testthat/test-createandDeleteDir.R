context("Test for creating and deleting a directory in a namespace ")


test_that("Creates a directory",
          {
            
            source("testAuth.R")
          tempDir<-"testdir12344321"
          result<-createDir(tempDir,namespace,auth)     
          expect_that(result, equals("Directory Created"))
})

test_that("deletes a directory",
{
  source("testAuth.R")
  tempDir<-"testdir12344321"
  result<-deleteDir(tempDir,namespace,auth)     
  expect_that(result, equals("Directory sucessfully deleted"))
  
})

