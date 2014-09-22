
test_that("auth returns a the proper string",
         
         {auth<-paste("Authorization: HCP ",base64enc::base64encode(charToRaw("testuser")),":",digest::digest("mypassword",algo="md5",serialize=FALSE),sep="" )
          expect_that(auth, equals("Authorization: HCP dGVzdHVzZXI=:34819d7beeabb9260a5c854bc85b3e44"))
         })
