context("Test dockerhub api for Bioconductor")

test_that("dockerhub requests happen", {
    expect_is(httr::GET("https://hub.docker.com/v2/repositories/bioconductor"),
              "response")
    expect_is(
        GET("https://hub.docker.com/v2/repositories/bioconductor/response-headers",
            query=list(`Content-Type`="application/json")),
        "response"
    )
})
