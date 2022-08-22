context("manager functions")

check_docker_install <- function() {
    code <- suppressWarnings(
        system(
            "docker --version",
            ignore.stderr = TRUE,
            ignore.stdout = TRUE
        )
    )

    if (code != 0) {
        skip("'docker' engine not installed on machine.")
    }
}

## available()
test_that("'available()' works as expected", {
    res <- BiocDockerManager::available()

    expect_true("bioconductor_docker" %in% res$IMAGE)
    expect_true("website" %in% res$IMAGE)

    expect_true(
        grepl("RELEASE_3_10",
              res[res$IMAGE == "bioconductor_docker", "TAGS"])
    )

    ## only one image is present
    res1 <- BiocDockerManager::available(
                pattern = "bioconductor_", 
                organization = "bioconductor", 
                deprecated = FALSE)
    expect_true("bioconductor_docker" %in% res1$IMAGE)
    expect_length(res1, 4)
    expect_length(nrow(res1), 1)

    ## include deprecated
    res2 <- BiocDockerManager::available(deprecated = TRUE)
    expect_true("bioconductor_full" %in% res2$IMAGE)
    expect_true("release_core2" %in% res2$IMAGE)
})


## install()
test_that("'install()' works as expected", {
    check_docker_install()

    expect_silent(
        BiocDockerManager::install(
            "bioconductor/bioconductor_docker",
            tag="RELEASE_3_10"
        )
    )
})


## version()
test_that("'version()' works as expected", {

    check_docker_install()
    ver <- BiocDockerManager::version(
        "bioconductor/bioconductor_docker", 
        tag = "RELEASE_3_10")

    expect_equal(ver, "3.10.3")

})


## maintainer()
test_that("'maintainer()' works as expected", {
    check_docker_install()

    maintainer <- BiocDockerManager::maintainer(
        "bioconductor/bioconductor_docker", 
        tag = "RELEASE_3_10")

    expect_match(maintainer, "maintainer@bioconductor.org", 
                fixed=TRUE)

})


## installed(), this is similar to .docker_images
test_that("'installed()' works as expected", {
    check_docker_install()
    res <- BiocDockerManager::installed()
    expect_is(res, "tbl_df")
})


## valid()
test_that("'valid()' works as expected", {
    check_docker_install()

    ## There should be nothing to update
    res <- BiocDockerManager::valid("bioconductor/bioconductor_docker", 
                                    tag = "RELEASE_3_10")
    expect_equal(dim(res)[1], 0)

    ## All images should need updating
    res <- BiocDockerManager::valid()
    expect_is(res, "tbl_df")
    if (nrow(res) > 0) {
        expect_match(res$REPOSITORY, "bioconductor")
    }
    
})
