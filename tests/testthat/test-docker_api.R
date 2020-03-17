check_docker_install <- function() {
    code <- suppressWarnings(
        system("docker --version", ignore.stderr = TRUE)
    )

    if (code != 0) {
        skip("'docker' engine not installed on machine.")
    }
}

test_that(".docker_images works as expected", {
    check_docker_install()

    expect_silent(
        BiocDockerManager:::.docker_pull(
            "bioconductor/bioconductor_docker", 
            tag = "devel"
        )
    )

    res <- .docker_images("bioconductor/bioconductor_docker")

    expect_equal(ncol(res), 5)
    expect_identical(names(res), c("REPOSITORY", "TAG", "IMAGE ID", "CREATED","SIZE"))
})


test_that(".docker_pull works as expected", {

    check_docker_install()

    expect_silent(
        BiocDockerManager:::.docker_pull(
            "bioconductor/bioconductor_docker", 
            tag = "devel"
        )
    )

    res <- installed()
    expect_true("bioconductor/bioconductor_docker" %in% res$REPOSITORY)
    expect_true("devel" %in% res$TAG)
    
})


test_that(".docker_inspect_label works as expected", {
    check_docker_install()

    expect_silent(
        BiocDockerManager:::.docker_pull(
            "bioconductor/bioconductor_docker", 
            tag = "devel"
        )
    )

    expect_equal(
        .docker_inspect_label(
            "bioconductor/bioconductor_docker", 
            tag = "devel", label = "name"
        ), "bioconductor/bioconductor_docker"
    )

    expect_equal(
        .docker_inspect_label(
            "bioconductor/bioconductor_docker", 
            tag = "devel", label = "license"
        ), "Artistic-2.0"
    )

    expect_equal(
        .docker_inspect_label(
            "bioconductor/bioconductor_docker", 
            tag = "devel", label = "url"
        ), "https://github.com/Bioconductor/bioconductor_docker"
    )

    expect_equal(
        .docker_inspect_label(
            "bioconductor/bioconductor_docker", 
            tag = "devel", label = "vendor"
        ), "Bioconductor Project"
    )

    expect_error(
        .docker_inspect_label(
            "bioconductor/bioconductor_docker", 
            tag = "devel", label = "not-available")
    )
})

test_that(".docker_inspect_digest works as expected", {

    check_docker_install()

    expect_silent(
        BiocDockerManager:::.docker_pull(
            "bioconductor/bioconductor_docker", 
            tag = "devel"
        )
    )

    res <- .docker_inspect_digest(
                "bioconductor/bioconductor_docker", tag = "devel"
            )

    expect_is(res, "tbl_df")
    expect_identical(names(res), c("REPOSITORY", "TAG","DIGEST"))
    expect_true("bioconductor/bioconductor_docker" %in% res$REPOSITORY)
    expect_true("devel" %in% res$TAG)
    expect_true(nchar(res$DIGEST[1]) == 71)
    expect_match(res$DIGEST, "sha256:.")

})

# > valid()
# Error: No such object: bioconductor/bioconductor_docker:latest
# Error: No such object: bioconductor/bioconductor_docker:RELEASE_3_10
# The following bioconductor images need to be updated:

# # A tibble: 2 x 2
#   REPOSITORY                       TAG
#   <chr>                            <chr>
# 1 bioconductor/bioconductor_docker latest
# 2 bioconductor/bioconductor_docker RELEASE_3_10
# Warning messages:
# 1: In system2(cmd, args, if (stdout) stdout = stdout) :
#   running command ''docker' inspect -f '{{.RepoDigests}}' bioconductor/bioconductor_docker:latest' had status 1
# 2: In system2(cmd, args, if (stdout) stdout = stdout) :
#   running command ''docker' inspect -f '{{.RepoDigests}}' bioconductor/bioconductor_docker:RELEASE_3_10' had status 1
