context("dockerhub api")

test_that(".docker_image_description is as expected", {
    expect_equal(
        BiocDockerManager:::.docker_image_description("bioconductor/bioconductor_docker"),
                 "Bioconductor Docker Images")
})


test_that(".docker_image_list is as expected", {
    res <- BiocDockerManager:::.docker_image_list("bioconductor")
    expect_equal(sum(grepl("bioconductor_docker", res)), 1)
})


test_that(".docker_repository_list is as expected", {
    res <- BiocDockerManager:::.docker_repository_list("bioconductor", "bioconductor_docker")
    expect_equal(res, "bioconductor/bioconductor_docker")

    res <- BiocDockerManager:::.docker_repository_list("bioconductor", "website")
    expect_equal(res, "bioconductor/website")
})


test_that(".docker_image_tags_list is as expected", {
    res <- .docker_image_tags_list("bioconductor/bioconductor_docker")
    expect_length(res, 3)
    expect_true("latest" %in% res)
    expect_true("RELEASE_3_10" %in% res)
    expect_true("devel" %in% res)
})


test_that(".docker_image_digests is as expected", {
    res <- .docker_image_digests("bioconductor/bioconductor_docker")
    expect_type(res, "list")
    expect_is(res, "tbl_df")

    ## Check length of digest sha
    expect_true(nchar(res$DIGEST[1]) == 71)
    expect_match(res$DIGEST, "sha256:.")
})

test_that(".docker_image_pull_count is as expected", {
    res <- .docker_image_pull_count("bioconductor/bioconductor_docker")
    expect_gt(res,1500)
})

test_that(".docker_get_query is as expected", {
    path = "https://hub.docker.com/v2/repositories/bioconductor"

    expect_is(GET(path), "response")
    expect_equal(GET(path)$status, 200)
})