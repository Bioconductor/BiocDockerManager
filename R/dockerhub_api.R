#' Dockerhub API get query
#'
#' @importFrom httr GET stop_for_status content
.docker_get_query <-
    function(query)
{
    response <- GET(query)
    stop_for_status(response)
    content(response)
}

#' Build path for Dockerhub API query
#'
.docker_get <-
    function(path, api = "https://hub.docker.com/v2",
             path_root = "/repositories/")
{
    query <- sprintf("%s%s%s", api, path_root, path)
    .docker_get_query(query)
}


#' Get Docker image pull count (number of times downloaded)
#'
#' @param `character(1)` docker image name with organization
#'
#' @examples
#' .docker_image_pull_count("bioconductor/bioconductor_docker")
#'
#' @return `numeric(1)` number showing how many times the image has
#'     been downloaded
.docker_image_pull_count <-
    function(image = character(1))
{
    .docker_get(image)$pull_count
}

#' Get the list of images under an organization account.
#'
#' @details Get the list of images under and organization or
#'     username. Important to note that organization/username is
#'     different from the image name on dockerhub.
#'
#' @param `character(1)` name of organization or "username".
#'
#' @examples
#' .docker_image_list("bioconductor")
#'
#' @return `character(1)` vector of images hosted by the organization
#'     on dockerhub.
#'
.docker_image_list <-
    function(organization = character(1))
{
    org_pages <- .docker_get(sprintf("%s/?page_size=100",organization))
    paste(
        organization,
        vapply(org_pages$results, `[[`, character(1), "name"),
        sep="/"
    )
}

#' List the tags of an Image
.docker_image_tags_list <-
    function(image = character(1))
{
    tags_pages <- .docker_get(paste(image, "tags", sep="/"))
    vapply(tags_pages$results, `[[`, character(1), "name")
}

#' Get docker image description
#'
#' @param `character(1)` image name with organization name
.docker_image_description <-
    function(image=character(1))
{
    .docker_get(image)$description
}

#' List available images with tags for Bioconductor
#'
#' @importFrom tibble tibble
#' 
#' @export
available <-
    function(pattern = "")
{
    ## Pattern validity check
    stopifnot(
        is.character(pattern),
        length(pattern) == 1L,
        !is.na(pattern)
    )

    ## Get images
    bioc_images <- .docker_image_list("bioconductor")
    ## Get descriptions
    bioc_image_descriptions <- vapply(bioc_images, .docker_image_description, character(1))
    ## get tags
    bioc_image_tags <- sapply(bioc_images, .docker_image_tags_list)
    ## TODO: Fix how to display tags
    tbl <- tibble::tibble(Image = bioc_images,
                             Description = bioc_image_descriptions,
                             Tags = bioc_image_tags)
    ## Only show images which are not in "pattern"
    result <- tbl[grep(pattern, tbl$Image, value = FALSE, ignore.case = TRUE),]
    result
}
