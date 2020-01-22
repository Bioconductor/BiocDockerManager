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
    vapply(org_pages$results, `[[`, character(1), "name")
}


#' Get list of repositories, i.e, organization/image_name
#'
.docker_repository_list <-
    function(organization = character(1), images=NULL)
{
    if (is.null(images)) {
        images = .docker_image_list(organization)
    }
    paste(organization, images, sep="/")
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
    trimws(.docker_get(image)$description)
}

#' List available images with tags for Bioconductor
#'
#' @importFrom tibble tibble
#'
#' @export
available <-
    function(pattern = "", version = BiocManager::version(),
             organization = "bioconductor", deprecated = FALSE)
{
    ## Pattern validity check
    stopifnot(
        is.character(pattern),
        length(pattern) == 1L,
        !is.na(pattern),
        is.logical(deprecated)
    )

    ## Get images
    images <- .docker_image_list(organization)

    images <- images[grep(pattern,
                          images,
                          value = FALSE, ignore.case = TRUE)]

    repositories <- .docker_repository_list(organization, images)

    ## Get descriptions
    image_descriptions <- vapply(repositories,
                                 .docker_image_description,
                                 character(1))
    ## TODO: DEPRECATE AND FILTER HERE

    ## get tags in a list
    image_tags <- lapply(repositories, .docker_image_tags_list)

    ## TODO: replace 0 length strings with NA (nzchar), make helper
    ## function to make "Tags" clean and Description

    tags_string <- vapply(image_tags, paste, character(1), collapse=", ")

    ## result
    tbl <- tibble::tibble(Image = images,
                   Description = image_descriptions,
                   Tags = tags_string,
                   Repository = repositories,
                   Tags_list = image_tags)
    if (!deprecated) {
        tbl <- tbl[ !grepl("DEPRECATED", tbl$Description),]
    }
    tbl
}


#' Check if all images available are
#'
#'
#' @export
valid <-
    function()
{
    return(NULL)
}

.docker_inspect <-
    function()
{
    return(NULL)
}

version <-
    function(name = "")
{
    ver <- integer(1)

    return(ver)
}

###  python docker
# docker = import("docker")
#client = docker$from_env()
#client$images$list()
#image = client$images$list()[[1]]
#image$labels
#client$images$list(name="bioconductor/bioconductor_docker:devel")[[1]]$labels


## TODO

## valid()
## install()
## version()

## bonus

## run()
## use_dockerfile()
