#' Dockerhub API get query
#'
#' @keywords internal
#'
#' @param `character(1)`, query for the api.
#' @return reponse of the query to the dockerhub API.
#'
#' @importFrom httr GET stop_for_status content
.docker_get_query <-
    function(query)
{
    stopifnot(.is_scalar_character(query))

    response <- GET(query)
    tryCatch({
        stop_for_status(response)
    }, error = function(e) {
        stop("That URL doesn't exist")
    })
    content(response)
}


#' Build query for Dockerhub API
#'
#' @keywords internal
#'
#' @param path `character(1)` query substring
#' @param api `character(1)`, dockerhub api url
#' @param path_root `character(1)` query substring
#'
.docker_get <-
    function(path,
             api = "https://hub.docker.com/v2",
             path_root = "/repositories/")
{
    stopifnot(
        .is_scalar_character(path),
        .is_scalar_character(api),
        .is_scalar_character(path_root)
    )

    query <- sprintf("%s%s%s", api, path_root, path)
    .docker_get_query(query)
}


#' Get Docker image pull count (number of times downloaded)
#'
#' @keywords internal
#'
#' @param `character(1)` docker image name with organization
#' @return `numeric(1)` number showing how many times the image has
#'     been downloaded
#'
#' @examples
#'
#' \dontrun{
#' .docker_image_pull_count("bioconductor/bioconductor_docker")
#' }
#'
.docker_image_pull_count <-
    function(image)
{
    stopifnot(.is_scalar_character(image))

    .docker_get(image)$pull_count
}


#' Get the list of images under an organization account.
#'
#' @details Get the list of images under and organization or
#'     username. Important to note that organization/username is
#'     different from the image name on dockerhub.
#'
#' @keywords internal
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
    function(organization)
{
    stopifnot(.is_scalar_character(organization))

    org_pages <- .docker_get(sprintf("%s/?page_size=100",organization))
    vapply(org_pages$results, `[[`, character(1), "name")
}


#' Get list of repositories, i.e, organization/image_name
#'
#' @keywords internal
#'
.docker_repository_list <-
    function(organization, images=NULL)
{
    stopifnot(.is_scalar_character(organization))

    if (is.null(images)) {
        images <- .docker_image_list(organization)
    }
    paste(organization, images, sep="/")
}


#' List the tags of an Image
#'
#' @keywords internal
.docker_image_tags_list <-
    function(image)
{
    stopifnot(.is_scalar_character(image))

    tags_pages <- .docker_get(paste(image, "tags", sep="/"))
    vapply(tags_pages$results, `[[`, character(1), "name")
}


#' Get docker image description
#'
#' @param `character(1)` image name with organization name
#'
#' @keywords internal
#'
.docker_image_description <-
    function(image)
{
    stopifnot(.is_scalar_character(image))

    trimws(.docker_get(image)$description)
}


#' Get all the docker image digest values for the image name provided
#'
#' @param `character(1)` image name with organization name
#'
#' @keywords internal
#'
.docker_image_digests <-
    function(image = "bioconductor/bioconductor_docker")
{
    ## query
    tags <- .docker_get(paste(image, "tags", sep="/"))

    ## tag names
    tag <- vapply(tags$results, `[[`, character(1), "name")

    ## get digest SHA
    images <- vapply(tags$results, `[[`, list(1), "images")
    digest <- vapply(images, `[[`,character(1), "digest")

    ## return tibble
    tibble(image = image,
           tag = tag,
           ## last_updated = last_updated,
           repo_digest = digest)
}
