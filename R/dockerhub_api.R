## Dockerhub API get query
##
## @keywords internal
##
## @param `character(1)`, query for the api.
## @return reponse of the query to the dockerhub API.
##
#' @importFrom httr GET stop_for_status content
.docker_get_query <-
    function(query)
{
    stopifnot(.is_scalar_character(query))

    response <- GET(query)
    tryCatch({
        stop_for_status(response)
    }, error = function(e) {
        stop("That URL doesn't exist: \n  ", conditionMessage(e))
    })
    content(response)
}


## Build query for Dockerhub API
##
## @keywords internal
##
## @param path `character(1)` query substring
## @param api `character(1)`, dockerhub api url
## @param path_root `character(1)` query substring
##
.docker_get <-
    function(
        path,
        api = "https://hub.docker.com/v2",
        path_root = "/repositories/"
    )
{
    stopifnot(
        .is_scalar_character(path),
        .is_scalar_character(api),
        .is_scalar_character(path_root)
    )

    query <- sprintf("%s%s%s", api, path_root, path)
    .docker_get_query(query)
}


## Get Docker image pull count (number of times downloaded)
##
## @keywords internal
##
## @param repository `character(1)` docker image name with organization
##
## @return `numeric(1)` number showing how many times the image has
##     been downloaded
##
## @examples
##
## \dontrun{
## .docker_image_pull_count("bioconductor/bioconductor_docker")
## }
##
.docker_image_pull_count <-
    function(repository)
{
    stopifnot(.is_scalar_character(repository))

    split_repo <- unlist(strsplit(repository, "/", fixed = TRUE))

    name <- split_repo[2]

    organization <- split_repo[1]

    full_org <- .memoised_docker_get_organization(organization)

    idx <- which(name == vapply(full_org, `[[`, character(1), "name"))

    full_org[[idx]]$pull_count
}


## Get the list of images under an organization account.
##
## @details Get the list of images under and organization or
##     username. Important to note that organization/username is
##     different from the image name on dockerhub.
##
## @keywords internal
##
## @param organization `character(1)` name of organization or "username".
##
## @examples
##
## \dontrun{
##    .docker_image_list("bioconductor")
## }
##
## @return `character(1)` vector of images hosted by the organization
##     on dockerhub.
##
.docker_image_list <-
    function(organization)
{
    stopifnot(.is_scalar_character(organization))

    full_org <- .memoised_docker_get_organization(organization)
    vapply(full_org, `[[`, character(1), "name")
}


.docker_get_organization <-
    function(organization)
{
    .docker_get(sprintf("%s/?page_size=150",organization))$results
}


#' @importFrom memoise memoise
.memoised_docker_get_organization <-
    memoise::memoise(.docker_get_organization)


## Get list of repositories, i.e, organization/image_name
##
## @keywords internal
##
## @param organization `character(1)` name of organization or "username".
##
## @param images `character(1)` name of image, leave as default NULL
##
.docker_repository_list <-
    function(organization, images=NULL)
{
    stopifnot(.is_scalar_character(organization))

    if (is.null(images)) {
        images <- .docker_image_list(organization)
    }
    paste(organization, images, sep="/")
}


.docker_image_tags <-
    function(repository)
{
    .docker_get(paste(repository, "tags", sep = "/"))$results
}

#' @importFrom memoise memoise
.memoised_docker_image_tags <-
    memoise::memoise(.docker_image_tags)

## List the tags of an Image
##
## @keywords internal
##
## @param repository `character(1)` name of the repository
##
## @return list of tags for repository
##
.docker_image_tags_list <-
    function(repository)
{
    stopifnot(.is_scalar_character(repository))

    tags_pages <- .memoised_docker_image_tags(repository)
    ## tags_pages <- .docker_get(paste(repository, "tags", sep="/"))
    vapply(tags_pages, `[[`, character(1), "name")
}


## Get docker image description
##
## @param repository `character(1)` repository name
##
## @keywords internal
##
.docker_image_description <-
    function(repository)
{
    stopifnot(.is_scalar_character(repository))

    split_repo <- unlist(strsplit(repository, "/", fixed = TRUE))

    name <- split_repo[2]

    organization <- split_repo[1]

    full_org <- .memoised_docker_get_organization(organization)

    idx <- which(name == vapply(full_org, `[[`, character(1), "name"))

    trimws(full_org[[idx]]$description)
}


## Get all the docker image digest values for the image name provided
##
## @param repository `character(1)` image name with organization name
##
## @keywords internal
##
.docker_image_digests <-
    function(repository = "bioconductor/bioconductor_docker")
{
    ## query
    tags <- .memoised_docker_image_tags(repository)

    ## tag names
    tag <- vapply(tags, `[[`, character(1), "name")

    ## get digest SHA
    images <- vapply(tags, `[[`, list(1), "images")
    digest <- vapply(images, `[[`,character(1), "digest")

    ## return tibble
    tibble(
        REPOSITORY = repository,
        TAG = tag,
        DIGEST = digest
    )
}
