#' List available images with tags for Bioconductor
#'
#' @details Return a tibble of all the available docker images in an
#'     organization. The tibble will show the Image name, description,
#'     tags, repository-name, pull-count. By default this function is
#'     designed to show the available images under the Bioconductor
#'     organization in dockerhub.
#'
#' @param pattern `character(1)`, a pattern to filter the names of
#'     images.
#'
#' @param organization `character(1)`, organization whose list of
#'     images hosted on dockerhub will be displayed. Default is
#'     'bioconductor'.
#'
#' @param deprecated `logical(1)`, TRUE will show deprecated images.
#'
#' @return `tibble` of available images
#'
#' @examples
#' \dontrun{
#' res <- available()
#'
#' res <- available("bioconductor_docker")
#'
#' res <- available(pattern = "rstudio", organization = "rocker")
#'
#' res <- available(deprecated = TRUE)
#' }
#'
#' @importFrom tibble tibble
#'
#' @export
available <-
    function(pattern,
             organization = "bioconductor",
             deprecated = FALSE)
{
    ## Pattern validity check
    stopifnot(
        missing(pattern) || .is_scalar_character(pattern),
        .is_scalar_character(organization),
        .is_scalar_logical(deprecated)
    )

    ## Get images
    images <- .docker_image_list(organization)

    if (!missing(pattern)) {
        filter <- grep(pattern, images, value = FALSE, ignore.case = TRUE)
        images <- images[filter]
    }

    repositories <- .docker_repository_list(organization, images)

    ## Get descriptions
    image_descriptions <- vapply(repositories,
                                 .docker_image_description,
                                 character(1))
    ## Filter deprecated images
    if(!deprecated) {
        include <- !grepl("DEPRECATED", image_descriptions)
        image_descriptions <- image_descriptions[include]
        repositories <- repositories[include]
        images <- images[include]
    }

    ## get tags in a list
    tags <- lapply(repositories, .docker_image_tags_list)
    image_tags <- vapply(tags, paste, character(1), collapse=", ")
    image_tags[!nzchar(image_tags)] <- NA_character_
    image_descriptions[!nzchar(image_descriptions)] <- NA_character_

    ## Get pull count
    pull_count <- vapply(
        repositories,
        .docker_image_pull_count,
        numeric(1)
    )

    ## result
    tibble(
        IMAGE = images,
        DESCRIPTION = image_descriptions,
        TAGS = trimws(image_tags),
        REPOSITORY = repositories,
        DOWNLOADS = pull_count
    )
}

#' Get version of bioconductor docker image
#'
#' @export
version <-
    function(name = "bioconductor/bioconductor_docker", tag)
{
    labels <- .docker_inspect(name, tag)
    labels$version
}


#' Install/Pull a docker image
#'
#' @export
install <-
    function(name, tag, quiet = FALSE, all_tags = FALSE)
{
    .docker_pull(name, tag, quiet, all_tags)
}


#' Get installed docker images
#'
#' @param name `character(1)`, name of image; if not given
#'     all images will be shown.
#'
#' @export
installed <-
    function(name = "", ...)
{
    .docker_images(name, ...)
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
