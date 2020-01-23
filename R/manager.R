o#' List available images with tags for Bioconductor
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

    ## Get pull count
    pull_count <- vapply(repositories,
                         .docker_image_pull_count,
                         numeric(1))
    
    ## result
    tbl <- tibble::tibble(Image = images,
                          Description = image_descriptions,
                          Tags = trimws(tags_string),
                          Repository = repositories,
                          Downloads = pull_count)
    tbl
}


version <-
    function(name = "")
{
    ver <- integer(1)

    return(ver)
}


#' Install/Pull a docker image
#' 
#' @export
install <-
    function(name, tag, ...)
{
    .docker_pull(name, tag, ...)
}


#' Get installed docker images
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
    
}


#' Use dockerfile template
#'
#' @export
use_dockerfile <-
    function()
{
    return(NULL)
}


