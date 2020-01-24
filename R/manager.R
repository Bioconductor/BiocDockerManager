#' List available images with tags for Bioconductor
#'
#' @details 
#'
#' @param pattern
#' @param organization
#' @param deprecated
#' @return  
#'
#' @examples
#'
#' @importFrom tibble tibble
#'
#' @export
available <-
    function(pattern = "",
             organization = "bioconductor", 
             deprecated = FALSE)
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

## FIXME: test
#' Use dockerfile template
#'
#' @examples 
#' 
#' \dontrun{
#'     
#'     use_dockerfile("custom_image", tempdir())
#' 
#' }
#' @importFrom whisker whisker.render
#' @export
use_dockerfile <-
    function(name, path)
{
    ## Create folder with Dockerfile
    f <- file.path(path, name)
    if (!dir.exists(f)) {
        dir.create(f)
    }
    from = system.file("extdata", "Dockerfile", 
                        package = "BiocDockerManager")
    to = file.path(f, "Dockerfile")
    file.copy(from, to, overwrite = FALSE)
    
    ## render with name
    writeLines(whisker::whisker.render(to, list(name)), to)
    file.create(file.path(f, "README.md"))
}


