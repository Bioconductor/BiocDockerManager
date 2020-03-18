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
#'
#' res <- available()
#'
#' res <- available("bioconductor_docker")
#'
#' res <- available(pattern = "rstudio", organization = "rocker")
#'
#' res <- available(deprecated = TRUE)
#' 
#'
#' @importFrom dplyr tibble
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
    pull_count <- vapply(repositories, .docker_image_pull_count,
                         numeric(1))
    ## result
    tibble(IMAGE = images,
           DESCRIPTION = image_descriptions,
           TAGS = trimws(image_tags),
           REPOSITORY = repositories,
           DOWNLOADS = pull_count)
}


#' Get version of bioconductor docker image
#'
#' @details The version of the images provided by Bioconductor are
#'     unique. They are represented by 'x.y.z' where, the 'x.y'
#'     represent the version of Bioconductor and the '.z' represents
#'     the version of the Dockerfile used to build the Docker
#'     image. This is especially useful in terms of reproducibility
#'     and tracking changes when using the Docker images provided by
#'     Bioconductor.
#'
#' @param repository `character(1)`, repository of the docker image.
#'
#' @param tag `character(1)`, tag of the docker image.
#'
#' @return `character` vector representing the version number.
#'
#' @examples
#' \dontrun{
#' BiocDockerManager::version("bioconductor/bioconductor_docker",
#'                                tag = "latest")
#'
#' BiocDockerManager::version("bioconductor/bioconductor_docker",
#'                                tag = "devel")
#' }
#'
#' @export
version <-
    function(repository = "bioconductor/bioconductor_docker", tag)
{
    .docker_inspect_label(repository, tag, label = "version")
}


#' Get maintainer of bioconductor docker image
#'
#' @details The maintainer name and email provides information for who
#'     you can contact in case the image isn't working as expected.
#'
#' @param repository `character(1)`, repository of the docker image.
#'
#' @param tag `character(1)`, tag of the docker image.
#'
#' @return `character` vector representing the maintainer.
#'
#' @examples
#' \dontrun{
#' BiocDockerManager::maintainer("bioconductor/bioconductor_docker",
#'                                tag = "latest")
#' }
#' @export
maintainer <-
    function(repository = "bioconductor/bioconductor_docker", tag)
{
    .docker_inspect_label(repository, tag, label = "maintainer")
}


#' Install a docker image on your local machine
#'
#' @details The function works similar to the 'docker pull'
#'     command. It downloads a docker image from Dockerhub on to the
#'     local machine, in a place which the docker engine knows about,
#'     building your local registry of docker images.
#'
#' @param repository `character(1)`, repository name of the docker image.
#'
#' @param tag `character(1)`, tag of the docker image.
#'
#' @param quiet `logical(1)`, if TRUE suppress verbose output
#'     generated from the download.
#'
#' @param all_tags `logical(1)`, pull all the tags of the image
#'
#' @examples
#' \dontrun{
#' BiocDockerManager::install(repository = "bioconductor/bioconductor_docker",
#'                            tag = "latest")
#'
#' }
#'
#' @return NULL
#'
#' @export
install <-
    function(repository, tag, quiet = FALSE, all_tags = FALSE)
{
    ## TODO:
    ## tag or all_tags = TRUE
    if (missing(tag) && !all_tags) {
        tag = "latest"
    }

    if (all_tags) {
        tag = NA_character_
    }

    ## validity check
    stopifnot(
        .is_scalar_character(repository),
        .is_scalar_logical(quiet),
        .is_scalar_logical(all_tags)
    )

    .docker_pull(repository, tag, quiet, all_tags)
}


#' Get installed docker images
#'
#' @param repository `character(1)`, repository name of image; if not
#'     given all images will be shown.
#'
#' @examples
#' \dontrun{
#'    BiocDockerManager::installed()
#'
#'    BiocDockerManager::installed(
#'        repository = "bioconductor/bioconductor_docker"
#'    )
#' }
#'
#' @return stdout of docker images on your local machine.
#'
#' @export
installed <-
    function(repository)
{
    if (missing(repository))
        repository = character(1)

    .docker_images(repository)
}

#' Check if all images available are valid
#'
#' @details Check if the image is valid, i.e to see if the image is up
#'     to date with the image hosted by bioconductor on the Dockerhub
#'     organization page.
#'
#' @param repository `character(1)`, repository name of the docker image.
#'
#' @param tag `character(1)`, tag of the docker image.
#'
#'
#' @importFrom dplyr anti_join
#' @importFrom dplyr select
#' @importFrom dplyr bind_rows
#'
#' @return tibble with the repository and tag of image which needs to be
#'     updated.
#'
#' @examples
#' \dontrun{
#'
#' BiocDockerManager::valid()
#' 
#' BiocDockerManager::valid("bioconductor/bioconductor_docker",
#'                          tag = "devel")
#' }
#'
#' @export
valid <-
    function(repository = "bioconductor/bioconductor_docker",
             tag)
{
    stopifnot(
        .is_scalar_character(repository),
        missing(tag) || .is_scalar_character(tag)
    )

    ## Image digests from dockerhub
    hub <- .docker_image_digests()

    ## Local image digest
    if (missing(tag)) {
        tags <- .docker_image_tags_list(repository)

        local <- tibble(REPOSITORY = character(0),
                        TAG = character(0),
                        DIGEST = character(0))
        
        for (tag in tags) {
            local <- bind_rows(
                local,
                suppressWarnings(
                    .docker_inspect_digest(repository = repository, tag)
                ))
        }
    } else {
        local <- .docker_inspect_digest(repository, tag)
    }
    
    to_update <- dplyr::anti_join(x = local, y = hub, by = c("DIGEST"))
    
    message("The following bioconductor images need to be updated:")
    select(to_update, REPOSITORY, TAG)
}


#' Help function to direct brower to Bioconductor dockerhub
#'
#' @param repository `character(1)`, repository name of image. Default
#'     image is the main 'bioconductor/bioconductor_docker' image.
#'
#' @importFrom utils browseURL
#'
#' @return Open a browser tab with docker repository
#'
#' @export
help <-
    function(repository = "bioconductor/bioconductor_docker")
{
    url <- paste0("https://hub.docker.com/r/", repository)
    browseURL(url)
}
