#' Docker command line function to run with arguments
#'
.docker_do <- function(args) {
    err <- system2("docker", args, wait = TRUE)
    if (err)
        stop(
            "\n  ", sprintf("docker %s", paste(args, collapse=" ")),
            "\n  failed: error code ", err
        )
    invisible(err)
}

## Dockerhub API get query
.docker_get_query <-
    function(query)
{
    response <- GET(query)
    stop_for_status(response)
    content(response)
}

.docker_get <-
    function(path, api = "https://hub.docker.com/v2",
             path_root = "/repositories")
{
    query <- sprintf("%s%s%s", api, path, path_root)
    .docker_get_query(query)
}

#' Get Docker image pull count (number of times downloaded)
#' 
#'  @param `character(1)` docker image name with organization 
docker_image_pull_count <-
    function(image = character(1))
{
    .docker_get(image)$pull_count
}

#' 
docker_image_list <-
    function(organization = character(1))
{
    bioc <- .docker_get(path = sprintf("%s/?page_size=100", organization),
                        path_root = "") 
    paste0("bioconductor/", sapply(bioc$results, `[[`, "name"))
}

#' Get docker image description
#' 
#' @param `character(1)` image name with organization name
docker_image_description <-
    function(image=character(1))
{
    .docker_get(image)$description
}

