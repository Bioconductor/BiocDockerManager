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
    function(path, api="https://hub.docker.com/v2")
{
    query <- sprintf("%s%s", api, path)
    .docker_get_query(query)
}

#' 
docker_get_pull_count <-
    function(image = character(1))
{
    .docker_get(sprintf("/repositories/%s", image))$pull_count
}


docker_list_bioconductor_images <-
    function()
{
    query <- .docker_get("/repositories/bioconductor/?page_size=100")
    paste0("bioconductor/", sapply(query$results, `[[`, "name"))
}


docker_get_description <-
    function(image=character(1))
{
    .docker_get(sprintf("/repositories/%s", image))$description
}
