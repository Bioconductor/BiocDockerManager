#' Use dockerfile template
#'
#' @details The 'use_dockerfile' function is used to create new
#'     dockerfiles based on the bioconductor/bioconductor_docker set
#'     of images. The function provides a template for creating your
#'     image.
#'
#' @param name `character(1)`, name of the image.
#'
#' @param person `person(1)`, class person which holds
#'     information. Only the maintainer is listed on the LABEL
#'     information of the Docker image.
#'
#' @param description `character(1)`, description of image.
#'
#' @param license `character(1)`, license of image.
#'
#' @param path `character(1)`, parent path where folder repository
#'     should be created.
#'
#' @examples
#'
#' ## detailed example
#'
#' use_dockerfile(
#'     name = "custom_image",
#'     person = person("Foo", "Bar",
#'         email="foo@bar.com", role = c("aut", "cre")
#'     ),
#'     description = "My image",
#'     license = "GPL",
#'     path = tempdir()
#' )
#'
#' ## minimal exmaple
#' use_dockerfile("custom_image",
#'     person(
#'         "abc", "def",
#'         email = "abc@def.com",
#'         role = c("cre", "aut")
#'     )
#' )
#'
#' @importFrom whisker whisker.render
#' @importFrom methods is
#'
#' @return invisible
#'
#' @export
use_dockerfile <-
    function(
        name, person, description,
        license = "Artistic-2.0",
        path = tempdir()
    )
{
    ## Validity checks
    stopifnot(
        !is.na(name),
        !is.na(path),
        is(person, "person")
    )
    ## Description
    if (missing(description))
        description <- "Docker image description."
    ## Create folder with Dockerfile
    f <- file.path(path, name)
    if (!dir.exists(f)) dir.create(f, recursive = TRUE)
    ## Read dockerfile
    dockerfile <- readLines(
        system.file(
            "extdata", "Dockerfile",
            package = "BiocDockerManager"
        ))
    ## Find maintainer
    maintainer <- .find_maintainer(person)
    ## fill data for whisker
    data <- list(
        name = name,
        given_name = maintainer$given,
        family_name = maintainer$family,
        maintainer_email = maintainer$email,
        description = description,
        license = license
    )
    ## render with repository
    writeLines(
        whisker.render(
            dockerfile,
            data = data
        ),
        file.path(f, "Dockerfile")
    )
    file.create(file.path(f, "README.md"))
    f
}


## Find maintainer in a persons vector
.find_maintainer <-
    function(persons)
{
    for (p in persons) {
        if ("cre" %in% p$role) {
            return(p)
        }
    }
    stop("no maintainer with the role 'cre' in persons list.")
}
