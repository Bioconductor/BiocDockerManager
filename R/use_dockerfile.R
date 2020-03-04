#' Use dockerfile template
#'
#' @param name `character(1)`, name of the image.
#' @param path `character(1)`, parent path where folder repository
#'     should be created.
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
    function(name, person, description, license = "Artistic-2.0",
             path = tempdir())
{
    stopifnot(
        !is.na(name), !is.na(path),
        is(person, "person")
    )

    ## Create folder with Dockerfile
    f <- file.path(path, name)
    if (!dir.exists(f)) {
        dir.create(f, recursive = TRUE)
    }

    dockerfile = readLines(
        system.file("extdata", "Dockerfile",
                    package = "BiocDockerManager")
    )

    ## render with name
    writeLines(
        whisker.render(dockerfile,
                       data = list(name = name)),
        file.path(f, "Dockerfile")
    )
    file.create(file.path(f, "README.md"))

    f
}
