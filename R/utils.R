.is_character <-
    function(x, na.ok = FALSE, zchar = FALSE)
{
    is.character(x) &&
        (na.ok || all(!is.na(x))) &&
        (zchar || all(nzchar(x)))
}

.is_scalar_character <- function(x, na.ok = FALSE, zchar = FALSE)
    length(x) == 1L && .is_character(x, na.ok, zchar)

.is_character_0_or_1 <-
    function(x, na.ok = FALSE, zchar = FALSE)
{
    (length(x) == 0L || length(x) == 1L) &&
        .is_character(x, na.ok, zchar)
}

.is_scalar_logical <- function(x, na.ok = FALSE)
    is.logical(x) && length(x) == 1L && (na.ok || !is.na(x))
