context("use dockerfile")

test_that(".find_maintainer works as expected", {
    persons <- c(
        person(
            given="Main", family="Person",
            email = "main.person@email.com",
            role = c("aut", "cre")
        ),
        person(
            "Second", "Author",
            email = "second.author@email.com",
            role = c("aut")
        )
    )
    expect_equal(
        .find_maintainer(persons)$email,
        "main.person@email.com"
    )
})

