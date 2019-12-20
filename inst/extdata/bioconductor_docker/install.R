install.packages("BiocManager", repos="https://cran.rstudio.com")

library(BiocManager)

if(! BiocManager:::isDevel()){
    BiocManager::install(version="3.11",
                         update=TRUE, ask=FALSE)
}

builtins <- c("Matrix", "KernSmooth", "mgcv", "devtools")

for (builtin in builtins)
    if (!suppressWarnings(require(builtin, character.only=TRUE)))
        suppressWarnings(BiocManager::install(builtin,
                                              version="3.11",
                                              update=TRUE, ask=FALSE))
