## 1. Please register maintainer email on
## https://stat.ethz.ch/mailman/listinfo/bioc-devel
##
## 2. Check your Dockerfile on https://www.fromlatest.io/#/

## Depend on bioconductor devel image
FROM bioconductor/bioconductor_docker:devel

## Add Description information
LABEL name = "{{name}}" \
      version = "0.99.0" \
      url = "https://github.com/username/{{name}}" \
      vendor = "FirstName LastName" \
      maintainer = "username@email.com" \
      description = "Docker image description" \
      license = "Artistic-2.0"

## Install listed packages
RUN R -e "BiocManager::install(c(package, package))"
