## Depend on bioconductor devel image
FROM bioconductor/bioconductor_docker:devel

## Add Description information
LABEL name="{{{name}}}}" \
      version="0.0.1" \
      url="https://github.com/username/repository" \ 
      vendor="Firstname Lastname" \
      maintainer="name@email.com" \
      description="Docker image description" \
      license="Artistic-2.0"

## Install listed packages
RUN R -e "BiocManager::install(c(package, package))"
