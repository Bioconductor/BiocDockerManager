## Depend on bioconductor devel image
FROM bioconductor/bioconductor_docker:devel

## Add Description information
LABEL name="{{{name}}}}" \
      version="{{{version}}}" \
      url="{{{url}}}" \ 
      vendor="{{{owner}}}" \
      maintainer="{{{maintainer}}}" \
      description="{{{description}}}" \
      license="{{{license}}}"

## Install listed packages
RUN R -e "BiocManager::install(c({{{packages}}}))"
