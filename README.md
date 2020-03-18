# BiocDockerManager - An R Package to manage Bioconductor Docker Images

The **BiocDockerManager** package was designed to work analogous to
*BiocManager* but for docker images. Use the **BiocDockerManager** package
manage docker images provided by the Bioconductor project. The package
provides convenient ways to install images, update images, confirm
validity and find which Bioconductor based docker images are
available.

## Install

First install the package `BiocManager`, then install
`BiocDockerManager` in your R session

```
if (!require("BiocManager"))
    install.packages("BiocManager")
BiocManager::install("BiocDockerManager")
```


## Example Workflow

We hope to provide functionality which is useful to R and Bioconductor
Docker users in the form of an R package.

The typical workflow would look like the following:

First, you check the `available()` images. Then you `install()` a
required image say **bioconductor/bioconductor_docker:devel**.

```
## 1. Check available images
BiocDockerManager::available()

## 2. Install a new image
BiocDockerManager::install(
	repository = "bioconductor/bioconductor_docker",
	tag = "devel"
)
```

Once some time has passed and if you are not sure if you image is up
to date, you have to check if the image is `valid()`.

Then, `install()` an update if the validity check returns that it is
out of date. Check the `version()` of the latest image to make sure
you understand the consequences of updating.

```
## 3. Check if image is valid
BiocDockerManager::valid(
	"bioconductor/bioconductor_docker",
	tag = "devel"
)

## 4. Download update to image
BiocDockerManager::install(
	"bioconductor/bioconductor_docker",
	tag = "devel"
)

## 5. Check version
BiocDockerManager::version(
	"bioconductor/bioconductor_docker",
	tag = "devel"
)
```

## Help

Please read the package vignette for more help.
