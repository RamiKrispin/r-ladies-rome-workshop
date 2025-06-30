# A VScode Template for Dockerized R Environment

This Github repository provides a template for a dockerized R development environment with VScode and the Dev Containers extension. It contains the following folders and files:



```shell
.
├── .devcontainer
│   └── devcontainer.json
├── .vscode
│   ├── extensions.json
│   └── settings.json
├── docker
│   ├── build_base-r.sh
│   ├── build_r-dev.sh
│   ├── Dockerfile.base-r
│   ├── Dockerfile.r-dev
│   └── setting_files
│       ├── install_cli_tools.sh
│       ├── install_debian.sh
│       ├── install_packages.R
│       ├── install_python.sh
│       ├── install_quarto.sh
│       ├── packages_vscode.json
│       ├── packages.json
│       └── requirements.txt
├── README.md
├── .Rprofile
└── tests
    ├── app.R
    ├── htmlwidgets.R
    ├── plot.R
    └── shiny_run.R

```

It includes the following folders and files:
- `.devcontainer` - defines the dockerized environment settings with the `devcontainer.json` file
- `.vscode` - enables the modification of the VScode general settings for the dockerized environment with the `settings.json` file
- `docker` - contains the template image settings
- `tests` - R scripts for testing the environment functionality (e.g., Shiny app, static and interactive plots, etc.)

The template default image in the template is `rkrispin/vscode_r_dev:0.1.0`, which comes with R version `4.4.0` and core packages (e.g., `dplyr`, `shiny`, `ggplot2`, `plotly`, etc.). 


## Usage

To use the template, follow these steps:
- Fork this repository and clone it to your local machine.
- Updated the `packages.json` file, under the `docker/setting_files` folder, with the desired packages and versions.
- Update the build settings using the `build_r-dev.sh` script and build the image
- Rename the image name on the `.devcontainer.json` file with the new image name (e.g., `rkrispin/vscode_r_dev:0.1.0`)
