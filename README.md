## Summarizing bird migration and stopovers within urbanized landscapes using Motus data

__*Pilot Phase*__

Principal Investigators: **[Barbara Frei](https://www.thebirdsthetrees.com/), [Elizabeth Gow](https://www.elizabethgow.com/) and Krista De Groot**  
Workflow design: [Steffi LaZerte](https://steffilazerte.ca)


## Getting Started

This repository contains the workflow for obtaining and filtering Motus track data
for the analysis of Urban/Rural stopovers during avian migration.

The [reports](https://steffilazerte.ca/urban_motus) contains the full annotated code and outputs run as of the dates
on each page.

There are two ways to use this workflow

1. **Walk through this website**, familiarize yourself with the code and outputs [Most users]

2. **Modify this workflow** by working directly in the qmd scripts used to create
this workflow [Some users]
    - To do this, you can either create a [PR](https://happygitwithr.com/big-picture.html?q=pull%20request#special-features-of-github)
    to the workflow or [clone](https://happygitwithr.com/push-pull-github.html?q=clone#git-clone-command-line)
    it and work on your own copy.
    - Each qmd file is setup to be run interactively (i.e. in an R session) as well
    as to be compiled non-interactively to create this script
    - Be warned that compiling the whole site can take a long time, especially if
  new data is being downloaded!
    - Use `renv::restore()` to ensure you have the same packages and versions
    installed in this project to reproduce this workflow.

> [More details...](https://steffilazerte.ca/urban_motus)