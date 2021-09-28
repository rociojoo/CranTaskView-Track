# CRAN Task View for tracking packages

This repository acts as the main project page for the [Tracking CRAN Task View
(CTV)](https://cran.r-project.org/web/views/Tracking.html), as well as the main
communication point for the project. If you have any questions feel free to open
up an issue.

To be more inclusive we allow submissions from any kind of R package not just
CRAN specific packages. There are two major rules for submissions:

1. Package must exist somewhere publically accessible online or through the R
   console.
2. Package must pass all CRAN checks (i.e. `R CMD CHECK`). Details on how we run
   our checks can be found [here](checks/).


## Adding new packages to the list

If you are a developer or simply have a tracking package you'd like to be
considered for the Tracking CTV, go to the issues tab on GitHub, start a New
issue and hit "Add Package" which will open a [special issue
form](https://github.com/rociojoo/CranTaskView-Track/issues/new?assignees=&labels=use-case&template=add-package.md&title=%5BAdd+pkg%5D+Name+of+the+package+%28change+this+title%29).

We normally update the Tracking CTV **twice** a year, roughly January and
July. At that moment we discuss if a package fits the definition of a tracking
package, run checks on all packages, and update the Tracking CTV.

Our current suite of potential packages can be found in
[checks/Tracking_tbl.csv](checks/Tracking_tbl.csv) and the outputs from CRAN
checks can be found at
[checks/Checked_packages.csv](checks/Checked_packages.csv).
