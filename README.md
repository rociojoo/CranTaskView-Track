# Cran Task View project for tracking data

This repository acts as the main project page for the [Tracking CranTaskView](https://cran.r-project.org/web/views/Tracking.html), as well as main communication point for the project. If you have any questions feel free to open up an issue.

To be more inclusive we allow submissions from any kind of R package not just Cran specific packages. There are two major rules for submissions:  

1. Package must exist somewhere publically accessible online or through the r console.  
2. Package must pass all cran checks ex. `R CMD CHECK`. Details on how we run our checks are in the `/checks folder.`

### Adding new packages to the list
If you are a developer or simply have a tracking package you'd like to be considered for the TaskView go to the `issues` tab on github and hit `Add Package` which will open a special issue form.

We update the CranTaskView **twice** as year, roughly January and July. At that moment we discuss if a package fits the definition of a tracking package, run checks on all packages, and update the TaskView.

Our current suite of potential packages can be found in `checks/Tracking_table.csv` and the outputs from cran checks can be found at `checks/Checked_packages.csv`.
 
