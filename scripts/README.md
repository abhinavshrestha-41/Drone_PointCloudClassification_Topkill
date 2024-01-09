# Scripts

1. The numbered directories (e.g. `01_...`, `02_...`) follow the methodology outlined in the MS thesis/manuscript for this project. Each folder is a major step in the methodology.  

2. Each numbered directory contains:
* `README` document with content title (top of document) set as the name of the step in the project methodology  
* `DATA` sub-directory that contains the `INPUT`data
* `EXPORTS` sub-directory that contains the `OUTPUT` data
* The remaining files in the sub-directories are scripts run for the methodology stage of the directory.

## Project methodology workflow:

![Methodology workflow](../docs/ProjectMethodolgy.png)  

*Workflow diagram outlining the methodology developed for this project. The major steps in the methodology for which the code is available are outlined in dashed lines and named "Folder [&lt;folder number&gt;]" referring to the respective numbered directories in this repository.*

<hr>

## References

* Creating a CHM from point cloud (TUTORIAL): [https://r-lidar.github.io/lidRbook/chm.html](https://r-lidar.github.io/lidRbook/chm.html)
* Publication on tutorial for individual tree detection using point cloud data:
  + Main paper: [https://www.degruyter.com/document/doi/10.1515/geo-2020-0290/html?lang=en](https://www.degruyter.com/document/doi/10.1515/geo-2020-0290/html?lang=en)
  + Tutorial in supplementary material: [https://www.degruyter.com/document/doi/10.1515/geo-2020-0290/downloadAsset/suppl/geo-2020-0290_sm.pdf](https://www.degruyter.com/document/doi/10.1515/geo-2020-0290/downloadAsset/suppl/geo-2020-0290_sm.pdf)

Roussel, J.R., Auty, D., Coops, N. C., Tompalski, P., Goodbody, T. R. H., Sánchez Meador, A., Bourdon, J.F., De Boissieu, F., Achim, A. (2021). lidR : An R package for analysis of Airborne Laser Scanning (ALS) data. Remote Sensing of Environment, 251 (August), 112061. [doi:10.1016/j.rse.2020.112061](https://doi.org/10.1016/j.rse.2020.112061).

Jean-Romain Roussel and David Auty (2023). Airborne LiDAR Data Manipulation and Visualization for Forestry Applications. R package version 3.1.0. https://cran.r-project.org/package=lidR 

