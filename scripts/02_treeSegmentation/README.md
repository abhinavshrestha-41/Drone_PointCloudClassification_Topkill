# Code repository (sub-directory) for tree segmentation of point cloud

Things to note:
1. The `treeSeg_li2012_PCbased.R` and `treeSeg_silva2016_CHMbased.R` scripts in this sub-directory are for segmentation of the point cloud into trees. 
> **The methodology and code for this script were prepared using the [lidRbook](https://r-lidar.github.io/lidRbook/chm.html) and the manuscript by [Mohan et al. (2021)](https://www.degruyter.com/document/doi/10.1515/geo-2020-0290/html?lang=en) -- especially the tutorial included in their [supplementary materials (Mohan et al., 2021)](https://www.degruyter.com/document/doi/10.1515/geo-2020-0290/downloadAsset/suppl/geo-2020-0290_sm.pdf)**
2. The script with `li2012` in its name performs the point cloud-based tree segmentation algorithm by [Li et al., 2012](https://www.ingentaconnect.com/content/asprs/pers/2012/00000078/00000001/art00006;jsessionid=1w3c8p4z49t5g.x-ic-live-02). The script with `silva2016` in its name performs the raster-based tree segmentation algorithm by [Silva et al., 2016](https://www.tandfonline.com/doi/full/10.1080/07038992.2016.1196582).
3. The `INPUT` for this script is the ground/non-ground classified point cloud with height normaliztion operation  (in the `DATA` folder) and the `OUTPUT` is a tree segmented point cloud that identifies individual trees with a `treeID` attribute. 
4. Before running script, make sure to change variables in the `CHANGE VARIABLES HERE` section and any section that is highlighted with `~~~~~~~~~~` or `===========` or `--------------` 

## Tree segmentation algorithm used for project

<img align="right" src="../../docs/treeSeg.gif">

* The tree segmentation results from the `li2012()` and `silva2016()` algorithms were tested on a subset data.  
* Visual assessment indicated a better performance in tree crown delineation by the point cloud-based `li2012()` algorithm.  
* Visual presented (*on the right*) is the segmentation result from the `li2012()` algorithm. Individual trees are displayed as different colors.
