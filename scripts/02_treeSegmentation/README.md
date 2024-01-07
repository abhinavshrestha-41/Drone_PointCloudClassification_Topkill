# Code repository (sub-directory) for tree segmentation of point cloud

Things to note:
1. The `treeSeg_li2012_PCbased.R` and `treeSeg_silva2016_CHMbased.R` scripts in this sub-directory are for segmentation of the point cloud into trees. 
2. The script with `li2012` in its name performs the point cloud-based tree segmentation algorithm by Li et al., 2012. The script with `silva2016` in its name performs the raster-based tree segmentation algorithm by Silva et al., 2016.
3. The `INPUT` for this script is the ground/non-ground classified point cloud with height normaliztion operation  (in the `DATA` folder) and the `OUTPUT` is a tree segmented point cloud that identifies individual trees with a `treeID` attribute. 
4. Before running script, make sure to change variables in the `CHANGE VARIABLES HERE` section and any section that is highlighted with `~~~~~~~~~~` or `===========` or `--------------` 