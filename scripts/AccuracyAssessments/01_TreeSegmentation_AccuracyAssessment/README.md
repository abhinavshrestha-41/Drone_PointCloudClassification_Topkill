# Code repository (sub-directory) for accuracy assessment of tree segmentation

Things to note:
1. The `treeSegAA_SørensenCoefficient_withManualDigitizedPolygons.R` script computes the Sørensen Coefficient that compares manually delineated tree crowns and crowns from the tree segmentation algorithm.
2. The `INPUT` for this script is the `ManualDigitization_V_treeSeg_100.csv` file in the `DATA` folder. This file was exported from ArcGIS (output of the `Union` tool)
3. Before running script, make sure to change variables in the `CHANGE VARIABLES HERE` section and any section that is highlighted with `~~~~~~~~~~` or `===========` or `--------------` 
4. The `PythonScripts_forArcGIS` sub-directory contains Python code used in ArcGIS for the accuracy assessments.