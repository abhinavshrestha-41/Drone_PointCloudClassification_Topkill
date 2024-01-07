def recodeTreeSegAA(RefID, TreeSegID):
    
    if (RefID >= 0) & (TreeSegID >= 0):

        # Area of manual digitaization and tree segmentation agree
        recodeValue = "A"
    elif (RefID < 0) & (TreeSegID >= 0):

        # Area outlined by tree segmentation but outside manual digitaization
        recodeValue = "B"
        
    elif (RefID >= 0) & (TreeSegID < 0):

        # Area missed by tree segmentation but within manual digitaization
        
        recodeValue = "C"
    else:
        recodeValue = "NA"

    return(recodeValue)
