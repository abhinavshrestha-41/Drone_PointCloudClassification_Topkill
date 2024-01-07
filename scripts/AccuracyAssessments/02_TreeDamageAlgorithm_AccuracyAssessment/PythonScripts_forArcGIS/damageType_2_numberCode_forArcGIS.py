def damageCodeConv(damageType):
    
    damageCodeLookup_dict = {
        "Healthy"           : 1, 
        "Minor damage"      : 2, 
        "Moderate damage"   : 3, 
        "Major damage"      : 4, 
        "Dead (red)"        : 5, 
        "Dead (gray)"       : 6,
        "Dead (mixed)"      : 7
    }
    
    if damageType in damageCodeLookup_dict:
        damageCode = damageCodeLookup_dict[damageType]
    else:
        damageCode = 0
    
    return(damageCode)