def damageCodeConv(damageType):
    
    damageCodeLookup_dict = {
        "Healthy"           : 1, 
        "Non-top-kill"      : 2, 
        "Top-kill"          : 3
    }
    
    if damageType in damageCodeLookup_dict:
        damageCode = damageCodeLookup_dict[damageType]
    else:
        damageCode = 0
    
    return(damageCode)