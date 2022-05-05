# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# Aviation_Utils
#
# Author: lefebvre
# ----------------------------------------------------------------------------

import SmartScript

class Aviation_Utils(SmartScript.SmartScript):
    
    def __init__(self, dbss, eaMgr=None):
        SmartScript.SmartScript.__init__(self, dbss)
        

    def getModelWeights(self, dialogNames, varDict):
        
        models = []
        modelWeights = {}
        sumWeights = 0.0
        #  Get name of chosen model(s) - and fix it up so we can use it later on.
        for model in dialogNames:

            #  Get the weight associated with this model
            weight = int(varDict[model])

            #  If the weight is zero
            if weight == 0:
                continue                    #  move on to next model
            #  Assume this is the latest run of this model
            run = 0
            #  If this is the "previous" version of this model
            if model.startswith("Previous"):
                #  Clean up model name and adjust the run we are looking for
                model = model.split()[1].strip()
                run = -1
            #  Get the D2D database ID of this model
            db=self.findDatabase("D2D_%s" % (model), run)
            id=db.modelIdentifier()

            #  If the model identifier is not missing
            if db.isValid():
                #  Store the weight for this model
                modelWeights[id] = weight
                #  Add this weight to the total weights
                sumWeights += weight
                #  Add this database to the list of models we are using
                if id not in models:
                    models.append(id)
                    
            return models, modelWeights


