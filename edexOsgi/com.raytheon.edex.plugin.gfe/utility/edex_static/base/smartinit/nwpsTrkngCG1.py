from Init import *
import os

##--------------------------------------------------------------------------
## Module that calculates surface elements from ENP model
## output.
##
##--------------------------------------------------------------------------

class nwpsTrkngCG1Forecaster8(Forecaster):
    def __init__(self):
        Forecaster.__init__(self, "nwpsTrkngCG1", "nwpsTrkngCG1")

##--------------------------------------------------------------------------
##  Calculates Periods from nwpsTrkngCG1 D2D model.
##--------------------------------------------------------------------------
    def calcPeriod1(self, SWPER_OSEQD1):
        # Clip off anything over land
        period = clip(SWPER_OSEQD1, 0, 25) 
        return period 

    def calcPeriod2(self, SWPER_OSEQD2):
        # Clip off anything over land
        period = clip(SWPER_OSEQD2, 0, 25)
        return period

    def calcPeriod3(self, SWPER_OSEQD3):
        # Clip off anything over land
        period = clip(SWPER_OSEQD3, 0, 25)
        return period

    def calcPeriod4(self, SWPER_OSEQD4):
        # Clip off anything over land
        period = clip(SWPER_OSEQD4, 0, 25)
        return period

    def calcPeriod5(self, SWPER_OSEQD5):
        # Clip off anything over land
        period = clip(SWPER_OSEQD5, 0, 25)
        return period

    def calcPeriod6(self, SWPER_OSEQD6):
        # Clip off anything over land
        period = clip(SWPER_OSEQD6, 0, 25)
        return period

    def calcPeriod7(self, SWPER_OSEQD7):
        # Clip off anything over land
        period = clip(SWPER_OSEQD7, 0, 25)
        return period

    def calcPeriod8(self, SWPER_OSEQD8):
        # Clip off anything over land
        period = clip(SWPER_OSEQD8, 0, 25)
        return period

    def calcWave1(self, SWELL_OSEQD1, SWDIR_OSEQD1):
        # extract the wind speed and direction
        mag = where(greater(SWELL_OSEQD1, 50), 0.0, SWELL_OSEQD1 * 3.28)
        mag = clip(mag, 0, 50)
        dir = clip(SWDIR_OSEQD1, 0, 359.5)
        # Clip off anything over land
        

        return (mag, dir)

    def calcWave2(self, SWELL_OSEQD2, SWDIR_OSEQD2):
        # extract the wind speed and direction
        mag = where(greater(SWELL_OSEQD2, 50), 0.0, SWELL_OSEQD2 * 3.28)
        mag = clip(mag, 0, 50)
        dir = clip(SWDIR_OSEQD2, 0, 359.5)
        # Clip off anything over land
        

        return (mag, dir)

    def calcWave3(self, SWELL_OSEQD3, SWDIR_OSEQD3):
        # extract the wind speed and direction
        mag = where(greater(SWELL_OSEQD3, 50), 0.0, SWELL_OSEQD3 * 3.28)
        mag = clip(mag, 0, 50)
        dir = clip(SWDIR_OSEQD3, 0, 359.5)
        # Clip off anything over land
        

        return (mag, dir)

    def calcWave4(self, SWELL_OSEQD4, SWDIR_OSEQD4):
        # extract the wind speed and direction
        mag = where(greater(SWELL_OSEQD4, 50), 0.0, SWELL_OSEQD4 * 3.28)
        mag = clip(mag, 0, 50)
        dir = clip(SWDIR_OSEQD4, 0, 359.5)
        # Clip off anything over land
        

        return (mag, dir)


    def calcWave5(self, SWELL_OSEQD5, SWDIR_OSEQD5):
        # extract the wind speed and direction
        mag = where(greater(SWELL_OSEQD5, 50), 0.0, SWELL_OSEQD5 * 3.28)
        mag = clip(mag, 0, 50)
        dir = clip(SWDIR_OSEQD5, 0, 359.5)
        # Clip off anything over land
        

        return (mag, dir)

    def calcWave6(self, SWELL_OSEQD6, SWDIR_OSEQD6):
        # extract the wind speed and direction
        mag = where(greater(SWELL_OSEQD6, 50), 0.0, SWELL_OSEQD6 * 3.28)
        mag = clip(mag, 0, 50)
        dir = clip(SWDIR_OSEQD6, 0, 359.5)
        # Clip off anything over land
        

        return (mag, dir)


    def calcWave7(self, SWELL_OSEQD7, SWDIR_OSEQD7):
        # extract the wind speed and direction
        mag = where(greater(SWELL_OSEQD7, 50), 0.0, SWELL_OSEQD7 * 3.28)
        mag = clip(mag, 0, 50)
        dir = clip(SWDIR_OSEQD7, 0, 359.5)
        # Clip off anything over land
        

        return (mag, dir)

    def calcWave8(self, SWELL_OSEQD8, SWDIR_OSEQD8):
        # extract the wind speed and direction
        mag = where(greater(SWELL_OSEQD8, 50), 0.0, SWELL_OSEQD8 * 3.28)
        mag = clip(mag, 0, 50)
        dir = clip(SWDIR_OSEQD8, 0, 359.5)
        # Clip off anything over land
        

        return (mag, dir)



class nwpsTrkngCG1Forecaster7(Forecaster):
    def __init__(self):
        Forecaster.__init__(self, "nwpsTrkngCG1", "nwpsTrkngCG1")

##--------------------------------------------------------------------------
##  Calculates Periods from nwpsTrkngCG1 D2D model.
##--------------------------------------------------------------------------
    def calcPeriod1(self, SWPER_OSEQD1):
        # Clip off anything over land
        period = clip(SWPER_OSEQD1, 0, 25)
        return period

    def calcPeriod2(self, SWPER_OSEQD2):
        # Clip off anything over land
        period = clip(SWPER_OSEQD2, 0, 25)
        return period

    def calcPeriod3(self, SWPER_OSEQD3):
        # Clip off anything over land
        period = clip(SWPER_OSEQD3, 0, 25)
        return period

    def calcPeriod4(self, SWPER_OSEQD4):
        # Clip off anything over land
        period = clip(SWPER_OSEQD4, 0, 25)
        return period

    def calcPeriod5(self, SWPER_OSEQD5):
        # Clip off anything over land
        period = where( SWPER_OSEQD5, 0)
        return period

    def calcPeriod6(self, SWPER_OSEQD6):
        # Clip off anything over land
        period = where( SWPER_OSEQD6, 0)
        return period

    def calcPeriod7(self, SWPER_OSEQD7):
        # Clip off anything over land
        period = where( SWPER_OSEQD7, 0)
        return clip(period, 0, 70)

##--------------------------------------------------------------------------
##  Calculates Primary and Secondary Wave
##--------------------------------------------------------------------------

    def calcWave1(self, SWELL_OSEQD1, SWDIR_OSEQD1):
        # extract the wind speed and direction
        mag = where(greater(SWELL_OSEQD1, 50), 0.0, SWELL_OSEQD1 * 3.28)
        mag = clip(mag, 0, 50)
        dir = clip(SWDIR_OSEQD1, 0, 359.5)
        # Clip off anything over land
        

        return (mag, dir)

    def calcWave2(self, SWELL_OSEQD2, SWDIR_OSEQD2):
        # extract the wind speed and direction
        mag = where(greater(SWELL_OSEQD2, 50), 0.0, SWELL_OSEQD2 * 3.28)
        mag = clip(mag, 0, 50)
        dir = clip(SWDIR_OSEQD2, 0, 359.5)
        # Clip off anything over land
        

        return (mag, dir)

    def calcWave3(self, SWELL_OSEQD3, SWDIR_OSEQD3):
        # extract the wind speed and direction
        mag = where(greater(SWELL_OSEQD3, 50), 0.0, SWELL_OSEQD3 * 3.28)
        mag = clip(mag, 0, 50)
        dir = clip(SWDIR_OSEQD3, 0, 359.5)
        # Clip off anything over land
        

        return (mag, dir)

    def calcWave4(self, SWELL_OSEQD4, SWDIR_OSEQD4):
        # extract the wind speed and direction
        mag = where(greater(SWELL_OSEQD4, 50), 0.0, SWELL_OSEQD4 * 3.28)
        mag = clip(mag, 0, 50)
        dir = clip(SWDIR_OSEQD4, 0, 359.5)
        # Clip off anything over land
        

        return (mag, dir)


    def calcWave5(self, SWELL_OSEQD5, SWDIR_OSEQD5):
        # extract the wind speed and direction
        mag = where(greater(SWELL_OSEQD5, 50), 0.0, SWELL_OSEQD5 * 3.28)
        mag = clip(mag, 0, 50)
        dir = clip(SWDIR_OSEQD5, 0, 359.5)
        # Clip off anything over land
        

        return (mag, dir)

    def calcWave6(self, SWELL_OSEQD6, SWDIR_OSEQD6):
        # extract the wind speed and direction
        mag = where(greater(SWELL_OSEQD6, 50), 0.0, SWELL_OSEQD6 * 3.28)
        mag = clip(mag, 0, 50)
        dir = clip(SWDIR_OSEQD6, 0, 359.5)
        # Clip off anything over land
        

        return (mag, dir)

    def calcWave7(self, SWELL_OSEQD7, SWDIR_OSEQD7):
        # extract the wind speed and direction
        mag = where(greater(SWELL_OSEQD7, 50), 0.0, SWELL_OSEQD7 * 3.28)
        mag = clip(mag, 0, 50)
        dir = clip(SWDIR_OSEQD7, 0, 359.5)
        # Clip off anything over land
        

        return (mag, dir)



class nwpsTrkngCG1Forecaster6(Forecaster):
    def __init__(self):
        Forecaster.__init__(self, "nwpsTrkngCG1", "nwpsTrkngCG1")

##--------------------------------------------------------------------------
##  Calculates Periods from nwpsTrkngCG1 D2D model.
##--------------------------------------------------------------------------
    def calcPeriod1(self, SWPER_OSEQD1):
        # Clip off anything over land
        period = clip(SWPER_OSEQD1, 0, 25)
        return period

    def calcPeriod2(self, SWPER_OSEQD2):
        # Clip off anything over land
        period = clip(SWPER_OSEQD2, 0, 25)
        return period

    def calcPeriod3(self, SWPER_OSEQD3):
        # Clip off anything over land
        period = clip(SWPER_OSEQD3, 0, 25)
        return period

    def calcPeriod4(self, SWPER_OSEQD4):
        # Clip off anything over land
        period = clip(SWPER_OSEQD4, 0, 25)
        return period

    def calcPeriod5(self, SWPER_OSEQD5):
        # Clip off anything over land
        period = where( SWPER_OSEQD5, 0)
        return period

    def calcPeriod6(self, SWPER_OSEQD6):
        # Clip off anything over land
        period = where( SWPER_OSEQD6, 0)
        return period

##--------------------------------------------------------------------------
##  Calculates Primary and Secondary Wave
##--------------------------------------------------------------------------

    def calcWave1(self, SWELL_OSEQD1, SWDIR_OSEQD1):
        # extract the wind speed and direction
        mag = where(greater(SWELL_OSEQD1, 50), 0.0, SWELL_OSEQD1 * 3.28)
        mag = clip(mag, 0, 50)
        dir = clip(SWDIR_OSEQD1, 0, 359.5)
        # Clip off anything over land
        
        
        return (mag, dir)

    def calcWave2(self, SWELL_OSEQD2, SWDIR_OSEQD2):
        # extract the wind speed and direction
        mag = where(greater(SWELL_OSEQD2, 50), 0.0, SWELL_OSEQD2 * 3.28)
        mag = clip(mag, 0, 50)
        dir = clip(SWDIR_OSEQD2, 0, 359.5)
        # Clip off anything over land
        
        
        return (mag, dir)

    def calcWave3(self, SWELL_OSEQD3, SWDIR_OSEQD3):
        # extract the wind speed and direction
        mag = where(greater(SWELL_OSEQD3, 50), 0.0, SWELL_OSEQD3 * 3.28)
        mag = clip(mag, 0, 50)
        dir = clip(SWDIR_OSEQD3, 0, 359.5)
        # Clip off anything over land
        
        
        return (mag, dir)

    def calcWave4(self, SWELL_OSEQD4, SWDIR_OSEQD4):
        # extract the wind speed and direction
        mag = where(greater(SWELL_OSEQD4, 50), 0.0, SWELL_OSEQD4 * 3.28)
        mag = clip(mag, 0, 50)
        dir = clip(SWDIR_OSEQD4, 0, 359.5)
        # Clip off anything over land
        
        
        return (mag, dir)

    def calcWave5(self, SWELL_OSEQD5, SWDIR_OSEQD5):
        # extract the wind speed and direction
        mag = where(greater(SWELL_OSEQD5, 50), 0.0, SWELL_OSEQD5 * 3.28)
        mag = clip(mag, 0, 50)
        dir = clip(SWDIR_OSEQD5, 0, 359.5)
        # Clip off anything over land
        
        
        return (mag, dir)

    def calcWave6(self, SWELL_OSEQD6, SWDIR_OSEQD6):
        # extract the wind speed and direction
        mag = where(greater(SWELL_OSEQD6, 50), 0.0, SWELL_OSEQD6 * 3.28)
        mag = clip(mag, 0, 50)
        dir = clip(SWDIR_OSEQD6, 0, 359.5)
        # Clip off anything over land
        
        
        return (mag, dir)


class nwpsTrkngCG1Forecaster5(Forecaster):
    def __init__(self):
        Forecaster.__init__(self, "nwpsTrkngCG1", "nwpsTrkngCG1")


##--------------------------------------------------------------------------
##  Calculates Periods from nwpsTrkngCG1 D2D model.
##--------------------------------------------------------------------------
    def calcPeriod1(self, SWPER_OSEQD1):
        # Clip off anything over land
        period = clip(SWPER_OSEQD1, 0, 25)
        return period

    def calcPeriod2(self, SWPER_OSEQD2):
        # Clip off anything over land
        period = clip(SWPER_OSEQD2, 0, 25)
        return period

    def calcPeriod3(self, SWPER_OSEQD3):
        # Clip off anything over land
        period = clip(SWPER_OSEQD3, 0, 25)
        return period

    def calcPeriod4(self, SWPER_OSEQD4):
        # Clip off anything over land
        period = clip(SWPER_OSEQD4, 0, 25)
        return period

    def calcPeriod5(self, SWPER_OSEQD5):
        # Clip off anything over land
        period = where( SWPER_OSEQD5, 0)
        return period

##--------------------------------------------------------------------------
##  Calculates Primary and Secondary Wave
##--------------------------------------------------------------------------

    def calcWave1(self, SWELL_OSEQD1, SWDIR_OSEQD1):
        # extract the wind speed and direction
        mag = where(greater(SWELL_OSEQD1, 50), 0.0, SWELL_OSEQD1 * 3.28)
        mag = clip(mag, 0, 50)
        dir = clip(SWDIR_OSEQD1, 0, 359.5)
        # Clip off anything over land
        
        
        return (mag, dir)

    def calcWave2(self, SWELL_OSEQD2, SWDIR_OSEQD2):
        # extract the wind speed and direction
        mag = where(greater(SWELL_OSEQD2, 50), 0.0, SWELL_OSEQD2 * 3.28)
        mag = clip(mag, 0, 50)
        dir = clip(SWDIR_OSEQD2, 0, 359.5)
        # Clip off anything over land
        
        
        return (mag, dir)

    def calcWave3(self, SWELL_OSEQD3, SWDIR_OSEQD3):
        # extract the wind speed and direction
        mag = where(greater(SWELL_OSEQD3, 50), 0.0, SWELL_OSEQD3 * 3.28)
        mag = clip(mag, 0, 50)
        dir = clip(SWDIR_OSEQD3, 0, 359.5)
        # Clip off anything over land
        
        
        return (mag, dir)

    def calcWave4(self, SWELL_OSEQD4, SWDIR_OSEQD4):
        # extract the wind speed and direction
        mag = where(greater(SWELL_OSEQD4, 50), 0.0, SWELL_OSEQD4 * 3.28)
        mag = clip(mag, 0, 50)
        dir = clip(SWDIR_OSEQD4, 0, 359.5)
        # Clip off anything over land
        
        
        return (mag, dir)


    def calcWave5(self, SWELL_OSEQD5, SWDIR_OSEQD5):
        # extract the wind speed and direction
        mag = where(greater(SWELL_OSEQD5, 50), 0.0, SWELL_OSEQD5 * 3.28)
        mag = clip(mag, 0, 50)
        dir = clip(SWDIR_OSEQD5, 0, 359.5)
        # Clip off anything over land
        
        
        return (mag, dir)


######################################################################################
######################################################################################


class nwpsTrkngCG1Forecaster(Forecaster):
    def __init__(self):
        Forecaster.__init__(self, "nwpsTrkngCG1", "nwpsTrkngCG1")


##--------------------------------------------------------------------------
##  Calculates Periods from nwpsTrkngCG1 D2D model.
##--------------------------------------------------------------------------
    def calcPeriod1(self, SWPER_OSEQD1):
        # Clip off anything over land
        period = clip(SWPER_OSEQD1, 0, 25)
        return period

    def calcPeriod2(self, SWPER_OSEQD2):
        # Clip off anything over land
        period = clip(SWPER_OSEQD2, 0, 25)
        return period

    def calcPeriod3(self, SWPER_OSEQD3):
        # Clip off anything over land
        period = clip(SWPER_OSEQD3, 0, 25)
        return period

    def calcPeriod4(self, SWPER_OSEQD4):
        # Clip off anything over land
        period = clip(SWPER_OSEQD4, 0, 25)
        return period

##--------------------------------------------------------------------------
##  Calculates Primary and Secondary Wave  
##--------------------------------------------------------------------------

    def calcWave1(self, SWELL_OSEQD1, SWDIR_OSEQD1):
        # extract the wind speed and direction
        mag = where(greater(SWELL_OSEQD1, 50), 0.0, SWELL_OSEQD1 * 3.28)
        mag = clip(mag, 0, 50)
        dir = clip(SWDIR_OSEQD1, 0, 359.5)
        # Clip off anything over land
        
        
	return (mag, dir)

    def calcWave2(self, SWELL_OSEQD2, SWDIR_OSEQD2):
        # extract the wind speed and direction
        mag = where(greater(SWELL_OSEQD2, 50), 0.0, SWELL_OSEQD2 * 3.28)
        mag = clip(mag, 0, 50)
        dir = clip(SWDIR_OSEQD2, 0, 359.5)
        # Clip off anything over land
        
        
	return (mag, dir)

    def calcWave3(self, SWELL_OSEQD3, SWDIR_OSEQD3):
        # extract the wind speed and direction
        mag = where(greater(SWELL_OSEQD3, 50), 0.0, SWELL_OSEQD3 * 3.28)
        mag = clip(mag, 0, 50)
        dir = clip(SWDIR_OSEQD3, 0, 359.5)
        # Clip off anything over land
        
        
	return (mag, dir)

    def calcWave4(self, SWELL_OSEQD4, SWDIR_OSEQD4):
        # extract the wind speed and direction
        mag = where(greater(SWELL_OSEQD4, 50), 0.0, SWELL_OSEQD4 * 3.28)
        mag = clip(mag, 0, 50)
        dir = clip(SWDIR_OSEQD4, 0, 359.5)
        # Clip off anything over land
        
        
	return (mag, dir)

######################################################################################
######################################################################################

class nwpsTrkngCG1Forecaster3(Forecaster):
    def __init__(self):
        Forecaster.__init__(self, "nwpsTrkngCG1", "nwpsTrkngCG1")


##--------------------------------------------------------------------------
##  Calculates Periods from nwpsTrkngCG1 D2D model.
##--------------------------------------------------------------------------
    def calcPeriod1(self, SWPER_OSEQD1):
        # Clip off anything over land
        period = clip(SWPER_OSEQD1, 0, 25)
        return period

    def calcPeriod2(self, SWPER_OSEQD2):
        # Clip off anything over land
        period = clip(SWPER_OSEQD2, 0, 25)
        return period

    def calcPeriod3(self, SWPER_OSEQD3):
        # Clip off anything over land
        period = clip(SWPER_OSEQD3, 0, 25)
        return period


##--------------------------------------------------------------------------
##  Calculates Primary and Secondary Wave
##--------------------------------------------------------------------------

    def calcWave1(self, SWELL_OSEQD1, SWDIR_OSEQD1):
        # extract the wind speed and direction
        mag = where(greater(SWELL_OSEQD1, 50), 0.0, SWELL_OSEQD1 * 3.28)
        mag = clip(mag, 0, 50)
        dir = clip(SWDIR_OSEQD1, 0, 359.5)
        # Clip off anything over land
        
        
        return (mag, dir)

    def calcWave2(self, SWELL_OSEQD2, SWDIR_OSEQD2):
        # extract the wind speed and direction
        mag = where(greater(SWELL_OSEQD2, 50), 0.0, SWELL_OSEQD2 * 3.28)
        mag = clip(mag, 0, 50)
        dir = clip(SWDIR_OSEQD2, 0, 359.5)
        # Clip off anything over land
        
        
        return (mag, dir)

    def calcWave3(self, SWELL_OSEQD3, SWDIR_OSEQD3):
        # extract the wind speed and direction
        mag = where(greater(SWELL_OSEQD3, 50), 0.0, SWELL_OSEQD3 * 3.28)
        mag = clip(mag, 0, 50)
        dir = clip(SWDIR_OSEQD3, 0, 359.5)
        # Clip off anything over land
        
        
        return (mag, dir)


class nwpsTrkngCG1Forecaster2(Forecaster):
    def __init__(self):
        Forecaster.__init__(self, "nwpsTrkngCG1", "nwpsTrkngCG1")


##--------------------------------------------------------------------------
##  Calculates Periods from nwpsTrkngCG1 D2D model.
##--------------------------------------------------------------------------
    def calcPeriod1(self, SWPER_OSEQD1):
        # Clip off anything over land
        period = clip(SWPER_OSEQD1, 0, 25)
        return period

    def calcPeriod2(self, SWPER_OSEQD2):
        # Clip off anything over land
        period = clip(SWPER_OSEQD2, 0, 25)
        return period


##--------------------------------------------------------------------------
##  Calculates Primary and Secondary Wave
##--------------------------------------------------------------------------

    def calcWave1(self, SWELL_OSEQD1, SWDIR_OSEQD1):
        # extract the wind speed and direction
        mag = where(greater(SWELL_OSEQD1, 50), 0.0, SWELL_OSEQD1 * 3.28)
        mag = clip(mag, 0, 50)
        dir = clip(SWDIR_OSEQD1, 0, 359.5)
        # Clip off anything over land
        
        
        return (mag, dir)

    def calcWave2(self, SWELL_OSEQD2, SWDIR_OSEQD2):
        # extract the wind speed and direction
        mag = where(greater(SWELL_OSEQD2, 50), 0.0, SWELL_OSEQD2 * 3.28)
        mag = clip(mag, 0, 50)
        dir = clip(SWDIR_OSEQD2, 0, 359.5)
        # Clip off anything over land
        
        
        return (mag, dir)



def main():
    try:
        nwpsTrkngCG1Forecaster8().run()
    except:
        try:
            nwpsTrkngCG1Forecaster7().run()
        except:
            try:
    	        nwpsTrkngCG1Forecaster6().run()
            except:
                try:
       	            nwpsTrkngCG1Forecaster5().run()
                except:
                    try:	
                        nwpsTrkngCG1Forecaster().run()
                    except:
    	                try:
                            nwpsTrkngCG1Forecaster3().run()
    	                except:
    	                    nwpsTrkngCG1Forecaster2().run()
		
   #os.system('/awips/GFESuite/primary/bin/sendGfeMessage -u -m "nwpsTrkngCG1 Version 3 WAVE DATA ARE NOW IN GFE"')
    #os.system('/awips/GFESuite/primary/bin/runProcedure -n nwpsTrkngCG1_Confirm -u SITE -c localConfig')
    # os.system('/awips/GFESuite/primary/bin/runProcedure -n nwpsTrkngCG1_Init -u SITE -c localConfig')
    
if __name__ == "__main__":
    main()

