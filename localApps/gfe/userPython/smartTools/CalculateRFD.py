# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# CalculateRFD.py
#
# Author: dtomalak
# Optimized by njensen
# ----------------------------------------------------------------------------


ToolType = "numeric"
WeatherElementEdited = "RFD"
from numpy import *
import time
HideTool = 0

# Set up Class
import SmartScript
# For available commands, see SmartScript


class Tool (SmartScript.SmartScript):
    def __init__(self, dbss):
        SmartScript.SmartScript.__init__(self, dbss)

    # Required Method: Execute
    #  %comment
    # Fill in the arguments you want to use -- WeatherElement1, WeatherElement2...

    def execute(self, T, RH, Wind, Sky, PoP, RFD, PERCENTGREEN, varDict, GridTimeRange):
        "Put your tool description here"
        t0 = time.time()
        self._determinemaxmin(T, RH, Wind, PERCENTGREEN) #find absolute max/MIN values
        self._popthresh = self._preciptthresh()
        #st = time.time()
        fuellag = self._create1hrtimelag(T,RH,Sky,GridTimeRange)
        #ed = time.time()
        #diff = ed- st
        #diff = str(diff)
        #print diff + " seconds to run fuel lag"
        #st = time.time()
        finemoist = self._calcfinefuelmoisture(fuellag, PERCENTGREEN, GridTimeRange)
        #ed = time.time()
        #diff = ed- st
        #diff = str(diff)
        #print diff + " seconds to run fine moist"
        #finefuel, temp, Sky
        #st = time.time()
        ignite = self._calcignition(finemoist, T, Sky)
        #ed = time.time()
        #diff = ed- st
        #diff = str(diff)
        #print diff + " seconds to run ignition"
        #st = time.time()
        RFD = self._calcRFDINDEX(ignite, Wind)
        #ed = time.time()
        #diff = ed- st
        #diff = str(diff)
        #print diff + " seconds to run RFD"
        RFDcat = self._catagoricalRFD(RFD)
        #CLEAN UP WHERE SIG PRECIP IS OCCURING
        #st = time.time()
        #njensen RFDcat = where(greater_equal(PoP,self._popthresh),0,RFDcat)
        RFDcat[greater_equal(PoP,self._popthresh)] = 0
        #ed = time.time()
        #diff = ed- st
        #diff = str(diff)
        #print diff + " seconds to run RFDcat"
        #print Wind[1]
        #self.createGrid("EXP","finemoiost","SCALAR",finemoist,GridTimeRange)
        #self.createGrid("EXP","ignite","SCALAR",ignite,GridTimeRange)
        #self.createGrid("EXP","RFD","SCALAR",RFD,GridTimeRange)
        #self.createGrid("EXP","SunT","SCALAR",sun,GridTimeRange)
        #self.createGrid("EXP","FuelT","SCALAR",fuel,GridTimeRange)
        t1 = time.time()
        print "inside CalculateRFD_NDJ took:", (t1-t0)
        return RFDcat

    #EACH STEP IN THE PROCESS WILL GET ITS OWN MODULE
    #it appears that there are 4 main steps
    #CALCULATIONS
    #SINCE THIS TOOL REQUIRES SEVERAL DIFFERENT CALCULATIONS/COMPARISONS
    #WILL DO EACH MODULALLY
    def _create1hrtimelag(self, Tgrid, RHgrid,Skygrid,timer):
        #Step one Make RH and T Catagory grids
        #there are 6 temperature catagories and 21 Rh catagories
        #create the temp catagories 0 - 5
        #njensen Tcat = where(Tgrid,0,0)
        Tcat = zeros(Tgrid.shape, int32)
        count = 0
        starter = 29
        tlist = []
        if self._mint <= starter:
            #njensen Tcat = where(less_equal(Tgrid, starter), count, Tcat)
            Tcat[Tgrid <= starter] = count
            tlist.append(count)
        count = 1
        while starter < 109:
            if starter > self._maxt:
                break
            if starter+20 >= self._mint and starter <= self._maxt:
                #njensen Tcat = where(logical_and(greater(Tgrid, starter), less_equal(Tgrid,starter + 20)), count, Tcat)
                Tcat[logical_and(greater(Tgrid, starter), less_equal(Tgrid,starter + 20))] = count
                tlist.append(count)
            count = count + 1
            starter = starter + 20
            continue
        if self._maxt >= 109:
             #njensen Tcat = where(greater_equal(Tgrid, 109), count, Tcat)
             Tcat[Tgrid >= 109] = count
             tlist.append(count)
        #NOW THE RHCAT
        #njensen RHcat = where(RHgrid,0,0)
        RHcat = zeros(RHgrid.shape, int32)
        #njensen RHcat = where(less(RHgrid, self._minrh), 100, RHcat)
        RHcat[RHgrid < self._minrh] = 100
        validrh = []
        if self._minrh <= 4:
            #njensen RHcat = where(less_equal(RHgrid, 4), 0 , RHcat)
            RHcat[RHgrid <= 4] = 0
            validrh.append(0)
        starter = 1
        baseRh = 4
        while starter <= 19:
            if baseRh > self._maxrh:
                break
            if self._minrh <= baseRh + 5 and baseRh <= self._maxrh:
                #since RH values are even 4 % increments can use this short cut                
                #njensen RHcat = where(logical_and(greater(RHgrid, baseRh), less_equal(RHgrid, baseRh + 5)), starter, RHcat)
                RHcat[logical_and(greater(RHgrid, baseRh), less_equal(RHgrid, baseRh + 5))] = starter
                validrh.append(starter)
            starter = starter + 1
            baseRh = baseRh + 5
            continue
        
        if self._maxrh > 99:
            #njensen RHcat = where(greater(RHgrid, 99), 20, RHcat)
            RHcat[RHgrid > 99] = 20
            validrh.append(20)
        SKYcat = where(greater_equal(Skygrid, 75), 1, 0)
        suntable,cloudtable = self._onehrtimelagtable()
        
        
        ####NOW THE DATA HAS BEEN CATAGORIZED WILL HAVE TO STEP THROUGH EACH
        #CATAGORY TO DETERMINE 1hr fuel lag grid
        x = tlist[0]
        y = validrh[0]
        xsize = tlist[-1]
        ysize = validrh[-1]
        #njensen sunnyfuel = where(Skygrid, 0 , 0)
        sunnyfuel = zeros(Skygrid.shape, int32)
        hrtimelag = SKYcat
##        while x <= xsize:
##            row = suntable[x]
##            rowcld = cloudtable[x]
##            y = 0
##            while y <= ysize:
##                value = row[y]
##                valuecld = rowcld[y]
##                tempo = where(logical_and(equal(Tcat, x), equal(RHcat, y)), 1, 0)
##                fl = value#where(equal(SKYcat, 1), valuecld,value)
##                sunnyfuel = tempo * fl  #all locations that have 0 for tempo will be set to zero
##                #sunnyfuel = where(logical_and(equal(Tcat, x), equal(RHcat, y)), value, sunnyfuel)
##                y = y + 1
##                hrtimelag = where(greater(sunnyfuel,0),sunnyfuel, hrtimelag)
##                continue
##            x = x + 1
##            continue
        while x <= xsize:
            row = suntable[x]
            y = validrh[0]
            while y <= ysize:
                value = row[y]
                #njensen sunnyfuel = where(logical_and(equal(Tcat, x), equal(RHcat, y)), value, sunnyfuel)
                sunnyfuel[logical_and(equal(Tcat, x), equal(RHcat, y))] = value
                y = y + 1
                continue
            x = x + 1
            continue
        if SKYcat.any() :
            #njensen cloudyfuel = where(Skygrid, 0 , 0)
            cloudyfuel = zeros(Skygrid.shape, int32)
            x = tlist[0]
            y = validrh[0]
            xsize = tlist[-1]
            ysize = validrh[-1]
            while x <= xsize:
                row = cloudtable[x]
                #row = suntable[x]
                y = validrh[0]
                while y <= ysize:
                    value = row[y]
                    #njensen cloudyfuel = where(logical_and(equal(Tcat, x), equal(RHcat, y)), value, cloudyfuel)
                    cloudyfuel[logical_and(equal(Tcat, x), equal(RHcat, y))] = value
                    y = y + 1
                    continue
                x = x + 1
                continue
        hrtimelag = where(Skygrid,0,0)
        if SKYcat.any():
            hrtimelag =  where(equal(SKYcat, 1), cloudyfuel, sunnyfuel)
        else:
            hrtimelag = sunnyfuel
        #self.createGrid("EXP","cldmoist","SCALAR",cloudyfuel,timer)
        #self.createGrid("EXP","Sunmoist","SCALAR",hrtimelag,timer)
        #self.createGrid("EXP","RHCAT","SCALAR",RHcat,timer)
        #self.createGrid("EXP","SKYCAT","SCALAR",SKYcat,timer)
        
        return  hrtimelag
    def _calcfinefuelmoisture(self, timelagfuel, greeness, timer):
        tabledat = self._finefuelmoisturetable()
        x = len(timelagfuel)
        y = len(timelagfuel[0])
        sizer = x * y
        lagdata = reshape(timelagfuel,(sizer,))
        lagdata = sort(lagdata)
        lagmin = lagdata[0]
        lagmax = lagdata[-1]
        #STEP ONE NEED TO CATAGORIZE THE 1hr time time lag and percent green data
        #SIMILAR TO TEMPS AND RH
        #THERE ARE 15 CATAGORIES FOR Fuel moisture and roughly 9 for precent green
        #njensen Fuelcat = where(greeness,0,0)
        Fuelcat = zeros(greeness.shape, int32)
        #1-6 value the same as the step
        stepper = 1
        count = 0
        laglist = []
        flcatlngth = len(tabledat)
        if lagmin <= 6:
            while stepper <= 6:
                if stepper >= lagmin and stepper <= lagmax:
                    #njensen Fuelcat = where(equal(timelagfuel,stepper),count,Fuelcat)
                    Fuelcat[equal(timelagfuel,stepper)] = count
                    laglist.append(count)
                stepper = stepper + 1
                count = count + 1
               # print stepper,count
                continue
        else:
            stepper = 7
            count = 5
        if lagmin < 19 and lagmax > 6:
            while stepper < 19:
                #print stepper
                if stepper+1 >= lagmin and stepper <= lagmax:
                    #njensen Fuelcat = where(logical_and(greater_equal(timelagfuel, stepper), less_equal(timelagfuel,stepper+1)),count,Fuelcat)
                    Fuelcat[logical_and(greater_equal(timelagfuel, stepper), less_equal(timelagfuel,stepper+1))] = count
                    laglist.append(count)   
                stepper = stepper + 2
                count = count + 1
                #print stepper, count
                continue
        else:
            stepper = 19
            count = 11
        if lagmin < 25 and lagmax >= 19:
            while stepper < 25:
                #print str(stepper) + "$$$"
                if stepper+2 >= lagmin and stepper <= lagmax:
                    #Fuelcat = where(logical_and(greater_equal(timelagfuel, stepper), less_equal(timelagfuel,stepper+2)),count,Fuelcat)
                    Fuelcat[logical_and(greater_equal(timelagfuel, stepper), less_equal(timelagfuel,stepper+2))] = count
                    laglist.append(count)
                stepper = stepper + 3
                print stepper
                count = count + 1
                #print stepper,count
                continue
        else:
            stepper = 25
            count = 14
        if lagmax >= 25:
            if stepper >= 25:
                #njensen Fuelcat = where(greater_equal(timelagfuel, stepper),count,Fuelcat)
                Fuelcat[timelagfuel > stepper] = count
                laglist.append(count)
            #print stepper, count
        #NOW PERCENT GREEN CATAGORIES
        #ON THE PERCENT GREEN TABLE ONLY THE 1st and LAST are different...all other are 9
        #percent increments
        #njensen GREENcat = where(greeness,0,0)
        GREENcat = zeros(greeness.shape, int32)
        grlist = []
        if self._mingreen < 5:
             #njensen GREENcat = where(less(greeness, 5), 0, GREENcat)
             GREENcat[greeness < 5] = 0
             grlist.append(0)
        count = 1
        ender = 8
        stepper  = 5
        while count < ender:
            #print count, stepper
            if stepper+10 > self._mingreen and stepper <= self._maxgreen:
                #njensen GREENcat = where(logical_and(greater_equal(greeness,stepper),less(greeness, stepper+10)),count,GREENcat)
                GREENcat[logical_and(greater_equal(greeness,stepper),less(greeness, stepper+10))] = count
                grlist.append(count)
            count = count + 1
            stepper = stepper + 10
            continue
        if self._maxgreen >= 75:
            #print count, stepper
            #njensen GREENcat = where(greater(greeness, 75), count, GREENcat)
            GREENcat[greeness > 75] = count
            grlist.append(count)
        #CALCULATE THE FINE FUEL MOISTURE
        x = laglist[0]
        y = grlist[0]
        xsize = laglist[-1]
        ysize = grlist[-1]
        #njensen finemoisture = where(greeness, 0 , 0)
        finemoisture = zeros(greeness.shape, int32)
        #print len(GREENcat[0]), len(Fuelcat[0]), len(finemoisture)
        while x <= xsize:
            row = tabledat[x]
            y = grlist[0]
            while y <= ysize:
                value = row[y]
                #print value
                #njensen finemoisture = where(logical_and(equal(Fuelcat, x), equal(GREENcat, y)), value, finemoisture)
                finemoisture[logical_and(equal(Fuelcat, x), equal(GREENcat, y))] = value
                y = y + 1
                continue
            x = x + 1
            continue
        ######       Commented out the following line as we were
        ######        getting abnormally high values when rh was around 50 percent.
        ######        Seems when 1-hr Time Lag Fuel Moisture is cat 8 and Percent Green
        ######        is between 5-14 percent, Fine Fuel moisture does not go to a 9 as tables
        ######        show they should.
        finemoisture = where(equal(finemoisture,0),timelagfuel,finemoisture)

        #self.createGrid("EXP","finemoist","SCALAR",finemoisture,timer)
        #self.createGrid("EXP","tlagfuel","SCALAR",timelagfuel,timer)
        #self.createGrid("EXP","FuelCat","SCALAR",Fuelcat,timer)
        #self.createGrid("EXP","GREENcat","SCALAR",GREENcat,timer)
        return finemoisture
        
    #TABLES SECTION
    def _calcignition(self, finefuel, temp, Sky):
        tabledat = self._ignitioncomptable()
        #njensen
        #Fuelcat = where(Sky,0,0)
        #SunnyTcat = where(Sky,0,0)
        #CloudyTcat = where(Sky, 0, 0)
        #ignitionsun = where(Sky,0,0)
        #ignitioncld = where(Sky,0,0)
        Fuelcat = zeros(Sky.shape, int32)
        SunnyTcat = zeros(Sky.shape, int32)
        CloudyTcat = zeros(Sky.shape, int32)
        ignitionsun = zeros(Sky.shape, int32)
        ignitioncld = zeros(Sky.shape, int32)
        Skycat = where(greater_equal(Sky,80),1,0)
        temp = where(logical_and(equal(Skycat,1), greater_equal(temp, 20)),temp-20,temp)
        #njensen temp = where(less(temp,10),10,temp)
        temp[temp < 10] = 10
        x = len(temp)
        y = len(temp[0])
        newsize = x*y                #should be same size for all arrays
        tdata = reshape(temp,(newsize,))
        tdata = sort(tdata)
        self._skymint= tdata[0]
        if Skycat.any() and self._skymint < self._mint:
            self._mint = self._skymint
        x = len(finefuel)
        y = len(finefuel[0])
        sizer = x * y
        fueldata = reshape(finefuel,(sizer,))
        fueldata = sort(fueldata)
        fuelmin = fueldata[0]
        fuelmax = fueldata[-1]
        #First create Fine fuel catagories
        #EXACT SAME METHOD AS finefuelcat
        #1-6 value the same as the step
        stepper = 1
        fuellist = []
        count = 0
        if fuelmin <= 6:
            while stepper <= 6:
                if stepper >= fuelmin and stepper <= fuelmax:
                    #njensen Fuelcat = where(equal(finefuel,stepper),count,Fuelcat)
                    Fuelcat[equal(finefuel,stepper)] = count
                    fuellist.append(count)
                stepper = stepper + 1
                count = count + 1
               # print stepper,count
                continue
        else:
            stepper = 7
            count = 5
        if fuelmin < 19 and fuelmax > 6:
            while stepper < 19:
                #print stepper
                if stepper+1 >= fuelmin and stepper <= fuelmax:
                    #njensen Fuelcat = where(logical_and(greater_equal(finefuel, stepper), less_equal(finefuel,stepper+1)),count,Fuelcat)
                    Fuelcat[logical_and(greater_equal(finefuel, stepper), less_equal(finefuel,stepper+1))] = count
                    fuellist.append(count)   
                stepper = stepper + 2
                count = count +1
                #print stepper, count
                continue
        else:
            stepper = 19
            count = 11
        if fuelmin < 25 and fuelmax >= 19:
            while stepper < 25:
                #print str(stepper) + "$$$"
                if stepper+2 >= fuelmin and stepper <= fuelmax:
                    #njensen Fuelcat = where(logical_and(greater_equal(finefuel, stepper), less_equal(finefuel,stepper+2)),count,Fuelcat)
                    Fuelcat[logical_and(greater_equal(finefuel, stepper), less_equal(finefuel,stepper+2))] = count
                    fuellist.append(count)
                stepper = stepper + 3
                count = count + 1
                #print stepper,count
                continue
        else:
            stepper = 25
            count = 14
        if fuelmax >= 25:
            if stepper >= 25:
                #Fuelcat = where(greater_equal(finefuel, stepper),count,Fuelcat)
                Fuelcat[finefuel >= stepper] = count
                fuellist.append(count)
        stepper = 20
        tlist = []
        if self._mint < 20:
            #njensen SunnyTcat = where(less(temp, stepper), 0 , SunnyTcat)
            SunnyTcat[temp < stepper] = 0
            tlist.append(0)
        count = 1
        ender =len(tabledat) - 1
        while count < ender:
            if stepper+10 > self._mint and stepper <= self._maxt:
                #njensen SunnyTcat =where(logical_and(greater_equal(temp,stepper), less(temp,stepper+10)),count, SunnyTcat)
                SunnyTcat[logical_and(greater_equal(temp,stepper), less(temp,stepper+10))] = count
                tlist.append(count)
            count = count+1
            stepper = stepper + 10
            continue
        if self._maxt >= stepper:
            #njensen SunnyTcat = where(greater_equal(temp, stepper), count, SunnyTcat)
            SunnyTcat[temp >= stepper] = count
            tlist.append(count)
        #IGNITION
        x=tlist[0]
        y=fuellist[0]
        xsize = tlist[-1]
        ysize = fuellist[-1]
        while x <= xsize:
            row = tabledat[x]
            y = fuellist[0]
            while y <= ysize:
                value = row[y]
                #njensen ignitionsun = where(logical_and(equal(SunnyTcat, x), equal(Fuelcat, y)), value, ignitionsun)
                ignitionsun[logical_and(equal(SunnyTcat, x), equal(Fuelcat, y))] = value
                y = y + 1
                continue
            x = x + 1
            continue
        return ignitionsun
    
    def _calcRFDINDEX(self, ignition, wind):
        #THIS TOOL WILL CALCULATE THE RANGELAND FIRE DANGER...BASED OF THE IGNITION Component
        #and WINDSPEED
        #variables
        x = len(ignition)
        y = len(ignition[0])
        sizer = x * y
        igdata = reshape(ignition,(sizer,))
        igdata = sort(igdata)
        igmin = igdata[0]
        igmax = igdata[-1]
        #njensen RFD = where(ignition,0,0)
        RFD = zeros(ignition.shape, int32)
        spd = wind[0]
        wind = spd * 1.15 #convert to mph
        self._minwind = self._minwind * 1.15
        self._maxwind = self._maxwind * 1.15
        tabledat = self._RFDtable()
        #SPLIT DATA INTO x,y "component" grids Ignition -x Wind speed y
        #First ignition
        count = 1
        #SPECIAL CASE For 0
        iglist = []
        #njensen IGcat = where(ignition,0,0)
        IGcat = zeros(ignition.shape, int32)
        if igmin <= 0:
            #njensen IGcat = where(equal(ignition,0),0,IGcat)
            IGcat[equal(ignition,0)] = 0
            #no need to iterate through this axis as RFD will be 0
        stepper = 1
        ender = len(tabledat) - 1
        while count <= ender:
            #print str(stepper) + "-" + str(stepper+5) + ":::" + str(count)
            if stepper+5 > igmin and stepper <= igmax:
                #njensen IGcat = where(logical_and(greater_equal(ignition,stepper), less(ignition,stepper+5)), count, IGcat)
                IGcat[logical_and(greater_equal(ignition,stepper), less(ignition,stepper+5))] = count
                iglist.append(count) 
            count = count + 1
            stepper = stepper + 5
            continue
        #WIND (y axis)
        #1-8 mph are every 1 mph
        #after 8 mph it is every 2
        ender = len(tabledat[0])-2
        count = 0
        wlist = []
        stepper = 0.0   #am using .5 decimal place to account for conversions leaving remainders
                        #ie 0-1 = > 0 <1.5
        #njensen Wndcat = where(wind,0,0)
        Wndcat = zeros(wind.shape, int32)
        test = 0
        while count <=7:
            #print count
            #print ":::::"
            if count == 0 and self._minwind <= 1.4:
                #njensen Wndcat = where(logical_and(greater_equal(wind, stepper), less(wind, stepper+1.5)), count, Wndcat)
                Wndcat[logical_and(greater_equal(wind, stepper), less(wind, stepper+1.5))] = count
                #print stepper, stepper + 1.5
                #print "?????????????????"
                wlist.append(count)
                stepper = stepper + 1.5
                test = 1
            else:
                if test != 1:
                    test = 1
                    count = 1
                    stepper = 1.5 #should only happen once
                if stepper+1.0 > self._minwind and stepper <= self._maxwind:
                    #njensen Wndcat = where(logical_and(greater_equal(wind, stepper), less(wind, stepper+1)), count, Wndcat)
                    Wndcat[logical_and(greater_equal(wind, stepper), less(wind, stepper+1))] = count
                    wlist.append(count)
                stepper = stepper + 1
            count = count + 1
        while count <= ender:
            #every 2 mph until 26.5 mph
            #print count, stepper, stepper + 2
            if stepper+2.0 > self._minwind and stepper <= self._maxwind:
                #njensen Wndcat = where(logical_and(greater_equal(wind, stepper), less(wind, stepper+2)), count, Wndcat)
                Wndcat[logical_and(greater_equal(wind, stepper), less(wind, stepper+2))] = count
                wlist.append(count)
            stepper = stepper + 2
            count = count + 1
            continue
        if count > ender:
            #print count, stepper
            if stepper <= self._maxwind:
                #njensen Wndcat = where(greater_equal(wind, stepper), count, Wndcat)
                Wndcat[wind >= stepper] = count
                wlist.append(count)
        x=iglist[0]
        y=wlist[0]
        xsize = iglist[-1]
        ysize = wlist[-1]
        while x <= xsize:
            row = tabledat[x]
            y = wlist[0]
            while y <= ysize:
                value = row[y]
                #njensen RFD = where(logical_and(equal(IGcat, x), equal(Wndcat, y)), value, RFD)
                RFD[logical_and(equal(IGcat, x), equal(Wndcat, y))] = value
                y = y + 1
                continue
            x = x + 1
            continue
##        if igmin <= 0:
##            RFD = where(equal(IGcat,0),0,RFD)
        return RFD
        
    def _catagoricalRFD(self,RFD):
        self._cat = self._statecriteria()
        keys = self._cat.keys()
        #njensen newRfd = where(RFD,0,0)
        newRfd = zeros(RFD.shape, int32)
        for area in keys:
            datalist = self._cat[str(area)]
            areamask = self.encodeEditArea(area)
            #njensen tempo = where(RFD,0,0)
            tempo = zeros(RFD.shape, int32)
            for config in datalist:
                cat = config[0]
                min = config[1]
                max = config[2]
                #njensen tempo = where(logical_and(greater_equal(RFD,min), less_equal(RFD,max)),cat,tempo)
                tempo[logical_and(greater_equal(RFD,min), less_equal(RFD,max))] = cat
                newRfd = where(greater_equal(areamask,1),tempo,newRfd)
                continue
            
        return newRfd       
    def _determinemaxmin(self, T, RH,Wind, PERCENTGREEN):
        #in order to save time this script will try to determine the maxium
        #and minimum range to look for each variable
        #for rh
        #rhdata = asarray(RH)
        #tdata = asarray(T)
        #wdata = asarray(Wind[0])
        x = len(RH)
        y = len(RH[0])
        newsize = x*y                #should be same size for all arrays
        rhdata = reshape(RH,(newsize,))
        tdata = reshape(T,(newsize,))
        winddata = reshape(Wind[0],(newsize,))
        greendata = reshape(PERCENTGREEN, (newsize,))
        rhdata = sort(rhdata)
        tdata = sort(tdata)
        winddata = sort(winddata)
        greendata = sort(greendata)
        self._minrh = rhdata[0]
        self._maxrh = rhdata[-1]
        self._mint= tdata[0]
        self._maxt = tdata[-1]
        self._minwind =winddata[0]
        self._maxwind =winddata[-1]
        self._mingreen =greendata[0]
        self._maxgreen =greendata[-1]
        return
        
    def _onehrtimelagtable(self):
        #returns two lists 1 for sunny 1 for cloudy
        #BASED ON TABLE 1
        #setup id [temprange[rhrangevalues]]
        #docmunetation pending
        sunnyhrlag = [[1,2,2,3,4,5,5,6,7,8,8,8,9,9,10,11,12,12,13,13,14],
                      [1,2,2,3,4,5,5,6,7,7,7,8,9,9,10,10,11,12,13,13,13],
                      [1,2,2,3,4,5,5,6,6,7,7,8,8,9, 9,10,11,12,12,12,13],
                      [1,1,2,2,3,4,5,5,6,7,7,8,8,8, 9,10,10,11,12,12,13],
                      [1,1,2,2,3,4,4,5,6,7,7,8,8,8, 9,10,10,11,12,12,13],
                      [1,1,2,2,3,4,4,5,6,7,7,8,8,8, 9,10,10,11,12,12,13]
                      ]
        cloudylag = [[1,2,4,5,5,6,7,8,9,10,11,12,12,14,15,17,19,22,25,25,25],
                     [1,2,3,4,5,6,7,8,9, 9,11,11,12,13,14,16,18,21,24,25,25],
                     [1,2,3,4,5,6,6,8,8, 9,10,11,11,12,14,16,17,20,23,25,25],
                     [1,2,3,4,4,5,6,7,8, 9,10,10,11,12,13,15,17,20,23,25,25],
                     [1,2,3,3,4,5,6,7,8, 9, 9,10,10,11,13,14,16,19,22,25,25],
                     [1,2,2,3,4,5,6,6,8, 8, 9, 9,10,11,12,14,16,19,21,24,25]
                     ]
        return sunnyhrlag, cloudylag

    def _finefuelmoisturetable(self):
        #returns a list based of Table2
        #will comapre 1 hr fuel moisture  to percent green
        #will be a two dimensional list with x =1 hr fuel moisture catagory
        #and y (the internal list being the the fine fuel moisture value with the
        #percent green catagory represented by the y index
        #0 represents a NO CHANGE value use the 1 hr time lag value
        table = [[0,2,3,4,5,8,13,18,21],
                 [0,3,4,5,7,10,16,19,22],
                 [0,4,5,7,9,14,18,20,22],
                 [0,5,6,8,12,16,19,21,23],
                 [0,6,8,11,14,18,20,22,23],
                 [0,7,10,13,16,19,20,22,23],
                 [0,9,12,15,18,20,21,22,23],
                 [0,12,15,17,19,20,22,23,24],
                 [0,14,17,18,20,21,22,23,24],
                 [0,16,18,19,20,21,22,23,24],
                 [0,17,19,20,21,22,22,23,24],
                 [0,19,20,21,21,22,23,23,24],
                 [0,21,21,22,22,23,23,24,24],
                 [0,24,24,24,24,24,24,24,25],
                 [0,25,25,25,25,25,25,25,25]
                 ]
        return table

    def _ignitioncomptable(self):
        #will return one table (only difference is temp ranges for cloudy vs sunny
        tble = [
                      [88,75,64,54,46,39,30,21,14,9,5,2,0,0,0],
                      [90,77,66,56,48,41,32,22,15,9,5,2,0,0,0],
                      [93,80,68,58,50,42,33,23,16,10,6,3,0,0,0],
                      [95,82,71,61,52,44,35,25,17,11,7,3,1,0,0],
                      [98,85,73,63,54,46,36,26,18,12,7,4,1,0,0],
                      [100,87,76,65,56,48,38,28,19,13,8,5,1,0,0],
                      [100,90,78,68,58,50,40,29,21,14,9,5,2,0,0],
                      [100,93,81,70,61,53,42,31,22,15,10,6,2,0,0],
                      [100,97,84,73,63,55,44,32,23,16,11,7,3,0,0],
                      [100,100,87,76,66,57,46,34,25,18,12,8,4,0,0],
                      [100,100,90,79,69,60,49,36,27,19,13,9,4,1,0],
                      [100,100,92,80,70,61,50,37,28,20,14,9,5,1,0],
                      ]
 
        return tble
    def _RFDtable(self):
        #IGNITION COMPONENT(x) vs Wind spd (y)
        return [
                [  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0],
                [  3,  3,  3,  3,  3,  3,  3,  3,  4,  4,  4,  4,  4,  5,  5,  5,  6,  6],
                [  8,  8,  8,  8,  9,  9,  9,  9,  9, 10, 11, 11, 12, 13, 14, 14, 15, 16],
                [ 13, 14, 14, 14, 14, 14, 14, 15, 15, 16, 17, 18, 19, 21, 22, 25, 25, 25],
                [ 18, 19, 19, 19, 19, 20, 20, 20, 21, 22, 24, 25, 27, 29, 30, 32, 35, 35],
                [ 23, 24, 24, 24, 25, 25, 26, 26, 27, 29, 30, 32, 34, 36, 39, 42, 44, 46],
                [ 29, 29, 29, 30, 30, 31, 31, 32, 33, 35, 37, 39, 41, 44, 47, 51, 54, 57],
                [ 34, 34, 34, 35, 35, 36, 37, 38, 39, 41, 43, 46, 49, 52, 56, 60, 64, 68],
                [ 39, 39, 40, 40, 40, 41, 42, 43, 45, 47, 50, 53, 57, 60, 64, 69, 74, 79],
                [ 44, 44, 45, 45, 46, 47, 48, 49, 51, 54, 57, 60, 64, 68, 73, 78, 83, 89],
                [ 49, 49, 50, 51, 51, 52, 53, 55, 57, 60, 63, 67, 71, 76, 81, 87, 92, 97],
                [ 54, 55, 55, 56, 57, 58, 59, 60, 63, 66, 70, 74, 79, 84, 90, 96,100,100],
                [ 59, 60, 60, 61, 62, 63, 65, 66, 68, 72, 76, 81, 86, 92, 98,100,100,100],
                [ 64, 65, 66, 66, 68, 69, 70, 72, 74, 78, 83, 88, 94,100,100,100,100,100],
                [ 69, 70, 71, 72, 73, 74, 76, 77, 80, 85, 90, 95,100,100,100,100,100,100],
                [ 74, 75, 76, 77, 78, 80, 81, 83, 86, 91, 96,100,100,100,100,100,100,100],
                [ 79, 80, 81, 82, 84, 85, 89, 89, 92, 97,100,100,100,100,100,100,100,100],
                [ 85, 85, 86, 87, 89, 91, 92, 95, 98,100,100,100,100,100,100,100,100,100],
                [ 90, 91, 92, 93, 94, 96, 98,100,100,100,100,100,100,100,100,100,100,100],
                [ 95, 96, 97, 98,100,100,100,100,100,100,100,100,100,100,100,100,100,100],
                [100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100]

                 ]
    def _statecriteria(self):
        ####CONFIGURATION FOR EACH STATE
        ###DICTIONARY FORMAT
        ## DICT = {"STATE", [(catnumber,low,high), (catnumber,low,high)]}
        statedict = {"Kansas"       : [(0,0,30), (1,31,50), (2,51,70), (3,71,94), (4,95,100)],
                     "Colorado"     : [(0,0,30), (1,31,50), (2,51,70), (3,71,94), (4,95,100)],
                     "Nebraska"     : [(0,0,30), (1,31,50), (2,51,70), (3,71,94), (4,95,100)],
                     "Iowa"         : [(0,0,30), (1,31,50), (2,51,70), (3,71,94), (4,95,100)],
                     }
        return statedict

                 
    def _preciptthresh(self):
        #POP WHERE RFD WILL BE SET TO LOW
        #SET TO 101 if you want this disabled
        return 65
    
