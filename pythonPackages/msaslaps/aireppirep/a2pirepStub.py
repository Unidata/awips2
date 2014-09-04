from com.raytheon.uf.common.message.response import ResponseMessageGeneric
import BaseRequest

br = BaseRequest.BaseRequest( "pirep" )
br.addParameter("dataTime","BBBBB:00.0",">=")
br.addParameter("dataTime","EEEEE:59.9","<=")

# ArrayList of ResponseMessageGeneric
alrmg = br.execute()
nnn = alrmg.size()
if nnn == 0 :
   return ResponseMessageGeneric("No data available.")
typecode = 'yes'
typecode = 'no'

# ResponseMessageGeneric payload
msg = "\n"
i = 0
while i < nnn :
   oneRec = alrmg.get(i).getContents()
   i += 1
   mytime = oneRec.getDataURI().split('/',4)[2].split('.',2)[0]
   if mytime == ""  or mytime == "None" :
      continue
   myloc = str(oneRec.getLocation().getLocation()).split(' ',4)
   mylat = myloc[2].split(')',2)[0]
   if mylat == "" or mylat == "None" :
      continue
   try:
       mylat = "%.4f"%float(mylat)
   except:
       continue
   mylon = myloc[1].split('(',2)[1]
   if mylon == "" or mylon == "None" :
      continue
   try:
       mylon = "%.4f"%float(mylon)
   except:
       continue
   myflvl = str(oneRec.getFlightLevel())
   if myflvl == "" or myflvl == "None" :
      myflvl = "1e37"
   else :
      myflvl = "%d"%int(myflvl)
   mytemp = str(oneRec.getTemp())
   if mytemp == "" or mytemp == "None" :
      mytemp = "1e37"
   else :
      mytemp = "%.1f"%float(mytemp)
   mydir = str(oneRec.getWindDirection())
   if mydir == "" or mydir == "None" :
      mydir = "1e37"
   else :
      mydir = "%d"%int(mydir)
   myspd = str(oneRec.getWindSpeed())
   if myspd == "" or myspd == "None" :
      myspd = "1e37"
   else :
      myspd = "%.1f"%float(myspd)
   myvis = str(oneRec.getHorzVisibility())
   if myvis == "" or myvis == "None" :
      myvis = "1e37"
   else :
      myvis = "%.1f"%float(myvis)
   mycraft = str(oneRec.getAircraftType())
   if mycraft == "None" :
      mycraft = ""
   mywx = str(oneRec.getWeatherGroup())
   if mywx == "None" :
      mywx = ""
   cc = 0
   cldBas = ""
   cldTop = ""
   cldVal = ""
   ii = 0
   icgBas = ""
   icgTop = ""
   icgTyp = ""
   icgVal = ""
   tt = 0
   trbBas = ""
   trbTop = ""
   trbTyp = ""
   trbVal = ""
   for pld in oneRec.getAncPirepData().toArray():
       sep = ""
       ltyp = pld.getLayerType()
       if ltyp == "" or ltyp == "None" :
          continue
       base = str(pld.getBaseLayerHeight())
       if base == "" or base == "None" or base == "99999" :
          base = "1e37"
       else :
          base = "%.0f"%float(base)
       top = str(pld.getTopLayerHeight())
       if top == "" or top == "None"  or top == "99999" :
          top = "1e37"
       else :
          top = "%.0f"%float(top)
       dtyp = str(pld.getDataType())
       if dtyp == "None" :
          dtyp = ""
       fval = str(pld.getFirstValue())
       if fval ==  "None" :
          fval = ""
       sval = str(pld.getSecondValue())
       if sval ==  "None" :
          sval = ""
       if ltyp == "CLOUD" :
          if fval == "TOP" :
             fval = ""
          if sval == "TOP" :
             sval = ""
          if sval != "" :
             fval += "-"+sval
          if typecode == 'yes' :
             if fval == "CLR" :
                fval = "0"
             elif fval == "OVC" :
                fval = "8"
             elif fval == "SCT" :
                fval = "11"
             elif fval == "BKN" :
                fval = "12"
             elif fval == "FEW" :
                fval = "13"
             else :
                continue
          if cldBas != "" :
             sep = "|"
          cldBas += sep+base
          cldTop += sep+top
          cldVal += sep+fval
          cc += 1
       elif ltyp == "ICING" :
          if sval != "" :
             fval += "-"+sval
          if icgBas != "" :
             sep = "|"
          if typecode == 'yes' :
             if dtyp == "RIME" :
                dtyp = "1"
             elif dtyp == "CLR" :
                dtyp = "2"
             elif dtyp == "MXD" :
                dtyp = "3"
             else :
                dtyp = "-9999"
             if fval == "NEG" :
                fval = "0";
             elif fval == "TRACE" :
                fval = "1"
             elif fval == "TRACE-LGT" :
                fval = "2"
             elif fval == "LGT" :
                fval = "3"
             elif fval == "LGT-MOD" :
                fval = "4"
             elif fval == "MOD" :
                fval = "5"
             elif fval == "MOD-SEV" :
                fval = "7"
             elif fval == "SEV" :
                fval = "8"
             else :
                fval = "-9999"
             if fval == "-9999" and dtyp == "-9999" :
                continue
          icgBas += sep+base
          icgTop += sep+top
          icgTyp += sep+dtyp
          icgVal += sep+fval
          ii += 1
       elif ltyp == "TURBC" :
          if sval != "" :
             fval += "-"+sval
          if typecode == 'yes' :
             if dtyp == "CAT" :
                dtyp = "1"
             elif dtyp == "CHOP" :
                dtyp = "2"
             else :
                dtyp = "-9999"
             if fval == "NEG" :
                fval = "0";
             elif fval == "LGT" :
                fval = "2"
             elif fval == "LGT-MOD" :
                fval = "3"
             elif fval == "MOD" :
                fval = "4"
             elif fval == "MOD-SEV" :
                fval = "5"
             elif fval == "SEV" :
                fval = "6"
             elif fval == "EXTRM" :
                fval = "8"
             else :
                fval = "-9999"
             if fval == "-9999" and dtyp == "-9999" :
                continue
          if trbBas != "" :
             sep = "|"
          trbBas += sep+base
          trbTop += sep+top
          trbTyp += sep+dtyp
          trbVal += sep+fval
          tt += 1
   msg += mylat + "|" + mylon + "," + mytime + "," + myflvl + ",PIREP," + \
      mycraft + "," + mytemp + "," + mydir + "," + myspd + "," + \
      myvis + ",-1,-1,-1," + mywx + "," + \
      str(cc) + "," + cldBas + "," + cldTop + "," + cldVal + "," + \
      str(ii) + "," + icgBas + "," + icgTop + "," + \
      icgTyp + "," + icgVal + "," + \
      str(tt) + "," + trbBas + "," + trbTop + "," + \
      trbTyp + "," + trbVal + "\n"

# pass single RMG back to the uengine.
return ResponseMessageGeneric(msg)
