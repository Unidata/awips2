from com.raytheon.uf.common.message.response import ResponseMessageGeneric
import BaseRequest

br = BaseRequest.BaseRequest( "airep" )
br.addParameter("dataTime","BBBBB:00.0",">=")
br.addParameter("dataTime","EEEEE:59.9","<=")

# ArrayList of ResponseMessageGeneric
alrmg = br.execute()
nnn = alrmg.size()
if nnn == 0 :
   return ResponseMessageGeneric("No data available.")

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
   myfwx = str(oneRec.getFlightWeather())
   if myfwx == "" or myfwx == "None" :
      myfwx = "-1"
   else :
      myfwx = "%d"%int(myfwx)
   myhaz = str(oneRec.getFlightHazard())
   if myhaz == "" or myhaz == "None" :
      myhaz = "-1"
   else :
      myhaz = "%d"%int(myhaz)
   mycond = str(oneRec.getFlightConditions())
   if mycond == "" or mycond == "None" :
      mycond = "-1"
   else :
      mycond = "%d"%int(mycond)
   msg += mylat + "|" + mylon + "," + mytime + "," + myflvl + ",AIREP,," + \
      mytemp + "," + mydir + "," + myspd + ",1e37," + \
      myfwx + "," + myhaz + "," + mycond + ",,0,,,,0,,,,,0,,,,\n"

# pass single RMG back to the uengine.
return ResponseMessageGeneric(msg)
