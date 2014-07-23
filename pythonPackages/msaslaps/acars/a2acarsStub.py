from com.raytheon.uf.common.message.response import ResponseMessageGeneric
import BaseRequest

br = BaseRequest.BaseRequest( "acars" )
br.addParameter("dataTime","BBBBB:00.0",">=")
br.addParameter("dataTime","EEEEE:59.9","<=")

# ArrayList of ResponseMessageGeneric
alrmg = br.execute()
nnn = alrmg.size()
if nnn == 0 :
   return ResponseMessageGeneric("No data available.")

# ResponseMessageGeneric payload is ACARSRecord
msg = ""
i = 0
while i < nnn :
   oneRec = alrmg.get(i).getContents()
   i += 1
   mytime = oneRec.getDataURI().split('/',4)[2].split('.',2)[0]
   if mytime == ""  or mytime == "None" :
      continue
   mytail = str(oneRec.getTailNumber()).split(' ',2)[0]
   myloc = str(oneRec.getLocation().getLocation()).split(' ',4)
   mylat = myloc[2].split(')',2)[0]
   mylon = myloc[1].split('(',2)[1]
   if mylat == "" or mylon == "" or mylat == "None" or mylon == "None" :
      continue
   mylat = "%.4f"%float(mylat)
   mylon = "%.4f"%float(mylon)
   myrec = str(oneRec.getReceiver()).split(' ',2)[0]
   if myrec == "None" :
      myrec = ""
   mypres = str(oneRec.getPressure())
   if mypres == "" or mypres == "None" :
      mypres = "1e37"
   else :
      mypres = "%.0f"%(float(mypres))
   myphs = str(oneRec.getFlightPhase())
   if myphs == "" or myphs == "None" :
      myphs = "7"
   else :
      myphs = "%d"%int(myphs)
   myrol = str(oneRec.getRollAngleQuality())
   if myrol == "" or myrol == "None" :
      myrol = "3"
   else :
      myrol = "%d"%int(myrol)
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
   myhum = str(oneRec.getHumidity())
   if myhum == "" or myhum == "None" :
      myhum = "1e37"
   else :
      myhum = "%.0f"%float(myhum)
   mymix = str(oneRec.getMixingRatio())
   if mymix == "" or mymix == "None" :
      mymix = "1e37"
   else :
      mymix = "%.2f"%float(mymix)
#  myicg = str(oneRec.getIcing())
#  if myicg == "" or myicg == "None" :
#     myicg = "1e37"
#  else :
#     myicg = "%d"%int(myicg)
   msg += mytail + "," + mytime + "," + mylat + "," + mylon + "," + \
      myrec + "," + mypres + "," + myphs + "," + myrol + "," + \
      mytemp + "," + mydir + "," + myspd + "," + myhum + "," + mymix + "\n"

# pass single RMG back to the uengine.
return ResponseMessageGeneric(msg)
