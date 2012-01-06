      subroutine HAILCAST1 (To, Tdo, ebs, hvars, mumixr)
c  6/14/07 - Added ebs (/s) to determine storm type / updraft longevity
c  additions by ryan commented with "rej"
c  12/2007 - Added functionality to run on elevated soundings  
c  4/10 - Added new calibration based on mumixr bins
     
      implicit none

      real To, Tdo, T, Td, dT, Toff, ebs
      real D, Dav, Dmax, Dmin, Dcont, Daloft
      real DavC,davcm,sig,svr,davcb,thetae,es,qs,q,e,sh1,sh2,sh3
      real embryo, mumixr,WBASVU,bias,bias2,esicat
      real UPMAXV, UPMAXT,temp,dew,tlcl,thetaemax,plplmax,eqtemp,rh, 
     * ZBAS, TBAS, TOPT,TOPZ, CAPE, ESI, ESIMAX,tlpl,dlpl,sfc
      character*8 cdate
      real sounding(100,7), subcloud(50,7)
      integer idate,IC, report, i, nofroze, unitnumber, itel,elevated,
     * lplcount, which, version
      parameter(Toff = 1.0, dT = 0.5)
      character*60 fcst
      CHARACTER*72 filename, filename1
      real hvars(*)
 
      idate = 02000000
      hvars (1) = To
      hvars (2) = Tdo
      
c Low end limit of 10 kts for ebs (normalized to 6000 meters)
      if(ebs.lt.0.0008) ebs = 0.0008
              
c      CONVERT INTEGER FILE NAME TO CHARACTER EQUIVALENT
       write (cdate,'(i8.8)') idate

      D = 0.0
      Dav = 0.0
      DavC = 0.0
      Dmax = 0.0
      Dmin = 100000.0
      Dcont = 0.0
      Daloft = 0
      IC = 0
      sig = 0
      svr = 0
      ESIMAX = 0
      esicat = 0
      elevated = 0
      which = 1
      bias = 0
      version = 1
********************************* TOP BUN ************************************
*********************** ELevated Sounding Sandwich     ***********************
******************************************************************************

      thetaemax = -99
c      open(unit=1, file=cdate)
***   OPEN SOURCE DATA FILE      
      write(filename,4) cdate
      unitnumber=1
      open(unit=1, file=filename)
4     format("/tmp/",a8,".dat")

***   Pres = itel,2   Hght = itel,1   TEMP = itel,4   DEW = itel,5   DDD= itel,6 
***   Speed=itel,7 

        DO 109 itel=1,100
	
        READ(1,*, END=159) sounding(itel,2), sounding(itel,1),
     *  sounding(itel,4), sounding(itel, 5),
     *  sounding(itel,6), sounding(itel, 7) 
        sounding(itel,3)=0.0	

	if(itel.eq.1) sfc = sounding(itel,2)

	temp = sounding(itel,4)
	dew = sounding(itel,5)

        if(sounding(itel,2).ge.500) then
c Calculate LCL Temperature (C) Using Equation from Barnes 1968
	tlcl = dew - (0.001296*dew + 0.1963)*(temp - dew)
c Convert temp at LCL to kelvin	
	tlcl = tlcl + 273.15
c Calculate pressure of the LCL based on parcel at level (itel,2)
c	plcl = sounding(itel,2)*(tlcl/(temp+273.15))**(3.5)
c Calculate thetae at each level
c	theta = tlcl*(1000/plcl)**(.2857)
	es = 6.1*exp(0.073*(temp))
	qs = 0.622*(es/sounding(itel,2))
	rh = exp(0.073*(dew-temp))
	e = rh*es	
	q = (e*287.04)/(sounding(itel,2)*461.55)
        eqtemp = (temp+273.15)*(1 + (2.5E6*q)/(1004*tlcl))
	thetae = eqtemp*(1000/sounding(itel,2))**(0.2859)
	
c If maximum thetae, define level
      if(thetae.gt.thetaemax) then
	thetaemax = thetae
	plplmax = sounding(itel,2)
	tlpl = sounding(itel,4)
	dlpl = sounding(itel,5)
	lplcount = itel
	endif	

c	print *, 'thetae =',thetae,'  press =',sounding(itel,2)

      endif
	
109    CONTINUE
159    itel=itel-1

c rej If sounding does not extend to 300 mb, kill it
	if(sounding(itel,2).ge.300) then
	print *, '**** Sounding does not extend to 300 mb ****'
	print *, 'Highest level = ',sounding(itel,2),'mb'
	d = 0
	goto 9999
 	endif
	
	rewind(1)

C Automatically set parcel Temp and Dew depending upon LPL
      if(plplmax.lt.sounding(1,2)) then
	To = tlpl
	Tdo = dlpl
	elevated = 1
	print *, 'Elevated Parcel lifted from ',plplmax,'mb'
c Now read subcloud layer into new array for melting algorithm
        DO 77 itel=1,lplcount
	
        READ(1,*, END=179) subcloud(itel,2), subcloud(itel,1),
     *  subcloud(itel,4), subcloud(itel, 5),
     *  subcloud(itel,6), subcloud(itel, 7) 
        subcloud(itel,3)=0.0	

      
77     CONTINUE
179    itel=itel-1

	else
	To = sounding(1,4)
	Tdo = sounding (1,5)
      endif

	
c  Adjust parcel T and Td if they are too close to each other
        if(Tdo+1.gt.To-1) to=to+((Tdo+1)-(To-1))+0.01
        if(Tdo+1.eq.To-1) to=to + 0.01      
        hvars (1) = To
        hvars (2) = Tdo
	
c	print *, 'Level of most unstable parcel is ',plplmax,'mb'
c	print *, 'MUPARCEL: T= ',tlpl, 'Td= ',dlpl
c	print *, 'Thetae at the MULCL is ',thetaemax,'K'
	
	
        close(1)
	
******************************************************************************
******************************************************************************
*****************************  Bottom Bun   **********************************
      
c REJ 4/10 edits...choose hail model parameters based on mumixr bin
      if(mumixr.lt.11) then
c  If which = 0, use hail model average, if which = 1 (default) use max
      sh1 = 5
      sh2 = 5
      sh3 = 9
      embryo = 1500
      WBASVU = 8
      endif
      
      if(mumixr.ge.11.and.mumixr.lt.14) then
      which = 0
      sh1 = 3
      sh2 = 6
      sh3 = 6
      embryo = 300
      WBASVU = 8
      bias = 1.00
      endif

      if(mumixr.ge.14.and.mumixr.lt.17) then
      sh1 = 0
      sh2 = 6
      sh3 = 9
      embryo = 400
      WBASVU = 6
      endif

      if(mumixr.ge.17) then
      sh1 = 0
      sh2 = 2.5
      sh3 = 9
      embryo = 100
      WBASVU = 5
      endif
    
c Run hail model the second time for best category version
777   if(version.eq.2) then
      which = 1
      D = 0.0
      Dav = 0.0
      DavC = 0.0
      Dmax = 0.0
      Dmin = 100000.0
      IC = 0
      sig = 0
      svr = 0
      ESIMAX = 0
      esicat = 0

      sh1 = 1
      sh2 = 3
      sh3 = 5
      embryo = 700
      WBASVU = 15
      bias2 = -0.05
      endif
      
      do T = To - Toff, To + Toff, dT
         do Td = Tdo - Toff, Tdo + Toff, dT
            call hailcloud (T, Td, cdate, ebs,elevated,lplcount,sh1,
     *                      sh2,sh3,WBASVU, esicat)
	    call hailstone (UPMAXV, UPMAXT, D, ZBAS, TBAS, TOPT,
     *      TOPZ, CAPE, cdate, nofroze, Daloft, ebs,elevated,lplcount,
     *      subcloud,embryo)   

c          convert cm to inches      Dav = 0.0
            D=D/2.54
            if(d.ge.1.95) sig=sig+1
            if(d.ge.1.00) svr=svr+1
            ESI=(CAPE*ebs)
c rej parcel must move faster than 7 m/s and embryo must freeze
	    if((UPMAXV .gt. 7.0).and.(nofroze.eq.1)) then
		DavC = DavC + D
		IC= IC + 1
	    endif
	    
            if(D. gt. Dmax) Dmax = D
            if(D. lt. Dmin) Dmin = D
	    if(esi.gt.esimax) esimax=esi
	    enddo
           enddo
      
      if(version.eq.2) goto 778

      hvars (3) = ic
      
      if (ic.le.0) then
       davc=0 
       else
       DavC = DavC / IC
      endif

      hvars (4) = davc
      hvars (5) = dmax
      hvars (6) = dmin
      if(hvars(6).gt.1000) hvars(6) = 0 
      hvars (7) = sig
      hvars (8) = svr
      
c  rej  best fit for davc based on reported hail size
      if(davc.eq.0) then
      davcm=0
      report=1
      fcst =  'No hail produced'
      elseif((davc.lt..9).and.(davc.gt.0)) then 
      davcm=davc+.2 
      report=2
      fcst =   'Dime to Quarter most likely, isolated Golfball'
      elseif((davc.ge..9).and.(davc.le.1.4)) then 
      davcm=davc+.1 
      report=3
      fcst = 'A few golfballs possible'
      elseif((davc.gt.1.4).and.(davc.le.2.2)) then 
      davcm=davc+.4 
      report=4
      fcst = 'Tennis / Baseballs possible'
      elseif(davc.gt.2.2) then 
      davcm=davc+.7
      report=5
      fcst = 'Baseballs or Larger'
      endif   
      
      hvars (9) = davcm
      hvars (10) = report
      
      davcb=davc+.6
      
      hvars (11) = davcb
      hvars (12) = esimax
      
c rej convert daloft to inches      
      daloft = daloft/2.54
      
      hvars (13) = daloft
      
      if(which.eq.0) then
      hvars(14) = davc + bias
      else
      hvars(14) = dmax
      endif 

      hvars (15) = which
      hvars (16) = esicat
     
      version = 2
      goto 777

     
778   hvars (17) = ic

      if (ic.le.0) then
       davc=0 
       else
       DavC = DavC / IC
      endif 

      hvars (18) = davc
      hvars (19) = dmax
      hvars (20) = dmin
      if(hvars(20).gt.1000) hvars(20) = 0
      hvars (21) = sig
      hvars (22) = svr


      if(dmax.ge.0.05) then
      hvars(23) = dmax + bias2
      else
      hvars(23) = dmax
      endif

      hvars (24) = esicat


c       do 9345 i=1, 25
c    	   print *, i,'=', hvars(i)
c 9345   continue

c 	print *, ''
c	print *, '1=TEMP 2=DEW 3 = Convecting Member (of 25)'
c	print *, '4= Average Size 5= MaxSize 6= MinSize'
c	print *, '7= # SIG HAIL 8= # SVR HAIL'
c	
c	print *, 'SH1 SH2 SH3 MUMIXR EMBRYO WBASVU'
c	print *, sh1,' ',sh2,' ',sh3,' ',mumixr,' ',embryo,' ',WBASVU
       
 
9999   continue
       return 
       end




















      subroutine hailcloud(Tinput, Tdinput, cdate, ebs, elevated,
     *lplcount,sh1,sh2,sh3,WBASVU,esicat)

***************************************************************** 
*** SKYWATCH ONE-DIMENSIONAL CLOUD MODEL 
*** 
*** THE MODEL IS BASED ON THE PARCEL METHOD, BUT ALLOWS FOR WATER 
*** LOADING AND ENTRAINMENT. THE SURFACE TEMP AND DEWPOINT ARE  
*** USED TO LIFT THE PARCEL TO ITS LCL 
***************************************************************** 
 
**************************************************************** 
*** CODE TRANSLATED INTO ENGLISH BY J.C. BRIMELOW, JUNE 2002 
**************************************************************** 

      COMMON /DATA/ TFI(100,7),WOLKDTA(100,10),
     *              TCA(100),R(100)
      CHARACTER*72 filename, filename1
      character*8 cdate	 
      real Tinput, Tdinput, ebs, sounding(100,7),sh1,sh2,sh3,WBASVU
      real esicat
      integer unitnumber, parcel, elevated, lplcount
            
************************************************* 
***  CLOUD MODEL PARAMETERS: 
************************************************* 
 
***sounding(ITEL,1) = HEIGHT 
***sounding(ITEL,2) = PRESSURE 
***sounding(ITEL,4) = TEMPERATURE 
***sounding(ITEL,5) = DEWPOINT 
***sounding(ITEL,6) = WIND DIRECTION 
***sounding(ITEL,7) = WIND SPEED 
 
***To/TMAX = CONTROL TEMP 
***Tdo/TDOU = CONTROL DEWPOINT 
***CDATE = DATE USED AS IDENTIFIER FOR INPUT AND OUTPUT FILE 
***OPPDRUK = SURFACE PRESSURE 
 
***TFI= ARRAY FOR TFI DATA 
***sounding = ARRAY INTO WHICH INPUT SOUNDING DATA IS READ 
***WOLKDTA = ARRAY FOR CLOUD MODEL OUTPUT 
 
***R =  MIXING RATIO 
***RS = SATURATION MIXING RATIO 
***DIGTA = AIR DENSITY 
***CLWATER = CONDENSED CLOUD WATER 
***TCVIR = VIRTUAL PARCEL TEMPERATURE 
***VU =  UPDRAFT VELOCITY 
 
***WOLK= CLOUD 
***WOLKBAS = CLOUD BASE 
***WBASP = CLOUD BASE PRESSURE 
***WBASTMP= CLOUD BASE TEMP 
***WBASVU= UPDRAFT VELOCITY AT CLOUD BASE 
***WBASRS = SATURATION MIXING RATIO AT CLOUD BASE 
 
***WMAX =  MAX. UPDRAFT VELOCITY 
***BETA = BETTS' ENTRAINMENT PARAMETER 
***PSEUDO = PSEUDOADIABAT (IN DEGREES K) 
 
***ITIPWLK = CLOUD TYPE WHERE, 
***0 = CUMULUS OR NIL 
***1-3 = AIR-MASS THUNDERSTORM 
***3-5 = MULTI-CELL 
***5+ = SUPERCELL 
*** 
*************************************************** 
 
***  CONVERT TMAX (Tinput) AND COINCIDENT DEW-POINT  
***  (Tdinput) TO KELVIN     
   
      TMAX = Tinput + 273.16
      TDOU = Tdinput + 273.16

***   OPEN SOURCE DATA FILE      
      write(filename,4) cdate
      unitnumber=1
      open(unit=1, file=filename)
4     format("/tmp/",a8,".dat")

**   OPEN OUTPUT DATA FILE TO BE USED BY HAIL MODEL
      write(filename1,5) cdate          
      unitnumber=2
      open(unit=2, file=filename1)
5     format("/tmp/c",a8,".dat")
    
    
*** ORDER OF READ STATEMENT MAY HAVE TO BE CHANGED DEPENDING ON 
***   Pres = itel,2   Hght = itel,1   TEMP = itel,4   DEW = itel,5   
***   DDD= itel,6   Speed=itel,7 
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
*** READ DATA FROM INPUT UPPER-AIR FILE. MAX FILE LENGTH 100 LINES 

	if(elevated.eq.1) then
C Only read data from MULCL level up	
	DO 10 itel=1,100
        READ(1,*, END=15) sounding(itel,2), sounding(itel,1),
     *  sounding(itel,4), sounding(itel, 5),
     *  sounding(itel,6), sounding(itel, 7) 
        sounding(itel,3)=0.0
	
c REJ  if dewpoint value off sounding is less than -90, set to -90 	
c REJ  if temperature is less than -85, set to -85
c REJ  This fixes NSHARP missing data flag, which makes value -9999.00
	   if (sounding(itel,5).lt.-90) then
	    sounding(itel,5)=-90
	    endif
	   if (sounding(itel,4).lt.-85) then
	     sounding(itel,4)=-85
	    endif 

*** CONVERT TEMPERATURE AND DEWPOINT TO KELVIN
           sounding(itel,4)=sounding(itel,4) + 273.16
           sounding(itel,5)=sounding(itel,5) + 273.16


10    CONTINUE

15    itel=itel-1


C If sounding is elevated, then shift array values down so that muparcel
C Level will now be the surface (1st level)
      DO 16 itel = 1,101-lplcount
      sounding(itel,1) = sounding(itel + lplcount-1,1)
      sounding(itel,2) = sounding(itel + lplcount-1,2)
      sounding(itel,3) = sounding(itel + lplcount-1,3)
      sounding(itel,4) = sounding(itel + lplcount-1,4)
      sounding(itel,5) = sounding(itel + lplcount-1,5)
      sounding(itel,6) = sounding(itel + lplcount-1,6)
      sounding(itel,7) = sounding(itel + lplcount-1,7)
 
       
16    CONTINUE       
      
      
c      print *, sounding(1,2)
c      print *, sounding(2,2)
c      print *, sounding(3,2)
c      print *, sounding(4,2)
c      print *, sounding(5,2)
c      print *, sounding(6,2)
c      print *, sounding(7,2)
c      print *, sounding(8,2)
c      print *, sounding(9,2)
c      print *, sounding(10,2)
      

      endif

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C Or Read from Surface up	
       if(elevated.eq.0) then
        DO 190 itel=1,100

        READ(1,*, END=195) sounding(itel,2), sounding(itel,1),
     *  sounding(itel,4), sounding(itel, 5),
     *  sounding(itel,6), sounding(itel, 7) 
        sounding(itel,3)=0.0
	

c REJ  if dewpoint value off sounding is less than -90, set to -90 	
c REJ  if temperature is less than -85, set to -85
c REJ  This fixes NSHARP missing data flag, which makes value -9999.00

	   if (sounding(itel,5).lt.-90) then
	    sounding(itel,5)=-90
	    endif
	   if (sounding(itel,4).lt.-85) then
	     sounding(itel,4)=-85
	    endif 

*** CONVERT TEMPERATURE AND DEWPOINT TO KELVIN
           sounding(itel,4)=sounding(itel,4) + 273.16
           sounding(itel,5)=sounding(itel,5) + 273.16

     

190    CONTINUE

195    itel=itel-1

    

      endif

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC




*** INTERPOLATE A HIGHER RESOLUTION SOUNDING 
      CALL INTSOUN(TFI,sounding,ITEL,JTEL,OPPDRUK)
      

*** CALC THE CLOUD BASE PARAMETERS
      CALL WOLKBAS(TFI,JTEL,WBASP,WBASRS,WBASTMP,WOLKDTA,TMAX,
     *             TDOU,OPPDRUK)
     

*** DETERMINE THE TYPE OF CLOUD USING CAPE AND VERTICAL WIND SHEAR 
*** I.E. ESI = ebs*CAPE 
      CALL WOLKSRT(sounding,TFI,WBASP,WBASTMP,ITIPWLK,JTEL,CAPE
     *             ,ebs,RICH,PSEUDO,ITEL,sh1,sh2,sh3,esicat)
     
     
*** CALC THE PARCEL'S TEMPERATURE, AFTER INCL. ENTRAINMENT FROM  
*** CLOUD BASE TO 300 HPA 
      CALL NATAD(WBASP,WBASTMP,WBASRS,TFI,JTEL,NTRAIN,WOLKDTA,
     *           ITIPWLK,WMAX, BETA, WBASVU)



*** WRITE OUT THE CONVECTIVE, KINEMATIC AND CLOUD DATA TO  
*** c*.dat FILE

      WRITE(2,'(''*************   SP-CLOUD MODEL   **************'')')
      IF(NTRAIN.EQ.1)WRITE(2,*) 'CLOUDTOP ENTRAINMENT' 
      IF(NTRAIN.EQ.2)WRITE(2,*) 'LATERAL ENTRAINMENT'
      WRITE(2,'(''MAX TEMP    MAX DEW-POINT'')')
      WRITE(2,'(3X,F5.1,9X,F5.1)')TMAX-273.16,TDOU-273.16
      WRITE(2,
     *'('' CAPE    E.SHEAR(/s) TYPECLD PSEUDO'')')
      WRITE(2,'(F8.1,3X,F6.3,5X,I2,6X,F6.1)')
     *               CAPE,ebs,ITIPWLK,PSEUDO
      WRITE(2,
     *'(''  P       CP    TA    TD    TC    RS    VU    HGHT'')')


*** WRITE THE CLOUD DATA TO THE c*.dat FILE 
      DO 200 I=1,JTEL+1
      WRITE(2,
     * '(F5.0,F8.1,4F6.1,F7.1,F8.1, F6.1)')(WOLKDTA(I,KK),KK=1,8)
         if(WOLKDTA(I,7) .gt. wmaxim) then
            wmaxim = WOLKDTA(I,7)
         endif 
         if(WOLKDTA(I,7).lt.0.0) GOTO 666
	 
200   CONTINUE

666   continue


      close(unit=1)
      close(unit=2)
 
      return
      end



















 
      SUBROUTINE INTSOUN(TFI,sounding,ITEL,JTEL,OPPDRUK)
****************************************************************
*** INTERPOLATE A HIGHER RESOLUTION SOUNDING 
****************************************************************
      DIMENSION ISTNAAM(10),ISTHGTE(10),STDRUK(10)
      DIMENSION TFI(100,7),sounding(100,7)
      
      I=1 
      JTEL=1
      IHGTJA=0
      
	
*** FIND THE STATION HEIGHT FROM THE INPUT SOUNDING
           TFI(1,5)=sounding(1,1)
           OPPDRUK=sounding(1,2)*100
	   
           IHGTJA=1
	   
100   CONTINUE

      IPRES=10*(sounding(1,2)/10)
      PVLAK=IPRES

*** SEARCH FOR THE TWO LEVELS EACH SIDE OF P 
12    IF(I.LT.ITEL.AND.JTEL.LT.90)THEN

10      IF(PVLAK.LE.sounding(I,2).AND.PVLAK.GT.sounding(I+1,2).
     *                        AND.sounding(I+1,2).NE.0.0)THEN
          PDIFF=sounding(I,2)-sounding(I+1,2)
          VDIFF=sounding(I,2)-PVLAK
          VERH=VDIFF/PDIFF
          TDIFF=sounding(I+1,4)-sounding(I,4)
          TDDIFF=sounding(I+1,5)-sounding(I,5)
          TFI(JTEL,1)=PVLAK
          TFI(JTEL,3)=sounding(I,4)+(TDIFF*VERH)
          IF(sounding(I+1,4).GE.350.0)THEN
            TFI(JTEL,4)=sounding(I+1,5)
          ELSE
            TFI(JTEL,4)=sounding(I,5)+(TDDIFF*VERH)
          ENDIF
          IF(PVLAK.EQ.sounding(I,2))THEN 
            TFI(JTEL,2)=sounding(I,3)
            TFI(JTEL,4)=sounding(I,5)
          ELSE
            TFI(JTEL,2)=0.0
          ENDIF
          IF(IHGTJA.EQ.1)THEN 
            TFI(JTEL+1,5)=(287.04*(TFI(JTEL,3)+1.0)/(9.78956
     *               *TFI(JTEL,1)*100.0)) * 2500.0 + TFI(JTEL,5)
          ELSE
            TFI(JTEL+1,5)=0.0 
          ENDIF
          JTEL=JTEL+1
          PVLAK=PVLAK-25.0
        ELSE
          I=I+1
          GOTO 12
        ENDIF
***********
        GOTO 12
      ENDIF
      IF(sounding(ITEL,2).LE.10.0)THEN
        TFI(JTEL-1,4)=sounding(ITEL-1,4) 
        TFI(JTEL-1,5)=sounding(ITEL-1,5) 
      ENDIF
      JTEL=JTEL-1
      RETURN
      END 




      FUNCTION XINTERP(TFI,P,JVAL,JTEL) 
****************************************************************
***      INTERPOLATE BETWEEN TWO LEVELS
****************************************************************
      DIMENSION TFI(100,7)
*** SOEK DIE 2 VLAKKE WEERSKANTE VAN P
      DO 20 I=1,JTEL-1
        IF(P.LT.TFI(I,1).AND.P.GE.TFI(I+1,1))THEN 
          PDIFF=TFI(I,1)-TFI(I+1,1)
          VDIFF=TFI(I,1)-P
          VERH=VDIFF/PDIFF
          ADIFF=TFI(I+1,JVAL)-TFI(I,JVAL)
          IF(ADIFF.LT.0.0)ADIFF=-1.0*ADIFF
          IF(TFI(I+1,JVAL).LT.TFI(I,JVAL))THEN
            XINTERP=TFI(I,JVAL)-(ADIFF*VERH)
          ELSE
            XINTERP=TFI(I,JVAL)+(ADIFF*VERH)
          ENDIF
        ENDIF
20    CONTINUE
      RETURN
      END 


      FUNCTION XNATV(TK,P,DP) 
*******************************************************************
*** CALC THE TEMP AT THE LEVEL DP PASCAL HIGHER AS P ALONG THE WALR 
*******************************************************************
      CP=1004.64
      RD=287.04
      RV=461.48
      EPS=0.622
      
*** CALCULATE THE LATENT HEAT RELEASE IN PARCEL DUE TO CONDENSATION 
*** OF CLOUD WATER 
      IF(TK.LT.233.16)THEN
        AL=(-0.566*(TK-273.16)+1200.0)*4186.0
      ELSE
        AL=(-0.566*(TK-273.16)+597.3)*4186.0
      ENDIF
      TB=TK
      
*** CALC DIFFERENT PARTS OF THE EQUATION
      XP=611.0*EXP((AL/RV)*(1.0/273.16-1.0/TB))
      A=1.0+(AL*EPS*XP/(RD*TB*P))
      B=1.0+(EPS*EPS*AL*AL*XP/(CP*RD*P*TB*TB))
      
*** CALC THE TEMP AT THE NEXT LEVEL BY FOLLOWING THE WALR
      XNATV=TB-(A/B)*((RD*TB*DP)/(P*CP))
      RETURN
      END 


      FUNCTION DATV(T1,P,DP)
*******************************************************************
*** CALC THE TEMP AT THE NEXT LEVEL WHICH IS DP PASCAL HIGHER THAN 
*** P. PARCELTEMP DECREASES ALONG THE DALR 
*******************************************************************
      CP=1004.64
      RD=287.04
      DATV=T1-RD*T1/(CP*P)*DP 
      
      RETURN
      END 















      SUBROUTINE SP(TE,TD,P,T1,P1)
*******************************************************************
*** CALCULATE THE PRESSURE AND TEMP AT THE SATURATION POINT (SP) 
*** - SP IS WHERE THE SURFACE AIR BECOMES SATURATED BY LIFTING IT 
*** ADIABATICALLY FROM THE SURFACE 
*******************************************************************
      CP=1004.64
      RD=287.04
      RV=461.48
      EPS=0.622
      
*** CALCUATE THE MIXING RATIO USING THE CLAUSSIUS CLAPEYRON EQUATION 
*** R(AT TE)= RS(AT TD)
      DP=500.0
      AL=(-0.566*(TD-273.16)+597.3)*4186.0
      E=611.0*EXP((AL/RV)*(1.0/273.16-1.0/TD))
      R=EPS*E/P
      P1=P
      T1=TE
      
*** CALC T ALONG THE DALR UNTIL RS=R I.E., T=TD, THE SATURATION POINT 
*** OR LCL
      DO 10 I=1,100 
      
*** CALCULATE THE SATURATION MIXING RATIO FOR TE 
        AL=(-0.566*(TE-273.16)+597.3)*4186.0
        ES=611.0*EXP((AL/RV)*(1.0/273.16-1.0/TE)) 
        RS=EPS*ES/P1
        IF(RS.LE.R)GOTO 15
	
*** CALCULATE THE TEMP AT THE NEXT LEVEL ALONG THE DALR
        T1=TE-RD*TE/(CP*P1)*DP
        TE=T1
        P1=P1-DP
	
10    CONTINUE
15    RETURN
      END 















      SUBROUTINE UPDRAFT(WBASRS,DR,TK,RS,VU,TFI,JTEL,ITIPWLK,CLWATER,
     * LOADING, WBASP)
*******************************************************************
*** CALCULATE THE UPDRAFT VELOCITY DUE TO BUOYANCY 
*******************************************************************
      DIMENSION TFI(100,7)

      RD=287.04
      G=9.78956
      DP=50.0
      TC=TK
      P=DR/100.0
      TA=XINTERP(TFI,P,3,JTEL)
c rej is this missing?   TD=XINTERP(TFI, P, 4, JTEL)

*** BEREKEN LUGDIGTHEID IN KD/M3
      DIGTA=(P*100.0/(RD*(1.0+0.609*RS/(1.0+RS))*(TA)))
     
*** CALC THE Z-INCREMENT USING THE HYDROSTATIC EQUATION
      DELZ=-100.0*(-DP)/(DIGTA*G)
      
*** CALC THE TOTAL WATER CONTENT AT LEVEL P
       CLWATER=WBASRS-RS

*** CALC VIRTUAL TEMP OF THE AMBIENT AIR. 
      TAKELV=TA
      VIRT=(1.0+0.608*(RS/(1.0+RS)))
      TAVIR=VIRT*TAKELV

*** CALC THE VIRTUAL TEMP. OF THE CLOUD/PARCEL 
c rej is this missing?   VIRTA=(1.0+0.608*(R/(1.0+R)))
      TCKELV=TC
      TCVIR=VIRT*TCKELV

*** CALCULATE THE UPDRAFT VELOCITY 
      A=VU*VU+2.0*G*DELZ*((TCVIR-TAVIR)/TAVIR)
      B=-2.0*G*CLWATER*DELZ	
      VOLGVU=SQRT(ABS(A+B))
      IF(A+B.LT.0.0)VOLGVU=-1.0*VOLGVU
      VU=VOLGVU
      RETURN
      END 




      FUNCTION XINTWLK(P1,P2,PTFI,WAARDE1,WAARDE2)
*******************************************************************
*** INTERPOLATE CLOUD DATA FOR LEVEL P LOCATED BETWEEN 2 WET ADIABTIC 
*** LEVELS
*******************************************************************
*** SEARCH FOR THE TWO LEVELS EACH SIDE OF P

      IF(PTFI.LT.P1.AND.PTFI.GE.P2)THEN 
        PDIFF=P1-P2 
        VDIFF=P1-PTFI
        VERH=VDIFF/PDIFF
        VERH=(EXP(VERH)-1.0)/1.71828
        ADIFF=WAARDE1-WAARDE2 
        XINTWLK=WAARDE1-(ADIFF*VERH)
      ENDIF
      RETURN
      END
























 
      SUBROUTINE NATAD(WBASP,WBASTMP,WBASRS,TFI,JTEL,NTRAIN,WOLKDTA,
     *                 ITIPWLK,WMAX, BETA, WBASVU)
*******************************************************************
*** CALC THE PARCEL'S TEMPERATURE AND MIXING RATIO. THE UPDRAFT VELOCITY 
*** IS THEN DETERMINED BY CALCULATING THE DIFFERENC EETWEEN THE PARCEL 
*** TEMP AND THAT OF THE AMBIENT AIR 
*******************************************************************
      DIMENSION TFI(100,7),WOLKDTA(100,10)
      CP=1004.64
      RD=287.04
      RV=461.48
      EPS=0.622
      DP=5000.0
      G=9.78956
      BL=2464759.0
      
***   UPDRAFT VELOCITY AT CLOUD BASE IN M/S      
c      WBASVU=4.0
      
*** SET THE ENTRAINMENT PARAMETER ACCORDING TO THE TYPE OF CLOUD 
*** HERE 0.1 EQUALS 10% ENTRAINMENT AND 0.075 7.5% ENTRAINMENT 
      IF(ITIPWLK.EQ.0)BETA=0.1
      IF(ITIPWLK.EQ.1)BETA=0.1
      IF(ITIPWLK.EQ.2)BETA=0.075
      IF(ITIPWLK.EQ.3)BETA=0.050
	

*** SET THE TYPE OF ENTRAINMENT: CLOUDTOP FOR CUMULONIMBUS (TYPE 1-4) 
*** AND LATERAL FOR TYPE 0
      IF(ITIPWLK.GE.1)THEN
        NTRAIN=1
      ELSE
        NTRAIN=2
      ENDIF

*** SPECIFY THE INITIAL CLOUD PARAMETER VALUES AT CLOUD BASE 
      TK=WBASTMP
      VU=WBASVU
      RS=WBASRS
      CSPT=WBASTMP
      CSPP=WBASP
      VORIGP=WBASP
      VORIGTK=WBASTMP
      VORIGRS=WBASRS
      VORIGVU=WBASVU
      P=WBASP-DP
      T=WBASTMP
      WMAX=VU

*** FIND THE CLOUD BASE 
      DO 100 J=1,100
        WOLKDTA(J,7)=WBASVU
        IF(WBASP.GT.TFI(J,1)*100.0)GOTO 20
100   CONTINUE

*** CALC THE SP AT CLOUD TOP - ASSUMED TO BE 300MB
20    IF(NTRAIN.EQ.1)THEN
        PTE3=XINTERP(TFI,300.0,3,JTEL)
        PTD3=XINTERP(TFI,300.0,4,JTEL)
        PTE4=XINTERP(TFI,400.0,3,JTEL)
        PTD4=XINTERP(TFI,400.0,4,JTEL)
        PTE=PTE3
        PTD=PTE-((PTE3-PTD3)+(PTE4-PTD4))*0.5
        CALL SP(PTE,PTD,30000.0,ESPT,ESPP)
      ENDIF
      JJ=J
      DO 200 I=1,200
        IF(NTRAIN.EQ.2)THEN

*** LATERAL ENTRAINMENT - INTERPOLATE THE ENVIRONMENTAL TEMP AND DEW-POINT 
*** (TO LEVEL P) IN KELVIN 
          PTE=XINTERP(TFI,(P/100.0),3,JTEL)
	  
          IF(P.GT.30000.0)THEN
            PTD=XINTERP(TFI,(P/100.0),4,JTEL)
          ELSE
            PTD=PTE-TDEPRES

*** HERE WE ASSUME THAT THE MOISTURE REMAINS THE SAME ABOVE 300 HPA 
          ENDIF
	  
*** CALC THE LEVEL OF THE SATURATION POINT (SP) IN MB 
          CALL SP(PTE,PTD,P,ESPT,ESPP)
        ENDIF

*** FOR CLOUDTOP ENTRAINMENT: NO ENTRAINMENT ABOVE 300 HPA, IE BETA=0
        IF(NTRAIN.EQ.1.AND.(P/100.0).LT.300.0)BETA=0.0

*** CALC EDELQW AND EDELQL AT ESPP - THAT THE TEMP DIFFERENCE BETWEEN 
*** ESPT AND XNATV AT ESPP (FROM CSPP), AND DATV AT ESPP 
*** RESPECTIVELY
        PRES=CSPP
        ESPTNAT=CSPT
        ESPTDRG=CSPT
110     IF(PRES.GT.(ESPP+300.0))THEN
          ESPTNAT=XNATV(ESPTNAT,PRES,500.0)
          ESPTDRG=DATV(ESPTDRG,PRES,500.0)
          PRES=PRES-500.0
          GOTO 110
        ENDIF
        EDELQW=ABS(ESPTNAT-ESPT)
        EDELQL=ABS(ESPT-ESPTDRG)

*** CALC THE LEVEL OF THE SATURATION POINT (SP)P FOR THE MIXED PARCEL
        AMSPP=CSPP-BETA*(CSPP-P)

*** CALC THE TEMP OF THE ENTRAINED/MIXED PARCEL 
        PRES=CSPP
        AMTNAT=CSPT 
        AMTDRG=CSPT 
120     IF(PRES.GT.(AMSPP+50.0))THEN
          AMTNAT=XNATV(AMTNAT,PRES,100.0)
          AMTDRG=DATV(AMTDRG,PRES,100.0)
          PRES=PRES-100.0
          GOTO 120
        ENDIF
        AMND=ABS(AMTNAT-AMTDRG)

*** NOW CALC AMDELQW AND THEN AMSPT
        AMDELQW=AMND*(1.0-EDELQL/(EDELQL+EDELQW)) 
        AMSPT=AMTNAT-AMDELQW

*** CALC THE PARCEL'S TEMP AND DEW-POINT AT LEVEL P
        PRES=AMSPP
        T=AMSPT
	
130     IF(PRES.GT.(P+50.0))THEN
          T=XNATV(T,PRES,100.0)
          PRES=PRES-100.0
          GOTO 130
        ENDIF

*** SET THE NEW PARCEL'S SP 
        CSPP=AMSPP
        CSPT=AMSPT

*** GET THE DEW-POINT DEPRESSION - WILL BE USED LATER IF DR<300MB
        TDEPRES=PTE-PTD
        TK=T

*** CALC THE FINAL VAPOUR PRESSURE AND MIXING RATIO OF PARCEL AFTER ENTRAINMENT
        E=611.0*EXP((BL/RV)*(1.0/273.16-1.0/TK))
        RS=EPS*E/(P-E)

*** CALC THE UPDRAFT VELOCITY IN M/S
        CALL UPDRAFT(WBASRS,P,TK,RS,VU,TFI,JTEL,ITIPWLK,CLWATER,LOADING,
     * WBASP)

       

*** TEST IF DR LIES ON ONE OF THE TFI'S PRESSURE LEVELS
        DO 140 MM=1,JTEL
          IF(TFI(MM,1).GE.(P/100.0).AND.TFI(MM,1).LT.(VORIGP/100.0))THEN
            WOLKDTA(MM+1,1)=TFI(MM,1)
            WOLKDTA(MM+1,2)=TFI(MM,2)
            WOLKDTA(MM+1,3)=TFI(MM,3)-273.16
            WOLKDTA(MM+1,4)=TFI(MM,4)-273.16
            WOLKDTA(MM+1,5)=XINTWLK(VORIGP,P,TFI(MM,1)*100.0
     *                             ,(VORIGTK-273.16),(TK-273.16))
            WOLKDTA(MM+1,6)=XINTWLK(VORIGP,P,TFI(MM,1)*100.0
     *                             ,(VORIGRS*1000.0),(RS*1000.0))
            WOLKDTA(MM+1,7)=XINTWLK(VORIGP,P,TFI(MM,1)*100.0
     *                             ,VORIGVU,VU)
            WOLKDTA(MM+1,8)=TFI(MM,5)
            WOLKDTA(MM+1,9)=CLWATER*1000.0
          ENDIF
140     CONTINUE

*** GO TO NEXT LEVEL
        VORIGP=P
        VORIGTK=TK
        VORIGRS=RS
        VORIGVU=VU
        P=P-DP

*** FIND THE MAX UPDRAFT VELOCITY
        IF(VU.GT.WMAX)THEN
          WMAX=VU
          WMAXDRK=P 
        ENDIF

	  
*** TEST FOR THE END OF THE RUN - UPDRAFT LESS THAN 0 M/S OR 
*** PRESSURE L.T. 100 HPA 
        IF(VU.LT.-20.0)GOTO 70
c rej  vu lt -20 or zero??
        IF(P.LE.TFI(JTEL,1)*100.0.OR.P.LE.10000.0)GOTO 70

200   CONTINUE
70    continue

      RETURN
      END 










      FUNCTION XINTBAS(TFI,PP,JVAL,JTEL)
*******************************************************************
*** INTERPOLATION OF CLOUD BASE BETWEEN 2 LEVELS 
*******************************************************************
      DIMENSION TFI(100,6)

*** SEARCH FOR THE 2 LEVELS EACH SIDE OF P
      P=PP/100.0
      DO 20 I=1,JTEL-1
        IF(P.LT.TFI(I,1).AND.P.GE.TFI(I+1,1))THEN 
          PDIFF=TFI(I,1)-TFI(I+1,1)
          VDIFF=TFI(I,1)-P
          VERH=VDIFF/PDIFF
          ADIFF=TFI(I+1,JVAL)-TFI(I,JVAL)
          IF(ADIFF.LT.0.0)ADIFF=-1.0*ADIFF
          IF(TFI(I+1,JVAL).LT.TFI(I,JVAL))THEN
            XINTBAS=TFI(I,JVAL)-(ADIFF*VERH)
          ELSE
            XINTBAS=TFI(I,JVAL)+(ADIFF*VERH)
          ENDIF
        ENDIF
20    CONTINUE
      RETURN
      END 























      SUBROUTINE WOLKBAS(TFI,JTEL,WBASP,WBASRS,WBASTMP,WOLKDTA,
     *                   TMAX,TDOU,OPPDRUK)
*******************************************************************
*** CALC THE CLOUD BASE PARAMETERS USING THE SFC T AND TD
*******************************************************************
      DIMENSION TFI(100,7),WOLKDTA(100,10)
      CP=1004.64
      RD=287.04
      RV=461.48
      EPS=0.622
      DP=100.0
      T1=TMAX
      TD1=TDOU
      P=OPPDRUK

*** CALC THE MIXING RATIO USING THE CLAUSSIUS CLAPEYRON EQTN: 
*** R(AT T1)=RS(AT TD1)
      AL=(-0.566*(TD1-273.16)+597.3)*4186.0
      E=611.0*EXP((AL/RV)*(1.0/273.16-1.0/TD1))
      R=EPS*E/P
      JJ=1

*** CALC T ALONG THE DALR UNTIL RS=R IE., T=TD (CLOUD BASE)
      DO 20 I=1,500 

*** CALC THE MIXING RATIO AT T1:
        AL=(-0.566*(T1-273.16)+597.3)*4186.0
        ES=611.0*EXP((AL/RV)*(1.0/273.16-1.0/T1)) 
        RS=EPS*ES/P 

*** WRITE THE DATA TO WOLKDTA 
        IF(TFI(JJ,1).GE.(P/100.0))THEN
          WOLKDTA(JJ,1)=TFI(JJ,1)
          WOLKDTA(JJ,2)=TFI(JJ,2)
          WOLKDTA(JJ,3)=TFI(JJ,3)-273.16
          WOLKDTA(JJ,4)=TFI(JJ,4)-273.16
          WOLKDTA(JJ,5)=T1-273.16
          WOLKDTA(JJ,6)=RS*1000
          WOLKDTA(JJ,8)=TFI(JJ,5)
          JJ=JJ+1
        ENDIF
	
        IF(RS.LE.R)GOTO 10
	
** CALC THE TEMP AT THE NEXT LEVEL FOLLOWING THE DALR 
        T2=T1-RD*T1/(CP*P)*DP 
        T1=T2
        P=P-DP
20    CONTINUE

10    IF(RS.LE.R)THEN
        WBASRS=RS
        WBASP=P
        WBASTMP=T1
        WOLKDTA(JJ,1)=WBASP/100.0
        WOLKDTA(JJ,2)=XINTBAS(TFI,WBASP,5,JTEL)
        WOLKDTA(JJ,3)=XINTBAS(TFI,WBASP,3,JTEL)-273.16
        WOLKDTA(JJ,4)=XINTBAS(TFI,WBASP,4,JTEL)-273.16
        WOLKDTA(JJ,5)=T1-273.16
        WOLKDTA(JJ,6)=RS*1000 
        WOLKDTA(JJ,8)=XINTBAS(TFI,WBASP,5,JTEL)
      ELSE
*** CLOUD BASE NOT OBTAINED - AIR TOO DRY 
        WBASRS=9999 
        WBASTMP=9999
        WBASP=9999
      ENDIF
      RETURN
      END 


















      SUBROUTINE WOLKSRT(sounding,TFI,WBASP,WBAST,ITIPWLK,JTEL,
     *                   CAPE,ebs,R,PSEUDO,ITEL,sh1,sh2,sh3,esicat)
*******************************************************************
*** USE SOUNDING DATA TO DETERMINE THE MOST LIKELY MODE OF CONVECTION 
*** ACCORDING TO THE AMOUNT OF CAPE AND VERTICAL WIND SHEAR. POSSIBLE  
*** MODES OF CONVECTION ARE CUMULUS, AIR-MASS THUNDERSTORM, 
*** MULTI-CELL THUNDERSTORM AND SUPERCELL THUNDERSTORM 
*******************************************************************
      real ebs,sh1,sh2,sh3,esicat
      DIMENSION TFI(100,7),sounding(100,7)
      RD=287.04
      RV=461.48
      EPS=0.622


*** CALC CAPE BY INTEGRATING THE POSITIVE AREA I.E., PARCEL WARMER 
*** THAN ENVIRONMENTAL AIR. INCREMENTS OF 5 MB 
      DP=500.0
      PRES=WBASP
      PSEUDO=WBAST
10    IF(PRES.LT.100500.0)THEN
        PSEUDO=XNATV(PSEUDO,PRES,-500.0)
        PRES=PRES+500.0
        GOTO 10
      ENDIF

***SET PRESSURE AND TEMP TO THOSE VALUES AT CLOUD BASE 
      PRES=WBASP
      TNAT=WBAST
      CAPE=0.0
      
20    IF(PRES.GT.15000.0)THEN 

*** T1 IS PARCEL TEMP, TO1 IS AMBIENT TEMP
        T1=TNAT
        TO1=XINTERP(TFI,PRES/100.0,3,JTEL)
        DP1000=PRES-100000.0
        Q1=DATV(T1,PRES,DP1000)
        QO1=DATV(TO1,PRES,DP1000)
        T2=XNATV(T1,PRES,DP)
        PRES=PRES-DP
        TO2=XINTERP(TFI,PRES/100.0,3,JTEL)
        DP1000=PRES-100000.0
        Q2=DATV(T2,PRES,DP1000)
        QO2=DATV(TO2,PRES,DP1000)

*** CALC THE CAPE IF THE PARCEL'S TEMP IS WARMER THAN THAT OF THE ENVIRONMENT 
*** THE CAPE IS CALC BY INTEGRATING THE POSITIVE AREA BETWEEN Q1,QO1,Q2,QO2 
        TEST=(0.5*RD*(T1+T2)*0.5/(0.5*(PRES+PRES+DP))*
     *         ((Q2-QO2)/QO2 + (Q1-QO1)/QO1) *DP) 
        IF(Q2.GE.QO2.AND.TEST.GE.0.0)THEN
          CAPE=CAPE+(0.5*RD*(T1+T2)*0.5/(0.5*(PRES+PRES+DP))*
     *         ((Q2-QO2)/QO2 + (Q1-QO1)/QO1) *DP) 
          
        ENDIF

        TNAT=T2
        GOTO 20
      ENDIF
      
c old
c      TYPE=CAPE*ebs
c      IF(TYPE.LE.1.0)ITIPWLK=0
c      IF(TYPE.LE.3.0.AND.TYPE.GT.1.0)ITIPWLK=1
c      IF(TYPE.GT.3.0.AND.TYPE.LE.5.0) ITIPWLK=2
c      IF(TYPE.GT.5.0) ITIPWLK=3
c REJ Modulate ESI thresholds 
      TYPE=CAPE*ebs
      IF(TYPE.LE.sh1) then
      ITIPWLK=0
      esicat = 1
      endif

      IF(TYPE.LE.sh2.AND.TYPE.GT.sh1) then
      ITIPWLK=1
      esicat = 2
      endif

      IF(TYPE.GT.sh2.AND.TYPE.LE.sh3) then
      ITIPWLK=2
      esicat = 3
      endif

      IF(TYPE.GT.sh3) then
      ITIPWLK=3
      esicat = 4 
      endif

     
c200   FORMAT(' CAPE=',F6.1,'  WINDSKUIWING=',F7.5,'  TIPE WOLK=',I1,
c     *       '  RICHARDSON=',F7.1)
      RETURN
      END 
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      subroutine hailstone(UPMAXV, UPMAXT, D, ZBAS, TBAS, TOPT,
     * TOPZ, CAPE, cdate,nofroze, Daloft, ebs, elevated,lplcount,
     * subcloud,embryo)
*****************************************************************
***   HAILCAST: ONE-DIMENSIONAL HAIL MODEL 
***   THE PROGRAM CALCULATES THE MAXIMUM EXPECTED HAIL DIAMETER 
***   AT THE SURFACE USING DATA FROM THE 1D CLOUD MODEL 
*****************************************************************
      implicit real (A-H), real (O-Z)
      DIMENSION TCA(100),R(100),VUU(100),DD(20),BEGTYD(20),TAA(100)
      DIMENSION JST(6),ISTN(6),IHT(6),IPRES(6),
     *          ZA(100)
      CHARACTER*12 DTIPE(7)
      integer nofroze
      real embryo
      COMMON /AAA/ PA(100), ITEL
      
***************************************************************** 
***           LIST OF VARIABLES 
***************************************************************** 
***  A        = VENTILATION COEFFICIENT 
***  AK       = THERMAL CONDUCTIVITY 
***  ANU      = DYNAMIC VISCOSITY 
***  ALF      = LATENT HEAT OF FUSION 
***  ALS      = LATENT HEAT OF SUBLIMATION 
***  ALV      = LATENT HEAT OF EVAPORATION 
***  CD       = DRAG COEFFICIENT OF HAILSTONE 
***  CI       = SPECIFIC HEAT CAPACITY OF ICE 
***  CW       =    "             "    OF WATER 
***  CLADWAT  = ADIABATIC CLOUD WATER CONTENT 
***  CLWATER  = TOTAL         "        " 
***  D        = DIAMETER OF HAILSTONE 
***  DELRW    = DIFFERENCE IN VAPOUR DENSITY BETWEEN HAIL SURFACE AND 
***             UPDRAFT AIR 
***  DENSA    = DENSITY OF THE IN-CLOUD AIR 
***  DENSE    =    "    OF THE HAILSTONE 
***  DI       = DIFFUSITIVITY 
***  DGM      = TOTAL INCREASE IN MASS OF THE HAILSTONE 
***  DGMI     = MASS INCREASE DUE TO ACCRETION OF ICE PARTICLES 
***  DGMW     =     "       DUE TO ACCRETION OF WATER DROPLETS 
***  EI       = COLLECTION EFFICIENCY FOR ICE 
***  EW       =    "          "       "  WATER 
***  FW       = FRACTION OF WATER IN HAILSTONE 
***  G        = ACCELERATION DUE TO GRAVITY 
***  GM       = TOTAL MASS OF THE HAILSTONE 
***  GMW      = MASS OF WATER IN THE HAILSTONE 
***  GMI      = MASS OF ICE IN THE HAILSTONE 
***  ISEK     = TIME COUNTER (NUMBER OF SECONDS) 
***  ISEKDEL  = TIME STEP(IN SECONDS) 
***  P        = PRESSURE 
***  PA       = ENVIRONMENTAL PRESSURE 
***  PC       = PERCENT CLOUD WATER 
***  R        = MIXING RATIO OF THE AIR 
***  RE       = REYNOLDS NUMBER 
***  RS       = SATURATED MIXING RATIO OF THE AIR 
***  REENWAT  = RAINWATER CONTENT OF THE CLOUD 
***  TAU      = MAXIMUM CLOUD LIFETIME 
***  T        = INTERPOLATED ENVIRONMENTAL TEMP 
***  TA       = ENVIRONMENTAL TEMP OF SOUNDING/TFI (MATRIX) 
***  TD       = ENVIRONMENTAL DEWPOINT OF SOUNDING/TFI 
***  TC       = CLOUD TEMP AT LEVEL P 
***  TCA      = CLOUD TEMP MATRIX 
***  TS       = HAILSTONE'S SURFACE TEMPERATURE 
***  V        = ACTUAL VELOCITY OF THE HAILSTONE 
***  VT       = TERMINAL VELOCITY OF THE HAILSTONE 
***  VUU      = UPDRAFT MATRIX 
***  VU       = UPDRAFT VELOCITY 
***  WBAST    = CLOUD BASE TEMP. 
***  WBASTD   =    "     DEW-POINT 
***  WBASRS   =    "     SATURATED MIXING RATIO 
***  WBASTW   =    "     WETBULB POTENTIAL TEMPERATURE 
***  WBASP    =    "     PRESSURE 
***  WTOPP    = CLOUD TOP PRESSURE 
***  WTOPT    =   "    TEMP 
***  WWATER   = CLOUD WATER CONTENT 
***  WYS      = CLOUD ICE     " 
***  XI       = CONCENTRATION OF ICE ENCOUNTERED BY EMBRYO/STONE 
***  XW       = CONCENTRATION OF WATER ENCOUNTERED BY EMBRYO/STONE 
***  Z        = HEIGHT OF EMBRYO/STONE ABOVE THE SURFACE 

      CHARACTER*72 filename, filename1
      INTEGER unitnumber, unitnumber1, elevated
      character*8 cdate
      DATA RV/461.48/,RD/287.04/,G/9.78956/
      DATA PI/3.141592654/,ALF/79.7/,ALV/597.3/
      DATA ALS/677.0/,CI/0.5/,CW/1./
      DATA DTIPE/'  NONE','   SHOT','   PEA','  GRAPE','  WALNUT',
     *           '  GOLF',' >GOLF'/
      DATA JST/71119,68442,68424,68242,68538,68461/
      DATA ISTN/3HWSE,3HBFN,3HUPN,3HMMA,3HDEA,3HBET/
      DATA IHT/766,1350,845,1282,1287,1681/
      DATA IPRES/925,864,900,880,880,840/
      
      write(filename,4) cdate
      unitnumber=1  
      open(unit=unitnumber, file=filename)
4     format("/tmp/c",a8,".dat")

c      write(filename1,5) cdate
c	unitnumber1=4
c	open(unit=unitnumber1, file=filename1)
c5     format("/tmp/h",a8,".dat")

c rej *** TIME STEP IN SECONDS
      SEKDEL=0.2
      
           
******************** 1. INPUT DATA ****************************** 
***            READ OUTPUT DATA FROM THE CLOUD MODEL, FROM c*.dat 
*****************************************************************       

      CALL LEESDTA(PA,ZA,VUU,R,TAA,TCA,IDAT,WBASP,VBASIS,
     *           ISTNR,ITEL,TMAXT,ITIPWLK, CAPE, ebs,
     *           UPMAXV, UPMAXT,RICH,TDEW, ZBAS, TBAS, TOPT,
     *           TOPZ)

C     BEGIN TIME (SECONDS)  
      BEGTYD(1)=60. 
      
C     INITIAL HAIL EMBRYO DIAMETER IN MICRONS, AT CLOUD BASE      
c      DD(1)=300.
      DD(1)= embryo
      
C     UPPER LIMIT OF SIMULATION IN SECONDS       
      TAU=4200.
      
C     STATION HEIGHT       
      STHGTE=ZA(1)
      
c      TAA(1)=TMAX 
      TCA(1)=TAA(1) 
      
C     DETERMINE VALUES OF PARAMETERS AT CLOUD BASE       
      DO 3 I=1,ITEL 
        IF(PA(I).EQ.WBASP)THEN
          PBEGIN=PA(I)
          RSBEGIN=R(I)
          P0=PA(I)
          RS0=R(I)
          TABEGIN=TAA(I)
          TCBEGIN=TCA(I)
          ZBAS=ZA(I)
          V=VUU(I)
          
        ENDIF
3     CONTINUE

C     INITIAL HEIGHT OF EMBRYO ABOVE STATION LEVEL 
      ZBEGIN=ZBAS-STHGTE
      
      JTEL=0

***  SET TEST FOR EMBRYO: 0 FOR NOT FROZEN FOR FIRST TIME, IF 1 
***  DON'T FREEZE AGAIN
      NOFROZE=0

35    CONTINUE

      JTEL=JTEL+1
      
*** INITIAL VALUES FOR VARIOUS PARAMETERS AT CLOUD BASE
      SEK=BEGTYD(JTEL)
      VU=V
      Z=ZBEGIN
      TC=TCBEGIN
      TA=TABEGIN
      WBASP=P0
      P=PBEGIN
      WBASRS=RS0
      RS=RSBEGIN
      RSS=RS/1000.

***  SET RAINWATER TO ZERO
      REENWAT=0.

***  CALC. DENSITY OF THE UPDRAFT AIR (G/CM3)
      DENSA=(P*100./(RD*(1.+0.609*RSS/(1.+RSS))*(TC+273.16)))/1000.

***            HAILSTONE PARAMETERS 
***  INITIALISE SFC.TEMP, DIAMETER(M),FRACTION OF WATER AND DENSITY

      TS=TC
      D=DD(JTEL)/10000.
      PC=0.
      FW=1.0
      DENSE=1.
      
c      write(4,'("  TS    TC     D     P    FW     Z      V",
c     *          "       VU      VT   SEK",
c     *       " TIPE GROEI")')

420    CONTINUE

***   ADVANCE ONE TIME-STEP 
      SEK=SEK+SEKDEL

  
**********************  2. CALCULATE PARAMETERS  ********************** 
***                 CALCULATE UPDRAFT PROPERTIES 
*********************************************************************** 
      CALL INTERP(VUU,VU,P,IFOUT)
      
C	CALCULATE DURATION OF THE UPDRAFT ACCORDING TO THE PRODUCT OF CAPE*SHEAR
        TIME=0.0
        upIndex=5
        TIME=CAPE*(ebs)
        
	
        IF(TIME.LT.1.0)TIME=1.0        
        
          if(upIndex.eq.5.and.TIME.lt.5.0) then
         
           DUR = (-2.5 * TIME**2 + 25.0 * TIME - 2.5)*60.0
           else
            DUR=3600.0
        endif
      
      ITIME = int(DUR)
      IF(ITIME.LT.600) ITIME = 600 
     
      IF(SEK.GT.ITIME)VU=0.0

      IF(IFOUT.EQ.1)GOTO 100


***  CALCULATE TERMINAL VELOCITY OF THE STONE (USE PREVIOUS VALUES) 
      CALL TERMINL(DENSA,DENSE,D,VT,TC) 

***  ACTUAL VELCITY OF THE STONES (UPWARDS IS POSITIVE)
      V=VU-VT

***  USE HYDROSTATIC EQTN TO CALC HEIGHT OF NEXT LEVEL
      P=(P*100.-(DENSA*1000.*G*V/100.)*SEKDEL)/100.
      Z=Z+(V/100.)*SEKDEL

***  CALCULATE NEW COULD TEMP AT NEW PRESSURE LEVEL 
      CALL INTERP(TCA,TC,P,IFOUT)
      IF(IFOUT.EQ.1)GOTO 100

***  CALCULATE PERCENTAGE OF FROZEN WATER USING SCHEME OF VALI AND STANSBURY
      PC=0.008*(1.274)**(-20.-TC)
      IF(TC.GT.-20.)PC=0.
      IF(PC.GT.1.)PC=1.
      IF(PC.LT.0.)PC=0.

***  CALC. MIXING RATIO AT NEW P-LEVEL AND THEN NEW DENSITY OF IN-CLOUD AIR
      CALL INTERP(R,RS,P,IFOUT)
      IF(IFOUT.EQ.1)GOTO 100
      RSS=RS/1000.
      DENSA=(P*100./(RD*(1.+0.609*RSS/(1.+RSS))*(TC+273.16)))/1000.

***  CALC. THE TOTAL WATER CONTENT IN THE CLOUD AT LEVEL P, ALSO CALC ADIABATIC 
***  VALUE
      CALL WOLKWAT(XW,XI,CLWATER,CLADWAT,REENWAT,WWATER,WYS,
     *           PC,WBASRS,RSS,TC,DENSA,ITIPWLK,SEK, WBASP, P)


**************  3. TEST FOR WET OR DRY GROWTH  ************** 
***       WET GROWTH - STONE'S SFC TEMP.GE.0, DRY - STONE'S SFC TEMP.LT.0

*** rej define temperature at which to freeze embryo ***
      IF(TS.GE.-9..AND.TC.GE.-9..AND.NOFROZE.EQ.0)GOTO 42
      IF(TS.LT.0.)THEN
***  DRY GROWTH 
        FW=0.
        ITIPE=1
        ELSE
***  WET GROWTH
        TS=0.
        ITIPE=2
      ENDIF


***     FREEZE THE HAIL EMBRYO AT -8 DEGC, define emb

42    IF(TS.GE.-9..AND.TC.GE.-9..AND.NOFROZE.EQ.0)THEN
        IF(TC.LE.-8.)THEN

***  DRY GROWTH 
          FW=0. 
          TS=TC 
          ITIPE=1 
          NOFROZE=1 
          ELSE 
 
***  WET GROWTH 
          FW=1. 
          TS=TC 
          ITIPE=2 
          NOFROZE=0 
	ENDIF 
       endif
     
      

***  DENSITY OF HAILSTONE - DEPENDS ON FW - ONLY WATER=1 GM/L=1000KG/M3 
***                                         ONLY ICE  =0.9 GM/L 
      DENSE=(FW*(1.0 - 0.9)+0.9)


***  VAPOUR DENSITY DIFFERENCE BETWTEEN STONE AND ENVIRONMENT 

      CALL DAMPDIG(DELRW,PC,TS,TC)


*********************  4. STONE'S MASS GROWTH  *******************
      CALL MASSAGR(GM,D,GM1,DGM,EW,EI,DGMW,DGMI,GMW,GMI,DI, 
     *           TC,TS,P,DENSE,FW,VT,XW,XI,SEKDEL)


***************  5. CALC HEAT BUDGET OF HAILSTONE  ************** 
      SEKK=SEK-BEGTYD(JTEL)
      
      CALL HEATBUD(TS,FW,TC,D,DENSA,GM1,DGM,VT,DELRW,DGMW,
     *           DGMI,GMW,GMI,DI,SEKDEL,ITIPE,P)
     
C      *****WRITE OUTPUT DATA FROM HAIL MODEL TO FILE 
     
c      IF(MOD(INT(SEK/SEKDEL),INT(60.0/SEKDEL)).EQ.0)
c     *WRITE(4,71)TS,TC,D,P,FW,Z,V,VU,VT,SEK,ITIPE,NOFROZE
71    FORMAT(F5.1,' ',F5.1,' ',F8.5,' ',F5.0,' ',F4.2,' ',
     *       F6.0,' ',F7.1,' ',F7.1,' ',F7.1,' ',F6.0,
     *       ' ',I2, ' ', I2)

      if (D.gt.Daloft) Daloft = d
      
c rej  fixes case where particle gets hung up near cloud base
       if((sek.gt.500).and.(v.lt.50).and.(v.gt.-50).and.
     *(vu.le.400).and.(nofroze.eq.0)) then
        upmaxv=4
	upmaxt=tbas
	ic=ic-1
	d=0
       endif

*********6.   TEST DIAMETER OF STONE AND HEIGHT ABOVE GROUND   ******* 
***  TEST IF DIAMETER OF STONE IS GREATER THAN LIMIT, IF SO  
***  BREAK UP 

      CALL BREAKUP(DENSE,D,GM,FW)

***  TEST IF STONE IS BELOW CLOUD BASE-- 
***  THEN NO CLOUD DROPLETS OR CLOUD ICE 
      IF(P.GE.WBASP)THEN
        XW=0.
        XI=0.
      ENDIF

***  TEST IF STONE HAS REACHED THE SURFACE 
      IF(Z.LE.0.) GOTO 100

***  TEST IF MAX CLOUD LIFETIME HAS BEEN REACHED
      IF(SEK.GE.TAU)GOTO 100 

***  GO BACK FOR PREVIOUS TIME STEP       
	GOTO 420
	
***  WRITE VALUES OUT AND STOP RUN

100   CONTINUE


************************************************************************
************************************************************************
**  if this is an elevated sounding, then use melting routine to melt the 
**  stone from lpl to original surface.
      if(elevated.eq.1) call melt(d,subcloud,lplcount)
************************************************************************
************************************************************************


*** CONVERT TIME TO MINUTES 
      TTYD=SEK/60. 
 
***  WRITE HAIL SIZES OUT 
***  IF FW=1.0 THEN HAIL HAS MELTED AND SET D TO 0.0
      IF(ABS(FW - 1.0).LT.0.001) D=0.0

   
C     CALCULATE CLOUD BASE INDEX; PRODUCT OF CLOUD BASE TEMP AND CLOUD BASE 
C     SATURATION MIXING RATIO 
   
      BINDEX=RSBEGIN*TCBEGIN

      close(unit = 1)
      close(unit = 5)
     
 20   continue

      return
      END 



















      SUBROUTINE HEATBUD(TS,FW,TC,D,DENSA,GM1,DGM,VT,DELRW,DGMW,
     *           DGMI,GMW,GMI,DI,SEKDEL,ITIPE,P)
******************************************************************
*** CALCULATE HAILSTONE'S HEAT BUDGET 
******************************************************************
      implicit real (A-H), real (O-Z)
      DATA RV/461.48/,RD/287.04/,G/9.78956/
      DATA PI/3.141592654/,ALF/79.7/,ALV/597.3/
      DATA ALS/677.0/,CI/0.5/,CW/1./
      
***  CALCULATE THE CONSTANTS 
      AK=(5.8+0.0184*TC)*10.**(-5.)
      TK=TC+273.15
      ANU=1.717E-4*(393.0/(TK+120.0))*(TK/273.15)**1.5

***  CALCULATE THE REYNOLDS NUMBER 
      RE=D*VT*DENSA/ANU
      H=(0.71)**(1.0/3.0)*(RE**0.50)
      E=(0.60)**(1.0/3.0)*(RE**0.50)

***   SELECT APPROPRIATE VALUES OF AH AND AE ACCORDING TO RE
      IF(RE.LT.6000.0)THEN
         AH=0.78+0.308*H
         AE=0.78+0.308*E
      ELSEIF(RE.GE.6000.0.AND.RE.LT.20000.0)THEN
         AH=0.76*H
         AE=0.76*E
      ELSEIF(RE.GE.20000.0) THEN
         AH=(0.57+9.0E-6*RE)*H
         AE=(0.57+9.0E-6*RE)*E
      ENDIF

***  FOR DRY GROWTH FW=0, CALCULATE NEW TS, ITIPE=1 
***  FOR WET GROWTH TS=0, CALCULATE NEW FW, ITIPE=2

      IF(ITIPE.EQ.2)GOTO 60

50    CONTINUE

***  DRY GROWTH; CALC NEW TEMP OF THE STONE 
      TS=TS-TS*DGM/GM1+SEKDEL/(GM1*CI)*(2.*PI*D*(AH*AK*(TC-TS)
     *-AE*ALS*DI*DELRW)+ DGMW/SEKDEL*(ALF+CW*TC)+DGMI/SEKDEL*CI*TC)

51    FORMAT(I4,'TS=',F6.2,'DGM=',F14.13,'GM1=',F14.12,
     *'DGMW=',F14.13,'DGMI=',F14.13)

      GOTO 70
60    CONTINUE

***  WET GROWTH; CALC NEW FW
      FW=FW-FW*DGM/GM1+SEKDEL/(GM1*ALF)*(2.*PI*D*(AH*AK*TC
     *-AE*ALV*DI*DELRW)+DGMW/SEKDEL*(ALF+CW*TC)+DGMI/SEKDEL*CI*TC)

70    CONTINUE
      IF(FW.GT.1.)FW=1.
      IF(FW.LT.0.)FW=0.
      RETURN
      END 





















******************************************************************
*** CALC THE STONE'S INCREASE IN MASS 
******************************************************************
      SUBROUTINE MASSAGR(GM,D,GM1,DGM,EW,EI,DGMW,DGMI,GMW,GMI,DI,
     *           TC,TS,P,DENSE,FW,VT,XW,XI,SEKDEL)
            
      implicit real (A-H), real (O-Z)
      PI=3.141592654

***  CALCULATE THE DIFFUSIVITY DI
      D0=0.226
      DI=D0*((TC+273.16)/273.16)**1.81*(1000./P)
  
***  COLLECTION EFFICIENCY FOR WATER AND ICE 
      EW=1.0
      
***   IF TS WARMER THAN -5 DEGC THEN ACCRETE ALL THE ICE (EI=1.0) 
***   OTHERWISE EI=0.21      
      IF(TS.GE.-5.0)THEN
        EI=1.00
        ELSE
        EI=0.21
      ENDIF

***  CALC HAILSTONE'S MASS (GM), MASS OF WATER (GMW) AND THE MASS OF 
***  ICE IN THE STONE (GMI)
      GM=PI/6.*(D**3.)*DENSE
      GMW=FW*GM
      GMI=GM-GMW

***  STORE THE MASS
      GM1=GM



*********************  4. STONE'S MASS GROWTH  ******************* 

***  CALCULATE THE NEW DIAMETER
      D=D+SEKDEL*0.5*VT/DENSE*(XW*EW+XI*EI)

***  CALCULATE THE INCREASE IN MASS DUE TO INTERCEPTED CLOUD WATER
      GMW2=GMW+SEKDEL*(PI/4.*D**2.*VT*XW*EW)
      DGMW=GMW2-GMW 
      GMW=GMW2

***  CALCULATE THE INCREASE IN MASS DUE INTERCEPTED CLOUD ICE
      GMI2=GMI+SEKDEL*(PI/4.*D**2.*VT*XI*EI)
      DGMI=GMI2-GMI 
      GMI=GMI2

***  CALCULATE THE TOTAL MASS CHANGE 
      DGM=DGMW+DGMI 
      RETURN
      END 




















******************************************************************
***  CALCULATE THE TOTAL WATER CONTENT OF THE CLOUD AT LEVEL P, AND 
***  THE ADIABATIC VALUE 
*****************************************************************
      SUBROUTINE WOLKWAT(XW,XI,CLWATER,CLADWAT,REENWAT,WWATER,WYS,
     *           PC,WBASRS,RSS,TC,DENSA,ITIPWLK,SEK, WBASP, P)

      implicit real (A-H), real (O-Z)

      CLWATER=WBASRS/1000.-RSS
      CLADWAT=DENSA*CLWATER

***  CLOUD ICE AND LIQUID WATER
      WWATER=CLADWAT
      WYS=PC*CLADWAT
      XW=WWATER-WYS 
      XI=WYS
      RETURN
      END 
















***  INTERPOLATE VALUES OF RS AT LEVEL P BETWEEN 2 LEVELS OF R 
***  (AT LEVEL PP) ******************************************************************

******************************************************************
      SUBROUTINE INTERP(R,RS,P,IFOUT)

      implicit real (A-H), real (O-Z)
      DIMENSION R(100)
      COMMON /AAA/ PP(100), ITEL
      IFOUT=0

***  SEARCH FOR THE 2 LEVELS EACH SIDE OF P
      DO 10 I=1,ITEL-1
      IF(P.LE.PP(I).AND.P.GT.PP(I+1))GOTO 20
10    CONTINUE

      IFOUT=1
      GOTO 30
20    CONTINUE

***  CALC RATIO BETWEEN VDIFF AND PDIFF 
      PDIFF=PP(I)-PP(I+1)
      VDIFF=PP(I)-P 
      VERH=VDIFF/PDIFF

***  CALCULATE THE DIFFERENCE BETWEEN 2 R VALUES
      RDIFF=ABS(R(I+1)-R(I))

***  CALCULATE NEW VALUE
      IF(R(I+1).LT.R(I))THEN
        RS=R(I)-(RDIFF*VERH)
        ELSE
        RS=R(I)+(RDIFF*VERH)
      ENDIF
30    RETURN
      END 
















**************************************************************
***  CALC THE DIFFERENCE IN SATURATION VAPOUR DENSITY (SI UNITS) 
***  BETWEEN THAT OVER THE HAILSTONE'S SURFACE AND THE IN-CLOUD 
***  AIR, DEPENDS ON THE WATER/ICE RATIO OF THE UPDRAFT, 
***  AND IF THE STONE IS IN WET OR DRY GROWTH REGIME 
**************************************************************
      SUBROUTINE DAMPDIG(DELRW,PC,TS,TC)
      implicit real (A-H), real (O-Z)
      DATA RV/461.48/,ALV/2500000./,ALS/2836050./ 

***  FOR HAILSTONE:  FIRST TEST IF STONE IS IN WET OR DRY GROWTH
      TSK=TS+273.16 
      TCK=TC+273.16 
      IF(TSK.GE.(0.+273.16))THEN
***  IF WET GROWTH
        ESAT=611.*EXP(ALV/RV*(1./273.16-1./TSK))
        ELSE
***  IF DRY GROWTH
        ESAT=611.*EXP(ALS/RV*(1./273.16-1./TSK))
      ENDIF
      RHOKOR=ESAT/(RV*TSK)
      
***  NOW FOR THE AMBIENT/IN-CLOUD CONDITIONS 
      ESATW=611.*EXP(ALV/RV*(1./273.16-1./TCK))
      RHOOMGW=ESATW/(RV*TCK)
      ESATI=611.*EXP(ALS/RV*(1./273.16-1./TCK))
      RHOOMGI=ESATI/(RV*TCK)
      RHOOMG=PC*(RHOOMGI-RHOOMGW)+RHOOMGW

***  CALC THE DIFFERENCE(G/CM3): <0 FOR CONDENSATION, >0 FOR EVAPORATION
      DELRW=(RHOKOR-RHOOMG) / 1000.
      RETURN
      END 


















********************************************************************
***  CALCULATE THE TERMINAL VELOCTY OF THE HAILSTONE (SI-UNITS)
********************************************************************

      SUBROUTINE TERMINL(DENSA,DENSE,D,VT,TC)
      implicit real (A-H), real (O-Z)
      DATA B0/-3.18657/,B1/0.992696/,B2/-0.00153193/,
     *B3/-0.0009877059/,B4/-0.000578878/,B5/0.0000855176/,
     *B6/-0.00000327815/,G/9.78956/, PI/3.141592654/

      DENSASI=DENSA*1000.
      DENSESI=DENSE*1000.
      DD=D/100.
      
***   MASS OF STONE IN GRAMS 
      GMASS=(DENSESI*PI*(DD**3.0))/6.0
      TCK=TC+273.16 

***  CALC DYNAMIC VISCOSITY 
      ANU=(0.00001718)*(273.16+120.)/(TCK+120.)*(TCK/273.16)**(3./2.)


***  CALC THE BEST NUMBER, X AND REYNOLDS NUMBER, RE 
      GX=(8.0*GMASS*G*DENSASI)/(PI*(ANU)**2.0)
      RE=(GX/0.6)**0.5


***   SELECT APPROPRIATE EQUATIONS FOR TERMINAL VELOCITY DEPENDING ON 
***   THE BEST NUMBER
      IF(GX.LT.550)GOTO 10
      IF(GX.GE.550.AND.GX.LT.1800)GOTO 20
      IF(GX.GE.1800.AND.GX.LT.3.45E08)GOTO 30
      IF(GX.GE.3.45E08)GOTO 40  

10    CONTINUE
      W=LOG10(GX)
      Y= -1.7095 + 1.33438*W - 0.11591*(W**2.0)      
      RE=10**Y
      VT=ANU*RE/(DD*DENSASI)
      GOTO 70
20    CONTINUE

      W=LOG10(GX)
      Y= -1.81391 + 1.34671*W - 0.12427*(W**2.0) + 0.0063*(W**3.0)
      RE=10**Y
      VT=ANU*RE/(DD*DENSASI)
      GOTO 70
30    CONTINUE

      RE=0.4487*(GX**0.5536)
      VT=ANU*RE/(DD*DENSASI)
      GOTO 70
40    CONTINUE

      RE=(GX/0.6)**0.5
      VT=ANU*RE/(DD*DENSASI)
      GOTO 70
70    CONTINUE


      VT=VT*100.
      RE1=RE

      RETURN
      END 



**************************************************************
***  TEST IF AMOUNT OF WATER ON SURFACE EXCEEDS CRTICAL LIMIT- 
***  IF SO INVOKE SHEDDING SCHEME 
**************************************************************
      SUBROUTINE BREAKUP(DENSE,D,GM,FW) 
      implicit real (A-H), real (O-Z)
      DATA PI/3.141592654/

C     ONLY TEST FOR EXCESS WATER IF STONE'S D IS GT 9 MM      
      IF(D.LE.0.9) GOTO 10

      WATER=FW*GM
      GMI=GM-WATER

C     CALC CRTICAL MASS CAPABLE OF BEING "SUPPORTED" ON THE STONE'S 
C     SURFACE 
      CRIT=0.268+0.1389*GMI 
       IF(WATER.GT.CRIT)THEN
           WAT=WATER-CRIT
           GM=GM-WAT
           FW=(CRIT)/GM
       ELSE
           GOTO 10
       ENDIF
       
      IF(FW.GT.1.0) FW=1.0
      IF(FW.LT.0.0) FW=0.0

C     RECALCULATE DENSITY AND DIAMETER AFTER SHEDDING 
      DENSE=(FW*(1.0 - 0.9)+0.9)
      D=(6.*GM/(PI*DENSE))**(1./3.)

10    CONTINUE
      RETURN
      END 














      SUBROUTINE LEESDTA(PA,ZA,VUU,R,TAA,TCA,IDAT,WBASP,VBASIS,
     *          ISTNR,ITEL,TMAXT,ITIPWLK, CAPE, ebs, UPMAXV, 
     *          UPMAXT, RICH, TDEW, ZBAS, TBAS, TOPT, TOPZ)


      implicit real (A-H), real (O-Z)

      DIMENSION TCA(100),R(100),VUU(100),TAA(100),
     *          PA(100),ZA(100)

      READ(1,*)
      READ(1,*)
      READ(1,*)
      READ(1,*)TMAXT,TDEW
      READ(1,*)
      READ(1,*)CAPE,ebs,TIPWLK,PSEUDO
      READ(1,*)
      DO 10 I=1,100 
        READ(1,*,END=20,ERR=20)PA(I),HGTE,TAA(I),DUM,TCA(I),
     *                  R(I),VUU(I),ZA(I)
        IF(HGTE.NE.0)WBASP=PA(I)
        IF(HGTE.NE.0)VBEGIN=VUU(I)
        IF(HGTE.NE.0)ZBAS=ZA(I)
        IF(HGTE.NE.0)TBAS=TCA(I)

***	MAX UPDRAFT VELOCITY TO M/S 
        VUU(I)=VUU(I)*100.0

10    CONTINUE

20    ITEL=I

        UPMAXV=0.0
        
        DO 30 K=1,ITEL  

         IF(UPMAXV.LT.VUU(K))THEN
           UPMAXV=VUU(K)
           UPMAXT=TCA(K)
           UPMAXP=PA(K)

         ENDIF
           IF(TCA(K).LT.0.AND.TCA(K).LT.TAA(K).OR.
     *        VUU(K).LT.0)THEN
              TOPT=TCA(K)
              TOPZ=ZA(K)
              TOPP=PA(K)
              GOTO 35
           ENDIF
30      CONTINUE
35      UPMAXV=UPMAXV/100.0


      RETURN
      END 


************************************************************************
************************************************************************
       SUBROUTINE MELT(d, subcloud, lplcount)
c
CJCS  This is a spherical hail melting estimate based on the Goyer et al. (1969)
CJCS  eqn (3).  The depth of the warm layer, estimated terminal velocity, and
CJCS  mean temperature of the warm layer are used.  DRB.  11/17/2003.
c
       real subcloud(50,7)
       real layer, tlayer, dlayer, player, eenv, gamma, 
     &      delta,ewet,de,der,wetold
       real*8 ld,sd,lt
       integer wcnt
       d=d/2.54
       tsum=0.0
       tdsum=0.0
       psum=0.0
       layer=subcloud(lplcount,1)-subcloud(1,1)
       do 1, i=1, lplcount
          tsum=subcloud(i,4)+tsum
          tdsum=subcloud(i,5)+tdsum
	  psum=subcloud(i,2)+psum
1      CONTINUE
       TLAYER=TSUM/lplcount
       DLAYER=TDSUM/lplcount
       PLAYER=PSUM/lplcount


c      print *,'layer depth(m) = ',layer
c      print *,'mean layer temp(c) = ',tlayer
c      print *,'mean layer dpt(c) = ',dlayer
c      print *,'mean layer press(mb) = ',player


c Convert the mean layer temp and mean layer dewpt to wet bulb temp...
c Now...calculate the wet bulb temperature.
        eenv = 0.6108*(exp((17.27*dlayer)/(237.3 + dlayer))) 
        eenv = eenv*10. ! convert to mb
        gamma = 6.6e-4*player
        delta = (4098.0*eenv)/((dlayer+237.7)**2)
        wetbulb = ((gamma*tlayer)+(delta*dlayer))/(gamma+delta) !Tw degC

c Now iterate to precisely determine wet bulb temp.
        wcnt = 0
 800    continue
c calc vapor pressure at wet bulb temp
        ewet = 0.6108*(exp((17.27*wetbulb)/(237.3 + wetbulb))) 
        ewet = ewet*10. ! convert to mb
        de = (0.0006355*player*(tlayer-wetbulb))-(ewet-eenv)
        der= (ewet*(.0091379024 - (6106.396/(273.155+wetbulb)**2))) 
     &       - (0.0006355*player)
        wetold = wetbulb
        wetbulb = wetbulb - de/der
        wcnt = wcnt + 1
        if((abs(wetbulb-wetold)/wetbulb).gt..0001.and.(wcnt.lt.11)) 
     &   goto 800


        ld = layer
        sd = d
        lt = wetbulb + 273.155

        call hailstonelpl(ld,lt,sd)
       d=sd
       return
       end
       
       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       subroutine hailstonelpl(depth,tenv,a)

c
CJCS  Compute the nondim parameter tao (in WAF, Dec 1996, pg 591) to determine
CJCS  whether a falling ice crystal melts.  Use in a precipitation type
CJCS  algorithm.
c

       DOUBLE PRECISION A
       real*8 depth,tervel,lf,r,t0,tres,tenv,ka,dmdt,re,rho,mass,
     &        delt,lv,dv,pi,dsig,esens,rhosenv,essfc,rhosfc,rv,
     &        massorg,rhoice,newmass

c       depth = 3000.0 ! depth of the warm layer (meters)
c       write(*,*) 'Enter the hail size diameter (inches)...'
c       read(*,*) a
       a = a*2.54 ! convert inches to cm
       a = 0.5*a/100. ! convert to radius and cm -> meters
       r = a ! r is the hail radius in meters (before melting, r = a)
       ka = .02 ! thermal conductivity of air
       lf = 3.34e5 ! latent heat of melting/fusion
       lv = 2.5e6  ! latent heat of vaporization
       t0 = 273.155 ! temp of ice/water melting interface
c       tenv = 271.155 ! mean temperature environment of warm layer (K)
       dv = 0.25e-4 ! diffusivity of water vapor (m2/s)
       pi = 3.1415927
       rv = 1004. - 287. ! gas constant for water vapor
       rhoice = 917.0 ! density of ice (kg/m**3)

c compute terminal speed based on Houze Cloud Dyn. (pg 91) simple method.
       tervel = 100.0*(9.0*(((a*2.0)*100.)**0.8)) ! cm/s
c       write(*,*) 'Terminal velocity (cm/s)= ',tervel

c computer residence time in the warm layer...
       tres = (depth*100.0)/tervel ! residence time in warm layer
       if(tres.lt.0.0) then
c        write(*,*) 'WARNING...Residence time < 0'
       endif
c       write(*,*) 'Residence time in warm layer is= ', tres

c
c Calc dmdt based on eqn (3) of Goyer et al. (1969)
c
c      Reynolds number...from pg 317 of Atmo Physics (Salby 1996)
c      Just use the density of air at 850 mb...close enough.
       rho = 85000./(287.*tenv)
       re = rho*r*tervel*.01/1.7e-5
c       write(*,*)   're= ',re

       delt = tenv - t0 ! temp difference between env and hailstone sfc.

c calculate the differencein vapor density of air stream and equil vapor
c density at the sfc of the hailstone
       esenv = 6.108*(exp((17.27*(tenv-273.155))/
     &         (237.3 + (tenv-273.155)))) ! es environment in mb
       esenv = esenv*100. ! mb to pa
       rhosenv = esenv/(rv*tenv)
       essfc = 6.108*(exp((17.27*(t0-273.155))/
     &         (237.3 + (t0-273.155)))) ! es environment in mb
       essfc = essfc*100. ! mb to pa
       rhosfc = essfc/(rv*t0)

c       write(*,*) 'rhosenv, rhosfc= ',rhosenv,rhosfc
       dsig = rhosenv - rhosfc

       dmdt = (-1.7*pi*r*(re**0.5)/lf)*((ka*delt)+((lv-lf)*dv*dsig))
       if(dmdt.gt.0.0) then
cc        write(*,*)'Warning...thermodynamics support further hail growth'
        dmdt = 0.
       endif
       mass = dmdt*tres
c       write(*,*)'dmdt (g/s), mass lost (g)= ',dmdt*1000.,mass*-1000.
c       mass = dmdt*240.
c       write(*,*)'dmdt (g/s), mass lost (g)= ',dmdt*1000.,mass*-1000.

c now find the new diameter of the hailstone...
       massorg = (4.0/3.0)*pi*r*r*r*rhoice
       newmass = massorg + mass
       if(newmass.lt.0.0) newmass = 0.0
c       write(*,*) 'mass_original, mass_new= ',massorg,newmass
       a = (0.75*newmass/(pi*rhoice))**0.333333333
c       write(*,11) 'Stone size at ground (cm)= ',a*2.0*100.
c       write(*,11) 'Stone size at ground (in)= ',a*2.0*100./2.54
 11    format(a30,f8.2)
*****   CONVERT a TO CENTIMETERS
       a=a*2.0*100.

       return
       end




