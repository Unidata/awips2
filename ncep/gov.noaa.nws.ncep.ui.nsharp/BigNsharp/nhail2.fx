      program ensemble
c     1/17/03 version SPC 2.0 nhail
      implicit none 
 
      double precision To, Tdo, T, Td, dT,Toff,count,I
      double precision D, Dmax,dmax1,dmax2,dmax3,Dmin, Dcont, Davc, 
     *davc1,davc2,davc3,bad,emb1,emb2,emb3,dmin1,dmin2,dmin3
      double precision ZBAS, TBAS,TOPT,TOPZ, CAPE, SHEAR, 
     *UPMAXV, UPMAXT, ESI,allavg,avgmax,avgmin,absmax
      integer Idate, unitnumber,IC,loop,nofroze
      character*8 cdate
      character*72 filename 
      REAL Do, hvars(12)
 
********************************************************* 
***  LIST OF VARIABLES 
********************************************************* 
***hvar1-hvar8: davc1-3,allavg,dmax1-3,avgmax(old ice)
***hvar2-hvar16: davc1-3,allavg,dmax1-3,avgmax(new ice scheme) 
***D = HAILSTONE DIAMETER (CM) 
***Dav = AVG. HAILSTONE DIAMETER 
***Dmax = MAX. HAILSTONE DIAMETER 
***Dmin = MIN. HAILSTONE DIAMETER 
***Dcont = CONTROL HAILSTONE DIAMETER 
 
***ZBAS = HEIGHT OF CLOUD BASE 
***TBAS = TEMP AT CLOUD BASE 
***TOPT = TEMP AT CLOUD TOP 
***TOPZ = HEIGHT OF CLOUD TOP 
***CAPE = CONVECTIVE AVAILABLE ENERGY, WITH VIRTUAL TEMP 
***       CORRECTION 
***SHEAR = VERTICAL WIND SHEAR BETWEEN 850 mb and 6 km 
***UPMAXV = MAX UPDRAFT VELOCITY 
***UPMAXT = TEMP AT LEVEL OF MAXIMUM UPDRAFT VELOCITY 
***ESI = ENERGY SHEAR INDEX (CAPE*SHEAR) 
********************************************************** 
     
***   Specify the offset (Toff) and increment(dT) for 
***   ensemble calculations 
      parameter(Toff = 1.0, dT = 0.5) 

c      print *, " "
      read *, idate
      read *, To
      read *, Tdo

C     CONVERT INTEGER FILE NAME TO CHARACTER EQUIVALENT
      write (cdate,'(i8.8)') Idate  

      loop=0

c 2323  continue

c     Initialise values for hail diameter calculation.  
      D = 0.0 
      davc1=0
      davc2=0
      davc3=0
      Dcont = 0.0 
      bad=0
      count=0
      avgmax=0
      avgmin=0
      allavg=0
      absmax=0.0
      
1000  continue 

      davc=0
      Dmax = 0.0 
      Dmin = 10000.0 
      ic=0
      bad=0
      count=0
            
c     Run cloud and hail model over range of T and Td as 
c     determined by magnitued of the offset and increment 
c     respectively.  Also vary embryo diameter between 
c     300 and 900 microns, in increments of 200 microns 
 
      do T = To - Toff, To + Toff, dT 
         do Td = Tdo - Toff, Tdo + Toff, dT 
	    call hailcloud(T, Td, cdate) 
            call hailstone(UPMAXV, UPMAXT, D, ZBAS, TBAS, TOPT, 
     *      TOPZ, CAPE, SHEAR, cdate, Do, bad,count,loop,nofroze) 

      
c     convert cm to inches
            D=D/2.54  
	    
c     Calc the Energy Shear Index 
           ESI=(CAPE*SHEAR)/1000.0
           
                 
c     Calc sum of all ensembles for average D calc. 
  	    if((UPMAXV.gt.7.0).and.(nofroze.eq.1)) then
	    	DavC = DavC + D
		IC= IC + 1
        	endif
		if(D. gt. Dmax) Dmax = D
	        if((D. lt. Dmin).and.(bad.eq.0)) Dmin = D
	  enddo	
	 enddo
      
     
c     Calculate average hail size from all ensembles 
      if (count.gt.ic) then
       davc=0
       else
      DavC = DavC / (IC-count)
      endif
      
      if(Dmax.gt.absmax) absmax = Dmax
      
      if (Dmin.eq.10000.0) Dmin=0.0 

      if (loop.le.2) then  
      allavg=allavg+davc
      avgmax=avgmax+dmax
      avgmin=avgmin+dmin
      endif
     
      if (loop.eq.0) then
      davc1=davc
      dmax1=dmax      
      loop=(loop+1)
      goto 1000
      endif
      
      if (loop.eq.1) then
      davc2=davc
      dmax2=dmax
      loop=(loop+1)
      goto 1000
      endif 
      
      if (loop.eq.2) then
      allavg=allavg/(3)
      avgmax=avgmax/(3)
      avgmin=avgmin/(3)
      davc3=davc
      dmax3=dmax
      endif      
      
c     adjust bias
      dmax1=dmax1+1.0
      dmax2=dmax2+.6
      dmax3=dmax3+.1    
      
c      write(*,202) davc1,davc2,davc3,allavg,dmax1,dmax2,dmax3,
c     *avgmax
     
      hvars (1) = To
      hvars (2) = Tdo  
      hvars (3) = ic    
      hvars (4) = davc1
      hvars (5) = davc2
      hvars (6) = davc3
      hvars (7) = allavg
      hvars (8) = dmax1
      hvars (9) = dmax2
      hvars (10) = dmax3
      hvars (11) = avgmax
      
      do 9345 i=1, 11
       	   print *, i, hvars(i)
9345   continue

     

c202   format(f4.2,", ",f4.2,", ",f4.2,", ",f4.2,", ",f4.2,
c     *", ",f4.2,", ",f4.2,", ",f4.2,", ")      

      stop  
      end 
 
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
      SUBROUTINE HAILCLOUD(Tinput, Tdinput, cdate) 
 
 
      COMMON /DATA/ TFI(200,7),SOUNDNG(100,7),WOLKDTA(200,10), 
     *              TCA(200),R(200) 
      CHARACTER*72 filename, filename1 
      character*8 cdate
      CHARACTER*7 date, junk 
      double precision Tinput, Tdinput 
      integer unitnumber, IDATUM, ISTASIE
 
 
************************************************* 
***  CLOUD MODEL PARAMETERS: 
************************************************* 
 
***SOUNDNG(ITEL,1) = HEIGHT 
***SOUNDNG(ITEL,2) = PRESSURE 
***SOUNDNG(ITEL,4) = TEMPERATURE 
***SOUNDNG(ITEL,5) = DEWPOINT 
***SOUNDNG(ITEL,6) = WIND DIRECTION 
***SOUNDNG(ITEL,7) = WIND SPEED 
 
***To/TMAX = CONTROL TEMP 
***Tdo/TDOU = CONTROL DEWPOINT 
***CDATE = DATE USED AS IDENTIFIER FOR INPUT AND OUTPUT FILE 
***OPPDRUK = SURFACE PRESSURE 
 
***TFI= ARRAY FOR TFI DATA 
***SOUNDNG = ARRAY INTO WHICH INPUT SOUNDING DATA IS READ 
***WOLKDTA = ARRAY FOR CLOUD MODEL OUTPUT 
 
***R =  MIXING RATIO 
***RS = SATURATION MIXING RATIO 
***DIGTA = AIR DENSITY 
***CLWATER = CONDENSED CLOUD WATER 
***TCVIR = VIRTUAL PARCEL TEMPERATURE 
***VU =  UPDRFAT VELOCITY 
 
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
4     format(a8,".dat") 
  
**   OPEN OUTPUT DATA FILE TO BE USED BY HAIL MODEL 
      write(filename1,5) cdate           
      unitnumber=2 
      open(unit=2, file=filename1) 
5     format("c",a8,".dat") 
   
 
*** READ DATA FROM INPUT UPPER-AIR FILE. MAX FILE LENGTH 100 LINES 
*** FILE MUST INCLUDE 850, 500, 400 AND 100 hPa levels 
*** ODER OF READ STATEMENT MAY HAVE TO BE CHANGED DEPENDING ON 
*** FORMAT OF INPUT UPPER-AIR SOUNDING FILE 
 
        DO 10 ITEL=1,100 
        READ(1,*, END=15) SOUNDNG(ITEL,2), SOUNDNG(ITEL,1), 
     *  soundng(itel,4), soundng(itel, 5), 
     *  soundng(itel,6), soundng(itel, 7)  
        SOUNDNG(ITEL,3)=0.0 
	
c REJ  if dewpoint value off sounding is less than -99, set to -90	
c REJ  fixes NSHARP missing data flag, which makes value -9999.00	
	if (soundng(itel,5).lt.-99.00) then
	 soundng(itel,5)=-90
	 endif
 
*** CONVERT TEMPERATURE AND DEWPOINT TO KELVIN 
        SOUNDNG(ITEL,4)=SOUNDNG(ITEL,4) + 273.16 
	SOUNDNG(ITEL,5)=SOUNDNG(ITEL,5) + 273.16 
         
*** IF WIND SPEED IN KNOTS, THEN MUST CONVERT TO M/S 
*** NOT NECESSARY FOR NSHARP FILES 
c        SOUNDNG(ITEL,7)= SOUNDNG(ITEL,7)/2.0 
 
 
*** PRINT DATA READ FROM INPUT FILE TO SCREEN FOR CHECK 
c       PRINT *,SOUNDNG(ITEL,2), SOUNDNG(ITEL,4), SOUNDNG(ITEL,5), 
c     *  SOUNDNG(ITEL,6), SOUNDNG(ITEL,7) 
 
10    CONTINUE 
 
15    ITEL=ITEL-1 
 
 
 
*** INTERPOLATE A HIGHER RESOLUTION SOUNDING 
      CALL INTSOUN(TFI,SOUNDNG,ITEL,JTEL,ISTASIE,OPPDRUK) 
 
 
*** CALC THE CLOUD BASE PARAMETERS 
      CALL WOLKBAS(TFI,JTEL,WBASP,WBASRS,WBASTMP,WOLKDTA,TMAX, 
     *             TDOU,OPPDRUK) 
 
 
*** DETERMINE THE TYPE OF CLOUD USING CAPE AND VERTICAL WIND SHEAR 
*** I.E. ESI = SHEAR1*CAPE 
      CALL WOLKSRT(SOUNDNG,TFI,WBASP,WBASTMP,ITIPWLK,JTEL,CAPE 
     *             ,SHEAR1,RICH,PSEUDO,ITEL) 
 
 
 
*** CALC THE PARCEL'S TEMPERATURE, AFTER INCL. ENTRAINMENT FROM  
*** CLOUD BASE TO 300 HPA 
      CALL NATAD(WBASP,WBASTMP,WBASRS,TFI,JTEL,NTRAIN,WOLKDTA, 
     *           ITIPWLK,WMAX, BETA) 
 
 
 
*** WRITE OUT THE CONVECTIVE, KINEMATIC AND CLOUD DATA TO  
*** c*.dat FILE 
 
      WRITE(2,'(''*************   SP-CLOUD MODEL   **************'')') 
      IF(NTRAIN.EQ.1)WRITE(2,'(A10,2X,I5,''  CLOUDTOP ENTRAINMENT'')')  
     *                           CDATE,ISTASIE 
      IF(NTRAIN.EQ.2)WRITE(2,'(A10,2X,I5,''  LATERAL ENTRAINMENT'')') 
     *                           CDATE,ISTASIE 
      WRITE(2,'(''MAX TEMP    DEWPOINT'')') 
      WRITE(2,'(3X,F4.1,9X,F4.1)')TMAX-273.16,TDOU-273.16 
      WRITE(2, 
     *'('' CAPE  SHEAR  RICHSON  TYPECLD  PSEUDO'')') 
      WRITE(2,'(F7.1,3X,F6.3,5X,F6.0,4X,9X,I2,6X,F6.1)') 
     *               CAPE,SHEAR1*1000.0,RICH,ITIPWLK,PSEUDO 
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
 
666   CONTINUE 
 
 
      close(unit=1) 
      close(unit=2) 
  
      RETURN 
      END 
 
 
 
 
      SUBROUTINE INTSOUN(TFI,SOUNDNG,ITEL,JTEL,ISTASIE,OPPDRUK) 
 
**************************************************************** 
*** INTERPOLATE A HIGHER RESOLUTION SOUNDING 
**************************************************************** 
      DIMENSION ISTNAAM(10),ISTHGTE(10),STDRUK(10) 
      DIMENSION TFI(200,7),SOUNDNG(100,7) 
 
***   NO LONGER USE THESE DATA 
C      DATA ISTNAAM/71119,71877,71878,719999,68538,68461,68174, 
C     *             68588,68816,68842/ 
C      DATA ISTHGTE/900,1084,905,988,1686,1600,1000, 
C     *            1000,1000, 1000/ 
C      DATA STDRUK/910.0,864.0,900.0,880.0,880.0,840.0,840.0, 
C     *            900.0,900.0,900.0/ 
 
       
      I=1  
      JTEL=1 
      IHGTJA=0 
 
***WMO STATION NUMBER. STONY PLAIN, ALBERTA IN THIS CASE 
      ISTASIE=71119 
       
 
*** FIND THE STATION HEIGHT FROM THE INPUT SOUNDING 
          TFI(1,5)=SOUNDNG(1,1) 
          OPPDRUK=SOUNDNG(1,2)*100.0 
 
          IHGTJA=1 
          IPRES=10*(SOUNDNG(1,2)/10) 
          PVLAK=IPRES 
 
*** SEARCH FOR THE TWO LEVELS EACH SIDE OF P 
12    IF(I.LT.ITEL.AND.JTEL.LT.90)THEN 
 
 
10      IF(PVLAK.LE.SOUNDNG(I,2).AND.PVLAK.GT.SOUNDNG(I+1,2). 
     *                        AND.SOUNDNG(I+1,2).NE.0.0)THEN 
          PDIFF=SOUNDNG(I,2)-SOUNDNG(I+1,2) 
          VDIFF=SOUNDNG(I,2)-PVLAK 
          VERH=VDIFF/PDIFF 
          TDIFF=SOUNDNG(I+1,4)-SOUNDNG(I,4) 
          TDDIFF=SOUNDNG(I+1,5)-SOUNDNG(I,5) 
          TFI(JTEL,1)=PVLAK 
          TFI(JTEL,3)=SOUNDNG(I,4)+(TDIFF*VERH) 
          IF(SOUNDNG(I+1,4).GE.350.0)THEN 
            TFI(JTEL,4)=SOUNDNG(I+1,5) 
          ELSE 
            TFI(JTEL,4)=SOUNDNG(I,5)+(TDDIFF*VERH) 
          ENDIF 
          IF(PVLAK.EQ.SOUNDNG(I,2))THEN  
            TFI(JTEL,2)=SOUNDNG(I,3) 
            TFI(JTEL,4)=SOUNDNG(I,5) 
          ELSE 
            TFI(JTEL,2)=0.0 
          ENDIF 
          IF(IHGTJA.EQ.1)THEN  
            TFI(JTEL+1,5)=(287.04*(TFI(JTEL,3)+1.0)/(9.78956 
     *               *TFI(JTEL,1)*100.0)) * 1000.0 + TFI(JTEL,5) 
          ELSE 
            TFI(JTEL+1,5)=0.0  
          ENDIF 
          JTEL=JTEL+1 
          PVLAK=PVLAK-10.0 
        ELSE 
          I=I+1 
          GOTO 12 
        ENDIF 
 
        GOTO 12 
 
        ENDIF 
 
 
      IF(SOUNDNG(ITEL,2).LE.10.0)THEN 
        TFI(JTEL-1,4)=SOUNDNG(ITEL-1,4)  
        TFI(JTEL-1,5)=SOUNDNG(ITEL-1,5)  
      ENDIF 
 
      JTEL=JTEL-1 
 
      RETURN 
      END  
 
 
 
 
 
 
 
 
 
      FUNCTION XINTERP(TFI,P,JVAL,JTEL)  
**************************************************************** 
***      INTERPOLATE BETWEEN TWO LEVELS 
**************************************************************** 
      DIMENSION TFI(200,7) 
 
*** SEARCH FOR THE 2 LEVELS EACH SIDE OF P 
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
      DP=100.0 
      AL=(-0.566*(TD-273.16)+597.3)*4186.0 
      E=611.0*EXP((AL/RV)*(1.0/273.16-1.0/TD)) 
      R=EPS*E/P 
 
      P1=P 
      T1=TE 
 
 
*** CALC T ALONG THE DALR UNTIL RS=R I.E., T=TD, THE SATURATION POINT 
*** OR LCL 
 
      DO 10 I=1,500 
 
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
      DIMENSION TFI(200,7) 
 
      RV=461.48 
      EPS=0.622 
      RD=287.04 
      G=9.78956 
      DP=50.0 
      TC=TK 
      P=DR/100.0 
 
 
***   GET AMBIENT TEMP AND DEW-POINT IN KELVIN 
      TA=XINTERP(TFI,P, 3,JTEL) 
      TD=XINTERP(TFI, P, 4, JTEL) 
 
*** CALC. THE AIR DENSITY OF AMBIENT AIR, KG/M^3 
      DIGTA=(P*100.0/(RD*(1.0+0.608*RS/(1.0+RS))*(TA))) 
 
*** CALC THE Z-INCREMENT USING THE HYDROSTATIC EQUATION 
      DELZ=-100.0*(-DP)/(DIGTA*G) 
 
*** CALC THE TOTAL WATER CONTENT AT LEVEL P 
       CLWATER=WBASRS-RS 
 
*** CALC MIXING RATIO OF AMBIENT AIR 
      AL=(-0.566*(TD-273.16)+597.3)*4186.0 
      E=611.0*EXP((AL/RV)*(1.0/273.16-1.0/TD)) 
      P1=P*100. 
      R=EPS*E/P1 
 
*** CALC VIRTUAL TEMP OF THE AMBIENT AIR. 
      TAKELV=TA 
      VIRTA=(1.0+0.608*(R/(1.0+R))) 
      TAVIR=VIRTA*TAKELV 
 
*** CALC THE VIRTUAL TEMP. OF THE CLOUD/PARCEL 
      VIRT=(1.0+0.608*(RS/(1.0+RS))) 
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
     *                 ITIPWLK,WMAX, BETA) 
******************************************************************* 
*** CALC THE PARCEL'S TEMPERATURE AND MIXING RATIO. THE UPDRAFT VELOCITY 
*** IS THEN DETERMINED BY CALCULATING THE DIFFERENC EETWEEN THE PARCEL 
*** TEMP AND THAT OF THE AMBIENT AIR 
****************************************************************** 
      DIMENSION TFI(200,7),WOLKDTA(200,10) 
      CP=1004.64 
      RD=287.04 
      RV=461.48 
      EPS=0.622 
      DP=5000.0 
      G=9.78956 
      BL=2464759.0 
 
***   UPDRAFT VELOCITY AT CLOUD BASE IN M/S 
      WBASVU=4.0 
       
 
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
      DO 100 J=1,200 
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
        PRES = CSPP 
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
        IF(VU.LT.0.0)GOTO 70 
        IF(P.LE.TFI(JTEL,1)*100.0.OR.P.LE.10000.0)GOTO 70 
 
200   CONTINUE 
70    RETURN 
      END  
 
 
 
 
 
 
 
 
 
 
      FUNCTION XINTBAS(TFI,PP,JVAL,JTEL) 
******************************************************************* 
*** INTERPOLATION OF COUD BASE BETWEEN 2 LEVELS 
******************************************************************* 
      DIMENSION TFI(200,6) 
 
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
      DIMENSION TFI(200,7),WOLKDTA(200,10) 
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
          WOLKDTA(JJ,6)=RS*1000.0 
          WOLKDTA(JJ,8)=TFI(JJ,5) 
          JJ=JJ+1 
        ENDIF 
 
        IF(RS.LE.R)GOTO 10 
 
*** CALC THE TEMP AT THE NEXT LEVEL FOLLOWING THE DALR 
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
        WOLKDTA(JJ,6)=RS*1000.0 
        WOLKDTA(JJ,8)=XINTBAS(TFI,WBASP,5,JTEL) 
      ELSE 
 
*** CLOUD BASE NOT OBTAINED - AIR TOO DRY 
        WBASRS=9999  
        WBASTMP=9999 
        WBASP=9999 
      ENDIF 
      RETURN 
      END  
 
 
 
 
 
 
 
 
 
      SUBROUTINE WOLKSRT(SOUNDNG,TFI,WBASP,WBAST,ITIPWLK,JTEL, 
     *                   CAPE,SHEAR1,R,PSEUDO,ITEL) 
******************************************************************* 
*** USE SOUNDING DATA TO DETERMINE THE MOST LIKELY MODE OF CONVECTION 
*** ACCORDING TO THE AMOUNT OF CAPE AND VERTICAL WIND SHEAR. POSSIBLE  
*** MODES OF CONVECTION ARE CUMULUS, AIR-MASS THUNDERSTORM, 
*** MULTI-CELL THUNDERSTORM AND SUPERCELL THUNDERSTORM 
******************************************************************* 
      DIMENSION TFI(200,7),WOLKDTA(200,10),SOUNDNG(100,7) 
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
      TO1OLD=0.0 
      T2BEGIN=0.0 
      T2OLD=0.0 
 
20    IF(PRES.GE.10000.0)THEN 
 
*** T1 IS PARCEL TEMP, TO1 IS AMBIENT TEMP 
 
        T1=TNAT 
***     SATURATION VAPOUR PRESSURE OF PARCEL AIR (IN Pa) 
        ES1=(6.112*EXP((17.67*(T1-273.16))/((T1-273.16)+243.5)))*100.0 
 
***     SATURATION MIXING RATIO OF PARCEL AIR 
        RS1=0.622*(ES1/PRES) 
 
***     VIRTUAL TEMPERATURE OF PARCEL AIR 
        T1=T1*(1.0+RS1*0.608) 
 
***     EXTRACT AMBIENT TEMP FROM THE SOUNDING FILE 
        TO1OLD=XINTERP(TFI,PRES/100.0,3,JTEL) 
        TO1=XINTERP(TFI,PRES/100.0,3,JTEL) 
 
***     EXTRACT AMBIENT DEWPOINT FROM THE SOUNDING FILE 
        TDO1=XINTERP(TFI,PRES/100.0,4,JTEL) 
 
***     SATURATION VAPOUR PRESSURE OF AMBIENT AIR 
        ESO1=(6.112*EXP((17.67*(TDO1-273.16))/((TDO1-273.16)+243.5)))*100.0 
 
***     MIXING RATIO OF AMBIENT AIR 
        RO1=0.622*(ESO1/PRES) 
 
***     VIRTUAL TEMPERATURE OF AMBIENT AIR AT LEVEL P 
        TO1=TO1*(1.0+RO1*0.608) 
 
***     CALC POTENTIAL TEMPERATURE OF PARCEL AND AMBIENT AIR 
        DP1000=PRES-100000.0 
        Q1=DATV(T1,PRES,DP1000) 
        QO1=DATV(TO1,PRES,DP1000) 
 
***     CALC VIRTUAL TEMPERATURE AT NEXT P LEVEL 
        T2BEGIN=TNAT 
        T2=XNATV(TNAT,PRES,DP) 
        T2OLD=T2 
        PRES=PRES-DP 
 
***     SATURATION VAPOUR PRESSURE AT NEXT LEVEL 
        ES2=(6.112*EXP((17.67*(T2-273.16))/((T2-273.16)+243.5)))*100.0 
 
***     SATURATION MIXING RATIO AT NEXT LEVEL 
        RS2=0.622*(ES2/PRES) 
 
***     VIRTUAL TEMPERATURE OF PARCEL AT NEXT LEVEL 
        T2=T2*(1.0+RS2*0.608) 
 
***     AS ABOVE, EXCEPT FOR AMBIENT AIR AT NEXT PRESSURE LEVEL 
        TO2=XINTERP(TFI,PRES/100.0,3,JTEL) 
        TO2OLD=TO2 
 
        TDO2=XINTERP(TFI,PRES/100.0,4,JTEL) 
        ESO2=(6.112*EXP((17.67*(TDO2-273.16))/((TDO2-273.16)+243.5)))*100.0 
        RO2=0.622*(ESO2/PRES) 
        TO2=TO2*(1.0+RO2*0.608) 
 
 
***     CALC POTENTIAL TEMPERATURE OF AMBIENT AND PARCEL AT NEXT P LEVEL 
        DP1000=PRES-100000.0 
        Q2=DATV(T2,PRES,DP1000) 
        QO2=DATV(TO2,PRES,DP1000) 
 
 
*** CALC THE CAPE IF THE PARCEL'S TEMP IS WARMER THAN THAT OF THE ENVIRONMENT 
*** THE CAPE IS CALC BY INTEGRATING THE POSITIVE AREA BETWEEN Q1,QO1,Q2,QO2 
        TEST=(0.5*RD*(T1+T2)*0.5/(0.5*(PRES+PRES+DP))* 
     *         ((Q2-QO2)/QO2 + (Q1-QO1)/QO1) *DP) 
 
c        IF(Q2.GE.QO2.AND.TEST.GE.0.0)THEN 
        IF(TEST.GE.0.0)THEN 
 
          CAPE=CAPE+(0.5*RD*(T1+T2)*0.5/(0.5*(PRES+PRES+DP))* 
     *         ((Q2-QO2)/QO2 + (Q1-QO1)/QO1) *DP)  
           
        ENDIF 
 
***SET PARCEL TEMP TO T2OLD 
        TNAT=T2OLD 
 
        GOTO 20 
      ENDIF 
 
 
 
 
 
*****************CALC THE VERTICAL WIND SHEAR***************** 
*** FIRST FIND THE WINDS AT 850, 500 AND 400 HPA 
      
       DO 100 I=1,ITEL-1 
          RIGTSFC=SOUNDNG(1,6) 
          SPOEDSF=SOUNDNG(1,7) 
        IF(SOUNDNG(I,2).EQ.850.0)THEN 
           RIGT850=SOUNDNG(I,6) 
           SPOED85=SOUNDNG(I,7) 
           H850=SOUNDNG(I,1) 
        ENDIF 
c make 850 wind surface wind if elevation < 850
	 if(soundng(1,2).lt.850) then
	  rigt850=soundng(1,6)
	  spoed85=soundng(1,7)
	 endif
          IF(SOUNDNG(I,2).EQ.700.0)THEN 
             RIGT700=SOUNDNG(I,6) 
             SPOED70=SOUNDNG(I,7) 
             H700=SOUNDNG(I,1) 
          ENDIF 
 
            IF(SOUNDNG(I,2).EQ.500.0)THEN 
                 RIGT500=SOUNDNG(I,6) 
                 SPOED50=SOUNDNG(I,7) 
                 H500=SOUNDNG(I,1) 
            ENDIF  
 
           IF(SOUNDNG(I,2).EQ.400.0)THEN 
              RIGT400=SOUNDNG(I,6) 
              SPOED40=SOUNDNG(I,7) 
              H400=SOUNDNG(I,1) 
           ENDIF 
100   CONTINUE 
 
 
*** CALC THE U AND V COMPONENTS IN M/S AT THE 
*** SURFACE, 850, 500 AND 400 hPa 
      USFC=-1.0*SPOEDSF*SIN(RIGTSFC*0.0174532) 
      VSFC=-1.0*SPOEDSF*COS(RIGTSFC*0.0174532) 
      U850=-1.0*SPOED85*SIN(RIGT850*0.0174532) 
      V850=-1.0*SPOED85*COS(RIGT850*0.0174532) 
      U700=-1.0*SPOED70*SIN(RIGT700*0.0174532) 
      V700=-1.0*SPOED70*COS(RIGT700*0.0174532) 
      U500=-1.0*SPOED50*SIN(RIGT500*0.0174532) 
      V500=-1.0*SPOED50*COS(RIGT500*0.0174532) 
      U400=-1.0*SPOED40*SIN(RIGT400*0.0174532) 
      V400=-1.0*SPOED40*COS(RIGT400*0.0174532) 
 
 
*** INTERPOLATE WINDS AT 600 hPa 
       U600=0.5*(U500+U400) 
       V600=0.5*(V500+V400) 
 
*** CALC SUM OF U AND V COMPONENTS OF WINDS AT  DIFF LEVELS 
       A=(U500-(U500-U400)*0.25) + U500 + U600+ U700 + U850 + USFC 
       B=(V500-(V500-V400)*0.25) + V500 + V600+ V700 + V850 + VSFC 
       UBO=A/6.0 
       VBO=B/6.0 
 
*** INTERPOLATE BETWEEN 400 AND 500 HPA TO CALC U AND V COMPONETS 
*** AT 6 KM ASL 
       UBO1= (U500-(U500-U400)*0.25) 
       VBO1= (V500-(V500-V400)*0.25) 
 
*** U AND V COMPONETS FOR LOWER LEVEL 
 
*** FOR BULK RICHARDSON NUMBER ALC, USE PREDETERMINED FRACTION OF  
*** THE 850 HPA WINDS-- THIS IS DONE TO APPROXIMATE THE MEAN WIND 
***   IN THE LOWEST 500 M AGL        
      UON=U850*0.935 
      VON=V850*0.935 
 
*** LOWER U AND V COMPONENTS FOR THE WIND SHEAR USED IN ESI CALC. 
      UON1=U850 
      VON1=V850 
 
*** CALC THE THICKNESS BETWEEN HEIGHT OF 850 HPA LEVEL AND 6 KM 
        DZ= (H500-(H500-H400)*0.25)-(H850*0.935) 
        DZ1= (H500-(H500-H400)*0.25)- H850 
          
*** WIND SHEAR USED FOR CAPE CALC = DU*DU + DV*DV 
      US=(UBO-UON)*(UBO-UON) + (VBO-VON)*(VBO-VON) 
 
*** WINDSHEAR CALC USING:  DV/DZ = DU/DZ I  +  DV/DZ J 
*** MAGNITUDE OF WIND SHEAR: 
*** DV/DZ = SQRT(DU/DZ * DU/DZ  +  DV/DZ * DV/DZ) 
 
*** WIND SHEAR USED TO CALC BULK RICHARDSON NUMBER (R) = SHEAR 
      SHEAR=SQRT(((UBO-UON)/DZ)*((UBO-UON)/DZ) + 
     *           ((VBO-VON)/DZ)*((VBO-VON)/DZ)) 
 
 
*** WIND SHEAR USED TO DETERMINE CLOUD TYPE, AMOUNT OF ENTRAINMENT 
*** AND UPDRAFT DURATION = SHEAR1 
      SHEAR1=SQRT(((UBO1-UON1)/DZ1)*((UBO1-UON1)/DZ1) + 
     *           ((VBO1-VON1)/DZ1)*((VBO1-VON1)/DZ1)) 
 
 
*** CALC THE BULK RICHARDSON NUMBER (R) 
      R=CAPE/(0.5*US) 
 
*** DEFINE THE TYPE OF CLOUD ACCORDING TO THE MAGNITUDE OF THE ENERGY 
*** SHEAR INDEX (SEE BRIMELOW 1999) 
 
      TYPE=CAPE*SHEAR1 
      IF(TYPE.LE.1.0)ITIPWLK=0 
      IF(TYPE.LE.3.0.AND.TYPE.GT.1.0)ITIPWLK=1 
      IF(TYPE.GT.3.0.AND.TYPE.LE.5.0) ITIPWLK=2 
      IF(TYPE.GT.5.0) ITIPWLK=3 
 
200   FORMAT(' CAPE=',F7.1,'  WIND SHEAR=',F7.5,'  TYPE CLOUD=',I1, 
     *       '  RICHARDSON NUMBER=',F7.1) 
      RETURN 
      END 
 
 
 
 
 
 
 
 
 
 
 
 
       SUBROUTINE HAILSTONE(UPMAXV, UPMAXT, D, ZBAS, TBAS, TOPT, 
     * TOPZ, CAPE, SHEAR, CDATE, Do, bad, count, loop,nofroze) 
 
***************************************************************** 
***   HAILCAST: ONE-DIMENSIONAL HAIL MODEL 
***   THE PROGRAM CALCULATES THE MAXIMUM EXPECTED HAIL DIAMETER 
***   AT THE SURFACE USING DATA FROM THE 1D CLOUD MODEL 
***************************************************************** 
      implicit double precision(A-H), double precision(O-Z) 
      DIMENSION TCA(100),R(100),VUU(100),DD(20),BEGTYD(20),TAA(100) 
      DIMENSION XX1(120),DUMMY(100),JST(6),ISTN(6),IHT(6),IPRES(6), 
     *          ZA(100) 
      CHARACTER*12 DTIPE(7) 
      integer nofroze
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
      INTEGER unitnumber, unitnumber1 
      INTEGER IWSE, IDATUM, ID, JUNK
      character*8 cdate
      DATA RV/461.48/,RD/287.04/,G/9.78956/ 
      DATA PI/3.141592654/,ALF/79.7/,ALV/597.3/ 
      DATA ALS/677.0/,CI/0.5/,CW/1./ 
      DATA DTIPE/'  NONE','   SHOT','   PEA','  GRAPE','  WALNUT', 
     *           '  GOLF',' >GOLF'/ 
      REAL Do 
 
*** THESE DATA ARE NO LONGER USED 
      DATA JST/71119,68442,68424,68242,68538,68461/ 
      DATA ISTN/3HWSE,3HBFN,3HUPN,3HMMA,3HDEA,3HBET/ 
      DATA IHT/766,1350,845,1282,1287,1681/ 
      DATA IPRES/925,864,900,880,880,840/ 
 
 
*** OPEN OUTPUT FILE FOR CLOUD MODEL DATA 
      write(filename,4) cdate 
      unitnumber=1 
      open(unit=unitnumber, file=filename) 
4     format("c",a8,".dat") 
 
 
*** OPEN OUTPUT FILE FOR HAIL MODEL DATA 
      write(filename1,5) cdate 
      unitnumber1=4 
      open(unit=unitnumber1, file=filename1) 
5     format("h",a8,".dat") 
 
 
 
c rej *** TIME STEP IN SECONDS 
      SEKDEL=1.0
 
 
 
******************** 1. INPUT DATA ****************************** 
***            READ OUTPUT DATA FROM THE CLOUD MODEL, FROM c*.dat 
***************************************************************** 
      CALL LEESDTA(PA,ZA,VUU,R,TAA,TCA,IDAT,WBASP,VBASIS, 
     *          ISTNR,ITEL,TMAXT,ITIPWLK, CAPE, SHEAR, UPMAXV, 
     *          UPMAXT, RICH, TDEW, ZBAS, TBAS, TOPT, TOPZ, Q) 
 
 
 
C     BEGIN TIME (SECONDS) 
      BEGTYD(1)=60. 
 
C     INITIAL HAIL EMBRYO DIAMETER IN MICRONS, AT CLOUD BASE  
      DD(1) = 300. 
 
C     UPPER LIMIT OF SIMULATION IN SECONDS 
      TAU=4200. 
 
C     STATION HEIGHT 
      STHGTE=ZA(1) 
 
      TAA(1)=TMAXT
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
3	CONTINUE 
 
 
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
c     *          "       VU        VT     SEC", 
c     *       " TIPE")') 
     
      
40    CONTINUE 
 
***   ADVANCE ONE TIME-STEP 
      SEK=SEK+SEKDEL 
 
 
**********************  2. CALCULATE PARAMETERS  ********************** 
***                 CALCULATE UPDRAFT PROPERTIES 
*********************************************************************** 
 
      CALL INTERP(VUU,VU,P,IFOUT) 
 
 
C	CALCULATE DURATION OF THE UPDRAFT ACCORDING TO THE PRODUCT OF CAPE*SHEAR 
        TIME=0.0 
        upIndex=5 
        TIME=CAPE*(SHEAR/1000.0) 
 
        IF(TIME.LT.1.0)TIME=1.0 
 
        IF(upIndex.eq.5.and.TIME.lt.5.0) THEN 
           DUR = (-2.5 * TIME**2 + 25.0 * TIME - 2.5)*60.0 
        ELSE 
          DUR=3600.0 
        ENDIF 
 
      ITIME = int(DUR) 
      IF(ITIME.LT.1200) ITIME = 1200 
 
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
 
 
*** IF HAILSTONE BELOW CLOUD CALC AMBIENT TEMP ALONG WALR 
*** COMMENT OUT START HERE TO REMOVE SUB CLOUD CHANGES *** 
C      IF(P.GT.PBEGIN.AND.SEK.GT.1200.)THEN 
C      TB=0.0 
C      TK=0.0 
C      DP=0.0 
 
C      CP=1004.64 
C      RD=287.04 
C      RV=461.48 
C      EPS=0.622 
 
C      P = P*100.0 
C      TK=TCBEGIN+273.16 
C      DP=P-(PBEGIN*100.0) 
 
 
*** CALC. THE LATENT HEAT 
C      AL=(-0.566*(TK-273.16)+597.3)*4186.0 
C      TB = TK 
 
*** CALC DIFFERENT PARTS OF THE EQUATION 
C      XP=611.0*EXP((AL/RV)*(1.0/273.16-1.0/TB)) 
C      A=1.0+(AL*EPS*XP/(RD*TB*P)) 
C      B=1.0+(EPS*EPS*AL*AL*XP/(CP*RD*P*TB*TB)) 
 
*** CALC AMBIENT TEMP AND SATURATION MIXING RATIO 
*** AT THE NEXT LEVEL BY FOLLOWING THE WALR 
C      TP=TK+(A/B)*((RD*TB*DP)/(P*CP)) 
C      TC=TP-273.16 
C      P = P/100.0 
*** SATURATION MIXING RATIO IS ASSUMED TO REMAIN CONSTANT BELOW CLOUD BASE 
*** AND IS SET EQUAL TO THE SAT. MIXING RATIO AT CLOUD BASE  
C      RSS=RSBEGIN/1000.0 
C      DENSA=(P*100./(RD*(TC+273.16)))/1000. 
C      ENDIF 
*** COMMENT OUT TO HERE TO REMOVE SUB CLOUD CHANGES ***  
 
c       if(loop.lt.3) then
c  rej  old cloud ice scheme       
c       PC=0.008*(1.274)**(-20.-TC)
c       IF(TC.GT.-20.)PC=0. 
c       IF(PC.GT.1.)PC=1. 
c       IF(PC.LT.0.)PC=0.
c       goto 2121
c       endif


***  CALCULATE PERCENTAGE OF FROZEN WATER USING SCHEME OF VALI AND STANSBURY 
***  TO REVERT TO OLD CLOUD ICE SCHEME REMOVE NEXT FOUR COMMENTS 
cc       PC=0.008*(1.274)**(-20.-TC) 
 
*** NO CLOUD ICE AT IN-CLOUD TEMPS WARMER THEN -20 C 
cc       IF(TC.GT.-20.)PC=0. 
cc       IF(PC.GT.1.)PC=1. 
cc       IF(PC.LT.0.)PC=0. 
 
**** CALC THE PERCENTAGE FROZEN CLOUD WATER USING SCHEME OF DANIELSEN (1971) 
c     MEAN CLOUD DROPLET DIAMETER IN CM 
      DROP=0.005 
c     VOLUME OF DROPLET 
      VOL=(4.0/3.0)*3.1415926*(DROP)**3.0 
      CK = 1.47*(EXP(-0.68*(TC+7.0))-1.0) 
c     CALC PERCENTAGE CLOUD ICE 
      PC=1-EXP(-1.0*VOL*CK) 
 
*** NO CLOUD ICE IF IN-CLOUD TEMP WARMER THAN -8 C 
      IF(TC.GT.-8.0)PC=0. 
      IF(PC.GT.1.)PC=1. 
      IF(PC.LT.0.)PC=0. 
*** COMMENT OUT TO HERE TO REVERT TO OLD CLOUD ICE 

c2121    continue
 
***  CALC. MIXING RATIO AT NEW P-LEVEL AND THEN NEW DENSITY OF IN-CLOUD AIR 
      CALL INTERP(R,RS,P,IFOUT) 
      IF(IFOUT.EQ.1)GOTO 100 
      RSS=RS/1000. 
      DENSA=(P*100./(RD*(1.+0.609*RSS/(1.+RSS))*(TC+273.16)))/1000. 

 
 
***  CALC. THE TOTAL WATER CONTENT IN THE CLOUD AT LEVEL P, ALSO CALC ADIABATIC 
***  VALUE 
      CALL WOLKWAT(XW,XI,CLWATER,CLADWAT,REENWAT,WWATER,WYS, 
     *  PC,WBASRS,RSS,TC,DENSA,ITIPWLK,SEK, WBASP, P,loop) 
 
 
**************  3. TEST FOR WET OR DRY GROWTH  ************** 
***       WET GROWTH - STONE'S SFC TEMP.GE.0, DRY - STONE'S SFC TEMP.LT.0 
 
c rej  define emb
c      if ((loop.eq.0).or.(loop.eq.3)) emb=0.07
c      if ((loop.eq.1).or.(loop.eq.4)) emb=0.16
c      if ((loop.eq.2).or.(loop.eq.5)) emb=0.25
      
      IF((TS.GE.-13).AND.(TC.GE.-13).AND.(NOFROZE.EQ.0)) GOTO 42       
      IF(TS.LT.0.)THEN 
***  DRY GROWTH 
        FW=0. 
        ITIPE=1 
        ELSE 
***  WET GROWTH 
        TS=0. 
        ITIPE=2 
      ENDIF 
  
***     FREEZE THE HAIL EMBRYO AT -12 DEGC, define emb

c rej  freeze at -12, define embryo at -12 level.
42      if((ts.ge.-13).and.(tc.ge.-13).and.(nofroze.eq.0))then
         IF((TC.lt.-11.0).AND.(tc.ge.-12).and.(v.gt.0))THEN 
	  if (loop.eq.0) d=0.07 
	  if (loop.eq.1) d=0.16 
	  if (loop.eq.2) d=0.25
	 endif
	 
	 iF(TC.Le.-12.0)THEN 
        
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
     *           DGMI,GMW,GMI,DI,SEKDEL,ITIPE,P,bad) 

C      *****WRITE OUTPUT DATA FROM HAIL MODEL TO FILE 
c  REJ  test if model gets "stuck" in wet growth after freezing 

       bad=0
      if(((ts.gt.0).and.(v.gt.0).and.(tc.lt.-12).and.
     *(tc.gt.-20).and.(itipe.eq.2)).or.((ts.eq.0).and.
     *(v.gt.0).and.(tc.lt.-40).and.(d.lt.1)))then
       count=count+1
        bad=1
	d=0
       endif    

c REJ fixes case where particle gets hung up near cloud base	 

      if((sek.gt.500).and.(v.lt.50).and.(v.gt.-50).and.
     *(vu.le.400).and.(bad.eq.0).and.(nofroze.eq.0)) then
        upmaxv=4
	upmaxt=tbas
	count=count+1
	bad=1
	d=0
       endif
       
c      IF(MOD(DINT(SEK/SEKDEL),DINT(30.0/SEKDEL)).EQ.0) 
c     *WRITE(4,71)TS,TC,D,P,FW,Z,V,VU,VT,SEK,ITIPE,NOFROZE
c71    FORMAT(F5.1,' ',F5.1,' ',F8.5,' ',F4.0,' ',F4.2,' ', 
c     *       F7.0,' ',F7.1,' ',F7.1,' ',F7.1,' ',F6.0, 
c     *       ' ',I2, '    ',I2)

 
*********TEST DIAMETER OF STONE AND HEIGHT ABOVE GROUND******** 
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
      IF(SEK.GE.TAU) GOTO 100 
 
***  GO BACK FOR PREVIOUS TIME STEP 
      GOTO 40 
 
***  WRITE VALUES OUT AND STOP RUN 
 
100   CONTINUE 
 
*** CONVERT TIME TO MINUTES 
      TTYD=SEK/60.0 
 
***  WRITE HAIL SIZES OUT 
***  IF FW=1.0 THEN HAIL HAS MELTED AND SET D TO 0.0 
      IF(DABS(FW - 1.0).LT.0.001) D=0.0 
 
 
c      WRITE(4,110)D,TTYD, emb,TMAXT, TDEW 
c110   FORMAT('Diameter=',F5.2,' cm   Time=',F5.1, 
c     *' min    Embryo=',F4.2,'   Max T/Td=',F4.1, 
c     * 1X, F4.1) 
 
 
 
C     CALCULATE CLOUD BASE INDEX; PRODUCT OF CLOUD BASE TEMP AND CLOUD BASE 
C     SATURATION MIXING RATIO 
      BINDEX=RSBEGIN*TCBEGIN 
 
       close(unit = 1) 
 
20    CONTINUE 
 
      RETURN 
      END 
 
 
 
      SUBROUTINE HEATBUD(TS,FW,TC,D,DENSA,GM1,DGM,VT,DELRW,DGMW, 
     *           DGMI,GMW,GMI,DI,SEKDEL,ITIPE,P,bad) 
****************************************************************** 
*** CALCULATE HAILSTONE'S HEAT BUDGET 
****************************************************************** 
      implicit double precision (A-H), double precision (O-Z) 
      DATA RV/461.48/,RD/287.04/,G/9.78956/ 
      DATA PI/3.141592654/,ALF/79.7/,ALV/597.3/ 
      DATA ALS/677.0/,CI/0.5/,CW/1./ 
 
***  CALCULATE THE CONSTANTS 
      AK=(5.8+0.0184*TC)*10.**(-5.) 
      TK=TC+273.15 
      ANU=1.717E-4*(393.0/(TK+120.0))*(TK/273.15)**1.5 
 
***   CALCULATE THE REYNOLDS NUMBER 
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
 
      implicit double precision(A-H), double precision (O-Z) 
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
     *           PC,WBASRS,RSS,TC,DENSA,ITIPWLK,SEK, WBASP, P,loop) 
 
      implicit double precision(A-H), double precision(O-Z) 
 
      CLWATER=WBASRS/1000.-RSS 
      CLADWAT=DENSA*CLWATER 
 
***  CLOUD ICE AND LIQUID WATER 
      WWATER=CLADWAT 
      WYS=PC*CLADWAT 
      XW=WWATER-WYS 
      XI=WYS 
      RETURN 
      END 
 
****************************************************************** 
***  INTERPOLATE VALUES OF RS AT LEVEL P BETWEEN 2 LEVELS OF R 
***  (AT LEVEL PP) 
****************************************************************** 
      SUBROUTINE INTERP(R,RS,P,IFOUT) 
 
      implicit double precision(A-H), double precision(O-Z) 
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
      implicit double precision(A-H), double precision(O-Z) 
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
      implicit double precision(A-H), double precision(O-Z) 
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
      W=DLOG10(GX) 
      Y= -1.7095 + 1.33438*W - 0.11591*(W**2.0) 
      RE=10**Y 
      VT=ANU*RE/(DD*DENSASI) 
      GOTO 70 
20    CONTINUE 
 
      W=DLOG10(GX) 
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
 
C     CONVERT TERMINAL VELOCITY TO CM/S 
      VT=VT*100. 
 
      RE1=RE 
      RETURN 
      END 
 
************************************************************** 
***  TEST IF AMOUNT OF WATER ON SURFACE EXCEEDS CRTICAL LIMIT- 
***  IF SO INVOKE SHEDDING SCHEME 
************************************************************** 
      SUBROUTINE BREAKUP(DENSE,D,GM,FW) 
      implicit double precision(A-H), double precision(O-Z) 
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
 
 
 
***************************************************************** 
***  READ IN THE OUTPUT DATA FROM THE CLOUD MODEL              *** 
***************************************************************** 
      SUBROUTINE LEESDTA(PA,ZA,VUU,R,TAA,TCA,IDAT,WBASP,VBASIS, 
     *          ISTNR,ITEL,TMAXT,ITIPWLK, CAPE, SHEAR, UPMAXV, 
     *          UPMAXT, RICH, TDEW, ZBAS, TBAS, TOPT, TOPZ, Q) 
 
      implicit double precision(A-H), double precision(O-Z) 
      DIMENSION TCA(100),R(100),VUU(100),TAA(100), 
     *          PA(100),ZA(100) 
 
 
      READ(1,*) 
      READ(1,'(I10,2X,I5)')IDAT,ISTNR 
      READ(1,*) 
      READ(1,*)TMAXT,TDEW 
      READ(1,*) 
      READ(1,*)CAPE,SHEAR,RICH,ITIPWLK,PSEUDO 
      READ(1,*) 
 
      DO 10 I=1,100 
        READ(1,*,END=20,ERR=20)PA(I),HGTE,TAA(I),DUM,TCA(I), 
     *                  R(I),VUU(I),ZA(I) 
        IF(HGTE.NE.0)WBASP=PA(I) 
        IF(HGTE.NE.0)VBEGIN=VUU(I) 
        IF(HGTE.NE.0)ZBAS=ZA(I) 
        IF(HGTE.NE.0)TBAS=TCA(I) 
 
*** CONVERT VUU TO CM/S!! 
       VUU(I)=VUU(I)*100.0 
 
10    CONTINUE 
20    ITEL=I 
 
 
*** CALC SATURATION MIXING RATIO (Q) OF AIR AT SURFACE 
      E=100.0*(6.112*EXP((17.67*TDEW)/(TDEW+243.5))) 
      Q=0.622*(E/(PA(1)*100.0)) 
 
 
        UPMAXV=0.0 
 
        DO 30 K=1,ITEL 
 
*** CALC P, T OF MAX UPDRAFT VELOCITY 
         IF(UPMAXV.LE.VUU(K))THEN 
            UPMAXV=VUU(K) 
            UPMAXT=TCA(K) 
            UPMAXP=PA(K) 
         ENDIF 
	

       
         IF(VUU(K).LT.0.0)THEN 
 
***      CALC CLOUD TOP HEIGHT, TEMP AND PRESSURE 
         VUU(K)=VUU(K)/100. 
         VUU(K-1)=VUU(K-1)/100. 
         DELTAV=ABS((VUU(K)-VUU(K-1))/(ZA(K)-ZA(K-1))) 
         DELTAZA= (VUU(K-1))/DELTAV 
          
         TOPZ=ZA(K-1)+DELTAZA 
         TOPT=TCA(K) 
         TOPP=PA(K) 
          
         VUU(K)=VUU(K)/100. 
         VUU(K-1)=VUU(K-1)/100. 
          
         GOTO 35 
         ENDIF 
 
 
30      CONTINUE 
 
***	MAX UPDRAFT VELOCITY TO M/S 
35      UPMAXV=UPMAXV/100.0 
 
 
      RETURN 
      END 
 
