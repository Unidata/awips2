      subroutine forecast(yr,mon,day,hr,min,stnid,snow,slat,slon,
     & p,ht,t,td,nlvls,ftmax,status)
      implicit none

C##############################################################################
C Statement of purpose.
C ---------------------
C Forecast the maximum temperature for a sounding station given the 12Z
C sounding and the thermodynamic profile.
C
C History.
C --------                    
C R. Hanas       ????         University of Alaska original code.
C T. Schlatter   Summer 1982  Updated original code.  Added different
C                             radiation processes and considerable
C                             documentation.
C D. Baker       Jan 1986     Updated code to operate for all domestic
C                             sounding sites.  Modularized/streamlined
C                             code for better efficiency.
C D. Perry       Sep 96       Adapted code for WFO;  Revised data stmts
C                             to account for RAOBs not listed;  changed
C                             method for determining date given WFO constructs
C
C Description of input and output.
C --------------------------------
C On input:
C ---------                
C yr,mon,day  Integer       date of the sounding data.
C hr,min      Integer       time of the sounding data.
C stnid       Character     ICAO station identifier.
C snow        Integer       Snow depth on ground at time of sounding (in).
C slat        Real          Station latitude (deg).
C slat        Real          Station longitude (deg neg west of Greenwich).
C p           Real          Pressure data of sounding (mb).
C ht          Real          Height data of sounding (m).
C t           Real          Temperature data of sounding (K).
C td          Real          Dewpoint data of sounding (K).
C nlvls       Integer       Number of sounding data levels.
C
C On output:
C ----------               
C ftmax       Real          Forecast maximum temperature (K).
C status      Integer       Return status.
C
C User notes:
C -----------
C 1) The maximum temperature in this model is computed based only on 
C    radiative heating/cooling therefore the technique may be expected
C    to perform poorly under the following conditions:
C       a) over or near large bodies of water due to lack of heat
C          storage
C       b) over wet soil due to lack of evaportation
C       c) when a different air mass is advected into the area during
C          the time of radiative heating (i.e. between 12Z and mid
C          afternoon)
C       d) when an inversion is eroded away by mountain wave activity
C          (dynamically) rather than by solar heating (thermodynamically).
C##############################################################################

C     Input arguments.

      integer yr,mon,day,hr,min,nlvls,snow
      character stnid*4
      real p(nlvls),ht(nlvls),t(nlvls),td(nlvls)
      real slat,slon

C     Output arguments.

      real ftmax
      integer status

C     Local variables.

      integer nr
      parameter (nr=67)
      real pp(nlvls),htt(nlvls),tt(nlvls),tdd(nlvls),deltaz(nlvls)
      real tmax(12,nr),tmin(12,nr),albmax(nr),albmin(nr)
      real tc(nlvls),tdc(nlvls)
c unused variable      character months(12)*3
      character ids(nr)*4
      real endlvl(3),cover(3),asol,max_alt,min_alt
      real thi,tlo,drange,trng,hour,tstart,tstop,decl,haset
      real tset,tsrad,albdo,albdhi,albdlo,sfct,sfcp,sfctd,tcld
      real stemp,ulw,vpmm,dlwclr,a,b,cldcor,blw,flw,sw,heat
      real esat,cnv,hilat
      integer mrh(3),julday
      integer lvl,sta,istatus,i
      integer*2 t_julday, t_mon, t_tyminc, t_tstart, t_tstop
      real*4 t_slat, t_tsrad

C     Constants.

      integer nhum,nclyr
      real tyminc,emis,grav,cp
      real cmbd,sbc,pi,sdecmx,veday
      real yrlen,deltap,cl,cm,ch
      parameter (nhum=15)         !number humidity levels for transmissivity
      parameter (nclyr=3)         !# layers for computing RH/cloud info
      parameter (tyminc=30.0)     !time interval (min) for summing solar energy
      parameter (emis=0.95)       !surface emissivity
      parameter (grav=980.0)      !acceleration due to gravity
      parameter (cp=0.24)         !specific heat of air @ constant p (cal/gm/K)
      parameter (cmbd=1000.0)     !number dynes/sq cm in 1 millibar
      parameter (sbc=8.12E-11)    !Stefan-Boltzmann constant
      parameter (pi=3.1415927)    !value of constant PI
      parameter (sdecmx=0.40927)  !maximum solar declanation (radians)
      parameter (veday=79.75)     !average julian day of vernal equinox
      parameter (yrlen=365.2563)  !length of year in days
      parameter (deltap=10.0)     !pressure interval for interpolation (mb)
      parameter (cl=0.80)         !empirical coefficient for low clouds
      parameter (cm=0.67)         !empirical coefficient for middle clouds
      parameter (ch=0.22)         !empirical coefficient for high clouds

C     Average maximum temperature by month for each raob station.

C         Month 01 02 03 04 05 06 07 08 09 10 11 12
      data tmax/76,77,79,83,85,88,89,90,88,84,80,76, !EYW 72201 1
     &          75,76,79,83,86,88,90,90,88,84,80,76, !PBI 72203 2
     &          60,62,68,76,83,88,89,89,85,77,68,61, !CHS 72208 3
     &          70,72,76,82,88,90,90,90,89,84,77,72, !TBW 72210 4
     &          00,00,00,00,00,00,00,00,00,00,00,00, !AYS 72213 5
     &          61,63,68,75,82,86,88,88,85,78,69,63, !AQQ 72220 6
     &          58,61,68,77,84,89,91,91,87,78,67,59, !CKL 72229 7
     &          62,65,70,78,85,90,90,91,87,80,70,64, !BVE 72232 8
     &          60,62,69,78,85,91,93,93,88,80,69,61, !JAN 72235 9
     &          62,65,70,78,84,90,91,91,88,82,71,64, !LCH 72240 10
     &          00,00,00,00,00,00,00,00,00,00,00,00, !GGG 72247 11
     &          70,73,77,83,87,91,93,93,90,85,78,72, !BRO 72250 12
     &          63,67,72,80,85,91,93,94,89,83,73,67, !VCT 72255 13
     &          00,00,00,00,00,00,00,00,00,00,00,00, !SEP 72260 14
     &          63,69,76,85,90,96,99,99,92,83,72,65, !DRT 72261 15
     &          58,62,69,79,87,93,95,94,88,79,68,60, !MAF 72265 16
     &          57,63,69,79,87,95,95,93,87,79,66,58, !ELP 72270 17
     &          65,67,72,81,90,98,98,95,93,84,72,69, !TUS 72274 18
     &          65,66,66,68,69,72,75,77,77,74,70,66, !SAN 72290 19
     &          52,53,58,66,74,81,84,83,80,71,63,55, !HAT 72304 20
     &          51,55,61,71,79,85,87,86,81,73,62,53, !AHN 72311 21
     &          52,53,61,72,79,86,88,87,82,72,62,52, !GSO 72318 22
     &          50,51,59,71,80,88,90,89,84,73,59,50, !BNA 72327 23
     &          50,54,62,74,81,89,93,93,86,76,62,52, !LIT 72340 24
     &          43,48,55,68,76,84,89,89,81,71,56,46, !UMN 72349 25
     &          48,53,60,72,79,87,93,93,85,74,61,51, !OKC 72353 26
     &          49,53,60,71,79,88,91,90,83,73,60,52, !AMA 72363 27
     &          47,53,59,70,80,90,92,90,83,72,57,48, !ABQ 72365 28
     &          46,53,60,70,80,90,94,91,85,73,58,47, !INW 72374 29
     &          00,00,00,00,00,00,00,00,00,00,00,00, !VBG 72393 30
     &          44,46,55,67,77,85,88,87,80,70,57,45, !IAD 72403 31
     &          41,43,51,62,72,81,85,83,77,68,56,44, !ACY 72407 32
     &          43,45,55,68,76,83,86,85,79,69,55,45, !HTS 72425 33
     &          40,43,52,66,75,84,87,86,80,69,53,42, !DAY 72429 34
     &          42,45,55,68,77,86,89,88,81,71,55,44, !SLO 72433 35
     &          43,47,54,67,76,86,91,90,81,71,55,45, !DDC 72451 36
     &          38,44,53,66,76,84,89,89,80,70,54,42, !TOP 72456 37
     &          44,46,50,61,70,80,87,86,78,67,53,46, !DEN 72469 38
     &          37,44,53,65,76,86,93,89,81,68,51,39, !GJT 72476 39
     &          00,00,00,00,00,00,00,00,00,00,00,00, !OAK 72493 40
     &          30,33,43,58,70,79,84,81,74,63,48,34, !ALB 72518 41
     &          37,40,49,63,72,81,84,83,77,66,52,40, !PIT 72520 42
     &          30,31,39,53,64,75,80,78,71,60,46,34, !BUF 72528 43
     &          32,26,46,62,72,82,86,86,84,76,66,48, !PIA 72532 44
     &          33,39,48,64,74,83,89,87,79,69,51,38, !OMA 72553 45
     &          37,41,47,61,71,81,88,87,77,67,51,40, !LBF 72562 46
     &          37,43,51,62,72,81,93,90,80,66,50,39, !SLC 72572 47
     &          31,38,44,55,66,75,86,84,73,60,43,34, !LND 72576 48
     &          41,47,52,61,70,79,91,89,80,67,52,43, !WMC 72583 49
     &          44,52,57,64,72,79,90,88,82,67,53,44, !MFR 72597 50
     &          31,33,41,53,64,73,79,78,70,60,48,35, !PWM 72606 51
     &          30,32,41,57,67,77,81,80,72,62,46,34, !FNT 72637 52
     &          24,27,37,54,66,76,81,79,70,60,42,29, !GRB 72645 53
     &          23,29,39,58,70,79,87,86,74,63,43,29, !HON 72654 54
     &          19,24,36,54,67,76,82,80,69,59,39,25, !STC 72655 55
     &          34,38,43,57,67,76,86,86,75,64,48,38, !RAP 72662 56
     &          37,44,52,61,71,78,91,88,78,65,49,39, !BOI 72681 57
     &          45,51,55,61,68,74,82,81,77,64,53,47, !SLE 72694 58
     &          20,23,33,46,60,70,76,73,65,53,38,24, !CAR 72712 59
     &          22,24,33,47,59,70,75,73,65,55,39,27, !SSM 72734 60
     &          13,19,32,49,63,72,78,76,64,54,33,18, !INL 72747 61
     &          19,25,35,55,67,76,84,84,71,60,39,26, !BIS 72764 62
     &          19,25,36,55,67,74,84,83,70,59,39,27, !GGW 72768 63
     &          29,36,40,55,65,72,84,82,70,59,43,35, !GTF 72775 64
     &          31,39,46,57,67,74,84,82,73,58,42,34, !GEG 72785 65
     &          44,48,49,54,60,63,68,68,66,59,51,46, !UIL 72797 66
     &          00,00,00,00,00,00,00,00,00,00,00,00/ !Others    67

C Average minimum temperature by month for each raob station.

C         Month 01 02 03 04 05 06 07 08 09 10 11 12
      data tmin/66,67,70,74,76,79,80,80,79,75,71,67, !EYW 72201 1
     &          56,56,60,65,69,73,74,74,75,70,63,57, !PBI 72203 2
     &          37,39,45,53,61,68,71,71,66,55,44,38, !CHS 72208 3
     &          50,52,56,62,67,72,74,74,73,66,56,51, !TBW 72210 4
     &          00,00,00,00,00,00,00,00,00,00,00,00, !AYS 72213 5
     &          46,49,54,62,68,74,75,75,72,63,53,48, !AQQ 72220 6
     &          37,40,45,54,61,69,72,71,66,54,43,38, !CKL 72229 7
     &          44,46,51,59,65,71,73,73,70,60,50,45, !BVE 72232 8
     &          39,38,43,53,60,68,71,70,64,52,42,37, !JAN 72235 9
     &          43,46,51,60,66,72,74,73,69,58,49,44, !LCH 72240 10
     &          00,00,00,00,00,00,00,00,00,00,00,00, !GGG 72247 11
     &          51,54,59,67,71,75,76,76,73,67,59,53, !BRO 72250 12
     &          44,47,52,62,68,74,75,75,71,62,52,46, !VCT 72255 13
     &          00,00,00,00,00,00,00,00,00,00,00,00, !SEP 72260 14
     &          38,43,49,59,66,72,74,74,69,59,47,40, !DRT 72261 15
     &          29,34,39,49,58,67,70,69,63,52,39,32, !MAF 72265 16
     &          30,34,40,49,57,66,70,68,61,50,37,31, !ELP 72270 17
     &          38,40,44,50,58,66,74,72,67,56,45,35, !TUS 72274 18
     &          45,48,50,54,57,60,64,65,63,58,52,47, !SAN 72290 19
     &          38,39,43,52,60,68,72,72,68,59,49,41, !HAT 72304 20
     &          33,36,41,51,59,67,69,69,63,52,41,34, !AHN 72311 21
     &          32,31,37,47,55,63,67,66,60,48,38,31, !GSO 72318 22
     &          32,31,38,49,57,66,69,68,61,49,38,31, !BNA 72327 23
     &          29,32,39,50,58,67,70,69,61,48,38,31, !LIT 72340 24
     &          23,27,33,45,54,63,67,65,57,47,35,26, !UMN 72349 25
     &          26,30,37,49,58,67,70,70,61,51,37,29, !OKC 72353 26
     &          23,26,31,42,52,61,66,65,57,46,33,26, !AMA 72363 27
     &          24,27,32,41,51,60,65,63,57,45,32,25, !ABQ 72365 28
     &          20,25,29,37,46,54,63,62,54,41,28,21, !INW 72374 29
     &          00,00,00,00,00,00,00,00,00,00,00,00, !VBG 72393 30
     &          28,29,35,46,56,65,69,68,61,50,39,30, !IAD 72403 31
     &          24,25,32,41,51,60,65,64,57,46,36,26, !ACY 72407 32
     &          26,27,34,44,53,61,65,63,56,45,36,27, !HTS 72425 33
     &          24,26,34,45,54,63,66,64,57,47,36,27, !DAY 72429 34
     &          24,26,34,46,54,63,67,64,57,45,35,27, !SLO 72433 35
     &          19,23,28,41,52,61,67,66,56,45,30,22, !DDC 72451 36
     &          18,23,30,43,53,63,67,66,56,45,32,22, !TOP 72456 37
     &          16,19,24,34,44,52,59,57,48,37,25,19, !DEN 72469 38
     &          17,23,30,39,49,57,64,62,53,42,29,20, !GJT 72476 39
     &          00,00,00,00,00,00,00,00,00,00,00,00, !OAK 72493 40
     &          13,14,24,36,46,56,60,58,50,40,31,18, !ALB 72518 41
     &          24,24,32,42,52,62,65,63,56,45,37,27, !PIT 72520 42 
     &          18,18,25,36,46,56,61,59,52,43,34,22, !BUF 72528 43
     &          16,19,28,41,51,61,65,63,55,44,31,20, !PIA 72532 44
     &          12,17,26,40,52,61,66,64,54,43,29,18, !OMA 72553 45
     &          10,15,21,34,45,55,61,59,48,35,22,14, !LBF 72562 46
     &          19,23,28,37,44,51,61,59,49,38,28,22, !SLC 72572 47
     &          08,13,19,30,40,47,55,54,44,33,20,12, !LND 72576 48
     &          16,21,23,29,37,45,51,47,39,29,22,18, !WMC 72583 49
     &          29,31,33,37,43,49,54,53,47,39,34,31, !MFR 72597 50
     &          12,13,23,33,42,51,57,55,47,38,30,16, !PWM 72606 51
     &          15,16,24,35,45,55,58,57,50,41,31,20, !FNT 72637 52
     &          07,09,20,34,43,53,58,56,48,39,26,13, !GRB 72645 53
     &          02,07,19,34,44,55,61,59,47,36,21,09, !HON 72654 54
     &          -1,02,16,32,43,54,59,57,46,36,21,07, !STC 72655 55
     &          10,14,20,32,43,52,59,57,46,36,23,15, !RAP 72662 56
     &          21,27,31,37,44,51,59,57,49,39,31,25, !BOI 72681 57
     &          32,34,35,39,43,48,51,51,47,42,37,35, !SLE 72694 58
     &          02,03,14,28,39,49,54,52,43,35,25,08, !CAR 72712 59
     &          06,07,16,29,39,47,53,53,46,38,27,13, !SSM 72734 60
     &          -9,-6,09,27,38,48,53,51,42,33,17,-1, !INL 72747 61
     &          -3,02,15,31,42,52,57,55,44,33,18,05, !BIS 72764 62
     &          -1,05,15,31,42,50,57,55,44,34,19,08, !GGW 72768 63
     &          12,17,21,32,42,50,55,53,45,37,26,18, !GTF 72775 64
     &          20,25,29,35,43,49,55,54,47,38,29,24, !GEG 72785 65
     &          33,35,35,38,42,47,50,50,47,43,38,35, !UIL 72797 66
     &          00,00,00,00,00,00,00,00,00,00,00,00/ !Others    67

C     Array of station id's.

      data ids/'EYW','PBI','CHS','TBW','AYS','AQQ','CKL','BVE','JAN',
     &         'LCH','GGG','BRO','VCT','SEP','DRT','MAF','ELP','TUS',
     &         'SAN','HAT','AHN','GSO','BNA','LIT','UMN','OKC','AMA',
     &         'ABQ','INW','VBG','IAD','ACY','HTS','DAY','SLO','DDC',
     &         'TOP','DNR','GJT','OAK','ALB','PIT','BUF','PIA','OMA',
     &         'LBF','SLC','LND','WMC','MFR','PWM','FNT','GRB','HON',
     &         'STC','RAP','BOI','SLE','CAR','SSM','INL','BIS','GGW',
     &         'GTF','GEG','UIL','MISC'/

C     Average minimum albedo values (no snow cover).

      data albmin/10,15,13,16,13,12,16,13,15,14,
     &            16,16,16,16,17,19,19,20,15,14,
     &            15,15,14,15,16,17,18,16,14,14,
     &            15,15,14,16,16,16,17,20,17,14,
     &            14,14,16,16,15,17,20,20,14,16,
     &            15,16,16,18,14,18,11,15,15,16,
     &            16,14,15,17,15,15,14/

C     Maximum albedo values derived using maximum January snow cover.

      data albmax/10,15,13,16,13,12,33,13,15,16,
     &            30,16,16,17,17,20,19,20,15,33,
     &            35,35,40,37,37,42,42,37,55,14,
     &            43,43,40,52,37,52,52,50,53,14,
     &            38,40,50,37,55,52,40,65,65,16,
     &            48,45,60,55,50,52,44,31,40,70,
     &            37,65,65,65,45,38,14/

C     Array of months of the year.

c unused variable      data months/'JAN','FEB','MAR','APR','MAY','JUN',
c unused variable     &            'JUL','AUG','SEP','OCT','NOV','DEC'/

	  real t1, t2, t3, t4

C     Statement function for computing celsius diurnal temperature range given
C     Fahrenheit max and min temperature.
      drange(thi,tlo)=(0.55556*(thi-32.0))-(0.55556*(tlo-32.0))

C     Exit if we dont have lat/lon.
      if (slat.lt.-90.0 .or. slat.gt.90.0 .or.
     &    slon.lt.-180.0 .or. slon.gt.180.0) then
      ftmax=t(1) !Kelvin
      go to 9999
      end if

      hour=hr+(min/60.0)

C     Determine station index.

      do 100 i=1,nr
       if (stnid.ne.ids(i)) go to 100
       sta=i
       go to 101
100   continue
      sta=67
101   continue

C     Compute julian day.

      call cv_date2jul(yr,mon,day,julday,istatus)

C     Determine dirunal range (degrees celsius).

      trng=drange(tmax(mon,sta),tmin(mon,sta))
C     if (trng.lt.1.) go to 9999

C     Compute the solar time corresponding to the sounding time.  The equation
C     of time is neglected.

      tstart=hour+(slon/15)

C     Compute the solar time at sunset.  The equation of time is neglected.

      decl=sdecmx*sin(2*pi*(julday-veday)/yrlen)

      hilat=0
      
C     Check for high latitude conditions where the sun never rises
C     nor sets.  If so, tsrad is assumed to be undefined, and set to 0.
C     Note:  This algorithm was derived for mid-latitudes.  This patch
C     attempts to correct this problem, but this needs to be looked at
C     in detail later.  Perhaps a more rigorous way to determine the
C     insolation at high latitudes during these two situations.  DRP
      
C     Define the angle of the maximum sun height
      
      max_alt = (1.570796327-abs(slat)*pi/180)+decl
      
C     Define the angle of the minimum sun height
      
      min_alt = (abs(slat)*pi/180)-(1.570796327-decl)
      
      if ( (max_alt.lt.0.0).or.(min_alt.gt.0.0) ) then
         tsrad = 0.0
         hilat = 1
         goto 150
      end if
      haset=acos(tan(slat*pi/180)*tan(decl))
      tset=12+12*(haset/pi)

C     The solar input is allowed until 2 hours before sunset...subtract 2 hours.

      tstop=tset-2

C     Compute the total undepleted solar radiation at the top of the atmosphere.
C     Used temporary variables to match the types that solax() expected.      
      t_julday = julday
      t_mon = mon
      t_slat = slat
      t_tyminc = int(tyminc)
      t_tstart = int(tstart)
      t_tstop = int(tstop)
      call solax(t_julday,t_mon,t_slat,t_tyminc,t_tstart,t_tstop,
     +t_tsrad)
      tsrad = dble (t_tsrad)

C     Convert input temperatures & dewpoints from Kelvin to Celsius.
      
150   do 200 i=1,nlvls
       tc(i)=t(i)-273.15
       tdc(i)=td(i)-273.15
200   continue

C     Interpolate the pressure, height, temperature, and dew point to 10 mb
C     resolution.

      call eqp(deltap,p,ht,tc,tdc,nlvls,pp,htt,tt,tdd,lvl)
      
C     Initialize surface conditions.

      sfcp=pp(1)
      sfct=tt(1)
      sfctd=tdd(1)

C     Calculate surface albedo.  Assume albedo varies linearly from maximum to
C     minimum values as the snow depth increases from zero to five inches.

      albdhi=albmax(sta)/100
      albdlo=albmin(sta)/100
      albdo=albdlo+0.2*snow*(albdhi-albdlo)
      if (albdo.gt.albdhi) albdo=albdhi

C     Compute the boundaries for the three levels used to compute mean values of
C     relative humidity and clouds.

      endlvl(1)=sfcp-150
      endlvl(2)=sfcp-300
      endlvl(3)=sfcp-500

C     Compute the mean relative humidity (percent) for the three layers bounded
C     by the pressures SFCP, ENDLVL(1), ENDLVL(2), and, ENDLVL(3).

      call rhbar(endlvl,mrh,nclyr,sfcp,pp,tt,tdd)

C Compute cloud cover.

      tcld=0.0
      do 300 i=1,nclyr
       cover(i)=0.0001*mrh(i)*mrh(i)
       tcld=amax1(tcld,cover(i))
300   continue      

C

      stemp=sfct+(0.5*trng)+273.15

C

      ulw=emis*sbc*(stemp*stemp*stemp*stemp)

C

      a=0.61
      b=0.05
      vpmm=0.75*esat(sfctd)
      dlwclr=ulw*(a+b*sqrt(vpmm))

C

      cldcor=1-0.01*(cl*mrh(1)+cm*mrh(2)+ch*mrh(3))
      if (cldcor.lt.0.0) cldcor=0.0

C

      flw=(ulw-dlwclr)*cldcor

C     Check for high latitude sites
      
      if (hilat.eq.1) then
         blw=flw
      else
         blw=flw*(tstop-tstart)*60
      end if

C

      sw=(1-albdo)*(1-tcld)*tsrad
C

      heat=sw-blw
      if (heat.le.0.0) then
       ftmax=sfct+273.15 !Kelvin
       go to 9999
      end if

C

      cnv=cp*deltap*(cmbd/grav)
      asol=heat/cnv

C     Compute thickness of each 10 mb layer.

      do 310 i=2,lvl
       deltaz(i-1)=htt(i)-htt(i-1)
310   continue

C     Compute the maximum temperature. Convert result from C back to K.

      call mxtp(asol,deltap,sfcp,pp(2),tt,deltaz,lvl,ftmax)
      ftmax=ftmax+273.15 !Kelvin

C Exit to main program.

9999  return
      end
