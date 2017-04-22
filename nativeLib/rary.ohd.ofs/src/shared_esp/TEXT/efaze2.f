C$PRAGMA C (ECOPY)
C MODULE EFAZE2
C-----------------------------------------------------------------------
C
      SUBROUTINE EFAZE2 (MC,C,MD,D,MP,P,MT,T,MTSESP,TSESP,MPESP,PESP,
     1 MSPESP,SPESP,MA,A)
C
C   THIS SUBROUTINE DOES THE EXECUTION FOR ESP ONE SEGMENT AT A TIME.
C
C   THIS SUBROUTINE WAS ORIGINALLY WRITTEN BY GERALD N. DAY .
C     MODIFIED 12/19/84 BY GEORGE F. SMITH BECAUSE OF
C      CHANGES IN FDRIVE CAUSED BY NEW RUN TIME MODS.
C
      EXTERNAL KKTRIM
      LOGICAL LBUG,KBUG
C
      INCLUDE 'common/fdbug'
      INCLUDE 'common/ionum'
      INCLUDE 'common/killcd'
      INCLUDE 'common/fcrunc'
      INCLUDE 'common/where'
      INCLUDE 'common/fcsegn'
      INCLUDE 'common/fctime'
      INCLUDE 'common/errdat'
      INCLUDE 'common/flarys'
      INCLUDE 'common/fcary'
      INCLUDE 'common/etime'
      INCLUDE 'common/esprun'
      INCLUDE 'common/eswtch'
      INCLUDE 'common/espfle'
      INCLUDE 'common/espseg'
      INCLUDE 'common/evar'
      INCLUDE 'common/elimit'
      INCLUDE 'common/modscb'
      INCLUDE 'common/fpwarn'
      INCLUDE 'common/etsunt'
      INCLUDE 'common/egentr'
      INCLUDE 'common/fcunit'
      INCLUDE 'common/fcio'
      INCLUDE 'common/fcsegc'
      INCLUDE 'common/fts'
      INCLUDE 'clbcommon/ccfdim'
      INCLUDE 'clbcommon/chfdim'
cav ISTRT var used in ex19 and pack19      
      common/sn19flg/ISTRT
cfan
      common /leapyearcarryover/ileapyear,icday0229
cfan


      INTEGER T
      CHARACTER*128  FILNM
c SAT RTi, Sept 2003
c Added to pass a length to ecopy.
      INTEGER lsegn,bsegn,esegn
      CHARACTER*8 osegn
C
      PARAMETER (MAXC=3000)
      DIMENSION C1(MAXC),C2(MAXC),C3(MAXC)
      DIMENSION C(MC),D(MD),P(MP),T(MT),
     1 PESP(MPESP),TSESP(MTSESP),SPESP(MSPESP),A(MA),NDAYS(12)
      DIMENSION SBNAME(2),OLDOPN(2)

cew   the mecards array must be the same size as the modcrds array
cew   so dimension it to maxcrd

cew the iecday and iechour arrays must be as big as the icday and
cew ichour arrays

      dimension mecards(20,1000)
      dimension iecday(20), iechour(20)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_esp/RCS/efaze2.f,v $
     . $',                                                             '
     .$Id: efaze2.f,v 1.16 2006/05/22 12:15:38 xfan Exp $
     . $' /
C    ===================================================================
C
C
      DATA NDAYS/31,28,31,30,31,30,31,31,30,31,30,31/
      DATA SBNAME/4HEFAZ,4HE2  /,IGT/2HGT/,DEBUG/4HETIM/,DEBUG2/4HEMAX/
C
      NCVALS=1
C
      DO 41 I=1,10
         NVALSW(I)=0
41       CONTINUE
C
      DO 51 I=1,10
         ICFNUM(I)=KFCTMP(I)
51       CONTINUE
C
      IOLDOP=IOPNUM
      IOPNUM=0
      DO 10 I=1,2
      OLDOPN(I)=OPNAME(I)
10    OPNAME(I)=SBNAME(I)
C
      IF(ITRACE.GE.1) WRITE(IODBUG,20)
20    FORMAT(1H0,17H** EFAZE2 ENTERED)
C
      NCARDS=0
C
      LBUG=.FALSE.
      IF(IFBUG(DEBUG).EQ.0) GO TO 30
      LBUG=.TRUE.
30    CONTINUE
C
      KBUG=.FALSE.
      IF(IFBUG(DEBUG2).EQ.0) GO TO 40
      KBUG=.TRUE.
40    CONTINUE
C
      IF(MC.LE.MAXC) GO TO 60
      WRITE(IPR,50) MC
50    FORMAT(1H0,10X,37H**WARNING** C1,C2, C3 ARRAYS MUST BE ,
     1 18HREDIMENSIONED, MC=,I5)
C
60    IOPT=1
      NOPARM=0

cew   done with most of fs5file reads
cew   close fs5files

      call uclosl

cew save the original carryover dates so they are not lost when
cew the carryover dates are updated.

cew first check if -1 option is specified for this run
cew -1 option is not valid in ESP


	if(ncstor.eq.-1)then
	 ncstor=0

         write(ipr,*)'**WARNING** -1 OPTION FOR THE NUMCOSAV TECHNIQUE I
     +S NOT ALLOWED.'
	 write(ipr,*) 'NO CARRYOVER WILL BE SAVED FOR THIS RUN.'

	 call warn
        endif

	do 186,i=1,ncstor
	 iecday(i)=icday(i)
	 iechour(i)=ichour(i)
186     continue

C
C   MAIN LOOP - SEGMENT
C
      DO 660 ISEGEX=1,NSEGEX
C
C   FILL FCSEGN WITH SEGMENT INFO AND READ P,T,TS ARRAYS
C   FROM FORECAST FILES.
C
      CALL FGETSG(ID,IRSGEX(ISEGEX),MP,P,MT,T,MA,A,IOPT,
     1 NOPARM,IER)
      IF(IER.EQ.0) GO TO 80
      WRITE(IPR,70) ISEGEX
70    FORMAT(1H0,10X,46H**ERROR** ERROR CODE RETURNED FROM FGETSG FOR ,
     1 15HSEGMENT NUMBER ,I6)
      CALL ERROR
      GO TO 670
80    CONTINUE
C
      IESGRC=IEREC
      IF(IESGRC.NE.0) GO TO 100
         WRITE(IPR,90) IDSEGN
90    FORMAT('0**ERROR** SEGMENT ',2A4,' IS NOT DEFINED IN THE ',
     * ' ESP PARAMETER FILE.')
         CALL ERROR
         GO TO 670
C
C   FILL ESPSEG WITH SEG INFO AND READ TSESP,PSESP,AND
C   SPESP FROM ESP FILE
100   CALL ESPRDF(1,1,IESGRC,TSESP,MTSESP,PESP,MPESP,SPESP,MSPESP,IER)
      IF(IER.EQ.0) GO TO 130
      WRITE(IPR,110) ISEGEX
110   FORMAT(1H0,10X,46H**ERROR** ERROR CODE RETURNED FROM ESPRDF FOR ,
     1 15HSEGMENT NUMBER ,I6)
      WRITE(IPR,120) MPESP,MTSESP,MSPESP
120   FORMAT(1H0,10X,39HNOT ENOUGH SPACE TO DEFINE PESP, TSESP,,
     1 16H OR SPESP ARRAYS,/,11X,8HMPESP = ,I5,11H, MTSESP = ,
     2 I5,11H, MSPESP = ,I5)
      CALL ERROR
      GO TO 670
130   CONTINUE
C
C   FILL CB FLARYS FROM CB FCSEGN
C
      LTS=NTS
      LP=NP
      LC=NC
      LT=NT
      LD=ND
C
C   CALCULATE WORK SPACE
C
      NWORK=MD-IWKLOC+1
      IF(KBUG) WRITE(IODBUG,170) MD,IWKLOC,NWORK
170   FORMAT(10X,15HMD,IWKLOC,NWORK,5X,3I6)
C
C   SET ISEG IN CB WHERE FROM IDSEGN IN CB FCSEGN
C
      ISEG(1)=IDSEGN(1)
      ISEG(2)=IDSEGN(2)
C
C   GET ARGUMENTS,TECHNIQUES, AND MODS FROM HCL
C
      CALL FCARGS(MODCRD,NCARDS,MAXCRD,IER)
C
C     SUPPRESS MODS BY SETTING NCARDS TO ZERO
C
cew   mods now allowed.
cew      IF(NCARDS.EQ.0)GO TO 190
cew      NCARDS=0
cew      IF(MODWRN.EQ.0)GO TO 190
cew      WRITE(IPR,180)IRSGEX(ISEGEX)
cew180   FORMAT(1H0,10X,'** WARNING ** MODS CANNOT BE USED IN ESP - ',
cew     1 'THE MODS ENTERED FOR SEGMENT NUMBER ',I5,' ARE IGNORED.')
cew      CALL WARN
C
cew save the original mod cards into an array so they are not lost
cew when the mods are updated.
         do 185 i=1,ncards
           do 184 j=1,20
           mecards(j,i)=modcrd(j,i)
 184     continue
 185     continue
C
cew this error check is for the call to fcargs above
190   IF(IER.EQ.0) GO TO 210
      WRITE(IPR,200) ISEGEX
200   FORMAT(1H0,10X,34H**ERROR** ERROR IN SEGMENT NUMBER ,I6,/,
     1 11X,47HERROR IN GETTING TECHNIQUES, ARGUMENTS, OR MODS)
      CALL ERROR
      GO TO 670
C
C   PRINT INFO THAT VARIES BY SEGMENT
C
210   CALL ESGINF
C
cew
cew Moved this section of code down so that the error message
cew prints out with the correct segment name
cew
C   CHECK IF CREATION DATE OF ESP SEG IS AFTER CREATION DATE OF
C   FORECAST SEG. IF NOT CALL ECHKTS.
C
      ICRH=ICRDTE(4)/100
      ICKH=IECKDT(4)/100
      CALL FCTZC(100,0,TZC)
      CALL JULDA(KJDFC,KHFC,ICRDTE(1),ICRDTE(2),ICRDTE(3),ICRH,
     1 100,0,TZC)
CC    CALL FCTZC(100,0,TZC)
      CALL JULDA(KJDESP,KHESP,IECKDT(1),IECKDT(2),IECKDT(3),ICKH,
     1 100,0,TZC)
      CALL FDATCK(KJDESP,KHESP,KJDFC,KHFC,IGT,ISW)
      IF(ISW.EQ.1) GO TO 160
      CALL ECHKTS(A,MA,TSESP,MTSESP,IER)
      IF(IER.EQ.0) GO TO 160
      WRITE(IPR,140) ISEGEX
140   FORMAT(1H0,10X,46H**ERROR** ERROR CODE RETURNED FROM ECHKTS FOR ,
     1 15H SEGMENT NUMBER ,I6)
      WRITE(IPR,150)
150   FORMAT(1H0,10X,43HALL THE NECESSARY TIME SERIES HAVE NOT BEEN,
     1 8H DEFINED)
      CALL ERROR
      GO TO 670
160   CONTINUE
C
C
c
c This if has to do with the generation of historical data
c for the ESP Verification process
      if(igen.eq.0) then

C   CHECK FOR FIRST SEGMENT TO BE EXECUTED
C
      IF(ISEGEX.EQ.1) GO TO 220
C
C   PUT CO DATES IN CB FCSEGC
C
      CALL FCDATE(IDSEGN,0)
220   IF(IHFC.GT.0) GO TO 230
      IHFC=24
      IJDFC=IJDFC-1
C
C   GET CO VALUES
C
      if (ncops .gt. 0) then
230   CALL FGETCO (IDSEGN,IJDFC,IHFC,C,MAXC,'ERROR',IER)
      IF(IHFC.LT.24) GO TO 240
      IHFC=0
      IJDFC=IJDFC+1
240   IF(IER.EQ.0) GO TO 280
      WRITE(IPR,250) ISEGEX
250   FORMAT(1H0,10X,46H**ERROR** ERROR CODE RETURNED FROM FGETCO FOR ,
     1 15HSEGMENT NUMBER ,I6)
      CALL ERROR
C
      IF(IER.NE.1) GO TO 270
      WRITE(IPR,260)
260   FORMAT(1H0,10X,27HCONTINUE PROCESSING SEGMENT)

       endif
C
C   CHECK TIMING VARIABLES NOW THAT START OF RUN HAS BEEN CHANGED
C
      CALL ETMEX
      GO TO 280
C
270   CALL ERROR
      GO TO 670
280   CONTINUE

c
c  This is the rest of the if started from above
c   for the ESP verification process
      elseif (igen.eq.1) then
c
         call ecord(idsegn,ijdlst,ihlst,c,maxc,ier)

	 if (ier .ne. 0) then
      write(ipr,*)'** ERROR ** ERROR READING HISTORICAL CARRYOVER FILE.'
          go to 670
	 endif
c
      endif
c



C   MAKE NECESSARY CHANGES TO ADJUST-Q OPERATION ARRAYS
C
      IF(JASS.EQ.0) GO TO 290
      CALL EADJ(P,T,C,TSESP)
290   CONTINUE
C
C   INITIALIZE EXTRA CARRYOVER ARRAYS
C
      IF(JHSS.EQ.0.AND.JASS.EQ.0) GO TO 310
      DO 300 I=1,LC
      C1(I)=C(I)
      C2(I)=C(I)
300   CONTINUE
C
C   CALCULATE LENGTH OF CONDITIONAL SIMULATION
C
310   LCOND=LJDLST-IJDLST
C
C   CALCULATE INITIAL MONTH,DAY,YEAR
C
      CALL MDYH1(IDARUN,IHRRUN,IMONTH,IDAY,IYEAR,IHOUR,100,0,TZC)
      IF(IHOUR.LT.24) GO TO 320
      IHOUR=0
      IDAY=IDAY+1
      NUMDA=NDAYS(IMONTH)
      IF(IMONTH.EQ.2.AND.IYEAR.EQ.(4*(IYEAR/4))) NUMDA=NUMDA+1
      IF(IDAY.LE.NUMDA) GO TO 320
      IDAY=1
      IMONTH=IMONTH+1
      IF(IMONTH.LE.12) GO TO 320
      IMONTH=1
      IYEAR=IYEAR+1
C
C   CALCULATE LAST MONTH, DAY, YEAR
C
320   CALL MDYH1(LDARUN,LHRRUN,LMONTH,LDAY,LYEAR,LHOUR,100,0,TZC)
C
C   CALCULATE NUMBER OF SIMULATION YEARS
C
      LSTSIM=LDARUN-LCOND
      CALL MDYH1(LSTSIM,IHRRUN,LSIMMO,LSIMDA,LSIMYR,LSIMHR,100,0,TZC)
      IF(LSIMHR.LT.24) GO TO 330
      LSIMHR=0
      LSIMDA=LSIMDA+1
      NUMDA=NDAYS(LSIMMO)
      IF(LSIMMO.EQ.2.AND.LSIMYR.EQ.(4*(LSIMYR/4))) NUMDA=NUMDA+1
      IF(LSIMDA.LE.NUMDA) GO TO 330
      LSIMDA=1
      LSIMMO=LSIMMO+1
      IF(LSIMMO.LE.12) GO TO 330
      LSIMMO=1
      LSIMYR=LSIMYR+1
C
C   CALCULATE NYRS - NUMBER OF DATA POINTS
C
330   NYRS=LSIMYR-IYEAR+1
      IDIFFY=NYRS-(LHYR-IHYR)
      NBYRS=(LBHYR-IBHYR)+IDIFFY
C
C   CALCULATE MAXKNT - NUMBER OF LOOPS NEEDED
C   IF ALL THE WINDOWS ARE LESS THAN A YEAR MAXKNT=NYRS.
C   IF A WINDOW IS GREATER THAN A YEAR MAXKNT=NYRS+1 , SO
C   THAT THE PROGRAM CAN DO THE HISTORICAL AND ADJUSTED
C   SIMULATION FOR THE EXCESS GREATER THAN THE LAST YEAR.
C
      MAXKNT=NYRS
      NMCOND=(LYEAR*12+LMONTH)-(LSIMYR*12+LSIMMO)+1
      NYCOND=(NMCOND-1)/12+1
      IF(NYCOND.GT.1) MAXKNT=NYRS+1
C
C   SET UP THE ACCUMULATOR ARRAY
C
      CALL ESETA(NYRS,PESP,MPESP,MA)
C
      KNTYR=0
340   KNTMO=1
      NM=IMONTH
      NDAY=IDAY
      NY=IYEAR+KNTYR
      KNTYR=KNTYR+1
cav   Flag is set to 1 in pack19, reset to zero every first
cav month of the year.  This flag is set to determine snow
cav density (ds) which is used in calculating simulated snow depth
      istrt = 0      
C
      IF(KNTYR.GT.NYRS.AND.JHSS.EQ.0.AND.JASS.EQ.0) GO TO 620
C
      DO 350 I=1,LC
      C3(I)=C(I)
350   CONTINUE
C
C
C   CALCULATE FIRST DAY IN LOOP
C
C   IF FIRST DAY IS 2/29 AND THIS IS NOT A LEAP YEAR,
C   SET IDLOOP = 2/28 OF THE YEAR.
C
      IF(NM.NE.2.OR.NDAY.NE.29) GO TO 360
      IF(NY.EQ.(4*(NY/4))) GO TO 360
      NDAY=28
360   CALL FCTZC(100,0,TZC)
      CALL JULDA(IDLOOP,IHLOOP,NM,NDAY,NY,IHOUR,100,0,TZC)
C
cew  if ncstor >= 1 (ie save carryover), then call the routine to 
cew  update the carryover values to reflect the correct year.
cew  also zero out the carryover files in ecosav

      do 188 i=1,ncstor
       icday(i)=iecday(i) 
       ichour(i)=iechour(i) 
188   continue

       if(ncstor.ge.1) call ecosav(ncstor,icday,ichour,iecday,iechour,
     +              ijdlst,ihlst,idloop,ljdlst,lhlst,ierr)
      
      if(ierr .ne. 0) then
      WRITE(IPR,189) IDSEGN
189   FORMAT(1H0,10X,45H**WARNING** PROBLEM REWRITING CARRYOVER DATES,
     +        12HFOR SEGMENT ,A8,30H.  NO CARRYOVER WILL BE SAVED.)
          ncstor=0
           CALL WARN
      endif

cew write the original mod cards back into the mecards array
cew This is required because of the possibility that the
cew positions may be shifted in the modcrd array.

         do 202 i=1,ncards
           do 201 j=1,20
           modcrd(j,i)=mecards(j,i)
 201     continue
 202     continue


cew determine mods dates as dates from carryover date
cew  and make mods time as function of beginning of loop
      call emods(ncards,modcrd,MP,P,mc,c,mts,
     1            ts,mt,t,md,d,nxtopn,nxtnam,
     2            ihzero,ijdlst,idloop,mecards,ierr)

      if(ierr .ne. 0) then
      WRITE(IPR,281) IDSEGN
281   FORMAT(1H0,10X,37H**ERROR** ERROR REWRITING MODS DATES ,
     1           12HFOR SEGMENT ,A8)
           CALL ERROR
      endif

c   CALCULATE LAST CONDITIONAL DAY IN LOOP
C
      LJDCON=IDLOOP+LCOND
      IF(KNTYR.GT.NYRS) LJDCON=IDLOOP
C
C   RESET IDLOOP FOR CASE WHEN INITIAL HOUR IS 24
C
      IF(IHLOOP.LT.24) GO TO 370
      IDLOOP=IDLOOP+1
      IHLOOP=0
C
C   CALCULATE IDA
C
370   IEPASS=0
      CALL FCTZC(100,0,TZC)
      CALL JULDA(IDA,IHZERO,NM,1,NY,24,100,0,TZC)
      IHZERO=0
C
      IDADAT=IDA
C
C   CALCULATE LDA
C
      LDA=IDA+NDAYS(NM)-1
      LHR=24
      IF(NM.EQ.2.AND.NY.EQ.(4*(NY/4))) LDA=LDA+1
C
      IF(LDA.LT.LJDCON) GO TO 410
      IF(KNTYR.LE.NYRS) GO TO 380
C
C   HISTORICAL AND ADJUSTED SIMULATION EXCESS LOOP
C
      IF(LDA.LT.LDARUN) GO TO 410
      GO TO 400
C
C   REGULAR LOOP - CHECK IF THE HISTORICAL AND/OR ADJUSTED
C   ARE STILL NEEDED.
C
380   IF(JHSS.EQ.0.AND.JASS.EQ.0) GO TO 390
      IF(KNTMO.GT.12) GO TO 390
      IF(LDA.LT.LDARUN) GO TO 410
      GO TO 400
390   LDA=LJDCON
      LHR=LHRRUN
      GO TO 410
400   LDA=LDARUN
      LHR=LHRRUN
C
C   RESET IDA AND IHZERO
C
410   IF(IDA.GT.IDLOOP) GO TO 430
      IF(JASS.EQ.0.AND.JHSS.EQ.0) GO TO 420
      IF(IDA.GT.IDARUN) GO TO 430
420   IDA=IDLOOP
      IHZERO=IHLOOP
C
C   LOAD TS DATA
cew set D array to missing for the portion that is used by the time series.
430   do 438 idloc=1,iwkloc
438    d(idloc)=-999.0
C
      IF(LBUG) WRITE(IODBUG,440) IEPASS,IDLOOP,IHLOOP,LJDCON,KNTYR,
     1 NYRS,IDA,IHZERO,LDA,LHR
440   FORMAT(1H0,10X,4X,6HIEPASS,4X,6HIDLOOP,4X,6HIHLOOP,4X,
     1 6HLJDCON,5X,5HKNTYR,6X,4HNYRS,7X,3HIDA,4X,6HIHZERO,7X,3HLDA,
     2 7X,3HLHR/11X,10I10)
C

      CALL ETSRD(TSESP,MTSESP,D,IWKLOC,MD,IDLOOP,LJDCON,IHZERO,KNTYR,
     1 NYRS,IERR)
      IF(IERR.EQ.0) GO TO 460
      WRITE(IPR,450) ISEGEX
450   FORMAT(1H0,10X,47H**ERROR** ERROR IN LOADING TS DATA FOR SEGMENT ,
     1 6HNUMBER,I6)
      CALL ERROR
      GO TO 630
460   CONTINUE
C
C   ***   HISTORICAL SIMULATION   ***
C
      IF(JHSS.EQ.0) GO TO 520
      IF(KNTMO.GT.12.AND.KNTYR.LE.NYRS) GO TO 520
      IEPASS=1
      IF(LBUG) WRITE(IODBUG,440) IEPASS,IDLOOP,IHLOOP,LJDCON,KNTYR,
     1 NYRS,IDA,IHZERO,LDA,LHR
C
      CALL ETSRD(TSESP,MTSESP,D,IWKLOC,MD,IDLOOP,LJDCON,IHZERO,KNTYR,
     1 NYRS,IERR)
      IF(IERR.EQ.0) GO TO 480
      WRITE(IPR,450) ISEGEX
      CALL ERROR
      GO TO 630
480   NE=NERRS
c mods are not allowed for the historical simulation.  suppress them by
c setting ncards=0.  must reset ncards after hist simulation
      ncards_old=ncards
      ncards=0
C-----------------------------------------------------------------------
      CALL FDRIVE(P,MP,C1,MAXC,T,MT,TSESP,MTSESP,D,MD,IHZERO)
C-----------------------------------------------------------------------
      ncards=ncards_old

      IF(NE.EQ.NERRS) GO TO 500
      WRITE(IPR,490) ISEGEX
490   FORMAT(1H0,10X,47H**ERROR** ERROR OCCURRED IN DRIVER FOR SEGMENT ,
     1 7HNUMBER ,I6)
      CALL ERROR
      GO TO 630
C
C
500   CALL EACDRV(PESP,MPESP,IDLOOP,KNTYR,A,MA,D,IWKLOC,NWORK,NYRS,
     1 IHZERO)
C
      CALL ETSWT(TSESP,MTSESP,D,MD,KNTYR,kntmo,NYRS,ihzero,ljdcon,IERR)
      IF(IERR.EQ.0) GO TO 520
      WRITE(IPR,510) ISEGEX
510   FORMAT(1H0,10X,46H**ERROR** ERROR OCCURRED IN ETSWT FOR SEGMENT ,
     1 7HNUMBER ,I6)
      CALL ERROR
      GO TO 630
C
C   ***   ADJUSTED SIMULATION   ***
C
520   IF(JASS.EQ.0) GO TO 550
      IF(KNTMO.GT.12.AND.KNTYR.LE.NYRS) GO TO 550
      IEPASS=2
      IF(LBUG) WRITE(IODBUG,440) IEPASS,IDLOOP,IHLOOP,LJDCON,KNTYR,
     1 NYRS,IDA,IHZERO,LDA,LHR
C
      CALL ETSRD(TSESP,MTSESP,D,IWKLOC,MD,IDLOOP,LJDCON,IHZERO,KNTYR,
     1 NYRS,IERR)
      IF(IERR.EQ.0) GO TO 530
      WRITE(IPR,450) ISEGEX
      CALL ERROR
      GO TO 630
530   NE=NERRS
c mods are not allowed for the adjusted simulation.  suppress them by
c setting ncards=0.  must reset ncards after adjusted simulation
      ncards_old=ncards
      ncards=0
      ncstor_old=ncstor
      ncstor=0
C-----------------------------------------------------------------------
      CALL FDRIVE(P,MP,C2,MAXC,T,MT,TSESP,MTSESP,D,MD,IHZERO)
C-----------------------------------------------------------------------
      ncards=ncards_old
      ncstor=ncstor_old


      IF(NE.EQ.NERRS) GO TO 540
      WRITE(IPR,490) ISEGEX
      CALL ERROR
      GO TO 670
C
540   CALL EACDRV(PESP,MPESP,IDLOOP,KNTYR,A,MA,D,IWKLOC,NWORK,NYRS,
     1 IHZERO)
C
      CALL ETSWT(TSESP,MTSESP,D,MD,KNTYR,kntmo,NYRS,ihzero,ljdcon,IERR)
      IF(IERR.EQ.0) GO TO 550
      WRITE(IPR,510) ISEGEX
      CALL ERROR
      GO TO 630
C
550   IF(KNTYR.GT.NYRS) GO TO 600
      IF(IDA.GT.LJDCON) GO TO 600
      IF(LDA.LT.LJDCON) GO TO 560
      LDA=LJDCON
      LHR=LHRRUN
C
C   *** CONDITIONAL SIMULATION ***
C
560   IEPASS=3
C
      IF(IDA.GT.IDLOOP) GO TO 570
      IDA=IDLOOP
      IHZERO=IHLOOP
570   IF(LBUG) WRITE(IODBUG,440) IEPASS,IDLOOP,IHLOOP,LJDCON,KNTYR,
     1 NYRS,IDA,IHZERO,LDA,LHR
C
      CALL ETSRD(TSESP,MTSESP,D,IWKLOC,MD,IDLOOP,LJDCON,IHZERO,KNTYR,
     1 NYRS,IERR)
      IF(IERR.EQ.0) GO TO 580
      WRITE(IPR,450) ISEGEX
      CALL ERROR
      GO TO 630
580   NE=NERRS

cew  suppress saving carryover for conditional runs
      ncstor_old=ncstor
      ncstor=0
C-----------------------------------------------------------------------
      CALL FDRIVE(P,MP,C3,MAXC,T,MT,TSESP,MTSESP,D,MD,IHZERO)
C-----------------------------------------------------------------------
      ncstor=ncstor_old

      IF(NE.EQ.NERRS) GO TO 590
         WRITE(IPR,490)ISEGEX
         CALL ERROR
         GO TO 670
590   CALL EACDRV(PESP,MPESP,IDLOOP,KNTYR,A,MA,D,IWKLOC,NWORK,NYRS,
     1 IHZERO)
C
      CALL ETSWT(TSESP,MTSESP,D,MD,KNTYR,kntmo,NYRS,ihzero,ljdcon,IERR)
      IF(IERR.EQ.0) GO TO 600
      WRITE(IPR,510) ISEGEX
      CALL ERROR
      GO TO 630
C
600   KNTMO=KNTMO+1
      NM=NM+1
      IF(NM.LE.12) GO TO 610
      NM=1
      NY=NY+1
C
610   IF(KNTYR.EQ.MAXKNT.AND.LDA.GE.LDARUN) GO TO 620
      IF(KNTMO.GT.12.AND.LDA.GE.LJDCON.AND.KNTYR.LE.NYRS) GO TO 340
      IF(JHSS.EQ.0.AND.JASS.EQ.0.AND.LDA.GE.LJDCON) GO TO 340
      GO TO 370
C
C   FILL THE ACCUMULATOR ARRAY FOR THE BASE PERIOD OUTSIDE
C   THE RUN PERIOD. CALL THE DISPLAY DRIVER.
C
620   CALL EANLYZ(IMONTH,IDAY,IHOUR,LCOND,IDIFFY,NYRS,NBYRS,
     1 TSESP,MTSESP,PESP,MPESP,SPESP,MSPESP,A,MA,D,IWKLOC,NWORK)
C
C
630     CONTINUE
C
C
CEW CHANGED CLOSED TO CCLOSL
       CALL CCLOSL
cew  must close all ESPTS files for each seg 
       call clfile('ESPTS ',0,ierr)
       nepts=0
       if(ierr.ne.0) then
       WRITE(IPR,659) ISEGEX
 659   FORMAT(1H0,10X,'** ERROR ** Problem closing ESPTS files.',
     1 '  Segment number ',i2)
        CALL ERROR
       endif

cew for each segment copy carryover files to permanent storage if this
cew is a carryover save run

      if ( ncstor .ge. 1) then
	 filnm=' '
        do 655 iclos=1,ncstor
655       call upclos(iclos+69, filnm , icerr)

c SAT RTi, Sept 2003
c Must determine the length of the segment name passed to ecopy and then
c pass the length!
        write(osegn,'(2A4)') idsegn
        call KKTRIM(osegn,bsegn,esegn)
        lsegn = esegn-bsegn+1
	call ecopy( ncstor, icday, ichour, idsegn, lsegn, nlstz, ierr)

cfan (begin)
c if the SaveCoDate is 03/01, the carryover files of both 02/28 and 02/29
c shoul dbe available.   HSD bug r26-26
        if (ileapyear.eq.1) then
        icday(1)=icday0229
	call ecopy( ncstor, icday, ichour, idsegn, lsegn, nlstz, ierr)
        endif
cfan (end)

cew error warning checking
	if(ierr .ne. 0) then
		if(ierr .eq. 1) then
                        write(ipr,*)'** WARNING ** NO ofs_fs5files TOKEN
     + FOUND.'
                
		elseif(ierr .eq. 2) then
                        write(ipr,*)'** WARNING ** NO esp_dir TOKEN FOUN
     +D.'

		elseif(ierr .eq. 3) then
                        write(ipr,*)'** WARNING ** CANNOT ACCESS THE OFS
     + FILES.'

		elseif(ierr .eq. 4) then 
                        write(ipr,651)
651                     format('** WARNING ** CANNOT COPY FILES ', 
     +                         'FROM THE ofs_fs5files DIRECTORY TO ',
     +                         'THE ESP CARRYOVER DIRECTORY.')

		elseif(ierr .eq. 5) then
                        write(ipr,*)'** WARNING ** CANNOT ACCESS ESP CAR
     +RYOVER DIRECTORY.'
                
		endif
        write(ipr,*)'** WARNING ** ESP CARRYOVER FILES MAY NOT BE CORREC
     +T.'
        write(ipr,*)'** NOTE ** CHECK LOG FILE FOR ADDITIONAL MESSAGES.'
        call warn
	endif

      endif

660   CONTINUE
C
C
C
670   CONTINUE


      IOPNUM=IOLDOP
      OPNAME(1)=OLDOPN(1)
      OPNAME(2)=OLDOPN(2)
C
      RETURN
      END
