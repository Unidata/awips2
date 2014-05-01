C MODULE EX47
C
C   THIS IS THE EXECUTION ROUTINE FOR THE 'PEAKFLOW' OPERATION.
C
C   THIS ROUTINE WAS INITIALLY WRITTEN BY:
C   BRYCE FINNERTY, HRL, DECEMBER, 1994.
C
      SUBROUTINE EX47(PO,CO,SIMTS,WORK,SWORK)
C
C
      CHARACTER*8  SNAME
      CHARACTER*1  filenm, fmt
      DIMENSION    PO(*),CO(*),SIMTS(*),ATZ(1)
      DIMENSION    WORK(*),SWORK(*),LASTDD(12)
      DIMENSION    temp(20)
      REAL         MAXQ
      INTEGER      curpeak
C
C  COMMON BLOCKS.
      INCLUDE 'common/fdbug'
      INCLUDE 'common/ionum'
      INCLUDE 'common/fwyds'
      INCLUDE 'common/fwydat'
      INCLUDE 'common/fprog'
      INCLUDE 'common/fconit'
      INCLUDE 'common/pudflt' 
      INCLUDE 'common/fctime'
      INCLUDE 'common/fcary'
      INCLUDE 'common/fengmt'
      
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/calb_peakflow/RCS/ex47.f,v $
     . $',                                                             '
     .$Id: ex47.f,v 1.6 2000/12/19 14:55:50 dws Exp $
     . $' /
C    ===================================================================
C
C
      DATA  SNAME  / 'EX47    ' /
CC    DATA  STATID /4H    /
      DATA  LASTDD /31,28,31,30,31,30,31,31,30,31,30,31/
      DATA  filenm /' '/, fmt/'U'/
C
C  TRACE LEVEL FOR ROUTINE=1, DEBUG SWITCH=IBUG
      CALL FPRBUG(SNAME,1,47,IBUG)
C  DEBUG OUTPUT - PRINT PO AND CO ARRAYS
C
      IDT=PO(10)
      IWINDOW=PO(13)
      IUSEP=24
      ICOPKS=2*IWINDOW
      IUSEC=5+2*ICOPKS
      LCHR=24
      ICHR=6
      NP=PO(14)
      IENDAT=20*NP
      LCPTEMP=0
      itunit=19
C
C      IBUG=1
      IF (IBUG.EQ.0) GO TO 99
      WRITE(IODBUG,10)
      WRITE(IODBUG,12) (PO(I),I=1,12)
      WRITE(IODBUG,13) (PO(I),I=13,IUSEP)
  10  FORMAT(/,10X,'** CONTENTS OF PO ARRAY')
  12  FORMAT(10X,F5.0,2X,5A4,2X,2A4,1X,A4,2X,F5.0,2X,2A4)
  13  FORMAT(10X,4(F7.2,2X),8a4)
      WRITE(IODBUG,14)
  14  FORMAT(/,10X,'** CONTENTS OF CO ARRAY')
      WRITE(IODBUG,16) (CO(I),I=1,IUSEC)
  16  FORMAT(10X,5F10.1)
C
C  DEBUG OUTPUT PRINT SIMULATED TIME SERIES SIMTS.
      ITIQ=(IDA-IDADAT)*24/IDT+IHR/IDT
      LTIQ=(LDA-IDADAT)*24/IDT+LHR/IDT
c      WRITE(IODBUG,18)
  18  FORMAT(/,10X,'** INPUT SIMULATED DISCHARGE TIME SERIES **')
c      WRITE(IODBUG,20) (SIMTS(I),I=ITIQ,LTIQ)
  20  FORMAT(10X,8F10.2)
C
  99  CONTINUE
c
cbf  start of peak processing loop
cbf  sssssssssssssssssssssssssssssssssssssssssssssssssstart of loop
c
 999  LPK=CO(1)
      LCP=CO(2)
      LPREC=CO(3)   
      curpeak=lpk+lcptemp+1
c     IBUG=1
      IF (IBUG.GT.0) WRITE(IODBUG,25)LCPTEMP,LPK,LCP,curpeak,np
c
      IF(LPK.EQ.NP) GO TO 905
      if ( curpeak .gt. np) go to 905
C
  25  FORMAT(/,'LCPTEMP,LPK,LCP,curpeak,np=',6I6)
C
     
      read(unit=itunit,rec=curpeak) temp
      IF (IBUG.GT.0) THEN
         WRITE(IODBUG,685) curpeak
         WRITE(IODBUG,680) (temp(K),K=1,20)
        END IF
  500 CONTINUE
c
  685 FORMAT(/,10X,'** CONTENTS OF SCRATCH FILE RECORD number =',i3)
  680 FORMAT(10F10.1)
c
C  FIND MONTH AND DAY OF NEXT OBSERVED PEAK TO BE PROCESSED IN THE
C  SCRATCH FILE RECORD.
C
c     WRITE(IODBUG,*)'NOUTZ,NOUTDS = ',NOUTZ,NOUTDS
      CALL MDYH1(IDA,IHR,NMONTH,NDAY,NYEAR,NHRR,NOUTZ,NOUTDS,ATZ)
      CALL MDYH1(LDA,LHR,LMONTH,LDAY,LYEAR,LHRR,NOUTZ,NOUTDS,ATZ)
      IMONTH=temp(2)
      IDAY=temp(3)
      IYEAR=temp(1)
C
c      IBUG=1
      IF (IBUG.GT.0) WRITE(IODBUG,26) NMONTH,NDAY,NYEAR,IMONTH,IDAY,
     &IYEAR
  26  FORMAT(/,'NMONTH=',I2,' NDAY=',I2,' NYEAR=',I4,' IMONTH=',I2,
     &' IDAY=',I2,' IYEAR=',I8)
C
C  LEAP YEAR CALCULATION.
C     LASTDA=LASTDD(NMONTH)
C     IF (IMONTH.EQ.2) THEN
C       IF (IYEAR.EQ.((IYEAR/4)*4)) LASTDA=29
C     END IF
C
C  CASE OF NO CARRYOVER AND NEXT PEAK TO BE PROCESSED DOES NOT FALL
C  WITHING THE CURRENT MONTH PLUS THE WINDOW INTO THE NEXT MONTH.
cbf  skip to end if there is no peaks to be processed.
C
C  CASE FOR PEAK IN SAME YEAR, AND MONTH IS JAN-OCT.
      IF ((IYEAR.EQ.NYEAR).AND.(LCP.EQ.0)) THEN
        IF ((NMONTH.LE.10).AND.(IMONTH.GT.(NMONTH+1))) GO TO 905
C  CASE FOR NOVEMBER.
        IF ((NMONTH.EQ.11).AND.(IMONTH.EQ.12)) THEN
          IF (IDAY.GT.IWINDOW) GO TO 905
        END IF
      END IF
C  CASE FOR PEAK IN WINDOW OF JANUARY OF NEXT YEAR.
      IF ((IYEAR.EQ.(NYEAR+1)).AND.(LCP.EQ.0)) THEN
        IF ((IMONTH.EQ.1).AND.(NMONTH.EQ.12)) THEN
          IF (IDAY.GT.IWINDOW) GO TO 905
        END IF
      END IF
C....................................................................
C  CASE OF CARRY OVER FROM THE PREVIOUS MONTH
C
      IF (LCP.GT.0) THEN
C
c      IBUG=1
      IF (IBUG.GT.0) THEN
        WRITE(IODBUG,27)
  27    FORMAT(/,'**PROCESSING CARRYOVER PEAK FROM PREVIOUS MONTH**')
      END IF
C
C  FIND WINDOW SPAN IN CURRENT MONTH
        LCDAY=CO(7)
        COMAX=CO(6)
        bound=0.01
C  CASE OF COMAX BEING AT THE WINDOW BOUNDARY.
        IF (COMAX.LT.0.) THEN
          BOUND=-1.01
          COMAX=-COMAX
        END IF
C
        IF (IDAY.GE.(LDAY-IWINDOW)) THEN
C took out term (LDAY - IDAY) 2/00
          LCWIN=IWINDOW
        END IF
        IF (IDAY.LE.IWINDOW) THEN
          LCWIN=IDAY+IWINDOW
        END IF
C  FIND LENGTH OF TS ARRAY TO SEARCH FOR SIMULATED PEAK.
        CALL FCTZC(NOUTZ,NOUTDS,ATZ)
        CALL JULDA(JLCWIN,JLCHR,NMONTH,LCWIN,NYEAR,LCHR,NOUTZ,NOUTDS,
     &  ATZ)
        IITIQ=(IDA-IDADAT)*24/IDT+ICHR/IDT
C changed from LCWIN to JLCWIN 2/00 
        LLTIQ=(JLCWIN-IDADAT)*24/IDT+LCHR/IDT
C
c        IBUG=1
        IF (IBUG.GT.0)THEN
          WRITE(IODBUG,300)COMAX,LCDAY,BOUND,LCWIN,IITIQ,LLTIQ
          WRITE(IODBUG,302)JLCWIN,JLCHR
 300      FORMAT(10X,'COMAX=',F10.0,' LCDAY=',I2,' BOUND=',F4.0,
     &    ' LCWIN=',I2,' IITIQ=',I3,' LLTIQ=',I8)
 302      FORMAT(10X,'JLCWIN=',I10,'  JLCHR=',I8)
        END IF
C  FIND MAXIMUM FLOW AND DAY IT OCCURED AND RECORD IN TEMPORARY
C  WORK SPACE.
C
        MAXQ=0.0
        MPOSITN=-1
        DO 2002 I=IITIQ,LLTIQ
          IF(SIMTS(I).GT.MAXQ) THEN
            MAXQ=SIMTS(I)
            MPOSITN=I
          END IF
 2002   CONTINUE
C  FIND WHERE IN WINDOW SIMULATED PEAK OCCURRED AND STORE DATA IN
C  SCRATCH FILE.
        IF (COMAX.GE.MAXQ) THEN
          MAXQ=COMAX
          MAXDA=LCDAY
          MAXMON=NMONTH-1
          MAXYR=NYEAR
          IF (NMONTH.EQ.1) THEN
            MAXMON=12
            MAXYR=NYEAR-1
          END IF
        END IF
C
        IF (COMAX.LT.MAXQ) THEN
C
          DT=IDT
          XMAXDA=DT*MPOSITN/24-DT/24+NDAY
          MAXDA=XMAXDA
          MAXMON=NMONTH
          MAXYR=NYEAR
C  CHECK FLAG IF PEAK IS AT WINDOW BOUNDARY.
          IF (MPOSITN.EQ.LLTIQ) BOUND=-1.01
          IF (MPOSITN.LT.LLTIQ) BOUND=0.01
        END IF
C
c        IBUG=1
        IF (IBUG.GT.0) THEN
          WRITE(IODBUG,305)MAXQ,MPOSITN,BOUND,MAXDA,MAXMON,MAXYR
 305      FORMAT(10X,'MAXQ=',F10.0,' MPOSITN=',I4,' BOUND=',F4.0,
     &    ' MAXDA=',I2,' MAXMON=',I2,' MAXYR=',I5)
        END IF
C  WRITE DATA TO SCRATCH FILE.
        temp(11)=MAXYR
        temp(12)=MAXMON
        temp(13)=MAXDA
        temp(14)=MAXQ
        temp(15)=BOUND
        curpeak=lpk+1
        WRITE (UNIT=ITUNIT,REC=curpeak) temp
C  RESET BOUND FLAG FOR NEXT CO PEAK.
        BOUND=0.01
C
C  SET DEFAULT CARRYOVER FOR PROCESSED PEAK AND MOVE REMAINING CO
C  PEAKS INTO FIRST POSITION IN CO ARRAY.
        LOOP=2*LCP+3
        DO 100 I=6,LOOP
          CO(I)=CO(I+2)
 100    CONTINUE
        CO(LOOP+1)=-999.01
        CO(LOOP+2)=-999
        LCP=LCP-1
        LPK=LPK+1
        LPREC=LPREC+1
        CO(2)=LCP
        CO(1)=LPK
        CO(3)=LPREC
C
        GO TO 999
C
      END IF
C.................................................................
C  CASE OF NO CARRYOVER AND PEAK IS IN THE MONTH, OR PEAK IS IN
C  THE SPAN OF THE WINDOW ON THE FIRST DAYS OF THE FIRST MONTH OF
C  THE RUN.
C
      IF ((NYEAR.EQ.IYEAR).AND.(NMONTH.EQ.IMONTH)) THEN
        IF (IDAY.LE.(LDAY-IWINDOW)) THEN
          IIDAY=IDAY-IWINDOW
          if (iiday .lt. 1) iiday=1
          LLDAY=IDAY+IWINDOW
          CALL FCTZC(NOUTZ,NOUTDS,ATZ)
          CALL JULDA(JIIDAY,JIIHR,NMONTH,IIDAY,NYEAR,ICHR,NOUTZ,
     &    NOUTDS,ATZ)
          CALL JULDA(JLLDAY,JLLHR,NMONTH,LLDAY,NYEAR,LCHR,NOUTZ,
     &    NOUTDS,ATZ)
C CHECK IF FIRST MONTH OF RUN AND PEAK DAY IS LESS THAN WINDOW SIZE.
          IF (JIIDAY.LT.IDARUN) JIIDAY=IDARUN
C
c          IBUG=1
          IF (IBUG.GT.0) WRITE(IODBUG,29)
 29       FORMAT(/,'**PROCESSING PEAK IN MIDDLE OF MONTH WITHOUT ',
     &    'CARRYOVER')
C
          IITIQ=(JIIDAY-IDADAT)*24/IDT+ICHR/IDT
          LLTIQ=(JLLDAY-IDADAT)*24/IDT+LCHR/IDT
          IF (IBUG.GT.0) WRITE(IODBUG,310) IITIQ,LLTIQ,JIIDAY,JLLDAY
 310      FORMAT(10X,'IITIQ=',I4,' LLTIQ=',I4,' JIIDAY=',I10,
     &    ' JLLDAY=',I10)
C
C  FIND MAXIMUM FLOW AND DAY IT OCCURED AND RECORD IN TEMPORARY WORK
C  SPACE.
C
          MAXQ=0.0
          MPOSITN=-1
          DO 2003 I=IITIQ,LLTIQ
            IF(SIMTS(I).GT.MAXQ) THEN
              MAXQ=SIMTS(I)
              MPOSITN=I
            END IF
 2003     CONTINUE
C  CONVERT BACK TO JULIAN DAY
          DT=IDT
          XMAXDA=DT*MPOSITN/24-DT/24+NDAY
          MAXDA=XMAXDA
          MAXMON=NMONTH
          MAXYR=NYEAR
C  SET FLAG IF PEAK IS AT WINDOW BOUNDARY.
          BOUND=0.01
          IF ((MPOSITN.EQ.IITIQ).OR.(MPOSITN.EQ.LLTIQ)) BOUND=-1.01
C
c          IBUG=1
          IF (IBUG.GT.0) THEN
            WRITE(IODBUG,315) MAXQ,MPOSITN,BOUND,MAXDA,MAXMON,MAXYR
 315        FORMAT(10X,'MAXQ=',F12.1,' MPOSITN=',I4,' BOUND=',F4.0,
     &      ' MAXDA=',I3,' MAXMON=',I3,' MAXYR=',I5)
          END IF
C  WRITE TO TEMPORARY SCRATCH WORK SPACE
        temp(11)=MAXYR
        temp(12)=MAXMON
        temp(13)=MAXDA
        temp(14)=MAXQ
        temp(15)=BOUND
        curpeak=lpk+lcptemp+1
        WRITE (UNIT=ITUNIT,REC=curpeak) temp
C
          IF (IBUG.GT.0) THEN
            WRITE(IODBUG,317) (temp(i),i=11,15)
 317        FORMAT(/,'** temp(11-15)**',5F10.1)
          END IF
C
          LPK=LPK+1
          LPREC=LPREC+1
          CO(1)=LPK
          CO(3)=LPREC
          GO TO 999
C
        END IF
      END IF
C
C....................................................................
C  CASE OF CARRYOVER IS AT THE END OF THE MONTH FROM AN OBSERVED PEAK
C  OCCURRING IN THE SEARCH WINDOW AT THE END OF THE CURRENT MONTH
C  OR BEGINNING OF THE NEXT MONTH.
C
      ISERCH=0
      IF ((IYEAR.EQ.NYEAR).AND.(IMONTH.EQ.NMONTH)) THEN
        IF (IDAY.GT.(LDAY-IWINDOW)) THEN
          ISERCH=IDAY-IWINDOW
        END IF
      END IF
C
      IF ((IYEAR.EQ.NYEAR).AND.(IMONTH.EQ.(NMONTH+1))) THEN
        IF (IDAY.LE.IWINDOW) THEN
          ISERCH=LDAY-(IWINDOW-IDAY)
        END IF
      END IF
C
      IF ((IYEAR.EQ.(NYEAR+1)).AND.(IMONTH.EQ.1).AND.(NMONTH.EQ.12))
     &THEN
        IF (IDAY.LE.IWINDOW) THEN
          ISERCH=LDAY-(IWINDOW-IDAY)
        END IF
      END IF
C
C  FIND MAXIMUM CARRYOVER FLOW AND DAY IT OCCURED, IF THERE IS
C  CARRYOVER PEAKS, AND STORE THEM IN CO ARRAY.
C
      IF (ISERCH.GT.0) THEN
C
c      IBUG=1
      IF (IBUG.GT.0) WRITE(IODBUG,31)
 31   FORMAT(/,'** PROCESSING PEAK THAT OCCURS IN WINDOW AT THE END OF',
     &' THE CURRENT MONTH OR',/,'  BEGINING OF THE NEXT MONTH, AND',
     &' WITH CARRYOVER REQUIRED FROM THE CURRENT MONTH.')
C
        CALL FCTZC(NOUTZ,NOUTDS,ATZ)
        CALL JULDA(JSERCH,JSHR,NMONTH,ISERCH,NYEAR,ICHR,NOUTZ,NOUTDS,
     &  ATZ)
        IITIQ=(JSERCH-IDADAT)*24/IDT+ICHR/IDT
        LLTIQ=(LDA-IDADAT)*24/IDT+LCHR/IDT
C
        IF (IBUG.GT.0) WRITE(IODBUG,320) IITIQ,LLTIQ,ISERCH,JSERCH
 320    FORMAT(10X,'IITIQ=',I5,' LLTIQ=',I5,' ISERCH=',I5,
     &  ' JSERCH=',I10)
C
        BOUND=0.01
        MAXQ=0.0
        MPOSITN=-1
        DO 2004 I=IITIQ,LLTIQ
          IF(SIMTS(I).GT.MAXQ) THEN
            MAXQ=SIMTS(I)
            MPOSITN=I
          END IF
 2004   CONTINUE
C  CONVERT BACK TO JULIAN DAY AND WRITE TO CO ARRAY.
        DT=IDT
        XMAXDA=DT*MPOSITN/24-DT/24+NDAY
        MAXDA=XMAXDA
C  SET (-) FLAG IF CO PEAK IS AT WINDOW BOUNDARY.
        IF (MPOSITN.EQ.IITIQ) MAXQ=-MAXQ
C
        CO(6+2*(LCPTEMP))=MAXQ
        CO(7+2*(LCPTEMP))=MAXDA
C
C  IF CO IS FOR LAST MONTH OF RUN THEN STORE THE SIMULATED CO PEAK
C  IN THE SCRATCH FILE AND IGNORE THE PORTION OF THE SEARCH WINDOW
C  IN THE NEXT MONTH.
        CALL MDYH1(LDARUN,LHRRUN,LMO,LDD,LYR,LHOUR,NOUTZ,NOUTDS,ATZ)
        IF ((NYEAR.EQ.LYR).AND.(NMONTH.EQ.LMO)) THEN
          temp(11)=NYEAR
          temp(12)=NMONTH
          temp(13)=MAXDA
C  IF MAXQ IS (-) THEN SET FLAG THAT PEAK IS AT WINDOW BOUNDRY.
          IF (MAXQ.LT.0.) THEN
            MAXQ=-MAXQ
            BOUND=-1.01
          END IF
C
          temp(14)=MAXQ
          temp(15)=BOUND
          curpeak=lpk+lcptemp+1
          WRITE (UNIT=ITUNIT,REC=curpeak) temp
C
          LPK=LPK+1
          CO(1)=LPK
        END IF
C
c        IBUG=1
        IF (IBUG.GT.0) THEN
          WRITE(IODBUG,325) MAXQ,MPOSITN,BOUND,MAXDA,NMONTH,NYEAR
 325      FORMAT(10X,'MAXQ=',F12.1,' MPOSITN=',I5,' BOUND=',F4.0,
     &    ' MAXDA=',I3,' MAXMON=',I3,' MAXYR=',I5)
        END IF
C
        LCPTEMP=LCPTEMP+1
        LPREC=LPREC+1
        CO(2)=LCP
        CO(3)=LPREC
C
        GO TO 999
C
      END IF
C
C  MAKE SURE IRECRD1 AND LPREC ARE IN THE CORRECT POSITIONS FOR NEXT
C  MONTH, IF THEY WERE INCREMENTED BY LOOKING AHEAD TO FILL CARRYOVER
C  PEAKS THAT WILL NOT GET COMPLETELY PROCESSED UNTIL THE FOLLOWING
C  MONTH.
C=================================================== continue after processing peak
  905 CONTINUE
C
C  RESTORE VALUES IN PO, CO, AND WY ARRAYS.
C
      LCP=LCPTEMP
      CO(2)=LCP
      CO(3)=LPREC
      CO(1)=LPK
C
c      IBUG=1
      IF (IBUG.GT.0) THEN
       curpeak=lpk+lcptemp
      Do 226 j=1,curpeak
         irecrd=j
         read(unit=itunit,rec=irecrd,iostat=ierr) temp
         write(iodbug,336)iodbug
         WRITE(IODBUG,227,iostat=ierr)irecrd
         WRITE(IODBUG,228,iostat=ierr) (temp(k),k=1,20)
 226  continue
 227  format('**Filled scratch file in ex47, record #',i3)
 228  format(5f10.2)
      write(iodbug,336)iodbug
      WRITE(IODBUG,224)
      WRITE(IODBUG,225) (CO(I),I=1,IUSEC)
 224  FORMAT(/,10X,'** FILLED CO ARRAY IN EX47 **')
 225  FORMAT(10X,5F10.1)
      END IF
C
C  IF THE SIMULATION PERIOD IS OVER AND ALL PEAKS HAVE BEEN
C  PROCESSED THEN THE SCRATCH FILE RECORDS NEED TO BE TRANSFERED
C  TO WORKSPACE FOR MAKING OF THE DISPLAY TABLE AND GRAPH.
C
      CALL MDYH1(LDARUN,LHRRUN,LDMON,LDDAY,LDYEAR,LDHR,NOUTZ,NOUTDS,
     & ATZ)
      CALL MDYH1(LDA,LHR,LMONTH,LDAY,LYEAR,LHRR,NOUTZ,NOUTDS,ATZ)
c      IBUG=1
      IF (IBUG.GT.0) THEN
        write(iodbug,336)iodbug
        WRITE(IODBUG,335) NP,LPK,LDARUN,LDA
        WRITE(IODBUG,337) LDMON,LDYEAR,LMONTH,LYEAR
 337    FORMAT(10X,'LDMON=',I3,' LDYEAR=',I5,' LMONTH=',I3,' LYEAR=',
     &  I5)
 335    FORMAT(10X,'NP=',I5,' LPK=',I5,' LDARUN=',I10,' LDA=',I10)
 336    format(45x,'$$$$$$$$$$$$ unit # iodbug=',i5)
      END IF
C
      IF (LDA.EQ.LDARUN) THEN
c       if (np .ne. curpeak) then
c         write(iodbug,666)np,curpeak
c         write(iodbug,667)
c       end if
 666    format('np=',i4,' and curpeak=',i4,'at end of run before table')
 667    format('np and curpeak should be equal if all peaks are',
     &  ' properly processed in ex47 routine and PEAKFLOW operation') 
        np=po(14)
        DO 200 I=1,np
          irecrd=I
c         write(iodbug,665)itunit
  665     format(50x,'################itunit scratch file =',i5)
          read(unit=itunit,REC=irecrd) temp
cbf  rearrange work array so that the obs Q is in first position and
cbf  dates (yr,mo,day) are in 3,4,5.
            WORK((I-1)*20+1)=temp(4)
            WORK((I-1)*20+2)=temp(5)
            WORK((I-1)*20+3)=temp(1)
            WORK((I-1)*20+4)=temp(2)
            WORK((I-1)*20+5)=temp(3)
            WORK((I-1)*20+6)=temp(6)
            WORK((I-1)*20+7)=temp(7)
            WORK((I-1)*20+8)=temp(8)
            WORK((I-1)*20+9)=temp(9)
            WORK((I-1)*20+10)=temp(10)
            work((I-1)*20+11)=temp(14)
            work((I-1)*20+12)=temp(15)
            work((I-1)*20+13)=temp(11)
            work((I-1)*20+14)=temp(12)
            work((I-1)*20+15)=temp(13)
            work((I-1)*20+16)=temp(16)
            work((I-1)*20+17)=temp(17)
            work((I-1)*20+18)=temp(18)
            work((I-1)*20+19)=temp(19)
            work((I-1)*20+20)=temp(20)
  200   CONTINUE
C
c        IBUG=1
        IENDAT=20*NP
        IF (IBUG.GT.0) THEN
          write(iodbug,221)np,iendat
          WRITE(iodbug,220) iodbug
          WRITE(iodbug,230) (WORK(K),K=1,IENDAT)
  220     FORMAT(/,10X,'** WY SCRATCH FILES TRANSFERED TO ',
     &    'WORK SPACE **  iodbug=',i5)
  230     FORMAT(10X,5F10.1)
  221     format(10x,'number peaks=',i5,'  iendat in work array=',i5)
        END IF
C
C  CALL ROUTINE TABL47 WHICH WRITES TABLE OF SIM VS. OBS PEAKS
C  AND COMPARATIVE STATISTICS.  ALSO CALL GRAF47 IF REQUESTED
C  WHICH REQUIRES A CALL TO SORT47 TO PUT PEAKS IN ORDER OF MAGNITUDE
C  PRIOR TO SORTING.
C
        IDISP=PO(15)
        MAGFLG=PO(16)
C  CASE OF TABLE IN CHRONOLOGICAL ORDER, WITH OUT GRAPH OPTION.
        IF (MAGFLG.EQ.0) THEN
          CALL TABL47(PO,WORK)
        end if
C  CASE OF TABLE IN CHRONOLOGICAL ORDER, WITH GRAPH OPTION.
cbf comment out graph option until it is available, if ever.
c         IF (IDISP.EQ.1) THEN
c           CALL SORT47(PO,CO,WORK,SWORK,ISTAT)
c           CALL GRAF47(PO,CO,WORK)
c         END IF
c       END IF
C  CASE OF TABLE DIPLAY BY MAGNITUDE OF FLOW, WITH OR WITH OUT GRAPH
C  OPTION.
       IF (MAGFLG.EQ.1) THEN
         CALL SORT47(PO,CO,WORK,SWORK,ISTAT)
         CALL TABL47(PO,WORK)
       end if
c         IF (IDISP.EQ.1) THEN
c           CALL GRAF47(PO,CO,WORK)
c         END IF
c       END IF
C
C  END IF FROM LAST MONTH OF RUN LOOP TO PRINT TABLE AND GRAPH.
c  close the scratch file
c
        CALL UPCLOS(ITUNIT,' ',ISTAT)
c
       END IF
C
C    ITRACE=1
      IF (ITRACE.GE.1) then
        WRITE(IODBUG,910)
      end if
  910 FORMAT(10X,'** EX47 EXITED **')
C
C
      RETURN
      END
