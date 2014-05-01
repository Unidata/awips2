C MODULE TABL47
C
C    THIS ROUTINE WRITES A TABLE OF SIMULATED V.S. OBSERVED PEAK
C    DISCHARGE VALUES, AND COMPARATIVE STATISTICS.
C
C    ROUTINE TABL47 WAS FIRST WRITEN BY:
C      BRYCE FINNERTY, HYDROLOGIC RESEARCH LAB, DECEMBER 1994.
C
      SUBROUTINE TABL47(PO,WORK)
C
      DIMENSION PO(*),WORK(*),LASTDD(12),SNAME(2),STYPE(1)
      DIMENSION INC(21),NX(21),NXPCT(21),INDEX(4)
C
      REAL INC,NX,NXPCT,STYPE
C
      CHARACTER*3 UNITQ,UNITQE,UNITQM
      CHARACTER*2 UNITH,UNITHE,UNITHM
      CHARACTER*1 BLANK,EXXX,XBOUND
c
      character*1 flagh,flagq
      real*4      xflagh,xflagq
      equivalence(flagq,xflagq)
      equivalence(flagh,xflagh)
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
     .$Source: /fs/hseb/ob72/rfc/calb/src/calb_peakflow/RCS/tabl47.f,v $
     . $',                                                             '
     .$Id: tabl47.f,v 1.3 1998/04/07 13:34:32 page Exp $
     . $' /
C    ===================================================================
C
C
      DATA SNAME /4HTABL,4H47  /
      DATA  STATID / 4h     /
      DATA  LASTDD /31,28,31,30,31,30,31,31,30,31,30,31/
      DATA  STYPE /4hSQIN/
      DATA  UNITQE /3hCFS/
      DATA  UNITHE /2hFT/
      DATA  UNITHM /2h M/
      DATA  BLANK /1h /
      DATA  EXXX  /1h*/
C
C
C  TRACE LEVEL FOR ROUTINE=1, DEBUG SWITCH=IBUG
      CALL FPRBUG(SNAME,1,47,IBUG)
C
      IUSEP=24
C
c     IBUG=0
      IF (IBUG.gt.0)  then
        WRITE(IODBUG,111)
        WRITE(IODBUG,112) (PO(I),I=1,12)
        WRITE(IODBUG,113) (PO(I),I=13,IUSEP)
 111    FORMAT(/,10X,'** CONTENTS OF PO ARRAY')
 112    FORMAT(10X,F5.0,2X,5A4,2X,2A4,1X,A4,2X,F5.0,2X,2A4)
 113    FORMAT(10X,4(F7.2,2X),8a4)
      end if
C  SET UNITS label AND WRITE TABLE HEADING
C
      UNITH=UNITHM
      CALL FDCODE(STYPE,UNITQ,DIMD,MSGD,NPDT,TSCALE,NADD,IERR)
      IF (METRIC.EQ.0) THEN
        UNITQ=UNITQE
        UNITH=UNITHE
      END IF
C
C  WRITE TITLE AND HEADINGS ON THE TABLE.
      WRITE(IPR,*)('  ')
      WRITE(IPR,*)(' ')
      WRITE(IPR,*)('  ')
      WRITE(IPR,*)('  ')
      WRITE(IPR,400)
      WRITE(IPR,410)
      WRITE(IPR,*)('  ')
      WRITE(IPR,*)('  ')
 400  FORMAT(35X,'PEAKFLOW DISCHARGE AND TIMING ERROR SUMMARY')
 410  FORMAT(35X,'___________________________________________')
      WRITE(IPR,500)
      WRITE(IPR,502)UNITQ,UNITH,UNITQ,UNITQ
      WRITE(IPR,504)
 500  FORMAT(15X,'OBSERVED PEAK',12X,'FLAG',9X,'SIMULATED PEAK',11X,
     &'TIMING',10X,'DISCHARGE',6X,'DISCHARGE RATIO')
 502  FORMAT(9X,'Q(',A3,')',4X,'H(',A2,')',6X,'DATE',6X,'Q',2X,'H',6X,
     &'Q(',A3,')',9X,'DATE',8X,'ERROR(DAYS)',6X,'ERROR(',A3,')',
     &8X,'(SIM/OBS)')
 504  FORMAT(5X,'_________________________________________________',
     &'_____________________________________________________________',
     &'___________')
C
C  LOOP TO PRINT INFO ON EACH OBSERVED AND SIMULATED PEAK.
C
      NP=PO(14)
cb      IWINDOW=PO(13)+1
cb      MAXERR=PO(19)
cb      INCERR=MAXERR/40
      ABSERR=0.
      TOTOBS=0.
      TOTSIM=0.
      TOTTIM=0.
      TOTDIF=0.
      TOTRAT=0.
      TOTH=0.
      ICOUNT=0
      SUMSQSQ=0.
      SUMTMNG=0.
      SUMSQ=0.
      SUMQQ=0.
      SUMSS=0.
C  SET ABSOLUTE ERROR INCREMENT CATEGORIES.
cb      DO 10 I=1,21
cb        IF (I.LE.4) INDEX(I)=0
cb        NX(I)=0
cb        NXPCT(I)=0
cb        IF ((I.GE.1).AND.(I.LE.10)) INC(I)=INCERR*I
cb        IF ((I.GE.11).AND.(I.LE.15)) INC(I)=(MAXERR/4)+(I-10)*2*INCERR
cb        IF ((I.GE.16).AND.(I.LE.20)) INC(I)=(MAXERR/2)+(I-15)*4*INCERR
cb 10   CONTINUE
cb      NX(21)=0.
cb      INC(21)=MAXERR
C
C  LOOP 100 PROCESSES EACH PEAK EVENT.
C
      DO 100 I=1,NP
C
C  UNIT CONVERSION
cb assume obs Q+H are in english, and sim Q are in metric.
cb convert obs to metric if metric=1.
cb if metric=0, convert sim Q to english.
        IF (METRIC.EQ.1) THEN
          SIMPK=WORK((I-1)*20+1)
          IF (SIMPK.EQ.-999.) GO TO 101
          WORK((I-1)*20+1)=SIMPK*0.02832
 101      WORK((I-1)*20+2)=(WORK((I-1)*20+2))*0.3048
        END IF
c
        if (metric .eq. 0) then
          WORK((I-1)*20+11)=(WORK((I-1)*20+11))/0.02832
        end if
C
        OBSQ=WORK((I-1)*20+1)
        OBSH=WORK((I-1)*20+2)
        IYR=WORK((I-1)*20+3)
        IMN=WORK((I-1)*20+4)
        IDY=WORK((I-1)*20+5)
        xflagq=work((i-1)*20+6)
        xflagh=work((i-1)*20+7)
        SIMQ=WORK((I-1)*20+11)
        BOUND=WORK((I-1)*20+12)
        LYR=WORK((I-1)*20+13)
        LMN=WORK((I-1)*20+14)
        LDY=WORK((I-1)*20+15)
C  SETS CHARACTER TO INDICATE A BOUNDARY CASE IF REQUIRED.
        XBOUND=BLANK
        IF (BOUND.LT.0) XBOUND=EXXX
C
C  SIGNIFICANT FIGURES CALCULATION PRIOR TO USING THE DATA.
        CALL FSIGFG(OBSQ,3,IER)
        CALL FSIGFG(OBSH,3,IER)
        CALL FSIGFG(SIMQ,3,IER)
C
C  LOOP FOR CASES OF OBSERVED PEAK DISCHARGE.
C
        IF (OBSQ.GT.0) THEN
C
C  TIMING OF PEAKS
C
C  LEAP YEAR CALCULATION.
      LASTDA=LASTDD(LMN)
      ILASTD=LASTDD(IMN)
      IF (IMN.EQ.2) THEN
        IF (IYR.EQ.((IYR/4)*4)) ILASTD=29
      END IF
      IF (LMN.EQ.2) THEN
        IF (LYR.EQ.((LYR/4)*4)) LASTDA=29
      END IF
C
          IF (IYR.EQ.LYR) THEN
            IF (IMN.EQ.LMN) ITIM=IDY-LDY
            IF (IMN.LT.LMN) ITIM=IDY-ILASTD-LDY
            IF (IMN.GT.LMN) ITIM=IDY+LASTDA-LDY
          END IF
C
          IF (IYR.LT.LYR) ITIM=IDY-ILASTD-LDY
          IF (IYR.GT.LYR) ITIM=IDY+LASTDA-LDY
C
C  INCREMENT INDEX OF TIMING ERROR CATEGORIES.
cb          DO 20 J=1,IWINDOW
cb            IITIM=ABS(ITIM)
cb            IKILL=J-1
cb            IF (IITIM.LE.IKILL) THEN
cb             INDEX(J)=INDEX(J)+1
cb            END IF
cb            IF ((J.EQ.IWINDOW).AND.(BOUND.LT.0.)) THEN
cb              INDEX(J)=INDEX(J)-1
cb            END IF
cb  20      CONTINUE
C
cb            IBUG=0
cb            IF (IBUG.GT.0) THEN
cb              WRITE(IODBUG,600) J,BOUND,IITIM
cb              WRITE(IODBUG,602)(INDEX(L),L=1,4)
cb 600          FORMAT(5X,'  J=',I4,' BOUND=',F4.0,' IITIM=',I3)
cb 602          FORMAT(5X,'INDEX(L)=',4I8)
cb            END IF
C
C  SIM VS OBS DIFFERENCES
          ABSDIF=SIMQ-OBSQ
          ABSERR=ABS(100*ABSDIF/OBSQ)
          SQSQ=ABSDIF**2
          RATIO=SIMQ/OBSQ
          SQ=SIMQ*OBSQ
          SS=SIMQ*SIMQ
          QQ=OBSQ*OBSQ
C
C  INCREMENT ABSOLUTE ERROR FREQUENCY DIAGRAM.
cb          DO 30 J=1,21
cb            IF (ABSERR.GT.INC(J)) GO TO 30
cb            NX(J)=NX(J)+1
cb  30      CONTINUE
C
C  TOTAL AVERAGE STATISTICS
          TOTOBS=TOTOBS+OBSQ
          TOTSIM=TOTSIM+SIMQ
          TOTTIM=TOTTIM+ITIM
          TOTDIF=TOTDIF+ABSDIF
          TOTRAT=TOTRAT+RATIO
          TOTH=TOTH+OBSH
          ICOUNT=ICOUNT+1
          SUMSQSQ=SUMSQSQ+SQSQ
          SUMTMNG=SUMTMNG+(ITIM**2)
          SUMSQ=SUMSQ+SQ
          SUMSS=SUMSS+SS
          SUMQQ=SUMQQ+QQ
C
C  CONVERT TO SIGNIFICANT DIGITS AND WRITE TO TABLE
C
          CALL FSIGFG(ABSDIF,3,IER)
          CALL FSIGFG(RATIO,3,IER)
          WRITE(IPR,510) OBSQ,OBSH,IMN,IDY,IYR,flagq,flagh,
     &    SIMQ,LMN,LDY,LYR,ITIM,XBOUND,ABSDIF,RATIO
  510     FORMAT(5X,F10.1,F8.1,4X,I2,'/',I2,'/',I4,3x,a1,2x,a1,
     &    1X,F10.1,6X,I2,'/',I2,'/',I4,9X,I2,3X,A1,7X,F11.1,7X,F7.2)
C
        END IF
C
C  LOOP FOR CASES OF OBSERVED PEAK STAGE AND NO PEAK DISCHARGE.
C
        IF (OBSQ.LE.0) THEN
          WRITE(IPR,520) OBSQ,OBSH,IMN,IDY,IYR,flagq,flagh,
     &    SIMQ,LMN,LDY,LYR
 520      FORMAT(5X,F10.1,F8.1,4X,I2,'/',I2,'/',I4,3x,a1,2x,a1,1x,
     &    F10.1,6X,I2,'/',I2,'/',I4)
        END IF
C
 100  CONTINUE
C
C  AVERAGE STATISTICS
      AVEOBS=TOTOBS/ICOUNT
      AVESIM=TOTSIM/ICOUNT
      AVETIM=TOTTIM/ICOUNT
      AVEDIF=TOTDIF/ICOUNT
      AVEH=TOTH/ICOUNT
      AVERAT=TOTRAT/ICOUNT
      AVEPCT=((AVESIM-AVEOBS)*100)/AVEOBS
C  ROOT MEAN SQUARE ERROR OF SIM Q VS. OBS Q
      RMSE=(SUMSQSQ/ICOUNT)**0.5
C  ROOT MEAN SQUARE ERROR OF TIMING ERRORS
      TRMSE=(SUMTMNG/ICOUNT)**0.5
C  CORRELATION COEFFICIENT OF INSTANTANEOUS PEAK DISCHARGE=R
      X=(ICOUNT*SUMSQ)-(TOTSIM*TOTOBS)
      XX=(ICOUNT*SUMSS)-(TOTSIM*TOTSIM)
      XXX=(ICOUNT*SUMQQ)-(TOTOBS*TOTOBS)
      XXXX=(XXX*XX)**(0.5)
      R=X/XXXX
C  BEST FIT LINE:  OBS Q = A + B * SIM Q
      B=((ICOUNT*SUMSQ)-(TOTSIM*TOTOBS))/
     &  ((ICOUNT*SUMSS)-(TOTSIM**2.))
      A=AVEOBS-B*AVESIM
C
C  WRITE OUT STATISTICS WITH APPROPRIATE SIGNIFICANT DIGITS.
      CALL FSIGFG(AVEOBS,4,IER)
      CALL FSIGFG(AVESIM,4,IER)
      CALL FSIGFG(AVEDIF,4,IER)
      CALL FSIGFG(A,4,IER)
      CALL FSIGFG(B,3,IER)
      CALL FSIGFG(AVEH,3,IER)
      CALL FSIGFG(RMSE,4,IER)
      CALL FSIGFG(TRMSE,2,IER)
      CALL FSIGFG(AVERAT,3,IER)
C
      WRITE(IPR,504)
      WRITE(IPR,540)AVEOBS,AVEH,AVESIM,AVETIM,AVEDIF,AVERAT
      WRITE(IPR,550)
      write(ipr,555)
      WRITE(IPR,*)('   ')
      WRITE(IPR,565)RMSE,UNITQ
      WRITE(IPR,567)TRMSE
      WRITE(IPR,560)AVEPCT
      WRITE(IPR,570)R
      WRITE(IPR,575)A,UNITQ,B
  530 FORMAT(10X,'...................................................',
     &'..............................................................',
     &'......')
  540 FORMAT('MEAN:',F10.1,2X,F6.1,22X,F10.1,24X,F5.1,10X,F10.1,5X,
     &F8.2)
  550 FORMAT('(OBSERVED DISCHARGE EVENTS ONLY)')
  555 format('"*" INDICATES SIMULATED PEAK ON SEARCH WINDOW BOUNDARY',
     &', WHICH PROBABLY IS NOT THE TRUE PEAK.')
  560 FORMAT(10X,'AVERAGE PERCENT ERROR (AVEOBSQ-AVESIMQ)/AVEOBSQ =',
     &F11.1,' %')
  565 FORMAT(10X,'DISCHARGE RMS ERROR =',F13.3,' (',A3,')')
  567 FORMAT(10X,'TIMING RMS ERROR    =',F13.3,' (DAYS)')
  570 FORMAT(10X,'CORRELATION COEFFICIENT (DISCHARGE) :        R  =',
     &F13.3)
  575 FORMAT(10X,'BEST FIT LINE:  OBSQ = A + B * SIMQ :        A  =',
     &F13.3,' (',A3,')',5X,'B =',F10.3)
cbf
cbf  Hard Code Legend of USGS return flags for peak Discharge and
cbf  peak Stage
cbf___________________________________________________________________
      write(ipr,700)
      write(ipr,710)
      write(ipr,720)
      write(ipr,730)
      write(ipr,740)
      write(ipr,741)
      write(ipr,742)
      write(ipr,743)
      write(ipr,744)
      write(ipr,745)
      write(ipr,746)
      write(ipr,747)
      write(ipr,748)
      write(ipr,749)
      write(ipr,750)
      write(ipr,751)
      write(ipr,752)
      write(ipr,753)
      write(ipr,754)
      write(ipr,755)
      write(ipr,757)
      write(ipr,758)
 700  format(/,5X,'LEGEND OF U.S.G.S. DATA FLAGS FOR PEAK DISCHARGE ',
     & 'AND STAGE')
 710  format(5x,'_________________________________________________',
     & '_________')
 720  format(/,5x,'Peak Discharge, Q, Qualification Flags')
 730  format(5x,'______________________________________')
 740  format(5x,'1 - discharge is a maximum daily average')
 741  format(5x,'2 - discharge is an estimate')
 742  format(5x,'3 - discharge affected by dam failure')
 743  format(5x,'4 - discharge less than indicated value')
 744  format(5x,'    which is minimum recordable discharge at ',
     &       'this site')
 745  format(5x,'5 - discharge affected to unknown degree by')
 746  format(5x,'    regulation or diversion')
 747  format(5x,'6 - discharge affected by regulation or diversion')
 748  format(5x,'7 - discharge is an historic peak')
 749  format(5x,'8 - discharge actually greater than indicated value')
 750  format(5x,'9 - discharge due to snowmelt, hurricane, ice-jam or')
 751  format(5x,'    debris dam breakup')
 752  format(5x,'A - year of occurrence is unknown or not exact')
 753  format(5x,'B - month or day of occurrence is unknown or not ',
     &       'exact')
 754  format(5x,'C - all or part of the record affected by',
     &       ' urbanization,')
 755  format(5x,'    mining, agricultural changes, channelization,',
     &       ' or others')
 757  format(5x,'D - base discharge changed during this year')
 758  format(5x,'E - only annual maximum peak available for this year')
cb
cb
      write(ipr,770)
      write(ipr,771)
      write(ipr,772)
      write(ipr,773)
      write(ipr,774)
      write(ipr,775)
      write(ipr,776)
      write(ipr,777)
 770  format(/,5x,'Gage Height, H, Qualification Flags')
 771  format(5x,'___________________________________')
 772  format(5x,'1 - gage height affected by backwater')
 773  format(5x,'2 - gage height not the maximum for the year')
 774  format(5x,'3 - gage height at different site and/or datum')
 775  format(5x,'4 - gage height below minimum recordable ',
     &          'elevation')
 776  format(5x,'5 - gage height is an estimate')
 777  format(5x,'6 - gage datum changed during this year')
c
C
C  SUMMARY OF TIMING AND RATIO ERRORS
cb      WRITE(IPR,*)
cb      WRITE(IPR,*)
cb      WRITE(IPR,*)
cb      WRITE(IPR,580)
cb      WRITE(IPR,581)
cb      WRITE(IPR,582)
cb      WRITE(IPR,584)
cb      WRITE(IPR,586)
cb  580 FORMAT(12X,'EVENT SUMMARY OF TIMING ERRORS',26X,'CUMULATIVE',
cb     &' FREQUENCY DIAGRAM OF DISCHARGE ERRORS')
cb  581 FORMAT(12X,'..............................',26X,'..........',
cb     &'.......................................')
cb  582 FORMAT(28X,'CUMMULATIVE % OF EVENTS',19X,'NUMBER',13X,'ABSOLUTE',
cb     &9X,'PERCENT OF')
cb  584 FORMAT(5X,'TIMING ERROR(DAYS)',5X,'< OR = TO TIMING ERROR',19X,
cb     &'OF CASES',10X,'PERCENT ERROR',6X,'TOTAL CASES')
cb  586 FORMAT(5X,'...............................................',
cb     &17X,'................................................')
C
cb      NX(21)=ICOUNT-NX(20)
cb      DO 40 I=1,21
cb        NXPCT(I)=100*NX(I)/ICOUNT
cb        IF (I.LE.IWINDOW) THEN
cb          K=I-1
cb          INDEX(I)=100.*INDEX(I)/ICOUNT
cb          WRITE(IPR,590)K,INDEX(I),NX(I),INC(I),NXPCT(I)
cb        END IF
C
cb        IF ((I.GT.IWINDOW).AND.(I.LT.21)) THEN
cb          WRITE(IPR,592) NX(I),INC(I),NXPCT(I)
cb        END IF
cb        IF (I.EQ.21) THEN
cb          WRITE(IPR,594) NX(I),INC(I),NXPCT(I)
cb          WRITE(IPR,*) ' '
cb          WRITE(IPR,596) ICOUNT
cb        END IF
C
cb  40  CONTINUE
C
C  WRITE BOUNDARY CASE MESSAGE DEFINING 'X' IN TIMING ERROR COLUMN.
cb      WRITE(IPR,*)' '
cb      WRITE(IPR,598)
cb      WRITE(IPR,599)
cb 590  FORMAT(10X,I2,24X,I5,' %',27X,F5.0,7X,'LT',7X,F5.1,12X,F6.2)
cb 592  FORMAT(70X,F5.0,7X,'LT',7X,F5.1,12X,F6.2)
cb 594  FORMAT(70X,F5.0,7X,'GT',7X,F5.1,12X,F6.2)
cb 596  FORMAT(72X,'TOTAL NUMBER OF CASES = ',I5)
cb 598  FORMAT('NOTE:  * INDICATES THAT THE SIMULATED PEAK WAS FOUND ',
cb     &'AT THE SEARCH WINDOW BOUNDARY.  IT IS ',
cb     &'POSSIBLE THAT THE SIMULATED PEAK')
cb 599  FORMAT('         IS LOCATED OUTSIDE OF THE',
cb     &' SEARCH WINDOW.')
C
C
C
C     ITRACE=1
      IF (ITRACE.GE.1) WRITE(IODBUG,910)
  910 FORMAT(10X,'** TABL47 EXITED **')
C
      RETURN
      END
