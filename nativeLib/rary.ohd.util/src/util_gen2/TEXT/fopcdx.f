C MEMBER FOPCDX
C  (from old member FCFOPCDE)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 12/01/2000 BY TLS
C
C @PROCESS LVL(77)
C
      SUBROUTINE FOPCDX (OPID,NUMOP)
C.......................................
C     THIS ROUTINE GIVES OPERATION I.D. FOR A GIVEN NUMBER.
C
C     SUBPROGRAM FOPCDE RETURNS THE APPROPRIATE INTERNAL
C        OPERATION NUMBER - = 0 IF THE OPERATION I.D. IS INCORRECT,
C        =-1 FOR THE STOP OPERATION.
C.......................................
C     ROUTINE INITIALLY WRITTEN BY. . .
C
C            ERIC ANDERSON - HRL     JUNE 1979
C.......................................
C
C
      CHARACTER*8 OPID,STOPID,BLANK
      CHARACTER*8 OPNAME(80)
C
C     COMMON BLOCK
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_gen2/RCS/fopcdx.f,v $
     . $',                                                             '
     .$Id: fopcdx.f,v 1.8 2006/03/28 15:12:45 aivo Exp $
     . $' /
C    ===================================================================
C
C
C     DATA STATEMENTS
      DATA OPNAME/ 'SAC-SMA ' ,'UNIT-HG ' ,'REDO-UHG',
     1 'CLEAR-TS' ,'SAC-PLOT' ,'MEAN-Q  ' ,'LAG/K   ',
     2 'CHANLOSS' ,'MUSKROUT' ,'ADD/SUB ' ,'LAY-COEF',
     3 'INSQPLOT' ,'TATUM   ' ,'ADJUST-Q' ,'WEIGH-TS',
     4 'STAT-QME' ,'WY-PLOT ' ,'PLOT-TS ' ,'SNOW-17 ',
     5 'CHANGE-T' ,'DWOPER  ' ,'SS-SAC  ' ,'STAGE-Q ',
     6 'API-CONT' ,'PLOT-TUL' ,'RES-SNGL' ,'LIST-FTW',
     7 'CHANLEAK' ,'API-MKC ' ,'MERGE-TS' ,'SNOW-43 ',
     8 'FFG     ' ,'API-CIN ' ,'API-SLC ' ,'API-HAR ',
     9 'XIN-SMA ' ,'LIST-MSP' ,'BASEFLOW' ,'LOOKUP  ',
     A 'WATERBAL' ,'API-HAR2' ,'RSNWELEV' ,'API-HFD ',
     B 'SARROUTE' ,'DELTA-TS' ,'NOMSNG  ' ,'PEAKFLOW',
     C 'MULT/DIV' ,'BEGASSIM' ,'ASSIM   ' ,'SSARRESV',
     D 'SUMPOINT' ,'LOOKUP3 ' ,'SWB-NILE' ,'FLDWAV  ',
     E 'GLACIER ' ,'CONS_USE' ,'RES-J   ' ,'TIDEREV ',
     F 'ADJUST-T' ,'STAGEREV' ,'ADJUST-H' ,'SET-TS  ',
     G 'DHM-OP  ' ,'        ' ,'        ' ,'        ',
     H '        ' ,'        ' ,'        ' ,'        ',
     I 9*'        '/
      DATA STOPID/'STOP'/
      DATA BLANK/' '/
C.......................................
C     TRACE LEVEL FOR THIS ROUTINE=3
C
      IF (ITRACE.GE.3) WRITE(IODBUG,902)
  902 FORMAT ('0','** FOPCDX ENTERED')
      IF ((NUMOP.GT.0).AND.(NUMOP.LE.80)) GO TO 125
         NUMOP=0
         OPID=BLANK
         GO TO 190
  125 OPID=OPNAME(NUMOP)
C.......................................
C     DEBUG OUTPUT.
C
  190 IF (IFBUG('SEGI').EQ.1) GO TO 195
      GO TO 199
  195 WRITE (IODBUG,901) OPID,NUMOP
  901 FORMAT ('0','   ROUTINE FOPCDX DEBUG-',5X,'I.D.=',A8,5X,
     1 'NUMBER=',I3)
C.......................................
C
199   RETURN
C
      END
