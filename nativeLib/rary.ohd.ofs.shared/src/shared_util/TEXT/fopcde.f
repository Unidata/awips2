C MEMBER FOPCDE
C  (from old member FCFOPCDE)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 12/01/2000 BY TLS
C
C @PROCESS LVL(77)
C
      SUBROUTINE FOPCDE (OPID,NUMOP)
C.......................................
C     THIS ROUTINE CONTAINS THE ACCEPTABLE OPERATION IDENTIFIERS
C
C        FOR THE FORECAST COMPONENT.  FOR A GIVEN OPERATION I.D.,
C        THIS ROUTINE RETURNS THE APPROPRIATE INTERNAL
C        OPERATION NUMBER - = 0 IF THE OPERATION I.D. IS INCORRECT,
C        =-1 FOR THE STOP OPERATION.
C     SUBPROGRAM FOPCDX GIVES OPERATION I.D. FOR A GIVEN NUMBER.
C.......................................
C     ROUTINE INITIALLY WRITTEN BY. . .
C
C            ERIC ANDERSON - HRL     JUNE 1979
c
C            Lec C.    ADDED DISTRIBUTED HYDROLOGICAL MODELLING OPERATION (64)   SEPT 2005
C.......................................
C
C
      CHARACTER*8 OPID,STOPID,BLANK
      CHARACTER*8 OPNAME(80)
C
C     COMMON BLOCK
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
C
C  =================================== RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_util/RCS/fopcde.f,v $
     . $',                                                             '
     .$Id: fopcde.f,v 1.8 2006/03/28 14:58:06 aivo Exp $
     . $' /
C  =====================================================================
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
      IF (ITRACE.GE.3) WRITE (IODBUG,900)
  900 FORMAT ('0','** FOPCDE ENTERED')
C     NO ERROR MESSAGES FOR THIS ROUTINE.
C.......................................
C     CHECK FOR STOP CODE.
C
      IF (OPID.NE.STOPID) GO TO 100
      NUMOP=-1
      GO TO 190
C.......................................
C     CHECK FOR ALL BLANKS -- NOT ACCEPTABLE.
C
  100 IF (OPID.NE.BLANK) GO TO 110
      NUMOP=0
      GO TO 190
C.......................................
C     SEARCH FOR MATCH WITH OPERATION I.D.
C
  110 DO 115 I=1,80
      IF (OPID.NE.OPNAME(I)) GO TO 115
C
C     IDENTIFIER WAS FOUND.
      NUMOP=I
      GO TO 190
  115 CONTINUE
C
C     IDENTIFIER DOES NOT MATCH
      NUMOP=0
      GO TO 195
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
