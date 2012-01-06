C MEMBER EFINTS
C  (from old member EEFINTS)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 11/14/94.13:13:35 BY $WC20SV
C
C @PROCESS LVL(77)
C
      SUBROUTINE EFINTS (USERID,DTYPE,ITIME,LOCD,LOCTS)
C.......................................
C     THIS FORECAST COMPONENT UTILITY ROUTINE LOCATES A
C        TIME SERIES AND THE INFORMATION ASSOCIATED WITH
C        IT IN THE TS ARRAY BY USING THE USER 8-CHARACTER
C        I.D., THE DATA TYPE CODE, AND THE TIME INTERVAL.
C.......................................
C   THIS ROUTINE WAS MODIFIED BY GERALD N DAY FROM ONE
C   ORIGINALLY WRITTEN BY ERIC ANDERSON.
C.......................................
      DIMENSION USERID(2)
C
C     COMMON BLOCKS
      INCLUDE 'common/fdbug'
      INCLUDE 'common/ets'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/espinit/RCS/efints.f,v $
     . $',                                                             '
     .$Id: efints.f,v 1.1 1995/09/17 18:46:22 dws Exp $
     . $' /
C    ===================================================================
C
C
C     DATA STATEMENTS
      DATA BLANK/4H    /
      DATA IBUG/4HSEGI/
      DATA REPL/4HREPL/
C.......................................
C     TRACE LEVEL FOR THIS ROUTINE=3
      IF (ITRACE.GE.3) WRITE (IODBUG,900)
  900 FORMAT (1H0,17H** FINDTS ENTERED)
C.......................................
C     START CHECKING AT THE BEGINNING OF THE TS ARRAY.
      LOCTS=1
C
C     CHECK TO DETERMINE IF THERE IS ANOTHER ENTRY.
  100 IT=TSESP(LOCTS)
      IF (IT.EQ.0) GO TO 190
C.......................................
C     CHECK FOR A MATCH OF THE INDENTIFIERS.
      IF (TSESP(LOCTS+2).NE.USERID(1)) GO TO 110
      IF (TSESP(LOCTS+3).NE.USERID(2)) GO TO 110
      IF (TSESP(LOCTS+4).NE.DTYPE) GO TO 110
      IT = TSESP(LOCTS+5)
      IF (IT.NE.ITIME) GO TO 110
C.......................................
C   MAKE SURE TIME SERIES HAS NOT BEEN REPLACED
C
      FTYPE=TSESP(LOCTS+9)
      IF(FTYPE.NE.REPL) GO TO 105
      GO TO 120
C
C     TIME SERIES HAS BEEN FOUND
  105 LOCD=TSESP(LOCTS+7)
C
      GO TO 195
C
C   TIME SERIES DO NOT MATCH, CHECK IF THE TIME SERIES HAS
C   BEEN REPLACED.
C
  110 FTYPE=TSESP(LOCTS+9)
      IF(FTYPE.NE.REPL) GO TO 120
      NV=TSESP(LOCTS+11)
      NADD=TSESP(LOCTS+12+NV)
      IF(TSESP(LOCTS+15+NV+NADD).NE.USERID(1)) GO TO 120
      IF(TSESP(LOCTS+16+NV+NADD).NE.USERID(2)) GO TO 120
      IF(TSESP(LOCTS+17+NV+NADD).NE.DTYPE) GO TO 120
      IT=TSESP(LOCTS+18+NV+NADD)
      IF(IT.NE.ITIME) GO TO 120
C
C   THE REPLACE TIME SERIES IS A MATCH.
C
      GO TO 105
C.......................................
C     INCREMENT TO THE NEXT TIME SERIES.
  120 NTS=TSESP(LOCTS+1)
      LOCTS=NTS
      IF (LOCTS.LE.MTSESP) GO TO 100
C.......................................
C     TIME SERIES CAN NOT BE FOUND.
  190 LOCTS=0
      LOCD=0
      DIMN=BLANK
C.......................................
C     DEBUG OUTPUT.
  195 IF (IFBUG(IBUG).EQ.1) GO TO 196
      GO TO 199
  196 WRITE (IODBUG,901) USERID,DTYPE,ITIME,LOCD,LOCTS,DIMN
  901 FORMAT (1H0,47HFINDTS DEBUG OUTPUT-VALUES OF THE ARGUMENTS ARE,2X,
     12A4,2X,A4,I4,I6,I5,2X,A4)
  199 CONTINUE
C.......................................
      RETURN
C
      END
