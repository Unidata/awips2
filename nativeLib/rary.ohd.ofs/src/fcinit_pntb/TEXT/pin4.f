C MODULE PIN4
C-----------------------------------------------------------------------
C
      SUBROUTINE PIN4 (TC,LEFT,IUSET,NXT,TS,MTS)
C
C  THIS IS THE INPUT SUBROUTINE FOR THE CLEAR OPERATION.
C
C  THIS OPERATION ONLY HAS AN ENTRY IN THE T ARRAY.
C  NO ENTRIES IN THE P OR C ARRAYS.
C
C  ROUTINE INITIALLY WRITTEN BY ERIC ANDERSON - HRL - 9/1979
C
      CHARACTER*8 TSID
C
      DIMENSION TS(MTS)
      INTEGER TC(*)
C
      INCLUDE 'common/ionum'
      INCLUDE 'common/fdbug'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_pntb/RCS/pin4.f,v $
     . $',                                                             '
     .$Id: pin4.f,v 1.2 2001/06/13 09:36:30 mgm Exp $
     . $' /
C    ===================================================================
C.
C
      IF (ITRACE.GE.1) WRITE(IODBUG,*) 'ENTER PIN4'
C
C  READ INFORMATION FOR TIME SERIES TO BE CLEARED.
      READ (IN,901) TSID,TYPE,IDT
  901 FORMAT (2X,A,1X,A4,3X,I2)
C
C  CHECK IF TIME SERIES EXISTS - FIND TS LOCATION
      CALL FINDTS (TSID,TYPE,IDT,LOCD,LOCTS,DIMN)
      IF (LOCTS.LE.0) GO TO 100
C
C  CALL CLEAR SUBROUTINE
      IDT=-IDT
      CALL CLEAR (TSID,TYPE,IDT,LOCD,LOCTS,TC,LEFT,IUSET,NXT,TS,MTS)
      GO TO 199
C
C  TIME SERIES DOES NOT EXIST.
  100 WRITE (IPR,902) TSID,TYPE,IDT
  902 FORMAT ('0**ERROR** TIME SERIES TO BE CLEARED HAS NOT ',
     * 'BEEN DEFINED : TSID=',A,' TYPE=',A4,' IDT=',I2,' HOURS')
      CALL ERROR
      IUSET=0
C
  199 IF (ITRACE.GE.1) WRITE (IODBUG,*) 'EXIT PIN4'
C
      RETURN
C
      END
