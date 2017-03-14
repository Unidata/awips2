C MEMBER DFCHKH
C  (from old member DFDSPOST)
C
C ----------------------------------------------------------------------
C
C @PROCESS LVL(77)
C
      SUBROUTINE DFCHKH (ILINES)
C
C
C ----------------------------------------------------------------------
C
C    DESCRIPTION
C
C    THIS ROUTINE WILL CHECK TO SEE IF THERE IS ENOUGH ROOM TO WRITE I
C    NUMBER OF LINES ON A PAGE.  IF NOT ENOUGH ROOM, START A NEW PAGE
C    AND WRITE A HEADER, THEN RETURN TO THE CALLING ROUTINE.
C
C ----------------------------------------------------------------------
C
      INCLUDE 'uio'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shefpost/RCS/dfchkh.f,v $
     . $',                                                             '
     .$Id: dfchkh.f,v 1.1 1995/09/17 19:26:11 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      CALL ULINEL (LP,ILINES,IRETRN)
      IF (IRETRN.GT.0) THEN
         CALL ULINE(LP,3)
         WRITE (LP,10)
      ENDIF
C
C
10    FORMAT('0',2X,'STATION ID',2X,'DATA TYPE',2X,'UNITS',2X,'SHEF',
     1    ' CODE',3X,'OBSERVATION DATE(TZC)',5X,'VALUE',6X,'DATA QUAL',
     2    2X,'REV CODE',3X,'DATA SOURCE',4X,'TIMES SERIES'/)
C
      RETURN
      END
