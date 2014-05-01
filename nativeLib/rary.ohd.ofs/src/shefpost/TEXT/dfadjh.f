C MODULE DFADJH
C-----------------------------------------------------------------------
C
      SUBROUTINE DFADJH (MSHPDB,ISHPDB,NSHPDB,JULHR,INDEX,ITYPE,ISCODE,
     *   IERR)
C
C  THIS ROUTINE ADJUSTS JULIAN HOUR TO BE A SYNOPTIC HOUR IF JULIAN
C  HOURS FALL WITHIN A CERTAIN WINDOW DEFINED FOR THE DATA TYPE.
C
C  ARGUMENT LIST:
C
C    NAME      TYPE   I/O   DIM   DESCRIPTION
C    ------    ----   ---   ---   -----------
C    JULHR      I     I/O    1    INPUT JULIAN HOUR, OUTPUT ADJUSTED
C    INDEX      I      I     1    INDEX OF DATA TYPE IN ISHPDD
C    ITYPE      A4     I     1    DATA TYPE CODE (PPDB)
C    ISCODE     A8     I     2    SHEF EXPANDED PARAMETER CODE
C    IERR       I      I     1    ERROR FLAG
C
      DIMENSION ISHPDB(9,MSHPDB)
C
      INCLUDE 'uiox'
      INCLUDE 'udebug'
      INCLUDE 'hclcommon/hdflts'
C
      DIMENSION ISCODE(2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shefpost/RCS/dfadjh.f,v $
     . $',                                                             '
     .$Id: dfadjh.f,v 1.2 2002/02/11 21:28:51 dws Exp $
     . $' /
C    ===================================================================
C
      DATA ITFMX/4HTFMX/
      DATA IPP24/4HPP24/,IPPDR/4HPPDR/
      DATA ITMAX/4HZXZ /
      DATA ITMIN/4HMNZ /
C
C
      IF (IDETR.GT.0) WRITE (IOGDB,*) 'ENTER DFADJH'
C
      IERR=0
C
C  SEE IF WE CARE ABOUT A SYNOPTIC HOUR (IF ISHPDB(7,INDEX)=0) WE DONT)
      IF (ISHPDB(7,INDEX).EQ.0) GO TO 70
      IHZ=LOCAL-NLSTZ
      IF (IHZ.GT.23) IHZ=IHZ-24
      IF (IHZ.LT.0) IHZ=IHZ +24
C
C  GET OFFSET FROM END OF HYDRO DAY
      IOFF=MOD(JULHR-IHZ,24)
C
C  IF OFFSET IS GREATER THAN 12, GO UP TO NEXT HOUR
      IF (IOFF.EQ.0) GO TO 70
      IF (IOFF.GT.12) IOFF=IOFF-24
C
C  NOW ADJUST FOR SYNOPTIC TIME - CHECK FOR A 24 HOUR TYPE
      IF (ISHPDB(7,INDEX).NE.2 .AND.
     *    ISHPDB(7,INDEX).NE.7 .AND.
     *    ISHPDB(7,INDEX).NE.8) GO TO 30
C
C  THIS IS A 24 HOUR TYPE, MAKE SURE WE HAVE HZ + OR - 2
      IF (ISCODE(2).EQ.ITMAX .OR. ISCODE(2).EQ.ITMIN) THEN
         IF (IOFF.GE.-2.AND.IOFF.LE.2 .OR.
     *       IOFF.GE.-8.AND.IOFF.LE.-3 ) THEN
             GO TO 20
            ELSE
               IERR=1
            ENDIF
         ELSE
            IF (IOFF.GE.-2.AND.IOFF.LE.2) GO TO 20
         ENDIF
C
C  OFFSET GREATER THAN 2 HOURS
      IF (ITYPE.NE.IPP24) GO TO 50
C
C  GREATER THAN 2 HOUR OFFSET OK FOR PPPR BUT NOT FOR PPDR.
      IF (ISCODE(1).EQ.IPPDR) THEN
         IF (IOFF.GE.-2.AND.IOFF.LE.4) GO TO 20
         IERR=1
         ELSE
            IOFF2=IOFF
            IF (IOFF2.LT.0) IOFF2=IOFF2+24
            I3HR=MOD(IOFF2,3)
            IF (I3HR.NE.0) THEN
               I6HR=MOD(IOFF2,6)
               IF (I6HR.EQ.2) JULHR=JULHR-2
               IF (I6HR.EQ.1) JULHR=JULHR-1
               IF (I6HR.EQ.5) JULHR=JULHR+1
               IF (I6HR.EQ.4) JULHR=JULHR+2
            ENDIF
         ENDIF
      GO TO 50
C
20    JULHR=JULHR-IOFF
      GO TO 50
C
C  PERHAPS NEED TO ADJUST MAX TEMP TO REPORT ON END OF HYDRO DAY
30    IF (ITYPE.NE.ITFMX) GO TO 40
      IF (IOFF.EQ.12) JULHR=JULHR+12
      GO TO 50
C
C  FOR SIX HOUR TYPE + OR - ONE HOUR OFFSET IS OKAY
40    IF (ISHPDB(7,INDEX).NE.4) GO TO 70
      IWK=IOFF
      IF (IOFF.LT.0) IWK=24+IOFF
      I6=MOD(IWK,6)
      IF (I6.EQ.1) JULHR=JULHR-1
      IF (I6.EQ.2) JULHR=JULHR-2
      IF (I6.EQ.5) JULHR=JULHR+1
      IF (I6.EQ.4) JULHR=JULHR+2
C
50    IF (IDETR.GT.0) WRITE (IOGDB,*) 'EXIT DFADJH - JULHR=',JULHR,
     *   ' INDEX=',INDEX
C
70    RETURN
C
      END
