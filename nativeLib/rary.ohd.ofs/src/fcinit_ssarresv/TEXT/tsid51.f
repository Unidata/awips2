C MEMBER SARP51
C--------------------------------------------------------------------
C
C@PROCESS LVL(77)
C
      SUBROUTINE TSID51(TSID,DTYPE,IDT,TSOK)
C
C---------------------------------------------------------------------
C  ARGS:
C     TSID - 8 CHARACTER ID OF TIME SERIES
C    DTYPE - DATATYPE OF TIME-SERIES
C      IDT - TIME INTERVAL OF TIME SERIES
C     TSOK - LOGICAL VARIABLE INDICATING THAT INFO HAS BEEN READ IN OK.
C---------------------------------------------------------------------
C
C  KUANG HSU - HRL - OCTOBER 1994
C----------------------------------------------------------------
      INCLUDE 'common/fld51'
C
      DIMENSION TSID(2)
      LOGICAL TSOK
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_ssarresv/RCS/tsid51.f,v $
     . $',                                                             '
     .$Id: tsid51.f,v 1.1 1996/03/21 14:41:30 page Exp $
     . $' /
C    ===================================================================
C
C
      TSOK = .FALSE.
C
C  ALWAYS LOOK FOR NEXT FIELD ON LINE AND NOTHING MORE
C
      NUMFLD = -2
      CALL UFLD51(NUMFLD,IERF)
      IF (IERF.GT.0) GO TO 9000
C
C  FIRST FIELD IS TS ID
C
      NUMWD = (LEN-1)/4 + 1
      IF (NUMWD.LE.2) GO TO 220
C
      CALL STER51(20,1)
      GO TO 9999
C
  220 CONTINUE
      DO 225 I=1,2
      TSID(I) = CHAR(I)
  225 CONTINUE
C
C  NEXT FIELD IS DATATYPE
C
      NUMFLD = -2
      CALL UFLD51(NUMFLD,IERF)
      IF (IERF.GT.0) GO TO 9000
C
      NUMWD = (LEN-1)/4 + 1
      IF (NUMWD.EQ.1) GO TO 230
C
      CALL STER51(20,1)
      GO TO 9999
C
  230 CONTINUE
      DTYPE = CHAR(1)
C
C  NEXT FIELD IS TIME INTERVAL. MUST BE POSITIVE INTEGER.
C
      NUMFLD = -2
      CALL UFLD51(NUMFLD,IERF)
      IF (IERF.GT.0) GO TO 9000
C
      IF (ITYPE.EQ.0) GO TO 240
C
      CALL STER51(5,1)
      GO TO 9999
C
  240 CONTINUE
      IF (INTEGR .GT. 0) GO TO 250
C
      CALL STER51(61,1)
      GO TO 9999
C
C  EVERYTHING OK IF WE REACH HERE
C
  250 CONTINUE
      IDT = INTEGR
      TSOK = .TRUE.
      GO TO 9999
C
 9000 CONTINUE
      IF (IERF.EQ.1) CALL STER51(19,1)
      IF (IERF.EQ.2) CALL STER51(20,1)
      IF (IERF.EQ.3) CALL STER51(21,1)
      IF (IERF.EQ.4) CALL STER51(1,1)
C
 9999 CONTINUE
      RETURN
      END
