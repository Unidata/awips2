C MEMBER TSID26
C  (from old member FCTSID26)
C
C DESC GETS THE ID, DATATYPE, AND TIME INTERVAL FROM A LINE OF INPUT
C---------------------------------------------------------------------
C
      SUBROUTINE TSID26(TSID,DTYPE,IDT,TSOK)
C
C---------------------------------------------------------------------
C  ARGS:
C     TSID - 8 CHARACTER ID OF TIME SERIES
C    DTYPE - DATATYPE OF TIME-SERIES
C      IDT - TIME INTERVAL OF TIME SERIES
C     TSOK - LOGICAL VARIABLE INDICATING THAT INFO HAS BEEN READ IN OK.
C---------------------------------------------------------------------
C
C  JTOSTROWSKI - HRL - MARCH 1983
C----------------------------------------------------------------
      INCLUDE 'common/fld26'
C
      DIMENSION TSID(2)
      LOGICAL TSOK
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_res/RCS/tsid26.f,v $
     . $',                                                             '
     .$Id: tsid26.f,v 1.1 1995/09/17 18:53:24 dws Exp $
     . $' /
C    ===================================================================
C
C
      TSOK = .FALSE.
C
C  ALWAYS LOOK FOR NEXT FIELD ON LINE AND NOTHING MORE
C
      NUMFLD = -2
      CALL UFLD26(NUMFLD,IERF)
      IF (IERF.GT.0) GO TO 9000
C
C  FIRST FIELD IS TS ID
C
      NUMWD = (LEN-1)/4 + 1
      IF (NUMWD.LE.2) GO TO 220
C
      CALL STER26(20,1)
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
      CALL UFLD26(NUMFLD,IERF)
      IF (IERF.GT.0) GO TO 9000
C
      NUMWD = (LEN-1)/4 + 1
      IF (NUMWD.EQ.1) GO TO 230
C
      CALL STER26(20,1)
      GO TO 9999
C
  230 CONTINUE
      DTYPE = CHAR(1)
C
C  NEXT FIELD IS TIME INTERVAL. MUST BE POSITIVE INTEGER.
C
      NUMFLD = -2
      CALL UFLD26(NUMFLD,IERF)
      IF (IERF.GT.0) GO TO 9000
C
      IF (ITYPE.EQ.0) GO TO 240
C
      CALL STER26(5,1)
      GO TO 9999
C
  240 CONTINUE
      IF (INTEGR .GT. 0) GO TO 250
C
      CALL STER26(61,1)
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
      IF (IERF.EQ.1) CALL STER26(19,1)
      IF (IERF.EQ.2) CALL STER26(20,1)
      IF (IERF.EQ.3) CALL STER26(21,1)
      IF (IERF.EQ.4) CALL STER26(1,1)
C
 9999 CONTINUE
      RETURN
      END
