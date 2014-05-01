C MODULE HX1126
C
      SUBROUTINE HX1126(IDEST,PLUS,RMINUS,HX,GETRUL,OKX)
C---------------------------------------------------------------------
C  SUBROUTINE TO READ AND INTERPRET ELEVATION INPUT FOR S/U #11
C   INDUCED SURCHARGE SCHEME
C---------------------------------------------------------------------
C  K HSU - HRL - NOVEMBER 1995
C----------------------------------------------------------------
C
      INCLUDE 'ufreex'
      INCLUDE 'common/comn26'
      INCLUDE 'common/err26'
      INCLUDE 'common/fld26'
      INCLUDE 'common/rc26'
      INCLUDE 'common/read26'
      INCLUDE 'common/warn26'
C
      DIMENSION TVAL(3)
      LOGICAL OKX,GETRUL
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_res/RCS/hx1126.f,v $
     . $',                                                             '
     .$Id: hx1126.f,v 1.2 1998/07/02 19:35:59 page Exp $
     . $' /
C    ===================================================================
C
C
      DATA RULE/4HRULE/
C
C  IF FIELD IS CHARACTER, LOOK FOR THE WORD 'RULE'.
C
      IF(ITYPE.GT.1)GETRUL=.TRUE.
      IF (ITYPE.GT.1) GO TO 1160
C
C  FIELD IS NUMERIC. SEE IF VALUE IS REAL POSITIVE AND WITHIN THE BOUNDS
C  OF THE ELVSSTOR CURVE.
C
      IF(ITYPE.EQ.1)GO TO 1120
      CALL STER26(4,1)
      GO TO 10
C
 1120  IF(REAL.GT.0.0)GO TO 1130
      CALL STER26(61,1)
      GO TO 10
C
 1130  REAL = REAL/CONVL
      CALL ELST26(REAL,1,IERST)
      IF(IERST.GT.0)GO TO 10
C
C  SAVE AS OK
C
      HX=REAL
      GO TO 1190
C
C  CHARACTER FIELD FOUND. IT MUST BE 'RULE'
C  OR IT CAN BE 'RULE +(-) FACTOR'
C
 1160  CONTINUE
C
       IF (IUSAME(CHAR,RULE,1).EQ.1) GO TO 1165
C
       CALL STRN26(59,1,RULE,1)
       GO TO 10
C
C  DETERMINE IF 'RULE' IS FOLLOWED BY A '+ FACTOR'.
C
 1165 IEND=ISTRT+LEN-1
      CALL USCHBF (PLUS,ISTRT,IEND,IX)
C
C  IF IX = 0 , NO '+' WAS FOUND.
C
      IF(IX.NE.0)ISIGN=0
      IF(IX.NE.0)GO TO 1168
C
C  DETERMINE IF 'RULE' IS FOLLOWED BY A '- FACTOR'.
C
      CALL USCHBF (RMINUS,ISTRT,IEND,IX)
C
C  IF IX = 0, NO '-' WAS FOUND.
C
      IF(IX.NE.0)ISIGN=1
      IF(IX.NE.0)GO TO 1168
      GO TO 1180
C
C  '+' OR '-' FOUND. GO GET CHARACTER STRING FOLLOWING IT. LOOKING FOR A
C  NUMBER, EITHER INTEGER OR REAL.
C
 1168 IXS = IX - ISTRT + 1
       CALL UAFT26(3,TVAL,ISTRT,IXS,LEN,IERA)
C
      IF(IERA.EQ.0)GO TO 1170
      IF(IERA.EQ.-1)IERN=4
      IF(IERA.EQ.2)IERN=20
      CALL STER26(IERN,6)
      GO TO 10
C
C  CONVERT CHARACTER STRING INTO NUMERIC REPRESENTATION
C
 1170  CONTINUE
      IXSS=IXS+1
      CALL UFRLFX(FACT,ISTRT,IXSS,LEN,0,IERR)
      IF(IERR.EQ.0)GO TO 1175
C
      CALL STER26(4,6)
      GO TO 10
C
C  ADD(SUBTRACT) FACTOR TO(FROM) -999. FOR STORING IN WORK ARRAY.
C
 1175  CONTINUE
      IF(ISIGN.EQ.0)HX=-999. + FACT/CONVL
      IF(ISIGN.EQ.1)HX=-999. - FACT/CONVL
      GO TO 1190
C
C  STORE ELEVATION AS -999.0 TO INDICATE RULE CURVE USAGE
C
 1180  HX=-999.0
C
C EVERYTHING IS OK
C
 1190  CONTINUE
      OKX = .TRUE.
 10   CONTINUE
      RETURN
      END
