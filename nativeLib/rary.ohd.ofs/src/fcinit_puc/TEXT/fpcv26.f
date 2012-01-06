C MEMBER FPCV26
C  (from old member FCPUC26)
C-----------------------------------------------------------------------
C                             LAST UPDATE: 07/10/95.11:45:28 BY $WC21DT
C
C***********************************************************************
C
C @PROCESS LVL(77)
C
      SUBROUTINE FPCV26(T1,T2,T3,IDAY,X,Y,INT,NV,MULT)
C
C     ROUTINE PUNCHES INPUT CARDS CONTAINING SETS OF VALUES
C     (X VS. Y  OR  IDAY VS. Y) OF THE CURVES USED BY THE RESERVOIR
C     OPERATION ,E.G., RULECURVE, RATING CURVES.
C     THE CURVE NAME IS PUNCHED ON THE FIRST CARD AND AN AMPERSAND
C     MUST BE PUNCHED IN COLUMN 72 FOR ANY CONTINUATION CARDS.
C     ROUTINE IS ALSO USED TO PUNCH MULTI-VALUED SINGLE VARIABLES.
C
C***********************************************************************
C     PROGRAMMED BY KAY KROUSE   JUNE 1983
C***********************************************************************
      DIMENSION IDAY(1),X(1),Y(1),TITLE(3)

      CHARACTER*4  FNV(5)
      CHARACTER*24 FMT1
      CHARACTER*28 FMT2,FMT3

      INCLUDE 'common/ionum'
      COMMON/FPOR26/IXY26
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_puc/RCS/fpcv26.f,v $
     . $',                                                             '
     .$Id: fpcv26.f,v 1.7 2004/04/19 13:36:12 hsu Exp $
     . $' /
C    ===================================================================
C
      DATA BLANK,BLNK1,AMP/4H    ,1H ,1H&/
      DATA FMT1/'(3A4,       I10,T72,A1) '/
      DATA FMT2/'(3A4,       F11.2,T72,A1)   '/
      DATA FMT3/'(3A4,       F11.0,T72,A1)   '/
      DATA FNV/'   1','   2','   3','   4','   5'/
C
      RNV=NV
      RNV=RNV/5.
      NV5=NV/5
      DIFF=RNV-NV5
      IF(DIFF.GT..001)NV5=NV5+1
      IF(MULT.GT.0)GO TO 30
C
      TITLE(1)=T1
      TITLE(2)=T2
      TITLE(3)=T3
      A=AMP
C
C      PUNCH 'X' OR 'IDAY' VALUES OF CURVE IF IXY26 IN /FPOR26/ = 0
C   ELSE IF IXY26 = 1, PUNCH Y VALUES FIRST
C
      JB=1
      JE=NV
      IF(JE.GT.5)JE=5
      DO 10 I=1,NV5
      IF(I.EQ.1)GO TO 5
      TITLE(1)=BLANK
      TITLE(2)=BLANK
      TITLE(3)=BLANK
  5   JP=JE-JB+1
      FMT1(9:12)=FNV(JP)
      FMT2(9:12)=FNV(JP)
      FMT3(9:12)=FNV(JP)
      IF (IXY26 .EQ. 0) GO TO 7
      IF (Y(NV).GE.100000.) THEN
        WRITE(IPU,FMT3)TITLE,(Y(J),J=JB,JE),A
      ELSE
        WRITE(IPU,FMT2)TITLE,(Y(J),J=JB,JE),A
      ENDIF
      GO TO 9
    7 CONTINUE
      IF(INT.EQ.0)WRITE(IPU,FMT1)TITLE,(IDAY(J),J=JB,JE),A
      IF(INT.GT.0)WRITE(IPU,FMT2)TITLE,(X(J),J=JB,JE),A
    9 CONTINUE
      JB=JE+1
      JE=JB+4
      IF(JE.GT.NV)JE=NV
 10   CONTINUE
C     PUNCH 'Y' VALUES OF CURVE IF IXY26 = 0, ELSE PUNCH 'X' OR 'IDAY'
C    VALUES.
C
      JB=1
      JE=NV
      TITLE(1)=BLANK
      TITLE(2)=BLANK
      TITLE(3)=BLANK
      IF(JE.GT.5)JE=5
      DO 20 I=1,NV5
      IF(I.EQ.NV5)A=BLNK1
      JP=JE-JB+1
      FMT3(9:12)=FNV(JP)
      FMT2(9:12)=FNV(JP)
      FMT1(9:12)=FNV(JP)
      IF (IXY26 .EQ. 0) GO TO 17
      IF(INT.EQ.0)WRITE(IPU,FMT1)TITLE,(IDAY(J),J=JB,JE),A
      IF(INT.GT.0)WRITE(IPU,FMT2)TITLE,(X(J),J=JB,JE),A
      GO TO 19
   17 CONTINUE
      IF (Y(NV).GE.100000.) THEN
        WRITE(IPU,FMT3)TITLE,(Y(J),J=JB,JE),A
      ELSE
        WRITE(IPU,FMT2)TITLE,(Y(J),J=JB,JE),A
      ENDIF
   19 CONTINUE
      JB=JE+1
      JE=JB+4
      IF(JE.GT.NV)JE=NV
 20   CONTINUE
      GO TO 50
C
C     PUNCH VALUES FOR SINGLE VARIABLE 'X'
C
 30   TITLE(1)=T1
      TITLE(2)=T2
      TITLE(3)=T3
      A=AMP
      JB=1
      JE=NV
      IF(JE.GT.5)JE=5
      DO 40 I=1,NV5
      IF(I.EQ.1)GO TO 45
      TITLE(1)=BLANK
      TITLE(2)=BLANK
      TITLE(3)=BLANK
 45   IF(I.EQ.NV5 .AND. MULT.NE.2)A=BLNK1
      JP=JE-JB+1
      FMT2(9:12)=FNV(JP)
      WRITE(IPU,FMT2)TITLE,(X(J),J=JB,JE),A
      JB=JE+1
      JE=JB+4
      IF(JE.GT.NV)JE=NV
 40   CONTINUE
  50  CONTINUE
C**********************************************************************
      RETURN
      END
