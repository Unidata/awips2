C MEMBER FRICT
C  (from old member FCFLOOPR)
C
C
C  DESC -- COMPUTES MANNING'S N FOR GIVEN WATER SURFACE ELEVATION
C @PROCESS LVL(77)
C
C.......................................................................
C
      SUBROUTINE FRICT(Y,CM,DCM)
C
C.......................................................................
C
C       THIS SUBROUTINE COMPUTES MANNINGS  N  COEFFICIENT FOR A
C      SPECIFIED WATER SURFACE ELEVATION.
C
C  ARGUMENT LIST:
C     Y      - SPECIFIED WATER SURFACE ELEVATION
C     CM     - MANNING'S N VALUE AT SPEC ELEV
C     DCM    - CHANGE IN MANNING'S N WITH ELEVATION
C
C.......................................................................
C
C  SUBROUTINE ORIGINALLY WRITTEN BY --
C      DANNY FREAD -- HRL --731101
C  CONVERTED FOR NWSRFS BY --
C      JONATHAN WETMORE - HRL -801031
C
C......................................................................
      INCLUDE 'common/fratng'
      INCLUDE 'common/facxsc'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_rc/RCS/frict.f,v $
     . $',                                                             '
     .$Id: frict.f,v 1.2 1998/07/02 17:11:34 page Exp $
     . $' /
C    ===================================================================
C
      LXE1=LXELEV
      LXEN=LXE1-1+NCROSS
      IF(Y.LE.XRC(LXE1)) THEN
        CM=CMANGN(1)
        DCM=0.0
        GO TO 35
      END IF
      IF(Y.GE.XRC(LXEN)) THEN
        CM=CMANGN(NCROSS)
        DCM=0.0
        GO TO 35
      END IF
      DO 20 I=2,NCROSS
      IF (Y-XRC(LXE1-1+I)) 10,10,20
 10   KT=I
      GO TO 30
 20   CONTINUE
 30   KL=KT-1
      DCM=(CMANGN(KT)-CMANGN(KL))/(XRC(LXE1-1+KT)-XRC(LXE1-1+KL))
      CM=CMANGN(KL)+DCM*(Y-XRC(LXE1-1+KL))
      IF(CM.LE.0.01) CM=0.01
      IF(CM.GE.0.30) CM=0.30
   35 RETURN
      END
