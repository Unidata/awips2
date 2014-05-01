C MEMBER FSECT
C  (from old member FCFLOOPR)
C
C
C  DESC -- COMPUTE GEOMETRIC PROPERTIES OF X-SECT AT GIVEN ELEV.
C @PROCESS LVL(77)
C
C.......................................................................
C
      SUBROUTINE FSECT (Y,A,B,BO,R,DB,DBO,DR,FK)
C
C.......................................................................
C
C  THIS SUBROUTINE COMPUTES THE GEOMETRICAL PROPERTIES OF THE
C  CROSS-SECTION AT A SPECIFIED WATER SURFACE ELEVATION.
C
C  ARGUMENT LIST:
C       Y      - SPECIFIED ELEVATION
C       A      - X-SECTIONAL AREA BELOW SPEC. ELEV.
C       B      - ACTIVE X-SECTION TOPWIDTH AT SPEC. ELEV.
C       BO     - OFF CHANNEL TOPWIDTH AT SPEC. ELEV.
C       R      - HYDRAULIC DEPTH AT SPEC. ELEV.
C       DB     - CHANGE IN ACTIVE TOPWIDTH WITH ELEV AT SPEC ELEV
C       DBO    - CHANGE IN OFF-CHANNEL TOPWIDTH WITH ELEV
C       DR     - CHANGE IN HYDRAULIC DEPTH WITH ELEV
C       FK     - CELERITY COEFFICIEN AT SPEC. ELEV.
C
C.......................................................................
C  SUBROUTINE ORIGINALLY WRITTEN BY --
C      DANNY FREAD -- HRL --731101
C  CONVERTED FOR NWSRFS BY --
C      JONATHAN WETMORE - HRL -801031
C
C.......................................................................
      INCLUDE 'common/fratng'
      INCLUDE 'common/facxsc'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_rc/RCS/fsect.f,v $
     . $',                                                             '
     .$Id: fsect.f,v 1.1 1995/09/17 19:05:17 dws Exp $
     . $' /
C    ===================================================================
C
      DO 20 K=2,NCROSS
      IF (Y-XRC(LXELEV-1+K))10,10,20
   10 KT=K
      GO TO 30
   20 CONTINUE
      KT=NCROSS
   30 KL=KT-1
      DB=(ACTW(KT)-ACTW(KL))/(XRC(LXELEV-1+KT)-XRC(LXELEV-1+KL))
      B=ACTW(KL)+DB*(Y-XRC(LXELEV-1+KL))
      IF(B.LE.1.0) B=1.0
      DADH=(ACAREA(KT)-ACAREA(KL))/(XRC(LXELEV-1+KT)-XRC(LXELEV-1+KL))
      A=ACAREA(KL)+DADH*(Y-XRC(LXELEV-1+KL))
      IF(A.LE.1.0) A=1.0
      ALESS =A-ACAREA(KL)
      FK=(ACAREA(KL)*CFK(KL)+(5./3.-(2./3.)*ALESS/B/B*DB)*ALESS)/A
      IF(FK.LE.1.0) FK=1.0
      R=(A/B)**(2./3.)
      DR=2./3.*R*(B/A-DB/B)
      IF (NOCS) 301,301,98
   98 IF(Y-XRC(LXELOC)) 301,301,99
  99  DO 200 K=2,NOCS
      IF (Y-XRC(LXELOC-1+K))100,100,200
  100 KT=K
      GO TO 300
  200 CONTINUE
      KT=NOCS
  300 KL=KT-1
      DBO=(OCTW(KT)-OCTW(KL))/(XRC(LXELOC-1+KT)-XRC(LXELOC-1+KL))
      BO=OCTW(KL)+DBO*(Y-XRC(LXELOC-1+KL))
      RETURN
 301  BO=0.
      DBO=0.
      RETURN
      END
