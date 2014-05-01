C MEMBER FLOC26
C  (from old member FCPUC26)
C-----------------------------------------------------------------------
C                             LAST UPDATE: 10/31/95.09:32:57 BY $WC21DT
C @PROCESS LVL(77)
C
      SUBROUTINE FLOC26(PO,LO,NU,INM)
C
C     FOR A MULTIPLE REFERENCED PARAMETER THIS ROUTINE DETERMINES THE
C     COMPUTATIONAL SCHEME(NU) AND VERSION NUMBER(INM) IN WHICH IT IS
C     PREVIOUSLY DEFINED AS GIVEN BY THE LOCATION(LO) IN THE P ARRAY(PO)
C
C**********************************************************************
C     PROGRAMMED BY KAY KROUSE  JUNE 1983
C***********************************************************************
C
      DIMENSION PO(*)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_puc/RCS/floc26.f,v $
     . $',                                                             '
     .$Id: floc26.f,v 1.2 1996/01/17 22:03:09 page Exp $
     . $' /
C    ===================================================================
C
      LOC=PO(9)
      NCOMPS=PO(LOC)
      LOC=LOC+1
C
C     NCOMPS IS THE NUMBER OF COMPUTATIONAL SCHEMES/UTILITIES TO SEARCH
C     LP IS THE BEGINNING LOCATION OF THE PARAMTERS IN THE P ARRAY FOR A
C       GIVEN SCHEME
      DO 100 I=1,NCOMPS
      NUM=PO(LOC)
      LP=PO(LOC+1)
      LOC=LOC+4
      IF(LO.GE.LP)GO TO 90
C     'LO' IS IN PREVIOUS SCHEME
      NM=NUMPRV/10
      RNM=NUMPRV/10.
      RNM=(RNM-NM)*10.+.01
      INM=RNM
C     INM IS THE VERSION NUMBER OF THE SCHEME 'NU'
      IF(NM.LT.150)GO TO 50
      NU=NM-150+13
      GO TO 110
 50   NU=NM-100
      GO TO 110
 90   NUMPRV=NUM
 100  CONTINUE
 110  CONTINUE
      RETURN
      END
