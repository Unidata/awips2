C MEMBER EGOPLT
C  (FROM OLD MEMBER EEDEX02)
C
C                             LAST UPDATE: 06/07/95.09:10:24 BY $WC30EW
C
      SUBROUTINE EGOPLT(ISAMP,NUMT,IP,IPP,XSS,XLL,IDIST,IDTS,
     X   NVAR,HEAD,VARNAM,IWIND,TDSP,KODE,VALUE,UNITS)
      DIMENSION G1(121),G2(121),G3(121),G4(121),IP(6,51),
     X  VARNAM(1),TDSP(1),HEAD(1),DHD(3),IPP(6,51,3),IDTS(1)
      INCLUDE 'common/ionum'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_esp/RCS/egoplt.f,v $
     . $',                                                             '
     .$Id: egoplt.f,v 1.1 1995/09/17 19:18:44 dws Exp $
     . $' /
C    ===================================================================
C
C
C
C
      DATA G1/1H.,119*1H ,1H./
      DATA G2/9*1H.,1H+,3*1H.,1H+,5*1H.,1H+,7*1H.,1H+,6*1H.,1H+,
     X  8*1H.,1H+,5*1H.,1H+,5*1H.,1H+,4*1H.,1H+,4*1H.,1H+,
     X  5*1H.,1H+,5*1H.,1H+,8*1H.,1H+,6*1H.,1H+,7*1H.,1H+,
     X  5*1H.,1H+,3*1H.,1H+,9*1H./
      DATA G3/1H+,119*1H ,1H+/
      WRITE(IPR,6004)
 6004 FORMAT(1H1)
      CALL EHEAD(NVAR,HEAD,VARNAM,IWIND,TDSP,UNITS,KODE,VALUE,1)
      CALL EDIS(IDIST,DHD)
      WRITE(IPR,900) (DHD(I),I=1,3)
C
C
C
      I=1
      YP=XLL
      DYY=(XLL-XSS)/10.
      DO 10 J=1,121
 10   G4(J)=G2(J)
      IF (IDIST.EQ.1) GO TO 100
      CALL EPG(NUMT,G4,IP,IDTS,I)
      IF (ISAMP.NE.1) GO TO 110
 100  CALL EPPG(NUMT,G4,IPP,IDTS,I)
 110  GP=ETRANI(YP,IDIST)
      WRITE(IPR,6001) GP,G4
      YP=YP-DYY
 11   I=I+1
      IF (I-1-((I-1)/5)*5) 20,20,15
 15   DO 16 J=1,121
 16   G4(J)=G1(J)
      IF (IDIST.EQ.1) GO TO 115
      CALL EPG(NUMT,G4,IP,IDTS,I)
      IF (ISAMP.NE.1) GO TO 120
 115  CALL EPPG(NUMT,G4,IPP,IDTS,I)
 120  WRITE(IPR,6002) G4
      GO TO 11
 20   IF (I-51) 21,30,30
 21   DO 26 J=1,121
 26   G4(J)=G3(J)
      IF (IDIST.EQ.1) GO TO 125
      CALL EPG(NUMT,G4,IP,IDTS,I)
      IF (ISAMP.NE.1) GO TO 130
 125  CALL EPPG(NUMT,G4,IPP,IDTS,I)
 130  GP=ETRANI(YP,IDIST)
      WRITE(IPR,6001) GP,G4
      YP=YP-DYY
      GO TO 11
 30   DO 31 J=1,121
 31   G4(J)=G2(J)
      IF (IDIST.EQ.1) GO TO 135
      CALL EPG(NUMT,G4,IP,IDTS,I)
      IF (ISAMP.NE.1) GO TO 140
 135  CALL EPPG(NUMT,G4,IPP,IDTS,I)
 140  GP=ETRANI(YP,IDIST)
      WRITE(IPR,6001) GP,G4
      WRITE(IPR,6003)
 6000 FORMAT(6X,A4)
 6001 FORMAT(1X,F10.0,1X,121A1)
 6002 FORMAT(12X,121A1)
 6003 FORMAT(18X,'.995',2X,'.99',3X,'.98',5X,'.95',
     X        4X,'.90',6X,'.80',3X,'.70',3X,'.60',2X,'.50',
     X        2X,'.40',3X,'.30',3X,'.20',6X,'.10',4X,'.05',
     X        5X,'.02',3X,'.01',2X,'.005')
      WRITE(IPR,6000) UNITS
      WRITE(IPR,7001)
      CALL ETITLE(ISAMP,NUMT,IDTS)
 900  FORMAT(10X,21HFITTING DISTRIBUTION:,3A4)
 7001 FORMAT(49X,22HEXCEEDANCE PROBABILITY/)
      RETURN
      END
