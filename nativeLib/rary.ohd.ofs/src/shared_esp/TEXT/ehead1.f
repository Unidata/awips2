C MEMBER EHEAD1
C  (FROM OLD MEMBER EEDEX02)
C
C                             LAST UPDATE: 06/07/95.09:10:24 BY $WC30EW
C
      SUBROUTINE EHEAD1(IT,XX,Y)
C
C          THIS SUBROUTINE STORES HEADERS FOR
C          TYPE OF TIME SERIES.
C
      DIMENSION XX(18),Y(18)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_esp/RCS/ehead1.f,v $
     . $',                                                             '
     .$Id: ehead1.f,v 1.1 1995/09/17 19:18:46 dws Exp $
     . $' /
C    ===================================================================
C
      DATA HIS/4H HIS/,TORI/4HTORI/,CAL/4HCAL /,AD/4H  AD/,
     X  TJUS/4HJUST/,ED/4HED  /,COND/4HCOND/,TIO/4HITIO/,
     X  TNAL/4HNAL /,BLNK/4H    /,OB/4H  OB/,SERV/4HSERV/,
     X  BASE/4HBASE/,PER/4H PER/,TIOD/4HIOD /,SIM/4H SIM/,
     X  ULAT/4HULAT/,TION/4HION /
C
C
C
      Y(1)=SIM
      Y(2)=ULAT
      Y(3)=TION
      IF (IT.GE.2) GO TO 107
      XX(1)=HIS
      XX(2)=TORI
      XX(3)=CAL
      GO TO 900
 107  IF (IT.GE.3) GO TO 117
      XX(1)=AD
      XX(2)=TJUS
      XX(3)=ED
      GO TO 900
 117  IF (IT.GE.4) GO TO 127
      XX(1)=COND
      XX(2)=TIO
      XX(3)=TNAL
      GO TO 900
 127  Y(1)=BLNK
      Y(2)=BLNK
      Y(3)=BLNK
      IF (IT.GE.5) GO TO 137
      XX(1)=OB
      XX(2)=SERV
      XX(3)=ED
      GO TO 900
 137  XX(1)=BASE
      XX(2)=PER
      XX(3)=TIOD
 900  RETURN
      END
