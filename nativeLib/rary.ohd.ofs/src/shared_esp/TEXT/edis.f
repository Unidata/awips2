C MEMBER EDIS
C  (FROM OLD MEMBER EEDEX02)
C
C                             LAST UPDATE: 06/07/95.09:10:24 BY $WC30EW
C
      SUBROUTINE EDIS(IDIST,DHD)
      DIMENSION DHD(1)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_esp/RCS/edis.f,v $
     . $',                                                             '
     .$Id: edis.f,v 1.1 1995/09/17 19:18:33 dws Exp $
     . $' /
C    ===================================================================
C
      DATA EMPI/4HEMPI/,RICA/4HRICA/,FL/4HL   /,FLOG/4HLOG-/,
     X   FNOR/4HNORM/,AL/4HAL  /,BLNK/4H    /
      GO TO (10,20,30),IDIST
 10   DHD(1)=EMPI
      DHD(2)=RICA
      DHD(3)=FL
      RETURN
 20   DHD(1)=FLOG
      DHD(2)=FNOR
      DHD(3)=AL
      RETURN
 30   DHD(1)=FNOR
      DHD(2)=AL
       DHD(3)=BLNK
      RETURN
      END
