C MEMBER PIN21
C  (from old member MCDUMMY)
C
      SUBROUTINE PIN21(PO,IPO,LEFTP,IUSEP,CO,LEFTC,IUSEC,D,MD)
      INCLUDE 'common/ionum'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/calb_stubs/RCS/pin21.f,v $
     . $',                                                             '
     .$Id: pin21.f,v 1.1 1996/05/22 19:46:32 dws Exp $
     . $' /
C    ===================================================================
C
      IUSEP=0
      IUSEC=0
      WRITE(IPR,600)
  600 FORMAT (1H0,10X,65H**ERROR** DWOPER OPERATION IS NOT INCLUDED IN  
     1 THIS LOAD MODULE.)
      CALL ERROR
      RETURN
      END
