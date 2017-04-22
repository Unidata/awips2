C MEMBER PIN37
C  (from old member MCDUMMY)
C
      SUBROUTINE PIN37(PO,LEFTP,IUSEP,P,MP)
      INCLUDE 'common/ionum'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/calb_stubs/RCS/pin37.f,v $
     . $',                                                             '
     .$Id: pin37.f,v 1.1 1996/05/22 19:46:32 dws Exp $
     . $' /
C    ===================================================================
C
      IUSEP=0
      WRITE(IPR,600)
  600 FORMAT (1H0,10X,65H**ERROR** LIST-MSP OPERATION IS NOT INCLUDED IN
     1 THIS LOAD MODULE.)
      CALL ERROR
      RETURN
      END
