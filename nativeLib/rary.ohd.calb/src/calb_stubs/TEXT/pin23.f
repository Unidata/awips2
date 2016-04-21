C MEMBER PIN23
C  (from old member MCDUMMY)
C
      SUBROUTINE PIN23(PO,LEFTP,IUSEP,CO,LEFTC,IUSEC)
      INCLUDE 'common/ionum'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/calb_stubs/RCS/pin23.f,v $
     . $',                                                             '
     .$Id: pin23.f,v 1.1 1996/05/22 19:46:32 dws Exp $
     . $' /
C    ===================================================================
C
      IUSEP=0
      IUSEC=0
      WRITE(IPR,600)
  600 FORMAT (1H0,10X,65H**ERROR** STAGE-Q OPERATION IS NOT INCLUDED IN 
     1 THIS LOAD MODULE.)
      CALL ERROR
      RETURN
      END
