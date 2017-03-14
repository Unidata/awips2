C MEMBER PIN3
C  (from old member MCDUMMY)
C
      SUBROUTINE PIN3(PO,LEFTP,IUSEP,CO,LEFTC,IUSEC,D,MD)
      COMMON /IONUM/ IN,IPR,IPU
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/calb_stubs/RCS/pin3.f,v $
     . $',                                                             '
     .$Id: pin3.f,v 1.1 1996/05/22 19:46:32 dws Exp $
     . $' /
C    ===================================================================
C
      IUSEP=0
      IUSEC=0
      WRITE(IPR,600)
  600 FORMAT (1H0,10X,69H**ERROR** OPERATION 3 (REDO-UHG) IS NOT INCLUDE
     1D IN THIS LOAD MODULE.)
      CALL ERROR
      RETURN
      END
