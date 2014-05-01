C MEMBER OUTP22
C **********************************************************************
C
      SUBROUTINE OUTP22(X,Y,DERY,IHLF,NDIM,PRMT)
      DIMENSION PRMT(5),Y(91),DERY(91)
C
C...for debugging:
       COMMON /FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_sssac/RCS/outp22.f,v $
     . $',                                                             '
     .$Id: outp22.f,v 1.2 2002/05/15 13:55:18 hank Exp $
     . $' /
C    ===================================================================
C
C
      IF (ITRACE .GT. 1) WRITE (IODBUG,990)
990   FORMAT(/10X,'** OUTP22 ENTERED.')
C
      IF (ITRACE .GT. 1) WRITE (IODBUG,991)
991   FORMAT(/10X,'** EXIT OUTP22.')
C
      RETURN
      END
