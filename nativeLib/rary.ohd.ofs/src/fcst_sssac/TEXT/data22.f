C MEMBER DATA22
C
      SUBROUTINE DATA22
C
C...for debugging:
       COMMON /FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_sssac/RCS/data22.f,v $
     . $',                                                             '
     .$Id: data22.f,v 1.1 2002/05/15 13:51:04 hank Exp $
     . $' /
C    ===================================================================
C
C
      IF (ITRACE .GT. 1) WRITE (IODBUG,990)
990   FORMAT(/10X,'** DATA22 ENTERED.')
C
      IF (ITRACE .GT. 1) WRITE (IODBUG,991)
991   FORMAT(/10X,'** EXIT DATA22.')
C
      RETURN
      END
