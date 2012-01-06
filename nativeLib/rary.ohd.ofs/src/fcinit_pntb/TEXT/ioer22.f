C MEMBER IOER22
C
       SUBROUTINE IOER22 (SNAME,IOS,IOSTMT,IOERR)
C
C...FOR DEBUGGING:
       COMMON /FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
C
       REAL SNAME(2)
       INTEGER IOS, IOSTMT, IOERR
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_pntb/RCS/ioer22.f,v $
     . $',                                                             '
     .$Id: ioer22.f,v 1.3 2002/05/15 13:46:22 hank Exp $
     . $' /
C    ===================================================================
C
C
       IF (ITRACE .GT. 1) WRITE (IODBUG,990)
990    FORMAT(/10X,'** IOER22 ENTERED.')
C
950    WRITE (IPR,95) (SNAME(I),I=1,2),IOS,IOSTMT
95     FORMAT (1H0,10X,'**ERROR** ',2A4,' INPUT ERROR',I4,
     +      'ENCOUNTERED IN IOSTMT',I4,' SEE LISTING.')
       CALL ERROR
       IOERR = 1
C
       IF (ITRACE .GT. 1) WRITE (IODBUG,991)
991    FORMAT(/10X,'** EXIT IOER22.')
C
       RETURN
       END
