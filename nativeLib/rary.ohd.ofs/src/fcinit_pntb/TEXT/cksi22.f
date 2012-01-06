C MEMBER CKSI22
C
       SUBROUTINE CKSI22 (IFLAG,ISTART,LEN,IVALUE,LEFT,A)
C
C...FOR DEBUGGING:
       COMMON /FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
C
       REAL A(1)
       INTEGER IVALUE(1)
       INTEGER IFLAG, ISTART, LEN, LEFT
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_pntb/RCS/cksi22.f,v $
     . $',                                                             '
     .$Id: cksi22.f,v 1.2 2002/05/15 13:45:31 hank Exp $
     . $' /
C    ===================================================================
C
C
       IF (ITRACE .GT. 1) WRITE (IODBUG,990)
990    FORMAT(/10X,'** CKSI22 ENTERED.')
C
       IF (IFLAG .NE. 1) GOTO 800
           IF (ITRACE .GT. 1) WRITE (IODBUG,991)
991        FORMAT(/10X,'** EXIT CKSI22.')
           RETURN
C
800    CONTINUE
       IF (ISTART+LEN-1 .LE. LEFT) GOTO 802
           IFLAG = 1
           GOTO 803
C
802    CONTINUE
       DO 100 I = 1,LEN
           A(ISTART+I-1) = IVALUE(I) + 0.01
100    CONTINUE
C
803    CONTINUE
C
       IF (ITRACE .GT. 1) WRITE (IODBUG,991)
C
       RETURN
       END
