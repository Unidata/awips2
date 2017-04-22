C MEMBER CKSR22
C
C***********************************************************************
C
       SUBROUTINE CKSR22 (IFLAG,ISTART,LEN,VALUE,LEFT,A)
C
C...FOR DEBUGGING:
       COMMON /FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
C
       REAL VALUE(1), A(1)
       INTEGER IFLAG, ISTART, LEN, LEFT
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_pntb/RCS/cksr22.f,v $
     . $',                                                             '
     .$Id: cksr22.f,v 1.2 2002/05/15 13:46:15 hank Exp $
     . $' /
C    ===================================================================
C
C
       IF (ITRACE .GT. 1) WRITE (IODBUG,990)
990    FORMAT(/10X,'** CKSR22 ENTERED.')
C
       IF (IFLAG .NE. 1) GO TO 800
           IF (ITRACE .GT. 1) WRITE (IODBUG,991)
991        FORMAT(/10X,'** EXIT CKSR22.')
           RETURN
C
800    CONTINUE
       IF (ISTART+LEN-1 .LE. LEFT) GOTO 802
           IFLAG = 1
           GOTO 803
C
802    CONTINUE
       DO 100 I = 1,LEN
           A(ISTART+I-1) = VALUE(I)
100    CONTINUE
C
803    CONTINUE
C
       IF (ITRACE .GT. 1) WRITE (IODBUG,991)
C
       RETURN
       END
