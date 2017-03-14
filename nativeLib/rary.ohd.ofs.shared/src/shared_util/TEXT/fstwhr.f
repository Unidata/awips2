      SUBROUTINE FSTWHR(NAMIN,NUMIN,NAMOUT,NUMOUT)
C
C     THIS SUBROUTINE RESETS THE VALUES OF IOPNUM AND IOPNAM
C     IN THE /WHERE/ COMMON BLOCK AND RETURNS THE CURRENT
C     VALUES TO THE CALLING SUBROUTINE.
C
      COMMON /WHERE/ ISEG(2),IOPNUM,IOPNAM(2)
C
      DIMENSION NAMIN(2),NAMOUT(2),NAMTMP(2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_util/RCS/fstwhr.f,v $
     . $',                                                             '
     .$Id: fstwhr.f,v 1.1 1995/09/17 19:23:59 dws Exp $
     . $' /
C    ===================================================================
C
C
      NAMTMP(1)=IOPNAM(1)
      NAMTMP(2)=IOPNAM(2)
      NUMTMP=IOPNUM
C
      IOPNAM(1)=NAMIN(1)
      IOPNAM(2)=NAMIN(2)
      IOPNUM=NUMIN
C
      NAMOUT(1)=NAMTMP(1)
      NAMOUT(2)=NAMTMP(2)
      NUMOUT=NUMTMP
C
      RETURN
      END
