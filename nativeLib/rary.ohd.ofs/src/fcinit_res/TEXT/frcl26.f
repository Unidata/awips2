C MODULE FRCL26
C-----------------------------------------------------------------------
C
      SUBROUTINE FRCL26 (IN,OUT,LORCL,NUM,IBUG)
C
C  SUBROUTINE COPIES CHARACTER BY CHARACTER THE INFORMATION IN
C  ARRAY 'IN' INTO ARRAY 'OUT' STARTING AT POSITION LORCL IN OUT.
C
      CHARACTER*1 IN,OUT
      DIMENSION IN(1),OUT(1)
C
      INCLUDE 'common/fdbug'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_res/RCS/frcl26.f,v $
     . $',                                                             '
     .$Id: frcl26.f,v 1.2 2001/06/13 10:10:26 mgm Exp $
     . $' /
C    ===================================================================
C
C
      IF (IBUG.GT.0) WRITE (IODBUG,5) LORCL,NUM,(IN(I),I=1,NUM)
5     FORMAT (' LORCL=',I3,' NUM=',I3,' IN=',20A1)
C
      DO 10 I=1,NUM
         K=LORCL+I-1
         OUT(K)=IN(I)
10       CONTINUE
C
      LORCL=LORCL+NUM
C
      RETURN
C
      END
