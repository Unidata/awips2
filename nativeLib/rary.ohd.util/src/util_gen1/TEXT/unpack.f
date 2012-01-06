C MEMBER UNPACK
C (from old member UTIL3)
C----------------------------------------------------------------------
C
       SUBROUTINE UNPACK (IN,IOUT,ICOUNT)
C
C          SUBROUTINE:  UNPACK
C
C             VERSION:  1.0.0
C
C              AUTHOR:  JIM ERLANDSON
C                       DATA SCIENCES INC
C                       8555 16TH ST, SILVER SPRING, MD 587-3700
C***********************************************************************
C
C          DESCRIPTION:
C
C    ROUTINE UNPACKS FOUR I*1 INTEGER VALUES INTO FOUR I*4 INTEGER
C    WORDS.  THE FIRST THREE BYTES OF THE I*4 WORDS ARE ZERO FILLED.
C
C***********************************************************************
C
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C       IN         L*1   I     4    INPUT ARRAY
C       IOUT       L*1   O     4    OUTPUT ARRAY
C       ICOUNT      I    I     1    NUMBER OF I*1 VALUES TO UNPACK
C
C***********************************************************************
C
C          COMMON:
C
      INCLUDE 'udsi'
      INCLUDE 'uio'
C
C***********************************************************************
C
C          DIMENSION AND TYPE DECLARATIONS:
C
      LOGICAL*1 IN(4),
     *          IOUT(4),
     *          Z1(4)
C
C***********************************************************************
C
C          DATA:
C
      EQUIVALENCE (Z1(1),Z4)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_gen1/RCS/unpack.f,v $
     . $',                                                             '
     .$Id: unpack.f,v 1.1 1995/09/17 19:01:43 dws Exp $
     . $' /
C    ===================================================================
C
C
      DATA Z4/0/
C
C***********************************************************************
C
C
      J=4
C
      DO 10 I =1,ICOUNT
         IOUT(J-3)=Z1(1)
         IOUT(J-2)=Z1(1)
         IOUT(J-1)=Z1(1)
         IOUT(J)=IN(I)
         J=J+4
10       CONTINUE
C
      IF (NOBUG.GE.1) WRITE (LPD,20)
20    FORMAT (' *** EXIT UNPACK')
C
      RETURN
C
      END
