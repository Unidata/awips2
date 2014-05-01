C MODULE FPRPC7
C-----------------------------------------------------------------------
C
      SUBROUTINE FPRPC7 (NP,P,NC,C)
C
C.......................................................................
C
C  THIS ROUTINE PRINTS THE P AND C ARRAYS FOR THE LAG/K OPERATION
C.......................................................................
C
C  SUBROUTINE ORIGINALLY PROGRAMMED BY
C            GEORGE F. SMITH - HRL   DECEMBER 1979
C.......................................................................
C
C  VARIABLES IN ARGUMENT LIST:
C
C    1. NP - NUMBER OF VALUES IN THE P ARRAY
C    2. P  - THE P ARRAY
C    3. NC - NUMBER OF VALUES IN THE C ARRAY
C    4. P  - THE C ARRAY
C.......................................................................
C
      DIMENSION P(NP),C(NC)
C
      INCLUDE 'common/fdbug'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_lagk/RCS/fprpc7.f,v $
     . $',                                                             '
     .$Id: fprpc7.f,v 1.2 2000/03/13 20:48:02 page Exp $
     . $' /
C    ===================================================================
C
C
      IF (NP.GT.0) WRITE (IODBUG,900) NP,(P(I),I=1,NP)
  900 FORMAT (' 'I5,' VALUES IN THE P ARRAY:' /
     *   1X,F12.3,2(1X,2A4,1X,A4,1X,F12.3) /
     *   (10(1X,F12.3)))
C   
      IF (NC.EQ.0) WRITE( IODBUG,901) NC,(C(I),I=1,NC)
  901 FORMAT (' ',I5,' VALUES IN THE C ARRAY' /
     *   (10(1X,F12.3)))
C
      RETURN
C
      END
