C MEMBER FFPUIN
C  (from old member FCFFPUIN)
C
      SUBROUTINE FFPUIN(I,N,IBUG)
C.......................................................................
C
C     THIS SUBROUTINE WRITES INTEGER VALUES TO UNIT 'IPU' IN A
C     FORM THAT CAN BE READ BY A FREE FORMAT READ SUBROUTINE (FFRDIN).
C.......................................................................
C
C     SUBROUTINE ORIGINALLY WRITTEN BY
C                GEORGE F. SMITH - HRL   OCTOBER 1979  VERSION 1
C.......................................................................
C
C          VARIABLES IN ARGUMENT LIST
C
C            1. I    - AN INTEGER ARRAY CONTAINING THE VALUES TO BE
C                      WRITTEN
C            2. N    - THE NUMBER OF VALUES TO BE WRITTEN
C            3. IBUG - A SWITCH WHICH DETERMINES IF DEBUG INFORMATION
C                      IS WRITTEN BY THIS SUBROUTINE
C                         = 0, NO DEBUG INFORMATION WRITTEN
C                         = 1, DEBUG INFORMATION WRITTEN
C.......................................................................
C
C      THE VALUE OF 'IPU' IS PASSED THROUGH THE COMMON BLOCK /IONUM/
C.......................................................................
C
      COMMON/FDBUG/IODBUG,IXX(23)
      COMMON/IONUM/IN,IPR,IPU
      DIMENSION I(N)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_puc/RCS/ffpuin.f,v $
     . $',                                                             '
     .$Id: ffpuin.f,v 1.1 1995/09/17 18:50:29 dws Exp $
     . $' /
C    ===================================================================
C
      IF(IBUG.EQ.1)WRITE(IODBUG,900)
  900 FORMAT(1H0,10X,16H**FFPUIN ENTERED)
      DO 50 J=1,N,5
      IF(J+4.EQ.N)GO TO 40
      IF(J+4.LT.N)GO TO 30
      WRITE(IPU,700)(I(K),K=J,N)
      GO TO 50
   30 L=J+4
      WRITE(IPU,700)(I(K),K=J,L)
  700 FORMAT(5(1X,I11),2H X)
      GO TO 50
   40 WRITE(IPU,701)(I(K),K=J,N)
  701 FORMAT(5(1X,I11))
   50 CONTINUE
      RETURN
      END
