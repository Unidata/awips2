C MODULE PROS7
C-----------------------------------------------------------------------
C
      SUBROUTINE PROS7(TAB,NT,IP)
C
C.......................................................................
C
C      THIS SUBROUTINE PRINTS THE 2*S/DT+O VS O TABLE
C.......................................................................
C
C      SUBROUTINE ORIGINALLY PROGRAMMED BY
C             GEORGE F. SMITH - HRL  DECEMBER 1979
C.......................................................................
C
C      VARIABLES IN ARGUMENT LIST
C
C        1. TAB - 2*S/DT+O VS O TABLE
C        2. NT  - NUMBER OF (2*S/DT+O,O) PAIRS IN TABLE
C        3. IP  - OUTPUT UNIT NUMBER ON WHICH TABLE WILL BE WRITTEN
C.......................................................................
C
      DIMENSION TAB(2,NT)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_pntb/RCS/pros7.f,v $
     . $',                                                             '
     .$Id: pros7.f,v 1.2 2000/03/13 20:48:51 page Exp $
     . $' /
C    ===================================================================
C
C
      DO 10 L=1,NT,10
         LEND=L+9
         IF (LEND.GT.NT) LEND=NT
         WRITE (IP,600) (TAB(1,K),K=L,LEND)
  600 FORMAT (' O          (CMS)=',10(1X,G9.4))
         WRITE(IP,601)(TAB(2,K),K=L,LEND)
  601 FORMAT (' 2*S/DT + O (CMS)=',10(1X,G9.4))
   10    CONTINUE
C
      RETURN
C
      END
