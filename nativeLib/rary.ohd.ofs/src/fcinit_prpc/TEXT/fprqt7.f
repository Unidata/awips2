C MEMBER FPRQT7
C  (from old member FCPRC7)
C
      SUBROUTINE FPRQT7(TAB,NT,IP,IMETR)
C
C.......................................................................
C
C      THIS SUBROUTINE PRINTS A Q VS TIME TABLE
C.......................................................................
C
C      SUBROUTINE ORIGINALLY PROGRAMMED BY GEORGE F. SMITH - HRL 12/1979
C                     UPDATED TO PRINT ENGLISH OR METRIC UNITS
C                       MARCH 1982
C.......................................................................
C
C      VARIABLES IN ARGUMENT LIST
C
C        1. TAB - Q VS TIME TABLE
C        2. NT  - THE NUMBER OF (Q,TIME) PAIRS IN TAB
C        3. IP  - THE UNIT NUMBER ON WHICH THE TABLE WILL BE WRITTEN
C        4. IMETR - =0, PRINT Q IN CFS
C                   =1, PRINT Q IN CMS
C.......................................................................
C
      DIMENSION TAB(2,NT),TEMP(10)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_prpc/RCS/fprqt7.f,v $
     . $',                                                             '
     .$Id: fprqt7.f,v 1.1 1995/09/17 18:49:33 dws Exp $
     . $' /
C    ===================================================================
C
C
      DO 10 L=1,NT,10
      LEND=L+9
      IF(LEND.GT.NT)LEND=NT
C
      IF(IMETR.EQ.0)GO TO 5
C
      WRITE(IP,600)(TAB(1,K),K=L,LEND)
  600 FORMAT(16X,12HQ    (CMS)  ,10(1X,G9.4))
      GO TO 9
C
    5 CALL FCONVT(4HCMS ,4HL3/T,IU,CFSM,CFSA,IER)
      IEND=LEND-L+1
      DO 7 I=1,IEND
    7 TEMP(I)=TAB(1,L+I-1)*CFSM
C
      WRITE(IP,602)(TEMP(I),I=1,IEND)
  602 FORMAT(16X,12HQ    (CFS)  ,10(1X,G9.4))
C
    9 WRITE(IP,601)(TAB(2,K),K=L,LEND)
  601 FORMAT(16X,12HTIME (HOURS),10(1X,G9.4))
C
   10 CONTINUE
C
      RETURN
      END
