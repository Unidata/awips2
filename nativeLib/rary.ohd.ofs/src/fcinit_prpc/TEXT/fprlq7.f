C MEMBER FPRLQ7
C  (from old member FCPRP7)
C
      SUBROUTINE FPRLQ7(TAB,NT,IP,IMETR)
C
C.......................................................................
C
C      THIS SUBROUTINE PRINTS AN L VS Q TABLE.
C.......................................................................
C
C      SUBROUTINE ORIGINALLY PROGRAMMED BY
C                  GEORGE F. SMITH - HRL   DECEMBER 1979
C                      UPDATED MARCH 1982 TO PRINT IN METRIC OR
C                         ENGLISH UNITS
C.......................................................................
C
C      VARIABLES IN ARGUMENT LIST
C
C        1. TAB - L VS Q TABLE
C        2. NT  - THE NUMBER OF (L,Q) PAIRS IN TAB
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
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_prpc/RCS/fprlq7.f,v $
     . $',                                                             '
     .$Id: fprlq7.f,v 1.1 1995/09/17 18:49:33 dws Exp $
     . $' /
C    ===================================================================
C
C
      IF(IMETR.EQ.0)CALL FCONVT(4HCMS ,4HL3/T,IUENG,CFSM,CFSA,IER)
C
      DO 10 L=1,NT,10
      LEND=L+9
      IF(LEND.GT.NT)LEND=NT
C
      WRITE(IP,600)(TAB(1,K),K=L,LEND)
  600 FORMAT(16X,11HLAG (HOURS),10F10.0)
C
      IF(IMETR.EQ.0)GO TO 2
C
      WRITE(IP,601)(TAB(2,K),K=L,LEND)
  601 FORMAT(16X,11HQ   (CMS)  ,10F10.1)
      GO TO 10
C
    2 JEND=LEND-L+1
      DO 5 I=1,JEND
    5 TEMP(I)=TAB(2,L+I-1)*CFSM
C
      WRITE(IP,602)(TEMP(I),I=1,JEND)
  602 FORMAT(16X,11HQ   (CFS)  ,10F10.0)
C
   10 CONTINUE
C
      RETURN
      END
