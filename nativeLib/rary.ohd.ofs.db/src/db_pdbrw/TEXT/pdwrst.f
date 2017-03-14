C MEMBER PDWRST
C  (from old member PDWPD1S)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 07/11/95.07:36:37 BY $WC20SV
C
C @PROCESS LVL(77)
C
      SUBROUTINE PDWRST (IWRITE,NTYPES,ISTAT)
C
C  THIS ROUTINE SETS THE STATUS FROM THE IWRITE ARRAY
C  STATUS=2 THRU 4 FOR 1 WRITE SET, 5=2+3 6=2+4 7=3+4 8=2+3+4
C
      PARAMETER (LSTRAY=6)
C
      INCLUDE 'udebug'
      DIMENSION IWRITE(1),ISTRAY(LSTRAY)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_pdbrw/RCS/pdwrst.f,v $
     . $',                                                             '
     .$Id: pdwrst.f,v 1.1 1995/09/17 18:44:24 dws Exp $
     . $' /
C    ===================================================================
C
C
      IF (IPDTR.GT.0) WRITE (IOGDB,10)
10    FORMAT (' *** ENTER PDWRST')
C
      ISTAT=0
      CALL UMEMST (0,ISTRAY,LSTRAY)
C
C  LOOP ON NUMBER OF TYPES
      DO 20 I=1,NTYPES
         IF (IWRITE(I).EQ.0) GO TO 20
         ISTRAY(IWRITE(I))=IWRITE(I)+1
20       CONTINUE
C
      IF (IPDDB.GT.0) WRITE (IOGDB,30) (ISTRAY(I),I=1,LSTRAY)
30    FORMAT (' ISTRAY=',6I4)
C
C  NOW SET SUMS
      DO 40 I=1,3
         ISTAT=ISTAT+ISTRAY(I)
40       CONTINUE
      IF (ISTAT.EQ.9) ISTAT=8
      IF (ISTRAY(4).NE.0) ISTAT=ISTAT+20
      IF (ISTRAY(5).NE.0) ISTAT=ISTAT+30
      IF (ISTRAY(6).NE.0) ISTAT=ISTAT+100
C
      IF (IPDTR.GT.0) WRITE (IOGDB,50)
50    FORMAT (' *** EXIT PDWRST')
C
      RETURN
C
      END
