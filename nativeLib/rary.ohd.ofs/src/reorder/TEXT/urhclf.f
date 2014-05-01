C MEMBER URHCLF
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 01/11/94.14:58:29 BY $WC20SV
C
       SUBROUTINE URHCLF (NAMUSR,ISTAT)
C
C          SUBROUTINE:  URHCLF
C
C             VERSION:  1.0.0
C
C                DATE:  9-29-83
C
C              AUTHOR:  SONJA R SIEGEL
C                       DATA SCIENCES INC
C                       8555 16TH ST, SILVER SPRING, MD 587-3700
C***********************************************************************
C
C          DESCRIPTION:
C
C   ROUTINE TO DYNAMICALLY ALLOCATE THE HCL LOCAL
C    DEFAULT FILE FOR THE REORDER PROGRAM.
C
C***********************************************************************
C
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C       NAMUSR     A8    I     2    RFC NAME IN 8 CHARS OR LESS
C       ISTAT      I     O     1    STATUS 0=OK, NOT 0=ERR
C
C***********************************************************************
C
C          COMMON:
C
      INCLUDE 'uio'
      INCLUDE 'udsi'
      INCLUDE 'udatas'
      INCLUDE 'hclcommon/hunits'
      INCLUDE 'urcommon/urunts'
C
C***********************************************************************
C
C          DIMENSION AND TYPE DECLARATIONS:
C
      DIMENSION IDDNAM(2,2),IDSNAM(11,2),NAMESF(3),NAMUSR(4),PREFIX(3)
      DIMENSION IBUF(16)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/reorder/RCS/urhclf.f,v $
     . $',                                                             '
     .$Id: urhclf.f,v 1.1 1995/09/17 19:17:40 dws Exp $
     . $' /
C    ===================================================================
C
C
C***********************************************************************
C
C          DATA:
C
      DATA PREFIX/4HHYD.,4HRFS5,4H.   /
      DATA IDDNAM/4HFT??,4HF001,4HFT??,4HF001/
      DATA NAMESF/4H.HCL,4HLDFL,4HT   /
C
C***********************************************************************
C
C
C  SET UNIT NUMBER IN DD STATEMENT
      CALL UFXDDN (IDDNAM(1,1),KLDFGD,IERR)
      CALL UFXDDN (IDDNAM(2,1),KUVOL,IERR2)
      IF (IERR.EQ.0.AND.IERR2.EQ.0) GO TO 10
         ISTAT=1
         GO TO 110
C
10    CALL UMEMST (IBLNK,IDSNAM,22)
C
C  CREATE THE DATA SET NAMES
C  NAMUSR CONTAINS A PAIR OF USER NAMES FOR THE OLD AND NEW DEFAULT
C  FILES TO BE ALLOCATED.
C
      J=1
      K=8
      IF (NOBUG.GT.0) WRITE (LPD,20) NAMUSR
20    FORMAT (' NAMUSR=',2(2A4))
      CALL UNPAKS(NAMUSR,IBUF,4,16,ISTAT)
      DO 70 I=1,2
      DO 30 N=J,K
         IF (IBUF(N).EQ.IBLNK) GO TO 40
30    CONTINUE
      N=N-J
      GO TO 50
40    CONTINUE
      N=N-1
      IF (I.EQ.2) N=N-J+1
50    CONTINUE
C
C NOW MOVE NAMESF INTO IDSNAM
C
      M=N+2
      CALL UMEMOV (PREFIX,IDSNAM(1,I),3)
      CALL UMOVEX (NAMUSR,J,IDSNAM(3,I),2,N)
      CALL UMOVEX (NAMESF(1),1,IDSNAM(3,I),M,9)
      IF (NOBUG.GT.0) WRITE (LPD,60) I,(IDSNAM(J,I),J=1,11)
60    FORMAT (' FILE(',I1,')=',11A4)
      J=J+8
      K=K+8
70    CONTINUE
C
C NOW CALL ROUTINE TO ALLOCATE FILES
C
      IOK=0
      DO 100 I=1,2
         CALL W3AK18 (IDDNAM(1,I),IDSNAM(1,I),1,ISTAT)
         IF (NOBUG.GT.0) WRITE (LPD,80) ISTAT
80       FORMAT (' BACK FROM W3AK18 ISTAT=',I8)
         IF (ISTAT.EQ.1) GO TO 100
         IOK=1
         WRITE (LPE,90) (IDSNAM(J,I),J=1,6),ISTAT
90       FORMAT (' **ERROR TRYING TO ASSIGN HCL LOCAL DEFAULT FILE ',
     *      6A4,I8)
100   CONTINUE
C
      IF (IOK.EQ.0) ISTAT=0
110   CONTINUE
C
      RETURN
C
      END
