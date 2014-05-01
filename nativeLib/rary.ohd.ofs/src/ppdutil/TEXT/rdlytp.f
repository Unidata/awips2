C MEMBER RDLYTP
C  (from old member PDRDLYTP)
C-----------------------------------------------------------------------
C
       SUBROUTINE RDLYTP (ITPARR,NTYP,LASTCD,ISTAT)
C
C          ROUTINE:  RDLYTP
C
C             VERSION:  1.0.0
C
C                DATE:  7-27-83
C
C              AUTHOR:  JANINE FRANZOI
C                       DATA SCIENCES INC
C
C***********************************************************************
C
C          DESCRIPTION:
C
C     THIS ROUTINE PARSES AN INPUT CARD FOR A STRING OF DAILY DATA
C     TYPES DEFINED ON THE PPDB AND RETURNS.
C
C***********************************************************************
C
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C       ITPARR     A     O   NTYP    ARRAY CONTAINING TYPES
C       NTYP       I     O     1     # OF TYPES READ FROM CARD
C       LASTCD     I     O     1     READ FLAG
C                                     0=LAST CARD READ
C                                     1=CALL ROUTINE AGAIN
C       ISTAT     I     O     1      STATUS INDICATOR
C                                     0=NORMAL RETURN
C                                     OTHER=READ ERROR
C***********************************************************************
C
C          COMMON:
C
      INCLUDE 'uio'
      INCLUDE 'ufreei'
      INCLUDE 'udebug'
      INCLUDE 'udatas'
C
C***********************************************************************
C
C          DIMENSION AND TYPE DECLARATIONS:
C
      DIMENSION ITPARR(1)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppdutil/RCS/rdlytp.f,v $
     . $',                                                             '
     .$Id: rdlytp.f,v 1.1 1995/09/17 19:09:48 dws Exp $
     . $' /
C    ===================================================================
C
C
C***********************************************************************
C
C          DATA:
C
      DATA ICONT/4H&   /
C
C***********************************************************************
C
C
      IF (IPDTR.GT.0) WRITE (IOGDB,100)
  100 FORMAT (' *** ENTER RDLYTP')
C
      LASTCD=0
C
C  BLANK TYPE ARRAY
      CALL UMEMST (IBLNK,ITPARR,10)
C
C  READ TYPES CARDS
      CALL RPCARD (IBUF,ISTAT)
      IF (ISTAT.NE.0) GO TO 950
      CALL UFREE (1,72)
      IF (ISTAT.NE.0) GO TO 950
C
C  READ TYPES FROM CARD
      DO 300 I=1,NFIELD
         NUM=IFSTOP(I)-IFSTRT(I)+1
         IF (NUM.GT.4) NUM=4
         CALL UPACK1 (IBUF(IFSTRT(I)),ITPARR(I),NUM)
         IF (ITPARR(I).EQ.ICONT) GO TO 350
  300    CONTINUE
      GO TO 900
C
C  RETURN AND CALL ROUTINE AGAIN
350   LASTCD=1
C
900   NTYP=NFIELD
      GO TO 999
C
C  SYSTEM ERROR
950   WRITE (LPE,995)
  995 FORMAT (' ***ERROR*** IN RDLYTP. READ ERROR.')
C
999   IF (IPDTR.GT.0) WRITE (IOGDB,925)
  925 FORMAT (' *** EXIT RDLYTP')
C
      RETURN
C
      END
