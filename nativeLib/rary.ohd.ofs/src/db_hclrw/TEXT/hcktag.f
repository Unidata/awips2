C MEMBER HCKTAG
C  (from old member HCLCK2)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 11/02/95.14:08:16 BY $WC20SV
C
C @PROCESS LVL(77)
C
      SUBROUTINE HCKTAG (ITECHB,IF,IOPTRC,NXOPT,NNC,ISTAT)
C
C          ROUTINE:  HCKTAG
C
C             VERSION:  1.0.0
C
C                DATE:  8-25-81
C
C              AUTHOR:  SONJA R SIEGEL
C                       DATA SCIENCES INC
C
C***********************************************************************
C
C          DESCRIPTION:
C
C    THIS ROUTINE CHECKS ITEM OF THE FORM ARGNAME=VALUE OR JUST
C    VALUE FOR NAMED AND RUN-TIME OPTIONS
C
C    VARIABLES IF AND NXOPT ARE RESET IF VALID ARGUMENTS WERE FOUND
C
C***********************************************************************
C
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C       ITECHB     I     I     ?    TECHNIQUE DEFINITION
C       IF         I    I/O    1    FIELD NUMBER
C       I OPTRC     I    I/O    ?   OPTION ARRAY
C       NXOPT      I    I/O    1    LAST USED WORD IN OPTION ARRAY
C       NNC        I    I/O    1    LAST CARD READ FROM COMMAND FILE
C       ISTAT       I     O     1   STATUS INDICATOR
C                                     0=OKAY
C                                     1=ERROR
C
C
C
C***********************************************************************
C
C          COMMON:
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'ufreei'
      INCLUDE 'udatas'
      INCLUDE 'hclcommon/hwords'
      INCLUDE 'hclcommon/hcomnd'
C
C***********************************************************************
C
C          DIMENSION AND TYPE DECLARATIONS:
C
      DIMENSION ITECHB(*),IOPTRC(*)
      DIMENSION LRAY(2,5),IANS(20)
      DIMENSION LEFT(2),IRIGHT(2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_hclrw/RCS/hcktag.f,v $
     . $',                                                             '
     .$Id: hcktag.f,v 1.2 1995/11/14 19:20:25 erb Exp $
     . $' /
C    ===================================================================
C
C
C***********************************************************************
C
C          DATA:
C
      DATA LRAY/4HINTE,4HGER ,4HREAL,4H    ,4HALPH,4HA   ,
     *          4HLOGI,4HCAL ,4HDATE,4H    /
      DATA MAX/20/
      DATA LAMP/4H&   /
C
C
C***********************************************************************
C
C
      IF (IHCLTR.GT.0) WRITE (LPD,10)
10    FORMAT (' *** ENTER HCKTAG')
C
      NXSAV=NXOPT+1
      NX1=NXOPT+2
      IARGPS=0
C
C  CHECK FOR ARGUMENTS IN THIS TECHNIQUE AND VALUE TO BE SET
20    ICONTF=0
      IF (IF.LT.NFIELD) GO TO 60
      IF (IF.GT.NFIELD) GO TO 40
C
C  LAST FIELD - CHECK FOR &
      N=IFSTOP(NFIELD)-IFSTRT(NFIELD)+1
      IF (N.NE.1.OR.IBUF(IFSTRT(NFIELD)).NE.LAMP) GO TO 60
C
C FOUND & - MAKE SURE ANOTHER CATD
      ICONTF=1
      IF (NNC.LT.NCARD) GO TO 50
         WRITE (LP,30)
30    FORMAT ('0**ERROR** CONTINUATION CARD EXPECTED.')
         ISTAT=1
          GO TO 300
C
C  CHECK IF READ ALL CARDS
40    IF (NNC.EQ.NCARD) GO TO 280
C
50    NNC=NNC+1
      CALL HCARDR (NNC,ISTA)
      IF (ISTA.NE.0) GO TO 290
      IF=1
C
C CHECK IF FIELD IS EMPTY
60    IF (IFSTRT(IF).GT.IFSTOP(IF)) GO TO 90
      CALL HFLPRN (IFSTRT(IF),IFSTOP(IF),IX)
      IF (IX.NE.IFSTRT(IF)) GO TO 80
C
C  CHECK IF TECHNIQUE IS UNIVERSAL
      IF (ITECHB(7).NE.0) THEN
         WRITE (LP,70)
70    FORMAT ('0**ERROR** IDENTIFIERS ARE INVALID FOR A UNIVERSAL ',
     *   'TECHNIQUE.')
         ISTAT=1
         GO TO 270
         ENDIF
C
C  CHECK FOR EQUAL SIGN
80    CALL HFEQLS (IF,LEFT,IRIGHT,ISTRGT)
      IF (ISTRGT.GT.IFSTRT(IF)) GO TO 120
      IF (ISTRGT.EQ.IFSTRT(IF).AND.IF.EQ.1.AND.ICONTF.EQ.0) GO TO 280
C
C  NO EQUAL SIGN - ARGUMENT MUST BE SET BY POSITION
90    IARGPS=IARGPS+1
      IF (IARGPS.LE.ITECHB(10)) GO TO 110
         WRITE (LP,100) ITECHB(10)
100   FORMAT ('0**ERROR** TECHNIQUE HAS ONLY ',I3,' ARGUMENTS.')
         ISTAT=1
         GO TO 280
C
110   K=11+(IARGPS-1)*4
C
C   CHECK IF FIELD IS EMPTY
      IF (IFSTRT(IF).GT.IFSTOP(IF)) GO TO 270
      GO TO 190
C
C MAKE SURE ARGUMENT NAME IS 8 CHARS OR LESS
120   J=IFSTRT(IF)
      N=ISTRGT-J-1
      IF (N.LE.8) GO TO 140
         WRITE (LP,130) IF
130   FORMAT ('0**ERROR** ARGUMENT NAME IN FIELD ',I3,
     *  ' IS GREATER THAN 8 CHARACTERS.')
         ISTAT=1
         GO TO 270
C
C  FIND ARGUMENT IN TECHNIQUE RECORD
140   N=ITECHB(10)
      K=11
      DO 160 I=1,N
         CALL UNAMCP (LEFT,ITECHB(K),IMATCH)
         IF (IHCLDB.GT.0) WRITE (LPD,150) ITECHB(K),ITECHB(K+1)
150      FORMAT (' COMPARING LEFT TO ',2A4)
         IF (IMATCH.EQ.0) GO TO 180
         K=K+4
160      CONTINUE
C
C  ARGUMENT NAME NOT FOUND
      WRITE (LP,170) LEFT,ITECHB(4),ITECHB(5)
170   FORMAT ('0**ERROR** ARGUMENT ',2A4,' NOT PART OF TECHNIQUE ',2A4,
     *   '.')
      ISTAT=1
      GO TO 270
C
180   IARGPS=I
C
C  CHECK ARGUMENT VALUE
190   CALL HCKARG (ISTRGT,IFSTOP(IF),ITYPE,IANS,MAX,NANS,ISTA)
      IF (ISTA.NE.0) ISTAT=ISTA
C
C  CHECK IF TYPE MATCHES
      IF (ITECHB(K+2).EQ.ITYPE) GO TO 260
      IF (ITECHB(K+2).GT.0) GO TO 220
C
C  TYPE SHOULD BE CHARACTER
      NCHAR=IFSTOP(IF)-ISTRGT+1
      IF (ITYPE.EQ.3.AND.NCHAR.LE.-ITECHB(K+2)) GO TO 260
      L=-ITECHB(K+2)
      IF (NCHAR.LE.L) GO TO 210
         WRITE (LP,200) IF,L
200   FORMAT ('0**ERROR** ARGUMENT IN FIELD ',I3,
     *   ' IS GREATER THAN ',I4,' CHARACTERS.')
         ISTAT=1
         GO TO 270
C
210   NANS=NCHAR
      CALL HPTSTR (IBUF(ISTRGT),NANS,IANS,ISTA)
      IF (ISTA.NE.0) ISTAT=ISTA
      GO TO 260
C
C  IF TYPE IS FLOAT AND ARGUMENT WAS INTEGER, CONVERT IT
220   IF (ITECHB(K+2).EQ.2.AND.ITYPE.EQ.1) GO TO 250
      IF (ITECHB(K+2).NE.5.OR.ITYPE.NE.1) GO TO 230
C
C  DATE WAS CALLED AN INTEGER
      CALL HCKDAT (ISTRGT,IFSTOP(IF),IANS,IERR)
      NANS=7
      IF (IERR.EQ.0) GO TO 260
C
230   L=ITECHB(K+2)
      WRITE (LP,240) IF,(LRAY(I,L),I=1,2)
240   FORMAT ('0**ERROR** ARGUMENT IN FIELD ',I2,' MUST BE ',2A4,
     *   ' VALUE.')
      ISTAT=1
      GO TO 270
C
250   R=IANS(1)
      CALL USWITC (R,IANS(1))
C
C  CHECK IF ERROR ENCOUNTERED
260   IF (ISTAT.NE.0) GO TO 270
C
C  STORE ARGUMENT
      IOPTRC(NXSAV)=IOPTRC(NXSAV)+1
      IOPTRC(NX1)=ITECHB(K+3)
      IOPTRC(NX1+1)=NANS
      CALL UMEMOV (IANS,IOPTRC(NX1+2),NANS)
      NX1=NX1+NANS+2
      NXOPT=NXOPT+NANS
C
270   IF=IF+1
      GO TO 20
C
280   NXOPT=NX1-1
      GO TO 300
C
290   ISTAT=ISTA
C
300   IF (IHCLDB.GT.0) WRITE (LPD,310) ITECHB(4),ITECHB(5),IF,ISTAT,
     *   (IOPTRC(J),J=1,NXOPT)
310   FORMAT (' IN HCKTAG - TECH=',2A4,' IF=',I3,' ISTAT=',I3 /
     *  ' IOPTRC=',16(1X,I5) / (8X,16(1X,I5)))
C
      RETURN
C
      END
