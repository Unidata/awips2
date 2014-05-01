C MEMBER UGETPC
C (from old member UTIL6)
C-----------------------------------------------------------------------
C
      SUBROUTINE UGETPC (NFLD,ITYPE,IWORD,INUM,RNUM,ISTCOD)
C
C          SUBROUTINE:  UGETPC
C
C             VERSION:  1.0.0
C
C                DATE:  06/20/82
C
C              AUTHOR: JONATHAN D. GERSHAN
C                       DATA SCIENCES INC
C                       8555 16TH ST, SILVER SPRING, MD 587-3700
C***********************************************************************
C
C          DESCRIPTION:
C
C      THIS ROUTINE EXTRACTS A DATA ITEM FROM THE CARD BUFFER
C      GETTING EITHER AN ALPHA, REAL OR INTEGER VALUE DEPENDING
C      ON THE TYPE SPECIFIED IN THE ARGUMENT LIST. FOR ALPHA
C      FIELDS, HFEQLS IS USED IN CASE THERE IS AN IMBEDDED =
C      SIGN. FOR NUMERIC FIELDS, UFIXED IS CALLED.
C
C
C***********************************************************************
C
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C         NFLD      I    I      1      FIELD IN IBUF TO BE EXTRACTED
C         ITYPE     I    I      1       TTYPE OF FIELD, 1 = ALPHA, 2 =
C                                      INTEGER,3 = REAL
C
C          IWORD     I    O      4      ALPHA STRING UP TO 16 CHARS.
C
C
C
C
C
C***********************************************************************
C
C          COMMON:
C
      INCLUDE 'uio'
      INCLUDE 'udatas'
      INCLUDE 'udsi'
      INCLUDE 'ufreei'
C
C***********************************************************************
C
C          DIMENSION AND TYPE DECLARATIONS:
C
       DIMENSION IWORD(4)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/reorder/RCS/ugetpc.f,v $
     . $',                                                             '
     .$Id: ugetpc.f,v 1.1 1995/09/17 19:17:01 dws Exp $
     . $' /
C    ===================================================================
C
C
C
C***********************************************************************
C
C          DATA:
C
C
C
C***********************************************************************
C
C
C
C ********** INITIALIZE STATUS CODE AND STRING DELIMITER VALUES:
C
       IF (NOBUG.EQ.1) WRITE(LPD,2002) NFLD,ITYPE
2002   FORMAT (' *** ENTER UGETPC - ',
     1   'FIELD# ',I4,'TYPE=',I4)
       NUMWDS=0
       ISTCOD=0
       ISTART=IFSTRT(NFLD)
       ISTOP =IFSTOP(NFLD)
       LENGTH=ISTOP-ISTART+1
C
C ********** FOR ALPHA TYPE FIELD, SET NUMWDS AND CHECK LENGTH:
C
       IF (ITYPE.EQ.1) NUMWDS=((LENGTH-1)/4)+1
C
       IF (NUMWDS.LE. 4) GO TO 50
C
          ISTCOD=1
          GO TO 400
C
50       GO TO (100,200,300),ITYPE
C
C **********  FOR ALPHA DATA :
C
100      CALL HFEQLS( NFLD,IWORD(3),IWORD(1),IPOS)
         CALL UMEMST( IBLNK,IWORD(3),2)
         GO TO 400
C
C ********** FOR INTEGER DATA :
C
200      CALL UFIXED( IBUF,INUM,ISTART,ISTOP,1,0 ,IERR)
         GO TO 400
C
C ********** FOR REAL DATA :
C
300     CALL UFIXED( IBUF,RNUM,ISTART,ISTOP,2,0,IERR)
C
C
400    IF (NOBUG.EQ.1) WRITE(LPD,2003)
2003   FORMAT (' *** EXIT UGETPC')
       RETURN
       END
