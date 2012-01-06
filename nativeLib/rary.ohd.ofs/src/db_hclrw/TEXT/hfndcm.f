C MODULE HFNDCM
C-----------------------------------------------------------------------
C
      SUBROUTINE HFNDCM (IFIELD,ICONTF)
C
C  THIS ROUTINE CHECKS STARTING AT THE SPECIFIED FIELD IN ARRAY IBUF 
C  FOR THE CONTINUATION INDICATOR (&).
C
C  ARGUMENT LIST:
C
C      NAME    TYPE  I/O   DIM   DESCRIPTION
C      ------  ----  ---   ---   -----------
C      IFIELD   I*4   I     1    STARTING FIELD NUMBER
C      ICONTF   I*4   0     1    INDICATOR IF CONTINUATION FOUND:
C                                  0=NO
C                                 >0=FIELD NUMBER IN WHICH CONTINUATION
C                                    INDICATOR FOUND
C
      INCLUDE 'uiox'
      INCLUDE 'udebug'
      INCLUDE 'ufreei'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_hclrw/RCS/hfndcm.f,v $
     . $',                                                             '
     .$Id: hfndcm.f,v 1.2 2001/06/13 13:42:28 dws Exp $
     . $' /
C    ===================================================================
C
      DATA LAND/4H&   /
C
C
      ICONTF=0
C
      IF (NFIELD.LT.IFIELD) GO TO 30
C
      DO 10 I=IFIELD,NFIELD
         N=IFSTOP(I)-IFSTRT(I)+1
         IF (N.GT.1) GO TO 10
         IF (IBUF(IFSTRT(I)).EQ.LAND) THEN
            ICONTF=I
            IF (IHCLDB.GT.1) WRITE (LPD,40) LAND,ICONTF
40    FORMAT (' IN HFNDCM - ',A4,' FOUND IN FIELD ',I3)
            GO TO 30
            ENDIF
10       CONTINUE
C
30    RETURN
C
      END
