C MODULE UPACKN
C-----------------------------------------------------------------------
C
C  ROUTINE UPACKN PACKS A1 CHARACTERS INTO A4 CHARACTERS.
C
      SUBROUTINE UPACKN (LEN,UNPKED,NCHAR,PACKED,ISTAT)
C
      CHARACTER*1 PACKED(1),UNPKED(1)
C
      INCLUDE 'udebug'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/new_alt/RCS/upackn.f,v $
     . $',                                                             '
     .$Id: upackn.f,v 1.2 1998/07/02 19:45:36 page Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (IUTLTR.GT.1) WRITE (IOGDB,10)
10    FORMAT (' *** ENTER UPACKN')
C
      ISTAT=0
C
      LENGTH=LEN
C
      IF (LEN.LT.1) GO TO 50
C
      NBYTES=NCHAR*4
C      
      IF (NBYTES.GE.LEN) GO TO 20
         ISTAT=1
         LENGTH=NBYTES
C         
20    DO 30 I=1,LENGTH
         PACKED(I)=UNPKED(1+(I-1)*4)
30       CONTINUE
C
      NLEFT=MOD(LENGTH,4)
      IF (NLEFT.EQ.0) GO TO 50
C
C  MUST FILL REST OF LAST FULL WORD OF PACKED WITH BLANKS
      NFILL=4-NLEFT
      DO 40 I=1,NFILL
         PACKED(LENGTH+I)=' '
40       CONTINUE
C
50    IF (IUTLDB.GT.1) WRITE (IOGDB,60) (PACKED(I),I=1,LENGTH)
60    FORMAT (' PACKED=',100A1)
C
      IF (IUTLTR.GT.1) WRITE (IOGDB,70) ISTAT
70    FORMAT (' *** EXIT UPACKN : ISTAT=',I2)
C
      RETURN
C
      END
