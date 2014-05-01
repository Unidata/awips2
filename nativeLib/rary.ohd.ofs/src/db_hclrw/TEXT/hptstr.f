C MODULE HPTSTR
C-----------------------------------------------------------------------
C
C  ROUTINE PACKS AND BLANK SUPRESSES CARD IMAGE RECORDS.
C
      SUBROUTINE HPTSTR (IBUF,LENGTH,ISBUF,ISTAT)
C
C  ARGUMENT LIST:
C
C     NAME     TYPE   I/O   DIM   DESCRIPTION
C     ------   ----   ---   ---   ------------
C     IBUF       I     I     72   INPUT BUFFER WITH STRING
C     LENGTH     I     I/O   1    LENGTH OF STRING
C     ISBUF      I     O     20   OUTPUT BUFFER WITH PACKED STRING
C     ISTAT      I     O     1    STATUS INDICATOR:
C                                   0=NORMAL RETURN
C                                   1=STRING EMPTY
C                                   2=LENGTH GT 72
C
      CHARACTER*8 RTNNAM,RTNOLD
C
      DIMENSION ISBUF(20),IBUF(1)
C
      INCLUDE 'uiox'
      INCLUDE 'udebug'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_hclrw/RCS/hptstr.f,v $
     . $',                                                             '
     .$Id: hptstr.f,v 1.2 2001/06/13 13:37:46 dws Exp $
     . $' /
C    ===================================================================
C
      DATA IQUOTE/4H'   /
C
C
      RTNNAM='HPTSTR'
C
      IF (IHCLTR.GT.1) WRITE (LP,*) 'ENTER ',RTNNAM
C
      IOPNUM=-1
      CALL FSTWHR (RTNNAM,IOPNUM,RTNOLD,IOLDOP)
C
      ISTAT=0
C
C  CHECK FOR EMPTY STRING
      IF (LENGTH.EQ.0) THEN
         WRITE (LP,10)
10    FORMAT ('0**ERROR** IN HPTSTR - STRING EMPTY')
         CALL ERROR
         ISTAT=1
         GO TO 90
         ENDIF
C
C  CHECK LENGTH
20    IF (LENGTH.GT.72) THEN
         WRITE (LP,30)
30    FORMAT ('0**ERROR** IN HPTSTR - STRING IS GREATER THEN 72 ',
     *   'CHARACTERS')
         CALL ERROR
         ISTAT=2
         GO TO 90
         ENDIF
C
C  CONVERT SINGLE TO DOUBLE QUOTE
      ICOUNT=0
      DO 70 I=1,72
         IF (IBUF(I).NE.IQUOTE) GO TO 50
            ICOUNT=ICOUNT+1
            IF (ICOUNT.NE.2) GO TO 60
            NUM=LENGTH-I+1
            LENGTH=LENGTH-1
            CALL UMEMOV (IBUF(I),IBUF(I-1),NUM)
50          CONTINUE
         ICOUNT=0
60       IF (I.EQ.LENGTH) GO TO 80
70       CONTINUE
      GO TO 20
C
C  BLANK SUPRESS
80    CALL HBLNKS (IBUF,ISBUF,LENGTH,ISTAT)
C
90    CALL FSTWHR (RTNOLD,IOLDOP,RTNOLD,IOLDOP)
C
      IF (IHCLTR.GT.2) WRITE (IOGDB,*) 'EXIT ',RTNNAM,' ISTAT=',ISTAT,
     *   ' LENGTH=',LENGTH
C
      RETURN
C
      END
