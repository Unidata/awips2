C MODULE HBLNKS
C-----------------------------------------------------------------------
C
      SUBROUTINE HBLNKS (IBUF,ISBUF,LENGTH,ISTAT)
C
C    ROUTINE TO SUPRESS BLANKS FOR CARD IMAGE RECORDS. IF THERE ARE 
C    MORE THAN 2 CONTIGUOUS BLANKS THE BLANKS WILL BE SUPRESSED.
C    ROUTINE UPACK1 IS CALLED TO PACK THE STRING AFTER BLANK ARE
C    SUPRESSED. THE LENGTH OF THE STRING IS PLACED IN FIRST BYTE AND 
C    RETURNED AS THE LENGTH OF PACKED STRING.
C
C    ARGUMENT LIST:
C
C       NAME      TYPE  I/O   DIM    DESCRIPTION
C       ------    ----  ---   ---    -----------
C       IBUF       I     I     1     INPUT ARRAY CONTAINING STRING
C       ISBUF      I     O     1     OUTPUT ARRAY PACKED AND SUPRESSED
C       LENGTH     I    I/O    1     LENGTH OF STRING
C       ISTAT      I     O     1     STATUS INDICATOR
C                                       0=NORMAL RETURN
C                                       1=STRING EMPTY
C
      INCLUDE 'uiox'
      INCLUDE 'udebug'
C
      DIMENSION IBUF(*),ISBUF(*)
      PARAMETER (LITBUF=80)
      DIMENSION ITBUF(LITBUF)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_hclrw_lx/RCS/hblnks.f,v $
     . $',                                                             '
     .$Id: hblnks.f,v 1.1 2001/09/28 14:13:56 dws Exp $
     . $' /
C    ===================================================================
C
C
      ISTAT=0
C      
C  CHANGE NEEDED FOR RUNNING ON LINUX MACHINE
CCC      IMULT=2**24
      IMULT=1
      ITBUF(1)=LENGTH*IMULT
      ISTOP=LENGTH
C
C  CHECK IF THERE ARE BLANKS TO SUPRESS
      CALL HFND3B (IBUF,1,ISTOP,K)
      IF (K.GT.ISTOP) GO TO 40
C
C  CHECK IF FIRST WORDS ARE BLANKS
      IPOS=2
      IF (K.EQ.1) GO TO 10
C
C  NO BLANKS FOUND MOVE FIRST FIELD
      NPOS=K-1
      CALL UMEMOV (IBUF,ITBUF(4),NPOS)
      ITBUF(2)=0
      ITBUF(3)=NPOS*IMULT
      IPOS=NPOS+4
C
C  DO REST OF FIELDS
C     FIND NUMBER OF BLANKS
10       CALL UNOBLK (IBUF,K,ISTOP,I)
         NUMBLK=I-K
         IF (I.GT.ISTOP) GO TO 50
C     FIND NUMBER OF CHARACTERS
         CALL HFND3B (IBUF,I,ISTOP,J)
         NUMCHR=J-I
C     MOVE CHARACTERS
         ITBUF(IPOS)=NUMBLK*IMULT
         ITBUF(IPOS+1)=NUMCHR*IMULT
         IF (IPOS+NUMCHR+2.GT.LITBUF) THEN
            WRITE (LP,30) LITBUF
30    FORMAT ('0**ERROR** IN HBLNKS - MAXIMUM NUMBER OF CHARACTERS ',
     *   'ALLOWED IN CHARACTER STRING (',I3,') EXCEEDED.')
            ISTAT=1
            GO TO 80
            ENDIF
         CALL UMEMOV (IBUF(I),ITBUF(IPOS+2),NUMCHR)
         IPOS=IPOS+NUMCHR+2
         IF (J.GT.ISTOP) GO TO 60
         K=J
         GO TO 10
C
C  NO BLANKS
40    ITBUF(2)=0
      ITBUF(3)=ISTOP*IMULT
      ITBUF(ISTOP+4)=0
      ITBUF(ISTOP+5)=0
      NUM=ISTOP+5
      CALL UMEMOV (IBUF,ITBUF(4),ISTOP)
      GO TO 70
C
C  REST OF RECORD IS EMPTY
50    NUM=IPOS+1
      ITBUF(IPOS)=NUMBLK*IMULT
      ITBUF(IPOS+1)=0
      GO TO 70
C
C  REST OF RECORD WAS CHARACTERS
60    ITBUF(IPOS)=0
      ITBUF(IPOS+1)=0
      NUM=IPOS+1
C
C  PACK THE STRING
70    CALL UPACK1 (ITBUF,ISBUF,NUM)
      LENGTH=(NUM+3)/4
C
80    IF (IHCLTR.GT.2) WRITE (IOGDB,90) NUM,(ITBUF(I),I=1,NUM)
90    FORMAT (' EXIT HBLNKS - NUM=',I3,' ITBUF=' /
     *   3(1X,I10) / (1X,80A1))
C
      RETURN
C
      END

