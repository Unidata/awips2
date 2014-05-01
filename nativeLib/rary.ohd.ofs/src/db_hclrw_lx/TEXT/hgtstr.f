C MODULE HGTSTR
C-----------------------------------------------------------------------
C
      SUBROUTINE HGTSTR (LKSBUF,ISBUF,KSBUF,LENGTH,ISTAT)
C
C  THIS ROUTINE EXPANDS A CHARACTER STRING THAT HAS HAD BLANKS REMOVED.
C
C  ARGUMENT LIST:
C
C       NAME      TYPE  I/O   DIM   DESCRIPTION
C       ------    ----  ---   ---   -----------
C       LKSBUF     I     I     1    NUMBER OF 4-BYTE WORDS IN KSBUF
C       ISBUF      A4    I     ?    INPUT ARRAY - BLANK SUPRESSED
C       KSBUF      A4    O   LKSBUF OUTPUT ARRAY - BLANK EXPANDED
C       LENGTH     I     O     1    LENGTH OF STRING
C       ISTAT      I     O     1    STATUS:
C                                     0=NORMAL RETURN
C                                     1=LKSBUF TOO SMALL
C
      INCLUDE 'uiox'
      INCLUDE 'udatas'
      INCLUDE 'udebug'
C
      CHARACTER*1 ISBUF(*),KSBUF(LKSBUF)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_hclrw_lx/RCS/hgtstr.f,v $
     . $',                                                             '
     .$Id: hgtstr.f,v 1.1 2001/09/28 14:13:56 dws Exp $
     . $' /
C    ===================================================================
C
C
      DO 5 I=1,LKSBUF*4
         KSBUF(I)=' '
5        CONTINUE         
C
      INTLEN=0
      CALL UMEMOV (ISBUF(1),INTLEN,1)      
C  CHANGE NEEDED FOR RUNNING ON LINUX MACHINE
CCC      LENGTH=INTLEN/2**24
      LENGTH=MOD(INTLEN,2**8)
      LEN=(LENGTH+3)/4
      IF (LEN.GT.LKSBUF) THEN
         WRITE (LP,10) LEN,LKSBUF
10    FORMAT ('0**ERROR** IN HGTSTR - NUMBER OF WORDS NEEDED IN ',
     *   'ARRAY TO HOLD STRING (',I3,') EXCEEDS ARRAY SIZE (',I4,').')
         ISTAT=1
         GO TO 30
         ENDIF
C
C  EXPAND STRING
      CALL HBLNKE (ISBUF(2),KSBUF,ISTAT)
      N=LKSBUF*4
      IF (IHCLTR.GT.2) WRITE (IOGDB,40) LENGTH,ISTAT,(KSBUF(I),I=1,N)
40    FORMAT (' EXIT HGTSTR - LENGTH=',I2,' ISTAT=',I2 /
     *        ' STRING=',80A1)
C
30    RETURN
C
      END
