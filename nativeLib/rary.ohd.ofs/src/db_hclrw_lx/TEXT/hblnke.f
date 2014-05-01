C MODULE HBLNKE
C-----------------------------------------------------------------------
C
      SUBROUTINE HBLNKE (ISBUF,KSBUF,ISTAT)
C
C  THIS ROUTINE EXPANDS A BLANK SUPRESSED STRING.
C  STRING MUST HAVE BEEN BLANK SUPRESSED BY HBLNKS.
C
C  ARGUMENT LIST:
C
C      NAME    TYPE  I/O   DIM   DESCRIPTION
C      ------  ----  ---   ---   -----------
C      ISBUF    A     I     ?    INPUT STRING
C      KSBUF    A     O     ?    OUTPUT STRING
C
      CHARACTER*1 ISBUF(*),KSBUF(*)
      CHARACTER*1 LUMBLK,LUMCHR
      INTEGER*4 IUMBLK,IUMCHR
      EQUIVALENCE (IUMBLK,LUMBLK),(IUMCHR,LUMCHR)
C
      INCLUDE 'uiox'
      INCLUDE 'udebug'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_hclrw_lx/RCS/hblnke.f,v $
     . $',                                                             '
     .$Id: hblnke.f,v 1.2 2004/06/21 14:09:31 hsu Exp $
     . $' /
C    ===================================================================
C
C
      ISTAT=0
C      
      J=1
      I=1      
C  CHANGE NEEDED FOR RUNNING ON LINUX MACHINE
CCC      IDIV=2**24
      IDIV=2**8
      MBUF=80
C
C  PROCESS EACH FIELD
40    LUMBLK=ISBUF(I)
      LUMCHR=ISBUF(I+1)      
C  CHANGE NEEDED FOR RUNNING ON LINUX MACHINE
CCC      NUMBLK=IUMBLK/IDIV
CCC      NUMCHR=IUMCHR/IDIV
      NUMBLK=MOD(IUMBLK,IDIV)
      NUMCHR=MOD(IUMCHR,IDIV)
      I=I+2
C
      IF (NUMBLK.EQ.0) GO TO 20
C
C  PUT IN BLANKS
      DO 10 II=1,NUMBLK
         IF (J.GT.MBUF) THEN
            WRITE (LP,50) MBUF
C ksh 06/09/2004
C 50    FORMAT ('0**ERROR** IN HBLNKE - MAXIMUM NUMBER OF CHARACTERS ',
C 
50    FORMAT ('0**WARNING** IN HBLNKE - MAXIMUM NUMBER OF CHARACTERS ',
     *   'ALLOWED IN CHARACTER STRING (',I3,') EXCEEDED.')
            ISTAT=1
            GO TO 80
            ENDIF
         KSBUF(J)=' '
         J=J+1
10       CONTINUE
C
20    IF (NUMCHR.EQ.0) GO TO 60
C
C  PUT IN CHARACTERS
      DO 30 II=1,NUMCHR
         IF (J.GT.MBUF) THEN
            WRITE (LP,50) MBUF
            ISTAT=1
            GO TO 80
            ENDIF
         KSBUF(J)=ISBUF(I)
         J=J+1
         I=I+1
30       CONTINUE
      GO TO 40
C
C  BLANK FILL LAST WORD
60    J=J-1
      IREM=MOD(J,4)
      IF (IREM.EQ.0) GO TO 80
      IREM=4-IREM
      J=J+1
      DO 70 M=1,IREM
         IF (J.GT.MBUF) THEN
            WRITE (LP,50) MBUF
            ISTAT=1
            GO TO 80
            ENDIF
         KSBUF(J)=' '
         J=J+1
70       CONTINUE
C
80    IF (IHCLTR.GT.2) WRITE (IOGDB,90) ISTAT
90    FORMAT (' EXIT HBLNKE - ISTAT=',I2)
C
      RETURN
C
      END
