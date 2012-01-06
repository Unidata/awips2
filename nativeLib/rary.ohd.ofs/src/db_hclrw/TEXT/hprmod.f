C MODULE HPRMOD
C-----------------------------------------------------------------------
C
      SUBROUTINE HPRMOD (KBUF)
C
C  ROUTINE TO PRINT MOD RECORDS FOR OPTIONS RECORDS.
C
C  ARGUMENT LIST:
C
C    NAME     TYPE   I/O   DIM   DESCRIPTION
C    ------   ----   ---   ---   -------------
C    KBUF       I     I     ?    ARAY CONTAINING MODS
C
      INCLUDE 'uiox'
      INCLUDE 'udebug'
C
      DIMENSION KBUF(*)
      CHARACTER*72 IBUF
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_hclrw/RCS/hprmod.f,v $
     . $',                                                             '
     .$Id: hprmod.f,v 1.3 2002/02/11 20:36:40 dws Exp $
     . $' /
C    ===================================================================
C
C
      LIBUF=LEN(IBUF)/4
C
      CALL ULINE (LP,2)
      WRITE (LP,10)
10    FORMAT ('0',13X,'MODS:')
C
      IF (KBUF(1).EQ.0) GO TO 20
C
C  PRINT IDENTIFIERS
      CALL HPRIDS (KBUF,IPOINT,2)
      GO TO 30
C
20    IPOINT=2
C
C  PRINT MODS
30    NUMCDS=KBUF(IPOINT)
      IF (NUMCDS.EQ.0) GO TO 70
      CALL ULINE (LP,1)
      WRITE (LP,40)
40    FORMAT (16X,'CARDS:')
      IPOS=IPOINT+2
      DO 60 I=1,NUMCDS
         CALL HGTSTR (LIBUF,KBUF(IPOS),IBUF,LENGTH,ISTAT)
         IF (ISTAT.NE.0) GO TO 90
         CALL ULINE (LP,1)
         WRITE (LP,50) I,IBUF(1:LENSTR(IBUF))
50       FORMAT (15X,I5,')  ',A)
         IPOS=IPOS+KBUF(IPOS-1)+1
60       CONTINUE
C
70    CALL ULINE (LP,1)
      WRITE (LP,80)
80    FORMAT (' ',13X,'ENDMODS')
C
      GO TO 110
C
90    CALL ULINE (LP,2)
      WRITE (LP,100) ISTAT
100   FORMAT ('0**ERROR** IN HPRMOD - HGTSTR STATUS CODE IS ',I2,'.')
C
110   IF (IHCLTR.GT.0) WRITE (IOGDB,120)
120   FORMAT (' EXIT HPRMOD')
C
      RETURN
C
      END
