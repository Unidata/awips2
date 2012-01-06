C MODULE RPCARD
C-----------------------------------------------------------------------
C
C  THIS ROUTINE READS A CARD INTO AN ARRAY.
C
       SUBROUTINE RPCARD (IBUF,ISTAT)
C
C  ARGUMENT LIST:
C
C     NAME    TYPE  I/O   DIM   DESCRIPTION
C     ------  ----  ---   ---   -----------
C     IBUF     A*1   O     80    ARRAY CONTAINING CARD IMAGE
C     ISTAT    I*4   O     1    STATUS INDICATOR:
C                                 0=RECORD READ
C                                 1=READ ERROR OR END OF FILE
C
      CHARACTER*4 IBUF(80)
C
      INCLUDE 'uiox'
      INCLUDE 'udebug'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_util/RCS/rpcard.f,v $
     . $',                                                             '
     .$Id: rpcard.f,v 1.2 2001/06/15 12:49:16 dws Exp $
     . $' /
C    ===================================================================
C
C
      ISTAT=0
C
      READ (ICD,10,ERR=20,END=20) IBUF
10    FORMAT (80A1)
      GO TO 30
C
C  READ ERROR OR END OF FILE
20    ISTAT=1
C
30    IF (IUTLDB.GT.0) WRITE (IOGDB,40) ICD,ISTAT,IBUF
c
c     DR17865 
cfan
c     Integer array can't be printed out in F format in Fortran 90 
cfan
cf 40 FORMAT(' IN RPCARD - ICD=',I3,'  ISTAT=',I3 / ' IBUF: ',A)
40    FORMAT(' IN RPCARD - ICD=',I3,'  ISTAT=',I3 / ' IBUF: ',80A1)  !cfan10/2006
C
      RETURN
C
      END
