C MODULE PDCKI2
C-----------------------------------------------------------------------
C
      SUBROUTINE PDCKI2 (I4VAL,ISTAT)
C
C  THIS ROUTINE CHECKS IF AN INTEGER*4 VALUE CAN BE STORED AS AN 
C  INTEGER*2.
C
C  ARGUMENT LIST:
C
C       NAME      TYPE  I/O   DIM   DESCRIPTION
C       ----      ----  ---   ---   -----------
C       I4VAL       I    I      1   INTEGER*4 VALUE TO BE CHECKED
C       ISTAT       I    O      1   STATUS CODE:
C                                     0=OK
C                                     1=ERROR
C
      INTEGER*2 I2VAL
      INTEGER*4 I4VAL
C      
      INCLUDE 'uio'
      INCLUDE 'udebug'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_pdbrw/RCS/pdcki2.f,v $
     . $',                                                             '
     .$Id: pdcki2.f,v 1.1 1999/04/26 11:51:43 page Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (IPDTR.GT.0) WRITE (IOGDB,*) 'ENTER PDCKI2'
C
      ISTAT=0
C
      I2VAL=I4VAL
C      
      IF (I2VAL.NE.I4VAL) THEN
         WRITE (LP,10) I4VAL
10    FORMAT ('0**ERROR** IN PDCKI2 - INTEGER*4 VALUE ',I6,
     *   ' CANNOT BE STORED AS AN INTEGER*2 VALUE.')
         ISTAT=1
         ENDIF
C
      IF (IPDTR.GT.0) WRITE (IOGDB,*) 'EXIT PDCKI2'
C
      RETURN
C
      END
