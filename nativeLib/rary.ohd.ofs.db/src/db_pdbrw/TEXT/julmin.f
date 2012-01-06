C MODULE JULMIN
C-----------------------------------------------------------------------
C
       FUNCTION JULMIN (NVLPOB,JHOUR)
C
C  THIS ROUTINE RETURNS THE JULIAN HOUR FROM A JULIAN HOUR
C  WHICH HAS MINUTES ENCODED FOR INSTANTANEOUS RRS DATA TYPES.
C  NON INSTANTANEOUS TYPE VALUES ARE NOT CHANGED.
C
C  ARGUMENT LIST:
C
C       NAME     TYPE   I/O   DIM   DESCRIPTION
C       ------   ----   ---   ---   -----------
C       NVLPOB     I     I     1    NUMBER OF VALUES PER OBSERVATION
C                                     2=INSTANTANEOUS
C                                     3=PERIOD TYPE
C       JHOUR      I     I     1    JULIAN HOUR (MINUTES ENCODED FOR
C                                   INSTANTANEOUS TYPES)
C
      INCLUDE 'udebug'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_pdbrw/RCS/julmin.f,v $
     . $',                                                             '
     .$Id: julmin.f,v 1.2 1999/04/23 19:32:54 page Exp $
     . $' /
C    ===================================================================
C
C
      IF (IPDTR.GT.1) WRITE (IOGDB,10) NVLPOB,JHOUR
10    FORMAT (' ENTER JULMIN : NVLPOB=',I2,3X,'JHOUR=',I8)
C
      JULMIN=JHOUR
C      
      IF (NVLPOB.EQ.2) THEN
         JULMIN=JHOUR/100
         ENDIF
C
      IF (IPDTR.GT.1) WRITE (IOGDB,30) JULMIN
30    FORMAT (' EXIT JULMIN : JULMIN=',I8)
C
      RETURN
C
      END
