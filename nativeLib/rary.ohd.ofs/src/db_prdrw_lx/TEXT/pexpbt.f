C MODULE PEXPBT
C-----------------------------------------------------------------------
C             VERSION:  1.0.0
C              AUTHOR:  JIM ERLANDSON
C                       DATA SCIENCES INC
C                       8555 16TH ST, SILVER SPRING, MD 587-3700
C***********************************************************************
C    ROUTINE EXPANDS THE HEADER PORTION OF A TIME SERIES RECORD.
C    I*1 AND I*2 VALUES ARE REPLACED BY I*4 WORDS IN THE OUTPUT ARRAY
C    EXPANDING THE HEADER TO DIMENSION (22). IF THE FIRST WORD IS
C    0, IT IS SET TO VALUE IN WORD 6(EXPANDED)-1.
C***********************************************************************
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C       IHEADI     I     I     18   INPUT ARRAY CONTAINING COMPACTED
C                                   HEADER
C
C       IHEADO     I     O     22   OUTPUT ARRAY CONTAINING EXPANDED
C                                   HEADER
C***********************************************************************
      SUBROUTINE PEXPBT (IHEADI,IHEADO)

      INCLUDE 'udebug'
      INCLUDE 'prdcommon/pdatas'

      INTEGER    IHEADI(18),IHEADO(22)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_prdrw_lx/RCS/pexpbt.f,v $
     . $',                                                             '
     .$Id: pexpbt.f,v 1.2 2002/02/21 20:26:42 dws Exp $
     . $' /
C    ===================================================================
C

        IHEAD = IHEADI(1)
        JHEAD = IHEAD/256
        IHEADO(1) = IHEAD - 256*JHEAD
        IHEAD = JHEAD/256
        IHEADO(2) = JHEAD - 256*IHEAD
        IHEADO(3) = IHEAD
        
CC      IHEAD = IHEADI(1)
CC      JHEAD = IHEAD/256

C  NOW DO I*2 WORDS

        IHEAD = IHEADI(2)
        JHEAD = IHEAD/65536
        IHEADO(4) = IHEAD - 65536*JHEAD
        IHEADO(5) = JHEAD

        IHEAD = IHEADI(3)
        JHEAD = IHEAD/65536
        IHEADO(6) = IHEAD - 65536*JHEAD
        IHEADO(7) = JHEAD

C  FILL IN WORDS  8-22 OF OUT WITH 4-18 OF IN

        DO 10 II=8,22
            IHEADO(II) = IHEADI(II-4)
   10   CONTINUE

      IF (IHEADO(1).EQ.0) IHEADO(1)=IHEADO(6)-1
      IF (IHEADO(1).EQ.LENHDC) IHEADO(1)=LENHED
      IF (IHEADO(6).EQ.0) IHEADO(6)=IHEADO(1)+1

      IF (IPRTR.GT.0) WRITE (IOGDB,20)
   20 FORMAT (' *** EXIT PEXPBT')

      RETURN
      END
