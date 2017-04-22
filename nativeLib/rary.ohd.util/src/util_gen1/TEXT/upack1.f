C MODULE UPACK1
C-----------------------------------------------------------------------
C
C  ROUTINE PACKS FOUR I*4 INTEGER WORDS INTO ONE WORD (4 I*1).
C
C  IF THE NUMBER OF WORDS IS NOT A MULTIPLE OF 4, THE LAST WORD IS
C  BLANK FILLED.
C
      SUBROUTINE UPACK1 (IN,IOUT,ICOUNT)
C
C  ARGUMENT LIST:
C    NAME     TYPE   I/O   DIM   DESCRIPTION
C    ------   ----   ---   ---   ------------
C    IN        A*1    I     4    INPUT ARRAY
C    IOUT      A*1    O     4    OUTPUT ARRAY
C    ICOUNT    I*4    I     1    NUMBER OF I*4 WORDS TO PACK
C
      CHARACTER*1 IOUT(4),IN(4)
C
      INCLUDE 'uiox'
      INCLUDE 'udebug'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_gen1/RCS/upack1.f,v $
     . $',                                                             '
     .$Id: upack1.f,v 1.2 2001/06/13 09:48:32 mgm Exp $
     . $' /
C    ===================================================================
C
C
      IF (IUTLTR.GE.1) WRITE (IOGDB,*) 'ENTER UPACK1'
C
      J=1
      DO 10 I=1,ICOUNT
         IOUT(I)=IN(J)
         J=J+4
10       CONTINUE
C
C  CHECK IF NEED TO FILL IN THE LAST WORD WITH BLANKS
      IADD=MOD(ICOUNT,4)
      IF (IADD.GT.0) THEN
         IADD=4-IADD
         I1=ICOUNT+1
         I2=ICOUNT+IADD
         DO 20 I=I1,I2
            IOUT(I)=' '
20          CONTINUE
         ENDIF
C 
      IF (IUTLTR.GE.1) WRITE (IOGDB,*) 'EXIT UPACK1'
C
      RETURN
C
      END
