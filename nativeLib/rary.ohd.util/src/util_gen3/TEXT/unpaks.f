C MODULE UNPAKS
C----------------------------------------------------------------------
C
C  ROUTINE TO UNPACK CHARACTER DATA TO A1 FORMAT.
C
      SUBROUTINE UNPAKS (IN,IOUT,NUM,MAX,ISTAT)
C
C  ROUTINE UNPACKS CHARACTER DATA TO A1 FORMAT.
C
C  ARGUMENT LIST:
C    NAME     TYPE   I/O   DIM   DESCRIPTION
C    ------   ----   ---   ---   ------------
C    IN        A4     I     ?    PACKED INPUT ARRAY
C    OUT       A1     O     ?    UNPACKED OUTPUT ARRAY
C    NUM       I      I     1    NUMBER OF PACKED WORDS IN ARRAY IN
C    MAX       I      I     1    DIMENSION OF ARRAY OUT
C    ISTAT     I      O     1    STATUS INDICATOR:
C                                  0=OK
C                                  1=ARRAY OUT TOO SMALL
C
      INCLUDE 'uiox'
      INCLUDE 'udebug'
C
      CHARACTER*1 IN(4),IOUT(4)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_gen3/RCS/unpaks.f,v $
     . $',                                                             '
     .$Id: unpaks.f,v 1.2 2001/06/13 09:46:00 mgm Exp $
     . $' /
C    ===================================================================
C
C
      IF (IUTLTR.GE.1) WRITE (IOGDB,*) 'ENTER UNPAKS'
C
      ISTAT=0
C
      NUMS=NUM*4
      IF (NUMS.GT.MAX) THEN
         WRITE (LP,40) NUMS,MAX
40    FORMAT ('0**ERROR** IN UNPAKS - NUMBER OF CHARACTERS TO BE ',
     *   ' PACKED (',I3,') EXCEEDS SIZE OF OUTPUT ARRAY (',I3,').')
         ISTAT=1
         GO TO 50
         ENDIF
C
      J=1
      DO 10 I=1,NUMS
         IOUT(J)=IN(I)
         IOUT(J+1)=' '
         IOUT(J+2)=' '
         IOUT(J+3)=' '
         J=J+4
10       CONTINUE
C
C  BLANK OUT REST OF ARRAY
      K=MAX*4
      IF (J.LE.K) THEN
         DO 20 I=J,K
             IOUT(I)=' '
20           CONTINUE
         ENDIF
C
50    IF (IUTLTR.GE.1) WRITE (IOGDB,*) 'EXIT UNPAKS'
C
      RETURN
C
      END
