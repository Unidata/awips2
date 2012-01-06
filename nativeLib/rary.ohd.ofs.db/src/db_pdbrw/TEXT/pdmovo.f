C MODULE PDMOVO
C-----------------------------------------------------------------------
C
      SUBROUTINE PDMOVO (INBUF,NVLPOB,LOBS,OBS,NUMOBS,LMIN,MIN,IMPOS,
     *   ISTAT)
C
C  THIS ROUTINE MOVES ONE OBSERVATION SET FROM AN RRS DATA RECORD
C  INTO THE OBS AND MIN ARRAYS.
C
C  ARGUMENT LIST:
C
C       NAME     TYPE   I/O   DIM   DESCRIPTION
C       ------   ----   ---   ---   -----------
C       INBUF      I     I     ?    BUFFER CONTAINING DATA VALUES
C       NVLPOB     I     I     1    NUMBER OF VALUES PER OBSERVARION
C       LOBS       I     I     1    LENGTH OF ARRAY OBS
C       OBS        R    I/O    ?    ARRAY FOR OBSERVATIONS
C       NUMOBS     I    I/O    1    NUMBER OF OBS IN ARRAY OBS
C       LMIN       I     I     1    LENGTH OF ARRAY MIN
C       MIN        I     O    I/O   ARRAY FOR MINUTES
C       IMPOS      I    I/O    1    POSITION IN MINUTES ARRAY
C       ISTAT      I     O     1    STATUS INDICATOR:
C                                     0=NORMAL RETURN
C                                     1=OBS FULL
C                                     2=MIN FULL
C
      INCLUDE 'uio'
      INCLUDE 'udsi'
      INCLUDE 'udebug'
C
      DIMENSION INBUF(1),OBS(LOBS),MIN(LMIN),ITBUF(3)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_pdbrw/RCS/pdmovo.f,v $
     . $',                                                             '
     .$Id: pdmovo.f,v 1.2 1999/01/20 13:25:22 page Exp $
     . $' /
C    ===================================================================
C
C
      IF (IPDTR.GT.1) WRITE (IOGDB,10)
10    FORMAT (' *** ENTER PDMOVO')
C
      ISTAT=0
C
C  CHECK FOR ROOM IN OBS ARRAY
      NPOS=NUMOBS*NVLPOB+1
      IF (NPOS+NVLPOB-1.GT.LOBS) THEN
         ISTAT=1
         GO TO 20
         ENDIF
C
C  MOVE INTO OBS ARRAY
      CALL UMEMOV (INBUF,ITBUF,NVLPOB)
      ITBUF(1)=JULMIN(NVLPOB,INBUF(1))
      CALL UMEMOV (ITBUF,OBS(NPOS),NVLPOB)
      NUMOBS=NUMOBS+1
C
C  CHECK IF NEED TO MOVE MINUTES
      IF (NVLPOB.EQ.2) THEN
         IMPOS=IMPOS+1
C     CHECK FOR ROOM IN MIN ARRAY
         IF (IMPOS.GT.LMIN) THEN
            ISTAT=2
            IMPOS=IMPOS-1
            GO TO 20
            ENDIF
         MIN(IMPOS)=MOD(INBUF(1),100)
         ENDIF
C
20    IF(IPDTR.GT.1) WRITE (IOGDB,30)
30    FORMAT(' *** EXIT PDMOVO')
C
      RETURN
C
      END
