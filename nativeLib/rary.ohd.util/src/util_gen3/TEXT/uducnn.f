C MODULE UDUCNN
C-----------------------------------------------------------------------
C
C  ROUTINE UDUCNN SETS VALUES NOT TO BE CONVETED WHEN ROUTINE UDUCNV IS
C  CALLED TO CONVERT DATA VALUES.
C
      SUBROUTINE UDUCNN (NCNVAL,CNVAL,ISTAT)
C
C  INPUT VARIABLES:
C     NCNVAL - NUMBER OF VALUE NOT TO BE CONVERTED
C                0=RESET NUMBER TO ZERO
C     CNVAL  - VALUE NOT TO BE CONVERTED
C
C  OUTPUT VARIABLES:
C     ISTAT  - STATUS CODE
C                0=NORMAL RETURN
C                1=MAXIMUM NUMBER OF VALUES EXCEEDED
C
      INCLUDE 'uiox'
      INCLUDE 'ucmdbx'
      INCLUDE 'uduntx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_gen3/RCS/uducnn.f,v $
     . $',                                                             '
     .$Id: uducnn.f,v 1.2 2001/06/13 10:14:05 mgm Exp $
     . $' /
C    ===================================================================
C
C
      IF (ICMTRC.GT.0) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,*) 'ENTER UDUCNN'
         ENDIF
C
      ISTAT=0
C
C  CHECK IF TO RESET
      IF (NCNVAL.EQ.0) THEN
          NNUCNV=0
          DO 5 I=1,MNUCNV
             UCNVAL(I)=0.0
5            CONTINUE
         GO TO 10
         ENDIF
C
C  CHECK IF MAXIMUM VALUES EXCEEDED
      IF (NCNVAL.GT.MNUCNV) THEN
         CALL UEROR (LP,1,-1)
         WRITE (LP,20) MNUCNV
         ISTAT=1
         GO TO 10
         ENDIF
C
      NNUCNV=NCNVAL
      UCNVAL(NNUCNV)=CNVAL
C
10    IF (ICMTRC.GT.0) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,*) 'EXIT UDUCNN - ISTAT=',ISTAT
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
20    FORMAT ('+*** ERROR - IN UDUCNN - MAXIMUM NUMBER OF ',
     *   'VALUES NOT TO BE CONVERTED (',I2,') EXCEEDED.')
C
      END
