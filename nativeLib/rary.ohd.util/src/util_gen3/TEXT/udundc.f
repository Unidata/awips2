C MEMBER UDUNDC
C-----------------------------------------------------------------------
C
C @PROCESS LVL(77)
C
C  ROUTINE UDUNDC RETURNS THE NUMBER OF DECIMAL PLACES FOR A DATA UNIT.
C
      SUBROUTINE UDUNDC (DUNIT,NDEC,ISTAT)
C
C  INPUT VARIABLES:
C     DUNIT  - DATA UNIT CODE
C
C  OUTPUT VARIABLES:
C     NDEC   - NUMBER OF DECIMAL PLACES
C     ISTAT  - STATUS CODE
C                0=NORMAL RETURN
C                1=INVALID DATA UNIT CODE
C                2=ERROR IN READING DATAUNIT FILE
C
      CHARACTER*(*) DUNIT
C
      INCLUDE 'uiox'
      INCLUDE 'ucmdbx'
      INCLUDE 'uduntx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_gen3/RCS/udundc.f,v $
     . $',                                                             '
     .$Id: udundc.f,v 1.1 1995/09/17 19:03:39 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (ICMTRC.GT.0) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,*) '*** ENTER UDUNDC'
         ENDIF
C
      ISTAT=0
C
      IF (IDUFIL.EQ.0) THEN
         CALL UDUNIT (MDDIMN,MDUNIT,DUDIMN,DUUNIT,DUCFAC,DUTFAC,NDUNDC,
     *                IERR)
         IF (IERR.NE.0) THEN
            CALL UEROR (LP,1,-1)
            WRITE (LP,50) IERR
            ISTAT=2
            GO TO 30
            ENDIF
         IDUFIL=1
         ENDIF
C
C  CHECK IF UNIT CODE IS VALID
      DO 10 IROW=1,MDDIMN
         DO 10 ICOL=1,MDUNIT
         IF (DUNIT.EQ.DUUNIT(IROW,ICOL)) GO TO 20
10       CONTINUE
      CALL UEROR (LP,1,-1)
      WRITE (LP,40) DUNIT
      ISTAT=1
      GO TO 30
C
C  SET NUMBER OF DECIMAL PLACES
20    NDEC=NDUNDC(IROW,ICOL)
C
30    IF (ICMTRC.GT.0) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,*) '*** EXIT UDUNDC -',
     *      ' NDEC=',NDEC,
     *      ' ISTAT=',ISTAT,
     *      ' '
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
40    FORMAT ('+*** ERROR - IN UDUNDC - ',A,' IS AN INVALID ',
     *   'INPUT UNIT.')
50    FORMAT ('+*** ERROR - IN UDUNDC - ERROR READING DATA UNIT FILE. ',
     *   'UDUNIT STATUS CODE = ',I2,'.')
C
      END
