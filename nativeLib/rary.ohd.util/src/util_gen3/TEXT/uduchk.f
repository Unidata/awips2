C MODULE UDUCHK
C-----------------------------------------------------------------------
C
C  ROUTINE TO CHECKSFOR A VALID DATA DIMENSION AND UNIT CODE.
C
      SUBROUTINE UDUCHK (DDIMN,DUNIT,BUNIT,IPRERR,ISTAT)
C
C  INPUT VARIABLES:
C     DDIMN  - DIMENSION CODE
C     DUNIT  - UNITS CODE
C     IPRERR - INDICATOR IF ERROR MESSAGE TO BE PRINTED
C                0=NO
C                1=YES
C
C  OUTPUT VARIABLES:
C     BUNIT  - BASE UNITS CODE
C     ISTAT  - STATUS CODE
C                0=NORMAL RETURN
C                1=DIMENSIONS AND UNITS BOTH BLANK
C                2=ERROR READING DATA UNIT FILE
C                3=INVALID DIMENSIONS
C                4=INVALID UNITS
C                5=INVALID DIMENSIONS AND UNITS
C                6=UNITS NOT VALID FOR DIMENSION
C
      CHARACTER*(*) DDIMN,DUNIT,BUNIT
C
      INCLUDE 'uiox'
      INCLUDE 'ucmdbx'
      INCLUDE 'uduntx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_gen3/RCS/uduchk.f,v $
     . $',                                                             '
     .$Id: uduchk.f,v 1.2 1998/10/14 15:56:49 page Exp $
     . $' /
C    ===================================================================
C
C
      IF (ICMTRC.GT.0) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,*) '*** ENTER UDUCHK -',
     *      ' DDIMN=',DDIMN,
     *      ' DUNIT=',DUNIT,
     *      ' '
         ENDIF
C
      ISTAT=0
C
      IF (DDIMN.EQ.' '.AND.DUNIT.EQ.' ') THEN
         IF (IPRERR.EQ.1) THEN
            CALL UEROR (LP,1,-1)
            WRITE (LP,70)
            ENDIF
         ISTAT=1
         ENDIF
C
C  CHECK IF COMMON BLOCK FILLED
      IF (IDUFIL.EQ.1) GO TO 10
C
C  FILL CONVERSION FACTOR COMMON BLOCK
      CALL UDUNIT (MDDIMN,MDUNIT,DUDIMN,DUUNIT,DUCFAC,DUTFAC,NDUNDC,
     *   IERR)
      IF (IERR.GT.0) THEN
         IF (IPRERR.EQ.1) THEN
            CALL UEROR (LP,1,-1)
            WRITE (LP,80) IERR
            ENDIF
         ISTAT=2
         IF (IERR.NE.3) GO TO 60
         ISTAT=0
         ENDIF
C
10    IDUFIL=1
C
C  CHECK IF DIMENSION CODE IS VALID
      LDIMN=0
      IF (DDIMN.NE.' ') THEN
         DO 20 IROW=1,MDDIMN
            IF (DDIMN.EQ.DUDIMN(IROW)) THEN
               LDIMN=IROW
               GO TO 30
               ENDIF
20          CONTINUE
         IF (IPRERR.EQ.1) THEN
            CALL UEROR (LP,1,-1)
            WRITE (LP,90) DDIMN,'DIMENSION'
            ENDIF
         ISTAT=3
         ENDIF
C
C  CHECK IF UNITS CODE IS VALID
30    LUNIT=0
      IF (DUNIT.NE.' ') THEN
         DO 40 IROW=1,MDDIMN
            DO 40 ICOL=1,MDUNIT
               IF (DUNIT.EQ.DUUNIT(IROW,ICOL)) THEN
                  LUNIT=IROW
                  GO TO 50
                  ENDIF
40          CONTINUE
         IF (IPRERR.EQ.1) THEN
            CALL UEROR (LP,1,-1)
            WRITE (LP,90) DUNIT,'UNIT'
            ENDIF
         IF (ISTAT.EQ.0) THEN
            ISTAT=4
            ELSE
               ISTAT=5
            ENDIF
         GO TO 60
50       BUNIT=DUUNIT(IROW,1)
         IF (ICMDBG.GT.0) THEN
            CALL ULINE (ICMPRU,1)
            WRITE (ICMPRU,*)
     *         ' IROW=',IROW,
     *         ' ICOL=',ICOL,
     *         ' BUNIT=',BUNIT,
     *         ' '
            ENDIF
         ENDIF
C
      IF (LDIMN.GT.0.AND.LUNIT.GT.0) THEN
         IF (LDIMN.NE.LUNIT) THEN
            IF (IPRERR.EQ.1) THEN
               CALL UEROR (LP,1,-1)
               WRITE (LP,100) DUNIT,DDIMN
               ENDIF
            ISTAT=6
            ENDIF
         ENDIF
C
60    IF (ICMTRC.GT.0) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,*) '*** EXIT UDUCHK -',
     *      ' ISTAT=',ISTAT,
     *      ' '
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
70    FORMAT ('+*** ERROR - IN UDUCHK - DATA DIMENSIONS AND UNITS ',
     *   'ARE BOTH BLANK.')
80    FORMAT ('+*** ERROR - IN UDUCHK - ERROR READING DATA UNIT FILE. ',
     *   'UDUNIT STATUS CODE = ',I2,'.')
90    FORMAT ('+*** ERROR - IN UDUCHK - ',A,' IS AN INVALID ',A,
     *   ' CODE.')
100   FORMAT ('+*** ERROR - IN UDUCHK - UNITS ',A,
     *   ' IS NOT A VALID FOR DIMENSION ',A,'.')
C
      END
