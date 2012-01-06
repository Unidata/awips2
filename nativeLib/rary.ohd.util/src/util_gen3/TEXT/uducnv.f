C MODULE UDUCNV
C-----------------------------------------------------------------------
C
C  ROUTINE TO CONVERT DATA FROM ONE UNIT TO ANOTHER OR TO RETURN
C  THE CONVERSION FACTORS TO CONVERT DATA FROM ONE UNIT TO ANOTHER.
C
      SUBROUTINE UDUCNV (UNTIN,UNTOUT,ICONV,NVAL,VAL1,VAL2,ISTAT)
C
C  INPUT VARIABLES:
C     UNTIN  - INPUT UNITS CODE
C     UNTOUT - OUTPUT UNITS CODE
C     ICONV  - INDICATOR WHETHER CONVERSION FACTORS TO BE RETURNED
C                1=CONVERT DATA
C                2=RETURN CONVERSION FACTORS
C     NVAL   - NUMBER OF DATA VALUES TO BE CONVERTED
C
C  OUTPUT VARIABLES:
C     VAL1   - IF ICONV=1, ARRAY CONTAINING DATA VALUES TO BE CONVERTED
C              IF ICONV=2, CONVERION FACTOR
C     VAL2   - IF ICONV=1, ARRAY CONTAINING CONVERTED DATA VALUES
C              IF ICONV=2, SECOND CONVERION FACTOR FOR TEMERATURE DATA
C     ISTAT  - STATUS CODE
C                0=NORMAL RETURN
C                1=INVALID INPUT UNITS
C                2=INCOMPATIBLE INPUT AND OUTPUT UNITS
C                3=INVALID CONVERSION OPTION (ICONV INVALID)
C                4=ERROR READING DATA UNIT FILE
C                5=CONVERSION FACTOR IS ZERO FOR INPUT UNITS
C                6=NUMBER OF VALUES TO BE CONVERTED IS LESS
C                  THAN ZERO
C
C  IF THE OPTION TO RETURN THE CONVERION FACTORS IS USED, DATA CAN
C  BE CONVERTED BY USING AN EQUATION SIMILAR TO THE FOLLOWING:
C       NEWVAL=OLDVAL*VAL1+VAL2
C
      CHARACTER*4 UNTIN,UNTOUT
      CHARACTER*6 RTNNAM
C
      REAL*8 FACTOR,FACTTB,FACTFB
C
      DIMENSION VAL1(NVAL),VAL2(NVAL)
C
      INCLUDE 'uiox'
      INCLUDE 'ucmdbx'
      INCLUDE 'uduntx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_gen3/RCS/uducnv.f,v $
     . $',                                                             '
     .$Id: uducnv.f,v 1.2 2000/07/20 07:37:28 page Exp $
     . $' /
C    ===================================================================
C
C
      RTNNAM='UDUCNV'
C
      IF (ICMTRC.GT.0) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,*) 'ENTER ',RTNNAM
         ENDIF
C
      IF (ICMTRC.GT.0) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,*)
     *      ' UNTIN=',UNTIN,
     *      ' UNTOUT=',UNTOUT,
     *      ' ICONV=',ICONV,
     *      ' NVAL=',NVAL,
     *      ' '
         ENDIF
C
      ISTAT=0
C
      INDERR=0
C
      TFACT=0.0
C
C  CHECK THE CONVERSION OPTION
      IF (ICONV.EQ.1.OR.ICONV.EQ.2) THEN
         ELSE
            CALL UEROR (LP,1,-1)
            WRITE (LP,160) ICONV
            ISTAT=3
            GO TO 130
         ENDIF
C
      IF (ICMDBG.GT.0) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,*)
     *      ' IDUFIL=',IDUFIL,
     *      ' '
         ENDIF
C
C  CHECK IF CONVERSION FACTOR COMMON BLOCK FILLED
      IF (IDUFIL.EQ.0) THEN
         CALL UDUNIT (MDDIMN,MDUNIT,DUDIMN,DUUNIT,DUCFAC,DUTFAC,NDUNDC,
     *      IERR)
         IF (IERR.GT.0) THEN
            CALL UEROR (LP,1,-1)
            WRITE (LP,150) IERR
            ISTAT=4
            IF (IERR.NE.3) GO TO 130
            ISTAT=0
            ENDIF
         IDUFIL=1
	 ENDIF
C
      IF (ICMDBG.GT.0) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,*)
     *      ' MDDIMN=',MDDIMN,
     *      ' MDUNIT=',MDUNIT,
     *      ' '
         ENDIF
C
C  CHECK IF INPUT UNIT IS VALID
      DO 30 IROW=1,MDDIMN
         DO 20 ICOL=1,MDUNIT
            IF (ICMDBG.GT.0) THEN
               CALL ULINE (ICMPRU,1)
               WRITE (ICMPRU,*)
     *            ' UNTIN=',UNTIN,
     *            ' IROW=',IROW,
     *            ' ICOL=',ICOL,
     *            ' DUUNIT(IROW,ICOL)=',DUUNIT(IROW,ICOL),
     *            ' '
               ENDIF
            IF (DUUNIT(IROW,ICOL).EQ.' ') GO TO 30
            IF (UNTIN.EQ.DUUNIT(IROW,ICOL)) GO TO 50
20          CONTINUE
30       CONTINUE
      CALL UEROR (LP,1,-1)
      WRITE (LP,170) UNTIN
      INDERR=1
C
50    JINCOL=ICOL
C
C  CHECK IF OUTPUT UNIT IS VALID
      DO 60 ICOL=1,MDUNIT
         IF (ICMDBG.GT.0) THEN
            CALL ULINE (ICMPRU,1)
            WRITE (ICMPRU,*)
     *         ' UNTOUT=',UNTOUT,
     *         ' IROW=',IROW,
     *         ' ICOL=',ICOL,
     *         ' DUUNIT(IROW,ICOL)=',DUUNIT(IROW,ICOL),
     *         ' '
            ENDIF
         IF (DUUNIT(IROW,ICOL).EQ.' ') GO TO 70
         IF (UNTOUT.EQ.DUUNIT(IROW,ICOL)) THEN
            JOTCOL=ICOL
            GO TO 80
            ENDIF
60       CONTINUE
C
C  INVALID UNITS CONVERSION
70    CALL UEROR (LP,1,-1)
      WRITE (LP,180) UNTIN,UNTOUT
      ISTAT=2
      INDERR=1
C
80    IF (INDERR.EQ.1) GO TO 130
C
C  CALCULATE CONVERSION FACTOR
      FACTTB=DUCFAC(IROW,JINCOL)
      IF (DUCFAC(IROW,JOTCOL).NE.0.0) GO TO 90
         CALL UEROR (LP,1,-1)
         WRITE (LP,190) UNTIN
         ISTAT=5
         GO TO 130
90    FACTFB=1.0/DUCFAC(IROW,JOTCOL)
      FACTOR=FACTTB*FACTFB
      IF (ICMDBG.GT.0) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,*)
     *      ' FACTTB=',FACTTB,
     *      ' FACTFB=',FACTFB,
     *      ' FACTOR=',FACTOR,
     *      ' '
         ENDIF
C
C  CHECK IF CONVERTING TEMPERATURE DATA
      IF (UNTIN.EQ.'DEGC'.OR.
     *    UNTIN.EQ.'DEGF'.OR.
     *    UNTIN.EQ.'DEGK'.OR.
     *    UNTIN.EQ.'DEGR') THEN
C     CALCULATE SECOND CONVERSION FACTOR
         TFACT1=DUTFAC(JINCOL)
         TFACT2=(-1.0)*DUTFAC(JOTCOL)*FACTFB
         TFACT=TFACT2+TFACT1*FACTFB
         IF (ICMDBG.GT.0) THEN
            CALL ULINE (ICMPRU,1)
            WRITE (ICMPRU,*)
     *         ' TFACT1=',TFACT1,
     *         ' TFACT2=',TFACT2,
     *         ' TFACT=',TFACT,
     *         ' '
            ENDIF
         ENDIF
C
C  CHECK CONVERSION OPTION
      IF (ICONV.EQ.2) GO TO 120
C
      IF (NVAL.EQ.0) THEN
         CALL ULINE (LP,2)
         WRITE (LP,200)
         GO TO 130
         ENDIF
C
      IF (NVAL.LT.0) THEN
         CALL UEROR (LP,1,-1)
         WRITE (LP,210) NVAL
         ISTAT=6
         GO TO 130
         ENDIF
C
C  CONVERT DATA
      DO 110 I=1,NVAL
C     CHECK IF VALUE NOT TO BE CONVERTED
         IF (NNUCNV.GT.0) THEN
            DO 100 N=1,NNUCNV
               IF (VAL1(I).EQ.UCNVAL(N)) GO TO 110
100            CONTINUE
            ENDIF
         VAL2(I)=FACTOR*VAL1(I)+TFACT
110      CONTINUE
      GO TO 130
C
C  STORE CONVERSION FACTORS
120   VAL1(1)=FACTOR
      VAL2(1)=TFACT
      IF (ICMDBG.GT.0) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,*)
     *      ' VAL1(1)=',VAL1(1),
     *      ' VAL2(1)=',VAL2(1),
     *      ' '
         ENDIF
C
130   IF (ICMTRC.GT.0) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,*) 'EXIT ',RTNNAM,' - ISTAT=',ISTAT
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
150   FORMAT ('+*** ERROR - IN UDUCNV - ERROR READING DATA UNIT FILE. ',
     *   'UDUNIT STATUS CODE = ',I2,'.')
160   FORMAT ('+*** ERROR - IN UDUCNV - ',I3,' IS AN INVALID ',
     *   'CONVERSION OPTION.')
170   FORMAT ('+*** ERROR - IN UDUCNV - ',A,' IS AN INVALID ',
     *   'INPUT UNIT.')
180   FORMAT ('+*** ERROR - IN UDUCNV - A UNITS CONVERSION FROM ',A,
     *   ' TO ',A,' IS INVALID.')
190   FORMAT ('+*** ERROR - IN UDUCNV - CONVERTION FACTOR FOR UNITS ',
     *   A,' IS 0.0')
200   FORMAT ('0*** NOTE - IN UDUCNV - NUMBER OF VALUES TO BE ',
     *   'CONVERTED IS ZERO.')
210   FORMAT ('+*** ERROR - IN UDUCNV - NUMBER OF VALUES TO BE ',
     *   'CONVERTED (',I5,') IS LESS THAN ZERO.')
C
      END
