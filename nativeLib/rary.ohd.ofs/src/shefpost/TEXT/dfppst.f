C MODULE DFPPST
C-----------------------------------------------------------------------
C
C  THIS SUBROUTINE DISPLAYS THE STATION ID, DATA TYPE, SHEF CODE,
C  DATES AND VALUES POSTED TO THE PREPROCESSOR DATA BASE.
C
       SUBROUTINE DFPPST (IDSTA,ITYPE,IUNIT,JULTME,MINS,NUMOBS,
     *    VALBUF,IREV,ISCODE,IDQAL,IDSRCE,ITME,JPRINT)
C
C  ARGUMENT LIST:
C
C     NAME    TYPE  I/O   DIM     DESCRIPTION
C     ------  ----  ---   ---     -----------
C     IDSTA     A8    I     2     STATION ID
C     ITYPE     A4    I     1     DATA TYPE
C     IUNIT     A4    I     1     UNITS DESCRIPTION
C     JULTME    I     I   NUMOBS  DATE ARRAY(JULIAN HOURS)
C     MINS      I     I   NUMOBS  MINUTES OF DATA
C     NUMOBS    I     I     1     NUMBER OF OBSERVATIONS POSTED
C     VALBUF    R     I   NUMOBS  DAILY DATA VALUES
C     IREV      I     I     1     REVISION FLAG
C     ISCODE    A8    I     2     SHEF PARAMETER CODE
C     IDQAL     A4    I     1     DATA QUALIFIER
C     IDSRCE    A8    I     2     DATA SOURCE
C     ITME      I     I     1     TIME SERIES INDICATOR
C
      INCLUDE 'uiox'
      INCLUDE 'udebug'
      INCLUDE 'udatas'
      INCLUDE 'hclcommon/hdflts'
      INCLUDE 'pdbcommon/pdtrrx'
      INCLUDE 'pdbcommon/pdincs'
C
      DIMENSION IDSTA(2),IDSRCE(2),ISCODE(2)
      DIMENSION VALBUF(*),MINS(*),JULTME(*)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shefpost/RCS/dfppst.f,v $
     . $',                                                             '
     .$Id: dfppst.f,v 1.4 2005/08/12 14:51:15 dws Exp $
     . $' /
C    ===================================================================
C
      DATA LET0/4H0   /
      DATA ICOLON/4H:   /
      DATA LPP24/4HPP24/
C
C
      IF (IDETR.GT.0) WRITE(LP,*) 'ENTER DFPPST'
C
      LINE=IBLNK
C
C  CHECK IF REPORT HEADER HAS ALREADY BEEN PRINTED
      IF (JPRINT.NE.1) THEN
         CALL ULINE(LP,3)
         WRITE(LP,60)
         JPRINT=1
         ENDIF
C
C  SEE IF RRS TYPE
      IRX = IPDCKR(ITYPE)
      IF (IRX.GT.0) GO TO 35
C
C  CONVERT DATES FOR REPORT
      DO 30 I=1,NUMOBS
         JULTME(I) = JULTME(I) + NHOPDB
         JDY = JULTME(I)/24
         JHR = JULTME(I) - JDY * 24
         IF (IDEDB.GT.0) WRITE(LP,70) JDY,JHR
         CALL MDYH2 (JDY,JHR,IMO,IDAY,IYR,IHR,ITZ,IDSAV,TIME(3))
         IYR = MOD(IYR,100)
         IF (I.LE.1) THEN
C        PRINT DATA FOR ONE TYPE
            CALL DFCHKH (1)
            CALL ULINE(LP,1)
            WRITE (LP,80) IDSTA,ITYPE,IUNIT,ISCODE,IMO,IDAY,IYR,
     *        IHR,IBLNK,TIME(3),VALBUF(I),IDQAL,IREV,IDSRCE,ITME
C        SEE IF INCREMENTAL PRECIP WAS CALCULATED FROM 24 PRECIP VALUE
            IF (ITYPE.NE.LPP24 .OR. .NOT.RUNINC) GO TO 30
               DO 20 INC=1,NDATIN
C              CHECK IF INCREMENTAL PRECIP CALCULATED
                  IF (INCSTA(INC).EQ.0) THEN
                     INCHR = IHR - INCINT*(INC-1)
                     CALL DFCHKH (1)
                     CALL ULINE(LP,1)
                     WRITE (LP,90) INCTYP,INCHR,TIME(3),DATINC
                  ENDIF
20                CONTINUE
               ELSE
C              DO THE REST OF THE TIME SERIES
                  CALL DFCHKH (1)
                  CALL ULINE(LP,1)
                  WRITE(LP,100) IMO,IDAY,IYR,IHR,IBLNK,TIME(3),VALBUF(I)
               ENDIF
30       CONTINUE
      GO TO 50
C
C  RRS TYPE
C
35    DO 40 I=1,NUMOBS
         IDL = 0
C     CHECK IF THIS IS A MISSING VALUE
         IF (VALBUF(I).LE.-999.AND.JULTME(I).GE.0) GO TO 40
         IF (JULTME(I).LT.0) THEN
C        VALUE HAS BEEN DELETED - CONVERT THE NEGATIVE JULIAN HOUR 
            JULTME(I) = -JULTME(I)
            IDL = 1
            ENDIF
         JULTME(I) = JULTME(I) + NHOPDB
         JDY = JULTME(I)/24
         JHR = JULTME(I) - JDY * 24
         CALL MDYH2 (JDY,JHR,IMO,IDAY,IYR,IHR,ITZ,IDSAV,TIME(3))
         IYR = MOD(IYR,100)
C     ENCODE MINUTES FOR RRS DATA
         IF (MINS(I).GE.10) THEN
            CALL FFI2A (LINE,2,2,1,MINS(I))
            ELSE
               CALL UMOVEX (LET0,1,LINE,2,1)
               CALL FFI2A (LINE,3,1,1,MINS(I))
            ENDIF
         CALL UMOVEX (ICOLON,1,LINE,1,1)
         CALL DFCHKH (1)
         IF (IDL.EQ.0) THEN
            IF (I.LE.1) THEN
C           PRINT DATA FOR ONE OBSERVATION
               CALL ULINE(LP,1)
               WRITE( LP,80) IDSTA,ITYPE,IUNIT,ISCODE,IMO,IDAY,IYR,
     *            IHR,LINE,TIME(3),VALBUF(I),IDQAL,IREV,IDSRCE,ITME
               ELSE
                  CALL ULINE(LP,1)
                  WRITE(LP,100) IMO,IDAY,IYR,IHR,LINE,TIME(3),VALBUF(I)
               ENDIF
            ELSE
               CALL ULINE(LP,1)
               WRITE(LP,110) IDSTA,ITYPE,IMO,IDAY,IYR,IHR,LINE,TIME(3)
            ENDIF
40       CONTINUE
C
50    IF (IDETR.GT.0) WRITE (LP,*) 'EXIT DFPPST'
C
      RETURN
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
60    FORMAT('0',2X,'STATION ID',2X,'DATA TYPE',2X,'UNITS',2X,'SHEF',
     1    ' CODE',3X,'OBSERVATION DATE(TZC)',5X,'VALUE',6X,'DATA QUAL',
     2    2X,'REV CODE',3X,'DATA SOURCE',4X,'TIMES SERIES'/)
70    FORMAT(' JDY= ',I6,' JHR= ',I2)
80     FORMAT(4X,2A4,6X,A4,5X,A4,3X,2A4,4X,
     1        I2.2,'/',I2.2,'/',I2.2,1X,I2,A3,1X,
     2       A4,1X,F15.2,5X,A2,8X,I1,9X,2A4,9X,I1)
90     FORMAT(18X,A4,33X,I2,4X,A4,1X,F15.2)
100    FORMAT(46X,I2.2,'/',I2.2,'/',I2.2,1X,I2,A3,1X,A4,1X,F15.2)
110    FORMAT(' DATA FOR STATION ',2A4,' TYPE ',A4,' WAS DELETED FOR ',
     1        'THE DATE ',I2.2,'/',I2.2,'/',I2.2,1X,I2,A3,1X,A4)
C
      END
