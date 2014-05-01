C MODULE QPRNT
C-----------------------------------------------------------------------
C
C  ROUTINE QPRNT FILLS THE PRNTOB AND PRNTTS ARRAYS WITH THE DATA TYPE
C  CODES FOR THOSE DATA TYPES THAT ARE FOUND IN THE PPDB FOR A GIVEN
C  STATION AND WHOSE PRINT TECH ARE TURNED ON.
C
C  ORIGINALLY CODED BY DEBBIE VAN DEMARK - 3/1/84
C
C----------------------------------------------------------------------
C
C  INPUT ARGUMENT:
C       MXTYPE -  NUMBER OF DATA TYPES ALLOWED
C       NUTYPE -  NUMBER OF DATA TYPES RETURNED FROM UDTRRS
C       NDTYPE - NUMBER OF DATA TYPES RETURNED FROM PPPDB
C        ARRAY - ARRAY WITH STATION INFO. FROM THE PPPDB
C         TYPE - ARRAY WITH DATA TYPES
C       IPRTYP - ARRAY WITH DATA PRINT CATEGORIES
C                 0 = NOT DEFINED
C                 1 = STAGE (STG) DATA
C                 2 = INSTANTANEOUS DISCHARGE (INQ) DATA
C                 3 = MEAN DISCHARGE (MNQ) DATA
C                 4 = RESERVOIR (RES) DATA
C                 5 = INSTANTANEOUS MISCELLANEOUS (INM) DATA
C                 6 = MEAN MISCELLANEOUS (MNM) DATA
C
C  OUTPUT ARGUMENT:
C       NPRTOB - NUMBER OF OBSERVED DATA TYPES TO BE DISPLAYED
C       NPRTTS - NUMBER OF TIME SERIES DATA TYPES TO BE DISPLAYED
C       PRNTOB - DATA TYPES TO HAVE OBSERVED DATA PRINTED
C       PRNTTS - DATA TYPES TO HAVE TIME SERIES DATA PRINTED
C       XPRTOB - DATA TYPES NOT TO HAVE OBSSERVED DATA PRINTED
C
C----------------------------------------------------------------------
C
      SUBROUTINE QPRNT (MXTYPE,NUTYPE,NDTYPE,ARRAY,TYPE,IPRTYP,NPRTTS,
     $   NPRTOB,PRNTOB,PRNTTS,PRSOB,NPSOB,XPRTOB)
C
      CHARACTER*8 OLDOPN
C
      CHARACTER*4 ARRAY(1),TYPE(1)
      CHARACTER*4 PRNTOB(1),PRNTTS(1),PRSOB(1),XPRTOB(1)
      DIMENSION IPRTYP(1)
C
      INCLUDE 'common/ionum'
      INCLUDE 'common/pudbug'
      INCLUDE 'common/qprint'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_rrs/RCS/qprnt.f,v $
     . $',                                                             '
     .$Id: qprnt.f,v 1.4 1999/07/06 15:57:30 page Exp $
     . $' /
C    ===================================================================
C
C
      IF (IPTRCE.GT.2) WRITE (IOPDBG,70)
C
      IBUG=IPBUG('QPNT')
C
      IOPNUM=-3
      CALL FSTWHR ('QPRNT   ',IOPNUM,OLDOPN,IOLDOP)
C
C  THE TRACE LEVEL FOR THIS ROUINTE IS 3
C
      IF (IBUG.GT.0) WRITE (IOPDBG,80) NDTYPE,NUTYPE
C
C  INITIALIZE THE PRINT DATA TYPE ARRAYS
      NPRTTS=0
      NPRTOB=0
      NPSOB=0
      DO 10 I=1,MXTYPE
         PRNTOB(I)=' '
         PRNTTS(I)=' '
         PRSOB(I)=' '
         XPRTOB(I)=' '
10       CONTINUE
C
C  CHECK THE PRINT TECHNIQUE VALUES AND FILL THE PRINT DATA TYPE ARRAYS
      DO 60 J=1,NDTYPE
         DO 50 I=1,NUTYPE
            IPRTY=IPRTYP(I)
            IF (IPRTY.EQ.1) THEN
               IF (ISTGTS.EQ.0.OR.TYPE(I).NE.ARRAY(15+J-1)) GO TO 30
               GO TO 20
               ENDIF
            IF (IPRTY.EQ.2) THEN
               IF (IINQTS.EQ.0.OR.TYPE(I).NE.ARRAY(15+J-1)) GO TO 30
               GO TO 20
               ENDIF
            IF (IPRTY.EQ.3) THEN
               IF (IMNQTS.EQ.0.OR.TYPE(I).NE.ARRAY(15+J-1)) GO TO 30
               GO TO 20
               ENDIF
            IF (IPRTY.EQ.4) THEN
               IF (IRESTS.EQ.0.OR.TYPE(I).NE.ARRAY(15+J-1)) GO TO 30
               GO TO 20
               ENDIF
            IF (IPRTY.EQ.5) THEN
               IF (IINMTS.EQ.0.OR.TYPE(I).NE.ARRAY(15+J-1)) GO TO 30
               GO TO 20
               ENDIF
            IF (IPRTY.EQ.6) THEN
               IF (IMNMTS.EQ.0.OR.TYPE(I).NE.ARRAY(15+J-1)) GO TO 30
               GO TO 20
               ENDIF
            WRITE (IPR,90) TYPE(I)
            CALL WARN
            GO TO 60
20          NPRTTS=NPRTTS+1
            PRNTTS(NPRTTS)=ARRAY(15+J-1)
30          IF (IPRTY.EQ.1) THEN
               IF (ISTGOB.EQ.0.OR.TYPE(I).NE.ARRAY(15+J-1)) GO TO 50
               GO TO 40
               ENDIF
            IF (IPRTY.EQ.2) THEN
               IF (IINQOB.EQ.0.OR.TYPE(I).NE.ARRAY(15+J-1)) GO TO 50
               GO TO 40
               ENDIF
            IF (IPRTY.EQ.3) THEN
               IF (IMNQOB.EQ.0.OR.TYPE(I).NE.ARRAY(15+J-1)) GO TO 50
               GO TO 40
               ENDIF
            IF (IPRTY.EQ.4) THEN
               IF (IRESOB.EQ.0.OR.TYPE(I).NE.ARRAY(15+J-1)) GO TO 50
               GO TO 40
               ENDIF
            IF (IPRTY.EQ.5) THEN
               IF (IINMOB.EQ.0.OR.TYPE(I).NE.ARRAY(15+J-1)) GO TO 50
               GO TO 40
               ENDIF
            IF (IPRTY.EQ.6) THEN
               IF (IMNMOB.EQ.0.OR.TYPE(I).NE.ARRAY(15+J-1)) GO TO 50
               GO TO 40
               ENDIF
            WRITE (IPR,90) TYPE(I)
            CALL WARN
            GO TO 50
40          NPSOB=NPSOB+1
            PRSOB(NPSOB)=ARRAY(15+J-1)
50          CONTINUE
60       CONTINUE
C
      IF (IBUG.GT.0) THEN
         IF (NPRTTS.EQ.0) THEN
            WRITE (IOPDBG,100)
            ELSE
               WRITE (IOPDBG,110)
               WRITE (IOPDBG,120) (PRNTTS(I),I=1,NPRTTS)
            ENDIF
         IF (NPRTOB.EQ.0) THEN
            WRITE (IOPDBG,130)
            ELSE
               WRITE (IOPDBG,140)
               WRITE (IOPDBG,120) (PRNTOB(I),I=1,NPRTOB)
            ENDIF
         IF (NPSOB.EQ.0) THEN
            WRITE (IOPDBG,150)
            ELSE
               WRITE (IOPDBG,160)
               WRITE (IOPDBG,120) (PRSOB(I),I=1,NPSOB)
            ENDIF
         ENDIF
C
      CALL FSTWHR (OLDOPN,IOLDOP,OLDOPN,IOLDOP)
C
      IF (IPTRCE.GT.2) WRITE (IOPDBG,170)
C
      RETURN
C
70    FORMAT (' *** ENTER QPRNT')
80    FORMAT (' IN QPRNT - NDTYPE=',I2,' NUTYPE=',I2)
90    FORMAT ('0**WARNING** DATA TYPE ',A4,' HAS AN INVALID PRINT ',
     $ 'CATEGORY OF ',I2,'. THE DATA TYPE WILL NOT BE DISPLAYED.')
100   FORMAT (' NO TIME SERIES DATA WILL BE DISPLAYED')
110   FORMAT (' THE DATA TYPES THAT WILL HAVE TIME SERIES DATA ',
     $ 'DISPLAYED ARE:')
120   FORMAT (' ',10(A4,2X))
130   FORMAT (' NO OBSERVED DATA WILL BE DISPLAYED')
140   FORMAT (' THE DATA TYPES THAT WILL HAVE OBSERVED DATA DISPLAYED ',
     $ 'IF DATA WAS RETURNED FROM THE PPDB ARE:')
150   FORMAT (' NO OBSERVED DATA WILL BE DISPLAYED SINGULARLY')
160   FORMAT (' THE DATA TYPES THAT WILL HAVE OBSERVED DATA DISPLAYED ',
     $ 'SINGULARLY IF DATA WAS RETURNED FROM THE PPDB ARE:')
170   FORMAT (' *** EXIT QPRNT')
c
      END
