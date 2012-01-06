C MEMBER PDSRST
C  (from old member PDBDMPST)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 11/14/94.11:11:22 BY $WC20SV
C
C @PROCESS LVL(77)
C
       SUBROUTINE PDSRST (IUNFLG)
C
C          ROUTINE:  PDSRST
C
C             VERSION:  1.0.0
C
C                DATE:  5-16-83
C
C              AUTHOR:  JANINE FRANZOI
C                       DATA SCIENCES INC
C
C***********************************************************************
C
C          DESCRIPTION:
C
C    THIS ROUTINE DISPLAYS THE STRANGER STATION STATISTICS
C    STORED ON THE PREPROCESSOR DATA BASE.
C
C***********************************************************************
C
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C         IUNFLG    I     I    1     PRECIPITATION UNITS FLAG
C                                      0=ENGLISH
C                                      1=METRIC
C
C***********************************************************************
C
C          COMMON:
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'hclcommon/hdflts'
      INCLUDE 'pdbcommon/pddtdr'
      INCLUDE 'pdbcommon/pdunts'
C
C***********************************************************************
C
C          DIMENSION AND TYPE DECLARATIONS:
C
      CHARACTER*4 DUNITS
      DIMENSION FLON(2),FLAT(2),X(2),Y(2)
C
      INTEGER*2 ISSTAT(32)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppdutil/RCS/pdsrst.f,v $
     . $',                                                             '
     .$Id: pdsrst.f,v 1.2 1997/06/25 13:16:39 page Exp $
     . $' /
C    ===================================================================
C
C
C***********************************************************************
C
C          DATA:
C
C***********************************************************************
C
C
C
      IF (IPDTR.GT.0) WRITE (IOGDB,20)
C
C  FIND DATA TYPE IN DIRECTORY
      IX=IPDCKD('PPSR')
      IF (IX.EQ.0) THEN
         WRITE (LP,30)
         GO TO 10
         ENDIF
      IDF=IDDTDR(4,IX)
      DUNITS='IN'
C
C  READ STATISTICS RECORD FROM FILE
      IREC=IDDTDR(14,IX)
      IF (IREC.LE.0) THEN
         WRITE (LP,40)
         GO TO 10
         ENDIF
      CALL UREADT (KPDDDF(IDF),IREC,ISSTAT,IERR)
      IF (IERR.NE.0) THEN
          WRITE (LP,50)
          GO TO 10
          ENDIF
      IF (ISSTAT(1).LT.0) THEN
          WRITE (LP,60)
          GO TO 10
          ENDIF
C
C  CHECK UNITS FLAG
      IF (IUNFLG.EQ.1) DUNITS='MM'
C
      WRITE (LP,70) DUNITS
C
      WRITE (LP,80)
C
C  CONVERT JULIAN DAY TO DATE FILE WAS REINITIALIZED
      CALL UMEMOV (ISSTAT(15),JDAY,1)
      CALL MDYH2 (JDAY,0,IMO,IDAY,IYR,IHR,ITZ,IDSAV,TIME(3))
      IYR=MOD(IYR,100)
C
      WRITE (LP,90) IMO,IDAY,IYR,ISSTAT(13),ISSTAT(14),
     *   (ISSTAT(I),I=1,12)
C
C  CONVERT DATES FOR LARGEST REPORT
      CALL UMEMOV (ISSTAT(20),LGDAY,1)
      CALL MDYH2 (LGDAY,0,LGMO,LGDA,LGYR,LGHR,ITZ,IDSAV,TIME(3))
      LGYR=MOD(LGYR,100)
C
C  CONVERT DATES FOR 2ND LARGEST REPORTS
      IG2DAY=0
      IF (ISSTAT(25).NE.-9999) THEN
         CALL UMEMOV (ISSTAT(25),LG2DAY,1)
         IF (LG2DAY.GT.0) THEN
            CALL MDYH2 (LG2DAY,0,LG2MO,LG2DA,LG2YR,LG2HR,ITZ,IDSAV,
     *         TIME(3))
            LG2YR=MOD(LG2YR,100)
            IG2DAY=1
            ENDIF
         ENDIF
C
C  CONVERT POLAR STEROGRAPHIC COORDINATES TO LAT/LON
      NUM=1
      Y(NUM)=ISSTAT(22)
      Y(NUM)=Y(NUM)/10
      X(NUM)=ISSTAT(23)
      X(NUM)=X(NUM)/10
      IF (IG2DAY.EQ.1) THEN
         NUM=2
         Y(NUM)=ISSTAT(27)
         Y(NUM)=Y(NUM)/10
         X(NUM)=ISSTAT(28)
         X(NUM)=X(NUM)/10
         ENDIF
      IBLLGD=0
      CALL SBLLGD (FLON,FLAT,NUM,X,Y,IBLLGD,IERR)
      IF (IERR.NE.0) GO TO 10
C
C  PRINT DATA
      RLG=ISSTAT(19)
      RLG=RLG/100
      IF (NUM.EQ.1) THEN
         WRITE (LP,110) '0'
         WRITE (LP,120)
         WRITE (LP,130)
         WRITE (LP,140) RLG,LGMO,LGDA,LGYR,FLAT(1),FLON(1)
         ENDIF
      IF (NUM.EQ.2) THEN
         RLG2=ISSTAT(24)
         RLG2=RLG2/100
         WRITE (LP,100) '0',' '
         WRITE (LP,110) ' ',' '
         WRITE (LP,120) ' '
         WRITE (LP,130) ' '
         WRITE (LP,140) RLG,LGMO,LGDA,LGYR,FLAT(1),FLON(1),
     *                  RLG2,LG2MO,LG2DA,LG2YR,FLAT(2),FLON(2)
         ENDIF
C
10    IF (IPDTR.GT.0) WRITE (IOGDB,150)
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
20    FORMAT (' *** ENTER PDSRST')
30    FORMAT ('0**NOTE** DATA TYPE PPSR NOT FOUND ',
     *   'IN THE PREPROCESSOR DATA BASE.')
40    FORMAT ('0**ERROR** NO SPACE IS ALLOCATED FOR PPSR DATA ',
     *   'IN THE PREPROCESSOR DATA BASE.')
50    FORMAT ('0**ERROR** READ ERROR FOR PPSR STATISTICS RECORD.')
60    FORMAT ('0**NOTE** NUMBER OF NEW REPORTS ENTERED IN THE LAST ',
     *   '12 MONTHS IS ZERO.')
70    FORMAT ('0*** STATISTICS FOR STRANGER REPORTS ***',3X,
     *   'DATA UNITS = ',A)
80    FORMAT ('0',10X,'MONTH OF' /
     *   1X,'DATE    ',2X,'MOST RECENT',2X,
     *     '- TOTAL NUMBER OF REPORTS  FOR EACH OF THE LAST 12 MONTHS -'
     *      /
     *   1X,'RESET   ',2X,'REPORT     ',2X,
     *      'JAN ',1X,'FEB ',1X,'MAR ',1X,'APR ',1X,'MAY ',1X,'JUN ',1X,
     *      'JUL ',1X,'AUG ',1X,'SEP ',1X,'OCT ',1X,'NOV ',1X,'DEC ' /
     *   1X,8('-'),2X,11('-'),2X,
     *      12(4('-'),1X))
90    FORMAT (' ',I2.2,'/',I2.2,'/',I2.2,4X,I2.2,'/',I2.2,6X,12(I4,1X))
100   FORMAT (A,T37,A,'SECOND ',2X,'DATE OF ',2X,'LAT/LONG')
110   FORMAT (A,'LARGEST',2X,'DATE OF ',2X,'LAT/LONG',:,
     *   T37,A,'LARGEST',2X,'SECOND  ',2X,'OF SECOND')
120   FORMAT (1X,'REPORT ',2X,'LARGEST ',2X,'OF LARGEST',:,
     *   T37,A,'REPORT ',2X,'LARGEST ',2X,'LARGEST')
130   FORMAT (1X,2(7('-'),2X,8('-'),2X,12('-'),4X,:,A))
140   FORMAT (' ',2(F7.2,2X,2(I2.2,'/'),I2.2,2X,F5.2,'/',F6.2,:,5X))
150   FORMAT (' *** EXIT PDSRST')
C
      END
