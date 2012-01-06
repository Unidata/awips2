C MODULE SSPRD2
C-----------------------------------------------------------------------
C
C  ROUTINE TO PRINT LEVEL 2 STATUS FOR THE PROCESSED DATA BASE.
C
      SUBROUTINE SSPRD2 (NUMTS,LUNIT,TSID,TSTYPE,IREC,
     *   IFORM,IFORMO,NCOUNT,IARRAY,LWKBUF,IWKBUF,
     *   LARRAY,ARRAY,ISTAT)
C
      CHARACTER*4 TSTYPE
      CHARACTER*7 CNTTYP
      CHARACTER*8 TSID
C
      DIMENSION ARRAY(LARRAY),IWKBUF(LWKBUF),IARRAY(3,1)
      DIMENSION IFARR(3)
C
      INCLUDE 'uiox'
      INCLUDE 'udatas'
      INCLUDE 'scommon/sudbgx'
      INCLUDE 'prdcommon/pdftbl'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_s/RCS/ssprd2.f,v $
     . $',                                                             '
     .$Id: ssprd2.f,v 1.6 2003/08/21 07:45:32 scv Exp $
     . $' /
C    ===================================================================
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'ENTER SSPRD2'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG('STAT')
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,20) LUNIT,TSID,TSTYPE,IREC,LWKBUF
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      ISTAT=0
C
C  GET THE TIME SERIES
      CALL RTSRCD (IREC,TSID,TSTYPE,LUNIT,LWKBUF,IWKBUF,IERR)
      IF (IERR.NE.0) THEN
         WRITE (LP,30) IERR,TSID,TSTYPE,IREC
         CALL SUERRS (LP,2,-1)
         ISTAT=1
         GO TO 10
         ENDIF
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,40) (IWKBUF(I),I=1,22)
         CALL SULINE (IOSDBG,1)
         WRITE (IOSDBG,50) (IWKBUF(I),I=1,22)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  GET TIME SERIES IDENTIFIER
      CALL UMEMOV (IWKBUF(8),TSID,2)
C
C  CHECK IF DELETED
      IF (TSID.EQ.'DELETED') GO TO 10
C
C  CHECK DATA TYPE
      ITYPE=IWKBUF(10)
      CALL PFDTYP (IWKBUF(10),INDX)
      IF (DATFIL(7,INDX).LT.0) THEN
         INDXR=-DATFIL(7,INDX)
         ITYPE=DATFIL(1,INDXR)
         ENDIF
C
C  SET THE DATES
      IMO=0
      IDAY=0
      IYR=0
      IHR=0
      IMOF=0
      IDAYF=0
      IYRF=0
      IHRF=0
      IF (IWKBUF(14).NE.0) THEN
         IF (DATFIL(7,INDX).NE.0) THEN
            CALL DDGHC2 (IWKBUF(14),IYR,IMO,IDAY,IHR)
            ELSE
C           MIXED RECORD
               IF (IWKBUF(6).EQ.IWKBUF(7)) THEN
                  CALL DDGHC2 (IWKBUF(14),IYRF,IMOF,IDAYF,IHRF)
                  ELSE
                     CALL DDGHC2 (IWKBUF(14),IYR,IMO,IDAY,IHR)
                     IF (IWKBUF(7) .NE. 0) THEN
                        JHOUR=IWKBUF(14)+((IWKBUF(7)-IWKBUF(6))/
     *                        IWKBUF(3))*IWKBUF(2)
                        CALL DDGHC2 (JHOUR,IYRF,IMOF,IDAYF,IHRF)
                        ENDIF
                  ENDIF
            ENDIF
         ENDIF
C
      CALL UMEMOV (IWKBUF(12),RLAT,1)
      CALL UMEMOV (IWKBUF(13),RLON,1)
C
      IHEADR=0
C
C  CHECK IF FIRST DATA TYPE
      IF (IFORMO.EQ.-999) THEN
         IHEADR=1
         ENDIF
C
C  CHECK IF FIRST TIME SERIES OF DATA TYPE
      IF (NCOUNT.EQ.0) THEN
         IHEADR=1
         ENDIF
C
C  CHECK IF AT TOP OF NEW PAGE
      NSNWPG=ISNWPG(LP)
      IF (NSNWPG.EQ.1) THEN
         IHEADR=1
         IFORMO=-999
         ENDIF
C
C  CHECK IF NEED TO PRINT HEADER
      IF (IHEADR.EQ.1) THEN
         IF (IFORM.NE.IFORMO) THEN
C        CHECK NUMBER OF LINES LEFT ON PAGE
            NLINES=6
            NSLEFT=ISLEFT(NLINES)
            IF (NSLEFT.EQ.1) THEN
               CALL SUPAGE
               ENDIF
            IF (IFORM.EQ.0) THEN
               WRITE (LP,80)
               CALL SULINE (LP,5)
               ELSE
                  WRITE (LP,60)
                  CALL SULINE (LP,5)
               ENDIF
            ENDIF
         ENDIF
      IFORMO=IFORM
C
      NCOUNT=NCOUNT+1
C
      IF (DATFIL(7,INDX).EQ.0) THEN
C     MIXED DATA TYPE
         CNTTYP='MIXED'
         JYR=IYR-(IYR/100)*100
         JYRF=IYRF-(IYRF/100)*100
         WRITE (LP,90) NCOUNT,(IWKBUF(I),I=8,9),ITYPE,CNTTYP,
     *     IWKBUF(11),(IWKBUF(J),J=2,5),
     *     IMO,IDAY,JYR,IHR,IMOF,IDAYF,JYRF,IHRF,
     *     RLAT,RLON,(IWKBUF(K),K=18,22)
         CALL SULINE (LP,1)
         ELSE
C        NON-MIXED TYPE
            CALL PFDFID (IWKBUF(15),IWKBUF(10),IFARR,IERR)
            CNTTYP='REGULAR'
            IF (DATFIL(7,INDX).LT.0) CNTTYP='FUTURE'
            JYR=IYR-(IYR/100)*100
            WRITE (LP,70) NCOUNT,(IWKBUF(I),I=8,9),ITYPE,CNTTYP,
     *       IWKBUF(11),(IWKBUF(J),J=2,5),
     *       IMO,IDAY,JYR,IHR,
     *       RLAT,RLON,(IWKBUF(K),K=18,22),IFARR
           CALL SULINE (LP,1)
         ENDIF
C
10    IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'EXIT SSPRD2 : ISTAT=',ISTAT
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
20    FORMAT (' LUNIT=',I2,3X,'TSID=',A,3X,'TSTYPE=',A,3X,
     *   'IREC=',I5,3X,'LWKBUF=',I5)
30    FORMAT ('0*** ERROR - IN SSPRD2 - ERROR ENCOUNTERED IN RTSRCD. ',
     *   'STATUS CODE=',I2,3X,
     *   'IDENTIFIER=',A,3X,
     *   'DATA TYPE=',A,3X,
     *   'RECORD=',I5)
40    FORMAT (' IWKBUF(1-22) IN I4: ',22(I4,1X))
50    FORMAT (' IWKBUF(1-22) IN A4: ',22(A4,1X))
60    FORMAT ('0',5X,
     *       'TIME    ',2X,
     *       '    ',2X,
     *       '       ',2X,
     *       '     ',2X,
     *       '    ',2X,
     *       'VALUES',2X,
     *       'MAXIMUM',2X,
     *       'ACTUAL',2X,
     *       'DATE OF' /
     *   ' ',5X,
     *       'SERIES  ',2X,
     *       'DATA',2X,
     *       '       ',2X,
     *       '     ',2X,
     *       'TIME',2X,
     *       '/TIME ',2X,
     *       'DATA   ',2X,
     *       'DATA  ',2X,
     *       'FIRST DATA  ',2X,
     *       '     ',2X,
     *       '      ',2X,
     *       '                    ',2X,
     *       '-- FUTURE  --' /
     *   ' ',5X,
     *       'ID      ',2X,
     *       'TYPE',2X,
     *       'TS TYPE',2X,
     *       'UNITS',2X,
     *       'STEP',2X,
     *       'STEP  ',2X,
     *       'VALUES ' ,2X,
     *       'VALUES' ,2X,
     *       'VALUE       ',2X,
     *       'LAT  ',2X,
     *       'LON   ',2X,
     *       'DESCRIPTION         ',2X,
     *       'TS ID   ',1X,'TYPE' /
     *   ' ',5X,
     *       8('-'),2X,
     *       4('-'),2X,
     *       7('-'),2X,
     *       5('-'),2X,
     *       4('-'),2X,
     *       6('-'),2X,
     *       7('-'),2X,
     *       6('-'),2X,
     *       12('-'),2X,
     *       5('-'),2X,
     *       6('-'),2X,
     *       20('-'),2X,
     *       8('-'),1X,4('-'))
70    FORMAT (' ',I4,1X,
     *   2A4,2X,
     *   A4,2X,
     *   A7,2X,
     *   A4,1X,2X,
     *   I4.2,2X,
     *   I6,2X,
     *   I7,2X,
     *   I6,2X,
     *   3(I2.2,'/'),I2.2,'Z',2X,
     *   F5.2,2X,
     *   F6.2,2X,
     *   5A4,2X,
     *   2A4,1X,A4)
80    FORMAT ('0',5X,
     *       'TIME    ',2X,
     *       '    ',2X,
     *       '       ',2X,
     *       '     ',2X,
     *       '    ',2X,
     *       'VALUES',2X,
     *       'MAXIMUM',2X,
     *       'ACTUAL',2X,
     *       'DATE OF     ',2X,
     *       'DATE OF     ' /
     *   ' ',5X,
     *       'SERIES  ',2X,
     *       'DATA',2X,
     *       '       ',2X,
     *       '     ',2X,
     *       'TIME',2X,
     *       '/TIME ',2X,
     *       'DATA   ',2X,
     *       'DATA  ',2X,
     *       'FIRST DATA  ',2X,
     *       'FIRST DATA  ' /
     *   ' ',5X,
     *       'ID      ',2X,
     *       'TYPE',2X,
     *       'TS TYPE',2X,
     *       'UNITS',2X,
     *       'STEP',2X,
     *       'STEP  ',2X,
     *       'VALUES ' ,2X,
     *       'VALUES' ,2X,
     *       'REGULAR     ',2X,
     *       'FUTURE      ',2X,
     *       'LAT  ',2X,
     *       'LON   ',2X,
     *       'DESCRIPTION' /
     *   ' ',5X,
     *       8('-'),2X,
     *       4('-'),2X,
     *       7('-'),2X,
     *       5('-'),2X,
     *       4('-'),2X,
     *       6('-'),2X,
     *       7('-'),2X,
     *       6('-'),2X,
     *       12('-'),2X,
     *       12('-'),2X,
     *       5('-'),2X,
     *       6('-'),2X,
     *       20('-'))
90    FORMAT (' ',I4,1X,
     *   2A4,2X,
     *   A4,2X,
     *   A7,2X,
     *   A4,1X,2X,
     *   I4.2,2X,
     *   I6,2X,
     *   I7,2X,
     *   I6,2X,
     *   3(I2.2,'/'),I2.2,'Z',2X,
     *   3(I2.2,'/'),I2.2,'Z',2X,
     *   F5.2,2X,
     *   F6.2,2X,
     *   5A4)

      END
