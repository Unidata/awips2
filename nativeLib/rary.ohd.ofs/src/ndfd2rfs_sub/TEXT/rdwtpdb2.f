C23456---1---------2---------3---------4---------5---------6---------712
C$PRAGMA C (get_apps_defaults)
C$PRAGMA C (SETUPAPI)
C-----------------------------------------------------------------------
C
      SUBROUTINE RDWTPDB2(II,TSID,TSTYPE,TSDATIN,NVALS,
     &                   IYEAR,JULDAY,IHOUR,ISTAT)
C
C-----------------------------------------------------------------------
C
C  ROUTINE TO WRITE TIMES SERIES DATA TO THE PROCESSED DATA BASE.
C
C  ARGUMENT LIST:
C
C      NAME       TYPE  I/O   DIM   DESCRIPTION
C      ----       ----  ---   ---   -----------
C      II         I     I     1     Flag for print time infomation
C      TSID       A8    I     1     Time series identifier
C      TSTYPE     A4    I     1     Data Type code
C      TSDATIN    R     I     Nvals Time series input data
C      NVALS      I     I     1     Number of time series input data
C      IYEAR      I     I/O   1
C      JULDAY     I     I/O   1
C      IHOUR      I     I/O   1
C      IFPTR      I     I     1     location of future data in tsdatin array
C      ISTAT      I     O     1     Status indicator:
C                                     0=NORMAL RETURN
C                                     2=NUM TO BIG (TRUNCATED)
C                                     others=ERROR
C
      CHARACTER*4 TIMEZC /'Z   '/
      CHARACTER*4 TSTYPE
      CHARACTER*8 TSID,TSIDH,FTSID
      CHARACTER*20 TSDESC
      CHARACTER*200 CARD
      DIMENSION IHEAD(22)
      PARAMETER (LTSDAT=2000)
      DIMENSION TSDAT(LTSDAT)
      DIMENSION TSDATIN(*)
      PARAMETER (LWKBUF=2000)
      DIMENSION IWKBUF(LWKBUF)
      DIMENSION XBUF(LWKBUF)    !cfan
      DIMENSION IXBUF(4)

      CHARACTER*100 LINE
      CHARACTER*12  NDFDBGN,NDFDEND
      COMMON/TIMESTAMP/NDFDBGN,NDFDEND
      INTEGER NDFDEY,NDFDEM,NDFDED,NDFDEH
C
      INCLUDE 'uiox'
      INCLUDE 'udebug'
      INCLUDE 'upagex'
C LC include pdftbl to grab correct ICALL
      INCLUDE 'prdcommon/pdftbl'
      INCLUDE 'prdcommon/prdqpf'
      INCLUDE 'hclcommon/hdflts'
C
      INCLUDE 'common/x'
      INCLUDE 'scommon/sudbgx'
      INCLUDE 'scommon/suerrx'
      INCLUDE 'scommon/sworkx'
      INCLUDE 'scommon/suoptx'
      INCLUDE 'scommon/sugnlx'
C
      INTEGER     JTYPE,IYEAR,JULDAY,HOUR
      CHARACTER*4 JCTYPE
      EQUIVALENCE ( JTYPE,JCTYPE )
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ndfd2rfs_sub/RCS/rdwtpdb2.f,v $
     . $',                                                             '
     .$Id: rdwtpdb2.f,v 1.2 2004/11/02 20:20:12 xfan Exp $
     . $' /
C    ===================================================================
C

      CALL UPINIO()
C
C  READ DATA BASE CONTROL RECORDS
C
      CALL RPDBCI (ISTAT)
      IF (ISTAT.GT.0) GO TO 50
C
C  READ TIME SERIES HEADER
C
      LXBUF=1

      CALL WRITEMSGF(16384+128+80, ' RPRDH')

      CALL RPRDH (TSID,TSTYPE,LXBUF,IHEAD,NXBUF,XBUF,FTSID,ISTAT)

      CALL WRITEMSGF(24576+128+80, ' RPRDH , Find  '
     &               //TSID//' successfully!')

      IF (ISTAT.GT.0) GO TO 50
C
C  GET TIME STEP and UNITS
C
      ISTEP=IHEAD(2)
      IUNITS=IHEAD(11)
C
C  DETERMINE REGULAR DATA BEGINS : JHOUR1
C
      NHOUR=0
      CALL GETDTE (JDAY,HOUR,NBMO,NBDA,NBYR,NBHR,NTZC,IDLS,TIMEZC,
     *   IHEAD,NHOUR,TDAY)

       IF (II.EQ.1) THEN
       WRITE(CARD,'(I4.4,2(1h/,I2.2),1h-,I2.2)')
     *       NBYR,NBMO,NBDA,NBHR
       CALL WRITEMSGF(57344+128+0,
     *      ' PDB REGULAR MAT DATA begins at '//CARD(1:13))
       ENDIF
C
C  JHOUR0 : the beginning of NDFD data
C  JHOUR00: the ending of NDFD data
C
      JHOUR0=((IYEAR*365)+(IYEAR-1)/4+JULDAY-1)*24+IHOUR

      READ(NDFDEND,'(I4)')NDFDEY
      NDFDEY=NDFDEY-1900
      READ(NDFDEND,'(4x,I2)')NDFDEM
      READ(NDFDEND,'(6x,I2)')NDFDED
      READ(NDFDEND,'(8x,I2)')NDFDEH

      CALL CALDATE(JULDAY0,NDFDEM,NDFDED,NDFDEY,1)
      JHOUR00=((NDFDEY*365)+(NDFDEY-1)/4+JULDAY0-1)*24+NDFDEH

      JHOUR1=(JDAY-1)*24+HOUR

      IF(JHOUR0.LE.0)Then
         JHOUR0=JHOUR1
         JULDAY=TDAY
         IYEAR=NBYR-1900
         IHOUR=NBHR
      ENDIF

C
C  DETERMINE FUTURE DATA BEGINS : JHOUR2
C
      NHOUR=(IHEAD(7)-IHEAD(6))/IHEAD(3)*IHEAD(2)
      CALL GETDTE (JDAY,HOUR,NFMO,NFDA,NFYR,NFHR,NTZC,IDLS,TIMEZC,
     *   IHEAD,NHOUR,TDAY)

       IF (II.EQ.1) THEN
       WRITE(CARD,'(I4.4,2(1h/,I2.2),1h-,I2.2)')
     *       NFYR,NFMO,NFDA,NFHR
       CALL WRITEMSGF(57344+128+0,
     *      ' PDB FUTURE  MAT DATA begins at '//CARD(1:13))
       ENDIF
C
      JHOUR2=(JDAY-1)*24+HOUR

      IF (JHOUR00 .LT. JHOUR2) THEN
        WRITE(LINE,1101)
1101    FORMAT(40H There is no NDFD data for PDB MAT data!)
        CALL WRITEMSGF(57344+384+0,LINE)
        WRITE(LINE,1102)NDFDEND(1:4),NDFDEND(5:6),NDFDEND(7:8),
     &                  NDFDEND(9:10)
1102    FORMAT(33H NDFD Ending Date              = ,
     &  A4,1H/,A2,1H/,A2,1H-,A2)
        CALL WRITEMSGF(57344+384+0,LINE)
        WRITE(LINE,1103) CARD(1:13)
1103    FORMAT(33H PDB Future MAT Beginning Date = ,A13)
        CALL WRITEMSGF(57344+384+0,LINE)
        STOP
      ENDIF

C  DETERMINE DATE DATA ENDS : JHOUR3

      NHOUR=IHEAD(5)/IHEAD(3)*IHEAD(2)
      CALL GETDTE (JDAY,HOUR,NEMO,NEDA,NEYR,NEHR,NTZC,IDLS,TIMEZC,
     *   IHEAD,NHOUR,TDAY)

       IF (II.EQ.1) THEN
       WRITE(CARD,'(I4.4,2(1h/,I2.2),1h-,I2.2)')
     *       NEYR,NEMO,NEDA,NEHR
       CALL WRITEMSGF(57344+128+0,
     *      ' PDB FUTURE  MAT DATA ends   at '//CARD(1:13))
       ENDIF

      JHOUR3=(JDAY-1)*24+HOUR

      IF (JHOUR0 .GE. JHOUR3) THEN
        WRITE(LINE,1104)
1104    FORMAT(40H There is no NDFD data for PDB MAT data!)
        CALL WRITEMSGF(57344+384+0,LINE)
        WRITE(LINE,1105)NDFDBGN(1:4),NDFDBGN(5:6),NDFDBGN(7:8),
     &                  NDFDBGN(9:10)
1105    FORMAT(30H NDFD Beginning Date        = ,
     &  A4,1H/,A2,1H/,A2,1H-,A2)
        CALL WRITEMSGF(57344+384+0,LINE)
        WRITE(LINE,1106) CARD(1:13)
1106    FORMAT(30H PDB Future MAT Ending Date = ,A13)
        CALL WRITEMSGF(57344+384+0,LINE)
        STOP
      ENDIF
C
C  SEARCH THE DATA TYPE DIRECTORY FOR A DATA TYPE

      JCTYPE = TSTYPE
      CALL PFDTYP (JTYPE,INDXD)

C
C  GET UNIT NUMBER FOR DATA TYPE

      IUNIT=DATFIL(2,INDXD)

C
C  CHANGE TO FORCE ICALL TO BE CORRECT

      ICALL=DATFIL(11,INDXD)

C
C  FIND THE TIME SERIES

      CALL PSERCH (TSID,TSTYPE,IFREE,IXREC,IXBUF)

C
C  READ TIME SERIES DATA

      JHOUR=((IYEAR*365)+(IYEAR-1)/4+JULDAY-1)*24+IHOUR
      NUMPD=IHEAD(5)/IHEAD(3)
      RMISS=-999.
      IFPTR=0

      CALL WRITEMSGF(16384+128+80, ' RPRDD')
      IFUT=0
      CALL RPRDDT (TSID,TSTYPE,JHOUR,ISTEP,NUMPD,IUNITS,RMISS,
     *   LTSDAT,TSDAT,IFPTR,LWKBUF,IWKBUF,IFUT,ISTAT)

      IF (ISTAT.GT.0.AND.ISTAT.NE.2) GO TO 50

      CALL WRITEMSGF(24576+128+80, ' RPRDD , Read  TS '
     &               //TSID//' successfully!')
      IF (IXREC.EQ.0)THEN
          ISTAT=9
          GO TO 50
      ENDIF

      IREC=IXBUF(4)

      DO I=1,22
          IWKBUF(I)=IHEAD(I)
      ENDDO

C  CHECK IF TSDATIN(I) IS MISSING, IF IT IS MISSING, DON'T OVERWRITE PDB

      DO I=1, NVALS
      IF(TSDATIN(I) .LT. -998.) GOTO 50
      ENDDO

C  FIND THE RELATIVE POSITION OF FUTURE DATA IN THE IWKBUF ARRAY

      IFRSTW=IWKBUF(6)+(JHOUR0-JHOUR1)/IWKBUF(2)

      CALL WRITEMSGF(16384+128+80, ' WTSRCD')

C  NEVER OVERWRITE OBSERVATION DATA, JUST OVERWRITE FUTURE DATA

C  WRITE TIME SERIES DATA

      IF (IFRSTW .LT. IWKBUF(7)) THEN
        CALL UMEMOV (TSDATIN(IWKBUF(7)-IFRSTW+1),IWKBUF(IWKBUF(7)),
     &               NVALS-IWKBUF(7)+IFRSTW)

        WRITE(CARD,'(I5,2X,A8,2X,I4,1h/,I2.2,1h/,I2.2,1h-,I2.2,
     &                     3H - ,A4,1h/,A2,1h/,A2,1h-,A2)')
     &  II,TSID,NFYR,NFMO,NFDA,NFHR,
     &  NDFDEND(1:4),NDFDEND(5:6),NDFDEND(7:8),NDFDEND(9:10)
        CALL WRITEMSGF(57344+128+50, CARD(1:50))

        IF (CARD(18:30) .LT. CARD(34:46)) THEN
        WRITE(CARD,'(35F4.0)')
     & (TSDATIN(JJ),JJ=IWKBUF(7)-IFRSTW+1, NVALS)
        CALL WRITEMSGF(57344+128+50, CARD(1:150))
        ELSE
        CALL WRITEMSGF(57344+128+50,
     &  ' There is no data written to database!')
        ENDIF

        CALL WTSRCD (IREC,IUNIT,IWKBUF,ISTAT)
        IF (ISTAT.GT.0) GO TO 50
      ELSE
        CALL CALDATE(JULDAY,IMB,IDB,1900+IYEAR,0)
        CALL UMEMOV (TSDATIN,IWKBUF(IFRSTW),NVALS)

        WRITE(CARD,'(I5,2X,A8,2X,I4,1h/,I2.2,1h/,I2.2,1h-,I2.2,
     &                     3H - ,A4,1h/,A2,1h/,A2,1h-,A2)')
     &  II,TSID,1900+IYEAR,IMB,IDB,IHOUR,
     &  NDFDEND(1:4),NDFDEND(5:6),NDFDEND(7:8),NDFDEND(9:10)
        CALL WRITEMSGF(57344+128+50, CARD(1:50))
        WRITE(CARD,'(35F4.0)')
     & (TSDATIN(JJ),JJ=1,NVALS)
        CALL WRITEMSGF(57344+128+50, CARD(1:150))

        CALL WTSRCD (IREC,IUNIT,IWKBUF,ISTAT)
        IF (ISTAT.GT.0) GO TO 50
      ENDIF

      CALL WRITEMSGF(24576+128+80, ' WTSRCD, Write TS '
     &                //TSID//' successfully!')

C
C  WRITE CONTROL RECORDS TO FILE
      CALL WPDBCO (ISTAT)
      IF (ISTAT.GT.0) GO TO 50
C
C  CLOSE FILES
      CALL UCLOSL
C
C
50    CONTINUE
      RETURN
81    FORMAT (' ',
     *   'DATE OF FIRST DATA VALUE = ',
     *      I2.2,'/',I2.2,'/',I4,'-',I2.2)
82    FORMAT (' ',
     *   'DATE OF LAST DATA VALUE = ',
     *      I2.2,'/',I2.2,'/',I4,'-',I2.2)
      END
C-------------------------------------------------------------------------
      SUBROUTINE IS_LEAP_YEAR(IYR,IYESNO)
C-------------------------------------------------------------------------

      I=MOD(IYR,4)
      J=MOD(IYR,100)
      K=MOD(IYR,400)
      IF (((I.EQ.0).AND.(J.NE.0)).OR.(K.EQ.0))THEN
         IYESNO=1
      ELSE
         IYESNO=0
      ENDIF
      END
C-------------------------------------------------------------------------
      SUBROUTINE GETDTE (JDAY,IHOUR,NEMO,NEDA,NEYR,NEHR,NTZC,
     *IDLS,TIMEZC,IHEAD,NHOUR,TTDAY)
C-------------------------------------------------------------------------
      DIMENSION IHEAD(1)
      NHOPDB=0
C
      JHOUR=IHEAD(14)+NHOUR-NHOPDB
      JDAY=JHOUR/24
      IHOUR=JHOUR-JDAY*24
      JDAY=JDAY+1
      CALL MDYH2 (JDAY,IHOUR,NEMO,NEDA,NEYR,NEHR,NTZC,IDLS,TIMEZC)
      TTDAY=0
      IF(NEMO.NE.1)THEN
         DO I=1,NEMO-1
           CALL DAYS_OF_MONTH(I,NEYR,IDAY)
           TTDAY=TTDAY+IDAY
         ENDDO
      ENDIF
      TTDAY=TTDAY+NEDA
      END
C-------------------------------------------------------------------------
      SUBROUTINE DAYS_OF_MONTH(IM,IY,IDAY)
C-------------------------------------------------------------------------
      INTEGER NUM_DAYS(12)

      DATA NUM_DAYS/31,28,31,30,31,30,31,31,30,31,30,31/
      ILEAP=0
      IF( IM.LE.0.OR.IM.GT.12)THEN
       PRINT*,'MONTH [1-12] AND YEAR [YYYY] NEED TO BE GIVEN CORRECTLY.'
       PRINT*,'MONTH , YEAR',IM,IY
       STOP
      END IF
      IF(IM.EQ.2)THEN
         CALL IS_LEAP_YEAR(IY,IYESNO)
         IF(IYESNO.ne.0) ILEAP=1
      ENDIF
      IDAY=ILEAP+NUM_DAYS(IM)
      END
C-------------------------------------------------------------------------
      SUBROUTINE CALDATE(JULDAY,IMONTH,IDAY,IYEAR,IFLAG)
C-------------------------------------------------------------------------
C
C     CALCULATE JULDAY FROM IMONTH, IDAY OF IYEAR WHEN IFLAG=1
C     CALCULATE IMONTH, IDAY FROM JULDAY OF IYEAR WHEN IFLAG=0
C
      DIMENSION IDOY1(13),IDOY2(13)
      DATA IDOY1/0,31,60,91,121,152,182,213,244,274,305,335,366/
      DATA IDOY2/0,31,59,90,120,151,181,212,243,273,304,334,365/

      CALL IS_LEAP_YEAR(IYEAR,IYESNO)

      IF (IYESNO .EQ. 1) THEN
C  for leap year
        IF (IFLAG.EQ.0) THEN
         DO I=1,12
          IF (JULDAY .LE. IDOY1(I+1)) GO TO 20
         ENDDO
  20     IDAY=JULDAY - IDOY1(I)
         IMONTH=I
        ELSE
         JULDAY=IDOY1(IMONTH)+IDAY
        ENDIF
      ELSE
C  for non-leap year
        IF (IFLAG.EQ.0) THEN
         DO I=1,12
          IF (JULDAY .LE. IDOY2(I+1)) GO TO 40
         ENDDO
  40     IDAY=JULDAY - IDOY2(I)
         IMONTH=I
        ELSE
         JULDAY=IDOY2(IMONTH)+IDAY
        ENDIF
      ENDIF
      END

