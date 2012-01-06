C MODULE VDSPL2
C-----------------------------------------------------------------------
C
C  THIS ROUTINE PRINTS THE MAPE TIME SERIES DATA.
C
      SUBROUTINE VDSPL2 (PECOMP,NAREA,MXDDIM,PEPARM,MXPDIM,
     *   ESTSYM,IPLSTD,LTSDATA,TSDATA)
C
      CHARACTER*4 UNITS
      PARAMETER (MDASHES=10)
      CHARACTER*6 DASHES(MDASHES)
      CHARACTER*8 RTNNAM,OLDOPN
C
      DIMENSION PECOMP(MXDDIM),PEPARM(MXPDIM),ESTSYM(30),TSDATA(LTSDATA)
      DIMENSION KDAY(30),KMON(30)
C
      INCLUDE 'common/ionum'
      INCLUDE 'common/pudbug'
      COMMON /FCTIM2/ INPTZC,NHOPDB,NHOCAL,NHROFF(8)
      COMMON /VFIXD /LRY,MRY,NDAYS,NDAYOB,LARFIL,LPDFIL
      COMMON /VTIME /JDAY,IYRS,MOS,JDAMOS,IHRO,IHRS,JDAMO,JDAYYR
      COMMON /VMODAS /LSTDAM(13)
      COMMON /VSTUP/ PTYPE,DTYPE,METRIC,CPETIM(2),UTYPE,BTYPE,EST(3)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_mape/RCS/vdspl2.f,v $
     . $',                                                             '
     .$Id: vdspl2.f,v 1.3 2000/07/21 19:16:46 page Exp $
     . $' /
C    ===================================================================
C
C
      RTNNAM='VDSPL2'
C
      IF (IPTRCE.GT.0) WRITE (IOPDBG,*) 'ENTER ',RTNNAM
C
      IBUG=IPBUG('VDS2')
C
      IOPNUM=-1
      CALL FSTWHR (RTNNAM,IOPNUM,OLDOPN,IOLDOP)
C
      IF (IBUG.GT.0) WRITE (IOPDBG,*) 'NDAYS=',NDAYS,' NDAYOB=',NDAYOB,
     *   ' JDAY=',JDAY
C
C  CHECK IF THE PRINT LAST OBSERVED DAY ONLY OPTION SPECIFIED
      LSTPNT=1
      IF (IPLSTD.EQ.1) LSTPNT=NDAYOB
C
C  GET BEGINNING AND ENDING DATES
      IBHRO=0
      CALL MDYH2 (JDAY,IBHRO,IBMO,IBDA,IBYR,IBHR,XD1,XD2,INPTZC)
      JDAYL=JDAY+NDAYS-1
      IEHRO=IBHRO
      CALL MDYH2 (JDAYL,IEHRO,IEMO,IEDA,IEYR,IEHR,XD1,XD1,INPTZC)
C
C  CHECK IF FIRST TIME SERIES
      IF (NAREA.EQ.1) THEN
         IF (METRIC.EQ.0) THEN
            UNITS='IN'
            ELSE
               UNITS='MM'
            ENDIF
         WRITE (IPR,10) IBMO,IBDA,IBYR,IEMO,IEDA,IEYR,UNITS
10    FORMAT (
     * '0',10X,
     *    'DAILY MEAN AREAL POTENTIAL EVAPORATION TIME SERIES DATA ',
     *    'FOR ',I2.2,'/',I2.2,'/',I4,' THRU ',I2.2,'/',I2.2,'/',I4,3X,
     *    '(UNITS=',A,')')
         WRITE (IPR,20)
20    FORMAT (
     * '0',30X,'B = AN ESTIMATED VALUES THAT IS BLENDED TO THE ',
     *     'CLIMATIC MEAN' /
     * ' ',30X,'M = THE VALUE IS ESTIMATED FROM THE CLIMATIC MEAN ',
     *     'WITH NO BLENDING')
         ENDIF
C
C  CHECK IF NEED TO CONVERT DATA
      IF (METRIC.EQ.0) THEN
         DO 80 I=LSTPNT,NDAYS 
            ICONV=1
            NVAL=1
            CALL UDUCNV ('MM  ','IN  ',ICONV,NVAL,TSDATA(I),TSDATA(I),
     *         IERR)
80          CONTINUE
            ENDIF
C
C  CHECK IF CAN PRINT TABLE
      IF (NDAYS.LE.MDASHES) THEN
         IF (NAREA.EQ.1) THEN
C        SET DAY NUMBERS
            IMO=IBMO
            IDA=IBDA
            IYR=IBYR
            DO 30 I=LSTPNT,NDAYS
               KMON(I)=IMO
               KDAY(I)=IDA
	       IDA=IDA+1
               MDAYS=LSTDAM(IMO)
               IF (IMO.EQ.2.AND.IYR/4*4.EQ.IYR) MDAYS=LSTDAM(13)
	       IF (IDA.GT.MDAYS) THEN
	          IDA=1
	          IMO=IMO+1
	          IF (IMO.GT.12) THEN
	             IMO=1
	             IYR=IYR+1
	             ENDIF
	          ENDIF
30             CONTINUE
            WRITE (IPR,40)
40    FORMAT ('0',T35,'DAY')
            WRITE (IPR,50) (KMON(I),KDAY(I),I=LSTPNT,NDAYS)
50    FORMAT (' ','AREA ID ',2X,'DESCRIPTION',T35,
     *   15(1X,I2.2,'/',I2.2,1X))
            DO 60 I=1,MDASHES
               CALL UREPET ('-',DASHES(I),LEN(DASHES(I)))
60             CONTINUE
            WRITE (IPR,70) (DASHES(I),I=LSTPNT,NDAYS)
70    FORMAT (' ','--------',2X,20('-'),T35,15(A6,'-'))
            ENDIF
         WRITE (IPR,90) (PEPARM(I),I=2,8),
     *     (TSDATA(I),ESTSYM(I),I=LSTPNT,NDAYS)
90    FORMAT (' ',2A4,2X,5A4,T35,15(F5.2,A1,1X))
         GO TO 160
         ENDIF
C
      WRITE (IPR,100) (PEPARM(I),I=2,3),(PEPARM(I),I=4,8)
100   FORMAT ('0','AREA ID = ',2A4,2X,'DESCRIPTION = ',5A4)
      NPERLN=15
      NPRINT=NPERLN
      IF (NDAYS.LT.NPRINT) NPRINT=NDAYS
      NLEFT=(LSTPNT-NDAYS+1)/NPERLN
      JDA=JDAY
      JHR=0
      CALL MDYH1 (JDA,JHR,IMO,IDA,IYR,IHR,0,0,CODE)
      IBEG=1
      IEND=NPRINT
      IF (IPLSTD.EQ.1.AND.LSTPNT.GT.0) IBEG=LSTPNT
      WRITE (IPR,140) IMO,IDA,IYR,IHR,CODE,
     *   (TSDATA(J),ESTSYM(J),J=IBEG,IEND)
140   FORMAT (' ',I2.2,'/',I2.2,'/',I4.4,'-',I2.2,A4,15(F5.2,A1,1X))
      NLEFT=NDAYS-NPERLN
      IF (NLEFT.GT.0) THEN
         NTIMES=NLEFT/NPERLN
         IF (NTIMES*NPERLN.NE.NLEFT) NTIMES=NTIMES+1
         DO 150 ITIMES=1,NTIMES
            IBEG=NPERLN*ITIMES+1
            IEND=IBEG+NPERLN-1
            IF (IEND.GT.NDAYS) IEND=NDAYS
            JDA=JDA+NPERLN
            JHR=0
            CALL MDYH1 (JDA,JHR,IMO,IDA,IYR,IHR,0,0,CODE)
            WRITE (IPR,140) IMO,IDA,IYR,IHR,CODE,
     *         (TSDATA(J),ESTSYM(J),J=IBEG,IEND)
150      CONTINUE
         ENDIF
C
160   CALL FSTWHR (OLDOPN,IOLDOP,OLDOPN,IOLDOP)
C
      IF (IPTRCE.GT.0) WRITE (IOPDBG,*) 'EXIT ',RTNNAM
C
      RETURN
C
      END
