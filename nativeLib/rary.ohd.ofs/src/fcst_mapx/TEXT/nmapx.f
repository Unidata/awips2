C MODULE NMAPX
C-----------------------------------------------------------------------
C
C  MAIN CONTROLLING ROUTINE FOR THE MAPX COMPUTATIONS
C
      SUBROUTINE NMAPX
C
C-----------------------------------------------------
C       WRITTEN BY R.SHEDD (HRL) 11/90
C-----------------------------------------------------
C
C  JTO - MARFC - 8/2004
C   Increased size of ITEMP array from 200000 to 500000
C    to accomodate increased number of MAPX areas defined
C    at MARFC with their FFG experiment (i.e. creating
C    one MAPX area per HRAP cell).  Their HRAP bounds
C    are 200x200 but only about 14000 are within their
C    RFC boundary.  May have to be increased if any
C    other RFC follows this approach as MARFC is smallest
C    geographically.
C
C    Also, changed WRITE statement about running out
C    of space in LITEMP array and run length needed
C    to be trimmed back.
C
C   JTO - MARFC - 4/2006
C    Bumped size of ITEMP array from 500K to 2M.  At
C    approx. 4300 mapx areas, total run length was trimmed
C    to less than 120 hrs.  The 120 hrs are needed to
C    maintain a 5 day forecast period for MAPX operational
C    use, so upped the size by 4X as about 1/4 of all MAPX areas
C    currently defined.
C
      CHARACTER*4 PTYPE,DTYPE,UNITS
      CHARACTER*8 OLDOPN,PARMID,AREAID,SUMMID
      INTEGER*2 IVALUE
      PARAMETER (LBUF=1000)
      DIMENSION BUF(LBUF)
      DIMENSION IHRAP(4)
      PARAMETER (LITEMP=2000000)
      INTEGER*2 ITEMP(LITEMP)
C
      INCLUDE 'common/ionum'
      INCLUDE 'common/fctime'
      INCLUDE 'common/fctim2'
      INCLUDE 'common/x'
      INCLUDE 'common/pudbug'
      INCLUDE 'prdcommon/pdatas'
      INCLUDE 'prdcommon/pmaxdm'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_mapx/RCS/nmapx.f,v $
     . $',                                                             '
     .$Id: nmapx.f,v 1.8 2004/10/18 19:52:41 edwin Exp $
     . $' /
C    ===================================================================
C
C
      IOPNUM=-1
      CALL FSTWHR ('NMAPX   ',IOPNUM,OLDOPN,IOLDOP)
C
      IBUG=0
      ICPBUG=0
      IF (IPBUG('NMPX').EQ.1) IBUG=1
      IF (IPBUG('NCPU').EQ.1) ICPBUG=1
C
      IF (IPTRCE.GE.1) WRITE (IOPDBG,*) 'ENTER NMAPX'
C
C   READ USER PARAMETERS FROM PPDB
      PARMID=' '
      PTYPE='USER'
      IPTR=0
      CALL RPPREC (PARMID,PTYPE,IPTR,MX,X(1),NFILL,INX,ISTAT)
      IF (ISTAT.GT.0) THEN
          WRITE (IPR,90) PTYPE,ISTAT
          CALL ERROR
          GO TO 80
          ENDIF
      IHRAP(1)=X(32)
      IHRAP(2)=X(33)
      IHRAP(3)=X(34)
      IHRAP(4)=X(35)
      IF (IBUG.EQ.1) WRITE (IOPDBG,*) 'IHRAP=',IHRAP
C
C  READ 'ORDR' PARAMETERS
      PARMID=' '
      PTYPE='ORDR'
      IPTR=0
      CALL RPPREC (PARMID,PTYPE,IPTR,MX,X(1),NFILL,INX,ISTAT)
      IF (ISTAT.NE.0) THEN
         WRITE (IPR,100) PTYPE,ISTAT
         CALL WARN
         ELSE
            IF (IBUG.EQ.1) CALL PDUMPA (NFILL,X(1),PTYPE,PARMID,1)
            IVORDR=X(1)
            IF (IVORDR.GT.1) THEN
               NAMAPX=X(9)
               IF (NAMAPX.GT.0) THEN
                  WRITE (IPR,110) NAMAPX
                  CALL WARN
                  ENDIF
               ENDIF
            IF (IVORDR.GT.2) THEN
               NCBASN=X(10)
               IF (NCBASN.EQ.1) THEN
                  WRITE (IPR,120)
                  CALL WARN
                  ENDIF
               ENDIF
         ENDIF
C
C   READ MXCO PARAMETERS INTO X ARRAY
      PTYPE='MXCO'
      PARMID=' '
      IPTR=0
      CALL RPPREC (PARMID,PTYPE,IPTR,MX,X(1),NFILL,L,ISTAT)
      IF (ISTAT.NE.0) THEN
          WRITE (IPR,90) PTYPE,ISTAT
          CALL ERROR
          GO TO 80
          ENDIF
C
c  GET NUMBER OF MAPX AREAS
      NXA=X(2)
      NMXFL=NFILL-4
      DO 10 I=1,NMXFL
         X(I)=X(I+4)
10       CONTINUE
C
C  READ XGRD PARAMETERS INTO X ARRAY AND COMPACT
      PARMID=' '
      PTYPE='XGRD'
      IPTR=0
      CALL RPPREC (PARMID,PTYPE,IPTR,MX,X(NMXFL+1),NFILL,L,ISTAT)
      IF (ISTAT.NE.0) THEN
         WRITE (IPR,90) PTYPE,ISTAT
         CALL ERROR
         GO TO 80
         ENDIF
      MINPNT=NMXFL+1
      MAXPNT=NMXFL+NFILL-4
      DO 20 I=MINPNT,MAXPNT
         X(I)=X(I+4)
20       CONTINUE
      NXGRD=NFILL-4
C
C  CHECK SIZE OF X ARRAY
      LG=((IHRAP(2)*IHRAP(4))+1)/2
      DTYPE='MAPX'
      MAXDAY=IPRDMD(DTYPE)
      MAXVAL=(((24*MAXDAY+21)/LRECLT)+1)*LRECLT
      NS=LG+MAXVAL
      NX=NXGRD+NMXFL+NS
      IF (NX.GT.MX) THEN
         WRITE (IPR,220) MX,NX
         CALL ERROR
         GO TO 80
         ENDIF
C
C  CHECK IF RUN TIMES CONSISTENT WITH DATA
      IF (IDARUN.GT.LDACPD) THEN
         LDACPD=IDARUN
         JDA=LDACPD/24+1
         JHR=MOD(LDACPD,24)
         CALL MDYH1 (JDA,JHR,JM,JD,JY,JH,0,0,CODE)
         WRITE (IPR,230) JM,JD,JY,JH,CODE
         CALL WARN
         ENDIF
      IF (LDACPD.GT.LDARUN) THEN
         LDARUN=LDACPD
         JDA=LDARUN/24+1
         JHR=MOD(LDARUN,24)
         CALL MDYH1 (JDA,JHR,JM,JD,JY,JH,0,0,CODE)
         WRITE (IPR,240) JM,JD,JY,JH,CODE
         CALL WARN
         ENDIF
      MAXEND=(MAXDAY-MINDAY)*24+LDACPD
      IF (LDARUN.GT.MAXEND) THEN
         LDARUN=MAXEND
         JDA=LDARUN/24+1
         JHR=MOD(LDARUN,24)
         CALL MDYH1 (JDA,JHR,JM,JD,JY,JH,0,0,CODE)
         WRITE (IPR,250) MAXDAY,JM,JD,JY,JH,CODE
         CALL WARN
         ENDIF
C
C  DETERMINE IF ALL MAPX VALUES FOR ALL AREAS FOR THE ENTIRE RUN CAN
C  BE STORED IN THE HOLDING ARRAY (USED TO MAXIMIZE EFFICIENCY FOR
C  WRITING TO THE PROCESSED DATA BASE)
      LJHOUR=MIN0(LDACPD,LDARUN)
C
C  NHRS IS THE NUMBER OF HOURS OF OBSERVED DATA TO PROCESS
      NHRS=LJHOUR-IDARUN
      NHRSX=NHRS
      MAXRQD=NXA*NHRS
      IF (MAXRQD.GT.LITEMP) THEN
C     ARRAY TO STORE MAPX VALUES TOO SMALL - TRUNCATE RUN
         NHRS=LITEMP/NXA
         LJHOUR=IDARUN+NHRS-1
C
C  JTO - MARFC - 8/2004 - made third var. below NHRS instead of NHRSX
C
         WRITE (IPR,260) NHRSX,NHRS,NHRS
         NHRSX=NHRS
         CALL WARN
         ENDIF
C
C  PRINT HEADING
      CALL UPAGE (IPR)
      WRITE (IPR,270)
      JDAY=IDARUN/24+1
      JHR=MOD(IDARUN,24)
      CALL MDYH1 (JDAY,JHR,JM1,JD1,JY1,JH1,0,0,CODE)
      JDAY=LDARUN/24+1
      JHR=MOD(LDARUN,24)
      CALL MDYH1 (JDAY,JHR,JM2,JD2,JY2,JH2,0,0,CODE)
      JDAY=LDACPD/24+1
      JHR=MOD(LDACPD,24)
      CALL MDYH1 (JDAY,JHR,LCM,LCD,LCY,LCH,NOUTZ,NOUTDS,TZC)
      WRITE (IPR,280) JM1,JD1,JY1,JH1,CODE,
     &   JM2,JD2,JY2,JH2,CODE,
     &   LCM,LCD,LCY,LCH,TZC
C
      IGPNT=NXGRD+NMXFL+1
      ISIZ=(MX -IGPNT+1)*2
C
      IF (IBUG.GT.0) THEN
         WRITE (IOPDBG,*)
     &      ' IDARUN=',IDARUN,
     &      ' LDARUN=',LDARUN,
     &      ' LDACPD=',LDACPD,
     &      ' NHRS=',NHRS,
     &      ' NHOPDB=',NHOPDB,
     &      ' '
         JDAY=IDARUN/24+1
         JHR=MOD(IDARUN,24)
         CALL MDYH1 (JDAY,JHR,JM,JD,JY,JH,0,0,CODE)
         WRITE (IOPDBG,130) JM,JD,JY,JH,CODE
         JDAY=LDARUN/24+1
         JHR=MOD(LDARUN,24)
         CALL MDYH1 (JDAY,JHR,JM,JD,JY,JH,0,0,CODE)
         WRITE (IOPDBG,140) JM,JD,JY,JH,CODE
         JDAY=LDACPD/24+1
         JHR=MOD(LDACPD,24)
         CALL MDYH1 (JDAY,JHR,JM,JD,JY,JH,0,0,CODE)
         WRITE (IOPDBG,150) JM,JD,JY,JH,CODE
         ENDIF
C
      IF (ICPBUG.GT.0) THEN
         ITOTCP=1
         CALL URTIMR (LAPSCP,ITOTCP)
         TOTCP=FLOAT(ITOTCP)/100.
         IBEGCP=ITOTCP
         WRITE (IOPDBG,160) TOTCP
         ENDIF
C
C  READ GRIDDED DATA AND CALCULATE MAPX FOR EACH HOUR
C
      DO 30 JHOUR=IDARUN+1,LJHOUR,1
C     INITIALIZE GRID
         IVALUE=-999
         CALL INGRID (IVALUE,IHRAP(2),IHRAP(4),X(IGPNT),ISIZ,IGSTAT)
C     READ GRIDDED DATA
         DTYPE='xmrg'
         CALL RDGRID (DTYPE,JHOUR,IHRAP(1),IHRAP(2),IHRAP(3),
     &      IHRAP(4),X(IGPNT),ISIZ,IGSTAT)
         IF (ICPBUG.GT.0) THEN
            CALL URTIMR (LAPSCP,ITOTCP)
            CPLAPS=FLOAT(LAPSCP)/100.
            WRITE (IOPDBG,170) JHOUR,CPLAPS
            ENDIF
         IF (IGSTAT.NE.0) THEN
            JDAY=JHOUR/24+1
            JHR=MOD(JHOUR,24)
            CALL MDYH1 (JDAY,JHR,JM,JD,JY,JH,0,0,CODE)
            WRITE (IOPDBG,330) JM,JD,JY,JH,CODE,DTYPE
            CALL WARN
            LJHOUR=JHOUR-1
            NHRS=LJHOUR-IDARUN
            IF (NHRS.GT.0) GO TO 40
               WRITE (IPR,340) DTYPE
               CALL ERROR
               GO TO 80
            ENDIF
         IRELHR=JHOUR-IDARUN
C     COMPUTE MAPX FOR THIS HOUR FOR ALL BASINS
         CALL NCLMPX (X(1),NMXFL,X(MINPNT),2*NXGRD,X(IGPNT),IHRAP,
     &      IHRAP(2),IHRAP(4),NXA,IRELHR,NHRSX,ITEMP,LITEMP)
         IF (ICPBUG.GT.0) THEN
            CALL URTIMR (LAPSCP,ITOTCP)
            CPLAPS=FLOAT(LAPSCP)/100.
            WRITE (IOPDBG,180) NXA,JHOUR,CPLAPS
            ENDIF
30       CONTINUE
C
40    IF (NHRS.GT.LBUF) THEN
         WRITE (IPR,350) NHRS,LBUF
         CALL ERROR
         GO TO 80
         ENDIF
C
C  PROCESS EACH MAPX BASIN FOR ALL HOURS
C
      IFILL=MX-NX
      SUMMAX=0.0
C      
      ICHKID=0
C
      DO 70 IAREA=1,NXA
C     CALCULATE POSITION IN ITEMP ARRAY WHERE MAPX VALUE FOR THIS HOUR
C     FOR THIS AREA IS TO BE PLACED
         LOCTMP=(IAREA-1)*NHRSX+1
         CALL UMEMOV (X(3*IAREA-2),AREAID,2)
         IF (IBUG.EQ.1) WRITE (IOPDBG,*) 'AREAID=',AREAID
C     CHECK IF AREA TO BE PROCESSED
         IF (ICHKID.EQ.1) THEN
            IF (AREAID.NE.'SOHN3MER') THEN
               WRITE (IPR,*) '**NOTE** AREA ',AREAID,
     &            'WILL NOT BE PROCESSED.'
               GO TO 70
               ELSE
                  WRITE (IPR,*) '**NOTE** PROCESSING AREA ',AREAID,'.'
               ENDIF
            ENDIF
C     CONVERT I*2 MAPX VALUES INTO REALS
         SUM=0
         DO 50 I=1,NHRS
            ITEMP2=ITEMP(LOCTMP+I-1)
            BUF(I)=FLOAT(ITEMP2)/100.0
            IF (ITEMP(LOCTMP+I-1).LT.0) BUF(I)=FLOAT(ITEMP2)
            IF (BUF(I).GE.0) SUM=SUM+BUF(I)
50          CONTINUE
         IF (SUM.LT.SUMMAX) GO TO 60
            SUMMAX=SUM
            SUMMID=AREAID
60       IF (IBUG.GT.0) THEN
            WRITE (IOPDBG,290) AREAID,LOCTMP
            WRITE (IOPDBG,300) (ITEMP(J),J=LOCTMP,LOCTMP+NHRS-1)
            WRITE (IOPDBG,310) (BUF(J),J=1,NHRS)
            ENDIF
C   WRITE MAPX TIME SERIES
         ISTPRD=IDARUN+NHOPDB+1
         DTYPE='MAPX'
         UNITS='MM'
         ITIME=1
         IFPTR=0
         ICALL=0
         IREC=0
         IF (IBUG.EQ.1) THEN
            WRITE (IOPDBG,*)
     &         ' AREAID=',AREAID,
     &         ' DTYPE=',DTYPE,
     &         ' ISTPRD=',ISTPRD,
     &         ' NHRS=',NHRS,
     &         ' '
            ENDIF
         CALL WPRDD (AREAID,DTYPE,ISTPRD,ITIME,NHRS,UNITS,NHRS,
     &      LBUF,BUF,IFPTR,ICALL,IFILL,X(NX+1),IREC,ISTAT)
         IF (IBUG.GE.1) WRITE (IOPDBG,*) 'WPRDD ISTAT=',ISTAT
         IF (ISTAT.NE.0) THEN
            IF (ISTAT.EQ.2.OR.ISTAT.EQ.7) THEN
               WRITE (IPR,360) DTYPE,AREAID
               CALL WARN
               ELSE
                  WRITE (IPR,370) DTYPE,AREAID,ISTAT
                  CALL ERROR
               ENDIF
            ENDIF
         IF (ICPBUG.GT.0) THEN
            CALL URTIMR (LAPSCP,ITOTCP)
            CPLAPS=FLOAT(LAPSCP)/100.
            WRITE (IOPDBG,200) AREAID,CPLAPS
            ENDIF
70       CONTINUE
C
      IF (IBUG.EQ.1) WRITE (IPR,320) SUMMAX,SUMMID
C
C  CHECK IF NEED TO FILL IN REMAINDER OF FUTURE ARRAYS
      NHRS=LDARUN-LJHOUR
      IF (NHRS.GT.0) THEN
         CALL NFUTR (X(1),X(NMXFL+1),NX,NHRS,LJHOUR,LDARUN,NXA,NMXFL,
     &      LBUF,BUF,IFILL,X(NX+1),X(NMXFL+1),ICHKID)
         ENDIF
C
      IF (ICPBUG.GT.0) THEN
         CALL URTIMR (LAPSCP,ITOTCP)
         TOTCP=FLOAT(ITOTCP)/100.
         RUNCP=FLOAT(ITOTCP-IBEGCP)/100.
         WRITE (IOPDBG,190) RUNCP,TOTCP
         ENDIF
C
C  PRINT TIME SERIES DATA
      CALL NXTABL (X(1),NMXFL,X(NMXFL+1),MX-NX,BUF,LBUF,NXA,ICHKID)
      IF (ICPBUG.GT.0) THEN
         CALL URTIMR (LAPSCP,ITOTCP)
         CPLAPS=FLOAT(LAPSCP)/100.
         TOTCP=FLOAT(ITOTCP)/100.
         RUNCP=FLOAT(ITOTCP-IBEGCP)/100.
         WRITE (IOPDBG,210) CPLAPS,TOTCP
         ENDIF
C
80    CALL FSTWHR (OLDOPN,IOLDOP,OLDOPN,IOLDOP)
C
      IF (IPTRCE.GE.1) WRITE (IOPDBG,*) 'EXIT NMAPX'
C
      RETURN
C
90    FORMAT ('0**ERROR** READING PREPROCESSOR PARAMETRIC DATA BASE ',
     &   'FOR PARAMETER TYPE ',A,'. RPPREC STATUS CODE = ',I2)
100   FORMAT ('0**WARNING** ',A,' PARAMETERS NOT ',
     &   'SUCCESSFULLY READ. RPPREC STATUS CODE = ',I2)
110   FORMAT ('0**WARNING** ',I3,' MAPX AREAS HAVE BEEN ADDED SINCE ',
     &   'THE PROGRAM PPINIT COMMAND ORDER WAS RUN.')
120   FORMAT ('0**WARNING** ONE OR MORE BASINS USED BY MAPX AREAS ',
     &   'HAVE BEEN CHANGED SINCE ',
     &   'THE PROGRAM PPINIT COMMAND ORDER WAS RUN.')
130   FORMAT (' START OF RUN: ',I2.2,'/',I2.2,'/',I4,'-',I2.2,A4)
140   FORMAT (' END   OF RUN: ',I2.2,'/',I2.2,'/',I4,'-',I2.2,A4)
150   FORMAT (' END   OF OBS: ',I2.2,'/',I2.2,'/',I4,'-',I2.2,A4)
160   FORMAT (' START OF HOURLY LOOP FOR DATA RETRIEVAL AND ',
     &   'CALCULATIONS',
     &   ' - ',
     &   'TOTAL CPU TIME USED IN RUN = ',F7.2,' SECONDS')
170   FORMAT (' GRIDDED DATA FOR HOUR ',I7,' RETRIEVED ',
     &   ' - ',
     &   'CPU TIME USED = ',F7.2,' SECONDS')
180   FORMAT (' MAPX CALCULATIONS FOR ',I3,' AREAS FOR HOUR ',I7,
     &   ' COMPLETED',
     &   ' - ',
     &   'CPU TIME USED = ',F7.2,' SECONDS')
190   FORMAT (' COMPLETION OF MAPX CALCULATIONS',
     &   ' - ',
     &   'CPU TIME USED FOR CALCULATIONS = ',F7.2,' SECONDS ',
     &   'TOTAL CPU TIME USED IN RUN = ',F7.2,' SECONDS')
200   FORMAT (' PRD WRITTEN TO FOR AREA ',A,
     &   ' - ',
     &   'CPU TIME USED = ',F7.2,' SECONDS')
210   FORMAT (' AFTER DISPLAYING MAPX TABLE',
     &   ' - ',
     &   'CPU TIME USED = ',F7.2,' SECONDS ',
     &   'TOTAL CPU TIME = ',F7.2,' SECONDS')
220   FORMAT ('0**ERROR** NUMBER OF WORDS AVAILABLE IN THE X ARRAY (',
     &   I6,') IS LESS THAN NEEDED (',I6,').')
230   FORMAT ('0**WARNING** START TIME IS GREATER THAN TIME OF LAST ',
     &   'OBSERVED DATA. LAST COMPUTATIONAL DAY CHANGED TO ',
     &   I2.2,'/',I2.2,'/',I4,'-',I2,A4,'.')
240   FORMAT ('0**WARNING** TIME OF LAST OBSERVED DATA IS GREATER ',
     &   'THAN END TIME. END TIME CHANGED TO ',
     &   I2.2,'/',I2.2,'/',I4,'-',I2,A4,'.')
250   FORMAT ('0**WARNING** END TIME IS GREATER THAN ALLOWED BY ',
     &   'MAXDAY (',I3,'). END TIME CHANGED TO ',
     &   I2.2,'/',I2.2,'/',I4,'-',I2,A4,'.')
260   FORMAT ('0**WARNING** NUMBER OF HOURS IN RUN PERIOD (',I4,
     &   ') EXCEEDS MAXIMUM ALLOWED (',I4,') AND HAS BEEN CHANGED ',
     &   'TO ',I4,'.')
270   FORMAT ('0',45X,'MAPX FUNCTION')
280   FORMAT ('0',30X,'RUN PERIOD',2X,
     &      I2.2,'/',I2.2,'/',I4.4,'-',I2.2,A4,' THRU ',
     &      I2.2,'/',I2.2,'/',I4.4,'-',I2.2,A4 /
     &   '0',30X,'OBSERVED DATA ENDS AT ',
     &      I2.2,'/',I2.2,'/',I4.4,'-',I2.2,A4)
290   FORMAT (' WRITING TO PRD -  AREAID = ',A,
     &   ' ITEMP ARRAY POSN = ',I5)
300   FORMAT (' VALUES IN ITEMP ARRAY: ' / (1X,(10I6)))
310   FORMAT (' VALUES IN BUF ARRAY: ' / (1X,(10F9.2)))
320   FORMAT (' MAXIMUM PRECIPITATION IS ',F6.1,' IN AREA ',A)
330   FORMAT ('0**WARNING** NO GRIDDED DATA FIELD FOUND FOR ',
     &      I2.2,'/',I2.2,'/',I4,'-',I2.2,' ',A4,' FOR TYPE ',A,'.' /
     &   13X,'MISSING DATA IS NOT ALLOWED. ',
     &      'LSTCMPDY WILL BE RESET TO THE PRIOR HOUR.')
340   FORMAT ('0**ERROR** NO GRIDDED DATA FOR TYPE ',A,
     &   ' ARE AVAILABLE.')
350   FORMAT ('0**ERROR** NUMBER OF HOURS TO BE PROCESSED (',I4,
     &   ') EXCEEDS SIZE OF TIME SERIES ARRAY (',I4,').')
360   FORMAT('0**WARNING** IN NMAPX - ',A,' TIME SERIES FOR AREA ',A,
     & ' COULD NOT ALL BE WRITTEN TO THE PDB - TRUNCATED AT THE END.')
370   FORMAT ('0**ERROR** IN NMAPX - ',A,' TIME SERIES FOR AREA ',A,
     & ' COULD NOT BE WRITTEN TO THE PDB. WPRDD STATUS CODE = ',I2)
C
      END
