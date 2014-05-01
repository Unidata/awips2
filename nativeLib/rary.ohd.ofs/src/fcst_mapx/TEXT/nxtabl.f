C MODULE NXTABL
C---------------------------------------------------------------------
C
      SUBROUTINE NXTABL (MXCO,LMXCO,IWKBUF,LWKBUF,BUF,LBUF,NXA,ICHKID)
C
C  ROUTINE TO PRINT MAPX TIME SERIES DATA.
C
C---------------------------------------------------------------------
C
      CHARACTER*4 DTYPE,UNITS
      CHARACTER*8 AREAID,OLDOPN
C
      DIMENSION MXCO(LMXCO),BUF(LBUF),IWKBUF(LWKBUF)
C
      INCLUDE 'common/ionum'
      INCLUDE 'common/fctime'
      INCLUDE 'common/fctim2'
      INCLUDE 'common/pudbug'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_mapx/RCS/nxtabl.f,v $
     . $',                                                             '
     .$Id: nxtabl.f,v 1.6 2000/07/21 19:16:25 page Exp $
     . $' /
C    ===================================================================
C
C
      IOPNUM=-1
      CALL FSTWHR ('NXTABL  ',IOPNUM,OLDOPN,IOLDOP)
C
      IF (IPTRCE.GE.1) WRITE (IOPDBG,*) 'ENTER NXTABL'
C
      IBUG=0      
C
      UNITS='IN'
C
C  SET AND DISPLAY APPROPRIATE TIME
      JDA=(IDARUN+1)/24+1
      JHR=MOD((IDARUN+1),24)
      CALL MDYH1 (JDA,JHR,IMO,IDA,IYR,IHR,0,0,CODE)
      WRITE (IPR,30) IMO,IDA,IYR,IHR,CODE,UNITS
      ISTPRD=IDARUN+NHOPDB+1
      NHRS=LDARUN-IDARUN
      IFPTR=0
C
C  READ AND PRINT TIME SERIES DATA FOR EACH AREA
      RMISS=-999.
      DO 20 I=1,NXA
         CALL UMEMOV (MXCO(3*I-2),AREAID,2)
         IF (IBUG.EQ.1) WRITE (IOPDBG,*) 'AREAID=',AREAID
C     CHECK IF AREA TO BE PROCESSED         
         IF (ICHKID.EQ.1) THEN
            IF (AREAID.NE.'SOHN3MER') THEN
               WRITE (IPR,*) '**NOTE** AREA ',AREAID,
     &            'WILL NOT BE PROCESSED.'
               GO TO 20
               ELSE
                  WRITE (IPR,*) '**NOTE** PROCESSING AREA ',AREAID,'.'
               ENDIF
            ENDIF
C     READ DATA            
         DTYPE='MAPX'
	 ISTEP=1
         CALL RPRDD (AREAID,DTYPE,ISTPRD,ISTEP,NHRS,UNITS,RMISS,
     &      LBUF,BUF,IFPTR,LWKBUF,IWKBUF,ISTAT)
         IF (ISTAT.NE.0) THEN
            IF (ISTAT.NE.2) THEN
                WRITE (IPR,50) DTYPE,AREAID,ISTAT
                CALL WARN
                GO TO 20
                ENDIF
             ENDIF
C     PRINT DATA             
         IF (IBUG.EQ.1) THEN
            WRITE (IPR,40) AREAID,(BUF(J),J=1,NHRS)
            ENDIF
         WRITE (IPR,43) AREAID
C     SET NUMBER OF VALUES TO BE PRINTED PER LINE          
         NPERLN=12
	 NPRINT=NPERLN
	 IF (NHRS.LT.NPRINT) NPRINT=NHRS
         JDARUN=IDARUN
         JDA=(JDARUN+1)/24+1
         JHR=MOD((JDARUN+1),24)
         CALL MDYH1 (JDA,JHR,IMO,IDA,IYR,IHR,0,0,CODE) 
         WRITE (IPR,45) IMO,IDA,IYR,IHR,CODE,(BUF(J),J=1,NPRINT) 
         NLEFT=NHRS-NPERLN
	 IF (NLEFT.GT.0) THEN
            NTIMES=NLEFT/NPERLN
            IF (NTIMES*NPERLN.NE.NLEFT) NTIMES=NTIMES+1
            DO 10 ITIMES=1,NTIMES            
               IBEG=NPERLN*ITIMES+1
               IEND=IBEG+NPERLN-1
               IF (IEND.GT.NHRS) IEND=NHRS
               JDARUN=JDARUN+NPERLN
               JDA=(JDARUN+1)/24+1
               JHR=MOD((JDARUN+1),24)
               CALL MDYH1 (JDA,JHR,IMO,IDA,IYR,IHR,0,0,CODE)
               WRITE (IPR,45) IMO,IDA,IYR,IHR,CODE,(BUF(J),J=IBEG,IEND)
10             CONTINUE
            ENDIF
20       CONTINUE
C
      CALL FSTWHR (OLDOPN,IOLDOP,OLDOPN,IOLDOP)
C
      IF (IPTRCE.GE.1) WRITE (IOPDBG,*) 'EXIT NXTABL'
C
      RETURN
C
30    FORMAT ('0MAPX TIME SERIES DATA:',3X,
     &   'FIRST DATA VALUE = ',I2.2,'/',I2.2,'/',I4.4,'-',I2.2,A4,3X,
     &   'DATA UNITS = ',A)
40    FORMAT ('0',A,1X,12(F7.2,2X) / 19(10X,12(F7.2,2X)/))
43    FORMAT ('0AREAID = ',A)
45    FORMAT (' ',I2.2,'/',I2.2,'/',I4.4,'-',I2.2,A4,12F9.2)
50    FORMAT ('0**WARNING** ',A,' TIME SERIES DATA NOT SUCCESSFULLY ',
     &   'READ FOR AREA ',A,'. RPRDD STATUS CODE = ',I2)
C
      END
