C MODULE TDSPI
C-----------------------------------------------------------------------
C
      SUBROUTINE TDSPI (NUM,STAID,STATE,DESCR,MAXT,MINT,INSTT,NINST,IDT,
     1 KODE,IFLAG)
C
C  THIS ROUTINE DISPLAYS DATA FOR INSTANTANEOUS TEMPERATURE STATIONS
C
C  THIS ROUTINE WAS ORIGINALLY WRITTEN BY W. GILREATH
C
      CHARACTER*1 KODE(10)
      CHARACTER*4 UNITS,TZC
      CHARACTER*8 STAID,OLDOPN
      CHARACTER*20 DESCR
      INTEGER*2 MAXT,MINT,INSTT
C
      DIMENSION INSTT(8),RINSTT(8)
C
      INCLUDE 'common/pudbug'
      INCLUDE 'common/ionum'
      INCLUDE 'common/fctime'
      INCLUDE 'common/tout'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_mat/RCS/tdspi.f,v $
     . $',                                                             '
     .$Id: tdspi.f,v 1.2 2000/07/21 19:18:20 page Exp $
     . $' /
C    ===================================================================
C
C
      IF (IPTRCE.GE.1) WRITE (IOPDBG,*) 'ENTER TDSPI'
C
      IOPNUM=-1
      CALL FSTWHR ('TDSPI   ',IOPNUM,OLDOPN,IOLDOP)
C
      UNITS='DEGF'
      RMAXT=MAXT/10.
      RMINT=MINT/10.
      DO 10 I=1,NINST
         RINSTT(I)=INSTT(I)/10.
10       CONTINUE
C
      IF (METRIC.EQ.1) THEN
C     CONVERT TO CENTIGRADE
         UNITS='DEGC'
         ICONV=1
         NVAL=1
         CALL UDUCNV ('DEGF','DEGC',ICONV,NVAL,RMAXT,RMAXT,IERR)
         CALL UDUCNV ('DEGF','DEGC',ICONV,NVAL,RMINT,RMINT,IERR)
         DO 20 I=1,NINST
            CALL UDUCNV ('DEGF','DEGC',ICONV,NVAL,RINSTT(I),RINSTT(I),
     *         IERR)
20          CONTINUE
         ENDIF
C
      IF (IFLAG.NE.1) GO TO 100
C
C  WRITE HEADER
      WRITE (IPR,30) UNITS
30    FORMAT ('0',T40,'INSTANTANEOUS TEMPERATURES  (UNITS=',A,')')
      JDAY=1
      INTHR=6
      CALL MDYH1 (JDAY,INTHR,IM,ID,IY,IH1,NOUTZ,NOUTDS,TZC)
      IH2=IH1+6
      IH3=IH2+6
      IH4=IH3+6
      IF (TZC.EQ.'Z') GO TO 40
         IF (IH2.GT.24) IH2=IH2-24
         IF (IH3.GT.24) IH3=IH3-24
         IF (IH4.GT.24) IH4=IH4-24
         GO TO 50
40    IF (IH1.EQ.24) IH1=0
      IF (IH2.GE.24) IH2=IH2-24
      IF (IH3.GE.24) IH3=IH3-24
      IF (IH4.GE.24) IH4=IH4-24
50    WRITE (IPR,60)
60    FORMAT ('0',T20,
     1 'C = CORRECTED',5X,
     2 'E = ESTIMATED',5X,
     3 'I = ESTIMATED FROM INSTANTANEOUS VALUES')
      WRITE (IPR,70)
70    FORMAT ('0STATION STATION          STATION              ',
     1 'MAXIMUM MINIMUM                         ',
     2 'INSTANTANEOUS TEMPERATURES')
      WRITE (IPR,80) IH1,TZC,IH2,TZC,IH3,TZC,IH4,TZC
80    FORMAT (' NUMBER  IDENTIFIER STATE DESCRIPTION          ',
     1 'TEMP.   TEMP.  ',
     2 10X,I2.2,A,10X,I2.2,A,10X,I2.2,A,10X,I2.2,A)
      WRITE (IPR,90)
90    FORMAT (' ------- ---------- ----- -------------------  ------- ',
     1 '-------',4(10X,'------'))
C
100   IF (IDT.EQ.6) THEN
C     INSTANTANEOUS TEMPERATURES ARE 6 HOUR VALUES
         WRITE (IPR,110) NUM,STAID,STATE,DESCR,RMAXT,KODE(1),
     1      RMINT,KODE(2),(RINSTT(I),KODE(I+2),I=1,NINST)
110   FORMAT (' ',1X,I5,1X,1X,A,2X,1X,A2,3X,1X,A,1X,
     1 F6.1,A1,1X,F6.1,A1,1X,
     2 4(8X,F6.1,A1,1X))
         GO TO 130
         ENDIF
C
C  INSTANTANEOUS TEMPERATURES ARE 3 HOUR VALUES
      WRITE (IPR,120) NUM,STAID,STATE,DESCR,RMAXT,KODE(1),
     1   RMINT,KODE(2),(RINSTT(I),KODE(I+2),I=1,NINST)
120   FORMAT (1X,1X,I5,1X,1X,A,2X,1X,A2,3X,1X,A,1X,
     1 F6.1,A1,1X,F6.1,A1,1X,
     2 8(F6.1,A1,1X))
C
130   CALL FSTWHR (OLDOPN,IOLDOP,OLDOPN,IOLDOP)
C
      RETURN
C
      END
