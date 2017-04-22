C MODULE TDSPM
C-----------------------------------------------------------------------
C
      SUBROUTINE TDSPM (NUM,STAID,STATE,DESCR,MAXT,MINT,KODE,IFLAG,MSNG)
C
C  THIS ROUTINE DISPLAYS DATA FOR MAXIMUM AND MINIMUM TEMPERATURE
C  STATIONS
C
C  THIS ROUTINE WAS ORIGINALLY WRITTEN BY W. GILREATH
C
      CHARACTER*1 KODE
      CHARACTER*4 UNITS
      CHARACTER*8 OLDOPN,STAID(2)
      CHARACTER*20 DESCR(2)
      INTEGER*2 MAXT,MINT,MSNG
C
      DIMENSION NUM(2),RMAXT(2),RMINT(2)
      DIMENSION MAXT(2),MINT(2),KODE(3,2),STATE(2)
C
      INCLUDE 'common/ionum'
      INCLUDE 'common/pudbug'
      INCLUDE 'common/tout'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_mat/RCS/tdspm.f,v $
     . $',                                                             '
     .$Id: tdspm.f,v 1.2 2000/07/21 19:18:36 page Exp $
     . $' /
C    ===================================================================
C
C
      IF (IPTRCE.GE.1) WRITE (IOPDBG,*) 'ENTER TDSPM'
C
      IOPNUM=-1
      CALL FSTWHR ('TDSPM   ',IOPNUM,OLDOPN,IOLDOP)
C
      UNITS='DEGF'
      DO 10 I=1,2
         RMAXT(I)=MAXT(I)/10.
         RMINT(I)=MINT(I)/10.
10       CONTINUE
C
      IF (METRIC.EQ.1) THEN
C     CONVERT TO CENTIGRADE
         UNITS='DEGC'
         ICONV=1
         NVAL=2
         CALL UDUCNV ('DEGF','DEGC',ICONV,NVAL,RMAXT,RMAXT,IERR)
         CALL UDUCNV ('DEGF','DEGC',ICONV,NVAL,RMINT,RMINT,IERR)
         ENDIF
C
      IF (IFLAG.NE.1) GO TO 70
C
C PRINT WRITE HEADER
      WRITE (IPR,20) UNITS
20    FORMAT ('0',T40,'MAXIMUM/MINIMUM TEMPERATURES  (UNITS=',A,')')
      WRITE (IPR,30)
30    FORMAT ('0',T35,
     1 'B = BLENDED',3X,
     3 'C = CORRECTED',3X,
     4 'E = ESTIMATED',3X,
     2 'M = MEAN')
      WRITE (IPR,40)
40    FORMAT (2(' STATION STATION          STATION              ',
     1 'MAXIMUM MINIMUM   '))
      WRITE (IPR,50)
50    FORMAT (2(' NUMBER  IDENTIFIER STATE DESCRIPTION          ',
     1 'TEMP.   TEMP.     '))
      WRITE (IPR,60)
60    FORMAT (2(' ------- ---------- ----- -------------------- ',
     1 '------- -------   '))
C
C CHECK IF NUMBER OF STATIONS IS AN ODD NUMBER
70    NSTA=2
      IF (NUM(2).EQ.MSNG) NSTA=1
      WRITE (IPR,80) (NUM(I),STAID(I),STATE(I),
     1  DESCR(I),RMAXT(I),KODE(1,I),RMINT(I),KODE(2,I),I=1,NSTA)
80    FORMAT (2(' ',1X,I5,1X,1X,A,2X,1X,A2,3X,1X,A,1X,F6.1,A1,1X,F6.1,
     2 A1,3X))
C
      CALL FSTWHR (OLDOPN,IOLDOP,OLDOPN,IOLDOP)
C
      RETURN
C
      END
