C MODULE FSTAFG
C-----------------------------------------------------------------------
C
C  ROUTINE TO PRINT FORECAST GROUP STATUS REPORT.
C
      SUBROUTINE FSTAFG (NOOBSO)
C
      CHARACTER*8 FGIDZ
C
      DIMENSION SGTBLE(2,7,22)
C
      INCLUDE 'common/ionum'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/fcfgs'
      INCLUDE 'common/fcunit'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_status/RCS/fstafg.f,v $
     . $',                                                             '
     .$Id: fstafg.f,v 1.4 2002/02/11 20:25:21 dws Exp $
     . $' /
C    ===================================================================
C
      DATA BLANK/4h    /
C
C
      IF (ITRACE.GE.1) WRITE (IODBUG,*) 'ENTER FSTAFG'
C
C  PRINT PAGE HEADER
      IFIRST=0
      CALL FSTAHD (IFIRST)
      WRITE (IPR,20)
20    FORMAT ('0- FORECAST GROUP STATUS -')
C
C  GET NUMBER OF FORECAST GROUPS
      IREC=1
      CALL UREADT (KFFGST,IREC,FGID,ISTAT)
      IF (ISTAT.NE.0) GO TO 260
C
      NFGREC=IDUMYG
      MFGREC=NFGREC
      IF (MFGREC.LE.0) MFGREC=1
C
      IF (NFGREC.LE.0) THEN
         WRITE (IPR,250)
250   FORMAT ('0**NOTE** NO FORECAST GROUPS DEFINED.')
         GO TO 260
         ENDIF
C
      NOBSOL=0
C
C  PROCESS EACH FORECAST GROUP
      DO 220 IFGREC=1,MFGREC
         MLINE=0
         CALL UREADT (KFFGST,IFGREC,FGID,ISTAT)
         IF (ISTAT.NE.0) GO TO 260
         CALL UMEMOV (FGID,FGIDZ,LEN(FGIDZ)/4)
         IF (FGIDZ.EQ.'OBSOLETE') THEN
            NOBSOL=NOBSOL+1
            IF (NOOBSO.EQ.1) GO TO 220
            ENDIF
         IF (IFGREC.GT.1) THEN
            CALL FSTAHD (IFIRST)
            WRITE (IPR,40)
40    FORMAT ('0- FORECAST GROUP STATUS (CONTINUED) -')
            ENDIF
         IYEAR=MOD(ICRDTF(3),100)
         WRITE (IPR,50) FGID,DESCR,ICRDTF(1),ICRDTF(2),IYEAR,
     *      ICRDTF(4),ICRDTF(5)
50    FORMAT ('0',4X,2A4,' -- ',5A4,' -- DEFINED ON ',
     *    I2.2,'/',I2.2,'/',I2.2,'-',I4.4,'.',I4.3)
         IF (ISPEC.EQ.1) WRITE (IPR,60)
60    FORMAT ('+',72X,'SPECIAL')
         WRITE (IPR,80) NSEG,MINDTF
80    FORMAT ('0',13X,'CONTAINS ',I4,' SEGMENTS.  MINIMUM TIME STEP = ',
     *   I2,' HOURS')
         WRITE (IPR,90) IFGREC
90    FORMAT ('0',13X,'RECORD ',I4,' IN FILE FCFGSTAT')
         IF (CGIDF(1).EQ.BLANK.AND.CGIDF(2).EQ.BLANK) GO TO 110
            WRITE (IPR,100) ICOSEQ,CGIDF
100   FORMAT ('0',13X,'IS EXECUTED',I4,'-TH IN CARRYOVER GROUP ',2A4)
            GO TO 130
110      WRITE (IPR,120)
120   FORMAT ('0',13X,'IS NOT ASSIGNED TO ANY CARRYOVER GROUP')
130      WRITE (IPR,140) IREC
140   FORMAT ('0',13X,'LIST OF SEGMENTS BEGINS AT RECORD ',I5,
     *   ' IN FILE FCFGLIST')
C     START SEGMENT LIST
         NPAGES=(NSEG-1)/154+1
         DO 210 IPAGE=1,NPAGES
            IF (IPAGE.EQ.1) GO TO 160
            CALL FSTAHD (0)
            WRITE (IPR,40)
            WRITE (IPR,150) FGID
150   FORMAT ('0',4X,2A4,' (CONTINUED)')
160         WRITE (IPR,170)
170   FORMAT ('0',13X,'*** LIST OF SEGMENTS IN COMPUTATIONAL ',
     *   'ORDER (READ DOWN) ***')
180   FORMAT ('0',12X,2A4,2X,2A4,2X,2A4,2X,2A4,2X,2A4,2X,2A4,2X,2A4)
C        FILL SGTBLE WITH BLANKS
            DO 190 K=1,22
               DO 190 J=1,7
               DO 190 I=1,2
               SGTBLE(I,J,K)=BLANK
190            CONTINUE
            IS1=(IPAGE-1)*154+1
            IS2=IPAGE*154
            IF (IS2.GT.NSEG) IS2=NSEG
C        FILL SEGMENT TABLE
            DO 200 IS=IS1,IS2
               IR=IREC+IS-1
               ISP=IS-IS1+1
               ICOLUM=(ISP-1)/22+1
               LINE=ISP-(ICOLUM-1)*22
               IF (LINE.GT.MLINE) MLINE=LINE
               CALL UREADT (KFFGL,IR,SGTBLE(1,ICOLUM,LINE),ISTAT)
               IF (ISTAT.NE.0) GO TO 260
200            CONTINUE
C        PRINT SEGMENT TABLE
            WRITE (IPR,180) (((SGTBLE(I,J,K),I=1,2),
     *         J=1,7),K=1,MLINE)
210         CONTINUE
220      CONTINUE
C
      WRITE (IPR,230) NOBSOL
230   FORMAT ('0THERE ARE ',I3,' OBSOLETE FORECAST GROUPS DEFINED')
C
260   RETURN
C
      END
