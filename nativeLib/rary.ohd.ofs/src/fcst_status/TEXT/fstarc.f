C MODULE FSTARC
C-----------------------------------------------------------------------
C
C  ROUTINE TO PRINT STATUS REPORT FOR RATING CURVES
C
      SUBROUTINE FSTARC (NOOBSO)
C
C  THIS ROUTINE PRINTS ALL RATING CURVES (INCLUDE OBSOLETE)
C  AND THE TYPE OF EXTENSION USED OR WHETHER LOOP RATING IS USED.
C
C  ORIGINALLY WRITTEN BY JOE OSTROWSKI - HRL - 5/1981
C
      CHARACTER*8 CHKID
      CHARACTER*21 REMARK(4)/
     *  'LINEAR EXTENSION',
     *  'LOGARITHMIC EXTENSION',
     *  'HYDRAULIC EXTENSION',
     *  'LOOP RATING EXTENSION'/
C
      DIMENSION IBUF(3)
C
      INCLUDE 'common/ionum'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/fcunit'
      INCLUDE 'common/frcptr'
      INCLUDE 'common/fratng'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_status/RCS/fstarc.f,v $
     . $',                                                             '
     .$Id: fstarc.f,v 1.5 2002/02/11 20:25:31 dws Exp $
     . $' /
C    ===================================================================
C
C
      IF (ITRACE.GE.1) WRITE (IODBUG,*) 'ENTER FSTARC'
C
C  PRINT PAGE HEADER
      IFIRST=0
      CALL FSTAHD (IFIRST)
      WRITE (IPR,40)
40    FORMAT ('0- RATING CURVE STATUS -')
C
C  FILL COMMON BLOCK FRCPTR
      IREC=1
      CALL UREADT (KFRCPT,IREC,IBUF,ISTAT)
      IF (ISTAT.NE.0) GO TO 130
C
C  GET NUMBER OF RATING CURVES
      NRC=IBUF(1)
C
      IF (NRC.EQ.0) THEN
         WRITE (IPR,20)
20       FORMAT ('0**NOTE** NO RATING CURVES ARE DEFINED.')
         GO TO 130
         ENDIF
C
C  SET MAXIMUM NUMBER OF RATING CURVES PER PAGE
      NMAX=75
C
      IPAGE=1
      NRCPG=1
      INEWPG=1
      NRCP=0
      NOBSOL=0
C
C  PROCESS EACH RATING CURVE
      DO 110 IREC=1,NRC
         CALL UREADT (KFRTCV,IREC,RTCVID,ISTAT)
         IF (ISTAT.NE.0) GO TO 130
         IF (INEWPG.EQ.1) THEN
C        PRINT HEADING FOR NEW PAGE
            IF (IREC.GT.1) CALL FSTAHD (IFIRST)
            INEWPG=0
            IF (IPAGE.EQ.1) THEN 
               ELSE
                  WRITE (IPR,50)
50          FORMAT ('0- RATING CURVE STATUS (CONTINUED) -')
               ENDIF
            WRITE (IPR,60)
60    FORMAT (
     *   ' ',5X,'IDENTIFIER',2X,'INTERPOLATION METHOD ',2X,
     *      'EXTRAPOLATION METHOD' /
     *   ' ',5X,10('-'),2X,21('-'),2X,21('-'))
            ENDIF
C     CHECK IF OBSOLETE
         CALL UMEMOV (RTCVID,CHKID,LEN(CHKID)/4)
         IF (CHKID.EQ.'OBSOLETE') THEN
C        ADD TO OBSOLETE COUNT - PRINT ONLY IF WANT OBSOLETES
            NOBSOL=NOBSOL+1
            IF (NOOBSO.EQ.1) GO TO 110
            ENDIF
         NRCP=NRCP+1
         ICOL=2
         IF (NCROSS.LE.0) THEN
            IF (INT(EMPTY(4)).EQ.1) ICOL=1
            WRITE (IPR,90) NRCP,RTCVID,REMARK(ICOL),REMARK(ICOL)
90       FORMAT (' ',I4,1X,2A4,2X,2X,A,2X,A)
            GO TO 100
            ENDIF
         ICOL1=2
         ICOL2=3
         IF (INT(EMPTY(1)).GT.0) THEN
            ICOL1=4
            ICOL2=4
            ENDIF
         IF ((INT(EMPTY(1)).EQ.0).AND.(INT(EMPTY(4)).EQ.1)) ICOL1=1
         WRITE (IPR,90) NRCP,RTCVID,REMARK(ICOL1),REMARK(ICOL2)
100      NRCPG=NRCPG+1
         IF (NRCPG.GT.NMAX) THEN
C        NEW PAGE
            INEWPG=1
            NRCPG=1
            IPAGE=IPAGE+1
            ENDIF
110      CONTINUE
C
      WRITE (IPR,120) NOBSOL
120   FORMAT ('0THERE ARE ',I3,' OBSOLETE RATING CURVES DEFINED')
C
130   IF (ITRACE.GE.1) WRITE (IODBUG,*) 'EXIT FSTARC'
C
      RETURN
C
      END
