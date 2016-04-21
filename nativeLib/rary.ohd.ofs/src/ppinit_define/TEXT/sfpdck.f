C MODULE SFPDCK
C-----------------------------------------------------------------------
C
C  ROUTINE TO CHECK AVAILABLE SPACE IN PREPROCESSOR DATA BASE.
C
      SUBROUTINE SFPDCK (STAID,ICSTAN,
     *   NGPS,GPS,NGPSN,GPSN,
     *   MDLYTP,DLYTYP,NDLYTP,
     *   ITPPVR,
     *   ITYOBS,ITTAVR,ITFMM,
     *   RRSTYP,NRRSTP,IRSTAT,NVLPOB,NUMOBS,
     *   NUMERR,NUMWRN,ISTAT)
C
      CHARACTER*4 GPSN(NGPSN)
C
C  STAN PARAMETER ARRAYS
      INCLUDE 'scommon/dimstan'
C
C  RRS PARAMETER ARRAYS
      INCLUDE 'scommon/dimrrs'
      DIMENSION IRSTAT(NRRSTP)
      CHARACTER*4 RRSNEW
      DIMENSION RRSNEW(MRRSTP),NPRNEW(MRRSTP),NOBNEW(MRRSTP)
C
C  PREPROCESSOR DATA BASE READ/WRITE ARRAYS
      CHARACTER*4 DLYTYP(MDLYTP)
      PARAMETER (MDLNEW=25)
      CHARACTER*4 DLYNEW(MDLNEW)
      DIMENSION IADDLY(MDLNEW)
      PARAMETER (MRRNEW=MRRSTP)
      DIMENSION IADRRS(MRRNEW)
C
      PARAMETER (MEROR1=20,MEROR2=3)
      DIMENSION IEROR(MEROR1,MEROR2)
C
      INCLUDE 'uio'
      INCLUDE 'scommon/sudbgx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_define/RCS/sfpdck.f,v $
     . $',                                                             '
     .$Id: sfpdck.f,v 1.2 1998/04/07 15:08:27 page Exp $
     . $' /
C    ===================================================================
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,200)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG('DEFN')
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,210) STAID,ICSTAN,ITPPVR,ITYOBS,ITTAVR,ITFMM
         CALL SULINE (IOSDBG,1)
         WRITE (IOSDBG,220) NGPS,(GPS(I),I=1,NGPS)
         CALL SULINE (IOSDBG,1)
         WRITE (IOSDBG,230) NGPSN,(GPSN(I),I=1,NGPSN)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      ISTAT=0
C
      CALL UMEMST (0,IEROR,MEROR1*MEROR2)
C
C  CHECK IF PREPROCESSOR DATA BASE ALLOCATED
      IDPPD=1
      CALL SUDALC (0,0,0,IDPPD,0,0,0,0,0,0,NUMERR,IERR)
C
C  CHECK DATA GROUPS SPECIFIED
      IPCPN=0
      ITEMP=0
      IPE=0
      IRRS=0
      DO 10 I=1,NGPSN
         IF (GPSN(I).EQ.'PCPN') IPCPN=1
         IF (GPSN(I).EQ.'TEMP') ITEMP=1
         IF (GPSN(I).EQ.'PE') IPE=1
         IF (GPSN(I).EQ.'RRS') IRRS=1
10       CONTINUE
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  DETERMINE DAILY DATA TYPES TO BE DEFINED
C
      NDLYTP=0
      DO 20 I=1,NGPSN
         IF (GPSN(I).EQ.'PCPN') THEN
            IF (NDLYTP+1.GT.MDLYTP) GO TO 30
            NDLYTP=NDLYTP+1
            DLYTYP(NDLYTP)='PP24'
            IF (ITPPVR.EQ.6) THEN
               IF (NDLYTP+1.GT.MDLYTP) GO TO 30
               NDLYTP=NDLYTP+1
               DLYTYP(NDLYTP)='PP06'
               ENDIF
            IF (ITPPVR.EQ.3) THEN
               IF (NDLYTP+1.GT.MDLYTP) GO TO 30
               NDLYTP=NDLYTP+1
               DLYTYP(NDLYTP)='PP03'
               ENDIF
            IF (ITPPVR.EQ.1) THEN
               IF (NDLYTP+1.GT.MDLYTP) GO TO 30
               NDLYTP=NDLYTP+1
               DLYTYP(NDLYTP)='PP01'
               ENDIF
            GO TO 20
            ENDIF
         IF (GPSN(I).EQ.'TEMP') THEN
            IF (NDLYTP+1.GT.MDLYTP) GO TO 30
            NDLYTP=NDLYTP+1
            DLYTYP(NDLYTP)='TM24'
            IF (ITYOBS.EQ.4) GO TO 20
               IF (ITYOBS.NE.1.AND.ITTAVR.EQ.6) THEN
                  IF (NDLYTP+1.GT.MDLYTP) GO TO 30
                  NDLYTP=NDLYTP+1
                  DLYTYP(NDLYTP)='TA06'
                  ENDIF
               IF (ITYOBS.NE.1.AND.ITTAVR.EQ.3) THEN
                  IF (NDLYTP+1.GT.MDLYTP) GO TO 30
                  NDLYTP=NDLYTP+1
                  DLYTYP(NDLYTP)='TA03'
                  ENDIF
               IF (ITYOBS.NE.1.AND.ITTAVR.EQ.1) THEN
                  IF (NDLYTP+1.GT.MDLYTP) GO TO 30
                  NDLYTP=NDLYTP+1
                  DLYTYP(NDLYTP)='TA01'
                  ENDIF
               IF (ITFMM.EQ.1) THEN
                  IF (NDLYTP+1.GT.MDLYTP) GO TO 30
                  NDLYTP=NDLYTP+1
                  DLYTYP(NDLYTP)='TF24'
                  ENDIF
            GO TO 20
            ENDIF
         IF (GPSN(I).EQ.'PE') THEN
            IF (NDLYTP+1.GT.MDLYTP) GO TO 30
            NDLYTP=NDLYTP+1
            DLYTYP(NDLYTP)='EA24'
            GO TO 20
            ENDIF
20       CONTINUE
      GO TO 40
C
C  MAXIMUM DAILY DATA TYPES EXCEEDED
30    WRITE (LP,240) MDLYTP
      CALL SUERRS (LP,2,NUMERR)
C
C  CHECK NUMBER OF DAILY AND RRS TYPES
40    IF (NDLYTP.EQ.0.AND.NRRSTP.EQ.0) THEN
         WRITE (LP,250) STAID
         CALL SUERRS (LP,2,NUMERR)
         ISTAT=1
         GO TO 190
         ENDIF
C
      IF (LDEBUG.GT.0) THEN
         IF (NDLYTP.GT.0) THEN
            WRITE (IOSDBG,260) STAID
            CALL SULINE (IOSDBG,1)
            DO 50 I=1,NDLYTP
               WRITE (IOSDBG,270) I,DLYTYP(I)
               CALL SULINE (LP,1)
50             CONTINUE
            ENDIF
         IF (NRRSTP.GT.0) THEN
            WRITE (IOSDBG,290) STAID
            CALL SULINE (IOSDBG,1)
            DO 60 I=1,NRRSTP
               WRITE (IOSDBG,280) I,RRSTYP(I),NVLPOB(I),NUMOBS(I)
               CALL SULINE (IOSDBG,1)
60             CONTINUE
            ENDIF
         ENDIF
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  PROCESS DAILY TYPES
C
      NDLNEW=0
      IF (NGPSN.EQ.0) GO TO 140
C
C  GET LIST OF NEW DAILY DATA TYPES
      DO 90 I=1,NGPSN
         IF (ICSTAN.EQ.1) GO TO 80
         IF (NGPS.EQ.0) GO TO 80
         DO 70 J=1,NGPS
            IF (GPSN(I).EQ.GPS(J)) GO TO 90
70          CONTINUE
80       IF (GPSN(I).EQ.'PCPN') THEN
            IF (NDLNEW+1.GT.MDLNEW) GO TO 100
            NDLNEW=NDLNEW+1
            DLYNEW(NDLNEW)='PP24'
            IF (ITPPVR.EQ.6) THEN
               IF (NDLNEW+1.GT.MDLNEW) GO TO 100
               NDLNEW=NDLNEW+1
               DLYNEW(NDLNEW)='PP06'
               ENDIF
            IF (ITPPVR.EQ.3) THEN
               IF (NDLNEW+1.GT.MDLNEW) GO TO 100
               NDLNEW=NDLNEW+1
               DLYNEW(NDLNEW)='PP03'
               ENDIF
            IF (ITPPVR.EQ.1) THEN
               IF (NDLNEW+1.GT.MDLNEW) GO TO 100
               NDLNEW=NDLNEW+1
               DLYNEW(NDLNEW)='PP01'
               ENDIF
            GO TO 90
            ENDIF
         IF (GPSN(I).EQ.'TEMP') THEN
            IF (NDLNEW+1.GT.MDLNEW) GO TO 100
            NDLNEW=NDLNEW+1
            DLYNEW(NDLNEW)='TM24'
            IF (ITYOBS.EQ.4) GO TO 90
               IF (ITYOBS.NE.1.AND.ITTAVR.EQ.6) THEN
                  IF (NDLNEW+1.GT.MDLNEW) GO TO 100
                  NDLNEW=NDLNEW+1
                  DLYNEW(NDLNEW)='TA06'
                  ENDIF
               IF (ITYOBS.NE.1.AND.ITTAVR.EQ.3) THEN
                  IF (NDLNEW+1.GT.MDLNEW) GO TO 100
                  NDLNEW=NDLNEW+1
                  DLYNEW(NDLNEW)='TA03'
                  ENDIF
               IF (ITYOBS.NE.1.AND.ITTAVR.EQ.1) THEN
                  IF (NDLNEW+1.GT.MDLNEW) GO TO 100
                  NDLNEW=NDLNEW+1
                  DLYNEW(NDLNEW)='TA01'
                  ENDIF
               IF (ITFMM.EQ.1) THEN
                  IF (NDLNEW+1.GT.MDLNEW) GO TO 100
                  NDLNEW=NDLNEW+1
                  DLYNEW(NDLNEW)='TF24'
                  ENDIF
            GO TO 90
            ENDIF
         IF (GPSN(I).EQ.'PE') THEN
            IF (NDLNEW+1.GT.MDLNEW) GO TO 100
            NDLNEW=NDLNEW+1
            DLYNEW(NDLNEW)='EA24'
            ENDIF
            GO TO 90
90       CONTINUE
      GO TO 110
C
C  MAXIMUM DAILY DATA TYPES EXCEEDED
100   WRITE (LP,300) MDLYTP
      CALL SUERRS (LP,2,NUMERR)
C
110   IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,370) NDLNEW,(DLYNEW(I),I=1,NDLNEW)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      IF (NDLNEW.EQ.0) GO TO 140
C
      CALL SUDOPN (1,'PPD ',IERR)
      IF (IERR.GT.0) GO TO 140
C
      IF (NDLNEW.GT.MEROR1) THEN
         WRITE (LP,320) NDLNEW,MEROR1
         CALL SUERRS (LP,2,NUMERR)
         ISTAT=1
         GO TO 140
         ENDIF
C
C  CHECK FOR ENOUGH SPACE TO STORE NEW DAILY DATA TYPES
      CALL RPDCKD (NDLNEW,DLYNEW,IADDLY,IEROR,IERR)
      IF (IERR.GT.0) THEN
         ISTAT=1
         IF (IERR.EQ.1) THEN
            WRITE (LP,330) 'THE FOLLOWING DAILY',STAID,':'
            CALL SUERRS (LP,2,NUMERR)
            DO 120 I=1,NDLYTP
               IF (IADDLY(I).EQ.0) GO TO 120
                  WRITE (LP,340) DLYTYP(I)
                  CALL SULINE (LP,1)
120            CONTINUE
            DO 130 I=1,NDLYTP
               IF (IEROR(I,1).EQ.1) THEN
                  WRITE (LP,360) DLYTYP(I),'ENTRIES'
                  CALL SULINE (LP,2)
                  ENDIF
               IF (IEROR(I,2).EQ.1) THEN
                  WRITE (LP,360) DLYTYP(I),'POINTER WORDS'
                  CALL SULINE (LP,2)
                  ENDIF
               IF (IEROR(I,3).EQ.1) THEN
                  WRITE (LP,360) DLYTYP(I),'DATA WORDS'
                  CALL SULINE (LP,2)
                  ENDIF
130            CONTINUE
            GO TO 140
            ENDIF
         IF (IERR.EQ.2) THEN
            WRITE (LP,330) 'ALL DAILY',STAID,'.'
            CALL SUERRS (LP,2,NUMERR)
            GO TO 140
            ENDIF
         WRITE (LP,350) IERR,STAID
         CALL SUERRS (LP,2,NUMERR)
         GO TO 140
       ENDIF
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  PROCESS RRS TYPES
C
140   NRRNEW=0
      IF (NRRSTP.EQ.0) GO TO 190
C
C  GET LIST OF NEW RRS DATA TYPES
      DO 150 I=1,NRRSTP
         IF (IRSTAT(I).EQ.0.OR.
     *       IRSTAT(I).EQ.2.OR.
     *       IRSTAT(I).EQ.3) THEN
            NRRNEW=NRRNEW+1
            IF (NRRNEW.GT.MRRNEW) GO TO 160
               RRSNEW(NRRNEW)=RRSTYP(I)
               NPRNEW(NRRNEW)=NVLPOB(I)
               NOBNEW(NRRNEW)=NUMOBS(I)
            ENDIF
150      CONTINUE
      GO TO 170
C
C  MAXIMUM NEW RRS DATA TYPES EXCEEDED
160   WRITE (LP,310) MRRNEW
      CALL SUERRS (LP,2,NUMERR)
C
170   IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,380) NRRNEW,(RRSNEW(I),I=1,NRRNEW)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      IF (NRRNEW.EQ.0) GO TO 190
C
      CALL SUDOPN (1,'PPD ',IERR)
      IF (IERR.GT.0) GO TO 190
C
C  CHECK FOR ENOUGH SPACE TO STORE NEW RRS DATA TYPES
      CALL RPDCKR (NRRNEW,RRSNEW,NPRNEW,NOBNEW,IADRRS,IERR)
      IF (IERR.GT.0) THEN
         ISTAT=1
         IF (IERR.EQ.1) THEN
            WRITE (LP,330) 'THE FOLLOWING RRS',STAID,':'
            CALL SUERRS (LP,2,NUMERR)
            DO 180 I=1,NRRSTP
               IF (IADRRS(I).EQ.0) GO TO 180
                  WRITE (LP,340) RRSTYP(I)
                  CALL SULINE (LP,1)
180            CONTINUE
            GO TO 190
            ENDIF
         IF (IERR.EQ.2) THEN
            WRITE (LP,330) 'ALL RRS',STAID,'.'
            CALL SUERRS (LP,2,NUMERR)
            GO TO 190
            ENDIF
         WRITE (LP,390) IERR,STAID
         CALL SUERRS (LP,2,NUMERR)
         GO TO 190
         ENDIF
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  CHECK FOR ENOUGH ROOM FOR STATION INFORMATION FILE RECORDS
C
190   CALL RPDCKS (NDLYTP,DLYTYP,NRRSTP,RRSTYP,IERR)
      IF (IERR.GT.0) THEN
         WRITE (LP,400) STAID
         CALL SUERRS (LP,2,NUMERR)
         ISTAT=1
         ENDIF
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,410) ISTAT
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
200   FORMAT (' *** ENTER SFPDCK')
210   FORMAT (' STAID=',A,3X,'ICSTAN=',I2,3X,'ITPPVR=',I2,3X,
     *   'ITYOBS=',I2,3X,'ITTAVR=',I2,3X,'ITFMM=',I2)
220   FORMAT (' NGPS=',I2,3X,'GPS=',5(A4,1X))
230   FORMAT (' NGPSN=',I2,3X,'GPSN=',5(A4,1X))
240   FORMAT ('0*** ERROR - IN SFPDCK - MAXIMUM NUMBER OF DAILY DATA ',
     *   'TYPES (',I2,') EXCEEDED.')
250   FORMAT ('0*** ERROR - IN SFPDCK - STATION ',A,' HAS NO DAILY ',
     *   'OR RRS DATA TYPES.')
260   FORMAT (' DAILY DATA TYPES FOR STATION ',A)
270   FORMAT (' I=',I2,3X,'DLYTYP=',A4)
280   FORMAT (' I=',I2,3X,'RRSTYP=',A4,3X,'NVLPOB=',I3,3X,
     *   'NUMOBS=',I3)
290   FORMAT (' RRS DATA TYPES FOR STATION ',A)
300   FORMAT ('0*** ERROR - IN SFPDCK - MAXIMUM NUMBER OF NEW DAILY ',
     *   'DATA TYPES (',I2,') EXCEEDED.')
310   FORMAT ('0*** ERROR - IN SFPDCK - MAXIMUM NUMBER OF NEW RRS ',
     *   'DATA TYPES (',I2,') EXCEEDED.')
320   FORMAT ('0*** ERROR - IN SFPDCK - NUMBER OF DAILY DATA TYPES (',
     *   I2,' EXCEEDS SIZE OF ARRAY IEROR (',I2,').')
330   FORMAT ('0*** ERROR - SPACE NOT AVAILABLE IN THE ',
     *   'PREPROCESSOR DATA BASE FOR ',A,
     *   ' DATA TYPES FOR STATION ',A,A)
340   FORMAT (T15,A4)
350   FORMAT ('0*** ERROR - IN SFPDCK - STATUS CODE RETURNED FROM ',
     *   'ROUTINE RPDCKD (',I3,') FOR STATION ',A,' NOT RECOGNIZED.')
360   FORMAT ('0*** NOTE - SPACE NOT AVAILABLE FOR DAILY DATA TYPE ',A4,
     *   ' BECAUSE MAXIMUM ',A,' EXCEEDED.')
370   FORMAT (' NDLNEW=',I2,3X,'DLYNEW=',10(A4,1X))
380   FORMAT (' NRRNEW=',I2,3X,'RRSNEW=',10(A4,1X))
390   FORMAT ('0*** ERROR - IN SFPDCK - STATUS CODE RETURNED FROM ',
     *   'ROUTINE RPDCKR (',I3,') FOR STATION ',A,' NOT RECOGNIZED.')
400   FORMAT ('0*** ERROR - SPACE NOT AVAILABLE FOR ',
     *   'STATION INFORMATION FILE RECORDS FOR STATION ',A,'.')
410   FORMAT (' *** EXIT SFPDCK - STATUS CODE=',I2)
C
      END
