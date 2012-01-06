C MODULE SFUMDR
C-----------------------------------------------------------------------
C
C  ROUTINE TO DEFINE DEFINE GENERAL USER MDR PARAMETERS.
C
      SUBROUTINE SFUMDR (DISP,PRNOTE,INULL,NFLD,NUMFLD,INTEGR,ITYPE,
     *   NUMERR,NUMWRN,MDRCHK,MDRSUB,IOFSET,UNSD,ISTAT)
C
      CHARACTER*(*) DISP,PRNOTE
C
      DIMENSION MDRCHK(*),MDRSUB(*)
C
      INCLUDE 'uio'
      INCLUDE 'scommon/sudbgx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_define/RCS/sfumdr.f,v $
     . $',                                                             '
     .$Id: sfumdr.f,v 1.2 1998/04/07 15:14:18 page Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,230)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG('UGNL')
C
      ISTAT=0
C
      IUNSD=UNSD-.01
C
      NLFLD=NUMFLD-IOFSET
      GO TO (10,60,120,170),NLFLD
      WRITE (LP,240) NLFLD
      ISTAT=1
      GO TO 220
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  WESTERN MOST MDR COLUMN
C
10    IF (INULL.EQ.1) THEN
         MDRSUB(1)=IUNSD
         IF (PRNOTE.EQ.'YES') THEN
            WRITE (LP,260) NUMFLD,IUNSD
            CALL SULINE (LP,2)
            ENDIF
         GO TO 50
         ENDIF
      IF (DISP.EQ.'OLD') THEN
         IF (INTEGR.NE.MDRSUB(1)) THEN
            WRITE (LP,250) NLFLD,NUMFLD,NFLD
            CALL SUERRS (LP,2,NUMERR)
            ENDIF
         GO TO 220
         ENDIF
      IF (ITYPE.NE.0) THEN
         WRITE (LP,270) NUMFLD,NFLD
         CALL SUERRS (LP,2,NUMERR)
         GO TO 220
         ENDIF
      MDRSUB(1)=INTEGR
      MAXCOL=MDRCHK(1)+MDRCHK(2)
      IF (MDRSUB(1).GE.MDRCHK(1).AND.MDRSUB(1).LE.MAXCOL) GO TO 50
         WRITE (LP,290) MDRSUB(1),MAXCOL
         CALL SUWRNS (LP,2,NUMWRN)
         MDRSUB(1)=MDRCHK(1)
         WRITE (LP,380) MDRCHK(1)
         CALL SULINE (LP,1)
50    IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,300) MDRSUB(1)
         CALL SULINE (IOSDBG,1)
         ENDIF
      GO TO 220
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  NUMBER OF MDR COLUMNS
C
60    IF (INULL.EQ.1) THEN
         MDRSUB(2)=IUNSD
         IF (PRNOTE.EQ.'YES') THEN
            WRITE (LP,260) NUMFLD,IUNSD
            CALL SULINE (LP,2)
            ENDIF
         GO TO 100
         ENDIF
      IF (DISP.EQ.'OLD') THEN
         IF (INTEGR.NE.MDRSUB(2)) THEN
            WRITE (LP,250) NLFLD,NUMFLD,NFLD
            CALL SUERRS (LP,2,NUMERR)
            ENDIF
         GO TO 220
         ENDIF
      IF (ITYPE.NE.0) THEN
         WRITE (LP,270) NUMFLD,NFLD
         CALL SUERRS (LP,2,NUMERR)
         GO TO 220
         ENDIF
      MDRSUB(2)=INTEGR
      IF (MDRSUB(2).GT.0.AND.MDRSUB(2).LE.MDRCHK(2)) GO TO 100
         WRITE (LP,310) MDRSUB(2),MDRCHK(2)
         CALL SUWRNS (LP,2,NUMWRN)
         MDRSUB(2)=MDRCHK(2)
         WRITE (LP,380) MDRCHK(2)
         CALL SULINE (LP,1)
100   IF (MDRSUB(2).GT.MDRCHK(5)) THEN
         WRITE (LP,320) MDRSUB(2),MDRCHK(5)
         CALL SUERRS (LP,2,NUMERR)
         GO TO 220
         ENDIF
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,330) MDRSUB(2)
         CALL SULINE (IOSDBG,1)
         ENDIF
      GO TO 220
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  SOUTHERN MOST MDR ROW
C
120   IF (INULL.EQ.1) THEN
         MDRSUB(3)=IUNSD
         IF (PRNOTE.EQ.'YES') THEN
            WRITE (LP,260) NUMFLD,IUNSD
            CALL SULINE (LP,2)
            ENDIF
         GO TO 160
         ENDIF
      IF (DISP.EQ.'OLD') THEN
         IF (INTEGR.NE.MDRSUB(3)) THEN
            WRITE (LP,250) NLFLD,NUMFLD,NFLD
            CALL SUERRS (LP,2,NUMERR)
            ENDIF
         GO TO 220
         ENDIF
      IF (ITYPE.NE.0) THEN
         WRITE (LP,270) NUMFLD,NFLD
         CALL SUERRS (LP,2,NUMERR)
         GO TO 220
         ENDIF
      MDRSUB(3)=INTEGR
      MAXROW=MDRCHK(3)+MDRCHK(4)
      IF (MDRSUB(3).GE.MDRCHK(3).AND.MDRSUB(3).LE.MAXROW) GO TO 160
         WRITE (LP,340) MDRSUB(3),MAXROW
         CALL SUWRNS (LP,2,NUMWRN)
         MDRSUB(3)=MDRCHK(3)
         WRITE (LP,380) MDRCHK(3)
         CALL SULINE (LP,1)
160   IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,350) MDRSUB(3)
         CALL SULINE (IOSDBG,1)
         ENDIF
      GO TO 220
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  NUMBER OF MDR ROWS
C
170   IF (INULL.EQ.1) THEN
         MDRSUB(4)=IUNSD
         IF (PRNOTE.EQ.'YES') THEN
            WRITE (LP,260) NUMFLD,IUNSD
            CALL SULINE (LP,2)
            ENDIF
         GO TO 210
         ENDIF
      IF (DISP.EQ.'OLD') THEN
         IF (INTEGR.NE.MDRSUB(4)) THEN
            WRITE (LP,250) NLFLD,NUMFLD,NFLD
            CALL SUERRS (LP,2,NUMERR)
            ENDIF
         GO TO 220
         ENDIF
      IF (ITYPE.NE.0) THEN
         WRITE (LP,270) NUMFLD,NFLD
         CALL SUERRS (LP,2,NUMERR)
         GO TO 220
         ENDIF
      MDRSUB(4)=INTEGR
      IF (MDRSUB(4).GT.0.AND.MDRSUB(4).LE.MDRCHK(4)) GO TO 210
         WRITE (LP,360) MDRSUB(4),MDRCHK(4)
         CALL SUWRNS (LP,2,NUMWRN)
         MDRSUB(4)=MDRCHK(4)
         WRITE (LP,380) MDRCHK(4)
         CALL SULINE (LP,1)
210   IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,370) MDRSUB(4)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
220   IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,390)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
230   FORMAT (' *** ENTER SFUMDR')
240   FORMAT ('0*** ERROR - IN SFUMDR - ',I2,' IS AN INVALID FIELD ',
     *   'NUMBER.')
250   FORMAT ('0*** ERROR - MDR SUBSET PARAMETER NUMBER ',I2,
     *   ' IN INPUT FIELD ',I2,' (CARD FIELD ',I2,
     *   ') CANNOT BE CHANGED.')
260   FORMAT ('0*** NOTE - NO VALUE FOUND FOR REQUIRED FIELD ',I2,
     *   '. DEFAULT VALUE (',I8,') WILL BE USED.')
270   FORMAT ('0*** ERROR - INTEGER DATA EXPECTED IN INPUT FIELD ',
     *   I2,' (CARD FIELD ',I2,').')
290   FORMAT ('0*** WARNING - WESTERN MOST MDR COLUMN (',I4,
     *   ') EXCEEDS MAXIMUM MDR COLUMN ALLOWED (',I3,').')
300   FORMAT (' WESTERN MDR COLUMN SET TO ',I5)
310   FORMAT ('0*** WARNING - NUMBER OF MDR COLUMNS (',I4,
     *   ') EXCEEDS MAXIMUM MDR COLUMNS ALLOWED (',I3,').')
320   FORMAT ('0*** ERROR - NUMBER OF MDR COLUMNS (',I3,') EXCEEDS ',
     *   'MAXIMUM ALLOWABLE VALUE (',I2,').')
330   FORMAT (' NUMBER OF MDR COLUMNS SET TO ',I5)
340   FORMAT ('0*** WARNING - SOUTHERN MOST MDR ROW (',I4,
     *   ') EXCEEDS MAXIMUM MDR ROW ALLOWED (',I3,').')
350   FORMAT (' SOUTHERN MDR ROW SET TO ',I5)
360   FORMAT ('0*** WARNING - NUMBER OF MDR ROWS (',I4,
     *   ') EXCEEDS MAXIMUM MDR ROWS ALLOWED (',I3,').')
370   FORMAT (' NUMBER OF MDR ROWS SET TO ',I5)
380   FORMAT (T16,'THE DEFAULT VALUE (',I3,') WILL BE USED.')
390   FORMAT (' *** EXIT SFUMDR')
C
      END
