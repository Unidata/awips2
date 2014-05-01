C MODULE SETNSP
C-----------------------------------------------------------------------
C
C ROUTINE FOR SETTING OPTIONS NOT CURRENTLY AVAILABLE.
C
      SUBROUTINE SETNSP (NFLD,ISTAT)
C
      CHARACTER*20 STRNG,STRNG2
C
      INCLUDE 'uiox'
      INCLUDE 'scommon/sudbgx'
      INCLUDE 'scommon/suprtx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_misc/RCS/setnsp.f,v $
     . $',                                                             '
     .$Id: setnsp.f,v 1.3 2001/06/13 13:59:42 dws Exp $
     . $' /
C    ===================================================================
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'ENTER SETNSP'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG('SNSP')
C
      ISTAT=0
      NUMERR=0
      IENDIN=0
      IPRINT=1
C
      STRNG=' '
      STRNG2=' '
      LSTRNG=-LEN(STRNG)
      LSTRNG2=-LEN(STRNG2)
      ILPFND=0
      IRPFND=0
      NPIFLD=0
      NPCFLD=0
      ISTRT=1
C
C  PRINT CARD
      CALL SUPCRD
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  CHECK FIELDS FOR DEFINE OPTIONS
C
10    CALL UFIELD (NFLD,ISTRT,LENGTH,ITYPE,NREP,INTEGR,REAL,
     *   LSTRNG,STRNG,LLPAR,LRPAR,LASK,LATSGN,LAMPS,LEQUAL,IERR)
      IF (LDEBUG.GT.0) THEN
         CALL UPRFLD (NFLD,ISTRT,LENGTH,ITYPE,NREP,INTEGR,REAL,
     *      LSTRNG,STRNG,LLPAR,LRPAR,LASK,LATSGN,LAMPS,LEQUAL,IERR)
         ENDIF
      IF (IERR.NE.1) GO TO 20
         IF (LDEBUG.GT.0) THEN
            WRITE (IOSDBG,110) NFLD
            CALL SULINE (IOSDBG,1)
            ENDIF
         GO TO 10
C
C  CHECK FOR COMMAND
20    IF (LATSGN.EQ.1) THEN
         IENDIN=1
         GO TO 90
         ENDIF
C
      IF (NFLD.EQ.1) CALL SUPCRD
C
      CALL SUPFND (ILPFND,IRPFND,NPIFLD,NPCFLD)
      IF (LLPAR.GT.0) ILPFND=1
C
C  CHECK FOR PARENTHESIS IN FIELD
      IF (LLPAR.GT.0) CALL UFPACK (LSTRNG2,STRNG2,ISTRT,1,LLPAR-1,IERR)
      IF (LLPAR.EQ.0) CALL UFPACK (LSTRNG2,STRNG2,ISTRT,1,LENGTH,IERR)
C
C  CHECK IF TO HAVE NO NON-SUPPORTED CODES
      IF (STRNG.EQ.'NONE') THEN
         NSUPRT=0
         GO TO 10
         ENDIF
C
C  CHECK IF TO PRINT NON-SUPPORTED CODES
      IF (STRNG.EQ.'?') GO TO 90
C
C  ADD CODE TO ARRAY
      IF (NSUPRT+1.GT.MSUPRT) THEN
         WRITE (LP,140) MSUPRT
         CALL SUERRS (LP,2,NUMERR)
         GO TO 10
         ENDIF
      NSUPRT=NSUPRT+1
      SUPRT(NSUPRT)=STRNG
      IPRINT=1
      GO TO 10
C
90    IF (IPRINT.EQ.1) THEN
         IF (NSUPRT.GT.0) THEN
            WRITE (LP,150) (SUPRT(I),I=1,NSUPRT)
            CALL SULINE (LP,2)
            ENDIF
         IF (NSUPRT.EQ.0) THEN
            WRITE (LP,160)
            CALL SULINE (LP,2)
            ENDIF
         ENDIF
      IPRINT=0
      IF (IENDIN.EQ.0) GO TO 10
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'EXIT SETNSP'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
110   FORMAT (' BLANK STRING FOUND IN FIELD ',I2)
140   FORMAT ('0*** ERROR - MAXIMUM NUMBER OF CODES (',I2,') EXCEEDED.')
150   FORMAT ('0SETNSP CODES IN EFFECT : ',10(A,2X) / 26X,10(A,2X))
160   FORMAT ('0*** NOTE - NO CODES IN EFFECT.')
C
      END
