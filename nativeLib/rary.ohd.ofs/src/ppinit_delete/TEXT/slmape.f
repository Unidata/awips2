C MODULE SLMAPE
C-----------------------------------------------------------------------
C
C  ROUTINE FOR DELETING MAPE AREAS.
C
      SUBROUTINE SLMAPE (LARRAY,ARRAY,NFLD,ICKREF,ISTAT)
C
      CHARACTER*4 DTYPE
      CHARACTER*8 AREAID
      CHARACTER*20 CHAR
C
      DIMENSION ARRAY(LARRAY)
C
      INCLUDE 'uio'
      INCLUDE 'scommon/sudbgx'
      INCLUDE 'scommon/sworkx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_delete/RCS/slmape.f,v $
     . $',                                                             '
     .$Id: slmape.f,v 1.4 1999/04/26 11:36:24 page Exp $
     . $' /
C    ===================================================================
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,30)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG('DELT')
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*)
     *     ' LARRAY=',LARRAY,
     *     ' NFLD=',NFLD,
     *     ' '
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      ISTAT=0
C
      ISTRT=-1
      LCHAR=LEN(CHAR)/4
      NUMFLD=0
      NUMERR=0
      NUMWRN=0
      IERALC=0
      ICKALC=0
      NDELTE=0
      IFIRST=1
      NUMID1=0
      NUMID2=0
      NUMID3=0
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  GET NEXT INPUT FIELD
C
10    CALL UFIELD (NFLD,ISTRT,LENGTH,ITYPE,NREP,INTEGR,REAL,LCHAR,
     *   CHAR,LLPAR,LRPAR,LASK,LATSGN,LAMPS,LEQUAL,IERR)
      IF (LDEBUG.GT.0) THEN
         CALL UPRFLD (NFLD,ISTRT,LENGTH,ITYPE,NREP,INTEGR,REAL,LCHAR,
     *      CHAR,LLPAR,LRPAR,LASK,LATSGN,LAMPS,LEQUAL,IERR)
         ENDIF
      IF (IERR.EQ.1) THEN
         IF (LDEBUG.GT.0) THEN
            WRITE (IOSDBG,60) NFLD
            CALL SULINE (IOSDBG,1)
            ENDIF
         GO TO 10
         ENDIF
C
C  CHECK FOR END OF INPUT
      IF (NFLD.EQ.-1) GO TO 20
C
C  CHECK FOR COMMAND
      IF (LATSGN.EQ.1) GO TO 20
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'CHAR=',CHAR
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      NUMFLD=NUMFLD+1
C
      IF (NUMFLD.EQ.1) THEN
         IF (NFLD.EQ.1) CALL SUPCRD
C     CHECK PARAMETER TYPE
         IF (CHAR.NE.'MAPE') THEN
            WRITE (LP,50) CHAR(1:LENSTR(CHAR))
            CALL SUERRS (LP,2,NUMERR)
            ENDIF
         GO TO 10
         ENDIF
C
C  CHECK FOR KEYWORD
      CALL SUIDCK ('DELT',CHAR,NFLD,0,IKEYWD,IERR)
      IF (IERR.EQ.2) GO TO 20
C
      IF (NFLD.EQ.1) CALL SUPCRD
C
      IF (IERALC.GT.0) GO TO 10
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
      IF (ICKALC.EQ.0) THEN
C     CHECK IF DATA BASES ALLOCATED
         CALL SUDOPN (1,'PPP ',IERR1)
         CALL SUDOPN (1,'PRD ',IERR2)
         CALL SUDOPN (1,'FC  ',IERR3)
         ICKALC=1
         IF (IERR1.EQ.0.AND.IERR2.EQ.0.AND.IERR3.EQ.0) THEN
            ELSE
               IERALC=1
               GO TO 10
            ENDIF
         ENDIF
C
      AREAID=CHAR
      DTYPE='MAPE'
C
      IF (ICKREF.EQ.1) THEN
C     CHECK IF AREA IS REFERENCED
         LARAY2=1000
         LARAY1=LARRAY-LARAY2
         CALL SLCHK (DTYPE,AREAID,LARAY1,ARRAY,LARAY2,ARRAY(LARAY1+1),
     *      LSWORK,SWORK,IFIRST,IXSORT,NUMID1,NUMID2,NUMID3,NUMERR,IERR)
         IF (IERR.NE.1) IFIRST=0
         IF (IERR.NE.0) GO TO 10
         ENDIF
C
      WRITE (LP,40)
      CALL SULINE (LP,1)
C
      IFOUND=0
C
C  DELETE PARAMETRIC DATA FROM PREPROCESSOR PARAMETRIC DATA BASE
      CALL WPPDEL (AREAID,DTYPE,IERR)
      IF (IERR.EQ.0) THEN
         IF (LDEBUG.GT.0) THEN
            WRITE (IOSDBG,100) 'PARAMETERS',AREAID
            CALL SULINE (LP,1)
            ENDIF
         CALL SUDWRT (1,'PPP ' ,IERR)
         IFOUND=1
         ELSE
            IF (IERR.EQ.1) THEN
               WRITE (LP,70) DTYPE,AREAID(1:LENSTR(AREAID))
               CALL SUWRNS (LP,2,NUMWRN)
               ELSE
                  WRITE (LP,90) 'ERROR',IERR,'WPPDEL',DTYPE,
     *               AREAID(1:LENSTR(AREAID))
                  CALL SUERRS (LP,2,NUMERR)
               GO TO 10
               ENDIF
         ENDIF
C
C  DELETE TIME SERIES FROM PROCESSED DATA BASE
      IFUT=0
      IPRERR=0
      CALL WPRDEL (AREAID,DTYPE,IFUT,ICKREF,IPRERR,IERR)
      IF (IERR.EQ.0) THEN
         IF (LDEBUG.GT.0) THEN
            WRITE (IOSDBG,100) 'TIME SERIES',DTYPE,AREAID
            CALL SULINE (IOSDBG,1)
            ENDIF
         CALL SUDWRT (1,'PRD ' ,IERR)
         IFOUND=1
         ELSE
            IF (IERR.EQ.1) THEN
               WRITE (LP,80) DTYPE,AREAID(1:LENSTR(AREAID))
               CALL SUWRNS (LP,1,NUMWRN)
               ELSE
                  WRITE (LP,90) 'ERROR',IERR,'WPRDEL',
     *              AREAID(1:LENSTR(AREAID))
                  CALL SUERRS (LP,2,NUMERR)
               ENDIF
         ENDIF
C
      IF (IFOUND.EQ.1) THEN
C     AREA SUCCESSFULLY DELETED
         WRITE (LP,110) DTYPE,AREAID
         CALL SULINE (LP,2)
         NDELTE=NDELTE+1
         ENDIF
C
      GO TO 10
C
C  PRINT NUMBER OF AREAS DELETED
20    IF (NDELTE.EQ.0) THEN
         WRITE (LP,120) DTYPE
         CALL SULINE (LP,2)
         ELSE
            WRITE (LP,130) NDELTE,DTYPE
            CALL SULINE (LP,2)
         ENDIF
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,140)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
30    FORMAT (' *** ENTER SLMAPE')
40    FORMAT (' ')
60    FORMAT (' NULL FIELD FOUND IN FIELD ',I2)
50    FORMAT ('0*** ERROR - IN SLMAPE - ',A,' IS AN INVALID PARAMETER ',
     *   'TYPE.')
70    FORMAT (' *** WARNING - ',A,' PARAMETERS  NOT FOUND FOR AREA ',A,
     *   '.')
80    FORMAT (' *** WARNING - ',A,' TIME SERIES NOT FOUND FOR AREA ',A,
     *   '.')
90    FORMAT ('0*** ',A,' - STATUS CODE ',I3,' RETURNED BY ROUTINE ',A,
     *   ' WHILE DELETING ',A,' AREA ',A,'.')
100   FORMAT (' ',A,' DELETED FOR ',A,' AREA ',A)
110   FORMAT ('0*** NOTE - ',A,' AREA ',A,' SUCCESSFULLY DELETED.')
120   FORMAT ('0*** NOTE - NO ',A,' AREAS SUCCESSFULLY DELETED.')
130   FORMAT ('0*** NOTE - ',I3,' ',A,' AREAS SUCCESSFULLY DELETED.')
140   FORMAT (' *** EXIT SLMAPE')
C
      END
