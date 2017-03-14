C MODULE SLMAT
C-----------------------------------------------------------------------
C
C  ROUTINE FOR DELETING MAT AREAS.
C
      SUBROUTINE SLMAT (LARRAY,ARRAY,NFLD,ICKREF,ISTAT)
C
      CHARACTER*4 DTYPE,XDISP
      CHARACTER*8 AREAID,BASNID
      CHARACTER*8 BLNK8/' '/
      CHARACTER*20 CHAR
C
      DIMENSION ARRAY(LARRAY)
C
      INCLUDE 'uio'
      INCLUDE 'scommon/sudbgx'
      INCLUDE 'scommon/sworkx'
      INCLUDE 'scommon/swrk2x'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_delete/RCS/slmat.f,v $
     . $',                                                             '
     .$Id: slmat.f,v 1.5 1999/04/26 11:37:44 page Exp $
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
     *      ' LARRAY=',LARRAY,
     *      ' NFLD=',NFLD,
     *      ' '
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
      ICKALC=0
      IERALC=0
      NDELTE=0
      IFIRST=1
      NUMID1=0
      NUMID2=0
      NUMID3=0
      NAREA=0
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  CHECK FIELDS FOR IDENTIFIERS
C
10    CALL UFIELD (NFLD,ISTRT,LENGTH,ITYPE,NREP,INTEGR,REAL,LCHAR,
     *   CHAR,LLPAR,LRPAR,LASK,LATSGN,LAMPS,LEQUAL,IERR)
      IF (LDEBUG.GT.0) THEN
         CALL UPRFLD (NFLD,ISTRT,LENGTH,ITYPE,NREP,INTEGR,REAL,LCHAR,
     *      CHAR,LLPAR,LRPAR,LASK,LATSGN,LAMPS,LEQUAL,IERR)
         ENDIF
      IF (IERR.EQ.1) THEN
         IF (LDEBUG.GT.0) THEN
            WRITE (IOSDBG,50) NFLD
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
         IF (CHAR.NE.'MAT') THEN
            WRITE (LP,60) CHAR(1:LENSTR(CHAR))
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
      DTYPE='MAT'
C
      IF (ICKREF.EQ.1) THEN
C     CHECK IF AREA IS REFERENCED
         LARAY2=2000
         LARAY1=LARRAY-LARAY2
         CALL SLCHK (DTYPE,AREAID,LARAY1,ARRAY,LARAY2,ARRAY(LARAY1+1),
     *      LSWORK,SWORK,IFIRST,IXSORT,NUMID1,NUMID2,NUMID3,NUMERR,
     *      IERR)
         IF (IERR.NE.1) IFIRST=0
         IF (IERR.NE.0) GO TO 10
         ENDIF
C
      WRITE (LP,40)
      CALL SULINE (LP,1)
C
      NAREA=NAREA+1
C
      IFOUND=0
C
C  DELETE TIME SERIES FROM PROCESSED DATA BASE
      IFUT=0
      IPRERR=0
      CALL WPRDEL (AREAID,DTYPE,IFUT,ICKREF,IPRERR,IERR)
      IF (IERR.EQ.0) THEN
         WRITE (LP,120) DTYPE,AREAID(1:LENSTR(AREAID))
         CALL SULINE (LP,1)
         CALL SUDWRT (1,'PRD ',IERR)
         IFOUND=1
         ELSE
            IF (IERR.EQ.1) THEN
               WRITE (LP,80) DTYPE,AREAID(1:LENSTR(AREAID))
               CALL SUWRNS (LP,1,NUMWRN)
               ELSE
                  WRITE (LP,90) 'ERROR',IERR,'WPRDEL',
     *               DTYPE,'TIME SERIES',AREAID(1:LENSTR(AREAID))
                  CALL SUERRS (LP,2,NUMERR)
               ENDIF
         ENDIF
C
C  READ MAT PARAMETER RECORD
      IPTR=0
      CALL RPPREC (AREAID,DTYPE,IPTR,LSWRK2,SWRK2,NFILL,IPTRNX,
     *   IERR)
      IF (IERR.EQ.0) THEN
C     CHECK IF BASIN BOUNDARY USED
         CALL SUBSTR (SWRK2(11),1,8,BASNID,1)
         IF (BASNID.NE.' ') THEN
            IPTR=0
            CALL RPPREC (BASNID,'BASN',IPTR,LSWRK2,SWRK2,NFILL,IPTRNX,
     *         IERR)
            IF (IERR.NE.0) THEN
               CALL SRPPST (BASNID,'BASN',IPTR,LSWRK2,NFILL,IPTRNX,
     *            IERR)
               WRITE (LP,130) 'READING BASN',BASNID(1:LENSTR(BASNID))
               CALL SUERRS (LP,2,NUMERR)
               GO TO 10
               ENDIF
C        SET AREA IDENTIFIER TO BLANK
            CALL SUBSTR (BLNK8,1,8,SWRK2(16),1)
C        SET INDICATOR THAT BASIN AND AREA DEFINITION NOT CONSISTENT
            MATFLG=0
            SWRK2(19)=MATFLG+.01
            NPOS=NFILL
            CALL WPPREC (BASNID,'BASN',NPOS,SWRK2,IPTR,IERR)
            IF (IERR.NE.0) THEN
               CALL SWPPST (BASNID,'BASN',NPOS,IPTR,IERR)
               WRITE (LP,130) 'WRITING BASN',BASNID(1:LENSTR(BASNID))
               CALL SUERRS (LP,2,NUMERR)
               GO TO 10
               ENDIF
            WRITE (LP,140) 'BASN',BASNID(1:LENSTR(BASNID))
            CALL SULINE (LP,1)
            ENDIF
         ENDIF
C
C  DELETE PARAMETRIC DATA FROM PREPROCESSOR PARAMETRIC DATA BASE
      CALL WPPDEL (AREAID,DTYPE,IERR)
      IF (IERR.EQ.0) THEN
         WRITE (LP,150) DTYPE,AREAID(1:LENSTR(AREAID))
         CALL SULINE (LP,1)
         CALL SUDWRT (1,'PPP ',IERR)
         IFOUND=1
         ELSE
            IF (IERR.EQ.1) THEN
               WRITE (LP,100) DTYPE,AREAID(1:LENSTR(AREAID))
               CALL SUWRNS (LP,1,NUMWRN)
               ELSE
                  WRITE (LP,110) IERR,DTYPE,AREAID(1:LENSTR(AREAID))
                  CALL SUERRS (LP,2,NUMERR)
               ENDIF
            GO TO 10
         ENDIF
C
      IF (IFOUND.EQ.1) THEN
C     AREA SUCCESSFULLY DELETED
         WRITE (LP,160) DTYPE,AREAID(1:LENSTR(AREAID))
         CALL SULINE (LP,1)
         ENDIF
C
      NDELTE=NDELTE+1
C
      GO TO 10      
C      
C  PRINT NUMBER OF MAT AREAS DELETED
20    IF (NDELTE.EQ.0) THEN
         WRITE (LP,170) DTYPE
         CALL SULINE (LP,2)
         ELSE
            WRITE (LP,180) NDELTE,DTYPE
            CALL SULINE (LP,2)
         ENDIF
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,190)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
30    FORMAT (' *** ENTER SLMAT')
40    FORMAT (' ')
50    FORMAT (' NULL FIELD FOUND IN FIELD ',I2)
60    FORMAT ('0*** ERROR - IN SLMAT - ',A,' IS AN INVALID PARAMETER ',
     *   'TYPE.')
70    FORMAT ('0*** ERROR - ',A,' AREA CANNOT BE DELETED BECAUSE ',
     *   'USER GENERAL PARAMETERS NOT DEFINED.')
80    FORMAT (' *** WARNING - ',A,' TIME SERIES NOT FOUND FOR AREA ',
     *   A,'.')
90    FORMAT ('0*** ',A,' - STATUS CODE ',I3,' RETURNED BY ROUTINE ',A,
     *   ' WHILE DELETING ',A,' ',A,' FOR IDENTIFIER ',A,'.')
100   FORMAT (' *** WARNING - ',A,' PARAMETERS  NOT FOUND FOR AREA ',
     *   A,'.')
110   FORMAT ('0*** ERROR - STATUS CODE ',I3,' FROM WPPDEL ',
     *   'ENCOUNTERED WHILE DELETING ',A,' AREA ',A,'.')
120   FORMAT (' *** NOTE - ',A,' TIME SERIES SUCCESSFULLY DELETED ',
     *   'FOR AREA ',A,'.')
130   FORMAT ('0*** ERROR - IN SLMAT - ',A,' PARAMETERS FOR ',
     *   'AREA ',A,'.')
140   FORMAT (' *** NOTE - ',A,' PARAMETERS SUCCESSFULLY UPDATED ',
     *   'FOR BASIN ',A,'.')
150   FORMAT (' *** NOTE - ',A,' PARAMETERS SUCCESSFULLY DELETED ',
     *   'FOR AREA ',A,'.')
160   FORMAT (' *** NOTE - ',A,' AREA ',A,' SUCCESSFULLY DELETED.')
170   FORMAT ('0*** NOTE - NO ',A,' AREAS SUCCESSFULLY DELETED.')
180   FORMAT ('0*** NOTE - ',I3,' ',A,' AREAS SUCCESSFULLY DELETED.')
190   FORMAT (' *** EXIT SLMAT')
C
      END
