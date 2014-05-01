C MODULE SLFMAP
C-----------------------------------------------------------------------
C
C  ROUTINE TO DELETE FMAP AREAS.
C
      SUBROUTINE SLFMAP (LARRAY,ARRAY,NFLD,ICKREF,IOAUTO,ISTAT)
C
      CHARACTER*4 DTYPE,DTYPER
      CHARACTER*8 AREAID
      CHARACTER*20 STRNG
C
      DIMENSION ARRAY(LARRAY)
C
      DIMENSION UNUSED(10)
C
      INCLUDE 'uiox'
      INCLUDE 'scommon/sudbgx'
      INCLUDE 'scommon/sordrx'
      INCLUDE 'scommon/sworkx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_delete/RCS/slfmap.f,v $
     . $',                                                             '
     .$Id: slfmap.f,v 1.8 2002/02/11 21:02:55 dws Exp $
     . $' /
C    ===================================================================
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'ENTER SLFMAP'
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
      LSTRNG=LEN(STRNG)/4
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
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  CHECK FIELDS FOR IDENTIFIERS
C
10    CALL UFIELD (NFLD,ISTRT,LENGTH,ITYPE,NREP,INTEGR,REAL,
     *   LSTRNG,STRNG,LLPAR,LRPAR,LASK,LATSGN,LAMPS,LEQUAL,IERR)
      IF (LDEBUG.GT.0) THEN
         CALL UPRFLD (NFLD,ISTRT,LENGTH,ITYPE,NREP,INTEGR,REAL,
     *      LSTRNG,STRNG,LLPAR,LRPAR,LASK,LATSGN,LAMPS,LEQUAL,IERR)
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
      IF (NFLD.EQ.-1) GO TO 30
C
C  CHECK FOR COMMAND
      IF (LATSGN.EQ.1) GO TO 30
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'STRNG=',STRNG
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      NUMFLD=NUMFLD+1
C
      IF (NUMFLD.EQ.1) THEN
         IF (NFLD.EQ.1) CALL SUPCRD
C     CHECK PARAMETER TYPE
         IF (STRNG.NE.'FMAP') THEN
            WRITE (LP,60) STRNG(1:LENSTR(STRNG))
            CALL SUERRS (LP,2,NUMERR)
            ENDIF
         GO TO 10
         ENDIF
C
C  CHECK FOR KEYWORD
      CALL SUIDCK ('DELT',STRNG,NFLD,0,IKEYWD,IERR)
      IF (IERR.EQ.2) GO TO 30
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
      AREAID=STRNG
      DTYPE='FMAP'
C
      IF (ICKREF.EQ.1) THEN
C     CHECK IF AREA IS REFERENCED
         LARAY2=1000
         LARAY1=LARRAY-LARAY2
         CALL SLCHK (DTYPE,AREAID,LARAY1,ARRAY,LARAY2,ARRAY(LARAY1+1),
     *      LSWORK,SWORK,IFIRST,IXSORT,NUMID1,NUMID2,NUMID3,NUMERR,
     *      IERR)
         IF (IERR.NE.1) IFIRST=0
         IF (IERR.NE.0) GO TO 10
         ENDIF
C
C  DELETE TIME SERIES FROM PROCESSED DATA BASE
      DTYPER='MAP'
      IFUT=1
      IPRERR=0
      CALL WPRDEL (AREAID,DTYPER,IFUT,ICKREF,IPRERR,IERR)
      IF (IERR.EQ.0) THEN
         WRITE (LP,100) DTYPE,AREAID(1:LENSTR(AREAID))
         CALL SULINE (LP,2)
         NDELTE=NDELTE+1
         CALL SUDWRT (1,'PRD ',IERR)
         ELSE
            IF (IERR.EQ.1) THEN
               WRITE (LP,70) DTYPE,AREAID(1:LENSTR(AREAID))
               CALL SUWRNS (LP,2,NUMWRN)
               ELSE
                  WRITE (LP,80) 'ERROR',IERR,'WPRDEL',
     *              'DELETING FMAP TIME SERIES FOR AREA',
     *              AREAID(1:LENSTR(AREAID))
                  CALL SUERRS (LP,2,NUMERR)
               ENDIF
            GO TO 10
         ENDIF
C
C  READ FUTURE MAP COMPUTATIONAL ORDER
      IPRERR=0
      CALL SRFMPO (LARRAY,ARRAY,IVFMPO,UNUSED,MFMPID,FMPID,NFMPID,
     *   IPRERR,IERR)
      IF (IERR.NE.0) THEN
         IF (IERR.EQ.2) THEN
            WRITE (LP,120)
            CALL SULINE(LP,2)
            ELSE
               WRITE (LP,80) 'ERROR',IERR,'SRFMPO',
     *           'READING FMPO PARAMETER RECORD.'
               CALL SUERRS (LP,2,NUMERR)
            ENDIF
         GO TO 10
         ENDIF
C
      IF (LDEBUG.GT.0) THEN
         IPRNT=1
         CALL SPFMPO (IPRNT,IVFMPO,FMPID,NFMPID,IERR)
         IF (IERR.NE.0) THEN
            WRITE (LP,80) 'ERROR',IERR,'SPFMPO',
     *           'PRINTING FMPO PARAMETER RECORD.'
            CALL SUERRS (LP,2,NUMERR)
            GO TO 10
            ENDIF
         ENDIF
C
C  CHECK IF FMAP IDENTIFIER FOUND IN PARAMETER ARRAY
      DO 20 I=1,NFMPID
         IF (AREAID.EQ.FMPID(I)) THEN
            WRITE (LP,110) DTYPE,AREAID
            CALL SULINE (LP,2)
            IOAUTO=1
            GO TO 10
            ENDIF
20       CONTINUE
C
      GO TO 10
C
C  PRINT NUMBER OF AREAS DELETED
30    IF (NDELTE.EQ.0) THEN
         WRITE (LP,130) DTYPE
         CALL SULINE (LP,2)
         ELSE
            WRITE (LP,140) NDELTE,DTYPE
            CALL SULINE (LP,2)
         ENDIF
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'EXIT SLFMAP'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
50    FORMAT (' NULL FIELD FOUND IN FIELD ',I2)
60    FORMAT ('0*** ERROR - IN SLFMAP - ',A,' IS AN INVALID ',
     *   'PARAMETER TYPE.')
70    FORMAT ('0*** WARNING - ',A,' TIME SERIES NOT FOUND FOR AREA ',
     *   A,'.')
80    FORMAT ('0*** ',A,' - STATUS CODE ',I3,' RETURNED BY ROUTINE ',A,
     *   ' WHILE ',A : ' ',A,'.')
100   FORMAT ('0*** NOTE - ',A,' TIME SERIES ',A,' SUCCESSFULLY ',
     *   'DELETED.')
110   FORMAT (' *** NOTE - ',A,' IDENTIFIER ',A,
     *   ' FOUND IN MPFO PARAMETER RECORD. ',
     *   'OPTION SET TO RUN COMPUTATIONAL ORDER COMMAND.')
120   FORMAT ('0*** NOTE - NO FMPO PARAMETER RECORD FOUND.')
130   FORMAT ('0*** NOTE - NO ',A,' AREAS SUCCESSFULLY DELETED.')
140   FORMAT ('0*** NOTE - ',I3,' ',A,' AREAS SUCCESSFULLY DELETED.')
C
      END
