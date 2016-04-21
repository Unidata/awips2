C MODULE SLMAP
C-----------------------------------------------------------------------
C
C  ROUTINE TO DELETE MAP AREA PARAMETERS.
C
      SUBROUTINE SLMAP (LARRAY,ARRAY,NFLD,ICKREF,IOAUTO,ISTAT)
C
      CHARACTER*4 DTYPE,WDISP
      CHARACTER*8 AREAID,BASNID,XNAME,FGID
      CHARACTER*8 BLNK8/' '/
      CHARACTER*20 STRNG
C
      DIMENSION ARRAY(LARRAY)
      DIMENSION UNUSD(10)
C
      INCLUDE 'uiox'
      INCLUDE 'scommon/sudbgx'
      INCLUDE 'scommon/sugnlx'
      INCLUDE 'scommon/sworkx'
      INCLUDE 'scommon/swrk2x'
      INCLUDE 'scommon/sordrx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_delete/RCS/slmap.f,v $
     . $',                                                             '
     .$Id: slmap.f,v 1.9 2002/02/11 21:03:04 dws Exp $
     . $' /
C    ===================================================================
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'ENTER SLMAP'
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
      LSTRNG=LEN(STRNG)/4
      NUMFLD=0
      NUMERR=0
      IERALC=0
      INDALC=0
      NUMWRN=0
      NDELTE=0
      IFIRST=1
      NUMID1=0
      NUMID2=0
      NUMID3=0
      IUGFIL=0
      IPRERR=0
      NAREA=0
      NFOUND=0
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
            WRITE (IOSDBG,90) NFLD
            CALL SULINE (IOSDBG,1)
            ENDIF
         GO TO 10
         ENDIF
C
C  CHECK FOR END OF INPUT
      IF (NFLD.EQ.-1) GO TO 50
C
C  CHECK FOR COMMAND
      IF (LATSGN.EQ.1) GO TO 50
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
         IF  (STRNG.NE.'MAP') THEN
            WRITE (LP,80) STRNG(1:LENSTR(STRNG))
            CALL SUERRS (LP,2,NUMERR)
            ENDIF
         GO TO 10
         ENDIF
C
C  CHECK FOR KEYWORD
      CALL SUIDCK ('DELT',STRNG,NFLD,0,IKEYWD,IERR)
      IF (IERR.EQ.2) GO TO 50
C
      IF (NFLD.EQ.1) CALL SUPCRD
C
      IF (IERALC.GT.0) GO TO 10
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
      IF (IUGFIL.EQ.0) THEN
C     READ USER GENERAL PARAMETERS
         CALL SUGTUG (LARRAY,ARRAY,IERR)
         IF (IERR.EQ.0) THEN
            IUGFIL=1
            ELSE
               WRITE (LP,100)
               CALL SUERRS (LP,2,NUMERR)
               IERALC=1
               GO TO 10
            ENDIF
         ENDIF
C
      IF (INDALC.EQ.0) THEN
C     CHECK IF DATA BASES ALLOCATED
         CALL SUDOPN (1,'PPP ',IERR)
         CALL SUDOPN (1,'PRD ',IERR2)
         CALL SUDOPN (1,'FC  ',IERR3)
         INDALC=1
         IF (IERR.EQ.0.AND.IERR2.EQ.0.AND.IERR3.EQ.0) THEN
            ELSE
               IERALC=1
               GO TO 10
            ENDIF
         ENDIF
C
      AREAID=STRNG
      DTYPE='MAP'
C
      IF (ICKREF.EQ.1) THEN
C     CHECK IF AREA IS REFERENCED
         LARAY2=1000
         LARAY1=LARRAY-LARAY2
         LWORK2=1000
         LWORK3=1000
         LWORK1=LSWORK-LWORK2-LWORK3
         CALL SLCHK (DTYPE,AREAID,LARAY1,ARRAY,LARAY2,ARRAY(LARAY1+1),
     *      LWORK1,SWORK,IFIRST,IXSORT,NUMID1,NUMID2,NUMID3,NUMERR,
     *      IERR)
         IF (IERR.NE.1) IFIRST=0
         IF (IERR.NE.0) GO TO 10
         ENDIF
C
      IF (NAREA.EQ.0) THEN
C     SET MAP STATUS INDICATOR TO INCOMPLETE
         ICOMPX=ICUGNL(1)
         ICUGNL(1)=1
         UNSD=-999.
         WDISP='OLD'
         INCLUDE 'scommon/callswugnl'
         CALL SUPCLS (1,'USER',IERR)
         ENDIF
C
      WRITE (LP,70)
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
         WRITE (LP,140) DTYPE,AREAID(1:LENSTR(AREAID))
         CALL SULINE (LP,1)
         CALL SUDWRT (1,'PRD ',IERR)
         IFOUND=1
         ELSE
            IF (IERR.EQ.1) THEN
               WRITE (LP,110) AREAID(1:LENSTR(AREAID))
               CALL SUWRNS (LP,1,NUMWRN)
               ELSE
                  WRITE (LP,120) 'ERROR',IERR,'WPRDEL',
     *               'DELETING MAP TIME SERIES',AREAID(1:LENSTR(AREAID))
                  CALL SUERRS (LP,2,NUMERR)
               ENDIF
         ENDIF
C
C  READ MAP PARAMETER RECORD
      IPTR=0
      CALL RPPREC (AREAID,DTYPE,IPTR,LSWRK2,SWRK2,NFILL,IPTRNX,
     *   IERR)
      IF (IERR.EQ.0) THEN
C     CHECK IF BASIN BOUNDARY USED
         CALL SUBSTR (SWRK2(10),1,LEN(BASNID),BASNID,1)
         IF (BASNID.NE.' ') THEN
            IPTR=0
            CALL RPPREC (BASNID,'BASN',IPTR,LSWRK2,SWRK2,NFILL,IPTRNX,
     *         IERR)
            IF (IERR.NE.0) THEN
               CALL SRPPST (BASNID,'BASN',IPTR,LSWRK2,NFILL,IPTRNX,
     *            IERR)
               WRITE (LP,150) 'READING','BASN',AREAID(1:LENSTR(AREAID))
               CALL SUERRS (LP,2,NUMERR)
               GO TO 10
               ENDIF
C        SET AREA IDENTIFIER TO BLANK
            CALL SUBSTR (BLNK8,1,8,SWRK2(14),1)
C        SET INDICATOR THAT BASIN AND AREA DEFINITION NOT CONSISTENT
            MAPFLG=0
            SWRK2(18)=MAPFLG+.01
            NPOS=NFILL
            CALL WPPREC (BASNID,'BASN',NPOS,SWRK2,IPTR,IERR)
            IF (IERR.EQ.0) THEN
               WRITE (LP,160) 'BASN',BASNID(1:LENSTR(BASNID))
               CALL SULINE (LP,1)
               ELSE
                  CALL SWPPST (BASNID,'BASN',NPOS,IPTR,IERR)
                  WRITE (LP,150) 'WRITING','BASN',
     *               BASNID(1:LENSTR(BASNID))
                  CALL SUERRS (LP,2,NUMERR)
                  GO TO 10
               ENDIF
            ENDIF
         ELSE
            IF (IERR.EQ.2) THEN
               WRITE (LP,130) DTYPE,AREAID(1:LENSTR(AREAID))
               CALL SUWRNS (LP,1,NUMWRN)
               ELSE
                  CALL SRPPST (AREAID,DTYPE,IPTR,LSWRK2,NFILL,IPTRNX,
     *               IERR)
                  WRITE (LP,150) 'READING','BASN',
     *               AREAID(1:LENSTR(AREAID))
                  CALL SUERRS (LP,2,NUMERR)
               GO TO 10
               ENDIF
         ENDIF
C
C  DELETE PARAMETRIC DATA FROM PREPROCESSOR PARAMETRIC DATA BASE
      CALL WPPDEL (AREAID,'MAPS',IERR)
      IF (IERR.EQ.0) THEN
         WRITE (LP,170) 'MAPS',AREAID(1:LENSTR(AREAID))
         CALL SULINE (LP,1)
         CALL SUDWRT (1,'PPP ',IERR)
         IFOUND=1
         ELSE
            IF (IERR.EQ.1) THEN
               WRITE (LP,130) 'MAPS',AREAID(1:LENSTR(AREAID))
               CALL SUWRNS (LP,1,NUMWRN)
               ELSE
                  WRITE (LP,120) 'ERROR',IERR,'WPPDEL',
     *               'DELETING MAPS PARAMETERS',AREAID(1:LENSTR(AREAID))
                  CALL SUERRS (LP,2,NUMERR)
               ENDIF
         ENDIF
      CALL WPPDEL (AREAID,DTYPE,IERR)
      IF (IERR.EQ.0) THEN
         WRITE (LP,170) DTYPE,AREAID(1:LENSTR(AREAID))
         CALL SULINE (LP,1)
         CALL SUDWRT (1,'PPP ',IERR)
         IFOUND=1
         ELSE
           IF (IERR.EQ.1) THEN
               WRITE (LP,130) DTYPE,AREAID(1:LENSTR(AREAID))
               CALL SUWRNS (LP,1,NUMWRN)
               ELSE
                  WRITE (LP,120) 'ERROR',IERR,'WPPDEL',
     *               'DELETING MAP PARAMETERS',AREAID(1:LENSTR(AREAID))
                  CALL SUERRS (LP,2,NUMERR)
               ENDIF
         ENDIF
C
      IF (IFOUND.EQ.0) GO TO 10
C
C  AREA SUCCESSFULLY DELETED
      WRITE (LP,200) AREAID(1:LENSTR(AREAID))
      CALL SULINE (LP,1)
C
      NDELTE=NDELTE+1
C
C  READ MAP COMPUTATIONAL ORDER PARAMETER RECORD
      IPTRFG=0
20    XNAME=' '
      CALL SRMPFO (XNAME,IPTRFG,LARRAY,ARRAY,IVMPFO,FGID,UNUSD,
     *   MRMPID,RMPID,NRMPID,MRMPID,MAPSR,NMAPSR,IPRERR,IPTRNF,IERR)
      IF (IERR.NE.0) THEN
         IF (IERR.EQ.2) THEN
            WRITE (LP,180)
            CALL SULINE (LP,2)
            ELSE
               WRITE (LP,120) 'ERROR',IERR,'SRMPFO',
     *            'READING MPFO PARAMETERS',XNAME(1:LENSTR(XNAME))
               CALL SUERRS (LP,2,NUMERR)
            ENDIF
         GO TO 10
         ENDIF
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*)
     *      ' IPTRFG=',IPTRFG,
     *      ' IPTRNF=',IPTRNF,
     *      ' '
         CALL SULINE (IOSDBG,1)
         IPRNT=1
         CALL SPMPFO (IPRNT,NUMFG,IVMPFO,FGID,RMPID,NRMPID,
     *      MAPSR,NMAPSR,IERR)
         IF (IERR.NE.0) THEN
            WRITE (LP,190) XNAME
            CALL SUERRS (LP,2,NUMERR)
            GO TO 10
            ENDIF
         ENDIF
C
C  CHECK IF MAP AREA IDENTIFIER FOUND IN PARAMETER ARRAY
      DO 30 I=1,NRMPID
         IF (AREAID.EQ.RMPID(I)) THEN
            NFOUND=NFOUND+1
            WRITE (LP,210) AREAID,FGID
            CALL SULINE (LP,2)
            IOAUTO=1
            GO TO 40
            ENDIF
30       CONTINUE
C
40    IF (IPTRNF.GT.0) THEN
         IPTRFG=IPTRNF
         GO TO 20
         ENDIF
      GO TO 10
C
C  RESET MAP STATUS INDICATOR
50    IF (NFOUND.EQ.0) THEN
         ICUGNL(1)=ICOMPX
         WDISP='OLD'
         INCLUDE 'scommon/callswugnl'
         CALL SUPCLS (1,'USER',IERR)
         ENDIF
C
C  PRINT NUMBER OF AREAS DELETED
      IF (NDELTE.EQ.0) THEN
         WRITE (LP,220)
         CALL SULINE (LP,2)
         ELSE
            WRITE (LP,230) NDELTE
            CALL SULINE (LP,2)
         ENDIF
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'EXIT SLMAP'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
70    FORMAT (' ')
80    FORMAT ('0*** ERROR - IN SLMAP - ',A,' IS AN INVALID PARAMETER ',
     *   'TYPE.')
90    FORMAT (' NULL FIELD FOUND IN FIELD ',I2)
100   FORMAT ('0*** ERROR - MAP AREA CANNOT BE DELETED BECAUSE ',
     *   'GENERAL USER PARAMETERS ARE NOT DEFINED.')
110   FORMAT (' *** WARNING - MAP  TIME SERIES NOT FOUND FOR AREA ',
     *   A,'.')
120   FORMAT ('0*** ',A,' - STATUS CODE ',I3,' RETURNED BY ROUTINE ',A,
     *   ' WHILE ',A,' FOR IDENTIFIER ',A,'.')
130   FORMAT (' *** WARNING - ',A,' PARAMETERS  NOT FOUND FOR AREA ',
     *   A,'.')
140   FORMAT (' *** NOTE - ',A,' TIME SERIES SUCCESSFULLY DELETED ',
     *   'FOR AREA ',A,'.')
150   FORMAT ('0*** ERROR - IN SLMAP - ',A,' ',A,' PARAMETERS FOR ',
     *   'AREA ',A,'.')
160   FORMAT (' *** NOTE - ',A,' PARAMETERS SUCCESSFULLY ',
     *   'UPDATED FOR BASIN ',A,'.')
170   FORMAT (' *** NOTE - ',A,' PARAMETERS SUCCESSFULLY DELETED ',
     *   'FOR AREA ',A,'.')
180   FORMAT ('0*** NOTE - NO MPFO PARAMETER RECORD FOUND.')
190   FORMAT ('0*** ERROR - PRINTING MPFO ORDER PARAMETERS FOR ',A,
     * 'IDENTIFIER')
200   FORMAT (' *** NOTE - MAP  AREA ',A,' SUCCESSFULLY DELETED.')
210   FORMAT (' *** NOTE - MAP  IDENTIFIER ',A,
     *   ' FOUND IN MPFO PARAMETER RECORD ',A,'. ',
     *   'OPTION SET TO RUN COMPUTATIONAL ORDER COMMAND.')
220   FORMAT ('0*** NOTE - NO MAP AREAS SUCCESSFULLY DELETED.')
230   FORMAT ('0*** NOTE - ',I3,' MAP AREAS SUCCESSFULLY DELETED.')
C
      END
