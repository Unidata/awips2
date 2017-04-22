C MODULE SLMAPX
C-----------------------------------------------------------------------
C
C  ROUTINE FOR DELETING MAPX AREAS.
C
      SUBROUTINE SLMAPX (LARRAY,ARRAY,NFLD,ICKREF,IOAUTO,ISTAT)
C
      CHARACTER*4 DTYPE 
      CHARACTER*8 AREAID,BASNID,XNAME
      CHARACTER*8 BLNK8/' '/
      CHARACTER*20 CHAR
C
      PARAMETER (MAXBASN=99)                                  
      DIMENSION BASNID00(MAXBASN*2)                       
      DIMENSION ARRAY(LARRAY)
      DIMENSION UNUSED(10)
      DIMENSION DESC(5),FMAPID(2)
C
      INCLUDE 'uiox'
      INCLUDE 'scommon/sudbgx'
      INCLUDE 'scommon/sworkx'
      INCLUDE 'scommon/swrk2x'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_delete/RCS/slmapx.f,v $
     . $',                                                             '
     .$Id: slmapx.f,v 1.6 2002/10/10 15:56:48 dws Exp $
     . $' /
C    ===================================================================
C
      CALL UREPET (' ',BASNID,8)           !cfan
      CALL UREPET (' ',BASNID00,72)    !cfan
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,40)
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
      IF (NFLD.EQ.-1) GO TO 30
C
C  CHECK FOR COMMAND
      IF (LATSGN.EQ.1) GO TO 30
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
         IF (CHAR.NE.'MAPX') THEN
            WRITE (LP,70) CHAR(1:LENSTR(CHAR))
            CALL SUERRS (LP,2,NUMERR)
            ENDIF
         GO TO 10
         ENDIF
C
C  CHECK FOR KEYWORD
      CALL SUIDCK ('DELT',CHAR,NFLD,0,IKEYWD,IERR)
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
      AREAID=CHAR
      DTYPE='MAPX'
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
      WRITE (LP,50)
      CALL SULINE (LP,1)
C
      IFOUND=0
C
C  DELETE TIME SERIES FROM PROCESSED DATA BASE
      IFUT=0
      IPRERR=0
      CALL WPRDEL (AREAID,DTYPE,IFUT,ICKREF,IPRERR,IERR)
      IF (IERR.EQ.0) THEN
         WRITE (LP,110) DTYPE,'TIME SERIES',AREAID(1:LENSTR(AREAID))
         CALL SULINE (LP,1)
         CALL SUDWRT (1,'PRD ' ,IERR)
         IFOUND=1
         ELSE
            IF (IERR.EQ.1) THEN
               WRITE (LP,90) DTYPE,AREAID(1:LENSTR(AREAID))
               CALL SUWRNS (LP,1,NUMWRN)
               ELSE
                  WRITE (LP,100) 'ERROR',IERR,'WPRDEL',
     *               DTYPE,'TIME SERIES',AREAID(1:LENSTR(AREAID))
                  CALL SUERRS (LP,2,NUMERR)
               ENDIF
         ENDIF
C
C  LC 05/24/02 made changes for version 2 of mapx records
C  use new srmapx and loop on # of basins
C  READ MAPX PARAMETER RECORD
      IPTR=0
      IPRERR=0
      ITIME=1
      NUMB=1
      CALL SRMAPX (IVMAPX,AREAID,ITIME,DESC,BASNID00,FMAPID,   
     *   UNUSED,LARRAY,ARRAY,IPTR,IPRERR,IPTRNX,ISMAPX,NUMB) 
       IF(NUMB.NE.0)THEN
         DO 190 II=1,NUMB
         CALL SUBSTR (BASNID00,(II*8)-7,8,BASNID,1)
            IPTR=0
            CALL RPPREC (BASNID,'BASN',IPTR,LSWRK2,SWRK2,NFILL,IPTRNX,
     *         IERR)
            IF (IERR.NE.0) THEN
               CALL SRPPST (BASNID,'BASN',IPTR,LSWRK2,NFILL,IPTRNX,
     *            IERR)
               WRITE (LP,120) 'READING','BASN',BASNID(1:LENSTR(BASNID))
               CALL SUERRS (LP,2,NUMERR)
               GO TO 10
            ENDIF
C        SET MAPX AREA IDENTIFIER TO BLANK
            CALL SUBSTR (BLNK8,1,8,SWRK2(20),1)
            NPOS=NFILL
            CALL WPPREC (BASNID,'BASN',NPOS,SWRK2,IPTR,IERR)
            IF (IERR.EQ.0) THEN
               WRITE (LP,130) 'BASN',BASNID
C               WRITE (LP,130) 'BASN',BASNID(1:LENSTR(BASNID))
               CALL SULINE (LP,1)
               ELSE
                  CALL SWPPST (BASNID,'BASN',NPOS,IPTR,IERR)
                  WRITE (LP,120) 'WRITING','BASN',
     *               BASNID(1:LENSTR(BASNID))
                  CALL SUERRS (LP,2,NUMERR)
                  GO TO 10
            ENDIF
190      CONTINUE 
       ELSE
         IF (IERR.NE.2) THEN
           CALL SRPPST (AREAID,'BASN',IPTR,LSWRK2,NFILL,IPTRNX,
     *         IERR)
           WRITE (LP,120) 'READING','BASN',
     *       AREAID(1:LENSTR(AREAID))
            CALL SUERRS (LP,2,NUMERR)
            GO TO 10
         ENDIF
       ENDIF
C LC 05/25/02 my changes should not affect anything below

C
C  DELETE PARAMETRIC DATA FROM PREPROCESSOR PARAMETRIC DATA BASE
      CALL WPPDEL (AREAID,'MAPX',IERR)
      IF (IERR.EQ.0) THEN
         WRITE (LP,110) 'MAPX PARAMETERS',AREAID(1:LENSTR(AREAID))
         CALL SULINE (LP,1)
         CALL SUDWRT (1,'PPP ' ,IERR)
         IFOUND=1
         ELSE
            IF (IERR.EQ.1) THEN
               WRITE (LP,80) 'MAPX',AREAID(1:LENSTR(AREAID))
               CALL SUWRNS (LP,2,NUMWRN)
               ELSE
                  WRITE (LP,100) 'ERROR',IERR,'WPPDEL',
     *               DYPTE,'PAMETERS',AREAID(1:LENSTR(AREAID))
                  CALL SUERRS (LP,2,NUMERR)
               GO TO 10
               ENDIF
         ENDIF
C
      IF (IFOUND.EQ.0) GO TO 10
C
      WRITE (LP,150) AREAID
      CALL SULINE (LP,2)
C
      NDELTE=NDELTE+1
C
C  READ MAPX CARRYOVER GROUP AREA COMPUTATIONAL ORDER PARAMETERS
      MXA=LSWORK/3
      IPRERR=0
      CALL SRMXCO (IVMXCO,UNUSED,MXA,NXA,SWORK(1),SWORK(MXA*2+1),
     *   LARRAY,ARRAY,IPRERR,IERR)
      IF (IERR.GT.0) THEN
         WRITE (LP,120) 'READING','MXCO'
         CALL SUERRS (LP,2,NUMERR)
         GO TO 10
         ENDIF
      IF (LDEBUG.GT.0) THEN
         IPRNT=1
         CALL SPMXCO (IPRNT,NXA,IVMXCO,SWORK(1),SWORK(MXA*2+1),IERR)
         IF (IERR.GT.0) THEN
            WRITE (LP,120) 'PRINTING','MXCO'
            CALL SUERRS (LP,2,NUMERR)
            ENDIF
         ENDIF
C
C  CHECK IF MAPX AREA IDENTIFIER FOUND IN PARAMETER ARRAY
      DO 20 I=1,NXA
         J=(I*2)-1
         CALL SUBSTR (SWORK(J),1,LEN(XNAME),XNAME,1)
         IF (AREAID.EQ.XNAME) THEN
            WRITE (LP,140) DTYPE,AREAID
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
         WRITE (LP,160) DTYPE
         CALL SULINE (LP,2)
         ELSE
            WRITE (LP,170) NDELTE,DTYPE
            CALL SULINE (LP,2)
         ENDIF
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,180)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
40    FORMAT (' *** ENTER SLMAPX')
50    FORMAT (' ')
60    FORMAT (' NULL FIELD FOUND IN FIELD ',I2)
70    FORMAT ('0*** ERROR - IN SLMAPX - ',A,' IS AN INVALID PARAMETER ',
     *   'TYPE.')
80    FORMAT (' *** WARNING - ',A,' PARAMETERS  NOT FOUND FOR AREA ',A,
     *   '.')
90    FORMAT (' *** WARNING - ',A,' TIME SERIES NOT FOUND FOR AREA ',A,
     *   '.')
100   FORMAT ('0*** ',A,' - STATUS CODE ',I3,' RETURNED BY ROUTINE ',A,
     *   ' WHILE DELETING ',A,' AREA ',A,'.')
110   FORMAT (' *** NOTE - ',A,' ',A,' SUCCESSFULLY DELETED ',
     *   'FOR AREA ',A,'.')
120   FORMAT ('0*** ERROR - IN SLMAPX - ',A,' ',A,' PARAMETERS ' :
     *   'FOR AREA ',A,'.')
130   FORMAT (' *** NOTE - ',A,' PARAMETERS SUCCESSFULLY ',
     *   'UPDATED FOR BASIN ',A,'.')
140   FORMAT (' *** NOTE - ',A,' IDENTIFIER ',A,
     *   ' FOUND IN MXCO PARAMETER RECORD. ',
     *   'OPTION SET TO RUN COMPUTATIONAL ORDER COMMAND.')
150   FORMAT ('0*** NOTE - ',A,' AREA ',A,' SUCCESSFULLY DELETED.')
160   FORMAT ('0*** NOTE - NO ',A,' AREAS SUCCESSFULLY DELETED.')
170   FORMAT ('0*** NOTE - ',I3,' ',A,' AREAS SUCCESSFULLY DELETED.')
180   FORMAT (' *** EXIT SLMAPX')
C
      END
