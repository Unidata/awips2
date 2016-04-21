C MODULE SLBASN
C-----------------------------------------------------------------------
C
C  ROUTINE FOR DELETING BASN PARAMETERS.
C
      SUBROUTINE SLBASN (LARRAY,ARRAY,NFLD,ISTAT)
C
      CHARACTER*8 BASNID
      CHARACTER*8 ZCKREF/'CHECKREF'/
      CHARACTER*20 CHAR,CHK
C
      DIMENSION ARRAY(LARRAY)
C
      INCLUDE 'uio'
      INCLUDE 'scommon/sudbgx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_delete/RCS/slbasn.f,v $
     . $',                                                             '
     .$Id: slbasn.f,v 1.3 1999/07/07 11:21:18 page Exp $
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
     *      'LARRAY=',LARRAY,
     *      'NFLD=',NFLD,
     *      ' '
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      ISTAT=0
C
      LCHAR=-LEN(CHAR)
      LCHK=-LEN(CHK)
      NUMFLD=0
      NUMERR=0
      IERALC=0
      NUMWRN=0
      NDELTE=0
      IFIRST=1
      NUMID1=0
      NUMID2=0
      NUMID3=0
      ILPFND=0
      IRPFND=0
      ICKREF=1
C
C  SET INDICATOR TO REPROCESS CURRENT FIELD
      ISTRT=-1
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  GET NEXT INPUT FIELD
C
10    CALL UFIELD (NFLD,ISTRT,LENGTH,ITYPE,NREP,INTEGR,REAL,LCHAR,CHAR,
     *   LLPAR,LRPAR,LASK,LATSGN,LAMPS,LEQUAL,IERR)
      IF (LDEBUG.GT.0) THEN
         CALL UPRFLD (NFLD,ISTRT,LENGTH,ITYPE,NREP,INTEGR,REAL,
     *      LCHAR,CHAR,LLPAR,LRPAR,LASK,LATSGN,LAMPS,LEQUAL,IERR)
         ENDIF
      IF (IERR.EQ.1) THEN
         IF (LDEBUG.GT.0) THEN
            WRITE (IOSDBG,40) NFLD
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
C  CHECK FOR PAIRED PARENTHESIS
      IF (ILPFND.GT.0.AND.IRPFND.EQ.0) THEN
         WRITE (LP,51) NFLD
         CALL SULINE (LP,2)
         ILPFND=0
         IRPFND=0
         ENDIF
      IF (LLPAR.GT.0) ILPFND=1
      IF (LRPAR.GT.0) IRPFND=1
C
C  CHECK FOR PARENTHESIS IN FIELD
      IF (LLPAR.GT.0) CALL UFPACK (LCHK,CHK,ISTRT,1,LLPAR-1,IERR)
      IF (LLPAR.EQ.0) CALL UFPACK (LCHK,CHK,ISTRT,1,LENGTH,IERR)
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'CHK=',CHK
         CALL SULINE (IOSDBG,1)
         ENDIF
C       
C  CHECK FOR OPTION
      IF (CHK.EQ.ZCKREF) THEN
         IF (NFLD.EQ.1) CALL SUPCRD
         IF (LLPAR.EQ.0) THEN
            CHK='YES'
            WRITE (LP,52) ZCKREF(1:LENSTR(ZCKREF)),CHK(1:LENSTR(CHK))
            CALL SULINE (LP,2)
            GO TO 15
            ENDIF
         IF (LRPAR.GT.0) IRPFND=1
         IF (LRPAR.EQ.0) THEN
            WRITE (LP,53) NFLD
            CALL SULINE (LP,2)
            LRPAR=LENGTH+1
            ENDIF
         CALL UFPACK (LCHK,CHK,ISTRT,LLPAR+1,LRPAR-1,IERR)
         IF (CHK.EQ.'NO'.OR.CHK.EQ.'YES') GO TO 15
            WRITE (LP,54) ZCKREF(1:LENSTR(ZCKREF)),CHK(1:LENSTR(CHK))
            CALL SUERRS (LP,2,NUMERR)
            GO TO 10
15       IF (CHK.EQ.'NO') ICKREF=0
         IF (CHK.EQ.'YES') ICKREF=1
         WRITE (LP,55) ZCKREF(1:LENSTR(ZCKREF)),CHK(1:LENSTR(CHK))
         CALL SULINE (LP,2)
         GO TO 10
         ENDIF
C
      NUMFLD=NUMFLD+1
C
      IF (NUMFLD.EQ.1) THEN
         IF (NFLD.EQ.1) CALL SUPCRD
C     CHECK PARAMETER TYPE
         IF (CHAR.NE.'BASIN') THEN
            WRITE (LP,50) CHAR
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
      BASNID=CHAR
C
      IF (ICKREF.EQ.1) THEN
C     CHECK IF BASIN IDENTIFIER IS REFERENCED
         LARAY2=2000
         LARAY1=LARRAY-LARAY2
         LARAY3=0
         CALL SLCHK ('BASN',BASNID,LARAY1,ARRAY,LARAY2,ARRAY(LARAY1+1),
     *      LARAY3,ARAY3,IFIRST,IXSORT,NUMID1,NUMID2,NUMID3,NUMERR,IERR)
         IF (IERR.NE.1) IFIRST=0
         IF (IERR.NE.0) GO TO 10
         ENDIF
C
C  DELETE PARAMETERS FROM PREPROCESSOR PARAMETRIC DATA BASE
      CALL SUDOPN (1,'PPP ',IERR)
      IF (IERR.GT.0) THEN
         IERALC=1
         GO TO 10
         ENDIF
      CALL WPPDEL (BASNID,'BASN',IERR)
      IF (IERR.EQ.0) THEN
         WRITE (LP,80) BASNID
         CALL SULINE (LP,2)
         NDELTE=NDELTE+1
         CALL SUDWRT (1,'PPP ',IERR)
         ELSE
            IF (IERR.EQ.1) THEN
               WRITE (LP,60) BASNID(1:LENSTR(BASNID))
               CALL SUWRNS (LP,2,NUMWRN)
               ELSE
                  WRITE (LP,70) 'ERROR',IERR,'WPPDEL',
     *             'DELETING BASN PARAMETERS',BASNID(1:LENSTR(BASNID))
                  CALL SUERRS (LP,2,NUMERR)
               ENDIF
         ENDIF
C
      GO TO 10
C
C  PRINT NUMBER OF BASINS DELETED
20    IF (NDELTE.EQ.0) THEN
         WRITE (LP,90)
         CALL SULINE (LP,2)
         ELSE
            WRITE (LP,100) NDELTE
            CALL SULINE (LP,2)
         ENDIF
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,110)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
30    FORMAT (' *** ENTER SLBASN')
40    FORMAT (' NULL FIELD FOUND IN FIELD ',I2)
50    FORMAT ('0*** ERROR - IN SLBASN - ',A,' IS AN INVALID ',
     *   'PARAMETER TYPE.')
51    FORMAT ('0*** NOTE - RIGHT PARENTHESES ASSUMED IN FIELD ',
     *   I2,'.')
52    FORMAT ('0*** NOTE - NO LEFT PARENTHESIS FOUND. ',A,
     *   'OPTION SET TO ',A,'.')
53    FORMAT ('0*** NOTE - RIGHT PARENTHESIS ASSUMED IN FIELD ',I2,
     *   '.')
54    FORMAT ('0*** ERROR - INVALID ',A,' OPTION : ',A)
55    FORMAT ('0*** NOTE - ',A,' OPTION SET TO ',A,'.')
60    FORMAT ('0*** WARNING - BASN PARAMETERS NOT FOUND FOR BASIN ',
     *   A,'.')
70    FORMAT ('0*** ',A,' - STATUS CODE ',I3,' RETURNED BY ROUTINE ',A,
     *   ' WHILE ',A,' FOR IDENTIFIER ',A,'.')
80    FORMAT ('0*** NOTE -  BASIN ',A,' SUCCESSFULLY DELETED.')
90    FORMAT ('0*** NOTE - NO BASINS SUCCESSFULLY DELETED.')
100   FORMAT ('0*** NOTE - ',I3,' BASINS SUCCESSFULLY DELETED.')
110   FORMAT (' *** EXIT SLBASN')
C
      END
