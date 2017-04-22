C MODULE SLAREA
C-----------------------------------------------------------------------
C
C  ROUTINE FOR DELETING AREA PARAMETERS.
C
      SUBROUTINE SLAREA (LARRAY,ARRAY,NFLD,IOAUTO,ISTAT)
C
      CHARACTER*3 XCKREF/' '/
      CHARACTER*4 TYPE
      CHARACTER*8 ZCKREF/'CHECKREF'/
      CHARACTER*20 CHAR,CHK
C
      DIMENSION ARRAY(LARRAY)
C
      INCLUDE 'uiox'
      INCLUDE 'scommon/sudbgx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_delete/RCS/slarea.f,v $
     . $',                                                             '
     .$Id: slarea.f,v 1.4 2002/02/11 21:02:18 dws Exp $
     . $' /
C    ===================================================================
C
C
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
      LCHK=LEN(CHK)/4
      NUMKEY=0
      NUMERR=0
      NUMWRN=0
      ILPFND=0
      IRPFND=0
      ICKREF=1
      XCKREF='YES'
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  GET NEXT INPUT FIELD
10    CALL UFIELD (NFLD,ISTRT,LENGTH,ITYPE,NREP,INTEGR,REAL,LCHAR,
     *   CHAR,LLPAR,LRPAR,LASK,LATSGN,LAMPS,LEQUAL,IERR)
      IF (NFLD.EQ.-1) GO TO 30
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
C  CHECK FOR COMMAND
       IF (LATSGN.EQ.1) GO TO 30
C
C  CHECK FOR PAIRED PARENTHESIS
      IF (ILPFND.GT.0.AND.IRPFND.EQ.0) THEN
         WRITE (LP,191) NFLD
191   FORMAT ('0*** NOTE - RIGHT PARENTHESES ASSUMED IN FIELD ',
     *   I2,'.')
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
      IF (CHK.EQ.ZCKREF) GO TO 41
C
      TYPE=CHK
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'TYPE=',TYPE
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      IF (TYPE.EQ.'MAP'.OR.
     *    TYPE.EQ.'FMAP'.OR.
     *    TYPE.EQ.'MAT'.OR.
     *    TYPE.EQ.'MAPE'.OR.
     *    TYPE.EQ.'MAPX') THEN
         ELSE
            CALL SUIDCK ('DELT',CHAR,NFLD,0,IKEYWD,IERR)
            IF (IERR.EQ.2) GO TO 30
            WRITE (LP,50) CHAR(1:LENSTR(CHAR))
            CALL SUERRS (LP,2,NUMERR)
         ENDIF
      GO TO 61
C
C  CHECKREF OPTION
41    IF (NFLD.EQ.1) CALL SUPCRD
      IF (LLPAR.EQ.0) THEN
         CHK='YES'
         WRITE (LP,201) ZCKREF(1:LENSTR(ZCKREF)),CHK(1:LENSTR(CHK))
         CALL SULINE (LP,2)
         GO TO 51
         ENDIF
      IF (LRPAR.GT.0) IRPFND=1
      IF (LRPAR.EQ.0) THEN
         WRITE (LP,211) NFLD
         CALL SULINE (LP,2)
         LRPAR=LENGTH+1
         ENDIF
      CALL UFPACK (LCHK,CHK,ISTRT,LLPAR+1,LRPAR-1,IERR)
      IF (CHK.EQ.'NO'.OR.CHK.EQ.'YES') GO TO 51
         WRITE (LP,221) ZCKREF(1:LENSTR(ZCKREF)),CHK(1:LENSTR(CHK))
         CALL SUERRS (LP,2,NUMERR)
         GO TO 10
51    IF (CHK.EQ.'NO') ICKREF=0
      IF (CHK.EQ.'YES') ICKREF=1
      XCKREF=CHK
      WRITE (LP,231) ZCKREF(1:LENSTR(ZCKREF)),CHK(1:LENSTR(CHK))
      CALL SULINE (LP,2)
201   FORMAT ('0*** NOTE - NO LEFT PARENTHESIS FOUND. ',A,
     *   'OPTION SET TO ',A,'.')
211   FORMAT ('0*** NOTE - RIGHT PARENTHESIS ASSUMED IN FIELD ',I2,
     *   '.')
221   FORMAT ('0*** ERROR - INVALID ',A,' OPTION : ',A)
231   FORMAT ('0*** NOTE - ',A,' OPTION SET TO ',A,'.')
      GO TO 10      
C
61    NUMKEY=NUMKEY+1
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  MAP AREA PARAMETERS
      IF (TYPE.EQ.'MAP') THEN
         CALL SLMAP (LARRAY,ARRAY,NFLD,ICKREF,IOAUTO,IERR)
         GO TO 20
         ENDIF
C
C  FMAP AREA PARAMETERS
      IF (TYPE.EQ.'FMAP') THEN
         CALL SLFMAP (LARRAY,ARRAY,NFLD,ICKREF,IOAUTO,IERR)
         GO TO 20
         ENDIF
C
C  MAT AREA PARAMETERS
      IF (TYPE.EQ.'MAT') THEN
         CALL SLMAT (LARRAY,ARRAY,NFLD,ICKREF,IERR)
         GO TO 20
         ENDIF
C
C  MAPE AREA PARAMETERS
      IF (TYPE.EQ.'MAPE') THEN
         CALL SLMAPE (LARRAY,ARRAY,NFLD,ICKREF,IERR)
         GO TO 20
         ENDIF
C
C  MARO AREA PARAMETERS
      IF (TYPE.EQ.'MARO') THEN
CCC         CALL SLMARO (LARRAY,ARRAY,NFLD,ICKREF,IERR)
         WRITE (LP,80) TYPE
         CALL SUERRS (LP,2,NUMERR)
         GO TO 20
         ENDIF
C
C  MAPX AREA PARAMETERS
      IF (TYPE.EQ.'MAPX') THEN
         CALL SLMAPX (LARRAY,ARRAY,NFLD,ICKREF,IOAUTO,IERR)
         GO TO 20
         ENDIF
C
C  INVALID PARAMETER TYPE
      WRITE (LP,90) TYPE
      CALL SUERRS (LP,2,NUMERR)
      GO TO 10
C
C  CHECK CURRENT FIELD FOR KEYWORD
20    ISTRT=-1
      GO TO 10
C
C  CHECK NUMBER OF KEYWORDS FOUND
30    IF (NUMKEY.EQ.0) THEN
         WRITE (LP,70)
         CALL SUWRNS (LP,2,NUMWRN)
         ENDIF
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,100)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
40    FORMAT (' *** ENTER SLAREA')
50    FORMAT ('0*** ERROR - INVALID AREA PARAMETER TYPE : ',A)
60    FORMAT (' NULL FIELD FOUND IN FIELD ',I2)
70    FORMAT ('0*** WARNING - NO PARAMETER TYPE KEYWORD ',
     *   '(MAP, MAPX, FMAP, MAT OR MARO) WAS FOUND.')
80    FORMAT ('0*** ERROR - ROUTINE TO DELETE ',A,
     *   ' PARAMETERS IS NOT IN THIS VERSION.')
90    FORMAT ('0*** ERROR - IN SLAREA - PARAMETER TYPE ',A,' CANNOT ',
     *   'BE PROCESSED.')
100   FORMAT (' *** EXIT SLAREA')
C
      END
