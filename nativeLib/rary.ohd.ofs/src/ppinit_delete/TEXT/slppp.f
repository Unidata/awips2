C MODULE SLPPP
C-----------------------------------------------------------------------
C
C  ROUTINE TO DELETE PARAMETERS FROM PARAMETRIC DATA BASE.
C
      SUBROUTINE SLPPP (NFLD,PARMID,TYPE,NUMERR,NUMWRN,
     *   LARRAY,ARRAY,ISTAT)
C
      CHARACTER*4 TYPE,XTYPE
      CHARACTER*8 TYPERR,PARMID
      CHARACTER*20 CHAR/' '/,CHK/' '/
C
      DIMENSION ARRAY(LARRAY)
C
      INCLUDE 'uio'
      INCLUDE 'scommon/sudbgx'
      INCLUDE 'scommon/sgboxx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_delete/RCS/slppp.f,v $
     . $',                                                             '
     .$Id: slppp.f,v 1.4 2000/12/18 23:00:28 dws Exp $
     . $' /
C    ===================================================================
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'ENTER SLPPP'
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
      LCHAR=LEN(CHAR)/4
      LCHK=LEN(CHK)/4
C
      NDELTE=0
C
C  CHECK OPTION TO PRINT BLANK LINE BEFORE DELETE MESSAGE
      IPRSPC=1
      IF (NFLD.EQ.-2) IPRSPC=0
C
C  CHECK IF INPUT FIELD TO BE READ
      IF (NFLD.EQ.-1) GO TO 90
      IF (NFLD.EQ.-2) GO TO 90
C
      ISTRT=-1
      ILPFND=0
      IRPFND=0
      NUMFLD=0
      NUMERR=0
      NUMWRN=0
      NXTFLD=1
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
            WRITE (IOSDBG,230) NFLD
            CALL SULINE (IOSDBG,1)
            ENDIF
         GO TO 10
         ENDIF
C
C  CHECK FOR END OF INPUT
      IF (NFLD.EQ.-1) GO TO 150
C
C  CHECK FOR COMMAND
      IF (LATSGN.EQ.1) GO TO 150
C
C  CHECK FOR PAIRED PARENTHESIS
      IF (ILPFND.GT.0.AND.IRPFND.EQ.0) THEN
         IF (NFLD.EQ.1) CALL SUPCRD
         WRITE (LP,240) NFLD
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
      NUMFLD=NUMFLD+1
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
      IF (NUMFLD.EQ.1) THEN
C     CHECK KEYWORD
         TYPE=CHK
         IF (NFLD.EQ.1) CALL SUPCRD
         IF (TYPE.NE.'PPP') THEN
            WRITE (LP,190) 'PPP',NFLD,CHK(1:LENSTR(CHK))
            CALL SUERRS (LP,2,NUMERR)
            ENDIF
         GO TO 10
         ENDIF
C
      IF (NXTFLD.EQ.2) GO TO 70
C
C  CHECK FOR KEYWORD
      CALL SUIDCK ('DELT',CHK,NFLD,0,IKEYWD,IERR)
      IF (IERR.EQ.2) GO TO 150

      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'CHK=',CHK
         CALL SULINE (IOSDBG,1)
         ENDIF
C
70    IF (NFLD.EQ.1) CALL SUPCRD
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
      IF (NXTFLD.EQ.2) GO TO 80
C
C  SET IDENTIFIER
      CALL SUBSTR (CHAR,1,LEN(PARMID),PARMID,1)
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,210) PARMID
         CALL SULINE (IOSDBG,1)
         ENDIF
      NXTFLD=2
      GO TO 10
C
C  SET PARAMETER TYPE
80    TYPE=CHAR
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,220) TYPE
         CALL SULINE (IOSDBG,1)
         ENDIF
C
90    IPTYPE=1
      IF (PARMID.EQ.'SNGL') THEN
         MAXVAL=4
         IF (LENSTR(PARMID).GT.MAXVAL) THEN
            WRITE (LP,250) LENGTH,CHAR(1:LENSTR(CHAR)),MAXVAL
            CALL SUERRS (LP,2,NUMERR)           
            GO TO 130
            ENDIF
         IPTYPE=2
         PARMID=' '
         ENDIF
C
C  OPEN DATA BASE
      CALL SUDOPN (1,'PPP ',IERR)
      IF (IERR.GT.0) GO TO 130 
C
C  DELETE PARAMETERS FROM PREPROCESSOR PARAMETRIC DATA BASE
      IF (TYPE.EQ.'PCPN'.OR.TYPE.EQ.'TEMP') THEN
         IPTR=0
         CALL RPPREC (PARMID,TYPE,IPTR,LARRAY,ARRAY,NFILL,IPTRNX,IERR)
         IF (IERR.NE.2.AND.IERR.NE.6) GO TO 100
            IF (PARMID.EQ.' ') THEN
               WRITE (LP,295) TYPE
               CALL SUWRNS (LP,2,NUMWRN)
               ENDIF
            IF (PARMID.NE.' ') THEN
               WRITE (LP,300) TYPE,PARMID
               CALL SUWRNS (LP,2,NUMWRN)
               ENDIF
            GO TO 130
100      IF (IERR.NE.0) THEN
            CALL SRPPST (PARMID,TYPE,IPTR,LARRAY,NFILL,IPTRNX,IERR)
            CALL SUERRS (LP,2,NUMERR)
            GO TO 130
            ENDIF
         IF (TYPE.EQ.'PCPN') THEN
            IPCHAR=ARRAY(23)
            IF (IPCHAR.GT.0) THEN
C        DELETE PRECIPITATION CHARACTERISTICS
               CALL WPPDCH (IPCHAR,IERR)
               IF (IERR.NE.0) THEN
                  IF (IERR.EQ.1) THEN
                     WRITE (LP,260) 'PCPN','CHAR',PARMID,IPCHAR
                     CALL SUERRS (LP,2,NUMERR)
                     GO TO 130
                     ENDIF
                  IF (IERR.EQ.2) THEN
                     WRITE (LP,270) 'PCPN','CHAR',PARMID,IPCHAR
                     CALL SUWRNS (LP,2,NUMWRN)
                     ENDIF
                  IF (IERR.EQ.3) THEN
                     WRITE (LP,280) 'PCPN','CHAR',IPCHAR,PARMID
                     CALL SUERRS (LP,2,NUMERR)
                     GO TO 130
                     ENDIF
                  GO TO 110
                  ENDIF
               IF (IPRSPC.EQ.1) THEN
                  WRITE (LP,180)
                  CALL SULINE (LP,1)
                  ENDIF
               WRITE (LP,290) 'PCPN','CHAR',PARMID
               CALL SULINE (LP,1)
               ENDIF
            ENDIF
110      IF (TYPE.EQ.'TEMP') THEN
            IPMMMT=ARRAY(17)
            IF (IPMMMT.GT.0) THEN
C           DELETE MAX/MIN TEMPERATURES
               CALL WPPDMT (IPMMMT,IERR)
               IF (IERR.NE.0) THEN
                  IF (IERR.EQ.1) THEN
                     WRITE (LP,260) 'TEMP','MMMT',PARMID,IPMMMT
                     CALL SUERRS (LP,2,NUMERR)
                     GO TO 130
                     ENDIF
                  IF (IERR.EQ.2) THEN
                     WRITE (LP,270) 'TEMP','MMMT',PARMID,IPMMMT
                     CALL SUWRNS (LP,2,NUMWRN)
                     ENDIF
                  IF (IERR.EQ.3) THEN
                     WRITE (LP,280) 'TEMP','MMMT',IPMMMT,PARMID
                     CALL SUERRS (LP,2,NUMERR)
                     GO TO 130
                     ENDIF
                  GO TO 120
                  ENDIF
               IF (IPRSPC.EQ.1) THEN
                  WRITE (LP,180)
                  CALL SULINE (LP,1)
                  ENDIF
               WRITE (LP,290) 'TEMP','MMMT',PARMID
               CALL SULINE (LP,1)
               ENDIF
            ENDIF
         ENDIF
120   XTYPE=TYPE
      IF (TYPE.EQ.'STAN') XTYPE='GENL'
      IF (TYPE.EQ.'UGNL') XTYPE='USER'
      CALL WPPDEL (PARMID,XTYPE,IERR)
      IF (IPTYPE.EQ.2) CALL UREPET (' ',PARMID,LEN(PARMID))
      IF (IERR.NE.0) THEN
         IF (NFLD.GT.0) TYPERR='WARNING'
         IF (NFLD.EQ.-2) TYPERR='ERROR'
         CALL SWPLST (TYPERR,PARMID,XTYPE,IERR)
         IF (TYPERR.EQ.'WARNING') CALL SUWRNS (0,0,NUMWRN)
         IF (TYPERR.EQ.'ERROR') CALL SUERRS (0,0,NUMERR)
         GO TO 130
         ENDIF
C
C  PARAMETER RECORD SUCCESSFULLY DELETED
      CALL SUDWRT (1,'PPP ',IERR)
      IF (IPRSPC.EQ.1) THEN
         WRITE (LP,180)
         CALL SULINE (LP,1)
         ENDIF
      IF (PARMID.EQ.' ') THEN
         WRITE (LP,305) TYPE
         CALL SULINE (LP,1)
         ENDIF
      IF (PARMID.NE.' ') THEN
         WRITE (LP,310) TYPE,PARMID
         CALL SULINE (LP,1)
         ENDIF
      IF (NFLD.GT.0) NDELTE=NDELTE+1
C
C  IF GBOX PARAMETER TYPE, SET INDICATOR THAT COMMON NOT FILLED
      IF (TYPE.EQ.'GBOX') IBXFIL=0
C
130   IF (NFLD.EQ.-2) GO TO 160
C
      NXTFLD=1
      GO TO 10
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  PRINT NUMBER OF PARAMETER RECORDS DELETED
150   IF (NDELTE.EQ.0) THEN
         WRITE (LP,320)
         CALL SULINE (LP,2)
         ENDIF
      IF (NDELTE.GT.0) THEN
         WRITE (LP,330) NDELTE
         CALL SULINE (LP,2)
         ENDIF
C
160   IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'EXIT SLPPP'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
180   FORMAT (' ')
190   FORMAT ('0*** ERROR - IN SLPPP - THE CHARACTERS ',A,
     *   ' WERE EXPECTED IN FIELD ',I2,' BUT ',A,' WERE FOUND.')
210   FORMAT (' IDENTIFIER SET TO : ',A)
220   FORMAT (' TYPE SET TO : ',A)
230   FORMAT (' NULL FIELD FOUND IN FIELD ',I2)
240   FORMAT ('0*** NOTE - RIGHT PARENTHESES ASSUMED IN FIELD ',I2,'.')
250   FORMAT ('0*** ERROR - NUMBER OF CHARACTERS (',I2,
     *   ') IN SINGLE RECORD PARAMETER TYPE SPECIFIED (',A,
     *   ') EXCEEDS ',I2,'.')
260   FORMAT ('0*** ERROR - SYSTEM ERROR ACCESSING ',A,
     *   ' ',A,' FOR IDENTIFIER ',A,'. POINTER=',I3)
270   FORMAT ('0*** WARNING - ',A,' ',A,' AT LOCATION ',I3,
     *   'FOR IDENTIFIER ',A,' ARE ALREADY MARKED AS DELETED.')
280   FORMAT ('0*** ERROR - INVALID VALUE OF ',A,' ',A,
     *   'POINTER (',I3,') FOR IDENTIFIER ',A,'.')
290   FORMAT (' *** NOTE - ',A,' ',A,' VALUES SUCCESSFULLY DELETED ',
     *   'FOR IDENTIFIER ',A,'.')  
295   FORMAT ('0*** WARNING - ',A,' PARAMETERS NOT FOUND.')  
300   FORMAT ('0*** WARNING - ',A,' PARAMETERS NOT FOUND FOR ',
     *   'IDENTIFIER ',A,'.')
305   FORMAT (' *** NOTE - ',A,' PARAMETERS SUCCESSFULLY DELETED.')
310   FORMAT (' *** NOTE - ',A,' PARAMETERS FOR IDENTIFIER ',A,
     *   ' SUCCESSFULLY DELETED.')
320   FORMAT ('0*** NOTE - NO  PARAMETER RECORDS SUCCESSFULLY DELETED.')
330   FORMAT ('0*** NOTE - ',I3,' PARAMETER RECORDS ',
     *   'SUCCESSFULLY DELETED.')
C
      END
