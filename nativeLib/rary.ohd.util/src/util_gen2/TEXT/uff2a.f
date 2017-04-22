C MODULE UFF2A
C-----------------------------------------------------------------------
C
C  ROUTINE UFF2A CONVERTS A REAL VALUE TO CHARACTERS.
C
      SUBROUTINE UFF2A (REALVL,CHAR,IBEG,NCHAR,NDECML,IPRERR,NPUNIT,
     *   ISTAT)
C
      CHARACTER*1 CHAR(1)
C
      INCLUDE 'uiox'
      INCLUDE 'ucmdbx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_gen2/RCS/uff2a.f,v $
     . $',                                                             '
     .$Id: uff2a.f,v 1.3 2001/06/13 09:59:42 mgm Exp $
     . $' /
C    ===================================================================
C
      IF (ICMTRC.GT.0) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,*) 'ENTER UFF2A'
         ENDIF
C
      ISTAT=0
C
      NVALUE=1
C
      IF (ICMDBG.GT.0) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,20) NVALUE,IBEG,NCHAR,REALVL
         ENDIF
C
C  CONVERT REAL
      CALL FFF2A (CHAR,IBEG,NCHAR,NDECML,NVALUE,REALVL)
      IF (CHAR(IBEG).EQ.'*') THEN
         IF (IPRERR.GT.0) THEN
            CALL UEROR (NPUNIT,1,-1)
            WRITE (NPUNIT,30) REALVL,NCHAR,NDECML
            ENDIF
         ISTAT=1
         ENDIF
C
      IF (ICMDBG.GT.0) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,40)
     *      (CHAR(N),N=IBEG,IBEG+NCHAR-1)
         ENDIF
C
      IF (ICMTRC.GT.0) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,*) 'EXIT UFF2A - ISTAT=',ISTAT
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
20    FORMAT (' NVALUE=',I3,3X,'IBEG=',I3,3X,'NCHAR=',I3,3X,
     *   'REALVL=',F13.3)
30    FORMAT ('+*** ERROR - IN UFF2A - REAL VALUE ',F13.3,' CANNOT BE ',
     *   'STORED IN ',I2,' CHARACTERS USING ',I2,' DECIMAL PLACES.')
40    FORMAT (' CHAR(IBEG...IBEG+NCHAR-1)=',50A1)
C
      END
