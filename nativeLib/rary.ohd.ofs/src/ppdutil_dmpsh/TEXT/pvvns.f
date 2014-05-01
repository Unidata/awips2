C MODULE PVVNS
C-----------------------------------------------------------------------
C
C  ROUTINE TO CHECK IF STATION TO BE PROCESSED.
C
      SUBROUTINE PVVNS (STAID,LSSTA,LSMSF,NSTA,IMF,ICOND)
C
C   IN: STAID ...... STA ID NAME (AS 8 CHARS) FOR CURRENT OPERATN - CHAR
C   IN: LSSTA(1) ... LIST OF ALLOWABLE STA (8 CHR/STA), USES 'ALL' - INT
C   IN: LSMSF(1) ... INDICATORS TO INCLUDE MISSG DATA (0,2=NO  1,3=YES),
C   IN:              OR ESTIMATED DATA (0,1=NO  2,3=YES) - INT
C   IN: NSTA ....... NUMBER OF STATION NAMES IN LSSTA - INT
C  OUT: IMF ........ INCLUDE-MISSING-DATA FLAG, 0=NO, 1=YES - INT
C  I/O: ICOND ...... PGM COND: IF NOT 0 SKIP, SET 1 = ERR, NO TYP - INT
C
      CHARACTER*8 STAID
      CHARACTER*4 LSSTA(*)
      DIMENSION LSMSF(*)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppdutil_dmpsh/RCS/pvvns.f,v $
     . $',                                                             '
     .$Id: pvvns.f,v 1.2 2000/07/21 19:53:59 page Exp $
     . $' /
C    ===================================================================
C
C
      CALL PVSUBB ('PVVNS    ',ICOND)
C
      ICOND=0
C
C  CHCEK IF STATION IN LIST
      IMF=0
      ILSSTA=1
      DO 45 ISTA=1,NSTA
	 IF (LSSTA(ILSSTA).EQ.'ALL') THEN
            IMF=LSMSF(ISTA)
            GO TO 50
	    ENDIF
         CALL U4BCP (STAID(1:4),LSSTA(ILSSTA),ISTAT)
         IF (ISTAT.NE.0) GO TO 40
         CALL U4BCP (STAID(5:8),LSSTA(ILSSTA+1),ISTAT)
         IF (ISTAT.NE.0) GO TO 40
         IMF=LSMSF(ISTA)
	 GO TO 50
40       ILSSTA=ILSSTA+2
45       CONTINUE      
C
      ICOND=1       
C
50    CALL PVSUBE ('PVVNS    ',ICOND)
C
      RETURN
C
      END
