C MODULE WPDD
C-----------------------------------------------------------------------
C
       SUBROUTINE WPDD (IDSTA,IDTYPE,ISTAT)
C
C          ROUTINE:  WPDD
C          VERSION:  1.0.0
C             DATE:  2-8-83
C           AUTHOR:  JANINE FRANZOI
C                    DATA SCIENCES INC
C
C***********************************************************************
C
C          DESCRIPTION:
C
C  THIS ROUTINE DELETES A STATION FROM THE PREPROCESSOR DATA BASE.
C
C***********************************************************************
C
C          ARGUMENT LIST:
C
C       NAME      TYPE  I/O   DIM    DESCRIPTION
C
C       IDSTA      A8    I     2     STATION IDENTIFIER
C                  OR
C                  I*4   I     1     STATION NUMBER
C       IDTYPE     I     I     1     IDENTIFICATION CODE
C                                      0 = IDSTA IS IDENTIFIER
C                                      1 = IDSTA IS NUMBER
C       ISTAT      O     I     1     STATUS INDICATOR
C                                      0 = NORMAL RETURN
C                                      1 = IDSTA NOT ON FILE
C                                      2 = SYSTEM ERROR - ID LOCATION
C                                          ON FILE INVALID
C                                      3 = READ OR WRITE ERROR
C
C***********************************************************************
C
C          COMMON:
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'pdbcommon/pdunts'
      INCLUDE 'pdbcommon/pdrrsc'
      INCLUDE 'pdbcommon/pddtdr'
      INCLUDE 'pdbcommon/pdbdta'
      INCLUDE 'pdbcommon/pdsifc'
      INCLUDE 'pdbcommon/pdhshi'
      INCLUDE 'pdbcommon/pdhshc'
      INCLUDE 'pdbcommon/pdi2max'
C
C***********************************************************************
C
C          DIMENSION AND TYPE DECLARATIONS:
C
      PARAMETER (LDELTP=20)
      CHARACTER*4 DELTP(LDELTP)
      CHARACTER*8 IDELET
      PARAMETER (LSIBUF=128)
      INTEGER*2 ISIBUF(LSIBUF),JSIBUF(LSIBUF)
C
      DIMENSION IDSTA(2)
      DIMENSION IRRBUF(16),IFPBUF(16)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_pdbrw/RCS/wpdd.f,v $
     . $',                                                             '
     .$Id: wpdd.f,v 1.5 2003/03/14 18:55:35 dws Exp $
     . $' /
C    ===================================================================
C
C
C***********************************************************************
C
C          DATA:
C
      DATA    IDELET / 'DELETED ' /
C***********************************************************************
C
C
      ISTAT=0
C
      IFINDI=0
C
      IF (IPDTR.GT.0) WRITE (IOGDB,130)
C
C  CHECK TYPE CODE FOR INPUT
      IF (IDTYPE.EQ.1) GO TO 10
      IF (IDTYPE.NE.0) GO TO 120
C
C  GET SIF RECORD NUMBER USING STATION IDENTIFIER
      CALL PDFNDR (IDSTA,LSIBUF,IFINDC,ISIREC,ISIBUF,IFREE,ISTAT)
      IF (ISTAT.NE.0) GO TO 110
      IF (IFINDC.EQ.0) GO TO 90
C
C  CHECK IF STATION HAS STATION NUMBER
      NUMSTA=ISIBUF(6)
      IF (NUMSTA.EQ.0) GO TO 20
C
C  GET SIF RECORD NUMBER USING STATION NUMBER
      CALL PDFNDI (NUMSTA,LSIBUF,IFINDI,LSIREC,ISIBUF,IFREE,ISTAT)
      IF (ISTAT.NE.0) GO TO 110
C
C  CHECK IF STATION NUMBER IS VALID
      IF (ISIREC.NE.LSIREC) GO TO 100
      GO TO 20
C
C  GET SIF RECORD NUMBER USING STATION NUMBER
10    CALL PDFNDI (IDSTA(1),LSIBUF,IFINDI,ISIREC,ISIBUF,IFREE,ISTAT)
      IF (ISTAT.NE.0) GO TO 110
      IF (IFINDI.EQ.0) GO TO 90
C
C  GET SIF RECORD NUMBER USING STATION IDENTIFIER
      CALL PDFNDR (ISIBUF(2),LSIBUF,IFINDC,LSIREC,ISIBUF,IFREE,ISTAT)
      IF (ISTAT.NE.0) GO TO 110
      IF (ISIREC.NE.LSIREC) GO TO 100
C
C  ZERO STATION NUMBER IN SIF RECORD
20    ISIBUF(6)=0
C
C  SET THE STATION ID TO DELETED
      CALL UMEMOV (IDELET,ISIBUF(2),2)
C
C  ZERO NEW RECORD FOR DELETE
      CALL UMEMS2 (0,JSIBUF,1,LSIBUF)
C
C  SET POINTER AND DATA SLOTS TO UNUSED
      NDELTP=0
      ISAVPP=0
      ISAVTA=0
      CALL PDCOLD (ISIBUF,JSIBUF,LDELTP,DELTP,NDELTP,
     *   ISAVPP,ISAVTA,ISTAT)
      IF (ISTAT.NE.0) GO TO 110
C
C  CHECK IF ANY ADDITIONAL TYPES DEFINED
      NTYPE=ISIBUF(10)
      IF (NTYPE.GT.0) THEN
C     PROCESS EACH TYPE      
         J=11
         DO 60 I=1,NTYPE
            CALL UMEMOV (ISIBUF(J),IRTYPE,1)
C        CHECK IF AN RRS TYPE            
            IRX=IPDCKR(IRTYPE)
            IF (IRX.EQ.0) GO TO 50
            IRREC=ISIBUF(J+2)
CMGM 4/2002 THE RRS RECORD NUMBER (IRREC) MAY BE STORED IN THE ISIBUF
C    ARRAY AS A NEGATIVE NUMBER. THIS ALLOWS TWICE AS MANY RECORD 
C    NUMBERS IN THE I2 ARRAY. CONVERT IRREC TO A POSITIVE RECORD NUMBER
C    IRREC2 TO READ/WRITE (UREADT/UWRITT) THE KPDRRS FILE.
            IRREC2=IRREC
	    IF(IRREC2.LT.0)IRREC2=IRREC2+2*I2MAX
            IF (IPDDB.GT.0) THEN
               WRITE (IOGDB,140) IRREC
               WRITE (IOGDB,150) KPDRRS
            ENDIF
C        READ THE FIRST RRS RECORD
            CALL UREADT (KPDRRS,IRREC2,IRRBUF,ISTAT)
            IF (ISTAT.NE.0) GO TO 110
C        SET THE STATION ID TO DELETED
            CALL UMEMOV (IDELET,IRRBUF(2),2)
            IF (IPDDB.GT.0) WRITE (IOGDB,160) IRRBUF(13)
C        CHECK IF ANY FREEPOOL RECORDS            
            IF (IRRBUF(13).LE.0) GO TO 40
            IFPREC=IRRBUF(13)
C        READ FREEPOOL RECORD
30          CALL UREADT (KPDDDF(LUFREE),IFPREC,IFPBUF,ISTAT)
C jgg            IF (ISTAT.NE.0) GO TO 110
C jgg 01/03 changed handling of error to allow deleting record - MR 1731
            IF (ISTAT.NE.0) THEN
	       WRITE( LP, 210)
	       GO TO 40
	    ENDIF   
C jgg end of change	       
C        WRITE FREEPOOL RECORD
            IFPRECC=IFPBUF(1)
            IFPBUF(1)=-1
            CALL UWRITT (KPDDDF(LUFREE),IFPREC,IFPBUF,ISTAT)
            IF (ISTAT.NE.0) GO TO 110
            IF (IPDDB.GT.0) WRITE (IOGDB,170) IFPRECC
C        CHECK IF ANY FREEPOOL RECORDS            
            IF (IFPRECC.GT.0) THEN
               IFPREC=IFPRECC
               GO TO 30
               ENDIF
C        WRITE THE FIRST RRS RECORD             
40          CALL UWRITT (KPDRRS,IRREC2,IRRBUF,ISTAT)
            IF (ISTAT.NE.0) GO TO 110
50          J=J+3
60          CONTINUE
         ENDIF
C
C  WRITE SIF RECORD
      CALL UWRITT (KPDSIF,ISIREC,ISIBUF,ISTAT)
      IF (ISTAT.NE.0) GO TO 110
      NPDSTA=NPDSTA-1
C
C  RESET INTEGER HASH ARRAY
      IF (IFINDI.GT.0) IPDHSI(IFINDI)=-1
C
C  RESET CHARACTER HASH ARRAY
      IPDHSC(IFINDC)=-1
C
      IF (IPDDB.GT.0) WRITE (IOGDB,180) ISIREC
      GO TO 120
C
C  STATION ID NOT FOUND
90    ISTAT=1
      GO TO 120
C
C  SYSTEM ERROR
100   WRITE (LP,190) IDSTA,ISIREC,LSIREC
      ISTAT=2
      GO TO 120
C
C  SYSTEM ERROR
110   ISTAT=3
C
120   IF (IPDTR.GT.0) WRITE (IOGDB,200)
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
130   FORMAT (' *** ENTER WPDD')
140   FORMAT (' RRS DELETED RECORD IS ',I6)
150   FORMAT (' RRS FILE UNIT IS ',I4)
160   FORMAT (' RRS FREEPOOL RECORD IS ',I6)
170   FORMAT (' CONTINUATION FREEPOOL RECORD IS ',I6)
180   FORMAT (' ISIREC=',I6)
190   FORMAT ('0**ERROR** IN WPDD - STATION NUMBER OR ',
     *   'STATION ID ',2A4,' INVALID. ISIREC=',I6,' LSIREC=',I6)
200   FORMAT (' *** EXIT WPDD')
210   FORMAT (' ** IGNORING ERROR READING FREEPOOL RECORD')
C
      END
