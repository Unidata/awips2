C MEMBER UFI2AZ
C-----------------------------------------------------------------------
C
C  PGM: UFI2AZ(IBUF,IPOS,IWIDTH,NUM,LOC) .. INTGRS TO CHRS, LEADG ZEROS
C
C
C  OUT: IBUF(1) ... BUFFER FOR CHAR STRING, PACKED CHARS - CHARACTER*1
C   IN: IPOS ...... LOCATN OF FIRST CHAR POSTN TO BE USED IN IBUF - INT
C   IN: IWIDTH .... NUM OF CHR POS TO BE USED IN IBUF FOR EA INTGR - INT
C   IN: NUM ....... NUM OF INTGRS IN ARRAY LOC TO BE CONVERTED - INT
C   IN: LOC(1) .... ARRAY OF INTEGERS TO BE PLACED IN IBUF - INT
C
C
C  RQD: SUBPROGRAMS:  FFI2A
C
C
C  HIS: WRITTEN BY D. STREET IN APRIL 1988 USING RTN FFI2A.
C  =====================================================================
      SUBROUTINE UFI2AZ (IBUF,IPOS,IWIDTH,NUM,LOC)
C
C
      CHARACTER*1 IBUF(1)
C
      INTEGER*4 LOC(NUM)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_gen2/RCS/ufi2az.f,v $
     . $',                                                             '
     .$Id: ufi2az.f,v 1.1 1995/09/17 19:02:36 dws Exp $
     . $' /
C    ===================================================================
C
C
C
C  CONVERT TO INTEGER NUMBER(S) IN CHAR STRING(S)
        CALL FFI2A (IBUF,IPOS,IWIDTH,NUM,LOC)
C
C  FOR EACH FIELD, CHANGE LEADING BLANKS TO ZEROS
      IE=IPOS-1
      DO 100 I=1,NUM
         IS=IE+1
         IE=IE+IWIDTH
         DO 90 K=IS,IE
            IF (IBUF(K).NE.' ') GO TO 100
            IBUF(K)='0'
90          CONTINUE
100      CONTINUE
C
  900 RETURN
C
      END
