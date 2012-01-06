C MODULE IUNRCD
C-----------------------------------------------------------------------
C
        FUNCTION IUNRCD (NWORDS,LRECL)
C
C  FUNCTION TO COMPUTE THE NUMBER OF RECORDS NEEDED FOR NWORDS WITH A 
C  RECORD LENGTH OF LRECL.
C
C  ARGUMENT LIST:
C 
C     NAME     TYPE  I/O   DIM   DESCRIPTION
C     ----     ----  ---   ---   -----------
C     NWORDS    I     I     1    NUMBER OF WORDS
C     LRECL     I     I     1    LOCICAL RECORD LENGTH
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_util/RCS/iunrcd.f,v $
     . $',                                                             '
     .$Id: iunrcd.f,v 1.2 1999/01/19 19:42:21 page Exp $
     . $' /
C    ===================================================================
C
C
      IUNRCD=0
C    
      IF (NWORDS.GT.0.AND.LRECL.GT.0) THEN
         IUNRCD=(NWORDS+LRECL-1)/LRECL
         ENDIF
C         
      RETURN
C      
      END
