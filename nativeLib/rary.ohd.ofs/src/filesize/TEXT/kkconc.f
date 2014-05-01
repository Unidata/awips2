C MODULE KKCONC
C  =====================================================================
C  pgm: KKCONC .. Concatenate 2 strings with provided delimiter
C
C  i/o:      STRING - string added to - CHAR*(*)
C   in:      APPEND - string to be appended - CHAR*(*) 
C   in:      DELIM  - delimiter string - CHAR*(*)
C  =====================================================================
       SUBROUTINE KKCONC(STRING, APPEND, DELIM)

       INTEGER IS,IE,LEN
       CHARACTER*(*) STRING, APPEND, DELIM
       CHARACTER*256 TEMP
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/filesize/RCS/kkconc.f,v $
     . $',                                                             '
     .$Id: kkconc.f,v 1.1 2006/05/09 13:18:21 jgofus Exp $
     . $' /
C    ===================================================================
C

       LEN=LENSTR(STRING)
       CALL KKTRIM(APPEND,IS,IE)
       STRING=STRING(1:LEN)//DELIM//APPEND(IS:IE)

       RETURN
       END
