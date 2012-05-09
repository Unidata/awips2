        SUBROUTINE DB_ISDBFILE  ( afile, iret )
C************************************************************************
C* DB_ISDBFILE                                                          *
C*                                                                      *
C* This subroutine puts DB info into common.                            *
C*                                                                      *
C* DB_ISDBFILE  ( AFILE, IRET )                                 	*
C*                                                                      *
C* Input parameters:                                                    *
C*      AFILE           CHAR*           a file                          *
C*                                                                      *
C* Output parameters:                                                   *
C*      IRET            INTEGER         Return code                     *
C*                                        0 = A2DB file			*
C*                                       -1 = non-A2DB file		*
C**                                                                     *
C* Log:                                                                 *
C* m.gamazaychikov/CWS  05/11                                           *
C* m.gamazaychikov/CWS  09/11	Removed dbflag from CS, added -1 return	*
C************************************************************************
        INCLUDE         'GEMPRM.PRM'
        INCLUDE         'dbcmn.cmn'
C*
        CHARACTER*(*)   afile
        CHARACTER       message*720, funcnm*20, loglevel*6
C*
C------------------------------------------------------------------------
C*
        loglevel = "debug"
        funcnm="DB_ISDBFILE"
        CALL ST_NULL ( funcnm,   funcnm,   lenq, ier )
        CALL ST_NULL ( loglevel, loglevel, lenq, ier )
        IF ( INDEX(afile,'A2DB' ) .gt. 0 ) THEN 
           dbread = .true.
           iret = 0
          ELSE 
           dbread = .false.
           iret = -1
        END IF
        WRITE (message, 1001 ) iret
 1001   FORMAT ("iret=", I5)
        CALL ST_NULL ( message,  message,  lenq, ier )
        CALL DB_MSGCAVE ( funcnm, loglevel, message, ier )
        RETURN
        END
