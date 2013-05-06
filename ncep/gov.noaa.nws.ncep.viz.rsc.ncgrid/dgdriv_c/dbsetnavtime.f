        SUBROUTINE DB_SETNAVTIME  ( atime, iret )
C************************************************************************
C* DB_SETNAVTIME                                                        *
C*                                                                      *
C* This subroutine puts DB info into common.                            *
C*                                                                      *
C* DB_SETNAVTIME  ( ATIME, IRET )                                       *
C*                                                                      *
C* Input parameters:                                                    *
C*      ATIME		CHAR*		a time                          *
C*                                                                      *
C* Output parameters:                                                   *
C*      IRET            INTEGER         Return code                     *
C*                                        0 = normal return             *
C**                                                                     *
C* Log:                                                                 *
C* m.gamazaychikov/CWS  10/10                                           *
C************************************************************************
        INCLUDE         'GEMPRM.PRM'
        INCLUDE         'dbcmn.cmn'
C*
        CHARACTER*(*)   atime
C*
        CHARACTER       message*720, funcnm*20, loglevel*6
C------------------------------------------------------------------------
        CALL ST_RNUL (atime, atime, lens, ier)
        loglevel = "debug"
        funcnm="DB_SETNAVTIME"
        CALL ST_NULL ( funcnm,   funcnm,   lenq, ier )
        CALL ST_NULL ( loglevel, loglevel, lenq, ier )
        navtime = atime(:lens)
        isnavtime = .true.
        iret = 0
        message = "set navtime to =" // atime
        CALL ST_NULL ( message,  message,  lenq, ier )
        CALL DB_MSGCAVE ( funcnm, loglevel, message, ier )
        RETURN
        END
