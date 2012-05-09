        SUBROUTINE DB_SETENSMBRS  ( aname, iret )
C************************************************************************
C* DB_SETENSMBRS                                                        *
C*                                                                      *
C* This subroutine puts DB info into common.                            *
C*                                                                      *
C* DB_SETENSMBRS  ( ANAME, IRET )                                       *
C*                                                                      *
C* Input parameters:                                                    *
C*      ANAME           CHAR*           a name                          *
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
        CHARACTER*(*)   aname

        CHARACTER       message*720, funcnm*20, loglevel*6
C*
C------------------------------------------------------------------------
C*
        loglevel = "debug"
        funcnm="DB_SETENSMBRS"
        CALL ST_NULL ( funcnm,   funcnm,   lenq, ier )
        CALL ST_NULL ( loglevel, loglevel, lenq, ier )
        CALL ST_RNUL (aname, aname, lens, ier)
        message = "setting ensnames to " // aname(:lens)
        CALL ST_NULL ( message,  message,  lenq, ier )
        CALL DB_MSGCAVE ( funcnm, loglevel, message, ier )
        ensnames = aname(:lens)
        isensnames = .true.
        iensmem = 1
        iret = 0
        RETURN
        END
