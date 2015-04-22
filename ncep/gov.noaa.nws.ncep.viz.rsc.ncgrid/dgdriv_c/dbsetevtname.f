        SUBROUTINE DB_SETEVTNAME  ( aname, iret )
C************************************************************************
C* DB_SETEVTNAME                                                        *
C*                                                                      *
C* This subroutine puts DB debug into common.                            *
C*                                                                      *
C* DB_SETEVTNAME  ( ANAME, IRET )                                       *
C*                                                                      *
C* Input parameters:                                                    *
C*      ANAME		CHAR*		a name                          *
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
        funcnm="DB_SETEVTNAME"
        CALL ST_NULL ( funcnm,   funcnm,   lenq, ier )
        CALL ST_NULL ( loglevel, loglevel, lenq, ier )

        CALL ST_RNUL (aname, aname, lens, ier)
        evtname = aname(:lens)
        isevtname = .true.
        iret = 0
        message = "setting event name to " // evtname ( :lens)
        CALL ST_NULL ( message,  message,  lenq, ier )
        CALL DB_MSGCAVE ( funcnm, loglevel, message, ier )
        RETURN
        END
