        SUBROUTINE DB_GETEVTNAME  ( aname, iret )
C************************************************************************
C* DB_GETEVTNAME                                                        *
C*                                                                      *
C* This subroutine gets evtname from  common.                           *
C*                                                                      *
C* DB_GETEVTNAME  ( ANAME, IRET )                                       *
C*                                                                      *
C* Input parameters:                                                    *
C*      ANAME           CHAR*           Event name                      *
C*                                                                      *
C* Output parameters:                                                   *
C*      IRET            INTEGER         Return code                     *
C*                                        0 = normal return             *
C**                                                                     *
C* Log:                                                                 *
C* m.gamazaychikov/CWS	07/09                                           *
C************************************************************************
        INCLUDE         'GEMPRM.PRM'
        INCLUDE         'dbcmn.cmn'
C*
        CHARACTER*(*)   aname
        CHARACTER       message*720, funcnm*20, loglevel*6
C*
C------------------------------------------------------------------------
C*
        loglevel = "info"
        funcnm="DB_GETEVTNAME"
        CALL ST_NULL ( funcnm,   funcnm,   lenq, ier )
        CALL ST_NULL ( loglevel, loglevel, lenq, ier )

        IF ( isevtname ) THEN 
           CALL ST_LSTR ( evtname, levtname, ier )
           aname = evtname ( :levtname)
           iret = 0   
c           isevtname = .false.
           message = "setting event name to " // evtname ( :levtname)
           CALL ST_NULL ( message,  message,  lenq, ier )
           CALL DB_MSGCAVE ( funcnm, loglevel, message, ier )
         ELSE
           iret = -1
           aname =  ' '
        END IF
        IF ( isensnames ) THEN
c           print *, "DB_GETEVTNAME setting ens member name"
c           print *, "DB_GETEVTNAME before setting iensmem=", iensmem
           CALL ST_LSTR ( ensnames, lensnames, ier )
           aname = ensnames(:lensnames)
c           print *, "DB_GETEVTNAME set ens member name to ", aname
           message = "set ens member name to " // aname 
           CALL ST_NULL ( message,  message,  lenq, ier )
           CALL DB_MSGCAVE ( funcnm, loglevel, message, ier )
c           print *, "DB_GETEVTNAME after setting iensmem=", iensmem
        END IF
        CALL ST_NULL (aname, aname, lstr, ier)
        RETURN
        END
