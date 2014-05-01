#include <limits.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>

#include "error.h"
#include "ulog.h"

int
main(int argc, char* args[])
{
    int status;
    if (openulog("errorTest", LOG_PID, 0, "-") == -1) {
        perror("openulog()");

        status = 1;
    }
    else {
        ErrorObj* err;

        (void)setulogmask(LOG_UPTO(LOG_DEBUG));

        err = ERR_NEW(0, NULL, "Simple message");
        err_log(err, ERR_ERROR); 
        err_free(err);

        err = ERR_NEW(
            0,
            ERR_NEW(1, NULL, "Nested message 2"),
            "Nested message 1");
        err_log(err, ERR_ERROR); 
        err_free(err);

        err =
            ERR_NEW(
                0,
                ERR_NEW1(
                    1,
                    ERR_NEW1(2, NULL, "Nested message 3: %s", 
                        strerror(ENOMEM)),
                    "Nested message 2: %d",
                    INT_MAX),
                "Nested message 1");
        err_log(err, ERR_DEBUG); 
        err_free(err);

        status = 0;
    }

    return status;
}
