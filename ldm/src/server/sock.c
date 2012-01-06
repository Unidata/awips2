#include <arpa/inet.h>
#undef NDEBUG
#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <netdb.h>
#include <sys/socket.h>
#include <stddef.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#define PORTMAP
#include <rpc/rpc.h>

int main()
{
    int   status = 0;
    pid_t pid;

    assert(-1 != (pid = fork()));

    if (0 != pid) {
        int childStatus;

        assert(pid == wait(&childStatus));

        status = WEXITSTATUS(childStatus);
    }
    else {
        struct hostent*     hp;
        struct sockaddr_in  addr;
        int                 sock1 = RPC_ANYSOCK;
        CLIENT*             clnt;
        int                 sock2;
        SVCXPRT*            xprt;
        int                 flags;

        assert(NULL != (hp = gethostbyname("emo.unidata.ucar.edu")));

        (void)memset(&addr, 0, sizeof(addr));
        (void)memcpy(&addr.sin_addr, hp->h_addr_list[0], hp->h_length);

        addr.sin_family= AF_INET;
        addr.sin_port = htons(388);

        assert(NULL != (clnt = clnttcp_create(&addr, 300029, 6, &sock1, 0, 0)));

        (void)fprintf(stderr, "sock1 = %d\n", sock1);

        assert(-1 != (flags = fcntl(sock1, F_GETFL)));
        (void)fprintf(stderr, "fcntl(sock1)&O_NONBLOCK = %0x\n",
            flags&O_NONBLOCK);
        (void)fprintf(stderr, "fcntl(sock1)&O_APPEND = %0x\n", flags&O_APPEND);
        (void)fprintf(stderr, "fcntl(sock1)&O_DSYNC = %0x\n", flags&O_DSYNC);
        (void)fprintf(stderr, "fcntl(sock1)&O_RSYNC = %0x\n", flags&O_RSYNC);
        (void)fprintf(stderr, "fcntl(sock1)&O_SYNC = %0x\n", flags&O_SYNC);

        /* NULLPROC-call to server */

        /* FEEDME-call to server */

        assert(-1 != (sock2 = dup(sock1)));

        (void)fprintf(stderr, "sock2 = %d\n", sock2);

        auth_destroy(clnt->cl_auth);
        clnt_destroy(clnt);
        (void)close(sock1);

        assert(-1 != (flags = fcntl(sock2, F_GETFL)));
        (void)fprintf(stderr, "fcntl(sock2)&O_NONBLOCK = %0x\n",
            flags&O_NONBLOCK);
        (void)fprintf(stderr, "fcntl(sock2)&O_APPEND = %0x\n", flags&O_APPEND);
        (void)fprintf(stderr, "fcntl(sock2)&O_DSYNC = %0x\n", flags&O_DSYNC);
        (void)fprintf(stderr, "fcntl(sock2)&O_RSYNC = %0x\n", flags&O_RSYNC);
        (void)fprintf(stderr, "fcntl(sock2)&O_SYNC = %0x\n", flags&O_SYNC);

        errno = 0;
        xprt = svcfd_create(sock2, 0, 0);
        if (0 != errno)
            (void)fprintf(stderr, "%s\n", strerror(errno));
        assert(NULL != xprt);
    }

    return status;
}
