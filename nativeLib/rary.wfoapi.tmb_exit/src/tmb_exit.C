// +++ 	Name: tmb_exit
//	Purpose: This program is either called by TestMode Controller Program
//		(TMCP) to request TMB to exit or can be a utility program.
//	Inputs: This program takes 2 arguments
//		(1) Host or machine which is running.
//		(2) The port number the TMB is listening on.
//		e.g. tmb_exit lx6-nhda 1234
//
// History:
// 23-dec-04 P. Wu	Initial
// ---*************************************************************************
#include "testmode.H"

int main(int argc, char *argv[])
{
  int sock;
  int rc;
  struct sockdata_t input_buf;
  int port;

  if (argc != 3) {
    fprintf(stderr,"Usage:  tmb_exit host port\n");
    fprintf(stderr,"where host is the machine which is running the\n");
    fprintf(stderr,"tmb program, and port is the port number (e.g. 1234)\n");
    fprintf(stderr,"listening on.\n");
    exit(EXIT_FAILURE);
  }

  ignore_pipe();
  
  port = get_port(DEFAULT_PORT_NUM, SOCK_STREAM);

  sock = make_connection(argv[2], SOCK_STREAM, argv[1], port);
  if (sock == -1) {
    fprintf(stderr,"make_connection failed.\n");
    return -1;
  }

  sprintf(input_buf.sender,"tmb_exit_handler"); 
  sprintf(input_buf.msg,"Exit"); 

  rc = send(sock, (const void*)&input_buf, sizeof(input_buf), 0);

  close(sock);
  return 0;
}
