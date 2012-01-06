/* READDATA Function Prototypes */

short read_sndg1(void);
short read_sndg3(char *searchtag);
short read_sndg2(void);
short read_config(void);
void  read_command(int argc, char *argv[], char *envp[]);
short get_sndg(void);
void  security(char *envp[]);
void  copy_sndg(void);

void swapsoundings(void);
