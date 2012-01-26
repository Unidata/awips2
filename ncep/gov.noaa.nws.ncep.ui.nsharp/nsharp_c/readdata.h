	/* READDATA Function Prototypes */

	short read_sndg1( void );
	short read_sndg2( char *searchtag );
	void xtnd_sndg( void );
	short read_config( void );
	void interp_sndg( void );
	void read_command( int argc, char *argv[], char *envp[] );
	short get_sndg( void );
	void security(char *envp[] );
	void copy_sndg(void);

