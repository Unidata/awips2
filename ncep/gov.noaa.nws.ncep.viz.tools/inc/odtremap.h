#define BUFSIZE 65536	/* Working buffer size */
#define MINBFW 500000	/* Minimum block size (bytes) for domap */
#define MINBLKSIZ 10	/* Minimum block size (lines) */
#define	PI 3.14159265359 /* PI */
#define FALSE 0
#define TRUE 1
#define MAXSTRING 256	/* Maximum text string for distortion file */

int mapper(int, int);
int domap(int, float *, float *, int, int);
void corner(int, int, int, float *, float *);
int uinit(void);
int init(int, int, int *);
int umap(int, int, int *, int *);
int findpoint(float,float,int *,int *);

struct remap
{
	int in_bfw;		/* Block size (bytes) input file */
	int out_bfw;	/* Block size (bytes) output file */
	int nspl;		/* Number of splines/line */
	int nspe;		/* Number of splines/elem */
	int slb;		/* Source blocksize */
	int dlb;		/* Dest blocksize */
	int ncl;		/* Number of corners in line */
	int nce;		/* Number of corners in elem */
	FILE *in_fd;	/* Input file descriptor */
	FILE *out_fd;	/* Output file descriptor */
} remap_vars;

struct tiff_header {	/* TIFF header */
	short int order;	/* Byte order */
	short int version;	/* Version */
	int point;			/* Pointer */
} ;

struct tiff_record {		/* TIFF directory record */
	short int tag;		/* TIFF tag */
	short int type;		/* Data type */
	int length;			/* Length */
	int voff;			/* Pointer or value */
} ;

struct dis
{
	double xrectl;	/* Output number of lines */
	double xrecte;	/* Output number of elems */
} dis_vars;

struct tiff {
	int nbits;
	int photo;
	int unit;
	int in_lines;
	int in_elems;
	int out_lines;
	int out_elems;
	int ratx[2];
	int raty[2];
} tiff_vars;
