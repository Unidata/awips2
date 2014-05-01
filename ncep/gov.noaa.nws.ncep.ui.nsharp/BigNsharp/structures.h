/* NSHARP global structures */
struct _skewt
{
	   short tlx;
	   short tly;
	   short brx;
	   short bry;
	   short hspread;
	   short vspread;
	   short brtemp;
	   short type;
};

struct _hodog
{
	   short tlx;
	   short tly;
	   short brx;
	   short bry;
	   short xshift;
	   short yshift;
	   short hodomag;
	   short scale;
};

struct _stpinset
{
	short tlx;
	short tly;
	short brx;
	short bry;
	short brstp;
};

struct _configure
{
	char filename[256];
	char prntype[16];
	char lptname[64];
};

struct _pagemodes
{
	char title[80];
	int  tlx;
	int  tly;
	int  brx;
	int  bry;
};

struct _startup
{
	char filename[256];
	char dattim[80];
	char station[80];
	char giffile[256];
};
