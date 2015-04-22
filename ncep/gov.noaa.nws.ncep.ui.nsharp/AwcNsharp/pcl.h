/* PCL Raster Graphics Definitions */
void print_sounding_pcl( void );
void pcprinter_setup( FILE *fp );
void to_pclraster( short row, short maxcol, FILE *fp );
void pcclose_printer( FILE *fp );

