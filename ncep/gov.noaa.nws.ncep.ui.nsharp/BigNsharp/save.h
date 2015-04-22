#define MAX_COLORS 6
unsigned long colors[MAX_COLORS];
int ncolors;

void set_color(int color);
int get_colors(void);
int save_gif(char *filename);
void load_font(XFontStruct **font_info);
void draw_string(char *coord, float x, float y, char *str);
