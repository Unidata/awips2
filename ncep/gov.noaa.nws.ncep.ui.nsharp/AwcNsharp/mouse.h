/* Include file for mouse calls. */

/* Mouse events */
#define LEFT_DOWN   0x01        /* 0000 0010 Left button pressed    */
#define RIGHT_DOWN  0x02        /* 0000 1000 Right button pressed   */
#define MIDDLE_DOWN 0x04        /* 0010 0000 Middle button pressed  */

/* Mouse event structure */
typedef struct _EVENT
{
    short     x, y;
    unsigned  fsBtn;
} EVENT;

/* Mouse pointer shape union containing structures for graphics and text */
typedef union _PTRSHAPE
{
    struct
    {
        unsigned char atScreen;
        unsigned char chScreen;
        unsigned char atCursor;
        unsigned char chCursor;
    } t;
    struct
    {
        unsigned xHot, yHot;
        unsigned afsPtr[32];
    } g;
} PTRSHAPE;

/* Values for SetPtrVis function */
typedef enum _PTRVIS { SHOW = 1, HIDE } PTRVIS;

/* Public mouse functions */
int MouseInit( void );
int GetMouseEvent( EVENT *pEvent );
int GetPtrPos( EVENT *pEvent );
int SetPtrPos( short x, short y );
int SetPtrVis( PTRVIS pv );
int SetPtrShape( PTRSHAPE *ps );
