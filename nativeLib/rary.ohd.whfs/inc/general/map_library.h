
/****************************************************************************
 *
 * Header: map_library.h
 *
 * Description: contains prototypes for map api
 *
 ***************************************************************************/

#ifndef MAP_LIBRARY_H
#define MAP_LIBRARY_H

#include <Xm/Xm.h>
#include "map_defines.h"
#include "map_menubar_cb.h"

/*-------------------------------------------------------------------------
 
 Prototype definations

 --------------------------------------------------------------------------*/

extern int mDefineOverlay ( int overlay ,
                            enum MapState status ,
                            char * color ,
                            int line_width ,
                            char ** filepath , char ** filename ,
                            int num_files,
                            enum MapOverlayTypes type ,
                            enum MapState store_in_memory ,
                            enum MapState fillarea ,
                            OverlayCalculatorRoutine pCalcRoutine ,
                            ExternalOverlayRoutine pDatabaseRoutine ) ;
extern void mSetRegionArea(float,float,float,float);
extern int mGetLegendStatus();
extern void mCreateMapScreen ( Widget parent , char res_title [ ] ) ;
extern int mInitLegend ( int leg , int legend_location , int width ,
                         int height ) ;
extern void mInitMapScreen(int,int,int,int,float,int,int,char*) ;
extern void mInitMap ( int num_maps , int overv , char * bgcolor , 
                       char * border_color ) ;
extern int mSetOverlayType ( int overlay ,
                             enum MapOverlayTypes type ) ;
extern struct _Overlay * mGetOverlay ( int overlay ) ;
extern void mOpenMapScreen();
extern int mAddLegendDrawCb ( int index , MotionCallback legend_routine ) ;
extern void mAddLegendExpose(int,void (*)());
extern void mAddMapExpose(int,void (*)());
extern void mAddMapSelectRoutine(int,void (*)());
extern void mAddLegendSelectRoutine(int,  MouseClickEventCallback routine );
extern void mAddAboutAppCallback ( MotionCallback about_app_cb ) ;
extern void mAddLeaveCallback ( int, MotionCallback ) ;
extern void mAddMotionCallback ( int, MotionCallback ) ;
extern void mChangeTitle(char *);
extern void mSetCursor(int);
extern void mSetFont(int);
extern int  mGetFont();
int mGetStringWidth(char * string);
int mGetStringHeight(char * string);
extern void mUpdateMap(int);
extern void mUpdateLegend(int);
extern void mCloseMapScreen();
extern void mSetMapSize(int,int);
extern void mSetCenterLatLon(float,float);
extern void mSetMapProjection(int);
extern void mSetWidthInNmi(float);
extern float mGetWidthInNmi ( ) ;
extern void mSetLineWidth(int);
extern void mSetColor(char *);
extern void mDrawCircle(int,int,int,int,int,int);
extern void mDrawFillCircle(int,int,int,int,int,int);
extern void mDrawLine(int,int,int,int,int,int);
extern void mDrawBox(int,int,int,int,int,int);
extern void mDrawFillBox(int,int,int,int,int,int);
extern void mDrawFillPolygon(int,int,XPoint *,int,int,int);
extern void mDrawText(int,int,int,int,char *);
extern void mDrawSymbol(int,int,int,int,char *,int);
extern int mSetOverlayColor( int index ,
                             char * color ) ;
extern const char * mGetOverlayColor ( int index ) ;
extern int mGetOverlayLineWidth ( int index ) ;
extern int mSetMemoryFlag ( int overlay ,
                             enum MapState store_in_memory ) ;
extern int mSetOverlayCalcRoutine ( int overlay ,
                                    OverlayCalculatorRoutine pCalcRoutine ) ;
extern OverlayCalculatorRoutine mGetOverlayCalcRoutine 
                               ( int overlay ) ;
extern int mSetOverlayExternalRoutine ( int overlay ,
                                    ExternalOverlayRoutine pDatabaseRoutine ) ;
extern int mSetFileInfo ( int overlay ,
                          int num__files ,
                          char ** filepath ,
                          char ** filename ) ;
extern void mConvertLatLon2XY(float,float,int *,int *);
extern void mConvertXY2LatLon(int,int,float *,float *);
extern void mSetRegion(int,char *);
extern void mCreateMenu(Widget *,Widget *,char * , char);
extern void mCreateMenuItem ( Widget , Widget * , char * , char , char *, char *) ;
extern void mCreateMenuToggle ( Widget , Widget * , char * , char, char *, char *) ;
extern void mCreateMenuSeparator(Widget);
extern void mAddMapCleanUp ( CleanCallback clean_routine ) ;
extern void mAddStationMenuItem(Widget *,char *);
extern void mAddStationMenuToggle(Widget *,char *);
extern void mAddStationMenuSeparator();
extern int mGetMapProjection();
extern void mGetCenterLatLon(float *,float *);
extern void mGetUpperLeftLatLon ( float * , float * ) ;
extern void mAddFocusChangeRoutine ( int index , GeneralCallback routine ) ;
extern void mDisableExposeWatch ( ) ;
extern void mEnableExposeWatch ( ) ;

#endif /* #ifndef MAP_LIBRARY_H */
