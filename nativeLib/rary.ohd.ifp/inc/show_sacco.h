#include <stdio.h>
#include <sacco.h>

#define  UNITS_METRIC     1
#define  UNITS_ENGLISH    0

typedef struct {
char ID[50] ;
char Seg[20] ;
char Model[20] ;
float MaxWaterLevels[7] ;  /*use %5.2f format */
float CurrentWaterLevels[7] ;  /*use %5.2f format*/
float NewWaterLevels[7] ;  /*use %5.2f format*/
int ActionFlag;  /*1=OK, 0=no action taken, -1 =false*/
} WaterLevelType ;
