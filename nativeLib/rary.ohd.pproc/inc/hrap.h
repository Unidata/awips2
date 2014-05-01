#ifndef HRAP_H
#define HRAP_H

typedef struct HRAP    {
                        float x,y;
                       } HRAP;

typedef struct         {
                        int x, y;
                       } point;

HRAP LatLongToHrapPproc(float lat, float lon);

HRAP HrapToLatLongPproc(point hrap);

#endif /* #ifndef HRAP_H */
