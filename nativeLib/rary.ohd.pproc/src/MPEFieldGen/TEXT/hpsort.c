#include "mpe_fieldgen.h" 

/************************************************************************
* function performs heap-sorting based on hrap_x.
*
* version Aug 17, 2005 by Guoxian Zhou
*
* input 
*
* arraySize - number of data points 
* hrap_x    - array of HRAP x-coordinate of data 
* hrap_y    - array of HRAP y-coordinate of data 
* heapArray - array of data 
*
* output 
*
* hrap_x    - sorted array of HRAP x-coordinate of data 
* hrap_y    - sorted array of HRAP y-coordinate of data 
* heapArray - sorted array of data 
* ***********************************************************************
*/

void MPEFieldGen_trickleDownForGeoIndex(float heapArray[], short hrap_x[], short hrap_y[],
                    int currentSize, int index)
{
    int largerChild;
    short topIndex_x = hrap_x[index] ;   // save root
    short topIndex_y = hrap_y[index] ;
    float top = heapArray[index];

    while(index < currentSize/2)   /* not on bottom row */
    {
        int leftChild  = 2 * index + 1 ;
        int rightChild = leftChild + 1 ;

        /* find larger child */
        if(rightChild < currentSize &&
            hrap_x[leftChild] < hrap_x[rightChild])
            largerChild = rightChild ;
        else
            largerChild = leftChild ;

        /* top >= largerChild ? */
        if(topIndex_x >= hrap_x[largerChild])
            break ;

        heapArray[index] = heapArray[largerChild] ;
        hrap_x[index] = hrap_x[largerChild] ;
        hrap_y[index] = hrap_y[largerChild] ;
        index = largerChild ;    
    }

    heapArray[index] = top ;
    hrap_x[index] = topIndex_x ;
    hrap_y[index] = topIndex_y ;

}

void MPEFieldGen_heapSortForGeoIndex(float heapArray[], short hrap_x[], 
                    short hrap_y[], int arraySize)
{
    int j;
    short biggestIndex_x, biggestIndex_y ;
    float biggestNode ;

    int currentSize = arraySize ;

    for(j = arraySize/2 - 1; j >= 0; j--)
        MPEFieldGen_trickleDownForGeoIndex(heapArray, hrap_x, hrap_y, currentSize, j);
    
    for(j = arraySize - 1; j >= 0; j--)
    {
        --currentSize ;

        biggestIndex_x = hrap_x[0];
        hrap_x[0] = hrap_x[currentSize];

        biggestIndex_y = hrap_y[0];
        hrap_y[0] = hrap_y[currentSize];

        biggestNode = heapArray[0];
        heapArray[0] = heapArray[currentSize];

        MPEFieldGen_trickleDownForGeoIndex(heapArray, hrap_x, hrap_y, currentSize, 0);

        hrap_x[j] = biggestIndex_x ;
        hrap_y[j] = biggestIndex_y ;
        heapArray[j] = biggestNode ;
    }
}

void MPEFieldGen_trickleDownForDoubleAndGeoIndex(double heapArray[], int index_x[], 
                    int index_y[], int currentSize, int index)
{
    int largerChild;
    double top = heapArray[index];  // save root
    int topIndex_x = index_x[index] ;
    int topIndex_y = index_y[index] ;

    while(index < currentSize/2) // not on bottom row
    {
        int leftChild  = 2 * index + 1 ;
        int rightChild = leftChild + 1 ;

        // find larger child
        if(rightChild < currentSize &&
           heapArray[leftChild] < heapArray[rightChild])
            largerChild = rightChild ;
        else
            largerChild = leftChild ;

        // top >= largerChild ?
        if(top >= heapArray[largerChild])
            break ;

        heapArray[index] = heapArray[largerChild] ;
        index_x[index]   = index_x[largerChild] ;
        index_y[index]   = index_y[largerChild] ;
        index = largerChild ;    
    }

    heapArray[index] = top ;
    index_x[index]   = topIndex_x ;
    index_y[index]   = topIndex_y ;

}

void MPEFieldGen_heapSortForDoubleAndGeoIndex(double heapArray[], int index_x[],
                       int index_y[], int arraySize)
{
    int j, biggestIndex_x, biggestIndex_y ;
    double biggestNode ;

    int currentSize = arraySize ;

    for(j = arraySize/2 - 1; j >= 0; j--)
        MPEFieldGen_trickleDownForDoubleAndGeoIndex(heapArray, index_x,
             index_y, currentSize, j);
    
    for(j = arraySize - 1; j >= 0; j--)
    {
        --currentSize ;
        biggestNode  = heapArray[0];
        heapArray[0] = heapArray[currentSize];

        biggestIndex_x = index_x[0];
        index_x[0]     = index_x[currentSize];

        biggestIndex_y = index_y[0];
        index_y[0]     = index_y[currentSize];

        MPEFieldGen_trickleDownForDoubleAndGeoIndex(heapArray, index_x, 
                 index_y, currentSize, 0);

        heapArray[j] = biggestNode ;
        index_x[j]   = biggestIndex_x ;
        index_y[j]   = biggestIndex_y ;
    }
}

