/*
        File:	GetCoords.c
        Date:	9/8/95
        Author: Chip Gobs

        Purpose:  Provide ability to map from data coordinates to window
        	  coordinates. Note:  min_win_coord corresponds to the
        	  window coordinate that maps to min_data_coord.  min_win_coord
        	  may be LARGER than max_win_coord (and probably should be)
        	  if the coordinate being calculated is the Y coordinate.
        	  
        	  forward and back examples:
        	  
        	  y = GetWinCoord(cur_stage, minstage, maxstage,
			      	  height-BOTTOM_Y_OFFSET, TOP_Y_OFFSET);
				  
		  cur_stage = GetDataCoord(y, minstage, max_stage,
		  		  height-BOTTOM_Y_OFFSET, TOP_Y_OFFSET);
				  
			
		  x = GetWinCoord(year, min_year, max_year,
		  			0, width);
					
		  year = (floor (0.5 + GetDataCoord( x, min_year, max_year,
		  					0, width));
							
							
		  The floor function with the 0.5 rounds up.  This is
		  needed because year is a variable of an integral type.
        	  

*/


#include <math.h>
#include <stdlib.h>
#include "Xtools.h"


Position GetWinCoord
		(
		double		data_value,
		double		min_data_coord,
		double		max_data_coord,
		Position	min_win_coord,
		Position	max_win_coord
		)
{
 	Position 	pos,
	   	 	useable_win_length,
			length_drawn;
	double		extreme_data_diff,
	   		cur_data_diff;	
	int		flip_orientation = 0;
	
	
	/*
		Determine the orientation.
	*/
	if (max_win_coord < min_win_coord)
		flip_orientation = 1;
	
	
	/*
		Compute 
	*/
	useable_win_length = abs(max_win_coord - min_win_coord);	
	extreme_data_diff  = max_data_coord - min_data_coord;
	cur_data_diff 	   = data_value - min_data_coord;

		
	/*
		avoid possible divide by 0
	*/
	if (extreme_data_diff == 0)
	{
		return 0;
	}
	
	
	/*
		normal rounding
	*/
	length_drawn =  floor ( 0.5 +
			        ((cur_data_diff / extreme_data_diff) *
				 (useable_win_length)));
	
	
	
	/*
		When doing y coordinates, it will be necessary to flip
		orientation.
	*/
	if (flip_orientation)
	   	pos = min_win_coord - length_drawn;	
	else
	   	pos = min_win_coord + length_drawn;
	
	
	/*
		Return the position.
	*/
	return(pos);	
}  


double GetDataCoord(Position win_value,
		   double min_data_coord,
		   double max_data_coord,
		   Position min_win_coord,
		   Position max_win_coord)
{

 	Position 	useable_win_length,
			length_drawn;
			
		
	double	data_pos,
		max_data_length,
		fraction_drawn;
	
	int	flip_orientation = 0;
	
	if (max_win_coord < min_win_coord)
		flip_orientation = 1;
	
		
	useable_win_length = abs(max_win_coord - min_win_coord);


	/*
		Avoid possible divide by zero
	*/	
	if (useable_win_length == 0)
	{
		return 0;	
	}
	
	max_data_length = max_data_coord - min_data_coord;
	
	
	/*
		When doing y coordinates, it will be necessary to flip
		orientation.
	*/
	if (flip_orientation)
	{   
	 	length_drawn = abs(win_value - max_win_coord);
		
		fraction_drawn = (double) length_drawn /
			     	 (double) useable_win_length;
		
		fraction_drawn = 1 - fraction_drawn;
	}
	
	else
	{   
		length_drawn = abs(win_value - min_win_coord);
		fraction_drawn = (double) length_drawn /
			     (double) useable_win_length;
	}

	
	
	data_pos = (fraction_drawn * max_data_length) + min_data_coord;	
	
	return data_pos;
}  
