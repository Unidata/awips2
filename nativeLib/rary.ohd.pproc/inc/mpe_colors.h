#ifndef MPE_COLORS_H
#define MPE_COLORS_H

#include "mpe_field_names.h"

#define num_height_colors  18
#define num_height_levels  16
#define num_index_colors 18
#define num_index_levels  16
#define num_locbias_colors  14
#define num_locbias_levels  12
#define num_locspan_colors  13
#define num_locspan_levels  11
#define num_precip_colors  17
#define num_precip_levels  15
#define num_prism_colors  17
#define num_prism_levels  15
#define num_max_temp_prism_colors  14
#define num_max_temp_prism_levels  12
#define num_min_temp_prism_colors  14
#define num_min_temp_prism_levels  12
#define num_radclim_colors  3
#define num_radclim_levels  2

#define num_6hr_gridded_precip_colors 16
#define num_24hr_gridded_precip_colors 16
#define num_6hr_mean_precip_colors 16
#define num_24hr_mean_precip_colors 16

#define num_6hr_gridded_temperature_colors 16
#define num_max_gridded_temperature_colors 16
#define num_min_gridded_temperature_colors 16
#define num_6hr_mean_temperature_colors 16

#define num_6hr_gridded_freezing_colors 16
#define num_6hr_mean_freezing_colors 16

#define num_precip_diff_colors  19
#define num_precip_diff_levels  17

#define num_precip_ratio_colors  17
#define num_precip_ratio_levels  15

void writeDefaultMPEColorDataFile();

static const char * height_colors [ num_height_colors ] =
              { "GRAY30" , "BLACK" , "ORANGE" , "YELLOW" , "GREENYELLOW" ,
                "YELLOWGREEN" , "GREEN" , "TURQUOISE4" , "TURQUOISE3" ,
                "TURQUOISE2" , "DEEPSKYBLUE1" , "DEEPSKYBLUE2" ,
                "DODGERBLUE1" , "BLUE" , "PURPLE2" , "PURPLE3" ,
                "PURPLE4" , "WHITE" } ;
static const double height_levels [ num_height_colors ] =
              { -9999.0 , -8888.0 , 0.0 , 250. , 500. , 750. , 1000. , 1500. ,
                2000. , 2500. , 3000. , 3500. , 4000. , 5000. , 6000. ,
                7000. , 8000. , 10000. } ;

static const char * index_colors [ num_index_colors ] =
              { "GRAY30" , "BLACK" , "YELLOW" , "GREENYELLOW" ,
                "YELLOWGREEN" , "GREEN" , "TURQUOISE4" , "TURQUOISE3" ,
                "TURQUOISE2" , "DEEPSKYBLUE1" , "DEEPSKYBLUE2" ,
                "DODGERBLUE1" , "BLUE" , "PURPLE2" , "PURPLE3" ,
                "PURPLE4" , "WHITE" , "ORANGE" } ;
static const double index_levels [ num_index_colors ] =
              { -9999.0 , -8888.0 , 1.0 , 2.0 , 3.0 , 4.0 , 5.0 , 6.0 ,
                7.0 , 8.0 , 9.0 , 10.0 , 11.0 , 12.0 , 13.0 , 14.0 , 15.0 ,
                16.0 } ;

static const char * locbias_colors [ num_locbias_colors ] =
              { "GRAY30" , "BLACK" , "RED" , "DODGERBLUE1" ,
                "CYAN" , "DARKGREEN" , "GREEN" , "GREENYELLOW" ,
                "YELLOW" , "GOLD2" , "DARKORANGE1" , "RED" ,
                "RED3" , "RED4" } ;
static const double locbias_levels [ num_locbias_colors ] =
              { -9999.0 , -8888.0 , 0.0 , 0.4 , 0.6 , 0.8 , 1.0 , 1.2 ,
                1.4 , 1.6 , 1.8 , 2.0 , 2.5 , 3.0 } ;

static const char * locspan_colors [ num_locspan_colors ] =
              { "GRAY30" , "BLACK" , "RED" , "DODGERBLUE1" ,
                "CYAN" , "DARKGREEN" , "GREEN" , "GREENYELLOW" ,
                "YELLOW" , "GOLD2" , "DARKORANGE1" , "RED" ,
                "WHITE" } ;
static const double locspan_levels [ num_locspan_colors ] =
              { -9999.0 , -8888.0 , 0.0 , 1.0 , 2.0 , 3.0 , 4.0 , 5.0 ,
                6.0 , 7.0 , 8.0 , 9.0 , 10.0 } ;

static const char * precip_colors [ num_precip_colors ] =
              { "GRAY30" , "BLACK" , "BLACK" , "DODGERBLUE1" , "CYAN" ,
                "DARKGREEN" , "GREEN" , "GREENYELLOW" , "YELLOW" ,
                "GOLD2" , "DARKORANGE1" , "RED" , "RED3" , "RED4" ,
                "MAGENTA1" , "DARKORCHID" , "WHITE" } ;

static const double precip_levels [ num_precip_colors ] =
              { -9999.0 , -8888.0 , 0.00 , 0.01 , 0.10 , 0.20 , 0.30 ,
                0.40 , 0.50 , 0.75 , 1.00 , 1.25 , 1.50 , 1.75 , 2.00 ,
                2.50 , 3.00 } ;

static const char * prism_colors [ num_prism_colors ] =
              { "GRAY30" , "BLACK" , "BLACK" , "DODGERBLUE1" ,
                "CYAN" , "DARKGREEN" , "GREEN" , "GREENYELLOW" ,
                "YELLOW" , "GOLD2" , "DARKORANGE1" , "RED" ,
                "RED3" , "RED4" , "MAGENTA1" , "DARKORCHID" ,
                "WHITE" } ;
static const double prism_levels [ num_prism_colors ] =
              { -9999.0 , -8888.0 , 0.0 , 0.1 , 0.2, 0.3, 0.4,
                0.5, 0.75, 1.0, 1.25, 1.50, 2.0, 2.5, 3.0,
                4.0, 5.0} ;
static const char * max_temp_prism_colors [ num_max_temp_prism_colors ] =
              { "GRAY30" , "BLACK" , "PURPLE" , "BLUE" ,
                "CYAN" , "DARKGREEN" , "GREEN" , "GREENYELLOW" ,
                "YELLOW" , "GOLD2" , "DARKORANGE1" , "RED" ,
                "RED3" , "RED4" };
static const double max_temp_prism_levels [ num_max_temp_prism_colors ] =
              { -9999.0 , -8888.0 , 0.0, 10.0 , 20.0 , 30.0 , 40.0 ,
                50.0 , 60.0 , 65.0 , 70.0 , 75.0 , 80.0 , 90.0 };
static const char * min_temp_prism_colors [ num_min_temp_prism_colors ] =
              { "GRAY30" , "BLACK" , "PURPLE" , "BLUE" ,
                "CYAN" , "DARKGREEN" , "GREEN" , "GREENYELLOW" ,
                "YELLOW" , "GOLD2" , "DARKORANGE1" , "RED" ,
                "RED3" , "RED4" };
static const double min_temp_prism_levels [ num_min_temp_prism_colors ] =
              { -9999.0 , -8888.0 , 0.0, 10.0 , 20.0 , 30.0 , 40.0 ,
                50.0 , 60.0 , 65.0 , 70.0 , 75.0 , 80.0 , 90.0 };

static const char * radclim_colors [ num_radclim_colors ] =
              { "GRAY30" , "BLACK" , "RED" } ;

static const double radclim_levels [ num_radclim_colors ] = { -9999.0 ,
                                                             0.0 , 1.0 } ;

/* Define the color sets for the DailyQC data. */

/* 6h gridded precip */

static const char * gridded_precip_colors_6hr [ num_6hr_gridded_precip_colors ] = 
              { "GRAY69", "GRAY43", "DARKKHAKI", "LIGHTSEAGREEN", "GREEN1",
                "GREEN4", "MAGENTA1", "MAGENTA3", "BLUE1", "BLUE3", "GOLD3",
                "YELLOW1", "ORANGE1", "RED1", "RED3", "GRAY100" };

static const double gridded_precip_levels_6hr [ num_6hr_gridded_precip_colors ] =
              { 0.01, 0.1, 0.2, 0.3, 0.5, 0.7, 1.0, 
	        1.3, 1.6, 2.0, 2.3, 2.5, 2.8, 3.0, 3.5, 4.0};
		
/*{ 0.01, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0, 
	        1.1, 1.2, 1.3, 1.4, 1.5 }; */		

/* 24h gridded precip */

static const char * gridded_precip_colors_24hr [ num_24hr_gridded_precip_colors ] = 
              { "GRAY69", "GRAY43", "DARKKHAKI", "LIGHTSEAGREEN", "GREEN1",
                "GREEN4", "MAGENTA1", "MAGENTA3", "BLUE1", "BLUE3", "GOLD3",
                "YELLOW1", "ORANGE1", "RED1", "RED3", "GRAY100" };

static const double gridded_precip_levels_24hr [ num_24hr_gridded_precip_colors ] =
              { 0.01, 0.1, 0.2, 0.3, 0.5, 1.0, 1.5, 2.0, 2.5, 
	        3.0, 3.5, 4.0, 4.5, 5.0, 5.5, 6.0};		
		
   /*  { 0.01, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0, 
	        1.1, 1.2, 1.3, 1.4, 1.5 }; */
			
/* 6hr mean precip */

static const char * mean_precip_colors_6hr [ num_6hr_mean_precip_colors ] = 
              { "GRAY69", "GRAY43", "DARKKHAKI", "LIGHTSEAGREEN", "GREEN1",
                "GREEN4", "MAGENTA1", "MAGENTA3", "BLUE1", "BLUE3", "GOLD3",
                "YELLOW1", "ORANGE1", "RED1", "RED3", "GRAY100" };

static const double mean_precip_levels_6hr [ num_6hr_mean_precip_colors ] =
               { 0.01, 0.1, 0.2, 0.3, 0.5, 0.7, 1.0, 
	        1.3, 1.6, 2.0, 2.3, 2.5, 2.8, 3.0, 3.5, 4.0};
		
/* 24h mean precip */

static const char * mean_precip_colors_24hr [ num_24hr_mean_precip_colors ] = 
              { "GRAY69", "GRAY43", "DARKKHAKI", "LIGHTSEAGREEN", "GREEN1",
                "GREEN4", "MAGENTA1", "MAGENTA3", "BLUE1", "BLUE3", "GOLD3",
                "YELLOW1", "ORANGE1", "RED1", "RED3", "GRAY100" };

static const double mean_precip_levels_24hr [ num_24hr_mean_precip_colors ] =
               { 0.01, 0.1, 0.2, 0.3, 0.5, 1.0, 1.5, 2.0, 2.5, 
	        3.0, 3.5, 4.0, 4.5, 5.0, 5.5, 6.0};				
		

/* 6hr gridded freezing level */

static const char * gridded_freezing_colors_6hr [ num_6hr_gridded_freezing_colors ] = 
              { "GRAY0", "GRAY43", "DARKKHAKI", "LIGHTSEAGREEN", "GREEN1", 
                "GREEN4", "MAGENTA1", "MAGENTA3", "BLUE1", "BLUE3", "GOLD3", 
                "YELLOW1", "ORANGE1", "RED1", "RED3", "GRAY100" };
		
static const double gridded_freezing_levels_6hr [ num_6hr_gridded_freezing_colors ] =
              { 0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0,
	         9.0, 10.0, 11.0, 12.0, 13.0, 14.0, 15.0 };

/* 6hr mean freezing level */

static const char * mean_freezing_colors_6hr [ num_6hr_mean_freezing_colors ] = 
              { "GRAY0", "GRAY43", "DARKKHAKI", "LIGHTSEAGREEN", "GREEN1", 
                "GREEN4", "MAGENTA1", "MAGENTA3", "BLUE1", "BLUE3", "GOLD3", 
                "YELLOW1", "ORANGE1", "RED1", "RED3", "GRAY100" };
		
static const double mean_freezing_levels_6hr [ num_6hr_mean_freezing_colors ] =
              { 0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0,
	         9.0, 10.0, 11.0, 12.0, 13.0, 14.0, 15.0 };

/* 6hr gridded temperature */

static const char * gridded_temperature_colors_6hr [ num_6hr_gridded_temperature_colors ] =    
              { "GRAY74", "GRAY43", "DARKKHAKI", "LIGHTSEAGREEN", "GREEN1",
                "GREEN4", "MAGENTA1", "MAGENTA3", "BLUE1", "BLUE3", "GOLD3",
                "YELLOW1", "ORANGE1", "RED1", "RED3", "GRAY100" }; 
	     
		
	      /*{ "GRAY74", "GRAY43", "MAGENTA4", "MAGENTA3", "MAGENTA2",
	        "BLUEVIOLET", "DARKVIOLET", "BLUE4", "BLUE3", "BLUE2", "CYAN3", 
		"LIGHTSKYBLUE",   "GREEN4", "LIMEGREEN", "GREEN2", "GREENYELLOW",
		"YELLOW", "YELLOW2", "GOLD", "GOLD2", "ORANGE", "ORANGE2",
		"ORANGE3", "RED3", "RED2", "BROWN1", "PINK2", "LIGHTPINK", "WHITE" };*/

              /*{ "GRAY74", "GRAY43", "DARKKHAKI", "LIGHTSEAGREEN", "GREEN1",
                "GREEN4", "MAGENTA1", "MAGENTA3", "BLUE1", "BLUE3", "GOLD3",
                "YELLOW1", "ORANGE1", "RED1", "RED3", "GRAY100" };*/
			   			     
		
static const double gridded_temperature_levels_6hr [ num_6hr_gridded_temperature_colors ] =  
                       { -30.0, -20.0, -10.0, 0.0, 10.0, 
 	                20.0, 30.0, 40.0, 50.0,  
			60.0, 70.0, 80.0, 90.0, 
			100.0, 110.0, 120.0 };   
			      
             /* { -15.0, -10.0, -5.0, 0.0, 5.0, 10.0, 15.0, 
	        20.0, 25.0, 30.0, 35.0, 40.0, 45.0, 50.0, 55.0, 60.0 };		 */
            
	    /* { -20.0, -15.0, -10.0, -5.0, 0.0, 5.0, 10.0, 
 	                15.0, 20.0, 25.0, 30.0, 35.0, 40.0, 45.0, 50.0, 55.0, 
			60.0, 65.0, 70.0, 75.0, 80.0, 85.0, 90.0, 95.0, 
			100.0, 105.0, 110.0, 115.0, 120.0 };			*/
               
	     
			
/* 24h max, min temperature*/
           
static const char * gridded_temperature_colors_max [ num_max_gridded_temperature_colors ] =
	     { "GRAY74", "GRAY43", "DARKKHAKI", "LIGHTSEAGREEN", "GREEN1",
                "GREEN4", "MAGENTA1", "MAGENTA3", "BLUE1", "BLUE3", "GOLD3",
                "YELLOW1", "ORANGE1", "RED1", "RED3", "GRAY100" };

static const char * gridded_temperature_colors_min [ num_min_gridded_temperature_colors ] =
	     { "GRAY74", "GRAY43", "DARKKHAKI", "LIGHTSEAGREEN", "GREEN1",
                "GREEN4", "MAGENTA1", "MAGENTA3", "BLUE1", "BLUE3", "GOLD3",
                "YELLOW1", "ORANGE1", "RED1", "RED3", "GRAY100" };
		
static const double gridded_temperature_levels_max [ num_max_gridded_temperature_colors ] =  
                      { -30.0, -20.0, -10.0, 0.0, 10.0, 
 	                20.0, 30.0, 40.0, 50.0, 60.0, 70.0,
			80.0, 90.0, 100.0, 110.0, 120.0 };	
static const double gridded_temperature_levels_min [ num_min_gridded_temperature_colors ] =  
                      { -30.0, -20.0, -10.0, 0.0, 10.0, 
 	                20.0, 30.0, 40.0, 50.0, 60.0, 70.0,
			80.0, 90.0, 100.0, 110.0, 120.0 };	

/* 6hr mean temperature */
	   
static const char * mean_temperature_colors_6hr [ num_6hr_mean_temperature_colors ] =                             
	     { "GRAY74", "GRAY43", "DARKKHAKI", "LIGHTSEAGREEN", "GREEN1",
                "GREEN4", "MAGENTA1", "MAGENTA3", "BLUE1", "BLUE3", "GOLD3",
                "YELLOW1", "ORANGE1", "RED1", "RED3", "GRAY100" };
		
	    
		
static const double mean_temperature_levels_6hr [ num_6hr_mean_temperature_colors ] =  
                      { -30.0, -20.0, -10.0, 0.0, 10.0, 
 	                20.0, 30.0, 40.0, 50.0, 60.0, 70.0,
			80.0, 90.0, 100.0, 110.0, 120.0 };	
			           
/*
static const char * precip_diff_colors [ num_precip_diff_colors ] =
              { "GRAY30", "GRAY30" ,
                 "DODGERBLUE1" , "CYAN" ,  "DARKGREEN" ,
                 "GREEN" , "GREENYELLOW" , "YELLOW" ,

                 "DARKORANGE1" ,"BLACK" ,  "BLACK",

                 "RED" , "RED2" , "RED3" ,
                 "MAGENTA1" , "MAGENTA2", "DARKORCHID"
              } ;
*/

static const char * precip_diff_colors [ num_precip_diff_colors ] =
              { "GRAY30", "GRAY30" ,

                 "MAGENTA1" ,"MEDIUMORCHID",  "DARKORCHID",
                 "MEDIUMBLUE" ,"BLUE", "DODGERBLUE", "DARKTURQUOISE" ,

                 "CYAN",

                 "BLACK" , "BLACK",
                 "GREEN" , "GREENYELLOW" , "YELLOW" ,

                 "DARKORANGE1" ,

                 "ORANGERED" , "RED2" , "RED3"

              } ;

static const double precip_diff_levels [ num_precip_diff_colors ] =
              { -9999, -8888,
                 -0.75 , -0.5, -.25 ,
              -0.1, -0.08 , -0.06 ,-0.03,

                -0.01, //CYAN

                0.0, 0.01,

                0.03, 0.06 , 0.08 ,

                0.1,
                0.25 , 0.50 , .75
                } ;


static const char * precip_ratio_colors [ num_precip_ratio_colors ] =
              {  "GRAY30", "GRAY30" ,

                 "MAGENTA1", "MEDIUMORCHID",  "DARKORCHID",
                 "MEDIUMBLUE" ,"DODGERBLUE", "DARKTURQUOISE" ,"CYAN",

                 "BLACK",
                 "GREEN", "GREENYELLOW" , "YELLOW" ,

                 "DARKORANGE1" ,

                 "ORANGERED" , "RED2" , "RED3"

              } ;


static const double precip_ratio_levels [ num_precip_ratio_colors ] =
                {
                -9999, -8888,

                0.1, 0.2 ,
                0.3, 0.5 , 0.6 , 0.75,

                0.95, 1.05,

                1.5, 2.0 , 4.0 ,

                6.0,
                8.0 , 16.0 , 32.0
                } ;


#endif /* #ifndef MPE_COLORS_H */
