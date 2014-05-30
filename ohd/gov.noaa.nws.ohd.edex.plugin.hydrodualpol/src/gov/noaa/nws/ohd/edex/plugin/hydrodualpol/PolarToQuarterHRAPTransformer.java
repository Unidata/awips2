package gov.noaa.nws.ohd.edex.plugin.hydrodualpol;

import com.raytheon.uf.common.status.IUFStatusHandler;

/**
 * Class to handle conversion from Polar coordinates to Quarter HRAP coordinates for  DSA, and DPR product processors for
 * HPE/HPN. 
 * <p>
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * August 2013 DCS 167    C. Gobs   Initial Creation
 * 
 * </pre>
 * 
 * 
 * @author Chip Gobs
 */


public class PolarToQuarterHRAPTransformer
{

	//from decode_constants.h
	
	private static final short NO_DATA_FLAG = 0;

	private static final int MAX_AZIMUTH = 360;
	private static final int MAX_RANGE_BINNUM = 920;  /* for dual pol DSA */

	public static final int MAX_IHRAP = 524;
	public static final int MAX_JHRAP  = 524;
	
	private static final int NUM_ROW = 524;
	private static final int NUM_COL  = 524;

	private static final int BEYOND_RANGE = -99;
	private static final int BEYOND_GRID  = -77;

	private static final double GRID_COORD_I = 433.0;
	private static final double GRID_COORD_J = 433.0;

	private static final double ANGLE_THRESH = 9.81E-6;

	private static final double R2KO  = 249.6348607;
	private static final double PRIME =  105.0;

//	private static final int IKA   =  40;
	//This section matches to an array-based section in PolarToHRAPTransformer
	//All of this code should be merged into a single routine with
	//varying use of constants.
	private static final double KA   = 40.0;
	private static final int OFFSET  = 263; 
	private static final double B_CON = 0.025; 

	private static final float DEFAULT_RADAR_1KM  =  -0.9f;
	//private static final double MAX_RATE           =  999.0; 
	//private static final double MIN_RATE           = -999.0;

	private static final double CONST  = 135.0;
	private static final double EARTH_RADIUS = 6380.0;

	//private static final double FLOAT_MISSING = -999.0;
	
	// end of code from decode_constants.h


	// start from build_lookup_table
	private static final double ONE = 1.0;
	private static final double HALF = 0.5;
	private static final double RNG_INC = 0.25;  /* for dual pol DSA/DPR product with 0.25 km resolution */

	private static final double DEGREE_TO_RADIAN = 0.01745329;
	private static final double EARTH_RADIUS_SQ = EARTH_RADIUS * EARTH_RADIUS;
	// end from build_lookup_table

	private int[][] radar_to_quarter_hrap_i = new int[MAX_RANGE_BINNUM][MAX_AZIMUTH];
	private int[][] radar_to_quarter_hrap_j = new int[MAX_RANGE_BINNUM][MAX_AZIMUTH];

	private int[][] quarter_hrap_to_radar_azimuth = new int[MAX_IHRAP][MAX_JHRAP];
	private int[][]  quarter_hrap_to_radar_range = new int[MAX_IHRAP][MAX_JHRAP];


	private IUFStatusHandler statusHandler = null;  //leave in to make future debugging easier


	public PolarToQuarterHRAPTransformer()
	{
		;
	}

	
	public PolarToQuarterHRAPTransformer(IUFStatusHandler statusHandler)
	{
		this.statusHandler = statusHandler;
	}

	void build_lookup_table(double radar_lat, double radar_lon)
	{

		double  pre_gxs, site_grid_i, site_grid_j;
		double  grid_i, grid_j, rl, cos_delta_lamda, sin_delta_lamda, sin_angle_s, cos_angle_s;
		double  sin_site_lat, cos_site_lat, sin_lat, cos_lat, sin_bearing, cos_bearing;
		double  bear_angle, range, cos_lambas_prime, sin_lamdas_prime;
		int     site_i, site_j, iaz, irg;

		int /*long*/    lfm_i, lfm_j;
		int     ihrap,  jhrap;

		/*
		 * re-initialize tables before starting lookup table generation
		 */

		for(iaz = 0; iaz < MAX_AZIMUTH; iaz++)
		{
			for (irg = 0; irg < MAX_RANGE_BINNUM; irg++)
			{
				radar_to_quarter_hrap_i[irg][iaz] = BEYOND_GRID;
				radar_to_quarter_hrap_j[irg][iaz] = BEYOND_GRID;
			}
		}   

		for(ihrap = 0; ihrap < MAX_IHRAP; ihrap++)
		{
			for (jhrap = 0; jhrap < MAX_JHRAP; jhrap++)
			{
				quarter_hrap_to_radar_azimuth[ihrap][jhrap] = BEYOND_GRID;
				quarter_hrap_to_radar_range[ihrap][jhrap] = BEYOND_GRID;
			}
		}   

		/*
		 * compute parts of equations used multiple times later
		 */

		cos_lambas_prime = Math.cos((radar_lon + PRIME) * DEGREE_TO_RADIAN);
		sin_lamdas_prime =  Math.sin((radar_lon + PRIME) * DEGREE_TO_RADIAN);
		sin_site_lat =  Math.sin(radar_lat * DEGREE_TO_RADIAN);
		cos_site_lat =  Math.cos(radar_lat * DEGREE_TO_RADIAN);

		/*
		 * compute common part of the gis and gjs equations
		 */

		pre_gxs = R2KO * cos_site_lat / (ONE + sin_site_lat);

		/*
		 * compute reference grid box coordinates
		 */

		site_grid_i = pre_gxs * sin_lamdas_prime + GRID_COORD_I;
		site_grid_j = pre_gxs * cos_lambas_prime + GRID_COORD_J;

		/*
		 * compute grid box numbers for box 0,0 of local grids, OFFSET is the 
		 * center of 524x524 HRAP grid which is the center of radar, this value changes
		 *  when the HRAP grid size changes.
		 */

		site_i = (int)(KA * site_grid_i) - OFFSET;
		site_j = (int)(KA * site_grid_j) - OFFSET;

	//	System.out.printf("KA=%f, OFFSET=%d, B_CON=%f\n", KA, OFFSET, B_CON);

		/*
		 * initialize bearing
		 */

		bear_angle = -HALF;

		/*
		 * DO FOR ALL BEARINGS
		 */

		for(iaz = 1; iaz <= MAX_AZIMUTH; iaz++) 
		{
			bear_angle += ONE;
			sin_bearing = Math.sin(bear_angle * DEGREE_TO_RADIAN);
			cos_bearing = Math.cos(bear_angle * DEGREE_TO_RADIAN);

			/*
			 * initialize range
			 */       

			/* half of the range increment value */
			range = -RNG_INC/2;

			/*
			 * do for each input data range values (hydro application next)
			 */

			for (irg = 1; irg <= MAX_RANGE_BINNUM; irg++) 
			{           	    
				range = range + RNG_INC;
				sin_angle_s = (range / EARTH_RADIUS) * 
						(ONE - (CONST * range / EARTH_RADIUS_SQ));
				cos_angle_s = Math.sqrt(ONE - sin_angle_s * sin_angle_s);

				sin_lat = sin_site_lat * cos_angle_s+cos_site_lat
						* sin_angle_s * cos_bearing;
				cos_lat = Math.sqrt(ONE - sin_lat * sin_lat);

				sin_delta_lamda = sin_angle_s * sin_bearing/cos_lat;
				cos_delta_lamda = Math.sqrt(ONE - sin_delta_lamda * sin_delta_lamda);

				rl = R2KO * cos_lat/(ONE + sin_lat);

				grid_i = rl * (sin_delta_lamda * cos_lambas_prime +
						cos_delta_lamda * sin_lamdas_prime)+GRID_COORD_I;
				grid_j = rl * (cos_delta_lamda * cos_lambas_prime -
						sin_delta_lamda * sin_lamdas_prime)+GRID_COORD_J;

				/*
				 * compute 1/160 lfm i and j coordinates of the range/azimuth bin
				 */

				lfm_i = (int)(grid_i * KA) - site_i;
				lfm_j = (int)(grid_j * KA) - site_j;

				/*
				 * if lfm coordinates are within local grid save it into the lookup
				 * table and set the 1/160 lfm box range number table to within range
				 */

				if ((lfm_i > 0) && (lfm_i <= MAX_IHRAP) &&
						(lfm_j > 0) && (lfm_j <= MAX_JHRAP) )
				{
					radar_to_quarter_hrap_i[irg-1][iaz-1] = lfm_i; 
					radar_to_quarter_hrap_j[irg-1][iaz-1] = lfm_j;

					quarter_hrap_to_radar_azimuth[lfm_i-1][lfm_j-1] = iaz;
					quarter_hrap_to_radar_range[lfm_i-1][lfm_j-1] = irg;

				}
			}
		}

		/*
		 * call find_holes() to find holes and determine hole filling data
		 */

		find_holes(radar_lat, radar_lon);

		//GridChecker checker = new GridChecker();
		//String outputDir = "/home/pst2/";
		
		//checker.writeIntegerGrid(radar_to_quarter_hrap_i, MAX_RANGE_BINNUM, MAX_AZIMUTH, outputDir + "radar_to_quarter_hrap_i.txt" );
		//checker.writeIntegerGrid(radar_to_quarter_hrap_j, MAX_RANGE_BINNUM, MAX_AZIMUTH, outputDir + "radar_to_quarter_hrap_j.txt" );
		
		
		//checker.writeIntegerGrid(quarter_hrap_to_radar_azimuth, MAX_IHRAP, MAX_JHRAP, outputDir + "quarter_hrap_to_radar_azimuth.txt" );
		//checker.writeIntegerGrid(quarter_hrap_to_radar_range, MAX_IHRAP, MAX_JHRAP, outputDir + "quarter_hrap_to_radar_range.txt" );


	} //end build_lookup_table()


	void find_holes(double LS, double LAMDA)
	{
		double RG_IJ, AZ_IJ, AZ_RND, RG_RND, MAX_RANGE_LENGTH;
		int    lfm_i, lfm_j, ONE, count;
		int    ihrap, jhrap;

		/* initialize the quarterhrap to azimuth value */
		AZ_IJ = 0.0;
		/* increment on azimuth */
		AZ_RND = 1.05;
		/* initialize the quarterhrap to range value */
		RG_IJ = 0.0;
		/* increment on range */
		RG_RND = 0.25;

		ONE = 1;

		/* maximum range length for DSA/DPR products which is 230.0km */
		MAX_RANGE_LENGTH = 230.0;

		count = 0;

		/*
		 * search for holes in the 1/160 lfm lookup table and identify the
		 * azimuth/range of the data to be used to fill in the hole
		 */

		for(ihrap = 0; ihrap < MAX_IHRAP; ihrap ++)
		{
			for (jhrap = 0; jhrap < MAX_JHRAP; jhrap ++)
			{
				/* with BEYOND_GRID value means this bin is not filled yet */
				if( (quarter_hrap_to_radar_azimuth[ihrap][jhrap] == BEYOND_GRID) &&
						(quarter_hrap_to_radar_range[ihrap][jhrap] == BEYOND_GRID) )
				{
					lfm_i = ihrap + 1;
					lfm_j = jhrap + 1;

					/* calculate the azimuth AZ_IJ and range RG_IJ for the hrap point (lfm_i, lfm_j) */
					AzimuthAndRangePoint point =  quarterhrap_to_az_range(LS, LAMDA, lfm_i, lfm_j);

					AZ_IJ = point.getAzimuth();
					RG_IJ = point.getRange();

					/*
					 *  if the range is within the product coverage area (for DSA product the maximum range length is 230 km) 
					 *  requirement then
					 *  store azimuth and range index into the lookup table
					 */ 

					if (RG_IJ < MAX_RANGE_LENGTH)
					{
						count ++;

						/* fill the hole's azimuth with (AZ_IJ+AZ_RND), range with (RG_IJ/RG_RND+ONE) */ 
						quarter_hrap_to_radar_azimuth[ihrap][jhrap]
								= (int)(AZ_IJ + AZ_RND);
						quarter_hrap_to_radar_range[ihrap][jhrap]
								= (int)(RG_IJ/RG_RND) + ONE;			
					}
					else  /* assign BEYOND_RANGE when RG_IJ is out of the maximum range length */
					{
						quarter_hrap_to_radar_azimuth[ihrap][jhrap]
								= BEYOND_RANGE;
						quarter_hrap_to_radar_range[ihrap][jhrap]
								= BEYOND_RANGE;            
					}
				}
			}
		}

	//	System.out.printf("In find holes: count is %d\n", count);

	} //end find_holes()


	AzimuthAndRangePoint quarterhrap_to_az_range(double LS, double LAMDAS, 
			int LFM_I, int LFM_J /*, double *THETA_CIJ, double *RG_IJ */  )
	{

		AzimuthAndRangePoint point = new AzimuthAndRangePoint();
		double THETA_CIJ;
		double RG_IJ;

		double PRE_GXS, GIS, GJS, DTR, ONE;
		double COS_LAMDAS_PRIME, SIN_LAMDAS_PRIME, SIN_DLAMDA;
		double HALF, AI, AJ;
		double CII, CJJ, SIN_LS, COS_LS;
		double AA, BB, COS_SS, SIN_SS, COS_LIJ, SIN_LIJ, R_360;
		double LAMDA_IJ, L_IJ, NINTY, TWO, ZERO;
		int   IS, JS;

		R_360 = 360.0;
		NINTY = 90.0;
		ZERO = 0.0;
		TWO = 2.0;
		HALF = 0.5;
		ONE = 1.0;
		DTR = DEGREE_TO_RADIAN;
		
		COS_LS = 0.0;
		SIN_LS = 0.0;


		COS_LAMDAS_PRIME = Math.cos((LAMDAS + PRIME) * DTR);
		SIN_LAMDAS_PRIME = Math.sin((LAMDAS + PRIME) * DTR);
		SIN_LS =  Math.sin(LS * DTR);
		COS_LS =  Math.cos(LS * DTR);

		/*
		 * COMPUTE COMMON PART OF THE GIS AND GJS EQUATIONS
		 */

		PRE_GXS = R2KO * COS_LS/(ONE + SIN_LS);

		/*
		 * COMPUTE REGERENCE GRID BOX COORDINATES
		 */

		GIS = PRE_GXS * SIN_LAMDAS_PRIME + GRID_COORD_I;
		GJS = PRE_GXS * COS_LAMDAS_PRIME + GRID_COORD_J;

		/*
		 * COMPUTE GRID BOX NUMBERS FOR BOX 0,0 OF LOCAL GRIDS
		 */

		IS = (int)(KA * GIS) - OFFSET;
		JS = (int)(KA * GJS) - OFFSET;

		/*
		 * COMPUTE AI AND AJ CONSTANTS
		 */

		AI = (IS - GRID_COORD_I * KA + HALF)/KA;
		AJ = (JS - GRID_COORD_J * KA + HALF)/KA;

		CII = AI + B_CON * LFM_I;
		CJJ = AJ + B_CON * LFM_J;
		L_IJ = DTR * NINTY - TWO *  Math.atan( Math.sqrt(CII * CII + CJJ * CJJ) / R2KO);

		/*
		 * IF BOTH INPUTS TO DATAN2 ARE 0, DONT CALL FUNCTION
		 */

		if ( (CII == ZERO) && (CJJ == ZERO) ) 
		{
			LAMDA_IJ = ZERO;
		} 

		/*
		 * OTHERWISE COMPUTE LAMDA_IJ
		 */

		else
		{
			LAMDA_IJ = -PRIME * DTR +  Math.atan2(CII, CJJ);
		} 

		/*
		 * COMPUTE INTERMEDIATE VALUES
		 */

		COS_LIJ =  Math.cos(L_IJ);
		SIN_LIJ =  Math.sin(L_IJ);
		SIN_DLAMDA =  Math.sin(LAMDA_IJ - DTR * LAMDAS);
		AA = COS_LIJ * SIN_DLAMDA;
		BB = COS_LS * SIN_LIJ - SIN_LS * COS_LIJ *  Math.cos(LAMDA_IJ - DTR * LAMDAS);
		SIN_SS =  Math.sqrt(AA * AA + BB * BB);
		COS_SS =  Math.sqrt(ONE - SIN_SS * SIN_SS);

		/*
		 * COMPUTE RANGE
		 */

		RG_IJ = (CONST * SIN_SS + EARTH_RADIUS) * SIN_SS;

		/*
		 * IF SIN_SS IS GREATER THAN A SMALL POSITIVE NUMBER
		 * COMPUTE THETA_CIJ
		 */

		if (SIN_SS >= ANGLE_THRESH) 
		{

			THETA_CIJ =  Math.atan2(COS_LIJ * COS_LS * SIN_DLAMDA,
					(SIN_LIJ - SIN_LS * COS_SS));
		} 

		/*
		 * OTHERWISE, SET THETA_CIJ TO 0
		 */

		else
		{
			THETA_CIJ = ZERO;
		} 

		THETA_CIJ = THETA_CIJ / DTR;

		/*
		 * IF ANGLE IS LESS THAN 0 ... ADD 360 DEGREES
		 */

		if (THETA_CIJ < ZERO) 
		{
			THETA_CIJ = THETA_CIJ + R_360;
		}

		point.setAzimuth(THETA_CIJ);
		point.setRange(RG_IJ);

		return point;

	}  //end quarterhrap_to_az_range()


	float [][]  transform250MeterPolarToQuarterHRAP(float [][] polarGrid, 
													double radar_lat, 
													double radar_lon)
	{

		int[][]   num_bin_1km = new int[MAX_IHRAP][MAX_JHRAP];
		float[][] radar_1km = new float[MAX_IHRAP][MAX_JHRAP];
		float[][] tmp_radar_1km = new float[MAX_IHRAP][MAX_JHRAP];
		float[][] sum_rad_1km = new float[MAX_IHRAP][MAX_JHRAP];
		float[][] precip = new float[MAX_AZIMUTH][MAX_RANGE_BINNUM];
	//	int i, j;
		int range, azimuth;

		/* initialize precipitation array in float for polar coordinate */
		for(int i = 0; i < MAX_AZIMUTH; i++)
		{
			for(int j = 0; j < MAX_RANGE_BINNUM; j++)
			{
				precip[i][j] = 0.0f ;
			}        
		}


		/* initialize quarter HRAP array precipitation data */   
		for(int i = 0; i < MAX_IHRAP; i++)
		{
			for(int j = 0; j < MAX_JHRAP; j++)
			{
				radar_1km[i][j]   = DEFAULT_RADAR_1KM ;  /* default to -0.9*/
				num_bin_1km[i][j] = 0 ;
				sum_rad_1km[i][j] = 0.0f ;
			}
		}


		build_lookup_table(radar_lat, radar_lon) ;    

		//This comment came from DSA C-code, to be removed after we finish DSA 
		/* convert DSA product storm-total precipitation into appropriate scale (0.01 inch) in mm unit, do not
use the offset/scale format when the initial raw data is 0 which represents NO Data */

		for(int i = 0; i < MAX_AZIMUTH; i++)
		{
			for(int j = 0; j < MAX_RANGE_BINNUM; j++)
			{
				if( polarGrid[i][j] != NO_DATA_FLAG)
				{
					precip[i][j] = polarGrid[i][j];
				}
			}
		}
		
		int index ;
		/* convert between polar coordinate and quarter HRAP */


		
		for (int i = 0; i < MAX_IHRAP; i ++ )
		{
			for (int j = 0; j < MAX_JHRAP; j ++ )
			{
				index = MAX_JHRAP + 1 - (j + 1)  - 1;

				azimuth = quarter_hrap_to_radar_azimuth[i][j] ;

				if(azimuth > MAX_AZIMUTH)
				{
					azimuth = MAX_AZIMUTH;
				}

				range = quarter_hrap_to_radar_range[i][j] ;	    	   

				if ( (range != BEYOND_RANGE) && (azimuth != BEYOND_RANGE) )
				{
					if(azimuth  < 1 || azimuth > MAX_AZIMUTH)
					{
						System.out.printf("Azimuth angle (%d, %d) out of range at [%d][%d]\n",
								azimuth, range, j, i);
					}

					if(range < 1 || range > MAX_RANGE_BINNUM)
					{
						System.out.printf("Range (%d, %d) out of range at [%d][%d]\n",
								azimuth, range, j, i);
					}

					num_bin_1km[i][index] ++ ;

					sum_rad_1km[i][index] += precip[azimuth - 1][range - 1];
				}
			} //end for ( j = 0; j < MAX_JHRAP; j ++ )
		} // end 	for ( i = 0; i < MAX_IHRAP; i ++ )

		
		/* calculate the average value for quarter HRAP grids */
		for (int i = 0; i < MAX_IHRAP; i ++ )
		{
			for (int j = 0; j < MAX_JHRAP; j ++ )
			{
				if ( num_bin_1km[i][j] > 0)
				{
					radar_1km[i][j] 
					             = sum_rad_1km[i][j] / num_bin_1km[i][j];
					
				//	System.out.println("radar_1km[" + i +"][" + j +"] = " + radar_1km[i][j]);
					
				//	statusHandler.handle(Priority.INFO, "\n" +
				//			"DPR product: sum_rad_1km[" + i +"][" + j +"] = " + sum_rad_1km[i][j]);
					
				//	System.out.printf("count = %d sum_rad_1km[%d][%d] = %f\n", num_bin_1km[i][j], i, j, sum_rad_1km[i][j]);
							
				}
			}
		}

		
		for(int i = 0; i < NUM_ROW; i++)
		{
			for(int j = 0; j < NUM_COL; j++)
			{
				tmp_radar_1km[i][j] = radar_1km[j][i];
			}
		}
	
		return tmp_radar_1km;
		//return radar_1km;

	} //end transform250MeterPolarToQuarterHRAP()


} //end class PolarToQuarterHRAPTransformer()
