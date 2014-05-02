package gov.noaa.nws.ohd.edex.plugin.hydrodualpol;


/**
 * Class to handle conversion from Polar coordinates to HRAP coordinates for DAA, DSA, and DPR product processors for
 * MPE and HPE/HPN. 
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

public class PolarToHRAPTransformer
{
   // double[] LFM_GRID40 = new double[17161];
    int[][] lfm40flag = new int[2][17161];
    int[][] lfm40grid = new int[360][920];
  
    //from TransformPolarToCartesian.h
    
    static final int BEYOND_GRID = -77;
    static final int FLAG_AZ = 0;
    static final int FLAG_RNG = 1;
    static final int BEYOND_RANGE = -99;
    static final int NO_DATA_FLAG = 0;
    static final int WITHIN_RANGE = 0;
    static final int MAX_AZMTHS = 360;
    static final int KBINS = 920; /* # of bins in the 232KM polar grid -- original value was 115 */
    static final int HYZ_LFM40 = 131;

    
    //from build_lfm _lookup C routines ( .h and .c files)
    static final double DTR = 0.01745329; 
    static final double HALF = 0.5;
    static final double RNG_INC = 0.25;   /* previous value was 2.0 */
    
    static final int FLAG_SIZE = 2;

    static final int NUM_LFM40 = (HYZ_LFM40 * HYZ_LFM40);

    static final int MAX_AZMTH = 360;

    static final int ONE = 1;

    static final double r2ko = 249.6348607;
    static final double ip = 433.0;
    static final double jp = 433.0;

    static final double LFM40_RNG = 230.0;

    static final double AZ_RND = 1.05;       // Factor used to convert azimuth angle to azimuth
                                            // index of bin closet to grid hole.          
    static final double RG_RND = 0.25;      // Factor used to convert range to range 
                                            // bin index of closet to grid hole.         
    static final double prime = 105.0;
    static final int lfm40_idx = 2;
    static final int LFMMX_IDX = 3;
    static final double const_val = 135.0;
    static final double angle_thresh = 9.81e-6;
    static final double re_proj = 6380.0;
    static final int KRADS = 360;

    static final double  re_proj_sq  = (re_proj*re_proj); /*6380.0*6380.0*/

    static double ka = 10.0;
    static int  offset = 66;
    
    
    //from lfm_to_azran.c
    static final double R_360 = 360.0;     // Constant representing the value 360.0  
    static final double  NINTY = 90.0;     // Constant representing the value 90.0 
    static final double  ZERO = 0.0;       // Constant representing the value 0.0 
    static final double  TWO = 2.0;        // Constant representing the value 2.0 
  //  static final double  DTR = 0.01745329;  // Degrees to radians coversion factor
  //  static final double  HALF = 0.5;     // Parameter equals to 0.5                
    
    //azimuth and range
    private int iaz;
    private int irg;
    
    double MAX_LFMVAL = 0;

    public PolarToHRAPTransformer()
    {
        ;
    }
    
    
    private void build_lfm_lookup(double radarLat, double radarLon)
    {
        double b, r, ls, lamdas, pre_gxs, gis,gjs, gi, gj, rl,
        cos_dlamda, sin_dlamda, sin_s, cos_s,
        sin_ls, cos_ls, sin_l, cos_l, sin_b, cos_b,
        cos_lamdas_prime, sin_lamdas_prime;

        int[]    is = new int[LFMMX_IDX];
        int[]  js = new int[LFMMX_IDX];

        int lfmbox;
        int status;

        int j;

        double  az_ij=0.;           //Azimuth angle to the center of an LFM box 
        double  rg_ij=0.;           // Range to the center of an LFM box    
        int     i,                  // Loop control for absolute LFM box number 
        lfm_i,              // LFM I coordinate (i increases to the right) 
        lfm_j;              // LFM J coordinate (j increases downward)

        //original C code worked, but there was no need for static, the variables are actually supposed to be constants.
        //static double ka = 10.0;
        //static int  offset = 66;

        sin_ls = 0.;
        cos_ls = 0.;

        // according to ICD, radar longitude is negative for West longitude
        // in A2, we receive the radarLon as negative, so we had to remove the A1 correction
        // 7/19/13 cg and pt
        
        ls = radarLat; 
        lamdas = radarLon; 

        /** Initialize tables before starting lookup table generation*/

        for ( i=0; i<KRADS; i++ ) 
        {
            for ( j=0; j<KBINS; j++ ) 
            {
                lfm40grid[i][j] = BEYOND_GRID;
            }

        }/* End loop KRADS */

        for ( i=0; i<FLAG_SIZE; i++ ) 
        {
            for ( j=0; j<NUM_LFM40; j++ )
            {
                lfm40flag[i][j] = BEYOND_RANGE;
            }

        }/* End loop FLAG_SIZE */ 

        /** Compute common part of the gis and gjs equations*/
        pre_gxs=r2ko*cos_ls/(ONE+sin_ls);

        /** Compute parts of equations used multiple times later*/
        cos_lamdas_prime = Math.cos((lamdas+prime)*DTR);
        sin_lamdas_prime = Math.sin((lamdas+prime)*DTR);
        sin_ls = Math.sin(ls*DTR);
        cos_ls = Math.cos(ls*DTR);

        /** Compute common part of the gis and gjs equations*/
        pre_gxs = r2ko*cos_ls/(ONE+sin_ls);

        /** Compute reference grid box coordinates*/
        gis = pre_gxs*sin_lamdas_prime+ip;
        gjs = pre_gxs*cos_lamdas_prime+jp;

        /** Compute grid box numbers for box 0,0 of local grids*/
        is[lfm40_idx] = (int)(ka*gis)-offset;
        js[lfm40_idx] = (int)(ka*gjs)-offset;

        /** Initialize bearing*/
        b=-HALF;

        /** Do for all bearings*/
        for ( iaz=0; iaz<MAX_AZMTH; iaz++ ) 
        {
            b = b+ONE;
            sin_b = Math.sin(b*DTR);
            cos_b = Math.cos(b*DTR);

            /** Initialize range*/
            r = -0.125; /* previously was r = -ONE; */

            /** Do for each input data range values (hydro application next)*/
            for ( irg=0; irg<KBINS; irg++ ) 
            {
                r = r+RNG_INC;
                sin_s = (r/re_proj)*(ONE-(const_val*r/re_proj_sq));
                cos_s = Math.sqrt(ONE-sin_s*sin_s);
                
                sin_l = (sin_ls * cos_s) + (cos_ls * sin_s * cos_b);
                cos_l = Math.sqrt(ONE-sin_l*sin_l);
                
                sin_dlamda = sin_s*sin_b/cos_l;
                cos_dlamda = Math.sqrt(ONE-sin_dlamda*sin_dlamda);
                
                rl = r2ko*cos_l/(ONE+sin_l);
                gi = rl*(sin_dlamda*cos_lamdas_prime+
                        cos_dlamda*sin_lamdas_prime)+ip;
                gj = rl*(cos_dlamda*cos_lamdas_prime-
                        sin_dlamda*sin_lamdas_prime)+jp;

                /** Compute 1/40 lfm i and j coordinates of the range/azimuth bin*/
                lfm_i = (int)(gi*ka)-is[lfm40_idx];
                lfm_j = (int)(gj*ka)-js[lfm40_idx];

                /** If lfm coordinates are within local gridi, then  save it into the lookup
     table and set the 1/40 lfm box range number table to within range*/
                if ((lfm_i>0)&&(lfm_i<=HYZ_LFM40)&&(lfm_j>0)&&(lfm_j<=HYZ_LFM40))
                {
                    lfmbox = (lfm_j-1)*HYZ_LFM40+lfm_i;
                    lfm40grid[iaz][irg] = lfmbox; 
                    lfm40flag[FLAG_RNG][lfmbox-1] = WITHIN_RANGE;
                }

            } /*end loop KBINS*/

        } /*end loop MAX_AZMTH*/

        /** Find holes and determine hole filling data*/

        /* Search for holes in the 1/40 lfm lookup table and identify the
    az/ran of the data to be used to fill in the hole 
         */
        for (i=0; i<NUM_LFM40; i++) 
        {   

            /* Holes exist where lfm40flag[FLAG_RNG][i] contains the inital value*/
            if ( lfm40flag[FLAG_RNG][i] == BEYOND_RANGE ) 
            {   

                /* Compute i/j coordinates of the hole*/
                lfm_j = (i/HYZ_LFM40+1)-1;
                lfm_i = i-lfm_j*HYZ_LFM40;
                /*printf("build_lfm_lookup:  hole found at j = %d  i = %d \n", lfm_j, lfm_i); */

                /* Compute the az/ran of the hole*/
                AzimuthAndRangePoint point = lfm_to_azran( ls, lamdas, lfm40_idx, lfm_i+1, lfm_j+1);
                az_ij = point.getAzimuth();
                rg_ij = point.getRange();

                /* If the range is within the product coverage area requirement then
    store azimuth and range index into the lfm40flag lookup table
                 */
                if ( rg_ij <= LFM40_RNG ) 
                {   
                    lfm40flag[FLAG_AZ][i] =(int)(az_ij+AZ_RND);
                    lfm40flag[FLAG_RNG][i]=(int)(rg_ij/RG_RND)+ONE;

                }   

            }/* End if block lfm40flag equals to BEYOND_RANGE */

        }/* end for (i=0; i<NUM_LFM40; i++) */

    }

    
    AzimuthAndRangePoint lfm_to_azran ( double ls, double lamdas, int lfmsize, int lfm_i,
            int lfm_j )//double *theta_cij, double *rg_ij )
    {
        
        AzimuthAndRangePoint point = new AzimuthAndRangePoint();
        double theta_cij;
        double rg_ij;
        
        double pre_gxs,gis,gjs,
        cos_lamdas_prime,sin_lamdas_prime,sin_dlamda;
 
        double[] ai= new double[3];
        double[] aj= new double[3];
        
        double cii,cjj,sin_ls,cos_ls,
        aa,bb,cos_ss,sin_ss,cos_lij,sin_lij,
        lamda_ij,l_ij;
        
        int[] is = new int[3];
        int[] js = new int[3];
        int i;

        final double b_con[]  = {1.0, 0.25, 0.10};
        final int    ika[]    = {1, 4, 10};
        final double ka[]     = {1.0, 4.0, 10.0};
        final int    offset[] = {7, 49, 66};
        
        cos_ls = 0.0;
        sin_ls = 0.0;

        cos_lamdas_prime = Math.cos((lamdas+prime)*DTR);
        sin_lamdas_prime = Math.sin((lamdas+prime)*DTR);
        sin_ls = Math.sin(ls*DTR);
        cos_ls = Math.cos(ls*DTR);

        /* Compute common part of the gis and gjs equations*/
        pre_gxs = r2ko*cos_ls/(ONE+sin_ls);

        /* Compute regerence grid box coordinates*/
        gis = pre_gxs*sin_lamdas_prime+ip;
        gjs = pre_gxs*cos_lamdas_prime+jp;

        /* Compute grid box numbers for box 0,0 of local grids*/
        is[2]= (int)(ka[2]*gis)-offset[2];
        js[2]= (int)(ka[2]*gjs)-offset[2];

        /* Compute ai and aj constants*/
        ai[2] = (double)((is[2]-ip*ka[2]+HALF)/ka[2]);
        aj[2] = (double)((js[2]-jp*ka[2]+HALF)/ka[2]);
        cii = ai[2]+b_con[2]*lfm_i;
        cjj = aj[2]+b_con[2]*lfm_j;
        l_ij= DTR*NINTY-TWO* Math.atan(Math.sqrt(cii*cii+cjj*cjj)/r2ko);

        /* If both inputs to atan2 are 0, dont call function*/
        if ( (cii == ZERO) && (cjj == ZERO) ) 
        {
            lamda_ij = ZERO;
        }
        /* Otherwise compute lamda_ij */
        else 
        {
            lamda_ij = -prime*DTR+ Math.atan2(cii,cjj);
        }

        /* Compute intermediate values*/
        cos_lij = Math.cos(l_ij);
        sin_lij = Math.sin(l_ij);
        sin_dlamda = Math.sin(lamda_ij-DTR*lamdas);
        aa = cos_lij*sin_dlamda;
        bb = cos_ls*sin_lij-sin_ls*cos_lij* Math.cos(lamda_ij-DTR*lamdas);
        sin_ss = Math.sqrt(aa*aa+bb*bb);
        cos_ss = Math.sqrt(ONE-sin_ss*sin_ss);

        /* Compute range*/
        rg_ij = (const_val*sin_ss+re_proj)*sin_ss;

        /* If sin_ss is greater than a small positive number compute theta_cij */
        if ( (double)sin_ss>=(double)angle_thresh ) 
        {
            theta_cij =  Math.atan2(cos_lij*cos_ls*sin_dlamda,(sin_lij-sin_ls*cos_ss));
        } 
        /* Otherwise, set theta_cij to 0*/
        else 
        {
            theta_cij = ZERO;
        }

        theta_cij = theta_cij/DTR;

        /* If angle is less than 0 ... add 360 degrees*/
        if ( theta_cij < ZERO ) 
        {
            theta_cij = theta_cij+R_360;
        }
        
        point.setAzimuth(theta_cij);
        point.setRange(rg_ij);
        
        return point;
        
    }
    
    public float[] transformHrapPrecipToDBA(float[] hrapGrid, float outputNoDataFlag)
    {
    	int totalGridBins = HYZ_LFM40 * HYZ_LFM40;
    	float [] dbaArray = new float[totalGridBins];
    	int k = 0;
    	int index = 0;
    	
    	for (int x = 0; x < HYZ_LFM40; x++)
    	{	
    		for (int y = HYZ_LFM40-1; y > -1; y--)
    		{		
    		
    			index = totalGridBins  - (HYZ_LFM40 * x) - (y + 1);
    			float hrapGridValue = hrapGrid[index];	
    			
    			
    			if (hrapGridValue == BEYOND_RANGE)
    			{
    				dbaArray[k] = BEYOND_RANGE;
    			}
    			
    			else if (hrapGridValue == NO_DATA_FLAG)
    			{
    				dbaArray[k] = outputNoDataFlag;
    			}
    			
    			else if (hrapGridValue == BEYOND_GRID)
    			{
    				dbaArray[k] = BEYOND_RANGE; //not an error
    			}
    			
    			else
    			{
    				dbaArray[k] =(float) (10.0 * Math.log10(  (hrapGridValue * .254) ) );
    			}
    			
    			k++;
    		} //end for y
    	} // end for x
    	
    	return dbaArray;
    	
    } //end transformHrapPrecipToSingleArrayInDBA()
    
    public float [] transform250MeterPolarToFullHRAP(float [][] polarGrid, double radarLat, double radarLon )
    {

    	float[] hrapOutputGrid = new float[17161];

    	int       FLG_ZERHLY, FLG_NOHRLY;
    	int       GRID_LAT,GRID_LON;
    	int       SIRDALAT, SIRDALON ;

    	float RADIAN_AZIMUTH, SIN_MIDPT, COS_MIDPT, HALF=0.5f;
    	float[] SIN_QUARTR = new float[2]; //2
    	float[] COS_QUARTR = new float[2]; //2
    	float[] RANGE = new float[920];//920
    	float radar_lat, radar_lon;
    	float minValueData;

    	int AZIMUTH, SPLIT;
    	int CLEAR=0, SET=1;
    	int XDIR=1, YDIR=2;
    	int X, Y, XY_DELTA, HOLEXY;
    	int i, j, WEST=1, EAST=2, NORTH=3, SOUTH=4, SUM, COUNT;
    	//  int null=0;

    	int[] DENOM_GRD = new int[17161];

    	/*---------------------------------------------------------------------*/
    	/*  generate lookup tables for polar to Cartesian grid transformation  */
    	/*---------------------------------------------------------------------*/

    	build_lfm_lookup(radarLat, radarLon);

    	/*-------------------------------------------------------------------*/
    	/*  this section of the code uses the arrays generated above to      */
    	/*   map the precip grid from polar to Cartesian                     */
    	/*  code below came from module a31467.ftn                           */
    	/*-------------------------------------------------------------------*/

    	/*----------------------------------------------------------*/
    	/*  Initialize numerator and denominator to zero            */
    	/*----------------------------------------------------------*/

    	for(i = 0; i < 17161; i++)
    	{
    		DENOM_GRD[i]  = 0;
    		hrapOutputGrid[i] = 0;
    	}
    	MAX_LFMVAL = 0.;  
    	minValueData = 99;

    	/*----------------------------------------------------------*/
    	/*  Do for each radial azimuth & each bin along the radial  */
    	/*----------------------------------------------------------*/

    	for (j = 0; j < MAX_AZMTHS; j++)
    	{

    		/*---------------------------------------------------------------------*/
    		/* DO UNTIL THE END OF THE RADIAL OR THE GRID DOMAIN HAS BEEN EXCEEDED */
    		/*---------------------------------------------------------------------*/

    		for (i = 0; i < KBINS; i++)
    		{

    			int hrapGridBinIndex = lfm40grid[j][i];

    			/*----------------------------*/
    			/* IF BIN BEYOND GRID         */
    			/*----------------------------*/

    			if(hrapGridBinIndex == BEYOND_GRID)
    			{
    				/* printf("J = %d  I = %d -- beyond grid\n", J,I); */
    				break;
    			}
    			else
    			{

    				/*---------------------------------------------------------------------*/
    				/*  Increment numerator by precip value                                */
    				/*  Increment denominator by one count...                              */
    				/*---------------------------------------------------------------------*/

    				hrapOutputGrid[hrapGridBinIndex] += polarGrid[j][i];
    				DENOM_GRD[hrapGridBinIndex] ++;
    			}

    		}  /* end for (I = 0; I < KBINS; I++) */

    	} /* end for (J = 0; J < MAX_AZMTHS; J++) */

    	/*----------------------------*/
    	/* Do for each LFM bin...     */
    	/*----------------------------*/

    	for (i = 0; i < 17161; i++)
    	{

    		/*----------------------------------*/
    		/* CHECK IF POINT IS BEYOND RANGE   */
    		/*----------------------------------*/

    		if (lfm40flag[FLAG_RNG][i] == WITHIN_RANGE)
    		{

    			/*----------------------------------*/
    			/*  Calculate value for numerator...*/
    			/*----------------------------------*/

    			if (DENOM_GRD[i] > 0)
    			{
    				hrapOutputGrid[i] = hrapOutputGrid[i]/DENOM_GRD[i];

    				if (MAX_LFMVAL < hrapOutputGrid[i])
    				{
    					MAX_LFMVAL = hrapOutputGrid[i];
    				}
    				if (minValueData > hrapOutputGrid[i])
    				{
    					minValueData = hrapOutputGrid[i];
    				}

    			}

    		}
    		else if(lfm40flag[FLAG_RNG][i] == BEYOND_RANGE)
    		{
    			/*---------------*/
    			/* BEYOND RANGE  */
    			/*---------------*/

    			/*printf("bin # %d -- beyond range\n", I); */
    			hrapOutputGrid[i] = BEYOND_RANGE;

    		}
    		else
    		{
    			/*-----------------------------------*/
    			/* FILL HOLE WITH NEAREST BIN DATA   */
    			/*-----------------------------------*/

    			/* printf("bin # %d -- hole filled by nearby bin\n", I); */

    			int azimuthBinIndex = lfm40flag[FLAG_AZ][i];
    			int rangeBinIndex = lfm40flag[FLAG_RNG][i];

    			hrapOutputGrid[i] = (float) polarGrid[azimuthBinIndex][rangeBinIndex];
    		}

    	} /* end for (i = 0; i < 17161; i++)  */

    	/*---------------------------*/
    	/* print min value to log    */
    	/* units = inches (same units as max value in db table */
    	/*---------------------------*/

    	minValueData = (minValueData/100);
    	/* printf("min value = %f\n", minValueData); */

    	return hrapOutputGrid;
    }

}
