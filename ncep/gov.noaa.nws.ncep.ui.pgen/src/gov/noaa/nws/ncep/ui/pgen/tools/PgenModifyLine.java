/*
 * gov.noaa.nws.ncep.ui.pgen.tools.PgenModifyLine
 * 
 * May 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.tools;


import gov.noaa.nws.ncep.ui.pgen.display.CurveFitter;

/**
 * Implements an algorithm to modify a line given a set of clicked points.
 * 
 * The code is mainly converted directly from the legacy C code at
 * "$GEMPAK/source/gemlib/cv/cvmdfy.c" with minimum changes to maintain 
 * the rules in the original modification algorithm. The modification is 
 * done in the "pixel" coordinate.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 05/09		#120		J. Wu   	Initial Creation.
 *
 * </pre>
 * 
 * @author	J. Wu
 */

public class PgenModifyLine {
	
	/**
     *  Computational constants
     */
	private static double TIE_DIST = 10.0;
	private static int GHOST_POINTS = 3;
	private static double PRECISION = 1e-3;	
	private static double GDIFFD = 1e-6;
	private	static float DEV_SCALE = 50.0f;
	
	/**
     *  Input required for modification
     */     
    private double[][] originalPts = null;  /* Original line */
    private double[][] clickPts = null;		/* Clicked line */
    private boolean	closed = false;			/* Closure of the original line */
    private int	smoothLevel = 0;			/* Smooth level of the original line */
    
	/**
     *  Available Output after modification
     */     
    private double[][] modifiedPts = null;	/* New line after applying modification */
    private double[][] ghostPts = null;		/* Line for ghosting after applying modification */
    private int	is = 0;						/* Starting point of ghost line on new line */
    private int	ie = 0;						/* End point of ghost line on new line */
    private int	np = 0;						/* Total number of point on new line */
    

	/**
     *  Default Constructor
     */        
    public PgenModifyLine(){    	
    	super();   	
    }
    
	/**
     *  Constructor
     */        
    public PgenModifyLine( double[][] opt,  double[][] cpt, boolean closed, int smthLevel ) {   	
    	
    	super();
    	
    	this.setOriginalPts( opt );
    	this.setClickPts( cpt );
       	this.setClosed( closed ); 	
       	this.setSmoothLevel( smthLevel ); 	
    	
       	if ( closed ) {
       		this.setOriginalPts( ensureClosed( opt ) ); 
       	}
    
    }


	/**
	 * @param clickPts the clickPts to set
	 */
	public void setClickPts(double[][] clickPts) {
		this.clickPts = clickPts;
	}


	/**
	 * @return the clickPts
	 */
	public double[][] getClickPts() {
		return clickPts;
	}


	/**
	 * @param originalPts the originalPts to set
	 */
	public void setOriginalPts(double[][] originalPts) {
		this.originalPts = originalPts;
	}


	/**
	 * @return the originalPts
	 */
	public double[][] getOriginalPts() {
		return originalPts;
	}
	
	/**
	 * @param smoothLevel the smoothLevel to set
	 */
	public void setSmoothLevel(int smoothLevel) {
		this.smoothLevel = smoothLevel;
	}

	/**
	 * @return the smoothLevel
	 */
	public int getSmoothLevel() {
		return smoothLevel;
	}
	

	/**
	 * @param closed the closed to set
	 */
	public void setClosed(boolean closed) {
		this.closed = closed;
	}

	/**
	 * @return the closed
	 */
	public boolean isClosed() {
		return closed;
	}

	/**
	 * @return the modifiedPts
	 */
	public double[][] getModifiedPts() {				
		return modifiedPts;
	}

	/**
	 * @param ghostPts the ghostPts to set
	 */
	public void setModifiedPts(double[][] modifiedPts) {
		this.modifiedPts = modifiedPts;
	}

	/**
	 * @param ghostPts the ghostPts to set
	 */
	public void setGhostPts(double[][] ghostPts) {
		this.ghostPts = ghostPts;
	}

	/**
	 * @return the ghostPts
	 */
	public double[][] getGhostPts() {
		return ghostPts;
	}	 		

	/**
	 * Perform the modification
	 */
	public void PerformModify() {
		
		/*
		 *  Ensure the first point is the same as the last point for a closed line.
		 */
		double[][] origPts;
       	if ( closed ) {
       		origPts = ensureClosed( originalPts ); 
       	}
       	else {
       		origPts = originalPts;
       	}
		
       	
       	/*
		 *  Prepare the input for calling cv_mdfy().
		 */
		int ipo = origPts.length;
		int ipc = clickPts.length;
		
		double[] xpo = new double[ ipo ];
		double[] ypo = new double[ ipo ];
		double[] xpc = new double[ ipc ];
		double[] ypc = new double[ ipc ];
		
		for ( int ii = 0; ii < ipo; ii++ ) {
			xpo[ ii ] = origPts[ii][0];
			ypo[ ii ] = origPts[ii][1];
		}

		for ( int ii = 0; ii < ipc; ii++ ) {
			xpc[ ii ] = clickPts[ii][0];
			ypc[ ii ] = clickPts[ii][1];
		}
		

		/*
		 *  Call cv_mdfy() to perform modification.
		 */
		this.setModifiedPts( cv_mdfy ( xpo, ypo, xpc, ypc, smoothLevel, closed ) );

		
		/*
		 *  Set the ghost line as well.
		 */		
		this.setGhostPts( this.getModifiedPts() );
		
	}
    

	/**
	 * Makes sure last data point is the same as the first
	 * @param  data Input data points
	 * @return Same data points with first and last point the same
	 */
	private double[][] ensureClosed( double[][] data ) {
	
		int n = data.length - 1;
		
		/*
		 * if first point equals last point, return data
		 */
		if ( (data[0][0] == data[n][0]) && (data[0][1] == data[n][1]) ) {
			return data;
		}
		else {
			/*
			 * add first point to end of data, and return new data points
			 */
			double[][] newdata = new double[data.length+1][ data[0].length ];
			
			for ( int i = 0; i < data.length; i++ ) {
				newdata[ i ] = data[ i ];
			}
			
			newdata[ data.length ] = newdata[ 0 ];
			
			return newdata;
		}
		
	}

	
	/*
	 * Calculate distance bewteeen two points
	 * @param x1, y1  coordinates of first point
	 * @param x2, y2  coordinates of second point
	 * @return        distance between two points
	 */
	private double distance( double x1, double y1, double x2, double y2) {
		
		return ( Math.sqrt( (x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1) ) );
	
	}


    /*=========================== NOTE =====================================*/
    /* The following code is directly converted from the legacy C code at   */
    /* "$GEMPAK/source/gemlib/cv/cvmdfy.c" with minimum changes to maintain */
	/* the rules in the original algorithm.								    */
    /*======================================================================*/

    private double[][] cv_mdfy ( double[] xo, double[] yo, double[] xc, 
    		                     double[] yc, int sm_lvl, boolean closed )
    /************************************************************************
     * cv_mdfy																*
     *																		*
     * This function accepts a set of points to be graphically modified,    *
     * along with a set of points that have already been clicked, and       *
     * returns the resulting line set of points. Also returned are indices 	*
     * pointing to the beginning and end of the intended modify section     *
     *																		* 
     * Notes for modifying closed lines: 									* 
     *   1.  For input, line attribute 'closed' should be set to true only  *
     *       when last point must have same coordinates as first point      *
     *   2.  For output, returned line will always be 'open' - first point  *
     *       will not repeat at the end of line. So before starting to      *
     *       plot ghost line, the first point should always be added to the *
     *       end of line to assure proper display                           *
     *   3.  For ghost line, it starts at 'is' and goes to 'ie' when     	*
     *       is < ie; if is > ie, ghost line should starts from 'is' and   	*
     *       passes through the end of line (also the beginning) and then   *
     *       goes to 'ie'                                                	*
     *																		*
     * double[][] cv_mdfy ( xo, yo, xc, yc, sm_lvl, closed)                 *
     * 	                                                                	*
     * Input parameters:													*
     *	xo		double[]	Original line x-coordinates		        		*
     *	yo		double[]	Original line y-coordinates		       			*
     *	xc		double[]	Clicked line x-coordinates		       			*
     *	yc		double[]	Clicked line x-coordinates		       			*
     *  closed	boolean		Line type (false - open true - closed)			*
     *  sm_lvl 	int			Smoothing level of line                         *
     * 	                                                                	*
     * Output parameters:													*
     *	cv_mdfy() 	double[][]  Coordinates of the modified line			*
     *	is			int     	Starting index of modification              *
     *	ie			int     	Ending index of modification                *
     *																		*
     **																		*
     * Log:																	*
     * J. Wu/Chugach	05/09	Created					       				*
     ***********************************************************************/
    { 
       int    ii, jj, nn, sm_pts, flag;
       int    op_index, os_index, opd, osd;
       double fang1, fang2, dfl;
       
       boolean modified = false;

       /*-------------------------------------------------------------------*
        * Some definitions
        * OL - Original Line         FCP - First Clicked Point
        * CL - Clicked Line          LCP - Last Clicked Point
        *
        * =v      - A point (FCP or LCP ) is within the tie distance of at 
        *           least one of OL's vortex.
        * online  - A point (FCP or LCP ) is within the tie distance of OL     
        *           but not within tie distance of any vortex of OL.
        * offline - A point (FCP/LCP ) is not within the tie distance of OL.
        *
        *------------------------------------------------------------------*/

       /* 
        * Initialize local variables 
        */
       int npo = xo.length;
       int npc = xc.length;
      
       is = 0;
       ie = npo - 1;
       
       modified = false; /* false = not modified   true - already modified    */
       op_index = -1;    /* index of FCP on OL if FCP=v                 */
       os_index = -1;    /* index of FCP on OL if FCP online            */
      	   	   
	   double[][] newLine = null;	
	   
	   /*
		*  Apply parametric smoothing on pixel coordinates, if required
		*/
	   double[][] smoothPts = new double[npo][2];
	   for ( ii = 0; ii < npo; ii++ ) {
           smoothPts[ii][0] = xo[ii];
           smoothPts[ii][1] = yo[ii];
	   } 
	   
       if ( closed ) {
      	    smoothPts = ensureClosed( smoothPts ); 
	   }
	   
	   if ( sm_lvl > 0 ) {
			
			float density;
			
			if ( sm_lvl == 1 )  {
 	    		density = DEV_SCALE / 1.0f;
 	    	}
 	    	else {
 	    		density = DEV_SCALE / 5.0f;
 	    	}
 	    		    	
            smoothPts = CurveFitter.fitParametricCurve( smoothPts, density );
 	    	
            if ( closed ) {
           	    smoothPts = ensureClosed( smoothPts ); 
   	        }
          
		}
	   
       sm_pts = smoothPts.length; 
	   
       double[] sm_x = new double[ sm_pts  ];
       double[] sm_y = new double[ sm_pts  ];
       
       for ( ii = 0; ii < sm_pts; ii++ ) {
           sm_x [ ii ] = smoothPts[ ii ][0];
           sm_y [ ii ] = smoothPts[ ii ][1];  	   
       }
        
       
       /* 
        * Make a copy of CL 
        * */      
       double[] xcp = new double[ npc ];
       double[] ycp = new double[ npc ];

       for ( ii = 0; ii < npc; ii++ ) {
            xcp [ ii ] = xc [ ii ];
            ycp [ ii ] = yc [ ii ];
       }

       /* 
        * Get the relative position of FCP on OL or its smoothed one. 
        */        
        if ( sm_lvl != 1 && sm_lvl != 2 )  {  
           
        	op_index = cvm_cptv ( xo, yo, xcp[0], ycp[0], closed );
            if ( op_index == -1 ) {
                 os_index = cvm_cptl ( xo, yo, xcp[0], ycp[0] );
    	    }
        
        }    
        else {

            op_index = cvm_cptv ( sm_x, sm_y, xcp[0], ycp[0], closed );
            if ( op_index == -1 )  {
                os_index = cvm_cptl ( sm_x, sm_y, xcp[0], ycp[0] );
            }
            
       }
             
      
       /*
        * Special case 1: if FCP offline, return CL
        */
        
       if ( op_index < 0 && os_index < 0  ) {  

           is = 0;
           np = npc;
           ie = np - 1;       
                     
           if ( closed && npc > 1 && 
                ( Math.abs(xc[0] - xc[npc-1]) < PRECISION ) &&
                ( Math.abs(yc[0] - yc[npc-1]) < PRECISION ) ) {
    	        ( np )--;
           }          
 
           newLine = new double[np][2]; 
           for ( ii = 0; ii < np; ii++ ) {
               newLine[ ii ][0] = xc [ ii ];
               newLine[ ii ][1] = yc [ ii ];                               
           }
    
           modified = true; 
       }
       
       /* 
        * Special case 2: return OL - (a) if FCP =v/online and number of   
        * clicked points is less than 2. 
        * (b) for closed lines, if FCP and LCP hits same point and number
        * of clicked points is less than 3 or CL is a straight line.
        */
       
       if ( !modified ) {  	
           
    	   if ( npc > 1 ) {
    	       fang1 = cvm_angl( xc[0], yc[0], xc[1], yc[1] );
    	   }
    	   else {
    		   fang1 = cvm_angl( xc[0], yc[0], xc[0], yc[0] );
    	   }
           
    	   fang2 = cvm_angl( xc[0], yc[0], xc[npc-1], yc[npc-1] );
           
    	   dfl = distance( xc[0], yc[0], xc[npc-1], yc[npc-1] );                  
          
           if ( ( ( op_index != -1 || os_index != -1 )   && 
                  ( npc < 2 ) )           || 
    	        ( ( closed && ( dfl <= TIE_DIST ) ) &&
    	          ( npc <= 3 || Math.abs( fang1 - fang2 ) < PRECISION ) ) ) {
    	     
               is = 0;
    	       ie = npo - 1;	   	    	                          
               np = npo;
               
               newLine = new double[np][2]; 
               for ( ii = 0; ii < np; ii++ ) {
                   newLine[ ii ][0] = xo [ ii ];
                   newLine[ ii ][1] = yo [ ii ];                              
               }
               
               modified = true;	   
           }       
       }   
       
       /* 
        * Special Case 3: for closed lines, if FCP =v on unsmoothed OL,  
        * ignore specified smoothing level & modify without smoothing
        */

      if ( !modified && closed ) { 

           osd = - 1;
           opd = cvm_cptv ( xo, yo, xcp[0], ycp[0], closed );
           if ( opd >= 0 )  {  
               
        	   newLine = cvm_mdfy( xo, yo, xcp, ycp, closed, opd, osd );             
          
               cvm_index( newLine.length, is, ie, closed ); 

               modified = true;              
           }
       }
       
       /* 
        * Normal Case 1: modify without smoothing
        */
       
       if ( !modified && sm_lvl != 1 && sm_lvl != 2 )  {  

    	   newLine = cvm_mdfy( sm_x, sm_y, xcp, ycp, closed, op_index, os_index );       
   	          
           cvm_index( newLine.length, is, ie, closed ); /* Indexing for ghost line */

           modified = true;
       }
       
       /* 
        * Normal Case 2: modify with smoothing
        */
      
       if ( !modified ) {       
          
    	   double[][] tmpLine = cvm_mdfy( sm_x, sm_y, xcp, ycp, closed, op_index, os_index );
    	              
    	   newLine = new double[ tmpLine.length][ 2 ];
           
           /*
            * Pick desired points from returned new line to form final
    	    * un-smoothed new line.
    	    */                   
    	  nn = 0;
          for ( ii = 0; ii < tmpLine.length; ii++ ) {
    	      flag = - 1;
    	            
    	      for ( jj = 0; jj < npc; jj++ ) {
    	          if ( flag == - 1 &&   
    	              Math.abs( ( xcp[ jj ] - tmpLine[ ii ][0] ) ) < PRECISION  &&
    	              Math.abs( ( ycp[ jj ] - tmpLine[ ii ][1] ) ) < PRECISION ) {                  
    		              
    	        	  newLine[ nn ][0] = tmpLine[ ii ][0];
                      newLine[ nn ][1] = tmpLine[ ii ][1];
  	      	          
                      nn++;		  
    		          flag = 1;		  
    	          }
    	      }
              
    	      if ( flag == - 1 ) {
    	          for ( jj = 0; jj < npo; jj++ ) {
    	              
    	        	  if ( flag == - 1 &&      	                  
    	        	       Math.abs( ( xo[ jj ] - tmpLine[ ii ][0] ) ) < PRECISION   &&
    	                   Math.abs( ( yo[ jj ] - tmpLine[ ii ][1] ) ) < PRECISION ) {
    		              
        	        	  newLine[ nn ][0] = tmpLine[ ii ][0];
                          newLine[ nn ][1] = tmpLine[ ii ][1];
                   	  
                          nn++;
    		              flag = 1;
    		          }
    	          }
    	      }	  	  	  
          }
   	      
 
          /* 
           *  Eliminate duplicates for close lines when LCP=v on OL.
           */
          if ( closed ) { 
        	  
        	  opd = cvm_cptv ( xo, yo, xc[npc-1], yc[npc-1], closed );
              if ( opd >= 0 )  {
    	          osd = -1;
    	          for ( ii = 0; ii < nn-1; ii++ ) {	  
    	              if ( osd == -1 &&
    		               ( ( Math.abs( newLine[ii][0] - xc[npc-1] ) < PRECISION  &&
    		    		       Math.abs( newLine[ii][1] - yc[npc-1] ) < PRECISION  &&
    		    		       Math.abs( newLine[ii+1][0] - xo[opd] ) < PRECISION  &&
    		    		       Math.abs( newLine[ii+1][1] - yo[opd] ) < PRECISION ) || 
                             ( Math.abs( newLine[ii][0] - xo[opd] ) < PRECISION  &&
                               Math.abs( newLine[ii][1] - yo[opd] ) < PRECISION  &&
                               Math.abs( newLine[ii+1][0] - xc[npc-1] ) < PRECISION &&
                               Math.abs( newLine[ii+1][1] - yc[npc-1] ) < PRECISION ) ) ) { 
                           
    	            	  osd = ii;
    		          }
    	          }
    	      
    	          if ( osd >= 0 ) {  /* shift to eliminate duplicate */
    		          newLine[ osd ][0] = xc[ npc - 1 ];
                      newLine[ osd ][1] = yc[ npc - 1 ];
    	          
                      for ( ii = osd+1; ii < nn-1; ii++ ) {
    		              newLine[ ii ][0] = newLine[ ii + 1 ][0];
                          newLine[ ii ][1] = newLine[ ii + 1 ][1];		  
                      }
                  
    		          nn--;
    	          }
    	      }	  
          }

          np = nn;
          
          while ( np > 1 && distance( newLine[np-1][0], newLine[np-1][1], 
	    		                      newLine[np-2][0], newLine[np-2][1] ) < TIE_DIST ) {
	    	  np--;
	      }

          /* 
           * get indices of FCP/LCP on new line 
           */               
          nn = np;
          if ( closed )  nn = np - 1;
          
          flag = - 1;
          for ( ii = 0; ii < nn; ii++ ) { 
    	      if ( flag == - 1 && 
    	          Math.abs( ( xcp[ 0 ] - newLine[ ii ][0] ) ) < PRECISION &&
    	          Math.abs( ( ycp[ 0 ] - newLine[ ii ][1] ) ) < PRECISION ) {
                  
    	    	  is = ii;
    		      flag = 1;
    	      }
          }                 

          flag = - 1;
          for ( ii = is + 1; ii < nn; ii++ ) { 
    	      if ( flag == - 1 && 
    	          Math.abs( ( xcp[ npc -1 ] - newLine[ ii ][0] ) ) < PRECISION &&
    	          Math.abs( ( ycp[ npc -1 ] - newLine[ ii ][1] ) ) < PRECISION ) {
               
    		      ie = ii;
    	          flag = 1;
    	      }
           }                 
                           
           cvm_index( np, is, ie, closed ); /* Indexing for ghost line */
       }              

       
       /*
        * chop off the last point from a closed line to return an open
        * line, the 'line_closed' attribute is still reserved via 'closed'. 
        */             
       double[][]  modLine = null;
       if ( newLine != null && newLine.length > 0 ) {
           
    	   int len = np;
           if ( closed && ( Math.max ( op_index, os_index ) >= 0 ) )  {
    	      len = len - 1;		
    	   }
    	      	   
    	   
           modLine = new double[ len ][ 2 ];
           for ( ii = 0; ii < len; ii++ ) {
    	       modLine[ ii ][0] = newLine[ ii ][0];
       	       modLine[ ii ][1] = newLine[ ii ][1];   
    	   }
           
       }
    
       return modLine;
       
    }
    
	/*---------------------------------------------------------------------------------*/	

	private int cvm_cptv ( double[] xin, double[] yin, double xci, double yci, boolean closed ) 
	/************************************************************************
	 * cvm_cptv																*
	 *																		*
	 * This function finds if a clicked point is within the tie distance    *
	 * of any vortex on a given line. If within the tie distances of more   *
	 * than two vortice, choose the closest one.                            *
	 *																		*
	 * int cvm_cptv ( xin, yin, xci, yci, closed )                      	*
	 * 	                                                                	*
	 * Input parameters:													*
	 *	xin		double[]	x-coordinates of input line		        		*
	 *	yin		double[]	y-coordinates of input line	                	*
	 *	xci		double		x-coordinate of the clicked point				*
	 *	yci		double		y-coordinate of the clicked point				*
	 *  closed	boolean		type of input line ( false-open; true-closed)	*
	 *																		*
	 * Return parameters:													*
	 * 	cvm_cptv()	int		index of the matched vortex						*
	 *						-1 - not within tie distance of any vortex  	* 
	 **								        								*
	 * Log:																	*
	 * J. Wu/Chugach	05/09	Created					       				*
	 ***********************************************************************/
	{
	   int npi, opi, ii, check_points;
	   double min_dist, ds;
	    
	   opi = - 1;
	   min_dist = TIE_DIST;
	   
	   npi = xin.length;
	   check_points = npi;
	   if ( closed ) check_points = npi - 1;
	   
	   for ( ii = 0; ii < check_points; ii++) {
 
		   ds = distance( xci, yci, xin[ ii ], yin[ ii ] );  
           		   		   
	       if ( ds <= min_dist ) {
	           min_dist = ds;
	           opi = ii;
	       }
	   }
	   
	   return opi;
	}
	

	private int cvm_cptl ( double[] xin, double[] yin, double xci, double yci )
	/************************************************************************
	 * cvm_cptl																*
	 *																		*
	 * This function finds if a clicked point is within the tie distance    *
	 * of any line segments on a given line. If the clicked point is within *
	 * the tie distances of more than two segments, choose the closest one. *
	 *																		*	 
	 * Note: call this function only when cv_cptv returns -1.               *  
	 *																		*
	 * int cvm_cptl ( xin, yin, xci, yci )                             		*
	 *																		*
	 * Input parameters:													*
	 *	xin		double[]	x-coordinates of input line		        		*
	 *	yin		double[]	y-coordinates of input line	                	*
	 *	xci		double		x-coordinate of the clicked point				*
	 *	yci		double		y-coordinate of the clicked point				*
	 *																		*
	 * Output parameters:													*
	 *	cvm_cptl()	int 	index of the first vortex on matched segment	*
	 *                                     									*
	 **								       									*
	 * Log:																	*
	 * J. Wu/Chugach	05/09	Created					        			*
	 ***********************************************************************/
	{
	   int ii, npi, osi;
	   double min_dist, dd, d_line, cond1, cond2;
	   double at, bt, a, b, c, ds, dleft, dright;
	   
	   osi = - 1; 
	   min_dist = TIE_DIST;
	   d_line = TIE_DIST * 10.;
	   
	   npi = xin.length;
	   
	   for ( ii = 0; ii < npi - 1; ii++ ) {        
	       at = xin [ ii + 1 ] - xin [ ii ];
	       bt = yin [ ii + 1 ] - yin [ ii ]; 
	       
	       if ( Math.abs( at ) < GDIFFD &&  Math.abs( bt ) < GDIFFD ) {
	           d_line = distance ( xci, yci, xin[ ii ], yin[ ii ] );
	       }
	       else if ( Math.abs( at ) < GDIFFD && Math.abs( bt ) > 0.0F ) {
	           d_line = Math.abs( xci - xin[ ii ] );           
	       }
	       else if ( Math.abs( bt ) < GDIFFD && Math.abs( at ) > 0.0F ) {
	           d_line = Math.abs( yci - yin[ ii ] );                 
	       }
	       else {       
	           a =   1.0F / at ;
	           b = - 1.0F / bt ;
	           c = yin [ ii ] / bt - xin [ ii ] / at; 
	           
	           dd = a * xci + b * yci + c;
	           d_line = Math.abs( dd / Math.sqrt ( a * a + b * b ) );
	       }
      
	       if ( d_line <= min_dist ) {           
	           
	    	   dleft =  distance( xci, yci, xin[ ii ], yin[ ii ]);           
	           dright = distance( xci, yci, xin[ ii+1 ], yin[ ii+1 ]);      
	           
	           ds = distance( xin[ ii+1 ], yin[ ii+1 ], xin[ ii ], yin[ ii ]);
		       
	           cond1 = dleft * dleft + ds * ds - dright * dright;
		       cond2 = dright * dright + ds * ds - dleft * dleft;
	         	   	   
		       if ( cond1 >= 0. && cond2 >= 0. ) {	      
		           min_dist = d_line;
		           osi = ii;	        	
		        }            
	        }
	    }
	    
	    return osi;
	}

	/*---------------------------------------------------------------------*/

	private double cvm_angl ( double xs, double ys, double xe, double ye ) 
	/************************************************************************
	 * cvm_angl																*
	 *																		*
	 * This function calculates the direction angle of a line segment       *
	 * (measured from x-axis to the segment, anticlockwise, 0 ~ 360 )       *
	 *																		*
	 * double cvm_angl ( xs, ys, xe, ye )                              		*
	 * 	                                                                	*
	 * Input parameters:													*
	 *	*xs		double	x-coordinate of the starting point	        		*
	 *	*ys		double	y-coordinate of the starting point              	*
	 *	*xe		double	x-coordinate of the ending point	        		*
	 *	*ye		double	y-coordinate of the ending point               	 	*
	 * 	                                                                	* 
	 * Output parameters:													*
	 * 	cvm_angl()	double	anticlockwise angle of the segment				*
	 *																		* 
	 **								        								*
	 * Log:																	*
	 * J. Wu/Chugach	05/09	Created					        			*
	 ***********************************************************************/
	{
	    double dx, dy, angle;
	   
	    angle = 90.0F;   /* default for dx = 0 */    
	    dx = xe - xs;
	    dy = ye - ys; 
	    
	    if ( Math.abs( dx ) > GDIFFD ) {
	        angle = Math.toDegrees( Math.atan ( Math.abs( dy / dx ) ) );
	    }
	    
	    if ( dx > 0.0F && dy >  0.0F )  angle = 360.0F - angle;
	    if ( dx < 0.0F && dy >= 0.0F )  angle = 180.0F + angle;
	    if ( dx < 0.0F && dy < 0.0F )   angle = 180.0F - angle;	    
	    
	    return angle;
	}


	/*---------------------------------------------------------------------*/
	
	private double[][] cvm_mdfy ( double[] xo, double[] yo, double[] xc, 
			double[] yc, boolean closed, int opi, int osi ) 
	/************************************************************************
	 * cvm_mdfy																*
	 *																		*
	 * This function accepts a set of points to be graphically modified,    *
	 * along with a set of points that have already been clicked, and       *
	 * returns the resulting line set of points. Also returned are indices 	*
	 * pointing to the beginning and end of the modified section.           *
	 *																		*
	 * void cvm_mdfy ( xo, yo, xc, yc, closed, opi, osi )                   *
	 * 	                                                                	*
	 * Input parameters:													*
	 *	xo		double[]	Original line x-coordinates		        		*
	 *	yo		double[]	Original line y-coordinates		        		*
	 *	xc		double[]	Clicked line x-coordinates		        		*
	 *	yc		double[]	Clicked line x-coordinates		        		*
	 *  oline	boolean		Line type of OL ( false-open true-closed)		* 
	 * 	opi		int			index of FCP on OL if FCP=v		        		*
	 * 	osi		int     	index of FCP on OL if FCP online				*
	 * 	                                                                	*
	 * Output parameters:													*
	 *	cvm_mdfy	double[][]  Coordinates of modified line             	*
	 *	is	int     Starting index of modification                  		*
	 *	ie	int     Ending index of modification                   			*
	 **																		*
	 * Log:																	*
	 * J. Wu/Chugach	05/09	Created					       				*
	 ***********************************************************************/
	{
	   int   npo, npc, ii, epi, esi, new_npo, to_stop, st, se;  
	   double org_angle, new_angle, direction;
	   double[][]  newLine = null;
	   	
	  /* 
	   * Initialize local variables 
	   */
	   npo = xo.length;
	   npc = xc.length;
	   direction = 0.0;
	   new_npo = npo;
	   epi = -1;  /* index of LCP on OL if LCP=v      */
	   esi = -1;  /* index of LCP on OL if LCP online */ 
	   
	   /*
	    * Eliminate possible repeating points at the end of closed lines
	    */	    
	    if ( closed ) {
	        to_stop = 0;
	        ii = npo - 1;
	        while ( to_stop == 0 && ii >= npo / 2 ) {
	            if ( distance( xo[ii], yo[ii], xo[ii-1], yo[ii-1] ) < PRECISION )
		            new_npo--;  
	            else
		            to_stop = 1;
		      
	            ii--;
	        }
	    }
	 
	   /* 
	    * Calculate the relative position of LCP on OL. 
	    */	        
	    epi = cvm_cptv ( xo, yo, xc[npc-1], yc[npc-1], closed );
	    if ( epi == -1 ) {
	        esi = cvm_cptl ( xo, yo, xc[npc-1], yc[npc-1] );
	    }
	    
	   /* 
	    * Calculate the angle between two line segments: FCP -> SCP  
	    * (second clicked point) and OL[FCP] -> OL[FCP+1] 
	    */	    
	    st = Math.max( opi, osi );
	    se = st + 1;
	    if ( !closed && st == ( new_npo - 1 ) ) {
	         se = st;
		     st = se - 1;
	    }   	            

	    org_angle = cvm_angl( xo[st], yo[st], xo[se], yo[se]);    
	    new_angle = cvm_angl( xc[0], yc[0], xc[1], yc[1]);   	
	    direction = Math.abs( new_angle - org_angle );

	   /* Modify OL according to the line type, modify direction and the
	    * relative positions of FCP, LCP to OL.
	    */	         
	    if ( !closed ) {       
		    newLine = cvm_opmd ( xo, yo, xc, yc, opi, osi, epi, esi, direction );
	    }        
	    else {                 	
	        newLine = cvm_csmd ( xo, yo, xc, yc, opi, osi, epi, esi, direction); 
	    }
	       
	   /* 
	    * If errors occurred, return OL 
	    */	    
	   if ( newLine == null ) {
		   is = 0;
		   ie = npo - 1;
		   np = npo;		          
           
		   newLine = new double[np][2]; 
           for ( ii = 0; ii < np; ii++ ) {
               newLine[ ii ][0] = xo [ ii ];
               newLine[ ii ][1] = yo [ ii ];                             
           } 	      
	   
	   }
	   
       return newLine;
	   
	} 

	/*---------------------------------------------------------------------*/   

	private void cvm_index ( int np, int is, int ie, boolean closed ) 
	/************************************************************************
	 * cvm_index															*
	 *																		*
	 * This function determines the start and end index of the ghost line   *
	 * on an input line.						        					*
	 *																		*
	 * void cvm_index ( np, is, ie, closed )                         		*
	 * 	                                                                	*
	 * Input parameters:													*
	 *	np		int		Number of points on input line  	        		*
	 *	is		int		Start index for ghost line							*
	 *	ie		int		End index for ghost line 	                		*
	 *	closed	boolean	type of input line ( false - open  true - close )	*
	 * 	                                                                	* 
	 * Output parameters:													*
	 *              None													*
	 **								       									*
	 * Log:																	*
	 * J. Wu/Chugach	05/09	Created					        			*
	 ***********************************************************************/
	{	   
	    /* 
	     * Adjust starting index backward & end index forward by number of
	     * 'GHOST_POINTS'.
	     */	     
	    is = is - GHOST_POINTS;
	    ie = ie + GHOST_POINTS;
	    
	    /* 
	     * If there are limited points on input line or the adjusted
	     * starting & end indices overlap, draw whole line as ghost line.     
	     * Otherwise, ghost line starts at 'is' and goes to 'ie' if 
	     * is < ie,  or starts from 'is' and passes the end of line  
	     * (also the beginning) and then goes to 'ie' when is > ie. 
	     */	     
	    if ( np <= 2 * GHOST_POINTS ) {
		    is = 0;
		    ie = np - 1;
	    }
	    else {         
	        if ( closed ) {  /* closed line */	    	    
		        if ( is >= 0 && ie > np - 1 ) {
		            ie = ie - np;
		        
			        if ( ie >= is ) { 
			            is = 0;
			            ie = np - 1;
			        }
		        }
		        else if ( is < 0 && ie <= np - 1 ) {
		            is = is + np;
			        if ( is <= ie ) { 
			            is = 0;
			            ie = np - 1;
			        }		
			    }
		        else if ( is < 0 && ie > np - 1 ) {
		            is = 0;
		            ie = np - 1;	    
		        }	    
		    }
	        else  {   /* open line */
	            if ( is < 0 )  is = 0;
	            if ( ie > np - 1 )  ie = np - 1;       		
	        }
	    }        
	}
	
	/*-----------------------------------------------------------------------------------*/
	
	private double[] cvm_swap ( double[] xin ) 
	/************************************************************************
	 * cvm_swap																*
	 *																		*
	 * This function reverses the sequence of an array.                     *
	 *																		*
	 * double[] cvm_swap ( xin )                                      		*
	 * 	                                                        			*
	 * Input parameters:													*
	 *	xin		double[]		Input array		                			*
	 *																		*
	 * Output parameters:													*
	 * 	cvm_swap()	double[]	Reversed array   	                     	*
	 **								        								*
	 * Log:																	*
	 * J. Wu/Chugach	05/09	Created					        			*
	 ***********************************************************************/
	{	   
	   int npi = xin.length;
	   double[] xout = new double[ npi ];
	   
	   for ( int ii = 0; ii < npi; ii++ ) {
	       xout[ ii ] = xin[ npi - ii - 1 ];
	   } 
	   
	   return xout;
	} 

	/*---------------------------------------------------------------------*/

	private double[][] cvm_opmd ( double[] xo, double[] yo, double[] xc, 
			               		  double[] yc, int fin, int fis, int lin, 
			               		  int lis, double drct )
	/************************************************************************
	 * cvm_opmd																*
	 *																		*
	 * This function accepts an open line to be graphically modified,       *
	 * along with a set of points that have already been clicked, and       *
	 * returns the resulting set of points.                                 *
	 *																		*
	 * double[][] cvm_opmd (xo, yo, xc, yc, fin, fis, lin, lis, drct )		*
	 * 	                                                                	*
	 * Input parameters:													*
	 *	xo		double[]	x-coordinates of OL		                		*
	 *	yo		double[]	y-coordinates of OL		                		*
	 *	xc		double[]	x-coordinates of CL		                		*
	 *	yc		double[]	y-coordinates of CL		                		*
	 * 	fin		int			index of FCP on OL when f FCP=v		        	*
	 *	fis		int    	 	index of FCP on OL when FCP online				*
	 *	lin		int     	index of LCP on OL when LCP=v		        	*
	 *	lis		int     	index of LCP on OL when LCP online				*
	 *	drct	double		angle between OL & CL segments                  *
	 * 	                                                               		*
	 * Output parameters:													*
	 *	is		int     	Starting index of modified section              *
	 *	ie		int     	Ending index of modified section                *
	 *	cvm_opmd()	double[][]  - Coordinates of modified line              *
	 * 	                                                               		*
	 **																		*
	 * Log:																	*
	 * J. Wu/Chugach	05/09	Created					       				*
	 ***********************************************************************/
	{
	    int FCP, LCP, ii, totalpts, isp, iep, FCP_d, F_L;
	    double dlast, dtmp, tmp;

        double[][] newLine = null;        
	    
	    int npo = xo.length;
	    int npc = xc.length;
	    
	    FCP_d = 0;   /* direction FCP->SCP  0 - same as OL  -1 - opposite  */
	    F_L = 0;     /* direction FCP->LCP  0 - same as OL  -1 - opposite  */
	    FCP = Math.max( fin, fis );
	    LCP = Math.max( lin, lis );
	    
	    if ( fin >= 0 ) {
	        FCP_d = cvm_drct( xo, yo, xc, yc, FCP, 0 );
	        F_L = cvm_drct( xo, yo, xc, yc, FCP, 2 );
	    }

	    if ( fis >= 0 ) {     
	        if ( drct > 90. && drct < 270. )   FCP_d = -1; 
	        
		    dlast = cvm_angl( xc[0], yc[0], xc[npc-1], yc[npc-1] );   	
	        dtmp = cvm_angl( xo[FCP], yo[FCP], xo[FCP+1], yo[FCP+1] );   	
	     
	        tmp = Math.abs( dlast - dtmp );
	        if ( tmp > 90. && tmp < 270. ) F_L = -1; 
	    }
	           
	    /* 
	     *  FCP & LCP both on line & satisfying internal replacement.
	     */	            
	    if ( LCP >= 0 && 
	         ( ( FCP < LCP && FCP_d == 0 ) || 
	           ( FCP > LCP && FCP_d == -1 ) ||
		       ( FCP == LCP && FCP_d == 0 && F_L == 0 ) || 
		       ( FCP == LCP && FCP_d == -1 && F_L == -1 ) ) ) {
	        
		    isp = Math.min ( FCP, LCP );
	        iep = Math.max ( FCP, LCP );

	        if ( ( FCP < LCP  && FCP_d == 0 && fin >= 0 ) ||
		         ( FCP == LCP && FCP_d == 0 && F_L == 0 && fin >= 0 ) )  
		        isp--; 
	        
		    if ( ( FCP > LCP && FCP_d == -1 ) || 
		         ( FCP == LCP && fis >= 0 && FCP_d == -1 && F_L == -1 ) ) {
		        xc = cvm_swap( xc );
		        yc = cvm_swap( yc );	            
		        if ( lin >= 0 ) isp--;	    	   
	        }
	        	
		    is = isp;              /* Starting index for modification */	
		    ie = isp + 1 + npc;    /* Ending index for modification */	
		    if ( is < 0 ) is = 0;
		    np = isp + 1 + npc + npo - iep - 1;
		    
	        newLine = new double[np][3]; 

		    /* 
	         *  If total output points exceeds 'maxpts', no modification. 
	         *  otherwise, link the segments together to form NL.
		     */	 	        	
		    for ( ii = 0; ii <= isp; ii++ )  {
		    	newLine[ ii ][0] = xo [ ii ];
		    	newLine[ ii ][1] = yo [ ii ];
	        }
		    
	        totalpts = isp + 1;
		
		    for ( ii = 0; ii < npc; ii++)  {
		    	newLine[ totalpts +  ii ][0] = xc [ ii ];
		    	newLine[ totalpts +  ii ][1] = yc [ ii ];	        
		    } 
	        
		    totalpts = totalpts + npc;
				
		     for ( ii = iep + 1; ii < npo; ii++)  {
			     newLine[ totalpts + ii - iep - 1][0] = xo [ ii ];
			     newLine[ totalpts + ii - iep - 1][1] = yo [ ii ];		        
		     } 
	        
	    }
	    
	    else  {  /* External replacement */
		
		    if ( FCP_d == 0 ) { 

		        isp = FCP;
		        if ( fin >= 0 ) isp--;
		        is = isp;
		        if ( is < 0 ) is = 0;
		        np = isp + 1 + npc;
		        ie = np - 1;	    		     
		        
		        newLine = new double[np][2]; 
		        
	            for ( ii = 0; ii <= isp; ii++)  {
				     newLine[ ii ][0] = xo [ ii ];
				     newLine[ ii ][1] = yo [ ii ];		        
	            }
		    
	            for ( ii = 0; ii < npc; ii++)  {
				     
	            	 newLine[ isp + ii + 1 ][0] = xc [ ii ];
				     newLine[ isp + ii + 1 ][1] = yc [ ii ];	        
	            	 
	            }
	        }		
		    else {	    
		        xc = cvm_swap( xc );
		        yc = cvm_swap( yc );	     	     
		        iep = FCP;	    
		        is = 0;
		        ie = npc; 
		        np = npc + npo - iep - 1;
		        
		        newLine = new double[np][2]; 
		    
	            for ( ii = 0; ii < npc; ii++)  {
				     newLine[ ii ][0] = xc [ ii ];
				     newLine[ ii ][1] = yc [ ii ];		        	                 
	            }
		    	    
	            for ( ii = iep + 1; ii < npo; ii++ )  {
				     newLine[ npc + ii - iep - 1  ][0] = xo [ ii ];
				     newLine[ npc + ii - iep - 1  ][1] = yo [ ii ];		        	                 
	            }            
	        }		    
		}
	    
	    return newLine;
	    
	    
	}

	/*----------------------------------------------------------------------*/
	  
	private int cvm_drct ( double[] xo, double[] yo, double[] xc, double[] yc, 
			               int indx, int flag )
	/************************************************************************
	 * cvm_drct																*
	 *																		*
	 * This function determines the relative direction of an given segment  *
	 * to the direction of original line.                                   *
	 * 	                                                                	*
	 * int cvm_drct ( xo, yo, xc, yc, indx, flag )                			*
	 * 	                                                                	*
	 * Input parameters:													*
	 *	xo		double[]	x-coordinates of OL		                		*
	 *	yo		double[]	y-coordinates of OL		       					*
	 *	xc		double[]	x-coordinates of OL		                		*
	 *	yc		double[]	y-coordinates of OL		                		*
	 *	indx	int			Starting index of the given segment 			*
	 *	flag	int			0 - calculate direction for FCP->SCP 			*
	 *						1 - calculate direction for xo[LCP]->LCP 		*
	 *						2 - calculate direction for FCP->LCP 			*
	 * 	                                                                	*
	 * Output parameters:													*
	 *   cvm_drct()	int     0 - The given segment runs same direction as OL	*
	 *                     -1 - The given segment runs opposite direction   *
	 *								  as OL 								*
	 **																		*
	 * Log:																	*
	 * J. Wu/Chugach	05/09	Created					        			*
	 * J. Wu/Chugach	05/10	Fixed the bug for the starting/end points	*
	 ***********************************************************************/
	{ 
	    int 	direction, npo, npc;
	    double  alpha, beta, angc, ang_from, ang_to, tmp;
	    
	    direction = 0;
	    npo = xo.length;
	    npc = xc.length;
	    
	    angc = cvm_angl( xc[0], yc[0], xc[1], yc[1] );
	    if ( flag == 1 ) {
	        angc = cvm_angl( xo[indx], yo[indx], xc[npc-1], yc[npc-1] );
	    }
	    
	    if ( flag == 2 ) {
	        angc = cvm_angl( xc[0], yc[0], xc[npc-1], yc[npc-1] );
	    }
	    
        // oops, (indx+1) may be out of bound - add a check here
	    if ( indx < (npo-1) ) {	    
	        alpha = cvm_angl( xo[indx], yo[indx], xo[indx+1], yo[indx+1] ); 	    
	    }
	    else {
	        alpha = cvm_angl( xo[indx-1], yo[indx-1], xo[indx], yo[indx] ); 	    	    	
	    }
	    
	    if ( indx > 0 ) {
	        beta = cvm_angl( xo[indx], yo[indx], xo[indx-1], yo[indx-1] );    
	    }
	    else {
//	        beta = cvm_angl( xo[indx], yo[indx], xo[npo-2], yo[npo-2] );    
	        //when indx = 0, should use direction from point 1 to point 0.
	        beta = cvm_angl( xo[indx+1], yo[indx+1], xo[indx], yo[indx] );    
	    }
	    
	    tmp = Math.max( alpha, beta );
	    ang_from = tmp - Math.abs( beta - alpha ) / 2;
	    ang_to = ang_from + 180.;	
	    
	    if ( beta >= alpha ) {    
	        if ( ang_to <= 360. )  {
		        if ( angc > ang_from  && angc < ang_to )  {
		        	direction = -1;
		        }
		    }
	        else {
		        ang_to = ang_to - 360.;
	            if ( angc <= ang_to  || angc >= ang_from ) {
	            	direction = -1;
	            }
		    }
	    }
	    else {
	        if ( ang_to <= 360. )  {
	            if ( angc <= ang_from || angc >= ang_to ) {
	            	direction = -1;
	            }
		    }
	        else {
		        ang_to = ang_to - 360.;
	            if ( angc > ang_to && angc < ang_from ) {
	            	direction = -1;
	            }
		    }
	    }	              

	    return direction;
	}

	/*---------------------------------------------------------------------*/

	private double[][] cvm_csmd ( double[] xo, double[] yo, double[] xck, 
								double[] yck, int fin, int fis, 
								int lin, int lis, double drct )
	/************************************************************************
	 * cvm_csmd																*
	 *																		*
	 * This function accepts an closed line to be graphically modified,     *
	 * along with a set of points that have already been clicked, and       *
	 * returns the resulting line set of points.                            *
	 *																		*
	 * double[][] cvm_csmd ( xo, yo, xck, yck, fin, fis, lin, lis, drct)	*
	 * 	                                                                	*
	 * Input parameters:													*
	 *	xo		double[]	x-coordinates of OL		               			*
	 *	yo		double[]	y-coordinates of OL		       					*
	 *	xck		double[]	x-coordinates of OL		                		*
	 *	yck		double[]	y-coordinates of OL		                		*
	 * 	fin		int			index of FCP on OL when FCP=v 		        	*
	 *	fis		int     	index of FCP on OL when FCP online				*
	 *	lin		int     	index of LCP on OL when LCP=v 		        	*
	 *	lis		int     	index of LCP on OL when LCP online				*
	 *	drct	double		angle between OL & CL segments                  *
	 * 	                                                                	*
	 * Output parameters:													*
	 *	is		int     	Starting index of modified section              *
	 *	ie		int     	Ending index of modified section                *
	 *	cvm_csmd()	double[][]	Coordinates of modified line 				*
	 *                              										*
	 **																		*
	 * Log:																	*
	 * J. Wu/Chugach	05/09	Created					        			*
	 ***********************************************************************/
	{
	    int FCP, LCP, ii, isp, iep;
	    int FCP_drct, LCP_drct, FCP_to_LCP;
	    double ds, min_dist, dlast, dtmp, tmp;
	    
	    int npo = xo.length;
	    int npc = xck.length;
	    
	    double[] xc = new double[ npc ];
	    double[] yc = new double[ npc ];
	    
	    for ( ii = 0; ii < npc; ii++ ) {
	    	xc[ ii ] = xck[ ii ];
	    	yc[ ii ] = yck[ ii ];
	    }
	    
	    double[][] newLine = new double[npc + npo + 2][2];	    
	    
	    min_dist = 1e10;
	    LCP_drct = 0;    /* OL[LCP] -> CL[LCP], when LCP off line */
	    FCP_drct = 0;    /* FCP -> SCP                            */	
	    FCP_to_LCP = 0;  /* CL[0] -> CL[ npc - 1 ]                */	
	        
	    if ( fin == npo - 1 )  {
	    	fin = 0;
	    }
	    
	    FCP = Math.max( fin, fis );
	    LCP = Math.max( lin, lis );

	    /*
	     * If FCP & LCP hits same vortex, return CL
	     */	     
	    if ( distance( xc[0], yc[0], xc[npc-1], yc[npc-1] ) <= TIE_DIST ) {
	        is = 0;
	        np = npc;
	        ie = np - 1;       
	        for ( ii = 0; ii < npc; ii++ ) {
	            newLine[ii][0] = xc[ii];
	            newLine[ii][1] = yc[ii];
	        }
	        
	        return newLine;
	    }
	    
	    /* 
	     * If LCP is not on original line, find the closest vortex to it. 
	     */	       
	    if ( LCP < 0 ) {
	        for ( ii = 0; ii < npo - 1; ii++) {
	             ds = distance( xc[npc-1], yc[npc-1], xo[ii], yo[ii] ); 
		     if ( ds <= min_dist ) {
	                 min_dist = ds;
	                 LCP = ii;
	             } 
	        }       
	    }
	    
	    /* 
	     * Calculate the starting modify direction (FCP_drct),
	     * the ending modify direction (LCP_drct, if LCP offline)
	     * and the direction from segment FCP->LCP relative to OL.
	     */	          
	    if ( fis >= 0 ) { /* FCP online */      
	         if ( drct > 90. && drct < 270. )  FCP_drct = - 1; 

	         dlast = cvm_angl( xc[0], yc[0], xc[npc-1], yc[npc-1] );   	
	         dtmp = cvm_angl( xo[FCP], yo[FCP], xo[FCP+1], yo[FCP+1] );   	
	     
	         tmp = Math.abs( dlast - dtmp );
	         if ( tmp > 90. && tmp < 270. ) FCP_to_LCP = - 1; 
	    }
	     
	    if ( fin >= 0 ) { /* FCP =v */
	         FCP_drct = cvm_drct( xo, yo, xc, yc, FCP, 0 );
	         FCP_to_LCP = cvm_drct( xo, yo, xc, yc, FCP, 2 );
	    }
	                
	    if ( lin == -1 && lis == -1 ) {  /* LCP offline */
	        LCP_drct = cvm_drct( xo, yo, xc, yc, LCP, 1 );
	    }    

	    /* 
	     * Choose a proper vortex on OL to connect with LCP if LCP offline. 
	     */
	    if ( lin == - 1 && lis == - 1 ) { 

		    if ( FCP != LCP ) {

			    if ( fin >= 0 || ( fis >= 0 && FCP_drct >= 0 ) ) {

		            if ( FCP_drct >= 0 && LCP_drct >= 0 ) {
		                LCP++;
		                if ( FCP_to_LCP >= 0 && FCP == LCP )  LCP--;
	                }	    
	            
		            if ( FCP_drct < 0  &&  LCP_drct < 0 ) {
	                    if ( FCP == ( npo - 2 ) && LCP == 0 ) {
			                LCP = FCP - 1;
	                    }
			            else {
		                    LCP--;	
		                    if ( FCP_to_LCP < 0 && FCP == LCP ) LCP++;
			            }
	                }	        	   
		        }
		    
		        if ( fis >= 0 && FCP_drct < 0 ) {
		            if ( LCP_drct < 0 && FCP_to_LCP < 0 ) {
		            	FCP++;
		                LCP--;
		            }
		           // else if ( LCP_drct >= 0 && FCP_to_LCP < 0 ) {
		           // 	LCP++;
		           // }
		            else {
		                FCP++;
		            }
		        }
		        
		                	    
		        if ( LCP < 0 ) LCP = npo - 2; 
		        if ( FCP > ( npo - 2 ) ) FCP = npo - 2; 
		    
		    }
	        else {
	             if ( fin >= 0 ) { /* FCP =v */
		             if ( FCP_to_LCP >= 0 ) 
			             LCP++;
			         else {       /* FCP online */
			             LCP--;
			             if ( lin >= 0 ) FCP++;
			         }
	                 
		             if ( LCP < 0 ) LCP = npo - 2; 
		         }
		         else {
		             if ( FCP_drct >= 0 && FCP_to_LCP >= 0 ) LCP++;		 
		             if ( FCP_drct < 0  ) {
		                 if ( LCP_drct >= 0 && FCP_to_LCP < 0 ) 
		                     FCP++;
		                 else if ( LCP_drct < 0 && FCP_to_LCP < 0 ) {
		                     FCP++;
		                     LCP--;
		                 }
			 
		             }
		         }
	         }	               
	    } 
       
	    /*
	     * Case 1 : FCP & LCP fall on same segment of OL
	     */	 
	    if ( FCP == LCP ) {
		    
	    	if ( FCP_to_LCP < 0 && FCP_drct < 0 ) {
		        xc = cvm_swap( xck );             
	            yc = cvm_swap( yck );
		    }                

		    /* 
		     * a: CL is insert at begining of NL & closed 
		     */		 
		    isp = 0;
		    iep = npc - 1;   
		    is = isp;
		    np = npc;		    

		    for ( ii = 0; ii < npc; ii++ ) {
		        newLine[ii][0] = xc [ ii ];
		        newLine[ii][1] = yc [ ii ];
		    }

		    if ( fis >= 0 && lin == -1 && lis == -1 ) {

		    	newLine[ npc ][0] = xo[ FCP ];
		    	newLine[ npc ][1] = yo[ FCP ];	    	    

		    	newLine[ npc + 1 ][0] = xc[ 0 ];
		    	newLine[ npc + 1 ][1] = yc[ 0 ];	    	     		    	     	
		    
		        np = np + 2;
		        ie = np - 1;
		        
		        double[][] finalLine = new double[ np ][ 2 ];

		        for ( int ij = 0; ij < np; ij++ ) {
		        	finalLine[ ij ][0] = newLine[ ij ][0];
			        finalLine[ ij ][1] = newLine[ ij ][1];	    	     		     			    			        	
		        }

		        return finalLine;		    
		    }
		
		    
	        if ( ( fin >= 0 && lis >= 0  && FCP_drct < 0 )   ||
		        ( fin >= 0 && lin == -1 && lis == -1 )      ||
	            ( fis >= 0 && lin >= 0  && FCP_drct >= 0 )  ||
	            ( fis >= 0 && lis >= 0  && (
		          ( FCP_to_LCP < 0 && FCP_drct >= 0 ) ||
		          ( FCP_to_LCP >= 0 && FCP_drct < 0 ) ) ) ) {		   	   
		   
		        newLine[ npc ][0] = xc [ 0 ];
		        newLine[ npc ][1] = yc [ 0 ];	    	     		    	     	
		    	
		        np = np + 1;
		        ie = np - 1;

		        double[][] finalLine = new double[ np ][ 2 ];

		        for ( int ij = 0; ij < np; ij++ ) {
		        	finalLine[ ij ][0] = newLine[ ij ][0];
			        finalLine[ ij ][1] = newLine[ ij ][1];	    	     		    	     			    			        	
		        }
		        
		        return finalLine;
		    }
		
		    /* 
		     * b: CL is inserted into middle of NL 
		     */					
		    isp = FCP;
		    iep = FCP + 1;
	  	   
		    /* 
		     * Starting segment 
		     */		
		    is = isp;
		    np = isp + 1;
		    
		    for ( ii = 0; ii <= isp; ii++ ) {
		        newLine[ ii ][0] = xo [ ii ];
		        newLine[ ii ][1] = yo [ ii ];
		    }
		    
		    /* 
		     * Middle segment 
		     */		 
		    if ( ( fin >= 0 && lis >= 0 && FCP_drct >= 0 ) ||  
		    	  ( fis >= 0 && lin >= 0 && FCP_drct < 0  ) ) { 
		         
		    	np = np + npc - 1;
		        
		    	for ( ii = 1; ii < npc; ii++ ) {
		        	 newLine[ ii + isp ][0] = xc [ ii ];
		        	 newLine[ ii + isp ][1] = yc [ ii ];
		        }
		    }	           

		    if ( fis >= 0 && lis >= 0 && (
		          ( FCP_to_LCP >= 0 &&  FCP_drct >= 0  ) ||  
		          ( FCP_to_LCP < 0 &&  FCP_drct < 0  ) ) )   { 
		        
		    	np = np + npc;
		        for ( ii = 0; ii < npc; ii++ ) {
		            newLine[ ii + isp + 1 ][0] = xc [ ii ];
		            newLine[ ii + isp + 1 ][1] = yc [ ii ];
		         }
		     }	           
		    
		     ie = np;
		 
		     /* 
		      * Ending segment 
		      */		 
		     for ( ii = iep; ii < npo; ii++ ) {
		         newLine[ ii + np - iep ][0] = xo[ ii ];
		         newLine[ ii + np - iep ][1] = yo[ ii ];
		     }	           	
		
		     np = np + npo - iep; 	    
	         if ( ie > ( np - 1 ) )  ie = np - 1;
		 
		     double[][] finalLine = new double[ np ][ 2 ];

		     for ( int ij = 0; ij < np; ij++ ) {
		         finalLine[ ij ][0] = newLine[ ij ][0];
			     finalLine[ ij ][1] = newLine[ ij ][1];	    	     	    	     			    			        	
		     }

	         return  finalLine;
	    
	    }


	    /* 
	     * Case 2: FCP, LCP fall on different segments of OL
	     */	       
	    if ( FCP_drct >= 0 ) { /* CL runs same direction of original */
	        isp = FCP;
		    iep = LCP;
		    if ( lis >= 0 ) iep = LCP + 1;
	    }        
	    else  {               /* CL runs opposite direction of original */
	        isp = LCP;
		    iep = FCP;
		    if ( fis >= 0 && ( lin >= 0 || lis >= 0 ) )  iep = FCP + 1; 
	         	
		    xc = cvm_swap( xck );             
	        yc = cvm_swap( yck );                
	    }

	    /* 
	     *  Join the line segments together to form the new line
	     */
	                   
	    if ( ( FCP_drct >= 0 && FCP < LCP ) || 
	         ( FCP_drct <  0 && FCP > LCP ) )  {

	        /* 
		     * Starting segment 
		     */	        	
	        is = isp;
	        np = isp + 1; 

	        if ( lin == - 1 && lis == - 1 && isp < 0 ) isp = 0;
		     
	        for ( ii = 0; ii <= isp; ii++ ) {
	            newLine[ ii ][0] = xo [ ii ];
	            newLine[ ii ][1] = yo [ ii ];
	        }
	        	        
		
		    /* 
		     * Middle segment 
		     */
		    if ( distance( xc[0], yc[0], xo[isp], yo[isp] ) <= TIE_DIST ) { 
		        for ( ii = 0; ii < npc; ii++ ) {
		            newLine[ ii + np - 1 ][0] = xc [ ii ];
		            newLine[ ii + np - 1 ][1] = yc [ ii ];	     
		        }    
		    
		        np = np + npc - 1;
		    }
		    else {
		        
		    	for ( ii = 0; ii < npc; ii++ ) {
		            newLine[ ii + np ][0] = xc [ ii ];
		            newLine[ ii + np ][1] = yc [ ii ];
		    	}
		    
		        np = np + npc;
		    }	           

		    ie = np;
		 	           		
		    /* 
		     * Ending segment 
		     */
		    if ( distance( xc[npc-1], yc[npc-1], xo[iep], yo[iep] ) <= TIE_DIST ) {
		        iep++; 
		    }
		
		    for ( ii = iep; ii < npo; ii++ ) {
		        newLine[ ii + np - iep ][0] = xo [ ii ];
		        newLine[ ii + np - iep ][1] = yo [ ii ];
		    }
		
		    np = np + npo - iep;
					    
	        double[][] finalLine = new double[ np ][ 2 ];
	        int ij = 0;
	        for ( ij = 0; ij < np; ij++ ) {
	        	finalLine[ ij ][0] = newLine[ij][0];
		        finalLine[ ij ][1] = newLine[ij][1];	    	     		    	     			    			        	
	        }

		    return finalLine;
	   }
	   
	   else {             	 
  
	        /* 
		     * Starting segment 
		     */			
		    is = 0; 
		    np = npc;
	        	        
		    for ( ii = 0; ii < npc; ii++ ) {
		        newLine[ ii ][0] = xc [ ii ];
		        newLine[ ii ][1] = yc [ ii ];
		    }	           	
		
	        ie = np; 
			
		    /* 
		     * Ending segment 
		     */		 	
		    if ( fin >= 0 && lin >= 0 )  {
		        np = np + ( isp - iep - 1 );
		        for ( ii = iep + 1; ii < isp; ii++ ) {
		            newLine[ npc + ii - iep - 1 ][0] = xo [ ii ];
		            newLine[ npc + ii - iep - 1 ][1] = yo [ ii ];
		        }
		                
		        if ( distance( xc[0], yc[0], xo[isp], yo[isp] ) > TIE_DIST ) { 
		            newLine[ np ][0] = xo [ isp ];	      		
		            newLine[ np ][1] = yo [ isp ];	      		

		            (np)++;
	 	        }
		    
		        newLine[ np ][0] = xc [ 0 ];
		        newLine[ np ][1] = yc [ 0 ];	      	    
		        
		        np++;
		    }	           	
		 
		    else if ( ( FCP_drct >= 0 && FCP < LCP && fin >= 0  && lin == -1 ) ||
	                  ( FCP_drct < 0 && FCP > LCP && fin == -1 && lin >= 0 ) )  {
		        np = np + isp - iep + 1;
		        for ( ii = iep; ii <= isp; ii++ ) {
		            newLine[ npc + ii - iep ][0] = xo [ ii ];
		            newLine[ npc + ii - iep ][1] = yo [ ii ];
		        }
		    }
		
		    else if ( ( FCP_drct >= 0 && FCP < LCP && fin == -1 && lin >= 0 ) ||
	                  ( FCP_drct < 0 && FCP > LCP && fin >= 0  && lin == -1 ) ) {
		        np = np + isp - iep + 1;
		        for ( ii = iep + 1; ii <= isp; ii++ ) {
		            newLine[ npc + ii - iep - 1 ][0] = xo[ ii ];
		            newLine[ npc + ii - iep - 1 ][1] = yo[ ii ];
		            
		        }
		        
		        newLine[ np - 1][0] = xc[ 0 ];
		        newLine[ np - 1][1] = yc[ 0 ];	    
		            
		    }   
		
		    else  {
		        ds = distance( xc[npc-1], yc[npc-1], xo[iep], yo[iep] );
		        if ( ds <= TIE_DIST ) iep++; /* avoid duplicate */
		        np = np + isp - iep;
		        for ( ii = iep; ii < isp; ii++ ) {
		            newLine[ npc + ii - iep ][0] = xo [ ii ];
		            newLine[ npc + ii - iep ][1] = yo [ ii ];
		            
		        }	           

		        if ( distance( xc[0], yc[0], xo[isp], yo[isp] ) > TIE_DIST ) {
		            newLine[ np ][0] = xo [ isp ];	      		
		            newLine[ np ][1] = yo [ isp ];	      		      		

		            np++;
		            
	 	        }

	 	        newLine[ np ][0] = xc [ 0 ];
		        newLine[ np ][1] = yc [ 0 ];	      
		              
		        np++;
	            
		    }

		    
	        double[][] finalLine = new double[ np ][ 2 ];	        	        
	        for ( int ij = 0; ij < np; ij++ ) {
	        	finalLine[ ij ][0] = newLine[ij][0];
		        finalLine[ ij ][1] = newLine[ij][1];	    	     	  	     			    			        	
	        }

	        return finalLine;
	    }	
	}

}

