/*
 * gov.noaa.nws.ncep.ui.pgen.graphToGrid.ContoursExtension
 * 
 * March 2010
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.graphtogrid;

import gov.noaa.nws.ncep.gempak.parameters.core.categorymap.CatMap;
import gov.noaa.nws.ncep.ui.pgen.PgenSession;
import gov.noaa.nws.ncep.ui.pgen.PgenUtil;
import gov.noaa.nws.ncep.ui.pgen.contours.ContourLine;
import gov.noaa.nws.ncep.ui.pgen.contours.Contours;
import gov.noaa.nws.ncep.ui.pgen.display.CurveFitter;
import gov.noaa.nws.ncep.ui.pgen.elements.AbstractDrawableComponent;
import gov.noaa.nws.ncep.ui.pgen.elements.Line;
import gov.noaa.nws.ncep.ui.pgen.rsc.PgenResource;

import java.awt.Color;
import java.util.ArrayList;

import com.vividsolutions.jts.geom.Coordinate;

/**
 * Class for Graph-to-Grid to extend contour lines to an given boundary
 * and prepare data for later calculation in grid space.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 03/10		#215		J. Wu   	Initial Creation.
 * 
 * </pre>
 * 
 * @author	J. Wu
 */

public class ContoursExtension {

//	private final static Logger logger = Logger.getLogger( ContoursExtension.class);

	/*
	 * Input: contours, a coordinate transformation tool and a rectangle bound.
	 */
	private Contours	contour;
	private CoordinateTransform	gtrans;
	
	private int		kx;
	private int		ky;
	
	private Color[]		extColors;
	
	private CatMap	catmap;

	/*
	 * Output.
	 */
	private int			nlines = 0;		//Total number of lines
	private int[]		npts = null;	//Number of points on each line
	private float[][]	flat = null;	//Latitudes as [nlines][ npts[ line n ] ]
	private float[][]	flon = null;	//Longitudes as  [nlines][ npts[ line n ] ]
	private int[]		nOrigPts = null; //Number of points on each contour line
	private float[][]	fi_orig = null;	//Smoothed latitudes in Grid coordinate
	private float[][]	fj_orig = null;	//Smoothed longitudes in Grid coordinate
	private int[]		nExtPts = null; //Number of points on each line
	private float[][]	fi_ext = null;	//Smoothed and extended latitudes in Grid space
	private float[][]	fj_ext = null;	//Smoothed and extended latitudes in Grid space
	
	private float[]		value = null;	//Value for each contour line
	private int[]		closed = null;	//closeness for each line
	private int[]		ismth = null;	//smoothing factor for each line
	
	// Collection of all extension segments.
	private ArrayList<AbstractDrawableComponent>	extLines = null;
	
    
	/**
     * Constructor
     */
	public ContoursExtension( Contours currentGraph, CoordinateTransform gtrans,
			                  int kx, int ky, Color[] extClr, CatMap cmap ) {				
        
		this.contour = currentGraph;
		this.gtrans = gtrans;
		this.extColors = extClr;
        setExtent( kx, ky );
        
        setCatmap( cmap );
		
		computeExtentions();				
	}

	/**
	 * @param contour the contour to set
	 */
	public void setContour(Contours contour) {
		this.contour = contour;
	}
	
	/**
	 * @param gtrans the ToordinateTransform to set
	 */
	public void setGtrans( CoordinateTransform gtrans ) {
		this.gtrans = gtrans;
	}

	/**
	 * @param minx the kx to set
	 */
	public void setKx( int kx) {
		this.kx = kx;
	}

	/**
	 * @param minx the k to set
	 */
	public void setKy( int ky) {
		this.ky = ky;
	}

	/**
	 * @param extColors the extColors to set
	 */
	public void setExtColors(Color[] extColors) {
		this.extColors = extColors;
	}

	
	/**
	 * @return the catmap
	 */
	public CatMap getCatmap() {
		return catmap;
	}

	/**
	 * @param catmap the catmap to set
	 */
	public void setCatmap(CatMap catmap) {
		this.catmap = catmap;
	}

	/*
	 * set the extent area (in pixel coordinate)
	 */
	public void setExtent(  int kx, int ky ) {
		this.kx = kx;
		this.ky = ky;
		
	}
	
	/**
	 * @return the extLines
	 */
	public ArrayList<AbstractDrawableComponent> getExtLines() {
		return extLines;
	}

	/**
	 * @return the nlines
	 */
	public int getNlines() {
		return nlines;
	}

	/**
	 * @return the npts
	 */
	public int[] getNpts() {
		return npts;
	}

	/**
	 * @return the flat
	 */
	public float[][] getFlat() {
		return flat;
	}

	/**
	 * @return the flon
	 */
	public float[][] getFlon() {
		return flon;
	}

	/**
	 * @param nOrigPts the nOrigPts to set
	 */
	public void setNOrigPts(int[] nOrigPts) {
		this.nOrigPts = nOrigPts;
	}

	/**
	 * @return the nOrigPts
	 */
	public int[] getNOrigPts() {
		return nOrigPts;
	}

	/**
	 * @return the fi_orig
	 */
	public float[][] getFiOrig() {
		return fi_orig;
	}

	/**
	 * @return the fj_orig
	 */
	public float[][] getFjOrig() {
		return fj_orig;
	}

	/**
	 * @param nExtPts the nExtPts to set
	 */
	public void setNExtPts(int[] nExtPts) {
		this.nExtPts = nExtPts;
	}

	/**
	 * @return the nExtPts
	 */
	public int[] getNExtPts() {
		return nExtPts;
	}

	/**
	 * @return the fi_ext
	 */
	public float[][] getFiExt() {
		return fi_ext;
	}

	/**
	 * @return the fj_ext
	 */
	public float[][] getFjExt() {
		return fj_ext;
	}
	/**
	 * @return the value
	 */
	public float[] getValue() {
		return value;
	}

	/**
	 * @return the closed
	 */
	public int[] getClosed() {
		return closed;
	}

	/**
	 * @return the ismth
	 */
	public int[] getIsmth() {
		return ismth;
	}

	/*
	 * Calculate the extension points for later retrieval.
	 */
	private void computeExtentions() {
		 
		ArrayList<ContourLine> clines = contour.getContourLines();
		
		nlines = clines.size();
		npts = new int[ nlines ];
		nOrigPts = new int[ nlines ];
		nExtPts = new int[ nlines ];
		value = new float[ nlines ];
		closed = new int[ nlines ];
		ismth = new int[ nlines ];
		flat = new float[nlines][];
		flon = new float[nlines][];
		fi_orig = new float[nlines][];
		fj_orig = new float[nlines][];
		fi_ext = new float[nlines][];
		fj_ext = new float[nlines][];
		
		extLines = new ArrayList<AbstractDrawableComponent>();
		
		int ii = 0;
		for ( ContourLine cline : clines ) {

			// Make sure a closed line is actually closed.
			Line line = ensureClosed( cline.getLine() );
			
			Coordinate[] linePts = line.getLinePoints();
			npts[ ii ] = linePts.length;
		    
		    flat[ii] = new float[ npts[ii] ];
		    flon[ii] = new float[ npts[ii] ];
		    
		    for ( int jj = 0; jj < npts[ii]; jj++ ) {
		    	flat[ ii ][ jj ] = (float)linePts[ jj ].y;
		    	flon[ ii ][ jj ] = (float)linePts[ jj ].x;

//		    	ismth[ ii ] = line.getSmoothFactor();
//		    	closed[ ii ] = ( line.isClosedLine()) ? 1 : 0;
		    	
//		    	value[ ii ] = Float.parseFloat( cline.getLabelString()[0] );
		    }
	    	
	    	ismth[ ii ] = line.getSmoothFactor();
	    	closed[ ii ] = ( line.isClosedLine()) ? 1 : 0;
		    value[ ii ] = GraphToGrid.getValueForLabel( catmap, cline.getLabelString()[0] );

		    /*
		     *  Smooth the line
		     */
		    smoothLine( line, ii );

		    /*
		     *  Extend the line
		     */
		    computeLineExtentions( line, ii );		    
		    
		    ii++;
		}
		               		
    }	
	
	
	/**
	 *  Compute extension points.
	 */
	private void smoothLine( Line ln, int index ) {
		
		if ( ln != null ) {

			PgenResource drawingLayer = PgenSession.getInstance().getPgenResource();
			
		    /*
		     * Find the visible part of the contour line in the current view.
		     */	    		    
		    Coordinate[] pts = ln.getLinePoints();
			float density;
		    double[][] pixels;
		    double[][] smoothpts;

			/*
			 * convert lat/lon coordinates to pixel coordinates
			 */
		    pixels = PgenUtil.latlonToPixel( pts, drawingLayer.getDescriptor() );
		
			/*
			 *  Apply parametric smoothing on pixel coordinates, if required
			 */
			if ( ln.getSmoothFactor() > 0 ) {
				float devScale = 50.0f;
	  	    	if ( ln.getSmoothFactor() == 1 )  {
	  	    		density = devScale / 1.0f;
	  	    	}
	  	    	else {
	  	    		density = devScale / 5.0f;
	  	    	}
	  	    	
	  	    	smoothpts = CurveFitter.fitParametricCurve( pixels, density );
	  	    }
			else {
				smoothpts = pixels;
			}			
			
           	/*
           	 *  Save the points (in map coordinate). 
           	 */
           	int origPts = smoothpts.length;
           	
           	nOrigPts[ index ] = origPts;
           	
           	fi_orig[ index ]= new float[ origPts ];
           	fj_orig[ index ]= new float[ origPts ];			
          	
           	double[] pt;			         	
        	double[] ptsIn = new double[ origPts * 2 ];
           	
           	for ( int kk = 0; kk < smoothpts.length; kk++ ) {
           		
           		pt = drawingLayer.getDescriptor().pixelToWorld( 
 				       new double[]{ smoothpts[ kk ][ 0 ], smoothpts[ kk ][ 1 ], 0.0 } );
   		        
           		ptsIn[ kk * 2 ] = pt[ 0 ];
   		        ptsIn[ kk * 2 + 1 ] = pt[ 1 ]; 
 				       
           	}
           	               	    
        	double[] outp = gtrans.worldToGrid( ptsIn );
            
        	for ( int jj = 0; jj < origPts; jj++ ) {
		        fi_orig[ index ][ jj ] = (float)outp[ jj * 2 + 1 ];
   		        fj_orig[ index ][ jj ] = (float)outp[ jj * 2 ];
        	}          	
    	}
		
	}
	
	
	/**
	 *  Compute extension points for a line.
	 */
	private void computeLineExtentions( Line ln, int index ) {
		
	    
		if ( ln != null  ) {
			
			/*
			 *  Extend the beginning of the line to the boundary.
			 */
        	Coordinate p0 = null;
        	Coordinate p1 = null;
        	double[] newPt = null;
        	double[] newPt2 = null;   
        	
        	if ( !ln.isClosedLine() ) {
			    
        		newPt = extendLine( fj_orig[index][0], fi_orig[index][0], 
        				            fj_orig[index][1], fi_orig[index][1], 
	                                kx, ky );
        		
        		if ( newPt != null ) {			    
			        
			    	double[] lonlat = gtrans.gridToWorld( newPt );			        		    			    

			        p0 = new Coordinate( lonlat[ 0 ], lonlat[ 1 ] );			    
		       					    
			        Line firstSeg = (Line)ln.copy();
			    
			        ArrayList<Coordinate> twoPts = new ArrayList<Coordinate>();
			        twoPts.add( p0 );
			        twoPts.add( ln.getLinePoints()[0] );
			    
			        firstSeg.setPointsOnly( twoPts );
			        if ( extColors != null ) firstSeg.setColors( extColors );
			    
			        extLines.add( firstSeg );

			    }
	        
			    /*
			     *  Extend the end of the line to the boundary.
			     */			                         
			     int np = fi_orig[index].length;
			     newPt2 = extendLine ( fj_orig[index][np -1], fi_orig[index][np -1], 
			    		               fj_orig[index][np -2], fi_orig[index][np -2],
					                   kx, ky );
			     
			     if ( newPt2 != null ) {
			    			
				     double[] lonlat = gtrans.gridToWorld( newPt2 );			        		    			    
             	
			         p1 = new Coordinate( lonlat[0], lonlat[1] );		         
					   			    			    
			         Line secondSeg = (Line)ln.copy();
			    
			         ArrayList<Coordinate> seg2 = new ArrayList<Coordinate>();
			         seg2.add( p1 );
			    
			         int np1 = ln.getLinePoints().length;
			         seg2.add( ln.getLinePoints()[ np1 - 1 ] );			    
			    
			         secondSeg.setPointsOnly( seg2 );
			         if ( extColors != null ) secondSeg.setColors( extColors );
			    
			         extLines.add( secondSeg );

			     }
        	}
     	
           	/*
           	 *  Save the points (in map coordinate). 
           	 */
           	int extPts = fi_orig[ index ].length;
           	if ( p0 != null )  extPts++;
           	if ( p1 != null ) extPts++;
           	
           	nExtPts[ index ] = extPts;

           	fi_ext[ index ]= new float[ extPts ];
           	fj_ext[ index ]= new float[ extPts ];			         			         	
           	          	
           	int npp = 0;
          	if ( newPt != null ) {
           		fi_ext[ index ][ npp ] = (float)newPt[ 1 ];
           		fj_ext[ index ][ npp ] = (float)newPt[ 0 ];
           		npp++;
           	}
           	
           	for ( int kk = 0; kk < fi_orig[ index ].length; kk++ ) {           		
				fi_ext[ index ][ npp ] = fi_orig[ index ][ kk ];
           		fj_ext[ index ][ npp ] = fj_orig[ index ][ kk ];
           		npp++;
           	}
           	
           	if ( newPt2 != null ) {
           		fi_ext[ index ][ npp ] = (float)newPt2[ 1 ];
           		fj_ext[ index ][ npp ] = (float)newPt2[ 0 ];
           	}         	
        
    	} 		
		
	}

	/**
	 *  Calculate the intersection point of a directional line segment (p2->p1) with 
	 *  a given rectangle boundary.
	 *  
	 *  Note: the input is assumed in a Cartesian coordinate (X-axis should be W->E 
	 *  and Y-axis should be S->N)
	 */
	private double[] extendLine( float px1, float py1, 
			                     float px2, float py2, int kx, int ky ) {
         
		double[]  extPts = null;
		int minx = 1;
		int miny = 1;
		int maxx = kx;
		int maxy = ky;
		
		// Check if the first point(px1, py1) is inside the area.
		if ( px1 >= minx && px1 <= maxx && py1 >= miny && py1 <= maxy	) {

		    float xnew, ynew;
			
			extPts = new double[2];
			
			if ( px2 == px1 ) {
				xnew = px1;
				
				if ( px1 < px2 ) {
					ynew = miny - 1;
				}
				else {
				   ynew = maxy + 1;
				}
			}
			else {
				
				float m = ( py2 - py1) / (px2 - px1);
				float b = py1 - ( m * px1 );
								
				if ( px1 < px2 ) {
					xnew = minx - 1;
				}
				else {
					xnew = maxx + 1;
				}
				
				ynew = m * xnew + b;
				
				if ( ynew > maxy ) {
					ynew = maxy + 1;
					xnew = (ynew - b ) / m;
				}
				else if ( ynew < miny ) {
					ynew = miny - 1;
					xnew = (ynew - b ) / m;	    						
				}	    					
				
			}
			
	        extPts[ 0 ] = xnew;
	        extPts[ 1 ] = ynew;
		}
		
		return extPts;
	}
	
    /**
     * Ensure a closed line is closed by adding the first point to the end when
     * necessary.
     * 
     * @param ln
     * @return A closed line if the line is defined as closed.
     */
	private Line ensureClosed( Line ln ) {
    	
		Line newLine = (Line)ln.copy();
    	
    	if ( ln.isClosedLine() ) {
    		ArrayList<Coordinate> pts = newLine.getPoints();
    		int nn = pts.size() - 1;
    		if ( pts.get( 0 ).x != pts.get( nn ).x || 
    			 pts.get( 0 ).y != pts.get( nn ).y	) {
    			
    			pts.add( pts.get( 0 ) );       			
    		}
    	}
    	
    	return newLine;
    }		

}


