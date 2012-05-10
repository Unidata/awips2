/*
 * gov.noaa.nws.ncep.ui.pgen.graphToGrid.CoordinateTransform
 * 
 * March 2010
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.graphToGrid;

import java.io.File;

import gov.noaa.nws.ncep.ui.pgen.PgenStaticDataProvider;
import gov.noaa.nws.ncep.viz.customprojection.CustomProjectionServiceImpl;
import gov.noaa.nws.ncep.viz.customprojection.ICustomProjectionService;
import gov.noaa.nws.ncep.gempak.parameters.core.marshaller.garea.GraphicsAreaCoordinates;

//import org.apache.log4j.Logger;
import org.geotools.coverage.grid.GeneralGridGeometry;
import org.geotools.referencing.operation.DefaultMathTransformFactory;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.operation.MathTransform;

import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Class to generate a grid space and transform coordinates between the grid
 * space and the map (world) coordinate system.
 * 
 * Note: Given a projection, a graphical area ( a 2-point box specified
 *       at lower-left corner and upper right corner),  number of grid points
 *       desired at I (longitude) direction as kx and J(latitude) as ky, this
 *       class will create a grid space with (1, 1) as the lower left corner
 *       of the area, and (kx, ky) as the upper right corner of the area and 
 *       find transformations between the world map coordinate and the new grid
 *       space. The points should be organized as (lon1, lat1, lon2, lat2, ...) 
 *       in subsequent translations. 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 03/10		#215		J. Wu   	Initial Creation.
 * 07/11        #450        G. Hull     NcPathManager
 * 
 * </pre>
 * 
 * @author	J. Wu
 */

public class CoordinateTransform {
	
//	private final static Logger logger = Logger.getLogger( CoordinateTransform.class );

	/*
	 * Configure the two files in the extension
	 */
	private final String geogFilePath = PgenStaticDataProvider.getProvider().getGeogFile().getAbsolutePath();
	
	private final String stationFilePath =  PgenStaticDataProvider.getProvider().getSfcStnFile().getAbsolutePath();

    /*
     *	Max dimension for holding intersections defined in grphgd.cmn
     *  (this allows for 1000x1000 grid)
     */
	private final static int MAXDIM = 2000;

    /*
     *	Ultimate max # grid points defined in gemprm.h
     */
	private final static int LLMXTG = 1000000;

	private static DefaultMathTransformFactory dmtFactory = new DefaultMathTransformFactory();

	/*
	 *  Parameters to define and generate a grid space (see $GEMTBL/gradnav.tbl)
	 *  
	 * projection	- grid projection name string in format of "PRJ/A1/A2/A3", where
	 *				PRJ  - grid projection, e.g., 'STR' is stereographic
	 * 				A1    - Angle #1 per GEMPAK projection definitions.
	 * 				A2    - Angle #2 per GEMPAK projection definitions.
	 * 				A3    - Angle #3 per GEMPAK projection definitions.
	 * garea	- name of the geographic area, may be a name defined in "geog.tbl" or 
	 *            in format of "LLT;LLN;ULT;ULN", where
	 *				LLT   - Latitude  of lower-left  corner of grid
	 * 				LLN   - Longitude of lower-left  corner of grid
	 *				ULT   - Latitude  of upper-right corner of grid
	 *				ULN   - Longitude of upper-right corner of grid
	 *
	 * kx   	- Number of grid points in X-direction (I: horizontal/longitude)
	 * ky    	- Number of grid points in Y-direction (J: vertical/latitude) 
	 *  
	 */	
	private String	projection = null;	
	private String	garea = null;		
	private int		kx = -9999;			
	private int		ky = -9999;	
	private Coordinate[] extent = null;
	
	
	/*
	 *  Math transformation and the scaling factor for coordinate translation.
	 */
	private MathTransform	worldToGeneralGrid = null;
	private MathTransform	generalGridToWorld = null;	
	private double			xsize = -9999.0;
	private double			ysize = -9999.0;	
	private double			xscale = -9999.0;
	private double			yscale = -9999.0;	
	
	
	/**
     * Constructor
     */
	public CoordinateTransform ( String proj, String garea, 
			                     int kx, int ky ) {
 	    this.projection = proj;
 	    this.garea = garea;
 	    this.kx = kx;	    
 	    this.ky = ky;
 	    
 	    findTransform();
 	    
	}
	
	/**
	 * @return the projection
	 */
	public String getProjection() {
		return projection;
	}

	/**
	 * @param projection to be set
	 */
	public void setProjection(String projection) {
		this.projection = projection;
 	    findTransform();
	}

	/**
	 * @return the garea
	 */
	public String getGarea() {
		return garea;
	}

	/**
	 * @param garea to be set
	 */
	public void setGarea(String garea) {
		this.garea = garea;
 	    findTransform();
	}

	/**
	 * @return the kx
	 */
	public int getKx() {
		return kx;
	}

	/**
	 * @param kx to be set
	 */
	public void setKx(int kx) {
		this.kx = kx;
 	    findTransform();
	}
	
	/**
	 * @return the ky
	 */
	public int getKy() {
		return ky;
	}

	/**
	 * @param ky to be set
	 */
	public void setKy(int ky) {
		this.ky = ky;
 	    findTransform();
	}

	/**
	 * @return the extent
	 */
	public Coordinate[] getExtent() {
		return extent;
	}

	
	/**
	 *  Set up the default values and find the math transformation
	 *  and the scaling factors to be used for coordinate translation.
	 */
	private void findTransform() {
		
		/*
		 * Default will the Mercator projection within US with a 63 by 28
		 * grid.
		 */		
		if ( this.projection == null || this.projection.length() == 0 ) {
			this.projection = "DEF";
//			logger.warn( "Invalid input for PROJ - default to DEF");
		}
		
		if ( this.garea == null || this.garea.length() == 0 ) {
			this.garea = "US";
//			logger.warn( "Invalid input for GAREA - default to US");
		}
				
		if ( this.kx <= 0 ) {
			this.kx =  63;
//			logger.warn( "Invalid input for kx - default to 63");
		}
		
		if ( this.ky <= 0 ) {
			this.ky = 28;
//			logger.warn( "Invalid input for ky - default to 28");
		}
		
		if ( ( kx * ky ) > LLMXTG || ( kx + ky ) > MAXDIM ) {
			this.kx =  63;
			this.ky = 28;
//			logger.warn( "Grid is too large - default to 63 x 28");
		}
		   	
		/*
		 * Parse the garea and projection.
		 */
//        logger.debug( "Projection/garea/kx/ky are " + projection  + "/" + garea +
//        		               "/" + kx + "/" + ky );
         
		GraphicsAreaCoordinates gareaCoordObj = new GraphicsAreaCoordinates( garea ); 
        
		gareaCoordObj.setGeogFileName( geogFilePath ); 
		gareaCoordObj.setStationFileName( stationFilePath ); 
       
        boolean tempParseFlag = gareaCoordObj.parseGraphicsAreaString( garea ); 
    	gareaCoordObj.setGraphicsAreaStrValid(tempParseFlag); 

        /*
    	 *  Generate a new CRS and geometry
    	 */
    	try {
			Coordinate ll = new Coordinate();
			Coordinate ur = new Coordinate();
			
			ll.x = gareaCoordObj.getLowerLeftLon();
			ll.y = gareaCoordObj.getLowerLeftLat();

			ur.x = gareaCoordObj.getUpperRightLon();
			ur.y = gareaCoordObj.getUpperRightLat();
			
			extent = new Coordinate[ 2 ];
			extent[ 0 ] = ll;
			extent[ 1 ] = ur;		        
	        
			try {

				ICustomProjectionService customProj = 
	    		    	new CustomProjectionServiceImpl( projection, garea, 
	    		    									 geogFilePath, stationFilePath); 
                CoordinateReferenceSystem crs = customProj.getCoordinateReferenceSystem();
	            GeneralGridGeometry gridGeom = MapDescriptor.createGridGeometry( crs, ll, ur);	            

	            /*
	      	     * Find the math transformation between world coordinate and
	      	     * the native geometry.
	      	     */	      	    	      	    
	      	    MathTransform g2c = gridGeom.getGridToCRS();
	      	    MathTransform c2g = gridGeom.getGridToCRS().inverse();
	      	    
	      	    MathTransform c2m = MapUtil.getTransformToLatLon( crs );
	      	    MathTransform m2c = MapUtil.getTransformFromLatLon( crs );
				
	      	    worldToGeneralGrid = dmtFactory.createConcatenatedTransform( m2c, c2g );
	      	    generalGridToWorld = dmtFactory.createConcatenatedTransform( g2c, c2m );
	      	    	      	    
	            /*
	      	     * Find the scaling factor between the native geometry and your
	      	     * customized grid space.
	      	     */	      	    	      	    
	      	    double[] pts = { ll.x, ll.y, ur.x, ur.y };	      	    
	      	    double[] outp = new double[ pts.length ];

	      	    worldToGeneralGrid.transform( pts, 0, outp, 0, pts.length/2 );

	      	    xsize = Math.abs( (outp[2] - outp[0]) );
	      	    ysize = Math.abs( (outp[3] - outp[1]) );
	      	    
	      	    xscale = xsize / (kx - 1);
	      	    yscale = ysize / (ky - 1);
	      	          	        	      	    
	        } catch (Exception e) {
//			    logger.error( "Failed to create CoordinateTransform", e );
				e.printStackTrace();
	        }

	    } catch ( Exception e) {	    			    
//		    logger.error( "Failed to create CoordinateTransform", e );
			e.printStackTrace();
	    }
	    			
	}
	
	/**
	 * Convert a set of points in map coordinate to the customized grid coordinate -
	 * which scales coordinates between (1, 1) to (kx, ky) with (1,1) at the lower 
	 * left corner.
	 * 
	 * Note: the input should organized as (lon1, lat1, lon2, lat2, ...).
	 */
	public double[] worldToGrid( double[] lonlat ) {        
		
		double[] gridout = null;
		
		int numPts = ( lonlat == null ) ? 0 : (lonlat.length / 2);
	    		
		if ( numPts > 0 && worldToGeneralGrid != null ) {
      	    
			double[] outp = new double[ numPts * 2 ];
      	    
			/*
			 *  Convert to the native grid geometry first. 
			 */ 
			try {      	        
				worldToGeneralGrid.transform( lonlat, 0, outp, 0, numPts );
			} catch (Exception e) {
//			    logger.error( "Failed to transform from world to grid coordinate", e );
				e.printStackTrace();
			}
			
			/*
			 *  Scale from the native grid geometry to this grid space.
			 */
      	    if ( outp != null && outp.length > 0 ) {
			    gridout = new double[ numPts * 2];
      	    	for ( int ii = 0; ii < numPts; ii++ ) {
      	    	   gridout[ii*2] = (outp[ii*2] + 0.5)/ xscale + 1;
      	    	   gridout[ii*2 + 1] = ( ysize - outp[ii*2 + 1] -0.5)/ yscale + 1;
      	        }
      	    }

	    }
		
		return gridout;
		
	}

	/**
	 * Convert a set of points in the customized grid coordinate to map coordinate.
	 * The input points are scaled between (1, 1) to (kx, ky) with (1,1) at the lower 
	 * left corner.
	 * 
	 * Note: the input should be organized as (x1, y1, x2, y2, ...) where
	 *       x is corresponding to longitude in map coordinate and y is 
	 *       corresponding to latitude.
	 */
	public double[] gridToWorld( double[] gridin ) {
	    
		double[] lonlat = null;
		int numPts = ( gridin == null ) ? 0 : ( gridin.length / 2 );

		if ( numPts > 0 && generalGridToWorld != null ) {
      	    
			/*
			 *  Scale the input from the input grid space to 
			 *  the native grid geometry.
			 */
			double[] outp = new double[ numPts * 2 ];
            
			for ( int ii = 0; ii < numPts; ii++ ) {
      	    	outp[ii * 2] = ( gridin[ii * 2] - 1) * xscale -0.5;
      	    	outp[ii*2 + 1] = ysize - ( gridin[ii*2 + 1] - 1) * yscale - 0.5;
      	    }

			
			/*
			 *  Convert to map coordinate now. 
			 */ 
			try {
				lonlat = new double[ numPts * 2 ]; 
				generalGridToWorld.transform( outp, 0, lonlat, 0, numPts );
			} catch (Exception e) {
//			    logger.error( "Failed to transform from grid to world coordinate", e );
				e.printStackTrace();
			}
			
	    }
		
		return lonlat;
		
	}
		
	
	/**
	 * Convert a set of points in map coordinate to general grid coordinate.
	 * Note: the input should organized as (lon1, lat1, lon2, lat2, ...).
	 */
	public double[] worldToGeneralGrid( double[] lonlat ) {        
		
		double[] gridout = null;
		
		int numPts = ( lonlat == null ) ? 0 : (lonlat.length / 2);
	    		
		if ( numPts > 0 && worldToGeneralGrid != null ) {
      	    
			gridout = new double[ numPts * 2 ];
      	    
			/*
			 *  Convert to the native grid geometry. 
			 */ 
			try {      	        
				worldToGeneralGrid.transform( lonlat, 0, gridout, 0, numPts );
			} catch (Exception e) {
//			    logger.error( "Failed to transform from world to grid coordinate", e );
				e.printStackTrace();
			}
			
	    }
		
		return gridout;
		
	}
	
	
	/**
	 * Convert a set of points in the general grid coordinate to map coordinate.
	 * 
	 * Note: the input should be organized as (x1, y1, x2, y2, ...) where
	 *       x is corresponding to longitude in map coordinate and y is 
	 *       corresponding to latitude.
	 */
	public double[] generalGridToWorld( double[] gridin ) {
	    
		double[] lonlat = null;
		int numPts = ( gridin == null ) ? 0 : ( gridin.length / 2 );

		if ( numPts > 0 && generalGridToWorld != null ) {
      	    			
			/*
			 *  Convert to map coordinate. 
			 */ 
			try {
				lonlat = new double[ numPts * 2 ]; 
				generalGridToWorld.transform( gridin, 0, lonlat, 0, numPts );
			} catch (Exception e) {
//			    logger.error( "Failed to transform from grid to world coordinate", e );
				e.printStackTrace();
			}
			
	    }
		
		return lonlat;
		
	}

	/**
	 * Convert an array of map coordinate points to general grid coordinate.
	 */
	public Coordinate[] worldToGeneralGrid( Coordinate[] lonlat ) {        
		
		Coordinate[] gridout = null;
		
		int numPts = ( lonlat == null ) ? 0 : (lonlat.length);
				
		double[]  ptsin = new double[ numPts * 2 ];
		for ( int ii = 0; ii < numPts; ii++ ) {
			ptsin[ ii * 2 ] = lonlat[ ii ].x;
			ptsin[ ii * 2 + 1 ] = lonlat[ ii ].y;			
		}
		
		double[] ptsout = worldToGeneralGrid( ptsin );
		if ( ptsout != null && ptsout.length > 0 ) {
			gridout = new Coordinate[ numPts ];
			for ( int ii = 0; ii < numPts; ii++ ) { 
				gridout[ ii ] = new Coordinate( ptsout[ ii * 2 ],  ptsout[ ii * 2 + 1 ] );
			}
		}
				
		return gridout;
		
	}

	/**
	 * Convert an array of grid coordinate points to map coordinate.
	 */
	public Coordinate[] generalGridToWorld( Coordinate[] gridpts ) {        
		
		Coordinate[] gridout = null;
		
		int numPts = ( gridpts == null ) ? 0 : (gridpts.length);
				
		double[]  ptsin = new double[ numPts * 2 ];
		for ( int ii = 0; ii < numPts; ii++ ) {
			ptsin[ ii * 2 ] = gridpts[ ii ].x;
			ptsin[ ii * 2 + 1 ] = gridpts[ ii ].y;			
		}
		
		double[] ptsout = generalGridToWorld( ptsin );
		
		if ( ptsout != null && ptsout.length > 0 ) {
			gridout = new Coordinate[ numPts ];
			for ( int ii = 0; ii < numPts; ii++ ) { 
				gridout[ ii ] = new Coordinate( ptsout[ ii * 2 ],  ptsout[ ii * 2 + 1 ] );
			}
		}
				
		return gridout;
		
	}
	
	/**
	 * Convert an array of custom grid coordinate points to map coordinate.
	 */
	public Coordinate[] gridToWorld( Coordinate[] gridpts ) {        
		
		Coordinate[] gridout = null;
		
		int numPts = ( gridpts == null ) ? 0 : (gridpts.length);
				
		double[]  ptsin = new double[ numPts * 2 ];
		for ( int ii = 0; ii < numPts; ii++ ) {
			ptsin[ ii * 2 ] = gridpts[ ii ].x;
			ptsin[ ii * 2 + 1 ] = gridpts[ ii ].y;			
		}
		
		double[] ptsout = gridToWorld( ptsin );
		
		if ( ptsout != null && ptsout.length > 0 ) {
			gridout = new Coordinate[ numPts ];
			for ( int ii = 0; ii < numPts; ii++ ) { 
				gridout[ ii ] = new Coordinate( ptsout[ ii * 2 ],  ptsout[ ii * 2 + 1 ] );
			}
		}
				
		return gridout;
		
	}

	/**
	 * Convert an array of map coordinate points to custom grid coordinate.
	 */
	public Coordinate[] worldToGrid( Coordinate[] lonlat ) {        
		
		Coordinate[] gridout = null;
		
		int numPts = ( lonlat == null ) ? 0 : (lonlat.length);
				
		double[]  ptsin = new double[ numPts * 2 ];
		for ( int ii = 0; ii < numPts; ii++ ) {
			ptsin[ ii * 2 ] = lonlat[ ii ].x;
			ptsin[ ii * 2 + 1 ] = lonlat[ ii ].y;			
		}
		
		double[] ptsout = worldToGrid( ptsin );
		if ( ptsout != null && ptsout.length > 0 ) {
			gridout = new Coordinate[ numPts ];
			for ( int ii = 0; ii < numPts; ii++ ) { 
				gridout[ ii ] = new Coordinate( ptsout[ ii * 2 ],  ptsout[ ii * 2 + 1 ] );
			}
		}
				
		return gridout;
		
	}
	
}
