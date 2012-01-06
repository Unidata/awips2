/*
 * gov.noaa.nws.ncep.ui.pgen.graphToGrid.ContoursToGrid
 * 
 * June 2010
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.graphToGrid;

import gov.noaa.nws.ncep.ui.pgen.contours.ContourLine;
import gov.noaa.nws.ncep.ui.pgen.contours.Contours;
import gov.noaa.nws.ncep.viz.common.dbQuery.NcDirectDbQuery;

import java.awt.Color;
import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.viz.core.catalog.DirectDbQuery.QueryLanguage;
import com.raytheon.uf.viz.core.exception.VizException;

import com.vividsolutions.jts.geom.*;
import com.vividsolutions.jts.io.ParseException;
import com.vividsolutions.jts.io.WKBReader;

import org.apache.log4j.Logger;


/**
 * Class for retrieving/storing bound polygons.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 06/10		#215		J. Wu   	Initial Creation.
 * 
 * </pre>
 * 
 * @author	J. Wu
 */

public class BoundPolygon {
	
	private final static Logger logger = Logger.getLogger( BoundPolygon.class);
	
	/** 
	 * The bound specification string. 
	 */
	private	String 			bnds;
	
	/**
	 * The table alias to be looked up in CONFIG.CLO table, which corresponds to the 
	 * table name in bounds schema.
	 */
	public String			boundsTableAlias;

	/** 
	 * The column name. 
	 */
	private String			columnName;

	/** 
	 * The column value. 
	 */
	private String			columnValue;

	/** 
	 * The inout flag. 
	 */
	private Boolean			inout;

	/** 
	 * Database name, currently defaulted to "ncep". 
	 */
	public String			database		= "ncep";

	/** 
	 * Schema name, currently defaulted to "bounds". 
	 */
	public String			schema			= "bounds";
   
	/** 
	 * Factory 
	 * */
	private GeometryFactory	geometryFactory	= new GeometryFactory();
	
	/** 
	 *  Bound polygons retrieve for the specified bounds. 
	 */
	private ArrayList<MultiPolygon>	multiPolygons;
		
//	private	ArrayList<BoundPolygon> allBounds;
	
    /**
     * Constructor
     */
	public BoundPolygon( String bnds ) {				
	    
		this.bnds = bnds;
	    
	    multiPolygons = new ArrayList<MultiPolygon>();
		inout = true;
		
	    retrieveBounds();    
	}
	
	/**
	 * @return the boundsTableAlias
	 */
	public String getBoundsTableAlias() {
		return boundsTableAlias;
	}

	/**
	 * @return the columnName
	 */
	public String getColumnName() {
		return columnName;
	}

	/**
	 * @return the columnValue
	 */
	public String getColumnValue() {
		return columnValue;
	}

	/**
	 * @return the inout
	 */
	public Boolean getInout() {
		return inout;
	}

	/**
	 * @return the boundPolygon
	 */
	public ArrayList<MultiPolygon> getBoundPolygons() {
		return multiPolygons;
	}

	/**
	 *  Retrieve all bounds and save in a MultiPolygon.
	 * 
	 * @throws VizException
	 */
	private void retrieveBounds() {
	    
		// Parse the bound specification
	    parseBounds();
	    	    
		// Query the bound table name in config.clo
	    String bndsTable = "select t.table_name from config.clo t where t.alias_name = '" +
		                    boundsTableAlias.toUpperCase() + "'";
    	
	    logger.debug( "bndsTable=" + bndsTable );
	    
	    List<Object[]> tableFile = null;		
	    try {	    
	        tableFile  = NcDirectDbQuery.executeQuery(
	        		bndsTable, database, QueryLanguage.SQL );
	    } catch (Exception e ) {
	    	logger.debug( "Cannot find table for " + boundsTableAlias );
	    }
	    
		                      
	    String tableName = null; 
	    if ( tableFile.isEmpty() ){
		    // no record, try to use it directly as a table name
		    tableName = boundsTableAlias;
	    } else {
		    for (Object o : tableFile.get(0) ) {
			    tableName = (String) o;
		    }
	    }	        	
		
	    // Query the bounds from the table
	    String queryBnds;
    	queryBnds = "SELECT AsBinary(t.the_geom) FROM " + schema + "." + tableName;	    
    	if ( columnName != null  && columnValue != null ) {
//	    	queryBnds += " t" + " WHERE t." + columnName + " like '" + columnValue + "%'";
	    	queryBnds += " t" + " WHERE t." + columnName.toUpperCase() + " = '" + 
	    	                                  columnValue.toUpperCase() + "'";
	    }
    	
    	logger.debug( "queryBnds=" + queryBnds );
		
    	List<Object[]> bounds = null;
    	try {	    
		    bounds = NcDirectDbQuery.executeQuery(
                     queryBnds, database, QueryLanguage.SQL );

	    } catch (Exception e ) {
	    	logger.debug( "Cannot find bounds for " + boundsTableAlias );
	    }
       
		
	    // Read and store it
	    WKBReader wkbReader = new WKBReader();
	    
        if ( bounds != null && !bounds.isEmpty() ) {
		    for ( Object[] bnd : bounds ){
			    
		    	if ( bnd[0] != null ){
					
	    		    MultiPolygon mpoly = null;
	    			try {
	    				Geometry g = wkbReader.read( (byte[]) bnd[0] );
	    				if ( g instanceof MultiPolygon ) {
	    					mpoly = (MultiPolygon) g;
	    			    }
	    				else if ( g instanceof Polygon ) {
	    					mpoly = new MultiPolygon( new Polygon[]{ (Polygon)g },
	    							       geometryFactory );
	    				}
	    				
	    			} catch ( ParseException e) {
	    				logger.debug( "Error reading bound polygon" + e );
	    			}
	    	        	    			
	    			if ( mpoly != null ) {
	    				multiPolygons.add( mpoly );
	    			}
	    			
			    }
		    }
        }	    
      
	}
	    	 
	
	/**
	 * Parse a single bound string in format of "SSA|<AREA>HPC050|true"
	 * 
	 * @param bnds	 
	 */
	private void parseBounds() {
		
		String[] bndStr = bnds.split("\\|");
    	logger.debug( "bnds=" + bnds );
    	
    	inout = true;
    	
    	if ( bndStr != null ) {
			if ( bndStr.length > 0 ) {
			    boundsTableAlias = bndStr[0];
			}
		   	
			logger.debug( "boundsTableAlias=" +  boundsTableAlias);
			
			if ( bndStr.length > 1 ) {
				int  inds = bndStr[1].indexOf("<");
				int  inde = bndStr[1].indexOf(">");
				
				if ( inds >= 0 && inde >= 0 && inde > inds ) {
				    columnName = bndStr[1].substring(bndStr[1].indexOf("<") + 1, bndStr[1].indexOf(">"));
				    columnValue = bndStr[1].substring(bndStr[1].indexOf(">") + 1, bndStr[1].length());				
				}
			}
			
			logger.debug( "columnName=" +  columnName);			
			logger.debug( "columnvale=" +  columnValue);
			
			if ( bndStr.length > 2 ) {
				if ( bndStr[2].trim().charAt(0) != 'T' &&
					 bndStr[2].trim().charAt(0) != 't'	) {
					inout = false;
				}
			}
		}				
	}	
	
	/**
	 * Decides whether a point is inside of outside of  a set of polygons.
	 * 
	 * Note that "contains" method returns false if a point within a multi-point
	 *polygon.
	 * 
	 * @param p
	 * 
	 * @param polys
	 * 
	 * @return true = inside, false = outside
	 */
	public static boolean pointInPolygon( Point p, MultiPolygon polys ) {
		return !(polys.contains( p ));
	}
	
	/**
	 * Save polygons from a single bound into a Contours element - the label will be 
	 * "-9999". If flag "inout" is true, otherwise, it is "9999".
	 * 
	 * @return Contours
	 */
	public Contours getBoundsAsContours() {
	
	    Contours boundContours = new Contours();
	
	    int nn = 0, mm = 0;
	    for ( MultiPolygon mpoly : multiPolygons ) {
	    
		    String cvalue = "" + G2GCommon.RMISSD;
		    if ( !inout ) cvalue = "" + ( -G2GCommon.RMISSD );
        
		    ArrayList<Coordinate> linePts = new ArrayList<Coordinate>();
		    for ( int ii = 0; ii < mpoly.getNumGeometries(); ii++) {
		
		        Geometry g = mpoly.getGeometryN( ii );
		        Coordinate[] cg = g.getCoordinates();
            
		        for ( Coordinate pp : cg ) {
		    	    linePts.add( pp );
		        }
		    
		        ContourLine cline = new ContourLine( linePts, true, 
	                new String[]{ cvalue }, 1 ); 
            
	            cline.setParent( boundContours ); 
	            cline.getLine().setColors( new Color[]{ Color.green } );
	            cline.getLine().setLineWidth( 4 );
	            cline.getLine().setSmoothFactor( 0 );
	            boundContours.add( cline );
            
	            linePts.clear();
	        
	            mm++;
		    }
		
	        nn++;
	    }
	
	    logger.debug( "# of bounds MultiPolygon found: " + nn );
	    logger.debug( "# of bound Polygon found: " + mm );
        
	    return boundContours;
	}
	
	/**
	 * Retrieve polygons for multiple bounds and return as a list of BoundPolygon.
	 * The input should be in format of "bnd1+bnd2+...", where each single bound should
	 * be in format such as "STATEBNDS|<STATE>CO|true"
	 * 
	 * @return ArrayList<BoundPolygon>
	 */
	public static ArrayList<BoundPolygon> getAllBounds( String bndStr ) {
	    
		ArrayList<BoundPolygon> allBounds = new ArrayList<BoundPolygon>();
		
		String[] bndArray = bndStr.split("\\+");
		logger.debug( "Total # of bounds"+ bndArray.length );	
		
		for ( String str : bndArray ) {	
			if ( str.trim().length() > 0 ) {
			    BoundPolygon one = new BoundPolygon( str );
			    if  ( one.getBoundPolygons().size() > 0 ) {
				    allBounds.add( one );				
			    }	
			}
		}
							        
	    return allBounds;
	}

}

