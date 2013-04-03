/*
 * gov.noaa.nws.ncep.ui.pgen.attrDialog.LineAttrDlg
 * 
 * July 2010
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.gfa;

import gov.noaa.nws.ncep.common.staticdata.CostalWater;
import gov.noaa.nws.ncep.common.staticdata.FAArea;
import gov.noaa.nws.ncep.common.staticdata.FARegion;
import gov.noaa.nws.ncep.common.staticdata.GreatLake;
import gov.noaa.nws.ncep.common.staticdata.USState;
import gov.noaa.nws.ncep.ui.pgen.PgenStaticDataProvider;
import gov.noaa.nws.ncep.ui.pgen.PgenUtil;
import gov.noaa.nws.ncep.ui.pgen.display.FillPatternList.FillPattern;
import gov.noaa.nws.ncep.ui.pgen.display.IText.DisplayType;
import gov.noaa.nws.ncep.ui.pgen.display.IText.FontStyle;
import gov.noaa.nws.ncep.ui.pgen.display.IText.TextJustification;
import gov.noaa.nws.ncep.ui.pgen.display.IText.TextRotation;
import gov.noaa.nws.ncep.ui.pgen.elements.Layer;
import gov.noaa.nws.ncep.ui.pgen.elements.Line;
import gov.noaa.nws.ncep.ui.pgen.elements.Product;
import gov.noaa.nws.ncep.ui.pgen.elements.ProductInfo;
import gov.noaa.nws.ncep.ui.pgen.elements.ProductTime;
import gov.noaa.nws.ncep.ui.pgen.elements.Text;
import gov.noaa.nws.ncep.ui.pgen.file.FileTools;
import gov.noaa.nws.ncep.ui.pgen.file.ProductConverter;
import gov.noaa.nws.ncep.ui.pgen.file.Products;
import gov.noaa.nws.ncep.viz.common.SnapUtil.SnapVOR;

import java.awt.Color;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;

//import org.apache.log4j.Logger;
import org.dom4j.Document;
import org.dom4j.Node;
import org.dom4j.io.SAXReader;

import com.raytheon.uf.viz.core.exception.VizException;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.CoordinateList;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryCollection;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.LinearRing;
import com.vividsolutions.jts.geom.MultiPolygon;
import com.vividsolutions.jts.geom.Point;
import com.vividsolutions.jts.geom.Polygon;
import com.vividsolutions.jts.geom.impl.CoordinateArraySequence;

/**
 * GFA clipping functionality.
 * 
 * Note: See in-method documentation for algorithms used.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 07/10		#223		M.Laryukhin	Initial creation
 * 02/11					J. Wu		Do not throw exception for invalid 
 * 										bounds (the Great Lakes) for now.
 * 02/11					J. Wu		Added airmet coastal water bounds 
 * 04/11					J. Wu		Implemented basic FA regional clip.
 * 04/11					J. Wu		Load MT_OBSC table.
 * 05/11					J. Wu		Find/snap intersection points with
 * 										international bounds/common regional 
 *  									border, mark those points and all bound 
 *  									points non-reduce-able - to keep boundary 
 *  									intact. Also remove residue points caused
 *  									by clipping a polygon followed by a union 
 *  									of the clipped parts.
 * 05/11					J. Wu		Exclude null and non-GFA-related States..
 * 06/11					J. Wu		Temporarily hard-coded in fixStateBounds()
 * 										to fix known errors in ID/VT/FL/WA/OH
 * 07/11        #450        G. Hull     NcPathManager
 * 02/12        #597        S. Gurung   Moved snap functionalities to SnapUtil from SigmetInfo. 
 * 10/12					J. Wu		Fix a special case in getRegionIntersectionPt().
 * 
 * </pre>
 * 
 * @author M.Laryukhin
 * @version 1
 */
public class GfaClip { 

	private static GfaClip instance = new GfaClip();
	
//	private final static Logger logger = Logger.getLogger( GfaClip.class );

	/** Factory */
	private static GeometryFactory geometryFactory;

	/** Database name, currently defaulted to "ncep". */
	public final String DATABASE = "ncep";

	/** Schema name, currently defaulted to "bounds". */
	public final String SCHEMA = "bounds";

	public final String FA_REGION_TABLE = "fa_region";
	public final String FA_AREA_TABLE = "fa_area";
	public final String STATE_BNDS_TABLE = "statebnds";
	public final String GREATE_LAKE_BNDS_TABLE = "greatlakesbnds";
	public final String COASTAL_WATER_BNDS_TABLE = "airmetcstlbnds";
	public final String FA_AREAX_TABLE = "fa_areax";
	
	/**
	 * The states listed in Raytheon's database but we do not need for GFA.
	 * Note: the first entry in Reytheon's state list is a "null" one
	 *       which is part of Canada.
	 */
	private final String[] EXCLUDE_STATES = new String[]{ "AK", "HI", "UM", "GU", "AS", "PR", "VI" };
//	private final String[] WRONG_STATES = new String[] {"VT", "ID", "OH", "FL", "WA", "AK", "HI", "UM", "GU", "AS", "PR", "VI"};
//	private final String[] WRONG_STATES = new String[] {"VT", "ID", "OH", "FL", "WA"}; //OB11.5
	private final String[] WRONG_STATES = new String[] {"HI","VI","ME","VA","MI","MD","MA","AS","AR",
			 "IL","MN", "MS","NJ","PR","AK","AL","TX","NC","ND","NY","OK","OH", 
			 "FL","SD","SC","WI","LA","GU","WA" }; //OB11.7
	
	/**
	 * The union of three FA region bound shapes.
	 */
	private Geometry faInternationalBound;
	private Geometry faInternationalBoundInGrid;
	
	/**
	 * The list of region bound shapes to clip against.
	 */
	private HashMap<String, Geometry> faRegionBounds;
	private HashMap<String, Geometry> faRegionBoundsInGrid;

	/**
	 * The list of area bound shapes to clip against.
	 */
	private HashMap<String, Geometry> faAreaBounds;
	private HashMap<String, Geometry> faAreaBoundsInGrid;

	/**
	 * The list of extended area bound shapes to clip against.
	 */
	private HashMap<String, Geometry> faAreaXBounds;
	private HashMap<String, Geometry> faAreaXBoundsInGrid;

	/**
	 * The list of state bound shapes to clip against.
	 */
	private HashMap<String, Geometry> stateBounds;
	private HashMap<String, Geometry> stateBoundsInGrid;

	/**
	 * The list of great lake shapes to clip against.
	 */
	private HashMap<String, Geometry> greatLakesBounds;
	private HashMap<String, Geometry> greatLakesBoundsInGrid;
	
	/**
	 * The list of great lake shapes to clip against.
	 */
	private HashMap<String, Geometry> coastalWaterBounds;
	private HashMap<String, Geometry> coastalWaterBoundsInGrid;
	
	/**
	 * The list of region bound shapes to clip against.
	 */
	private HashMap<String, Geometry> faRegionCommBounds;
	private HashMap<String, Geometry> faRegionCommBoundsInGrid;

	/**
	 * The list of region bound shapes to clip against.
	 */
	private HashMap<String, Geometry> faAreaXCommBounds;
	private HashMap<String, Geometry> faAreaXCommBoundsInGrid;
	
	
	/** State list that allows MT_OBSC */
	private static Document mtObscTbl = null;
	
	/** XPATH for states in document */
	public static String MTOBSC_XPATH = "/MT_OBSC";
	
	/** State list that allows MT_OBSC */
	private static List<String> mtObscStates = null;
	
	/** State list that allows MT_OBSC */
	private static final int NEW_POINT = 2;
	private static final int BOUND_POINT = 1;
	private static final int ORIGINAL_POINT =  0;
	
	/** Tie distance for two points in map coord */
	private static final double SMALLF = (0.01);	
	
	
	/**
	 * Private constructor, we only need to read the database once.
	 */
	private GfaClip() {
		geometryFactory = new GeometryFactory();
	}

	/**
	 * Singleton instance.
	 * 
	 * @return
	 */
	public static GfaClip getInstance() {
		return instance;
	}

	/**
	 * Clips to regions.
	 * 
	 * @param smear
	 * @return
	 */
	public ArrayList<Gfa> simpleclip(Gfa smear) {
		
//		logger.debug("clipping started");
		
		ArrayList<Gfa> list = new ArrayList<Gfa>();
		for (String key : getFaRegionBounds().keySet()) {
			Geometry region = faRegionBounds.get(key);

			Polygon polygon = gfaToPolygon(smear);

			Geometry intersection = null;
			if (region.intersects(polygon)) {
				intersection = region.intersection(polygon);
				if (intersection instanceof MultiPolygon) {
					MultiPolygon mp = (MultiPolygon) intersection;
					for (int i = 0; i < mp.getNumGeometries(); i++) {
						Gfa g = geometryToGfa(smear, mp.getGeometryN(i));
						g.addNotToBeSnapped(region.getCoordinates());
						list.add(g);
					}
				} else {
					Gfa g = geometryToGfa(smear, intersection);
					g.addNotToBeSnapped(region.getCoordinates());
					list.add(g);
				}
			} else if (region.covers(polygon)) {
				smear.addNotToBeSnapped(region.getCoordinates());
				list.add(smear);
			}
		}
		
		return list;
	}

	
	/**
	 *  Generate a JTS polygon from a GFA polygon.
	 * 
	 * @param gfa	a GFA element
	 * @return
	 */
	public Polygon gfaToPolygon( Gfa gfa ) {
		if ( gfa != null ) {		
		    return pointsToPolygon( gfa.getLinePoints() );
		}
		else {
			return null;
		}		
	}

	/**
	 *  Generate a JTS polygon in grid coordinate from a GFA polygon.
	 * 
	 * @param gfa	a GFA element
	 * @return
	 */
	public Polygon gfaToPolygonInGrid( Gfa gfa ) {
		if ( gfa != null ) {		
		    return pointsToPolygon( PgenUtil.latlonToGrid( gfa.getLinePoints() ) );
		}
		else {
			return null;
		}		
	}

	/**
     * Generate a JTS polygon from a set of points.
     * 
     * It is assumed that the first point is not repeated at the end
     * in the input array of point.
     * 
     * @param points	array of points
     * @return
     */
	public Polygon pointsToPolygon( Coordinate[] points ) {
		
		Coordinate[] coords = Arrays.copyOf( points, points.length + 1 );
		coords[coords.length - 1] = coords[0];

		CoordinateArraySequence cas = new CoordinateArraySequence( coords );
		LinearRing ring = new LinearRing( cas, geometryFactory );	

		Polygon polygon = new Polygon( ring, null, geometryFactory );
		
		return polygon;
	}

    /**
     * Generate a JTS polygon from a set of points.
     * 
     * @param points	array of points
     * @return
     */
	public Geometry pointsToGeometry( Coordinate[] points ) {
		
		Geometry geom;
		if ( points == null || points.length == 0 ) {
			geom = null;
		}
		else if ( points.length == 1 ) {
			CoordinateArraySequence cas = new CoordinateArraySequence( points );
			geom = new Point( cas, geometryFactory ) ;			
		}
		else if ( points.length == 2 ) {			
		    geom = pointsToLineString( points );
		}
		else {
			geom = pointsToPolygon( points );
		}
		
		return geom;
	}	

	/**
     * Generate a JTS polygon from a set of points.
     * 
     * @param pots	list of points
     * @return
     */
	public Geometry pointsToGeometry( ArrayList<Coordinate> pts ) {
		
		Coordinate[] points = new Coordinate[ pts.size() ];
		pts.toArray( points );
		
		return pointsToGeometry( points );
	}	
	
	
    /**
     * Generate a JTS LineString from a set of points.
     * 
     * @param points	array of points
     * @return
     */
	public Geometry pointsToLineString( Coordinate[] points ) {
		
		CoordinateArraySequence cas = new CoordinateArraySequence( points );
		
		return new LineString( cas, geometryFactory );
	}	
	
	/**
	 * Create a new GFA from an input GFA with points in a given Geometry.
	 * 
	 * Note: the input geometry should not have holes in it.
	 * 
	 * @param gfaIn
	 * @param geomIn
	 * @return
	 */
	public Gfa geometryToGfa( Gfa gfaIn, Geometry geomIn ) {
		
		Coordinate[] c = geomIn.getCoordinates();
		ArrayList<Coordinate> coor = new ArrayList<Coordinate>();
		
		coor.addAll( Arrays.asList( c ) );
		coor.remove( coor.size() - 1 ); // remove last
		
		Gfa g = gfaIn.copy();
				
		g.setPoints( coor );		
		g.setGfaTextCoordinate( g.getCentroid() );
		
		return g;
	}

	/**
	 * Creates the bounds polygon.
	 * 
	 * @throws VizException
	 */
	private void readFaRegionBounds() throws VizException {
		faRegionBounds = new  HashMap<String, Geometry>();
		for ( FARegion fa : PgenStaticDataProvider.getProvider().getFARegions()){
			faRegionBounds.put(fa.getRegion(), fa.getGeometry());
		}
		loadFaRegionCommBounds();
	}
	
	/**
	 * Creates the FA area bounds polygon.
	 * 
	 * @throws VizException
	 */
	private void readFaAreaBounds() throws VizException {
		faAreaBounds = new  HashMap<String, Geometry>();
		for ( FAArea fa : PgenStaticDataProvider.getProvider().getFAAreas()){
			faAreaBounds.put(fa.getArea(), fa.getGeometry());
		}
	}
	
	/**
	 * Creates the extended FA area bounds polygon.
	 * 
	 * @throws VizException
	 */
	private void readFaAreaXBounds() throws VizException {
		faAreaXBounds = new  HashMap<String, Geometry>();
		for ( FAArea fa : PgenStaticDataProvider.getProvider().getFAAreaX()){
			faAreaXBounds.put(fa.getArea(), fa.getGeometry());
		}
	}
	
	/**
	 * Creates the state bounds polygon - only include States used for GFA.
	 * 
	 * @throws VizException
	 */
	private void readStateBounds() throws VizException {
		// TODO change to use SCHEMA, DATABASE, and STATE_BNDS_TABLE
//		String sql = "select t.state, AsBinary(t.the_geom) from mapdata.states t";
//		String sql = "select t.state, AsBinary(t.the_geom_0) from mapdata.states t";
//		String sql = "select t.state, AsBinary(t.the_geom_0_001) from mapdata.states t";
//		String sql = "select t.state, AsBinary(t.the_geom_0_004) from mapdata.states t";
//		String sql = "select t.state, AsBinary(t.the_geom_0_016) from mapdata.states t";
//		String sql = "select t.state, AsBinary(t.the_geom_0_064) from mapdata.states t";
		
//		long l1 = System.currentTimeMillis();
//		stateBounds = readBounds( "maps", sql );
//		System.out.println("Time to load state bounds =  " + (System.currentTimeMillis() - l1 ) );

		
		HashMap<String, Geometry>  originalStateBounds = new HashMap<String, Geometry>();
		
		for ( USState st : PgenStaticDataProvider.getProvider().getAllstates()){
			originalStateBounds.put(st.getStateAbrv(), st.getShape());
		}
		
		//Exclude a few states.       
		for ( String s : EXCLUDE_STATES ) {
			 originalStateBounds.remove( s );
	    }
		
		stateBounds = fixStateBounds( originalStateBounds );
		
	}
	
	/**
	 * Creates the great Lakes bounds polygon.
	 * 
	 * @throws VizException
	 */
	private void readGreatLakeBounds() throws VizException {
		greatLakesBounds = new  HashMap<String, Geometry>();
		for ( GreatLake lake : PgenStaticDataProvider.getProvider().getGreatLakes()){
			greatLakesBounds.put(lake.getId(), lake.getGeometry());
		}
	}
	
	/**
	 * Creates the coastal water bounds polygon.
	 * 
	 * @throws VizException
	 */
	private void readCoastalWaterBounds() throws VizException {
		coastalWaterBounds = new  HashMap<String, Geometry>();
		for ( CostalWater water : PgenStaticDataProvider.getProvider().getCostalWaters()){
			coastalWaterBounds.put(water.getId(), water.getGeometry());
		}
	}

	/**
	 * Load FA Region bounds into a HashMap (West, Central, East)
	 * @return	
	 */
	public HashMap<String, Geometry> getFaRegionBounds() {
		if ( faRegionBounds == null ){
			try {
				readFaRegionBounds();
			} catch ( VizException  e) {
//				logger.error("Error ", e);
				e.printStackTrace();
			}
		}
		
		return faRegionBounds;
	}

	/**
	 * Load FA Area bounds into a HashMap (SLC, SFO, CHI, DFW, BOS, MIA)
	 * @return
	 */
	public HashMap<String, Geometry> getFaAreaBounds() {
		if ( faAreaBounds == null ) {
			try {
				readFaAreaBounds();
			} catch ( VizException e ) {
//				logger.error("Error ", e);
				e.printStackTrace();
			}
		}
		
		return faAreaBounds;
	}

	/**
	 * Load extended FA Area bounds into a HashMap (SLC, SFO, CHI, DFW, BOS, MIA)
	 * @return
	 */
	public HashMap<String, Geometry> getFaAreaXBounds() {
		if ( faAreaXBounds == null ) {
			try {
				readFaAreaXBounds();
			} catch ( VizException e ) {
//				logger.error("Error ", e);
				e.printStackTrace();
			}
		}

		return faAreaXBounds;
	}

	/**
	 * Load state bounds into a HashMap keyed by state name
	 * 
	 * @return
	 */
	public HashMap<String, Geometry> getStateBounds() {
		if ( stateBounds == null ) {
			try {
				readStateBounds();
			} catch ( VizException e ) {
//				logger.error("Error ", e);
				e.printStackTrace();
			}
		}
		
		return stateBounds;
	}
    
	/**
	 *  Load Great Lake bounds into a HashMap
	 * @return
	 */
	public HashMap<String, Geometry> getGreatLakeBounds() {
		if ( greatLakesBounds == null ) {
			try {
				readGreatLakeBounds();
			} catch ( VizException e) {
//				logger.error("Error ", e);
				e.printStackTrace();
			}
		}
		
		return greatLakesBounds;
	}
	
	/**
	 *  Load Coastal Water bounds into a HashMap
	 * @return
	 */
	public HashMap<String, Geometry> getCoastalWaterBounds() {
		if ( coastalWaterBounds == null ) {
			try {
				readCoastalWaterBounds();
			} catch ( VizException e ) {
//				logger.error("Error ", e);
				e.printStackTrace();
			}
		}
		
		return coastalWaterBounds;
	}
	
	/**
	 * Create a union of three FA Region bounds (international bound)
	 * @return	
	 */
	public Geometry getFaInternationalBound() {
		
		if ( faInternationalBound == null ) {			
			ArrayList<Geometry> rbnds = new ArrayList<Geometry>( getFaRegionBounds().values() );
			faInternationalBound = quickUnion( rbnds );
		}

		return faInternationalBound;
	}
	
	/**
	 * Create a union of a list of Geometries.
	 * 
	 * This method uses GeometryCollection's buffer method to union
	 * a collection of Geometries, which is believed to be faster than
	 * using Geometry's union method to union geometries one by one,
	 * according to JTS documentation.
	 * 
	 * @param	geoms	A list of geometries	
	 * @return	
	 */
	public Geometry quickUnion( ArrayList<Geometry> geoms ) {
		
		if ( geoms.size() <= 0 ) return null;
		
	    Geometry[] regs = new Geometry[ geoms.size() ];
	    regs = geoms.toArray( regs );
			
		GeometryCollection gc = new GeometryCollection( regs, geometryFactory );
		
		return gc.buffer( 0.0 );
			
	}

	
	/**
	 * Clip to FA regions.
	 * 
     * This routine clips a non-FZLVL GFA airmet/outlook against FA region 
     * boundaries to generates a list of clipped smears.
     * 
     * The algorithm is as following:
     * 
     * 1. Check if the smear is big enough ( >= 3K sq. nautical miles )
     * 2. Clip against the international boundary and check if the intersection is big enough
     * 3. Clip against FA region boundaries - 
	 *  	3.1 clip the smear polygon against international boundary.
	 *      3.2 clip the result against each FA region (WEST, CENTRAL, EAST)
	 *      3.3 create a list of parts bigger than/equal to 3K for each region
	 *      3.4 hold the parts smaller than 3K for all regions in another list.         
 	 * 4. Create smears from VALID parts and add into the return list
	 *      4.1 Union the parts in each FA region with the list of small parts -
	 *          If a small part touches one of the bigs (only one, I believe), it
	 *          will be united into that big and thus be included into a new
	 *          smear.  So  no part gets lost and all will be included eventually.
	 *      4.2 Check the resulting union for VALID geometries and create new
	 *          smears for each of them.	    			
     *
     *	    A VALID geometry is one that  
     *        a. is large enough ( >= 3K sq. nautical miles )
	 *        b. intersects at least one of its associated snapshots (from 
	 *           which the smear polygon is generated) with an area >= 3K.
	 *        c. intersects snapshot "6" with an area >= 3K, if it is a "6-6" smear
	 *           and there are snapshots associated with it.
	 * 
	 * @param smear		the GFA smear to be clipped
	 * @param snapshots	snapshots from where the input smear is created
	 * @return 			a list of clipped GFA smears.
	 */
	public ArrayList<Gfa> clipFARegions( Gfa smear, ArrayList<Gfa> snapshots ) {

		ArrayList<Gfa> clippedList = new ArrayList<Gfa>();
		if ( smear.getGfaHazard().equals( "FZLVL" ) ) {
			return clippedList;
		}
		
		/*
		 *  If the smear size < AREA_LIMIT, stop.
		 */
		if ( !isBiggerInMap( gfaToPolygon( smear ) ) ) {
		    return clippedList;
		}
										
	    /*
	     * Do international clipping (in grid coordinate)
	     */		
		boolean _clippingFlg = true;		
		
		Geometry intlBound = getFaInternationalBound();
		Geometry intlBoundInGrid = getFaInternationalBoundInGrid() ;
		
		Polygon smearPoly = gfaToPolygon( smear );
		Polygon smearPolyInGrid = pointsToPolygon( PgenUtil.latlonToGrid( smear.getLinePoints() ) );
		
		Geometry  clipAgstIntlBnd = null;
				
		if ( smearPolyInGrid.intersects( intlBoundInGrid ) ) {
			clipAgstIntlBnd = smearPolyInGrid.intersection( intlBoundInGrid );	
		}

		if ( clipAgstIntlBnd == null || clipAgstIntlBnd.getNumGeometries() <= 0 ||
			 !isBiggerInGrid( clipAgstIntlBnd ) ) {
			return clippedList;
		}
				
       
	    /*
	     * Find intersection points with the international bound and the common borders
	     * between West and Central, Central and East regions.  Then insert and snap them
	     * for later replacement.
	     */		
		HashMap<Coordinate, Coordinate> intlPts = getIntlIntersectionPt( smearPoly, intlBound, clipAgstIntlBnd );
        
		HashMap<Coordinate, Coordinate> regionInterPts = getRegionIntersectionPt( smearPoly );

		/*
		 * Build a list of non-reduce-able point, include all bound points and snapped
		 * intersection points
		 */		
		HashMap<String, Geometry> rgbnds = getFaRegionBounds();				
		HashMap<String, Geometry> rgbndsInGrid = getFaRegionBoundsInGrid();				

		CoordinateList nonReduceable = new CoordinateList();
		nonReduceable.addAll( intlPts.values(), false );
		nonReduceable.addAll( regionInterPts.values(), false );
		for ( Geometry gb : rgbnds.values() ) {
			nonReduceable.addAll( Arrays.asList( gb.getCoordinates() ), false );
		}
		        
		
		//Build a map of all replaceable points
		HashMap<Coordinate, Coordinate> replacePts = new HashMap<Coordinate, Coordinate>();
		replacePts.putAll( intlPts );				
		replacePts.putAll( regionInterPts );
       
		/*
		 *  Regional clipping as following:
		 *  
		 *  1. clip the smear polygon against international boundary.
		 *  2. clip the result against each FA region (WEST, CENTRAL, EAST)
		 *  3. create a list of parts bigger than/equal to 3K for each region
		 *  4. hold the parts smaller than 3K for all regions in another list.         
		 */
        HashMap<String, ArrayList<Geometry> > clipWithRegions = new HashMap<String, ArrayList<Geometry> >();                       
        
        ArrayList<Geometry> smallPoly = new ArrayList<Geometry>();                     
        
		for ( String regionName : rgbnds.keySet() ) {
            			
			clipWithRegions.put( regionName, new ArrayList<Geometry>() );
			
			// Always start from the smear clipped within international bound
			Geometry startPoly = intlBoundInGrid.intersection( smearPolyInGrid );
		    	
		    //Do regional clipping as described above.
			if ( _clippingFlg ) {
				 
				 Geometry regionBnd = rgbndsInGrid.get( regionName );				 

				 if ( regionBnd.intersects( startPoly ) ) {
				     Geometry regionPoly = regionBnd.intersection( startPoly );	
						
				     if ( regionPoly != null ) {					        							

				    	 for ( int kk = 0; kk < regionPoly.getNumGeometries(); kk++ )  {					    	    
							Geometry bigPoly = regionPoly.getGeometryN( kk );					    	        
								 
					        if ( isBiggerInGrid( bigPoly ) )  {					        	
					        	clipWithRegions.get( regionName ).add( bigPoly ); 
				            }
					        else {
					        	smallPoly.add( bigPoly ); 
					        }
						}
				    }                
				}	   
			}
		}								
					    
		/*
		 *  Create smears from VALID parts and add into the return list
		 *  
		 *  1. Union the parts in each FA region with the list of small parts -
		 *     If a small part touches one of the bigs (only one, I believe), it
		 *     will be united into that big and thus be included into a new
		 *     smear.  So  no part gets lost and all will be included eventually.
		 *  2. When a small part is united into a big one (if they intersect), 
		 *     the common points are still retained in the result, we need to 
		 *     remove those extra point. The common points are resulted from the
		 *     previous clipping with the regional boundaries.
		 *  3. check the resulting union for VALID geometries and create new
		 *     smears for each of them.	    			
		 */
		for ( String regionName : clipWithRegions.keySet() ) {
        				
			ArrayList<Geometry>  bigs = clipWithRegions.get( regionName );
						
			ArrayList<Geometry> toBeUnioned = new ArrayList<Geometry>();
			toBeUnioned.addAll( bigs );
			toBeUnioned.addAll( smallPoly );
			Geometry result = quickUnion( toBeUnioned );           			
			
			ArrayList<Coordinate> commPts = getCommonPoints( bigs, smallPoly );
			
			if ( result != null ) {		    
		        for ( int kk = 0; kk < result.getNumGeometries(); kk++ ) {
	                
					Geometry one = result.getGeometryN( kk );
									
					/* 
					 * Remove residue points due to clipping followed by subsequent union.
					 * Done in grid coordinate.
					 */
					Geometry onePart = removeCommonPoints( one, commPts );
					
					/*
					 * Clean up some improper cases - done in MAP coordinate.
					 */	
					Coordinate[] gPts = PgenUtil.gridToLatlon( onePart.getCoordinates() );
					ArrayList<Coordinate> points = new ArrayList<Coordinate>();
					for ( Coordinate c : gPts ) {
						points.add( c );
					}
					points.remove( points.size() - 1 );
															
					Geometry cleanPts = cleanupPoints( pointsToGeometry( points ) );
					
					if ( isAddableAsSmear( cleanPts, snapshots, smear ) ) {
						
						//Replace intersection points with their pre-snapped pair - map
						Geometry rplPts = replacePts( cleanPts, replacePts );
						
						//Clean up duplicate point from point replacement - map
						Geometry finalPts = cleanupPoints( rplPts );

						if ( finalPts != null ) {
							//Create a new GFA smear.
							Gfa newElm = geometryToGfa( smear, finalPts );

							//Mark non-reduce-able points.
							addReduceFlags( newElm, nonReduceable );
							
							newElm.addAttribute( "FA_REGION", new String(regionName) );

							clippedList.add( newElm );
						}
				    }
				}
			}
											
		} 
		
		return clippedList;
	}
	
	
	/**
	 *  Check if a polygon is valid to be added as a new smear:
	 *  
	 *  Note: the input geometry is assumed to be in map coordinate.
	 *  
	 *  1. it must be large enough ( >= 3K sq. nautical miles )
	 *  2. it must intersect at least one of its associated snapshots (from 
	 *     which the smear polygon is generated) with an area >= 3K.
	 *  3. if it is "6-6" smear, it must intersect snapshot "6" with an 
	 *     area >= 3K.
	 *  
	 * @param	g			A Geometry (polygon) in map coordinate.
	 * @param	snapshots	A list of associated snapshots
	 * @param	smear		Original GFA smear
	 * @return	
	 */
	public boolean isAddableAsSmear( Geometry g,  ArrayList<Gfa> snapshots, Gfa smear ) {		
		
		return ( isBiggerInMap( g ) 					&& 
				 passed6_6Rule( g, snapshots, smear )	&&
				 intersectBigWithSS( g, snapshots ) ) ;
	
	}
	
	
	/**
	 *  Check if a polygon intersects at least one of the snapshots with an area >= 3K.
	 *  
	 *  Note: the input is assumed in MAP coordinate.
	 *  
	 * @param	g			A Geometry (polygon)
	 * @param	snapshots	A list of associated snapshots
	 * @return	
	 */
	private boolean intersectBigWithSS( Geometry g,  ArrayList<Gfa> snapshots ) {

		boolean addable = false;
		
		if ( snapshots == null || snapshots.size() <= 0 ) {
			addable = true;
		}
		else {
			Coordinate[] pts = PgenUtil.latlonToGrid( g.getCoordinates() );
			Geometry poly = pointsToGeometry( pts );
			for ( Gfa ss : snapshots ) {        		
				if ( isBiggerInGrid( poly.intersection( gfaToPolygonInGrid( ss ) ) ) )  {
					addable = true;
					break;
				}
			}
		}

		return addable;

	}
	
	/**
	 *  Check if a 6-6 polygon intersect snapshot "6" with an area >= 3K.
	 *  
	 *  Note: if the smear is not a "6-6" smear or no snapshots, return true.
	 *        if there are snapshots but no snapshot "6" somehow, return false;
	 *  
	 * @param	g			A Geometry (polygon)
	 * @param	snapshots	A list of associated snapshots
	 * @return	
	 */
	private boolean passed6_6Rule ( Geometry g, ArrayList<Gfa> snapshots, Gfa smear ) {
        
		boolean addable = true;
		
		if ( snapshots != null && snapshots.size() > 0 && 
			 smear.getForecastHours().equals( "6-6" ) ) {
			
			Gfa ss_6 = null;

			for ( Gfa ss : snapshots ) {        		                
				if ( ss.getForecastHours().equals( "6" ) ) {
                    ss_6 = ss;
                    break;
				}
			}
			
			if ( ss_6 == null )  {
				addable = false;
			}			
			
			if ( addable ) {
				Coordinate[] pts = PgenUtil.latlonToGrid( g.getCoordinates() );
				Geometry poly = pointsToGeometry( pts );
		        if ( !isBiggerInGrid( poly.intersection( gfaToPolygonInGrid( ss_6 ) ) ) ) {
		        	addable = false;
		        }
			}

		}

		return addable;

	}

	/**
	 *  Check if a polygon is bigger than a value in unit of sq. nautical miles.
	 * @param	g	A Geometry
	 * @return	
	 */
	private boolean isBigger( Geometry g, double limit ) {				
		return ( g != null && PgenUtil.getSphPolyArea( g ) >= limit );	
	}

	/**
	 *  Check if a polygon is bigger than the GFA AREA_LIMIT (3K sq. nautical miles).
	 *  
	 * @param	g	A Geometry
	 * @return	
	 */
	public boolean isBiggerInMap( Geometry g ) {
				
		return isBigger( g, GfaRules.AREA_LIMIT );
	
	}
	
	/**
	 *  Check if a polygon is bigger than the GFA AREA_LIMIT (3K sq. nautical miles).
	 *  
	 *  Note: the input is assumed to be in grid coordinate, not map coordinate.
	 *  
	 * @param	g	A Geometry
	 * @return	
	 */
	public boolean isBiggerInGrid( Geometry g ) {
				
		double area = 0.0;
		if ( g instanceof Polygon ) {
			area = PgenUtil.getSphPolyAreaInGrid( (Polygon)g );
		} else if ( g instanceof MultiPolygon ) {
			MultiPolygon mp = (MultiPolygon)g;
			for( int nn = 0; nn <  mp.getNumGeometries(); nn++ ) {
				area += PgenUtil.getSphPolyAreaInGrid( (Polygon)mp.getGeometryN( nn ) );
			}
		}
							
		return ( area >= GfaRules.AREA_LIMIT ) ? true : false;
	
	}
	

	/**
	 * Get a list of states that allows MT_OBSC hazard.
	 * @param
	 * @return
	 */
	public List<String> getMtObscStates(){ 
		
		if ( mtObscStates == null ) {
			mtObscStates = new ArrayList<String>();
		    String xpath = MTOBSC_XPATH;
		
		    Document dm = readMtObscTbl();
		
		    if ( dm != null ) {
		        Node mtObscInfo = dm.selectSingleNode(xpath);
		        List<Node> nodes = mtObscInfo.selectNodes( "area" );
		        for (Node node : nodes) {
		    	    node.selectNodes("state");
		    	    for ( Object nd : node.selectNodes("state") ) {
		    	    	mtObscStates.add( ((Node)nd).getText() );
		    	    }
		        }
		    }
		}
		
		return mtObscStates;
	}
	
	/**
	 * Read mt_obsc_states.xml
	 * @return - document
	 */
	private static Document readMtObscTbl() {
		
		if ( mtObscTbl == null) {
			try {
				String mtObscFile = PgenStaticDataProvider.getProvider().getFileAbsolutePath(
						PgenStaticDataProvider.getProvider().getPgenLocalizationRoot() + "mt_obsc_states.xml");

				SAXReader reader = new SAXReader();
				mtObscTbl = reader.read( mtObscFile );
			} catch ( Exception e ) {
				e.printStackTrace();
			}
		}
		
		return mtObscTbl;
	}
		
	
    /**
	 * This routine clips a GFA polygon against the international bound and
	 * finds the intersection points of the GFA polygon with the bound.
	 * Then the intersection points are snapped individually to points 
	 * outside of the Clipped GFA polygon.  Both the intersection points and
	 * their snapped matches are returned in map coordinate.
	 * 
	 * The input is assumed to be in MAP coordinate.
	 * 
	 *  @param	gfaPoly		Polygon from GFA 
	 *  @param	bndPoly		Polygon of international boundary
	 *  @param	clipped		Intersection Polygon of international boundary
	 *  					with the GFA polygon (in grid coordinate)
	 *  
     */
	private HashMap<Coordinate, Coordinate> getIntlIntersectionPt( Geometry gfaPoly, 
			 Geometry bndPoly, Geometry clipped )
	{
		HashMap<Coordinate, Coordinate>	interPts = new HashMap<Coordinate, Coordinate>();	
		
		//Get points in clockwise order - do not repeat the first point at the end.
		Coordinate[] pts = clipped.getGeometryN( 0 ).getCoordinates();		
		Coordinate[] intlPoly = PgenUtil.gridToLatlon( pts );	
		ArrayList<Coordinate> intlList = new ArrayList<Coordinate>();
		intlList.addAll( Arrays.asList( intlPoly ) );
		intlList.remove( intlList.size() - 1 );
		ArrayList<Coordinate> cwPoly = GfaSnap.getInstance().reorderInClockwise( intlList , null );

        /*
		 *   Check out each point as either a new point, an international boundary point,
		 *   or a point of the original polygon.
		 */    
        int[] ptFlag = new int[ cwPoly.size() ];
        for ( int ii = 0; ii < cwPoly.size(); ii++ ) {
    		       	
        	ptFlag[ ii ] = NEW_POINT;
        	
        	for ( Coordinate bndc : bndPoly.getCoordinates() ) {
        		if ( isSamePoint( bndc, cwPoly.get( ii ), SMALLF ) ) {
        		    ptFlag[ ii ] =  BOUND_POINT;
        		    break;
        		}
        	}
        	
        	if ( ptFlag[ ii ] != NEW_POINT  )  continue;
        	
        	for ( Coordinate orgc : gfaPoly.getCoordinates() ) {
        		if ( isSamePoint( orgc, cwPoly.get( ii ), SMALLF ) ) {
        		    ptFlag[ ii ] = ORIGINAL_POINT;
        		    break;
       		    }
        	}
        }

        //Shift the array to let it start with an original point.    
        int  eflag;
        while (  ptFlag[ 0 ] != ORIGINAL_POINT ) {            
        	
        	cwPoly = GfaSnap.getInstance().shiftArray( cwPoly );
         	
        	eflag = ptFlag[ 0 ];
        	for ( int jj = 0; jj < (ptFlag.length - 1); jj++ ) {
        		ptFlag[ jj ] = ptFlag[ jj + 1 ];
        	}
       	
         	ptFlag[ ptFlag.length - 1 ] = eflag;
         	
         }	
        
        
        //Snap and match any new points.  
       	int snap_indx1;
    	int snap_indx2;
    	int status;
    	int np = cwPoly.size();
    	Coordinate[]  snapPt = new Coordinate[1];
		
    	ArrayList<Coordinate> usedPts = new ArrayList<Coordinate>();
		usedPts.addAll( cwPoly );
   	
        for ( int ii = 0; ii < cwPoly.size(); ii++ ) {
   		                   
            if ( ptFlag[ ii ] != NEW_POINT ) {
        	    continue;
        	}
            
            /*
             * Handle NN case - a "small" triangle may be cut off when the smear intersects
             * the international boundary. The two new close points by the intersection 
             * should be replaced by the original point in the smear.
             */           
            if ( (ii + 1) < cwPoly.size() ) {
            	if ( ptFlag[ ii + 1 ] == NEW_POINT &&
            		 GfaSnap.getInstance().isCluster( cwPoly.get( ii ), cwPoly.get( ii + 1 ) ) ) {
            		
            		int ptBefore = (ii - 1 + cwPoly.size() ) % cwPoly.size();
               		int ptAfter = (ii + 2 + cwPoly.size() ) % cwPoly.size();
               		
               		List<Coordinate> gfaPts = Arrays.asList( gfaPoly.getCoordinates() );
               		int indInGfa1 = findPoint( cwPoly.get( ptBefore ), gfaPts, SMALLF );
               		int indInGfa2 = findPoint( cwPoly.get( ptAfter ), gfaPts, SMALLF );
               		
               		if ( indInGfa1 >= 0 &&  indInGfa2 >= 0 && 
               			 (indInGfa2 -  indInGfa1) == 2 ) {
               			
               			 int indBtw = indInGfa1 + 1;   //the original point between       			
               			 if ( GfaSnap.getInstance().isCluster( cwPoly.get( ii ), gfaPts.get( indBtw ) ) && 
               				  GfaSnap.getInstance().isCluster( cwPoly.get( ii + 1 ), gfaPts.get( indBtw ) ) ) {

               				 interPts.put( cwPoly.get( ii ) , gfaPts.get( indBtw ) );      			
               				 interPts.put( cwPoly.get( ii + 1 ) , gfaPts.get( indBtw ) );      			

               				 ii++; //skip these two new points.
               				 continue;
               			 }
               		}           		
            	}
            }
                    	
        	/*
        	 *  All other cases - snap the intersection point to a point with a 
        	 *  clustering distance away and outside the polygon.  For clustering 
        	 *  points, snap them to a single snap point.  If fail to snap a cluster
        	 *  of points, try to snap them individually.
        	 *  
        	 */
        	snap_indx1 = ii;
        	snap_indx2 = ii;
        	
        	while ( snap_indx1 > 0 && 
                    GfaSnap.getInstance().isCluster( cwPoly.get( ii ), 
                		                             cwPoly.get( snap_indx1-1 ) ) ) {       	                
        	    snap_indx1--;
            }
        		    
        	while ( snap_indx2 < ( np - 1 ) && 
        			GfaSnap.getInstance().isCluster( cwPoly.get( ii ),
        					cwPoly.get( snap_indx2+1 ) ) ) {       	                
        		snap_indx2++;
        	}

        	status = GfaSnap.getInstance().snapPtGFA( snap_indx1, snap_indx2, 
        			              usedPts, null, cwPoly, true, true, 3.0F, snapPt );        		
			

        	if ( status != 0 && ( snap_indx1 != snap_indx2 ) ) {

        		snap_indx1 = ii;
        		snap_indx2 = ii;

        		status = GfaSnap.getInstance().snapPtGFA( snap_indx1, snap_indx2, 
        				          usedPts, null, cwPoly, true, true, 3.0F, snapPt );
       	    } 
            
        	// Snap failed - use the original point.
        	if ( status != 0 ) {
        		snapPt[ 0 ].x = cwPoly.get( ii ).x;
        		snapPt[ 0 ].y = cwPoly.get( ii ).y;
        	}	    
    		
        	/*
        	 *  Pair the intersection point with its snapped counterpart.
        	 *  Also replace the intersection points with the snapped ones
        	 *  for snapping the next one.
        	 */
       		for ( int jj = snap_indx1;  jj <= snap_indx2; jj++ ) {
        		interPts.put( cwPoly.get( jj ), snapPt[ 0 ] );      			        		
//        		cwPoly.set( jj, new Coordinate( snapPt[ 0 ] ) );;    			      	
        	}
       		
       		ii += ( snap_indx2 - snap_indx1 ); //added to skip duplicate calculation.
        	       	
        	usedPts.add( snapPt[ 0 ] );
       
        }
    	        		
	    return interPts;		
 		
	}
    
	/**
	 * Check if two pints are within a given tie distance. If so, they are 
	 * considered as the same point.
	 */
	private boolean isSamePoint( Coordinate p1, Coordinate p2, double tieDist ) {
	    
		boolean sameP = false;
		if ( p1 != null && p2 != null && 
			 Math.abs( p1.x - p2.x ) < Math.abs( tieDist )  && 
		     Math.abs( p1.y - p2.y ) < Math.abs( tieDist )  ) {
		    sameP = true;
		}
		
		return sameP;
	}
    
    /**
	 * This routine clips a GFA polygon against common border between FA regional
	 * bounds to find the intersection points of the GFA polygon with the bound.
	 * Then the intersection points are snapped individually to points outside 
	 * of the polygon.  Both the intersection points and their snapped matches 
	 * are returned in map coordinate.
     * 
     * Note: the input is assumed to be in MAP coordinate
     * 
	 *  @param	gfaPoly		Polygon from GFA 
	 *  
     */
	private HashMap<Coordinate, Coordinate> getRegionIntersectionPt( Geometry gfaPoly )
	{
		HashMap<Coordinate, Coordinate>	interPtsPair = new HashMap<Coordinate, Coordinate>();	

		ArrayList<Coordinate> interPts = new ArrayList<Coordinate>();		
		ArrayList<Coordinate> pts;
		ArrayList<Integer> interIndex = new ArrayList<Integer>();
		ArrayList<Integer> indx = new ArrayList<Integer>();
		
		//Reorder in clockwise - first point is repeated at the end.
		ArrayList<Coordinate> gfaPoints = new ArrayList<Coordinate>();
		for ( Coordinate c : gfaPoly.getCoordinates() ) {
			gfaPoints.add( c );
		}
		
		ArrayList<Coordinate> cwGfaPts = GfaSnap.getInstance().reorderInClockwise( gfaPoints ,  null );
		
		//Get all intersection point with FA region common border.
		HashMap<String, Geometry> regionCommBnds = getFaRegionCommBounds();	
		for ( Geometry  bnd : regionCommBnds.values() ) {
			pts = lineIntersect( cwGfaPts.toArray( new Coordinate[ cwGfaPts.size() ]), 
					                  bnd.getCoordinates(), indx  );
			
			if ( pts.size() > 0 ) {
				interPts.addAll( pts );
				interIndex.addAll( indx );
			}
		}

		if ( interPts.size() <= 0 ) {
			return interPtsPair;
		}

/*
		ArrayList<Coordinate> closePts = new ArrayList<Coordinate>();		
		ArrayList<Integer> closeIndex = new ArrayList<Integer>();
		
		for ( int ii = 0; ii < interPts.size(); ii++ ) {
			 
		     Coordinate interP = interPts.get( ii );
			 Coordinate ptBefore = cwGfaPts.get( interIndex.get( ii ) );
			 Coordinate ptAfter = cwGfaPts.get( interIndex.get( ii ) + 1 );
			 
			 double qdist1 = GfaSnap.getInstance().distance( interP, ptBefore );
			 double qdist2 = GfaSnap.getInstance().distance( interP, ptAfter );
			 
			 double qdist = Math.min( qdist1, qdist2 );
			 if ( ( qdist / PgenUtil.NM2M ) <= GfaSnap.CLUSTER_DIST ) {
				 closePts.add( interP );
				 closeIndex.add( interIndex.get(ii) );
			 }
		}
		
		interPts.removeAll( closePts );
		interIndex.removeAll( closeIndex );
*/	
		/*
		 *  Find the Central boundary points inside the polygon to ensure
		 *  the new snap points will not cluster with these points.
		 */
		 ArrayList<Coordinate> checkPoints = new ArrayList<Coordinate>();
		 Geometry centralBnd = getFaRegionBounds().get( "CENTRAL" );
		 for ( Coordinate c : centralBnd.getCoordinates() ) {
			 Geometry pp = pointsToGeometry( new Coordinate[]{ c } );
			 if ( pp.within( gfaPoly ) ) {
				 checkPoints.add( c );
			 }
		 }

		 /*
		  *  Now insert each common intersection point into the el polygon
		  *  and snap it outside of the el polygon.
		  *  
		  *  Note: the point cannot be any point of the el polygon or any 
		  *  snap point that has been used.
		  *   
		  *  If an intersection point is not within the clustering distance of 
		  *  the point before it (Pb) or point after it (Pa), simply insert it into 
		  *  the polygon and snap it. Otherwise, do the following:
		  *  
		  *  1. Pick the closer one of Pb and Pa as Pn.  
		  *  2. Check if Pn is within the clustering distance of the common boundary
		  *     points inside the FROM line.  
		  *  3. If so, snap Pn to the closest point not within the clustering distance
		  *     and match the intersection point and Pn to the new point. 
		  *  4. If not, match the intersection point with Pn.
		  */
		 ArrayList<Coordinate> usedPoints = new ArrayList<Coordinate>();
		 usedPoints.addAll( cwGfaPts );
		 
		 ArrayList<Coordinate> ePts = new ArrayList<Coordinate>();		 
		 for ( int ii = 0; ii < interPts.size(); ii++ ) {
			 
		     Coordinate interP = interPts.get( ii );
			 Coordinate ptBefore = cwGfaPts.get( interIndex.get( ii ) );
			 Coordinate ptAfter = cwGfaPts.get( interIndex.get( ii ) + 1 );
			 
			 double qdist1 = GfaSnap.getInstance().distance( interP, ptBefore );
			 double qdist2 = GfaSnap.getInstance().distance( interP, ptAfter );

			 int addOne = -1;            	 
			 int qmatch = -1;
			 double qdist;

			 if ( qdist1 < qdist2 )  {
				 qmatch = interIndex.get( ii ); 
				 qdist  = qdist1;
			 }
			 else {
				 qmatch = interIndex.get( ii ) + 1;
				 qdist  = qdist2;
			 }

			 if ( ( qdist / PgenUtil.NM2M ) < GfaSnap.CLUSTER_DIST ) {

				 Coordinate ptMatch = cwGfaPts.get( qmatch );
				 for ( Coordinate c : checkPoints ) {
					 if ( GfaSnap.getInstance().isCluster( c, ptMatch ) ) {
						 addOne = qmatch;
						 break;
					 }
				 }
				 				 
				 if ( addOne < 0 ) {
					 interPtsPair.put( new Coordinate( interP ),
							           new Coordinate( ptMatch ) );
					 continue;  //Done - ptMatch is a snapped point.
				 }
			 }
			 
			 // Snap
			 int kk, kk2;
			 if ( addOne >= 0 ) {
			     kk = addOne;
			     kk2 = kk;

			     ePts.clear();
//			     ePts.addAll( cwGfaPts );
                 //If the point to be added is at the end, need to insert in - JW - 10/2012.
			     if ( addOne == (cwGfaPts.size() - 1) ) {
					 ePts.addAll( GfaSnap.getInstance().insertArray( cwGfaPts, interIndex.get( ii ) + 1, interP ) );
					 kk = interIndex.get( ii ) + 1;
					 kk2 = kk;
			     }
			     else {
			     ePts.addAll( cwGfaPts );
			 }
			 }
			 else {				 
			     ePts.clear();
				 ePts.addAll( GfaSnap.getInstance().insertArray( cwGfaPts, interIndex.get( ii ) + 1, interP ) );
				 kk = interIndex.get( ii ) + 1;
				 kk2 = kk;
			 }

			 
			 ePts.remove( ePts.size() - 1 );
			 
			 Coordinate[] snapped = new Coordinate[1];
             
			 //snap....
			 int status = GfaSnap.getInstance().snapPtGFA( kk, kk2, usedPoints, checkPoints, 
						                        ePts, true, true, 3.0F, snapped );
			 
			 if ( status != 0 ) {
				 if ( addOne >= 0 ) {
					 snapped[ 0 ] = new Coordinate( ePts.get( kk ) );
				 }
				 else {
					 snapped[ 0 ] = new Coordinate( interP );
				 }
			 }
			 
			 interPtsPair.put( new Coordinate( interP ), 
			                   new Coordinate( snapped[ 0 ] ) );
			 
			 if ( addOne >= 0 ) {
				 interPtsPair.put( new Coordinate( cwGfaPts.get( addOne ) ), 
		                           new Coordinate( snapped[ 0 ] ) );				 
			 }	
			 
			 usedPoints.add( snapped[ 0 ] );
			 
		 }


		
		return interPtsPair;
	}	

	/**
	 * Computes the intersection points of two multi-point lines.
	 * 
	 * The input is assumed to be in MAP coordinate.
	 * 
	 * Note: 1. the computation is done segment by segment, so if a line is closed 
	 *          it is the caller's responsibility to ensure the first point is repeated
	 *          at the end of the line.
	 *       2. the index of the point before the intersection in line1 is retained. So 
	 *          the index is dependent the direction of the line1 (Clockwise or
	 *          counterclockwise)
	 */
	public ArrayList<Coordinate> lineIntersect( Coordinate[] line1, Coordinate[] line2,
			                                    ArrayList<Integer> indexInLine1 ) {
	    
		ArrayList<Coordinate> interPts = new ArrayList<Coordinate>();
		if ( indexInLine1 == null ) {
			indexInLine1 = new ArrayList<Integer>();
		}
		else {
			indexInLine1.clear();
		}

		Coordinate[] line1InGrid = PgenUtil.latlonToGrid( line1 );
		Coordinate[] line2InGrid = PgenUtil.latlonToGrid( line2 );

		Coordinate onePt;
		for ( int ii = 0; ii < (line1.length - 1); ii++ ) {
			for ( int jj = 0; jj < (line2.length - 1); jj++ ) {
				
				onePt = segmentIntersect( line1InGrid[ ii ], line1InGrid[ ii + 1 ], 
						                  line2InGrid[ jj ], line2InGrid[ jj + 1 ] );
				if ( onePt != null ) {
					Coordinate[] onePtInMap = PgenUtil.gridToLatlon( new Coordinate[]{ onePt } );
					interPts.add( new Coordinate( onePtInMap[ 0 ] ) );
					indexInLine1.add( new Integer( ii ) );
				}				
			}
		}
	    
		return interPts;
	}	
	
	/**
	 * Computes the intersection points of two line segments (not extended lines).
	 * 
	 * No coordinate system is assumed.
	 * 
	 */
	public Coordinate segmentIntersect( Coordinate seg1_start, Coordinate seg1_end,
			                            Coordinate seg2_start, Coordinate seg2_end ) {
	    Coordinate interPt = null;
	    
	    Geometry g1 = pointsToLineString( new Coordinate[]{seg1_start, seg1_end} );
	    Geometry g2 = pointsToLineString( new Coordinate[]{seg2_start, seg2_end} );
	     
	    if ( g1.intersects( g2 ) ) {
	    	interPt = g1.intersection( g2 ).getCoordinates()[0];
	    }
	    
		return interPt;
	}	

	/**
	 * Get the common bounds between FA West and Central, Central and East.
	 */
	public HashMap<String, Geometry> getFaRegionCommBounds()
	{
		if (  faRegionCommBounds == null ){
		    loadFaRegionCommBounds();
		}
	    		
		return faRegionCommBounds;
	}	
	
	/**
	 * Calculate the common bounds between FA West and Central, Central and East.
	 */
	private void loadFaRegionCommBounds()
	{
		faRegionCommBounds = new HashMap<String, Geometry>();			
		HashMap<String, Geometry> rgbnds = getFaRegionBounds();				
        ArrayList<String>  used = new ArrayList<String>();		
		
        for ( String regionName1 : rgbnds.keySet() ) {            
			used.add( regionName1 );
			Geometry bnd1 = rgbnds.get( regionName1 );
			for ( String regionName2 : rgbnds.keySet() ) {
				if ( !used.contains( regionName2 ) ) {
					Geometry bnd2 = rgbnds.get( regionName2 );
					if ( bnd1.intersects( bnd2 ) ) {
						String bname = new String( regionName1 + "-" + regionName2 );
						Geometry comm = bnd1.intersection( bnd2 );
						//Use CoordinateList to remove duplicate points
						CoordinateList clist = new CoordinateList( );
						clist.add( comm.getCoordinates(), false );
						
						faRegionCommBounds.put( bname, pointsToLineString( clist.toCoordinateArray() ) );
					}
				}
			}
		}       
	}	

	/**
	 * Get the common bounds between extended SFO/SLC bounds, and 
	 * extended CHI-BOS and DFW-MIA bounds.
	 */
	public HashMap<String, Geometry> getFaAreaXCommBounds()
	{
		if (  faAreaXCommBounds == null ){
		    loadFaAreaXCommBounds();
		}
	    		
		return faAreaXCommBounds;
	}	
	
	/**
	 * Calculate the common bounds between extended FA area SLC and dSFC,  
	 * CHI-BOS and DFW-MIA.
	 */
	private void loadFaAreaXCommBounds()
	{
		faAreaXCommBounds = new HashMap<String, Geometry>();			
		HashMap<String, Geometry> areaxbnds = getFaAreaXBounds();				
		
        Geometry bnd1 = areaxbnds.get( "SLC" );
        Geometry bnd2 = areaxbnds.get( "SFO" );
        
		if ( bnd1.intersects( bnd2 ) ) {
			String bname = new String( "SLC-SFO" );
			Geometry comm = bnd1.intersection( bnd2 );
			//Use CoordinateList to remove duplicate points
			CoordinateList clist = new CoordinateList( );
			clist.add( comm.getCoordinates(), false );
			
			faAreaXCommBounds.put( bname, pointsToLineString( clist.toCoordinateArray() ) );
		}
		
		bnd1 = areaxbnds.get( "CHI" );
		bnd2 = areaxbnds.get( "DFW" );
		
		if ( bnd1.intersects( bnd2 ) ) {
			String bname = new String( "CHI-DFW" );
			Geometry comm = bnd1.intersection( bnd2 );
			//Use CoordinateList to remove duplicate points
			CoordinateList clist = new CoordinateList( );
			clist.add( comm.getCoordinates(), false );
			
			faAreaXCommBounds.put( bname, pointsToLineString( clist.toCoordinateArray() ) );
			
			String bname2 = new String( "BOS-MIA" );			
			faAreaXCommBounds.put( bname2, pointsToLineString( clist.toCoordinateArray() ) );
			
		}
     
        
	}	

	/**
	 *  Cleans up improper point sequence:
	 *  
	 *  Note: MAP coordinate is assumed.
	 *  
	 *  1. compresses the consecutive duplicate points to single point 
	 *  2. compress ABA point sequence to a single point A
	 *  
	 */
	public Geometry cleanupPoints( Geometry geom )
	{
		Coordinate[]  gPts = geom.getCoordinates();
		ArrayList<Coordinate>  gList = new ArrayList<Coordinate>();

		for ( Coordinate c : gPts ) {
		    gList.add( new Coordinate( Math.rint(c.x * 100) / 100,
		    		                   Math.rint(c.y * 100) / 100 ) );
	    }
		
		gList.remove( gList.size() - 1 ); //remove last point.		
		
		//Compresses consecutive duplicate points to single point. 
        int ii = 0, jj, nshift;
        boolean done = false; 
        while ( !done )  {

        	jj = ii;
        	while ( ( jj + 1 ) < gList.size() &&
        			( Math.abs( gList.get( ii ).x - gList.get( jj + 1 ).x ) < SMALLF && 
        			  Math.abs( gList.get( ii ).y - gList.get( jj + 1 ).y ) < SMALLF  ) ) {
        		jj += 1;
        	}
        	nshift = jj - ii;

        	if ( nshift != 0 )  { 
        		gList = GfaSnap.getInstance().collapseArray( gList, ii, nshift );
        	}

        	ii++;

        	if ( ii >= gList.size() ) done  = true; 
        }

        //Reduce ABA case to A (line starts at A, goes to B and then back to A).        
        ii = 0;
        done = false;    
        while ( !done ) {

        	if ( ( ii + 2) < gList.size() &&
			     ( Math.abs( gList.get( ii ).x - gList.get( ii + 2 ).x ) < SMALLF && 
			       Math.abs( gList.get( ii ).y - gList.get( ii + 2 ).y ) < SMALLF  ) ) {
        		
        		gList = GfaSnap.getInstance().collapseArray( gList, ii, 2 );
        	}

        	ii++;

        	if ( ii >= gList.size() ) done  = true; 
        	
        }
        
        
	    //Make sure the first point is not duplicated at the end.
    	if ( ( Math.abs( gList.get( 0 ).x - gList.get( gList.size() - 1 ).x ) < SMALLF && 
			   Math.abs( gList.get( 0 ).y - gList.get( gList.size() - 1 ).y ) < SMALLF  ) ) {
	        gList.remove( gList.size() - 1 );
    	}
       
        Coordinate[] outp = gList.toArray( new Coordinate[ gList.size() ] );
				
		return pointsToGeometry( outp ) ;
		
	}	
	
	/**
	 * Finds and replaces the points in a Geometry with its pair.
	 */
	protected Geometry replacePts( Geometry geom, HashMap<Coordinate, Coordinate> pairs ) {
	    
		Coordinate[]  poly = geom.getCoordinates();
		Coordinate[]  pts = new Coordinate[ poly.length - 1 ]; //remove last point
		
		for ( int ii = 0; ii < ( poly.length - 1 ); ii++ ) {           		
	    	
			pts[ ii ] = new Coordinate( poly[ ii ] );
			
	    	if ( pairs != null ) {
				for ( Coordinate p : pairs.keySet() ) {	    
					if ( Math.abs( poly[ ii ].x - p.x ) < SMALLF && 
						 Math.abs( poly[ ii ].y - p.y ) < SMALLF ) {
						pts[ ii ] = new Coordinate( pairs.get( p ) );
						break;
					}
				}
			}	    	
	    }
				
		return pointsToGeometry( pts );
					
	}	

	
	/**
	 * Flags a set of points in a GFA element as non-reduce-able they are in tie
	 * distance of one of the non-reduce-able points.  This strategy helps to
     * preserve the integrity of the boundary after clipping and point reduction.
	 * 
	 * The non-reduce-able points includes: all the FA regional boundary points, the 
	 * snapped intersection points of the original GFA with the FA international 
	 * and common regional bounds, as well as the points immediately before and 
	 * such a non-reduce-able point.
	 */
	private void addReduceFlags( Gfa elm, CoordinateList pts ) {

		// Note: the first point is not repeated at the end for a DrawableElement.
		Coordinate[]  poly = elm.getLinePoints();
		int np = poly.length;
		boolean[]  reduceable = new boolean[ np ];
		for ( int ii = 0; ii < np; ii++ ) {            					
			reduceable[ ii ] = true;
		}

		for ( int ii = 0; ii < np; ii++ ) {            								
			for ( int jj = 0; jj < pts.size(); jj++  ) {	    
	    	    if ( Math.abs( poly[ ii ].x - pts.getCoordinate( jj ).x ) < SMALLF && 
	    	         Math.abs( poly[ ii ].y - pts.getCoordinate( jj ).y ) < SMALLF ) {
	    		    reduceable[ ii ] = false;
	    		    reduceable[ ( ii - 1 + np ) % np ] = false;
	    		    reduceable[ ( ii + 1 + np ) % np ] = false;
	    		    break;
	    	    }
	    	}			
	    }
		
		elm.setReduceFlags( reduceable );

	}		

	/**
	 * Finds the common points between two sets of Geometries.
	 */
	public ArrayList<Coordinate> getCommonPoints( ArrayList<Geometry> big,
												   ArrayList<Geometry> small ) {
		
		ArrayList<Coordinate> commPts = new ArrayList<Coordinate>();

		for ( Geometry g1 : big ) {
			for ( Geometry g2 : small ) {
				if ( g1.intersects( g2 ) ) {
					for ( Coordinate c : g1.intersection( g2 ).getCoordinates() ) {
						if ( !commPts.contains( c ) ) {
							commPts.add( new Coordinate( c ) );
						}
					}
				}
			}
		}
				
		return commPts;
	}
	
	/**
	 * Remove a list of points from a Geometry and form a new Geometry.
	 */
	public Geometry removeCommonPoints( Geometry gm, ArrayList<Coordinate> pts ) {
		
		if ( pts == null || pts.size() == 0 ) {
			return gm;
		}
		
		ArrayList<Coordinate> newPts = new ArrayList<Coordinate>();
        
		Coordinate[] gPts = gm.getCoordinates();

		for ( int ii = 0; ii < ( gPts.length - 1 ); ii++ ) {
			if ( !pts.contains( gPts[ ii ] ) ) {
				newPts.add( new Coordinate( gPts[ ii ] ) );
			}
		}
		
		return pointsToGeometry( newPts ); 
			
	}
	
	/**
	 * Load all bounds needed for Gfa processing.
	 */
	public void loadGfaBounds() {
		
		//States, FA Regions, FA Areas, Fa extended Areas.
		getStateBounds();
		getFaRegionBounds();
		getFaAreaBounds();
		getFaAreaXBounds();
				
		getGreatLakeBounds();		
		getCoastalWaterBounds();
		getFaInternationalBound();
		getMtObscStates();

		getFaRegionCommBounds();
		getFaAreaXCommBounds();
				
		//Convert to grid coordinates.
		updateGfaBoundsInGrid();

		//Snap points
		SnapVOR.getSnapStns( null, 16 );
		
	}
	
	/**
	 * Update all bounds needed for Gfa processing.
	 */
	public void updateGfaBoundsInGrid() {
        
		faInternationalBoundInGrid = geometryInGrid( getFaInternationalBound() );
		
		faRegionBoundsInGrid = updateBoundsInGrid( getFaRegionBounds() );
		faAreaBoundsInGrid = updateBoundsInGrid( getFaAreaBounds() );
		faAreaXBoundsInGrid = updateBoundsInGrid( getFaAreaXBounds() );

		stateBoundsInGrid = updateBoundsInGrid( getStateBounds() );

/*
		stateBoundsInGrid = updateBoundsInGrid( getStateBounds() );
		StringBuilder invalidSts = new StringBuilder();
		for ( String key : stateBoundsInGrid.keySet() ) {
		    Geometry g1 = stateBounds.get( key );
		    Geometry g2 = stateBoundsInGrid.get( key );
		    
		    if ( !g1.isValid() || (g1.isValid() && !g2.isValid()) ) {
		    	invalidSts.append( " " + key );
		    }
		}
		
		System.out.println( "Invalid state = " + invalidSts );

		for ( String key : stateBoundsInGrid.keySet() ) {
	    Geometry g1 = stateBounds.get( key );
	    Geometry g2 = stateBoundsInGrid.get( key );
	    System.out.println( "Original State Bound for " + key + " is " + g1.isValid() );
	    System.out.println( "\tState Bound in Grid for " + key + " is " + g2.isValid() );
	}
	stateBoundsInGrid = updateBoundsInGrid( getStateBounds() );
*/
		greatLakesBoundsInGrid = updateBoundsInGrid( getGreatLakeBounds() );
		coastalWaterBoundsInGrid = updateBoundsInGrid( getCoastalWaterBounds() );
		faRegionCommBoundsInGrid = updateBoundsInGrid( getFaRegionCommBounds() );
		faAreaXCommBoundsInGrid = updateBoundsInGrid( getFaAreaXCommBounds() );
				
	}
	
	/**
	 * Load all bounds in grid coordinate.
	 */
	private HashMap<String, Geometry> updateBoundsInGrid( HashMap<String, Geometry> bndInMap ) {
		
		HashMap<String, Geometry> bndInGrid = new HashMap<String, Geometry>();				
								
		for ( String bname : bndInMap.keySet() ) {          			
			bndInGrid.put( bname , geometryInGrid( bndInMap.get( bname ) ) );
		}
		
		return bndInGrid;
		
	}
		

	/**
	 * Getters for all bounds in grid coordinate.
	 */
	public Geometry getFaInternationalBoundInGrid() {
		return faInternationalBoundInGrid;
	}

	public HashMap<String, Geometry> getFaRegionBoundsInGrid() {
		return faRegionBoundsInGrid;
	}

	public HashMap<String, Geometry> getFaAreaBoundsInGrid() {
		return faAreaBoundsInGrid;
	}

	public HashMap<String, Geometry> getFaAreaXBoundsInGrid() {
		return faAreaXBoundsInGrid;
	}

	public HashMap<String, Geometry> getStateBoundsInGrid() {
		return stateBoundsInGrid;
	}

	public HashMap<String, Geometry> getGreatLakesBoundsInGrid() {
		return greatLakesBoundsInGrid;
	}

	public HashMap<String, Geometry> getCoastalWaterBoundsInGrid() {
		return coastalWaterBoundsInGrid;
	}

	public HashMap<String, Geometry> getFaRegionCommBoundsInGrid() {
		return faRegionCommBoundsInGrid;
	}

	public HashMap<String, Geometry> getFaAreaXCommBoundsInGrid() {
		return faAreaXCommBoundsInGrid;
	}	
	
	/*
	 * Convert a Geometry in map coordinate into grid coordinate.
	 */
	private Geometry geometryInGrid( Geometry geomInMap ) {
    	
		Geometry gout = null;
		if ( geomInMap instanceof Polygon || geomInMap instanceof MultiPolygon ) {
		    
			Polygon[] polygons = new Polygon[ geomInMap.getNumGeometries() ];
 				 												
	        for ( int ii = 0; ii < geomInMap.getNumGeometries(); ii++ ) {
		        Geometry g = geomInMap.getGeometryN( ii );
		        Coordinate[] gpts = g.getCoordinates();
		    	Coordinate[] gpts1 = Arrays.copyOf( gpts, gpts.length - 1 );
	    	    Coordinate[] pts = PgenUtil.latlonToGrid( gpts1 );
	    	
			    polygons[ ii ] = pointsToPolygon( pts );
		    }
		
	        gout = geometryFactory.createMultiPolygon( polygons );
		}
		else if ( geomInMap instanceof LineString ) {
	        Coordinate[] gpts = geomInMap.getCoordinates();
    	    Coordinate[] pts = PgenUtil.latlonToGrid( gpts );
	        
    	    gout = pointsToLineString( pts );
		}
		
	    return gout;
		
	}
		

	/**
	 * Try to write state bounds in error to a PGEN line element for visual check.
	 */
	public void validateGfaBounds() {
		writeErrorBound( "FA_States_Map_Fixed", getStateBounds() );				
	}
	
	/*
	 * 	check the invalid polygons in state bounds and write each state into
	 *  a PGEN file.
	 */
	private void writeErrorBound( String bndName, HashMap<String, Geometry> bndMap ) {
    		    	
	    ArrayList<String>  errorStates = new ArrayList<String>();
//	    for ( String s : WRONG_STATES ) {
//	    	errorStates.add( s );
//	    }
        
	    boolean isvalid = true;

		StringBuilder ss = new StringBuilder();
        for ( String key : bndMap.keySet() ) {		    
		       ss.append( key + " ");
		}	    	
		
	    StringBuilder as = new StringBuilder();
		for ( String key : bndMap.keySet() ) {

		    Product activeProduct = new Product("Default", "Default", "Default",
		  		      new ProductInfo(), new ProductTime(), new ArrayList<Layer>() );
			    
			Layer activeLayer = new Layer();
			activeProduct.addLayer( activeLayer );
			    
			List<Product> productList = new ArrayList<Product>();
			productList.add( activeProduct );

		    Geometry g = bndMap.get( key );
		    
	    	ArrayList<Coordinate> pts = new ArrayList<Coordinate>();
            
//	    	if ( errorStates.contains( key ) ) {
    		isvalid = g.isValid();
	    	if ( !isvalid ) {
    			as.append( " " + key );
	    		System.out.println( "\tInvalid Bound " + key + "\t " + g.getNumGeometries() );
    		}
	    	
//	    	if ( !g.isValid() ) {
        	    
	    		
	    		System.out.println( "\tProcess Bound " + key + "\t " + g.getNumGeometries() );
			    
	    		for ( int jk = 0; jk < g.getNumGeometries(); jk++ ) {
			        
	    			pts.clear(); //!!!
	    			
	    			Geometry one = g.getGeometryN( jk );
			        			        				        
		        	for ( Coordinate c : one.getCoordinates() ) {
						pts.add( new Coordinate( c ) );
					}					
		        	pts.remove( pts.size() - 1 );
					    			
			        Line cline = new Line( null, new Color[]{ Color.red }, 
			            		2.0f, 1.0, true, false, pts, 0, FillPattern.SOLID, 
			            		"Lines", "LINE_SOLID" );
                    String str = "";
                    str = str + key + "_" + jk;
			            
			        Text lbl = new Text( null, "Courier", 14.0f, 
			            		TextJustification.CENTER, null, 0.0, 
			            		TextRotation.SCREEN_RELATIVE, new String[]{ str },
                                FontStyle.REGULAR, Color.GREEN, 0, 0, true, DisplayType.NORMAL,
                                "Text", "General Text" );	
		                        lbl.setLocation( cline.getLinePoints()[0] );
		            
//		             activeLayer.add( lbl );		    		    
		  		     activeLayer.add( cline );
			    }			
	    	
	    	    /*
	             * Write into a file.
	             */
		        if ( productList.get(0).getLayer(0).getDrawables().size() > 0 ) {			        
		        	Products filePrds = ProductConverter.convert( productList  );
	                String a;
		        	if ( isvalid ) {
		        	    a = "/export/cdbsrv/jwu/stateBnd/" + bndName + "_high_" + key + "_correct_nolabel.xml";
	                }
	                else {
		        	    a = "/export/cdbsrv/jwu/stateBnd/" + bndName + "_high_" + key + "_icorrect_nolabel.xml";	                	
	                }
		        	System.out.println( "Write to validation file: " + a );
		            FileTools.write( a, filePrds ); 
		        }
		    }
		
    	    System.out.println( "Invalid states are: " + as );		    
		
//		}
	}

	
	/*
	 * Fixes the incorrect state bounds for a few states.
	 */
	private HashMap<String, Geometry> fixStateBounds( HashMap<String, Geometry> stateBnd ) {
		
		HashMap<String, Geometry> fixedBounds = new HashMap<String, Geometry>();
		
        ArrayList<String>  errorStates = new ArrayList<String>();
        for ( String s : WRONG_STATES ) {
             errorStates.add( s );
        }
     		 	                   
		for ( String key : stateBnd.keySet() ) {
			
			Geometry g = stateBnd.get( key );
			
			if ( !errorStates.contains( key ) ) {
				fixedBounds.put( key, g );
			}
			else {		    
	    	    ArrayList<Coordinate> pts = new ArrayList<Coordinate>();
			
	    	    Polygon[] polygons = new Polygon[ g.getNumGeometries() ];
                    	    			    
	    		for ( int jk = 0; jk < g.getNumGeometries(); jk++ ) {
			        
	    			pts.clear(); //!!!
	    				    			
	    			Geometry one = g.getGeometryN( jk );
	    			Coordinate[] bpts = one.getCoordinates();
	    			Coordinate firstPt = bpts[0]; 
	    			pts.add( firstPt );
	    			int nn = 1;
	    			while ( ! bpts[ nn ].equals( firstPt ) && nn < bpts.length ) {					
	    				pts.add( bpts[ nn ] );	
	    				nn++;
	    			}		            
              			    	
	    	        polygons[ jk ] = pointsToPolygon( pts.toArray( new Coordinate[ pts.size() ] ) );
	    		}
	    		
	    		MultiPolygon multipolygon = geometryFactory.createMultiPolygon( polygons );
	    		
	    		fixedBounds.put( key,  multipolygon );
	    	}	    			    	
		} 
		
		return fixedBounds;
	}	
	
	/*
	 * Find the first occurrence of a point in an array.
	 * 
	 * @param pt
	 * @param list
	 * @param tie
	 */
	private int findPoint( Coordinate pt, List<Coordinate> list, double tie ) {
		
		int indx = -1;
		
		if ( pt != null && list != null ) {
        	for ( int ii = 0; ii < list.size(); ii++ ) {
         		if ( isSamePoint( pt, list.get( ii ), tie ) ) {
        		    indx =  ii;
        		    break;
        		}
        	}
		}
		
		return indx;
		
	}
	
	/*
	 * Some tests for JTS, could be removed later.
	 */
	public void testJTS() {
		Coordinate[] polyPts = new Coordinate[]{ new Coordinate(-110.0, 40.0), 
												 new Coordinate(-110.0, 50.0), 	
												 new Coordinate(-90.0, 50.0), 	
												 new Coordinate(-90.0, 40.0) };
		Geometry poly = pointsToGeometry( polyPts );
        Geometry bndPts = 	pointsToGeometry( new Coordinate[]{ new Coordinate(-110.0, 50.0) } );	
        Geometry inPts = 	pointsToGeometry( new Coordinate[]{ new Coordinate(-100.0, 45.0) } );	
        Geometry outPts = 	pointsToGeometry( new Coordinate[]{ new Coordinate(-100.0, 30.0) } );	
        
        Geometry a1 = bndPts.intersection( poly );
        Geometry a2 = inPts.intersection( poly );
        Geometry a3 = outPts.intersection( poly );
 
        Geometry bndlin = pointsToGeometry( new Coordinate[]{ new Coordinate(-110.0, 50.0),
				                                              new Coordinate(-110.0, 40.0) } );	
       
        Geometry inlin = pointsToGeometry( new Coordinate[]{ new Coordinate(-110.0, 50.0),
        													 new Coordinate(-100.0, 45.0) } );	
       
        Geometry outlin = pointsToGeometry( new Coordinate[]{ new Coordinate(-110.0, 50.0),
				                                              new Coordinate(-100.0, 30.0) } );	
        Geometry a4 = bndlin.intersection( poly );
        Geometry a5 = inlin.intersection( poly );
        Geometry a6 = outlin.intersection( poly );
       
        System.out.println( "\n\na1="+ a1.getCoordinates().length );
        System.out.println( "a2="+ a2.getCoordinates().length );
        System.out.println( "a3="+ a3.getCoordinates().length );
        System.out.println( "a4="+ a4.getCoordinates().length );
        System.out.println( "a5="+ a5.getCoordinates().length );
        System.out.println( "a6="+ a6.getCoordinates().length );
        
        Geometry l1 = pointsToGeometry( new Coordinate[]{ new Coordinate(-110.0, 40.0),
				 new Coordinate(-110.0, 50.0) } );	
        Geometry l2 = pointsToGeometry( new Coordinate[]{ new Coordinate(-90., 40.0),
				 new Coordinate(-100.0, 45.0) } );	
        Geometry l3 = pointsToGeometry( new Coordinate[]{ new Coordinate(-90., 40.0),
				 new Coordinate(-115.0, 40.0) } );	
     
        Geometry a7 = l1.intersection( l2 );
        System.out.println( "a7="+ a7.getCoordinates().length );
       
        Geometry a8 = l1.intersection( l3 );
        System.out.println( "a8="+ a8.getCoordinates().length );

        Geometry g1 = pointsToGeometry( new Coordinate[]{ 
        		 new Coordinate(-110.0, 47.9),
				 new Coordinate(-104.0, 46.85),  
				 new Coordinate(-104.0, 42.80),
				 new Coordinate(-110.0, 43.0)} );	
       
        Geometry g2 = pointsToGeometry( new Coordinate[]{ 
				 new Coordinate(-104.0, 46.85),  
				 new Coordinate(-97.0, 46.4),  
				 new Coordinate(-98, 42.8),  
				 new Coordinate(-104.0, 42.80) });	
        
        Geometry u1 = g1.union( g2 );
        System.out.println( "\nTest union of two polygons clipped from one polygon:" );
        for ( Coordinate c : u1.getCoordinates() ) {
            System.out.println( c.y+","+c.x);
        }
        
		ArrayList<Coordinate>  uniquePts = new ArrayList<Coordinate>();
		for ( Coordinate c : u1.getCoordinates() ) {
			if (  Arrays.asList( g1.getCoordinates() ).contains( c ) &&
				  Arrays.asList( g2.getCoordinates() ).contains( c ) ) {				
			}
			else {
				uniquePts.add( c );
			}
		}
							    
		System.out.println( "\nUnique points found:" );
	    for ( Coordinate c : uniquePts ) {
	        System.out.println( c.y+","+c.x);
	    }
	 		
        Geometry p1 = pointsToGeometry( new Coordinate[]{ new Coordinate(20, 10), new Coordinate(30, 20),
                new Coordinate(40,10) } );
        Geometry p2 = pointsToGeometry( new Coordinate[]{ new Coordinate(20, 10) } );
		System.out.println( "\nTest:" );
        
		if ( p1.covers( p2 )) {
	        System.out.println( "Covered");
		}
		else {
	        System.out.println( "Not Covered");			
		}
      
		if ( p2.intersects( p1 )) {
	        System.out.println( "Intersect");
		}
		else {
	        System.out.println( "Not Intersect");			
		}

		/*
		Coordinate  s1 = new Coordinate( 0, 0);
		Coordinate  s2 = new Coordinate( 100, 100);
		
		Coordinate  t1 = new Coordinate( 0, 0);
		Coordinate  t2 = new Coordinate( -100, -100);
		
		Coordinate  o1 = new Coordinate( 0, 100 );
		Coordinate  o2 = new Coordinate( 0, -100 );
		
		if ( onLeft( o1, s1, s2 ) ) {
		    System.out.println( "o1 is at left of s1s2 " );
		}
		else {
		    System.out.println( "o1 is at right of s1s2 " );			
		}
       
		if ( onLeft( o2, s1, s2 ) ) {
		    System.out.println( "o2 is at left of s1s2 " );
		}
		else {
		    System.out.println( "o2 is at right of s1s2 " );			
		}
		
		if ( onLeft( o1, t1, t2 ) ) {
		    System.out.println( "o1 is at left of t1t2 " );
		}
		else {
		    System.out.println( "o1 is at right of t1t2 " );			//
		}
       
		if ( onLeft( o2, t1, t2 ) ) {
		    System.out.println( "o2 is at left of t1t2 " );
		}
		else {
		    System.out.println( "o2 is at right of t1t2 " );			
		}
		*/
		
	}

		
}
