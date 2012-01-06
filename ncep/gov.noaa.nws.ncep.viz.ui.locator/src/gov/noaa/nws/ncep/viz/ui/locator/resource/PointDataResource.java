package gov.noaa.nws.ncep.viz.ui.locator.resource;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.NonSI;
import javax.measure.unit.SI;

import java.io.File;
import java.io.IOException;
import java.net.MalformedURLException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.List;

import org.geotools.data.DefaultQuery;
import org.geotools.data.DefaultTransaction;
import org.geotools.data.FeatureReader;
import org.geotools.data.shapefile.indexed.IndexType;
import org.geotools.data.shapefile.indexed.IndexedShapefileDataStore;
import org.geotools.factory.CommonFactoryFinder;
import org.geotools.factory.GeoTools;
import org.geotools.feature.FeatureIterator;
import org.geotools.feature.IllegalAttributeException;
import org.geotools.geometry.jts.ReferencedEnvelope;
import org.geotools.referencing.GeodeticCalculator;
import org.geotools.referencing.datum.DefaultEllipsoid;
import org.opengis.feature.simple.SimpleFeature;
import org.opengis.filter.Filter;
import org.opengis.filter.FilterFactory2;

import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.viz.core.exception.VizException;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;

/**
 * 
 * Locator point data resource
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 *  01/2009    #48        M. Li     Initial version
 *  02/2009		64		  M. Li		Add locator editing	
 *  03/2009		80		  M. Li		Use .qix index in TO10
 * 
 * 
 * * @author mli
 * * @version 1.0
 * 
 */
@SuppressWarnings("deprecation")
public class PointDataResource {

	// shapefile name
	private String shapefile = null;
	
	private int filterFactor = 1;
	
	private final int MaxFactorNum = 6; 
	
	// Filter box factor
	private final int boxFactor = 2;
	
	private final double POINT_INQUIRY_TOLERANCE = 0.75;  
	
	// Default attribute name for point shapefile
	private String attribute = "NAME";		
	
	// Default attribute name for State ID
	private String stateIDAttr = "ST";	
		
	private final int MAX_POINT_NUM = 25;
	
	/**
	 * Constructor
	 * 
	 * @param file  
	 * @param attribute
	 */
    public PointDataResource(String file, String attribute) {
        this.shapefile = file;  
        this.attribute = attribute;
    }
    
    /**
     * Retrieve point data from a shape file.
     * 
     * @param attribute
     * @param ll
     * @param isFiltered
     * @param tolerance
     * @param factor
     * @return List<LocatorPointData>
     * @throws IOException 
     * @throws Exception
     */
    @SuppressWarnings("unchecked")
    private List<LocatorPointData> getPointDataList(String attribute, 
    		Coordinate ll, boolean isFiltered, double tolerance, int factor ) throws VizException, IOException {
    	
    	IndexedShapefileDataStore shapefileDataStore = null;
        
        FeatureIterator<SimpleFeature> featureIterator = null;
        
        String shapeField = null;        

        DefaultQuery query = null;
        
    	File theshape = null;
    	
    	theshape = new File(shapefile);
    	  	
		try {
			shapefileDataStore = new IndexedShapefileDataStore(
				        theshape.toURI().toURL(), null, false, true,
				        IndexType.QIX);
		} catch (MalformedURLException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}				       
        
        String[] types = shapefileDataStore.getTypeNames();   
        query = new DefaultQuery();
        query.setTypeName(types[0]);

        /*
         * set the filter
         */ 
        if (isFiltered) {
        	FilterFactory2 ff = CommonFactoryFinder.getFilterFactory2(GeoTools        
        			.getDefaultHints());
        	GeometryFactory gf = new GeometryFactory();
        	shapeField = shapefileDataStore.getFeatureSource().getSchema().getGeometryDescriptor()
             		.getLocalName();
        	if ( factor > 1 ) tolerance = factor * tolerance;
        	double minLon = ll.x - tolerance;
        	double maxLon = ll.x + tolerance;
        	double minLat = ll.y - tolerance;
        	double maxLat = ll.y + tolerance;        	
        	ReferencedEnvelope bbox = new ReferencedEnvelope(minLon, maxLon,
        		minLat, maxLat, MapUtil.LATLON_PROJECTION);
        	
        	Geometry envelopeGeometry = gf.toGeometry(bbox);
        	Filter filter = ff.intersects(ff.literal(envelopeGeometry), ff
                    .property(shapeField));
        	query.setFilter(filter);
        	//Filter bboxFilter = ff.bbox(ff.property(shapeField), bbox);
        	//query.setFilter(bboxFilter);
        	
        }
        
        /*
         * read shapefile
         */ 
        featureIterator = shapefileDataStore.getFeatureSource().getFeatures(query)
        	.features();
        
        List<LocatorPointData> pointList = new ArrayList<LocatorPointData>();
        while (featureIterator.hasNext()) {           
        	SimpleFeature f = featureIterator.next();
        	
        	LocatorPointData point = new LocatorPointData();
	            
				//get name
	            String name= f.getAttribute(attribute).toString().trim();	           
	            if (name.length() < 2) continue;
	            point.setName(name);
	                    
	            //get lat/lon
	            double lon = Double.valueOf(f.getAttribute("LON").
	            		toString().trim()).doubleValue();
	            double lat = Double.valueOf(f.getAttribute("LAT").
	            		toString().trim()).doubleValue();
	            point.setLon(lon);
	            point.setLat(lat);
	            
	            pointList.add(point);      
        }
        if (featureIterator != null) {
            featureIterator.close();
        }
    	return pointList;
    	
    }
    
    /**
     * Compute the nearest point from given point.
     * 
     * @param ll -- given point coordinate
     * @return distance, direction and name of the nearest point
     * @throws Exception
     * @throws RuntimeException
     * @throws IllegalAttributeException
     * @throws IOException 
     */
    public String getNearestPoint(Coordinate ll, DisplayOptions displayOption) 
    	throws VizException, RuntimeException, IOException  {
    	/*
    	 * Check for invalid Coordinate
    	 */
    	if (ll.x > 180.0 || ll.x < -180.0 || ll.y > 90.0 || ll.y < -90.0)
    		return null;
    	
    	/*
    	 * Retrieve all points within the envelope
    	 */
    	List<LocatorPointData> list = new ArrayList<LocatorPointData>();
        	    	
		list = getPointDataList(attribute, ll, 
					true, POINT_INQUIRY_TOLERANCE, filterFactor );
		
		/*
		 * expand the envelope if no point inside
		 */
		int factorNum = 1;
		boolean filter = true;
    	while (list.size() < 1) {
    		filterFactor = factorNum * boxFactor;
    		
    		if (factorNum > MaxFactorNum) filter = false;
    		list = getPointDataList(attribute, ll, 
					filter, POINT_INQUIRY_TOLERANCE, filterFactor );
    		
    		if (factorNum > MaxFactorNum || filter == false) break;
    		factorNum++;    		
    	}
    	
    	if (list.size() < 1) return null;
    	
    	/*
    	 * Compute distance (in meter) of the nearest point
    	 */
    	int index = -1;
    	double distance = -999.99;
    	for (int ii = 0; ii < list.size(); ii++) {
    		GeodeticCalculator gc = new GeodeticCalculator(
                    DefaultEllipsoid.WGS84);
            gc.setStartingGeographicPoint(ll.x, ll.y);
            gc.setDestinationGeographicPoint(list.get(ii).getLon(), list.get(ii).getLat());            
            double dist = 0;
            dist = gc.getOrthodromicDistance();
            
            if (ii == 0) {
            	distance = dist;
            	index = 0;
            } else {
            	if (distance > dist ) {
            		distance = dist;
            		index = ii;
            	}
            }            
    	}
    	if (index < 0 ) return null;
    	
    	//UnitConverter does not work with SWT????
    	//UnitConverter metersToMiles = SI.METER.getConverterTo(NonSI.MILE);
    	//int distanceInMile = (int)metersToMiles.convert(distance);
    	//int distanceInMile = (int) (distance / 1609.344);
    	
    	/*
    	 * Compute direction, and format output string
    	 */

    	if (distance > 0) {
    		GeodeticCalculator gc = new GeodeticCalculator(
    				DefaultEllipsoid.WGS84);
    		gc.setStartingGeographicPoint(list.get(index).getLon(), 
    				list.get(index).getLat());
    		gc.setDestinationGeographicPoint(ll.x, ll.y);        
    		double direction = gc.getAzimuth();
    		if ( direction < 0 )
    			direction = 360 + direction;

    		int rounding = Integer.valueOf(LocatorTool.ROUNDING_OPTIONS[0]);
    		if (displayOption.getRoundingToNearest() != null) {
    			rounding = displayOption.getRoundingToNearest();
    		}
    		String distanceOuput = LocatorTool.distanceDisplay(distance, 
    				rounding, displayOption.getDistanceUnit());
    		
    		String dirOutput = LocatorTool.directionDisplay(direction,
    				displayOption.getDirectionUnit());

    		return distanceOuput + " " + dirOutput + " " + list.get(index).getName();
    		
    	} else {
    		return list.get(index).getName();
    	} 
    	
    }
    
    /**
     *  Find the closest points from given point
     *  
     * @param ll
     * @return
     * @throws VizException
     * @throws RuntimeException
     * @throws IOException
     */
    public LocatorPointData[] getClosestPoints(Coordinate ll) 
		throws VizException, RuntimeException, IOException  {
    	/*
    	 * Check for invalid Coordinate
    	 */
    	if (ll.x > 180.0 || ll.x < -180.0 || ll.y > 90.0 || ll.y < -90.0)
    		return null;
	
    	/*
    	 * Retrieve all points within the envelope
    	 */
    	List<LocatorPointData> list = new ArrayList<LocatorPointData>();
    	    	
    	list = getPointDataList(attribute, ll, 
				true, 2 * POINT_INQUIRY_TOLERANCE, filterFactor );
	
    	/*
    	 * expand the envelope if no point inside
    	 */
    	int factorNum = 1;
    	boolean filter = true;
    	while (list.size() < MAX_POINT_NUM) {
    		filterFactor = factorNum * boxFactor;
		
    		if (factorNum > MaxFactorNum) filter = false;
    		
    		list.clear();
    		list = getPointDataList(attribute, ll, 
				filter, POINT_INQUIRY_TOLERANCE, filterFactor );
		
    		if (factorNum > MaxFactorNum || filter == false) break;
    		factorNum++;    		
    	}
	
    	if (list.size() < 1) return null;
	
    	/*
    	 * Compute distance (in meter) of the closest points
    	 */
    	LocatorPointData[] ptDataOut = new LocatorPointData[list.size()];
    	for (int ii = 0; ii < list.size(); ii++) {
		
    		GeodeticCalculator gc = new GeodeticCalculator(
    				DefaultEllipsoid.WGS84);
    		gc.setStartingGeographicPoint(ll.x, ll.y);
    		gc.setDestinationGeographicPoint(list.get(ii).getLon(), list.get(ii).getLat());            
    		double dist = -999.99;
    		dist = gc.getOrthodromicDistance();
    		//list.get(ii).setDistanceInMeter((int)dist);
        
    		double direction = -999.99;
    		if (dist > 0) {
    			direction = gc.getAzimuth();
    			if ( direction < 0 )
    				direction = 360 + direction;
    		}
    		//list.get(ii).setDir(direction);
    		
    		ptDataOut[ii] = new LocatorPointData();
    		ptDataOut[ii].setName(list.get(ii).getName());
    		ptDataOut[ii].setDistanceInMeter((int)dist);
    		ptDataOut[ii].setDir(direction);
    		ptDataOut[ii].setLat(list.get(ii).getLat());
    		ptDataOut[ii].setLon(list.get(ii).getLon());
        
    	}
	
    	// Sort the list by distance
    	Arrays.sort(ptDataOut, new LocatorPoitntDataComparator());
    	
    	// Only export top 25 closest points
    	int number = (ptDataOut.length > MAX_POINT_NUM) ? MAX_POINT_NUM : ptDataOut.length;
    	LocatorPointData[] out = new LocatorPointData[number];
    	for (int i = 0; i < number; i++) {
    		out[i] = new LocatorPointData();
    		out[i].setName(ptDataOut[i].getName());
    		out[i].setDistanceInMeter(ptDataOut[i].getDistanceInMeter());
    		out[i].setDir(ptDataOut[i].getDir());
    		out[i].setLat(ptDataOut[i].getLat());
    		out[i].setLon(ptDataOut[i].getLon());
    	}
    	
    	return out;
    }
    
    private class LocatorPoitntDataComparator implements Comparator<LocatorPointData> {

		public int compare(LocatorPointData o1, LocatorPointData o2) {
			return o1.getDistanceInMeter() - o2.getDistanceInMeter();
		}

	}
    
    
    public LocatorPointData[] getMatchedPoints(String prefix) 
    		throws VizException, RuntimeException, IOException  {

    	IndexedShapefileDataStore shapefileDataStore = null;
        FeatureIterator<SimpleFeature> featureIterator = null;
        DefaultQuery query = null;
    	File theshape = null;
    	theshape = new File(shapefile);
    	  	
		try {
			shapefileDataStore = new IndexedShapefileDataStore(
				        theshape.toURI().toURL(), null, false, true,
				        IndexType.QIX);
		} catch (MalformedURLException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}				       
        
        String[] types = shapefileDataStore.getTypeNames();   
        query = new DefaultQuery();
        query.setTypeName(types[0]);

   
        /*
         * read shapefile
         */ 
        featureIterator = shapefileDataStore.getFeatureSource().getFeatures(query)
        	.features();
        
        List<LocatorPointData> list = new ArrayList<LocatorPointData>();
        int len = prefix.length();
        while (featureIterator.hasNext()) {           
        	SimpleFeature f = featureIterator.next();
        	
        	LocatorPointData point = new LocatorPointData();

        	//get name
        	String name= f.getAttribute(attribute).toString().trim();	           
        	if (name.length() < 2 || name.length() < len) continue;
        	
        	//System.out.println(name.substring(0, len));
        	
        	if (name.substring(0, len).equalsIgnoreCase(prefix)) {
        		point.setName(name);
        		point.setStateID(f.getAttribute(stateIDAttr).toString().trim());

        		//get lat/lon
        		double lon = Double.valueOf(f.getAttribute("LON").
        				toString().trim()).doubleValue();
        		double lat = Double.valueOf(f.getAttribute("LAT").
        				toString().trim()).doubleValue();
        		point.setLon(lon);
        		point.setLat(lat);

        		list.add(point);     
        	}
        }
        System.out.println("point search ... list.size="+list.size());
        
        if (featureIterator != null) {
            featureIterator.close();
        }
        
        LocatorPointData[] ptDataOut = new LocatorPointData[list.size()];
        for (int i = 0; i < list.size(); i++) {
        	ptDataOut[i] = new LocatorPointData();
        	ptDataOut[i].setName(list.get(i).getName());
        	ptDataOut[i].setStateID(list.get(i).getStateID());
        	ptDataOut[i].setLat(list.get(i).getLat());
        	ptDataOut[i].setLon(list.get(i).getLon());
        }

    	
    	// Sort the list by name
    	Arrays.sort(ptDataOut, new PointDataNameComparator());

    	return ptDataOut;
    }
    
    private class PointDataNameComparator implements Comparator<LocatorPointData> {

		public int compare(LocatorPointData o1, LocatorPointData o2) {
			return o1.getName().compareTo(o2.getName());
		}

	}


	public String getStateIDAttr() {
		return stateIDAttr;
	}

	public void setStateIDAttr(String stateIDAttr) {
		this.stateIDAttr = stateIDAttr;
	}

    
}
