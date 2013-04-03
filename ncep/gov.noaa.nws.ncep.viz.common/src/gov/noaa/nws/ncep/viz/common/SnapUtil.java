package gov.noaa.nws.ncep.viz.common;

import gov.noaa.nws.ncep.viz.common.dbQuery.NcDirectDbQuery;
import gov.noaa.nws.ncep.edex.common.stationTables.Station;
import gov.noaa.nws.ncep.edex.common.stationTables.StationTable;
import gov.noaa.nws.ncep.viz.localization.NcPathManager;
import gov.noaa.nws.ncep.viz.localization.NcPathManager.NcPathConstants;

import org.geotools.referencing.GeodeticCalculator;
import org.geotools.referencing.datum.DefaultEllipsoid;

import java.io.*;
import java.util.*;

import com.raytheon.uf.viz.core.catalog.DirectDbQuery.QueryLanguage;
import com.raytheon.uf.viz.core.exception.VizException;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;

/**
 * Utility class for snapping.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 02/2012		597			S. Gurung 	Initial Creation. 
 * 02/2012                  S. Gurung   Removed references to pgen  
 * 03/2012                  S. Gurung   Fixed a bug while getting VOR text
 * 11/2012		873			B. Yin		When snapping, check sigmet type to make sure
 * 										no space between distance and direction for CONV_SIGMET.
 *      
 * </pre>
 * 
 * @author	sgurung
 */

public class SnapUtil {
	
	public static List<Station> VOR_STATION_LIST;
	
	public static final String GFA_TEXT = new String( "GFA_TYPE" );
	
	public static final String[] DIRECT_ARRAY = new String[]{	"N","NNE","NE","ENE","E","ESE","SE","SSE",
		"S","SSW","SW","WSW","W","WNW","NW","NNW"};

	private static final Map<Integer, double[]> compassPtsAzimuths = new HashMap<Integer,double[]>();
	private static final Map<Integer, double[]> compassPtsAzimuthsMinus = new HashMap<Integer,double[]>();
	
	/*
	 * Nautical miles to meters
	 */
	public static final float NM2M = 1852.0f;
	
	static{		
		File stnFile = NcPathManager.getInstance().getStaticFile( 
				NcPathConstants.VORS_STN_TBL ); 
		VOR_STATION_LIST = new StationTable( stnFile.getAbsolutePath() ).getStationList();

		initCompassPtsAzimuths();
	}
		
	public static String getAzimuthInNSWEString(double azimuth){
		if(azimuth > -11.25 && azimuth <= 11.25) 	return DIRECT_ARRAY[0];
		if(azimuth > 11.25  &&  azimuth <= 33.75)  	return DIRECT_ARRAY[1];
		if(azimuth > 33.75  &&  azimuth <= 56.25)  	return DIRECT_ARRAY[2];
		if(azimuth > 56.25  &&  azimuth <= 78.75) 	return DIRECT_ARRAY[3];
		if(azimuth > 78.75  &&  azimuth <= 101.25)  return DIRECT_ARRAY[4];
		if(azimuth > 101.25 &&  azimuth <= 123.75) 	return DIRECT_ARRAY[5];
		if(azimuth > 123.75 &&  azimuth <= 146.25) 	return DIRECT_ARRAY[6];
		if(azimuth > 146.25 &&  azimuth <= 168.75) 	return DIRECT_ARRAY[7];
		if(azimuth > 168.75 ||  azimuth <= -168.75) return DIRECT_ARRAY[8];
		if(azimuth > -168.75&&  azimuth <= -146.25)	return DIRECT_ARRAY[9];
		if(azimuth > -146.25&&  azimuth <= -123.75)	return DIRECT_ARRAY[10];
		if(azimuth > -123.75&&  azimuth <= -101.25)	return DIRECT_ARRAY[11];
		if(azimuth > -101.25&&  azimuth <= -78.75) 	return DIRECT_ARRAY[12];
		if(azimuth > -78.75 &&  azimuth <= -56.25) 	return DIRECT_ARRAY[13];
		if(azimuth > -56.25 &&  azimuth <= -33.75) 	return DIRECT_ARRAY[14];
		else /* azimuth > -33.75&&<= -11.25 */		return DIRECT_ARRAY[15];
	}

	/**
	 * Parameters:
	 * 
	 * List<Coordinate> coors: 	the points to be snapped 	
	 * List<Station> stnList:		(NOT USED for reference: the stations to snap upon)	 
	 * int rounding:					5nm, 10nm,...
	 * boolean compassPts8:		true: 8-point; false: 16-point.
	 */
	
	public static ArrayList<Coordinate> getSnapWithStation(List<Coordinate> coors, 
						List<Station> stnList, int rounding, int numOfCompassPts){
		return getSnapWithStation(coors, stnList, rounding, numOfCompassPts, true);
	}

	/**
	 * Parameters:
	 * 
	 * @param coors <code>List<Coordinate></code> the points to be snapped 	
	 * @param stnList <code>List<Station></code> (NOT USED for reference: the stations to snap upon)	 
	 * @param rounding int 5nm, 10nm,...
	 * @param numOfCompassPts
	 * @param useJTS
	 * @return
	 */
	public static ArrayList<Coordinate> getSnapWithStation(List<Coordinate> coors, 
						List<Station> stnList, int rounding, int numOfCompassPts, boolean useJTS){
		
//		String nonSnappingMsg = "SOME OR ALL POINTS NOT SNAPPED, ORIGINAL POINTS USED!";
		
		if( ! SnapVOR.isSnappable(coors) || stnList==null) {
//			SnapVOR.openMsgBox(nonSnappingMsg);
			ArrayList<Coordinate> nlist = new ArrayList<Coordinate>();
			nlist.addAll( coors );
			return nlist;
		}
		
		ArrayList<Coordinate> snapPtsList = new ArrayList<Coordinate>();

		GeodeticCalculator gc = new GeodeticCalculator(DefaultEllipsoid.WGS84);		
		TreeMap<Double,Station> treeMap = new TreeMap<Double,Station>();	
	
		Coordinate coor = null;
		
		//coor NOT actually used, keep for reference.
		stnList=SnapVOR.getSnapStns(coor, numOfCompassPts);	
		
		for(int i=0; coors!=null && i<coors.size(); i++){// keep the order in snapPtsList
			coor = coors.get(i);
			
			populateStationsTreeMap(treeMap, stnList, gc, coor, useJTS);
			
			if(treeMap.isEmpty()) {																	
				
//				SnapVOR.openMsgBox(nonSnappingMsg);
				return (ArrayList<Coordinate>) coors;
			
			}else{//use pre-calculated VOR-snapping points, wrapped as Stations too.			
				
				double distance = treeMap.firstKey();
				Station vorStn = treeMap.get(distance);
				snapPtsList.add(new Coordinate(vorStn.getLongitude(),vorStn.getLatitude()));
			}
		
			treeMap.clear();
		}	
		
		return snapPtsList;
	}
	
	/**
	 * Populates treeMap with the stations sorted by distance.
	 * 
	 * @param treeMap
	 * @param stnList
	 * @param gc
	 * @param coor
	 * @param useJTS 
	 */
	public static void populateStationsTreeMap(TreeMap<Double, Station> treeMap,
			List<Station> stnList, GeodeticCalculator gc, Coordinate coor, boolean useJTS) {
		double geoDistance = Double.NaN;
		for (Station stn : stnList) {
			if(useJTS) {
				gc.setStartingGeographicPoint(stn.getLongitude(), stn.getLatitude());
				gc.setDestinationGeographicPoint(coor.x, coor.y);
				try {
					geoDistance = gc.getOrthodromicDistance();
				} catch (ArithmeticException e) {
					try {
						geoDistance = DefaultEllipsoid.WGS84.orthodromicDistance(stn.getLongitude(),
								stn.getLatitude(), coor.x, coor.y);
					} catch (ArithmeticException ee) {
						geoDistance = Double.NaN;
					}
				}
			} else {
				double dx = coor.x -stn.getLongitude();
				double dy = coor.y -stn.getLatitude();
				geoDistance = dx*dx + dy*dy;
			}

			treeMap.put(geoDistance, stn);// TODO: handle gc ArithmeticException

		}
	}
	
	/*
	 * Parameters:
	 * 
	 * List<Coordinate> coors: 		the points to be snapped 	
	 * List<Coordinate> coorList:	the points to snap upon in Coordinate format	 
	 * int rounding:				5nm, 10nm,...
	 * boolean compassPts8:			true: 8-point; false: 16-point.
	 */
	public static ArrayList<Coordinate> getSnapWithCoordinates(ArrayList<Coordinate> coors, 
						List<Coordinate> coorList, int rounding, int numOfCompassPts, boolean useJTS){
		
		List<Station> stnList = new ArrayList<Station>();
		
		for(Coordinate coor : coorList){
			Station s = new Station();
			s.setLongitude((float)coor.x);
			s.setLatitude((float)coor.y);
			stnList.add(s);
		}
		
		return getSnapWithStation(coors, stnList, rounding, numOfCompassPts, useJTS);
	}	
	
	/*
	 * parameters:
	 * 
	 * azimuth:			the actual direction
	 * numOfCompassPts: common Compass Point: 4/8/16/32, etc; a non-regular point will be coerced into 
	 * 					the closest regular one	
	 */
	
	public static double getSnapDir(double azimuth, int numOfCompassPts){
				
		double[] ap = getAzimuths(numOfCompassPts, true), am = getAzimuths(numOfCompassPts,false);
		TreeMap<Double, Double> treeMap = new TreeMap<Double,Double>();
		
		if(azimuth == 0){			
			return 0;			
		}else if(azimuth > 0){			
			for(double d : ap)	treeMap.put(Math.abs(d - azimuth),d);			
		}else{			
			for(double dd : am)	treeMap.put(Math.abs(dd - azimuth),dd);				
		}
		
		return treeMap.get(treeMap.firstKey());
	}
	
	/*
	 * parameters:
	 * 
	 * d: 			the actual distance to be rounded
	 * rounding:	10nm, 5nm, etc
	 */
	
	public static double getSnapDistance(double d, int rounding){		
		
		if(rounding <= 0) rounding = 10;//default
		
		int distance = (int)(d/NM2M);
		int nm = distance / rounding;
		double remain = distance % rounding;
		if(remain >= ( ((double)rounding)/2) ) nm++;
		
		return rounding*nm*NM2M;		
	}	
	
	private static void initCompassPtsAzimuths(){	

		compassPtsAzimuths.put(1, 		new double[]{0});
		compassPtsAzimuthsMinus.put(1, 	new double[]{0});

		compassPtsAzimuths.put(2, 		new double[]{0,180});
		compassPtsAzimuthsMinus.put(2, 	new double[]{0,-180});

		compassPtsAzimuths.put(4, 		new double[]{0,90,180});
		compassPtsAzimuthsMinus.put(4, 	new double[]{0,-90,-180});

		compassPtsAzimuths.put(6, 		new double[]{0,60,120,180});
		compassPtsAzimuthsMinus.put(6, 	new double[]{0,-60,-120,-180});

		compassPtsAzimuths.put(8, 		new double[]{0,45,90,135,180});
		compassPtsAzimuthsMinus.put(8, 	new double[]{0,-45,-90,-135,-180});

		compassPtsAzimuths.put(10, 		new double[]{0,36,72,108,144,180});
		compassPtsAzimuthsMinus.put(10, new double[]{0,-36,-72,-108,-144,-180});

		compassPtsAzimuths.put(12, 		new double[]{0,30,60,90,120,150,180});
		compassPtsAzimuthsMinus.put(12, new double[]{0,-30,-60,-90,-120,-150,-180});

		compassPtsAzimuths.put(16, 		new double[]{0,22.5,45,67.5,90,112.5,135,157.5,180});
		compassPtsAzimuthsMinus.put(16, new double[]{0,-22.5,-45,-67.5,-90,-112.5,-135,-157.5,-180});

		compassPtsAzimuths.put(18, 		new double[]{0,20,40,60,80,100,120,140,160,180});
		compassPtsAzimuthsMinus.put(18, new double[]{0,-20,-40,-60,-80,-100,-120,-140,-160,-180});
		
		compassPtsAzimuths.put(32, 
		new double[]{0,11.25,22.5,33.75,45,56.25,67.5,78.75,90,101.25,112.5,123.75,135,146.25,157.5,168.75,180});
		compassPtsAzimuthsMinus.put(32,
		new double[]{0,-11.25,-22.5,-33.75,-45,-56.25,-67.5,-78.75,-90,-101.25,-112.5,-123.75,-135,-146.25,-157.5,-168.75,-180});
		
	}
	
	private static int getCompassPoint(int pts){
		
		Set<Integer> keys = compassPtsAzimuths.keySet();
		
		if(keys.contains( pts ) ) 
			return pts;
		
		if(pts <= 0 || pts > 360) 
			return 8;//default;
		
		TreeMap<Integer, Integer> map = new TreeMap<Integer, Integer>();
		
		for(Integer i : keys)
			map.put(Math.abs(pts-i), i);
				
		return map.get(map.firstKey());		
	}

	
	public static double[] getAzimuths(int numOfCompassPts, boolean positive){

		int key = getCompassPoint(numOfCompassPts);

		return positive 
			? compassPtsAzimuths.get(key)
			: compassPtsAzimuthsMinus.get(key);
	}

	/**
	 * Creates a text from the coordinates with respect to VOR stations.  
	 * 
	 * @param coors
	 * @param vorConnector
	 * @param lineType
	 * @param numPerLines number of coordinates per line, if negative, then Integer.MAX_VALUE is used instead.
	 * @param isSnapped <code>boolean</code> if the coors have been snapped
	 * @return  VOR text string
	 */
	public static String getVORText(Coordinate[] coors, String vorConnector, String lineType, int numPerLines, boolean isSnapped) {
		return getVORText(coors, vorConnector, lineType, numPerLines, isSnapped, true, false );
	}
	
	/**
	 * Creates a text from the coordinates with respect to VOR stations.  
	 * 
	 * @param coors
	 * @param vorConnector
	 * @param lineType
	 * @param numPerLines number of coordinates per line, if negative, then Integer.MAX_VALUE is used instead.
	 * @param isSnapped <code>boolean</code> if the coors have been snapped
	 * @param useJTS <code>boolean</code> false to speed up the calculation (bypass the use of JTS library), true by default  
	 * @return  VOR text string
	 */
	public static String getVORText(Coordinate[] coors, String vorConnector, String lineType, int numPerLines, 
            boolean isSnapped, boolean useJTS, boolean isGfa ) {
		return getVORText(coors, vorConnector, lineType, numPerLines, isSnapped, true, isGfa, null);
	}
		
	/**
	 * Creates a text from the coordinates with respect to VOR stations.  
	 * 
	 * @param coors
	 * @param vorConnector
	 * @param lineType
	 * @param numPerLines number of coordinates per line, if negative, then Integer.MAX_VALUE is used instead.
	 * @param isSnapped <code>boolean</code> if the coors have been snapped
	 * @param useJTS <code>boolean</code> false to speed up the calculation (bypass teh use of JTS library), true by default 
	 * @param sigmetType the type of sigmet
	 * @return  VOR text string
	 */
	public static String getVORText(Coordinate[] coors, String vorConnector, String lineType, int numPerLines, 
			                        boolean isSnapped, boolean useJTS, boolean isGfa, String sigmetType) {
		
		if (lineType != null && lineType.startsWith("Line")) {
			coors = reorderLineCoordinates(coors);
		} else if ("Area".equals(lineType)) {
			coors = reorderAreaCoordinates(coors);
		}
		
		if( isSnapped ) 
			return SnapVOR.getSnapVORTxt(coors, vorConnector, lineType, useJTS, isGfa );
    	
		List<Station> list = VOR_STATION_LIST;
		GeodeticCalculator gc = new GeodeticCalculator(DefaultEllipsoid.WGS84);
		
		TreeMap<Double,Station> treeMap = new TreeMap<Double,Station>();
		ArrayList<VORStation> resultList = new ArrayList<VORStation>();
		
		for(Coordinate coor : coors){	

			populateStationsTreeMap(treeMap, list, gc, coor, useJTS);
			
			double distance = treeMap.firstKey();
			Station vorStn = treeMap.get(distance);
			gc.setStartingGeographicPoint(vorStn.getLongitude(), vorStn.getLatitude());
			gc.setDestinationGeographicPoint(coor.x, coor.y);
			
			String azimuth = getAzimuthInNSWEString(gc.getAzimuth());
			
			//resultList.add(new VORStation(vorStn.getStid(),azimuth,""+(int)(Math.round(distance/PgenUtil.NM2M))));
			 /*
		     *  Round distance to the nearest 10 nautical miles;
		     *  If convective outlook and less than 30 nm, set to 0.
		     */
			distance = getSnapDistance(distance,10)/NM2M; 			
			if ("OUTL_SIGMET".equals(sigmetType) && (int)distance < 30)
				distance = 0;
			
			resultList.add(new VORStation(vorStn.getStid(),azimuth,""+(int) distance));
			treeMap.clear();
		}
		
		if(numPerLines < 0) numPerLines=Integer.MAX_VALUE;
			
		StringBuilder result = new StringBuilder();		
		String first ="";
		int count = 0; 
		for(VORStation vs : resultList) {
			if ( isGfa ) {
				vs.setPgenType( GFA_TEXT );
			}
			else if ("CONV_SIGMET".equals(sigmetType) || "NCON_SIGMET".equals(sigmetType) || "OUTL_SIGMET".equals(sigmetType)){
				vs.setPgenType( sigmetType);
			}
			
			if(count==0) first = vs.toString();//first vor at the end for AREA
			result.append(vs.toString() + ( ((++count)%numPerLines)==0 ? "\n" : vorConnector ));
		}    	
		String resultString = "Area".equals(lineType) 
				?	result.append(first).toString()
				:	result.substring(0, result.lastIndexOf(vorConnector));;	
    	return resultString;
    }

	/*
	 * This function reorders (Line) processing of points to do either west-to-east 
	 * or north-to-south. West-to-east defined as all points within W2ELIM 
	 * degrees of one another.
	 * 
	 * @param coors the coordinates containing the lats and lons
	 * @return newCoors reordered coordinates  
	 * 
	 */
	public static Coordinate[] reorderLineCoordinates(Coordinate[] coors) {
		
		if (coors == null)
			return coors;
		
		final int N2S = 0, W2E = 1;
		final double W2ELIM = 0.1;
		
		int lineOrder = N2S;
		boolean reverse = false;
		double minLat, maxLat, dLat;
		int numPoints = coors.length;
				
		minLat = coors[0].y;
		maxLat = minLat;
		
		for ( Coordinate coor : coors ) {
			minLat = Math.min(minLat, coor.y);
			maxLat = Math.max(maxLat, coor.y);
		}
		
		dLat = Math.abs(maxLat - minLat);
		
		if ( dLat <= W2ELIM )
			lineOrder = W2E;
		
		if ( lineOrder == N2S && coors[0].y < coors[numPoints -1].y )
			reverse = true;
		if ( lineOrder == W2E && coors[0].x > coors[numPoints - 1].x )
			reverse = true;
		
		Coordinate[] newCoors = new Coordinate[numPoints];
		if ( reverse ) {
			for ( int i=0; i<numPoints; i++ ) {
				newCoors[i] = coors[numPoints -1 - i];
			}
		} else {
			newCoors = coors;
		}		
		
		return newCoors;
	}
	
	/*
	 * This function reorders a closed polygon into a clockwise fashion
	 * and the first point is the northernmost point.
	 * 
	 * @param coors the coordinates containing the lats and lons
	 * @return newCoors reordered coordinates  
	 * 
	 */
	public static Coordinate[] reorderAreaCoordinates(Coordinate[] coors) {
		
		if (coors == null)
			return coors;
		
		int numPoints = coors.length;
		int iptr = 0, i, j;
		double maxLat, dirNext, dirPrev;
		
		/*
		 *  Re-order processing of points to do northernmost first 
		 *  and proceed clockwise.
		 */
		maxLat = -90.0;
		for ( i=0; i<numPoints; i++ ) {
			Coordinate coor = coors[i];
			if ( coor.y > maxLat ) {
				iptr = i;
				maxLat = coor.y;
			}
		}
		
		/*
         *  Check directions for each adjacent point; the point with
         *  the smallest angle from north is in the clockwise direction.
         */
        if ( iptr != 0 )
        	dirPrev = getDirection (coors[iptr-1], coors[iptr]);
        else
        	dirPrev = getDirection (coors[numPoints-1], coors[iptr]);

        if ( iptr != numPoints-1 )
        	dirNext = getDirection (coors[iptr+1], coors[iptr]);
        else
        	dirNext = getDirection (coors[0], coors[iptr]);

        Coordinate[] newCoors = new Coordinate[numPoints];
       
        if ( dirNext < dirPrev )  {
		    i = 0;
		    for ( j = iptr; j < numPoints; j++ )  {
		    	newCoors[i] = coors[j];
		    	i++;
		    }
		    for ( j = 0; j < iptr; j++ )  {		    	
		    	newCoors[i] = coors[j];
		    	i++;
		    }
        }
        else  {
		    i = 0;
		    for ( j = iptr; j >= 0; j-- )  {   	
		    	newCoors[i] = coors[j];
		    	i++;
		    }
		    for ( j = numPoints-1; j > iptr; j-- )  {	    	
		    	newCoors[i] = coors[j];
		    	i++;
		    }
        }
		         
        return newCoors;
	}
	
	/*
	 * This function figures out the direction from point 1 to point 2.
	 * 
	 * @param coor1 coordinates for point 1
	 * @param coor2 coordinates for point 2
	 * @return dir direction from point 1 to point 2
	 * 
	 */
	public static double getDirection(Coordinate coor1, Coordinate coor2) {
		
		final double PI = 3.14159265,
	         		 HALFPI	= PI / 2.0,
	         		 RTD = 180.0 / PI,
	         		 DTR = PI / 180.0;

		double lat1d, lat2d, lon1d = 0.0,lon2d = 0.0;
		double dir, dLon, theta, alpha, val, tang;
		
		lat1d = (double) coor1.y * DTR;
		lat2d = (double) coor2.y * DTR;
		
		if (Math.abs(coor1.x - coor2.x) < 180.0F) {
			lon1d = ((double) (coor1.x + 360.0) % 360.0) * DTR;
			lon2d = ((double) (coor2.x + 360.0) % 360.0) * DTR;
		}
		
		dLon = lon1d - lon2d;
		
		if (Math.abs(lat2d - HALFPI) < 0.000001) {
			dir = 180.0F;
		}
		else if (Math.abs(-lat2d - HALFPI) < 0.000001) {
			dir = 0.0F;
		}
		else {
			val = (double)( Math.sin(lat1d) * Math.sin(lat2d) +
					Math.cos(lat1d) * Math.cos(lat2d) * Math.cos(dLon) );

			if  ( -1.0 <= val && val <= 1.0 )  {
			    theta = Math.acos(val);

			    if (Math.abs(theta - 0.0) < 0.000001)  {
			    	dir = 0.0F;
			    }
			    else  {
					tang = ( Math.sin(lat1d) - Math.sin(lat2d) * Math.cos(theta) ) /
			                        ( Math.cos(lat2d) * Math.sin(theta) );
					tang = Math.min( tang,  1.0 );
					tang = Math.max( tang, -1.0 );
					alpha = Math.acos( tang );
	
					dir = (float)(alpha*RTD);
					if ( dLon < 0.0 )  dir = 360.0 - dir;
			    }
			}
			else {
		           dir = 0.0;
			}
		}
		
		return dir;
	}
	
    public static class VORStation extends Station{
    	String name;
    	String azimuth;
    	String distance;
    	String pgenType="";
    	
    	VORStation(String n,String z, String d){name=n;azimuth=z;distance=d;}
    	public void setPgenType(String pt){ this.pgenType = pt;}
    	
    	public String toString() {
			if ("0".equals(distance)) {
				return name;
			}
			
			if("CONV_SIGMET".equalsIgnoreCase(pgenType) || 
					"NCON_SIGMET".equalsIgnoreCase(pgenType) ||
					"OUTL_SIGMET".equalsIgnoreCase(pgenType) ){
				return distance + azimuth + " " + name;
			}
			//For GFA - should be (30NNW LGC"), not "30 NNW LGC"
			else if ( GFA_TEXT.equalsIgnoreCase( pgenType ) ){
				return distance + azimuth + " " + name;
			}

			return distance + " " + azimuth + " " + name;
		}
    }
	
	/**
	 * class for the Snapping to pre-calculated VOR points. 
	 * 
	 * @author gzhang
	 *
	 */	
	public static class SnapVOR{
		
		//query string parts
		private static final String dbTable = "stns.snap";//"stns.snap_8";
		private static final String nameField = "station_name";
		private static final String latField = "latitude";
		private static final String lonField = "longitude";		
	
	    //for snapped point-to-vor lookup
	    public static final Map<Coordinate, VORStation> coorStnMap = new HashMap<Coordinate, VORStation>();
	    //envelops cannot cover all corner cases
	    private static List<Station> stnList = new ArrayList<Station>();
	    
	    static{	 	    	
	    	initSnapData();
	    	stnList.addAll(coorStnMap.values());
	    }  
	    
	    /**
	     * load the pre-calculted snapping VOR stations
	     * from the DB
	     * @return
	     */
	    public static void  initSnapData(){ 	    	
	    	List<Object[]> allBounds = null;
	    	StringBuilder query = new StringBuilder();	    	
    		
	    	query.append(
	    			"Select "
	    			+nameField 
	    			+ " , "
	    			+latField 
	    			+ " , " 
	    			+lonField 
	    			+ " FROM " 
	    			+ dbTable);
	    		
	    	try {
	    		allBounds = NcDirectDbQuery.executeQuery( 
	    						query.toString(), "ncep", QueryLanguage.SQL);
	    	}catch (VizException ve ){
	    		ve.printStackTrace();		    		
	    	}catch (Throwable te){
	    		System.out.println("ERROR: "+te.getMessage());
	    	}
	    	    	
	    	if(allBounds == null || allBounds.size() == 0){
	    		System.out.println("*** Error loading data! ");	 
	    		return;
	    	}
    	
	    	for(Object[] obs : allBounds){
	    		if(obs == null || obs.length < 3){	    			
	    			continue;
	    		}	    		
	    		
	    		String n = (String)obs[0];
	    		if(n == null){
	    			n = "";
	    		}else{
	    			if(n.contains("_")){
	    				n = n.substring(n.indexOf("_")+1);
	    			}
	    		}
	    		
	    		String azimuth = getVORNameDirDist((String)obs[0])[1];
	    		
	    		if(azimuth != null){			
	    			
		    		VORStation stn = new VORStation(
		    				getVORNameDirDist((String)obs[0])[0],
		    				azimuth,
		    				getVORNameDirDist((String)obs[0])[2]);
		    		
		    		stn.setLatitude(((Double)obs[1]).floatValue());
		    		stn.setLongitude(((Double)obs[2]).floatValue());
		    				    		
		    		Coordinate nCoor = new Coordinate( ((Double)obs[2]).floatValue(),((Double)obs[1]).floatValue());
					
		    		coorStnMap.put(nCoor, stn);
	    		}
	    	} 	    	
	    }	    
	   	    
	    /**
	     * parse the stn name to get name, dir, dist
	     * 
	     * @param String: station_name field from the snap table in ncep DB;
	     * @return String[]: elements: name,azimuth,distance;
	     */
	    private static String[] getVORNameDirDist(String n){
	    	String[] ndd = null;
	    	
	    	if(n == null){
    			ndd = new String[]{"","",""};
    		}else{
    			if(n.contains("_")){// i.e. 20N_YSJ
    				String name = n.substring(n.indexOf("_")+1);
    				
    				String dirDist = n.substring(0,n.indexOf("_"));
    				char[] c = dirDist.toCharArray();
    				int dirIndex = -1;
    				for(int i=0; i<c.length; i++){
    					if(c[i]=='N' || c[i]=='E' || c[i]=='S' || c[i]=='W' ){
    						dirIndex = i;
    						break;
    					}
    				}
    				
    				String dist = "";//n.contains("0") ? n.substring(0, n.indexOf("0")+1) : "";
    				String dir =  "";//n.substring(n.indexOf("0")+1, n.indexOf("_"));

    				if(dirIndex > 0 && dirIndex < n.indexOf("_")){
    					dist = n.substring(0, dirIndex);
    					dir  = n.substring(dirIndex, n.indexOf("_"));
    				}
    				
    				ndd = new String[]{name, dir, dist };
    				
    			}else{//i.e. YSJ
    				ndd = new String[]{n,"","" };
    			}
    		}
	    	
	    	return ndd;
	    }
	    
	    /**
	     * check the coordinate to get snap stations.
	     * @param stn: 				NOT USED (for Envlope:coordinate to be checked)
	     * @param numOfCompassPts:	8, 16,etc
	     * @return List<Station> 	all the snapping point stations
	     */
	    public static List<Station> getSnapStns(Coordinate stn, int numOfCompassPts){
	    	
	    	if(numOfCompassPts == 16) 
	    		return stnList;
	    	
	    	List<Station> list = stnList;
	    	List<Station> list8 = null;
	    	
	    	/*
	    	 * list8 calculated for space consummation consideration
	    	 */
	    	if(numOfCompassPts == 8){
	    		list8 = new ArrayList<Station>();
		    	for( Station s : list){
		    		String a = ((VORStation)s).azimuth;
		    		if( a != null && a.length() < 3 ){
		    			list8.add(s);
		    		}
		    	}		    			
		    	return list8;
	    	}
	    	
	    	//being defensive
	    	return new ArrayList<Station>();
	    }
	    	    
	    /**
	     * Write out snap points location in the format of a "lpi" file to be
	     * loaded as an overlay.
	     *  
	     * @param filename			file to be written into
	     */
	    public static void writeSnapStations ( String filename ){    	
	    	
	    	Writer output = null;	    	
	    	File file = new File( "/export/cdbsrv/jwu/workbak/pgen/snaps.txt" );
	    	try {
	    	    FileWriter fw = new FileWriter( file );
		    	output = new BufferedWriter( fw );

		    	for( Station s : stnList ){
			    	
		    		String c = new String( ((VORStation)s).getLatitude() + "\t" +
	    		                           ((VORStation)s).getLongitude() + "\t\t160\t\tnull\n" );
		    		output.write( c ); 
		    	}
		        
		    	output.close();
	    	
	    	}
	    	catch ( IOException e ) {
	    		e.printStackTrace();
	    	}
	    		    	
	    }

	    /**
	     * return the snapped VOR stations texts
	     * @param Coordinate[]: snapped points
	     * @param connector:    "-" or " TO "
	     * @param lineType:		Area or Line:ESOL
	     * @return String: 		VOR text
	     */
	    public static String getSnapVORTxt(Coordinate[] coors, String connector, String lineType, boolean useJts, 
	    		boolean isGfa ){
	    	String txt = "";
	    	String pgenType = "-".equals(connector) ? "CONV_SIGMET" : "";//2010-05-14 work around TODO:
	    	
	    	StringBuilder sb = new StringBuilder();
	    		
	    	for(int i=0; i<coors.length; i++){
	    		VORStation vStn = coorStnMap.get(coors[i]);//TODO: getVORStn(Coordinate stn, int numOfCompassPts)	    			
	    		
	    		//for out of range of VOR stations elements
	    		if( vStn == null){
	    			return getVORText(coors, connector, lineType, 6, false, useJts, isGfa );
	    		}
	    			
	    		vStn.setPgenType(pgenType);// for getting different string formatting
	    		if ( isGfa ) {
	    			vStn.setPgenType( GFA_TEXT );
	    		}
	    		sb.append(vStn.toString().trim());//TODO: text format differences
	    		sb.append(connector);
	    	}
	    	if("Area".equals(lineType)){
	    		txt = sb.append(sb.substring(0, sb.indexOf(connector))).toString();
	    	}else{
	    		txt = sb.toString().substring(0, sb.lastIndexOf(connector));
	    	}   	
	    	
	    	return txt;
	    }	    
	    	    	    	    
	    /**
	     * check if the points within reasonable(250NM) snapping range.
	     * @param coors: points to be checked
	     * @return: true snappable; false otherwise.
	     */
	    public static boolean isSnappable(List<Coordinate> coors){	    	
	    	
	    	if( coors == null || coors.size() == 0)
	    		return false;
	    	
	    	boolean snappable = true;
	    	
	    	/*
	    	 * check if the points located inside one of
	    	 * the three envelopes:
	    	 * continental US, Alask, and Hawaii.
	    	 */
	    	Envelope envContUS = new Envelope(-130,-60,20,55);
	    	//Envelope envAK   = new Envelope(180, -130, 45,75);//double check
	    	//Envelope envHA   = new Envelope(-165,-150,15,27); //double check
	    	
	    	for(Coordinate c : coors){
	    		if( ! envContUS.contains(c)){// && ! envAK.contains(c) && ! envHA.contains(c) ){
	    			snappable = false;
	    			break;
	    		}
	    	}
	    	return snappable;
	    }
	    		
	}		

	public static Coordinate getNorthMostPoint(Coordinate[] coors){
		if(coors == null || coors.length == 0) return new Coordinate();// or null?
		
		TreeMap<Double, Coordinate> map = new TreeMap<Double,Coordinate>();
		
		for(Coordinate coor : coors)	map.put(coor.y, coor);		
		
		return map.get(map.lastKey());
	}
}
