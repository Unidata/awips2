/*
 * gov.noaa.nws.ncep.ui.pgen.sigmet.SigmetInfo
 * 
 * September 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.sigmet;

import gov.noaa.nws.ncep.viz.common.dbQuery.NcDirectDbQuery;
import gov.noaa.nws.ncep.edex.common.stationTables.Station;
import gov.noaa.nws.ncep.edex.common.stationTables.StationTable;
import gov.noaa.nws.ncep.ui.pgen.PgenSession;
import gov.noaa.nws.ncep.ui.pgen.PgenUtil;
import gov.noaa.nws.ncep.ui.pgen.attrDialog.SigmetCommAttrDlg;
import gov.noaa.nws.ncep.ui.pgen.elements.AbstractDrawableComponent;
import gov.noaa.nws.ncep.viz.localization.NcPathManager;
import gov.noaa.nws.ncep.viz.localization.NcPathManager.NcPathConstants;

import java.awt.geom.Point2D;
import java.io.*;
import java.util.*;

import org.geotools.data.DefaultQuery;
import org.geotools.data.shapefile.indexed.IndexedShapefileDataStore;
import org.geotools.feature.FeatureIterator;
import org.geotools.referencing.GeodeticCalculator;
import org.geotools.referencing.datum.DefaultEllipsoid;
import org.opengis.feature.simple.SimpleFeature;

import com.raytheon.uf.viz.core.catalog.DirectDbQuery.QueryLanguage;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineSegment;
import com.vividsolutions.jts.geom.LinearRing;
import com.vividsolutions.jts.geom.Polygon;

/**
 * Utility class for sigmet.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 09/09		160			G. Zhang 	Initial Creation. 
 * 12/09		182			G. Zhang	Added Snapping for ConvSigmet 
 * 05/11		?			J. Wu		Correct VOR text format for GFA
 * 07/11		?			J. Wu		Comment out the message box in 
 * 										SnapVor.getSnapWithStation()
 * 08/11		?			B. Yin		Fixed part of TTR 239.
 * 07/11        450         G. Hull     NcPathManager for station tables
 * 10/11		?			J. Wu		Fixed non-snappable points for outlook.
 *
 * </pre>
 * 
 * @author	gzhang
 */

public class SigmetInfo {
	
	public static final List<Station> VOR_STATION_LIST;
	public static final List<Station> VOLCANO_STATION_LIST;
	
	public static final String[] SIGMET_TYPES = new String[] {"INTL","CONV","NCON","AIRM","OUTL"};//should be consistent with plugin.xml
	public static final String GFA_TEXT = new String( "GFA_TYPE" );
	
	public static final Map<String,String[]> AREA_MAP = new HashMap<String,String[]>();
	public static final Map<String,String[]> ID_MAP   = new HashMap<String,String[]>();
	public static final Map<String,String[]> PHEN_MAP = new HashMap<String,String[]>();
	
	public static final String[] SPEED_ARRAY  = new String[]{	"5","10","15","20","25","30","35","40","45","50"};
	public static final String[] DIRECT_ARRAY = new String[]{	"N","NNE","NE","ENE","E","ESE","SE","SSE",
																"S","SSW","SW","WSW","W","WNW","NW","NNW"};
	public static final String[] FIR_ARRAY    = new String[]{	"PAZA|ANCHORAGE","KZHU|HOUSTON_OCEANIC","KZMA|MIAMI_OCEANIC",
															    "KZNY|NEW_YORK_OCEANIC","KZAK|OAKLAND_OCEANIC","TJZS|SAN_JUAN"};
	public static final String[] TREND_ARRAY  = new String[]{	"-none-","NC","WKN","INFSF" };
	public static final String[] REM_ARRAY	  = new String[]{ 	"-none-","BASED_ON_SATELLITE_OBS","BASED_ON_ACFT_AND_SAT",
																"BASED_ON_LATST_ADVSRY","BASED_ON_SAT_AND_LTG_OBS",
																"BASED_ON_SATELLITE_OBS_AND_LATEST_ADVSRY",
																"BASED_ON_LATEST_WASHINGTON_VAAC_ADVISORY",
																"BASED_ON_ACFT_RPT"};
	
	public static final String[] VOL_NAME_BUCKET_ARRAY = new String[]{"-Not_listed,_Enter_Name/Location-"
						,"AA-AM","AN-AZ","B","CA-CH","CI-CZ","D-E","F","G","H","I-J","KA-KH","KI-KZ","L"
						,"MA-MC","ME-MZ","N","O-P","Q-R","SA-SE","SF-SZ","TA-TH" ,"TI-TZ","U","V-Z"	};
	
	public static final Map<String, List<String>> VOLCANO_BUCKET_MAP; 
	
	//public static final Map<String, Polygon>  FIR_POLYGON_MAP;
	public static final List<Object[]> ALL_STATES;	
	
	private static final Map<Integer, double[]> compassPtsAzimuths = new HashMap<Integer,double[]>();
	private static final Map<Integer, double[]> compassPtsAzimuthsMinus = new HashMap<Integer,double[]>();
	
	static{
		
		File stnFile = NcPathManager.getInstance().getStaticFile( 
				NcPathConstants.VORS_STN_TBL ); 
		VOR_STATION_LIST = new StationTable( stnFile.getAbsolutePath() ).getStationList();

		stnFile = NcPathManager.getInstance().getStaticFile( 
				NcPathConstants.VOLCANO_STN_TBL );
		VOLCANO_STATION_LIST = new StationTable( stnFile.getAbsolutePath() ).getStationList();

		VOLCANO_BUCKET_MAP = initVolBucketMap();
		
		AREA_MAP.put(SIGMET_TYPES[0], new String[]{"KKCI","PHFO","PAWU" });
		AREA_MAP.put(SIGMET_TYPES[1], new String[]{"KMKC" });
		AREA_MAP.put(SIGMET_TYPES[2], new String[]{"KSFO","KSLC","KCHI","KDFW","KBOS","KMIA","PHNL","PANC","PAFA","PAJN" });
		AREA_MAP.put(SIGMET_TYPES[3], new String[]{"KSFO","KSLC","KCHI","KDFW","KBOS","KMIA","PHNL","PANC","PAFA","PAJN" });
		AREA_MAP.put(SIGMET_TYPES[4], new String[]{"KSFO","KSLC","KCHI","KDFW","KBOS","KMIA","PHNL","PANC","PAFA","PAJN" });
		
		
		ID_MAP.put(SIGMET_TYPES[0], new String[]{
		    	"ALFA","BRAVO","CHARLIE","DELTA","ECHO","FOXTROT","GOLF","HOTEL","INDIA","JULIETT",
		    	"KILO","LIMA","MIKE","NOVEMBER","OSCAR","PAPA","QUEBEC","ROMEO","SIERRA","TANGO",
		    	"UNIFORM","VICTOR","WHISKEY","XRAY","YANKEE","ZULU"});
		ID_MAP.put(SIGMET_TYPES[1], new String[]{"EAST","CENTRAL","WEST" });
		ID_MAP.put(SIGMET_TYPES[2], new String[]{"NOVEMBER","OSCAR","PAPA","QUEBEC","ROMEO","UNIFORM","VICTOR","WHISKEY","XRAY","YANKEE" });
		ID_MAP.put(SIGMET_TYPES[3], new String[]{"SIERRA","TANGO","ZULU" });
		ID_MAP.put(SIGMET_TYPES[4], new String[]{"EAST","CENTRAL","WEST" });
		
		PHEN_MAP.put(SIGMET_TYPES[0], new String[]{	"FRQ_TS","OBSC_TS","EMBD_TS","SQL_TS",
											"WDSPR_TS","ACT_TS","ISOL_SEV_TS","MOD_TO_SEV_CAT",
											"MOD_OCNL_SEV_TURB","MOD_OCNL_SEV_CAT_TURB",
											"MOD_TURB","SEV_TURB","OCNL-SEV_TURB",
											"SEV_ICE","SEV_ICE_(FZRA)",
											"ISOL_CB","OCNL_CB","FRQ_CB",
											"MOD-SEV_TURB","MOD-SEV_ICE","VOLCANIC_ASH","TROPICAL_CYCLONE",
											"TROPICAL_STORM","TROPICAL_DEPRESSION"});
		
		//FIR_POLYGON_MAP = initFirPolygonMapFromShapfile();//initFirPolygonMap();
		
		ALL_STATES = initAllStates();
		
		initCompassPtsAzimuths();
	}
	
	public static String getSigmetTypeString(String pgenType){
		if(pgenType == null || "".equals(pgenType)) return SIGMET_TYPES[0];
		for(String temp : SIGMET_TYPES){
			if(pgenType.contains(temp))
				return temp;
		}
		return SIGMET_TYPES[0];	//default INTL	
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
	
	private static Map<String, List<String>> initVolBucketMap(){
		Map<String, List<String>> result = new HashMap<String,List<String>>();
		
		List<Station> volcanoStnList = SigmetInfo.VOLCANO_STATION_LIST;
		ArrayList<String>  volcanoList = new ArrayList<String>();
		for(Station s : volcanoStnList)	volcanoList.add(s.getStnname());		
		
		java.util.Collections.sort(volcanoList);
		
		for(int i=0,j=0; i<volcanoList.size() && j<VOL_NAME_BUCKET_ARRAY.length; i++){//VOL_NAME_BUCKET_ARRAY
			if(i==0){ 			result.put(VOL_NAME_BUCKET_ARRAY[j], null);j++;}
			if(i>=1	 && i<15){	result.put(VOL_NAME_BUCKET_ARRAY[j], volcanoList.subList(0, 15));	if(i==1)j++;}
			if(i>=15 && i<35){ 	result.put(VOL_NAME_BUCKET_ARRAY[j], volcanoList.subList(15, 35));	if(i==15)j++;}
			if(i>=35 && i<55){ 	result.put(VOL_NAME_BUCKET_ARRAY[j], volcanoList.subList(35, 55));	if(i==35)j++;}
			if(i>=55 && i<70){ 	result.put(VOL_NAME_BUCKET_ARRAY[j], volcanoList.subList(55, 70));	if(i==55)j++;}
			if(i>=70 && i<82){ 	result.put(VOL_NAME_BUCKET_ARRAY[j], volcanoList.subList(70, 82));	if(i==70)j++;}
			if(i>=82 && i<102){ result.put(VOL_NAME_BUCKET_ARRAY[j], volcanoList.subList(82, 102));	if(i==82)j++;}
			if(i>=102 && i<112){result.put(VOL_NAME_BUCKET_ARRAY[j], volcanoList.subList(102, 112));if(i==102)j++;}
			if(i>=112 && i<129){result.put(VOL_NAME_BUCKET_ARRAY[j], volcanoList.subList(112, 129));if(i==112)j++;}
			if(i>=129 && i<138){result.put(VOL_NAME_BUCKET_ARRAY[j], volcanoList.subList(129, 138));if(i==129)j++;}
			if(i>=138 && i<157){result.put(VOL_NAME_BUCKET_ARRAY[j], volcanoList.subList(138, 157));if(i==138)j++;}
			if(i>=157 && i<174){result.put(VOL_NAME_BUCKET_ARRAY[j], volcanoList.subList(157, 174));if(i==157)j++;}
			if(i>=174 && i<198){result.put(VOL_NAME_BUCKET_ARRAY[j], volcanoList.subList(174, 198));if(i==174)j++;}
			if(i>=198 && i<216){result.put(VOL_NAME_BUCKET_ARRAY[j], volcanoList.subList(198, 216));if(i==198)j++;}
			if(i>=216 && i<235){result.put(VOL_NAME_BUCKET_ARRAY[j], volcanoList.subList(216, 235));if(i==216)j++;}
			if(i>=235 && i<249){result.put(VOL_NAME_BUCKET_ARRAY[j], volcanoList.subList(235, 249));if(i==235)j++;}
			if(i>=249 && i<264){result.put(VOL_NAME_BUCKET_ARRAY[j], volcanoList.subList(249, 264));if(i==249)j++;}
			if(i>=264 && i<286){result.put(VOL_NAME_BUCKET_ARRAY[j], volcanoList.subList(264, 286));if(i==264)j++;}
			if(i>=286 && i<305){result.put(VOL_NAME_BUCKET_ARRAY[j], volcanoList.subList(286, 305));if(i==286)j++;}
			if(i>=305 && i<327){result.put(VOL_NAME_BUCKET_ARRAY[j], volcanoList.subList(305, 327));if(i==305)j++;}
			if(i>=327 && i<351){result.put(VOL_NAME_BUCKET_ARRAY[j], volcanoList.subList(327, 351));if(i==327)j++;}
			if(i>=351 && i<365){result.put(VOL_NAME_BUCKET_ARRAY[j], volcanoList.subList(351, 365));if(i==351)j++;}
			if(i>=365 && i<380){result.put(VOL_NAME_BUCKET_ARRAY[j], volcanoList.subList(365, 380));if(i==365)j++;}
			if(i>=380 && i<395){result.put(VOL_NAME_BUCKET_ARRAY[j], volcanoList.subList(380, 395));if(i==380)j++;}
			if(i>=395 && i<415){result.put(VOL_NAME_BUCKET_ARRAY[j], volcanoList.subList(395, 415));if(i==395)j++;}
		}
		
		return result;
	}
	
	public static boolean isVolcanoNameEntered(String name){
		//if( name == null || "".equals(name.trim())) return false;
		
		Collection<List<String>> lists = VOLCANO_BUCKET_MAP.values();
		for(List<String> list : lists){
			if(list != null && list.contains(name))
				return false;
		}		
		return true;
	}
	

	
	public static Polygon getPolygon(double[] latlonArray, IMapDescriptor mapDescriptor){		
		Coordinate[] coorArray = new Coordinate[latlonArray.length/2];
		double[] point = new double[3];
		for(int i=0,j=0; i<latlonArray.length-1 && j<coorArray.length; i+=2,j++){
			point = mapDescriptor.worldToPixel(new double[]{latlonArray[i+1],latlonArray[i],0.0 });//lon , lat
			coorArray[j] = new Coordinate(point[0],point[1]);
		}	
		
		GeometryFactory gf = new GeometryFactory();			
		return gf.createPolygon(gf.createLinearRing(coorArray), new LinearRing[]{});		
	}
	
	public static Polygon getPolygon(Coordinate[] latlonArray, IMapDescriptor mapDescriptor){		
		Coordinate[] coorArray = latlonToPixelInCoor(latlonArray,mapDescriptor);//new Coordinate[latlonArray.length];
		if(! (coorArray.length > 3) ) coorArray = new Coordinate[]{};
		GeometryFactory gf = new GeometryFactory();			
		return gf.createPolygon(gf.createLinearRing(coorArray), new LinearRing[]{});		
	}
	
	public static Coordinate[] latlonToPixelInCoor(Coordinate[] coor, IMapDescriptor mapDescriptor){
		Coordinate[] result = new Coordinate[coor.length];		
		double[][] temp = PgenUtil.latlonToPixel(coor, mapDescriptor);
		for(int i=0; i<result.length; i++){
			result[i] = new Coordinate(temp[i][0],temp[i][1]);
		}
		return result;		
	}
	
	public static Coordinate latlonToPixelInCoor1(Coordinate coor, IMapDescriptor mapDescriptor){
		return latlonToPixelInCoor(new Coordinate[]{coor},mapDescriptor)[0];
	}
	
	public static Coordinate[] pixelsToCoorArray(double[][] pixels){
		Coordinate[] result = new Coordinate[pixels.length];
		for(int i=0; i<pixels.length; i++){
			result[i] = new Coordinate(pixels[i][0],pixels[i][1]);
		}
		return result;
	}

	
	public static Coordinate[] getIsolated(Coordinate vertex, double widthInNautical,IMapDescriptor mapDescriptor){  

		double[] tmp = { vertex.x, vertex.y, 0.0 };											
		double[] center = mapDescriptor.worldToPixel(tmp);	  								       
		
		GeodeticCalculator gc = new GeodeticCalculator(DefaultEllipsoid.WGS84);        
        gc.setStartingGeographicPoint(tmp[0], tmp[1]);
        gc.setDirection(0.0, widthInNautical);
        
        double[] tmp2 = { gc.getDestinationGeographicPoint().getX(), 
				gc.getDestinationGeographicPoint().getY(), 		0.0	};
        double[] circum = mapDescriptor.worldToPixel(tmp2);
		
		int numpts = 360;
		double axisAngle = 0.0;
		
	  	double cosineAxis = Math.cos( 	-Math.toRadians(axisAngle));
	  	double sineAxis = Math.sin( 	-Math.toRadians(axisAngle));

		ArrayList<Coordinate> list = new ArrayList<Coordinate>();
		
		double diff[] = { circum[0] - center[0], circum[1] - center[1] };
		double width = Math.sqrt( (diff[0]*diff[0]) + (diff[1]*diff[1]) );

		double angle = 0.0;

		for(int i=0; i<numpts; i++){
		  double thisSine = Math.sin(   -Math.toRadians(angle)); 
		  double thisCosine = Math.cos( -Math.toRadians(angle));		  
		  
		  double[] temp = mapDescriptor.pixelToWorld(
				  new double[]{	center[0]+ width * (cosineAxis * thisCosine - sineAxis * thisSine ),
						  		center[1]+ width * (sineAxis * thisCosine   + cosineAxis * thisSine)});
		  list.add(new Coordinate(temp[0],temp[1]));

		  angle += 1.0;
		}
		return list.toArray(new Coordinate[]{});
	}
	
	public static Polygon getIsolatedPolygon(Coordinate vertex, double widthInNautical, IMapDescriptor mapDescriptor){  
		Coordinate[] isolated = getIsolated(vertex, widthInNautical, mapDescriptor);
		Coordinate[] ip = new Coordinate[isolated.length+1];
		ip = (Coordinate[]) Arrays.copyOf(isolated, isolated.length);
		ip[ip.length-1] = isolated[0];
		return getPolygon( ip,mapDescriptor );
	}
	
	public static Polygon getSOLPolygon(Coordinate[] coors, String line, double width, IMapDescriptor mapDescriptor){		
		Coordinate[] ip = getSOLCoors(coors, line, width, mapDescriptor);
		Coordinate[] ipPlus = new Coordinate[ip.length+1];
		ipPlus = (Coordinate[]) Arrays.copyOf(ip, ipPlus.length);
		ipPlus[ipPlus.length-1] = ip[0];
		return getPolygon( ipPlus,mapDescriptor );		
	}
	
	public static Coordinate[] getSOLCoors(Coordinate[] pts, String lineType, double width, IMapDescriptor mapDescriptor){		
		GeodeticCalculator gc = new GeodeticCalculator(DefaultEllipsoid.WGS84);
		
		if( ! "ESOL".equals(lineType)){
			
			double azimuth = 0.0;
    		if("SOF".equalsIgnoreCase(lineType)){
    			azimuth = 180.0;
    		}else if("EOF".equalsIgnoreCase(lineType)){
    			azimuth = 90.0;
    		}else if("WOF".equalsIgnoreCase(lineType)){
    			azimuth = -90.0;
    		}			
    		Coordinate[] sides = new Coordinate[pts.length+2];
            sides[0] = pts[0];              
            sides[sides.length-1] = pts[pts.length-1];  
            
            for(int i=0; i<pts.length; i++){ 	
                gc.setStartingGeographicPoint(pts[i].x,pts[i].y);
                gc.setDirection(azimuth, width);
                Point2D jpt = gc.getDestinationGeographicPoint();
                sides[i+1] = new Coordinate(jpt.getX(),jpt.getY());
            }
            return sides;
			
		}else{//"ESOL"
			Coordinate[][] sides = getSides(pts, width);
			Coordinate[][] sidesWithArcIntsc = getSidesWithArcIntsc(mapDescriptor, pts, sides[0], sides[1]);
			
			Coordinate[] result = new Coordinate[sidesWithArcIntsc[0].length + sidesWithArcIntsc[1].length];
			System.arraycopy(sidesWithArcIntsc[0], 0, result, 0, sidesWithArcIntsc[0].length);
			System.arraycopy(sidesWithArcIntsc[1], 0, result, sidesWithArcIntsc[0].length, sidesWithArcIntsc[1].length);
			
			return result;
		}		 
	}
	
	public static double[] getAzimuth(Coordinate[] vertice){
		double[] azimuthS = new double[vertice.length];
		GeodeticCalculator gc = new GeodeticCalculator(DefaultEllipsoid.WGS84);		
		
		for(int i=0; i<vertice.length; i++){			    		
			if( i > 0 ){
				gc.setStartingGeographicPoint(vertice[i-1].x,vertice[i-1].y);
				gc.setDestinationGeographicPoint(vertice[i].x,vertice[i].y);
				azimuthS[ i-1 ] = gc.getAzimuth();	
					    		
				if( i==vertice.length-1 ){ azimuthS[i] = azimuthS[i-1];	}
			}			    		
		}
		
		return azimuthS;
	}
	
	public static Coordinate[][] getSides(Coordinate[] vertice, double attrSigLineWidth){
		double[] azimuthS = getAzimuth(vertice);
		GeodeticCalculator gc = new GeodeticCalculator(DefaultEllipsoid.WGS84);
		
		int sidesLength = vertice.length*2;
		Coordinate[] sides = new Coordinate[sidesLength], sidesOther = new Coordinate[sidesLength];
		
		double azimuth=0.0, a1=0.0, a2=0.0;
		
		for(int i=0; i < sidesLength; i++){ 

			if( i==0 ){ sides[i] = vertice[i];sidesOther[i] = vertice[i];continue;}
			if( i>2 && i==sidesLength-1 ){ sides[i] = vertice[vertice.length-1];sidesOther[i] = vertice[vertice.length-1];break;}
					    	
			gc.setStartingGeographicPoint(vertice[i/2].x,vertice[i/2].y);				    	
			azimuth =  azimuthS[(i-1)/2];
					    	
			a1 = (azimuth> 90.0 && azimuth<= 180.0) ? azimuth-270 : azimuth+90.0;
			gc.setDirection(a1, attrSigLineWidth);
			Point2D jpt = gc.getDestinationGeographicPoint();
			sides[i] = new Coordinate(jpt.getX(),jpt.getY());
					    	
			a2 = (azimuth< -90.0 && azimuth>= -180.0) ? azimuth+270 : azimuth-90.0;
			gc.setDirection(a2, attrSigLineWidth);
			Point2D jptOther = gc.getDestinationGeographicPoint();
			sidesOther[i] = new Coordinate(jptOther.getX(),jptOther.getY());    	
					    	
		}
		
		return new Coordinate[][]{sides,sidesOther};
	}
	
	public static Coordinate[][] getSidesWithArcIntsc(IMapDescriptor mapDescriptor, Coordinate[] vertice, Coordinate[] sides, Coordinate[] sidesOther){
		ArrayList<Coordinate> sidesList = new ArrayList<Coordinate>();
	    ArrayList<Coordinate> sidesOtherList = new ArrayList<Coordinate>();
	    sidesList.add(sides[0]); sidesOtherList.add(sidesOther[0]);
	    sidesList.add(sides[1]); sidesOtherList.add(sidesOther[1]);				    
	    
	    for(int i=1; i<sides.length-4;i=i+2 ){ //i<sides.length-4;i=i+2				    	
	    				    	
		    	LineSegment ls1 = new LineSegment(latlonToPixelInCoor1(sides[i],mapDescriptor),
		    										latlonToPixelInCoor1(sides[i+1],mapDescriptor));
		    	LineSegment ls2 = new LineSegment(latlonToPixelInCoor1(sides[i+2],mapDescriptor),
													latlonToPixelInCoor1(sides[i+3],mapDescriptor));
		    	Coordinate coor = ls1.intersection(ls2);
		    	
		    	LineSegment lsA = new LineSegment(latlonToPixelInCoor1(sidesOther[i],mapDescriptor),
													latlonToPixelInCoor1(sidesOther[i+1],mapDescriptor));
		    	LineSegment lsB = new LineSegment(latlonToPixelInCoor1(sidesOther[i+2],mapDescriptor),
													latlonToPixelInCoor1(sidesOther[i+3],mapDescriptor));
		    	Coordinate coor2 = lsA.intersection(lsB);   	
    		
		    	if(coor != null){	
		    		double[] aPixel = mapDescriptor.pixelToWorld(new double[]{coor.x, coor.y});
		    		coor = new Coordinate(aPixel[0],aPixel[1]);				    		
		    		sidesList.add(coor);				    		
		    	}else{
		    		ArrayList<Coordinate> list = getArcPath(mapDescriptor, vertice[(i+1)/2],sides[i+1],sides[i+2]);
		    		sidesList.add(list !=null && list.size() > 0 ? list.get(0) :sides[i+1]);
		    		sidesList.addAll( list);//getArcPath(mapDescriptor, vertice[(i+1)/2],sides[i+1],sides[i+2]) );
		    		sidesList.add(list !=null && list.size() > 0 ? list.get(list.size()-1) : sides[i+2]);				    		
		    	}
		    	
		    	if(coor2 != null){	
		    		double[] bPixel = mapDescriptor.pixelToWorld(new double[]{coor2.x, coor2.y});
		    		coor2 = new Coordinate(bPixel[0],bPixel[1]);
		    		sidesOtherList.add(coor2);
		    	}else{									
		    		ArrayList<Coordinate> lhList = getArcPath(mapDescriptor, vertice[(i+1)/2],sidesOther[i+1],sidesOther[i+2]);									
		    		sidesOtherList.add(lhList!=null && lhList.size()>0 ? lhList.get(lhList.size()-1) : sidesOther[i+1]);
		    		//ArrayList<Coordinate> lhList = getArcPath(mapDescriptor, vertice[(i+1)/2],sidesOther[i+1],sidesOther[i+2]);
		    		for(int ii=lhList.size()-1; ii>=0; ii--) sidesOtherList.add(lhList.get(ii));					    		
		    		sidesOtherList.add(lhList!=null && lhList.size()>0 ? lhList.get(0) :sidesOther[i+2]);
		    	}    		    	
	    }
	    sidesList.add(sides[sides.length-2]); 
	    sidesOtherList.add(sidesOther[sidesOther.length-2]);
	    
	    sidesList.add(sides[sides.length-1]); 
	    sidesOtherList.add(sidesOther[sidesOther.length-1]);
		
		return new Coordinate[][]{sidesList.toArray(new Coordinate[]{}), sidesOtherList.toArray(new Coordinate[]{})};
	}
	
	public static ArrayList<Coordinate> getArcPath(IMapDescriptor mapDescriptor, Coordinate vertex, Coordinate side1, Coordinate side2){  

		double[] tmp = { vertex.x, vertex.y, 0.0 };
		double[] tmp2 = { side1.x, side1.y, 0.0	};
		double[] tmp3 = { side2.x, side2.y, 0.0	};
	  	
		double[] center = mapDescriptor.worldToPixel(tmp);  	
	  	double[] circum = mapDescriptor.worldToPixel(tmp2);  
		double[] circum2 = mapDescriptor.worldToPixel(tmp3);       

		double axisAngle  = Math.atan2((circum[1]-center[1]),(circum[0]-center[0]));
		double axisAngle2 = Math.atan2((circum2[1]-center[1]),(circum2[0]-center[0]));
		
		axisAngle  = 360-Math.toDegrees((2*Math.PI+axisAngle)%(2*Math.PI));
		axisAngle2 = 360-Math.toDegrees((2*Math.PI+axisAngle2)%(2*Math.PI));
		int numpts = Math.abs( (int)Math.round(getAngExt(axisAngle,axisAngle2)));
		axisAngle = getStartAngle(axisAngle,axisAngle2);
		
	  	double cosineAxis = Math.cos( 	-Math.toRadians(axisAngle));
	  	double sineAxis = Math.sin( 	-Math.toRadians(axisAngle));

		ArrayList<Coordinate> list = new ArrayList<Coordinate>();
		
		double diff[] = { circum[0] - center[0], circum[1] - center[1] };
		double width = Math.sqrt( (diff[0]*diff[0]) + (diff[1]*diff[1]) );

		double angle = 0.0;

		for(int i=0; i<numpts; i++){
		  double thisSine = Math.sin(   -Math.toRadians(angle)); 
		  double thisCosine = Math.cos( -Math.toRadians(angle));		  
		  
		  double[] temp = mapDescriptor.pixelToWorld(
				  new double[]{	center[0]+ width * (cosineAxis * thisCosine - sineAxis * thisSine ),
						  		center[1]+ width * (sineAxis * thisCosine   + cosineAxis * thisSine)});
		  list.add(new Coordinate(temp[0],temp[1]));

		  angle += 1.0;
		}
		return list;
	}

    public static double getStartAngle(double a1, double a2){
    	if(a2 < a1){
    		if(a1-a2 <= 180.0){
    			return a2;
    		}else{
    			return a1;
    		}
    	}else{
    		if(a2-a1 <= 180.0){
    			return a1;
    		}else{
    			return a2;
    		}
    	}
    }
    
    public static double getAngExt(double a1, double a2){
    	double diff = a2 - a1;
    	if(Math.abs(diff) <= 180.0) return -Math.abs(diff);
    	else {
    		return Math.abs(diff)-360;
    	}
    }
    
    public static boolean getAFOSflg(){
    	String sigmetFMT = System.getenv("SIGMETFMT");
    	
    	if(sigmetFMT == null) return false;
    	return "AFOS".equals(sigmetFMT);    	
    }
    
    public static Map<String, Polygon> initFirPolygonMapFromShapfile(){
    	Map<String, Polygon> result = new HashMap<String, Polygon>();		
		IMapDescriptor mapDescriptor = PgenSession.getInstance().getPgenResource().getDescriptor();
		
		HashMap<String,Coordinate[]> firGeoMap = getGeometriesFromShapefile();
		
		for(String firId : firGeoMap.keySet()){
			Coordinate[] coors = firGeoMap.get(firId);
			result.put(firId, SigmetInfo.getPolygon(coors, mapDescriptor) );
		}
    	
    	
		return result;
    }
    
    /*
	 * based on gov.noaa.ncep.ui.locator.LocatorShapefileResource
	 */
	private static HashMap<String,Coordinate[]>  getGeometriesFromShapefile() {		
		
    	String FILE_NAME =
    		 NcPathManager.getInstance().getStaticFile( 
    					NcPathConstants.PGEN_FIR_BOUNDS ).getAbsolutePath();
		String[] LABEL_ATTR = new String[]{"FIR_ID"};
				
		FeatureIterator<SimpleFeature> featureIterator = null;	        
        HashMap<String,Coordinate[]> firGeoMap = new HashMap<String,Coordinate[]>();
        IndexedShapefileDataStore shapefileDataStore=null;
        String shapeField=null;
        
        try{
        	File file = new File(FILE_NAME);          
            shapefileDataStore = new IndexedShapefileDataStore(file.toURI()
                    .toURL(), null, false, true, org.geotools.data.shapefile.indexed.IndexType.QIX);            
            
            shapeField = shapefileDataStore.getFeatureSource().getSchema().getGeometryDescriptor().getLocalName();   
        }catch(Exception e){
        	System.out.println("------- Exception: "+e.getMessage());
        }
     
        String[] labelFields = LABEL_ATTR;
        
        try {	        	
            
            String[] types = shapefileDataStore.getTypeNames();
            DefaultQuery query = new DefaultQuery();
            query.setTypeName(types[0]);

            String[] fields = new String[labelFields.length+1];
            for (int i = 0; i < labelFields.length; i++) {
                fields[i] = labelFields[i];
            }
            fields[labelFields.length] = shapeField;            
            
            query.setPropertyNames(fields);	                        
            featureIterator = shapefileDataStore.getFeatureSource().getFeatures(query).features();	           

            while (featureIterator.hasNext()) {
                SimpleFeature f = featureIterator.next();
                Geometry g = (Geometry) f.getDefaultGeometry();
                firGeoMap.put(f.getAttribute("FIR_ID").toString(), g.getCoordinates());   					
            }

        } catch (Exception e) {
        	System.out.println("---------Exception: "+e.getMessage());	            	            
        } finally {   if (featureIterator != null) { featureIterator.close();  }  }
		
		return firGeoMap;
	}	
	

//---------------------------------------------------snapping for ConvSigmet
	
	
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
		
		//Date d1 = new Date();
		
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
		
		//Date d2 = new Date(); System.out.println("^^^^^^^^^^^^ time : "+(d2.getTime()-d1.getTime()));
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
		
		int distance = (int)(d/PgenUtil.NM2M);
		int nm = distance / rounding;
		double remain = distance % rounding;
		if(remain >= ( ((double)rounding)/2) ) nm++;
		
		return rounding*nm*PgenUtil.NM2M;		
	}	
	
	public static Coordinate getNorthMostPoint(Coordinate[] coors){
		if(coors == null || coors.length == 0) return new Coordinate();// or null?
		
		TreeMap<Double, Coordinate> map = new TreeMap<Double,Coordinate>();
		
		for(Coordinate coor : coors)	map.put(coor.y, coor);		
		
		return map.get(map.lastKey());
	}
	
	public static List<Object[]> initAllStates(){
		List<Object[]> list = null;		
		String query = "Select AsBinary(the_geom_0_016), state FROM mapdata.states";
		
		try {
			list = NcDirectDbQuery.executeQuery( query, "maps", QueryLanguage.SQL);
		}
		catch (Exception e ){
			System.out.println("-----DB exception: "+e.getMessage());
			list = new ArrayList<Object[]>();
		}
		
		return list;
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
	 * @param useJTS <code>boolean</code> false to speed up the calculation (bypass teh use of JTS library), true by default  
	 * @return  VOR text string
	 */
	public static String getVORText(Coordinate[] coors, String vorConnector, String lineType, int numPerLines, 
			                        boolean isSnapped, boolean useJTS, boolean isGfa ) {
		
		if( isSnapped ) 
			return SnapVOR.getSnapVORTxt(coors, vorConnector, lineType, useJTS, isGfa );
    	
		List<Station> list = SigmetInfo.VOR_STATION_LIST;
		GeodeticCalculator gc = new GeodeticCalculator(DefaultEllipsoid.WGS84);
		
		TreeMap<Double,Station> treeMap = new TreeMap<Double,Station>();
		ArrayList<VORStation> resultList = new ArrayList<VORStation>();
		
		for(Coordinate coor : coors){	

			populateStationsTreeMap(treeMap, list, gc, coor, useJTS);
			
			double distance = treeMap.firstKey();
			Station vorStn = treeMap.get(distance);
			gc.setStartingGeographicPoint(vorStn.getLongitude(), vorStn.getLatitude());
			gc.setDestinationGeographicPoint(coor.x, coor.y);
			
			String azimuth = SigmetInfo.getAzimuthInNSWEString(gc.getAzimuth());
			
			resultList.add(new VORStation(vorStn.getStid(),azimuth,""+(int)(Math.round(distance/PgenUtil.NM2M))));
			treeMap.clear();
		}
		
		if(numPerLines < 0) numPerLines=Integer.MAX_VALUE;
			
		StringBuilder result = new StringBuilder();		
		String first ="";
		int count = 0; 
		for(VORStation vs : resultList) {
			if ( isGfa ) vs.setPgenType( GFA_TEXT );
			if(count==0) first = vs.toString();//first vor at the end for AREA
			result.append(vs.toString() + ( ((++count)%numPerLines)==0 ? "\n" : vorConnector ));
		}    	
		String resultString = "Area".equals(lineType) 
				?	result.append(first).toString()
				:	result.substring(0, result.lastIndexOf(vorConnector));;	
    	return resultString;
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
			
			if(ConvSigmet.SIGMET_PGEN_TYPE.equals(pgenType))
				return distance + azimuth + " " + name;
			
			//For GFA - should be (30NNW LGC"), not "30 NNW LGC"
			if ( GFA_TEXT.equals( pgenType ) )
				return distance + azimuth + " " + name;

			return distance + " " + azimuth + " " + name;
		}
    }
	
	public static double[][] getESOLArea( Coordinate[] side1, Coordinate[] side2, IMapDescriptor map){
		Coordinate[] sides = new Coordinate[side1.length + side2.length];
		
		System.arraycopy(side1,0,sides,0, side1.length);
		
		List<Coordinate> list = Arrays.asList(side2);
		Collections.reverse(list);
		
		System.arraycopy(list.toArray(new Coordinate[]{}),0,sides,side1.length,side2.length);
			
		GeometryFactory gf = new GeometryFactory();
		return PgenUtil.latlonToPixel(gf.createPolygon(gf.createLinearRing(sides),null).getCoordinates(),map);
		//return PgenUtil.latlonToPixel(sides, map);
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
	     * @param lineType:		AREA or ESOL
	     * @return String: 		VOR text
	     */
	    public static String getSnapVORTxt(Coordinate[] coors, String connector, String lineType, boolean useJts, 
	    		boolean isGfa ){
	    	String txt = "";
	    	String pgenType = "-".equals(connector) ? ConvSigmet.SIGMET_PGEN_TYPE : "";//2010-05-14 work around TODO:
	    	
	    	StringBuilder sb = new StringBuilder();
	    		
	    	for(int i=0; i<coors.length; i++){
	    		VORStation vStn = coorStnMap.get(coors[i]);//TODO: getVORStn(Coordinate stn, int numOfCompassPts)	    			
	    		
	    		//for out of range of VOR stations elements
	    		if( vStn == null){
	    			return SigmetInfo.getVORText(coors, connector, lineType, 6, false, useJts, isGfa );
	    		}
	    			
	    		vStn.setPgenType(pgenType);// for getting different string formatting
	    		if ( isGfa ) {
	    			vStn.setPgenType( GFA_TEXT );
	    		}
	    		sb.append(vStn.toString().trim());//TODO: text format differences
	    		sb.append(connector);
	    	}
	    	if(SigmetCommAttrDlg.AREA.equals(lineType)){
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
	    
	    /**
	     * open a Message Box informing the user
	     * @param s: the message
	     */
	    public static void openMsgBox(String s){
	    	VaaInfo.openMsgDlg(s);
	    }
		
	}	
	
    /**
     * check if the adc needs snapping
     * @param adc: the element to be checked
     * @return: true: snapping needed; false not needed
     */
    public static boolean isSnapADC(AbstractDrawableComponent adc){
        String pt = adc.getPgenType();
        return "CONV_SIGMET".equals(pt) || "NCON_SIGMET".equals(pt);
    }
    
    /**
     * get the num of compass points for snapping need
     * @param adc: the element to be snapped
     * @return int: the num of compass points for snapping
     */
    public static int getNumOfCompassPts(AbstractDrawableComponent adc){
        String pt = adc.getPgenType();
        if("CONV_SIGMET".equals(pt) || "NCON_SIGMET".equals(pt)){
        	return 8;
        }
        return 16;
    }	
	
}
