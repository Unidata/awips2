package gov.noaa.nws.ncep.edex.plugin.aww.util;

import gov.noaa.nws.ncep.common.dataplugin.aww.AwwLatlons;

import java.util.ArrayList;
import java.util.List;

import com.vividsolutions.jts.geom.Coordinate;

public class AwwLatLonUtil {
	
	public static List<AwwLatlons> getAwwLatLonsListBySereveWeatherStatusPointLine(String pointAndDirectionInfoString) {
		List<LatLonInfo> pointsInLatLonInfoList = getPointsInLatLonInfoList(pointAndDirectionInfoString); 
		int latlonsIndex = 1; 
		List<AwwLatlons> awwLatlonsList = new ArrayList<AwwLatlons>(); 
		for(LatLonInfo eachPointLatLonInfo : pointsInLatLonInfoList) {
			 AwwLatlons currentLatlons = new AwwLatlons();
			 currentLatlons.setIndex(latlonsIndex);
			 
			 currentLatlons.setLat((float)eachPointLatLonInfo.getLatInDecimal());
			 currentLatlons.setLon((float)eachPointLatLonInfo.getLonInDecimal());
			 awwLatlonsList.add(currentLatlons); 
			 latlonsIndex++; 
		}
		return awwLatlonsList; 
	}
	
	/**
	 * The input String is in the format of "25 ESE GNV  TO 20 SW JAX TO 40 NE SGJ"
	 * @param pointAndDirectionInfoString
	 * @return
	 */
	public static List<LatLonInfo> getPointsInLatLonInfoList(String pointAndDirectionInfoString) {
		System.out.println("@@@@@@@@==========, 1st line of getPointsInLatLonInfoList, the input pointAndDirectionInfoString="+pointAndDirectionInfoString); 
		List<LatLonInfo> pointsLatLonInfoList = new ArrayList<LatLonInfo>(); 
		if(!StringUtil.isStringEmpty(pointAndDirectionInfoString)) {
			String[] pointInfoStringArray = StringUtil.parseString(pointAndDirectionInfoString, "TO"); 
			for(String eachPointInfoString : pointInfoStringArray) {
				LatLonInfo pointLatLonInfo = getPointLatLonInfo(eachPointInfoString); 
				if(pointLatLonInfo != null)
					pointsLatLonInfoList.add(pointLatLonInfo); 
				else {
					System.out.println("============ within getPointsInLatLonInfoList with eachPointInfoString="+eachPointInfoString); 
					System.out.println("=============================== within getPointsInLatLonInfoList pointLatLonInfo = getPointLatLonInfo(eachPointInfoString) is NULL!!!!!"); 
				}
			}
		}
		
		return pointsLatLonInfoList; 
	}

	/**
	 * The point info String in the format of "25 ESE GNV", the 1st one is distance value, 
	 * the 2nd is direction string and the third one is airport ID
	 * @param eachPointInfoString
	 * @return
	 */
	public static LatLonInfo getPointLatLonInfo(String eachPointInfoString) {
		System.out.println("@@@@@@@@==========, 1st line of getPointLatLonInfo, the input eachPointInfoString="+eachPointInfoString); 
		String [] pointInfoStringArray = StringUtil.parseString(eachPointInfoString.trim(), " "); 
		if(pointInfoStringArray == null) {
			System.out.println("@@@@@@@@==========, within getPointLatLonInfo and the input eachPointInfoString="+
					eachPointInfoString + "   FIND pointInfoStringArray is NULL"); 
		} else {
			System.out.println("@@@@@@@@==========, within getPointLatLonInfo and the input eachPointInfoString="+
					eachPointInfoString + "   FIND pointInfoStringArray is NOTNOT NULL, the array size="+pointInfoStringArray.length); 
			for(int i=0; i<pointInfoStringArray.length; i++) {
				System.out.println("@@@@@@@@==========, within getPointLatLonInfo and the input eachPointInfoString="+
						eachPointInfoString + "  array["+i+"]="+pointInfoStringArray[i]); 
				
			}
		}
		
		
		LatLonInfo pointLatLonInfo = null; 
		try {
			String distanceString = pointInfoStringArray[0].trim(); 
			String directionString = pointInfoStringArray[1].trim(); 
			String airportIdString = pointInfoStringArray[2].trim(); 
			
			double distanceInDecimal = Double.parseDouble(distanceString); 
			double directionDegreeInDecimal = DirectionStringToDegreeConverter.getDegreeDecimal(directionString); 
			System.out.println("@@@@@@@@==========, before call AwwVtecDataUtil.retrieveAirportLatLonInfoByAirportId(...),  airportIdString="+airportIdString); 
			LatLonInfo airportLatLonInfo = AwwVtecDataUtil.retrieveAirportLatLonInfoByAirportId(airportIdString); 
			Coordinate airportCoordinate = new Coordinate(airportLatLonInfo.getLonInDecimal(), 
					airportLatLonInfo.getLatInDecimal(), 0); 
			Coordinate newPointCoordinate = CoordinateCalculationutil.getExtrapPoint(airportCoordinate, 
					directionDegreeInDecimal, distanceInDecimal); 
			if(newPointCoordinate != null) {
				pointLatLonInfo = new LatLonInfo(newPointCoordinate.y, newPointCoordinate.x); 
			}
		} catch(Exception e) {
			//do nothing
		}
		
		return pointLatLonInfo; 
	}
}
