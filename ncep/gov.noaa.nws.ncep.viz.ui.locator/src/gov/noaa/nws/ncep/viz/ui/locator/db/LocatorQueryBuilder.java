package gov.noaa.nws.ncep.viz.ui.locator.db;

import java.util.*;

import gov.noaa.nws.ncep.viz.ui.locator.resource.*;

import com.vividsolutions.jts.geom.*;


/**
 * Building database queries based on the Locator.
 * @author gzhang
 *
 */
public class LocatorQueryBuilder {
	
	public static final String SEPERATOR = ",";
	
	public static final List<String> SMALL_ROW_NUMBER_LIST = 
		Arrays.asList(new String[]{"ANCHOR","VOR","MARINE","COASTAL","VOLCANO","ISLAND","NEXRAD","COUNTY","WR_QPF","TROPCY_BRKS"});
	
	private Locator locator;
	
	private Coordinate coor;
	
	private String[] dbFields;
	
	public LocatorQueryBuilder(){
		
	}
	
	public LocatorQueryBuilder(Locator l){
		locator = l;
		dbFields = getDBFields(locator);
	}
	
	public LocatorQueryBuilder(Locator l, Coordinate c){
		this(l);
		coor = c;
	}
	
	private String[] getDBFields(Locator l){
		if(l == null)
			return new String[]{};
		
		String s = l.getShapefilePath();//locator.getShapefilePath();
		
		if(s == null || s.isEmpty() || ! s.contains(SEPERATOR))
			return new String[]{};
		
		return s.split(SEPERATOR);
	}
	
	public String getQueryWithoutWhere(){
		
		StringBuilder sb = new StringBuilder();
		
		sb.append("select ");
		for(int i=0; i<dbFields.length; i++){
			sb.append(dbFields[i]);
			
			if(i != (dbFields.length-1) )
				sb.append(", ");			
		}
			
		sb.append(" from ").append(locator.getShapefileName());		
		
		return sb.toString();
	}
	
	public String getQueryWithoutWhere(Locator l){
		
		StringBuilder sb = new StringBuilder("");		
		
		String[] locatorDBFields = getDBFields(l);
		
		if(locatorDBFields==null || locatorDBFields.length==0)//2011-05-03
			return sb.toString();
		
		sb.append("select ");
		for(int i=0; i<locatorDBFields.length; i++){
			sb.append(locatorDBFields[i]);
			
			if(i != (locatorDBFields.length-1) )
				sb.append(", ");			
		}
			
		sb.append(" from ").append(l.getShapefileName());		
		
		return sb.toString();
	}
	
	public String getQueryWithBetweenLatOnly(Coordinate c, double corrLat){
		StringBuilder sb = new StringBuilder();
		
		sb.append( getQueryWithoutWhere() );		
		sb.append(" where ").append(dbFields[2]).append(" between ");
		
		double minLat= c.x-corrLat;
		double maxLat =c.x+corrLat;
		
		sb.append(""+minLat).append(" and ").append(""+maxLat);		
		
		return sb.toString();		
	}
	
	public String getQueryWithBetweenLatLon(Coordinate c, double corrLat, double corrLon){
		StringBuilder sb = new StringBuilder();
		
		sb.append( getQueryWithBetweenLatOnly(c,corrLat) );				
		sb.append(" and ").append(dbFields[3]).append(" between ");
		
		double minLon=c.y-corrLon;
		double maxLon=c.y+corrLon;
		
		sb.append(""+minLon).append(" and ").append(""+maxLon);
		
		return sb.toString();
	}
	
	public String getQuery(boolean isQuery2){
		
		if( isWithoutWhereClause()){		
			
			return getQueryWithoutWhere();
		}else {
			
			if( isQuery2)
				return getQueryWithBetweenLatOnly(coor, 1.5);
			else
				return getQueryWithBetweenLatLon(coor, 1.5, 5.0);
		}
			
	}
	
	/*
	 * some locators only have a few hundreds database rows
	 * and may be NOT located geographically evenly; thus 
	 * the where clause using min/max lat/lon is NOT needed
	 * and we just loop throught the whole records list.
	 */
	public boolean isWithoutWhereClause(){
						
		return SMALL_ROW_NUMBER_LIST.contains(locator.getLocatorName());		
	}

}
