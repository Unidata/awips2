package gov.noaa.nws.ncep.edex.plugin.geomag;

import gov.noaa.nws.ncep.common.dataplugin.geomag.GeoMagRecord;
import gov.noaa.nws.ncep.common.dataplugin.geomag.dao.GeoMagDao;
import gov.noaa.nws.ncep.common.dataplugin.geomag.exception.GeoMagException;
import gov.noaa.nws.ncep.common.dataplugin.geomag.table.GeoMagStation;
import gov.noaa.nws.ncep.common.dataplugin.geomag.table.Group;
import gov.noaa.nws.ncep.common.dataplugin.geomag.util.GeoMagStationLookup;
import gov.noaa.nws.ncep.common.dataplugin.geomag.util.TableTimeStamp;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.edex.plugin.AbstractDecoder;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.time.DataTime;

/**
 * This java class decodes geomagnetic data.
 * 
 * <pre>
 * 
 * OFTWARE HISTORY
 *                   
 * ate          Ticket#     Engineer    Description
 * -----------  ----------  ----------- --------------------------
 * 03/27/2013   #975        sgurung     Initial Creation
 * 04/26/2013   #975        qzhou       Added unit checkup. Declared missingVal.
 * </pre>
 * 
 * @author sgurung
 * @version 1
 * */

public class GeoMagDecoder extends AbstractDecoder {
	
	private String pluginName;
	
	private GeoMagDao dao;
	
	private final Log logger = LogFactory.getLog(getClass());
			
	private SimpleDateFormat obsTimeDateFormat = new SimpleDateFormat("yyyy-MM-dd");
	
	private static final String STATION_CODE = "stationCode";
	
	private static final String OBS_DATE = "obsDate";
	
	private static final String OBS_YEAR = "obsYear";
	
	private static final String OBS_TIME = "obsTime";
	
	private static final String OBS_HOUR = "obsHour";
	
	private static final String OBS_MINUTE = "obsMinute";	

	private static final String OBS_MINUTE_NUM = "obsMinuteNum";
	
	private static final String OBS_DAY_OF_YEAR = "obsDayOfYear";
	
	private static final String SOURCE = "source";
	
	private static final String COMPONENT_1 = "component1";
	
	private static final String COMPONENT_2 = "component2";
	
	private static final String COMPONENT_3 = "component3";
	
	private static final String COMPONENT_4 = "component4";
	
	private static final String UNIT = "unit";
	
	private static final float missingVal = 99999;
	
	
	public GeoMagDecoder(String name) {
        pluginName = name;
    }

	 public PluginDataObject[] decode(File file) throws Exception {

		PluginDataObject[] retData = null;

        GeoMagRecord record = null;
          
        List<Long> obsTimesList = new ArrayList<Long>();
        List<Float> comp1List = new ArrayList<Float>();
        List<Float> comp2List = new ArrayList<Float>();
        List<Float> comp3List = new ArrayList<Float>();  
        List<Float> comp4List = new ArrayList<Float>();        
        
        if (file == null || (file.length() < 1))
            return new PluginDataObject[0];
        
        BufferedReader in = null;
        
        try{
    	   String input;
    	   in = new BufferedReader(new FileReader(file));
    	   record = new GeoMagRecord();
    	   
    	   // get station code from the file name
    	   String stationCode = file.getName().substring(0,3).toUpperCase();
    	   
    	   // for Hartland (HAD) data, filename does not have full station code
    	   if (stationCode.startsWith("HA"))
    		   stationCode = "HAD";
    	   // for Korea (JEJ) data, filename does not have the exact station code
    	   else  if (stationCode.startsWith("M"))
    		   stationCode = "JEJ";  
    	   
    	   // get the station detail from metadata file 'geoMagStations.xml' 
   		   GeoMagStation station = getStationDetail(stationCode);  
    	   
    	   if (station == null) {
    		  logger.error("Error decoding geomag file! Station code not found in geoMagStations.xml file.");
    		  return new PluginDataObject[0];
    	   }
    	   
    	   String source = (station.getSource()!=null)?station.getSource()[0]:null;
    	   
    	   boolean containsHeader = (station.getRawDataFormat().getHeaderFormat()!=null)?true:false;
    	   boolean containsData = (station.getRawDataFormat().getDataFormat()!=null)?true:false;
    	   
    	   Pattern HEADER_EXP = null;
    	   Pattern DATA_EXP = null;
    	   boolean conversionRequired = false;
    	   HashMap<String, Group> headerGroupMap = new HashMap<String, Group>();
     	   HashMap<String, Group> dataGroupMap = new HashMap<String, Group>();
    	   
     	   /* Get regular expression for the header from the station metadata file */
    	   if (containsHeader) {
    		   HEADER_EXP = Pattern.compile(station.getRawDataFormat().getHeaderFormat().getPattern());
    		 
        	   Group[] headerGroup = station.getRawDataFormat().getHeaderFormat().getGroup();
        	   
        	   if (headerGroup != null) {
        		   for (Group group: headerGroup) {
        			   headerGroupMap.put(group.getName(), group);
        		   }
        	   }
    	   }
    	   
    	   /* Get regular expression for the data from the station metadata file */
    	   if (containsData) {
    		   DATA_EXP = Pattern.compile(station.getRawDataFormat().getDataFormat().getPattern());
    		   Group[] dataGroup = station.getRawDataFormat().getDataFormat().getGroup();
        	   
        	   if (dataGroup != null) {
        		   for (Group group: dataGroup) {
        			   dataGroupMap.put(group.getName(), group);
        		   }
        	   }
        	   
        	   conversionRequired = station.getRawDataFormat().getDataFormat().getConversionRequired();
    	   }    
    	    
    	   boolean firstLine = true;
    	   String unit = "";
    	   while((input = in.readLine()) != null){  
    			
    		    int groupId = -1;    		    
			   
			    if (firstLine && containsHeader) {
			    	/* if this is the first line and header exists, parse the header information */
			    	
	    	        Matcher headerMatcher = HEADER_EXP.matcher(input);
	
	    	        if (headerMatcher.find()) {
	
	    	        	// set the station code
	    	        	groupId = (headerGroupMap.get(STATION_CODE) != null)? headerGroupMap.get(STATION_CODE).getId():-1;
	    	            if (groupId != -1)
	    	            	record.setStationCode(headerMatcher.group(groupId));
	    	            
	    	            // set the source
	    	            groupId = (headerGroupMap.get(SOURCE) != null)? headerGroupMap.get(SOURCE).getId():-1;
	    	            if (groupId != -1) {
	    	            	source = headerMatcher.group(groupId);	    	            	
	    	            }
	    	            
	    	            // set the source
	    	            groupId = (headerGroupMap.get(UNIT) != null)? headerGroupMap.get(UNIT).getId():-1;
	    	            if (groupId != -1) {
	    	            	unit = headerMatcher.group(groupId);	    	            	
	    	            }
	    	            
	    	            record.setDataTime(getRecordDataTime(headerMatcher, headerGroupMap));
	    	        }
			    } 
			   
			    if (containsData) {
			    	/* if data exists, parse the data information */
			    	
	    	        Matcher dataMatcher = DATA_EXP.matcher(input);
	
	    	        if (dataMatcher.find()) {
	    	        	
	    	        	/* if this is the first line and header does not exist */
	    	        	if (firstLine && !containsHeader) {
		    	        	// set the station code, if it exists in the data section
		    	        	groupId = (dataGroupMap.get(STATION_CODE) != null)? dataGroupMap.get(STATION_CODE).getId():-1;
		    	            if (groupId != -1)
		    	            	record.setStationCode(dataMatcher.group(groupId));	 
	
		    	            record.setDataTime(getRecordDataTime(dataMatcher, dataGroupMap));
	    	        	}
	    	        	
	    	        	// set the observation time for the minute data
	    	        	Calendar obsTime = getObsTime(dataMatcher, dataGroupMap, record);
	    	        	obsTimesList.add(obsTime.getTimeInMillis());
	    	        	
	    	        	/* get and set the component values (h or x, d or y ,z, f) */
	    	        	Float comp1Val = null;
	    	        	Float comp2Val = null;
	    	        	Float comp3Val = null;
	    	        	Float comp4Val = null;
	    	        	
	    	        	String comp1RefersTo = null;
	    	        	String comp2RefersTo = null;
	    	        	String comp3RefersTo = null;
	    	        	String comp4RefersTo = null;
	    	        	
	    	        	groupId = (dataGroupMap.get(COMPONENT_1) != null)? dataGroupMap.get(COMPONENT_1).getId():-1;
	    	            if (groupId != -1) {
	    	            	comp1RefersTo = dataGroupMap.get(COMPONENT_1).getRefersTo();
	    	            	comp1Val = Float.parseFloat(dataMatcher.group(groupId));
	    	            }
	    	            
	    	            groupId = (dataGroupMap.get(COMPONENT_2) != null)? dataGroupMap.get(COMPONENT_2).getId():-1;
	    	            if (groupId != -1) {
	    	            	comp2RefersTo = dataGroupMap.get(COMPONENT_2).getRefersTo();
	    	            	comp2Val = Float.parseFloat(dataMatcher.group(groupId));
	    	            }
	    	            
	    	            groupId = (dataGroupMap.get(COMPONENT_3) != null)? dataGroupMap.get(COMPONENT_3).getId():-1;
	    	            if (groupId != -1)  {
	    	            	comp3RefersTo = dataGroupMap.get(COMPONENT_3).getRefersTo();
	    	            	comp3Val = Float.parseFloat(dataMatcher.group(groupId));
	    	            }
	    	            
	    	            groupId = (dataGroupMap.get(COMPONENT_4) != null)? dataGroupMap.get(COMPONENT_4).getId():-1;
	    	            if (groupId != -1) {
	    	            	comp4RefersTo = dataGroupMap.get(COMPONENT_4).getRefersTo();
	    	            	comp4Val = Float.parseFloat(dataMatcher.group(groupId));
	    	            }	    	            
	    	            
	    	            if (unit.equalsIgnoreCase("0.01nT")) {
	    	            	/*
	    	            	 * title line defined unit, e.g. 0.01nT
	    	            	 */
	    	            	comp1Val = comp1Val/100;
	    	            	comp2Val = comp2Val/100;
	    	            	comp3Val = comp3Val/100;
	    	            	comp4Val = comp4Val/100;
	    	            }
	    	            
	    	            if (conversionRequired) {
	    	            	/*
	    	            	 * Raw data from some providers might not be reported in the appropriate format/units. 
	    	            	 * These data needs to be converted to northward component (X) in nT and eastward component (Y) 
	    	            	 * in nT using the general formula:
       							X = H Cos D   
       							Y = H Sin D
	    	            	 */
	    	            	Float h = null;
		    	        	Float d = null;
		    	        	
		       	    		if ("H".equalsIgnoreCase(comp1RefersTo) && comp1Val != null)
		       	    			h = comp1Val;
		       	    		else if ("D".equalsIgnoreCase(comp1RefersTo) && comp1Val != null)
		       	    			d = comp1Val;
		       	    		
		       	    		if ("H".equalsIgnoreCase(comp2RefersTo) && comp2Val != null)
		       	    			h = comp2Val;
		       	    		else if ("D".equalsIgnoreCase(comp2RefersTo) && comp2Val != null)
		       	    			d = comp2Val;
		       	    		
		       	    		if (h != null && d != null) {
		       	    			comp1Val = (float) (h * Math.cos(d));
		       	    			comp2Val = (float) (h * Math.sin(d));
		       	    		}
		       	    			
		       	    	}
	    	           
    	            	if (comp1Val != null)
    	            		comp1List.add(comp1Val);
    	            	if (comp2Val != null)
    	            		comp2List.add(comp2Val);
    	            	if (comp3Val != null)
    	            		comp3List.add(comp3Val);
    	            	if (comp4Val != null)
    	            		comp4List.add(comp4Val);	    	            
	    	        
	    	        }
			    }   
	    		   
   	           firstLine = false;
    		   
    	   }    	   
    	   
    	    // set obsTime list        	
       	    long[] obsTimes = new long[obsTimesList.size()];
    	   	for (int i = 0; i < obsTimes.length; i++) {
    		   Long f = obsTimesList.get(i);	        	
    		   obsTimes[i] = (f != null ? f : 99999); 
    	   	}
            record.setObsTimes(obsTimes);
    	   
           // set component 1 data 
           float[] comp1_data = new float[(comp1List.size()>0)?comp1List.size():obsTimesList.size()];
           for (int i = 0; i < comp1_data.length; i++) {
	        	comp1_data[i] = (comp1List.size()>0)?comp1List.get(i):missingVal; 
           }
           record.setComp1Data(comp1_data);
                       
           // set component 2 data
       	   float[] comp2_data = new float[(comp2List.size()>0)?comp2List.size():obsTimesList.size()];
           for (int i = 0; i < comp2_data.length; i++) {
	        	comp2_data[i] = (comp2List.size()>0)?comp2List.get(i):missingVal; 
           }
           record.setComp2Data(comp2_data);
           
           // set component 3 data
       	   float[] comp3_data = new float[(comp3List.size()>0)?comp3List.size():obsTimesList.size()];
           for (int i = 0; i < comp3_data.length; i++) {
        	   comp3_data[i] = (comp3List.size()>0)?comp3List.get(i):missingVal; 
           }
           record.setComp3Data(comp3_data);
           
           // set the component 4 list
       	   float[] comp4_data = new float[(comp4List.size()>0)?comp4List.size():obsTimesList.size()];
           for (int i = 0; i < comp4_data.length; i++) {
        	   comp4_data[i] = (comp4List.size()>0)?comp4List.get(i):missingVal; 
           }
           record.setComp4Data(comp4_data);
           
           if (record.getStationCode() == null)
        	   record.setStationCode(station.getStationCode());
           
           if (source != null) {
        	    /* 
	   			 * Get and set the source ID from source name
	   			 */
	   			int sourceId = 0;
	            if ( dao.getGeoMagSourceId(source) != null ) {
	                  sourceId = dao.getGeoMagSourceId(source.toLowerCase());
	            }
        	    record.setSourceId(sourceId);
           }
          
           record.setReportType("GEOMAG");
    	   record.setPluginName(pluginName);
           record.setOverwriteAllowed(true);
    	   record.constructDataURI();
    	   
        } catch (Exception e) {
        	logger.error("Failed to decode file: ["+ file.getAbsolutePath() + "]", e);
        } finally {
            try {
                in.close();
            } catch (IOException e) {
            	 throw new GeoMagException("", e);
            }
        }
        if (record == null) {
            retData = new PluginDataObject[0];
        } else {
            retData = new PluginDataObject[] { record };
        }
        return retData;
    }	
	 
   public DataTime getRecordDataTime(Matcher matcher, HashMap<String, Group> groupMap) throws ParseException {
		
		int groupId = -1;
		
		String obsDateStr = null;
		String obsYearStr = null;
		String obsDayOfYearStr = null;
		
		String format = "dd-MMM-yy";
		
		Calendar cal = Calendar.getInstance();
		Date obsDate = cal.getTime();
		SimpleDateFormat inputDateFormat = new SimpleDateFormat(format);
		
	 	groupId = (groupMap.get(OBS_DATE) != null)? groupMap.get(OBS_DATE).getId():-1;
        if (groupId != -1) {
        	obsDateStr = matcher.group(groupId);  
        	format =  (groupMap.get(OBS_DATE).getFormat()!=null)?groupMap.get(OBS_DATE).getFormat():format;
        }
     
	    groupId = (groupMap.get(OBS_YEAR) != null)? groupMap.get(OBS_YEAR).getId():-1;
        if (groupId != -1) {
    	   obsYearStr = matcher.group(groupId);        	
        }
      
        groupId = (groupMap.get(OBS_DAY_OF_YEAR) != null)? groupMap.get(OBS_DAY_OF_YEAR).getId():-1;
        if (groupId != -1) {
    	   obsDayOfYearStr = matcher.group(groupId);        	
        }
       
       // get Observation Date using obsDate
       if (obsDateStr != null) {
    	   inputDateFormat = new SimpleDateFormat(format);
    	   obsDate = obsTimeDateFormat.parse(obsTimeDateFormat.format(inputDateFormat.parse(obsDateStr)));
       }
    	
       // get Observation Date using obsYear and obsDayOfYear
       if (obsYearStr != null && obsDayOfYearStr != null) {
    	   Calendar tmpCal = Calendar.getInstance();
    	   tmpCal.set(Calendar.YEAR, Integer.parseInt(obsYearStr));
    	   tmpCal.set(Calendar.DAY_OF_YEAR, Integer.parseInt(obsDayOfYearStr));
       	  
       	   obsDate = obsTimeDateFormat.parse(obsTimeDateFormat.format(tmpCal.getTime()));   
       }
       
       cal.setTime(obsDate);              
       
       DataTime dataTime = new DataTime(cal);
      
	   return dataTime;
	}
	 
	public Calendar getObsTime(Matcher matcher, HashMap<String, Group> groupMap, GeoMagRecord record) throws ParseException {
		
		int groupId = -1;
		
		String obsDateStr = null;
		String obsTimeStr = null;
		String obsHourStr = null;
		String obsMinuteStr = null;
		String obsMinuteNumStr = null;
		
		String dateFormat = "dd-MMM-yy";
		String timeFormat = "HH:mm:ss";
		SimpleDateFormat inputDateFormat = new SimpleDateFormat(dateFormat + " " + timeFormat);
		
		Calendar obsTime = record.getDataTime().getRefTimeAsCalendar();
          
		groupId = (groupMap.get(OBS_DATE) != null)? groupMap.get(OBS_DATE).getId():-1;
        if (groupId != -1) {
        	obsDateStr = matcher.group(groupId);          	 
        	dateFormat =  (groupMap.get(OBS_DATE).getFormat()!=null)?groupMap.get(OBS_DATE).getFormat():dateFormat;
        }
        
        groupId = (groupMap.get(OBS_TIME) != null)? groupMap.get(OBS_TIME).getId():-1;
        if (groupId != -1) {
        	obsTimeStr = matcher.group(groupId);          	 
        	timeFormat =  (groupMap.get(OBS_TIME).getFormat()!=null)?groupMap.get(OBS_TIME).getFormat():timeFormat;
        }
		
		groupId = (groupMap.get(OBS_MINUTE_NUM) != null)? groupMap.get(OBS_MINUTE_NUM).getId():-1;
        if (groupId != -1) {
        	obsMinuteNumStr = matcher.group(groupId);
        }
        
        groupId = (groupMap.get(OBS_HOUR) != null)? groupMap.get(OBS_HOUR).getId():-1;
        if (groupId != -1) {
        	obsHourStr = matcher.group(groupId);  
        }
        
        groupId = (groupMap.get(OBS_MINUTE) != null)? groupMap.get(OBS_MINUTE).getId():-1;
        if (groupId != -1) {
        	obsMinuteStr = matcher.group(groupId);  
        }
            
        // get obsTime using obsMinuteNum
        if (obsMinuteNumStr != null) {
        	obsTime.add(Calendar.MINUTE, (obsMinuteNumStr != null)?Integer.parseInt(obsMinuteNumStr):1);
        }
        // get obsTime using obsHour and obsMinute
        else if (obsHourStr != null && obsMinuteStr != null) {
        	int minutes = Integer.parseInt(obsHourStr) * 3600 + Integer.parseInt(obsMinuteStr) * 60;
        	obsTime.add(Calendar.MINUTE, minutes);
        }
        // get obsTime using obsDate and obsTime
        else if (obsDateStr != null && obsTimeStr != null) {
        	String obsDateTimeStr = obsDateStr + " " + obsTimeStr;
        	inputDateFormat =  new SimpleDateFormat(dateFormat + " " +timeFormat);
    		
        	Date obsDateTime = (Date)inputDateFormat.parse(obsDateTimeStr);
        	obsTime.setTime(obsDateTime); 
        }        
      
		return obsTime;
	}
	 
	public GeoMagDao getDao() {
		return dao;
	}

	public void setDao(GeoMagDao dao) {
		this.dao = dao;
	}
	
    public GeoMagStation getStationDetail(String stnCode) throws GeoMagException {
    	GeoMagStation station = null;

        if (stnCode != null) {
           TableTimeStamp.updateXmlTables();
           station = GeoMagStationLookup.getInstance().getStationByCode(stnCode);
        }

        return station;
    }
	
//
//	 public PluginDataObject[] decode(File file) throws Exception {
//
//		parseGeoMagStationsFile();
//		 
//       PluginDataObject[] retData = null;
//
//       GeoMagRecord record = null;
//         
//       List<Long> obsTimesList = new ArrayList<Long>();
//       List<Float> comp1List = new ArrayList<Float>();
//       List<Float> comp2List = new ArrayList<Float>();
//       List<Float> comp3List = new ArrayList<Float>();  
//       List<Float> comp4List = new ArrayList<Float>();        
//       
//       if (file == null || (file.length() < 1))
//           return new PluginDataObject[0];
//       
//       BufferedReader in = null;
//       
//       try{
//   	   String input;
//   	   in = new BufferedReader(new FileReader(file));
//   	   record = new GeoMagRecord();
//   	   
//   	   String stationCode = file.getName().substring(0,3).toUpperCase();
//   	   if (stationCode.startsWith("HA"))
//   		   stationCode = "HAD";
//   	   else  if (stationCode.startsWith("M"))
//   		   stationCode = "JEJ";   
//   	   
//   	   GeoMagStation station = GEOMAGSTATIONS_MAP.get(stationCode);
//   	   boolean containsHeader = (station.getRawDataFormat().getHeaderFormat()!=null)?true:false;
//   	   boolean containsData = (station.getRawDataFormat().getDataFormat()!=null)?true:false;
//   	   
//   	   Pattern HEADER_EXP = null;
//   	   Pattern DATA_EXP = null;
//   	   
//   	   if (containsHeader)
//   		   HEADER_EXP = Pattern.compile(station.getRawDataFormat().getHeaderFormat().getPattern());
//   	   
//   	   if (containsData)
//   		   DATA_EXP = Pattern.compile(station.getRawDataFormat().getDataFormat().getPattern());
//   	  
//   	   boolean firstLine = true; 
//   	   while((input = in.readLine()) != null){    
//   		 
//   		 
//   		   if (station.getProvider().equalsIgnoreCase("USGS") || station.getProvider().equalsIgnoreCase("IGPP")) {
//   			   
//   			    if (firstLine) {
//		    	        Matcher headerMatcher = HEADER_EXP.matcher(input);
//		
//		    	        if (headerMatcher.find()) {
//		    	        	
//		    	        	System.out.println(" input: " + input);
//		
//		    	            record.setStationCode(headerMatcher.group(1));
//		    	            record.setSource(headerMatcher.group(9));
//		    	            
//		    	            Date startDate = obsTimeDateFormat.parse(obsTimeDateFormat.format(olddf.parse(headerMatcher.group(4))));
//		    	            Calendar calendar= Calendar.getInstance();
//		    	            calendar.setTime(startDate);
//		
//		    	            DataTime dataTime = new DataTime(calendar);
//		    	            record.setDataTime(dataTime);
//		    	        }
//   			    } 
//   			   
//	    	        Matcher dataMatcher = DATA_EXP.matcher(input);
//
//	    	        if (dataMatcher.find()) {
//	    	        	
//	    	        	String minute = dataMatcher.group(1);
//	    	        	int minuteOfDay = Integer.parseInt(minute);
//	    	        	
//	    	        	Calendar obsTime = (record.getDataTime()!=null) ? record.getDataTime().getRefTimeAsCalendar() : Calendar.getInstance();
//	    	        	obsTime.add(Calendar.MINUTE, minuteOfDay);
//	    	        	obsTimesList.add(obsTime.getTimeInMillis());
//	    	        	
//	    	        	comp1List.add(Float.parseFloat(dataMatcher.group(2)));
//	    	        	comp2List.add(Float.parseFloat(dataMatcher.group(3)));
//	    	        	comp3List.add(Float.parseFloat(dataMatcher.group(4)));
//	    	        	comp4List.add(Float.parseFloat(dataMatcher.group(5)));
//	    	        
//	    	        }
//   		   }
//   		   else if (station.getProvider().equalsIgnoreCase("BGS")) {
//   			  
//   				
//     	            record.setStationCode(station.getStationCode());
//     	            record.setSource(station.getSource()[0]);
//     	            
//     	            Matcher dataMatcher = DATA_EXP.matcher(input);
//
//	    	        if (dataMatcher.find()) {
//	    	        	//System.out.println(" input: " + input);
//	    	        	  
//	    	        	String year = dataMatcher.group(1);
//	    	        	String dayOfYear = dataMatcher.group(2);
//	    	        	String hour = dataMatcher.group(3);
//	    	        	String minute = dataMatcher.group(4);
//	    	        	
//	    	        	int minutes = Integer.parseInt(hour) * 3600 + Integer.parseInt(minute) * 60;
//	    	        	
//	    	        	if (firstLine) {
//		    	        	Calendar calendar = Calendar.getInstance();  
//		    	        	calendar.set(Calendar.YEAR, Integer.parseInt(year));
//		    	        	calendar.set(Calendar.DAY_OF_YEAR, Integer.parseInt(dayOfYear));
//		    	        	
//		    	        	Date obsDate = obsTimeDateFormat.parse(obsTimeDateFormat.format(calendar.getTime()));
//		      	            Calendar calendar2 = Calendar.getInstance();
//		      	            calendar2.setTime(obsDate);
//	
//		      	            DataTime dataTime = new DataTime(calendar2);
//		      	            record.setDataTime(dataTime);
//	    	        	}
//	    	        	
//	    	        	Calendar obsTime = (record.getDataTime()!=null) ? record.getDataTime().getRefTimeAsCalendar() : Calendar.getInstance();
//	    	        	obsTime.add(Calendar.MINUTE, minutes);
//	    	        	obsTimesList.add(obsTime.getTimeInMillis());
//	    	        	
//	    	        	comp1List.add(Float.parseFloat(dataMatcher.group(5)));
//	    	        	comp2List.add(Float.parseFloat(dataMatcher.group(6)));
//	    	        	comp3List.add(Float.parseFloat(dataMatcher.group(7)));
//	    	        
//	    	        }
//   		   }
//   		   else if (station.getProvider().equalsIgnoreCase("GA") || station.getProvider().equalsIgnoreCase("GFZ")) {     			  
//  				
//    	            record.setStationCode(station.getStationCode());
//    	            record.setSource(station.getSource()[0]);
//    	            
//    	            Matcher dataMatcher = DATA_EXP.matcher(input);
//
//	    	        if (dataMatcher.find()) {
//	    	        	  
//	    	        	String date = dataMatcher.group(1);
//	    	        	String time = dataMatcher.group(2);
//	    	        	String dayOfYear = dataMatcher.group(3);
//	    	        	
//	    	        	String obsDateTimeStr = date + " " + time;
//	    	        	SimpleDateFormat formatter = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSS");
//   	        		
//	    	        	Date obsDateTime = (Date)formatter.parse(obsDateTimeStr);
//	    	        	Calendar obsTime = Calendar.getInstance(); 
//	    	        	obsTime.setTime(obsDateTime);
//	    	        		    	        	
//	    	        	if (firstLine) {		    	        	
//		      	            DataTime dataTime = new DataTime(obsTime);
//		      	            record.setDataTime(dataTime);
//	    	        	}
//	    	        	
//	    	        	obsTimesList.add(obsTime.getTimeInMillis());
//	    	        	
//	    	        	comp1List.add(Float.parseFloat(dataMatcher.group(4)));
//	    	        	comp2List.add(Float.parseFloat(dataMatcher.group(5)));
//	    	        	comp3List.add(Float.parseFloat(dataMatcher.group(6)));
//	    	        	comp4List.add(Float.parseFloat(dataMatcher.group(7)));
//	    	        
//	    	        }
//  		       }
//   		   else if (station.getProvider().equalsIgnoreCase("NRCAN")) {     			  
//     				
//   	            record.setStationCode(station.getStationCode());
//   	            record.setSource(station.getSource()[0]);
//   	            
//   	            Matcher dataMatcher = DATA_EXP.matcher(input);
//
//	    	        if (dataMatcher.find()) {
//	    	        	  
//	    	        	String date = dataMatcher.group(2);
//	    	        	String time = dataMatcher.group(3);
//	    	        	
//	    	        	String obsDateTimeStr = date + " " + time;
//	    	        	SimpleDateFormat formatter = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
//  	        		
//	    	        	Date obsDateTime = (Date)formatter.parse(obsDateTimeStr);
//	    	        	Calendar obsTime = Calendar.getInstance(); 
//	    	        	obsTime.setTime(obsDateTime);
//	    	        		    	        	
//	    	        	if (firstLine) {		    	        	
//		      	            DataTime dataTime = new DataTime(obsTime);
//		      	            record.setDataTime(dataTime);
//	    	        	}
//	    	        	
//	    	        	obsTimesList.add(obsTime.getTimeInMillis());
//	    	        	
//	    	        	comp1List.add(Float.parseFloat(dataMatcher.group(4)));
//	    	        	comp2List.add(Float.parseFloat(dataMatcher.group(5)));
//	    	        	comp3List.add(Float.parseFloat(dataMatcher.group(6)));
//	    	        	comp4List.add(Float.parseFloat(dataMatcher.group(7)));
//	    	        
//	    	        }
// 		       }   	
//   		   else if (station.getProvider().equalsIgnoreCase("KOREA")) {     			  
//    				
//  	            record.setStationCode(station.getStationCode());
//  	            record.setSource(station.getSource()[0]);
//  	            
//  	            Matcher dataMatcher = DATA_EXP.matcher(input);
//
//	    	        if (dataMatcher.find()) {
//	    	        	  
//	    	        	String date = dataMatcher.group(1);
//	    	        	String time = dataMatcher.group(2);
//	    	        	
//	    	        	String obsDateTimeStr = date + " " + time;
//	    	        	SimpleDateFormat formatter = new SimpleDateFormat("yyyy/MM/dd HH:mm:ss");
// 	        		
//	    	        	Date obsDateTime = (Date)formatter.parse(obsDateTimeStr);
//	    	        	Calendar obsTime = Calendar.getInstance(); 
//	    	        	obsTime.setTime(obsDateTime);
//	    	        		    	        	
//	    	        	if (firstLine) {		    	        	
//		      	            DataTime dataTime = new DataTime(obsTime);
//		      	            record.setDataTime(dataTime);
//	    	        	}
//	    	        	
//	    	        	obsTimesList.add(obsTime.getTimeInMillis());
//	    	        	
//	    	        	comp1List.add(Float.parseFloat(dataMatcher.group(3)));
//	    	        	comp2List.add(Float.parseFloat(dataMatcher.group(4)));
//	    	        	comp3List.add(Float.parseFloat(dataMatcher.group(5)));
//	    	        	comp4List.add(Float.parseFloat(dataMatcher.group(6)));
//	    	        
//	    	        }
//		       } 
//   		   
//  	           firstLine = false;
//   		   
//   	   }    	   
//   	   
//   	   	long[] obsTimes = new long[obsTimesList.size()];
//   	   	for (int i = 0; i < obsTimes.length; i++) {
//   		   Long f = obsTimesList.get(i);
//   		   obsTimes[i] = (f != null ? f : missingVal); 
//   	   	}
//           record.setObsTimes(obsTimes);
//   	   
//          float[] h_data = new float[(comp1List.size()>0)?comp1List.size():obsTimesList.size()];
//          for (int i = 0; i < h_data.length; i++) {
//	       	    if (comp1List.size() > 0) {
//	        	    h_data[i] = comp1List.get(i); 
//	    	    } else {
//	    	    	h_data[i] = missingVal;
//	    	    }
//          }
//          record.setComp1Data(h_data);
//                     
//          float[] d_data = new float[(comp2List.size()>0)?comp2List.size():obsTimesList.size()];
//          for (int i = 0; i < d_data.length; i++) {
//	       	    if (comp2List.size() > 0) {
//	        	    d_data[i] = comp2List.get(i); 
//	    	    } else {
//	    	    	d_data[i] = missingVal;
//	    	    }
//          }
//          record.setComp2Data(d_data);
//          
//          float[] z_data = new float[(comp3List.size()>0)?comp3List.size():obsTimesList.size()];
//          for (int i = 0; i < z_data.length; i++) {
//	       	    if (comp3List.size() > 0) {
//	        	    z_data[i] = comp3List.get(i); 
//	    	    } else {
//	    	    	z_data[i] = missingVal;
//	    	    }
//          }
//          record.setComp3Data(z_data);
//          
//          float[] f_data = new float[(comp4List.size()>0)?comp4List.size():obsTimesList.size()];
//          for (int i = 0; i < f_data.length; i++) {
//       	    if (comp4List.size() > 0) {
//	        	    f_data[i] = comp4List.get(i); 
//       	    } else {
//       	    	f_data[i] = missingVal;
//       	    }
//          }
//          record.setComp4Data(f_data);
//          
//          record.setReportType("GEOMAG");
//   	   record.setPluginName(pluginName);
//          record.setOverwriteAllowed(true);
//   	   record.constructDataURI();
//   	   
//       } catch (Exception e) {
//       	logger.info("GeoMagDecoder: Error decoding : " + file.getAbsolutePath());
//       } finally {
//           try {
//               in.close();
//           } catch (IOException e) {
//           	logger.info("GeoMagDecoder: Error decoding : " + file.getAbsolutePath());
//           }
//       }
//       if (record == null) {
//           retData = new PluginDataObject[0];
//       } else {
//           retData = new PluginDataObject[] { record };
//       }
//       return retData;
//   }		
	 
}

