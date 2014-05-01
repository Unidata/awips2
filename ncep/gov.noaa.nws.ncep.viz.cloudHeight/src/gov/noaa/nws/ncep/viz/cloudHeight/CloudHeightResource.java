package gov.noaa.nws.ncep.viz.cloudHeight;

import gov.noaa.nws.ncep.viz.common.display.NcDisplayType;
import gov.noaa.nws.ncep.viz.ui.display.AbstractNcEditor;
import gov.noaa.nws.ncep.viz.ui.display.NCMapDescriptor;
import gov.noaa.nws.ncep.viz.ui.display.NcEditorUtil;
import gov.noaa.nws.ncep.viz.ui.display.NcDisplayMngr;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.eclipse.swt.graphics.RGB;
import org.geotools.referencing.GeodeticCalculator;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery.QueryLanguage;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.viz.ui.editor.AbstractEditor;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Cloud Height Resource used to draw 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 05/02/09					Greg Hull	Created
 * 06/24/09					M. Li		remove resource legend
 * 07/22/09					M. Li		TO10 -> TO11
 * 10/05/09      169        Greg Hull   integrate with NCMapEditor
 * 11/24/09                 Greg Hull   migrate to to11d6
 * 05/28/10      #281       Greg Hull   fix bug drawing seld point
 * 01/05/11      #393       Archana     Added logic to find the nearest time-matched station
 * 09/21/11      #393       Archana     Changed the name of the table to ncuair 
 *                                      and updated the field names in the query string
 * 02/16/12      #583       B. Hebbard  On upper air query from DB, only get stations where
 *                                      "nil" flag is FALSE
 * 06/01/12		#747		B. Yin		Comment out the part to draw all stations.
 * 02/11/2013   972         G. Hull     AbstractEditor instead of NCMapEditor
 *
 * </pre>
 * 
 * @version 1
 */
public class CloudHeightResource extends AbstractVizResource<CloudHeightResourceData, NCMapDescriptor> {

	protected AbstractEditor    mapEditor;
	protected String         name;
	
    private RGB color = new RGB(255, 255, 0);
    
    // the point at which to draw a marker 
    private Coordinate seldLoc = null;
    
    // The following variable/classes are used for the Station Data option
    private Coordinate nearestLoc = null;
    private StationData[] stnLocs = null;
    
    private List<DataTime> stnTimes; // list of times normalized by range&offset
    // String is the DataTime of the normalized times
    //
    /** A map of a list of StationData objects to a corresponding DataTime object */
    public Map<DataTime, List<StationData >> stationDataMap;
	private final double DIST = 1.0;
	private final String DATABASE = "metadata";
	private final String UPPER_AIR_TABLE = "ncuair";  
	private final int TEN_MINUTES = 10; 
    private final int NUM_MINS_IN_ONE_HOUR = 60;
	protected final double INVALID_DISTANCE = -1.0;
	protected double minimumDistance = INVALID_DISTANCE;
	
    public class  StationData{
        Coordinate stationCoordinate;
        String stationId;
 //       String stationNumber;
        DataTime stationRefTime;
        StationData(String stationId,  Coordinate stationCoordinate, DataTime  stationRefTime2){
        	this.stationId = stationId;
 //       	this.stationNumber = stationNumber;
        	this.stationCoordinate = stationCoordinate;
        	this.stationRefTime = stationRefTime2;
        }
    }
    // End Station data needed variables.
    
    public CloudHeightResource( CloudHeightResourceData resourceData,
    		                    LoadProperties loadProperties ) throws VizException {
    	super( resourceData, loadProperties );
        mapEditor = NcDisplayMngr.getActiveNatlCntrsEditor();
        if( NcEditorUtil.getNcDisplayType( mapEditor ) != NcDisplayType.NMAP_DISPLAY ) {
        	throw new VizException( "Can not use Cloud Height on non-map display type");
        }

        name = "Cloud Height";
        stationDataMap = new HashMap<DataTime, List<StationData>>(0);
    	minimumDistance = INVALID_DISTANCE;
    	this.stnTimes = new ArrayList<DataTime>(0);
    	this.stnLocs = new StationData[0];
    }
    
    public void setSelectedLoc( Coordinate c1 ) {
    	seldLoc = c1;
    }

    public String getName() {
        return new String("");
    }

    public void initInternal( IGraphicsTarget target) throws VizException {
		// TODO Auto-generated method stub
    }

    public void paintInternal(IGraphicsTarget target, PaintProperties paintProps)
            throws VizException {
    	if( seldLoc != null ) {
    		float zoomLevel = paintProps.getZoomLevel();
    		int d = (int)(200 * zoomLevel + 1.0);

    		// Draw end point cross
    		double[] p1 = getDescriptor().worldToPixel(new double[] { seldLoc.x, seldLoc.y });
    		if( p1 != null ) {
        		target.drawLine( p1[0]+d, p1[1],   0.0, p1[0]-d, p1[1], 0.0,  color, 1.0f );
        		target.drawLine( p1[0],   p1[1]+d, 0.0, p1[0],   p1[1]-d, 0.0, color, 1.0f );
    		}
    	}

    	// draw the point of the nearest station which is being used in the calculations.
    	// Not implemented 
    	if( nearestLoc != null ) {
    		float zoomLevel = paintProps.getZoomLevel();
    		int d = (int)(200 * zoomLevel + 1.0);
    		d = d;
    		// Draw end point cross
    		double[] p1 = getDescriptor().worldToPixel(new double[] { nearestLoc.x, nearestLoc.y });
    		target.drawLine( p1[0]+d, p1[1],   p1[0]-d, p1[1], 0.0, 0.0,
    				 new RGB(0,255,0), 1.0f );
    		target.drawLine( p1[0],   p1[1]+d, p1[0],   p1[1]-d, 0.0, 0.0,
    				 new RGB(0,255,0), 1.0f );    		
    	}
  
    	//When there are many stations, the drawing becomes too slow.
    	//It does not seem necessary to draw all stations.  --bingfan
 /*   	if(stnLocs != null && stnLocs.length > 0 ) {
    		float zoomLevel = paintProps.getZoomLevel();
    		int d = (int)(200 * zoomLevel + 1.0);
//    		d = d;

    		for( StationData sd : stnLocs ) {
    			double[] p1 = getDescriptor().worldToPixel(
    					           new double[] { sd.stationCoordinate.x, sd.stationCoordinate.y });
    			if( p1 != null ) {
    				target.drawLine( p1[0]+d, p1[1],   p1[0]-d, p1[1],  0.0, 0.0,
    						new RGB(255,0,0), 1.0f );
    				target.drawLine( p1[0],   p1[1]+d, p1[0],   p1[1]-d, 0.0, 0.0,
    						new RGB(255,0,0), 1.0f );
    			}
    		}
    		
    	}
    	
    	*/
    }

    public RGB getColor() {
        return color;
    }

    public void setColor(RGB c) {
        color = c;
    }

	
   /**Returns the station closest to the user selected point on the screen*/
	public StationData getStationData( Coordinate loc, Double maxDist, DataTime currFrameTime, int maxIntervalInHoursForValidStationData ) {
		return getNearestStation(loc, maxDist, currFrameTime, maxIntervalInHoursForValidStationData);
	}
	
	@Override
	public DataTime[] getDataTimes() {
        if( stnTimes == null ) {
            return new DataTime[0];
        }
        DataTime[] returnTimes = new DataTime[stnTimes.size()];
        return stnTimes.toArray( returnTimes );
	}

  
	@Override
	protected void disposeInternal() {
		// TODO Auto-generated method stub
		
	}

	/***
	 * Retrieves all the stations from the station table (currently uair table)
	 */
	
    private void getStationDataFromDataBase(){
    	StringBuilder queryString = new StringBuilder();
    	//TODO: pass the table name as one of the arguments to this method, if the station information is to be accessed
    	//from other tables like the BUFRUA. Or pass a different query string altogether, if the field names vary as well.
    	queryString.append("select reftime, latitude, longitude, stationId from ");    	
     	queryString.append(UPPER_AIR_TABLE);
  		queryString.append(" where nil=FALSE ;");
  		try {
  			/*execute the query*/
  			List<Object[]>  results = new ArrayList<Object[]>(DirectDbQuery.executeQuery(queryString.toString(), 
  					DATABASE, QueryLanguage.SQL));
  			if(results != null && !results.isEmpty()){
  				for(Object[] eachObjectArray : results){
  					if(eachObjectArray.length == 4){
  						Calendar stationRefTime = Calendar.getInstance();
  						stationRefTime.setTime( (Date) eachObjectArray[0]);
  						DataTime stationTime = new DataTime(stationRefTime);
//  						Float latitude = (Float)(eachObjectArray[1]);
//  						Float longitude = (Float)(eachObjectArray[2]);
  						Double latitude = (Double)(eachObjectArray[1]);
  						Double longitude = (Double)(eachObjectArray[2]);  						
  						/*Sanity check to avoid creating a StationData object with invalid coordinates*/
  						if ( (latitude.doubleValue() >= -90.0 && latitude.doubleValue() <= 90.0) 
  								&& (longitude.doubleValue() >= -180.0 && longitude.doubleValue() <= 180.0)) {
							                 Coordinate stationCoordinate = new Coordinate(longitude.doubleValue(),
									                                                                                 latitude.doubleValue());
//							String stationNumber = (String) (eachObjectArray[3]);
							String stationId = (String) (eachObjectArray[3]);
							StationData stationData = new StationData( stationId, stationCoordinate, stationTime);
							List<StationData> aListOfStationData = new ArrayList<CloudHeightResource.StationData>(0);
							if (!this.stationDataMap.isEmpty() && this.stationDataMap.containsKey(stationTime)) {
								aListOfStationData = this.stationDataMap.get(stationTime);
							}
							aListOfStationData.add(stationData);
							this.stationDataMap.put(stationTime,aListOfStationData);
							this.stnTimes.add(stationTime);
						}
  					}
  				}
  			}
  		}
  		catch(Exception e){
  			System.out.println(e.getMessage());
  		}
    }
    
    /***
     * Determines the station closest to the point clicked by the user
     * @param loc - the coordinate of the point clicked by the user
     * @param soundingDist - the maximum distance from the user's selected point 
     * @param currFrameTime - the satellite image time
     * @param maxIntervalInHoursForValidStationData - the maximum interval (in hours) on either
     *  side of the satellite image time     
     * @return the station closest to the point clicked by the user.
     */
    protected StationData getNearestStation(Coordinate loc, Double soundingDist, DataTime currFrameTime, int maxIntervalInHoursForValidStationData){
         boolean isStationNonConvergent = false;
         minimumDistance = soundingDist;
		GeodeticCalculator gc =  new GeodeticCalculator();
		gc.setStartingGeographicPoint(loc.x, loc.y);
		StationData sd = null;
		if(this.stationDataMap.isEmpty()){
			getStationDataFromDataBase();
		}

		List<StationData> listOfStationData  = findListOfTimeMatchedStations(
					currFrameTime, maxIntervalInHoursForValidStationData);
			if ( listOfStationData!= null &&  listOfStationData.size() > 0) {

				try {
					/*Store the list of stations in an array*/
					this.stnLocs = new StationData[listOfStationData.size()];
					listOfStationData.toArray(this.stnLocs);

					for (StationData eachStation : listOfStationData) {
						gc.setDestinationGeographicPoint(
								eachStation.stationCoordinate.x,
								eachStation.stationCoordinate.y);
						double distance = gc.getOrthodromicDistance();
						if (distance < minimumDistance) {
							minimumDistance = distance;
							sd = eachStation;

						}
					}
				} catch (ArithmeticException e) {
					isStationNonConvergent = true;
					System.out.println(e.getMessage());
				}
			}
            if(sd != null && !isStationNonConvergent){
	             this.nearestLoc = new Coordinate(sd.stationCoordinate);
            }
		return sd;
    }   
    
    /***
     * 
     * @param imageTime
     * @param maxIntervalInHoursForValidStationData
     * @return
     */
    private List<StationData> findListOfTimeMatchedStations(DataTime imageTime, int maxIntervalInHoursForValidStationData){
    	List<StationData> listOfStationData =new ArrayList<CloudHeightResource.StationData>(0);
    	int maxIntervalForStationDataInMinutes = maxIntervalInHoursForValidStationData * NUM_MINS_IN_ONE_HOUR;
    	int baseOffset = 0;
    	
    	while(  ((baseOffset >= -maxIntervalForStationDataInMinutes) && (baseOffset <= maxIntervalForStationDataInMinutes))){
    		listOfStationData.addAll(new ArrayList<StationData>(getListOfStationDataClosestToFrameImageTime(imageTime, baseOffset)));
    		baseOffset += TEN_MINUTES;
    	}
    	
    	return listOfStationData;
    }
    
    /***
     * 
     * @param imageTime
     * @param offset
     * @return
     */
    private List<StationData> getListOfStationDataClosestToFrameImageTime(DataTime imageTime,  int offset){
	   Calendar timeToCompare = imageTime.getRefTimeAsCalendar();
 	   //Check for a time in the future
	   timeToCompare.add(Calendar.MINUTE, offset);  
	   List<StationData> listOfStationData = new ArrayList<CloudHeightResource.StationData>(0);
       Set<DataTime> setOfDataTimes = this.stationDataMap.keySet();
	     for(DataTime eachStationTime : setOfDataTimes){
	    	 if(isTimeMatchedListOfStationsFound( new DataTime(timeToCompare), eachStationTime)){
	    		 listOfStationData = this.stationDataMap.get(eachStationTime);
//	    		 System.out.println("List of stations found at time: " + eachStationTime.toString());
//	    		 break;
	    	 }
	    	 else{
	    		 //reset the timeToCompare to the original imageTime
	    		Calendar timeToCompareAgain = imageTime.getRefTimeAsCalendar();
	    		//Check for a time in the past
	    		timeToCompareAgain.add(Calendar.MINUTE, -offset); 
		    	 if(isTimeMatchedListOfStationsFound( new DataTime(timeToCompareAgain), eachStationTime)){
		    		 listOfStationData = this.stationDataMap.get(eachStationTime);
//		    		 System.out.println("List of stations found at time: " + eachStationTime.toString());
//		    		 break;
		    	 }	    		 
	    	 }
		 }
    	return listOfStationData;
    }
    
   private boolean isTimeMatchedListOfStationsFound(DataTime imageTime, DataTime stationTime){
    	boolean isFound = false;
        long imageTimeInMillis = imageTime.getRefTimeAsCalendar().getTimeInMillis();
        long stationTimeInMillis = stationTime.getRefTimeAsCalendar().getTimeInMillis();
        long numMilliSeconds =  ( Math.abs(imageTimeInMillis - stationTimeInMillis) );
        if(numMilliSeconds  < 0){
        	numMilliSeconds *= -1;
        }
        long milliSecInTenMins = TEN_MINUTES *60*1000;
        if (numMilliSeconds <= milliSecInTenMins){
    	     isFound = true; 
        }

        return isFound;
    }
}
