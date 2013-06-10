package gov.noaa.nws.ncep.viz.rsc.wtch.rsc;

import gov.noaa.nws.ncep.common.dataplugin.aww.AwwLatlons;
import gov.noaa.nws.ncep.common.dataplugin.aww.AwwRecord;
import gov.noaa.nws.ncep.common.dataplugin.aww.AwwUgc;
import gov.noaa.nws.ncep.common.dataplugin.aww.AwwVtec;
import gov.noaa.nws.ncep.common.log.logger.NcepLogger;
import gov.noaa.nws.ncep.common.log.logger.NcepLoggerManager;
import gov.noaa.nws.ncep.ui.pgen.display.ArrowHead;
import gov.noaa.nws.ncep.ui.pgen.display.ArrowHead.ArrowHeadType;
import gov.noaa.nws.ncep.viz.common.ui.NmapCommon;
import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsResource;
import gov.noaa.nws.ncep.viz.resources.INatlCntrsResource;
import gov.noaa.nws.ncep.viz.rsc.wtch.util.WtchUtil;
import gov.noaa.nws.ncep.viz.ui.display.NCMapDescriptor;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.TimeZone;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.TextStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Watch resourceResource - Display WTCH from aww data.
 * 
 *  This code has been developed by the SIB for use in the AWIPS2 system.
 *
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct. 26, 2011          Michael Gao  Initial creation.
 * 05/23/2012     785     Q. Zhou      Added getName for legend.
 * 08/30/12       853     Q. Zhou      Displayed watch number. Modified time in getWatchLabelList().
                                       Fixed label colorCode not change problem. Fixed colorCode not change back problem.
 * </pre>
 * 
 * @author mgao 
 * @version 1.0
 */

public class WtchResource  extends AbstractNatlCntrsResource< WtchResourceData, NCMapDescriptor> 
													implements INatlCntrsResource{
	private NcepLogger logger = NcepLoggerManager.getNcepLogger(this.getClass()); 

	private WtchResourceData wtchRscData;
	
	
	public class WtchRscDataObj implements IRscDataObject  {
		String 				dataUri;       //used as a key string
		DataTime        	issueTime;     //  issue time from bulletin
		DataTime        	eventTime;    
		String 				reportType;
		
		int 				watchNumber; 
		
		String 				actionType;
		String 				officeId; 
		String 				eventTrackingNumber; 
		String 				phenomena; 
		String 				significance; 
//		DataTime			eventStartTime;
//		DataTime			eventEndTime;
		
		
		
		List<String>		countyUgcList; 
		List<String>		countyNameList; 
		List<String>		stateNameList;
		
		String 				eventType;

//		WtchPointData		wtchPointData;
		
//		List<LatLon[]> 		latLonArrayList; 
		List<WatchPointsLatLonInfo> watchPointsLatLonInfoList; 
		List<AirportInfo>	airportInfoList; 
		
		@Override 
		public DataTime getDataTime() {  
			return eventTime;  
		}
	}
	
	public class LineColorAndWidth {
		RGB lineColor; 
		int lineWidth; 
		
		public LineColorAndWidth(RGB lineColor, int lineWidth) {
			this.lineColor = lineColor; 
			this.lineWidth = lineWidth; 
		}

		public RGB getLineColor() {
			return lineColor;
		}

		public int getLineWidth() {
			return lineWidth;
		}
	}
	
	public class WatchPointsLatLonInfo {
		LatLon[] pointsLatLonArray; 
		double lowestLat; 
		double lonAssociatedWithLowestLat; 
		
		public WatchPointsLatLonInfo(LatLon[] pointsLatLonArray) {
			this.pointsLatLonArray = pointsLatLonArray; 
		}
		
		public double getLowestLat() {
			return lowestLat;
		}
		public void setLowestLat(double lowestLat) {
			this.lowestLat = lowestLat;
		}

		public double getLonAssociatedWithLowestLat() {
			return lonAssociatedWithLowestLat;
		}
		public void setLonAssociatedWithLowestLat(double lonAssociatedWithLowestLat) {
			this.lonAssociatedWithLowestLat = lonAssociatedWithLowestLat;
		}

		public LatLon[] getPointsLatLonArray() {
			return pointsLatLonArray;
		}

	}
	
//	public class WtchPointData {
//		List<LatLon[]> latLonArrayList; 
////		DataTime eventStartTime; 
////		DataTime eventEndTime; 
//		
//		public WtchPointData(List<LatLon[]> latLonArrayList) {
//			this.latLonArrayList = latLonArrayList; 
//		}
//		
//		public WtchPointData(List<LatLon[]> latLonArrayList, 
//				DataTime eventStartTime, DataTime eventEndTime) {
//			this.latLonArrayList = latLonArrayList; 
////			this.eventStartTime = eventStartTime; 
////			this.eventEndTime = eventEndTime; 
//		}
//		
////		public DataTime getEventStartTime() {
////			return eventStartTime;
////		}
////
////		public void setEventStartTime(DataTime eventStartTime) {
////			this.eventStartTime = eventStartTime;
////		}
////
////		public DataTime getEventEndTime() {
////			return eventEndTime;
////		}
////
////		public void setEventEndTime(DataTime eventEndTime) {
////			this.eventEndTime = eventEndTime;
////		}
//
//		public List<LatLon[]> getLatLonArrayList() {
//			return latLonArrayList;
//		}
//
//	}
	
	public class LatLon {
		int index; 
		double lat; 
		double lon; 
		
		public LatLon(int index, double lat, double lon) {
			this.index = index; 
			this.lat = lat; 
			this.lon = lon; 
		}
		
		public int getIndex() {
			return index;
		}

		public double getLat() {
			return lat;
		}

		public double getLon() {
			return lon;
		}

	}
	
	public class AirportInfo {
		double angleDegree; 
		double lat; 
		double lon; 
		
		public AirportInfo(double angleDegree, double lat, double lon) {
			this.angleDegree = angleDegree; 
			this.lat = lat; 
			this.lon = lon; 
		}
		
		public double getAngleDegree() {
			return angleDegree;
		}

		public double getLat() {
			return lat;
		}

		public double getLon() {
			return lon;
		}

	}
	
	protected class FrameData extends AbstractFrameData {
		
		HashMap<String, WtchRscDataObj> wtchDataMap;

		public FrameData(DataTime frameTime, int timeInt) {
			super( frameTime, timeInt );
			wtchDataMap = new HashMap<String, WtchRscDataObj>();  
		}
		
		@Override
		public boolean updateFrameData( IRscDataObject rscDataObj ) {
			if( !(rscDataObj instanceof WtchRscDataObj) ) {
//				System.out.println("WtchResource.updateFrameData: expecting objects "+
//						" of type WtchRscDataObj???");
				return false;
			}
  
			WtchRscDataObj wtchRscDataObj = (WtchRscDataObj) rscDataObj;
            updateEachWatchNumberData(wtchRscDataObj);
			return true;
		}
		
		private void updateEachWatchNumberData(WtchRscDataObj wtchRscDataObj) {
			WtchRscDataObj existingWtchRscDataObj = wtchDataMap.get(wtchRscDataObj.dataUri); 
//			if(existingWtchRscDataObj == null) {
//				System.out.println("==================, existingWtchRscDataObj is NULL"); 
//			} else {
//					System.out.println("==================, finally, find existingWtchRscDataObj is not NULL, existingWtchRscDataObj.dataUri=" 
//							+ existingWtchRscDataObj.dataUri); 
//			}
			if(existingWtchRscDataObj == null || 
					newIssueTimeGreaterThanExistingIssueTime(wtchRscDataObj.issueTime, existingWtchRscDataObj.issueTime)) {
//				System.out.println("!!!!!####==================, finally, add something to wtchDataMap!!!!, wtchRscDataObj.dataUri="+wtchRscDataObj.dataUri); 
				wtchDataMap.put(wtchRscDataObj.dataUri, wtchRscDataObj);  
			} 
		}

		private boolean newIssueTimeGreaterThanExistingIssueTime(DataTime newIssueTime, DataTime existingIssueTime) {
			boolean isNewIssueTimeGreaterThanExistingIssueTime = false; 
			if(newIssueTime != null && existingIssueTime != null) { 
				if(newIssueTime.greaterThan(existingIssueTime))
					isNewIssueTimeGreaterThanExistingIssueTime = true; 
			}
			return isNewIssueTimeGreaterThanExistingIssueTime; 	
		}
	}
	
	public WtchResource( WtchResourceData rscData,LoadProperties loadProperties ) throws VizException { 
		super(rscData, loadProperties);	
		wtchRscData = (WtchResourceData) resourceData;
		//modifyList = new ArrayList<WtchRscDataObj>();			//addRDChangedListener();//T456
	}

	@Override
	public void initResource(IGraphicsTarget grphTarget) throws VizException {
		queryRecords();
	}

	@Override
	public void disposeInternal() {

	}

	/**
	 * This overrides the default which works for PluginDataObjects.
	 * This method is called by queryRecords to turn the records from the database
	 * into WtchRscDataObj objects.
	 */
    @Override
    public IRscDataObject[] processRecord( Object awwObj ) {
    	if(!(awwObj instanceof AwwRecord) ) {
//    		System.out.println("WtchResource.processRecord: object is not a "+
//    				"AwwRecord: "+ awwObj.getClass().getName() );
    		return new IRscDataObject[]{};
    	}

    	List<WtchRscDataObj> wtchRscDataObjList = getWtchData( (AwwRecord) awwObj );
    	if(wtchRscDataObjList.size() == 0) {
    		return new IRscDataObject[]{};
    	}

    	return wtchRscDataObjList.toArray(new WtchRscDataObj[]{});//frdos.toArray(new FfaRscDataObj[]{});//
    }

    private List<WtchRscDataObj> getWtchData(AwwRecord awwRecord) {
//		System.out.println("======within getWtchData(...), the very first line!!!"); 
    	List<WtchRscDataObj> wtchRscDataObjList = new ArrayList<WtchRscDataObj>(); 
    	
    	if(WtchUtil.isWtchRecord(awwRecord)) {
    		WtchRscDataObj wtchData = new WtchRscDataObj(); 
    		wtchData.issueTime = new DataTime(awwRecord.getIssueTime());
    		wtchData.reportType = awwRecord.getReportType();
    		wtchData.dataUri = awwRecord.getDataURI();
    		wtchData.officeId = awwRecord.getIssueOffice(); 
    		
    		try {
    			int watchNumber = Integer.parseInt(awwRecord.getWatchNumber()); 
    			wtchData.watchNumber = watchNumber; 
//    			System.out.println("======within getWtchData(...), wtchData.watchNumber="+wtchData.watchNumber); 
    		} catch(NumberFormatException nfe) {
    			//do nothing
    		}
    		
			Set<AwwUgc> awwUgcSet = awwRecord.getAwwUGC();
//			System.out.println("The retrieved total number of AWWUGCRecord for WTCH is:"+awwUgcSet.size());  
			logger.debug("The retrieved total number of AWWUGCRecord for WTCH is:"+awwUgcSet.size());  
			for (AwwUgc eachAwwUgc : awwUgcSet) {
				wtchData.eventTrackingNumber = eachAwwUgc.getEventTrackingNumber(); 
				Set<AwwLatlons> awwLatLonSet = eachAwwUgc.getAwwLatLon(); 
//				System.out.println("======within getWtchData(...), before call getLatLonArrayList, awwLatLonSet.size="+awwLatLonSet.size()); 
				List<LatLon[]> latLonArrayList = getLatLonArrayList(awwLatLonSet); 
//				System.out.println("======within getWtchData(...), the retrieved latLonArrayList.size="+latLonArrayList.size()); 
				wtchData.watchPointsLatLonInfoList = getWatchPointsLatLonInfoList(latLonArrayList); 
//				System.out.println("======within getWtchData(...), the retrieved wtchData.watchPointsLatLonInfoList.size="+wtchData.watchPointsLatLonInfoList.size()); 
				
				Set<AwwVtec> awwVtecSet = eachAwwUgc.getAwwVtecLine(); 

				DataTime thisEventTime = getEventTime(awwVtecSet, awwRecord.getIssueTime()); 
				if(thisEventTime != null) 
					wtchData.eventTime = thisEventTime; 

				wtchData.airportInfoList = getAirportInfoList(awwVtecSet); 
				
//				if(ugcline!=null && ugcline!=""){
//					
//				}
				
			}
			wtchRscDataObjList.add(wtchData); 
    	}
//		System.out.println("======within getWtchData(...), before return, wtchRscDataObjList.size="+wtchRscDataObjList.size()); 
    	return wtchRscDataObjList; 
    }
    
    private DataTime getEventTime(Set<AwwVtec> awwVtecSet, Calendar awwIssueTimeCalendar) {
    	DataTime newCalculatedEventTime = null; 
    	/*
    	 * Now try to retrieve valid event start/end times
    	 */
    	if(awwVtecSet != null) {
    		Iterator<AwwVtec> itr = awwVtecSet.iterator(); 
    		if(itr.hasNext()) {
    			AwwVtec awwVtec = itr.next(); 
    			Calendar eventStartCalendar = awwVtec.getEventStartTime(); 
    			Calendar eventEndCalendar = awwVtec.getEventEndTime(); 
    			/*
    			 * If startEventTime is still NULL, use the issueTime of AwwRecord to substitute it
    			 * This solution may not be 100% accurate for some scenarios. 
    			 */
    			if(eventStartCalendar == null && awwIssueTimeCalendar != null)
    				eventStartCalendar = awwIssueTimeCalendar; 

    			if(eventStartCalendar != null && eventEndCalendar != null) {
    				newCalculatedEventTime = new DataTime(eventStartCalendar, 
    						new TimeRange(eventStartCalendar, eventEndCalendar)); 
    			}
    		}
    	}
    	return newCalculatedEventTime; 
    }
    
    private List<LatLon[]> getLatLonArrayList(Set<AwwLatlons> awwLatLonSet) {
    	List<LatLon[]> latLonArrayList = new ArrayList<LatLon[]>(); 
    	if(awwLatLonSet != null && awwLatLonSet.size() > 0) {
        	int latLonArraySize = awwLatLonSet.size(); 
        	LatLon[] latLonArray = new LatLon[latLonArraySize]; 
        	for(AwwLatlons eachAwwLatLons : awwLatLonSet) {
        		int awwLatLonsIndex = eachAwwLatLons.getIndex(); 
        		if(isLatLonIndexValid(awwLatLonsIndex, latLonArraySize)) {
        			int arrayIndex = awwLatLonsIndex - 1; 
        			LatLon latLon = new LatLon(eachAwwLatLons.getIndex(), 
        					eachAwwLatLons.getLat(), eachAwwLatLons.getLon()); 
        			latLonArray[arrayIndex] = latLon; 
        		}
    		}
        	latLonArrayList.add(latLonArray); 
    	}
    	
    	return latLonArrayList; 
    }

    private List<WatchPointsLatLonInfo> getWatchPointsLatLonInfoList(List<LatLon[]> latLonArrayList) {
    	List<WatchPointsLatLonInfo> watchPointsLatLonInfoList = new ArrayList<WatchPointsLatLonInfo>(); 
    	for(LatLon[] eachLatLonArray : latLonArrayList) {
    		WatchPointsLatLonInfo watchPointsLatLonInfo = new WatchPointsLatLonInfo(eachLatLonArray); 
    		int latLonArrayIndexWithLowestLatValue = getLatLonArrayIndexWithLowestLatValue(eachLatLonArray); 
    		watchPointsLatLonInfo.setLowestLat(eachLatLonArray[latLonArrayIndexWithLowestLatValue].getLat()); 
    		watchPointsLatLonInfo.setLonAssociatedWithLowestLat(eachLatLonArray[latLonArrayIndexWithLowestLatValue].getLon()); 
    		watchPointsLatLonInfoList.add(watchPointsLatLonInfo); 
    	}
    	return watchPointsLatLonInfoList; 
    }

    private int getLatLonArrayIndexWithLowestLatValue(LatLon[] latLonArray) {
    	int index = 0; 
    	if(latLonArray != null) {
    		double lowestLatValue = latLonArray[0].getLat();
    		for(int i=1; i<latLonArray.length-1; i++) {
    			double tempLatValue = latLonArray[i].getLat(); 
    			if(tempLatValue < lowestLatValue) {
    				lowestLatValue = tempLatValue; 
    				index = i; 
    			}
    		}
    	}
    	return index; 
    }

    private boolean isLatLonIndexValid(int index, int latLonArraySize) {
    	boolean isValid = false; 
    	if(index > 0 && index <= latLonArraySize)
    		isValid = true; 
    	return isValid; 
    }
    
    private List<AirportInfo> getAirportInfoList(Set<AwwVtec> awwVtecSet) {
    	List<AirportInfo> airportInfoList = new ArrayList<AirportInfo>(); 
    	
    	return airportInfoList;
    }
    
	@Override
	protected void paintFrame(AbstractFrameData frameData,
						IGraphicsTarget target, PaintProperties paintProps)	throws VizException {
		if( paintProps == null ) {
			return;
		}
		FrameData currFrameData = (FrameData) frameData;

//		Envelope envelope = null;	
//		try {
//			PixelExtent extent = (PixelExtent) paintProps.getView().getExtent();
//			Envelope e = descriptor.pixelToWorld(extent, descriptor.getCRS());
//			ReferencedEnvelope referencedEnvelope = new ReferencedEnvelope(e, descriptor.getCRS());
//			envelope = referencedEnvelope.transform(MapUtil.LATLON_PROJECTION, true);
//		} catch (Exception e) {
//			throw new VizException("Error transforming extent", e);
//		}
		
		
		
		
		
		
		RGB watchBoxLineColor = new RGB (255, 0, 0); //(255, 0, 0) (155, 155, 155);
		LineStyle watchBoxLineStyle = LineStyle.SOLID;
		int watchBoxLineWidth = 2;

		RGB statusLineColor = new RGB (0, 255, 0); //(255, 0, 0) (155, 155, 155);
		LineStyle statusLineStyle = LineStyle.SOLID;
		int statusLineWidth = 1;

		Collection<WtchRscDataObj> wtchRscDataValues = currFrameData.wtchDataMap.values(); 
//		System.out.println("=====?????####, within paintFrame, wtchRscDataValues.size=" + wtchRscDataValues.size()); 

/////		IWireframeShape wireframeShape = target.createWireframeShape(false, descriptor, 0.0f);

		for(WtchRscDataObj eachWtchRscDataObj : wtchRscDataValues) {
			drawWatch(eachWtchRscDataObj, wtchRscData, target, 
					paintProps, watchBoxLineColor, watchBoxLineStyle, watchBoxLineWidth); 

			drawStatusInfo(eachWtchRscDataObj, wtchRscData, target, 
					paintProps, statusLineColor, statusLineStyle, statusLineWidth); 
//			else if(wtchRscData.getStatusEnable() && isSevereWeatherStatusNotification(eachWtchRscDataObj.reportType.trim())) {
//				System.out.println("================= start doing drawStatusLine!!!!!!!!!!!!!!!!!!!!!!====================="); 
//				drawStatusLine(eachWtchRscDataObj, target, paintProps, statusLineColor, statusLineStyle, statusLineWidth); 
//			}
		}
		
		/*
		 * dispose wireframeShape
		 */
/////		wireframeShape.dispose();  
	}

	private boolean isWatchNumberMatchedAndEnabled(int watchNumberOfTheWtchRscDataObj, WtchResourceData wtchRscData) {
		boolean isWatchNumberMatchedAndEnabled = false; 
		int lastDigitOfWatchNumber = watchNumberOfTheWtchRscDataObj%10; 
		switch(lastDigitOfWatchNumber) {
			case 0:
				if(wtchRscData.getWatchxxx0Enable())
					isWatchNumberMatchedAndEnabled = true; 
			break; 
			case 1:
				if(wtchRscData.getWatchxxx1Enable())
					isWatchNumberMatchedAndEnabled = true; 
			break; 
			case 2:
				if(wtchRscData.getWatchxxx2Enable())
					isWatchNumberMatchedAndEnabled = true; 
			break; 
			case 3:
				if(wtchRscData.getWatchxxx3Enable())
					isWatchNumberMatchedAndEnabled = true; 
			break; 
			case 4:
				if(wtchRscData.getWatchxxx4Enable())
					isWatchNumberMatchedAndEnabled = true; 
			break; 
			case 5:
				if(wtchRscData.getWatchxxx5Enable())
					isWatchNumberMatchedAndEnabled = true; 
			break; 
			case 6:
				if(wtchRscData.getWatchxxx6Enable())
					isWatchNumberMatchedAndEnabled = true; 
			break; 
			case 7:
				if(wtchRscData.getWatchxxx7Enable())
					isWatchNumberMatchedAndEnabled = true; 
			break; 
			case 8:
				if(wtchRscData.getWatchxxx8Enable())
					isWatchNumberMatchedAndEnabled = true; 
			break; 
			case 9:
				if(wtchRscData.getWatchxxx9Enable())
					isWatchNumberMatchedAndEnabled = true; 
			break; 
		}
		return isWatchNumberMatchedAndEnabled; 
	}
	
	@Override
	protected AbstractFrameData createNewFrame(DataTime frameTime, int frameInterval) {
		return (AbstractFrameData) new FrameData( frameTime, frameInterval );	
	}

	private void drawWatch(WtchRscDataObj wtchRscDataObj, WtchResourceData wtchResourceData, IGraphicsTarget target, 
			PaintProperties paintProps, RGB lineColor, LineStyle lineStyle, int lineWidth) {
		if(wtchResourceData.getThunderstormEnable() && isThunderstormWatch(wtchRscDataObj.reportType.trim())) {
//			System.out.println("======= inside drawWatch, before doDrawWatch for Thunderstorm"); 
			lineColor = wtchResourceData.getThunderstormColor(); 
			lineWidth = wtchResourceData.getThunderstormLineWidth(); 
			doDrawWatch(wtchRscDataObj, wtchResourceData, target, 
					paintProps, lineColor, lineStyle, lineWidth); 
		}
		if(wtchResourceData.getTornadoEnable() && isTornadoWatch(wtchRscDataObj.reportType.trim())) {
//			System.out.println("======= inside drawWatch, before doDrawWatch for Tornado"); 
			lineColor = wtchResourceData.getTornadoColor(); 
			lineWidth = wtchResourceData.getTornadoLineWidth(); 
			doDrawWatch(wtchRscDataObj, wtchResourceData, target, 
					paintProps, lineColor, lineStyle, lineWidth); 
		}
	}
	
	private void doDrawWatch(WtchRscDataObj wtchRscDataObj, WtchResourceData wtchResourceData, IGraphicsTarget target, 
			PaintProperties paintProps, RGB lineColor, LineStyle lineStyle, int lineWidth) {
		LineColorAndWidth lineColorAndWidth = getLineColorAndWidthForWatch(wtchRscDataObj, wtchResourceData, 
				lineColor, lineWidth); 
		RGB watchLineColor = lineColorAndWidth.getLineColor();  
		int watchLinWidth = lineColorAndWidth.getLineWidth();  
//		System.out.println("======= inside doDrawWatch, before call drawPolygon"); 
		drawPolygon(wtchRscDataObj, target, paintProps, watchLineColor, lineStyle, watchLinWidth); 
		/*
		 * Now try to draw labels
		 */
		List<String> watchLabelStringList = getWatchLabelList(wtchRscDataObj, wtchResourceData); 
		if(isThereALabelForDrawing(watchLabelStringList)) {
			drawLabel(wtchRscDataObj, watchLabelStringList, target, 
					paintProps, watchLineColor); 
		}
	}
	
	private void drawStatusInfo(WtchRscDataObj wtchRscDataObj, WtchResourceData wtchResourceData, IGraphicsTarget target, 
			PaintProperties paintProps, RGB lineColor, LineStyle lineStyle, int lineWidth) {
//		System.out.println("=======, within drawStatusInfo, wtchRscData.getStatusEnable()="+wtchRscData.getStatusEnable()); 
//		System.out.println("=======, within drawStatusInfo, isSevereWeatherStatusNotification(wtchRscDataObj.reportType.trim())="+
//				isSevereWeatherStatusNotification(wtchRscDataObj.reportType.trim())); 
		if(wtchRscData.getStatusEnable() && isSevereWeatherStatusNotification(wtchRscDataObj.reportType.trim())) {
			lineColor = wtchResourceData.getStatusColor();  
			lineWidth = wtchResourceData.getStatusLineWidth();  
//			System.out.println("=====, within drawStatusInfo, right before call doDrawStatusLine(...)"); 
			doDrawStatusLine(wtchRscDataObj, target, 
					paintProps, lineColor, lineStyle, lineWidth); 
			/*
			 * First try to draw a watch number  and status line number if they are enabled
			 */
			List<String> statusLabelStringList = getStatusLabelList(wtchRscDataObj, wtchResourceData); 
			if(isThereALabelForDrawing(statusLabelStringList)) {
				drawLabel(wtchRscDataObj, statusLabelStringList, target, 
						paintProps, lineColor); 
			}
			
//			/*
//			 * Now try to draw status lines time
//			 */
//			String statusLinesTimeLabel = getStatusLinesTimeValue(wtchRscDataObj, wtchResourceData); 
//			if(isThereALabelForDrawing(statusLinesTimeLabel)) {
//				drawLabel(wtchRscDataObj, statusLinesTimeLabel, target, 
//						paintProps, lineColor); 
//			}
		}
		
	}
	
	private boolean isThereALabelForDrawing(List<String> labelStringList) {
		boolean isLabelDrawable = false; 
		if(labelStringList != null && labelStringList.size() > 0) 
			isLabelDrawable = true; 
		return isLabelDrawable; 
	}
	
	private List<String> getWatchLabelList(WtchRscDataObj wtchRscDataObj, WtchResourceData wtchResourceData) {
		List<String> watchLabelStringList = new ArrayList<String>(); 
		/*
		 * Append the watch number first
		 */
		if(wtchResourceData.getWatchBoxNumberEnable()) {
			int watchNumber = wtchRscDataObj.watchNumber; 
//			watchLabelStringBuilder.append(watchNumber); 
			if(watchNumber > 0) {
				watchLabelStringList.add(String.valueOf(watchNumber)); 
			}
		}
		/*
		 * Now check and append the event start and end time
		 */
		if(wtchResourceData.getWatchBoxTimeEnable()) {
			DataTime watchEventDataTime = wtchRscDataObj.getDataTime(); 
			String watchBoxTimeString = getTimeValueByDataTime(watchEventDataTime); 
			
			String modifiedTime = watchBoxTimeString.substring(0,2) + watchBoxTimeString.substring(3,8) + watchBoxTimeString.substring(9,11);
			watchLabelStringList.add(modifiedTime); //12:15-21:00 -> 1215-2100
		}
		return watchLabelStringList;  
	}
	
	private List<String> getStatusLabelList(WtchRscDataObj wtchRscDataObj, WtchResourceData wtchResourceData) {
		List<String> statusLabelStringList = new ArrayList<String>(); 
		/*
		 * Append the watch number first
		 */
		if(wtchResourceData.getStatusLinesNumberEnable()) {
			int watchNumber = wtchRscDataObj.watchNumber; 
			if(watchNumber > 0) {
				statusLabelStringList.add(String.valueOf(watchNumber)); 
			}
		}
		
		/*
		 * Now check and append the event start and end time
		 */
		if(wtchResourceData.getStatusLinesTimeEnable()) {
			DataTime statusEventDataTime = wtchRscDataObj.getDataTime(); 
			String statusLinesTimeString = getTimeValueByDataTime(statusEventDataTime); 
			statusLabelStringList.add(statusLinesTimeString); 
		}
		return statusLabelStringList; 
	}
	
	private String getTimeValueByDataTime(DataTime dataTime) {
		StringBuilder dataTimeStringBuilder = new StringBuilder(); 
		if(dataTime != null) {
			TimeRange eventTimeRange = dataTime.getValidPeriod(); 
			if(eventTimeRange != null) {
				Calendar calendar = Calendar.getInstance(); 
//				System.out.println("=====, eventStartTime in Date="+eventTimeRange.getStart()); 
				calendar.setTime(eventTimeRange.getStart()); 
				String startHourMinuteString = getHourMinuteString(calendar); 
//				System.out.println("=====, eventStartTime in String, startHourMinuteString="+startHourMinuteString); 
//				System.out.println("=====, eventEndTime in Date="+eventTimeRange.getEnd()); 
				calendar.setTime(eventTimeRange.getEnd());
				String endHourMinuteString = getHourMinuteString(calendar); 
//				System.out.println("=====, eventEndTime in String, endHourMinuteString="+endHourMinuteString); 
				dataTimeStringBuilder.append(startHourMinuteString)
									 .append("-")
									 .append(endHourMinuteString); 
			}
		}
		return dataTimeStringBuilder.toString(); 
	}
	
	private String getHourMinuteString(Calendar calendar) {
		calendar.setTimeZone(TimeZone.getTimeZone("UTC")); 
		StringBuilder strBuilder = new StringBuilder(); 
		int hourInt = calendar.get(Calendar.HOUR_OF_DAY); 
		int minuteInt = calendar.get(Calendar.MINUTE); 
		String hourString = ""; 
		if(hourInt < 10)
			hourString = "0" + hourInt; 
		else 
			hourString = String.valueOf(hourInt); 
		String minuteString = ""; 
		if(minuteInt < 10)
			minuteString = "0" + minuteInt; 
		else 
			minuteString = String.valueOf(minuteInt); 
//		System.out.println("=========DEBUG, getHourMinuteString, HOUR ="+hourString); 
//		System.out.println("=========DEBUG, getHourMinuteString, MINUTE ="+minuteString); 
		
		strBuilder.append(hourString)
				  .append(":")
				  .append(minuteString);
		return strBuilder.toString(); 
	}
	
	private LineColorAndWidth getLineColorAndWidthForWatch(WtchRscDataObj wtchRscDataObj, WtchResourceData wtchResourceData, 
			RGB defaultLineColor, int defaultLineWidth) {
		RGB lineColor = defaultLineColor; 
		int lineWidth = defaultLineWidth; 
		int watchNumber = wtchRscDataObj.watchNumber; 
		
		if(wtchResourceData.getColorCodeEnable()){
			int lastDigitOfWatchNumber = watchNumber%10; 
			switch(lastDigitOfWatchNumber) {
			case 0:
				if(wtchResourceData.getWatchxxx0Enable()) {
					lineColor = wtchResourceData.getWatchxxx0Color();
					lineWidth = wtchResourceData.getWatchxxx0LineWidth(); 
				}
			break; 
			case 1:
				if(wtchResourceData.getWatchxxx1Enable()) {
					lineColor = wtchResourceData.getWatchxxx1Color();
					lineWidth = wtchResourceData.getWatchxxx1LineWidth(); 
				}
			break; 
			case 2:
				if(wtchResourceData.getWatchxxx2Enable()) {
					lineColor = wtchResourceData.getWatchxxx2Color();
					lineWidth = wtchResourceData.getWatchxxx2LineWidth(); 
				}
			break; 
			case 3:
				if(wtchResourceData.getWatchxxx3Enable()) {
					lineColor = wtchResourceData.getWatchxxx3Color();
					lineWidth = wtchResourceData.getWatchxxx3LineWidth(); 
				}
			break; 
			case 4:
				if(wtchResourceData.getWatchxxx4Enable()) {
					lineColor = wtchResourceData.getWatchxxx4Color();
					lineWidth = wtchResourceData.getWatchxxx4LineWidth(); 
				}
			break; 
			case 5:
				if(wtchResourceData.getWatchxxx5Enable()) {
					lineColor = wtchResourceData.getWatchxxx5Color();
					lineWidth = wtchResourceData.getWatchxxx5LineWidth(); 
				}
			break; 
			case 6:
				if(wtchResourceData.getWatchxxx6Enable()) {
					lineColor = wtchResourceData.getWatchxxx6Color();
					lineWidth = wtchResourceData.getWatchxxx6LineWidth(); 
				}
			break; 
			case 7:
				if(wtchResourceData.getWatchxxx7Enable()) {
					lineColor = wtchResourceData.getWatchxxx7Color();
					lineWidth = wtchResourceData.getWatchxxx7LineWidth(); 
				}
			break; 
			case 8:
				if(wtchResourceData.getWatchxxx8Enable()) {
					lineColor = wtchResourceData.getWatchxxx8Color();
					lineWidth = wtchResourceData.getWatchxxx8LineWidth(); 
				}
			break; 
			case 9:
				if(wtchResourceData.getWatchxxx9Enable()) {
					lineColor = wtchResourceData.getWatchxxx9Color();
					lineWidth = wtchResourceData.getWatchxxx9LineWidth(); 
				}
			break; 
			}
		}
		
		else {
			if (wtchRscDataObj.reportType.equalsIgnoreCase("SEVERE_THUNDERSTORM_WATCH")){
				lineColor       = wtchResourceData.thunderstormColor;
				lineWidth = wtchResourceData.getThunderstormLineWidth();				
			} 
			else if (wtchRscDataObj.reportType.equalsIgnoreCase("TORNADO_WATCH_OUTLINE_UPDATE")){
				lineColor       = wtchResourceData.tornadoColor;
				lineWidth = wtchResourceData.getTornadoLineWidth();
			}
		}
		
		LineColorAndWidth lineColorAndWidth = new LineColorAndWidth(lineColor, lineWidth);
		return lineColorAndWidth; 
	}
	
	@SuppressWarnings("deprecation")
	private void drawLabel(WtchRscDataObj wtchRscDataObj, List<String> labelStringList, IGraphicsTarget graphicTarget, 
			PaintProperties paintProps, RGB labelColor) {
		IExtent screenExtentInPixels = paintProps.getView().getExtent();

        double ratio = screenExtentInPixels.getWidth()
                / paintProps.getCanvasBounds().width;

		IFont font = graphicTarget.initializeFont("Monospace", 14, new IFont.Style[] { IFont.Style.BOLD });

        String[] textArray = labelStringList.toArray(new String[0]); 

		List<WatchPointsLatLonInfo> watchPointsLatLonInfoList = wtchRscDataObj.watchPointsLatLonInfoList; 
		for(WatchPointsLatLonInfo eachWatchPointsLatLonInfo : watchPointsLatLonInfoList) {
			double[] labelLatLon = { eachWatchPointsLatLonInfo.getLonAssociatedWithLowestLat(), eachWatchPointsLatLonInfo.getLowestLat() }; 
			double[] labelPix = descriptor.worldToPixel( labelLatLon );
			try {
				graphicTarget.drawStrings(font, textArray,   
						labelPix[0], labelPix[1]+4*ratio, 0.0, TextStyle.NORMAL,
						new RGB[] {labelColor, labelColor, labelColor},
						HorizontalAlignment.LEFT, 
						VerticalAlignment.TOP );
			} catch(VizException vize){
				logger.error("VizException is thrown when trying to drawLabel for WATCH, error=" + vize.getMessage());
			}

		}
		
	}
	
	private void drawPolygon(WtchRscDataObj wtchRscDataObj, IGraphicsTarget target, 
			PaintProperties paintProps, RGB lineColor, LineStyle lineStyle, int lineWidth) {
//	private void drawPolygon(IWireframeShape wireframeShape, WtchRscDataObj wtchRscDataObj, IGraphicsTarget target, 
//			PaintProperties paintProps, RGB lineColor, LineStyle lineStyle, int lineWidth) {
		List<WatchPointsLatLonInfo> watchPointsLatLonInfoList = wtchRscDataObj.watchPointsLatLonInfoList; 
///		System.out.println("===== inside drawPolygon, watchPointsLatLonInfoList.size="+watchPointsLatLonInfoList.size()); 
//		displayLatLonArrayList(latLonArrayList); 
		List<Coordinate[]> coordinateArrayList = getCoordinateArrayListForWatch(watchPointsLatLonInfoList); 
///		System.out.println("===== inside drawPolygon, coordinateArrayList.size="+coordinateArrayList.size()); 

		IWireframeShape wireframeShape = target.createWireframeShape(false, descriptor, 0.0f);
//int listIndex = 1; 		
		for(Coordinate[] eachCoordinateArray : coordinateArrayList) {
//			IWireframeShape newOutlineShape = target.createWireframeShape(false, descriptor, 0.0f);
			wireframeShape.addLineSegment(eachCoordinateArray); 
//			displayCoordinateArray(eachCoordinateArray, listIndex);
//			listIndex++; 
		}
			wireframeShape.compile(); 
			try {
				target.drawWireframeShape(wireframeShape, lineColor, lineWidth, lineStyle);
//				System.out.println("@@@@@@@@@@@@@@@@@@@@@@ START DRAWING LINE!!!!! @@@@@@@@@@@@@@@@@@@@@@@@@@"); 
//				target.drawLine(eachCoordinateArray[0].x, eachCoordinateArray[0].y, 0, 
//						eachCoordinateArray[1].x, eachCoordinateArray[1].y, 0, lineColor, lineWidth, lineStyle); 
//				target.drawLine(eachCoordinateArray[1].x, eachCoordinateArray[1].y, 0, 
//						eachCoordinateArray[2].x, eachCoordinateArray[2].y, 0, lineColor, lineWidth, lineStyle); 
//				target.drawLine(eachCoordinateArray[2].x, eachCoordinateArray[2].y, 0, 
//						eachCoordinateArray[3].x, eachCoordinateArray[3].y, 0, lineColor, lineWidth, lineStyle); 
//				target.drawLine(eachCoordinateArray[3].x, eachCoordinateArray[3].y, 0, 
//						eachCoordinateArray[0].x, eachCoordinateArray[0].y, 0, lineColor, lineWidth, lineStyle); 
			} catch (VizException e) {
				logger.error("VizException is caught when do target.drawWireframeShape(...), error=" + e.getMessage()); 
			} finally {
//				newOutlineShape.dispose();  
			}
		
		wireframeShape.dispose();  

	}
	
	private void doDrawStatusLine(WtchRscDataObj wtchRscDataObj, IGraphicsTarget target, 
			PaintProperties paintProps, RGB lineColor, LineStyle lineStyle, int lineWidth) {
//		List<LatLon[]> latLonArrayList = wtchRscDataObj.latLonArrayList;
		List<WatchPointsLatLonInfo> watchPointsLatLonInfoList = wtchRscDataObj.watchPointsLatLonInfoList; 
		System.out.println("===within doDrawStatusLine(...), wtchRscDataObj.watchNumber="+wtchRscDataObj.watchNumber+
				", wtchRscDataObj.watchPointsLatLonInfoList.size()"+
				wtchRscDataObj.watchPointsLatLonInfoList.size()); 
//		displayLatLonArrayList(latLonArrayList); 
		List<Coordinate[]> coordinateArrayList = getCoordinateArrayListForStatus(watchPointsLatLonInfoList); 

		IWireframeShape wireframeShape = target.createWireframeShape(false, descriptor, 0.0f);
int listIndex = 1; 		
		for(Coordinate[] eachCoordinateArray : coordinateArrayList) {
//			IWireframeShape newOutlineShape = target.createWireframeShape(false, descriptor, 0.0f);
			/*
			 * add status line segment
			 */
			wireframeShape.addLineSegment(eachCoordinateArray); 

			/*
			 * calculate and then add arrow head line segment
			 */
			int coordinateArrayLength = eachCoordinateArray.length; 
			if(coordinateArrayLength > 1) {
				ArrowHead arrowHead = getArrowHead(eachCoordinateArray[coordinateArrayLength-2], 
						eachCoordinateArray[coordinateArrayLength-1], paintProps); 
				Coordinate[] arrowHeadShapeCoordinateArray = arrowHead.getArrowHeadShape(); 
				
				wireframeShape.addLineSegment(arrowHeadShapeCoordinateArray);
			}
			
			displayCoordinateArray(eachCoordinateArray, listIndex);
			listIndex++; 
		}
		
			try {
				target.drawWireframeShape(wireframeShape, lineColor, lineWidth, lineStyle);
//				System.out.println("@@@@@@@@@@@@@@@@@@@@@@ START DRAWING LINE!!!!! @@@@@@@@@@@@@@@@@@@@@@@@@@"); 
//				target.drawLine(eachCoordinateArray[0].x, eachCoordinateArray[0].y, 0, 
//						eachCoordinateArray[1].x, eachCoordinateArray[1].y, 0, lineColor, lineWidth, lineStyle); 
//				target.drawLine(eachCoordinateArray[1].x, eachCoordinateArray[1].y, 0, 
//						eachCoordinateArray[2].x, eachCoordinateArray[2].y, 0, lineColor, lineWidth, lineStyle); 
//				target.drawLine(eachCoordinateArray[2].x, eachCoordinateArray[2].y, 0, 
//						eachCoordinateArray[3].x, eachCoordinateArray[3].y, 0, lineColor, lineWidth, lineStyle); 
//				target.drawLine(eachCoordinateArray[3].x, eachCoordinateArray[3].y, 0, 
//						eachCoordinateArray[0].x, eachCoordinateArray[0].y, 0, lineColor, lineWidth, lineStyle); 
			} catch (VizException e) {
				logger.error("VizException is caught when do target.drawWireframeShape(...), error=" + e.getMessage()); 
			} finally {
//				newOutlineShape.dispose();  
			}
		
		wireframeShape.dispose();  

	}
	
	private ArrowHead getArrowHead(Coordinate startPointCoordinate, Coordinate endPointCoordinate, PaintProperties paintProps) {
//		Coordinate arrawHeadCoordinate = new Coordinate(endPointLatLon.getLon(), endPointLatLon.getLat()); 
		double pointAngle = 60.0; // Angle of arrow point - defining narrowness
		//calculate the direction of arrow head
		double slope = Math.toDegrees(Math.atan2( (endPointCoordinate.y - startPointCoordinate.y), 
				(endPointCoordinate.x - startPointCoordinate.x) ));
		
		IExtent screenExtentInPixels = paintProps.getView().getExtent();

        double ratio = screenExtentInPixels.getWidth() / paintProps.getCanvasBounds().width;
        double extent = 0.2; // 1.0; 
        double height = ratio * extent * 0.5;  //*1.5    // distance away from center line
        
        ArrowHead arrowHead = new ArrowHead(endPointCoordinate, pointAngle, slope, height, ArrowHeadType.OPEN); 
        return arrowHead; 
	}
	
	private Coordinate[] concatenateCoordinateArrays(Coordinate[] coordinateArray1, Coordinate[] coordinateArray2) {
		Coordinate[] concatenatedCoordinateArray = new Coordinate[coordinateArray1.length + coordinateArray2.length]; 
		for(int i=0; i<coordinateArray1.length; i++) {
			concatenatedCoordinateArray[i] = coordinateArray1[i]; 
		}
		for(int i=0,j=coordinateArray1.length; i<coordinateArray2.length; i++, j++) {
			concatenatedCoordinateArray[j] = coordinateArray2[i]; 
		}
		return concatenatedCoordinateArray; 
	}
	
	private boolean isThunderstormWatch(String reportType) {
//		System.out.println("==inside isSevereWeatherWatch, the input reportType="+reportType); 
		boolean isSevereWeatherWatch = false; 
		if("THUNDERSTORM_REPORT".equalsIgnoreCase(reportType))
			isSevereWeatherWatch = true; 
		return isSevereWeatherWatch; 
	}
	
	private boolean isTornadoWatch(String reportType) {
//		System.out.println("==inside isSevereWeatherWatch, the input reportType="+reportType); 
		boolean isSevereWeatherWatch = false; 
		if("TORNADO_REPORT".equalsIgnoreCase(reportType))
			isSevereWeatherWatch = true; 
		return isSevereWeatherWatch; 
	}
	
	private boolean isSevereWeatherStatusNotification(String reportType) {
//		System.out.println("==inside isSevereWeatherStatusNotification, the input reportType="+reportType); 
		boolean isSevereWeatherStatusNotification = false; 
		if("STATUS_REPORT".equalsIgnoreCase(reportType))
			isSevereWeatherStatusNotification = true; 
		return isSevereWeatherStatusNotification; 
	}
	
	private void displayLatLonArrayList(List<LatLon[]> latLonArrayList) {
		System.out.println("======== the input latLonArrayList size=" + latLonArrayList.size()); 
		int index = 1; 
		for(LatLon[] latLonArray : latLonArrayList) {
			System.out.println("=========   No." + index + "LatLon Array"); 
			displayLatLonArray(latLonArray); 
			index++; 
		}
	}
	
	private void displayLatLonArray(LatLon[] latLonArray) {
		int index = 0; 
		for(LatLon latLon : latLonArray) {
			System.out.println("=========================, No." + (index+1) + ": lat="+latLon.getLat() + 
					"   lon="+latLon.getLon());
			index++; 
		}
	}
	
	private void displayCoordinateArray(Coordinate[] coordinateArray, int listIndex) {
		int index = 0; 
		System.out.println("===========$$$$$$$$$$$$$$############, No." + listIndex + "===========$$$$$$$$$$$$$$############"); 
		for(Coordinate coordinate : coordinateArray) {
			System.out.println("=========================, No." + (index+1) + ": x="+coordinate.x + 
					"   y="+coordinate.y);
			index++; 
		}
	}
	
//	private List<Coordinate[]> getCoordinateArrayListForWatch(List<LatLon[]> latLonArrayList) {
	private List<Coordinate[]> getCoordinateArrayListForWatch(List<WatchPointsLatLonInfo> watchPointsLatLonInfoList) {
		List<Coordinate[]> coordinateArrayList = new ArrayList<Coordinate[]>(watchPointsLatLonInfoList.size() * 5); 
//		for(LatLon[] latLonArray : latLonArrayList) {
		for(WatchPointsLatLonInfo eachWatchPointsLatLonInfo : watchPointsLatLonInfoList) {
			LatLon[] latLonArray = eachWatchPointsLatLonInfo.getPointsLatLonArray(); 
			Coordinate[] coordinateArray = new Coordinate[latLonArray.length+1];
			for(int i = 0; i<latLonArray.length; i++) {
				LatLon eachLatLon = latLonArray[i]; 
				Coordinate eachCoordinate = new Coordinate(eachLatLon.getLon(), eachLatLon.getLat()); 
				coordinateArray[i] = eachCoordinate; 
			}
			int coordinateArrayLength = coordinateArray.length; 
			Coordinate startingPointCoordinate = new Coordinate(latLonArray[0].getLon(), latLonArray[0].getLat()); 
			coordinateArray[coordinateArrayLength-1] = startingPointCoordinate; 
			coordinateArrayList.add(coordinateArray);  
		}
		return coordinateArrayList; 
	}

//	private List<Coordinate[]> getCoordinateArrayListForStatus(List<LatLon[]> latLonArrayList) {
	private List<Coordinate[]> getCoordinateArrayListForStatus(List<WatchPointsLatLonInfo> watchPointsLatLonInfoList) {
		List<Coordinate[]> coordinateArrayList = new ArrayList<Coordinate[]>(watchPointsLatLonInfoList.size() * 4); 
//		for(LatLon[] latLonArray : latLonArrayList) {
		for(WatchPointsLatLonInfo eachWatchPointsLatLonInfo : watchPointsLatLonInfoList) {
			LatLon[] latLonArray = eachWatchPointsLatLonInfo.getPointsLatLonArray(); 
			Coordinate[] coordinateArray = new Coordinate[latLonArray.length];
			for(int i = 0; i<latLonArray.length; i++) {
				LatLon eachLatLon = latLonArray[i]; 
				Coordinate eachCoordinate = new Coordinate(eachLatLon.getLon(), eachLatLon.getLat()); 
				coordinateArray[i] = eachCoordinate; 
			}
			coordinateArrayList.add(coordinateArray);  
		}
		return coordinateArrayList; 
	}


    /**
     * avoid null pointers exception in super class  
     */
    @Override
	protected long getDataTimeMs(IRscDataObject rscDataObj) {
		//			long dataTimeMs = rscDataObj.getDataTime().getValidTime().getTime().getTime();
		if(rscDataObj == null)
			return 0;
		
    	java.util.Calendar validTimeInCalendar = null; 
		DataTime dataTime = rscDataObj.getDataTime(); 
		if(dataTime != null) {
			validTimeInCalendar = dataTime.getValidTime(); 
			
		} else {
			logger.info("===== find IRscDataObject rscDataObj.getDataTime() return NULL!!!"); 
		}
		long dataTimeInMs = 0; 
		if(validTimeInCalendar != null)
			dataTimeInMs = validTimeInCalendar.getTimeInMillis(); 
		return dataTimeInMs; 
	}
   
    @Override
	public String getName() {
		String legendString = super.getName();
		FrameData fd = (FrameData) getCurrentFrame();
		if (fd == null || fd.getFrameTime() == null || fd.wtchDataMap.size() == 0) {
			return legendString +"-No Data";
		}
		return legendString + " "+ NmapCommon.getTimeStringFromDataTime( fd.getFrameTime(), "/");
	}
}
