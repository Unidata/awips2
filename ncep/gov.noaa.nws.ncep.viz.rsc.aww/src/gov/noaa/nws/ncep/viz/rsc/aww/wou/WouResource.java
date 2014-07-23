package gov.noaa.nws.ncep.viz.rsc.aww.wou;


import java.awt.Color;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.StringTokenizer;

import org.eclipse.swt.graphics.RGB;
import org.geotools.geometry.jts.ReferencedEnvelope;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.TextStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery.QueryLanguage;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.IShadedShape;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.edex.decodertools.core.LatLonPoint;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.viz.core.rsc.jts.JTSCompiler;
import com.raytheon.viz.core.rsc.jts.JTSCompiler.PointStyle;
import com.raytheon.viz.ui.editor.AbstractEditor;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryCollection;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.Point;
import com.vividsolutions.jts.geom.Polygon;
import com.vividsolutions.jts.io.ParseException;
import com.vividsolutions.jts.io.WKBReader;

import gov.noaa.nws.ncep.ui.pgen.display.DisplayElementFactory;
import gov.noaa.nws.ncep.ui.pgen.display.IDisplayable;
import gov.noaa.nws.ncep.ui.pgen.elements.Symbol;
import gov.noaa.nws.ncep.viz.common.ui.NmapCommon;
import gov.noaa.nws.ncep.viz.localization.NcPathManager;
import gov.noaa.nws.ncep.viz.localization.NcPathManager.NcPathConstants;
import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsResource;
import gov.noaa.nws.ncep.viz.resources.INatlCntrsResource;
import gov.noaa.nws.ncep.viz.ui.display.NCMapDescriptor;
import gov.noaa.nws.ncep.viz.ui.display.NcDisplayMngr;
import gov.noaa.nws.ncep.edex.common.stationTables.IStationField;
import gov.noaa.nws.ncep.edex.common.stationTables.Station;
import gov.noaa.nws.ncep.edex.common.stationTables.StationTable;
import gov.noaa.nws.ncep.common.dataplugin.aww.AwwRecord;
import gov.noaa.nws.ncep.common.dataplugin.aww.AwwRecord.AwwReportType;
import gov.noaa.nws.ncep.common.dataplugin.aww.AwwUgc;
import gov.noaa.nws.ncep.common.dataplugin.aww.AwwVtec;

/**
 * Wou resourceResource - Display WOU from aww data.
 * 
 *  This code has been developed by the SIB for use in the AWIPS2 system.
 *
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 4 May 2010           Uma Josyula  Initial creation.
 * 01/10/11				Uma Josyula	 Made changes to preprocess update and event date
 * 07/28/11      #450   Greg Hull    NcPathManager   
 * 09/28/11             Xilin Guo    Made changes to create IWireframeShape for watch number 
 * 12/27/11             Xilin Guo    Checked available watch data                          
 * 05/23/2012    785    Q. Zhou      Added getName for legend.
 * 08/17/12      655    B. Hebbard   Added paintProps as parameter to IDisplayable draw
 * 09/05/12      857    Q. Zhou      Displayed watch number. Modified time string and alignment in drawTimeLabelWatchNumber().
 *                                    Added label/time for union.  Fixed a bug for querying county. Modified fill alpha to 0.5.
 * 09/13/12      857    Q. Zhou      Remove constraint & metamap in initResource().   
 * 08/14/13     1028    G. Hull      Move to aww project. Use AwwReportType enum.
 *                                
 * </pre>
 * 
 * @author ujosyula 
 * @version 1.0
 */

public class WouResource extends AbstractNatlCntrsResource< WouResourceData, NCMapDescriptor> 
implements     INatlCntrsResource, IStationField { 

	private IFont font;
	private StationTable stationTable; 
	private WouResourceData wouRscData;
	private List<WouRscDataObj> modifyList ;

	//Area change flag
	private boolean areaChangeFlag = false;
	
		private class WouRscDataObj implements IRscDataObject {
			String 				datauri;       //used as a key string
			DataTime        	issueTime;     //  issue time from bulletin
			DataTime            eventTime;
			AwwReportType 		reportType;   
			int             	countyNumPoints;
			float[]      		countyLat;
			float[]      		countyLon;
			      
			List<LatLonPoint>	countyPoints;
			List<String>		countyUgc,countyNames,stateNames;     
			String 				eventType;


			String              watchNumber;   //  watch number to be displayed

			String              evTrack;
			DataTime			evEndTime;
			String				evOfficeId;
			String				evPhenomena;
			String 				evProductClass;
			String				evSignificance;

		@Override
		public DataTime getDataTime() {
			return eventTime;
		}
	}

	private class WouCntyRscData implements IRscDataObject {
			String 				keyStr;       //used as a key string countyName/stateName
			DataTime        	issueTime;     //  issue time from bulletin
			DataTime            eventTime;
			AwwReportType 		reportType;   
			float      			countyLat;
			float      			countyLon;
			      
			LatLonPoint			countyPoints;
			String				countyName,stateName;     
			String 				eventType;


			String              watchNumber;   //  watch number to be displayed
			DataTime			evEndTime;
			List<byte []>             g;
            List<byte []>             countyGeo;
			
		@Override
		public DataTime getDataTime() {
			return eventTime;
		}
	}

	private class WouData {
		String 				key;// watchnumber
		AwwReportType 		reportType;
		HashMap<String, WouCntyRscData> data;
		int 				numOfActCnties;
		IWireframeShape  	outlineShape;
		IShadedShape  		shadedShape;
		IWireframeShape 	unionShape;
		Boolean				rebuild;
		Boolean             colorCode;
		RGB					color;
		RGB					symbolColor;
		int					symbolWidth;
		int					symbolSize;
	}
		
	@Override 
    public void project(CoordinateReferenceSystem crs) throws VizException {
 		areaChangeFlag = true;			
	}

	protected class FrameData extends AbstractFrameData {
		HashMap<String, WouData> wouFrameData;
		
		public FrameData(DataTime frameTime, int timeInt) {
			super( frameTime, timeInt );
			wouFrameData = new HashMap<String, WouData>();
		}
		@Override
		public boolean updateFrameData( IRscDataObject rscDataObj ) {
			if( !(rscDataObj instanceof WouRscDataObj) ) {
				System.out.println("WouResource.updateFrameData: expecting objects "+
						" of type WouRscDataObj???");
				return false;
			}
  
			WouRscDataObj wouRscDataObj = (WouRscDataObj) rscDataObj;
            updateEachWatchNumberData (wouRscDataObj);
			return true;
		}
		
		private void updateEachWatchNumberData (WouRscDataObj wouRscDataObj) {
			String keyStr;

			if ( wouRscDataObj.countyNames.size() <= 0 ) {
				return;
			}
			WouData wData = wouFrameData.get(wouRscDataObj.watchNumber);
			if (wData == null) {
				wouFrameData.put(wouRscDataObj.watchNumber, createWouData(wouRscDataObj));
				wData = wouFrameData.get(wouRscDataObj.watchNumber);
			}
			
			for(int i = 0; i < wouRscDataObj.countyNames.size();i++) {
			    keyStr = wouRscDataObj.countyNames.get(i)+"/" + wouRscDataObj.stateNames.get(i);
			    //update county's status
			    WouCntyRscData existingCntyWouData = wData.data.get(keyStr);
			    if ( existingCntyWouData != null ) { 
			    	if ( wouRscDataObj.issueTime.greaterThan(existingCntyWouData.issueTime) ) {
			    		Boolean oldStatus = isDisplay(existingCntyWouData);
			    		
			    		existingCntyWouData.issueTime = wouRscDataObj.issueTime;
			    		existingCntyWouData.eventTime = wouRscDataObj.eventTime;
			    		existingCntyWouData.reportType = wouRscDataObj.reportType;
			    		existingCntyWouData.countyLat = wouRscDataObj.countyLat[i];
			    		existingCntyWouData.countyLon = wouRscDataObj.countyLon[i];
			    		existingCntyWouData.countyPoints = wouRscDataObj.countyPoints.get(i);
			    		existingCntyWouData.eventType = wouRscDataObj.eventType;
			    		existingCntyWouData.evEndTime = wouRscDataObj.evEndTime;
			    		Boolean newStatus = isDisplay(existingCntyWouData);
			    		if ( (oldStatus != newStatus) && 
			    				(wData.outlineShape !=null)) {
			    			wData.rebuild = true;
			    		}
			    		if ( oldStatus != newStatus ) {
			    			if ( newStatus ) wData.numOfActCnties ++;
			    			else wData.numOfActCnties --;
			    		}
			    	}
			    }
			    else {
			    	WouCntyRscData wouData = getCntyWonData(wouRscDataObj,i);
			    	wData.data.put(keyStr, wouData);
			    	if ( isDisplay (wouData)) {
			    		wData.numOfActCnties ++;
			    		if ( wData.outlineShape != null ) {
			    			wData.rebuild = true;
			    		}
			    	}
			    }
			}
		}
		
		
		private WouData createWouData (WouRscDataObj wouRscDataObj) {
			WouData wData = new WouData();
			wData.key = wouRscDataObj.watchNumber;
			wData.reportType = wouRscDataObj.reportType;
			wData.data = new HashMap<String,WouCntyRscData>();
			wData.numOfActCnties = 0;
			wData.colorCode = false;
			wData.rebuild = false;
			return wData;
		}
		
		private WouCntyRscData getCntyWonData (WouRscDataObj wouRscDataObj, int index ) {
			WouCntyRscData cntyWouData = new WouCntyRscData() ;
			
		    cntyWouData.keyStr 			= wouRscDataObj.countyNames.get(index) + "/" + wouRscDataObj.stateNames.get(index);
		    cntyWouData.issueTime		= new DataTime ();
		    cntyWouData.issueTime 		= wouRscDataObj.issueTime;
		    cntyWouData.eventTime 		= new DataTime();
		    cntyWouData.eventTime 		= wouRscDataObj.eventTime;
		    cntyWouData.reportType 		= wouRscDataObj.reportType;
		    cntyWouData.countyLat 		= wouRscDataObj.countyLat[index];
		    cntyWouData.countyLon 		= wouRscDataObj.countyLon[index];
		    cntyWouData.countyPoints 	= wouRscDataObj.countyPoints.get(index);
		    cntyWouData.countyName 		= wouRscDataObj.countyNames.get(index);
		    cntyWouData.stateName 		= wouRscDataObj.stateNames.get(index);
		    cntyWouData.eventType 		= wouRscDataObj.eventType;
		    cntyWouData.watchNumber 	= wouRscDataObj.watchNumber;
		    cntyWouData.evEndTime		= new DataTime();
		    cntyWouData.evEndTime 		= wouRscDataObj.evEndTime;
		    cntyWouData.g               = new ArrayList<byte []>();
            cntyWouData.countyGeo       = new ArrayList<byte []>();
			return cntyWouData;
		}
	}

	public WouResource( WouResourceData rscData, 
				LoadProperties loadProperties ) throws VizException {
			super(rscData, loadProperties);
			wouRscData = (WouResourceData) resourceData;	
			modifyList = new ArrayList<WouRscDataObj>();
	}
		
	protected AbstractFrameData createNewFrame( DataTime frameTime, int timeInt ) {
			return (AbstractFrameData) new FrameData( frameTime, timeInt );
	}

		

		// turn the db record into an WouRscDataObj which will be timeMatched and 
	    // added to one or more of the FrameData's.
	    //
	@Override
	public IRscDataObject[] processRecord( Object pdo ) {

			AwwRecord awwRecord = (AwwRecord) pdo;
			ArrayList<WouRscDataObj> wouDataList = getAwwtData( awwRecord );
			if( wouDataList == null ) {
				return new IRscDataObject[]{};
			}
			else {
				return wouDataList.toArray(new WouRscDataObj[0]);
			}
		}
	    
	    
		private ArrayList<WouRscDataObj> getAwwtData( AwwRecord awwRecord) {
			WouRscDataObj wouStatusData = null;
			List<WouRscDataObj> wouDataList = new ArrayList<WouRscDataObj>();

					try{

						Set<AwwUgc> awwUgc = awwRecord.getAwwUGC();

						for (AwwUgc awwugcs : awwUgc) {
							wouStatusData= new WouRscDataObj();
							wouStatusData.issueTime =new DataTime(awwRecord.getIssueTime());
							wouStatusData.reportType= 
									AwwReportType.getReportType( awwRecord.getReportType() );
							wouStatusData.datauri=awwRecord.getDataURI();
							
							String ugcline = awwugcs.getUgc();//get the ugc line to find the counties
							if(ugcline!=null && ugcline!=""){
								wouStatusData.watchNumber = awwugcs.getEventTrackingNumber();
								wouStatusData.countyUgc = new ArrayList<String>();
								int i=0;
								String temp;
								String countyname= ugcline.substring(0,3);
								StringTokenizer strugcs = new StringTokenizer(ugcline);
								while (strugcs.hasMoreTokens()) {
									temp=strugcs.nextToken("-");
									if (temp!=null){
										if (temp.contains("\r\r\n")){
											String temp1=temp.substring(3);
											temp=temp1;
										}
										if (temp.contains(countyname)){
											(wouStatusData.countyUgc).add(temp);
										}
										else{
											(wouStatusData.countyUgc).add(countyname.concat(temp));
										}
										i++;
									}
								}
								if(i>1){
									wouStatusData.countyUgc.remove(i-1);
									wouStatusData.countyUgc.remove(i-2);
								}

								wouStatusData = getCountyNameLatLon(wouStatusData);
							}
							int vtechNumber = awwugcs.getAwwVtecLine().size();
							if(vtechNumber>0){ 
								for (AwwVtec awwVtech : awwugcs.getAwwVtecLine()) {
						            wouStatusData.eventType			=awwVtech.getAction();
						            wouStatusData.evTrack			=awwVtech.getEventTrackingNumber();
						            wouStatusData.evEndTime			=new DataTime(awwVtech.getEventEndTime());
						            wouStatusData.evOfficeId		=awwVtech.getOfficeID();
						            wouStatusData.evPhenomena		=awwVtech.getPhenomena();
						            wouStatusData.evProductClass	=awwVtech.getProductClass();
						            wouStatusData.evSignificance	=awwVtech.getSignificance();
						           
						            if((awwVtech.getAction().equalsIgnoreCase("COR"))||(awwVtech.getAction().equalsIgnoreCase("CAN"))
						            			||(awwVtech.getAction().equalsIgnoreCase("EXP"))){
						            	modifyList.add(wouStatusData);
						            }
								
						            if(awwVtech.getEventStartTime()!=null && awwVtech.getEventEndTime()!=null){
						            	wouStatusData.eventTime=  new DataTime( awwVtech.getEventStartTime(),
						    			             new TimeRange( awwVtech.getEventStartTime(),
						    			            		 awwVtech.getEventEndTime() ));
						            }
						            else if (awwVtech.getEventEndTime()!=null){
						            	wouStatusData.eventTime=  new DataTime( wouStatusData.issueTime.getRefTimeAsCalendar(),
						    			             new TimeRange( wouStatusData.issueTime.getRefTimeAsCalendar(),
						    			            		 awwVtech.getEventEndTime() ));
						            }
						            else if (awwVtech.getEventStartTime()!=null){
						            	wouStatusData.eventTime=  new DataTime( awwVtech.getEventStartTime(),
						    			             new TimeRange( awwVtech.getEventStartTime(),
						    			            		 wouStatusData.issueTime.getRefTimeAsCalendar() ));
						            }
						            else{
						            	wouStatusData.eventTime = wouStatusData.issueTime;
						            }
								}
							}
							wouDataList.add(wouStatusData);
						}
					}
					catch(Exception e){
						System.out.println("at line 212"+e);
					}
				

			return (ArrayList<WouRscDataObj>)wouDataList;
		}
		private WouRscDataObj getCountyNameLatLon(WouRscDataObj wdata){
			wdata.countyPoints =new ArrayList<LatLonPoint>();
			wdata.countyNames =new ArrayList<String>();
			wdata.stateNames =new ArrayList<String>();
			wdata.countyLat= new float[wdata.countyUgc.size()];
			wdata.countyLon= new float[wdata.countyUgc.size()];

			try{
				int i=0;
				for (Iterator<String> iterator = wdata.countyUgc.iterator(); iterator.hasNext();) {
					Station station = stationTable.getStation(StationField.STID, iterator.next());
					if (station != null) {
						LatLonPoint point = new LatLonPoint (station.getLatitude(),station.getLongitude(),LatLonPoint.INDEGREES);
						wdata.countyPoints.add(point);
						wdata.countyNames.add(station.getStnname());
						wdata.stateNames.add(station.getState());
						wdata.countyLat[i]=station.getLatitude();
						wdata.countyLon[i]=station.getLongitude();
						i++;

					}

				}
			}
			catch(Exception e){
				System.out.println("wouResource.java at Line 245"+e);
			}
			wdata.countyNumPoints=wdata.countyNames.size();
			return wdata;

		}

		@Override
		protected boolean preProcessFrameUpdate() {

		//modifyQueue();
			return true;
		}


	
		private void modifyQueue(){
		if(modifyList!=null){
    		for(WouRscDataObj modify: modifyList){
    			    			
    			for(IRscDataObject rscDataObj:newRscDataObjsQueue){
    				WouRscDataObj candidate = (WouRscDataObj) rscDataObj;
    						if(modify.evTrack.equalsIgnoreCase(candidate.evTrack) && 
    						modify.evOfficeId.equalsIgnoreCase(candidate.evOfficeId) &&
    						modify.evPhenomena.equalsIgnoreCase(candidate.evPhenomena) &&
    						modify.evProductClass.equalsIgnoreCase(candidate.evProductClass) &&
    						modify.evSignificance.equalsIgnoreCase(candidate.evSignificance)){
    					if(candidate.eventType.equalsIgnoreCase("CAN")||candidate.eventType.equalsIgnoreCase("COR")
    							||candidate.eventType.equalsIgnoreCase("EXP")){
    					}
    					else{
    						candidate.evEndTime=modify.issueTime;
    						candidate.eventTime =new DataTime( candidate.eventTime.getRefTimeAsCalendar(),
    								new TimeRange( candidate.eventTime.getRefTimeAsCalendar(),
    										candidate.evEndTime.getRefTimeAsCalendar() ) );
    					}
    				}
    				
    			}
    		}
		}
	}
		public void initResource(IGraphicsTarget grphTarget) throws VizException {
			font = grphTarget.initializeFont("Monospace", 14, new IFont.Style[] { IFont.Style.BOLD });
			stationTable = new StationTable( 
					NcPathManager.getInstance().getStaticFile( 
							NcPathConstants.COUNTY_STN_TBL ).getAbsolutePath() );			
			queryRecords();
		}

	@Override
	protected void disposeInternal() {
		super.disposeInternal();
	}

	private void clearFrameShapes (FrameData currFrameData) {
		Collection<WouData> wouCntyDataValues = currFrameData.wouFrameData.values();
        if ( wouCntyDataValues.size() <= 0 ) {
        	return;
        }
        for( WouData wouData : wouCntyDataValues ) {
        	clearWatchNumberShapes (wouData);
        }
	}
	
	private void clearWatchNumberShapes (WouData wouData){
		if (wouData.outlineShape != null) {
			wouData.outlineShape.dispose();
			wouData.outlineShape = null;
		}
		if (wouData.shadedShape != null ) {
			wouData.shadedShape.dispose();
			wouData.shadedShape = null;
		}
		if ( wouData.unionShape !=null ) {
			wouData.unionShape.dispose();
			wouData.unionShape = null;
		}
	}

	public void paintFrame( AbstractFrameData frameData, 
			IGraphicsTarget target, PaintProperties paintProps) throws VizException {

		if( paintProps == null ) {
			return;
		}
		
		if( areaChangeFlag ){   //T456: dispose old outlineShape?/			
			areaChangeFlag = false; 
			postProcessFrameUpdate(); 
		}
		
		FrameData currFrameData = (FrameData) frameData;

		RGB color = new RGB (155, 155, 155);
		RGB symbolColor = new RGB (155, 155, 155);
		int symbolWidth = 2;
		int symbolSize  = 2;

		Collection<WouData> wouCntyDataValues = currFrameData.wouFrameData.values();
        if ( wouCntyDataValues.size() <= 0 ) {
        	return;
        }

		for( WouData wouData : wouCntyDataValues ) {

			if(wouRscData.getColorCodeEnable()){
				int watchNumberchoice =  Integer.parseInt(wouData.key)%10;
			

				switch(watchNumberchoice) {
				case 0:
					if(wouRscData.Watchxxx0Enable){
					color       = wouRscData.Watchxxx0Color;
					symbolWidth = wouRscData.Watchxxx0SymbolWidth;
					symbolSize  = wouRscData.Watchxxx0SymbolSize;
					symbolColor = wouRscData.Watchxxx0SymbolColor;
					}
					break;
				case 1:
					if(wouRscData.Watchxxx1Enable){
					color       = wouRscData.Watchxxx1Color;
					symbolWidth = wouRscData.Watchxxx1SymbolWidth;
					symbolSize  = wouRscData.Watchxxx1SymbolSize;
					symbolColor = wouRscData.Watchxxx1SymbolColor;
					}
					break;
				case 2:
					if(wouRscData.Watchxxx2Enable){
					color       = wouRscData.Watchxxx2Color;
					symbolWidth = wouRscData.Watchxxx2SymbolWidth;
					symbolSize  = wouRscData.Watchxxx2SymbolSize;
					symbolColor = wouRscData.Watchxxx2SymbolColor;
					}
					break;
				case 3:
					if(wouRscData.Watchxxx3Enable){
					color       = wouRscData.Watchxxx3Color;
					symbolWidth = wouRscData.Watchxxx3SymbolWidth;
					symbolSize  = wouRscData.Watchxxx3SymbolSize;
					symbolColor = wouRscData.Watchxxx3SymbolColor;
					}
					break;
				case 4:
					if(wouRscData.Watchxxx4Enable){
					color       = wouRscData.Watchxxx4Color;
					symbolWidth = wouRscData.Watchxxx4SymbolWidth;
					symbolSize  = wouRscData.Watchxxx4SymbolSize;
					symbolColor = wouRscData.Watchxxx4SymbolColor;
					}
					break;
				case 5:
					if(wouRscData.Watchxxx5Enable){
					color       = wouRscData.Watchxxx5Color;
					symbolWidth = wouRscData.Watchxxx5SymbolWidth;
					symbolSize  = wouRscData.Watchxxx5SymbolSize;
					symbolColor = wouRscData.Watchxxx5SymbolColor;
					}
					break;
				case 6:
					if(wouRscData.Watchxxx6Enable){
					color       = wouRscData.Watchxxx6Color;
					symbolWidth = wouRscData.Watchxxx6SymbolWidth;
					symbolSize  = wouRscData.Watchxxx6SymbolSize;
					symbolColor = wouRscData.Watchxxx6SymbolColor;
					}
					break;
				case 7:
					if(wouRscData.Watchxxx7Enable){
					color       = wouRscData.Watchxxx7Color;
					symbolWidth = wouRscData.Watchxxx7SymbolWidth;
					symbolSize  = wouRscData.Watchxxx7SymbolSize;
					symbolColor = wouRscData.Watchxxx7SymbolColor;
					}
					break;
				case 8:
					if(wouRscData.Watchxxx8Enable){
					color       = wouRscData.Watchxxx8Color;
					symbolWidth = wouRscData.Watchxxx8SymbolWidth;
					symbolSize  = wouRscData.Watchxxx8SymbolSize;
					symbolColor = wouRscData.Watchxxx8SymbolColor;
					}
					break;
				case 9:
					if(wouRscData.Watchxxx9Enable){
					color       = wouRscData.Watchxxx9Color;
					symbolWidth = wouRscData.Watchxxx9SymbolWidth;
					symbolSize  = wouRscData.Watchxxx9SymbolSize;
					symbolColor = wouRscData.Watchxxx9SymbolColor;
					}
					break;
				}
				
			}
			else{
				if( wouData.reportType == AwwReportType.SEVERE_THUNDERSTORM_WATCH ) {
					//.equalsIgnoreCase("SEVERE_THUNDERSTORM_WATCH")){
					color       = wouRscData.thunderstormColor;
					symbolWidth = wouRscData.thunderstormSymbolWidth;
					symbolSize  = wouRscData.thunderstormSymbolSize;
					symbolColor = wouRscData.thunderstormSymbolColor;
					
				} 
				else if( wouData.reportType == AwwReportType.TORNADO_WATCH_OUTLINE_UPDATE ) {
						//.equalsIgnoreCase("TORNADO_WATCH_OUTLINE_UPDATE")){
					color       = wouRscData.tornadoColor;
					symbolWidth = wouRscData.tornadoSymbolWidth;
					symbolSize  = wouRscData.tornadoSymbolSize;
					symbolColor = wouRscData.tornadoSymbolColor;
					
				}
			}
			wouData.color = new RGB (color.red,color.green, color.blue);
			wouData.symbolColor = new RGB (symbolColor.red, symbolColor.green, symbolColor.blue);
			wouData.symbolWidth = symbolWidth;
			wouData.symbolSize = symbolSize;

			if( (wouData.reportType == AwwReportType.SEVERE_THUNDERSTORM_WATCH &&
				        wouRscData.thunderstormEnable) ||
				(wouData.reportType == AwwReportType.TORNADO_WATCH_OUTLINE_UPDATE &&
				        wouRscData.tornadoEnable) ) {
				
				if( wouRscData.getWatchBoxOutlineEnable() || 
						  wouRscData.getWatchBoxFillEnable() || 
						  wouRscData.getWatchBoxUnionEnable() ){
					if ( wouData.outlineShape == null ) {
						queryCountyTable (wouData,target, paintProps);
						rebuildCntyWireFrame (wouData,target, paintProps);
						wouData.colorCode = wouRscData.getColorCodeEnable();
					}
					else if ( wouData.rebuild ){
						queryCountyTable (wouData,target, paintProps);
						rebuildCntyWireFrame (wouData,target, paintProps);
						wouData.rebuild = false;
					}
					if ( wouData.colorCode != wouRscData.getColorCodeEnable()) {
						wouData.colorCode = wouRscData.getColorCodeEnable();
						rebuildCntyWireFrame (wouData,target, paintProps);
					}
				}
			}

		}//if draw = true
		
		if ( wouRscData.getWatchBoxFillEnable() ) {
			if (wouRscData.thunderstormEnable) {
				drawSevereThunderstormWatch (currFrameData,  target,paintProps, 3);
			}
			if (wouRscData.tornadoEnable) {
				drawTornadoWatch (currFrameData,  target,paintProps, color, symbolWidth, 3);
			}
		}
		if ( wouRscData.getWatchBoxUnionEnable() ) {
			if (wouRscData.thunderstormEnable) {
				drawSevereThunderstormWatchUnion (currFrameData,  target);
			}
			if (wouRscData.tornadoEnable) {
				drawTornadoWatchUnion (currFrameData,  target);
			}
		}
		else if ( wouRscData.getWatchBoxOutlineEnable() ) {
			if (wouRscData.thunderstormEnable) {
				drawSevereThunderstormWatch (currFrameData,  target,paintProps, 1);
			}
			if (wouRscData.tornadoEnable) {
				drawTornadoWatch (currFrameData,  target,paintProps, color, symbolWidth, 1);
			}
		}
		
		if (wouRscData.getWatchBoxMarkerEnable()){
			drawWatchBoxmarker (currFrameData,target,paintProps);
		}

		if((wouRscData.getWatchBoxTimeEnable()||
				 wouRscData.getWatchBoxLabelEnable()||
				 wouRscData.getWatchBoxNumberEnable())
					&& (! wouRscData.getWatchBoxUnionEnable())) {
			drawTimeLabelWatchNumber(currFrameData,target);
		}
	}

	private void drawWatchBoxmarker (FrameData currFrameData,IGraphicsTarget target,PaintProperties paintProps ) throws VizException{
		Collection<WouData> wouCntyDataValues = currFrameData.wouFrameData.values();
		if ( wouCntyDataValues.size() <= 0 ) {
        	return;
        }
		for( WouData wouData : wouCntyDataValues ) {
			Collection<WouCntyRscData> wouDataValues = wouData.data.values();
			if ( wouDataValues.size() <= 0 ) continue;
			try{
				Color[] colors = new Color[] {new Color(wouData.color.red, wouData.color.green, wouData.color.blue)};
				for ( WouCntyRscData wData : wouDataValues) {
					if ( ! isDisplay( wData) ) continue;
					Coordinate coord = new Coordinate(
						wData.countyLon, wData.countyLat );					
					Symbol pointSymbol = new Symbol(null,colors,wouData.symbolWidth, wouData.symbolSize*0.4 ,false,
							coord,"Symbol","FILLED_BOX");
					DisplayElementFactory df = new DisplayElementFactory( target, getNcMapDescriptor() );
					ArrayList<IDisplayable> displayEls = df.createDisplayElements( pointSymbol , paintProps );
					for (IDisplayable each : displayEls) {
						each.draw(target, paintProps);
						each.dispose();
					}
				}
			}
			catch(Exception e){
				System.out.println("error at line 392 "+e);
			}
		}
	}
	
	public void queryCountyTable (WouData wouData,IGraphicsTarget target, PaintProperties paintProps) throws VizException{
	
		Envelope env = null;
		String keyStr;

		Collection<WouCntyRscData> wouCntyDataValues = wouData.data.values();
		if ( wouCntyDataValues.size() <= 0 ) {
        	return;
        }
		
		try {
			PixelExtent extent = (PixelExtent) paintProps.getView().getExtent();
			Envelope e = getNcMapDescriptor().pixelToWorld(extent, descriptor.getCRS());
			ReferencedEnvelope ref = new ReferencedEnvelope(e, descriptor.getCRS());
			env = ref.transform(MapUtil.LATLON_PROJECTION, true);
		} catch (Exception e) {
			throw new VizException("Error transforming extent", e);
		}

		String geoConstraint = String.format("the_geom_0_001 && ST_SetSrid('BOX3D(%f %f, %f %f)'::box3d,4326)",
				env.getMinX(), env.getMinY(), env.getMaxX(), env.getMaxY());
	    
		StringBuilder query = new StringBuilder(
		"select countyname, state, AsBinary(the_geom), AsBinary(the_geom_0_001) from mapdata.county where (");
        int i = 0;
	    for ( WouCntyRscData wData : wouCntyDataValues) {
	    	if ( ! isDisplay( wData) || wData.g.size() > 0 ) continue;
	    	
	    	if ( i != 0 ) {
	    		query.append(" OR ");
	    	}
	    	
	    	query.append("(countyname LIKE '%");
			query.append(wData.countyName.replace("_", " ").replace("'", "\\'")); //Queen_Anne's
			query.append("%' AND  state ='");
			query.append(wData.stateName);
			query.append("')");
			i ++;
	    	
	    }
	    
	    if ( i == 0 ) return;
         query.append(") AND ");
         query.append(geoConstraint);
		 query.append(";");
		 //System.out.println("query "+query);
		 List<Object[]> results = DirectDbQuery.executeQuery
			        (query.toString(), "maps", QueryLanguage.SQL);
		 WKBReader wkbReader = new WKBReader();

		 for (Object[] result : results) {
			String cntyName = getCountyName (wouCntyDataValues,result[0].toString().replace(" ", "_"),result[1].toString());
			if ( cntyName == null ) {
				keyStr = result[0].toString().replace(" ", "_") + "/" + result[1].toString();
			}
			else {
				keyStr = cntyName + "/" + result[1].toString();
			}
			WouCntyRscData wouCntyData = wouData.data.get(keyStr);
			if ( wouCntyData == null ) {
                continue;
			}
			byte[] wkb = (byte[]) result[3];
			byte[]wkb1=(byte[]) result[2];
			Geometry g;
			Geometry countyGeo = null;
			try {
				g = (Geometry) wkbReader.read(wkb);
				if (!(g instanceof Point)) {
//					if ( wouCntyData.g == null ) {
//						wouCntyData.g = new byte[wkb.length];
//						wouCntyData.g = wkb;
//					}
					wouCntyData.g.add(wkb);
			    }
				countyGeo= (Geometry)wkbReader.read(wkb1);
				if ( countyGeo != null ){
//					if ( wouCntyData.countyGeo == null ) {
//						wouCntyData.countyGeo = new byte[wkb1.length];
//						wouCntyData.countyGeo = wkb1;
//					}
					wouCntyData.countyGeo.add(wkb1);
				}
			} 
		    catch (ParseException e) {
			    e.printStackTrace();
		    }
		 }
	}

	public void rebuildCntyWireFrame (WouData wouData,IGraphicsTarget target, PaintProperties paintProps) throws VizException{

		Collection<WouCntyRscData> wouCntyDataValues = wouData.data.values();
		if ( wouData.numOfActCnties <= 0 ) {
        	return;
        }

		clearWatchNumberShapes (wouData);
		

	     wouData.outlineShape  = target.createWireframeShape(false, descriptor, 0.0f);
		 wouData.shadedShape  = target.createShadedShape(false,descriptor, true);
		 JTSCompiler jtsCompiler = new JTSCompiler(wouData.shadedShape,wouData.outlineShape, descriptor, PointStyle.CROSS);
		 WKBReader wkbReader = new WKBReader();

		 int num = getAvailableData (wouCntyDataValues);
		 if ( num == 0 ) return;
		 Geometry []gunion = new Geometry[num];
		 int i = 0;
		 for (WouCntyRscData wData : wouCntyDataValues) {
			 if ( ! isDisplay( wData) ) continue;
			 if ( wData.g.size() == 0 || wData.countyGeo.size() == 0) continue;
			Geometry g;

			try {
				for ( int j = 0; j < wData.g.size(); j ++ ) {
					g = (Geometry) wkbReader.read(wData.g.get(j));
					if (!(g instanceof Point)) {
						jtsCompiler.handle(g, wouData.symbolColor);
					}
				
					gunion[i] = (Geometry)wkbReader.read(wData.countyGeo.get(j));
					i ++;
				}
				
			} 
		    catch (ParseException e) {
			    e.printStackTrace();
		    }
		 }
		 wouData.outlineShape.compile();
		 wouData.shadedShape.compile();
		 GeometryFactory gf = new GeometryFactory();
		 GeometryCollection geometryCollection = gf.createGeometryCollection( gunion );
         try{
        	 wouData.unionShape  = target.createWireframeShape(false, descriptor, 0.0f);
        	 JTSCompiler jts = new JTSCompiler(null,wouData.unionShape, descriptor, PointStyle.CROSS);
        	 jts.handle(geometryCollection.union(), wouData.color);
        	 wouData.unionShape.compile();
         }
         catch (Exception e) {
                 e.printStackTrace();
         }

	}
	
	public void drawTimeLabelWatchNumber (FrameData currFrameData,IGraphicsTarget target){
		Collection<WouData> wouCntyDataValues = currFrameData.wouFrameData.values();
		if ( wouCntyDataValues.size() <= 0 ) {
        	return;
        }
		
		for( WouData wouData : wouCntyDataValues ) {
			Collection<WouCntyRscData> wouDataValues = wouData.data.values();
			if ( wouDataValues.size() <= 0 ) continue;
			try{
				for ( WouCntyRscData wData : wouDataValues) {
					if ( ! isDisplay( wData) ) continue;

					double[] labelLatLon = { wData.countyLon, wData.countyLat }; 
					double[] labelPix = descriptor.worldToPixel( labelLatLon );

					if( labelPix != null ){
						String[] text = new String[3];
						List<String> enabledText = new ArrayList<String>();
						
						if(wouRscData.getWatchBoxNumberEnable() ){
							enabledText.add(wData.watchNumber);
						}
						
						if(wouRscData.getWatchBoxLabelEnable() ){
							enabledText.add(wData.countyName);
						}
						
						if(wouRscData.getWatchBoxTimeEnable() ){
							DataTime startTime = new DataTime( wData.eventTime.getValidPeriod().getStart() );
							DataTime endTime = new DataTime( wData.eventTime.getValidPeriod().getEnd() );
							String temp = startTime.toString().substring(11, 13) + startTime.toString().substring(14,16)
								+ "-" + endTime.toString().substring(11, 13) +   endTime.toString().substring(14,16);
							enabledText.add(temp);
						}

						for (int i=enabledText.size(); i<3; i++)
							enabledText.add("");
						
						text = enabledText.toArray(text);

						target.drawStrings(font, text,   
								labelPix[0], labelPix[1], 0.0, TextStyle.NORMAL,
								new RGB[] {wouData.color, wouData.color, wouData.color},
								HorizontalAlignment.LEFT, 
								VerticalAlignment.TOP );
					}
				}
			}
			catch(Exception e){
				System.out.println("wouResource.java at Line 427"+e);
			}
		}
	}


	private void drawSevereThunderstormWatch (FrameData currFrameData, IGraphicsTarget target,PaintProperties paintProps, int drawtype ) throws VizException{
		LineStyle lineStyle = LineStyle.SOLID;
		Collection<WouData> wouCntyDataValues = currFrameData.wouFrameData.values();
        if ( wouCntyDataValues.size() <= 0 ) {
        	return;
        }

		for( WouData wouData : wouCntyDataValues ) {
			if( wouData.reportType == AwwReportType.SEVERE_THUNDERSTORM_WATCH ) {
				if ( drawtype == 1) {
					if ( wouData.outlineShape != null && wouData.outlineShape.isDrawable() ) {
						target.drawWireframeShape(wouData.outlineShape, wouData.color, wouData.symbolWidth,lineStyle);
					}
				}
				else if ( drawtype == 3) {
					if ( wouData.outlineShape != null && wouData.outlineShape.isDrawable() ) {
						float alpha = (float) 0.5;
						target.drawShadedShape(wouData.shadedShape, alpha);
					}
				}
				else {
					if ( wouData.shadedShape != null && wouData.shadedShape.isDrawable()){
						float alpha = paintProps.getAlpha();
						target.drawShadedShape(wouData.shadedShape, alpha);
					}
				}
			}
		}
	}
	
	@SuppressWarnings("deprecation")
	private void drawSevereThunderstormWatchUnion (FrameData currFrameData, IGraphicsTarget target ) throws VizException{
		LineStyle lineStyle = LineStyle.SOLID;
		Collection<WouData> wouCntyDataValues = currFrameData.wouFrameData.values();
        if ( wouCntyDataValues.size() <= 0 ) {
        	return;
        }
        			
        for( WouData wouData : wouCntyDataValues ) {
        	if( wouData.reportType == AwwReportType.SEVERE_THUNDERSTORM_WATCH ) {
        		Collection<WouCntyRscData> wouDataValues = wouData.data.values();
    			if ( wouDataValues.size() <= 0 ) 
    				continue;
    			if ( wouData.unionShape != null && wouData.unionShape.isDrawable()) {
        			
        			List<Coordinate> xyCloseList = new ArrayList<Coordinate>();
        			List<String> timeList = new ArrayList<String>();
        			String[] label = new String[2];
        			DataTime startTime = null;
					DataTime endTime = null;
					String time = "";
					String temp = "";
					
        			for ( WouCntyRscData wData : wouDataValues) {
    					if ( ! isDisplay( wData) ) 
    						continue;    					
    					xyCloseList.add( new Coordinate( wData.countyLon, wData.countyLat));
    					
    					startTime = new DataTime( wData.eventTime.getValidPeriod().getStart() );
    					endTime = new DataTime( wData.eventTime.getValidPeriod().getEnd() );
    					temp = startTime.toString().substring(11, 13) + startTime.toString().substring(14,16)
            				+ "-" + endTime.toString().substring(11, 13) +    endTime.toString().substring(14,16);
    					
    					if (time.equalsIgnoreCase("") || !time.equalsIgnoreCase(temp)) {
    						timeList.add(temp);
    						time = temp;
    					}
        			}	
    					
    				if ( !xyCloseList.get(0).equals(xyCloseList.get(xyCloseList.size()-1)) )
    						xyCloseList.add(xyCloseList.size(), xyCloseList.get(0));
    				
//    				double[] coorArray = new double[xyCloseList.size()];
//    				wouData.unionShape.addLabel(wouData.key, coorArray);
    				
    				GeometryFactory gf = new GeometryFactory();
    				Coordinate[] coorArray = new Coordinate[xyCloseList.size()];
    				for(int i=0; i<xyCloseList.size(); i++){
    						coorArray[i] = xyCloseList.get(i);
    				}
    					
    				Polygon xyClosePoly = gf.createPolygon(gf.createLinearRing(coorArray), null);
    				Point p = xyClosePoly.getCentroid();
    				
    				if (wouRscData.getWatchBoxNumberEnable()) {
    					label[0] = wouData.key;
    					if (wouRscData.getWatchBoxTimeEnable()) 
    						label[1] = timeList.get(0);
    					else
    						label[1] = "";
    				}
    				else if (wouRscData.getWatchBoxTimeEnable()) {
    					label[0] = timeList.get(0);
    					label[1] = "";
    				}
    				else {
    					label[0] = "";
    					label[1] = "";
    				}
        			
    				double allX = xyCloseList.get(0).x;
    				double allY = xyCloseList.get(0).y;
        			for (int i=1; i<xyCloseList.size(); i++) {
        				allX += xyCloseList.get(i).x;
        				allY += xyCloseList.get(i).y;
        			}
        			double[] labelLatLon = { allX/xyCloseList.size(), allY/xyCloseList.size() }; 
//    				double[] labelLatLon = { (xyCloseList.get(0).x + xyCloseList.get(xyCloseList.size()/2).x)/2, 
//    						(xyCloseList.get(0).y + xyCloseList.get(xyCloseList.size()/2).y)/2 }; 
        			
					double[] labelPix = descriptor.worldToPixel( labelLatLon );
        			target.drawStrings(font, label,   
        					labelPix[0], labelPix[1], 0.0, TextStyle.NORMAL,
        					new RGB[] {wouData.color, wouData.color, wouData.color},
        					HorizontalAlignment.LEFT, 
        					VerticalAlignment.TOP );
        			
        			target.drawWireframeShape(wouData.unionShape, wouData.color, wouData.symbolWidth, lineStyle ); 
        			
        		}
        	}
        }
	}
	
	private void drawTornadoWatch (FrameData currFrameData, IGraphicsTarget target,PaintProperties paintProps, RGB lineColor,int lineWidth, int drawtype ) throws VizException{
		LineStyle lineStyle = LineStyle.SOLID;
		Collection<WouData> wouCntyDataValues = currFrameData.wouFrameData.values();
        if ( wouCntyDataValues.size() <= 0 ) {
        	return;
        }

		for( WouData wouData : wouCntyDataValues ) {
			if( wouData.reportType == AwwReportType.TORNADO_WATCH_OUTLINE_UPDATE ) {
				if ( drawtype == 1) {
					if ( wouData.outlineShape != null && wouData.outlineShape.isDrawable() ) {
						target.drawWireframeShape(wouData.outlineShape, wouData.color, wouData.symbolWidth,lineStyle);
					}
				}
				else if ( drawtype == 3) {
					if ( wouData.outlineShape != null && wouData.outlineShape.isDrawable() ) {
						float alpha = (float) 0.5;
						target.drawShadedShape(wouData.shadedShape, alpha);
					}
				}
				else {
					if ( wouData.shadedShape != null && wouData.shadedShape.isDrawable()){
						float alpha = paintProps.getAlpha();
						target.drawShadedShape(wouData.shadedShape, alpha);
					}
				}
			}
		}
	}
	
	private void drawTornadoWatchUnion (FrameData currFrameData, IGraphicsTarget target ) throws VizException{
		LineStyle lineStyle = LineStyle.SOLID;
		Collection<WouData> wouCntyDataValues = currFrameData.wouFrameData.values();
        if ( wouCntyDataValues.size() <= 0 ) {
        	return;
        }
        for( WouData wouData : wouCntyDataValues ) {
        	if( wouData.reportType == AwwReportType.TORNADO_WATCH_OUTLINE_UPDATE ) {
//        		if ( wouData.unionShape != null && wouData.unionShape.isDrawable()) {
//        			target.drawWireframeShape(wouData.unionShape, wouData.color,wouData.symbolWidth,lineStyle );
//        		}
        		Collection<WouCntyRscData> wouDataValues = wouData.data.values();
    			if ( wouDataValues.size() <= 0 ) 
    				continue;
    			if ( wouData.unionShape != null && wouData.unionShape.isDrawable()) {
        			
        			List<Coordinate> xyCloseList = new ArrayList<Coordinate>();
        			List<String> timeList = new ArrayList<String>();
        			String[] label = new String[2];
        			DataTime startTime = null;
					DataTime endTime = null;
					String time = "";
					String temp = "";
					
        			for ( WouCntyRscData wData : wouDataValues) {
    					if ( ! isDisplay( wData) ) 
    						continue;    					
    					xyCloseList.add( new Coordinate( wData.countyLon, wData.countyLat));
    					
    					startTime = new DataTime( wData.eventTime.getValidPeriod().getStart() );
    					endTime = new DataTime( wData.eventTime.getValidPeriod().getEnd() );
    					temp = startTime.toString().substring(11, 13) + startTime.toString().substring(14,16)
            				+ "-" + endTime.toString().substring(11, 13) +    endTime.toString().substring(14,16);
    					
    					if (time.equalsIgnoreCase("") || !time.equalsIgnoreCase(temp)) {
    						timeList.add(temp);
    						time = temp;
    					}
        			}	
    					
    				if ( !xyCloseList.get(0).equals(xyCloseList.get(xyCloseList.size()-1)) )
    						xyCloseList.add(xyCloseList.size(), xyCloseList.get(0));
    				
//    				double[] coorArray = new double[xyCloseList.size()];
//    				wouData.unionShape.addLabel(wouData.key, coorArray);
    				
    				GeometryFactory gf = new GeometryFactory();
    				Coordinate[] coorArray = new Coordinate[xyCloseList.size()];
    				for(int i=0; i<xyCloseList.size(); i++){
    						coorArray[i] = xyCloseList.get(i);
    				}
    					
    				Polygon xyClosePoly = gf.createPolygon(gf.createLinearRing(coorArray), null);
    				Point p = xyClosePoly.getCentroid();
    				
    				if (wouRscData.getWatchBoxNumberEnable()) {
    					label[0] = wouData.key;
    					if (wouRscData.getWatchBoxTimeEnable()) 
    						label[1] = timeList.get(0);
    					else
    						label[1] = "";
    				}
    				else if (wouRscData.getWatchBoxTimeEnable()) {
    					label[0] = timeList.get(0);
    					label[1] = "";
    				}
    				else {
    					label[0] = "";
    					label[1] = "";
    				}
        			
    				double allX = xyCloseList.get(0).x;
    				double allY = xyCloseList.get(0).y;
        			for (int i=1; i<xyCloseList.size(); i++) {
        				allX += xyCloseList.get(i).x;
        				allY += xyCloseList.get(i).y;
        			}
        			double[] labelLatLon = { allX/xyCloseList.size(), allY/xyCloseList.size() }; 
//    				double[] labelLatLon = { (xyCloseList.get(0).x + xyCloseList.get(xyCloseList.size()/2).x)/2, 
//    						(xyCloseList.get(0).y + xyCloseList.get(xyCloseList.size()/2).y)/2 }; 
        			
					double[] labelPix = descriptor.worldToPixel( labelLatLon );
        			target.drawStrings(font, label,   
        					labelPix[0], labelPix[1], 0.0, TextStyle.NORMAL,
        					new RGB[] {wouData.color, wouData.color, wouData.color},
        					HorizontalAlignment.LEFT, 
        					VerticalAlignment.TOP );
        			
        			target.drawWireframeShape(wouData.unionShape, wouData.color, wouData.symbolWidth, lineStyle ); 
        			
        		}       	
        	}
		}
	}
	
	private Boolean isDisplay ( WouCntyRscData wData ) {
		Boolean display = true;
		if (wData.eventType.equalsIgnoreCase("CAN")||wData.eventType.equalsIgnoreCase("COR")
				||wData.eventType.equalsIgnoreCase("EXP") ) {
			display = false;
		}
		return display;
	}
	
	private int getAvailableData (Collection<WouCntyRscData> wouCntyDataValues ) {
		int num= 0;
		
		for (WouCntyRscData wData : wouCntyDataValues) {
			 if ( ! isDisplay( wData) ) continue;
			 if ( wData.g.size() == 0 || wData.countyGeo.size() == 0) continue;
			 num += wData.g.size();
		}
		return num;
	}
	
	private String getCountyName ( Collection<WouCntyRscData> wouCntyDataValues, String retCntyName, String retStName ){
		String cntyName=null;
		for (WouCntyRscData wData : wouCntyDataValues) {
			 if ( ! isDisplay( wData) ) continue;
			 if ( retCntyName.contains(wData.countyName) && wData.stateName.equalsIgnoreCase(retStName)) {
				 cntyName = wData.countyName;
				 break;
			 }
			 
		}		
		return cntyName;
	}
	
	@Override
	public String getName() {
		String legendString = super.getName();
		FrameData fd = (FrameData) getCurrentFrame();
		if (fd == null || fd.getFrameTime() == null || fd.wouFrameData.size() == 0) {
			return legendString + "-No Data";
		}
		return legendString + " "+ NmapCommon.getTimeStringFromDataTime( fd.getFrameTime(), "/");
	}
}
