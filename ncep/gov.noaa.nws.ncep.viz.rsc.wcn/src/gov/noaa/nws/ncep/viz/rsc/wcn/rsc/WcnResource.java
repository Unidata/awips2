package gov.noaa.nws.ncep.viz.rsc.wcn.rsc;

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
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryCollection;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.MultiPolygon;
import com.vividsolutions.jts.geom.Point;
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
import gov.noaa.nws.ncep.edex.common.stationTables.IStationField;
import gov.noaa.nws.ncep.edex.common.stationTables.Station;
import gov.noaa.nws.ncep.edex.common.stationTables.StationTable;
import gov.noaa.nws.ncep.common.dataplugin.aww.AwwFips;
import gov.noaa.nws.ncep.common.dataplugin.aww.AwwRecord;
import gov.noaa.nws.ncep.common.dataplugin.aww.AwwUgc;
import gov.noaa.nws.ncep.common.dataplugin.aww.AwwVtec;

/**
 *
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 *                         Uma Josyula  Initial creation.
 * 10/01/10       #307     Greg Hull    implement processRecords and change to 
 *                                      process WcnData as the IRscDataObj
 * 01/10/11					Uma Josyula	 Made changes to preprocess update and event date
 * 07/28/11       #450      Greg Hull    NcPathManager    
 * 02/16/2012     #555      S. Gurung    Added call to setAllFramesAsPopulated() in queryRecords().                                  
 * 05/23/2012     785       Q. Zhou     Added getName for legend.
 * 08/17/2012     655       B. Hebbard  Added paintProps as parameter to IDisplayable draw
 * 09/30/2012     857       Q. Zhou     Displayed watch number. Modified time string and alignment in drawTimeLabelWatchNumber().
 *                                      Added label/time for union.  Modified fill alpha to 0.5.
 * 										Remove constraint & metamap in initResource().
 * </pre>
 * 
 * @author ujosyula 
 * @version 1.0
 */

public class WcnResource extends AbstractNatlCntrsResource< WcnResourceData, IMapDescriptor> 
implements     INatlCntrsResource, IStationField {
	private IFont font;
	private StationTable stationTable; 

	private WcnResourceData wcnRscData;
	private List<WcnRscDataObj> modifyList ;


	public class WcnRscDataObj implements IRscDataObject  {
		String 				datauri;       //used as a key string
		DataTime        	issueTime;     //  issue time from bulletin
		DataTime        	eventTime;    
		String 				reportType;   

		int             	countyNumPoints;
		float[]      		countyLat;
		float[]      		countyLon;

		List<LatLonPoint>	countyPoints;
		List<String>		countyUgc,countyNames,stateNames, countyFips = new ArrayList<String>();//T456: fips     //To get all the counties 
		String 				eventType;

		String         		watchNumber;   //  watch number to be displayed

		String              evTrack;
		DataTime			evEndTime;
		String				evOfficeId;
		String				evPhenomena;
		String 				evProductClass;
		String				evSignificance;
		
		boolean 			isCounty;//T456
		RGB color = new RGB (155, 155, 155);//T456
		public String getKey(){ return getWcnRscDataObjKey(this);}
			
		@Override public DataTime getDataTime() {  return eventTime;  }
	}


	protected class FrameData extends AbstractFrameData {
		HashMap<String, WcnRscDataObj> wcnDataMap;  

		public FrameData(DataTime frameTime, int timeInt) {
			super( frameTime, timeInt );
			wcnDataMap = new HashMap<String,WcnRscDataObj>();
		}

		public boolean updateFrameData( IRscDataObject rscDataObj ) {
			if( !(rscDataObj instanceof WcnRscDataObj) ) {
				System.out.println("WcnResource.updateFrameData: expecting objects "+
				" of type WcnRscDataObj???");
				return false;
			}

			WcnRscDataObj wcnRscDataObj = (WcnRscDataObj) rscDataObj;			
			WcnRscDataObj existingWcnData = wcnDataMap.get(wcnRscDataObj.datauri);

			if( existingWcnData == null || 
					wcnRscDataObj.issueTime.greaterThan( existingWcnData.issueTime ) ) {
				wcnDataMap.put(wcnRscDataObj.datauri, wcnRscDataObj);
			}			

			return true;
		}
	}

	public WcnResource( WcnResourceData rscData,LoadProperties loadProperties ) throws VizException { 
		super(rscData, loadProperties);	
		wcnRscData = (WcnResourceData) resourceData;
		modifyList = new ArrayList<WcnRscDataObj>();	
		addRDChangedListener();//T456
	}


	protected AbstractFrameData createNewFrame( DataTime frameTime, int timeInt ) {
		return (AbstractFrameData) new FrameData( frameTime, timeInt );
	}

	// turn the db record into an WarnRscDataObj which will be timeMatched and 
	// added to one or more of the FrameData's.

	@Override
	protected IRscDataObject[] processRecord( Object pdo ) {
		AwwRecord awwRecord = (AwwRecord) pdo;
		ArrayList<WcnRscDataObj> wcnDataList = getAwwtData( awwRecord );
		if( wcnDataList == null ) {
			return new IRscDataObject[]{};
		}
		else {
			return wcnDataList.toArray( new WcnRscDataObj[0] );
		}
	}


	private ArrayList<WcnRscDataObj> getAwwtData( AwwRecord awwRecord) {
		WcnRscDataObj wcnStatusData = null;
		List<WcnRscDataObj> wcnDataList = new ArrayList<WcnRscDataObj>();
			try{
				Set<AwwUgc> awwUgc = awwRecord.getAwwUGC();
				for (AwwUgc awwugcs : awwUgc) {
					wcnStatusData= new WcnRscDataObj();
					
					wcnStatusData.issueTime =new DataTime(awwRecord.getIssueTime());
					wcnStatusData.reportType=awwRecord.getReportType();
					wcnStatusData.datauri=awwRecord.getDataURI();
					
					if(awwRecord.getBullMessage().contains("THUNDERSTORM")){
						wcnStatusData.reportType ="THUNDERSTORM";
					}
					else if (awwRecord.getBullMessage().contains("TORNADO")){
						wcnStatusData.reportType="TORNADO";
					}

					if( ! (wcnStatusData.isCounty=isCountyUgs(awwugcs)) ) 
						setMarineZonesFips(awwugcs.getAwwFIPS(),wcnStatusData);//T456

					String ugcline = awwugcs.getUgc();//get the ugc line to find the counties
					if(ugcline!=null && ugcline!=""){
						wcnStatusData.watchNumber = awwugcs.getEventTrackingNumber();
						wcnStatusData.countyUgc = new ArrayList<String>();
						
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
									(wcnStatusData.countyUgc).add(temp);
								}
								else{
									(wcnStatusData.countyUgc).add(countyname.concat(temp));
								}
								i++;
							}
						}
						if(i>1){
							wcnStatusData.countyUgc.remove(i-1);
							wcnStatusData.countyUgc.remove(i-2);
						}

						wcnStatusData = getCountyNameLatLon(wcnStatusData);
					}

					int vtechNumber = awwugcs.getAwwVtecLine().size();
					if(vtechNumber>0){ 
						for (AwwVtec awwVtech : awwugcs.getAwwVtecLine()) {
							wcnStatusData.eventType			=awwVtech.getAction();
							wcnStatusData.evTrack			=awwVtech.getEventTrackingNumber();
							wcnStatusData.evEndTime			=new DataTime(awwVtech.getEventEndTime());
							wcnStatusData.evOfficeId		=awwVtech.getOfficeID();
							wcnStatusData.evPhenomena		=awwVtech.getPhenomena();
							wcnStatusData.evProductClass	=awwVtech.getProductClass();
							wcnStatusData.evSignificance	=awwVtech.getSignificance();

							if((awwVtech.getAction().equalsIgnoreCase("COR"))||(awwVtech.getAction().equalsIgnoreCase("CAN"))
									||(awwVtech.getAction().equalsIgnoreCase("EXP"))){
								modifyList.add(wcnStatusData);
							}
							
								if(awwVtech.getEventStartTime()!=null && awwVtech.getEventEndTime()!=null){
									wcnStatusData.eventTime=  new DataTime( awwVtech.getEventStartTime(),
											new TimeRange( awwVtech.getEventStartTime(),
													awwVtech.getEventEndTime() ));

								}
								else if (awwVtech.getEventEndTime()!=null){
									wcnStatusData.eventTime=  new DataTime( wcnStatusData.issueTime.getRefTimeAsCalendar(),
											new TimeRange( wcnStatusData.issueTime.getRefTimeAsCalendar(),
													awwVtech.getEventEndTime() ));

								}
								else if (awwVtech.getEventStartTime()!=null){
									wcnStatusData.eventTime=  new DataTime( awwVtech.getEventStartTime(),
											new TimeRange( awwVtech.getEventStartTime(),
													wcnStatusData.issueTime.getRefTimeAsCalendar() ));
								}
								else{
									wcnStatusData.eventTime = wcnStatusData.issueTime;

								}
							

						}
					}

					wcnDataList.add(wcnStatusData);
				}

			}
			catch(Exception e){
				System.out.println("at line 212"+e);
			}
		

		return (ArrayList<WcnRscDataObj>) wcnDataList;
	}

	private WcnRscDataObj getCountyNameLatLon(WcnRscDataObj wdata){
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
					i++;	if( wdata.isCounty ){ String s=station.getStnnum();	wdata.countyFips.add(s.length()==4?"0"+s:s); }//T456 AwwUgc.getAwwFIPS
				}
			}
		}
		catch(Exception e){
			System.out.println("wcnResource.java at Line 245"+e);
		}

		wdata.countyNumPoints=wdata.countyNames.size();

		return wdata;
	}
	@Override
	protected boolean preProcessFrameUpdate() {
		
		modifyQueue();
		
		return true;
	}


	private void modifyQueue(){
		if(modifyList!=null){
			for(WcnRscDataObj modify: modifyList){
				for(IRscDataObject rscDataObj:newRscDataObjsQueue){
					WcnRscDataObj candidate = (WcnRscDataObj) rscDataObj;
					
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
//		HashMap<String, RequestConstraint> metadataMap =new HashMap<String, RequestConstraint>(resourceData.getMetadataMap());
//		metadataMap.put("reportType",new RequestConstraint("WATCH_COUNTY_NOTIFICATION"));
//		resourceData.setMetadataMap(metadataMap);
		queryRecords();
	}

	@Override
	public void disposeInternal() {
	}

	public void paintFrame( AbstractFrameData frameData, 
			IGraphicsTarget target, PaintProperties paintProps) throws VizException {

		if( paintProps == null ) {
			return;
		}

		if( areaChangeFlag ){ 
			areaChangeFlag = false; 
			postProcessFrameUpdate(); }//T456: dispose old outlineShape? TODO	
		
		FrameData currFrameData = (FrameData) frameData;

//		RGB color = new RGB (155, 155, 155);
		RGB symbolColor = new RGB (155, 155, 155);
		LineStyle lineStyle = LineStyle.SOLID;
		int symbolWidth = 2;
		int symbolSize  = 2;

		Collection<WcnRscDataObj> wcnDataValues = currFrameData.wcnDataMap.values();

		for( WcnRscDataObj wcnData : wcnDataValues ) {

			Boolean draw = false, drawLabel = true;

			if(wcnRscData.getColorCodeEnable()){
				int watchNumberchoice =  Integer.parseInt(wcnData.watchNumber)%10;

				switch(watchNumberchoice) {

				case 0:
					if(wcnRscData.Watchxxx0Enable){
						color       = wcnRscData.Watchxxx0Color;
						symbolWidth = wcnRscData.Watchxxx0SymbolWidth;
						symbolSize  = wcnRscData.Watchxxx0SymbolSize;
						symbolColor = wcnRscData.Watchxxx0SymbolColor;
					}
					break;
				case 1:
					if(wcnRscData.Watchxxx1Enable){
						color       = wcnRscData.Watchxxx1Color;
						symbolWidth = wcnRscData.Watchxxx1SymbolWidth;
						symbolSize  = wcnRscData.Watchxxx1SymbolSize;
						symbolColor = wcnRscData.Watchxxx1SymbolColor;
					}
					break;
				case 2:
					if(wcnRscData.Watchxxx2Enable){
						color       = wcnRscData.Watchxxx2Color;
						symbolWidth = wcnRscData.Watchxxx2SymbolWidth;
						symbolSize  = wcnRscData.Watchxxx2SymbolSize;
						symbolColor = wcnRscData.Watchxxx2SymbolColor;
					}
					break;
				case 3:
					if(wcnRscData.Watchxxx3Enable){
						color       = wcnRscData.Watchxxx3Color;
						symbolWidth = wcnRscData.Watchxxx3SymbolWidth;
						symbolSize  = wcnRscData.Watchxxx3SymbolSize;
						symbolColor = wcnRscData.Watchxxx3SymbolColor;
					}
					break;
				case 4:
					if(wcnRscData.Watchxxx4Enable){
						color       = wcnRscData.Watchxxx4Color;
						symbolWidth = wcnRscData.Watchxxx4SymbolWidth;
						symbolSize  = wcnRscData.Watchxxx4SymbolSize;
						symbolColor = wcnRscData.Watchxxx4SymbolColor;
					}
					break;
				case 5:
					if(wcnRscData.Watchxxx5Enable){
						color       = wcnRscData.Watchxxx5Color;
						symbolWidth = wcnRscData.Watchxxx5SymbolWidth;
						symbolSize  = wcnRscData.Watchxxx5SymbolSize;
						symbolColor = wcnRscData.Watchxxx5SymbolColor;
					}
					break;
				case 6:
					if(wcnRscData.Watchxxx6Enable){
						color       = wcnRscData.Watchxxx6Color;
						symbolWidth = wcnRscData.Watchxxx6SymbolWidth;
						symbolSize  = wcnRscData.Watchxxx6SymbolSize;
						symbolColor = wcnRscData.Watchxxx6SymbolColor;
					}
					break;
				case 7:
					if(wcnRscData.Watchxxx7Enable){
						color       = wcnRscData.Watchxxx7Color;
						symbolWidth = wcnRscData.Watchxxx7SymbolWidth;
						symbolSize  = wcnRscData.Watchxxx7SymbolSize;
						symbolColor = wcnRscData.Watchxxx7SymbolColor;
					}
					break;
				case 8:
					if(wcnRscData.Watchxxx8Enable){
						color       = wcnRscData.Watchxxx8Color;
						symbolWidth = wcnRscData.Watchxxx8SymbolWidth;
						symbolSize  = wcnRscData.Watchxxx8SymbolSize;
						symbolColor = wcnRscData.Watchxxx8SymbolColor;
					}
					break;
				case 9:
					if(wcnRscData.Watchxxx9Enable){
						color       = wcnRscData.Watchxxx9Color;
						symbolWidth = wcnRscData.Watchxxx9SymbolWidth;
						symbolSize  = wcnRscData.Watchxxx9SymbolSize;
						symbolColor = wcnRscData.Watchxxx9SymbolColor;
					}
					break;
				}
			}
			else{
				if((wcnData.reportType).equalsIgnoreCase("THUNDERSTORM")){
					color       = wcnRscData.thunderstormColor;
					symbolWidth = wcnRscData.thunderstormSymbolWidth;
					symbolSize  = wcnRscData.thunderstormSymbolSize;
					symbolColor = wcnRscData.thunderstormSymbolColor;
				} 
				else if ((wcnData.reportType).equalsIgnoreCase("TORNADO")){
					color       = wcnRscData.tornadoColor;
					symbolWidth = wcnRscData.tornadoSymbolWidth;
					symbolSize  = wcnRscData.tornadoSymbolSize;
					symbolColor = wcnRscData.tornadoSymbolColor;
				}
			}

			if((((wcnData.reportType).equalsIgnoreCase("THUNDERSTORM")) &&(wcnRscData.thunderstormEnable) ) ||
					(((wcnData.reportType).equalsIgnoreCase("TORNADO")) &&(wcnRscData.tornadoEnable) ))	{

				draw=true;
			}
			//draw the polygon

			if (draw){

				if (wcnRscData.getWatchBoxMarkerEnable()){
					try{
						Color[] colors = new Color[] {new Color(color.red, color.green, color.blue)};

						for(int i = 0; i < wcnData.countyNumPoints;i++) {

							Coordinate coord = new Coordinate(
									wcnData.countyLon[i], wcnData.countyLat[i] );					
							Symbol pointSymbol = new Symbol(null,colors,symbolWidth, symbolSize*0.4 ,false,
									coord,"Symbol","FILLED_DIAMOND");
							DisplayElementFactory df = new DisplayElementFactory( target, descriptor );
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
				if (wcnRscData.getWatchBoxFillEnable()){if( ! color.equals(wcnData.color) ){ postProcessFrameUpdate(); wcnData.color=color;}
//					drawCountyOutline(wcnData,target,symbolColor,symbolWidth,lineStyle,paintProps,1);
drawCountyOutline2(wcnData,target,symbolColor,symbolWidth,lineStyle,paintProps,1);
					if(!wcnRscData.getWatchBoxUnionEnable()) {
						drawTimeLabelWatchNumber(wcnData,target,color);
					}//only if watchbox unionenable is false                              
				}

				if(wcnRscData.getWatchBoxUnionEnable()){

					drawLabel = false;	
					double allX = wcnData.countyLon[0];
    				double allY = wcnData.countyLat[0];
        			for (int i=1; i<wcnData.countyNumPoints; i++) {
        				allX += wcnData.countyLon[i];
        				allY += wcnData.countyLat[i];
        			}
        			double[] labelLatLon = { allX/wcnData.countyNumPoints, allY/wcnData.countyNumPoints }; 
//					double[] labelLatLon = { (wcnData.countyLon[0]+wcnData.countyLon[(wcnData.countyNumPoints)/2])/2, 
//							(wcnData.countyLat[0]+wcnData.countyLat[(wcnData.countyNumPoints)/2])/2 }; 

					double[] labelPix = descriptor.worldToPixel( labelLatLon );
					if( labelPix != null ){
						String[] text = new String[2];
						List<String> enabledText = new ArrayList<String>();
						
						if(wcnRscData.getWatchBoxNumberEnable()){
							enabledText.add(wcnData.watchNumber);
						}
							
						if(wcnRscData.getWatchBoxTimeEnable()){
							DataTime startTime = new DataTime( wcnData.eventTime.getValidPeriod().getStart() );
							DataTime endTime = new DataTime( wcnData.eventTime.getValidPeriod().getEnd() );
							String temp = startTime.toString().substring(11, 13) + startTime.toString().substring(14,16)
								+ "-" + endTime.toString().substring(11, 13) + startTime.toString().substring(14,16);
							enabledText.add(temp);
						}

						for (int i=enabledText.size(); i<2; i++)
							enabledText.add("");
						
						text = enabledText.toArray(text);

						target.drawStrings(font, text,   
								labelPix[0], labelPix[1], 0.0, TextStyle.NORMAL,
								new RGB[] {color, color},
								HorizontalAlignment.LEFT, 
								VerticalAlignment.TOP );
					}

drawCountyOutline2(wcnData,target,color,symbolWidth,lineStyle,paintProps,2);					
//					drawCountyOutline(wcnData,target,color,symbolWidth,lineStyle,paintProps,2);
				}
				
				else if(wcnRscData.getWatchBoxOutlineEnable() ){
drawCountyOutline2(wcnData,target,color,symbolWidth,lineStyle,paintProps,0);//T456
//					drawCountyOutline(wcnData,target,color,symbolWidth,lineStyle,paintProps,0);
				}

				if(((wcnRscData.getWatchBoxTimeEnable()||wcnRscData.getWatchBoxLabelEnable()||wcnRscData.getWatchBoxNumberEnable())
						&&drawLabel)&&!wcnRscData.watchBoxFillEnable) {
					drawTimeLabelWatchNumber(wcnData,target,color);
				}
			}

		}//if draw = true				
	}

	/* 0 for drawoutline
	   1 for shaded shape
	   2 for union*/

	public void drawCountyOutline(WcnRscDataObj wcnData,IGraphicsTarget target,RGB color,int symbolWidth ,
			LineStyle lineStyle,PaintProperties paintProps,int drawOutlineOrShadedshapeorUnion) throws VizException{
		String countyName, stateName;
		Envelope env = null;	
		try {
			PixelExtent extent = (PixelExtent) paintProps.getView().getExtent();
			Envelope e = descriptor.pixelToWorld(extent, descriptor.getCRS());
			ReferencedEnvelope ref = new ReferencedEnvelope(e, descriptor.getCRS());
			env = ref.transform(MapUtil.LATLON_PROJECTION, true);
		} catch (Exception e) {
			throw new VizException("Error transforming extent", e);
		}
		Collection<Geometry> gCollection = new ArrayList<Geometry>();;

		String geoConstraint = String.format("the_geom_0_001 && ST_SetSrid('BOX3D(%f %f, %f %f)'::box3d,4326)",
				env.getMinX(), env.getMinY(), env.getMaxX(), env.getMaxY());
		for(int i = 0; i < wcnData.countyNames.size();i++) {
			countyName=wcnData.countyNames.get(i);
			stateName= wcnData.stateNames.get(i);
			if(countyName.contains("_")){
				countyName=countyName.replace("_"," ");
			}
			StringBuilder query = new StringBuilder(
			"select AsBinary(the_geom), AsBinary(the_geom_0_001) from mapdata.county where countyname ='");
			query.append(countyName);
			query.append("' AND  state ='");
			query.append(stateName);
			query.append("' AND ");
			query.append(geoConstraint);
			query.append(";");
			List<Object[]> results = DirectDbQuery.executeQuery
			(query.toString(), "maps", QueryLanguage.SQL);
			IWireframeShape newOutlineShape = target.createWireframeShape(false, descriptor, 0.0f);
			IShadedShape newShadedShape = target.createShadedShape(false,descriptor, true);
			JTSCompiler jtsCompiler = new JTSCompiler(newShadedShape,newOutlineShape, descriptor, PointStyle.CROSS);

			WKBReader wkbReader = new WKBReader();
			for (Object[] result : results) {
				int k = 0;
				byte[] wkb = (byte[]) result[k+1];
				byte[]wkb1=(byte[]) result[k];
				Geometry g;
				MultiPolygon countyGeo = null;
				try {
					g = wkbReader.read(wkb);
					if (!(g instanceof Point)) {
						jtsCompiler.handle(g, color);
					}
					countyGeo= (MultiPolygon)wkbReader.read(wkb1);
					if ( countyGeo != null ){
						gCollection.add(countyGeo);
					}
				}
				catch (VizException e) {
					System.out.println("Error reprojecting map outline:"+e.getMessage());
				}
				catch (ParseException e) {
					e.printStackTrace();
				}
			}
			newOutlineShape.compile();
			newShadedShape.compile();
			float alpha = paintProps.getAlpha();
			if(drawOutlineOrShadedshapeorUnion ==1){
				if (newShadedShape != null && newShadedShape.isDrawable() ) {
					target.drawShadedShape(newShadedShape, alpha);
				}
			}else if(drawOutlineOrShadedshapeorUnion ==0){

				if (newOutlineShape != null && newOutlineShape.isDrawable()){
					target.drawWireframeShape(newOutlineShape, color,symbolWidth,lineStyle );
				}
			}
		}
		if(drawOutlineOrShadedshapeorUnion ==2){
			IWireframeShape newUnionShape = target.createWireframeShape(false, descriptor, 0.0f);
			JTSCompiler jtsCompile = new JTSCompiler(null,newUnionShape, descriptor, PointStyle.CROSS);
			GeometryFactory gf = new GeometryFactory();

			GeometryCollection geometryCollection =
				(GeometryCollection) gf.buildGeometry( gCollection );
			try{
				jtsCompile.handle(geometryCollection.union(), color);
				newUnionShape.compile();

				if (newUnionShape != null && newUnionShape.isDrawable()){
					target.drawWireframeShape(newUnionShape, color,symbolWidth,lineStyle );
				}	
			}
			catch (Exception e) {
				e.printStackTrace();
			}
		}
	}


	@SuppressWarnings("deprecation")
	public void drawTimeLabelWatchNumber(WcnRscDataObj wcnData,IGraphicsTarget target,RGB color){
		try{
			for(int i = 0; i < wcnData.countyNumPoints;i++) {
				double[] labelLatLon = { wcnData.countyLon[i], wcnData.countyLat[i] }; 
				double[] labelPix = descriptor.worldToPixel( labelLatLon );

				if( labelPix != null ){
					String[] text = new String[3];
					List<String> enabledText = new ArrayList<String>();

					if(wcnRscData.getWatchBoxNumberEnable() ){
						enabledText.add(wcnData.watchNumber);
					}
					
					if(wcnRscData.getWatchBoxLabelEnable() ){
						enabledText.add(wcnData.countyNames.get(i));
					}
					
					if(wcnRscData.getWatchBoxTimeEnable() ){
						DataTime startTime = new DataTime( wcnData.eventTime.getValidPeriod().getStart() );
						DataTime endTime = new DataTime( wcnData.eventTime.getValidPeriod().getEnd() );
						String temp = startTime.toString().substring(11, 13) + startTime.toString().substring(14,16)
							+ "-" + endTime.toString().substring(11, 13) + startTime.toString().substring(14,16);
						enabledText.add(temp);
					}

					for (int j=enabledText.size(); j<3; j++)
						enabledText.add("");
					
					text = enabledText.toArray(text);

					target.drawStrings(font, text,   
							labelPix[0], labelPix[1], 0.0, TextStyle.NORMAL,
							new RGB[] {color, color, color},
							HorizontalAlignment.LEFT, 
							VerticalAlignment.MIDDLE );
				}
			}
		}
		catch(Exception e){
			System.out.println("wcnResource.java at Line 427"+e);
		}
	}
	
//---------------------------------------------------------------T456:
	
	//for pre-query the database 
	private WcnCountyQueryResult queryResult;
	
	//for storing result of pre-calculation
	private IWireframeShape outlineShape;
	
	//for storing result of pre-calculation of union
	private IWireframeShape newUnionShape;
	
	//for storing result of pre-calculation of shade
	private IShadedShape shadedShape;
	
	//for pre-calculate the IWiredframeShape
	private CountyResultJob crJob = new CountyResultJob("");
	
	//if it is 1st round in the loop then draw outline since it pre-calculated for all zones
	private boolean isFirstRound = true;
	
	//Area change flag
	private boolean areaChangeFlag = false;
	
	private static java.util.logging.Logger logger = 
					java.util.logging.Logger.getLogger("gov.noaa.nws.ncep.viz.rsc.ffa.rsc.WcnResource");
	
	
	private RGB color = new RGB (155, 155, 155);
	
	
	
	
	class CountyResultJob  extends org.eclipse.core.runtime.jobs.Job{
		
		private java.util.Map<String,Result> keyResultMap = new java.util.concurrent.ConcurrentHashMap<String,Result>();
		private java.util.Map<String,Result> keyShadeMap =  new java.util.concurrent.ConcurrentHashMap<String,Result>();
		private java.util.Map<String,Result> keyUnionMap =  new java.util.concurrent.ConcurrentHashMap<String,Result>();
		
		private IGraphicsTarget target;
    	private IMapDescriptor descriptor;
    	private RGB symbolColor = new RGB (155, 155, 155);
//    	private RGB color = new RGB (155, 155, 155);
    	
        public class Result {
        	
            public IWireframeShape outlineShape;
            public IWireframeShape newUnionShape;
            public IShadedShape shadedShape;
            
            public java.util.Map<Object, RGB> colorMap;

            private Result(IWireframeShape outlineShape,IWireframeShape nuShape,
                     			IShadedShape shadedShape,java.util.Map<Object, RGB> colorMap){
            	
            	this.outlineShape = outlineShape;   
            	this.shadedShape = shadedShape;
            	this.newUnionShape = nuShape;
                this.colorMap = colorMap;
            }
        }

		public CountyResultJob(String name) {
			super(name);			
		}
		
		public void setRequest(IGraphicsTarget target, IMapDescriptor descriptor,
        		String query, boolean labeled, boolean shaded, java.util.Map<Object, RGB> colorMap){
			
			this.target = target;
			this.descriptor = descriptor;					
			this.run(null);//this.schedule();
			
    	}
		
		@Override
		protected org.eclipse.core.runtime.IStatus run(org.eclipse.core.runtime.IProgressMonitor monitor){
			
			for(AbstractFrameData afd : frameDataMap.values())	{
				
				FrameData fd = (FrameData)afd;								
			
				for(WcnRscDataObj wrdo : fd.wcnDataMap.values()){	
					
					Collection<Geometry> gw = new ArrayList<Geometry>(), gu = new ArrayList<Geometry>();					
										
					for(int i=0; i<wrdo.countyFips.size(); i++){
						
						//another loop handles multiple rows in maps mapdata.county table						
						for(ArrayList<Object[]> results : queryResult.getStateCountyResult2(wrdo.countyFips.get(i))){								
				    	
					    	if(results==null) continue;					
						
					    	WKBReader wkbReader = new WKBReader();
							for (Object[] result : results) {
								int k = 0;
								byte[] wkb1=(byte[]) result[k];
								
								MultiPolygon countyGeo = null;
								try{										
									countyGeo= (MultiPolygon)wkbReader.read(wkb1);
									
									if ( countyGeo != null && !countyGeo.isEmpty()){																					
										gu.add((MultiPolygon)countyGeo.clone());	
										gw.add(countyGeo);																			
									}
								}catch (Exception e) {	logger.info("__Error: "+e.getMessage());	}						
							}					
						}					
					}
					if(gw.size() == 0) 
						continue;	
					else 
						//keyResultMap.put(wrdo.getKey(),	new Result(getEachWrdoShape(gw),null,null,null));
						setEachWrdoShape(wrdo,gw,gu);
				}
				
			}			
			
			return org.eclipse.core.runtime.Status.OK_STATUS;
		}
		
    	public /*IWireframeShape*/void setEachWrdoShape(WcnRscDataObj wrdo,Collection<Geometry> gw,Collection<Geometry> gu){
	    	
	    	IWireframeShape newOutlineShape = target.createWireframeShape(false, descriptor, 0.0f);
	    	IWireframeShape newUnionShape = target.createWireframeShape(false, descriptor, 0.0f);
			IShadedShape newShadedShape = target.createShadedShape(false, descriptor, true);
			
			JTSCompiler jtsCompiler = new JTSCompiler(newShadedShape,newOutlineShape, descriptor, PointStyle.CROSS);
			JTSCompiler jcu = new JTSCompiler(null,newUnionShape, descriptor, PointStyle.CROSS);
			
			com.vividsolutions.jts.geom.GeometryCollection gColl=
				(com.vividsolutions.jts.geom.GeometryCollection) new com.vividsolutions.jts.geom.GeometryFactory().buildGeometry( gw );
			
			com.vividsolutions.jts.geom.GeometryCollection gCollu=
				(com.vividsolutions.jts.geom.GeometryCollection) new com.vividsolutions.jts.geom.GeometryFactory().buildGeometry( gu );

			try{	
				gColl.normalize();
				gCollu.normalize();
				
				jtsCompiler.handle(gColl, color);				
				jcu.handle(gCollu.union(),color);
				
				newOutlineShape.compile();	
				newUnionShape.compile();
				newShadedShape.compile();
				
			}catch (Exception e) {	logger.info("_____Error: "+e.getMessage());	}
			
			String key = wrdo.getKey();
			keyResultMap.put(key, new Result(newOutlineShape,null,null,null));
			keyShadeMap.put(key, new Result(null,null,newShadedShape,null));
	    	keyUnionMap.put(key, new Result(null,newUnionShape,null,null));
	    	
//	    	return newOutlineShape;
	    }
		
	}
	
    private void drawCountyOutline2(WcnRscDataObj wData, IGraphicsTarget target, RGB color, int outlineWidth, 
			  LineStyle lineStyle, PaintProperties paintProps, int drawOutlineOrShadedshapeorUnion){
  	
    	String key = wData.getKey();
	  	CountyResultJob.Result result = crJob.keyResultMap.get(key);
	  	CountyResultJob.Result resultShaded = crJob.keyShadeMap.get(key);
	  	CountyResultJob.Result resultU = crJob.keyUnionMap.get(key);
	  	
	  	if (result != null) {	  		
	  		outlineShape = result.outlineShape; 	  	  
	  	}else 	  		return;
	  	
	  	
    	if (resultShaded != null) {    					 
    		shadedShape = resultShaded.shadedShape;	    		   
    	}else     		return;
    	
    	
		if (resultU != null) {			
			newUnionShape = resultU.newUnionShape;			
		}else			return;
		
		
		if (shadedShape != null && shadedShape.isDrawable() && drawOutlineOrShadedshapeorUnion==1){	
			try{
				target.drawShadedShape(shadedShape,  0.5f);   	
			}catch (VizException e) {	logger.info("VizException in drawCountyOutline2() of WcnResource");	}
		}
		
	  	if (outlineShape != null && outlineShape.isDrawable()  && drawOutlineOrShadedshapeorUnion==0){
	  		try{
	  			target.drawWireframeShape(outlineShape,  color,outlineWidth,lineStyle );
	  		} catch (VizException e) {	logger.info("VizException in drawCountyOutline2() of WcnResource");	}
	
	  	} else if (outlineShape == null){
			
			//target.setNeedsRefresh(true);
	  	}
	  	
	  	if( newUnionShape != null && newUnionShape.isDrawable() && drawOutlineOrShadedshapeorUnion==2)   
	  		try{
				target.drawWireframeShape(newUnionShape,  color,outlineWidth,lineStyle ); 
	  		}catch (VizException e) {	logger.info("VizException in drawCountyOutline2() of WcnResource");	}
  }
	
	/**
	 *  called in the constructor.
	 */
	private void addRDChangedListener(){
		com.raytheon.viz.ui.editor.AbstractEditor editor = gov.noaa.nws.ncep.viz.ui.display.NmapUiUtils.getActiveNatlCntrsEditor();
		editor.addRenderableDisplayChangedListener(this.new WcnDCListener());
	}
	
    /**
	 * change the flag so outlineShape can be re-calculated
	 */
	private class WcnDCListener implements com.raytheon.uf.viz.core.IRenderableDisplayChangedListener{

		@Override
		public void renderableDisplayChanged(com.raytheon.uf.viz.core.IDisplayPane pane,
				com.raytheon.uf.viz.core.drawables.IRenderableDisplay newRenderableDisplay, DisplayChangeType type) {
			
			areaChangeFlag = true;
			
		}
		
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
    
    public String getWcnRscDataObjKey(WcnRscDataObj w){
    	
    	StringBuilder sb = new StringBuilder(w.evOfficeId);
    	
    	sb.append(w.evTrack).append(w.evPhenomena).append(w.evSignificance).append(w.eventType);
    	
    	for(String s : w.countyFips) sb.append(s);//TODO: is this necessary?
    	
    	return sb.toString();    	
    }
    
    @Override
	protected boolean postProcessFrameUpdate() {
    	
    	gov.noaa.nws.ncep.viz.ui.display.NCMapEditor ncme = 
    				gov.noaa.nws.ncep.viz.ui.display.NmapUiUtils.getActiveNatlCntrsEditor();
    	
    	crJob.setRequest(ncme.getActiveDisplayPane().getTarget(), descriptor, null, false, false, null); 
    	
    	return true;
    }
    
	@Override
	public void queryRecords() throws VizException {
		// this method is almost similar to its super class's queryRecords(), may need to be modified later
		// to use the super class's version for the common part
		
		HashMap<String, com.raytheon.uf.common.dataquery.requests.RequestConstraint> queryList = 
			new HashMap<String, com.raytheon.uf.common.dataquery.requests.RequestConstraint>(resourceData.getMetadataMap());
		
		com.raytheon.uf.viz.core.catalog.LayerProperty prop = new com.raytheon.uf.viz.core.catalog.LayerProperty();
		prop.setDesiredProduct(com.raytheon.uf.viz.core.rsc.ResourceType.PLAN_VIEW);
		prop.setEntryQueryParameters(queryList, false);
		prop.setNumberOfImages(15000); // TODO: max # records ?? should we cap this?
		
		String script = null;
		script = com.raytheon.uf.viz.core.catalog.ScriptCreator.createScript(prop);
		
		if (script == null)
			return;

		Object[] pdoList = com.raytheon.uf.viz.core.comm.Connector.getInstance().connect(script, null, 60000);
		
		queryResult = new WcnCountyQueryResult();

		for (Object pdo : pdoList) {
			for( IRscDataObject dataObject : processRecord( pdo ) )	{	
				newRscDataObjsQueue.add(dataObject);
				
				queryResult.buildQueryPart2(dataObject);
			}
		}
		
		queryResult.populateMap();		   
    	setAllFramesAsPopulated();
	}

	public void setMarineZonesFips(Set<AwwFips> awwFipsSet , WcnRscDataObj wrdo){
		
		if(awwFipsSet!=null && wrdo!=null)			
			for(AwwFips eachAwwFips : awwFipsSet) 				
				wrdo.countyFips.add(eachAwwFips.getFips()); 			
					
	}
	
	public boolean isCountyUgs(AwwUgc au){
		Set<AwwFips> awwFipsSet = au.getAwwFIPS();
		
		if(awwFipsSet == null) 
			return false;
		else {
			
			for(AwwFips eachAwwFips : awwFipsSet) {
				
				String eachFips = eachAwwFips.getFips(); 
				
				if(eachFips==null || eachFips.isEmpty() || eachFips.length()!=6) 
					return false;
				
				return 'C'==eachFips.charAt(2);
			}
		}
		
		return false;
	}	
	
	@Override
	public String getName() {
		String legendString = super.getName();
		FrameData fd = (FrameData) getCurrentFrame();
		if (fd == null || fd.getFrameTime() == null || fd.wcnDataMap.size() == 0) {
			return legendString + "-No Data";
		}
		return legendString + " "+ NmapCommon.getTimeStringFromDataTime( fd.getFrameTime(), "/");
	}
}
