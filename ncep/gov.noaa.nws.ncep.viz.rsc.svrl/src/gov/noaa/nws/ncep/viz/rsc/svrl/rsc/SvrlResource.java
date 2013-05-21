package gov.noaa.nws.ncep.viz.rsc.svrl.rsc;


/**
 * Svrl resourceResource - Display SVRL from aww data.
 * 
 *  This code has been developed by the SIB for use in the AWIPS2 system.
 *
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 27 May 2010           Uma Josyula  Initial creation.
 * 01/10/11				Uma Josyula	 Made changes to preprocess update and event date  
 * 02/16/2012   555        S. Gurung   Added call to setAllFramesAsPopulated() in queryRecords().
 * </pre>
 * 
 * @author ujosyula 
 * @version 1.0
 */
import java.util.ArrayList;
import java.util.Collection;

import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.StringTokenizer;

import org.eclipse.swt.graphics.RGB;
import org.geotools.geometry.jts.ReferencedEnvelope;

import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
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
import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.Point;
import com.vividsolutions.jts.io.ParseException;
import com.vividsolutions.jts.io.WKBReader;

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
import gov.noaa.nws.ncep.common.dataplugin.aww.AwwUgc;
import gov.noaa.nws.ncep.common.dataplugin.aww.AwwVtec;

/**
 * SVRL resource - Display Severe local storm watch data from aww data.
 * 
 *  This code has been developed by the SIB for use in the AWIPS2 system.
 *
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 
 * 09/11/12      852        Q. Zhou     Modified time string and alignment in drawTimeLabelWatchNumber().
 * 02/25/13      972        Greg Hull   define on NCMapDescriptor instead of IMapDescriptor
 * 
 * </pre>
 * 
 * @author qzhou 
 * @version 1.0
 */
public class SvrlResource  extends AbstractNatlCntrsResource< SvrlResourceData, NCMapDescriptor> 
implements     INatlCntrsResource, IStationField  { 

	private IFont font;
	private StationTable stationTable; 
	private SvrlResourceData svrlRscData;
	private List<SvrlData> modifyList ;

	public class SvrlData implements  IRscDataObject{
		String 				datauri;       //used as a key string
		DataTime        	issueTime;     //  issue time from bulletin
		DataTime        	evStartTime;   //  Event start time of Vtec
		DataTime        	evEndTime;     // Event  end time of of Vtec
		DataTime			eventTime;
		String 				reportType;   
		int             	countyNumPoints;
		float[]      		countyLat;
		float[]      		countyLon;
		List<LatLonPoint>	countyPoints;
		List<String>		countyUgc,countyNames,stateNames,countyFips = new ArrayList<String>();//T456  //To get all the counties for the warning
		String 				eventType;
		String          	watchNumber;   //  watch number to be displayed

		String              evTrack;
		String				evOfficeId;
		String				evPhenomena;
		String 				evProductClass;
		String				evSignificance;
		
		@Override
		public DataTime getDataTime() {
			return eventTime;

		}
	}
	protected class FrameData extends AbstractFrameData {
		HashMap<String, SvrlData> svrlDataMap;  


		public FrameData(DataTime frameTime, int timeInt) {
			super( frameTime, timeInt );
			svrlDataMap = new HashMap<String,SvrlData>();
		}
		// turn the db record into an WarnRscDataObj which will be timeMatched and 
		// added to one or more of the FrameData's.
		//
		@Override
		public boolean updateFrameData( IRscDataObject rscDataObj ) {

			if( !(rscDataObj instanceof SvrlData) ) {
				System.out.println("SVRL Resource.updateFrameData: expecting objects "+
				" of type SvrlRscDataObj???");
				return false;
			}
			SvrlData svrlRscDataObj = (SvrlData) rscDataObj;
			SvrlData existingSvrlData = svrlDataMap.get(svrlRscDataObj.datauri);
			if(existingSvrlData==null ||
					svrlRscDataObj.issueTime.greaterThan(existingSvrlData.issueTime)){
				svrlDataMap.put(svrlRscDataObj.datauri, svrlRscDataObj);

			}

			return true;
		}
	}
	@Override
	public IRscDataObject[] processRecord( Object pdo ) {
		AwwRecord awwRecord = (AwwRecord) pdo;
		ArrayList<SvrlData> svrlDataList = getAwwtData( awwRecord );
		if( svrlDataList == null ) {
			return new IRscDataObject[]{};
		}
		else {
			return svrlDataList.toArray(new SvrlData[0]);
		}
	}

	private ArrayList<SvrlData> getAwwtData( AwwRecord awwRecord) {
		SvrlData svrlStatusData = null;
		List<SvrlData> svrlDataList = new ArrayList<SvrlData>();
					try{

				Set<AwwUgc> awwUgc = awwRecord.getAwwUGC();
				for (AwwUgc awwugcs : awwUgc) {
					svrlStatusData= new SvrlData();
					svrlStatusData.issueTime =new DataTime(awwRecord.getIssueTime());
					svrlStatusData.datauri=awwRecord.getDataURI();
					svrlStatusData.watchNumber=awwRecord.getWatchNumber();
					if(awwRecord.getBullMessage().contains("THUNDERSTORM")){
						svrlStatusData.reportType="SEVERE_THUNDERSTORM_WATCH";
					}
					else if (awwRecord.getBullMessage().contains("TORNADO")){
						svrlStatusData.reportType="TORNADO_WATCH_OUTLINE_UPDATE";
					}
					String ugcline = awwugcs.getUgc();//get the ugc line to find the counties
					if(ugcline!=null && ugcline!=""){

						svrlStatusData.countyUgc = new ArrayList<String>();
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
									(svrlStatusData.countyUgc).add(temp);
								}
								else{
									(svrlStatusData.countyUgc).add(countyname.concat(temp));
								}
								i++;
							}
						}
						if(i>1){
							svrlStatusData.countyUgc.remove(i-1);
							svrlStatusData.countyUgc.remove(i-2);
						}

						svrlStatusData = getCountyNameLatLon(svrlStatusData);
					}
					int vtechNumber = awwugcs.getAwwVtecLine().size();
					if(vtechNumber>0){ //		TO OBTAIN THE EVENT START AND END TIME
						for (AwwVtec awwVtech : awwugcs.getAwwVtecLine()) {

							svrlStatusData.eventType		=awwVtech.getAction();
							svrlStatusData.evTrack			=awwVtech.getEventTrackingNumber();
							svrlStatusData.evEndTime		=new DataTime(awwVtech.getEventEndTime());
							svrlStatusData.evOfficeId		=awwVtech.getOfficeID();
							svrlStatusData.evPhenomena		=awwVtech.getPhenomena();
							svrlStatusData.evProductClass	=awwVtech.getProductClass();
							svrlStatusData.evSignificance	=awwVtech.getSignificance();
							if((awwVtech.getAction().equalsIgnoreCase("COR"))||(awwVtech.getAction().equalsIgnoreCase("CAN"))
									||(awwVtech.getAction().equalsIgnoreCase("EXP"))){
								modifyList.add(svrlStatusData);
							}
							if(awwVtech.getEventStartTime()!=null && awwVtech.getEventEndTime()!=null){
								svrlStatusData.eventTime=  new DataTime( awwVtech.getEventStartTime(),
										new TimeRange( awwVtech.getEventStartTime(),
												awwVtech.getEventEndTime() ));
							}
							else if (awwVtech.getEventEndTime()!=null){
								svrlStatusData.eventTime=  new DataTime( svrlStatusData.issueTime.getRefTimeAsCalendar(),
										new TimeRange( svrlStatusData.issueTime.getRefTimeAsCalendar(),
												awwVtech.getEventEndTime() ));
							}
							else if (awwVtech.getEventStartTime()!=null){
								svrlStatusData.eventTime=  new DataTime( awwVtech.getEventStartTime(),
										new TimeRange( awwVtech.getEventStartTime(),
												svrlStatusData.issueTime.getRefTimeAsCalendar() ));
							}
							else{
								svrlStatusData.eventTime = svrlStatusData.issueTime;
							}

						}
					}

					svrlDataList.add(svrlStatusData);

				}

			}
			catch(Exception e){
				System.out.println("at line 212"+e);
			}
	
		

		return (ArrayList<SvrlData>)svrlDataList;
	}
	private SvrlData getCountyNameLatLon(SvrlData sdata){
		sdata.countyPoints =new ArrayList<LatLonPoint>();
		sdata.countyNames =new ArrayList<String>();
		sdata.stateNames =new ArrayList<String>();
		sdata.countyLat= new float[sdata.countyUgc.size()];
		sdata.countyLon= new float[sdata.countyUgc.size()];
		
		try{
			int i=0;
			for (Iterator<String> iterator = sdata.countyUgc.iterator(); iterator.hasNext();) {
				Station station = stationTable.getStation(StationField.STID, iterator.next());
				if (station != null) {
					LatLonPoint point = new LatLonPoint (station.getLatitude(),station.getLongitude(),LatLonPoint.INDEGREES);
					sdata.countyPoints.add(point);
					sdata.countyNames.add(station.getStnname());
					sdata.stateNames.add(station.getState());
					sdata.countyLat[i]=station.getLatitude();
					sdata.countyLon[i]=station.getLongitude();
					i++;
					String s=station.getStnnum();	sdata.countyFips.add(s.length()==4?"0"+s:s);//T456
				}

			}
		}
		catch(Exception e){
			System.out.println("svrlResource.java at Line 245"+e);
		}
		sdata.countyNumPoints=sdata.countyNames.size();
		return sdata;

	}



	public SvrlResource( SvrlResourceData rscData, 
			LoadProperties loadProperties ) throws VizException {
		super(rscData, loadProperties);
		svrlRscData = (SvrlResourceData) resourceData;	
		modifyList = new ArrayList<SvrlData>();
		addRDChangedListener();//T456
	}


	protected AbstractFrameData createNewFrame( DataTime frameTime, int timeInt ) {
		return (AbstractFrameData) new FrameData( frameTime, timeInt );
	}
	
	
	@Override
	protected boolean preProcessFrameUpdate() {
		
		modifyQueue();
		
		return true;
	}

	
	
	
	private void modifyQueue(){
		if(modifyList!=null){
    		for(SvrlData modify: modifyList){
    			    			
    			for(IRscDataObject rscDataObj:newRscDataObjsQueue){
    				SvrlData candidate = (SvrlData) rscDataObj;
    			
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
//		metadataMap.put("reportType",new RequestConstraint("SEVERE_WEATHER_STATEMENT"));
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
		if( areaChangeFlag ){ areaChangeFlag = false; postProcessFrameUpdate(); }//T456: dispose old outlineShape? TODO
		FrameData currFrameData = (FrameData) frameData;

		RGB color = new RGB (155, 155, 155);
		LineStyle lineStyle = LineStyle.SOLID;
		int symbolWidth = 2;
		int symbolSize  = 2;

		Collection<SvrlData> svrlDataValues = currFrameData.svrlDataMap.values();

		for( SvrlData svrlData : svrlDataValues ) {
			Boolean draw = false, drawLabel = true;

			if(svrlRscData.getColorCodeEnable()){
				int watchNumberchoice =  Integer.parseInt(svrlData.watchNumber)%10;
			

				switch(watchNumberchoice) {
				case 0:
					color       = svrlRscData.Watchxxx0Color;
					symbolWidth = svrlRscData.Watchxxx0SymbolWidth;
					symbolSize  = svrlRscData.Watchxxx0SymbolSize;
					break;
				case 1:
					color       = svrlRscData.Watchxxx1Color;
					symbolWidth = svrlRscData.Watchxxx1SymbolWidth;
					symbolSize  = svrlRscData.Watchxxx1SymbolSize;
					break;
				case 2:
					color       = svrlRscData.Watchxxx2Color;
					symbolWidth = svrlRscData.Watchxxx2SymbolWidth;
					symbolSize  = svrlRscData.Watchxxx2SymbolSize;
					break;
				case 3:
					color       = svrlRscData.Watchxxx3Color;
					symbolWidth = svrlRscData.Watchxxx3SymbolWidth;
					symbolSize  = svrlRscData.Watchxxx3SymbolSize;
					break;
				case 4:
					color       = svrlRscData.Watchxxx4Color;
					symbolWidth = svrlRscData.Watchxxx4SymbolWidth;
					symbolSize  = svrlRscData.Watchxxx4SymbolSize;
					break;
				case 5:
					color       = svrlRscData.Watchxxx5Color;
					symbolWidth = svrlRscData.Watchxxx5SymbolWidth;
					symbolSize  = svrlRscData.Watchxxx5SymbolSize;
					break;
				case 6:
					color       = svrlRscData.Watchxxx6Color;
					symbolWidth = svrlRscData.Watchxxx6SymbolWidth;
					symbolSize  = svrlRscData.Watchxxx6SymbolSize;
					break;
				case 7:
					color       = svrlRscData.Watchxxx7Color;
					symbolWidth = svrlRscData.Watchxxx7SymbolWidth;
					symbolSize  = svrlRscData.Watchxxx7SymbolSize;
					break;
				case 8:
					color       = svrlRscData.Watchxxx8Color;
					symbolWidth = svrlRscData.Watchxxx8SymbolWidth;
					symbolSize  = svrlRscData.Watchxxx8SymbolSize;
					break;
				case 9:
					color       = svrlRscData.Watchxxx9Color;
					symbolWidth = svrlRscData.Watchxxx9SymbolWidth;
					symbolSize  = svrlRscData.Watchxxx9SymbolSize;
					break;
				}
				
			}
			else{
				if((svrlData.reportType).equalsIgnoreCase("SEVERE_THUNDERSTORM_WATCH")){
					color       = svrlRscData.thunderstormColor;
					symbolWidth = svrlRscData.thunderstormSymbolWidth;
					symbolSize  = svrlRscData.thunderstormSymbolSize;
					
				} 
				else if ((svrlData.reportType).equalsIgnoreCase("TORNADO_WATCH_OUTLINE_UPDATE")){
					color       = svrlRscData.tornadoColor;
					symbolWidth = svrlRscData.tornadoSymbolWidth;
					symbolSize  = svrlRscData.tornadoSymbolSize;
					
				}
			}

			if((((svrlData.reportType).equalsIgnoreCase("SEVERE_THUNDERSTORM_WATCH")) &&(svrlRscData.thunderstormEnable) ) ||
					(((svrlData.reportType).equalsIgnoreCase("TORNADO_WATCH_OUTLINE_UPDATE")) &&(svrlRscData.tornadoEnable) ))	{
			
			
				draw=true;
			}
			//draw the polygon

			if (draw){


			/*	if (svrlRscData.getWatchBoxMarkerEnable()){
					try{
						Color[] colors = new Color[] {new Color(color.red, color.green, color.blue)};

						for(int i = 0; i < svrlData.countyNumPoints;i++) {

							Coordinate coord = new Coordinate(
									svrlData.countyLon[i], svrlData.countyLat[i] );					
							Symbol pointSymbol = new Symbol(null,colors,symbolWidth, symbolSize*0.4 ,false,
									coord,"Symbol","DIAMOND");
							DisplayElementFactory df = new DisplayElementFactory( target, descriptor );
							ArrayList<IDisplayable> displayEls = df.createDisplayElements( pointSymbol , paintProps );
							for (IDisplayable each : displayEls) {
								each.draw(target);
								each.dispose();
							}

						}
					}
					catch(Exception e){
						System.out.println("error at line 392 "+e);
					}

				}
				*/
				if(svrlRscData.getWatchBoxOutlineEnable() ){

drawCountyOutline2(svrlData,target,color,symbolWidth,lineStyle,paintProps);//T456
				}

				if(svrlRscData.getWatchBoxTimeEnable()||svrlRscData.getWatchBoxLabelEnable()) {
					drawTimeLabelWatchNumber(svrlData,target,color);

				}
			}//if draw = true

		}//for loop 
	}//paint frame method end



	public void drawCountyOutline(SvrlData svrlData,IGraphicsTarget target,RGB color,int symbolWidth ,
			LineStyle lineStyle,PaintProperties paintProps) throws VizException{
		String countyName, stateName;
		Envelope env = null;	
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
		for(int i = 0; i < svrlData.countyNames.size();i++) {
			countyName=svrlData.countyNames.get(i);
			stateName= svrlData.stateNames.get(i);
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
				Geometry g;
				try {
					g = wkbReader.read(wkb);
					if (!(g instanceof Point)) {
						jtsCompiler.handle(g, color);
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
			

			if (newOutlineShape != null && newOutlineShape.isDrawable()){
				target.drawWireframeShape(newOutlineShape, color,symbolWidth,lineStyle );

			}
		}
	
	}

	public void drawTimeLabelWatchNumber(SvrlData svrlData,IGraphicsTarget target,RGB color){
		try{
			for(int i = 0; i < svrlData.countyNumPoints;i++) {
				double[] labelLatLon = { svrlData.countyLon[i], svrlData.countyLat[i] }; 
				double[] labelPix = descriptor.worldToPixel( labelLatLon );

				if( labelPix != null ){
				
					String[] text = new String[2];
					List<String> enabledText = new ArrayList<String>();

					if(svrlRscData.getWatchBoxLabelEnable() ){
						enabledText.add(svrlData.countyNames.get(i));
					}
					
					if(svrlRscData.getWatchBoxTimeEnable() ){ 
						DataTime startTime = new DataTime( svrlData.eventTime.getValidPeriod().getStart() );
						DataTime endTime = new DataTime( svrlData.eventTime.getValidPeriod().getEnd() );
						String temp = startTime.toString().substring(11, 13) + startTime.toString().substring(14,16)
							+ "-" + endTime.toString().substring(11, 13) + startTime.toString().substring(14,16);
						enabledText.add(temp);
					}
					
					for (int j=enabledText.size(); j<2; j++)
						enabledText.add("");
					
					text = enabledText.toArray(text);
					
					target.drawStrings(font, text,   
							labelPix[0], labelPix[1], 0.0, TextStyle.NORMAL,
							new RGB[] {color, color, color},
							HorizontalAlignment.LEFT, 
							VerticalAlignment.TOP );
				}
			}
		}
		catch(Exception e){
			System.out.println("svrlResource.java at Line 427"+e);
		}
	}
	
//---------------------------------------------------------------T456
	
	
	//for handling query part
	SvrlCountyQueryResult scqr = new SvrlCountyQueryResult();
	
	//for pre-calculate the IWiredframeShape
	private CountyResultJob  crjob = new CountyResultJob("");
	
	//for storing result of pre-calculation
	private IWireframeShape outlineShape;
	
	//Area change flag
	private boolean areaChangeFlag = false;
	
	
	@Override
	public void queryRecords() throws VizException {
		// this method is almost similar to its super class's queryRecords(), may need to be modified later
		// to use the super class's version for the common part
		
		HashMap<String, RequestConstraint> queryList = new HashMap<String, RequestConstraint>(
				resourceData.getMetadataMap());

		com.raytheon.uf.viz.core.catalog.LayerProperty prop = new com.raytheon.uf.viz.core.catalog.LayerProperty();
		prop.setDesiredProduct(com.raytheon.uf.viz.core.rsc.ResourceType.PLAN_VIEW);
		prop.setEntryQueryParameters(queryList, false);
		prop.setNumberOfImages(15000); // TODO: max # records ?? should we cap
										// this ?
		String script = null;
		script = com.raytheon.uf.viz.core.catalog.ScriptCreator.createScript(prop);

		if (script == null)
			return;

		Object[] pdoList = com.raytheon.uf.viz.core.comm.Connector.getInstance().connect(script, null, 60000);

		for (Object pdo : pdoList) {
			for( IRscDataObject dataObject : processRecord( pdo ) )	{	
				newRscDataObjsQueue.add(dataObject);
				
				scqr.buildeQueryPart(dataObject);
			}
		}
		
		scqr.populateMap();
		setAllFramesAsPopulated();
	}
	
	
	/**
     * handles the IWireframeShape pre-calculation
     * 
     * @author gzhang     
     */
    private class CountyResultJob extends org.eclipse.core.runtime.jobs.Job {
    	
    	private java.util.Map<String,Result> keyResultMap = new java.util.concurrent.ConcurrentHashMap<String,Result>();
    	
    	private IGraphicsTarget target;
    	private IMapDescriptor descriptor;
    	private RGB symbolColor = new RGB (155, 155, 155);
    	
    	public CountyResultJob(String name) {
			super(name);			
		}
    	
    	 public class Result {
         	
             public IWireframeShape outlineShape;            
//             public java.util.Map<Object, RGB> colorMap;

             private Result(IWireframeShape outlineShape,IWireframeShape nuShape,
                      			IShadedShape shadedShape,java.util.Map<Object, RGB> colorMap){
             	
             	this.outlineShape = outlineShape;
                 
//                 this.colorMap = colorMap;
             }
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
    			
    			for(SvrlData sd : fd.svrlDataMap.values()){
    				
    				Collection<Geometry> gw = new ArrayList<Geometry>();
    				
    				for(int i=0; i<sd.countyFips.size(); i++){
    					
    					for(ArrayList<Object[]> counties : scqr.getCountyResult(sd.countyFips.get(i))){
    						
    						if(counties == null) continue;
    						
    						WKBReader wkbReader = new WKBReader();
    						
    						for (Object[] result : counties) {
								
								int k = 0;
								byte[] wkb1 = (byte[]) result[k];
								
								com.vividsolutions.jts.geom.MultiPolygon countyGeo = null;
								
								try{
									
									countyGeo= (com.vividsolutions.jts.geom.MultiPolygon)wkbReader.read(wkb1);
									
									if ( countyGeo != null && countyGeo.isValid() && ( ! countyGeo.isEmpty())){
										gw.add(countyGeo);
									}
									
								}catch(Exception e){
									System.out.println("Exception in run, CountyResultJob: "+e.getMessage());
								}
							}
    					}
    				}
    				
    				if(gw.size() == 0) 
						continue;
    				else
keyResultMap.put(sd.datauri, new Result(getEachWrdoShape(gw),null,null,null));//TODO: other key
    				
    			}   			
    			
    		}    		
    		
    		return org.eclipse.core.runtime.Status.OK_STATUS;
    	}
    	
    	public IWireframeShape getEachWrdoShape(Collection<Geometry> gw){
	    	
	    	IWireframeShape newOutlineShape = target.createWireframeShape(false, descriptor, 0.0f);
			
			JTSCompiler jtsCompiler = new JTSCompiler(null,newOutlineShape, descriptor, PointStyle.CROSS);
	    	
			com.vividsolutions.jts.geom.GeometryCollection gColl=
				(com.vividsolutions.jts.geom.GeometryCollection) new com.vividsolutions.jts.geom.GeometryFactory().buildGeometry( gw );
			
			try{	
				gColl.normalize();
				
				jtsCompiler.handle(gColl, symbolColor);				
						
				newOutlineShape.compile();	
											
			}catch (Exception e) {	System.out.println("_____Exception in getEachWrdoShape(), CountyResultJob : "+e.getMessage());	}
	    	
	    	return newOutlineShape;
	    }
    }
    
    private void drawCountyOutline2(SvrlData wData, IGraphicsTarget target, RGB color, int outlineWidth, 
			  LineStyle lineStyle, PaintProperties paintProps){
	
String key = wData.datauri;//.getKey(); //TODO other key?
	  	CountyResultJob.Result result = crjob.keyResultMap.get(key);
	  
	  	
	  	if (result != null) 	  		
	  		outlineShape = result.outlineShape; 	  	  
	  	else 	  		
	  		return;
	  	
		
	  	if (outlineShape != null && outlineShape.isDrawable()){
	  		try{
	  			target.drawWireframeShape(outlineShape,  color,outlineWidth,lineStyle );
	  		} catch (VizException e) {	System.out.println("VizException in drawCountyOutline2() of SvrlResource");	}
	
	  	} else if (outlineShape == null){
			
			//target.setNeedsRefresh(true);
	  	}
	  	
	  	
    }
	
    @Override
	protected boolean postProcessFrameUpdate() {
    	
    	AbstractEditor ed = NcDisplayMngr.getActiveNatlCntrsEditor();
    	
    	crjob.setRequest( ed.getActiveDisplayPane().getTarget(), getNcMapDescriptor(), null, false, false, null); 
    	
    	return true;
    }
	
	
	/**
	 *  called in the constructor.
	 */
	private void addRDChangedListener() {
    	AbstractEditor ed = NcDisplayMngr.getActiveNatlCntrsEditor();
		ed.addRenderableDisplayChangedListener(this.new SvrlDCListener());
	}
    
    /**
	 * change the flag so outlineShape can be re-calculated
	 */
	private class SvrlDCListener implements com.raytheon.uf.viz.core.IRenderableDisplayChangedListener{

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
			System.out.println("===== find IRscDataObject rscDataObj.getDataTime() return NULL!!!"); 
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
		if (fd == null || fd.getFrameTime() == null || fd.svrlDataMap.size() == 0) {
			return legendString + "-No Data";
		}
		return legendString + " "+ NmapCommon.getTimeStringFromDataTime( fd.getFrameTime(), "/");
	}
}
