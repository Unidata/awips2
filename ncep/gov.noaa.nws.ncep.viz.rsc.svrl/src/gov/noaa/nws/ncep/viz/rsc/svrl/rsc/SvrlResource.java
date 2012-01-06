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
 * </pre>
 * 
 * @author ujosyula 
 * @version 1.0
 */
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


import gov.noaa.nws.ncep.viz.localization.impl.LocalizationManager;


import com.raytheon.uf.edex.decodertools.core.LatLonPoint;

import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsResource;
import gov.noaa.nws.ncep.viz.resources.INatlCntrsResource;


import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.viz.core.rsc.jts.JTSCompiler;
import com.raytheon.viz.core.rsc.jts.JTSCompiler.PointStyle;
import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.geom.Geometry;

import com.vividsolutions.jts.geom.Point;
import com.vividsolutions.jts.io.ParseException;
import com.vividsolutions.jts.io.WKBReader;

import gov.noaa.nws.ncep.edex.common.stationTables.IStationField;
import gov.noaa.nws.ncep.edex.common.stationTables.Station;
import gov.noaa.nws.ncep.edex.common.stationTables.StationTable;

import gov.noaa.nws.ncep.common.dataplugin.aww.AwwRecord;
import gov.noaa.nws.ncep.common.dataplugin.aww.AwwUgc;
import gov.noaa.nws.ncep.common.dataplugin.aww.AwwVtec;


public class SvrlResource  extends AbstractNatlCntrsResource< SvrlResourceData, IMapDescriptor> 
implements     INatlCntrsResource, IStationField { 

	private IFont font;
	private StationTable stationTable; 
	private SvrlResourceData svrlRscData;
	private List<SvrlData> modifyList ;

	private class SvrlData implements  IRscDataObject{
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
		List<String>		countyUgc,countyNames,stateNames;     //To get all the counties for the warning
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
		stationTable = new StationTable(LocalizationManager.getInstance().getFilename("cntyStationFile") );
		HashMap<String, RequestConstraint> metadataMap =new HashMap<String, RequestConstraint>(resourceData.getMetadataMap());
		metadataMap.put("reportType",new RequestConstraint("SEVERE_WEATHER_STATEMENT"));
		resourceData.setMetadataMap(metadataMap);
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

					drawCountyOutline(svrlData,target,color,symbolWidth,lineStyle,paintProps);
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
			Envelope e = descriptor.pixelToWorld(extent, descriptor.getCRS());
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
				
					String[] text = new String[3];
					text[0]=" "+svrlData.watchNumber;
					text[1]=" "+svrlData.countyNames.get(i);
					DataTime evStartTime = new DataTime( svrlData.eventTime.getValidPeriod().getStart() );
					DataTime evEndTime = new DataTime( svrlData.eventTime.getValidPeriod().getEnd() );
					text[2] = " "+evStartTime.toString().substring(11, 16)
					+ "-" + evEndTime.toString().substring(11, 16);
					if(!svrlRscData.getWatchBoxTimeEnable() ){
						text[2] = null;
					}

					if(!svrlRscData.getWatchBoxLabelEnable() ){
						text[1]=null;
					}
					if(!svrlRscData.getWatchBoxNumberEnable()){
							text[0] = null;
				}
					target.drawStrings(font, text,   
							labelPix[0], labelPix[1], 0.0, TextStyle.NORMAL,
							new RGB[] {color, color, color},
							HorizontalAlignment.LEFT, 
							VerticalAlignment.MIDDLE );
				}
			}
		}
		catch(Exception e){
			System.out.println("svrlResource.java at Line 427"+e);
		}
	}
}
