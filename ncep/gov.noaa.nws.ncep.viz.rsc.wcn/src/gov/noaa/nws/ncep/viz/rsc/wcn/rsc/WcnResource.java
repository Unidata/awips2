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

import gov.noaa.nws.ncep.ui.pgen.display.DisplayElementFactory;
import gov.noaa.nws.ncep.ui.pgen.display.IDisplayable;
import gov.noaa.nws.ncep.ui.pgen.elements.Symbol;
import gov.noaa.nws.ncep.viz.localization.impl.LocalizationManager;



import com.raytheon.uf.edex.decodertools.core.LatLonPoint;

import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsResource;
import gov.noaa.nws.ncep.viz.resources.INatlCntrsResource;


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

import gov.noaa.nws.ncep.edex.common.stationTables.IStationField;
import gov.noaa.nws.ncep.edex.common.stationTables.Station;
import gov.noaa.nws.ncep.edex.common.stationTables.StationTable;
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
 * 
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


	private class WcnRscDataObj implements IRscDataObject  {
		String 				datauri;       //used as a key string
		DataTime        	issueTime;     //  issue time from bulletin
		DataTime        	eventTime;    
		String 				reportType;   

		int             	countyNumPoints;
		float[]      		countyLat;
		float[]      		countyLon;

		List<LatLonPoint>	countyPoints;
		List<String>		countyUgc,countyNames,stateNames;     //To get all the counties 
		String 				eventType;

		String         		watchNumber;   //  watch number to be displayed

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

	public WcnResource( WcnResourceData rscData, 
			LoadProperties loadProperties ) throws VizException {
		super(rscData, loadProperties);
		wcnRscData = (WcnResourceData) resourceData;	
		modifyList = new ArrayList<WcnRscDataObj>();
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
					wcnStatusData.watchNumber=awwRecord.getWatchNumber();
					if(awwRecord.getBullMessage().contains("THUNDERSTORM")){
						wcnStatusData.reportType ="THUNDERSTORM";
					}
					else if (awwRecord.getBullMessage().contains("TORNADO")){
						wcnStatusData.reportType="TORNADO";
					}


					String ugcline = awwugcs.getUgc();//get the ugc line to find the counties
					if(ugcline!=null && ugcline!=""){

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
					i++;
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
		stationTable = new StationTable( LocalizationManager.getInstance().getFilename("countyStationFileName") );
		HashMap<String, RequestConstraint> metadataMap =new HashMap<String, RequestConstraint>(resourceData.getMetadataMap());
		metadataMap.put("reportType",new RequestConstraint("WATCH_COUNTY_NOTIFICATION"));
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
				if (wcnRscData.getWatchBoxFillEnable()){
					drawCountyOutline(wcnData,target,symbolColor,symbolWidth,lineStyle,paintProps,1);

					if(!wcnRscData.getWatchBoxUnionEnable()) {
						drawTimeLabelWatchNumber(wcnData,target,color);
					}//only if watchbox unionenable is false                              
				}

				if(wcnRscData.getWatchBoxUnionEnable()){

					drawLabel = false;	
					double[] labelLatLon = { (wcnData.countyLon[0]+wcnData.countyLon[(wcnData.countyNumPoints)/2])/2, 
							(wcnData.countyLat[0]+wcnData.countyLat[(wcnData.countyNumPoints)/2])/2 }; 

					double[] labelPix = descriptor.worldToPixel( labelLatLon );
					if( labelPix != null ){
						String[] text = new String[2];
						//text[0]=null;text[1]=null;
						text[0]="";text[1]="";
						if(wcnRscData.getWatchBoxNumberEnable()){
							text[0]=" "+wcnData.watchNumber;
						}
						if(wcnRscData.getWatchBoxTimeEnable()){
							DataTime startTime = new DataTime( wcnData.eventTime.getValidPeriod().getStart() );
							DataTime endTime = new DataTime( wcnData.eventTime.getValidPeriod().getEnd() );
							text[1] = " "+startTime.toString().substring(11, 16)
							+ "-" + endTime.toString().substring(11, 16);
						}
						target.drawStrings(font, text,   
								labelPix[0], labelPix[1], 0.0, TextStyle.NORMAL,
								new RGB[] {color, color},
								HorizontalAlignment.LEFT, 
								VerticalAlignment.MIDDLE );
					}

					drawCountyOutline(wcnData,target,color,symbolWidth,lineStyle,paintProps,2);
				}
				else if(wcnRscData.getWatchBoxOutlineEnable() ){

					drawCountyOutline(wcnData,target,color,symbolWidth,lineStyle,paintProps,0);
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


	public void drawTimeLabelWatchNumber(WcnRscDataObj wcnData,IGraphicsTarget target,RGB color){
		try{
			for(int i = 0; i < wcnData.countyNumPoints;i++) {
				double[] labelLatLon = { wcnData.countyLon[i], wcnData.countyLat[i] }; 
				double[] labelPix = descriptor.worldToPixel( labelLatLon );

				if( labelPix != null ){
					String[] text = new String[3];
					text[0]=" "+wcnData.watchNumber;
					text[1]=" "+wcnData.countyNames.get(i);
					DataTime startTime = new DataTime( wcnData.eventTime.getValidPeriod().getStart() );
					DataTime endTime = new DataTime( wcnData.eventTime.getValidPeriod().getEnd() );
					text[2] = " "+startTime.toString().substring(11, 16)
					+ "-" + endTime.toString().substring(11, 16);

					if(!wcnRscData.getWatchBoxTimeEnable() ){
						text[2] = "";
					}
					if(!wcnRscData.getWatchBoxLabelEnable() ){
						text[1]="";
					}
					if(!wcnRscData.getWatchBoxNumberEnable() ){
						text[0] = "";
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
			System.out.println("wcnResource.java at Line 427"+e);
		}
	}
}
