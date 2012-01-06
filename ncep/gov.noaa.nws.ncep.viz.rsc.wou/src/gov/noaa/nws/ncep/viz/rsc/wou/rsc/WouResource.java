package gov.noaa.nws.ncep.viz.rsc.wou.rsc;


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
 * 
 * </pre>
 * 
 * @author ujosyula 
 * @version 1.0
 */

public class WouResource extends AbstractNatlCntrsResource< WouResourceData, IMapDescriptor> 
implements     INatlCntrsResource, IStationField { 

	private IFont font;
	private StationTable stationTable; 
	private WouResourceData wouRscData;
	private List<WouRscDataObj> modifyList ;

		private class WouRscDataObj implements IRscDataObject {
			String 				datauri;       //used as a key string
			DataTime        	issueTime;     //  issue time from bulletin
			DataTime            eventTime;
			String 				reportType;   
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


	protected class FrameData extends AbstractFrameData {
		HashMap<String, WouRscDataObj> wouDataMap;  

		public FrameData(DataTime frameTime, int timeInt) {
			super( frameTime, timeInt );
			wouDataMap = new HashMap<String,WouRscDataObj>();
		}
		@Override
		public boolean updateFrameData( IRscDataObject rscDataObj ) {
			if( !(rscDataObj instanceof WouRscDataObj) ) {
				System.out.println("WouResource.updateFrameData: expecting objects "+
						" of type WouRscDataObj???");
				return false;
			}
  
			WouRscDataObj warnRscDataObj = (WouRscDataObj) rscDataObj;
			WouRscDataObj existingWouData = wouDataMap.get(warnRscDataObj.datauri);
			if( existingWouData == null || 
					warnRscDataObj.issueTime.greaterThan( existingWouData.issueTime ) ) {//I doubt this condition
				wouDataMap.put(warnRscDataObj.datauri, warnRscDataObj);
			}
			return true;
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
							wouStatusData.reportType=awwRecord.getReportType();
							wouStatusData.datauri=awwRecord.getDataURI();
							wouStatusData.watchNumber=awwRecord.getWatchNumber();
							String ugcline = awwugcs.getUgc();//get the ugc line to find the counties
							if(ugcline!=null && ugcline!=""){

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

		modifyQueue();
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
			stationTable = new StationTable( LocalizationManager.getInstance().getFilename("countyStationFile") );
			HashMap<String, RequestConstraint> metadataMap =new HashMap<String, RequestConstraint>(resourceData.getMetadataMap());
			metadataMap.put("issueOffice",new RequestConstraint("KWNS"));
			metadataMap.put("wmoHeader",new RequestConstraint("WOUS64"));
			String wou[]={"SEVERE_THUNDERSTORM_WATCH", "TORNADO_WATCH_OUTLINE_UPDATE"};
			RequestConstraint ids = new RequestConstraint();
	        ids.setConstraintType(ConstraintType.IN);
	        ids.setConstraintValueList(wou);
			metadataMap.put("reportType",ids);
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

		Collection<WouRscDataObj> wouDataValues = currFrameData.wouDataMap.values();

		for( WouRscDataObj wouData : wouDataValues ) {
			Boolean draw = false, drawLabel = true;

			if(wouRscData.getColorCodeEnable()){
				int watchNumberchoice =  Integer.parseInt(wouData.watchNumber)%10;
			

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
				if((wouData.reportType).equalsIgnoreCase("SEVERE_THUNDERSTORM_WATCH")){
					color       = wouRscData.thunderstormColor;
					symbolWidth = wouRscData.thunderstormSymbolWidth;
					symbolSize  = wouRscData.thunderstormSymbolSize;
					symbolColor = wouRscData.thunderstormSymbolColor;
					
				} 
				else if ((wouData.reportType).equalsIgnoreCase("TORNADO_WATCH_OUTLINE_UPDATE")){
					color       = wouRscData.tornadoColor;
					symbolWidth = wouRscData.tornadoSymbolWidth;
					symbolSize  = wouRscData.tornadoSymbolSize;
					symbolColor = wouRscData.tornadoSymbolColor;
					
				}
			}

			if((((wouData.reportType).equalsIgnoreCase("SEVERE_THUNDERSTORM_WATCH")) &&(wouRscData.thunderstormEnable) ) ||
					(((wouData.reportType).equalsIgnoreCase("TORNADO_WATCH_OUTLINE_UPDATE")) &&(wouRscData.tornadoEnable) ))	{
			
				draw=true;
			}
			//draw the polygon

			if (draw){


				if (wouRscData.getWatchBoxMarkerEnable()){
					try{
						Color[] colors = new Color[] {new Color(color.red, color.green, color.blue)};

						for(int i = 0; i < wouData.countyNumPoints;i++) {

							Coordinate coord = new Coordinate(
									wouData.countyLon[i], wouData.countyLat[i] );					
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
				if (wouRscData.getWatchBoxFillEnable()){
					drawCountyOutline(wouData,target,symbolColor,symbolWidth,lineStyle,paintProps,1);
					drawTimeLabelWatchNumber(wouData,target,color);
				}

				if(wouRscData.getWatchBoxUnionEnable()){

					drawLabel = false;	
					double[] labelLatLon = { (wouData.countyLon[0]+wouData.countyLon[(wouData.countyNumPoints)/2])/2, 
							(wouData.countyLat[0]+wouData.countyLat[(wouData.countyNumPoints)/2])/2 }; 

					double[] labelPix = descriptor.worldToPixel( labelLatLon );
					if( labelPix != null ){
						String[] text = new String[2];
						text[0]=null;text[1]=null;
						if(wouRscData.getWatchBoxNumberEnable()){
							text[0]=" "+wouData.watchNumber;
						}
						if(wouRscData.getWatchBoxTimeEnable()){
							DataTime startTime = new DataTime( wouData.eventTime.getValidPeriod().getStart() );
							DataTime endTime = new DataTime( wouData.eventTime.getValidPeriod().getEnd() );
							text[1] = " "+startTime.toString().substring(11, 16)
							+ "-" + endTime.toString().substring(11, 16);
						}
						target.drawStrings(font, text,   
								labelPix[0], labelPix[1], 0.0, TextStyle.NORMAL,
								new RGB[] {color, color},
								HorizontalAlignment.LEFT, 
								VerticalAlignment.MIDDLE );
					}
					drawCountyOutline(wouData,target,color,symbolWidth,lineStyle,paintProps,2);


				}
				else if(wouRscData.getWatchBoxOutlineEnable() ){

					drawCountyOutline(wouData,target,color,symbolWidth,lineStyle,paintProps,0);
				}

				if(((wouRscData.getWatchBoxTimeEnable()||wouRscData.getWatchBoxLabelEnable()||wouRscData.getWatchBoxNumberEnable())
						&&drawLabel)&&!wouRscData.watchBoxFillEnable) {
					drawTimeLabelWatchNumber(wouData,target,color);

				}
			}

		}//if draw = true
	}

	/* 0 for drawoutline
	   1 for shaded shape
	   2 for union*/

	public void drawCountyOutline(WouRscDataObj wouData,IGraphicsTarget target,RGB color,int symbolWidth ,
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
		for(int i = 0; i < wouData.countyNames.size();i++) {
			countyName=wouData.countyNames.get(i);
			stateName= wouData.stateNames.get(i);
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


	public void drawTimeLabelWatchNumber(WouRscDataObj wouData,IGraphicsTarget target,RGB color){
		try{
			for(int i = 0; i < wouData.countyNumPoints;i++) {
				double[] labelLatLon = { wouData.countyLon[i], wouData.countyLat[i] }; 
				double[] labelPix = descriptor.worldToPixel( labelLatLon );

				if( labelPix != null ){
					String[] text = new String[3];
					text[0]=" "+wouData.watchNumber;
					text[1]=" "+wouData.countyNames.get(i);
					DataTime startTime = new DataTime( wouData.eventTime.getValidPeriod().getStart() );
					DataTime endTime = new DataTime( wouData.eventTime.getValidPeriod().getEnd() );
					text[2] = " "+startTime.toString().substring(11, 16)
					+ "-" + endTime.toString().substring(11, 16);
					


					if(!wouRscData.getWatchBoxTimeEnable() ){
						//text[2] = null;
						text[2] = "";
					}

					if(!wouRscData.getWatchBoxLabelEnable() ){
						//text[1]=null;
						text[1]="";
					}

					if(!wouRscData.getWatchBoxNumberEnable() ){
						//text[0] = null;
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
			System.out.println("wouResource.java at Line 427"+e);
		}
	}
}
