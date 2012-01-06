package gov.noaa.nws.ncep.viz.rsc.warn.rsc;

import gov.noaa.nws.ncep.common.dataplugin.aww.AwwLatlons;
import gov.noaa.nws.ncep.common.dataplugin.aww.AwwRecord;
import gov.noaa.nws.ncep.common.dataplugin.aww.AwwUgc;
import gov.noaa.nws.ncep.common.dataplugin.aww.AwwVtec;
import gov.noaa.nws.ncep.edex.common.stationTables.IStationField;
import gov.noaa.nws.ncep.edex.common.stationTables.Station;
import gov.noaa.nws.ncep.edex.common.stationTables.StationTable;
import gov.noaa.nws.ncep.ui.pgen.display.DisplayElementFactory;
import gov.noaa.nws.ncep.ui.pgen.display.IDisplayable;
import gov.noaa.nws.ncep.ui.pgen.elements.Symbol;
import gov.noaa.nws.ncep.viz.localization.impl.LocalizationManager;
import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsResource;
import gov.noaa.nws.ncep.viz.resources.INatlCntrsResource;
import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsResource.AbstractFrameData;
import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsResource.DfltRecordRscDataObj;
import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsResource.IRscDataObject;

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

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.edex.decodertools.core.LatLonPoint;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.TextStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery.QueryLanguage;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.IShadedShape;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.viz.core.rsc.jts.JTSCompiler;
import com.raytheon.viz.core.rsc.jts.JTSCompiler.PointStyle;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.Point;
import com.vividsolutions.jts.io.ParseException;
import com.vividsolutions.jts.io.WKBReader;

/**
 * Warn resourceResource - Display WARN from aww data.
 * 
 *  This code has been developed by the SIB for use in the AWIPS2 system.
 *
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 03/17/10                Uma Josyula  Initial creation.
 * 10/01/10       #307     Greg Hull    implement processRecords and change to 
 *                                      process WarnData as the IRscDataObj
 * 
 * </pre>
 * 
 * @author ujosyula 
 * @version 1.0
 */

public class WarnResource extends AbstractNatlCntrsResource< WarnResourceData, IMapDescriptor> 
implements     INatlCntrsResource, IStationField { 

	private IFont font;
	private StationTable stationTable; 
	private WarnResourceData warnRscData;

	private class WarnRscDataObj implements IRscDataObject {
		String 				datauri;       //used as a key string
		DataTime        	issueTime;     //  issue time from bulletin
		DataTime        	eventTime;     //  Event start time of Vtec
		String 				reportType;   
		int             	polyNumPoints,countyNumPoints;
		float[]      		polyLat,countyLat;
		float[]      		polyLon,countyLon;
		LatLonPoint[]   	polygonPoints;        
		List<LatLonPoint>	countyPoints;
		List<String>		countyUgc,countyNames,stateNames;     //To get all the counties for the warning
		
		// the dataTime will have the startTime as the refTime and a valid period of start/end 
		@Override
		public DataTime getDataTime() {
			return eventTime;
		}
	}
	
	
	protected class FrameData extends AbstractFrameData {
		HashMap<String, WarnRscDataObj> warnDataMap;  

		public FrameData(DataTime frameTime, int timeInt) {
			super( frameTime, timeInt );
			warnDataMap = new HashMap<String,WarnRscDataObj>();
		}
		
		@Override
		public boolean updateFrameData( IRscDataObject rscDataObj ) {
			if( !(rscDataObj instanceof WarnRscDataObj) ) {
				System.out.println("WarnResource.updateFrameData: expecting objects "+
						" of type WarnRscDataObj???");
				return false;
			}
			WarnRscDataObj warnRscDataObj = (WarnRscDataObj) rscDataObj;
			WarnRscDataObj existingWarnData = warnDataMap.get(warnRscDataObj.datauri);
			if( existingWarnData == null || 
					warnRscDataObj.issueTime.greaterThan( existingWarnData.issueTime ) ) {//I doubt this condition
				warnDataMap.put(warnRscDataObj.datauri, warnRscDataObj);
			}
			return true;
		}
	}

	/**
	 * Create a WARN resource.
	 * 
	 * @throws VizException
	 */
	public WarnResource( WarnResourceData rscData, 
			LoadProperties loadProperties ) throws VizException {
		super(rscData, loadProperties);
		warnRscData = (WarnResourceData) resourceData;	
	}


	protected AbstractFrameData createNewFrame( DataTime frameTime, int timeInt ) {
		return (AbstractFrameData) new FrameData( frameTime, timeInt );
	}

	// turn the db record into an WarnRscDataObj which will be timeMatched and 
    // added to one or more of the FrameData's.
    //
    @Override
	public IRscDataObject[] processRecord( Object pdo ) {
		AwwRecord awwRecord = (AwwRecord) pdo;
		WarnRscDataObj warnData = getAwwtData( awwRecord );
		if( warnData == null ) {
			return new IRscDataObject[]{};
		}
		else {
			return new WarnRscDataObj[]{ warnData };
		}
	}

	private WarnRscDataObj getAwwtData( AwwRecord awwRecord) {
		WarnRscDataObj warnStatusData= new WarnRscDataObj();
		warnStatusData.issueTime =new DataTime(awwRecord.getIssueTime());
	
			try{
				warnStatusData.reportType=awwRecord.getReportType();
				warnStatusData.datauri=awwRecord.getDataURI();

				Set<AwwUgc> awwUgc = awwRecord.getAwwUGC();
				for (AwwUgc awwugcs : awwUgc) {
					String ugcline = awwugcs.getUgc();//get the ugc line to find the counties
					if(ugcline!=null && ugcline!=""){

						warnStatusData.countyUgc = new ArrayList<String>();
						int i=0;//just for printout
						String temp;
						String countyname= ugcline.substring(0,3);
						StringTokenizer strugcs = new StringTokenizer(ugcline);
						while (strugcs.hasMoreTokens()) {
							temp=strugcs.nextToken("-");
							if (temp!=null){

								if (temp.contains(countyname)){
									(warnStatusData.countyUgc).add(temp);
								}
								else{
									(warnStatusData.countyUgc).add(countyname.concat(temp));
								}
								i++;
							}
						}
						if(i>1){
							warnStatusData.countyUgc.remove(i-1);
							warnStatusData.countyUgc.remove(i-2);
						}

						warnStatusData = getCountyNameLatLon(warnStatusData);//get the lat lon too NEW METHOD
					}
					
					int vtechNumber = awwugcs.getAwwVtecLine().size();
					if(vtechNumber>0){ //		TO OBTAIN THE EVENT START AND END TIME
						for (AwwVtec awwVtech : awwugcs.getAwwVtecLine()) {//This will be looped only once since the 
							//relationship between tables is one to one

							if((awwVtech.getAction().equalsIgnoreCase("COR")||(awwVtech.getAction().equalsIgnoreCase("CAN")))){
								warnStatusData.eventTime=warnStatusData.issueTime;   
							}
							else{
								warnStatusData.eventTime=new DataTime( awwVtech.getEventStartTime(), 
										 					  new TimeRange( awwVtech.getEventStartTime(),
										 							  		awwVtech.getEventEndTime()));
							}
						}
					}
					warnStatusData.polyNumPoints=awwugcs.getAwwLatLon().size();
					if(warnStatusData.polyNumPoints>0){
						warnStatusData.polygonPoints=new LatLonPoint[warnStatusData.polyNumPoints];
						warnStatusData.polyLat = new float[warnStatusData.polyNumPoints];
						warnStatusData.polyLon = new float[warnStatusData.polyNumPoints];
						int index;// =warnStatusData.polyNumPoints;
						for (AwwLatlons awwLatLon : awwugcs.getAwwLatLon()) {
							LatLonPoint point = new LatLonPoint (awwLatLon.getLat(), 
									awwLatLon.getLon(),LatLonPoint.INDEGREES);
							index=awwLatLon.getIndex();
							warnStatusData.polyLat[index-1]=awwLatLon.getLat();
							warnStatusData.polyLon[index-1]=awwLatLon.getLon();

							warnStatusData.polygonPoints[index-1] = point;
						}
					}
				}
			}
			catch(Exception e){
				System.out.println("at line 196"+e);
			}
	

		return warnStatusData;
	}

	private WarnRscDataObj getCountyNameLatLon(WarnRscDataObj wdata){
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
			System.out.println("warnResource.java at Line 222"+e);
		}
		wdata.countyNumPoints=wdata.countyNames.size();
		return wdata;

	}

	
	public void initResource(IGraphicsTarget grphTarget) throws VizException {
		font = grphTarget.initializeFont("Monospace", 14, new IFont.Style[] { IFont.Style.BOLD });
		stationTable = new StationTable( LocalizationManager.getInstance().getFilename("countyStnFile") );
		String str[]={"SEVERE_THUNDERSTORM_WARNING", "TORNADO_WARNING", "FLASH_FLOOD_WARNING"};
		RequestConstraint ids = new RequestConstraint();
        ids.setConstraintType(ConstraintType.IN);
        ids.setConstraintValueList(str);
		HashMap<String, RequestConstraint> metadataMap =new HashMap<String, RequestConstraint>(resourceData.getMetadataMap());
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
		LineStyle lineStyle = LineStyle.SOLID;
		int symbolWidth = 2;
		int symbolSize  = 2;

		Collection<WarnRscDataObj> warnDataValues = currFrameData.warnDataMap.values();

		for( WarnRscDataObj warnData : warnDataValues ) {
			Boolean draw = false;

			if((warnData.reportType).equalsIgnoreCase("SEVERE_THUNDERSTORM_WARNING")){
				color       = warnRscData.thunderstormColor;
				symbolWidth = warnRscData.thunderstormSymbolWidth;
				symbolSize  = warnRscData.thunderstormSymbolSize;
				if(warnRscData.thunderstormEnable){
					draw=true;
				}
			} 
			else if ((warnData.reportType).equalsIgnoreCase("TORNADO_WARNING")){
				color       = warnRscData.tornadoColor;
				symbolWidth = warnRscData.tornadoSymbolWidth;
				symbolSize  = warnRscData.tornadoSymbolSize;
				if(warnRscData.tornadoEnable){
					draw=true;
				}
			}
			else if ((warnData.reportType).equalsIgnoreCase("FLASH_FLOOD_WARNING")){
				color       = warnRscData.flashFloodColor;
				symbolWidth = warnRscData.flashFloodSymbolWidth;
				symbolSize  = warnRscData.flashFloodSymbolSize;
				if(warnRscData.flashFloodEnable){
					draw=true;
				}
			}
			
		
			//draw the polygon
			if(warnRscData.getStormBasedPolygonEnable() && draw){


				
				for(int i = 0; i < warnData.polyNumPoints;i++) {
					double[] latLon1 = { warnData.polyLon[i], warnData.polyLat[i] }; 
					double[] p1 = descriptor.worldToPixel( latLon1 );
					int idx2 = (i == warnData.polyNumPoints - 1) ? 0 : i+1;
					double[] latLon2 = { warnData.polyLon[idx2], warnData.polyLat[idx2] }; 
					double[] p2 = descriptor.worldToPixel( latLon2 );

					if( p1 != null && p2 != null ) {
						target.drawLine(p1[0], p1[1], 0.0, p2[0], p2[1], 0.0, 
								color,symbolWidth,lineStyle );
					}

				
				
					  
					
				}//for loop polyNumPoints
				drawLabel(warnData,target,color);


			}		    	//draw county outline
			else if(warnRscData.getCountyOutlineEnable() && draw){
				
				drawCountyOutline(warnData,target,color,symbolWidth,lineStyle,paintProps);
				
				 

			   
				 
				

				drawLabel(warnData,target,color);	
			}
			else if (draw){
	    		Color[] colors = new Color[] {new Color(color.red, color.green, color.blue)};

				//draw the line style to be dots and county points
	    		try{
				for(int i = 0; i < warnData.countyNumPoints;i++) {

					Coordinate coord = new Coordinate(
							warnData.countyLon[i], warnData.countyLat[i] );					
		        	Symbol pointSymbol = new Symbol(null,colors,symbolWidth, symbolSize*2.4 ,false,
		        					coord,"Symbol","DOT");
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
				drawLabel(warnData,target,color);

			}
		}

	}

  public void drawCountyOutline(WarnRscDataObj warnData,IGraphicsTarget target,
		     RGB color,int symbolWidth ,LineStyle lineStyle,PaintProperties paintProps) throws VizException{
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
	  for(int i = 0; i < warnData.countyNames.size();i++) {
		 countyName=warnData.countyNames.get(i);
		 stateName= warnData.stateNames.get(i);
		 if(countyName.contains("_")){
				countyName=countyName.replace("_"," ");
			}
		 StringBuilder query = new StringBuilder(
						"select AsBinary(the_geom_0_001) from mapdata.county where countyname ='");
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
			 byte[] wkb = (byte[]) result[k++];
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


	public void drawLabel(WarnRscDataObj warnData,IGraphicsTarget target,RGB color){
		try{
			
			for(int i = 0; i < warnData.countyNumPoints;i++) {
				double[] labelLatLon = { warnData.countyLon[i], warnData.countyLat[i] }; 
				double[] labelPix = descriptor.worldToPixel( labelLatLon );
			
				if( labelPix != null ){
					String[] text = new String[2];

					text[0]=" "+warnData.countyNames.get(i);
					DataTime startTime = new DataTime( warnData.eventTime.getValidPeriod().getStart() );
					DataTime endTime = new DataTime( warnData.eventTime.getValidPeriod().getEnd() );
					text[1] = " "+startTime.toString().substring(11, 16)
								 + "-" + endTime.toString().substring(11, 16);

					if(!warnRscData.getTimeEnable() ){
						//text[1] = null;
						text[1] = "";
					}

					if(!warnRscData.getCountyNameEnable() ){
						//text[0]=null;
						text[0]="";
					}

					target.drawStrings(font, text,   
							labelPix[0], labelPix[1], 0.0, TextStyle.NORMAL,
							new RGB[] {color, color},
							HorizontalAlignment.LEFT, 
							VerticalAlignment.MIDDLE );
				}
			}
		}
		catch(Exception e){
			System.out.println("warnResource.java at Line 427"+e);
		}
	}



}