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
import gov.noaa.nws.ncep.viz.common.ui.NmapCommon;
import gov.noaa.nws.ncep.viz.localization.NcPathManager;
import gov.noaa.nws.ncep.viz.localization.NcPathManager.NcPathConstants;
import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsResource;
import gov.noaa.nws.ncep.viz.resources.INatlCntrsResource;
import gov.noaa.nws.ncep.viz.ui.display.NCMapDescriptor;
import gov.noaa.nws.ncep.viz.ui.display.NcDisplayMngr;

import java.awt.Color;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.StringTokenizer;
import java.util.logging.*;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.edex.decodertools.core.LatLonPoint;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.TextStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment;
import com.raytheon.uf.viz.core.catalog.LayerProperty;
import com.raytheon.uf.viz.core.catalog.ScriptCreator;
import com.raytheon.uf.viz.core.comm.Connector;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.IShadedShape;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceType;
import com.raytheon.viz.core.rsc.jts.JTSCompiler;
import com.raytheon.viz.core.rsc.jts.JTSCompiler.PointStyle;
import com.raytheon.viz.ui.editor.AbstractEditor;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryCollection;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.MultiPolygon;
import com.vividsolutions.jts.io.WKBReader;

/**
 * Warn resourceResource - Display WARN from aww data.
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 03/17/10                Uma Josyula  Initial creation.
 * 10/01/10       #307     Greg Hull    implement processRecords and change to 
 *                                      process WarnData as the IRscDataObj
 * 07/28/11       #450     Greg Hull    NcPathManager    
 * 08/31/11		  #456	   Gang Zhang	AWW fix	   
 * 02/16/2012     #555     S. Gurung    Added call to setAllFramesAsPopulated() in queryRecords().                               
 * 05/23/2012     785      Q. Zhou      Added getName for legend.
 * 08/17/2012     655      B. Hebbard   Added paintProps as parameter to IDisplayable draw
 * 09/11/2012     854      Q. Zhou      Modified time string and alignment in drawLabel().
 * </pre>
 * 
 * @author ujosyula 
 * @version 1.0
 */

public class WarnResource extends AbstractNatlCntrsResource< WarnResourceData, NCMapDescriptor> 
												implements     INatlCntrsResource, IStationField { 

	private IFont font;
	private StationTable stationTable; 
	private WarnResourceData warnRscData;
	

	/**
	 * fields below are for T456 (Ticket 456)
	 */
	
	private boolean areaChangeFlag = false;
	
	private static final Logger logger = Logger.getLogger( WarnResource.class.getName() );
	
	private Map<String, java.util.TreeMap<java.util.Calendar, WarnRscDataObj>> wrdoExtActMap = 
						new HashMap<String,java.util.TreeMap<java.util.Calendar,WarnRscDataObj>>();

	private WarnCountyResult countyResult;
	
	private IWireframeShape outlineShape;
	
	private CountyResultJob crjob = new CountyResultJob("");	
	

	public class WarnRscDataObj implements IRscDataObject {
		String 				datauri;       //used as a key string
		DataTime        	issueTime;     //  issue time from bulletin
		DataTime        	eventTime, eStartTime, eEndTime;//T456:  Event start/end time of Vtec
		String 				reportType, actionType, officeId, eTrackingNo, phenomena, significance;//T456: last five   
		int             	polyNumPoints,countyNumPoints;
		float[]      		polyLat,countyLat;
		float[]      		polyLon,countyLon;
		LatLonPoint[]   	polygonPoints;        
		List<LatLonPoint>	countyPoints;
		List<String>		countyUgc,countyNames,stateNames, countyFips;//T456: fips     //To get all the counties for the warning
		
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
			return updateFrameData2(rscDataObj,warnDataMap);			
		}
	
	}

	/**
	 * Create a WARN resource.
	 * 
	 * @throws VizException
	 */
	public WarnResource( WarnResourceData rscData,LoadProperties loadProperties ) throws VizException { 
		super(rscData, loadProperties);	
		warnRscData = (WarnResourceData) resourceData;	
		addRDChangedListener();//T456: handling Area (IGLTarget) change
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
		List<WarnRscDataObj> warnData = getAwwtData2(awwRecord);//T456: WarnRscDataObj warnData = getAwwtData( awwRecord );
		if( warnData == null ) {
			return new IRscDataObject[]{};
		}
		else {
			return warnData.toArray(new IRscDataObject[]{});//T456: new WarnRscDataObj[]{ warnData };
		}
	}


	private WarnRscDataObj getCountyNameLatLon(WarnRscDataObj wdata){
		wdata.countyPoints =new ArrayList<LatLonPoint>();
		wdata.countyNames =new ArrayList<String>();	
		wdata.countyFips = new ArrayList<String>();//T456: 
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
					
					String s=station.getStnnum();
					wdata.countyFips.add(s.length()==4?"0"+s:s);//T456: 
					
					wdata.stateNames.add(station.getState());
					wdata.countyLat[i]=station.getLatitude();
					wdata.countyLon[i]=station.getLongitude();
					i++;

				}

			}
		}
		catch(Exception e){
			logger.log(Level.WARNING," Exception: "+e.getMessage());//T456:
		}
		wdata.countyNumPoints=wdata.countyNames.size();
		return wdata;

	}

	
	public void initResource(IGraphicsTarget grphTarget) throws VizException {
		font = grphTarget.initializeFont("Monospace", 14, new IFont.Style[] { IFont.Style.BOLD });
		stationTable = new StationTable( 
				NcPathManager.getInstance().getStaticFile( 
						NcPathConstants.COUNTY_STN_TBL ).getAbsolutePath() );
		String str[]={"SEVERE_THUNDERSTORM_WARNING", "TORNADO_WARNING", "FLASH_FLOOD_WARNING"};
		RequestConstraint ids = new RequestConstraint();
        ids.setConstraintType(ConstraintType.IN);
        ids.setConstraintValueList(str);
		HashMap<String, RequestConstraint> metadataMap =new HashMap<String, RequestConstraint>(resourceData.getMetadataMap());
		metadataMap.put("reportType",ids);
		resourceData.setMetadataMap(metadataMap);
		queryRecords();//T456:
	}

	@Override
	public void disposeInternal() {

	}
	
	public void paintFrame( AbstractFrameData frameData, 
			IGraphicsTarget target, PaintProperties paintProps) throws VizException {

		if( paintProps == null ) { return; }
			
		if( areaChangeFlag ){ areaChangeFlag = false; postProcessFrameUpdate(); }//T456: Area(IGLTarget) change, dispose old outlineShape? TODO
		
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

				drawCountyOutline3(warnData,target,color,symbolWidth,lineStyle,paintProps);	//T456:			
				 

			   
				 
				

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
		        	DisplayElementFactory df = new DisplayElementFactory( target, getNcMapDescriptor() );
	    			ArrayList<IDisplayable> displayEls = df.createDisplayElements( pointSymbol , paintProps );
	    			for (IDisplayable each : displayEls) {
	    				each.draw(target, paintProps);
	    				each.dispose();
	    			}
					
				}
	    		}
	    		catch(Exception e){
	    			logger.log(Level.INFO,"Exception: "+e.getMessage());
	    		}
				drawLabel(warnData,target,color);

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
					List<String> enabledText = new ArrayList<String>();
					
					if(warnRscData.getCountyNameEnable() ){
						enabledText.add(warnData.countyNames.get(i));
					}
					
					if(warnRscData.getTimeEnable() ){
						DataTime startTime = new DataTime( warnData.eventTime.getValidPeriod().getStart() );
						DataTime endTime = new DataTime( warnData.eventTime.getValidPeriod().getEnd() );
						String temp = startTime.toString().substring(11, 13) +startTime.toString().substring(14, 16)
									 + "-" + endTime.toString().substring(11, 13) +startTime.toString().substring(14, 16);
						enabledText.add(temp);
					}

					for (int j=enabledText.size(); j<2; j++)
						enabledText.add("");
					
					text = enabledText.toArray(text);
					
					target.drawStrings(font, text,   
							labelPix[0], labelPix[1], 0.0, TextStyle.NORMAL,
							new RGB[] {color, color},
							HorizontalAlignment.LEFT,//T456: .LEFT,2011-08-30 Dave 
							VerticalAlignment.TOP );
				}
			}
		}
		catch(Exception e){
			logger.log(Level.INFO,"Exception: "+e.getMessage());//T456: 
		}
	}

	
	//------------------------------------------------------------- T456: 	

	/**
	 * T456: 1). build query; 2). pre-loading counties.
	 */
	@Override
	public void queryRecords() throws VizException {
		// this method is almost similar to its super class's queryRecords(), may need to be modified later
		// to use the super class's version for the common part
		
		HashMap<String, RequestConstraint> queryList = new HashMap<String, RequestConstraint>(
				resourceData.getMetadataMap());

		LayerProperty prop = new LayerProperty();
		prop.setDesiredProduct(ResourceType.PLAN_VIEW);
		prop.setEntryQueryParameters(queryList, false);
		prop.setNumberOfImages(15000); // TODO: max # records ?? should we cap
										// this ?
		String script = null;
		script = ScriptCreator.createScript(prop);

		if (script == null)
			return;

		Object[] pdoList = Connector.getInstance().connect(script, null, 60000);

		countyResult = new WarnCountyResult(this,pdoList);
		
		for (Object pdo : pdoList) {
			for( IRscDataObject dataObject : processRecord( pdo ) )	{	
				newRscDataObjsQueue.add(dataObject);				

				countyResult.buildQueryPart2(dataObject);
			}
		} 
		countyResult.populateMap();
		setAllFramesAsPopulated();
	}

	/**
	 * 	outlineShape pre-calculated for county outline drawing performance tuning
	 */	
	public void drawCountyOutline3(WarnRscDataObj warnData,IGraphicsTarget target,
     		RGB color,int symbolWidth ,LineStyle lineStyle,PaintProperties paintProps) throws VizException{

		CountyResultJob.Result result = 
			crjob.uriResultMap.get(get4StringConcat(warnData.officeId,warnData.eTrackingNo,warnData.phenomena,warnData.significance)); 
		
		if (result != null) {
			if (outlineShape == null) {   
				outlineShape = result.outlineShape;   
			}else{									 
		 
				outlineShape = result.outlineShape;		
			}    
		}else {
			return;
		}
		
		if (outlineShape != null && outlineShape.isDrawable() ){
			target.drawWireframeShape(outlineShape,  color,symbolWidth,lineStyle );
		
		} else if (outlineShape == null){
		
			//target.setNeedsRefresh(true);
		}
}	
	
    public class CountyResultJob extends Job {
    	
    	public Map<Collection<WarnRscDataObj>, Result> collWrdoMap = 
    		new java.util.concurrent.ConcurrentHashMap<Collection<WarnRscDataObj>,Result>();
    	
    	private Map<String, Result> uriResultMap = new HashMap<String, Result>();		
    	private IGraphicsTarget target;
    	private IMapDescriptor descriptor;
    	private boolean labeled;
    	private boolean shaded;
    	private Map<Object, RGB> colorMap;
    	
		RGB color = new RGB (155, 155, 155);
		RGB symbolColor = new RGB (155, 155, 155);
		LineStyle lineStyle = LineStyle.SOLID;
		int symbolWidth = 2;
		int symbolSize  = 2;
    	
        public class Result {
        	        	
            public IWireframeShape outlineShape;            
            public Map<Object, RGB> colorMap;

            private Result(IWireframeShape outlineShape,IWireframeShape newUnionShape,
                     IShadedShape shadedShape,Map<Object, RGB> colorMap)    {
            	this.outlineShape = outlineShape;
                
                this.colorMap = colorMap;
            }
        }

		public CountyResultJob(String name) {	super(name);	}
		
		public void setRequest(IGraphicsTarget target, IMapDescriptor descriptor,
        		String query, boolean labeled, boolean shaded, Map<Object, RGB> colorMap){		
			
			this.target = target;
			this.descriptor = descriptor;	
			this.labeled = labeled;
			this.shaded = shaded;
			this.colorMap = colorMap;				
			this.run(null);//this.schedule();				
		}
				
		@Override
		protected IStatus run(IProgressMonitor monitor){		
			
			String countyName, stateName;
			ArrayList<ArrayList<Object[]>> resultsList;						
				
			for(AbstractFrameData afd : frameDataMap.values())	{
				
				FrameData fd = (FrameData)afd;								
			
				for(WarnRscDataObj wrdo : fd.warnDataMap.values()){	
					
					Collection<Geometry> gw = new ArrayList<Geometry>();					
										
					for(int i=0; i<wrdo.countyFips.size(); i++){
						
						//fips can have multiple rows in maps mapdata.county table,
						//so another loop needed.
						for(ArrayList<Object[]> results : countyResult.getStateCountyResult2(wrdo.countyFips.get(i))){								
				    	
					    	if(results==null) continue;					
						
					    	WKBReader wkbReader = new WKBReader();
							for (Object[] result : results) {
								int k = 0;
								
								byte[] wkb1=(byte[]) result[k];
								
								MultiPolygon countyGeo = null;
								try{										
									countyGeo= (MultiPolygon)wkbReader.read(wkb1);
									if ( countyGeo != null && countyGeo.isValid() && ( ! countyGeo.isEmpty())){																					
										gw.add(countyGeo);																			
									}
								}catch (Exception e) {	logger.log(Level.WARNING,"__Error: "+e.getMessage());	}						
							}					
						}					
					}
					if(gw.size() == 0) 
						continue;	
					else 
						uriResultMap.put(
								get4StringConcat(wrdo.officeId,wrdo.eTrackingNo,wrdo.phenomena,wrdo.significance), 
								new Result(getEachWrdoShape(gw),null,null,null));	
				}
				
			}						
			return Status.OK_STATUS;
		}
		
	    
	    public IWireframeShape getEachWrdoShape(Collection<Geometry> gw){
	    	
	    	IWireframeShape newOutlineShape = target.createWireframeShape(false, descriptor, 0.0f);
			
			JTSCompiler jtsCompiler = new JTSCompiler(null,newOutlineShape, descriptor, PointStyle.CROSS);
	    	
			GeometryCollection gColl=(GeometryCollection) new GeometryFactory().buildGeometry( gw );
			
			try{	
				gColl.normalize();
				
				jtsCompiler.handle(gColl, symbolColor);				
						
				newOutlineShape.compile();	
											
			}catch (Exception e) {	logger.log(Level.WARNING,"_____Error: "+e.getMessage());	}
	    	
	    	return newOutlineShape;
	    }
    }
    
    /**
     * start outlineShape pre-calculation 
     */
    @Override
	protected boolean postProcessFrameUpdate() {
    	
    	 crjob.setRequest(
     			NcDisplayMngr.getActiveNatlCntrsEditor().getActiveDisplayPane().getTarget(),
     			   getNcMapDescriptor(), null, false, false, null);
    	
		return true;
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
			logger.log(Level.INFO,"===== find IRscDataObject rscDataObj.getDataTime() return NULL!!!"); 
		}
		long dataTimeInMs = 0; 
		if(validTimeInCalendar != null)
			dataTimeInMs = validTimeInCalendar.getTimeInMillis(); 
		return dataTimeInMs; 
	}
    
    
    /**
     * called by FrameData.updateFrameData()
     * 1). if EXT action, replace warnDataMap with EXT map last value;
     * 2). if NEW or COR, directly put into the warnDataMap.
     */    
    public boolean updateFrameData2( IRscDataObject rscDataObj,Map<String, WarnRscDataObj> warnDataMap ) {
    	
		if( ! (rscDataObj instanceof WarnRscDataObj) ) 			
			return false;
 				
		WarnRscDataObj wrdo = (WarnRscDataObj)rscDataObj;		
	
		String key = get4StringConcat(wrdo.officeId,wrdo.eTrackingNo, wrdo.phenomena, wrdo.significance);
		
		if(wrdoExtActMap.containsKey(key)){//this is an EXT
						
			java.util.TreeMap<java.util.Calendar, WarnRscDataObj> map = wrdoExtActMap.get(key);
			
			if(map != null){
				WarnRscDataObj extWrdo = map.get(map.lastKey());//latest issue time prevail
				
				if(extWrdo != null)
					warnDataMap.put(key, extWrdo);//key can be re-used
			}
		}else{//this is a NEW or COR
			warnDataMap.put(key, wrdo);//NOT datauri
			
		}
		return true;
	
    }
    
    public String get4StringConcat(String office, String etn, String pheno, String signi){
    	StringBuilder sb = new StringBuilder(office);
    	
    	sb.append(etn);
    	sb.append(pheno);
    	sb.append(signi);
    	
    	return sb.toString();
    }



	/**
	 * 
	 * 1). add WarnRscDataObj to a List if NEW or COR action;
	 * 2). put into the EXT Map: wrdoExtActMap if EXT action.
	 *
	 */
    public List<WarnRscDataObj> addWarnRscDataObj(WarnRscDataObj wrdObj, AwwRecord awwRecord, AwwUgc awwugcs){
    	
    	List<WarnRscDataObj> wrdoList = new ArrayList<WarnRscDataObj>();
    	
		int vtechNumber = awwugcs.getAwwVtecLine().size();
		if(vtechNumber>0){ 
			for (AwwVtec awwVtech : awwugcs.getAwwVtecLine()) { 
				
				// a new WarnRscDataObj for each vtec line
				WarnRscDataObj wrdo = new WarnRscDataObj();
				
		    	java.util.Calendar esTime = awwVtech.getEventStartTime();
		    	java.util.Calendar eeTime = awwVtech.getEventEndTime();
		    	
		    	// Tiros Lee, 2011-08-23: right now if an eventStartTime is null, 
		    	// just log it since that means the original NEW event may NOT be
		    	// in the database, or purged.
		    	if( esTime == null ){
		    		logger.log(java.util.logging.Level.SEVERE, "___ setWarnRscDataObj(): AwwVtec eventStartTime is NULL !" );
		    		continue;
		    	}else
		    		wrdo.eStartTime = new DataTime(esTime);
		    	
		    	// same as event start time	
		    	if( eeTime == null){
		    		logger.log(java.util.logging.Level.SEVERE, "___ setWarnRscDataObj(): AwwVtec eventEndTime is NULL !" );    
		    		continue;
		    	}else
		    		wrdo.eEndTime = new DataTime(eeTime);
				
				wrdo.actionType = awwVtech.getAction();

				//copy county and polygon data to the new WarnRscDataObj
				wrdo.countyPoints = wrdObj.countyPoints;
				wrdo.countyNames = wrdObj.countyNames;	
				
				wrdo.countyFips = wrdObj.countyFips;//2011-08-31
				
				wrdo.stateNames = wrdObj.stateNames;
				wrdo.countyLat = wrdObj.countyLat;
				wrdo.countyLon= wrdObj.countyLon;
				wrdo.countyNumPoints = wrdObj.countyNumPoints;
				wrdo.countyUgc = wrdObj.countyUgc;				
				wrdo.polyNumPoints = wrdObj.polyNumPoints;				
				wrdo.polygonPoints = wrdObj.polygonPoints;
				wrdo.polyLat = wrdObj.polyLat;
				wrdo.polyLon = wrdObj.polyLon;
				wrdo.reportType = wrdObj.reportType;
				
		    	//wrdo.actionType = awwVtech.getAction();
		    	wrdo.eTrackingNo = awwVtech.getEventTrackingNumber();
		    	wrdo.officeId = awwVtech.getOfficeID();
		    	wrdo.phenomena = awwVtech.getPhenomena();
		    	wrdo.significance = awwVtech.getSignificance();
		    	
		    	//in Uma's code, this is in the if( COR || CAN ) block
		    	wrdo.eventTime = new DataTime( awwVtech.getEventStartTime(), 
		 					                   new TimeRange( awwVtech.getEventStartTime(),
		 							  		                  awwVtech.getEventEndTime()));		    	
		    	
		    	//put the object of action type EXT into the map 
		    	//the assumption: one AwwRecord (with an issue time) has only one EXT for a specific UGC, 
		    	if( "EXT".equalsIgnoreCase(wrdo.actionType)){		    		
		    		
		    		String key = get4StringConcat(wrdo.officeId, wrdo.eTrackingNo, wrdo.phenomena, wrdo.significance);		    		
		    		if(wrdoExtActMap.containsKey(key)){  
		    			
		    			// in a TreeMap, better use Calendar as the key, DataTime may change in the future.
		    			wrdoExtActMap.get(key).put(awwRecord.getIssueTime(), wrdo);
		    		
		    		}else{
		    			
		    			java.util.TreeMap<java.util.Calendar, WarnRscDataObj> tmap = new java.util.TreeMap<java.util.Calendar, WarnRscDataObj>();
		    			tmap.put(awwRecord.getIssueTime(), wrdo);
		    			
		    			wrdoExtActMap.put(key, tmap);
		    		}
		    	}
		    	
		    	wrdoList.add(wrdo);
		    	
			}//end: for-loop 
		}//end:     if(vtechNumber>0)   	

    	return wrdoList;
    }
    
    /**
     * handles action EXT in addWarnRscDataObj() method.
     */
	private List<WarnRscDataObj> getAwwtData2( AwwRecord awwRecord) {
		
		List<WarnRscDataObj> wrdoList = new ArrayList<WarnRscDataObj>();//2011-08-29
		WarnRscDataObj warnStatusData= new WarnRscDataObj();
		warnStatusData.issueTime =new DataTime(awwRecord.getIssueTime());
	
			try{
				warnStatusData.reportType=awwRecord.getReportType();
				warnStatusData.datauri=awwRecord.getDataURI();

				Set<AwwUgc> awwUgc = awwRecord.getAwwUGC();
				for (AwwUgc awwugcs : awwUgc) {
					String ugcline = awwugcs.getUgc();
					if(ugcline!=null && ugcline!=""){

						warnStatusData.countyUgc = new ArrayList<String>();
						int i=0;
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
									//handle multiple-line UGC
									if(temp.contains("\r\r\n") && temp.length()>3) 
										temp = temp.substring(3);
									
									(warnStatusData.countyUgc).add(countyname.concat(temp));
								}
								i++;
							}
						}
						if(i>1){
							warnStatusData.countyUgc.remove(i-1);
							warnStatusData.countyUgc.remove(i-2);
						}

						warnStatusData = getCountyNameLatLon(warnStatusData);
					}					

					warnStatusData.polyNumPoints=awwugcs.getAwwLatLon().size();
					if(warnStatusData.polyNumPoints>0){
						warnStatusData.polygonPoints=new LatLonPoint[warnStatusData.polyNumPoints];
						warnStatusData.polyLat = new float[warnStatusData.polyNumPoints];
						warnStatusData.polyLon = new float[warnStatusData.polyNumPoints];
						int index;
						for (AwwLatlons awwLatLon : awwugcs.getAwwLatLon()) {
							LatLonPoint point = new LatLonPoint (awwLatLon.getLat(), 
									awwLatLon.getLon(),LatLonPoint.INDEGREES);
							index=awwLatLon.getIndex();
							warnStatusData.polyLat[index-1]=awwLatLon.getLat();
							warnStatusData.polyLon[index-1]=awwLatLon.getLon();

							warnStatusData.polygonPoints[index-1] = point;
						}
					}
					
					//put the vtec-block into the below method after counties & polygons are already done.
					wrdoList = addWarnRscDataObj(warnStatusData, awwRecord, awwugcs);
				
				}
			} catch(Exception e){	logger.log(java.util.logging.Level.SEVERE, "___ getAwwtData2(): "+ e.getMessage() );		}	

		return wrdoList;
	}
	
	//------------------------------------------------------- Area change handling

	
	/**
	 *  called in the constructor.
	 */
	private void addRDChangedListener(){
		AbstractEditor editor = NcDisplayMngr.getActiveNatlCntrsEditor();
		editor.addRenderableDisplayChangedListener(this.new WarnDCListener());
	}
	
	/**
	 * change the flag so outlineShape can be re-calculated
	 */
	private class WarnDCListener implements com.raytheon.uf.viz.core.IRenderableDisplayChangedListener{

		@Override
		public void renderableDisplayChanged(IDisplayPane pane,
				IRenderableDisplay newRenderableDisplay, DisplayChangeType type) {
			
			areaChangeFlag = true;			
		}		
	}
	
	@Override
	public String getName() {
		String legendString = super.getName();
		FrameData fd = (FrameData) getCurrentFrame();
		if (fd == null || fd.getFrameTime() == null || fd.warnDataMap.size() == 0) {
			return legendString + "-No Data";
		}
		return legendString + " "+ NmapCommon.getTimeStringFromDataTime( fd.getFrameTime(), "/");
	}
}