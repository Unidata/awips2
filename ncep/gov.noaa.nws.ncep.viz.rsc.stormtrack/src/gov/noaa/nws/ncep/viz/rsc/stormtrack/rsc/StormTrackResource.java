/**
 * StormTrackResource
 * 
 * Date created 12 Oct 2011
 * 
 * This code has been developed by the SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.viz.rsc.stormtrack.rsc;

import java.awt.geom.Rectangle2D;
import java.text.SimpleDateFormat;

import java.util.HashMap;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.EnumMap;
import java.util.List;
import java.util.Map;
import java.util.TimeZone;

import gov.noaa.nws.ncep.common.dataplugin.stormtrack.StormTrackRecord;
import gov.noaa.nws.ncep.viz.common.ui.NmapCommon;
import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsResource;
import com.raytheon.uf.common.time.DataTime;
import gov.noaa.nws.ncep.viz.resources.INatlCntrsResource;

import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.TextStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.geom.PixelCoordinate;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.edex.decodertools.core.LatLonPoint;

import org.eclipse.swt.graphics.RGB;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.CoordinateList;
/**
 * Displays the Ensemble Storm Track ( ENS_CYC - MISC resource)  
 * 
  * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 10-Aug-2010    284      Archana     Initial creation.
 * 30-Sep-2010    307      Greg Hull   created AtcfCycloneRscDataObject wrapper 
 * 19-Oct-2010    307      Archana     updated queryRecords() and createQueryScriptForPython(String, String)
 *                                                        for time-matching 
 * 11-Nov-2010    307      Greg Hull   Use data with best timeMatch. adjust startTime in query.  
 * 01-Mar-2011             Greg Hull   frameInterval -> frameSpan
 * 12-Oct-2011             sgilbert    Modified from ATCFResource
 * 26-Oct-2011             B. Hebbard  Initial commit as StormTrack
 * 05/23/2012     785      Q. Zhou     Added getName for legend.                                               
 * @author sgilbert
 *</pre>
 */
public class StormTrackResource extends AbstractNatlCntrsResource<StormTrackResourceData, IMapDescriptor>
implements INatlCntrsResource{

	private StormTrackResourceData stormTrackResourceData;
	private static SimpleDateFormat QUERY_DATE_FORMAT = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
	private SimpleDateFormat sdf = new SimpleDateFormat("ddHH");

	/*
	private final String[] FORECAST_MODEL_NAMES = new String[] {"OHPC", "CLIP","AVNO", "MRFO", "UKM","AVNI", "NAMI", "EMX2", "EGRI", 
			"NGPI","UKM2","HWFI","GHMI","GFNI","AEMI","TCON","GUNA",
			"TVCN","EMXI","FSSE","UKMI","BAMM","BAMD","BAMS", "EGR2"};
			*/
	
	private EnumMap<Model, ForecastModelAttributes> modelToDisplayAttributesMap = 
		new EnumMap<Model, ForecastModelAttributes>(Model.class);
	
	private Map<Model, Boolean> modelLegendEnabledMap = new HashMap<Model, Boolean>(0);
	private Map<Model, RGB> modelLegendColorMap           = new HashMap<Model, RGB>(0); 

	private IFont font = null;
	float  baseFontSize = 14;
	private Rectangle2D charSize;
	private double charHeight;
	private double charWidth;	
	
	private enum Model{
		 GFSO,
		 NAM,
		 UKX,
		 NGX,
		 EC00,
		 EP01,
		 EP02,
		 EP03,
		 EP04,
		 EP05,
		 EP06,
		 EP07,
		 EP08,
		 EP09,
		 EP10,
		 EP11,
		 EP12,
		 EP13,
		 EP14,
		 UNKNOWN;
		 
		 static Model getEnumForModelName(String modelName) {
			 if ((modelName = modelName.trim()).isEmpty()) {
				 return UNKNOWN;
			 }
			 try {
				 return valueOf(modelName);
			 }
			 catch (IllegalArgumentException e) { // string not among above
				 //TODO:?  Log unknown model error
				 return UNKNOWN;
			 }
		 }
	}

	private ForecastModelAttributes getForecastModelAttributes(String model){
		ForecastModelAttributes modelAttributes = null;
		
		String prefix = model.toLowerCase();
		return modelAttributes;
	}
	
	private void setModelToDisplayAttributesMap() {

		 modelToDisplayAttributesMap.put(Model.GFSO, new ForecastModelAttributes (
				 stormTrackResourceData.gfsoEnable,
				 stormTrackResourceData.gfsoColor,
				 stormTrackResourceData.gfsoLineWidth,
				 stormTrackResourceData.gfsoSymbolWidth,
				 stormTrackResourceData.gfsoSymbolSize
				 ));
		 modelToDisplayAttributesMap.put(Model.NAM, new ForecastModelAttributes (
				 stormTrackResourceData.namEnable,
				 stormTrackResourceData.namColor,
				 stormTrackResourceData.namLineWidth,
				 stormTrackResourceData.namSymbolWidth,
				 stormTrackResourceData.namSymbolSize
				 ));
		 modelToDisplayAttributesMap.put(Model.UKX, new ForecastModelAttributes (
				 stormTrackResourceData.ukxEnable,
				 stormTrackResourceData.ukxColor,
				 stormTrackResourceData.ukxLineWidth,
				 stormTrackResourceData.ukxSymbolWidth,
				 stormTrackResourceData.ukxSymbolSize
				 ));
		 modelToDisplayAttributesMap.put(Model.NGX, new ForecastModelAttributes (
				 stormTrackResourceData.ngxEnable,
				 stormTrackResourceData.ngxColor,
				 stormTrackResourceData.ngxLineWidth,
				 stormTrackResourceData.ngxSymbolWidth,
				 stormTrackResourceData.ngxSymbolSize
				 ));
		 modelToDisplayAttributesMap.put(Model.EC00, new ForecastModelAttributes (
				 stormTrackResourceData.ec00Enable,
				 stormTrackResourceData.ec00Color,
				 stormTrackResourceData.ec00LineWidth,
				 stormTrackResourceData.ec00SymbolWidth,
				 stormTrackResourceData.ec00SymbolSize
				 ));
		 modelToDisplayAttributesMap.put(Model.EP01, new ForecastModelAttributes (
				 stormTrackResourceData.ep01Enable,
				 stormTrackResourceData.ep01Color,
				 stormTrackResourceData.ep01LineWidth,
				 stormTrackResourceData.ep01SymbolWidth,
				 stormTrackResourceData.ep01SymbolSize
				 ));
		 modelToDisplayAttributesMap.put(Model.EP02, new ForecastModelAttributes (
				 stormTrackResourceData.ep02Enable,
				 stormTrackResourceData.ep02Color,
				 stormTrackResourceData.ep02LineWidth,
				 stormTrackResourceData.ep02SymbolWidth,
				 stormTrackResourceData.ep02SymbolSize
				 ));
		 modelToDisplayAttributesMap.put(Model.EP03, new ForecastModelAttributes (
				 stormTrackResourceData.ep03Enable,
				 stormTrackResourceData.ep03Color,
				 stormTrackResourceData.ep03LineWidth,
				 stormTrackResourceData.ep03SymbolWidth,
				 stormTrackResourceData.ep03SymbolSize
				 ));
		 modelToDisplayAttributesMap.put(Model.EP04, new ForecastModelAttributes (
				 stormTrackResourceData.ep04Enable,
				 stormTrackResourceData.ep04Color,
				 stormTrackResourceData.ep04LineWidth,
				 stormTrackResourceData.ep04SymbolWidth,
				 stormTrackResourceData.ep04SymbolSize
				 ));
		 modelToDisplayAttributesMap.put(Model.EP05, new ForecastModelAttributes (
				 stormTrackResourceData.ep05Enable,
				 stormTrackResourceData.ep05Color,
				 stormTrackResourceData.ep05LineWidth,
				 stormTrackResourceData.ep05SymbolWidth,
				 stormTrackResourceData.ep05SymbolSize
				 ));
		 modelToDisplayAttributesMap.put(Model.EP06, new ForecastModelAttributes (
				 stormTrackResourceData.ep06Enable,
				 stormTrackResourceData.ep06Color,
				 stormTrackResourceData.ep06LineWidth,
				 stormTrackResourceData.ep06SymbolWidth,
				 stormTrackResourceData.ep06SymbolSize
				 ));
		 modelToDisplayAttributesMap.put(Model.EP07, new ForecastModelAttributes (
				 stormTrackResourceData.ep07Enable,
				 stormTrackResourceData.ep07Color,
				 stormTrackResourceData.ep07LineWidth,
				 stormTrackResourceData.ep07SymbolWidth,
				 stormTrackResourceData.ep07SymbolSize
				 ));
		 modelToDisplayAttributesMap.put(Model.EP08, new ForecastModelAttributes (
				 stormTrackResourceData.ep08Enable,
				 stormTrackResourceData.ep08Color,
				 stormTrackResourceData.ep08LineWidth,
				 stormTrackResourceData.ep08SymbolWidth,
				 stormTrackResourceData.ep08SymbolSize
				 ));
		 modelToDisplayAttributesMap.put(Model.EP09, new ForecastModelAttributes (
				 stormTrackResourceData.ep09Enable,
				 stormTrackResourceData.ep09Color,
				 stormTrackResourceData.ep09LineWidth,
				 stormTrackResourceData.ep09SymbolWidth,
				 stormTrackResourceData.ep09SymbolSize
				 ));
		 modelToDisplayAttributesMap.put(Model.EP10, new ForecastModelAttributes (
				 stormTrackResourceData.ep10Enable,
				 stormTrackResourceData.ep10Color,
				 stormTrackResourceData.ep10LineWidth,
				 stormTrackResourceData.ep10SymbolWidth,
				 stormTrackResourceData.ep10SymbolSize
				 ));
		 modelToDisplayAttributesMap.put(Model.EP11, new ForecastModelAttributes (
				 stormTrackResourceData.ep11Enable,
				 stormTrackResourceData.ep11Color,
				 stormTrackResourceData.ep11LineWidth,
				 stormTrackResourceData.ep11SymbolWidth,
				 stormTrackResourceData.ep11SymbolSize
				 ));
		 modelToDisplayAttributesMap.put(Model.EP12, new ForecastModelAttributes (
				 stormTrackResourceData.ep12Enable,
				 stormTrackResourceData.ep12Color,
				 stormTrackResourceData.ep12LineWidth,
				 stormTrackResourceData.ep12SymbolWidth,
				 stormTrackResourceData.ep12SymbolSize
				 ));
		 modelToDisplayAttributesMap.put(Model.EP13, new ForecastModelAttributes (
				 stormTrackResourceData.ep13Enable,
				 stormTrackResourceData.ep13Color,
				 stormTrackResourceData.ep13LineWidth,
				 stormTrackResourceData.ep13SymbolWidth,
				 stormTrackResourceData.ep13SymbolSize
				 ));
		 modelToDisplayAttributesMap.put(Model.EP14, new ForecastModelAttributes (
				 stormTrackResourceData.ep14Enable,
				 stormTrackResourceData.ep14Color,
				 stormTrackResourceData.ep14LineWidth,
				 stormTrackResourceData.ep14SymbolWidth,
				 stormTrackResourceData.ep14SymbolSize
				 ));
		 modelToDisplayAttributesMap.put(Model.UNKNOWN, new ForecastModelAttributes (
				 false,
				 new RGB(0, 0, 0),
				 0,
				 0,
				 0.0f
				 ));
		 
	}
	
/*	
	// Wrap the AtcfCyclone object returned from the query and implement IRscDataObject
	private static class AtcfCycloneRscDataObject implements IRscDataObject {
		private AtcfCyclone atcfCyclone;
		
		public AtcfCycloneRscDataObject( AtcfCyclone cycl ) {
			atcfCyclone = cycl;
		}
		public AtcfCyclone getAtcfCyclone() {
			return atcfCyclone;
		}
		@Override
		public DataTime getDataTime() {
			List<AtcfTrack> atcfTrackList = atcfCyclone.getTrackList();
			if( atcfTrackList.isEmpty() ) {
				return null;
			}
			else {
				return new DataTime( atcfTrackList.get(0).getWarningTime() );
			}
		}
	}
*/

	/* (non-Javadoc)
	 * @see com.raytheon.uf.viz.core.rsc.AbstractVizResource#project(org.opengis.referencing.crs.CoordinateReferenceSystem)
	 */
	@Override
	public void project(CoordinateReferenceSystem crs) throws VizException {
		//recreateDisplay = true;
		//for( AbstractFrameData frameData : frameDataMap.values() ) {
		//	frameData.recreate;
		//}
	}

	/* (non-Javadoc)
	 * @see gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsResource#resourceAttrsModified()
	 */
	@Override
	public void resourceAttrsModified() {
		//recreateDisplay = true;
		setModelToDisplayAttributesMap();  //  regenerate map, since attributes (may have) changed
	}

	protected StormTrackResource(StormTrackResourceData resourceData, LoadProperties props) {
		super(resourceData, props);
		stormTrackResourceData = resourceData;
		QUERY_DATE_FORMAT.setTimeZone(TimeZone.getTimeZone("GMT"));
		sdf.setTimeZone(TimeZone.getTimeZone("GMT"));
//		for(String thisLegend: this.FORECAST_MODEL_NAMES){
//			modelLegendEnabledMap.put( this.getEnumForModelName(thisLegend), new Boolean(false));
//		}

	}

	@Override
	protected AbstractFrameData createNewFrame( DataTime frameTime, int timeInt ) {
		/*The Python script will accept the time string only if it is formatted as given below....*/
//		String frameTimeStr = new String (QUERY_DATE_FORMAT.format(frameTime.getValidTime().getTime()));
		return (AbstractFrameData) new FrameData( frameTime, timeInt );
	}

	@Override
	public void initResource(IGraphicsTarget graphicsTarget) throws VizException {
		
		if( font == null ) {
			font         = graphicsTarget.initializeFont("Monospace", 11, new IFont.Style[] { IFont.Style.BOLD});
			charSize           = graphicsTarget.getStringBounds(font, "N");
			charHeight         = charSize.getHeight();
			charWidth          = charSize.getWidth();
		}

		long t0 = System.currentTimeMillis();
		queryRecords();
		long t1 = System.currentTimeMillis();
		//System.out.println("Initializing StormTrack...  took:" + (t1-t0) );
		
		for ( DataTime time : getFrameTimes() ) {
			//System.out.println(time);
		}
		
		setModelToDisplayAttributesMap();  //  set up map based on initial user selections
	}

	private String createQueryScriptForPython(String frameStartTimeStr, String frameEndTimeStr ){
		StringBuilder query = new StringBuilder();
		query.append("import StormTrackTrackRequest\n");
		query.append("req = StormTrackTrackRequest.StormTrackTrackRequest()\n");
		/*A single quote succeeds the '(' character and precedes the ')' character
		 * so that the python script recognizes it as an input string*/
        if ( frameStartTimeStr != null && !frameStartTimeStr.isEmpty() ){
		   query.append("req.setStartTime('"+frameStartTimeStr+"')\n" );
        }else{
        	  query.append("req.setStartTime()\n");
        }
        if ( frameEndTimeStr != null &&  ! frameEndTimeStr.isEmpty() ){
             query.append("req.setEndTime('"+frameEndTimeStr+"')\n" );
        }else{
        	  query.append("req.setEndTime()\n");
        }
        query.append("return req.execute()\n");
		return query.toString();
	}

/*	
	@Override
	public void queryRecords(){

		try {
			ArrayList<DataTime> frameTimes = getFrameTimes();
            Collections.sort(frameTimes);
			Calendar frameStartTime = Calendar.getInstance();
			Calendar frameEndTime = Calendar.getInstance();
			if( frameTimes != null && frameTimes.size() > 0){
				int frameTimeListSize = frameTimes.size();

				// adjust the start and end times to include the frameIntervals. If this isn't dominant,
				// we need to query everything that may match the first/last frames.
				long frameIntrvlMs = (long)resourceData.getFrameSpan()*60*1000;
				
				frameStartTime.setTime(  new Date( 
					frameTimes.get(0).getValidTime().getTime().getTime()-frameIntrvlMs) );
				frameEndTime.setTime( new Date(
					frameTimes.get(frameTimeListSize-1).getValidTime().getTime().getTime() + frameIntrvlMs) );
				
				// The Python script will accept the time string only if it is formatted as given below....
				String frameStartTimeStr = new String (QUERY_DATE_FORMAT.format(frameStartTime.getTime())+".0");
				String frameEndTimeStr = new String (QUERY_DATE_FORMAT.format(frameEndTime.getTime())+".0");
				Object[] pdoList = Connector.getInstance().connect( 
						createQueryScriptForPython(frameStartTimeStr, frameEndTimeStr) , null, 60000);

				for (Object pdo : pdoList) {
					for( IRscDataObject rscDataObj : processRecord( pdo ) )	{	
						System.out.println(rscDataObj);
						this.newRscDataObjsQueue.add(rscDataObj);
					}
				}				
				frameStartTime.clear();
			}

		} catch (VizException e) {
			e.printStackTrace();
		}
	}
*/

	/*
	// wrap the StormTrackCyclone with a RscDataObject so the Abstract class can time match it.
	//
	@Override
	public IRscDataObject[] processRecord( Object dataRec ) {
		if( !(dataRec instanceof StormTrackCyclone ) ) {
			return new IRscDataObject[]{};
		}
		StormTrackCycloneRscDataObject rscDataObj = 
			new StormTrackCycloneRscDataObject( (StormTrackCyclone)dataRec );
		return new StormTrackCycloneRscDataObject[]{ rscDataObj };
	}
	*/
	
	/***
	 * Converts the input Latitude and Longitude values into a PixelCoordinate object.
	 * @param aLat - input Latitude
	 * @param aLon - input Longitude 
	 * @return the PixelCoordiante equivalent of the input Lat/Lon values
	 */

	public PixelCoordinate convertFloatToPixelCoordinate(float aLat, float aLon ){
		LatLonPoint llp = new LatLonPoint(aLat, aLon, LatLonPoint.INDEGREES);
		Coordinate worldCoord = new Coordinate(llp.getLongitude(LatLonPoint.INDEGREES), llp.getLatitude(LatLonPoint.INDEGREES));
		double[] thisWorldPixDblArray = {worldCoord.x, worldCoord.y};
		double[] pixelArr = descriptor.worldToPixel(thisWorldPixDblArray);

		return new PixelCoordinate(pixelArr);
	}

	/***
	 * Converts each Lat/Lon point in the array to an equivalent PixelCoordinate object. 
	 * @param latPointArray - the input array of latitude points
	 * @param lonPointArray - the input array of longitude points
	 * @return the array of PixelCoordinate objects
	 */
	public PixelCoordinate[] convertTrackLocationPointsToPixelCoordinateArray(float latPointArray[], float lonPointArray[]  ){
		PixelCoordinate pixCoordArray[] = null;

		if (latPointArray != null && lonPointArray != null  
				&& latPointArray.length > 0 
				&& lonPointArray.length > 0
				&& latPointArray.length == lonPointArray.length  ) {
			pixCoordArray = new PixelCoordinate[ latPointArray.length];
			for (int index = 0; index < latPointArray.length; index++) {
				pixCoordArray[index] = convertFloatToPixelCoordinate(latPointArray[index], lonPointArray[index]);
			}
		}
		return pixCoordArray;
	}	
	/***
	 * Repaints the StormTrack tracks 
	 * @param frameData - the frame to be rendered
	 * @param graphicsTarget - the graphics target
	 * @param paintProps - the pain properties
	 */
	@Override
	public  void paintFrame(AbstractFrameData frameData,
			                IGraphicsTarget graphicsTarget, PaintProperties paintProps)
	throws VizException {
		/*  
		 *Calculate vertical/horizontal offset parameters 
		 */

		double screenToWorldRatio = paintProps.getCanvasBounds().width /paintProps.getView().getExtent().getWidth(); 
		double offsetY            = charHeight / screenToWorldRatio;
		double offsetX            = charWidth / screenToWorldRatio; 

		IFont legendFont         = graphicsTarget.initializeFont("Monospace", 16, new IFont.Style[] { IFont.Style.BOLD});

		if(frameData != null){
			double[] posA = null;
			FrameData currFrameData = (FrameData) frameData;
			//System.out.println(currFrameData.getFrameTime());
			//System.out.println("StormTrackResource.paintFrame() invoked....." + currFrameData.isPopulated());
			List<String> labelList = new ArrayList<String>();
			List<RGB> colorList = new ArrayList<RGB>();
			labelList.add("Model:");
			colorList.add(new RGB(255, 255, 255));

			for ( String model : currFrameData.tracks.getModels() ) {
				Model eModel = Model.getEnumForModelName(model);
				ForecastModelAttributes fma = modelToDisplayAttributesMap.get(eModel);
				if ( fma.getModelEnable() ) {

					IWireframeShape wire = graphicsTarget.createWireframeShape(false, descriptor);
					Collection<StormTrack> coll = currFrameData.tracks.getStormTracksByModel(model);
					//System.out.println(model+" : "+coll.size()+ " tracks.");
					if (coll.size() > 0) {
						labelList.add(model);
						colorList.add(fma.getColorOfModel());
					}
					for (StormTrack st: coll) {
						//System.out.println("      Track "+st.getStormId().getCycloneNum() + " : " +st.getTrack().size()+ " points");
						boolean drawLines = st.getTrack().size() > 1;
						CoordinateList clist = new CoordinateList();
						for ( StormLocation sloc : st.getTrack() ) {
							posA = descriptor.worldToPixel(new double[] {sloc.getLongitude(), sloc.getLatitude(), 0.0});
							PixelCoordinate pos = new PixelCoordinate(posA);
							clist.add( new Coordinate(sloc.getLongitude(), sloc.getLatitude()), false);
							wire.addLineSegment( clist.toCoordinateArray() );
							
							//  draw marker
							float markerSizeFactor = fma.getSymbolSize() * 0.70f;
							graphicsTarget.drawLine(
									pos.getX() - offsetY/2.0 * markerSizeFactor,
									pos.getY(),
									pos.getZ(),
									pos.getX() + offsetY/2.0 * markerSizeFactor,
									pos.getY(),
									pos.getZ(),
									fma.getColorOfModel(),
									fma.getSymbolWidth() /* ,
									lineStyle */);
							graphicsTarget.drawLine(
									pos.getX() ,
									pos.getY() - offsetY/2.0 * markerSizeFactor,
									pos.getZ(),
									pos.getX(),
									pos.getY() + offsetY/2.0 * markerSizeFactor,
									pos.getZ(),
									fma.getColorOfModel(),
									fma.getSymbolWidth() /* ,
									lineStyle */);
							
							//  draw pressure labels
							String presString = Integer.toString((int) sloc.getMslp());
							graphicsTarget.drawString(font, presString,
									pos.getX(),
									pos.getY()/* - offsetY * (labelList.size()+0.5)*/,
									0.0,
									TextStyle.NORMAL,
									fma.getColorOfModel(),
									HorizontalAlignment.LEFT,
									VerticalAlignment.TOP,
									0.0);

							//  draw final time labels
							if (sloc == st.getTrack().last()) {
								DataTime endTime = new DataTime(currFrameData.getFrameTime().getRefTime(), sloc.getForecastHour()*3600);
								Calendar endTimeC = endTime.getValidTime();
								String endDateS = Integer.toString(endTimeC.get(Calendar.DAY_OF_MONTH));
								if (endDateS.length() == 1) {
									endDateS = "0" + endDateS;
								}
								String endHourS = Integer.toString(endTimeC.get(Calendar.HOUR_OF_DAY));
								if (endHourS.length() == 1) {
									endHourS = "0" + endHourS;
								}
								String endTimeS = endDateS + "/" + endHourS;
								graphicsTarget.drawString(font, endTimeS,
										pos.getX(),
										pos.getY()/* - offsetY * (labelList.size()+0.5)*/,
										0.0,
										TextStyle.NORMAL,
										fma.getColorOfModel(),
										HorizontalAlignment.RIGHT,
										VerticalAlignment.BOTTOM,
										0.0);
							}

							//String text[] = new String[] {st.toString()};
							//graphicsTarget.drawStrings(font, text, pos[0], pos[1], 0.0, TextStyle.NORMAL, new RGB[]{new RGB(32,111,56)}, HorizontalAlignment.LEFT, VerticalAlignment.MIDDLE);
						}
					}
					wire.compile();
					graphicsTarget.drawWireframeShape(wire, fma.getColorOfModel(), fma.getLineWidth());
					wire.dispose();
				}
			}
			if (!labelList.isEmpty()) {
		        IExtent screenExtent = paintProps.getView().getExtent();
		        IExtent mapExtent = new PixelExtent(descriptor.getGridGeometry().getGridRange());
				double x0 = Math.max(mapExtent.getMinX(), screenExtent.getMinX()
                        + (screenExtent.getWidth() * 0.08));
				double y0 = Math.min(mapExtent.getMaxY(), screenExtent.getMaxY()
                        - (screenExtent.getHeight() * 0.10));
				Collections.reverse(labelList);
				Collections.reverse(colorList);
				graphicsTarget.drawStrings(legendFont, labelList.toArray(new  String[0]),
						x0,
						y0 /* - offsetY * (labelList.size()+0.5) */,
						0.0,
						TextStyle.NORMAL,
						colorList.toArray(new RGB[0]),
						HorizontalAlignment.RIGHT,
						VerticalAlignment.BOTTOM);
			}

		}
		
		
	}

	private Date getFutureValidTime(DataTime initial, int fcstHour) {
		return null;
		
	}
	
	@Override
	protected void disposeInternal() {
		//System.out.println("Disposing StormTrack...");
		super.disposeInternal();
		if ( font != null ) font.dispose();
	}

	private class FrameData extends AbstractFrameData{

		// map from the name of the cyclone to the StormTrackCyclone object
		private StormTrackContainer tracks;
		private boolean recreateDisplay = true;

		public FrameData(DataTime frameTime, int timeInt) {
			super(frameTime, timeInt);
			tracks = new StormTrackContainer();
		}

		@Override
		public boolean updateFrameData(IRscDataObject rscDataObj) {
			if ( rscDataObj instanceof DfltRecordRscDataObj ) {
				DfltRecordRscDataObj dfltObj = (DfltRecordRscDataObj)rscDataObj;
				
				//System.out.println("updateFrameData with: "+dfltObj.getPDO().getClass().getCanonicalName());
				//System.out.println("updateFrameData with: "+dfltObj.getPDO());
				if ( dfltObj.getPDO() instanceof StormTrackRecord ) {
					StormTrackRecord record = (StormTrackRecord)dfltObj.getPDO();
					tracks.addStormRecord(record);
				}
			}
			
			return true;
		}

		@Override
		public void dispose() {
			//System.out.println("I WONDER.....");
			tracks.clear();
		}

	}

	/***
	 * Private class to capture the attributes (such as the color, line width, symbol width etc) of each model 
	 * from the StormTrackResourceData class.
	 *
	 * @param <RGB> - the color of the model
	 * @param <Integer> - the line width of the track
	 * @param <Integer> - the line width of the tsymbol 
	 * @param <Float> - the size of the symbol
	 * @param <Boolean> - flag to check if the model is enabled
	 */
	private class ForecastModelAttributes {
		Boolean modelEnable;
		Integer    symbolWidth;
		Float symbolSize;
		Integer   lineWidth;
		RGB colorOfModel;

		public ForecastModelAttributes(Boolean mdlEnable, RGB modelColor, Integer lineWidth, Integer symbolWidth, 
				Float symbolSize){
			this.colorOfModel = modelColor;
			this.symbolSize = symbolSize;
			this.symbolWidth = symbolWidth;
			this.lineWidth = lineWidth;
			this.modelEnable = mdlEnable;
		}

		/**
		 * @return the modelEnable
		 */
		 private Boolean getModelEnable() {
			 return modelEnable;
		 }

		 /**
		  * @param modelEnable the modelEnable to set
		  */
		 private void setModelEnable(Boolean modelEnable) {
			 this.modelEnable = modelEnable;
		 }

		 /**
		  * @return the symbolWidth
		  */
		 private Integer getSymbolWidth() {
			 return symbolWidth;
		 }

		 /**
		  * @param symbolWidth the symbolWidth to set
		  */
		 private void setSymbolWidth(Integer symbolWidth) {
			 this.symbolWidth = symbolWidth;
		 }

		 /**
		  * @return the symbolSize
		  */
		 private Float getSymbolSize() {
			 return symbolSize;
		 }

		 /**
		  * @param symbolSize the symbolSize to set
		  */
		 private void setSymbolSize(Float symbolSize) {
			 this.symbolSize = symbolSize;
		 }

		 /**
		  * @return the lineWidth
		  */
		 private Integer getLineWidth() {
			 return lineWidth;
		 }

		 /**
		  * @param lineWidth the lineWidth to set
		  */
		 private void setLineWidth(Integer lineWidth) {
			 this.lineWidth = lineWidth;
		 }

		 /**
		  * @return the colorOfModel
		  */
		 private RGB getColorOfModel() {
			 return colorOfModel;
		 }

		 /**
		  * @param colorOfModel the colorOfModel to set
		  */
		 private void setColorOfModel(RGB colorOfModel) {
			 this.colorOfModel = colorOfModel;
		 }
	}
	
	@Override
	public String getName() {
		String legendString = super.getName();
		FrameData fd = (FrameData) getCurrentFrame();
		if (fd == null || fd.getFrameTime() == null || fd.tracks.getModels().size() == 0) {
			return legendString + "-No Data";
		}
		return legendString + " "+ NmapCommon.getTimeStringFromDataTime( fd.getFrameTime(), "/");
	}
}
