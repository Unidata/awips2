/**
 * AtcfResource
 * 
 * Date created 10 Aug 2010
 * 
 * This code has been developed by the SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.viz.rsc.atcf.rsc;

import java.awt.geom.Rectangle2D;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;

import java.util.HashMap;

import java.util.List;
import java.util.Map;
import java.util.TimeZone;

import gov.noaa.nws.ncep.viz.common.ui.NmapCommon;
import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsResource;
import com.raytheon.uf.common.time.DataTime;
import gov.noaa.nws.ncep.viz.resources.INatlCntrsResource;
import gov.noaa.nws.ncep.viz.ui.display.NCMapDescriptor;
import gov.noaa.nws.ncep.edex.uengine.tasks.atcf.AtcfCyclone;
import gov.noaa.nws.ncep.edex.uengine.tasks.atcf.AtcfTrack;

import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.TextStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment;
import com.raytheon.uf.viz.core.comm.Connector;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.geom.PixelCoordinate;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.edex.decodertools.core.LatLonPoint;
import com.vividsolutions.jts.geom.Coordinate;

import org.eclipse.swt.graphics.RGB;

/**
 * Displays the Automated Tropical Cyclone Forecast ( ATCF - MISC resource)  
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
 * 16 Feb 2012    555      S. Gurung   Added call to setAllFramesAsPopulated() in queryRecords()
 * 05/23/12       785      Q. Zhou     Added getName for legend.                                               
 * @author archana
 *</pre>
 */
public class AtcfResource extends AbstractNatlCntrsResource<AtcfResourceData, NCMapDescriptor>
implements INatlCntrsResource{

	private AtcfResourceData atcfResourceDataObj;
	private static SimpleDateFormat QUERY_DATE_FORMAT = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
	private SimpleDateFormat sdf = new SimpleDateFormat("ddHH");

	private final String[] FORECAST_MODEL_NAMES = new String[] {"OHPC", "CLIP","AVNO", "MRFO", "UKM","AVNI", "NAMI", "EMX2", "EGRI", 
			"NGPI","UKM2","HWFI","GHMI","GFNI","AEMI","TCON","GUNA",
			"TVCN","EMXI","FSSE","UKMI","BAMM","BAMD","BAMS", "EGR2"};
	private Map<CycloneForecastTechniqueType, Boolean> modelLegendEnabledMap = new HashMap<CycloneForecastTechniqueType, Boolean>(0);
	private Map<CycloneForecastTechniqueType, RGB> modelLegendColorMap           = new HashMap<CycloneForecastTechniqueType, RGB>(0); 

	private IFont font = null;
	float  baseFontSize = 14;
	private Rectangle2D charSize;
	private double charHeight;
	private double charWidth;	

	private enum CycloneForecastTechniqueType{
		OHPC_01,
		CLIP_02,
		AVNO_03,
		MRFO_04,
		UKM_5,
		AVNI_06,
		NAMI_07,
		EMX2_08,
		EGRI_09,
		NGPI_10,
		UKM2_11,
		HWFI_12,
		GHMI_13,
		GFNI_14,
		AEMI_15,
		TCON_16,
		GUNA_17, 
		TVCN_18,
		EMXI_19, 
		FSSE_20,
		UKMI_21,
		BAMM_22,
		BAMD_23,
		BAMS_24, 
		EGR2_25,
		CARQ_26,
		UNKNOWN
	}

	private CycloneForecastTechniqueType getEnumForModelName(String techniqueName){
		CycloneForecastTechniqueType cftt = CycloneForecastTechniqueType.UNKNOWN;
		if(techniqueName.contains("OHPC")){
			cftt = CycloneForecastTechniqueType.OHPC_01;
		}
		else if(techniqueName.contains("CLIP")){
			cftt = CycloneForecastTechniqueType.CLIP_02;
		}  

		else if(techniqueName.contains("AVNO")){
			cftt = CycloneForecastTechniqueType.AVNO_03;
		}  

		else if(techniqueName.contains("MRFO")){
			cftt = CycloneForecastTechniqueType.MRFO_04;
		}  

		else if(techniqueName.contains("AVNI")){
			cftt = CycloneForecastTechniqueType.AVNI_06;
		}  
		else if(techniqueName.contains("NAMI")){
			cftt = CycloneForecastTechniqueType.NAMI_07;
		}  
		else if(techniqueName.contains("EMX2")){
			cftt = CycloneForecastTechniqueType.EMX2_08;
		}          	
		else if(techniqueName.contains("EGRI")){
			cftt = CycloneForecastTechniqueType.EGRI_09;
		}          	
		else if(techniqueName.contains("NGPI")){
			cftt = CycloneForecastTechniqueType.NGPI_10;
		}          	
		else if(techniqueName.contains("UKM2")){
			cftt = CycloneForecastTechniqueType.UKM2_11;
		}          	
		else if(techniqueName.contains("HWFI")){
			cftt = CycloneForecastTechniqueType.HWFI_12;
		}          	
		else if(techniqueName.contains("GHMI")){
			cftt = CycloneForecastTechniqueType.GHMI_13;
		}          	
		else if(techniqueName.contains("GFNI")){
			cftt = CycloneForecastTechniqueType.GFNI_14;
		}          	
		else if(techniqueName.contains("AEMI")){
			cftt = CycloneForecastTechniqueType.AEMI_15;
		}         	
		else if(techniqueName.contains("TCON")){
			cftt = CycloneForecastTechniqueType.TCON_16;
		}          	
		else if(techniqueName.contains("GUNA")){
			cftt = CycloneForecastTechniqueType.GUNA_17;
		}          	
		else if(techniqueName.contains("TVCN")){
			cftt = CycloneForecastTechniqueType.TVCN_18;
		}         	
		else if(techniqueName.contains("EMXI")){
			cftt = CycloneForecastTechniqueType.EMXI_19;
		}          	
		else if(techniqueName.contains("FSSE")){
			cftt = CycloneForecastTechniqueType.FSSE_20;
		}    
		else if(techniqueName.contains("UKMI")){
			cftt = CycloneForecastTechniqueType.UKMI_21;
		}         	
		else if(techniqueName.contains("BAMM")){
			cftt = CycloneForecastTechniqueType.BAMM_22;
		}          	
		else if(techniqueName.contains("BAMD")){
			cftt = CycloneForecastTechniqueType.BAMD_23;
		}            	
		else if(techniqueName.contains("BAMS")){
			cftt = CycloneForecastTechniqueType.BAMS_24;
		}          	
		else if(techniqueName.contains("EGR2")){
			cftt = CycloneForecastTechniqueType.EGR2_25;
		}

		else if(techniqueName.contains("CARQ")){
			cftt = CycloneForecastTechniqueType.CARQ_26;
		}        	

		else if(techniqueName.contains("UKM")){
			cftt = CycloneForecastTechniqueType.UKM_5;
		}            	

		else{
			cftt = CycloneForecastTechniqueType.UNKNOWN;
		}

		return cftt; 
	}    


	private ForecastModelAttributes<RGB, Integer, Float, Boolean> getForecastModelAttributesForEnum(CycloneForecastTechniqueType cftt){
		ForecastModelAttributes<RGB, Integer, Float, Boolean> forecastModelAttributes = null;
		switch(cftt) {
		case  OHPC_01:
			forecastModelAttributes = new ForecastModelAttributes<RGB, Integer, Float, Boolean>
			(atcfResourceDataObj.ohpcEnable,
					atcfResourceDataObj.ohpcColor,
					atcfResourceDataObj.ohpcLineWidth,
					atcfResourceDataObj.ohpcSymbolWidth,
					atcfResourceDataObj.ohpcSymbolSize);
			break;
		case    CLIP_02:
			forecastModelAttributes = new ForecastModelAttributes<RGB, Integer, Float, Boolean>
			(atcfResourceDataObj.clipEnable,
					atcfResourceDataObj.clipColor,
					atcfResourceDataObj.clipLineWidth,
					atcfResourceDataObj.clipSymbolWidth,
					atcfResourceDataObj.clipSymbolSize);
			break;
		case    AVNO_03:
			forecastModelAttributes = new ForecastModelAttributes<RGB, Integer, Float, Boolean>
			(atcfResourceDataObj.avnoEnable,
					atcfResourceDataObj.avnoColor,
					atcfResourceDataObj.avnoLineWidth,
					atcfResourceDataObj.avnoSymbolWidth,
					atcfResourceDataObj.avnoSymbolSize);
			break; 
		case   MRFO_04:
			forecastModelAttributes = new ForecastModelAttributes<RGB, Integer, Float, Boolean>
			(atcfResourceDataObj.ohpcEnable,
					atcfResourceDataObj.mrfoColor,
					atcfResourceDataObj.mrfoLineWidth,
					atcfResourceDataObj.mrfoSymbolWidth,
					atcfResourceDataObj.mrfoSymbolSize);
			break;  
		case  UKM_5:
			forecastModelAttributes = new ForecastModelAttributes<RGB, Integer, Float, Boolean>
			(atcfResourceDataObj.ukmEnable,
					atcfResourceDataObj.ukmColor,
					atcfResourceDataObj.ukmLineWidth,
					atcfResourceDataObj.ukmSymbolWidth,
					atcfResourceDataObj.ukmSymbolSize);
			break;  
		case  AVNI_06:
			forecastModelAttributes = new ForecastModelAttributes<RGB, Integer, Float, Boolean>
			(atcfResourceDataObj.avniEnable,
					atcfResourceDataObj.avniColor,
					atcfResourceDataObj.avniLineWidth,
					atcfResourceDataObj.avniSymbolWidth,
					atcfResourceDataObj.avniSymbolSize);
			break;
		case   NAMI_07:
			forecastModelAttributes = new ForecastModelAttributes<RGB, Integer, Float, Boolean>
			(atcfResourceDataObj.namiEnable,
					atcfResourceDataObj.namiColor,
					atcfResourceDataObj.namiLineWidth,
					atcfResourceDataObj.namiSymbolWidth,
					atcfResourceDataObj.namiSymbolSize);
			break;
		case  EMX2_08:
			forecastModelAttributes = new ForecastModelAttributes<RGB, Integer, Float, Boolean>
			(atcfResourceDataObj.emx2Enable,
					atcfResourceDataObj.emx2Color,
					atcfResourceDataObj.emx2LineWidth,
					atcfResourceDataObj.emx2SymbolWidth,
					atcfResourceDataObj.emx2SymbolSize);
			break;  
		case  EGRI_09:

			forecastModelAttributes = new ForecastModelAttributes<RGB, Integer, Float, Boolean>
			(atcfResourceDataObj.egriEnable,
					atcfResourceDataObj.egriColor,
					atcfResourceDataObj.egriLineWidth,
					atcfResourceDataObj.egriSymbolWidth,
					atcfResourceDataObj.egriSymbolSize);
			break;  
		case  NGPI_10:
			forecastModelAttributes = new ForecastModelAttributes<RGB, Integer, Float, Boolean>
			(atcfResourceDataObj.ngpiEnable,
					atcfResourceDataObj.ngpiColor,
					atcfResourceDataObj.ngpiLineWidth,
					atcfResourceDataObj.ngpiSymbolWidth,
					atcfResourceDataObj.ngpiSymbolSize);
			break;  
		case  UKM2_11:
			forecastModelAttributes = new ForecastModelAttributes<RGB, Integer, Float, Boolean>
			(atcfResourceDataObj.ukm2Enable,
					atcfResourceDataObj.ukm2Color,
					atcfResourceDataObj.ukm2LineWidth,
					atcfResourceDataObj.ukm2SymbolWidth,
					atcfResourceDataObj.ukm2SymbolSize);
			break;  
		case  HWFI_12:
			forecastModelAttributes = new ForecastModelAttributes<RGB, Integer, Float, Boolean>
			(atcfResourceDataObj.hwfiEnable,
					atcfResourceDataObj.hwfiColor,
					atcfResourceDataObj.hwfiLineWidth,
					atcfResourceDataObj.hwfiSymbolWidth,
					atcfResourceDataObj.hwfiSymbolSize);
			break;  

		case  GHMI_13:
			forecastModelAttributes = new ForecastModelAttributes<RGB, Integer, Float, Boolean>
			(atcfResourceDataObj.ghmiEnable,
					atcfResourceDataObj.ghmiColor,
					atcfResourceDataObj.ghmiLineWidth,
					atcfResourceDataObj.ghmiSymbolWidth,
					atcfResourceDataObj.ghmiSymbolSize);
			break;
		case  GFNI_14:
			forecastModelAttributes = new ForecastModelAttributes<RGB, Integer, Float, Boolean>
			(atcfResourceDataObj.gfniEnable,
					atcfResourceDataObj.gfniColor,
					atcfResourceDataObj.gfniLineWidth,
					atcfResourceDataObj.gfniSymbolWidth,
					atcfResourceDataObj.gfniSymbolSize);
			break;
		case  AEMI_15:
			forecastModelAttributes = new ForecastModelAttributes<RGB, Integer, Float, Boolean>
			(atcfResourceDataObj.aemiEnable,
					atcfResourceDataObj.aemiColor,
					atcfResourceDataObj.aemiLineWidth,
					atcfResourceDataObj.aemiSymbolWidth,
					atcfResourceDataObj.aemiSymbolSize);
			break;  
		case  TCON_16:
			forecastModelAttributes = new ForecastModelAttributes<RGB, Integer, Float, Boolean>
			(atcfResourceDataObj.tconEnable,
					atcfResourceDataObj.tconColor,
					atcfResourceDataObj.tconLineWidth,
					atcfResourceDataObj.tconSymbolWidth,
					atcfResourceDataObj.tconSymbolSize);
			break;  
		case  GUNA_17:
			forecastModelAttributes = new ForecastModelAttributes<RGB, Integer, Float, Boolean>
			(atcfResourceDataObj.gunaEnable,
					atcfResourceDataObj.gunaColor,
					atcfResourceDataObj.gunaLineWidth,
					atcfResourceDataObj.gunaSymbolWidth,
					atcfResourceDataObj.gunaSymbolSize);
			break;  
		case  TVCN_18:
			forecastModelAttributes = new ForecastModelAttributes<RGB, Integer, Float, Boolean>
			(atcfResourceDataObj.tvcnEnable,
					atcfResourceDataObj.tvcnColor,
					atcfResourceDataObj.tvcnLineWidth,
					atcfResourceDataObj.tvcnSymbolWidth,
					atcfResourceDataObj.tvcnSymbolSize);
			break;  
		case  EMXI_19 :
			forecastModelAttributes = new ForecastModelAttributes<RGB, Integer, Float, Boolean>
			(atcfResourceDataObj.emxiEnable,
					atcfResourceDataObj.emxiColor,
					atcfResourceDataObj.emxiLineWidth,
					atcfResourceDataObj.emxiSymbolWidth,
					atcfResourceDataObj.emxiSymbolSize);
			break; 
		case   FSSE_20:
			forecastModelAttributes = new ForecastModelAttributes<RGB, Integer, Float, Boolean>
			(atcfResourceDataObj.fsseEnable,
					atcfResourceDataObj.fsseColor,
					atcfResourceDataObj.fsseLineWidth,
					atcfResourceDataObj.fsseSymbolWidth,
					atcfResourceDataObj.fsseSymbolSize);
			break; 
		case   UKMI_21:
			forecastModelAttributes = new ForecastModelAttributes<RGB, Integer, Float, Boolean>
			(atcfResourceDataObj.ukmiEnable,
					atcfResourceDataObj.ukmiColor,
					atcfResourceDataObj.ukmiLineWidth,
					atcfResourceDataObj.ukmiSymbolWidth,
					atcfResourceDataObj.ukmiSymbolSize);
			break; 
		case  BAMM_22:
			forecastModelAttributes = new ForecastModelAttributes<RGB, Integer, Float, Boolean>
			(atcfResourceDataObj.bammEnable,
					atcfResourceDataObj.bammColor,
					atcfResourceDataObj.bammLineWidth,
					atcfResourceDataObj.bammSymbolWidth,
					atcfResourceDataObj.bammSymbolSize);
			break;  
		case  BAMD_23:
			forecastModelAttributes = new ForecastModelAttributes<RGB, Integer, Float, Boolean>
			(atcfResourceDataObj.bamdEnable,
					atcfResourceDataObj.bamdColor,
					atcfResourceDataObj.bamdLineWidth,
					atcfResourceDataObj.bamdSymbolWidth,
					atcfResourceDataObj.bamdSymbolSize);
			break; 
		case  BAMS_24: 
			forecastModelAttributes = new ForecastModelAttributes<RGB, Integer, Float, Boolean>
			(atcfResourceDataObj.bamsEnable,
					atcfResourceDataObj.bamsColor,
					atcfResourceDataObj.bamsLineWidth,
					atcfResourceDataObj.bamsSymbolWidth,
					atcfResourceDataObj.bamsSymbolSize);
			break;  
		case  EGR2_25:
			forecastModelAttributes = new ForecastModelAttributes<RGB, Integer, Float, Boolean>
			(atcfResourceDataObj.egr2Enable,
					atcfResourceDataObj.egr2Color,
					atcfResourceDataObj.egr2LineWidth,
					atcfResourceDataObj.egr2SymbolWidth,
					atcfResourceDataObj.egr2SymbolSize);
			break;

		case  CARQ_26:
			forecastModelAttributes = new ForecastModelAttributes<RGB, Integer, Float, Boolean>
			(true,
					new RGB(255,255,255),
					3,
					1,
					1.0f);
			break;           

		default:
			forecastModelAttributes = new ForecastModelAttributes<RGB, Integer, Float, Boolean>
		(false,
				new RGB(0,0,0),
				0,
				0,
				0.0f);
		break;
		}
		return forecastModelAttributes;
	}

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

	protected AtcfResource(AtcfResourceData resourceData, LoadProperties props) {
		super(resourceData, props);
		atcfResourceDataObj = (AtcfResourceData) resourceData;
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
	public void initResource(IGraphicsTarget grphTarget) throws VizException {
		queryRecords();
	}

	private String createQueryScriptForPython(String frameStartTimeStr, String frameEndTimeStr ){
		StringBuilder query = new StringBuilder();
		query.append("import AtcfTrackRequest\n");
		query.append("req = AtcfTrackRequest.AtcfTrackRequest()\n");
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
				
				/*The Python script will accept the time string only if it is formatted as given below....*/
				String frameStartTimeStr = new String (QUERY_DATE_FORMAT.format(frameStartTime.getTime())+".0");
				String frameEndTimeStr = new String (QUERY_DATE_FORMAT.format(frameEndTime.getTime())+".0");
				Object[] pdoList = Connector.getInstance().connect( 
						createQueryScriptForPython(frameStartTimeStr, frameEndTimeStr) , null, 60000);

				for (Object pdo : pdoList) {
					for( IRscDataObject rscDataObj : processRecord( pdo ) )	{	
						this.newRscDataObjsQueue.add(rscDataObj);
					}
				}				
				frameStartTime.clear();
			}

			setAllFramesAsPopulated();
		} catch (VizException e) {
			e.printStackTrace();
		}
	}

	// wrap the AtcfCyclone with a RscDataObject so the Abstract class can time match it.
	//
	@Override
	public IRscDataObject[] processRecord( Object dataRec ) {
		if( !(dataRec instanceof AtcfCyclone ) ) {
			return new IRscDataObject[]{};
		}
		AtcfCycloneRscDataObject rscDataObj = 
			new AtcfCycloneRscDataObject( (AtcfCyclone)dataRec );
		return new AtcfCycloneRscDataObject[]{ rscDataObj };
	}
	
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
	 * Repaints the ATCF tracks each time the cursor is moved across the window displaying the ATCF resource 
	 * @param frameData - the frame to be rendered
	 * @param graphicsTarget - the graphics target
	 * @param paintProps - the pain properties
	 */
	@Override
	public  void paintFrame(
			AbstractFrameData frameData,
			IGraphicsTarget graphicsTarget, PaintProperties paintProps)
	throws VizException {
		/*  
		 *Calculate vertical/horizontal offset parameters 
		 */
		if( font == null ) {
			font         = graphicsTarget.initializeFont("Monospace", 14, new IFont.Style[] { IFont.Style.BOLD});
			charSize           = graphicsTarget.getStringBounds(font, "N");
			charHeight         = charSize.getHeight();
			charWidth          = charSize.getWidth();
		}

		double screenToWorldRatio = paintProps.getCanvasBounds().width /paintProps.getView().getExtent().getWidth(); 
		double offsetY            = charHeight / screenToWorldRatio;
		double offsetX            = charWidth / screenToWorldRatio; 

		if( paintProps == null ) {
			return;
		}

		if(frameData != null){
			//System.out.println("AtcfResource.paintFrame() invoked.....");
			FrameData currFrameData = (FrameData) frameData;
			List<AtcfTrack> thisAtcfTrackList = new ArrayList<AtcfTrack>(0);  
			Collection<AtcfCyclone>  atcfCycloneCollection =  currFrameData.atcfFrameCycloneMap.values();
			PixelCoordinate pixCoordArray[] = null;
			RGB trackColor                          = new RGB(0,0,0);
			LineStyle lineStyle                      = LineStyle.SOLID;
			float trackWidth                         = 0.0f;
			float symbolSize                     = 0.0f;              
			
			/*If the frame contains a list of cyclones with one or more cyclones in it*/
			if(atcfCycloneCollection != null ){

				/*Loop through each cyclone*/
				for(AtcfCyclone thisAtcfCyclone : atcfCycloneCollection ){

					String legendTime = "";
					float leftMostLocation = 180.0f ; //invalid value initialization
					float lowestLocation   = 90.0f; //invalid value initialization
					PixelCoordinate textLoc = null;

					thisAtcfTrackList = new ArrayList<AtcfTrack>(thisAtcfCyclone.getTrackList());
					if(thisAtcfTrackList != null && thisAtcfTrackList.size() > 0){

						/*Get the legend time to be displayed - the warning time per cyclone formatted to match legacy display*/
						legendTime = new String( sdf.format(thisAtcfTrackList.get(0).getWarningTime().getTime()));

						/*Disable the legend enable map for each cyclone so that the boolean values are reset 
						 in each frame*/
						for(String thisLegend: this.FORECAST_MODEL_NAMES){
							modelLegendEnabledMap.put( this.getEnumForModelName(thisLegend), new Boolean(false));
						}
						
						/*Loop through each track of the cyclone*/
						for(AtcfTrack thisAtcfTrack : thisAtcfTrackList ){

							float[] latArray = thisAtcfTrack.getClat();
							float[] lonArray = thisAtcfTrack.getClon();
							/*Convert the lat/lon points into PixelCoordinates to be used in the drawLine() method*/        	  
							pixCoordArray = this.convertTrackLocationPointsToPixelCoordinateArray(latArray, lonArray);

							if(pixCoordArray != null){
								CycloneForecastTechniqueType cftt =  this.getEnumForModelName(new String(thisAtcfTrack.getTechnique()));

								/*If the technique (model name) is not unknown - process it...*/
								if (cftt.compareTo(CycloneForecastTechniqueType.UNKNOWN) != 0) {

									/*For a CARQ track, if the storm name is not already set, store it*/
									if(cftt.compareTo(CycloneForecastTechniqueType.CARQ_26) == 0){
										if(  thisAtcfTrack.getStormName().isEmpty()){
											thisAtcfCyclone.setCyclone( new String(thisAtcfTrack.getStormName()));
										}
									}

									/*Get the track attributes associated with each technique*/
									ForecastModelAttributes<RGB, Integer, Float, Boolean>trackAttributes =  this.getForecastModelAttributesForEnum(cftt);

									/*If the technique/model is enabled plot it*/
									if (trackAttributes.getModelEnable()) {

										int windSpeedIndex =0;

										float[] windArr =  thisAtcfTrack.getWindMax();								
										PixelCoordinate prevLoc = null;
										trackColor = trackAttributes .getColorOfModel();
										trackWidth = trackAttributes.getLineWidth();
										int fcstHourIndex = 0;
										int[] fcstHourArray = thisAtcfTrack.getForecastHour();
										for (PixelCoordinate currLoc : pixCoordArray) {
											if (prevLoc != null) {

												if(cftt.compareTo(CycloneForecastTechniqueType.CARQ_26) == 0 && fcstHourArray[fcstHourIndex] > 0 ){
													lineStyle = LineStyle.DASHED;
													/*Add logic to change line style for future history tracks....*/
												}else{
													lineStyle = LineStyle.SOLID;
												}

												/*draw the track point by point*/
												graphicsTarget.drawLine(
														prevLoc.getX(),
														prevLoc.getY(),
														prevLoc.getZ(),
														currLoc.getX(),
														currLoc.getY(),
														currLoc.getZ(),
														trackColor,
														trackWidth,
														lineStyle);
											}
											prevLoc = currLoc;
											fcstHourIndex ++;
											/*If the check-box for plotting the marker is enabled, draw a cross-hair marker at each lat-lon point along each track.*/

											if(atcfResourceDataObj.getMarkerEnable()){

												symbolSize = trackAttributes.getSymbolSize();
												lineStyle = LineStyle.SOLID;

												graphicsTarget.drawLine(
														currLoc.getX() - offsetY/2,
														currLoc.getY(),
														currLoc.getZ(),
														currLoc.getX() + offsetY/2,
														currLoc.getY(),
														currLoc.getZ(),
														trackColor,
														symbolSize,
														lineStyle);														

												graphicsTarget.drawLine(
														currLoc.getX() ,
														currLoc.getY()- offsetY/2,
														currLoc.getZ(),
														currLoc.getX(),
														currLoc.getY()  + offsetY/2,
														currLoc.getZ(),
														trackColor,
														symbolSize,
														lineStyle);	

											}

											/*
											 * If the check-box for speed is enabled, plot the wind-speed at an offset from the marker for each lat/lon point for each track. 
											 * */
											if(atcfResourceDataObj.getSpeedEnable()){

												PixelCoordinate windSpeedLoc = new PixelCoordinate( currLoc);
												windSpeedLoc.addToX(offsetX);
												windSpeedLoc.addToY(offsetY);
												String theWindSpeedStrArr[] = new String[]{ new Float( windArr[windSpeedIndex]).toString()}; 
												DrawableString ds = new DrawableString(theWindSpeedStrArr, trackAttributes.getColorOfModel());
												ds.setCoordinates(windSpeedLoc.getX(), windSpeedLoc.getY());
												ds.font = font;
												ds.textStyle = TextStyle.BOXED;
												ds.horizontalAlignment = HorizontalAlignment.CENTER;
												graphicsTarget.drawStrings(ds);
												//graphicsTarget.drawStrings1Box(font, theWindSpeedStrArr, windSpeedLoc.getX(), windSpeedLoc.getY(), 0.0, TextStyle.NORMAL, new RGB[]{trackAttributes.getColorOfModel()}, HorizontalAlignment.CENTER, 0.0);
												windSpeedIndex++;
											}                                                

										}//for (PixelCoordinate currLoc : pixCoordArray) 

										//											/*To enable/disable the corresponding legend display*/
										//											if ( this.modelLegendEnabledMap != null 
										//													&& !this.modelLegendEnabledMap.isEmpty()
										//													&& this.modelLegendColorMap != null
										//
										//													){
										////												trackAttributes =  this.getForecastModelAttributesForEnum(cftt);
										//											    this.modelLegendEnabledMap.put(cftt, new Boolean(trackAttributes.getModelEnable()));
										//											    this.modelLegendColorMap.put(cftt, trackAttributes.getColorOfModel()); 
										//											}
										/*If the model is enabled, store the color for its name display*/
										if(this.modelLegendColorMap != null){
											RGB colorOfLegend = trackAttributes.getColorOfModel();
											this.modelLegendColorMap.put(cftt, new RGB(colorOfLegend.red,colorOfLegend.green, colorOfLegend.blue));
										}

										/*
										 * Determine the western-most and the lowest point from all tracks to set the location of the first model legend
										 * */
										float[] tempLatArray = new float[latArray.length];
										float[] tempLonArray = new float[lonArray.length];
										System.arraycopy(latArray, 0, tempLatArray, 0, latArray.length);
										System.arraycopy(lonArray, 0, tempLonArray, 0, lonArray.length);

										Arrays.sort(tempLatArray);
										Arrays.sort(tempLonArray);

										if(tempLatArray[0]   <    leftMostLocation){
											leftMostLocation = tempLatArray[0];
										}

										if(tempLonArray[0]   <    lowestLocation){
											lowestLocation = tempLonArray[0];
										}

										/* Determine the location of the first model legend
										 * (The leftmost/lowest point across all tracks is finally used to render the model names)
										 * */
										textLoc = new PixelCoordinate(this.convertFloatToPixelCoordinate(leftMostLocation, lowestLocation));

										/*Reset the index to retrieve the first wind-speed of the next track*/
										windSpeedIndex=0;
									}//if (trackAttributes.getModelEnable())

									/*Add the following key-value pair to a HashMap:
									 * The enum for the model name (the key) and whether it is enabled/not (corresponding value)
									 */
									if(this.modelLegendEnabledMap != null){
										this.modelLegendEnabledMap.put(cftt, new Boolean(trackAttributes.getModelEnable()));
									}

								}
							}//if(picCoordArray != null)
						}

						if (textLoc != null) {
							/*
							 * Render the names of the enabled tracks (techniques)
							 * */
							textLoc.addToX(-offsetX * 5);
							for (String thisLegendName : this.FORECAST_MODEL_NAMES) {
								CycloneForecastTechniqueType theEnumForModelName = this
								.getEnumForModelName(thisLegendName);
								if (this.modelLegendEnabledMap.get(
										theEnumForModelName).booleanValue()) {
									RGB thisLegendColorFromMap = this.modelLegendColorMap.get(theEnumForModelName);
									graphicsTarget.drawString(font,
											thisLegendName, textLoc.getX(),
											textLoc.getY(), 0.0,
											TextStyle.NORMAL,
											thisLegendColorFromMap,
											HorizontalAlignment.RIGHT,
											VerticalAlignment.BOTTOM, 0.0);

									textLoc.addToY(-offsetY * 1.75);
								}
							}
						}
					}

					if (textLoc != null) {

						/*If the storm name display is enabled, render it....*/
						if (atcfResourceDataObj.getNameEnable()) {
							graphicsTarget.drawString(font, thisAtcfCyclone
									.getCyclone(), textLoc.getX(), textLoc
									.getY(), 0.0, TextStyle.NORMAL,
									new RGB(255, 255, 255),
									HorizontalAlignment.RIGHT,
									VerticalAlignment.BOTTOM, 0.0);
							textLoc.addToY(-offsetY * 1.75);
						}

						/*If the storm's warn time display is enabled, render it....*/
						if (atcfResourceDataObj.getTimeEnable()) {

							graphicsTarget.drawString(font, legendTime,
									textLoc.getX(), textLoc.getY(), 0.0,
									TextStyle.NORMAL,
									new RGB(255, 255, 255),
									HorizontalAlignment.RIGHT,
									VerticalAlignment.BOTTOM, 0.0);
						}
					}	    			  
				}
			}
		}
	}


	private class FrameData extends AbstractFrameData{

		// map from the name of the cyclone to the AtcfCyclone object
		private Map<String,AtcfCyclone> atcfFrameCycloneMap;

		public FrameData(DataTime frameTime, int timeInt) {
			super(frameTime, timeInt);
			atcfFrameCycloneMap = new HashMap<String, AtcfCyclone>(0);
		}

		@Override
		public boolean updateFrameData(IRscDataObject rscDataObj) {
			if( !(rscDataObj instanceof AtcfCycloneRscDataObject ) ) {
				System.out.println("AtcfResource:updateFrameData() processing.....\n" +
						"Data belongs to a different class :"+ rscDataObj.getClass().toString() );
				return false;
			}
			AtcfCyclone atcfCyclone = ((AtcfCycloneRscDataObject)rscDataObj).getAtcfCyclone();			
			DataTime newTrackTime = ((AtcfCycloneRscDataObject)rscDataObj).getDataTime();
			String cycloneName = atcfCyclone.getCyclone();
			
			// if this cyclone is already in the map, check the time and determine if this
			// data is a better match.
			if( atcfFrameCycloneMap.containsKey(cycloneName) )  {
				AtcfCyclone existingCyclone = atcfFrameCycloneMap.get(cycloneName);

				if( !existingCyclone.getTrackList().isEmpty() ) {
					DataTime existingTime = new DataTime( 
							existingCyclone.getTrackList().get(0).getWarningTime() );
					
					if( timeMatch( newTrackTime ) < timeMatch( existingTime ) ) {
						// overwrite the existing cyclone with the better match
						atcfFrameCycloneMap.put( cycloneName, atcfCyclone );						
					}
				}
			}
			else {
				atcfFrameCycloneMap.put( cycloneName, atcfCyclone );
			}
			return true;
		}

	}

	/***
	 * Private class to capture the attributes (such as the color, line width, symbol width etc) of each model 
	 * from the AtcfResourceData class.
	 *  
	 * @author archana
	 *
	 * @param <RGB> - the color of the model
	 * @param <Integer> - the line width of the track
	 * @param <Integer> - the line width of the tsymbol 
	 * @param <Float> - the size of the symbol
	 * @param <Boolean> - flag to check if the model is enabled
	 */
	private class ForecastModelAttributes<RGB, Integer, Float, Boolean>{
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
		if (fd == null || fd.getFrameTime() == null || fd.atcfFrameCycloneMap.size() == 0) {
			return legendString + "-No Data";
		}
		return legendString + " "+ NmapCommon.getTimeStringFromDataTime( fd.getFrameTime(), "/");
	}
}
