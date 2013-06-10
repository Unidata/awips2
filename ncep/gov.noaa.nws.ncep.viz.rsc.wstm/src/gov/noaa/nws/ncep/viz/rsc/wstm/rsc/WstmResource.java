/*
 * WstmResource
 * 
 * Date created (November 05, 2010)
 *
 *  This code has been developed by the SIB for use in the AWIPS2 system. 
 */
package gov.noaa.nws.ncep.viz.rsc.wstm.rsc;

import java.awt.geom.Rectangle2D;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Calendar;
import java.util.TimeZone;
import java.awt.Color;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.TextStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery;
import com.raytheon.uf.viz.core.catalog.LayerProperty;
import com.raytheon.uf.viz.core.catalog.ScriptCreator;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery.QueryLanguage;
import com.raytheon.uf.viz.core.comm.Connector;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.IShadedShape;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.geom.PixelCoordinate;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceType;
import com.raytheon.uf.edex.decodertools.core.LatLonPoint;

import gov.noaa.nws.ncep.common.dataplugin.aww.AwwFips;
import gov.noaa.nws.ncep.common.dataplugin.aww.AwwRecord;
import gov.noaa.nws.ncep.common.dataplugin.aww.AwwUgc;
import gov.noaa.nws.ncep.common.dataplugin.aww.AwwVtec;
import gov.noaa.nws.ncep.ui.pgen.display.DisplayElementFactory;
import gov.noaa.nws.ncep.ui.pgen.display.IDisplayable;
import gov.noaa.nws.ncep.ui.pgen.elements.Symbol;
import gov.noaa.nws.ncep.viz.common.ui.NmapCommon;
import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsResource;
import gov.noaa.nws.ncep.viz.resources.INatlCntrsResource;
import gov.noaa.nws.ncep.viz.ui.display.NCMapDescriptor;
import gov.noaa.nws.ncep.viz.ui.display.NcDisplayMngr;

import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.viz.core.rsc.jts.JTSCompiler;
import com.raytheon.viz.core.rsc.jts.JTSCompiler.PointStyle;
import com.raytheon.viz.ui.editor.AbstractEditor;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.Point;
import com.vividsolutions.jts.io.WKBReader;
import com.vividsolutions.jts.io.ParseException;


/**
 * WstmResource - Displays Winter Storm Misc Resource
 * 
 *  
 *
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 05-Nov- 2010   247       Archana    Initial creation.   
 * 16 Feb 2012    555      S. Gurung   Added call to setAllFramesAsPopulated() in queryRecords().
 * 05/23/2012     785      Q. Zhou     Added getName for legend.
 * 17 Aug 2012    655      B. Hebbard  Added paintProps as parameter to IDisplayable draw
 * 31-Jan-2013    976      Archana         Updated paintFrame() to not render any null strings
 *                                                                Replaced the depreciated target.drawString() method with target.drawStrings().
 * </pre>
 * 
 * @author archana
 * @version 1.0
 */
public class WstmResource extends AbstractNatlCntrsResource<WstmResourceData, NCMapDescriptor >
implements INatlCntrsResource{
	List<String> issueOfficeList = new ArrayList<String>(0);
	private final String BOUNDS_SCHEMA = "bounds";
	private final String STATIONS_SCHEMA = "stns";
	private final String DATABASE = "ncep";
	private final String BOUNDS_TABLE = "pfzbnds";
	private final String ZONES_TABLE = "zones";
	private IFont font = null;
	float  baseFontSize = 14;
	private Rectangle2D charSize;
	private double charHeight;
	private double charWidth;	
	
    private WstmResourceData wstmResourceDataObj;
    
	private enum Significance{ADVISORY,WATCH,WARNING, STATEMENT, FORECAST, OUTLOOK, SYNOPSIS, UNKNOWN};
	
	private enum ProductType{OPERATIONAL, TEST, EXPERIMENTAL, EXPERIMENTAL_VTEC_IN_OPERATIONAL_PRODUCT, UNKNOWN};
	private enum PhenomenonType{BLIZZARD, WINTER_STORM, WINTER_WEATHER, SNOW, 
		                                                                HEAVY_SNOW, LAKE_EFFECT_SNOW, BLOWING_SNOW,
		                                                                SNOW_AND_BLOWING_SNOW, 
		                                                                LAKE_EFFECT_SNOW_AND_BLOWING_SNOW, 
		                                                                SLEET, FREEZING_RAIN,	ICE_STORM, 
		                                                                WIND_CHILL, UNKNOWN};
		                                                                
	private enum ActionType{UPGRADE, CANCEL , CONTINUE, 
		                                            CORRECTION, EXPIRE, EXTEND_AREA_ONLY,
		                                            EXTEND_TIME_ONLY, EXTEND_TIME_AND_AREA,
		                                            NEW, UNKNOWN};
/**
 *  Constructor
 *  Invokes the base class constructor to process the incoming records.
 *  Also associates the WSTM resource with its corresponding resource data object 		                                            
 * @param resourceData - The editable attributes of the  WSTM resource.
 * @param props - the options for loading the data for this resource
 */
	protected WstmResource(WstmResourceData resourceData, LoadProperties props) {
		super(resourceData, props);
		wstmResourceDataObj =  (WstmResourceData) resourceData;		addRDChangedListener();//T456
	}

	
	@Override
	/**
	 * Creates a new frame with the specified reference time and frame interval
	 * @param frameTime - The refernce time for the frame to be created
	 * @param frameInterval - The interval between the created frames
	 * 
	 * @return  Returns the new frame with the specified frame reference time and time interval 
	 */
	protected AbstractFrameData createNewFrame(
			DataTime frameTime, int frameInterval) {
		    return (AbstractFrameData) new FrameData( frameTime, frameInterval );
			}


	@Override
/**
 * Overridden method. Invokes queryRecords() to fetch the records from the database
 * per the metadata map in WSTM.xml
 */
	public void initResource(IGraphicsTarget grphTarget) throws VizException {long t1 = System.currentTimeMillis();
    	queryRecords();		long t2 = System.currentTimeMillis(); System.out.println("__^^^__ initResource (t2-t1): "+(t2-t1));
	}

	@Override
/***
 * Renders the WSTM resource on the frame
 */
	public void paintFrame(AbstractFrameData frameData,
			                                        IGraphicsTarget target, 
			                                        PaintProperties paintProps)	throws VizException {

		/*  
		 *Calculate vertical/horizontal offset parameters 
		 */
		if( font == null ) {
			font         = target.initializeFont("Monospace", 14, new IFont.Style[] { IFont.Style.BOLD});
			charSize           = target.getStringBounds(font, "N");
			charHeight         = charSize.getHeight();
			charWidth          = charSize.getWidth();
		}

		double screenToWorldRatio = paintProps.getCanvasBounds().width /paintProps.getView().getExtent().getWidth(); 
		double offsetY            = charHeight / screenToWorldRatio;
		double offsetX            = charWidth / screenToWorldRatio; 

		if( paintProps == null ) {
			return;
		}
		if( areaChangeFlag ){ areaChangeFlag = false; postProcessFrameUpdate(); }//T456		
		if(frameData != null){
			FrameData currFrameData = (FrameData) frameData;
			
			Collection<WstmRscDataObject> wstmRscDataObjCollection =   currFrameData.wstmDataMap.values();
			
			/*For each WstmRscDataObject in this frame*/
			for(WstmRscDataObject eachWstmRscDataObject : wstmRscDataObjCollection){
				
				WstmResourceAttributes<RGB, Integer, Float, Boolean> wstmRscAttr = null;
				String symbolTypeStr = "";
                 
					/*Retrieve the user-configurable attributes depending on whether the WstmRscDataObject denotes an
					 * advisory, watch or a warning*/
                             if(eachWstmRscDataObject.significance ==Significance.ADVISORY){
                              	 
         						wstmRscAttr = new WstmResourceAttributes<RGB, Integer, Float, Boolean>(
        								wstmResourceDataObj.getWstmAdvisoryEnable(),
        								wstmResourceDataObj.getWstmAdvisoryColor(), 
        								wstmResourceDataObj.getWstmAdvisoryLineWidth(),
        								wstmResourceDataObj.getWstmAdvisorySymbolWidth(),
        								wstmResourceDataObj.getWstmAdvisorySymbolSize());
         						        symbolTypeStr = new String( EditWstmAttrDialog.advisoryMarkerData);
                             }
                             else if(eachWstmRscDataObject.significance ==Significance.WARNING){

                            	 wstmRscAttr = new WstmResourceAttributes<RGB, Integer, Float, Boolean>(
        								wstmResourceDataObj.getWstmWarningEnable(),
        								wstmResourceDataObj.getWstmWarningColor(), 
        								wstmResourceDataObj.getWstmWarningLineWidth(),
        								wstmResourceDataObj.getWstmWarningSymbolWidth(),
        								wstmResourceDataObj.getWstmWarningSymbolSize());  
                            	 symbolTypeStr = new String( EditWstmAttrDialog.warningMarkerData);
                             }
                             else if(eachWstmRscDataObject.significance ==Significance.WATCH){
         						wstmRscAttr = new WstmResourceAttributes<RGB, Integer, Float, Boolean>(
        								wstmResourceDataObj.getWstmWatchEnable(),
        								wstmResourceDataObj.getWstmWatchColor(), 
        								wstmResourceDataObj.getWstmWatchLineWidth(),
        								wstmResourceDataObj.getWstmWatchSymbolWidth(),
        								wstmResourceDataObj.getWstmWatchSymbolSize());                           	 
         						        symbolTypeStr = new String( EditWstmAttrDialog.watchMarkerData);
                             }
                             
         					if ( wstmRscAttr != null && wstmRscAttr.getEventEnable()){
         						RGB colorOfEventRGB = wstmRscAttr.getColorOfEvent();
         						Color colorOfEvent = new Color( colorOfEventRGB.red,
         						                                                		      colorOfEventRGB.green,
         								                                                      colorOfEventRGB.blue);
							
         					  Color[] symbolColor = {colorOfEvent };
							   /*Retrieve the FIPS zones (names and centroid) to be rendered for the current WstmRscDataObject*/	
         					   List<FipsInfo> listOfFipsInfo =  eachWstmRscDataObject.aListOfFipsInfoObjects;
         					  
         					   if (listOfFipsInfo != null && listOfFipsInfo.size() > 0) {
 								
         						   for (FipsInfo eachFipsInfo : listOfFipsInfo) {
         							   LatLonPoint thisPoint =  wqr.getLatLonPoint(eachFipsInfo.getFipsCode());//eachFipsInfo.getFipsCentroid();//T456
							           Coordinate thisMarkerCoord = this.convertCentroidToWorldCoordinates(thisPoint);
							           PixelCoordinate pixCoord = null;

							           /*If the flag is enabled - Plot the duration for which the weather hazard (WstmRescDataObject) is valid*/
							           if(wstmResourceDataObj.getTimeEnable()){
								        	if (thisMarkerCoord != null) {
												double worldC[] = new double[] {thisMarkerCoord.x,
														                                                   thisMarkerCoord.y };
												pixCoord = new PixelCoordinate(descriptor
																             .worldToPixel(worldC));
												pixCoord.addToY(offsetY*1.75);
												if ( eachWstmRscDataObject.validTimePeriod != null ){
													  
													DrawableString validTimePeriodString = new DrawableString(eachWstmRscDataObject.validTimePeriod,colorOfEventRGB);
													validTimePeriodString.setCoordinates(pixCoord.getX(), pixCoord.getY());
													validTimePeriodString.textStyle = TextStyle.NORMAL;
													validTimePeriodString.horizontalAlignment = HorizontalAlignment.LEFT;
													validTimePeriodString.verticallAlignment = VerticalAlignment.TOP;
													target.drawStrings(validTimePeriodString);
//										               target.drawString(font,  eachWstmRscDataObject.validTimePeriod, pixCoord.getX(), pixCoord.getY(), 0.0, 
//												                                       TextStyle.NORMAL, wstmRscAttr.getColorOfEvent(),HorizontalAlignment.LEFT, 
//														                               VerticalAlignment.TOP, 0.0);  							
												}

											}
							           }
							           
							           /*If the flag is enabled - Plot the name of the current FIPS zone in which this weather hazard is valid*/ 
							          if(wstmResourceDataObj.getZoneNameEnable()){
							        	  if(pixCoord != null){
							        		  pixCoord.addToY(offsetY*1.75);
							        	  }
							        	  else{
								        		if (thisMarkerCoord != null) {
												double worldC[] = new double[] {thisMarkerCoord.x,
														                                                   thisMarkerCoord.y };
												pixCoord = new PixelCoordinate(descriptor
																             .worldToPixel(worldC));
												pixCoord.addToY(offsetY*1.75);

											}
							          }
							        	  String zoneName = wqr.getZoneName(eachFipsInfo.getFipsCode());
							        	  if ( zoneName != null ){
												DrawableString zoneNameString = new DrawableString(zoneName,colorOfEventRGB);
												zoneNameString.setCoordinates(pixCoord.getX(), pixCoord.getY());
												zoneNameString.textStyle = TextStyle.NORMAL;
												zoneNameString.horizontalAlignment = HorizontalAlignment.LEFT;
												zoneNameString.verticallAlignment = VerticalAlignment.TOP;
												target.drawStrings(zoneNameString);							        		  
//											target.drawString(font, wqr.getZoneName(eachFipsInfo.getFipsCode())/*eachFipsInfo.getZoneName()*/, pixCoord.getX(), pixCoord.getY(), 0.0, 
//													TextStyle.NORMAL, wstmRscAttr.getColorOfEvent(),HorizontalAlignment.LEFT, 
//													VerticalAlignment.TOP, 0.0);
							        	  }
							           }
							          
							          /*If the outline flag is enabled draw the outline else plot the marker at the centroid of the zone's area*/
									   if(wstmResourceDataObj.getOutlineEnable()){		
drawOutlineForZone2(eachFipsInfo.getFipsCode()/*eachFipsInfo.fipsNumber*/, target, wstmRscAttr.getColorOfEvent(), wstmRscAttr.getLineWidth());
							           }else{

									/*Plot the symbol selected by the user from the marker selection panel*/
							        	   if (thisMarkerCoord != null) {
													DisplayElementFactory df = new DisplayElementFactory( target, getNcMapDescriptor() );
													ArrayList<IDisplayable> displayEls = new ArrayList<IDisplayable>(0);	
										            Symbol symbol = new Symbol(
															null,
															symbolColor,
															wstmRscAttr.getSymbolWidth(),
															wstmRscAttr.getSymbolSize(), /* scale per NMAP*/
															false,
															thisMarkerCoord,
															"Symbol",
															this.getActualSymbolName(symbolTypeStr));
										            displayEls = df.createDisplayElements(symbol,paintProps);
													if (displayEls != null && !displayEls.isEmpty()) {
														for (IDisplayable each : displayEls) {
															each.draw(target, paintProps);
															each.dispose();
														}
													}
											}									           
							        }
								}
							}
        					}
         					wstmRscAttr = null;
			}
		}
		
	}

	/**
	 * Retrieves the geometric bounds of the zone from the database and plots it
	 * @param fipsCode - the zone whose bounds need to be retrieved
	 * @param target - the graphics target
	 * @param lineColor - the color of the outline
	 * @param lineWidth - the thickness of the outline 
	 * @throws VizException
	 */
	private void drawOutlineForZone( String fipsCode, IGraphicsTarget target,RGB lineColor,int lineWidth) throws VizException{
		StringBuilder queryString = new StringBuilder(
		"select AsBinary(the_geom_0_001) from ");//the_geom) from ");//T456
queryString.append("mapdata.zone");//BOUNDS_SCHEMA + "." + BOUNDS_TABLE);//T456
		queryString.append(" where state_zone = '");//" where fips = '");//T456
		queryString.append(fipsCode.substring(0,2)+fipsCode.substring(3));//fipsCode);//T456
		queryString.append("'");
		queryString.append(";");

		try {
List<Object[]> results = DirectDbQuery.executeQuery(queryString.toString(), "maps"/*DATABASE*/, QueryLanguage.SQL);//T456
			LineStyle lineStyle = LineStyle.SOLID;
			Geometry geometry = null;
			WKBReader wkbReader = new WKBReader();
			IWireframeShape newOutlineShape = target.createWireframeShape(false, descriptor, 0.0f);
			IShadedShape newShadedShape = target.createShadedShape(false,descriptor, true);
			JTSCompiler jtsCompiler = new JTSCompiler(newShadedShape,newOutlineShape, descriptor, PointStyle.CROSS);
			newShadedShape.compile();

			if(results != null && results.size() > 0){
				for(Object[] result : results){
					int k = 0;
					try {
						byte[] wkb = (byte[]) result[k++];    		
						geometry = (Geometry) wkbReader.read(wkb);
						if( ! (geometry instanceof Point) ){
							jtsCompiler.handle(geometry);				
						}
					}
					catch (VizException e) {
						System.out.println("Error in WstmResource.drawOutlineForZone().\n Unable to reproject the zone's outline\n "+ e.getMessage());
					}			      
					catch(ParseException pe){
						System.out.println("Error in WstmResource.drawOutlineForZone().\nParseException thrown while trying to read the geometry from bounds.pfzbnds table: "+pe.getMessage());
					}
				}
				newOutlineShape.compile();    
				if (newOutlineShape != null && newOutlineShape.isDrawable()) {
					target.drawWireframeShape(newOutlineShape, lineColor, lineWidth,lineStyle);
				}
			}
		} catch (VizException e) {
			System.out.println("Error in WstmResource.drawOutlineForZone(). \n + Unable to retrieve records from the bounds table:\n"
					+ e.getMessage());
		}
	}
	
	@Override
	
	/***
	 * Overridden method to process the incoming AWW records
	 * @param pdo - the AwwRecord
	 * Returns an array of IRscDataObject processed from the AwwRecord
	 */
	protected IRscDataObject[] processRecord(Object pdo) {long t1 = System.currentTimeMillis();
		if(! (pdo instanceof AwwRecord)) {
			System.out.println("Error: " + "Object is of type " + pdo.getClass().getCanonicalName() + "instead of type AwwRecord");
			return new IRscDataObject[]{};
		}
		AwwRecord awwRecord = (AwwRecord) pdo;
		
		List<WstmRscDataObject> wstmRscDataObjectList = getWstmData(awwRecord);
		if(wstmRscDataObjectList == null || wstmRscDataObjectList.size() == 0){
			return new IRscDataObject[]{};
		}
		int listSize = wstmRscDataObjectList.size();
		IRscDataObject[] irscDataObjArray = new IRscDataObject[listSize];
		for(int index = 0; index < listSize; index++){
			irscDataObjArray[index] = wstmRscDataObjectList.get(index);
		}//long t2 = System.currentTimeMillis(); System.out.println("+++___+++------------- processRecord (t2-t1): "+(t2-t1));
		return irscDataObjArray;
	}
	
	/***
	 * 
	 * @param latLonPt
	 * @return
	 */
	private Coordinate convertCentroidToWorldCoordinates(LatLonPoint latLonPt){
		Coordinate worldCoord = null;
		if (latLonPt != null) {
			double pointArr[] = new double[] {
					latLonPt.getLongitude(LatLonPoint.INDEGREES),
					latLonPt.getLatitude(LatLonPoint.INDEGREES) };
			worldCoord = new Coordinate(pointArr[0], pointArr[1]);
		}
		return worldCoord;
	}
	
	/***
	 * 
	 * @param iconName
	 * @return
	 */
	private String getActualSymbolName(String iconName){
	    String actualSymbolName = "ASTERISK";
	    if(iconName.compareTo("TRIANGLE") == 0){
	    	actualSymbolName = "FILLED_TRIANGLE"; //refer symbolPatterns.xml
	    }
       else if (iconName.compareTo("OCTAGON") == 0){
    	   actualSymbolName = "FILLED_OCTAGON";
	    }
       else if (iconName.compareTo("SQUARE") == 0){
    	   actualSymbolName = "FILLED_BOX";
	    } 
       else if (iconName.compareTo("STAR") == 0){
    	   actualSymbolName = "FILLED_STAR";
	    } 
       else if (iconName.compareTo("DIAMOND") == 0){
    	   actualSymbolName = "FILLED_DIAMOND";
	    } 
	    return actualSymbolName;
	}
	
	/***
	 * Returns a list of <code>WstmRscDataObject</code> 
	 * Each  <code>WstmRscDataObject</code> in the list maps to a Vtec line from the original bulletin.
	 * @param awwRecord - the AwwRecord retrieved from the database
	 * @return a list of  <code>WstmRscDataObject</code>
	 */
	private List<WstmRscDataObject> getWstmData(AwwRecord awwRecord){
		WstmRscDataObject wstmRscDataObject  = null;
List<WstmRscDataObject> wstmRscDataObjectList = new ArrayList<WstmRscDataObject>(0); 
			Set<AwwUgc>  thisAwwUgcSet =  awwRecord.getAwwUGC();
			
			for(AwwUgc eachAwwUgc : thisAwwUgcSet ){
				Set<AwwVtec> aSetOfAwwVtec= new HashSet<AwwVtec>( eachAwwUgc.getAwwVtecLine());
				Set<AwwFips> aSetOfAwwFips = new HashSet<AwwFips>( eachAwwUgc.getAwwFIPS());
//for(AwwVtec av : eachAwwUgc.getAwwVtecLine())System.out.println("____________vtec: "+av.getVtecLine());
				for(AwwVtec thisVtec: aSetOfAwwVtec){

							wstmRscDataObject                        = new WstmRscDataObject();
							/* (Non-Javadoc) - From each VTEC line in the bulletin
							 * retrieve the following information about the weather hazard:
							 * */
							wstmRscDataObject.actionType      = wstmRscDataObject.getActionTypeForThisAction(thisVtec.getAction());
							wstmRscDataObject.significance     =  wstmRscDataObject.getSignificanceType(thisVtec.getSignificance());
							wstmRscDataObject.productType    = wstmRscDataObject.getVTECProductTypeForThisProduct(thisVtec.getProductClass());
							wstmRscDataObject.phenomenonType = wstmRscDataObject.getPhenomenonType(thisVtec.getPhenomena());
							wstmRscDataObject.eventNumber    = thisVtec.getEventTrackingNumber();
							wstmRscDataObject.officeId             = thisVtec.getOfficeID();
							Calendar startTimeCal =  thisVtec.getEventStartTime();
							Calendar endTimeCal  =  thisVtec.getEventEndTime();
							/*(Non-Javadoc)
							 *  The startTimeCal will be null if the product is issued after the event started.
							 * In this case, the start time is set to the issue-time.
							 */
							 
							if(startTimeCal == null){
								startTimeCal =  awwRecord.getIssueTime();
                                wstmRscDataObject.isStartTimeSetToIssueTime = true;
							}
							
							if ( startTimeCal != null  && endTimeCal != null ){
								wstmRscDataObject.startTime = new DataTime(startTimeCal);
								wstmRscDataObject.endTime = new DataTime(endTimeCal);
								wstmRscDataObject.eventTime = new DataTime(startTimeCal, new TimeRange(startTimeCal, endTimeCal));								
								wstmRscDataObject.validTimePeriod = wstmRscDataObject.getEventTimePeriodAsString(wstmRscDataObject.eventTime);
							}	

							/* (Non-Javadoc) - From the UGC line corresponding to the VTEC line (in the bulletin)
							 * retrieve the list of zones for which the weather hazard is valid*/
							if  (aSetOfAwwFips != null &&  aSetOfAwwFips.size() > 0 ){
wqr.buildQueryPart(aSetOfAwwFips);	wstmRscDataObject.aListOfFipsInfoObjects=createListOfFipsInfoObjects2(aSetOfAwwFips);//T456
//wstmRscDataObject.aListOfFipsInfoObjects = new ArrayList<FipsInfo>(wstmRscDataObject.createListOfFipsInfoObjects(aSetOfAwwFips));
							}	

							/*If the phenomenon is not unknown, add the WstmRscDataObject to the list*/
							if(wstmRscDataObject.phenomenonType != PhenomenonType.UNKNOWN){
								wstmRscDataObjectList.add(wstmRscDataObject);
							}
							
				}
			}
		return wstmRscDataObjectList;
   }


/***
 * Contains the information about the weather hazard such as 
 * Its duration, the zones for which it is valid, the type of the phenomenon, 
 * the office that issued the bulletin, the action to be taken for this
 * event message and an event-tracking number. 
 * @author archana
 *
 */
	public class WstmRscDataObject implements IRscDataObject{

//          private Map<String, String> fipsCodeAndFipsStringMap = null;
          List<FipsInfo>  aListOfFipsInfoObjects = null;
          private DataTime eventTime = null;
          private DataTime startTime = null;
          private DataTime endTime = null;
          private boolean isStartTimeSetToIssueTime;
          private SimpleDateFormat TIME_FORMAT = new SimpleDateFormat("dd/HHmm");
          private String officeId;
          private String eventNumber;
  		  private ActionType actionType;
  		  private String validTimePeriod;
		  private Significance significance;
		  private ProductType productType;
		  private PhenomenonType phenomenonType;
        
		  /**
		   * Constructor
		   */
		  public WstmRscDataObject(){
       	       aListOfFipsInfoObjects = new ArrayList<FipsInfo>(0);
       	       this.TIME_FORMAT.setTimeZone( TimeZone.getTimeZone("GMT"));
      	}      
      	
		  /**
		   * Returns the enumeration matching the action specified in the VTEC line
		   * @param thisAction - the action to be matched
		   * @return the corresponding <code> ActionType </code> enumeration 
		   */
        private ActionType getActionTypeForThisAction(String thisAction){
        	ActionType thisActionType = ActionType.UNKNOWN;
        	if(thisAction.compareTo("CAN") == 0){
        		thisActionType = ActionType.CANCEL;
        	}
        	else	if(thisAction.compareTo("NEW") == 0 || thisAction.compareTo("EXA") == 0){
        		thisActionType = ActionType.NEW;
        	}
        	else	if(thisAction.compareTo("UPG") == 0){
        		thisActionType = ActionType.UPGRADE;
        	}
        	else	if(thisAction.compareTo("EXT") == 0 ){
        		thisActionType = ActionType.EXTEND_TIME_ONLY;
        	}

        	else	if(thisAction.compareTo("EXA") == 0){
        		thisActionType = ActionType.EXTEND_AREA_ONLY;
        	}

        	else	if(thisAction.compareTo("EXB") == 0){
        		thisActionType = ActionType.EXTEND_TIME_AND_AREA;
        	}
        	
        	else	if(thisAction.compareTo("EXP") == 0){
        		thisActionType = ActionType.EXPIRE;
        	}
        	
        	else	if(thisAction.compareTo("CON") == 0){
        		thisActionType = ActionType.CONTINUE;
        	}
        	else	if(thisAction.compareTo("COR") == 0){
        		thisActionType = ActionType.CORRECTION;
        	}
       	
        	return thisActionType;
        }
          
		  /**
		   * Returns the enumeration matching the product type specified in the VTEC line
		   * @param product - the product to be matched
		   * @return the corresponding <code> ProductType </code> enumeration 
		   */        
        private ProductType getVTECProductTypeForThisProduct(String product){
        	
        	ProductType wstmProductType = ProductType.UNKNOWN;
        	if (product.compareTo("O") == 0){
        		wstmProductType = ProductType.OPERATIONAL;
        	}
        	else if(product.compareTo("T") == 0){
        		wstmProductType = ProductType.TEST;
        	}
        	else if(product.compareTo("E") == 0){
        		wstmProductType = ProductType.EXPERIMENTAL;
        	}
        	else if(product.compareTo("X") == 0){
        		wstmProductType = ProductType.EXPERIMENTAL_VTEC_IN_OPERATIONAL_PRODUCT;
        	}
        	
        	return wstmProductType;
        }
        
        /**
         * Creates a list of FipsInfo objects by processing the UGC line of the bulletin
         * Each FipsInfo object contains information about the zone number, zone name and the centroid for that zone
         * @param aSetOfAwwFips - corresponding to the UGC line of the bulletin
         * @return a list of FipInfo objects
         */
        
      	private List<FipsInfo> createListOfFipsInfoObjects ( Set<AwwFips> aSetOfAwwFips ) {long t1=System.currentTimeMillis();
      		List<FipsInfo> thisListOfFipsInfo = new ArrayList<FipsInfo>(0);
      		
      		/*Create a map of the UGCs to their corresponding FIPS codes and zone names*/
      		Map<String,String> aMapOfUGCsFIPSCodesAndZoneNames = createAMapOfAwwFipsAndFipsStrings(aSetOfAwwFips);
      		
      		if (aMapOfUGCsFIPSCodesAndZoneNames != Collections.EMPTY_MAP) {
      			
				for (String eachFipsString : aMapOfUGCsFIPSCodesAndZoneNames.values()) {
			        
					FipsInfo fipsInfo = new FipsInfo();
					/*eachFipsString is of the form zoneNumber:zoneName*/
					/*separate the zone number from the zone name*/
					String  fipsCodeAndZoneName[] = eachFipsString.split(":");
					
					if (fipsCodeAndZoneName != null && fipsCodeAndZoneName.length == 2) {
					    
						/*Store the FIPS code and zone name in the newly created FipsInfo object*/
						fipsInfo.setFipsCode(fipsCodeAndZoneName[0]);
						fipsInfo.setZoneName(fipsCodeAndZoneName[1]);
						
						/*Create the query string to query the database for the centroid  corresponding to
						 * the FIPS number*/
						StringBuilder queryString = new StringBuilder("select ctrloc from ");
						queryString.append(BOUNDS_SCHEMA + "." + BOUNDS_TABLE);
						queryString.append(" where fips = '");
						queryString.append(fipsInfo.getFipsCode());
						queryString.append("'");
						queryString.append(";");
						try {
							/*Execute the query*/
							List<Object[]> results = DirectDbQuery
									.executeQuery(queryString.toString(),
											DATABASE, QueryLanguage.SQL);
							/*Extract the centroid from the query results*/
							LatLonPoint centroid = getCentroidFromDatabaseQueryResults(results);
							
							/*...and if it is not null, store it in the FipsInfo object*/
							if (centroid != null) {
								fipsInfo.setFipsCentroid(centroid);
							}
							
							/*...and add the FipsInfo object to a list*/
							thisListOfFipsInfo.add(fipsInfo);
						} catch (VizException e) {
							System.out
									.println("Error in WstmResource.createListOfFipsInfoObjects()"
											+ "Unable to retrieve records from the bounds table.\n"
											+ e.getMessage());
						}
					}
				}
			}
//long t2=System.currentTimeMillis();System.out.println("==================== createListOfFipsInfoObjects() t2-t1: "+(t2-t1));      		
      		/*return the list of newly created FipsInfo objects*/
			return thisListOfFipsInfo;
      	}      	

      	/***
      	 * Returns a string of the form dd1/HH1mm1 - dd2/HH2mm2, such that the
      	 * first string of the form dd/HHmm represents the start time of the event
      	 * and the last one represents its end time.
      	 * @param eventTime -  the <code> DataTime </code> whose start and
      	 * end times need to be formatted and represented as a <code>String</code>.  
      	 * @return a formatted string representing the valid time period of the event 
      	 */
    	private String getEventTimePeriodAsString(DataTime eventTime) {
    		StringBuilder builder = new StringBuilder(16); 
    		builder.append(" "); 
    		
    		if(eventTime != null){
    			builder.append(this.TIME_FORMAT.format(eventTime.getValidPeriod().getStart()));
        		builder.append("-");
        		builder.append(this.TIME_FORMAT.format(eventTime.getValidPeriod().getEnd()));
    		}

    		return builder.toString(); 
    }      	
      	
    	/**
    	 * Maps each FIPS code (<code>AwwFips</code>) to its corresponding zone number and zone name
    	 *
    	 * @param aSetOfAwwFips - the set of UGC codes from the UGC line in the AWW bulletin
    	 * @return a map of the input UGCs and their corresponding zone numbers and names
    	 * The key in the Map is a <code>String</code> representing the UGC and the value is a <code>String</code> of the form
    	 * zoneNumber:zoneName
    	*/
      	private Map<String, String>  createAMapOfAwwFipsAndFipsStrings(Set<AwwFips> aSetOfAwwFips){
      		Map<String, String> ugcFIPSMap = null;
      		if(aSetOfAwwFips != null && aSetOfAwwFips.size() > 0){
      			ugcFIPSMap = new HashMap<String, String>(0);
      			
      			/*For each AwwFips... */
      			for(AwwFips eachAwwFips : aSetOfAwwFips){
      				
      				/*retrieve its Universal Geographic code - of the for SSZXXX
      				 * where SS - 2 letter state code
      				 * Z - refers to the word zone
      				 * XXX - 3 digit zone identifier
      				 * */      		
      				String thisUGC = new String (eachAwwFips.getFips());
//System.out.println("________________________________ ugc: "+thisUGC);           
      				/*and use this UGC to retrieve the FIPS code and zone name from the stations table*/
      				String fipsCodeAndZoneName = new String (getZoneNumberAndNameFromZonesTable(thisUGC));
      				
      				/* put the input UGC and its corresponding FIPS code and name into the HashMap*/
      				ugcFIPSMap.put( thisUGC, new String(fipsCodeAndZoneName));
      			}
      			
      			/*and return the HashMap*/
      			return ugcFIPSMap;

      		}else{
      			return Collections.EMPTY_MAP;
      		}

      	}        

      	/**
      	 * Gets the zone number and name corresponding to the input FIPS code from the zones table 
      	 * @param fipsZone
      	 * @return The zone number and name corresponding to the input fips code
      	 */
      	private String getZoneNumberAndNameFromZonesTable(String fipsZone){
      		
      		String zoneNumberAndName = "";
      		
      		/*Create the query string to retrieve the station number and name for the input station id (UGC)*/
      		StringBuilder queryString = new StringBuilder();
      		queryString.append("select stnum, name from "+ STATIONS_SCHEMA + "." + ZONES_TABLE + " where " + "stid = '");
      		queryString.append(fipsZone);
      		queryString.append("'");
      		queryString.append(";");
      		try {
      			/*execute the query*/
      			List<Object[]>  results = DirectDbQuery.executeQuery(queryString.toString(), 
      					DATABASE, QueryLanguage.SQL);
      			if(results != null && results.size() > 0){
      			Object[] objArr = results.get(0);
      			   /*From the results of the query, retrieve the station number and name*/
      			    Integer thisInt = (Integer)objArr[0];
      			    String zoneName = (String)objArr[1];
      			    /*concatenate the station number and name along with a ':' character */
      			    zoneNumberAndName = new String( thisInt.toString() + ":" + zoneName);
      			}
      		} catch (VizException e) {
      		    System.out.println("Error while retrieving the state number from the stns.zones table:\n " + e.getMessage());
      		}    	 
      		return zoneNumberAndName;
      	}    	
 
  	
    	/**
    	 * Parses the database query results to extract the centroid
    	 * @param results
    	 * @return a LatLonPoint object representing the centroid
    	*/
    	private LatLonPoint getCentroidFromDatabaseQueryResults(List<Object[]> results){
    		LatLonPoint centroid = null;
    		if(results != null && results.size() > 0){
    			Object[]  centroidArr = results.get(0);
    			if(centroidArr  != null ){

    				String centroidAsStr = (String) centroidArr[0]; 
    				String[] substring = centroidAsStr.split(",");

    				if(substring.length == 2){
    					String latStr = new String(substring[0].replace('(', ' ').trim());
    					String lonStr = new String(substring[1].replace(')', ' ').trim());
    					Double lat   =   Double.parseDouble(latStr);
    					Double lon =  Double.parseDouble(lonStr);
    					centroid = new LatLonPoint(lat, lon, LatLonPoint.INDEGREES);
    				}
    			}  			
    		}
         	return centroid;
    	}      	
 
        @Override
		/**
		 * Overridden method used to time-match the resource to the frame
		 */
        public DataTime getDataTime() {
			  return this.eventTime;
		}
        
        /***
         * Returns the <code>Significance</code> enumeration for the input
         * one character length significance code from the VTEC line 
         * @param significance - the single character code for the significance
         * @return the equivalent enumeration for the Significance
         */
		private Significance getSignificanceType(String significance){
			Significance wstmSignificanceType = Significance.UNKNOWN;
			if ( significance.compareTo("A"  ) == 0 ){
				wstmSignificanceType = Significance.WATCH;
			}
			else if ( significance.compareTo("W"  ) == 0){
				wstmSignificanceType = Significance.WARNING;
			} 
			else if ( significance.compareTo("Y"  ) == 0){
				wstmSignificanceType = Significance.ADVISORY;
			}
			else if ( significance.compareTo("S"  ) == 0){
				wstmSignificanceType = Significance.STATEMENT;
			} 
			else if ( significance.compareTo("F"  ) == 0){
				wstmSignificanceType = Significance.FORECAST;
			}
			else if ( significance.compareTo("N"  ) == 0){
				wstmSignificanceType = Significance.SYNOPSIS;
			} 
			else if ( significance.compareTo("O"  ) == 0){
				wstmSignificanceType = Significance.OUTLOOK;
			}			
			return wstmSignificanceType;
		}

		/***
		 * Returns the PhenomenonType enumeration for input the 2-character
		 * phenomenon code from the VTEC line.
		 * @param phenomenon - the 2-character phenomenon code
		 * @return the corresponding PhenomenonType enumeration
		 */
		private PhenomenonType getPhenomenonType(String phenomenon){
			PhenomenonType wstmPhenomenonType = PhenomenonType.UNKNOWN;
			if ( phenomenon.compareTo("BZ"  ) == 0 ){
				wstmPhenomenonType = PhenomenonType.BLIZZARD;
			}
			else if ( phenomenon.compareTo("WS"  ) == 0){
				wstmPhenomenonType = PhenomenonType.WINTER_STORM;
			} 
			else if ( phenomenon.compareTo("WW"  ) == 0){
				wstmPhenomenonType = PhenomenonType.WINTER_WEATHER;
			} 
			else if ( phenomenon.compareTo("SN"  ) == 0){
				wstmPhenomenonType = PhenomenonType.SNOW;
			}
			else if ( phenomenon.compareTo("HS"  ) == 0){
				wstmPhenomenonType = PhenomenonType.HEAVY_SNOW;
			} 
			else if ( phenomenon.compareTo("LE"  ) == 0){
				wstmPhenomenonType = PhenomenonType.LAKE_EFFECT_SNOW;
			}			
			else if ( phenomenon.compareTo("BS"  ) == 0){
				wstmPhenomenonType = PhenomenonType.BLOWING_SNOW;
			}
			else if ( phenomenon.compareTo("SB"  ) == 0){
				wstmPhenomenonType = PhenomenonType.SNOW_AND_BLOWING_SNOW;
			}
			else if ( phenomenon.compareTo("LB"  ) == 0){
				wstmPhenomenonType = PhenomenonType.LAKE_EFFECT_SNOW_AND_BLOWING_SNOW;
			}
			else if ( phenomenon.compareTo("IP"  ) == 0){
				wstmPhenomenonType = PhenomenonType.SLEET;
			}
			else if ( phenomenon.compareTo("ZR"  ) == 0){
				wstmPhenomenonType = PhenomenonType.FREEZING_RAIN;
			}
			else if ( phenomenon.compareTo("IS"  ) == 0){
				wstmPhenomenonType = PhenomenonType.ICE_STORM;
			}
			else if ( phenomenon.compareTo("WC"  ) == 0){
				wstmPhenomenonType = PhenomenonType.WIND_CHILL;
			}
			return wstmPhenomenonType;
		}		
		
		/**
		 * @param action the action to set
		 */
		/**
		 * @return the phenomenon
		 */
		public Significance getPhenomenon() {
			return significance;
		}
		/**
		 * @param phenomenon the phenomenon to set
		 */
		public void setPhenomenon(Significance phenomenon) {
			this.significance = phenomenon;
		}

	}
	
	private class FipsInfo{
		
		/**
		 * @return the fipsName
		 */
		public String getFipsCode() {
			return fipsNumber;
		}
		/**
		 * @param fipsName the fipsName to set
		 */
		public void setFipsCode(String fipsName) {
			this.fipsNumber = fipsName;
		}

		/**
		 * @return the fipsCentroid
		 */
		public LatLonPoint getFipsCentroid() {
			return fipsCentroid;
		}
		/**
		 * @param fipsCentroid the fipsCentroid to set
		 */
		public void setFipsCentroid(LatLonPoint fipsCentroid) {
			this.fipsCentroid = fipsCentroid;
		}
		
		/**
		 * @param zoneName the zoneName to set
		 */
		private void setZoneName(String zoneName) {
			this.zoneName = zoneName;
		}
		/**
		 * @return the zoneName
		 */
		private String getZoneName() {
			return zoneName;
		}
		private String zoneName;
		private String fipsNumber;
		private LatLonPoint fipsCentroid;
	}
	
	
	private class FrameData extends AbstractFrameData{
    private Map<String, WstmRscDataObject> wstmDataMap = null;
		/**
		 * Overloaded Constructor
		 * @param ftime
		 * @param frameInterval
		 */
		protected FrameData(DataTime ftime, int frameInterval) {
			super(ftime, frameInterval);
			wstmDataMap = new HashMap<String, WstmRscDataObject>(0);
		}

	 @Override
		/**
		 * Updates the <code> Map of WstmRscDataObject </code> in each frame, based on the action type
		 * of the incoming <code> WstmRscDataObject </code>
		 */
		public boolean updateFrameData(IRscDataObject rscDataObj) {
              if(!( rscDataObj instanceof WstmRscDataObject)){
            	  System.out.println("Error: rscDataObj belongs to class" + rscDataObj.getClass().getCanonicalName());
            	  return false;
              }
              WstmRscDataObject thisWstmRscDataObject = (WstmRscDataObject) rscDataObj;
              String key = thisWstmRscDataObject.officeId +"." 
                                   + thisWstmRscDataObject.eventNumber + "."
                                   + thisWstmRscDataObject.phenomenonType + "."
                                   + thisWstmRscDataObject.significance ;
             
              if (thisWstmRscDataObject.actionType == ActionType.CANCEL) {
				if (!wstmDataMap.isEmpty()) {
					wstmDataMap.remove(key);
				}
			}
          
              else if ( thisWstmRscDataObject.actionType == ActionType.NEW ){
//            	 System.out.println("Action for " + key +" " + thisWstmRscDataObject.actionType); 
             	 wstmDataMap.put(key, thisWstmRscDataObject);
              }
             
             else if(thisWstmRscDataObject.actionType == ActionType.CONTINUE
            		 || thisWstmRscDataObject.actionType == ActionType.UPGRADE
            		 || thisWstmRscDataObject.actionType == ActionType.CORRECTION){
            	 if( ! wstmDataMap.containsKey(key)){
            		 wstmDataMap.put(key,thisWstmRscDataObject);
            	 }
            	 else{
            		     WstmRscDataObject objToChange = wstmDataMap.get(key);
            		     objToChange.actionType = thisWstmRscDataObject.actionType;
            		     if(! thisWstmRscDataObject.isStartTimeSetToIssueTime){
            		    	 objToChange.eventTime = new DataTime(thisWstmRscDataObject.eventTime.getRefTimeAsCalendar(),
            		    			                                                                  thisWstmRscDataObject.eventTime.getValidPeriod());
            		    	 objToChange.startTime = new DataTime( thisWstmRscDataObject.startTime.getRefTimeAsCalendar());
            		     }
        		    	 objToChange.endTime = new DataTime(thisWstmRscDataObject.endTime.getRefTimeAsCalendar());
        		    	 objToChange.validTimePeriod =  new String(thisWstmRscDataObject.validTimePeriod);
            		     wstmDataMap.put(key,objToChange);
            	 }
             }
         
             else   if(thisWstmRscDataObject.actionType == ActionType.EXPIRE ){
            	 if(wstmDataMap.containsKey(key)){
            		            WstmRscDataObject objToChange = wstmDataMap.get(key);
            		            if(thisWstmRscDataObject.endTime != null)
            		            {
            		            	objToChange.endTime = new DataTime(thisWstmRscDataObject.endTime.getRefTimeAsCalendar());
            		            }
            		            objToChange.eventTime = new DataTime(objToChange.startTime.getRefTimeAsCalendar(),
                                        new TimeRange(objToChange.startTime.getValidPeriod().getStart(),
                                                         objToChange.endTime.getRefTime()));     
            		            
            		           objToChange.actionType = thisWstmRscDataObject.actionType;
            		           objToChange.validTimePeriod = new String(objToChange.getEventTimePeriodAsString(objToChange.eventTime)); 
 //           		           System.out.println("Action match for " + key + " " +  objToChange.actionType);
                                  wstmDataMap.put(key, objToChange);
                     }             	 
             }
             
             else if( thisWstmRscDataObject.actionType == ActionType.EXTEND_TIME_ONLY
            		 ||  thisWstmRscDataObject.actionType == ActionType.EXTEND_TIME_AND_AREA){
            	 if(wstmDataMap.containsKey(key)){
 //           		  System.out.println("Action match for " + key + " " +  thisWstmRscDataObject.actionType);
  		            WstmRscDataObject objToChange = wstmDataMap.get(key);
 		            objToChange.actionType = thisWstmRscDataObject.actionType;  		            
	            	objToChange.endTime = new DataTime(thisWstmRscDataObject.endTime.getRefTimeAsCalendar());
	            	if (! thisWstmRscDataObject.isStartTimeSetToIssueTime){
	       		    	objToChange.startTime = new DataTime( thisWstmRscDataObject.startTime.getRefTimeAsCalendar());
	            		objToChange.eventTime = new DataTime(thisWstmRscDataObject.eventTime.getRefTimeAsCalendar(),
                                                                       new TimeRange(thisWstmRscDataObject.eventTime.getValidPeriod().getStart(),
                                                                                objToChange.endTime.getRefTime()));  	            		

	            	}
	            	else{
    		            objToChange.eventTime = new DataTime(objToChange.eventTime.getRefTimeAsCalendar(),
                                                                       new TimeRange(objToChange.eventTime.getValidPeriod().getStart(),
                                                                                                    objToChange.endTime.getRefTime()));  	   	            		
	            	}
 		            wstmDataMap.put(key, objToChange);
            	 }

             }
             
             else if( thisWstmRscDataObject.actionType == ActionType.EXTEND_AREA_ONLY
            		 ||  thisWstmRscDataObject.actionType == ActionType.EXTEND_TIME_AND_AREA){
            	 if(wstmDataMap.containsKey(key)){
 //          		  System.out.println("Action match for " + key + " " +  thisWstmRscDataObject.actionType);
		            WstmRscDataObject objToChange = wstmDataMap.get(key);
		            objToChange.actionType = thisWstmRscDataObject.actionType;  	
		            objToChange.aListOfFipsInfoObjects.addAll(thisWstmRscDataObject.aListOfFipsInfoObjects);
		            wstmDataMap.put(key, objToChange);
            	 }
             }
           
             
					return true;
		}
		
	}
	
	/***
	 * Private class to capture the attributes (such as the color, line width, symbol width etc) of each event 
	 * from the WstmResourceData class.
	 *  
	 * @author archana
	 *
	 * @param <Boolean> - flag to check if the model is enabled
	 * @param <RGB> - the color of the model
	 * @param <Integer> - the width of the outline (line width)
	 * @param <Integer> - the width of the the symbol 
	 * @param <Float> - the size of the symbol
	 *
	*/
	private class WstmResourceAttributes<RGB, Integer, Float, Boolean>{
		Boolean eventEnable;
		Integer    symbolWidth;
		Float symbolSize;
		Integer   lineWidth;
		RGB colorOfEvent;

		public WstmResourceAttributes(Boolean evEnable, RGB eventColor, Integer lineWidth, Integer symbolWidth, 
				Float symbolSize){
			this.colorOfEvent = eventColor;
			this.symbolSize = symbolSize;
			this.symbolWidth = symbolWidth;
			this.lineWidth = lineWidth;
			this.eventEnable = evEnable;
}

		/**
		 * @return the eventEnable
		*/
		 private Boolean getEventEnable() {
			 return eventEnable;
}

		 /**
		  * @param EventEnable the eventEnable to set
		 */
		 private void setEventEnable(Boolean EventEnable) {
			 this.eventEnable = EventEnable;
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
		  * @return the colorOfEvent
		 */
		 private RGB getColorOfEvent() {
			 return colorOfEvent;
}

		 /**
		  * @param ColorOfEvent the colorOfEvent to set
		 */
		 private void setColorOfEvent(RGB ColorOfEvent) {
			 this.colorOfEvent = ColorOfEvent;
}
}
	

	
	
//---------------------------------------------------------------T456:
	
	
	WstmQueryResult wqr = new WstmQueryResult();
	
	//for storing result of pre-calculation
	private IWireframeShape outlineShape;
	
	//for pre-calculate the IWiredframeShape
	private ZoneResultJob zrJob = new ZoneResultJob("");
	
	//if it is 1st round in the loop then draw outline since it pre-calculated for all zones
	private boolean isFirstRound = true;
	
	//Area change flag
	private boolean areaChangeFlag = false;
	
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

		for (Object pdo : pdoList) {
			for( IRscDataObject dataObject : processRecord( pdo ) )	{	
				newRscDataObjsQueue.add(dataObject);
			}
		}
		
		wqr.populateFipsMap();
		setAllFramesAsPopulated();
	}
	
	private List<FipsInfo> createListOfFipsInfoObjects2 ( Set<AwwFips> aSetOfAwwFips ) {
		
		List<FipsInfo> thisListOfFipsInfo = new ArrayList<FipsInfo>();
		
		for(AwwFips af : aSetOfAwwFips){
			FipsInfo fips = new FipsInfo();
			fips.fipsNumber = af.getFips();
			
			thisListOfFipsInfo.add(fips);
		}
		
		
		return thisListOfFipsInfo;
	}
	
    /**
     * handles the IWireframeShape pre-calculation
     * 
     * @author gzhang     
     */
    private class ZoneResultJob extends org.eclipse.core.runtime.jobs.Job {
    	
    	private Map<String,Result> keyResultMap = new java.util.concurrent.ConcurrentHashMap<String,Result>();
    	
    	private IGraphicsTarget target;
    	private IMapDescriptor descriptor;
    	private RGB symbolColor = new RGB (155, 155, 155);
    	
        public class Result {
        	
            public IWireframeShape outlineShape;            
            public Map<Object, RGB> colorMap;

            private Result(IWireframeShape outlineShape,IWireframeShape nuShape,
                     			IShadedShape shadedShape,Map<Object, RGB> colorMap){
            	
            	this.outlineShape = outlineShape;
                
                this.colorMap = colorMap;
            }
        }
    	
    	public ZoneResultJob(String name) {
			super(name);			
		}
    	
		public void setRequest(IGraphicsTarget target, IMapDescriptor descriptor,
        		String query, boolean labeled, boolean shaded, Map<Object, RGB> colorMap){
			
			this.target = target;
			this.descriptor = descriptor;					
			this.run(null);//this.schedule();
			
    	}
    	
    	
    	@Override
		protected org.eclipse.core.runtime.IStatus run(org.eclipse.core.runtime.IProgressMonitor monitor){
    		
    		List<Object[]> results;
    		
    		for(AbstractFrameData afd : frameDataMap.values())	{
    			
    			FrameData fd = (FrameData)afd;
    			
    			for( WstmRscDataObject wrdo : fd.wstmDataMap.values()){    				
    				
    				for( FipsInfo fi : wrdo.aListOfFipsInfoObjects){
    					
    					Collection<Geometry> gw = new ArrayList<Geometry>();
    					
    					for(ArrayList<Object[]> zones : wqr.getZoneResult(fi.fipsNumber)){
    						
    						if( zones == null ) continue;
    						
    						WKBReader wkbReader = new WKBReader();
    						
    						for (Object[] result : zones) {
    							
    							int k = 0;
								byte[] wkb1 = (byte[]) result[k];
								
								com.vividsolutions.jts.geom.MultiPolygon countyGeo = null;
								
								try{
									
									countyGeo= (com.vividsolutions.jts.geom.MultiPolygon)wkbReader.read(wkb1);
									
									if ( countyGeo != null && countyGeo.isValid() && ( ! countyGeo.isEmpty())){
										gw.add(countyGeo);
									}
									
								}catch(Exception e){
									System.out.println("Exception in run(),ZoneResultJob: "+e.getMessage());
								}
    						}    						
    					}
    					
    					if(gw.size() == 0)
    						continue;
    					else
    						keyResultMap.put(fi.fipsNumber, new Result(getEachWrdoShape(gw),null,null,null));
    				}
    				
    				
    				
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
											
			}catch (Exception e) {	System.out.println("_____Exception in getEachWrdoShape(), ZoneResultJob : "+e.getMessage());	}
	    	
	    	return newOutlineShape;
	    }
    }
    
    public String getKey(WstmRscDataObject thisWstmRscDataObject){
    	
    	return thisWstmRscDataObject.officeId +"." 
                             + thisWstmRscDataObject.eventNumber + "."
                             + thisWstmRscDataObject.phenomenonType + "."
                             + thisWstmRscDataObject.significance ;
    }
    
    
    private void drawOutlineForZone2( String fipsCode, IGraphicsTarget target,RGB lineColor,int lineWidth) throws VizException{
    	
    	ZoneResultJob.Result result = zrJob.keyResultMap.get(fipsCode);
    	
    	if (result != null) {
    		if (outlineShape == null) {   
    			outlineShape = result.outlineShape;   
    		}else{									 
		//if ( outlineShape.hashCode() != result.outlineShape.hashCode()) { //TODO: do NOT use outlineShape.hashCode !!!   
			//outlineShape.dispose(); 
    			outlineShape = result.outlineShape;
		//}
    		}    
    	}else {
    		return;
    	}
    	
    	if (outlineShape != null && outlineShape.isDrawable() ){
    		try{
    			target.drawWireframeShape(outlineShape,  lineColor,lineWidth,LineStyle.SOLID );
    		} catch (VizException e) {
    			System.out.println("Exception in drawCountyOutline2(), WstmResource"+e.getMessage()); 
    			//e.printStackTrace();
    		}

    	} else if (outlineShape == null){
		
  		//target.setNeedsRefresh(true);
    	}
    }
    
    @Override
	protected boolean postProcessFrameUpdate() {
    	
    	AbstractEditor ncme = NcDisplayMngr.getActiveNatlCntrsEditor();
    	
    	zrJob.setRequest(ncme.getActiveDisplayPane().getTarget(), getNcMapDescriptor(), null, false, false, null); 
    	
    	return true;
    }
    
	/**
	 *  called in the constructor.
	 */
	private void addRDChangedListener(){
		AbstractEditor editor = NcDisplayMngr.getActiveNatlCntrsEditor();
		editor.addRenderableDisplayChangedListener(this.new WstmDCListener());
	}
    
    /**
	 * change the flag so outlineShape can be re-calculated
	 */
	private class WstmDCListener implements com.raytheon.uf.viz.core.IRenderableDisplayChangedListener{

		@Override
		public void renderableDisplayChanged(com.raytheon.uf.viz.core.IDisplayPane pane,
				IRenderableDisplay newRenderableDisplay, DisplayChangeType type) {
			
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
		if (fd == null || fd.getFrameTime() == null || fd.wstmDataMap.size() == 0) {
			return legendString + "-No Data";
		}
		return legendString + " "+ NmapCommon.getTimeStringFromDataTime( fd.getFrameTime(), "/");
	}
}





















