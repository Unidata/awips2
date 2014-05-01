/*
 * IntlSigmetResource
 * 
 * Date created 14 April 2010
 * 
 * This code has been developed by the SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.viz.rsc.intlsig.rsc;



import gov.noaa.nws.ncep.common.dataplugin.intlsigmet.IntlSigmetLocation;
import gov.noaa.nws.ncep.common.dataplugin.intlsigmet.IntlSigmetRecord;
import gov.noaa.nws.ncep.common.tools.IDecoderConstantsN;
import gov.noaa.nws.ncep.ui.pgen.display.DisplayElementFactory;
import gov.noaa.nws.ncep.ui.pgen.display.IDisplayable;
import gov.noaa.nws.ncep.ui.pgen.display.IText;
import gov.noaa.nws.ncep.ui.pgen.display.IText.DisplayType;
import gov.noaa.nws.ncep.ui.pgen.display.IText.FontStyle;
import gov.noaa.nws.ncep.ui.pgen.display.IText.TextJustification;
import gov.noaa.nws.ncep.ui.pgen.display.IText.TextRotation;
import gov.noaa.nws.ncep.ui.pgen.elements.Symbol;
import gov.noaa.nws.ncep.ui.pgen.elements.Text;
import gov.noaa.nws.ncep.ui.pgen.sigmet.SigmetInfo;
import gov.noaa.nws.ncep.viz.common.dbQuery.NcDirectDbQuery;
import gov.noaa.nws.ncep.viz.common.ui.NmapCommon;
import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsResource;
import gov.noaa.nws.ncep.viz.resources.INatlCntrsResource;
import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsResource.AbstractFrameData;
import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsResource.IRscDataObject;
import gov.noaa.nws.ncep.viz.ui.display.NCMapDescriptor;

import java.awt.Color;
import java.awt.geom.Rectangle2D;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.Set;
import java.util.TimeZone;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.geospatial.util.WorldWrapChecker;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.edex.decodertools.core.LatLonPoint;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.TextStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery.QueryLanguage;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.geom.PixelCoordinate;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.viz.core.rsc.jts.JTSCompiler;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.Point;
import com.vividsolutions.jts.geom.Polygon;
;
/**
/**
 * IntlSigmetResource - Creates an International SIGMET resource
 *
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 14-Apr-2010    244      Archana     Initial creation.
 * 31-May-2010  migration  B. Hebbard  Updated missing-value constants for TO11DR11 (per Archana)
 * 14-Jun-2010     244    Archana      Added/updated logic to decide when drawPolygonSurroundingLine()
 *                                     is invoked. Added and updated the logic to render a polygon along 
 *                                     a given direction of a line or on both sides of a line.
 * 17-Jun-2010     244     Archana     Design update - moved the time-matching logic to FrameData class 
 *                                     Updated the constants used to denote the invalid Lat/Lon values
 * 30-Sep-2010     307     Greg Hull   renamed CondensedIntlSigmetRecord to IntlSigmetRscDataObj and
 *                                     implemented the IRscDataObj interface                                                                                                                 
 * 04/11			?	   B. Yin	   Re-factor IAttribute
 * 05/23/12       785      Q. Zhou     Added getName for legend.
 * 06/25/2012     758      S. Gurung   Added new method drawPolygon(...) to clip polygons that span the edges (in world view).
 *								       Added code to display the latest SIGMET currently in effect, if there are 
 *									   2 or more SIGMETs with same messageID valid at the same time frame.
 * 06/27/2012     830      S. Gurung   Added fix for the issue: changes in Edit Attributes Dialog not functioning properly. 
 * 08/17/2012     655      B. Hebbard  Added paintProps as parameter to IDisplayable draw
 * 10/25/2012              B. Hebbard  [per S. Gurung] Replace 3 WorldWrapChecker.getInverseCentralMeridian() calls,
 *                                     due to split by RTS in OB12.11.1 into get{High|Low}InverseCentralMeridian()
 * </pre>
 * 
 * @author Archana
 * @version 1.0
 */
@SuppressWarnings("unused")
public class IntlSigmetResource 
extends AbstractNatlCntrsResource<IntlSigmetResourceData, NCMapDescriptor>
implements INatlCntrsResource{

	private IntlSigmetResourceData   intlSigmetResourceDataObj;
	private final double METRE_TO_NM_CONVERSION_FACTOR = 1852.0;   
    private IFont font=null;
    float  baseFontSize = 14;
    Rectangle2D charSize;
    double charHeight;
    double charWidth;
    
	private ArrayList<IntlSigmetRecord> intlsigmetCancelList = new ArrayList<IntlSigmetRecord>();

	private enum WeatherHazardType{     
		TS,           // Thunder Storm
		TB,           // TurBulence
		HU,           // HUrricane
		TR,           // Tropical Storm
		TD,           // Tropical Depression
		VA,           // Volcanic Ash
		MW,           // Mountain Wave
		TC,           // Tropical Cyclone
		SQ,           // SQuall Line
		CAT,          // Clear Air Turbulence
		IC,           // ICing
		GR,           // Hail 
		DS,           // Dust Storm
		SS,           // Sand Storm
		CB,           // CumulonimBus
		WS,           // WindShear
		TDO,          // TornaDO   
		FC,           // Funnel Cloud
		WTSPT,        // WaTer SPouT
		HVY_GR,       // HeaVY hail
		HVY_SS,       // HeaVY Sand Storm   	 
		OBSC_TS,      // Obscured Thunder Storm         
		EMBD_TS,      // Embedded Thunder Storm
		WDSPR_TS,     // Widespread Thunder Storm
		SQL_TS,       // Squall Line Thunder Storm
		ISOL_SEV_TS,  // Isolated Severe Thunder Storm
		ISOL_TS,      // Isolated Severe Thunder Storm
		FRQ_TS,       // Frequent Thunderstorm 
		SEV_TURB,     // Severe Turbulence
		SEV_ICE,      // Severe Icing
		SEV_ICE_FRZA, // Severe Icing with FreeZing RAin
		WDSPR_DS,     // WiDe SPRead Dust Storm
		WDSPR_SS,     // WiDe SPRead Sand Storm
		WIND,
		FZRA,
		TEST,
		CANCEL,
		OTHER,
		UNKNOWN, 
		NIL 
	}
    
	/**
	 * Overloaded constructor 
	 * @param resourceData
	 * @param props
	 */
	public IntlSigmetResource(IntlSigmetResourceData resourceData,
			LoadProperties props) {
		super(resourceData, props);
		
		intlSigmetResourceDataObj     = (IntlSigmetResourceData) resourceData;
//		condensedIntligLinkedHashMap  = new HashMap<String, CondensedIntlSigmetRecord>(0);
		intlsigmetCancelList          = new ArrayList<IntlSigmetRecord>();
	}
	
	/***
	 * Overridden method to create a new frame containing International SIGMETs  
	 */
   @Override
    protected AbstractFrameData createNewFrame( DataTime frameTime, int timeInt ) {
    	return (AbstractFrameData) new FrameData( frameTime, timeInt );
    } 
   
   @Override
	protected boolean postProcessFrameUpdate() {
   	
	   // for each frame...
   	
	   	for (AbstractFrameData afd : frameDataMap.values()) {
	   		FrameData fd = (FrameData) afd;
	   		
	   		// ...go through all the data time matched to this frame
	   		// to determine, for every messageid, the latest issuance
	   		// of SIGMETs for that messageid
	   		
	   		Map<String,DataTime> latestSigmetIssuanceTimeForMessageId = new HashMap<String,DataTime>();
	   		
	   		for (IntlSigmetRscDataObj isigRDO : fd.condensedIntligLinkedHashMap.values()) {
	   			String sequenceNumber = isigRDO.sequenceNumber.trim();
	   			if (sequenceNumber != null && !sequenceNumber.isEmpty()) {
	   				String messageID = isigRDO.messageID.trim();
	   				// null International SIGMET still counts as 'issuance'
					DataTime latestSigmetIssuanceForThisMessageID =
						latestSigmetIssuanceTimeForMessageId.get(messageID);
					if (latestSigmetIssuanceForThisMessageID == null ||
							isigRDO.issueTime.greaterThan(latestSigmetIssuanceForThisMessageID)) {
						latestSigmetIssuanceTimeForMessageId.put(messageID, isigRDO.issueTime);
					}
	   					
	   			}
	   		}
	   		
	   		//latestSigmetIssuanceTimeForMessageId = latestSigmetIssuanceTimeForMessageId;
	   		// Now that we've determined the latest issuances for each messageId -- we make a second
	   		// pass through the data time matched to this frame.  This time,
	   		// we purge anything superseded by a later issuance.
	   		
	   		String[] keys = new String[1];
	   		keys = fd.condensedIntligLinkedHashMap.keySet().toArray(keys);
	   		for (String key : keys) {
	   			IntlSigmetRscDataObj isigRDO = fd.condensedIntligLinkedHashMap.get(key);
	   			String sequenceNumber = (isigRDO == null) ? null : isigRDO.sequenceNumber;
	   			if (sequenceNumber != null && !sequenceNumber.isEmpty()) {
	   				String messageID = isigRDO.messageID.trim();
	   				
					DataTime latestSigmetIssuanceForThisMessageID =
						latestSigmetIssuanceTimeForMessageId.get(messageID);
					if (latestSigmetIssuanceForThisMessageID != null &&
							latestSigmetIssuanceForThisMessageID.greaterThan(isigRDO.issueTime)) {
						fd.condensedIntligLinkedHashMap.remove(key);
					}
	   			}
	   		}
	   		
	   	}
   
		return true;
	}
    
   /***
    * Overridden method to render the International SIGMETs in each frame.
    * 
    * @param AbstractFrameData
    * @param IGraphicsTarget
    * @param PaintProperties
    */
	@Override
	public void paintFrame(AbstractFrameData frameData,
			IGraphicsTarget graphicsTarget, PaintProperties paintProps)
			throws VizException {
		if(font == null){
			font = graphicsTarget.initializeFont("Monospace", baseFontSize, new IFont.Style[] { IFont.Style.BOLD });
		}
		charSize              = graphicsTarget.getStringBounds(font, "N");
		charHeight         = charSize.getHeight();
		charWidth          = charSize.getWidth();
		/*  
		 *Calculate vertical/horizontal offset parameters 
		 */
		double screenToWorldRatio = paintProps.getCanvasBounds().width /paintProps.getView().getExtent().getWidth(); 
		
		double offsetY            = charHeight / screenToWorldRatio;
		double offsetX            = charWidth / screenToWorldRatio; 


		if( paintProps == null ) {
			return;
		}

	       if(frameData != null){
	    	   FrameData currFrameData = (FrameData) frameData;
	    	   DataTime activeFrameTime = currFrameData.getFrameTime();
               Collection<IntlSigmetRscDataObj> condensedISIGCollection= currFrameData.condensedIntligLinkedHashMap.values();
                            
	    	   for(IntlSigmetRscDataObj condensedISIG : condensedISIGCollection){
	    		   
	    		   /*  Check for invalid time range*/
	    			
//	    		    if (activeFrameTime.compareTo(condensedISIG.startTime) < 0 ||
//	    	            activeFrameTime.compareTo(condensedISIG.endTime) > 0) continue;

					/*Initialize defaults*/
	    		    SymbolAttributesSubSet<RGB, Integer, Float, Boolean, String> firstSymbolAttributes;
			    	PixelCoordinate firstVertex  = null;
			    	PixelCoordinate lastVertex   = null;
					PixelCoordinate textLocation = null;					
					RGB polygonLineColor         = new RGB(255,255, 255);
					int polygonLineWidth         = 2;
					LineStyle lineStyle          = LineStyle.SOLID;	    		    
					List<String> labelList       = new ArrayList<String>(0);
					ArrayList<RGB> textRGBArray  = new ArrayList<RGB>(0);
					Coordinate tempSymbolLocationWorldCoord = null;
					ArrayList<WeatherHazardType> weatherHarzardList = condensedISIG.getWeatherHazardEnumList();
					
					boolean enabled = false;		    		   
		    		int weatherHarzardListSize = (weatherHarzardList!=null)? weatherHarzardList.size():0;
		    		
					for (int i=0; i<weatherHarzardListSize; i++) {
					
						switch( weatherHarzardList.get(i) ) {
							case TS:
								enabled     = intlSigmetResourceDataObj.getThunderstormEnable();
								break;
							case TB:
								enabled     = intlSigmetResourceDataObj.getTurbulenceEnable();
								break;
							case HU:
								enabled     = intlSigmetResourceDataObj.getHurricaneEnable();
								break;
							case TR:
								enabled     = intlSigmetResourceDataObj.getTropicalStormEnable();
								break;
							case TD:
								enabled     = intlSigmetResourceDataObj.getTropicalDepressionEnable();
								break;
							case VA:
								enabled     = intlSigmetResourceDataObj.getVolcanicAshCloudEnable();
								break;
							case MW:
								enabled     = intlSigmetResourceDataObj.getMountainWaveEnable();
								break;
							case TC:
								enabled     = intlSigmetResourceDataObj.getTropicalCycloneEnable();
								break;
							case SQ:
								enabled     = intlSigmetResourceDataObj.getSquallLineEnable();
								break;
							case CAT:
								enabled     = intlSigmetResourceDataObj.getCatEnable();
								break;
							case IC:
								enabled     = intlSigmetResourceDataObj.getIcingEnable();
								break;
							case GR:
								enabled     = intlSigmetResourceDataObj.getHailEnable();
							break;
							case DS:
								enabled     = intlSigmetResourceDataObj.getDustStormEnable();
								break;
							case SS:
								enabled     = intlSigmetResourceDataObj.getSandStormEnable();
								break;
							case CB:
								enabled     = intlSigmetResourceDataObj.getCumulonimbusEnable();
								break;
							case WS:
								enabled     = intlSigmetResourceDataObj.getLowLevelWindShearEnable();
								break;
							default:
						}

					}
					
						if (enabled) {
						
		    			/* Retrieve the symbol specific attributes such as symbol size, width, pattern to be used etc, for all the
						 * weather hazards in the current international sigmet
						 **/
						condensedISIG.symbolAttributeSubsetList   = condensedISIG.generateListOfSymbolAttributesForAllWeatherHazards(
	                       		                                        weatherHarzardList);	    			
							
						if(condensedISIG.symbolAttributeSubsetList.size() > 0){
								firstSymbolAttributes = condensedISIG.symbolAttributeSubsetList.get(0);
								polygonLineColor = firstSymbolAttributes.getSymbolColor();
								polygonLineWidth = firstSymbolAttributes.getLineWidth();
						}
							
						if(weatherHarzardList.get(0).equals(WeatherHazardType.TEST)){
								lineStyle          = LineStyle.SHORT_DASHED;
						}
	
			
						
						condensedISIG.polygonVertexPixelCoordList = this.generatePixelCoordinateListFromWorldCoordinateList(condensedISIG.polygonVertexWorldCoord);
						if(condensedISIG.polygonVertexPixelCoordList != null){
							int polygonVertexPixelCoordListSize = condensedISIG.polygonVertexPixelCoordList.size();
	                        boolean isLocationLookUpFailed = condensedISIG.isLocationLookUpFailed();
							if(polygonVertexPixelCoordListSize > 1 && !isLocationLookUpFailed ){
								    //this.drawPolygon(graphicsTarget,condensedISIG.polygonVertexPixelCoordList,polygonLineColor,polygonLineWidth,lineStyle);
									
									Coordinate[] polygonCoordinatesList = condensedISIG.getPolygonLatLonCoordinates();
					    			this.drawPolygon(polygonCoordinatesList, graphicsTarget, polygonLineColor, polygonLineWidth, lineStyle);
								     
								     /*Get the pixel coordinate of the lowest vertex of the polygon to render the text */
								    textLocation = condensedISIG.getLabelLocation(condensedISIG.polygonVertexPixelCoordList);
								     
								    /*Get the centroid of the polygon in world coordinates to render the symbol*/
								    /* tempSymbolLocationWorldCoord = condensedISIG.getCentroidInWorldCoordinates(condensedISIG, this.getDescriptor(),
								    		                                  condensedISIG.polygonVertexPixelCoordList);*/
								    
								    tempSymbolLocationWorldCoord = condensedISIG.getCentroidInWorldCoordinates(polygonCoordinatesList, 
								    		this.getNcMapDescriptor(), graphicsTarget);
								    
								     int distance   = condensedISIG.getDistance();
								     String polyExtent = condensedISIG.getPolygonExtent();
								     if(distance != IDecoderConstantsN.INTEGER_MISSING){
								    	 if(!condensedISIG.isPolygonClosed()){
								    	 /*If a surrounding polygon is to be rendered */
	                                      this.drawPolygonSurroundingLine(graphicsTarget,
	                                    		this.getNcMapDescriptor(),
	                                    		condensedISIG.polygonVertexWorldCoord,
	                                    		condensedISIG.polygonVertexPixelCoordList,
	                                    		polygonLineColor,polygonLineWidth,
	                                    		LineStyle.SHORT_DASHED,distance,polyExtent);
								    	 }
								     }
								   
	                              }
	                            
	                              else if(polygonVertexPixelCoordListSize == 1 && !isLocationLookUpFailed){
	      							
	                            	  /*Get the location of the single vertex to draw the symbol at that point, but convert it first to world coordinates*/
	                            	  PixelCoordinate singlePoint  = condensedISIG.polygonVertexPixelCoordList.get(0);
	                            	  double[] pixLoc =  new double[]{singlePoint.getX(), singlePoint.getY()};
	                            	  PixelCoordinate symbolPixToWorldCoord  = new PixelCoordinate(descriptor.pixelToWorld(pixLoc)); 
	                            	  tempSymbolLocationWorldCoord = new Coordinate(symbolPixToWorldCoord.getX(), symbolPixToWorldCoord.getY());
	                            	  if(condensedISIG.getDistance() != IDecoderConstantsN.INTEGER_MISSING){
	
	                            		  /*This is a weather hazard within a circular area*/
	     								  double radius = condensedISIG.getDistance();
	     								    
	                            		  /*delta is used to draw the marker at the center of the circle*/				   
	  									  double delta = offsetY * 0.3;
	                            		  this.drawCircleWithMarker(graphicsTarget,singlePoint,radius, delta, polygonLineColor, polygonLineWidth);
	   						 	      }
	                            	  
	  						 	    	
									       /*This is retained from legacy code:
									        * For a single point (isolated) TEST SIGMET the label's text is preceded with the character 'T'.
									        **/
	              					if(weatherHarzardList.size() > 0 
	              							&& weatherHarzardList.get(0).equals(WeatherHazardType.TEST)){
									       labelList.add("T");   						 	    	
	              					}
	                            	  
	                            	  /*The location of the label should be the same as that of the single vertex, but with a small x and y offset
	                            	   * to separate it from the symbol*/
							 	      textLocation = singlePoint;
									  textLocation.addToX(offsetX);
									  textLocation.addToY(offsetY);                             	  
	
	                              }
	                              else if(isLocationLookUpFailed){
	                            	  /*The question marks are added to ensure that 
	                            	   *the user understands this is a case of location look up failure*/
	                            	  labelList.add("??");
	                            	  tempSymbolLocationWorldCoord = new Coordinate(condensedISIG.getAlternateSymbolLocationLongitude(),condensedISIG.getAlternateSymbolLocationLatitude());
	                                  double[] worldPixel = new double[]{tempSymbolLocationWorldCoord.x, tempSymbolLocationWorldCoord.y};
	                                  textLocation = new PixelCoordinate( descriptor.worldToPixel(worldPixel));
	                                  textLocation.addToX(offsetX);
	                                  textLocation.addToY(offsetY);
	                              }
							
	
							   /*Check if the symbols need to be displayed*/
		                       if(intlSigmetResourceDataObj.symbolEnable){
	
		                    	   /*If the symbol location is not null and the list of symbols and the list of weather hazards are not empty..*/
									if(tempSymbolLocationWorldCoord != null 
											&& !condensedISIG.symbolAttributeSubsetList.isEmpty()
											&& !weatherHarzardList.isEmpty()){
										
										PixelCoordinate coordOfSymbolInPixel;
										DisplayElementFactory df = new DisplayElementFactory( graphicsTarget, getNcMapDescriptor() );
										ArrayList<IDisplayable> displayEls = new ArrayList<IDisplayable>(0);
										String symbolType = "ASTERISK";
										
										Coordinate symbolCoordinate = tempSymbolLocationWorldCoord;
										int numSymbolsToDisplay = weatherHarzardList.size();
										boolean isDrawText = false;
										firstSymbolAttributes  = condensedISIG.symbolAttributeSubsetList.get(0);
										Color[] symbolColor = { new Color(firstSymbolAttributes.getSymbolColor().red,
			                                                        firstSymbolAttributes.getSymbolColor().green,
			                                                        firstSymbolAttributes.getSymbolColor().blue)};
										
										
										for( int i = 0 ; i < numSymbolsToDisplay ; i++ ){
	                                        /*(Non-Javadoc)
	                                         * If the symbol is to be displayed in the Southern Hemisphere,
	                                         * and if it is either a hurricane or a tropical storm, then
	                                         * different symbols are used than if it were to be rendered in the 
	                                         * Northern Hemisphere.
	                                         */
											if(symbolCoordinate.x < 0){
	                                        	if(weatherHarzardList.get(i) == WeatherHazardType.HU){
	                                        		symbolType = "HURRICANE_SH";
	                                        	}
	                                        	if(weatherHarzardList.get(i) == WeatherHazardType.TR){
	                                        		symbolType = "TROPICAL_STORM_SH";
	                                        	}                                            	 
	                                        }
											
	                                        /*
	                                         * (Non-Javadoc)
	                                         * For the weather hazards listed below, instead of symbols, a text representation
	                                         * of the same is rendered within a box.
	                                         * (Note): 
	                                         * The decision to render WIND/WTSPT hazards as text 
	                                         * (since no equivalent symbols exist) is an improvement over legacy, 
	                                         * which does not render them at all.
	                                         * For WIND/WTSPT, the text is rendered in the color white, since there is
	                                         * no predefined color.  
	                                         */
	                                        if(weatherHarzardList.get(i) == WeatherHazardType.MW 
	                                        		|| weatherHarzardList.get(i) == WeatherHazardType.WS
	                                        		|| weatherHarzardList.get(i) == WeatherHazardType.WIND
	                                        		|| weatherHarzardList.get(i) == WeatherHazardType.WTSPT){
	                                        	isDrawText = true;
	                                        	switch(weatherHarzardList.get(i)){
	                                        	case MW:
	                                        		symbolType = "MTW";
	                                        		break;
	                                        	case WS:
	                                        		symbolType = "LLWS";
	                                        		break;	                                        	
	                                        	case WIND:
	                                        		symbolType = "WIND";
	                                        		break;
	                                        	case WTSPT:
	                                        		symbolType = "WTSPT";
	                                        		break;	                                                      	
	                                        	default:
	                                        		break;
	                                        	}
	                                        }
	                                        
	                                        int    symbolWidth   = condensedISIG.symbolAttributeSubsetList.get(i).getSymbolWidth();
	                                        double symbolSize    =  condensedISIG.symbolAttributeSubsetList.get(i).getSymbolSize();
	
	                                        if(isDrawText){
	                                        	String[] textString = new String[]{symbolType};
	                                        	/*(Non-Javadoc)
	                                        	 * The symbol scaling calculation shown below are ad-hoc and not
	                                        	 *very accurate since there is no specific scaling factor
	                                        	 *that exists between the symbols and the fonts used in CAVE*/
	                                        	float symbolSizeInNMAP = (float)(7*symbolSize + 2.5);
	                                        	float fontSize = symbolSizeInNMAP;
	                                        	Text symbolText = new Text(null,
	                                        			                  "Courier", 
	                                        			                  symbolSizeInNMAP,
	                                        			                  TextJustification.CENTER,
	                                        			                  symbolCoordinate,
	                                        			                  0.0,
	                                        			                  TextRotation.SCREEN_RELATIVE,
	                                        			                  textString,
	                                        			                  FontStyle.BOLD,
	                                        			                  symbolColor[0],
	                                        			                  0,0,
	                                        			                  false,
	                                        			                  DisplayType.BOX,
	                                        			                  "Text",
	                                        			                  "General Text"
	                                        			                  );
	                                        	displayEls = df.createDisplayElements((IText)symbolText,paintProps);
	                                        	
	                                        }else{
												   symbolType = condensedISIG.symbolAttributeSubsetList.get(i).getSymbolType(); 
										           Symbol symbol = new Symbol(
															null,
															symbolColor,
															symbolWidth,
															symbolSize * 0.60, /* scale per NMAP*/
															false,
															symbolCoordinate,
															"Symbol",
															symbolType);
											         displayEls = df.createDisplayElements(symbol,paintProps);
	                                        }
	
											if (displayEls != null && !displayEls.isEmpty()) {
												for (IDisplayable each : displayEls) {
													each.draw(graphicsTarget, paintProps);
													each.dispose();
												}
											}
										   
										   isDrawText = false;
	
	                                       
	                                       
	                                       /*To get the x-offset and y-offset to display more than one symbol without overlapping*/
	                                       coordOfSymbolInPixel = new PixelCoordinate(descriptor.worldToPixel(new double[]{symbolCoordinate.x, symbolCoordinate.y}));
	                                       coordOfSymbolInPixel.addToX(offsetX*1.5);
	                                       coordOfSymbolInPixel.addToY(offsetY*1.5);
	                                       double pixelArr[] = new double[]{coordOfSymbolInPixel.getX(), coordOfSymbolInPixel.getY()};
	                                       double worldArr[] = descriptor.pixelToWorld(pixelArr);
	                                       symbolCoordinate = new Coordinate(worldArr[0], worldArr[1]);  
										}
										
									}
		                       }
							
							    /*Display the messageID and sequenceNumber*/
	                            if(intlSigmetResourceDataObj.nameOrNumberEnable){
	                            	/*
	                            	 * (Non-Javadoc)
	                            	 * If both the messageID and the sequenceNumber are not null, display them together.
	                            	 * If the message id alone is not null, display it, but if it is null,
	                            	 * don't display the sequence number.
	                            	 */
	                            	if(condensedISIG.getMessageID() != null  ) {
	                            		String locMessageId = condensedISIG.getMessageID();
	                            		/*
	                            		 * (Non-Javadoc)
	                            		 * This is retained from legacy code:
	                            		 * If any of the sigmets (KZNY/KZMA/KZHU/TJZS) in the Atlantic are encountered, 
	                            		 * the label is preceded with the character 'A'.
	                            		 * */
	                            		if(condensedISIG.isSIGMETInAtlantic(condensedISIG.getIssueOffice(),
	                            				                            condensedISIG.getOmwo(),
	                            				                            condensedISIG.getAtsu())){
	                            			labelList.add("A");
	                            		}
	
	                            		if(condensedISIG.getSequenceNumber() != null){
	                            			
	                            			labelList.add(new String (locMessageId + " " 
	                            					+ condensedISIG.getSequenceNumber()));
	                            		}
	                            		else{
	                            			labelList.add(locMessageId);	
	                            		}
	
	                            	}
	                            }					
	                            
	                            if(intlSigmetResourceDataObj.timeEnable){
	                            	labelList.add( condensedISIG.getStartTimeText()+ "-" + condensedISIG.getEndTimeText());
	                            } 									    
	
	                            if(intlSigmetResourceDataObj.motionEnable){
	                            	if(condensedISIG.getSpeed() != IDecoderConstantsN.INTEGER_MISSING ){
	                            		labelList.add(condensedISIG.getSpeed() + " KT " + condensedISIG.getDirection());
	                            	}
	
	                            }
	                            
	                            if(intlSigmetResourceDataObj.flightLevelEnable){
	                            	int lowerFlightLevel = condensedISIG.getFlightLevel1();
	                            	int upperFlightLevel = condensedISIG.getFlightLevel2();
	                            		if(lowerFlightLevel > 0 && upperFlightLevel > 0){
	
	                            			labelList.add(lowerFlightLevel + " / " + upperFlightLevel);
	
	                            		}else{
	                            			   if(lowerFlightLevel > 0){
	                            				   labelList.add(lowerFlightLevel + "");
	                            			   }else if(upperFlightLevel > 0){
	                            				   labelList.add(upperFlightLevel + "");
	                            			   }else{
	                            			    labelList.add("");
	                            			   }
	                            		}                            
	                            }
	                            
	                            
	                        	if(!labelList.isEmpty() && textLocation != null){
	
	                        		for(int j = 0; j < labelList.size(); j++){
	                        			textRGBArray.add(polygonLineColor);
	
	                        		}
	
	                        		graphicsTarget.drawStrings(
	                        				font, 
	                        				(labelList.toArray(new  String[0])), 
	                        				textLocation.getX() + offsetX,
	                          				textLocation.getY() + offsetY,
	                        				0.0,
	                        				TextStyle.NORMAL, 
	                        				textRGBArray.toArray(new RGB[0]), 
	                        				HorizontalAlignment.LEFT, 
	                        				VerticalAlignment.TOP);
	
	                        	}                              
	
						}
		    	   }	
	    	   }
	       }	   
	} 
	
	/***
	 * Helper method for the paintFrame() to draw a circle around a weather hazard, with a marker at its center
	 * @param graphicsTarget
	 * @param circleCenterPixelCoord
	 * @param radius
	 * @param delta
	 * @param polygonLineColor
	 * @param polygonLineWidth
	 * @throws VizException
	 */

    private void drawCircleWithMarker(IGraphicsTarget graphicsTarget,
		PixelCoordinate circleCenterPixelCoord, double radius, double delta, RGB polygonLineColor, int polygonLineWidth)
    throws VizException {
    	
    	graphicsTarget.drawCircle(circleCenterPixelCoord.getX(), 
    			circleCenterPixelCoord.getY(), 
    			circleCenterPixelCoord.getZ(), 
    			radius, 
    			polygonLineColor, 
    			polygonLineWidth);    	
    	
    	graphicsTarget.drawLine(circleCenterPixelCoord.getX()-delta, 
    			circleCenterPixelCoord.getY(), 
    			circleCenterPixelCoord.getZ(),
    			circleCenterPixelCoord.getX()+delta, 
    			circleCenterPixelCoord.getY(), 
    			circleCenterPixelCoord.getZ(), 
    			polygonLineColor, polygonLineWidth);

    	graphicsTarget.drawLine(circleCenterPixelCoord.getX(), 
    			circleCenterPixelCoord.getY()-delta, 
    			circleCenterPixelCoord.getZ(),
    			circleCenterPixelCoord.getX(), 
    			circleCenterPixelCoord.getY()+delta, 
    			circleCenterPixelCoord.getZ(), 
    			polygonLineColor, polygonLineWidth);

    }

    /***
     * Helper method for paintFrame() to draw a polygon on either side of a line or
     * along a specified  direction to the line.
     * @param graphicsTarget
     * @param descriptor
     * @param polygonVertexWorldCoord
     * @param polygonLineColor
     * @param polygonLineWidth
     * @param lineStyle
     * @param distance
     * @throws VizException
     */
	private void drawPolygonSurroundingLine(IGraphicsTarget graphicsTarget,
		IMapDescriptor descriptor,
		ArrayList<Coordinate> polygonVertexWorldCoord, 
		ArrayList<PixelCoordinate> polygonVertexPixelCoord,
		RGB polygonLineColor,
		int polygonLineWidth, LineStyle lineStyle, int distance, String polygonExtent) throws VizException {
		
		if (polygonVertexWorldCoord != null && polygonVertexWorldCoord.size() > 1) {
			Coordinate[] polygonWorldCoordinateArray = new Coordinate[polygonVertexWorldCoord
			                                                          .size()];
			polygonVertexWorldCoord.toArray(polygonWorldCoordinateArray);
			double distanceNM;
			distanceNM = distance * METRE_TO_NM_CONVERSION_FACTOR;

			if ((polygonExtent == null) || polygonExtent.isEmpty() 
					|| polygonExtent.contains("WI")  
					|| polygonExtent.contains("WTN") 
					|| polygonExtent.contains("WITHIN")) { 

				if((polygonExtent != null && !(polygonExtent.isEmpty())) 
						&& (polygonExtent.contains("WI")  
								|| polygonExtent.contains("WTN")
								|| polygonExtent.contains("WITHIN"))){
					distanceNM = distance/2 * METRE_TO_NM_CONVERSION_FACTOR;
				}
				
				/*(Non-Javadoc)
				 * Get the coordinates on either side of the line
				 */
				Coordinate[][] arrayOfVerticesOnEitherSide = SigmetInfo.getSides(
						polygonWorldCoordinateArray, distanceNM);

				Coordinate[][] arrayOfVerticesOnEitherSideForPolygonWithArc = SigmetInfo
				.getSidesWithArcIntsc(descriptor,
						polygonWorldCoordinateArray,
						arrayOfVerticesOnEitherSide[0],
						arrayOfVerticesOnEitherSide[1]);
				
				/*(Non-Javadoc)
				 * Ideally, it should be easy to just use the existing IntlSigmetResource.drawPolygon()
				 * method coupled with the IGraphicsTarget.drawArc() method. However when doing this, it was
				 * observed that given the multiple conversions that take place between the world and pixel coordinates,
				 * the arc does not get drawn correctly.
				 * Hence the code to render the polygon and its arc point-by-point have been added here. 
				 */

				/*PixelCoordinate prevLoc = null;
				for (Coordinate currCoordinate : arrayOfVerticesOnEitherSideForPolygonWithArc[0]) {
					double[] latLon = { currCoordinate.x, currCoordinate.y };
					PixelCoordinate currLoc = new PixelCoordinate(descriptor.worldToPixel(latLon));
					if (prevLoc != null) {
						graphicsTarget.drawLine(prevLoc.getX(), prevLoc.getY(),
								prevLoc.getZ(), currLoc.getX(), currLoc.getY(),
								currLoc.getZ(), polygonLineColor, polygonLineWidth,
								lineStyle);
					}

					prevLoc = currLoc;
				}*/
				
				/*
				 * (Non-Javadoc)
				 * prevLoc is set to the last element of the first side of the polygon
				 * The second side is drawn from its last vertex to the first, thereby allowing the 
				 * line in-between the first and second sides to be drawn first  
				 * */
				/*for (int j = arrayOfVerticesOnEitherSideForPolygonWithArc[1].length - 1; j >= 0; j--) {
					Coordinate currCoordinate = arrayOfVerticesOnEitherSideForPolygonWithArc[1][j];
					double[] latLon = { currCoordinate.x, currCoordinate.y };

					PixelCoordinate currLoc = new PixelCoordinate(descriptor
							.worldToPixel(latLon));
					if (prevLoc != null) {
						graphicsTarget.drawLine(prevLoc.getX(), prevLoc.getY(),
								prevLoc.getZ(), currLoc.getX(), currLoc.getY(),
								currLoc.getZ(), polygonLineColor, polygonLineWidth,
								lineStyle);
					}

					prevLoc = currLoc;
				}*/
				
				
				ArrayList<Coordinate> coordinatesList = new ArrayList<Coordinate>();
				
				for (Coordinate currCoordinate : arrayOfVerticesOnEitherSideForPolygonWithArc[0]) {		
					coordinatesList.add(new Coordinate(currCoordinate.x, currCoordinate.y));
				}
				
				for (int j = arrayOfVerticesOnEitherSideForPolygonWithArc[1].length-1; j >= 0; j--) {
					Coordinate currCoordinate = arrayOfVerticesOnEitherSideForPolygonWithArc[1][j];					
					coordinatesList.add(new Coordinate(currCoordinate.x, currCoordinate.y));
				}
				
				Coordinate[] coordinates = new Coordinate[coordinatesList.size()];
				for (int i=0; i<coordinatesList.size(); i++) {
					coordinates[i] = coordinatesList.get(i);
				}
				
				IWireframeShape wireframeShape = graphicsTarget.createWireframeShape(false, descriptor);	
				JTSCompiler jtsCompiler = new JTSCompiler(null, wireframeShape, descriptor);
				GeometryFactory gf = new GeometryFactory(); 
				
				LineString ls = gf.createLineString(coordinates);			
				
				try {
					jtsCompiler.handle(ls, polygonLineColor, true);
					wireframeShape.compile(); 
					graphicsTarget.drawWireframeShape(wireframeShape, polygonLineColor, polygonLineWidth, lineStyle);
				} catch (VizException e) {
					System.out.println("VizException caught when calling IntlSigmetResource.drawPolygonSurroundingLine(...): " + e.getMessage()); 
			    } 
			
				wireframeShape.dispose();  
			}	
			else{
				if(polygonExtent.contains("OF")){
					String lineType;
					lineType = polygonExtent.replaceAll("\\s+", "").trim();
					if(lineType.contains("NEOF") || lineType.contains("NWOF")){
						lineType = new String("NOF");
					}
					if(lineType.contains("SEOF") || lineType.contains("SWOF")){
						lineType = new String("SOF");
					}
					
					Coordinate[] sideOfLineArr = SigmetInfo.getSOLCoors(polygonWorldCoordinateArray, 
							lineType, distanceNM, descriptor);
					if(sideOfLineArr != null && sideOfLineArr.length > 1){
						List<Coordinate> sideOfLineList =  Arrays.asList(sideOfLineArr);
						ArrayList<PixelCoordinate>  sideOfLinePixCoordlist =  generatePixelCoordinateListFromWorldCoordinateList(sideOfLineList);
						drawPolygon(graphicsTarget, sideOfLinePixCoordlist, polygonLineColor, polygonLineWidth, lineStyle);
						
						//The following method should be used if we want the polygons that span the edges to be clipped 
						//Seems this case is extremely rare. No test cases were found. So for now not using the new drawPolygon method for this case.
						//this.drawPolygon(sideOfLineArrInLatLonCoordinates, graphicsTarget, polygonLineColor, polygonLineWidth, lineStyle);
					}
				}

			}
    	}
}

	/***
     * Overridden method to initialize the font, set the dimensions for any character to be displayed
     * by this resource and query the database.
     * 
     * @param grphTarget - The graphics target
     */
	@Override
	public void initResource(IGraphicsTarget grphTarget) throws VizException {
    	queryRecords();
	}
	
	
	
	/**
	 * Converts a list of world coordinates to a list of pixel coordinates. 
	 * @param listOfVerticesOnSide1 - the list of Coordinate objects
	 * @return an array-list of PixelCoordinate objects
	 */
	private ArrayList<PixelCoordinate> generatePixelCoordinateListFromWorldCoordinateList(List<Coordinate> listOfVerticesOnSide1){
		/*
		 * (Non-Javadoc) 
		 * Strictly speaking, this method to generate the pixel coordinates for the world coordinates should
		 * belong to the private class CondensedIntlSigmetRecord. However, since this conversion requires a descriptor object
		 * it is computed within the IntlSigmetResource class.
		 */
		ArrayList<PixelCoordinate> pixelCoordList = new ArrayList<PixelCoordinate>(0);
		if(listOfVerticesOnSide1 != null && listOfVerticesOnSide1.size() > 0){
			for(Coordinate currentPoint: listOfVerticesOnSide1){
				pixelCoordList.add(this.getPixelCoordinate(currentPoint));
			}
		}
		return pixelCoordList;
	}
	
	/***
	 * Converts the world coordinate object to an equivalent pixel coordinate
	 * @param locWorldCoordinate - the input Coordinate object
	 * @return The equivalent PixelCoordinate 
	 */
	private PixelCoordinate getPixelCoordinate(Coordinate locWorldCoordinate){
		/*
		 * (Non-Javadoc) 
		 * Strictly speaking, this method to generate the pixel coordinates for the world coordinates should
		 * belong to the private class CondensedIntlSigmetRecord. However, since this conversion requires a descriptor object
		 * it is computed within the IntlSigmetResource class.
		 */
		
		double[] worldPixelArr = {locWorldCoordinate.x, locWorldCoordinate.y};
		double[]pixelArr =  descriptor.worldToPixel(worldPixelArr);
		return new PixelCoordinate(pixelArr[0], pixelArr[1]);
	}
	
	/***
	 * Draws a polygon line-by-line with the given line color, line style and line width 
	 * @param graphicsTarget - the graphics target
	 * @param polygonCoordinatesList - the list of polygon coordinates in PixelCoordinate
	 * @param polygonLineColor - the color of the polygon in RGB
	 * @param polygonLineWidth - the width of the polygon in int
	 * @param lineStyle - the type of line to drawn
	 * @throws VizException
	 */
    private void drawPolygon(IGraphicsTarget graphicsTarget, ArrayList<PixelCoordinate> polygonCoordinatesList, 
    		                 RGB polygonLineColor, int polygonLineWidth, LineStyle lineStyle)
    throws VizException{
		if (polygonCoordinatesList != null && polygonCoordinatesList.size() > 0) {
			ListIterator<PixelCoordinate> it = polygonCoordinatesList.listIterator();
			PixelCoordinate currVertex = null;
			PixelCoordinate prevVertex = null;
			while (it.hasNext()) {
				currVertex = it.next();

				if (prevVertex != null) {
					graphicsTarget.drawLine(prevVertex.getX(), prevVertex
							.getY(), prevVertex.getZ(), currVertex.getX(),
							currVertex.getY(), currVertex.getZ(),
							polygonLineColor, polygonLineWidth, lineStyle);
				}

				prevVertex = currVertex;

			}
		}        	
    }	
    
    /***
	 * Draws a polygon with the given line color, line style and line width 
	 * Also, clips the polygons, if necessary.
	 * @param polygonCoordinatesList - the list of polygon coordinates in Lat/Lon
	 * @param graphicsTarget - the graphics target
	 * @param polygonLineColor - the color of the polygon in RGB
	 * @param polygonLineWidth - the width of the polygon in int
	 * @param lineStyle - the type of line to drawn
	 * @throws VizException
	 */
    private void drawPolygon(Coordinate[] polygonCoordinatesList, IGraphicsTarget target, RGB lineColor, int lineWidth, LineStyle lineStyle) {
    	
    	if (polygonCoordinatesList != null) {
			IWireframeShape wireframeShape = target.createWireframeShape(false, descriptor);		
			JTSCompiler jtsCompiler = new JTSCompiler(null, wireframeShape, descriptor);
			GeometryFactory gf = new GeometryFactory(); 
			LineString ls = gf.createLineString(polygonCoordinatesList);			
						
			try {
				jtsCompiler.handle(ls, lineColor, true);
				wireframeShape.compile(); 
				target.drawWireframeShape(wireframeShape, lineColor, lineWidth, lineStyle);
			} catch (VizException e) {
				System.out.println("VizException caught when calling IntlSigmetResource.drawPolygon(...): " + e.getMessage()); 
		    } 
			wireframeShape.dispose();  
		}    	

	}		
  
	/***
	 * Retrieves only a sub-set of the attributes of the IntlSigmetRecord required to
	 * paint it on the frame.
	 * The method fetches a sub-set of the attributes from the input IntlSigmetRecord to
	 * create an object of type IntlSigmetRscDataObj. Doing so, prevents repeated
	 * queries to the database and thereby makes the IntlSigmetResource.paintFrame() method much faster.
	 * <p>
	 * This method checks the input record for a location look-up failure and if one is found,
	 * attempts to find an alternate location to plot the symbol.
	 * If there is no lookup failure, it retrieves all the vertices for the polygon
	 * and closes the polygon, if it is not already closed. 
	 * @param completeIntlSigRecord - the complete record as retrieved from the database
	 * @return The condensed international SIGMET Record
	 */
	private IntlSigmetRscDataObj getCondensedIntlSigmetRecord(IntlSigmetRecord completeIntlSigRecord){
		IntlSigmetRscDataObj condensedIntlSigmetRecord = new IntlSigmetRscDataObj();
		condensedIntlSigmetRecord.FRAME_DATE_FORMAT.setTimeZone( TimeZone.getTimeZone("GMT"));

		condensedIntlSigmetRecord.eventTime = new DataTime( completeIntlSigRecord.getStartTime(),
											 new TimeRange( completeIntlSigRecord.getStartTime(),
													 	    completeIntlSigRecord.getEndTime()) );
		
		condensedIntlSigmetRecord.direction                = completeIntlSigRecord.getDirection();
		condensedIntlSigmetRecord.distance                 = completeIntlSigRecord.getDistance();
		condensedIntlSigmetRecord.endTime                  = new DataTime( completeIntlSigRecord.getEndTime() );    
		condensedIntlSigmetRecord.endTimeText              = new String(condensedIntlSigmetRecord.FRAME_DATE_FORMAT.format(condensedIntlSigmetRecord.endTime.getValidTime().getTime()));
		condensedIntlSigmetRecord.flightLevel1             = completeIntlSigRecord.getFlightlevel1();
		condensedIntlSigmetRecord.flightLevel2             = completeIntlSigRecord.getFlightlevel2();
		condensedIntlSigmetRecord.intensity                = completeIntlSigRecord.getIntensity();
		condensedIntlSigmetRecord.intlSigLocationSet       = completeIntlSigRecord.getIntlSigmetLocation();
		condensedIntlSigmetRecord.issueTime                = new DataTime(completeIntlSigRecord.getIssueTime());
		condensedIntlSigmetRecord.issueOffice              = completeIntlSigRecord.getIssueOffice();
		condensedIntlSigmetRecord.intlSigLocationSet       = completeIntlSigRecord.getIntlSigmetLocation();
		condensedIntlSigmetRecord.messageID                = completeIntlSigRecord.getMessageID();
		condensedIntlSigmetRecord.startTimeText            = new String(condensedIntlSigmetRecord.FRAME_DATE_FORMAT.format(
																	condensedIntlSigmetRecord.eventTime.getValidTime().getTime()));
		condensedIntlSigmetRecord.speed                    = completeIntlSigRecord.getSpeed();
		condensedIntlSigmetRecord.sequenceNumber           = completeIntlSigRecord.getSequenceNumber();
		condensedIntlSigmetRecord.bullMessage              = completeIntlSigRecord.getBullMessage();
		condensedIntlSigmetRecord.weatherHazardEnumList    = condensedIntlSigmetRecord
		                                                     .generateWeatherHazardEnumList
		(completeIntlSigRecord.getHazardType());
		condensedIntlSigmetRecord.atsu                     =  completeIntlSigRecord.getAtsu();
		condensedIntlSigmetRecord.omwo                     = completeIntlSigRecord.getOmwo();
		condensedIntlSigmetRecord.polygonExtent            = completeIntlSigRecord.getPolygonExtent();
			
            /*
             * (Non-Javadoc)
             * The alternate location to plot the weather hazard symbol would be the longitude/latitude of one of the following:
             * The issue office
             * The OMWO or
             * The ATSU
             * it is used when location look up fails 			
             */
		condensedIntlSigmetRecord.computeAlternateLocationForSymbol(condensedIntlSigmetRecord.getIssueOffice(),
				condensedIntlSigmetRecord.getOmwo(),
				condensedIntlSigmetRecord.getAtsu());

		int numPoints = condensedIntlSigmetRecord.getIntlSigLocationSet().size();
		int index = -1;
		LatLonPoint[] localArrayLatLonPoints = new LatLonPoint[numPoints];

		if (condensedIntlSigmetRecord.intlSigLocationSet.size() > 0 ) {
			for (IntlSigmetLocation currentIntlSigLocation : condensedIntlSigmetRecord
					.getIntlSigLocationSet()) {
				index = currentIntlSigLocation.getIndex();

				if (currentIntlSigLocation.getLatitude() != (double) IDecoderConstantsN.FLOAT_MISSING
						&& currentIntlSigLocation.getLongitude() != (double) IDecoderConstantsN.FLOAT_MISSING) {
					localArrayLatLonPoints[index - 1] = new LatLonPoint(
							currentIntlSigLocation.getLatitude(),
							currentIntlSigLocation.getLongitude(),
							LatLonPoint.INDEGREES);
				} else {
					condensedIntlSigmetRecord.setlocationLookUpFailed(true);
					break;
				}
			}
		}else{
			condensedIntlSigmetRecord.setlocationLookUpFailed(true);
		}

		condensedIntlSigmetRecord.arrayLatLonPoints        = localArrayLatLonPoints;
		condensedIntlSigmetRecord.polygonVertexWorldCoord  = condensedIntlSigmetRecord
		           .generateWorldCoordinatesForPolygonFromLatLonArray(
				            condensedIntlSigmetRecord.getArrayLatLonPoints());		

		/*If there is no surrounding polygon to be rendered */
		if(condensedIntlSigmetRecord.getDistance() == IDecoderConstantsN.INTEGER_MISSING){
			if ( !condensedIntlSigmetRecord.isLocationLookUpFailed() &&  condensedIntlSigmetRecord.polygonVertexWorldCoord != null) {
				/*
				 * (Non-Javadoc)
				 * If the distance is not set, check if the polygon needs to be closed.
				 * The polygon must be closed to compute its centroid, since the symbol(s) will
				 * be rendered at the centroid.
				 * If it is a single point, isolated event, there is no need to attempt to close it 
				 * and the symbol will be rendered at this point itself.
				 */
				int listSize = condensedIntlSigmetRecord.polygonVertexWorldCoord
				.size();
				if (listSize > 0) {

					if (listSize != 1) {

						Coordinate firstVertex = condensedIntlSigmetRecord.polygonVertexWorldCoord
						.get(0);
						Coordinate lastVertex = condensedIntlSigmetRecord.polygonVertexWorldCoord
						.get(listSize - 1);
						if (!firstVertex.equals2D(lastVertex)) {
							condensedIntlSigmetRecord.polygonVertexWorldCoord
							.add(new Coordinate(firstVertex.x,
									firstVertex.y));

						}
						condensedIntlSigmetRecord.setPolygonClosed(true);

					}
				}
			}
		}else{
			/*
			 * (Non-Javadoc)
			 * This piece of logic is just to ensure that the flag isPolygonClosed is set 
			 * correctly, since this flag decides whether the method drawPolygonSurroundingLine()
			 * is to be invoked in paintFrame()
			 * 
			 */
			String bullMsg = condensedIntlSigmetRecord.getBullMessage();
			if(! bullMsg.contains("OF LINE") && ! bullMsg.contains("OF LN")){
				condensedIntlSigmetRecord.setPolygonClosed(true);
			}
		}

		return condensedIntlSigmetRecord;
	}

	/**
	 * Private class to capture a subset of the attributes of the International SIGMET record. 
	 * @author Archana
	 *
	 */
    private class IntlSigmetRscDataObj implements IRscDataObject {    	
		/**
		 * @return the isPolygonClosed
		 */
		private boolean isPolygonClosed() {
			return isPolygonClosed;
		}

		/**
		 * @param isPolygonClosed the isPolygonClosed to set
		 */
		private void setPolygonClosed(boolean isPolygonClosed) {
			this.isPolygonClosed = isPolygonClosed;
		}


		public boolean isPolygonClosed;
		public String polygonExtent;
		DataTime        issueTime;     //  issue time from bulletin
		DataTime        eventTime;     //  start time of individual International SIGMET
		DataTime        endTime;       //  end time of individual International SIGMET
		String          startTimeText;
		String          endTimeText;    	
		String          intensity;     //  example: "NC" (NC - No Change -- not currently displayed)
		int             flightLevel1;  //  altitude LOWER bound; <0 if not specified
		int             flightLevel2;  //  altitude UPPER bound; <0 if not specified
		int distance;
		int speed;
		String direction;
		String messageID;
		String sequenceNumber;
		Set<IntlSigmetLocation> intlSigLocationSet;
		ArrayList<WeatherHazardType> weatherHazardEnumList;
		LatLonPoint[] arrayLatLonPoints;
		ArrayList <SymbolAttributesSubSet<RGB, Integer, Float, Boolean, String>> symbolAttributeSubsetList; 
		final SimpleDateFormat FRAME_DATE_FORMAT = new SimpleDateFormat("dd/HHmm");    	
    	
    	public String omwo;

		public String atsu;

		public ArrayList<PixelCoordinate> polygonVertexPixelCoordList;

		public String bullMessage;

		private double alternateSymbolLocationLatitude;
    	
    	private double alternateSymbolLocationLongitude; 

    	private boolean locationLookUpFailed;
    	
    	private String issueOffice;

    	private ArrayList<Coordinate> polygonVertexWorldCoord;
    	
    	IntlSigmetRscDataObj(){
    		polygonVertexWorldCoord     = new ArrayList<Coordinate>(0);
    		polygonVertexPixelCoordList = new ArrayList<PixelCoordinate>(0);
    	}
    	
    	@Override
    	public DataTime getDataTime() {
    		return eventTime;
    	}

    	/***
    	 * Returns the lowest point in the list of PixelCoordinate objects as the location of the label
    	 * for the International SIGMET.
    	 * @param polygonCoordinatesList
    	 * @return
    	 */
        private PixelCoordinate getLabelLocation(ArrayList<PixelCoordinate> polygonCoordinatesList){
    		PixelCoordinate labelPosition = null;
        	if (polygonCoordinatesList != null && polygonCoordinatesList.size() > 0) {
    			ListIterator<PixelCoordinate> it = polygonCoordinatesList.listIterator();
    			PixelCoordinate currVertex = null;
    			while (it.hasNext()) {
    				currVertex = it.next();
    				if(labelPosition == null || labelPosition.getY() < currVertex.getY()){
    					labelPosition = currVertex;
    				}
    			}
        	}
        	
        	return labelPosition;
        }    	
    	/***
    	 * Returns the mid-point of a polygon
    	 * @param currDescriptor    - the descriptor for this resource.
    	 * @param vertexListInPixel - the list of vertices of the polygon in pixel coordinates.
    	 * @return the centroid of the Polygon in world coordinates.
    	 */
		private Coordinate getCentroidInWorldCoordinates(IMapDescriptor currDescriptor, ArrayList<PixelCoordinate> vertexListInPixel) {
			Coordinate centroid = null;
			if (vertexListInPixel != null && vertexListInPixel.size() > 0) {
				ArrayList<Coordinate> locCoordArray = new ArrayList<Coordinate>(0);
				ListIterator<PixelCoordinate> it  =  vertexListInPixel.listIterator();
				while(it.hasNext()){
					PixelCoordinate currLoc = it.next();
					locCoordArray.add(new  Coordinate(currLoc.getX(), currLoc.getY()));
				}
				Coordinate[] coordArray       = new Coordinate[locCoordArray.size()];
				locCoordArray.toArray(coordArray);
				GeometryFactory geomFactory   = new GeometryFactory();
				LineString lineString         = geomFactory.createLineString(coordArray);
				Point centroidPoint           = lineString.getCentroid();
				Coordinate centroidCoordinate = centroidPoint.getCoordinate();
				double[] pixLoc               = {centroidCoordinate.x, centroidCoordinate.y};
				PixelCoordinate centroidInPixelCoord = new PixelCoordinate(currDescriptor.pixelToWorld(pixLoc));				
				centroid = new Coordinate(centroidInPixelCoord.getX(), centroidInPixelCoord.getY());
			}				
			
		    return centroid;
		}
	
		/* Returns the mid-point of a polygon
    	 * @param polygonCoordinatesList - the list of vertices of the polygon in pixel coordinates.
    	 * @param currDescriptor    - the descriptor for this resource.
    	 * @param graphicsTarget - the graphics target
    	 * @return the centroid of the Polygon in world coordinates.
    	 */
		  
		private Coordinate getCentroidInWorldCoordinates(Coordinate[] polygonCoordinatesList, IMapDescriptor currDescriptor, IGraphicsTarget target) {
			Coordinate centroid = null;
			
			if (polygonCoordinatesList != null && polygonCoordinatesList.length > 0) {				
				GeometryFactory geomFactory   = new GeometryFactory();
				LineString lineString         = geomFactory.createLineString(polygonCoordinatesList);								
				Point centroidPoint = lineString.getCentroid();
				
				if (isPolygonClosed) {
					WorldWrapChecker wwc = new WorldWrapChecker(descriptor.getGridGeometry());
					Point centroidPt = getCentroidForClippedPolygon(geomFactory.createLineString(getPolygonLatLonCoordinates()), wwc, currDescriptor,target);
					
					if (centroidPt != null) {
						centroidPoint = centroidPt;
					}
				}	
					
				Coordinate centroidCoordinate = centroidPoint.getCoordinate();
				double[] pixLoc               = {centroidCoordinate.x, centroidCoordinate.y};
				PixelCoordinate centroidInPixelCoord = new PixelCoordinate(currDescriptor.pixelToWorld(pixLoc));
				centroid = new Coordinate(centroidInPixelCoord.getX(), centroidInPixelCoord.getY());
				
			}				
			
		    return centroid;
		}
		
		/* Returns the mid-point of clipped polygon; returns null if polygon is not clipped.
		 * This method takes care of the issue where a symbol for a clipped polygon (displayed at the edges)
		 * is drawn in the middle of the map area instead of displaying it in the center of the polygon.
		 * 
		 * This method is copied from class WorldWrapChecker: 
		 * private void wrapCorrect(Geometry g, List<Geometry> geomList, double inverseCentralMeridian) 
		 * with some minor changes added to get the centroid of a clipped geometry. 
         *   
    	 * @param g
    	 * @param checker		 - the world wrap checker
    	 * @param currDescriptor - the descriptor for this resource.
    	 * @param graphicsTarget - the graphics target
    	 * @return the centroid of the Polygon OR null
    	 */
		private Point getCentroidForClippedPolygon(Geometry g, WorldWrapChecker checker, IMapDescriptor currDescriptor, IGraphicsTarget target) {
	    	
	    	List<Geometry> geomList = new ArrayList<Geometry>();
	    	if (g.isEmpty() == false) {
	            // Algorithm:
	            // Process primitive geometry type (non collection). Algorithm works
	            // in that it walks the geometry, when two points cross, it adds or
	            // subtracts 360 to the offset to flatten out the geometry. When
	            // first part is done, geometries will be continuous and not limited
	            // to -180 to 180, they will be technically be > neg infinitive, <
	            // pos infinity given that the algorithm supports wrapping around
	            // the world multiple times. When we have the continuous geometry,
	            // we split it up into sections by intersecting with a 360 deg
	            // inverse central meridian. We then normalize the points for each
	            // section back to -180 to 180
	            boolean handle = false;
	            if (checker.needsChecking()) {
	                boolean polygon = g instanceof Polygon;
	                Coordinate[] coords = g.getCoordinates();
	                if (polygon) {
	                    // remove duplicate last point for polygon
	                    coords = Arrays.copyOf(coords, coords.length - 1);
	                }
	                int length = coords.length + (polygon ? 0 : -1);
	                int truLen = coords.length;
	                double currOffset = 0.0;
	                double minOffset = 0.0, maxOffset = 0.0;
	                for (int i = 0; i < length; ++i) {
	                    int ip1 = (i + 1) % truLen;
	                    Coordinate a = coords[i];
	                    Coordinate b = coords[ip1];

	                    if (ip1 != 0) {
	                        b.x += currOffset;
	                    }

	                    Boolean low = null;
	                    if (a.x - b.x > 180.0) {
	                        low = false;
	                    } else if (b.x - a.x > 180.0) {
	                        low = true;
	                    } else if (checker.check(a.x, b.x)) {
	                        handle = true;
	                    }

	                    if (low != null) {
	                        handle = true;
	                        // we wrap either low end or high
	                        if (low) {
	                            currOffset -= 360;
	                            b.x -= 360.0;
	                            if (currOffset < minOffset) {
	                                minOffset = currOffset;
	                            }
	                           
	                        } else {
	                            currOffset += 360;
	                            b.x += 360;
	                            if (currOffset > maxOffset) {
	                                maxOffset = currOffset;
	                            }
	                            
	                        }
	                    }
	                }
	                if (handle) {
                	     // All coords in geometry should be denormalized now, get
                         // adjusted envelope, divide envelope into sections, for
                         // each section, intersect with geometry and add to
                         // geom list
                         List<Geometry> sections = new ArrayList<Geometry>();
                         List<Double> rolls = new ArrayList<Double>();
                         GeometryFactory gf = g.getFactory();
                         double delta = 0.00001;
                         double start = checker.getLowInverseCentralMeridian() + minOffset;
                         double end = checker.getHighInverseCentralMeridian() + maxOffset;
                         double minY = -90, maxY = 90;
                         while (start < end) {
                             double useStart = start;
                             double useEnd = start + 360;
                             double minX = useStart + delta;
                             double maxX = (useEnd) - delta;

                             Geometry section = gf.createPolygon(
                                     gf.createLinearRing(new Coordinate[] {
                                             new Coordinate(minX, maxY),
                                             new Coordinate(maxX, maxY),
                                             new Coordinate(maxX, minY),
                                             new Coordinate(minX, minY),
                                             new Coordinate(minX, maxY) }), null);                          
                             section = section.intersection(g);
                             
                             if (section.isEmpty() == false) {
                                 sections.add(section);
                                 rolls.add(useEnd);
                                
                             }
                             start += 360;
                         }
                        
                         if (sections.size() > 0) {
                        	 /* if sections.size() > 0, then the geometry is clipped */
                        	 for (int i = 0; i < sections.size(); ++i) {
	                             Geometry section = sections.get(i);
	                             double rollVal = rolls.get(i);
	                             rollLongitudes(section, rollVal, checker.getHighInverseCentralMeridian());
	                             geomList.add(section);
	                         }
                        	
                        	IWireframeShape wireframeShape = target.createWireframeShape(false, descriptor);		
                 			JTSCompiler jtsCompiler = new JTSCompiler(null, wireframeShape, descriptor);
                 				
                 			LineString ls = gf.createLineString(geomList.get(0).getCoordinates());		
                 			
                 			try {
                 				jtsCompiler.handle(ls, new RGB(255, 255, 255), true);
                 			} catch (VizException e) {
                 				System.out.println("VizException caught when calling IntlSigmetResource.getCentroidForClippedPolygon(...): " + e.getMessage()); 
                 		    } 
                 			
                 			//return the centroid of the first section 
             				return geomList.get(0).getCentroid();	             				
                         }                        
	                }
	            }
	        }
	      return null;
	    }
		
		/*
		 * This method is copied from class WorldWrapChecker: 
		 * private void rollLongitudes(Geometry g, double inverseCentralMeridianUsed, double inverseCentralMeridian) 
         */
		private void rollLongitudes(Geometry g, double inverseCentralMeridianUsed,
	            double inverseCentralMeridian) {
	        double diff = inverseCentralMeridian - inverseCentralMeridianUsed;
	        if (diff != 0) {
	            for (Coordinate c : g.getCoordinates()) {
	                c.x += diff;
	            }
	        }
	    }
		
		private Coordinate[] getPolygonLatLonCoordinates() {
	    	
	    	if (arrayLatLonPoints != null && arrayLatLonPoints.length > 0) {
	    		
	    		Coordinate[] coordinates = null;
	    		if (isPolygonClosed) {
	    			coordinates = new Coordinate[arrayLatLonPoints.length+1];
	    		} else {
	    			coordinates = new Coordinate[arrayLatLonPoints.length];
	    		}
	    			
	    		for(int i = 0; i<arrayLatLonPoints.length; i++) {
	    			LatLonPoint latLonPoint = arrayLatLonPoints[i]; 
					
	    			Coordinate eachCoordinate = new Coordinate(latLonPoint.getLongitude(LatLonPoint.INDEGREES), latLonPoint.getLatitude(LatLonPoint.INDEGREES)); 
	    			coordinates[i] = eachCoordinate; 
				}
			
				if (isPolygonClosed) {
					int coordinateArrayLength = coordinates.length; 
					Coordinate startingPointCoordinate = new Coordinate(arrayLatLonPoints[0].getLongitude(LatLonPoint.INDEGREES), arrayLatLonPoints[0].getLatitude(LatLonPoint.INDEGREES)); 
					coordinates[coordinateArrayLength-1] = startingPointCoordinate; 
				} 

		    	return coordinates;
	    	} 
	    	else {
	    		return null;
	    	}
	    }
		
		/**
		 * @return the startTimeText
		 */
		private String getStartTimeText() {
			return startTimeText;
		}

		public String getIssueOffice() {

			return issueOffice;
		}

		private void setlocationLookUpFailed(boolean b) {
			this.locationLookUpFailed = b;
		}

		/**
		 * @return the locationLookUpFailed
		 */
		private boolean isLocationLookUpFailed() {
			return locationLookUpFailed;
		}

		/**
		 * @return the endTimeText
		 */
		private String getEndTimeText() {
			return endTimeText;
		}

		/**
		 * @return the arrayLatLonPoints
		 */
		public LatLonPoint[] getArrayLatLonPoints() {
			return arrayLatLonPoints;
		}


		/**
		 * @return the weatherHazardEnumList
		 */
		public ArrayList<WeatherHazardType> getWeatherHazardEnumList() {
			return weatherHazardEnumList;
		}

		/**
		 * @return the bullMessage
		 */
		private String getBullMessage() {
			return bullMessage;
		}

		/**
		 * @return the omwo
		 */
		private String getOmwo() {
			return omwo;
		}

		/**
		 * @return the atsu
		 */
		private String getAtsu() {
			return atsu;
		}

		/**
		 * @return the polygonVertexPixelCoordList
		 */
		private ArrayList<PixelCoordinate> getPolygonVertexPixelCoordList() {
			return polygonVertexPixelCoordList;
		}

		/**
		 * @return the polygonVertexWorldCoord
		 */
		private ArrayList<Coordinate> getPolygonVertexWorldCoord() {
			return polygonVertexWorldCoord;
		}

		/***
		 * Determines the location of either the issue office or the omwo  or the atsu
		 * as the alternate location to plot the symbol in case of a location look up failure.
		 * An attempt is first made to get the location of the issue office from the database. 
		 * If this fails, the location of the OMWO is looked up and if that fails as well, 
		 * the location of the ATSU is retrieved from the database. 
		 * If none of these succeed, then the location of the symbol is set to an invalid
		 * value, so that the symbol is not plotted.
		 * @param sigmetIssueOffice - The office that issued the SIGMET
		 * @param omwoName - The Originating Meteorological Watch Office
		 * @param atsuName - The Air Traffic Service Unit
		 */
		private void computeAlternateLocationForSymbol(String sigmetIssueOffice, String omwoName, String atsuName){
			/* Code to get the coordinates of the issueOffice.*/
			String queryStr1    = "select station_id, latitude, longitude from stns.intlsig";			

			Double stnLatitude  = (double) IDecoderConstantsN.FLOAT_MISSING;  //TODO:  Should we create IDecoderConstantsN.DOUBLE_MISSING instead ?
			Double stnLongitude = (double) IDecoderConstantsN.FLOAT_MISSING;  //TODO:  Should we create IDecoderConstantsN.DOUBLE_MISSING instead ?
			try {
                List<Object[]> stationTableInfo = NcDirectDbQuery.executeQuery(queryStr1,"ncep", QueryLanguage.SQL);

				for (Object[] stationObjectArray: stationTableInfo){
				        String  stnCode = (String) stationObjectArray[0];
				        if(stnCode.equals(sigmetIssueOffice)){
							stnLatitude  = (Double) stationObjectArray[1];
							stnLongitude = (Double) stationObjectArray[2];
							this.setAlternateSymbolLocationLatitude(stnLatitude.doubleValue());
							this.setAlternateSymbolLocationLongitude(stnLongitude.doubleValue());
							break;
				        } else if(stnCode.equals(omwoName)){
							stnLatitude  = (Double) stationObjectArray[1];
							stnLongitude = (Double) stationObjectArray[2];
							this.setAlternateSymbolLocationLatitude(stnLatitude.doubleValue());
							this.setAlternateSymbolLocationLongitude(stnLongitude.doubleValue());
							break;				        	
				        }else if(stnCode.equals(atsuName)){
							stnLatitude  = (Double) stationObjectArray[1];
							stnLongitude = (Double) stationObjectArray[2];
							this.setAlternateSymbolLocationLatitude(stnLatitude.doubleValue());
							this.setAlternateSymbolLocationLongitude(stnLongitude.doubleValue());
							break;				        	
				        }
			    }
				
			} catch (VizException e) {
	             stnLatitude = (double) IDecoderConstantsN.FLOAT_MISSING;  //TODO:  Should we create IDecoderConstantsN.DOUBLE_MISSING instead ?
	             stnLongitude = (double) IDecoderConstantsN.FLOAT_MISSING;  //TODO:  Should we create IDecoderConstantsN.DOUBLE_MISSING instead ?
	             System.out.println("Unable to retrieve the latitude and longitude of " + sigmetIssueOffice + " from the database");
			}
		}
		
		/***
		 * Converts the vertices of the polygon from an array of LatLonPoint objects to an ArrayList of Coordinate objects
		 * @param localLatLonPointArray - The array of LatLonPoint objects
		 * @return the list of polygon vertices if successful or an empty list otherwise.
		 */
        private ArrayList<Coordinate> generateWorldCoordinatesForPolygonFromLatLonArray(LatLonPoint[] localLatLonPointArray){
    		ArrayList<Coordinate> polygonWorldCoordList = new ArrayList<Coordinate>(0);

    		if (!this.isLocationLookUpFailed() && localLatLonPointArray != null && localLatLonPointArray.length > 0) {

				for (LatLonPoint currentPoint : localLatLonPointArray) {
					double[] latLon = new double[]{currentPoint.getLongitude(LatLonPoint.INDEGREES),
					                   currentPoint.getLatitude(LatLonPoint.INDEGREES)};
					polygonWorldCoordList.add(new Coordinate(latLon[0], latLon[1]));
				}
			}

    		return polygonWorldCoordList;
        }
        
        /***
         * Checks if the International Sigmet is issued for/in the Atlantic region
         * @param issueOff  - The office that issued the SIGMET
         * @param omwo - The Originating Meteorological Watch Office
		 * @param atsu - The Air Traffic Service Unit
		 * <p>
		 * If any of the input parameters are set to one of the following stations - KZMA / KZNY / KZHU / TJZS, 
		 * then the method returns true.
         * @return true if the International SIGMET is issued for/in the Atlantic region or false otherwise
         */
		private boolean isSIGMETInAtlantic(String issueOff, String omwo, String atsu){
			boolean inAtlantic = false;
			if( issueOff != null && ( (issueOff.contains("KZMA"))
				|| (issueOff.contains("KZNY"))	
						|| (issueOff.contains("KZHU"))	
								|| (issueOff.contains("TJZS")))	){
				inAtlantic = true;
			}

			if( omwo != null && ((omwo.contains("KZMA"))
					|| (omwo.contains("KZNY"))	
							|| (omwo.contains("KZHU"))	
									|| (omwo.contains("TJZS")))	){
					inAtlantic = true;
				}
			
			if( atsu != null &&  ((atsu.contains("KZMA"))
					|| (atsu.contains("KZNY"))	
							|| (atsu.contains("KZHU"))	
									|| (atsu.contains("TJZS")))	){
					inAtlantic = true;
				}
			
			return inAtlantic;
		}
        
		/**
		 * @return the issueOfficeLatitude
		 */
		private double getAlternateSymbolLocationLatitude() {
			return alternateSymbolLocationLatitude;
		}

		/**
		 * @return the issueOfficeLongitude
		 */
		private double getAlternateSymbolLocationLongitude() {
			return alternateSymbolLocationLongitude;
		}

		/**
		 * @return the issueTime
		 */
		private final DataTime getIssueTime() {
			return issueTime;
		}

		/**
		 * @return the endTime
		 */
		private final DataTime getEndTime() {
			return endTime;
		}

		/**
		 * @return the intensity
		 */
		private final String getIntensity() {
			return intensity;
		}

		/**
		 * @return the flightLevel1
		 */
		private final int getFlightLevel1() {
			return flightLevel1;
		}

		/**
		 * @return the flightLevel2
		 */
		private final int getFlightLevel2() {
			return flightLevel2;
		}

		/**
		 * @return the distance
		 */
		private final int getDistance() {
			return distance;
		}

		/**
		 * @return the speed
		 */
		private final int getSpeed() {
			return speed;
		}

		/**
		 * @return the direction
		 */
		private final String getDirection() {
			return direction;
		}

		/**
		 * @return the polygonExtent
		 */
		public String getPolygonExtent() {
			return polygonExtent;
		}

		/**
		 * @return the messageID
		 */
		private final String getMessageID() {
			return messageID;
		}

		/**
		 * @return the sequenceNumber
		 */
		private final String getSequenceNumber() {
			return sequenceNumber;
		}

		/**
		 * @return the intlSigLocationSet
		 */
		private final Set<IntlSigmetLocation> getIntlSigLocationSet() {
			return intlSigLocationSet;
		}

		/**
		 * @return the symbolAttributeSubsetList
		 */
		public ArrayList<SymbolAttributesSubSet<RGB, Integer, Float, Boolean, String>> getSymbolAttributeSubsetList() {
			return symbolAttributeSubsetList;
		}

	/**
	   * Creates a list of enumerations representing the weather hazards
	   * for the current CondensedIntlSigmetRecord 
	   * @param hazard - The weather hazard decoded as a single <tt>String</tt> - retrieved from the database.
	   * @return The list of weather hazards pertaining to the current International SIGMET record
	   */
	  ArrayList<WeatherHazardType> generateWeatherHazardEnumList(String hazard){
		  ArrayList<WeatherHazardType> enumWeather = new ArrayList<WeatherHazardType>(0);
		 /*
		  * (Non-Javadoc)
		  * Currently, all the different thunderstorms are represented using the 
		  * same symbol. In the future, it may be required to distinguish 
		  * between an Obscure thunderstorm and an isolated thunderstorm.
		  *  
		  */
		  if( hazard.contains("THUNDERSTORM")){
			  if( !hazard.contains("OBSCURE") 
					  && (!hazard.contains("ISOLATED")) 
					  && (!hazard.contains("WIDE"))
					  && (!hazard.contains("EMBEDDED"))){
				  
				      enumWeather.add(WeatherHazardType.TS);
			  }
			  else{
				  if( hazard.contains("OBSCURE")){
					  enumWeather.add(WeatherHazardType.OBSC_TS);
				  }

				  if( hazard.contains("ISOLATED")){
					  enumWeather.add(WeatherHazardType.ISOL_TS);
				  }			   

				  if( hazard.contains("WIDE")){
					  enumWeather.add(WeatherHazardType.WDSPR_TS);
				  }			   

				  if( hazard.contains("EMBEDDED")){
					  enumWeather.add(WeatherHazardType.EMBD_TS);
				  }
			  } 
  
		   }

		  if(hazard.contains("TURBULENCE")){
			   enumWeather.add(WeatherHazardType.TB); 
		   }		  

		  if(hazard.contains("HURRICANE")){
			   enumWeather.add(WeatherHazardType.HU); 
		   }		  
		  
		  if(hazard.contains("TROPICAL STORM")){
			   enumWeather.add(WeatherHazardType.TR); 
		   }		  
		  
		  if(hazard.contains("DEPRESSION")){
			   enumWeather.add(WeatherHazardType.TD); 
		   }

		  if(hazard.contains("VOLCANIC")){
			   enumWeather.add(WeatherHazardType.VA); 
		   }		  

		  if(hazard.contains("MOUNTAIN")){
			   enumWeather.add(WeatherHazardType.MW); 
		   }		  

		  if(hazard.contains("CYCLONE")){
			   enumWeather.add(WeatherHazardType.TC); 
		   }
		  
		  if(hazard.contains("SQUALL")){
			   enumWeather.add(WeatherHazardType.SQ); 
		   }		  

		  if(hazard.contains("CAT")){
			   enumWeather.add(WeatherHazardType.CAT); 
		   }		  		  

		  if(hazard.contains("ICING")){
			   enumWeather.add(WeatherHazardType.IC); 
		   }		  		  		  

		  if(hazard.contains("HAIL")){
			   enumWeather.add(WeatherHazardType.GR); 
		   }
		  
		  if(hazard.contains("DUST")){
			   enumWeather.add(WeatherHazardType.DS); 
		   }		  

		  if(hazard.contains("SAND")){
			   enumWeather.add(WeatherHazardType.SS); 
		   }		  

		  if(hazard.contains("CUMULONIMBUS")){
			   enumWeather.add(WeatherHazardType.CB); 
		   }		  		  
		  
		  if(hazard.contains("SHEAR")){
			   enumWeather.add(WeatherHazardType.WS); 
		   }

		  if(!(hazard.contains("SHEAR"))
				  && (hazard.contains("WIND"))){
			   enumWeather.add(WeatherHazardType.WIND); 
		   }		  

		  if(hazard.contains("FREEZING RAIN")){
			   enumWeather.add(WeatherHazardType.FZRA); 
		   }		  
		  
		  if(hazard.contains("TORNADO")){
			   enumWeather.add(WeatherHazardType.TDO); 
		   }
		  
		  if(hazard.contains("FUNNEL")){
			   enumWeather.add(WeatherHazardType.FC); 
		   }		  

		  if(hazard.contains("WATERSPOUT")){
			   enumWeather.add(WeatherHazardType.WTSPT); 
		   }
		  
		  if (hazard.contains("TEST")){
			  enumWeather.add(WeatherHazardType.TEST);
		  }
		  
		  return enumWeather;
	  }


	  private ArrayList<SymbolAttributesSubSet<RGB, Integer, Float, Boolean, String>> generateListOfSymbolAttributesForAllWeatherHazards(
			                                                                    ArrayList<WeatherHazardType> listOfWeatherHazards){
		  ArrayList<SymbolAttributesSubSet<RGB, Integer, Float, Boolean, String>> localSymbolAttributeSubsetList 
		              = new ArrayList<SymbolAttributesSubSet<RGB, Integer, Float, Boolean, String>>(0);
		  
		  for( WeatherHazardType currentHazard : listOfWeatherHazards){
				switch(currentHazard){
				    
				    case TS:/*Thunderstorm*/
				    
				    	localSymbolAttributeSubsetList.add(new SymbolAttributesSubSet<RGB, Integer, Float, Boolean, String>(
				    			intlSigmetResourceDataObj.thunderstormColor,
				    			intlSigmetResourceDataObj.thunderstormSymbolWidth,
				    			intlSigmetResourceDataObj.thunderstormSymbolSize,
				    			intlSigmetResourceDataObj.thunderstormEnable,
				    			"PAST_WX_09", //refer symbolPatterns.xml
				    			intlSigmetResourceDataObj.thunderstormLineWidth));
				    	
					break;
					
				    case ISOL_SEV_TS:
				    	localSymbolAttributeSubsetList.add(new SymbolAttributesSubSet<RGB, Integer, Float, Boolean, String>(
				    			intlSigmetResourceDataObj.thunderstormColor,
				    			intlSigmetResourceDataObj.thunderstormSymbolWidth,
				    			intlSigmetResourceDataObj.thunderstormSymbolSize,
				    			intlSigmetResourceDataObj.thunderstormEnable,
				    			"PAST_WX_09", //refer symbolPatterns.xml
				    			intlSigmetResourceDataObj.thunderstormLineWidth));				    	
				    	break;
				    
				    case ISOL_TS:
				    	localSymbolAttributeSubsetList.add(new SymbolAttributesSubSet<RGB, Integer, Float, Boolean, String>(
				    			intlSigmetResourceDataObj.thunderstormColor,
				    			intlSigmetResourceDataObj.thunderstormSymbolWidth,
				    			intlSigmetResourceDataObj.thunderstormSymbolSize,
				    			intlSigmetResourceDataObj.thunderstormEnable,
				    			"PAST_WX_09", //refer symbolPatterns.xml
				    			intlSigmetResourceDataObj.thunderstormLineWidth));				    	
				    	break;
				    
				    case EMBD_TS:
				    	localSymbolAttributeSubsetList.add(new SymbolAttributesSubSet<RGB, Integer, Float, Boolean, String>(
				    			intlSigmetResourceDataObj.thunderstormColor,
				    			intlSigmetResourceDataObj.thunderstormSymbolWidth,
				    			intlSigmetResourceDataObj.thunderstormSymbolSize,
				    			intlSigmetResourceDataObj.thunderstormEnable,
				    			"PAST_WX_09", //refer symbolPatterns.xml
				    			intlSigmetResourceDataObj.thunderstormLineWidth));				    	
				    	break;
				    
				    case OBSC_TS:
				    	        localSymbolAttributeSubsetList.add(new SymbolAttributesSubSet<RGB, Integer, Float, Boolean, String>(
				    			intlSigmetResourceDataObj.thunderstormColor,
				    			intlSigmetResourceDataObj.thunderstormSymbolWidth,
				    			intlSigmetResourceDataObj.thunderstormSymbolSize,
				    			intlSigmetResourceDataObj.thunderstormEnable,
				    			"PAST_WX_09", //refer symbolPatterns.xml
				    			intlSigmetResourceDataObj.thunderstormLineWidth));				    	
				    	break;
					
					
				    case TB:/*Turbulence*/
				    
  				    	localSymbolAttributeSubsetList.add(new SymbolAttributesSubSet<RGB, Integer, Float, Boolean, String>(
				    			intlSigmetResourceDataObj.turbulenceColor,
				    			intlSigmetResourceDataObj.turbulenceSymbolWidth,
				    			intlSigmetResourceDataObj.turbulenceSymbolSize,
				    			intlSigmetResourceDataObj.turbulenceEnable,
				    			"TURBULENCE_5", //refer symbolPatterns.xml
				    			intlSigmetResourceDataObj.turbulenceLineWidth));

				    	
					break;		    					
				    
				    case HU:/*Hurricane*/
				    
  				    	localSymbolAttributeSubsetList.add(new SymbolAttributesSubSet<RGB, Integer, Float, Boolean, String>(
				    			intlSigmetResourceDataObj.hurricaneColor,
				    			intlSigmetResourceDataObj.hurricaneSymbolWidth,
				    			intlSigmetResourceDataObj.hurricaneSymbolSize,
				    			intlSigmetResourceDataObj.hurricaneEnable,
				    			"HURRICANE_NH", //change to HURRICANE_SH if location is in Southern hemisphere
				    			0)); // Hurricance does not have a line width associated with it.

				    	
					break;			    					

				    case TR:/*Tropical Storm*/
				    
  				    	localSymbolAttributeSubsetList.add(new SymbolAttributesSubSet<RGB, Integer, Float, Boolean, String>(
				    			intlSigmetResourceDataObj.tropicalStormColor,
				    			intlSigmetResourceDataObj.tropicalStormSymbolWidth,
				    			intlSigmetResourceDataObj.tropicalStormSymbolSize,
				    			intlSigmetResourceDataObj.tropicalStormEnable,
				    			"TROPICAL_STORM_NH", //change to TROPICAL_STORM_NH if location is in Southern hemisphere
				    			0)); // Tropical Storm does not have a line width associated with it.

				    	
					break;		    					

				    case TD:/*Tropical Depression*/
				    
  				    	localSymbolAttributeSubsetList.add(new SymbolAttributesSubSet<RGB, Integer, Float, Boolean, String>(
				    			intlSigmetResourceDataObj.tropicalDepressionColor,
				    			intlSigmetResourceDataObj.tropicalDepressionSymbolWidth,
				    			intlSigmetResourceDataObj.tropicalDepressionSymbolSize,
				    			intlSigmetResourceDataObj.tropicalDepressionEnable,
				    			"TROPICAL_DEPRESSION", //refer symbolPatterns.xml
				    			0)); // Tropical Depression does not have a line width associated with it.

				    	
					break;			    					

				    case VA:/*Volcanic Ash*/
				    
  				    	localSymbolAttributeSubsetList.add(new SymbolAttributesSubSet<RGB, Integer, Float, Boolean, String>(
				    			intlSigmetResourceDataObj.volcanicAshCloudColor,
				    			intlSigmetResourceDataObj.volcanicAshCloudSymbolWidth,
				    			intlSigmetResourceDataObj.volcanicAshCloudSymbolSize,
				    			intlSigmetResourceDataObj.volcanicAshCloudEnable,
				    			"PRESENT_WX_201", //refer symbolPatterns.xml
				    			intlSigmetResourceDataObj.volcanicAshCloudLineWidth));

				    	
					break;		    					
					
				    case MW:/*Mountain Wave*/
				    
  				    	localSymbolAttributeSubsetList.add(new SymbolAttributesSubSet<RGB, Integer, Float, Boolean, String>(
				    			intlSigmetResourceDataObj.mountainWaveColor,
				    			intlSigmetResourceDataObj.mountainWaveSymbolWidth,
				    			intlSigmetResourceDataObj.mountainWaveSymbolSize,
				    			intlSigmetResourceDataObj.mountainWaveEnable,
				    			"MT_WAVE", //refer symbolPatterns.xml
				    			intlSigmetResourceDataObj.mountainWaveLineWidth)); 
				    	
					break;		    					
					
				    case TC:/*Tropical Cyclone*/
				    
  				    	localSymbolAttributeSubsetList.add(new SymbolAttributesSubSet<RGB, Integer, Float, Boolean, String>(
				    			intlSigmetResourceDataObj.tropicalCycloneColor,
				    			intlSigmetResourceDataObj.tropicalCycloneSymbolWidth,
				    			intlSigmetResourceDataObj.tropicalCycloneSymbolSize,
				    			intlSigmetResourceDataObj.tropicalCycloneEnable,
				    			"TROPICAL_CYCLONE", //refer symbolPatterns.xml
				    			0)); // Tropical Cyclone does not have a line width associated with it.

				    	
					break;

				    case SQ:/*SQuall Line*/
				    
  				    	localSymbolAttributeSubsetList.add(new SymbolAttributesSubSet<RGB, Integer, Float, Boolean, String>(
				    			intlSigmetResourceDataObj.squallLineColor,
				    			intlSigmetResourceDataObj.squallLineSymbolWidth,
				    			intlSigmetResourceDataObj.squallLineSymbolSize,
				    			intlSigmetResourceDataObj.squallLineEnable,
				    			"PRESENT_WX_018", //refer symbolPatterns.xml
				    			intlSigmetResourceDataObj.squallLineWidth)); 
				    	
					break;					
					
				    case CAT:/*Clear Air Turbulence*/
				    
  				    	localSymbolAttributeSubsetList.add(new SymbolAttributesSubSet<RGB, Integer, Float, Boolean, String>(
				    			intlSigmetResourceDataObj.catColor,
				    			intlSigmetResourceDataObj.catSymbolWidth,
				    			intlSigmetResourceDataObj.catSymbolSize,  
				    			intlSigmetResourceDataObj.catEnable,
				    			"TURBULENCE_4", //refer symbolPatterns.xml
				    			intlSigmetResourceDataObj.catLineWidth)); 
				    	
					break;		    					
					
				    case IC:/*ICing*/
				    
				    	localSymbolAttributeSubsetList.add(new SymbolAttributesSubSet<RGB, Integer, Float, Boolean, String>(
				    			intlSigmetResourceDataObj.icingColor,
				    			intlSigmetResourceDataObj.icingSymbolWidth,
				    			intlSigmetResourceDataObj.icingSymbolSize,
				    			intlSigmetResourceDataObj.icingEnable,
				    			"ICING_08", //refer symbolPatterns.xml
				    			intlSigmetResourceDataObj.icingLineWidth)); 
				    	
					break;		    					
					
				    case GR:/*Hail*/
				    
				    	localSymbolAttributeSubsetList.add(new SymbolAttributesSubSet<RGB, Integer, Float, Boolean, String>(
				    			intlSigmetResourceDataObj.hailColor,
				    			intlSigmetResourceDataObj.hailSymbolWidth,
				    			intlSigmetResourceDataObj.hailSymbolSize,
				    			intlSigmetResourceDataObj.hailEnable,
				    			"PRESENT_WX_087", //refer symbolPatterns.xml
				    			intlSigmetResourceDataObj.hailLineWidth)); 
				    	
					break;		    					
					
				    case DS:/*Dust Storm*/
				    
				    	localSymbolAttributeSubsetList.add(new SymbolAttributesSubSet<RGB, Integer, Float, Boolean, String>(
				    			intlSigmetResourceDataObj.dustStormColor,
				    			intlSigmetResourceDataObj.dustStormSymbolWidth,
				    			intlSigmetResourceDataObj.dustStormSymbolSize,
				    			intlSigmetResourceDataObj.dustStormEnable,
				    			"PAST_WX_03", //refer symbolPatterns.xml
				    			intlSigmetResourceDataObj.dustStormLineWidth)); 
				    	
					break;		    					
					
				    case SS:/*Sand Storm*/
				    
				    	localSymbolAttributeSubsetList.add(new SymbolAttributesSubSet<RGB, Integer, Float, Boolean, String>(
				    			intlSigmetResourceDataObj.sandStormColor,
				    			intlSigmetResourceDataObj.sandStormSymbolWidth,
				    			intlSigmetResourceDataObj.sandStormSymbolSize,
				    			intlSigmetResourceDataObj.sandStormEnable,
				    			"PAST_WX_03", //refer symbolPatterns.xml
				    			intlSigmetResourceDataObj.sandStormLineWidth)); 
				    	
					break;		    					
					
				    case CB:/*Cumulonimbus*/
				    
				    	localSymbolAttributeSubsetList.add(new SymbolAttributesSubSet<RGB, Integer, Float, Boolean, String>(
				    			intlSigmetResourceDataObj.cumulonimbusColor,
				    			intlSigmetResourceDataObj.cumulonimbusSymbolWidth,
				    			intlSigmetResourceDataObj.cumulonimbusSymbolSize,
				    			intlSigmetResourceDataObj.cumulonimbusEnable,
				    			"CLOUD_TYPE_09", //refer symbolPatterns.xml
				    			intlSigmetResourceDataObj.cumulonimbusLineWidth)); 
				    	
					break;		    					

					
				    case WS:/*Wind Shear*/
				    
				    	localSymbolAttributeSubsetList.add(new SymbolAttributesSubSet<RGB, Integer, Float, Boolean, String>(
				    			intlSigmetResourceDataObj.lowLevelWindShearColor,
				    			intlSigmetResourceDataObj.lowLevelWindShearSymbolWidth,
				    			intlSigmetResourceDataObj.lowLevelWindShearSymbolSize,
				    			intlSigmetResourceDataObj.lowLevelWindShearEnable,
				    			"ASTERISK", /*Actually wind shear does not have any symbol associated with it*/
				    			intlSigmetResourceDataObj.lowLevelWindShearLineWidth)); 
				    	
					break;		    					
					
					
				    			    				    			    				    			    				    			    				    			    				    		    				    
				    default: /* TODO Check if this is applicable for TEST sigmets*/
				    	localSymbolAttributeSubsetList.add(new SymbolAttributesSubSet<RGB, Integer, Float, Boolean, String>(
				    			new RGB(255,255,255),
				    			2,
				    			1.5f,
				    			true,
				    			"ASTERISK", 
				    			2)); 		    				    	
					break;
				}
			}
	  
			return localSymbolAttributeSubsetList; 
	  
	  }


	/**
	 * @param bullMessage the bullMessage to set
	 */
	private void setBullMessage(String bullMessage) {
		this.bullMessage = bullMessage;
	}


	/**
	 * @param issueOfficeLatitude the issueOfficeLatitude to set
	 */
	private void setAlternateSymbolLocationLatitude(double issueOfficeLatitude) {
		this.alternateSymbolLocationLatitude = issueOfficeLatitude;
	}


	/**
	 * @param issueOfficeLongitude the issueOfficeLongitude to set
	 */
	private void setAlternateSymbolLocationLongitude(double issueOfficeLongitude) {
		this.alternateSymbolLocationLongitude = issueOfficeLongitude;
	}


	/**
	 * @param locationLookUpFailed the locationLookUpFailed to set
	 */
	private void setLocationLookUpFailed(boolean locationLookUpFailed) {
		this.locationLookUpFailed = locationLookUpFailed;
	}


	/**
	 * @param issueOffice the issueOffice to set
	 */
	private void setIssueOffice(String issueOffice) {
		this.issueOffice = issueOffice;
	}


	/**
	 * @param issueTime the issueTime to set
	 */
	private void setIssueTime(DataTime issueTime) {
		this.issueTime = issueTime;
	}

	/**
	 * @param endTime the endTime to set
	 */
	private void setEndTime(DataTime endTime) {
		this.endTime = endTime;
	}


	/**
	 * @param startTimeText the startTimeText to set
	 */
	private void setStartTimeText(String startTimeText) {
		this.startTimeText = startTimeText;
	}


	/**
	 * @param endTimeText the endTimeText to set
	 */
	private void setEndTimeText(String endTimeText) {
		this.endTimeText = endTimeText;
	}


	/**
	 * @param intensity the intensity to set
	 */
	private void setIntensity(String intensity) {
		this.intensity = intensity;
	}


	/**
	 * @param flightLevel1 the flightLevel1 to set
	 */
	private void setFlightLevel1(int flightLevel1) {
		this.flightLevel1 = flightLevel1;
	}


	/**
	 * @param flightLevel2 the flightLevel2 to set
	 */
	private void setFlightLevel2(int flightLevel2) {
		this.flightLevel2 = flightLevel2;
	}


	/**
	 * @param distance the distance to set
	 */
	private void setDistance(int distance) {
		this.distance = distance;
	}


	/**
	 * @param speed the speed to set
	 */
	private void setSpeed(int speed) {
		this.speed = speed;
	}


	/**
	 * @param direction the direction to set
	 */
	private void setDirection(String direction) {
		this.direction = direction;
	}


	/**
	 * @param messageID the messageID to set
	 */
	private void setMessageID(String messageID) {
		this.messageID = messageID;
	}


	/**
	 * @param sequenceNumber the sequenceNumber to set
	 */
	private void setSequenceNumber(String sequenceNumber) {
		this.sequenceNumber = sequenceNumber;
	}


	/**
	 * @param intlSigLocationSet the intlSigLocationSet to set
	 */
	private void setIntlSigLocationSet(Set<IntlSigmetLocation> intlSigLocationSet) {
		this.intlSigLocationSet = intlSigLocationSet;
	}


	/**
	 * @param weatherHazardEnumList the weatherHazardEnumList to set
	 */
	private void setWeatherHazardEnumList(
			ArrayList<WeatherHazardType> weatherHazardEnumList) {
		this.weatherHazardEnumList = weatherHazardEnumList;
	}


	/**
	 * @param arrayLatLonPoints the arrayLatLonPoints to set
	 */
	private void setArrayLatLonPoints(LatLonPoint[] arrayLatLonPoints) {
		this.arrayLatLonPoints = arrayLatLonPoints;
	}


	/**
	 * @param symbolAttributeSubsetList the symbolAttributeSubsetList to set
	 */
	private void setSymbolAttributeSubsetList(
			ArrayList<SymbolAttributesSubSet<RGB, Integer, Float, Boolean, String>> symbolAttributeSubsetList) {
		this.symbolAttributeSubsetList = symbolAttributeSubsetList;
	}
    }
    
    // This overrides the default which works for PluginDataObjects.
    // This method is called by queryRecords to turn the records from the database
    // into objects more suitable for the Resource
    @Override
    public IRscDataObject[] processRecord( Object isigObj ) {
    	if( !( isigObj instanceof IntlSigmetRecord) ) {
    		System.out.println("IntlSigmentResource.processRecord: object is not a "+
    				"IntlSigmetRecord: "+ isigObj.getClass().getName() );
    		return new IRscDataObject[]{};
    	}
    	IntlSigmetRecord isigRec = (IntlSigmetRecord) isigObj;
    	
    	if( !isigRec.getHazardType().contains("CANCEL") &&
        	!isigRec.getHazardType().contains("OTHER") && 
        	!isigRec.getHazardType().contains("NIL") &&
        	!isigRec.getHazardType().contains("UNKNOWN") ) {
    		
    		IntlSigmetRscDataObj intlSigRscDataObj = getCondensedIntlSigmetRecord(isigRec);
    		return new IRscDataObject[]{ intlSigRscDataObj };
    	}
    	else {
//    		isigRec.setEndTime(isigRec.getEndTime());
//    		intlsigmetCancelList.add(isigRec);
//    	}
    		return new IRscDataObject[]{};
    	}
    }
    
	/***
	 * Private class containing a list of International SIGMET records
	 * to be rendered in the current frame.
	 * @author Archana
	 *
	 */	
    private class FrameData extends AbstractFrameData{
    	private HashMap<String, IntlSigmetRscDataObj> condensedIntligLinkedHashMap; 
//      private ArrayList<IntlSigmetRecord> intlsigmetCancelList; move to resource class
        
	    /**
	     * Overridden method to determine if the International SIGMET belongs to the current frame
	     * @param dataObj - The International SIGMET record
	     * @return true if the SIGMET belongs to the current frame and false otherwise
	     */
//        @Override
//        public boolean isDataObjectInFrame( Object dataObj ) {
//        	if( !(dataObj instanceof IntlSigmetRecord ) ) {
//        		System.out.println("IntlSigmetResource:isDataObjectInFrame() processing.....\n" +
//        				"Data belongs to a different class :"+ dataObj.getClass().toString() );
//        		return false;
//        	}
//        	
//        	IntlSigmetRecord isigRec = (IntlSigmetRecord) dataObj;
//        	if(isigRec.getStartTime() != null && isigRec.getEndTime() != null){
//            	DataTime isigStartTime = new DataTime( isigRec.getStartTime() );
//            	DataTime isigEndTime   = new DataTime( isigRec.getEndTime() );
//            	
//            	// return false if the sigmet end time is before the frame start time or
//            	// if the sigmet start time is after the frame end time 
//    			if( isigStartTime.greaterThan( endTime ) ) {
//    				return false;
//    			}
//    			else if( !isigEndTime.greaterThan( startTime ) ) {
//    				return false;
//    			} 
//    			else {
//    				return true;
//    			}         		
//        	}else{
//        		return false;
//        	}
//	    }
	    
   	/**
   	 * Constructor
   	 * @param time
   	 * @param frameInterval
   	 */
    	public FrameData(DataTime time, int frameInterval) {
    		super(time, frameInterval);
    		condensedIntligLinkedHashMap = new LinkedHashMap<String, IntlSigmetRscDataObj>(0);
    	}  

   /***
      *Overridden method to sort the incoming International SIGMET based on its hazard type 
      *If the incoming SIGMET denotes a hazard of type CANCEL/NIL/UNKNOWN or OTHER,
      *it is put into a separate list after setting its end time to its 
      *start time. SIGMETs  with hazard types different from the ones mentioned above
      *are put into a HashMap, if their start time is less than the frame time  
      * and the frame time is less than its end time.
      * @param dataObj - The incoming International SIGMET
      *@return true if the SIGMET is added successfully to the HashMap and false otherwise.
      */    	
        public boolean updateFrameData( IRscDataObject rscDataObj ) {
        	if( !(rscDataObj instanceof IntlSigmetRscDataObj ) ) {
         		System.out.println("IntlSigmetResource:updateFrameData() processing.....\n" +
        				"Data belongs to a different class :"+ rscDataObj.getClass().toString() );
        		return false;
        	}
        	IntlSigmetRscDataObj intlSigmetRscDataObj = (IntlSigmetRscDataObj)rscDataObj;
        	
//        	IntlSigmetRecord isigRecord = (IntlSigmetRecord) dataObj;
//        	
//        	if(isigRecord.getHazardType().contains("CANCEL") 
//        			|| isigRecord.getHazardType().contains("OTHER") 
//        			|| isigRecord.getHazardType().contains("NIL") 
//        			|| isigRecord.getHazardType().contains("UNKNOWN") ){
//
//        		isigRecord.setEndTime(isigRecord.getEndTime());
//        		intlsigmetCancelList.add(isigRecord);
//        	}
//        	else{
//        		IntlSigmetRscDataObj condensedIntlSigmetRecord = getCondensedIntlSigmetRecord(isigRecord);
         	
    			/*(Non-Javadoc)
    			 * Since the messageId and the sequenceNumber may be null in some international sigmets, 
                 *the issue time is also added to create a key
                 **/
    			String key = intlSigmetRscDataObj.getMessageID() 
                              + intlSigmetRscDataObj.getSequenceNumber() 
                              + intlSigmetRscDataObj.getIssueTime();
    			
                if( this.condensedIntligLinkedHashMap.isEmpty() || 
                   !this.condensedIntligLinkedHashMap.containsKey(key)){

                	this.condensedIntligLinkedHashMap.put(key, intlSigmetRscDataObj);
                }        		
//        	}

        	return true;
        }    	
    }
    
    /**
     * Private parameterized class to capture the subset of the attributes of a <tt>Symbol</tt>
     * @author Archana
     *
     * @param <RGB>
     * @param <Integer>
     * @param <Float>
     * @param <Boolean>
     * @param <String>
     */
    @SuppressWarnings("hiding")
	private class SymbolAttributesSubSet<RGB, Integer, Float, Boolean, String>{
    	private RGB     symbolColor;
    	private Integer lineWidth;
    	private Float  symbolSize;
    	private Integer symbolWidth;
    	private Boolean symbolEnable;
    	private String  symbolType;
    	
    	public SymbolAttributesSubSet(RGB inSymbolColor, Integer symbolWidth, 
    			                       Float symbolSize, Boolean symbolEnable, 
    			                      String symbolType, Integer lineWidth){
    		
    		this.symbolColor  = (RGB) (inSymbolColor);
    		this.symbolWidth  = symbolWidth;
    		this.symbolSize   = symbolSize;
    		this.symbolEnable = symbolEnable;
            this.symbolType   = symbolType;
    		this.lineWidth    = lineWidth;
    		
    	}
    	
		/**
		 * @return the symbolColor
		 */
		public RGB getSymbolColor() {
			return symbolColor;
		}
		/**
		 * @return the lineWidth
		 */
		public Integer getLineWidth() {
			return lineWidth;
		}
		/**
		 * @return the symbolSize
		 */
		public Float getSymbolSize() {
			return symbolSize;
		}
		/**
		 * @return the symbolWidth
		 */
		public Integer getSymbolWidth() {
			return symbolWidth;
		}
		/**
		 * @return the symbolEnable
		 */
		public Boolean getSymbolEnable() {
			return symbolEnable;
		}
		/**
		 * @return the symbolType
		 */
		public String getSymbolType() {
			return symbolType;
		}
    }
    
    @Override
	public String getName() {
		String legendString = super.getName();
		FrameData fd = (FrameData) getCurrentFrame();
		if (fd == null || fd.getFrameTime() == null || fd.condensedIntligLinkedHashMap.size() == 0) {
			return legendString + "-No Data";
		}
		return legendString + " "+ NmapCommon.getTimeStringFromDataTime( fd.getFrameTime(), "/");
	}    
    
}

