package gov.noaa.nws.ncep.viz.rsc.solarimage.rsc;

import gov.noaa.nws.ncep.common.dataplugin.solarimage.SolarImageRecord;
import gov.noaa.nws.ncep.viz.common.ColorMapUtil;
import gov.noaa.nws.ncep.viz.localization.NcPathManager;
import gov.noaa.nws.ncep.viz.localization.NcPathManager.NcPathConstants;
import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsResource;
import gov.noaa.nws.ncep.viz.resources.INatlCntrsResource;
import gov.noaa.nws.ncep.viz.resources.colorBar.ColorBarResource;
import gov.noaa.nws.ncep.viz.resources.colorBar.ColorBarResourceData;
import gov.noaa.nws.ncep.viz.rsc.solarimage.actions.CarrLatLonCapability;
import gov.noaa.nws.ncep.viz.rsc.solarimage.actions.StonyLatLonCapability;
import gov.noaa.nws.ncep.viz.rsc.solarimage.actions.CylindricalCedCapability;
import gov.noaa.nws.ncep.viz.rsc.solarimage.display.SolarImageMatchCriteria;
import gov.noaa.nws.ncep.viz.ui.display.ColorBarFromColormap;
import gov.noaa.nws.ncep.viz.ui.display.NCNonMapDescriptor;

import java.io.File;
import java.text.SimpleDateFormat;
import java.util.List;
import java.util.Map;
import java.util.TimeZone;

import org.eclipse.swt.graphics.RGB;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.colormap.ColorMap;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.ColorMapParameters;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.IInputHandler;
import com.raytheon.uf.viz.core.rsc.IInputHandler.InputPriority;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ImagingCapability;
import com.raytheon.uf.viz.core.style.AbstractStylePreferences;
import com.raytheon.uf.viz.core.style.DataMappingPreferences;
import com.raytheon.uf.viz.core.style.LabelingPreferences;
import com.raytheon.uf.viz.core.style.MatchCriteria;
import com.raytheon.uf.viz.core.style.StyleRule;
import com.raytheon.uf.viz.core.style.StyleRuleset;
import com.raytheon.uf.viz.core.style.VizStyleException;
import com.raytheon.uf.viz.core.style.DataMappingPreferences.DataMappingEntry;
import com.raytheon.viz.core.style.image.DataScale;
import com.raytheon.viz.core.style.image.SamplePreferences;
import com.raytheon.viz.core.style.image.DataScale.Type;
import com.raytheon.viz.core.style.image.ImagePreferences;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * The resource class for Solar Image.
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer         Description
 * ------------ ---------- -----------      --------------------------
 * 02/21/2013    958       qzhou, sgurung   Initial creation
 * 03/06/2013    972       ghull            make a NatlCntrs resource on an
 *                                          NCNonMapDescriptor
 * 03/19/2013    958       qzhou, sgurung   implemented colormap and colorbar
 * 04/03/2013    958       qzhou            Added cylindrical display to updateFrameData, paintFrame, 
 * 												inspect, dispose and construct.
 * </pre>
 * 
 * @author qzhou, sgurung
 * @version 1.0
 */
public class SolarImageResource extends  
               AbstractNatlCntrsResource<SolarImageResourceData,NCNonMapDescriptor> implements 
                          INatlCntrsResource, IResourceDataChanged {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(SolarImageResource.class);
     
    private final SolarImageResourceData solarImgRscData;

    private ColorMapParameters colorMapParams;

    private boolean log10scale = false;

    protected ColorBarResource cbarResource;

    protected ResourcePair cbarRscPair;
        
    private DataScale scale = null;
    
    SolarImageDisplay imageDisplay;
    
    //sampling
    boolean sampling = false;  
    
    private Sampling samplingRsc;
    
    private IInputHandler inputAdapter = getSolarImageInputHandler();

    protected ReferencedCoordinate sampleCoord;
    
    protected static class SampleResult {

        public SampleResult() {
        }

        public String[] labels;
        public RGB[] colors;
    }
  
    // latlon overlay
    public boolean isCarrington = false;  
    
    protected boolean displayLatLonOverlay = false;
    
    private LatLonOverlay latLonOverlay;
    private LatLonCylindOverlay latLonCylindOverlay;
    
    // Cylindric projection
    private int cylindrical = 0; //0--no cylindrical, 1--stony, 2--carrington
    
    private CylindCedDisplay cylindCedDisplay;
    
    private int latLonInterval = 0;
    
	
    protected class FrameData extends AbstractFrameData {
    
    	// save only the image which best time matches to this frame.
    	// if this is the dominant resource then this will be an exact match
    	// since this record's time was used to generate the timeline.
    	// 
    	private SolarImageDisplay imageDisplay = null;
    	private CylindCedDisplay cylindCedDisplay = null;
    	   	
    	private long timeMatch=-1;
    	
    	private String legendStr = "No Data";
        private final SimpleDateFormat dateFmt = new SimpleDateFormat("yyMMdd/HHmm");
    	        
        protected FrameData(DataTime time, int interval) {
            super(time, interval);    
        	dateFmt.setTimeZone(TimeZone.getTimeZone("GMT"));
        }
        // use the dfltRecordRscDataObj which just stores the one
        // solar image record
        public boolean updateFrameData(IRscDataObject rscDataObj) {
            PluginDataObject pdo = ((DfltRecordRscDataObj) rscDataObj).getPDO();
            
            if( !(pdo instanceof SolarImageRecord) ) {
            	System.out.println("Unexpected resource data object, "+ 
            			pdo.getClass().getName()+". Expecting SolarImageRecored");
            	return false;
            }
            
            SolarImageRecord imgRec = (SolarImageRecord)pdo;
            
            long newTimeMatch = timeMatch( pdo.getDataTime() );
            
        	if( newTimeMatch < 0  ) { // sanity check.
        		return false;
        	}
 
        	/*  Add CylindCedDisplay with imageDisplay */
        	if ( imageDisplay == null ) {            
                try {                	
                	//setColorMapParametersAndColorBar();  
					imageDisplay = new SolarImageDisplay( imgRec, colorMapParams,
					         descriptor.getGridGeometry(), log10scale, scale);
					setLegendForFrame( imgRec );

				} catch (VizException e) {
					System.out.println("Error creating SolarImageDisplay:"+e.getMessage() );
					return false;
				}
                            		
                timeMatch = newTimeMatch;
                if ( imageDisplay != null && cylindCedDisplay != null)
                	return true;
            }
        		 
            if ( cylindCedDisplay == null ) {            
                try {                    	
                    //setColorMapParametersAndColorBar();  
                    cylindCedDisplay = new CylindCedDisplay( imgRec, colorMapParams,
     					         descriptor.getGridGeometry(), log10scale, scale, cylindrical, latLonInterval);
     				setLegendForFrame( imgRec );

     			} catch (VizException e) {
     				System.out.println("Error creating cylindCedDisplay:"+e.getMessage() );
     				//return false;
     			}
                                 		
                     timeMatch = newTimeMatch;
                if ( imageDisplay != null && cylindCedDisplay != null)
                     return true;
             }

            // determine if this image is a better time match than the current one.            
            if( newTimeMatch < timeMatch ) { 
            	imageDisplay.dispose();
                try {
                	//setColorMapParametersAndColorBar();  
					imageDisplay = new SolarImageDisplay( imgRec, colorMapParams,
					         descriptor.getGridGeometry(), log10scale, scale);
					setLegendForFrame( imgRec );
				} catch (VizException e) {
					System.out.println("Error creating SolarImageDisplay:"+e.getMessage() );
					return false;
				}
                            		
                //timeMatch = newTimeMatch;
                
            
           	    cylindCedDisplay.dispose();
                try {
                	//setColorMapParametersAndColorBar();  
               	 	cylindCedDisplay = new CylindCedDisplay( imgRec, colorMapParams,
					         descriptor.getGridGeometry(), log10scale, scale, cylindrical, latLonInterval);
					setLegendForFrame( imgRec );
				} catch (VizException e) {
					System.out.println("Error creating cylindCedDisplay:"+e.getMessage() );
					return false;
				}
                            		
                timeMatch = newTimeMatch;
                return true;
            }
            
//          if( imageDisplay == null ) {            
//          try {                	
//          	//setColorMapParametersAndColorBar();  
//				imageDisplay = new SolarImageDisplay( imgRec, colorMapParams,
//				         descriptor.getGridGeometry(), log10scale, scale);
//				setLegendForFrame( imgRec );
//
//			} catch (VizException e) {
//				System.out.println("Error creating SolarImageDisplay:"+e.getMessage() );
//				return false;
//			}
//                      		
//          timeMatch = newTimeMatch;
//          return true;
//      }
      
      // determine if this image is a better time match than the current one.            
//      if( newTimeMatch < timeMatch ) { 
//      	imageDisplay.dispose();
//          try {
//          	//setColorMapParametersAndColorBar();  
//				imageDisplay = new SolarImageDisplay( imgRec, colorMapParams,
//				         descriptor.getGridGeometry(), log10scale, scale, cylindrical);
//				setLegendForFrame( imgRec );
//			} catch (VizException e) {
//				System.out.println("Error creating SolarImageDisplay:"+e.getMessage() );
//				return false;
//			}
//                      		
//          timeMatch = newTimeMatch;
//            return true;
//        }
        
            return false;
        }
        
        public String getLegendForFrame() {
        	return legendStr;
        }
        
        // TODO : probably not correct : fix this as it needs to be.
        //
        public void setLegendForFrame( SolarImageRecord rec ) {
        	
        	String timeStr = dateFmt.format(rec.getDataTime().getRefTime() );
        	
        	// from nameGenerator
        	 String instru = rec.getInstrument();
             if (instru.equalsIgnoreCase("NA"))
                     instru = "";
           
             String wave = rec.getWavelength();
             if (wave.equalsIgnoreCase("NA"))
                     wave = "";

             String inttime = "";
             Double intTime = rec.getIntTime();
             if ( rec.getSatellite().startsWith("GOES"))
                     inttime = intTime.toString() + "s";

             String site = rec.getSite();
             if (site.equalsIgnoreCase("NA") || site.length() ==0)
                     site = "";
             
             legendStr = rec.getSatellite() + " " + (instru=="" ?"" :instru+" ")
                     + (site=="" ?"" :site+" ") + (wave=="" ?"" :wave+" ") + inttime +" "+timeStr;

        }
        
        @Override
        public void dispose() { 
        	if( imageDisplay != null ) {
        		imageDisplay.dispose();
        	}
        	if (cylindCedDisplay != null) {
        		cylindCedDisplay.dispose();        	
        	}
        	super.dispose();
        }
    }

    protected SolarImageResource(SolarImageResourceData resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties);
        solarImgRscData = resourceData;
        resourceData.addChangeListener(this);
        samplingRsc = new Sampling();
       
        getCapabilities().addCapability(CarrLatLonCapability.class);
        getCapabilities().addCapability(StonyLatLonCapability.class);   
        getCapabilities().addCapability(CylindricalCedCapability.class);  

        getCapability(ColorMapCapability.class).setSuppressingMenuItems(true);
        getCapability(ImagingCapability.class).setSuppressingMenuItems(true);
        getCapability(ColorableCapability.class).setSuppressingMenuItems(true);
    }

    @Override
    protected void disposeInternal() {
    	IDisplayPaneContainer container = getResourceContainer();
        if (container != null) {
            container.unregisterMouseHandler(inputAdapter);
        }
        
        // don't we want to remove this as a change listener?
        
        getDescriptor().getResourceList().remove(cbarRscPair);
        if( cbarResource.getResourceData().getColorbar() != null ){
        	cbarResource.getResourceData().getColorbar().dispose();
        	cbarResource.getResourceData().setColorBar(null);
        }
        
        super.disposeInternal();        
    }

    @Override
    public void paintFrame(AbstractFrameData frmData, IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {

        FrameData currFrame = (FrameData) frmData;

        // NOTE : this assumes that the solarImageDisplay is created
        // when the frame is updated. If this is time consuming we 
        // could always just save the record when the frame is updated and 
        // create/save the imageDisplay when on the first paint of this frame.
        
        if (cylindrical != 0) {
        	
        	cylindCedDisplay = currFrame.cylindCedDisplay;
        	cylindCedDisplay.setCylindrical(cylindrical);
        	
        	if (cylindCedDisplay != null) {
            	
                if (hasCapability(ImagingCapability.class)) {
                    ImagingCapability imaging = getCapability(ImagingCapability.class);
                    cylindCedDisplay.setBrightness(imaging.getBrightness());
                    cylindCedDisplay.setContrast(imaging.getContrast());
                    cylindCedDisplay.setInterpolationState(imaging.isInterpolationState());
                }
                
                if (hasCapability(ColorMapCapability.class)) {
                	ColorMapCapability cMap = getCapability(ColorMapCapability.class);
                	cylindCedDisplay.setColorMapParameters(cMap.getColorMapParameters());
                }
                
                cylindCedDisplay.paint(target, paintProps);
           
            	if (isSampling()) {
            		samplingRsc.paintResult(target, descriptor, paintProps, sampleCoord);
            	}
            	
            	 // TODO : draw the lat lon lines even if there is no image?
                if (isLatLonOverlayOn()) {        	
                	//int latLonInterval = 0;
                	
                	try {
	                	if (isCarrington)
	                		latLonInterval = Integer.parseInt(getCapabilities().getCapability(resourceData,
	        	                CarrLatLonCapability.class).getInterval());
	                	else
	                		latLonInterval = Integer.parseInt(getCapabilities().getCapability(resourceData,
	            	            StonyLatLonCapability.class).getInterval());
	                }
                	catch (NumberFormatException e ) {
                		
                	}
                	
                	latLonCylindOverlay = new LatLonCylindOverlay(cylindCedDisplay, descriptor, latLonInterval, paintProps);
                
                	try {
                		latLonCylindOverlay.drawLatLines(target);
                		latLonCylindOverlay.drawLonLines(target);				
        			} catch (TransformException e) {
        				statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        			}
                }       
            }
        }
        else {
        	imageDisplay = currFrame.imageDisplay;


        if (imageDisplay != null) {
        	
            if (hasCapability(ImagingCapability.class)) {
                ImagingCapability imaging = getCapability(ImagingCapability.class);
                imageDisplay.setBrightness(imaging.getBrightness());
                imageDisplay.setContrast(imaging.getContrast());
                imageDisplay.setInterpolationState(imaging.isInterpolationState());
            }
            
            if (hasCapability(ColorMapCapability.class)) {
            	ColorMapCapability cMap = getCapability(ColorMapCapability.class);
                imageDisplay.setColorMapParameters(cMap.getColorMapParameters());
            }
            
        	imageDisplay.paint(target, paintProps);
       
        	if (isSampling()) {
        		samplingRsc.paintResult(target, descriptor, paintProps, sampleCoord);
        	}
        	
        	 // TODO : draw the lat lon lines even if there is no image?
            if (isLatLonOverlayOn()) {        	
            	try {
	            	if (isCarrington)
	            		latLonInterval = Integer.parseInt(getCapabilities().getCapability(resourceData,
	    	                CarrLatLonCapability.class).getInterval());
	            	else
	            		latLonInterval = Integer.parseInt(getCapabilities().getCapability(resourceData,
	        	                StonyLatLonCapability.class).getInterval());
	            }
	        	catch (NumberFormatException e ) {
	        		
	        	}
	        	
            	latLonOverlay = new LatLonOverlay(imageDisplay, descriptor, latLonInterval, paintProps, isCarrington);
            
            	try {
            		latLonOverlay.drawLatLines(target);
    				latLonOverlay.drawLonLines(target);				
    			} catch (TransformException e) {
    				statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
    			}
            }       
        } 
        }
    }  
    
    public boolean isLatLonOverlayOn() {
        return displayLatLonOverlay;
    }

    public void setLatLonOverlay(boolean latLon) {
        this.displayLatLonOverlay = latLon;
    }
    
    public int getCylindrical() {
        return cylindrical;
    }
    
    public void setCylindrical(int cylind) {
        this.cylindrical = cylind;
    }
   
    @Override
    public void initResource(IGraphicsTarget target) throws VizException {       
    	
    	 // create the colorBar Resource and add it to the resourceList for this descriptor.
        cbarRscPair = ResourcePair
                .constructSystemResourcePair(new ColorBarResourceData(
                		solarImgRscData.getColorBar()));

        getDescriptor().getResourceList().add(cbarRscPair);
        getDescriptor().getResourceList().instantiateResources(
                getDescriptor(), true);

        cbarResource = (ColorBarResource) cbarRscPair.getResource();           
       
        setColorMapParametersAndColorBar();  
        
        if (descriptor.getRenderableDisplay().getContainer().getDisplayPanes().length > 1)
            descriptor.getTimeMatcher().redoTimeMatching(descriptor);

        IDisplayPaneContainer container = getResourceContainer();
        if (container != null) {
            container.registerMouseHandler(inputAdapter, InputPriority.RESOURCE);
        }
        
        // use the default which just gets all the images in the db regardless of 
        // the number of frames selected.     
        queryRecords();        
    }

    public String getLegendStr() {    	
    	FrameData curFrame = (FrameData)getCurrentFrame();
    	return (curFrame != null ? curFrame.getLegendForFrame() : "No Matching Data" );            
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractVizResource#inspect(com.raytheon
     * .uf.common.geospatial.ReferencedCoordinate)
     */
    @Override
    public String inspect(ReferencedCoordinate coord) throws VizException {
    	if (coord == null)
   		 return "No Data";
    	if (!sampling)
    		return "";
    	
        StringBuilder sb = new StringBuilder();
        
        if (cylindrical != 0) {
        	if (cylindrical == 1)
        		isCarrington = false;
        	else
        		isCarrington = true;
        	
        	try {
            	FrameData currFrame = (FrameData)getCurrentFrame();
            	
            	cylindCedDisplay = currFrame.cylindCedDisplay;
            	 
            	if( cylindCedDisplay == null ) {
            		return "";
            	}
            	
                sb.append("Pixel: ");
                
                double[] coordDbl = new double[2];
                coordDbl[0] = cylindCedDisplay.formatValue(coord.asLatLon().x);
                coordDbl[1] = cylindCedDisplay.formatValue(coord.asLatLon().y);
                //sb.append(coord.asLatLon().toString());
                sb.append((new Coordinate(coordDbl[0], coordDbl[1])).toString());
                sb.append('\n');
               
                Map<String, Object> map = cylindCedDisplay.interrogate(coord);

                for (Map.Entry<String, Object> item : map.entrySet()) {
                    sb.append(item.getKey() + ": ");
                    sb.append(item.getValue().toString());
                    sb.append('\n');
                }

                return sb.toString();
            } catch (TransformException e) {
                // TODO Auto-generated catch block. Please revise as appropriate.
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
            } catch (FactoryException e) {
                // TODO Auto-generated catch block. Please revise as appropriate.
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
            }
        }
        else {
        try {
        	FrameData currFrame = (FrameData)getCurrentFrame();
        	
        	imageDisplay = currFrame.imageDisplay;
        	 
        	if( imageDisplay == null ) {
        		return "";
        	}
        	
            sb.append("Pixel: ");
            
            double[] coordDbl = new double[2];
            coordDbl[0] = imageDisplay.formatValue(coord.asLatLon().x);
            coordDbl[1] = imageDisplay.formatValue(coord.asLatLon().y);
            //sb.append(coord.asLatLon().toString());
            sb.append((new Coordinate(coordDbl[0], coordDbl[1])).toString());
            sb.append('\n');
           
            Map<String, Object> map = imageDisplay.interrogate(coord);

            for (Map.Entry<String, Object> item : map.entrySet()) {
                sb.append(item.getKey() + ": ");
                sb.append(item.getValue().toString());
                sb.append('\n');
            }

            return sb.toString();
        } catch (TransformException e) {
            // TODO Auto-generated catch block. Please revise as appropriate.
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        } catch (FactoryException e) {
            // TODO Auto-generated catch block. Please revise as appropriate.
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }
        }
        return "No Data";
    }
    
    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.rsc.sampling.ISamplingResource#isSampling()
     */
    
    public IInputHandler getSolarImageInputHandler() {
        return new SolarImageInputAdapter<SolarImageResource>(this);
    }
    
    public boolean isSampling() {
        return sampling;
    }

    public void setSampling(boolean sampling) {
        this.sampling = sampling;
    }
    
    // the colorBar and/or the colormap may have changed so update the
    // colorBarPainter and the colorMapParametersCapability which holds
    // the instance of the colorMap that Raytheon's code needs
    @Override
    public void resourceAttrsModified() {
        // update the colorbarPainter with a possibly new colorbar
        ColorBarFromColormap colorBar = solarImgRscData.getColorBar();

        ColorMapParameters cmapParams = getCapability(ColorMapCapability.class)
                .getColorMapParameters();
        cmapParams.setColorMap(colorBar.getColorMap());
        cmapParams.setColorMapName(solarImgRscData.getColorMapName());
        // not currently an attribute but could be.
        cmapParams.setDisplayUnit(solarImgRscData.getDisplayUnit());

        getCapability(ColorMapCapability.class).setColorMapParameters(
                cmapParams);
        cbarResource.setColorBar(colorBar);

    }

    @Override
    protected AbstractFrameData createNewFrame(DataTime frameTime,
            int frameInterval) {
        return new FrameData(frameTime, frameInterval);
    }

	@Override
	public void resourceChanged(ChangeType type, Object object) {
		// Don't need this for a DATA_UPDATE but may need it 
		// for an image or colormap capability changed.
		if (type != null && type == ChangeType.CAPABILITY) {
            if (object instanceof ImagingCapability) {
                ImagingCapability imgCap = getCapability(ImagingCapability.class);
                ImagingCapability newImgCap = (ImagingCapability) object;
                imgCap.setBrightness(newImgCap.getBrightness(), false);
                imgCap.setContrast(newImgCap.getContrast(), false);
                imgCap.setAlpha(newImgCap.getAlpha(), false);
                solarImgRscData.setAlpha(imgCap.getAlpha());
                solarImgRscData.setBrightness(imgCap.getBrightness());
                solarImgRscData.setContrast(imgCap.getContrast());
                issueRefresh();

            } else if (object instanceof ColorMapCapability) {

                ColorMapCapability colorMapCap = getCapability(ColorMapCapability.class);
                ColorMapCapability newColorMapCap = (ColorMapCapability) object;
                colorMapCap.setColorMapParameters(
                        newColorMapCap.getColorMapParameters(), false);
                ColorMap theColorMap = (ColorMap) colorMapCap
                        .getColorMapParameters().getColorMap();
                String colorMapName = colorMapCap.getColorMapParameters()
                        .getColorMapName();
                solarImgRscData.setColorMapName(colorMapName);
                solarImgRscData.getRscAttrSet().setAttrValue("colorMapName",
                        colorMapName);

                ColorBarFromColormap cBar = solarImgRscData.getColorBar();
                cBar.setColorMap(theColorMap);
                ColorBarFromColormap colorBar = (ColorBarFromColormap) this.cbarResource
                        .getResourceData().getColorbar();
                if (colorBar != null) {
                    if (colorBar.getImagePreferences() != null
                            && cBar.getImagePreferences() == null) {
                        cBar.setImagePreferences(colorBar.getImagePreferences());
                    }

                    cBar.setIsScalingAttemptedForThisColorMap(colorBar
                            .isScalingAttemptedForThisColorMap());
                    cBar.setNumPixelsToReAlignLabel(colorBar
                            .isAlignLabelInTheMiddleOfInterval());
                }
                solarImgRscData.getRscAttrSet().setAttrValue("colorBar", cBar);
                solarImgRscData.setIsEdited(true);
                issueRefresh();

            }

        }
	}
	
	@Override
    public void propertiesChanged(ResourceProperties updatedProps) {
    	if( cbarRscPair != null ) {
    		cbarRscPair.getProperties().setVisible( updatedProps.isVisible() );
    	}
    }
	
	private String getLocFilePathForImageryStyleRule(){
    	return NcPathConstants.SOLAR_IMG_STYLE_RULES;
    }
	
		private void setColorMapParametersAndColorBar() throws VizException {
		 	double minPixVal = Double.NaN;
	        double maxPixVal = Double.NaN;

		    ColorMap colorMap = null;
		     
	        DataMappingPreferences dataMap = null;
	        LabelingPreferences labelPrefs = null;	    
	        
	        colorMapParams = getCapability(ColorMapCapability.class)
            .getColorMapParameters();        
      
		    try {
		        colorMap = (ColorMap) ColorMapUtil.loadColorMap(
		        		solarImgRscData.getResourceName().getRscCategory().getCategoryName(), (solarImgRscData.getColorMapName()==null?"Gray":solarImgRscData.getColorMapName()));
		    } catch (VizException e) {
		        throw new VizException("Error loading colormap: "
		                + solarImgRscData.getColorMapName());
		    }
		    
		    colorMapParams.setColorMap(colorMap);
		    getCapability(ColorMapCapability.class).setColorMapParameters(
		    		colorMapParams);    
	    	
		 	
	        ColorBarFromColormap colorBar = (ColorBarFromColormap) this.cbarResource
	                .getResourceData().getColorbar();
	        if (colorBar.getColorMap() == null) {
	            colorBar.setColorMap((ColorMap) getCapability(
	                    ColorMapCapability.class).getColorMapParameters()
	                    .getColorMap());
	        }
	        
	        MatchCriteria matchCriteria = 
	        	SolarImageMatchCriteria.constructFromResourceData( solarImgRscData );
	        
	        ImagePreferences imgPref = new ImagePreferences();
	        String locFileName = getLocFilePathForImageryStyleRule();
	        //matchCriteria.setParameterName(parameterList);

	        File file = NcPathManager.getInstance().getStaticFile(locFileName);
	        StyleRule sRule = null;
	        try {
	            @SuppressWarnings("deprecation")
				StyleRuleset styleSet = (StyleRuleset) SerializationUtil
	                    .jaxbUnmarshalFromXmlFile(file);

	            if (styleSet != null) {
	                List<StyleRule> styleRuleList = styleSet.getStyleRules();

	                for (StyleRule sr : styleRuleList) {
	                    MatchCriteria styleMatchCriteria = sr.getMatchCriteria();
	                    if (styleMatchCriteria.matches(matchCriteria) > 0) {

	                        AbstractStylePreferences stylePref = sr
	                                .getPreferences();
	                        if (stylePref != null
	                                && stylePref instanceof ImagePreferences) {
	                            imgPref = (ImagePreferences) stylePref;
	                            /*
	                             * Might need to change this if/when we use the
	                             * data-scaling
	                             */
	                            SamplePreferences samplePref = imgPref
	                                    .getSamplePrefs();
	                            if (samplePref != null) {
	                                minPixVal = imgPref.getSamplePrefs()
	                                        .getMinValue();
	                                maxPixVal = imgPref.getSamplePrefs()
	                                        .getMaxValue();
	                            } else if (imgPref.getDataScale() != null) {
	                                DataScale ds = imgPref.getDataScale();
	                                if (ds.getMaxValue() != null)
	                                    maxPixVal = ds.getMaxValue().doubleValue();
	                                if (ds.getMinValue() != null)
	                                    minPixVal = ds.getMinValue().doubleValue();
	                            }

	                            colorBar.setImagePreferences(imgPref);
	                            if (imgPref.getDisplayUnitLabel() != null) {
	                                colorBar.setDisplayUnitStr(imgPref
	                                        .getDisplayUnitLabel());

	                            }
	                            sRule = sr;
	                            break;
	                        }

	                    }

	                }

	            }
	        } catch (SerializationException e1) {

	            e1.printStackTrace();
	        } catch (VizStyleException e1) {

	            e1.printStackTrace();
	        } catch (NullPointerException e1) {

	            e1.printStackTrace();
	        }
	        
	        scale = ((ImagePreferences) sRule.getPreferences()).getDataScale();
	        dataMap = ((ImagePreferences) sRule.getPreferences()).getDataMapping();
	        labelPrefs = ((ImagePreferences) sRule.getPreferences())
	        .getColorbarLabeling();
             
//	        StyleRule sr = StyleManager.getInstance().getStyleRule(
//	    	                StyleManager.StyleType.IMAGERY, matchCriteria);        

            DataMappingPreferences dmPref = new DataMappingPreferences();
            if (imgPref.getDataMapping() != null) {        	
	        	
	        	dmPref = imgPref.getDataMapping();
	        	
	        	List<DataMappingEntry> dmEntriesList = dmPref.getEntries();
	        	
	        	if ( dmEntriesList.size() > 0 ){
	    		    DataMappingEntry[] dmEntryArray = new DataMappingEntry[dmEntriesList.size()];
	    		    dmEntriesList.toArray(dmEntryArray);
	     		    dmPref.setSerializableEntries(dmEntryArray);
	     		    imgPref.setDataMapping(dmPref);
	     		    SamplePreferences sampPref = new SamplePreferences();
	     		    sampPref.setMinValue(0);
	     		    sampPref.setMaxValue(255);
	     		    imgPref.setSamplePrefs(sampPref);	
	     		    colorBar.setImagePreferences(imgPref);
	     		    //colorBar.setDisplayUnitStr(solarImgRscData.getDisplayUnitStr());
	     		    colorBar.setAlignLabelInTheMiddleOfInterval(false);
	    		}	
	        	
	        }
            else if (minPixVal >=0 && maxPixVal <= 255) {
            	
                double ratio = (maxPixVal - minPixVal) / 255;
                DataMappingEntry dmEntry = new DataMappingEntry();
                double level = -1;
                for (int ii = 0; ii <= 255; ii++) {
                   if(ii%((int)(255/maxPixVal))==0 ) {
                	   level = Math.round((ii - 1) * ratio) + minPixVal;
                	   dmEntry = new DataMappingEntry();
                	   dmEntry.setPixelValue((double) ii);
                       dmEntry.setDisplayValue(level);                   
                       dmEntry.setLabel(Double.toString(level));
                       dmPref.addEntry(dmEntry);
                    }
                }
                
            }
            else {
            	if (labelPrefs !=null) {
	            	float[] labelValues = labelPrefs.getValues();
	            	int size = labelPrefs.getValues().length;
	            	
	            	 DataMappingEntry dmEntry = new DataMappingEntry();	                 
	                 dmEntry = new DataMappingEntry();
               	     dmEntry.setPixelValue(0.0);
                     dmEntry.setDisplayValue(minPixVal);                   
                     dmEntry.setLabel(Double.toString(minPixVal));
                     dmPref.addEntry(dmEntry);
                      
                     int count = 0;
	                 double level = -1;
	                 for (int ii = 1; ii < 255; ii++) {
	                    if(ii%((int)(255/(size+1)))==0 && count < size) {
	                 	   level = labelValues[count];
	                 	   dmEntry = new DataMappingEntry();
	                 	   dmEntry.setPixelValue((double) ii);
	                        dmEntry.setDisplayValue(level);                   
	                        dmEntry.setLabel(Double.toString(level));
	                        dmPref.addEntry(dmEntry);
	                        count++;
	                     }
	                 }
	                 
	                 dmEntry = new DataMappingEntry();
               	     dmEntry.setPixelValue(255.0);
                     dmEntry.setDisplayValue(maxPixVal);                   
                     dmEntry.setLabel(Double.toString(maxPixVal));
                     dmPref.addEntry(dmEntry);
            	}
            }
            
            if (!colorBar.isScalingAttemptedForThisColorMap()) {
                imgPref = new ImagePreferences();
                imgPref.setDataMapping(dmPref);
                SamplePreferences sPref = new SamplePreferences();
                sPref.setMaxValue(255);
                sPref.setMinValue(0);
                imgPref.setSamplePrefs(sPref);
                colorBar.setImagePreferences(imgPref);
                colorBar.scalePixelValues();
            }
	            
	        colorBar.setAlignLabelInTheMiddleOfInterval(false); 
	      
		    if (!colorBar.equals(solarImgRscData.getColorBar())) {
	            this.solarImgRscData.setColorBar(colorBar);

	        }
	        this.cbarResource.getResourceData().setColorBar(colorBar);

	        if (dataMap != null)
	            colorMapParams.setDataMapping(dataMap);

	        if (labelPrefs != null && labelPrefs.getValues() != null)
	            colorMapParams.setColorBarIntervals(labelPrefs.getValues());
	       
	        if (scale != null) {
	            colorMapParams.setColorMapMin(scale.getMinValue().floatValue());
	            colorMapParams.setColorMapMax(scale.getMaxValue().floatValue());
	            colorMapParams.setDataMin(scale.getMinValue().floatValue());
	            colorMapParams.setDataMax(scale.getMaxValue().floatValue());
	            log10scale = true;
	            if (scale.getScaleType() != null
	                    && scale.getScaleType() == Type.LINEAR)
	                log10scale = false;
	        } else {
	            colorMapParams.setColorMapMin(0.0f);
	            colorMapParams.setColorMapMax(255.0f);
	            colorMapParams.setDataMin(0.0f);
	            colorMapParams.setDataMax(255.0f);
	            log10scale = true;
	        }	        
		}


}
