package gov.noaa.nws.ncep.viz.rsc.solarimage.rsc;

import gov.noaa.nws.ncep.common.dataplugin.solarimage.SolarImageRecord;
import gov.noaa.nws.ncep.viz.rsc.solarimage.actions.CarrLatLonCapability;
import gov.noaa.nws.ncep.viz.rsc.solarimage.actions.StonyLatLonCapability;
import gov.noaa.nws.ncep.viz.rsc.solarimage.display.SolarImageMatchCriteria;

import java.text.SimpleDateFormat;
import java.util.HashMap;
import java.util.Map;
import java.util.TimeZone;

import org.eclipse.swt.graphics.RGB;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.colormap.ColorMap;
import com.raytheon.uf.common.colormap.IColorMap;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.ColorMapLoader;
import com.raytheon.uf.viz.core.drawables.ColorMapParameters;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IInputHandler;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.IInputHandler.InputPriority;
import com.raytheon.uf.viz.core.rsc.capabilities.Capabilities;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ImagingCapability;
import com.raytheon.uf.viz.core.style.DataMappingPreferences;
import com.raytheon.uf.viz.core.style.LabelingPreferences;
import com.raytheon.uf.viz.core.style.MatchCriteria;
import com.raytheon.uf.viz.core.style.StyleManager;
import com.raytheon.uf.viz.core.style.StyleRule;
import com.raytheon.uf.viz.xy.graph.XyGraphDescriptor;
import com.raytheon.viz.core.style.image.DataScale;
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
 * 02/21/2013   958        qzhou, sgurung   Initial creation
 * 
 * </pre>
 * 
 * @author qzhou, sgurung
 * @version 1.0
 */

public class SolarImageResource extends
        AbstractVizResource<SolarImageResourceData, XyGraphDescriptor> implements
        IResourceDataChanged {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(SolarImageResource.class);

    private static final String DATE_TIME_FORMAT = "yyMMdd/HHmm";

    private static final SimpleDateFormat sdf = new SimpleDateFormat(
            DATE_TIME_FORMAT);

    private ColorMapParameters colorMapParams;

    private DataTime displayedDateTime;

    private HashMap<DataTime, SolarImageDisplay> imageDisplayMap;

    private boolean log10scale = false;
    
    public boolean isCarrington;  
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
  
    protected boolean displayLatLonOverlay = false;
    
    private LatLonOverlay latLonOverlay;
     
    protected SolarImageResource(SolarImageResourceData resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties);
        resourceData.addChangeListener(this);
        imageDisplayMap = new HashMap<DataTime, SolarImageDisplay>();
        samplingRsc = new Sampling();
        System.out.println("isCarrington = " +isCarrington); 
        
        getCapabilities().addCapability(CarrLatLonCapability.class);
        getCapabilities().addCapability(StonyLatLonCapability.class);
    }

    @Override
    protected void disposeInternal() {
    	IDisplayPaneContainer container = getResourceContainer();
        if (container != null) {
            container.unregisterMouseHandler(inputAdapter);
        }

        for (SolarImageDisplay idisplay : imageDisplayMap.values()) {
            if (idisplay != null)
                idisplay.dispose();
        }
        imageDisplayMap.clear();
    }

    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {

        displayedDateTime = paintProps.getDataTime();

        SolarImageRecord record = resourceData.dataObjectMap.get(displayedDateTime);

        if (record == null) {
            // Don't have data for this frame
            return;
        }

        SolarImageDisplay imageDisplay = imageDisplayMap.get(displayedDateTime);

        if (imageDisplay == null) {

            imageDisplay = new SolarImageDisplay(record, colorMapParams,
                    descriptor.getGridGeometry(), log10scale);
            imageDisplayMap.put(displayedDateTime, imageDisplay);
        }

        if (hasCapability(ImagingCapability.class)) {
            ImagingCapability imaging = getCapability(ImagingCapability.class);
            imageDisplay.setBrightness(imaging.getBrightness());
            imageDisplay.setContrast(imaging.getContrast());
            imageDisplay.setInterpolationState(imaging.isInterpolationState());
        }

        imageDisplay.paint(target, paintProps);

        if (isSampling()) {
        	samplingRsc.paintResult(target, descriptor, paintProps, sampleCoord);
        }
        
        if (isLatLonOverlayOn()) {        	
        	int latLonInterval = 0;
        	if (isCarrington)
        		latLonInterval = Integer.parseInt(getCapabilities().getCapability(resourceData,
	                CarrLatLonCapability.class).getInterval());
        	else
        		latLonInterval = Integer.parseInt(getCapabilities().getCapability(resourceData,
    	                StonyLatLonCapability.class).getInterval());
        	
        	latLonOverlay = new LatLonOverlay(imageDisplay, descriptor, latLonInterval, paintProps, isCarrington);
        	
        	try {
        		latLonOverlay.drawLatLines(target);
				latLonOverlay.drawLonLines(target);				
			} catch (TransformException e) {
				statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
			}
        }       
  
    }  
    
    public boolean isLatLonOverlayOn() {
        return displayLatLonOverlay;
    }

    public void setLatLonOverlay(boolean latLon) {
        this.displayLatLonOverlay = latLon;
    }
    
    
    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        // TODO Auto-generated method stub
       
        String colormapfile = null;
        DataMappingPreferences dataMap = null;
        DataScale scale = null;
        LabelingPreferences labelPrefs = null;

        colorMapParams = getCapability(ColorMapCapability.class)
                .getColorMapParameters();

        MatchCriteria criteria = SolarImageMatchCriteria
                .constructFromResourceData(resourceData);
        StyleRule sr = StyleManager.getInstance().getStyleRule(
                StyleManager.StyleType.IMAGERY, criteria);
        if (sr != null) {
            colormapfile = ((ImagePreferences) sr.getPreferences())
                    .getDefaultColormap();
            scale = ((ImagePreferences) sr.getPreferences()).getDataScale();
            dataMap = ((ImagePreferences) sr.getPreferences()).getDataMapping();
            labelPrefs = ((ImagePreferences) sr.getPreferences())
                    .getColorbarLabeling();
        }
        if (colormapfile == null) {
            colormapfile = colorMapParams.getColorMapName();
            if (colormapfile == null)
                colormapfile = "solar/Gray";
        }

        IColorMap cxml = ColorMapLoader.loadColorMap(colormapfile);
        ColorMap colorMap = new ColorMap(colormapfile, (ColorMap) cxml);
        colorMapParams.setColorMap(colorMap);

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
            colorMapParams.setColorMapMax(5.0f);
            colorMapParams.setDataMin(0.0f);
            colorMapParams.setDataMax(5.0f);
            log10scale = true;
        }

        if (descriptor.getRenderableDisplay().getContainer().getDisplayPanes().length > 1)
            descriptor.getTimeMatcher().redoTimeMatching(descriptor);

        IDisplayPaneContainer container = getResourceContainer();
        if (container != null) {
            container
                    .registerMouseHandler(inputAdapter, InputPriority.RESOURCE);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.rsc.AbstractVizResource#getName()
     */
    @Override
    public String getName() {
        SolarImageRecord rec = resourceData.dataObjectMap.get(displayedDateTime);
        if (rec == null)
        	return super.getName() + " -No Data";
        // String dateTime = sdf.format(rec.getObservationTime().getTime());
        String dateTime = null;
        if (displayedDateTime == null) {
            dateTime = super.getName() + " -No Data";
        } else {
            sdf.setTimeZone(TimeZone.getTimeZone("GMT"));
            dateTime = sdf.format(displayedDateTime.getRefTime());
        }

        return super.getName() +" "+ getLegend(rec) + " " + dateTime;
            
    }
    private String getLegend(SolarImageRecord record) {
    	String legendStr = "";
        String instru = record.getInstrument();
        if (instru.equalsIgnoreCase("NA"))
        	instru = "";
        else if (instru.startsWith("AIA"))
        	instru = "AIA";
        
        String wave = record.getWavelength();
        if (wave.equalsIgnoreCase("NA"))
        	wave = "";
        
        String inttime = "";
        Double intTime = record.getIntTime();
        if ( record.getSatellite().startsWith("GOES"))
        	inttime = intTime.toString();

        String site = record.getSite();
        if (site.equalsIgnoreCase("NA") || site.length() ==0)
        	site = "";
               
        legendStr = record.getSatellite() + " " + (instru=="" ?"" :instru+" ")
        	+ (site=="" ?"" :site+" ") + (wave=="" ?"" :wave+" ") + inttime;
        
        return legendStr;
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
    	
        StringBuilder sb = new StringBuilder();
        try {
        	
        	SolarImageDisplay imageDisplay = imageDisplayMap
             .get(displayedDateTime);
        	 
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
        return "No Data";
    }

    @Override
    public void resourceChanged(ChangeType type, Object object) {
        if (type.equals(ChangeType.DATA_UPDATE)) {
            PluginDataObject[] pdos = (PluginDataObject[]) object;
            for (PluginDataObject pdo : pdos) {
                try {
                    SolarImageRecord solarimagerec = (SolarImageRecord) pdo;
                    resourceData.dataObjectMap.put(solarimagerec.getDataTime(),
                            solarimagerec);
                } catch (Exception e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Error updating Solar Image resource", e);
                }
            }
        }
        issueRefresh();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractVizResource#remove(com.raytheon.
     * uf.common.time.DataTime)
     */
    @Override
    public void remove(DataTime dataTime) {
        System.out.println("REMOVING: " + dataTime);
        super.remove(dataTime);
        SolarImageDisplay display = this.imageDisplayMap.remove(dataTime);
        if (display != null) {
            display.dispose();
        }
    }
    
    /**
     * Get and load the style rule
     * 
     * @return
     */
    // public SolarImageImageMatchCriteria getMatchCriteria() {
    //
    // RequestConstraint rc = resourceData.getMetadataMap().get("wavelength");
    // String wavelen = rc.getConstraintValue();
    // rc = resourceData.getMetadataMap().get("intTime");
    // String inttime = rc.getConstraintValue();
    //
    // SolarImageMatchCriteria match = new SolarImageMatchCriteria();
    // match.setWavelength(wavelen);
    // match.setIntTime(inttime);
    // return match;
    // }

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
   
    
}
