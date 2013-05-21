/*
 * gov.noaa.nws.ncep.viz.rsc.ncradar.rsc.graphic.RadarGraphicsResource
 * 
 * 12-16-2011
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.viz.rsc.ncradar.rsc.graphic;

import java.io.FileNotFoundException;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import org.eclipse.swt.graphics.RGB;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.colormap.ColorMap;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.radar.RadarDataKey;
import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.TextStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.catalog.LayerProperty;
import com.raytheon.uf.viz.core.catalog.ScriptCreator;
import com.raytheon.uf.viz.core.comm.Connector;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceType;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.IMiddleClickCapableResource;
import com.raytheon.uf.viz.core.rsc.capabilities.ImagingCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.MagnificationCapability;
import com.raytheon.viz.radar.IRadarConfigListener;
import com.raytheon.viz.radar.interrogators.IRadarInterrogator;

import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsResource.AbstractFrameData;
import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsResource.DfltRecordRscDataObj;
import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsResource.IRscDataObject;
import gov.noaa.nws.ncep.viz.rsc.ncradar.VizRadarRecord;
import gov.noaa.nws.ncep.viz.rsc.ncradar.rsc.AbstractRadarResource;
import gov.noaa.nws.ncep.viz.rsc.ncradar.rsc.RadarImageResource;
import gov.noaa.nws.ncep.viz.rsc.ncradar.rsc.RadarResourceData;
import gov.noaa.nws.ncep.viz.rsc.ncradar.rsc.RadarTileSet;
import gov.noaa.nws.ncep.viz.ui.display.ColorBarFromColormap;

import com.raytheon.viz.radar.ui.RadarDisplayManager;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;

/**
 * TODO Add Description
 * 
 * This class is based on Raytheon's code.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 12/16/2011   #541       S. Gurung   Initial Creation
 * 03/30/2012   #651       S. Gurung   Fixed NullPointerException bug in resourceChanged
 * 
 * </pre>
 * 
 * @author sgurung
 * @version 1.0
 */

public class RadarGraphicsResource extends AbstractRadarResource<MapDescriptor>
        implements IMiddleClickCapableResource, IRadarConfigListener {

    private Map<DataTime, RadarGraphicsDisplay> radarGraphicsDisplay;

    private RadarGraphicsDisplay rgd;

    public Set<String> filteredStormIds = null;

    protected Map<DataTime, RGB> lastColor = new HashMap<DataTime, RGB>();
    
    protected IGraphicsTarget grphTarget;

    protected String viewType;
    
    protected HashMap<String, RequestConstraint> queryList;	

    /**
     * @param rrd
     * @param loadProps
     * @throws VizException
     */
    public RadarGraphicsResource(RadarResourceData rrd,
            LoadProperties loadProps, IRadarInterrogator interrogator)
            throws VizException {
        super(rrd, loadProps, interrogator);
        radarGraphicsDisplay = new HashMap<DataTime, RadarGraphicsDisplay>();
        RadarDisplayManager.getInstance().addListener(this);
    }

    protected class FrameData extends AbstractFrameData {

		RadarTileSet tileSet;
		DataTime     tileTime; // the time of the data used to create the tileset
		                       // used to determine if we need to replace the tile 
		                       // with a better timematch. 
		
		public FrameData(DataTime time, int interval) {
			super(time, interval);	
			tileTime = null;
		}

		public boolean updateFrameData( IRscDataObject rscDataObj ) {

			if( !(rscDataObj instanceof DfltRecordRscDataObj) ) {
				System.out.println("Unrecognized Radar Image");
				return false;
			}
        	PluginDataObject pdo = ((DfltRecordRscDataObj)rscDataObj).getPDO();
			RadarRecord radarRecord = (RadarRecord) pdo;
			RadarGraphicsResource.this.addRecord(pdo);//TO Be Tested: 2011-03-05
	       
			VizRadarRecord rtr = radarRecords.get(pdo.getDataTime());

			/*try{
				setColorMapParameters( radarRecord,rtr );
			}
			catch (FileNotFoundException e) {
				e.printStackTrace();
			} catch (StorageException e) {
				e.printStackTrace();
			} catch (VizException e) {
				e.printStackTrace();
			}*/
			return true;
		}
		
		public void dispose() {
			if( tileSet != null ) {
				tileSet.dispose();
				tileSet = null;
			}
		}
	}
    
    /*
     * (non-Javadoc)
     * 
     * @see gov.noaa.nws.ncep.viz.src.rsc.ncradar.rsc.AbstractRadarResource#disposeInternal()
     */
    @Override
    protected void disposeInternal() {
        super.disposeInternal();

        synchronized (radarGraphicsDisplay) {
            for (RadarGraphicsDisplay display : radarGraphicsDisplay.values()) {
                display.dispose();
            }
            radarGraphicsDisplay.clear();
        }
        RadarDisplayManager.getInstance().removeListener(this);
    }

    @Override
    public void middleClicked() throws VizException {
        if (radarGraphicsDisplay.get(displayedDate) != null) {
            RadarGraphicsDisplay display = radarGraphicsDisplay
                    .get(displayedDate);
            int max = display.getNumPages();
            int pg = display.getCurrentPage();
            pg++;

            if (pg >= max) {
                pg = 0;
            }

            display.setCurrentPage(pg);

            issueRefresh();
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * gov.noaa.nws.ncep.viz.src.rsc.ncradar.rsc.AbstractRadarResource#resourceChanged(com.raytheon
     * .uf.viz.core.rsc.IResourceDataChanged.ChangeType, java.lang.Object)
     */
    @Override
    public void resourceChanged(ChangeType type, Object object) {
    	RadarResourceData radarRscData = ( RadarResourceData ) this.resourceData;
        //super.resourceChanged(type, object);
        if (type == ChangeType.CAPABILITY) {
            if (object instanceof MagnificationCapability) {
                double mag = ((MagnificationCapability) object)
                        .getMagnification();
                for (RadarGraphicsDisplay display : this.radarGraphicsDisplay
                        .values()) {
                    display.setMagnification(mag);
                }
            }
            
            else if (object instanceof ImagingCapability ){
        		ImagingCapability imgCap = getCapability(ImagingCapability.class);
           		ImagingCapability newImgCap = ( ImagingCapability ) object;
        		imgCap.setBrightness(newImgCap.getBrightness(), false);
        		imgCap.setContrast(newImgCap.getContrast(), false);
        		imgCap.setAlpha(newImgCap.getAlpha(), false);
        		radarRscData.setAlpha(  imgCap.getAlpha()  );
                radarRscData.setBrightness(  imgCap.getBrightness() );
                radarRscData.setContrast(  imgCap.getContrast() );
       		
        		
        	}
        	else if (object instanceof ColorMapCapability ){
        		
        		ColorMapCapability colorMapCap = getCapability(ColorMapCapability.class);
        		ColorMapCapability newColorMapCap = (ColorMapCapability) object;
        		if (newColorMapCap.getColorMapParameters() != null) {
	        		colorMapCap.setColorMapParameters(newColorMapCap.getColorMapParameters(), false);
	        		ColorMap theColorMap = ( ColorMap ) colorMapCap.getColorMapParameters().getColorMap();
	        		String colorMapName = colorMapCap.getColorMapParameters().getColorMapName();
	        		radarRscData.setColorMapName( colorMapName );
	        	    radarRscData.getRscAttrSet().setAttrValue( "colorMapName", colorMapName );
	        	    ColorBarFromColormap cBar = radarRscData.getColorBar();
	        	    cBar.setColorMap( theColorMap );
	        	    radarRscData.getRscAttrSet().setAttrValue( "colorBar", cBar );
	        	    radarRscData.setIsEdited( true );
        		}
        	}

            
            issueRefresh();
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * gov.noaa.nws.ncep.viz.src.rsc.ncradar.rsc.AbstractRadarResource#getUpperText(com.
     * raytheon .uf.common.time.DataTime)
     */
    @Override
    public String[] getUpperText(DataTime time) {
        // Upper text would interfere with the table
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * gov.noaa.nws.ncep.viz.src.rsc.ncradar.rsc.AbstractRadarResource#paintInternal(com
     * .raytheon .uf.viz.core.IGraphicsTarget,
     * com.raytheon.uf.viz.core.drawables.PaintProperties)
     */
    @Override
	protected void paintFrame(AbstractFrameData frameData,
			   IGraphicsTarget target, PaintProperties paintProps ) throws VizException{
    	
        displayedDate = null;

        if ((paintProps == null) || (paintProps.getDataTime() == null)) {
            return;
        }

        displayedDate = paintProps.getDataTime();

        displayedLevel = displayedDate.getLevelValue().floatValue();

        RadarRecord radarRecord = getRadarRecord(displayedDate);
        if (radarRecord == null) {
            return;
        }

        synchronized (radarGraphicsDisplay) {
            if ((radarGraphicsDisplay.get(displayedDate) == null)
                    || lastColor.get(paintProps.getDataTime()) != getCapability(
                            ColorableCapability.class).getColor()) {
                lastColor.put(paintProps.getDataTime(),
                        getCapability(ColorableCapability.class).getColor());
                rgd = new RadarGraphicsDisplay(radarRecord, target, getNcMapDescriptor(),
                        getFilteredStormIds(), getCapability(
                                MagnificationCapability.class)
                                .getMagnification(), lastColor.get(paintProps
                                .getDataTime()));
                radarGraphicsDisplay.put(displayedDate, rgd);
            }

            if (radarGraphicsDisplay.get(displayedDate) != null) {
                radarGraphicsDisplay.get(displayedDate).paint(target,
                        paintProps);
            }
        }
        // DMD specify
        if (radarRecord.getProductCode() == 149) {
            target.clearClippingPlane();
            int xPos = 95;
            int yPos = 10;
            xPos = paintProps.getCanvasBounds().x
                    + paintProps.getCanvasBounds().width - xPos;

            double[] pts = paintProps.getView().getDisplayCoords(
                    new double[] { xPos, yPos }, target);

            // Get the Lat/Lon of the screen Extent
            Envelope screenLatLon = getNcMapDescriptor().pixelToWorld(paintProps
                    .getView().getExtent());

            int offScreenCount = 0;
            int filteredCount = 0;
            Coordinate currFeature;
            for (RadarDataKey currPt : radarRecord.getSymbologyData().keySet()) {
                currFeature = new Coordinate(currPt.getLon(), currPt.getLat());

                if (!screenLatLon.contains(currFeature)) {
                    // Count how many are not on the screen
                    offScreenCount++;
                } else if (!radarRecord.getSymbologyData().get(currPt)
                        .isVisible()) {
                    // Count how many are not visible, that would be on the
                    // screen
                    filteredCount++;
                }
            }

            target.drawString(null, offScreenCount + " FEATURES OFF SCREEN",
                    pts[0], pts[1], 0, TextStyle.NORMAL,
                    this.getCapability(ColorableCapability.class).getColor(),
                    HorizontalAlignment.CENTER, VerticalAlignment.MIDDLE, 0.0);

            pts = paintProps.getView().getDisplayCoords(
                    new double[] { xPos, yPos + 20 }, target);

            target.drawString(null, filteredCount + " FEATURES NOT SHOWN",
                    pts[0], pts[1], 0, TextStyle.NORMAL,
                    this.getCapability(ColorableCapability.class).getColor(),
                    HorizontalAlignment.CENTER, VerticalAlignment.MIDDLE, 0.0);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * gov.noaa.nws.ncep.viz.src.rsc.ncradar.rsc.AbstractRadarResource#project(org.opengis.referencing
     * .crs.CoordinateReferenceSystem)
     */
    @Override
    public void project(CoordinateReferenceSystem mapData) throws VizException {
        updateConfig();
    }

    /**
     * Used by SCAN
     * 
     * @return
     */
    public Set<String> getFilteredStormIds() {
        return filteredStormIds;
    }

    /**
     * @return the radarGraphicsDisplay
     */
    public Map<DataTime, RadarGraphicsDisplay> getRadarGraphicsDisplay() {
        return radarGraphicsDisplay;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.radar.IRadarConfigListener#updateConfig()
     */
    @Override
    public void updateConfig() {
        VizApp.runAsync(new Runnable() {

            @Override
            public void run() {
                synchronized (radarGraphicsDisplay) {
                    for (RadarGraphicsDisplay display : radarGraphicsDisplay
                            .values()) {
                        display.dispose();
                    }
                    radarGraphicsDisplay.clear();
                }
                issueRefresh();
            }
        });

    }

	@Override
	public void initResource(IGraphicsTarget target) throws VizException {
		synchronized (this) {
			//super.initResource( target );
			
			this.viewType = target.getViewType();
            this.grphTarget = target;            
			queryRecords();       	
		}	
		
	}

	@Override
	protected gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsResource.AbstractFrameData createNewFrame(
			DataTime frameTime, int frameInterval) {
		return new FrameData(frameTime, frameInterval );//return null;
	}
	
	@Override
	public void queryRecords() throws VizException {

		queryList = new HashMap<String, RequestConstraint>(
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
	}
}
