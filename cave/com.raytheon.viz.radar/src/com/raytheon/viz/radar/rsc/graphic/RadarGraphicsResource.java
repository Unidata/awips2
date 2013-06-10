/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.viz.radar.rsc.graphic;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import org.eclipse.swt.graphics.RGB;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.dataplugin.radar.RadarDataKey;
import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ext.ICanvasRenderingExtension;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.IMiddleClickCapableResource;
import com.raytheon.uf.viz.core.rsc.capabilities.MagnificationCapability;
import com.raytheon.viz.radar.IRadarConfigListener;
import com.raytheon.viz.radar.interrogators.IRadarInterrogator;
import com.raytheon.viz.radar.rsc.AbstractRadarResource;
import com.raytheon.viz.radar.rsc.RadarResourceData;
import com.raytheon.viz.radar.ui.RadarDisplayManager;
import com.raytheon.viz.radar.VizRadarRecord;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 13, 2010            mnash     Initial creation
 * Jul 20, 2010 #6187      bkowal    The cache will be cleared out every time
 *                                   a refresh is required.
 * 03/05/2013   DCS51      zwang     Handle GFM product                                   
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class RadarGraphicsResource extends AbstractRadarResource<MapDescriptor>
        implements IMiddleClickCapableResource, IRadarConfigListener {

    private Map<DataTime, RadarGraphicsDisplay> radarGraphicsDisplay;

    private RadarGraphicsDisplay rgd;

    public Set<String> filteredStormIds = null;

    protected Map<DataTime, RGB> lastColor = new HashMap<DataTime, RGB>();

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

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.radar.rsc.RadarResource#disposeInternal()
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
     * com.raytheon.uf.viz.radar.rsc.RadarResource#resourceChanged(com.raytheon
     * .uf.viz.core.rsc.IResourceDataChanged.ChangeType, java.lang.Object)
     */
    @Override
    public void resourceChanged(ChangeType type, Object object) {
        super.resourceChanged(type, object);
        if (type == ChangeType.CAPABILITY) {
            if (object instanceof MagnificationCapability) {
                double mag = ((MagnificationCapability) object)
                        .getMagnification();
                for (RadarGraphicsDisplay display : this.radarGraphicsDisplay
                        .values()) {
                    display.setMagnification(mag);
                }
            }
            issueRefresh();
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.radar.rsc.AbstractRadarResource#getUpperText(com.
     * raytheon .uf.common.time.DataTime)
     */
    @Override
    public String[] getUpperText(DataTime time) {

    	VizRadarRecord record = getRadarRecord(time);
    	
    	// Use upper text to display gfmCount for GFM (140)
    	if (record != null && record.getProductCode() == 140) {
    		return super.getUpperText(time);
    	}
        
    	// Upper text would interfere with the table
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.radar.rsc.AbstractRadarResource#paintInternal(com
     * .raytheon .uf.viz.core.IGraphicsTarget,
     * com.raytheon.uf.viz.core.drawables.PaintProperties)
     */
    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
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
                rgd = new RadarGraphicsDisplay(radarRecord, target, descriptor,
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

            // Get the Lat/Lon of the screen Extent
            Envelope screenLatLon = descriptor.pixelToWorld(paintProps
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

            DrawableString offscreen = new DrawableString(offScreenCount
                    + " FEATURES OFF SCREEN", this.getCapability(
                    ColorableCapability.class).getColor());
            offscreen.setCoordinates(xPos, yPos);
            offscreen.horizontalAlignment = HorizontalAlignment.CENTER;
            offscreen.verticallAlignment = VerticalAlignment.MIDDLE;

            DrawableString notShown = new DrawableString(filteredCount
                    + " FEATURES NOT SHOWN", this.getCapability(
                    ColorableCapability.class).getColor());
            notShown.setCoordinates(xPos, yPos + 20);
            notShown.horizontalAlignment = HorizontalAlignment.CENTER;
            notShown.verticallAlignment = VerticalAlignment.MIDDLE;
            target.getExtension(ICanvasRenderingExtension.class).drawStrings(
                    paintProps, offscreen, notShown);

        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.radar.rsc.RadarResource#project(org.opengis.referencing
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
    public void remove(DataTime dataTime) {
        super.remove(dataTime);
        final RadarGraphicsDisplay display;
        synchronized (radarGraphicsDisplay) {
            display = radarGraphicsDisplay.remove(dataTime);
        }
        if (display != null) {
            // Run this in the UI thread to avoid accidentally disposing of
            // things that are painting. This is better than synchronizing
            // because it makes it much more difficult to deadlock.
            VizApp.runAsync(new Runnable() {
                @Override
                public void run() {
                    display.dispose();
                }
            });

        }
    }
}
