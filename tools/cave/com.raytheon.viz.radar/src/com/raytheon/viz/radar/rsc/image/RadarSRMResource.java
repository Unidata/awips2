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
package com.raytheon.viz.radar.rsc.image;

import java.awt.Rectangle;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Date;

import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.dataplugin.radar.util.RadarRecordUtil;
import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.viz.core.DrawableImage;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.ColorMapParameters;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.drawables.ext.colormap.IColormappedImageExtension;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.viz.awipstools.IToolChangedListener;
import com.raytheon.viz.awipstools.ToolsDataManager;
import com.raytheon.viz.awipstools.common.StormTrackData;
import com.raytheon.viz.radar.IRadarConfigListener;
import com.raytheon.viz.radar.RadarHelper;
import com.raytheon.viz.radar.VizRadarRecord;
import com.raytheon.viz.radar.interrogators.IRadarInterrogator;
import com.raytheon.viz.radar.rsc.RadarResourceData;
import com.raytheon.viz.radar.ui.RadarDisplayControls;
import com.raytheon.viz.radar.ui.RadarDisplayManager;

/**
 * For the derived product, SRM8
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 2, 2010            mnash     Initial creation
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class RadarSRMResource extends RadarRadialResource implements
        IRadarConfigListener, IToolChangedListener {

    protected SimpleDateFormat hhmmFormat = new SimpleDateFormat("HH:mm");

    public enum SRMSource {
        WARNGEN, STI, CUSTOM
    };

    /**
     * @param rrd
     * @param loadProps
     * @throws VizException
     */
    public RadarSRMResource(RadarResourceData rrd, LoadProperties loadProps,
            IRadarInterrogator interrogator) throws VizException {
        super(rrd, loadProps, interrogator);
        RadarDisplayManager.getInstance().addListener(this);
        ToolsDataManager.getInstance().addStormTrackChangedListener(this);
    }

    @Override
    protected void disposeInternal() {
        super.disposeInternal();
        RadarDisplayManager.getInstance().removeListener(this);
        ToolsDataManager.getInstance().removeStormTrackChangedListener(this);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.radar.IRadarConfigListener#updateConfig()
     */
    @Override
    public void updateConfig() {
        clearData();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.awipstools.IToolChangedListener#toolChanged()
     */
    @Override
    public void toolChanged() {
        clearData();
    }

    private void clearData() {
        synchronized (this.images) {
            for (DrawableImage image : this.images.values()) {
                if (image != null) {
                    image.dispose();
                }
            }
        }
        images.clear();
        upperTextMap.clear();
        issueRefresh();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.radar.rsc.RadarResource#createTile(com.raytheon.uf.viz
     * .core.IGraphicsTarget,
     * com.raytheon.viz.radar.RadarTimeRecord.RadarTiltRecord)
     */
    @Override
    protected void createTile(IGraphicsTarget target,
            VizRadarRecord populatedRecord) throws StorageException,
            IOException, ClassNotFoundException, VizException {
        loadSRMVelocity(populatedRecord);
        super.createTile(target, populatedRecord);
        upperTextMap.remove(populatedRecord.getDataTime());
    }

    private void loadSRMVelocity(RadarRecord record) throws VizException {
        RadarDisplayControls currentSettings = RadarDisplayManager
                .getInstance().getCurrentSettings();
        SRMSource srmSource = currentSettings.getSrmSource();

        double direction = 0;
        double speed = 0;
        Date movementTime = null;
        String sourceName = null;

        // for custom direction/speed as set in the Radar Display Controls
        // dialog
        if (srmSource.equals(SRMSource.WARNGEN)) {
            sourceName = "TRK";
            StormTrackData stormTrackData = ToolsDataManager.getInstance()
                    .getStormTrackData();
            if (stormTrackData != null && stormTrackData.isValid()
                    && stormTrackData.getMotionSpeed() < 100.0) {
                direction = (stormTrackData.getMotionDirection() + 180) % 360;
                speed = stormTrackData.getMotionSpeed();
                movementTime = stormTrackData.getDate();
            } else {
                // If no warngen, then try STI
                srmSource = SRMSource.STI;
            }
        }
        if (srmSource.equals(SRMSource.STI)) {
            sourceName = "STI";
            StormTrackData stormTrackData = RadarHelper.getSTIData(icao);
            if (stormTrackData != null && stormTrackData.isValid()) {
                direction = stormTrackData.getMotionDirection();
                speed = stormTrackData.getMotionSpeed();
                movementTime = stormTrackData.getDate();
            } else {
                // if no STI, use custom
                srmSource = SRMSource.CUSTOM;
            }
        }
        if (srmSource.equals(SRMSource.CUSTOM)) {
            sourceName = "USR";
            direction = currentSettings.getSrmDir();
            speed = currentSettings.getSrmSpeed();
            movementTime = SimulatedTime.getSystemTime().getTime();
        }

        RadarRecordUtil.setSRMData(record, direction, speed, movementTime,
                sourceName);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.radar.rsc.image.RadarRadialResource#toImageData(com.
     * raytheon.uf.viz.core.IGraphicsTarget,
     * com.raytheon.uf.viz.core.drawables.ColorMapParameters,
     * com.raytheon.uf.common.dataplugin.radar.RadarRecord, java.awt.Rectangle)
     */
    @Override
    protected IImage createImage(IGraphicsTarget target,
            ColorMapParameters params, RadarRecord record, Rectangle rect)
            throws VizException {
        byte[] table = createConversionTable(params, record);
        return target.getExtension(IColormappedImageExtension.class)
                .initializeRaster(
                        new RadarSRMDataRetrievalAdapter(record, table, rect),
                        params);
    }

    protected static class RadarSRMDataRetrievalAdapter extends
            RadarRadialDataRetrievalAdapter {

        public RadarSRMDataRetrievalAdapter(RadarRecord record, byte[] table,
                Rectangle rect) {
            super(record, table, rect);
        }

        @Override
        public byte[] convertData() {
            byte[] imageData = new byte[record.getNumBins()
                    * record.getNumRadials()];
            int i = 0;
            for (int h = 0; h < record.getNumRadials(); ++h) {
                for (int w = 0; w < record.getNumBins(); ++w) {
                    imageData[i] = table[RadarRecordUtil.getSRMDataValue(
                            record, h, w) & 0xFF];
                    ++i;
                }
            }
            return imageData;
        }

        /**
         * Overide equals to force records to == eachother so that srm will not
         * share unless records are same object to prevent issues when srm speed
         * and direction change.
         */
        @Override
        public boolean equals(Object obj) {
            if (this == obj)
                return true;
            if (obj == null)
                return false;
            if (getClass() != obj.getClass())
                return false;
            RadarSRMDataRetrievalAdapter other = (RadarSRMDataRetrievalAdapter) obj;
            if (record != other.record) {
                return false;
            }
            if (!Arrays.equals(table, other.table))
                return false;
            return true;
        }

    }

}
