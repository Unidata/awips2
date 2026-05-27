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
import java.util.Objects;

import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.dataplugin.radar.util.RadarRecordUtil;
import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.viz.core.DrawableImage;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.drawables.ext.colormap.IColormappedImageExtension;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.viz.awipstools.IToolChangedListener;
import com.raytheon.viz.awipstools.ToolsDataManager;
import com.raytheon.viz.radar.IRadarConfigListener;
import com.raytheon.viz.radar.RadarHelper;
import com.raytheon.viz.radar.VizRadarRecord;
import com.raytheon.viz.radar.interrogators.IRadarInterrogator;
import com.raytheon.viz.radar.rsc.RadarResourceData;
import com.raytheon.viz.radar.ui.RadarDisplayManager;

/**
 * For the derived product, SRM8
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 2, 2010             mnash       Initial creation
 * 03/07/2012   DR 14660   D. Friedman Use getSTIDataForRadarRecord
 * Feb 22, 2023 9021       mapeters    Move loadSRMVelocity() to RadarHelper,
 *                                     update RadarSRMDataRetrievalAdapter
 *                                     hashCode/equals
 *
 * </pre>
 *
 * @author mnash
 */
public class RadarSRMResource extends RadarRadialResource
        implements IRadarConfigListener, IToolChangedListener {

    protected SimpleDateFormat hhmmFormat = new SimpleDateFormat("HH:mm");

    public enum SRMSource {
        WARNGEN, STI, CUSTOM
    }

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

    @Override
    public void updateConfig() {
        clearData();
    }

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

    @Override
    protected void createTile(IGraphicsTarget target,
            VizRadarRecord populatedRecord) throws StorageException,
            IOException, ClassNotFoundException, VizException {
        RadarHelper.loadSRMVelocity(populatedRecord);
        super.createTile(target, populatedRecord);
        upperTextMap.remove(populatedRecord.getDataTime());
    }

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

    protected static class RadarSRMDataRetrievalAdapter
            extends RadarRadialDataRetrievalAdapter {

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
                    imageData[i] = table[RadarRecordUtil.getSRMDataValue(record,
                            h, w) & 0xFF];
                    ++i;
                }
            }
            return imageData;
        }

        @Override
        protected int initHashCode() {
            final int prime = 31;
            int hashCode = 1;
            hashCode = prime * hashCode + System.identityHashCode(record);
            hashCode = prime * hashCode + Arrays.hashCode(table);
            hashCode = prime * hashCode + rect.hashCode();
            return hashCode;
        }

        /**
         * Override equals to force records to == each other so that SRM will
         * not share unless records are same object to prevent issues when SRM
         * speed and direction change.
         *
         * We suppress SonarQube's warning to override hashCode() as well. The
         * hash code is pre-computed in the superclass, and our initHashCode()
         * override makes it match this equals() override.
         */
        @SuppressWarnings("squid:S1206")
        @Override
        public boolean equals(Object obj) {
            if (this == obj) {
                return true;
            }
            if (obj == null) {
                return false;
            }
            if (getClass() != obj.getClass()) {
                return false;
            }
            RadarSRMDataRetrievalAdapter other = (RadarSRMDataRetrievalAdapter) obj;
            if (record != other.record) {
                return false;
            }
            if (!Arrays.equals(table, other.table)) {
                return false;
            }
            if (!Objects.equals(rect, other.rect)) {
                return false;
            }
            return true;
        }
    }
}
