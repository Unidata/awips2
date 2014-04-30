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

package com.raytheon.edex.uengine.tasks.radar;

import org.geotools.coverage.grid.GridGeometry2D;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.dataplugin.radar.util.RadarTiler;
import com.raytheon.uf.common.datastorage.records.ByteDataRecord;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.datastorage.records.ShortDataRecord;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * Derived from original uEngine DecodeRadarImage task.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date             PR#             Engineer            Description
 * -----------      ----------      ------------        --------------------------
 * Apr 12, 2007                     njensen             Initial Creation
 * Apr 14, 2014           2984      njensen             Deprecated and remove uengine dependency
 * </PRE>
 * 
 */
@Deprecated
public class DecodeRadarImage {

    private static final IUFStatusHandler handler = UFStatus
            .getHandler(DecodeRadarImage.class);

    private RadarRecord radarRecord;

    private RadarTiler radarTiler;

    private GridGeometry2D geometry;

    public DecodeRadarImage(PluginDataObject aDataRecord, IDataRecord[] records) {
        radarRecord = (RadarRecord) aDataRecord;
        for (IDataRecord dataRecord : records) {
            if ("Data".equals(dataRecord.getName())) {
                radarRecord.setRawData(((ByteDataRecord) dataRecord)
                        .getByteData());
            } else if ("Angles".equals(dataRecord.getName())) {
                radarRecord.setAngleData(((FloatDataRecord) dataRecord)
                        .getFloatData());
            } else if ("Thresholds".equals(dataRecord.getName())) {
                radarRecord.setThresholds(((ShortDataRecord) dataRecord)
                        .getShortData());
            }
        }
        radarTiler = new RadarTiler(radarRecord, 1, 920);
        geometry = radarTiler.constructGridGeometry();
    }

    public byte[] execute() {
        byte[] bi = null;

        // get the radar file
        bi = generateProduct();
        return bi;
    }

    private byte[] generateProduct() {
        long t0 = System.currentTimeMillis();
        // DataBufferByte data = new
        // DataBufferByte(radarImage.createFullImage(),
        // radarImage.getHeight() * radarImage.getWidth());
        byte[] image = radarTiler.createFullImage();
        long t = System.currentTimeMillis() - t0;
        if (handler.isPriorityEnabled(Priority.INFO)) {
            handler.handle(Priority.INFO, "createFullImage: " + t + " ms");
        }

        // int[] nBits = { 8 };
        // ColorSpace cs = ColorSpace.getInstance(ColorSpace.CS_GRAY);
        // ColorModel cm = new ComponentColorModel(cs, nBits, false, true,
        // Transparency.OPAQUE, DataBuffer.TYPE_BYTE);
        // SampleModel sm =
        // cm.createCompatibleSampleModel(radarImage.getWidth(),
        // radarImage.getHeight());
        // WritableRaster raster = WritableRaster.createWritableRaster(sm, data,
        // null);
        return image;
    }

    public GridGeometry2D getGridGeometry() {
        return geometry;
    }

    public CoordinateReferenceSystem getCrs() {
        return geometry.getEnvelope2D().getCoordinateReferenceSystem();
    }
}
