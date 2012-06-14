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

import org.geotools.coverage.grid.GridGeometry2D;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.dataplugin.radar.util.RadarUtil;
import com.raytheon.uf.viz.core.DrawableImage;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IMesh;
import com.raytheon.uf.viz.core.PixelCoverage;
import com.raytheon.uf.viz.core.drawables.ColorMapParameters;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.drawables.ext.colormap.IColormappedImageExtension;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapMeshExtension;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.viz.radar.VizRadarRecord;
import com.raytheon.viz.radar.interrogators.IRadarInterrogator;
import com.raytheon.viz.radar.rsc.RadarImageResource;
import com.raytheon.viz.radar.rsc.RadarResourceData;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 29, 2010            mnash     Initial creation
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class RadarRasterResource extends RadarImageResource<MapDescriptor> {

    private PixelCoverage sharedCoverage = null;

    /**
     * @param rrd
     * @param loadProps
     * @throws VizException
     */
    public RadarRasterResource(RadarResourceData rrd, LoadProperties loadProps,
            IRadarInterrogator interrogator) throws VizException {
        super(rrd, loadProps, interrogator);
    }

    @Override
    protected void disposeInternal() {
        super.disposeInternal();
        if (sharedCoverage != null) {
            sharedCoverage.dispose();
            sharedCoverage = null;
        }
    }

    @Override
    public PixelCoverage buildCoverage(IGraphicsTarget target,
            VizRadarRecord timeRecord) throws VizException {
        if (sharedCoverage == null) {
            double maxExtent = RadarUtil.calculateExtent(timeRecord);
            GridGeometry2D geom = RadarUtil.constructGridGeometry(
                    timeRecord.getCRS(),
                    maxExtent,
                    Math.max(timeRecord.getNumBins(),
                            timeRecord.getNumRadials()));

            sharedCoverage = super.buildCoverage(target, timeRecord);
            sharedCoverage.setMesh(target.getExtension(IMapMeshExtension.class)
                    .constructMesh(geom, descriptor.getGridGeometry()));
        }
        return sharedCoverage;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.radar.rsc.RadarImageResource#project(org.opengis.referencing
     * .crs.CoordinateReferenceSystem)
     */
    @Override
    public void project(CoordinateReferenceSystem crs) throws VizException {
        super.project(crs);
        if (sharedCoverage != null && sharedCoverage.getMesh() != null) {
            IMesh oldMesh = sharedCoverage.getMesh();
            sharedCoverage.setMesh(oldMesh.clone(descriptor.getGridGeometry()));
            oldMesh.dispose();
        }
    }

    @Override
    protected void disposeImage(DrawableImage image) {
        if (image != null) {
            // Do not dispose this coverage since it is shared.
            image.setCoverage(null);
        }
        super.disposeImage(image);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.radar.rsc.RadarImageResource#toImageData(com.raytheon
     * .uf.viz.core.IGraphicsTarget,
     * com.raytheon.uf.viz.core.drawables.ColorMapParameters,
     * com.raytheon.uf.common.dataplugin.radar.RadarRecord, java.awt.Rectangle)
     */
    @Override
    protected IImage createImage(IGraphicsTarget target,
            ColorMapParameters params, RadarRecord record, Rectangle rect)
            throws VizException {
        byte[] table = createConversionTable(params, record);
        return target
                .getExtension(IColormappedImageExtension.class)
                .initializeRaster(
                        new RadarRasterDataRetrievalAdapter(record, table, rect),
                        params);
    }

    protected static class RadarRasterDataRetrievalAdapter extends
            RadarImageDataRetrievalAdapter {

        public RadarRasterDataRetrievalAdapter(RadarRecord record,
                byte[] table, Rectangle rect) {
            super(record, table, rect);
        }

        @Override
        public byte[] convertData() {
            int width = record.getNumRadials();
            int height = record.getNumBins();
            byte[] imageData = new byte[width * height];
            byte[] rawData = record.getRawData();
            for (int i = 0; i < rawData.length; ++i) {
                if (!createCircle(imageData, 0, 0, i)) {
                    imageData[i] = table[rawData[i] & 0xFF];
                }
            }
            return imageData;
        }

        protected boolean createCircle(byte[] imageData, int h, int w, int i) {
            int temp = record.getNumBins();
            // no circle is drawn for products of resolution higher than 1000 m
            if (i == (Math.pow(temp, 2) / 2)
                    && infoDict.getInfo(record.getProductCode())
                            .getResolution() <= 1000) {
                imageData[i + (temp / 2) - 1] = (byte) 0;
                imageData[i + (temp / 2)] = (byte) 0;
                imageData[i - (temp / 2) - 1] = (byte) 0;
                imageData[i - (temp / 2)] = (byte) 0;
                record.getRawData()[i + (temp / 2) - 1] = (byte) 0;
                record.getRawData()[i + (temp / 2)] = (byte) 0;
                record.getRawData()[i - (temp / 2) - 1] = (byte) 0;
                record.getRawData()[i - (temp / 2)] = (byte) 0;
                return true;
            }
            return false;
        }

    }

}
