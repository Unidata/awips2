/*
 * gov.noaa.nws.ncep.viz.rsc.ncradar.rsc.image.RadarRasterResource
 * 
 * 12-12-2011
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.viz.rsc.ncradar.rsc.image;

import java.awt.Rectangle;
import java.util.HashMap;

import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.referencing.CRS;
import org.geotools.referencing.crs.DefaultGeographicCRS;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.dataplugin.radar.util.RadarUtil;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.DrawableImage;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IMesh;
import com.raytheon.uf.viz.core.PixelCoverage;
import com.raytheon.uf.viz.core.catalog.LayerProperty;
import com.raytheon.uf.viz.core.catalog.ScriptCreator;
import com.raytheon.uf.viz.core.comm.Connector;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.drawables.ext.colormap.IColormappedImageExtension;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapMeshExtension;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.LoadProperties;

import com.raytheon.viz.radar.DefaultVizRadarRecord;
import com.raytheon.viz.radar.VizRadarRecord;

import gov.noaa.nws.ncep.viz.rsc.ncradar.rsc.RadarImageResource;
import gov.noaa.nws.ncep.viz.rsc.ncradar.rsc.RadarResourceData;

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
 * 12/13/2011      #541       S. Gurung   Initial creation
 * 03/30/2012      #651       S. Gurung   Removed method resourceChanged
 * 06/10/2013      #999     G. Hull     Use queryRecords from base class 
 * 
 * </pre>
 * 
 * @author sgurung
 * @version 1.0
 */

public class RadarRasterResource extends RadarImageResource<MapDescriptor> {

    private PixelCoverage sharedCoverage = null;

    /**
     * @param rrd
     * @param loadProps
     * @throws VizException
     */
    public RadarRasterResource(RadarResourceData rrd, LoadProperties loadProps ) throws VizException {
        super(rrd, loadProps);
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
     * gov.noaa.nws.ncep.viz.rsc.ncradar.rsc.RadarImageResource#project(org.opengis.referencing
     * .crs.CoordinateReferenceSystem)
     */
    @Override
    public void project(CoordinateReferenceSystem crs) throws VizException {
        super.project(crs);
        if (sharedCoverage != null) {
            sharedCoverage.dispose();
            sharedCoverage = null;
        }
        // TODO dispose just the coverage, not the image.
        for (DrawableImage image : images.values()) {
            if (image != null) {
                image.dispose();
            }
        }
        images.clear();
    }

    @Override
    protected void disposeImage(DrawableImage image) {
        if (image != null) {
            // Do not dispose this coverage since it is shared.
            image.setCoverage(null);
        }
        super.disposeImage(image);
    }

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

        public RadarRasterDataRetrievalAdapter(RadarRecord record, byte[] table,
        		Rectangle rect) {
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

        @Override
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

