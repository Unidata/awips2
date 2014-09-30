/*
 * gov.noaa.nws.ncep.viz.rsc.ncradar.rsc.image.RadarRadialResource
 * 
 * 12-07-2011
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.viz.rsc.ncradar.rsc.image;

import gov.noaa.nws.ncep.viz.rsc.ncradar.rsc.RadarImageResource;
import gov.noaa.nws.ncep.viz.rsc.ncradar.rsc.RadarResourceData;

import java.awt.Rectangle;

import javax.measure.Measure;
import javax.measure.quantity.Length;
import javax.measure.unit.NonSI;

import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.viz.core.DrawableImage;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IMesh;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.drawables.ext.colormap.IColormappedImageExtension;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.viz.radar.VizRadarRecord;
import com.raytheon.viz.radar.rsc.image.IRadialMeshExtension;
import com.raytheon.viz.radar.rsc.image.IRadialMeshExtension.RadialMeshData;

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
 * 12/09/2011   #541       S. Gurung   Initial creation
 * 12/16/2011              S. Gurung   Removed resourceAttrsModified()
 * 03/30/2012   #651       S. Gurung   Removed method resourceChanged
 * 09-04-2012              B. Hebbard  Add getGridGeometry() to descriptor per OB12.9.1 RTS
 *                                     change IRadialMeshExtension.constructMesh 2nd param
 * 06/16/2014   #2061      bsteffen    update IRangeableResource
 * 06/24/2014   #2061      bsteffen    Remove RadarRecord dependency for Radial Mesh
 * 
 * </pre>
 * 
 * @author sgurung
 * @version 1.0
 */

public class RadarRadialResource extends RadarImageResource<MapDescriptor> {

    private static final String EAV_VALUE = "EAC.Value";

    /**
     * @param rrd
     * @param loadProps
     * @throws VizException
     */
    public RadarRadialResource(RadarResourceData rrd, LoadProperties loadProps) throws VizException {
        super(rrd, loadProps);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.radar.rsc.AbstractRadarResource#getElevation()
     */
    @Override
    public Measure<?, Length> getElevation() {
        RadarRecord radarRecord = getCurrentRadarRecord();

        if (radarRecord != null) {
            return Measure.valueOf(radarRecord.getElevation(), NonSI.FOOT);
        }
        return super.getElevation();
    }

    @Override
    protected IImage createImage(IGraphicsTarget target,
            ColorMapParameters params, RadarRecord record, Rectangle rect)
            throws VizException {
        byte[] table = createConversionTable(params, record);
        return target
                .getExtension(IColormappedImageExtension.class)
                .initializeRaster(
                        new RadarRadialDataRetrievalAdapter(record, table, rect),
                        params);
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
        // TODO dispose just the coverage, not the image.
        for (DrawableImage image : images.values()) {
            if (image != null) {
                image.dispose();
            }
        }
        images.clear();
    }

    protected static class RadarRadialDataRetrievalAdapter extends
            RadarImageDataRetrievalAdapter {

        public RadarRadialDataRetrievalAdapter(RadarRecord record, byte[] table,
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
                    if (!createCircle(imageData, h, w, i)) {
                        imageData[i] = table[record.getRawIntDataValue(h, w)];
                    }
                    ++i;
                }
            }
            return imageData;
        }

        @Override
        protected boolean createCircle(byte[] imageData, int h, int w, int i) {
            if (w == 0) {
                imageData[0] = (byte) 0;
                if (record.getRawData() != null) {
                    record.getRawData()[i] = (byte) 0;
                }
                if (record.getRawShortData() != null) {
                    record.getRawShortData()[i] = (byte) 0;
                }
                return true;
            }
            return false;
        }

    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * gov.noaa.nws.ncep.viz.rsc.ncradar.rsc.RadarImageResource#buildMesh(com.raytheon.
     * uf.viz.core.IGraphicsTarget, gov.noaa.nws.ncep.viz.rsc.ncradar.VizRadarRecord)
     */
    @Override
    public IMesh buildMesh(IGraphicsTarget target, VizRadarRecord radarRecord)
            throws VizException {
        return target.getExtension(IRadialMeshExtension.class).constructMesh(
                new RadialMeshData(radarRecord),
                ((IMapDescriptor) descriptor).getGridGeometry());
    }
}

