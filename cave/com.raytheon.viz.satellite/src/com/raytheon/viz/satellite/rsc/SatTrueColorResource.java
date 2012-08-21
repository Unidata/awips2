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
package com.raytheon.viz.satellite.rsc;

import java.nio.ByteBuffer;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.geotools.coverage.grid.GridGeometry2D;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.satellite.SatelliteRecord;
import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.records.ByteDataRecord;
import com.raytheon.uf.common.geospatial.ISpatialObject;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.DrawableImage;
import com.raytheon.uf.viz.core.HDF5Util;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.RasterMode;
import com.raytheon.uf.viz.core.PixelCoverage;
import com.raytheon.uf.viz.core.RecordFactory;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.data.IColorMapDataRetrievalCallback;
import com.raytheon.uf.viz.core.drawables.ColorMapParameters;
import com.raytheon.uf.viz.core.drawables.IColorMapParametersListener;
import com.raytheon.uf.viz.core.drawables.IColormappedImage;
import com.raytheon.uf.viz.core.drawables.IRenderable;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ext.colormap.IMultiChannelImageExtension;
import com.raytheon.uf.viz.core.drawables.ext.colormap.IMultiChannelImageExtension.Channel;
import com.raytheon.uf.viz.core.drawables.ext.colormap.IMultiChannelImageExtension.ChannelData;
import com.raytheon.uf.viz.core.drawables.ext.colormap.IMultiChannelImageExtension.IImageChannel;
import com.raytheon.uf.viz.core.drawables.ext.colormap.IMultiChannelImageExtension.IMultiChannelImage;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.map.IMapMeshExtension;
import com.raytheon.uf.viz.core.rsc.AbstractPluginDataObjectResource;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged.ChangeType;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.AbstractCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ImagingCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.MultiChannelCapability;
import com.raytheon.uf.viz.core.rsc.hdf5.ImageTile;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * True color satellite resource
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 20, 2011            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */
public class SatTrueColorResource
        extends
        AbstractPluginDataObjectResource<SatTrueColorResourceData, IMapDescriptor>
        implements IColorMapParametersListener {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(SatTrueColorResource.class);

    private static final String NAME = "Sat True Color";

    private static class SatelliteDataRetriever implements
            IColorMapDataRetrievalCallback {

        private SatelliteRecord record;

        private boolean signed;

        public SatelliteDataRetriever(SatelliteRecord record, boolean signed) {
            this.record = record;
            this.signed = signed;
        }

        /*
         * (non-Javadoc)
         * 
         * @see com.raytheon.uf.viz.core.data.IColorMapDataRetrievalCallback#
         * getColorMapData()
         */
        @Override
        public ColorMapData getColorMapData() throws VizException {
            ColorMapDataType dataType = signed ? ColorMapDataType.SIGNED_BYTE
                    : ColorMapDataType.BYTE;
            IDataStore ds = DataStoreFactory.getDataStore(HDF5Util
                    .findHDF5Location(record));
            try {
                ISpatialObject so = record.getSpatialObject();
                ByteBuffer bb = ByteBuffer.wrap(((ByteDataRecord) ds.retrieve(
                        record.getDataURI(), "Data", Request.ALL))
                        .getByteData());
                return new ColorMapData(bb,
                        new int[] { so.getNx(), so.getNy() }, dataType);
            } catch (Exception e) {
                throw new VizException("Error requesting record data", e);
            }
        }

        /*
         * (non-Javadoc)
         * 
         * @see java.lang.Object#hashCode()
         */
        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = prime * result
                    + ((record == null) ? 0 : record.hashCode());
            result = prime * result + (signed ? 1231 : 1237);
            return result;
        }

        /*
         * (non-Javadoc)
         * 
         * @see java.lang.Object#equals(java.lang.Object)
         */
        @Override
        public boolean equals(Object obj) {
            if (this == obj)
                return true;
            if (obj == null)
                return false;
            if (getClass() != obj.getClass())
                return false;
            SatelliteDataRetriever other = (SatelliteDataRetriever) obj;
            if (record == null) {
                if (other.record != null)
                    return false;
            } else if (!record.equals(other.record))
                return false;
            if (signed != other.signed)
                return false;
            return true;
        }

    }

    private class SatTrueColorRenderable implements IRenderable {

        private SatelliteRecord baseRecord;

        private IMultiChannelImage image;

        private ImageTile tile;

        private Map<String, SatelliteDataRetriever> recordMap = new HashMap<String, SatelliteDataRetriever>();

        public SatTrueColorRenderable(SatelliteRecord[] records)
                throws VizException {
            for (SatelliteRecord record : records) {
                addRecord(record);
            }

            image = multiExt
                    .constructImage(createImageMapping(this,
                            getCapability(MultiChannelCapability.class)
                                    .getChannelMap()));
            ImagingCapability ic = getCapability(ImagingCapability.class);
            image.setBrightness(ic.getBrightness());
            image.setContrast(ic.getContrast());
            image.setInterpolated(ic.isInterpolationState());
        }

        public void addRecord(SatelliteRecord record) throws VizException {
            if (tile == null) {
                baseRecord = record;
                tile = constructCoverage(record);
            }

            Map<String, Object> mapping = RecordFactory.getInstance()
                    .loadMapFromUri(record.getDataURI());
            String key = String.valueOf(mapping.get(resourceData
                    .getUniqueField()));

            recordMap.put(key, new SatelliteDataRetriever(record, false));
        }

        /*
         * (non-Javadoc)
         * 
         * @see
         * com.raytheon.uf.viz.core.drawables.IRenderable#paint(com.raytheon
         * .uf.viz.core.IGraphicsTarget,
         * com.raytheon.uf.viz.core.drawables.PaintProperties)
         */
        @Override
        public void paint(IGraphicsTarget target, PaintProperties paintProps)
                throws VizException {
            target.drawRasters(paintProps, new DrawableImage(image,
                    tile.coverage, RasterMode.ASYNCHRONOUS));
        }

        public void dispose() {
            if (image != null) {
                image.dispose();
                image = null;
            }
            if (tile != null) {
                tile.dispose();
                tile = null;
            }

            recordMap.clear();
            baseRecord = null;
        }

    }

    private IMultiChannelImageExtension multiExt;

    private IMapMeshExtension meshExt;

    /**
     * @param resourceData
     * @param loadProperties
     */
    protected SatTrueColorResource(SatTrueColorResourceData resourceData,
            LoadProperties loadProperties, Collection<SatelliteRecord> records) {
        super(resourceData, loadProperties);
        String[] rgb = resourceData.getRedGreenBlue();
        MultiChannelCapability mcc = getCapability(MultiChannelCapability.class);
        mcc.setNames(rgb);
        Map<Channel, ChannelData> channelMap = mcc.getChannelMap();
        // Process new entries
        Channel[] channels = Channel.values();
        for (int i = 0; i < channels.length; ++i) {
            Channel c = channels[i];
            ChannelData cd = channelMap.get(c);
            if (cd == null) {
                cd = new ChannelData(i < rgb.length ? rgb[i] : null,
                        newParams(), false);
                channelMap.put(c, cd);
            } else {
                if (cd.name == null) {
                    cd.name = i < rgb.length ? rgb[i] : null;
                }
                if (cd.parameters == null) {
                    cd.parameters = newParams();
                }
            }
            cd.parameters.addListener(this);
        }
        getCapability(ImagingCapability.class);
        resourceData.fireChangeListeners(ChangeType.DATA_UPDATE,
                records.toArray(new SatelliteRecord[records.size()]));
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractVizResource#initInternal(com.raytheon
     * .uf.viz.core.IGraphicsTarget)
     */
    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        // Grab extensions that will be used
        multiExt = target.getExtension(IMultiChannelImageExtension.class);
        meshExt = target.getExtension(IMapMeshExtension.class);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.rsc.AbstractVizResource#getName()
     */
    @Override
    public String getName() {
        Map<Channel, ChannelData> channelMap = getCapability(
                MultiChannelCapability.class).getChannelMap();
        String components = "";
        if (channelMap != null) {
            int i = 0;
            for (Channel c : Channel.values()) {
                ChannelData cd = channelMap.get(c);
                if (cd != null && cd.name != null) {
                    if (i > 0) {
                        components += ",";
                    }
                    components += c.name().charAt(0);
                    ++i;
                }
            }
        }
        return NAME + " (" + components + ")";
    }

    /**
     * @param record
     * @return
     */
    private ImageTile constructCoverage(SatelliteRecord record)
            throws VizException {
        GridGeometry2D geom = MapUtil
                .getGridGeometry(record.getSpatialObject());
        ImageTile tile = new ImageTile();
        tile.setGridGeometry(geom);
        tile.coverage = new PixelCoverage(new Coordinate(), 0, 0);
        tile.coverage.setMesh(meshExt.constructMesh(geom, descriptor));
        return tile;
    }

    private Map<Channel, IImageChannel> createImageMapping(
            SatTrueColorRenderable renderable,
            Map<Channel, ChannelData> channelMap) {
        Map<Channel, IImageChannel> newMapping = new HashMap<Channel, IImageChannel>();
        for (Entry<Channel, ChannelData> entry : channelMap.entrySet()) {
            IColorMapDataRetrievalCallback callback = renderable.recordMap
                    .get(entry.getValue().name);
            if (callback != null) {
                try {
                    IImageChannel imageChannel = multiExt.constructImage(
                            callback, entry.getValue());
                    newMapping.put(entry.getKey(), imageChannel);
                } catch (VizException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            e.getLocalizedMessage(), e);
                }
            }
        }
        return newMapping;
    }

    private ColorMapParameters newParams() {
        ColorMapParameters params = new ColorMapParameters();
        params.setDataMin(0);
        params.setDataMax(255);
        params.setColorMapMin(0);
        params.setColorMapMax(255);
        return params;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.rsc.AbstractPluginDataObjectResource#
     * capabilityChanged(com.raytheon.uf.viz.core.drawables.IRenderable,
     * com.raytheon.uf.viz.core.rsc.capabilities.AbstractCapability)
     */
    @Override
    protected void capabilityChanged(IRenderable renderable,
            AbstractCapability capability) {
        SatTrueColorRenderable stcr = (SatTrueColorRenderable) renderable;
        if (stcr.image != null) {
            if (capability instanceof ImagingCapability) {
                ImagingCapability ic = (ImagingCapability) capability;
                stcr.image.setBrightness(ic.getBrightness());
                stcr.image.setContrast(ic.getContrast());
                stcr.image.setInterpolated(ic.isInterpolationState());
            } else if (capability instanceof MultiChannelCapability) {
                MultiChannelCapability mcc = (MultiChannelCapability) capability;
                final Map<Channel, IImageChannel> currMapping = stcr.image
                        .getImageMapping();
                stcr.image.setImageMapping(createImageMapping(stcr,
                        mcc.getChannelMap()));
                // Dispose old mapping
                VizApp.runAsync(new Runnable() {
                    @Override
                    public void run() {
                        for (IColormappedImage img : currMapping.values()) {
                            if (img != null) {
                                img.dispose();
                            }
                        }
                    }
                });
            }
            issueRefresh();
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.rsc.AbstractPluginDataObjectResource#
     * disposeRenderable(com.raytheon.uf.viz.core.drawables.IRenderable)
     */
    @Override
    protected void disposeRenderable(IRenderable renderable) {
        ((SatTrueColorRenderable) renderable).dispose();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.rsc.AbstractPluginDataObjectResource#
     * projectRenderable(com.raytheon.uf.viz.core.drawables.IRenderable,
     * org.opengis.referencing.crs.CoordinateReferenceSystem)
     */
    @Override
    protected boolean projectRenderable(IRenderable renderable,
            CoordinateReferenceSystem crs) throws VizException {
        SatTrueColorRenderable stcr = (SatTrueColorRenderable) renderable;
        ImageTile newTile = constructCoverage(stcr.baseRecord);
        ImageTile oldTile = stcr.tile;
        stcr.tile = newTile;
        if (oldTile != null) {
            oldTile.dispose();
        }
        return true;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.rsc.AbstractPluginDataObjectResource#
     * constructRenderable(java.util.List)
     */
    @Override
    protected IRenderable constructRenderable(List<PluginDataObject> records)
            throws VizException {
        SatelliteRecord[] satRecords = new SatelliteRecord[records.size()];
        for (int i = 0; i < satRecords.length; ++i) {
            satRecords[i] = (SatelliteRecord) records.get(i);
        }
        return new SatTrueColorRenderable(satRecords);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.rsc.AbstractPluginDataObjectResource#
     * updateRenderable(com.raytheon.uf.viz.core.drawables.IRenderable,
     * com.raytheon.uf.common.dataplugin.PluginDataObject)
     */
    @Override
    protected boolean updateRenderable(IRenderable renderable,
            PluginDataObject pdo) {
        try {
            SatTrueColorRenderable stcr = (SatTrueColorRenderable) renderable;
            stcr.addRecord((SatelliteRecord) pdo);
            capabilityChanged(stcr, getCapability(MultiChannelCapability.class));
            return true;
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error updating satellite renderable with new data", e);
        }
        return false;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.drawables.IColorMapParametersListener#
     * colorMapChanged()
     */
    @Override
    public void colorMapChanged() {
        issueRefresh();
    }
}
