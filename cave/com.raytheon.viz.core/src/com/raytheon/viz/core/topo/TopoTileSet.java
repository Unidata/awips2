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

package com.raytheon.viz.core.topo;

import java.io.File;
import java.util.Map;

import org.geotools.coverage.grid.GeneralGridEnvelope;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.geometry.GeneralEnvelope;
import org.geotools.referencing.operation.builder.GridToEnvelopeMapper;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.datum.PixelInCell;
import org.opengis.referencing.operation.MathTransform;

import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.geospatial.CRSCache;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.data.prep.HDF5DataRetriever;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.drawables.ext.colormap.IColormappedImageExtension;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.viz.core.rsc.hdf5.FileBasedTileSet;

/**
 * Implements a single tile of the SRTM dataset
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * Feb 15, 2007             chammack    Initial Creation.
 * Jan 14, 2013 1469        bkowal      The hdf5 data directory is no longer included in the
 *                                      DATA_FILE
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public class TopoTileSet extends FileBasedTileSet {

    private static String DATA_FILE = "/topo/srtm30.hdf";

    static {
        DATA_FILE = new File(DATA_FILE)
                .getAbsolutePath();
    }

    protected static int getNumLevels() throws VizException {
        IDataStore ds = DataStoreFactory.getDataStore(new File(DATA_FILE));
        try {
            return ds.getDatasets("/interpolated").length + 1;
        } catch (Exception e) {
            throw new VizException("Error getting interpolation levels", e);
        }
    }

    protected static GridGeometry2D getGridGeometry() throws VizException {
        IDataStore ds = DataStoreFactory.getDataStore(new File(DATA_FILE));

        Request request = Request.buildSlab(new int[] { 0, 0 }, new int[] { 1,
                1 });

        try {
            IDataRecord record = ds.retrieve("/", "full", request);
            Map<String, Object> attributes = record.getDataAttributes();
            int width = (Integer) attributes.get("Width");
            int height = (Integer) attributes.get("Height");
            double ulLat = (Double) attributes.get("ulLat");
            double ulLon = (Double) attributes.get("ulLon");
            double lrLat = (Double) attributes.get("lrLat");
            double lrLon = (Double) attributes.get("lrLon");
            String crsString = (String) attributes.get("CRS");

            // construct the grid geometry that covers the topo grid
            CoordinateReferenceSystem crs = CRSCache.getInstance()
                    .getCoordinateReferenceSystem(crsString);
            GeneralEnvelope ge = new GeneralEnvelope(2);
            ge.setCoordinateReferenceSystem(crs);
            ge.setRange(0, ulLon, lrLon);
            ge.setRange(1, lrLat, ulLat);

            GeneralGridEnvelope gr = new GeneralGridEnvelope(
                    new int[] { 1, 1 }, new int[] { width, height }, false);

            GridToEnvelopeMapper mapper = new GridToEnvelopeMapper();
            mapper.setEnvelope(ge);
            mapper.setGridRange(gr);
            mapper.setPixelAnchor(PixelInCell.CELL_CENTER);
            mapper.setReverseAxis(new boolean[] { false, true });
            MathTransform mt = mapper.createTransform();

            GridGeometry2D gridGeom = new GridGeometry2D(
                    PixelInCell.CELL_CORNER, mt, ge, null);

            return gridGeom;
        } catch (Exception e) {
            throw new VizException("Error getting grid geometry", e);
        }
    }

    public TopoTileSet(AbstractVizResource<?, ?> rsc, String viewType)
            throws VizException {

        super(DATA_FILE, null, null, getNumLevels(), 256, getGridGeometry(),
                rsc, PixelInCell.CELL_CORNER, viewType);
    }

    @Override
    protected IImage createTile(IGraphicsTarget target, int level, int i, int j)
            throws VizException {
        String dataset;
        if (level == 0) {
            dataset = "/full";
        } else {
            dataset = "/interpolated/" + level;
        }

        return target.getExtension(IColormappedImageExtension.class)
                .initializeRaster(
                        new HDF5DataRetriever(new File(this.hdf5File), dataset,
                                this.tileSet.getTile(level, i, j)
                                        .getRectangle()),
                        rsc.getCapability(ColorMapCapability.class)
                                .getColorMapParameters());
    }
}
