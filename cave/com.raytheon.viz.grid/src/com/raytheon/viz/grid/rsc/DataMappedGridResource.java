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
package com.raytheon.viz.grid.rsc;

import java.util.Map;

import javax.measure.converter.UnitConverter;

import org.geotools.coverage.grid.GridGeometry2D;
import org.opengis.referencing.datum.PixelInCell;

import com.raytheon.uf.common.dataplugin.grib.GribRecord;
import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.viz.core.drawables.ColorMapParameters;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.uf.viz.core.style.DataMappingPreferences.DataMappingEntry;
import com.raytheon.viz.core.rsc.hdf5.AbstractTileSet;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 13, 2010            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class DataMappedGridResource extends GridResource {

    private class NcwfMemoryBasedTileSet extends GridMemoryBasedTileSet {

        public NcwfMemoryBasedTileSet(String group, String dataset,
                AbstractTileSet sharedGeometryTileset, UnitConverter converter,
                GribRecord pdo) throws VizException {
            super(group, dataset, sharedGeometryTileset, converter, pdo);
        }

        public NcwfMemoryBasedTileSet(String dataURI, String string,
                int numLevels, int i, GridGeometry2D gridGeometry2D,
                GridResource gridResource, UnitConverter conversion,
                PixelInCell cellCorner, GribRecord record, String viewType)
                throws VizException {
            super(dataURI, string, numLevels, i, gridGeometry2D, gridResource,
                    conversion, cellCorner, record, viewType);
        }

        @Override
        protected void preloadDataObject(int level) throws StorageException {
            super.preloadDataObject(level);
            float[] data = new float[((float[]) loadedData[0]).length];
            for (int i = 0; i < data.length; i++) {
                data[i] = ((float[]) loadedData[0])[i];
                if (Float.isNaN(data[i])) {
                    data[i] = Float.NaN;
                }
            }

            loadedData[0] = data; // transformNCWFData(data);
        }
    }

    public DataMappedGridResource(GridResourceData data, LoadProperties props) {
        super(data, props);
    }

    private float[] transformNCWFData(float[] data) {
        ColorMapParameters params = getCapability(ColorMapCapability.class)
                .getColorMapParameters();
        if (params == null || params.getImageToDisplayConverter() == null) {
            return data;
        }
        UnitConverter converter = params.getDataToImageConverter();
        if (converter == null) {
            return data;
        }
        float[] newData = new float[data.length];
        for (int i = 0; i < newData.length; i++) {
            newData[i] = (float) converter.convert(data[i]);
        }
        return newData;
    }

    @Override
    public NcwfMemoryBasedTileSet createTile(GribRecord record,
            GridMemoryBasedTileSet commonTile) throws VizException {
        if (commonTile != null) {
            return new NcwfMemoryBasedTileSet(record.getDataURI(), "Data",
                    commonTile, conversion, record);
        }

        GridGeometry2D gridGeometry2D = record.getModelInfo().getLocation()
                .getGridGeometry();
        return new NcwfMemoryBasedTileSet(record.getDataURI(), "Data",
                numLevels, 256, gridGeometry2D, this, conversion,
                PixelInCell.CELL_CORNER, record, viewType);
    }

    @Override
    public String getName() {
        String name = this.resourceData.getCustomLegend();
        if (name == null) {
            name = super.getName();
        }
        return name;
    }

    @Override
    public String inspect(ReferencedCoordinate coord) throws VizException {

        Map<Float, GridMemoryBasedTileSet> map = tileSet.get(descriptor
                .getFramesInfo().getTimeForResource(this));
        if (map == null) {
            return "No Data";
        }

        GridMemoryBasedTileSet tile = map.get(displayedLevel);
        if (tile == null) {
            tile = map.values().iterator().next();
        }

        if (tile == null) {
            return "No Data";
        }

        Coordinate latLon;
        try {
            latLon = coord.asLatLon();
        } catch (Exception e) {
            throw new VizException("Error transforming coordinate to lat/lon",
                    e);
        }

        // get raw image pixels (0-255)
        Double val = tile.interrogate(latLon, true);
        if (val.isNaN() || val <= -9999) {
            return "No Data";
        }

        ColorMapParameters params = getCapability(ColorMapCapability.class)
                .getColorMapParameters();

        for (DataMappingEntry entry : params.getDataMapping().getEntries()) {
            double pixelValue = entry.getPixelValue();
            double relError = Math.abs((pixelValue - val) / val);
            String text = entry.getLabel();
            if (relError < 0.00001 && text != null) {
                return text;
            }
        }

        if (params != null && params.getImageToDisplayConverter() != null) {
            val = params.getImageToDisplayConverter().convert(val);
        }

        return ((DataMappedGridResourceData) this.getResourceData())
                .getSampleFormat().format(val) + this.units;
    }

    @Override
    protected ColorMapParameters initColorMapParameters(GribRecord record)
            throws VizException {
        ColorMapParameters params = super.initColorMapParameters(record);
        // Always have min/max of 0-255 for data mapped
        params.setColorMapMin(0.0f);
        params.setColorMapMax(255.0f);
        return params;
    }

}
