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
package com.raytheon.viz.grid.data;

import java.io.File;
import java.lang.ref.WeakReference;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.Unit;

import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.common.dataplugin.HDF5Util;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.dataplugin.grid.derivparam.data.GridRequestableData;
import com.raytheon.uf.common.dataplugin.grid.derivparam.data.SliceUtil;
import com.raytheon.uf.common.dataplugin.level.LevelFactory;
import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.dataplugin.radar.util.RadarDataRetriever;
import com.raytheon.uf.common.dataplugin.radar.util.RadarMapper;
import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.gridcoverage.GridCoverage;
import com.raytheon.uf.common.inventory.exception.DataCubeException;
import com.raytheon.uf.common.parameter.Parameter;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.grid.util.RadarAdapter;

/**
 * A requestable data record which wraps a RadarRecord and can convert radar
 * radial data into the expected radar projection and units.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 18, 2010 4473       rjpeter     Initial creation
 * Aug 30, 2013 2298       rjpeter     Make getPluginName abstract
 * Sep 09, 2014 3356       njensen     Remove CommunicationException
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */

public class RadarRequestableData extends GridRequestableData {

    private final RadarRecord radarSource;

    private final RadarMapper tiler;

    private WeakReference<FloatDataRecord> cache = null;

    public RadarRequestableData(RadarRecord source, String parameterAbbrev)
            throws VizException {
        this.radarSource = source;
        source.setAddSpatial(false);
        // set unit converter here
        ColorMapParameters cMapParams = RadarAdapter.getColorMap(radarSource);
        Unit<?> unit = cMapParams.getDisplayUnit();

        this.tiler = new RadarMapper(radarSource, RadarAdapter.getGridSize(),
                RadarAdapter.getGridSpacing());
        this.source = "radar";
        this.dataTime = source.getDataTime();
        this.space = RadarAdapter.getInstance().getCoverage();
        this.level = LevelFactory.getInstance().getLevel("TILT",
                source.getPrimaryElevationAngle());

        this.parameter = parameterAbbrev;
        this.parameterName = "";
        this.unit = unit;

        try {
            GridRecord record = new GridRecord();
            record.setDatasetId(this.source);
            record.setLocation(RadarAdapter.getInstance().getCoverage());
            record.setLevel(this.level);
            Parameter parameter = new Parameter(parameterAbbrev,
                    this.parameterName, unit);
            record.setParameter(parameter);
            record.setDataTime(source.getDataTime());
            setGridSource(record);
        } catch (Exception e) {
            throw new VizException(e);
        }
    }

    @Override
    public IDataRecord[] getDataValue(Object arg) throws DataCubeException {
        FloatDataRecord fdr = null;
        if (cache != null) {
            fdr = cache.get();
        }
        if (fdr == null) {
            File loc = HDF5Util.findHDF5Location(radarSource);
            IDataStore dataStore = DataStoreFactory.getDataStore(loc);
            try {
                RadarDataRetriever.populateRadarRecord(dataStore, radarSource);
            } catch (Exception e) {
                throw new DataCubeException(
                        "Error Retrieving Data from Radar Record", e);
            }
            // Call radar tiler to get tile data, look up color map to translate
            // to float
            ColorMapParameters cMapParams;
            try {
                cMapParams = RadarAdapter.getColorMap(radarSource);
            } catch (VizException e) {
                throw new DataCubeException(e);
            }
            cMapParams.setDataUnit(radarSource.getDataUnit());
            /*
             * UnitConverter dataToImage =
             * DataUtilities.getDataToImageConverter( radarSource, cMapParams);
             * tiler.setDataToImageConverter(dataToImage);
             */
            Unit<?> unit = cMapParams.getDisplayUnit();
            getGridSource().getParameter().setUnit(unit);
            setUnit(unit);
            UnitConverter converter = cMapParams.getDataToDisplayConverter();
            tiler.setDataConverter(converter);
            // Based off looking at Awips I Col Max reflectivity it looks like
            // they use -10 when there is no data.
            tiler.setNan(-10);
            float[] data = tiler.createImage();
            fdr = new FloatDataRecord();
            fdr.setFloatData(data);
            GridCoverage coverage = gridSource.getLocation();
            fdr.setSizes(new long[] { coverage.getNx(), coverage.getNy() });
            fdr.setDimension(2);
            cache = new WeakReference<FloatDataRecord>(fdr);
        }
        if (arg instanceof Request) {
            fdr = SliceUtil.slice(fdr, (Request) arg);
            return new IDataRecord[] { fdr };
        } else {
            return new IDataRecord[] { fdr };
        }

    }
}
