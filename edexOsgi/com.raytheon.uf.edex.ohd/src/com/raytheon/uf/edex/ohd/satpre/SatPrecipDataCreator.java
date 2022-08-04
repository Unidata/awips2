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
package com.raytheon.uf.edex.ohd.satpre;

import java.awt.Rectangle;
import java.io.FileNotFoundException;
import java.io.IOException;

import javax.measure.Unit;
import javax.measure.UnitConverter;

import org.geotools.coverage.grid.GeneralGridGeometry;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.dataaccess.util.DataWrapperUtil;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.mpe.PrecipRecord;
import com.raytheon.uf.common.dataplugin.satellite.SatMapCoverage;
import com.raytheon.uf.common.dataplugin.satellite.SatelliteRecord;
import com.raytheon.uf.common.dataplugin.satellite.units.SatelliteUnitsUtil;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.datastorage.records.ShortDataRecord;
import com.raytheon.uf.common.geospatial.data.UnitConvertingDataFilter;
import com.raytheon.uf.common.geospatial.interpolation.GridReprojection;
import com.raytheon.uf.common.geospatial.interpolation.NearestNeighborInterpolation;
import com.raytheon.uf.common.mpe.fieldgen.PrecipField;
import com.raytheon.uf.common.numeric.buffer.BufferWrapper;
import com.raytheon.uf.common.numeric.buffer.ShortBufferWrapper;
import com.raytheon.uf.common.numeric.source.DataSource;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.util.ITimer;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.units.UnitConv;
import com.raytheon.uf.common.xmrg.hrap.HRAP;
import com.raytheon.uf.common.xmrg.hrap.HRAPCoordinates;
import com.raytheon.uf.common.xmrg.hrap.HRAPSubGrid;
import com.raytheon.uf.common.xmrg.hrap.HrapConversionException;
import com.raytheon.uf.edex.database.plugin.PluginDao;
import com.raytheon.uf.edex.database.plugin.PluginFactory;
import com.raytheon.uf.edex.ohd.pproc.IPrecipDataCreator;
import com.raytheon.uf.edex.ohd.pproc.PrecipCreationException;

import si.uom.SI;
import tec.uom.se.unit.MetricPrefix;
import tec.uom.se.unit.Units;

/**
 * Creates an alternate version of the SATPRE hrap precip grid based on GOES-R
 * RRQPE data. The data acquired from GOES-R is converted to the expected unit
 * of measurement and reprojected into the hrap grid space via
 * {@link GridReprojection}.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Sep 13, 2017  6407     bkowal    Initial creation
 * Oct 06, 2017  6407     bkowal    Added logging.
 * Aug 15, 2018  7434     mapeters  Add necessary unit conversion
 * Apr 15, 2019  7596     lsingh    Updated units framework to JSR-363. Handled
 *                                  unit conversion.
 * Jun 03, 2021  8478     randerso  Remove unnecessary ProductUnit wrapper that
 *                                  was causing ClassCastExceptions.
 *
 * </pre>
 *
 * @author bkowal
 */

public class SatPrecipDataCreator
        implements IPrecipDataCreator<SatelliteRecord> {

    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(getClass());

    private final Rectangle extent;

    private final GeneralGridGeometry gridGeometry;

    public SatPrecipDataCreator() throws PrecipCreationException {
        try {
            extent = HRAPCoordinates.getHRAPCoordinates();
        } catch (NumberFormatException | IOException e) {
            throw new PrecipCreationException(
                    "Failed to retrieve the HRAP Coordinates.", e);
        }
        HRAPSubGrid subGrid = null;
        try {
            subGrid = HRAP.getInstance().getHRAPSubGrid(extent);
        } catch (HrapConversionException e) {
            throw new PrecipCreationException(
                    "Failed to retrieve the HRAP Sub-grid for extent: "
                            + extent.toString() + ".",
                    e);
        }
        try {
            gridGeometry = subGrid.getGridGeometry();
        } catch (HrapConversionException e) {
            throw new PrecipCreationException(
                    "Failed to retrieve the Grid Geometry for the HRAP Sub-grid.",
                    e);
        }
    }

    @Override
    public PluginDataObject[] build(PluginDataObject pdo)
            throws PrecipCreationException {
        final ITimer timer = TimeUtil.getTimer();
        timer.start();
        PrecipRecord precipRecord = build((SatelliteRecord) pdo);
        timer.stop();
        statusHandler.info("Successfully built: "
                + (precipRecord == null ? null : precipRecord.toString())
                + " in " + TimeUtil.prettyDuration(timer.getElapsedTime())
                + ".");
        return new PluginDataObject[] { precipRecord };
    }

    public PrecipRecord build(SatelliteRecord pdo)
            throws PrecipCreationException {
        final SatelliteData satelliteData = retrieveSatelliteData(pdo);
        return buildPrecipRecord(satelliteData);
    }

    private SatelliteData retrieveSatelliteData(SatelliteRecord pdo)
            throws PrecipCreationException {
        PluginDao dao = null;
        try {
            dao = PluginFactory.getInstance().getPluginDao(pdo.getPluginName());
        } catch (PluginException e) {
            throw new PrecipCreationException(
                    "Failed to retrieve the Satellite Dao.", e);
        }
        try {
            pdo = (SatelliteRecord) dao.getMetadata(pdo.getDataURI());
        } catch (PluginException e) {
            throw new PrecipCreationException(
                    "Failed to retrieve the Satellite Metadata.", e);
        }

        IDataStore dataStore = dao.getDataStore(pdo);
        final SatMapCoverage coverage = pdo.getCoverage();
        IDataRecord dataRec = null;
        try {
            dataRec = dataStore.retrieve(pdo.getDataURI(), "Data", Request.ALL);
        } catch (FileNotFoundException | StorageException e) {
            throw new PrecipCreationException(
                    "Failed to retrieve the satellite data.", e);
        }

        /*
         * The dataUnit is the current unit of the data, accounting for
         * offset/scale values in the satellite record.
         */
        Unit<?> dataUnit = SatelliteUnitsUtil
                .getDataUnit(SatelliteUnitsUtil.getRecordUnit(pdo), dataRec);
        /*
         * The other precip data creator (GridPrecipDataCreator) converts to
         * hundredths of millimeters, so match that (and CAVE is set to expect
         * that unit in MPEFieldResourceData.getDataUnitsForField()). It looks
         * like it does this so that the data can be stored as shorts (instead
         * of floats) while still maintaining decent precision. Ideally we
         * should actually store as floats, but all of the code expects short
         * data, so we do this for simplicity for now.
         */
        Unit<?> targetUnit = MetricPrefix.MILLI(SI.METRE).divide(Units.HOUR)
                .divide(100);
        UnitConverter converter = UnitConv.getConverterToUnchecked(dataUnit,
                targetUnit);

        // Construct unit-converting data source
        DataSource dataSource = DataWrapperUtil.constructArrayWrapper(dataRec,
                false);
        dataSource = UnitConvertingDataFilter.apply(dataSource, converter);

        GridReprojection reprojection = new GridReprojection(
                coverage.getGridGeometry(), gridGeometry);
        BufferWrapper dest = new ShortBufferWrapper((int) extent.getWidth(),
                (int) extent.getHeight());
        try {
            dest = reprojection.reprojectedGrid(
                    new NearestNeighborInterpolation(), dataSource, dest);
        } catch (FactoryException | TransformException e) {
            throw new PrecipCreationException(
                    "Failed to reproject the satellite data into the hrap sub-grid.",
                    e);
        }

        return new SatelliteData(pdo, dest);
    }

    private PrecipRecord buildPrecipRecord(final SatelliteData satelliteData) {
        final DataTime dataTime = satelliteData.getDataRecord().getDataTime();
        PrecipRecord precipRecord = new PrecipRecord();
        precipRecord.setPrecipField(PrecipField.SATPRE);
        precipRecord.setDataTime(dataTime);
        precipRecord.setHrapX((int) extent.getX());
        precipRecord.setHrapY((int) extent.getY());
        precipRecord.setHrapWidth((int) extent.getWidth());
        precipRecord.setHrapHeight((int) extent.getHeight());

        short[] data = (short[]) satelliteData.getData().getArray();
        for (int i = 0; i < data.length; i++) {
            if (data[i] < 0) {
                data[i] = 0;
            }
        }
        ShortDataRecord dataRecord = new ShortDataRecord("Data",
                precipRecord.getDataURI(),
                (short[]) satelliteData.getData().getArray(), 2, new long[] {
                        (long) extent.getWidth(), (long) extent.getHeight() });
        precipRecord.setMessageData(dataRecord);
        return precipRecord;
    }
}