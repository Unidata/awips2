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

import org.geotools.coverage.grid.GeneralGridGeometry;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.dataplugin.satellite.SatMapCoverage;
import com.raytheon.uf.common.dataplugin.satellite.SatelliteRecord;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.datastorage.records.ShortDataRecord;
import com.raytheon.uf.common.geospatial.interpolation.GridReprojection;
import com.raytheon.uf.common.geospatial.interpolation.NearestNeighborInterpolation;
import com.raytheon.uf.common.mpe.fieldgen.PrecipField;
import com.raytheon.uf.common.numeric.buffer.BufferWrapper;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.util.ITimer;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.xmrg.hrap.HRAP;
import com.raytheon.uf.common.xmrg.hrap.HRAPCoordinates;
import com.raytheon.uf.common.xmrg.hrap.HRAPSubGrid;
import com.raytheon.uf.common.xmrg.hrap.HrapConversionException;
import com.raytheon.uf.edex.database.plugin.PluginDao;
import com.raytheon.uf.edex.database.plugin.PluginFactory;
import com.raytheon.uf.edex.ohd.pproc.IPrecipDataCreator;
import com.raytheon.uf.edex.ohd.pproc.PrecipCreationException;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.mpe.PrecipRecord;

/**
 * Creates an alternate version of the SATPRE hrap precip grid based on GOES-R
 * RRQPE data. The data acquired from GOES-R already uses the expected unit of
 * measurement, so this precip data creator extracts the GOES-R data within the
 * extents of the hrap grid and uses {@link GridReprojection} to reproject it
 * into the hrap grid space.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 13, 2017 6407       bkowal      Initial creation
 * Oct 06, 2017 6407       bkowal      Added logging.
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

        GridReprojection reprojection = new GridReprojection(
                coverage.getGridGeometry(), gridGeometry);
        final short[] sourceData = ((ShortDataRecord) dataRec).getShortData();
        BufferWrapper source = BufferWrapper.wrapArray(sourceData,
                coverage.getNx(), coverage.getNy());
        BufferWrapper dest = BufferWrapper.create(source.getPrimitiveType(),
                (int) extent.getWidth(), (int) extent.getHeight());
        try {
            dest = reprojection.reprojectedGrid(
                    new NearestNeighborInterpolation(), source, dest);
        } catch (FactoryException | TransformException e) {
            throw new PrecipCreationException(
                    "Failed to reproject the satellite data into the hrap sub-grid.",
                    e);
        }

        return new SatelliteData(pdo, dest);
    }

    private PrecipRecord buildPrecipRecord(final SatelliteData satelliteData)
            throws PrecipCreationException {
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