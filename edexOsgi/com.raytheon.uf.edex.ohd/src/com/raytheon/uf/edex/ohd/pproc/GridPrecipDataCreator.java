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
package com.raytheon.uf.edex.ohd.pproc;

import java.awt.Rectangle;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.Date;

import javax.measure.IncommensurableException;
import javax.measure.UnconvertibleException;
import javax.measure.Unit;
import javax.measure.UnitConverter;
import javax.measure.format.ParserException;
import javax.measure.quantity.Length;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.mpe.constants.FilePermissionConstants;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.ohd.AppsDefaultsDirKeys;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.units.UnitConv;
import com.raytheon.uf.common.xmrg.XmrgFile;
import com.raytheon.uf.common.xmrg.XmrgFile.XmrgHeader;
import com.raytheon.uf.common.xmrg.hrap.HRAP;
import com.raytheon.uf.common.xmrg.hrap.HRAPCoordinates;
import com.raytheon.uf.edex.database.plugin.PluginDao;
import com.raytheon.uf.edex.database.plugin.PluginFactory;

import si.uom.SI;
import tec.uom.se.unit.MetricPrefix;

/**
 * Generates a Satellite Precipitation XMRG data file based on the data
 * associated with the specified {@link GridRecord}. Essentially
 * SatPrecipFileBuilder converted to a {@link IPrecipDataCreator}.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 13, 2017 6407       bkowal      Initial creation
 * Dec 14, 2017 6407       bkowal      Ensure that Parameter information is set
 *                                     on GridRecords that will be processed.
 *
 * </pre>
 *
 * @author bkowal
 */

public class GridPrecipDataCreator implements IPrecipDataCreator<GridRecord> {

    private static final String FILEPRE = "SATPRE";

    private static final String DATE_FORMAT = "yyyyMMddHH";

    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(getClass());

    private final Rectangle extent;

    private final int height;

    private final int width;

    private final int minX;

    private final int minY;

    private final int maxY;

    private final ThreadLocal<SimpleDateFormat> sdf = TimeUtil
            .buildThreadLocalSimpleDateFormat(DATE_FORMAT,
                    TimeUtil.GMT_TIME_ZONE);

    private final String outputPath;

    public GridPrecipDataCreator() throws PrecipCreationException {
        try {
            extent = HRAPCoordinates.getHRAPCoordinates();
        } catch (NumberFormatException | IOException e) {
            throw new PrecipCreationException(
                    "Failed to retrieve the HRAP Coordinates.", e);
        }
        maxY = (int) (HRAP.getInstance().getNy() - extent.getMinY());
        minY = (int) (maxY - extent.getHeight());
        minX = (int) extent.getMinX();
        width = (int) (extent.getWidth());
        height = (int) (extent.getHeight());

        AppsDefaults appsDefaults = AppsDefaults.getInstance();
        outputPath = appsDefaults.getToken(AppsDefaultsDirKeys.MPE_SATPRE_DIR);
        File outputDir = new File(outputPath);
        if (!outputDir.exists()) {
            try {
                com.raytheon.uf.common.util.file.Files.createDirectories(
                        outputDir.toPath(),
                        FilePermissionConstants.POSIX_DIRECTORY_ATTRIBUTES);
            } catch (IOException e) {
                throw new PrecipCreationException(
                        "Failed to create the XMRG output directory: "
                                + outputPath + ".",
                        e);
            }
        }
    }

    @Override
    public PluginDataObject[] build(PluginDataObject pdo)
            throws PrecipCreationException {
        /*
         * Need to retrieve the Parameter information for Grid Records.
         * Parameter retrieval is already implemented within the GridRecord
         * constructor that takes a Data URI (String).
         */
        GridRecord gridRecord = new GridRecord(pdo.getDataURI());
        build(gridRecord);
        return null;
    }

    public void build(GridRecord pdo) throws PrecipCreationException {
        pdo = retrieveGridData(pdo);
        float[] fa = (float[]) pdo.getMessageData();
        String gribUnit = pdo.getParameter().getUnitString();
        UnitConverter cv = null;
        Unit<?> gi = null;
        try {
            gi = UnitConv.deserializer(gribUnit);
        } catch (ParserException e) {
            throw new PrecipCreationException(
                    "Failed to parse grib unit: " + gribUnit + ".", e);
        }
        Unit<Length> xOut = MetricPrefix.MILLI(SI.METRE);
        try {
            cv = gi.getConverterToAny(xOut);
        } catch (UnconvertibleException | IncommensurableException e) {
            throw new PrecipCreationException(
                    "Failed to acquire a Unit Converter for grib unit: "
                            + gribUnit + " to desired unit: " + xOut.toString()
                            + ".",
                    e);
        }
        short[] data = new short[fa.length];
        int k = 0;
        for (int i = 0; i < height; i++) {
            for (int j = 0; j < width; j++) {
                if (fa[k] < 0) {
                    fa[k] = 0;
                }
                data[(i * width) + j] = (short) (cv.convert(fa[k]) * 100.0);
                k++;
            }
        }
        writeXmrgFile(data, pdo.getDataTime().getRefTime());
    }

    private GridRecord retrieveGridData(final GridRecord pdo)
            throws PrecipCreationException {
        PluginDao dao = null;
        try {
            dao = PluginFactory.getInstance().getPluginDao(pdo.getPluginName());
        } catch (PluginException e) {
            throw new PrecipCreationException(
                    "Failed to retrieve the Grid Dao.", e);
        }
        IDataStore dataStore = dao.getDataStore(pdo);

        int[] minIndex = { minX, minY };
        int[] maxIndex = { minX + width, minY + height };
        IDataRecord dataRec;
        try {
            dataRec = dataStore.retrieve(pdo.getDataURI(), "Data",
                    Request.buildSlab(minIndex, maxIndex));
        } catch (FileNotFoundException | StorageException e) {
            throw new PrecipCreationException(
                    "Failed to retrieve the grid data.", e);
        }
        if (dataRec instanceof FloatDataRecord) {
            pdo.setMessageData(((FloatDataRecord) dataRec).getFloatData());
        } else {
            pdo.setMessageData(dataRec);
        }
        return pdo;
    }

    private void writeXmrgFile(short[] data, Date date)
            throws PrecipCreationException {
        String fname = outputPath + File.separatorChar + FILEPRE
                + sdf.get().format(date) + "z";
        XmrgFile xmfile = new XmrgFile(fname);
        XmrgHeader xmhead = new XmrgHeader();
        xmhead.setValidDate(date);
        xmhead.setSaveDate(date);
        xmfile.setHeader(xmhead);
        xmfile.setHrapExtent(extent);
        xmfile.setData(data);
        try {
            xmfile.save(fname, FilePermissionConstants.POSIX_FILE_SET);
        } catch (IOException e) {
            throw new PrecipCreationException(
                    "Failed to write satellite precip xmrg file: " + fname
                            + ".",
                    e);
        }
        statusHandler.info(
                "Successfully created satellite precip xmrg file: " + fname);
    }
}