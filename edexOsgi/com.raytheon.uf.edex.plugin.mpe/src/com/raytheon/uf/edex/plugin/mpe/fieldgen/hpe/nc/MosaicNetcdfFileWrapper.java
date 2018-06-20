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
package com.raytheon.uf.edex.plugin.mpe.fieldgen.hpe.nc;

import java.awt.Rectangle;
import java.nio.file.Path;
import java.text.DateFormat;
import java.text.SimpleDateFormat;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.raytheon.uf.common.nc4.NcConstants;
import com.raytheon.uf.common.nc4.NcDimension;
import com.raytheon.uf.common.nc4.Netcdf;
import com.raytheon.uf.common.nc4.NetcdfException;
import com.raytheon.uf.common.nc4.NcDimension.IntDimension;
import com.raytheon.uf.common.nc4.NcDimension.ShortDimension;
import com.raytheon.uf.common.nc4.NcVariable.ByteVariable;
import com.raytheon.uf.common.nc4.NcVariable.FloatVariable;
import com.raytheon.uf.common.nc4.NcVariable.ShortVariable;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.edex.plugin.mpe.apps.AppsDefaultsConfigLoader;
import com.raytheon.uf.edex.plugin.mpe.apps.AppsDefaultsLoadException;
import com.raytheon.uf.edex.plugin.mpe.apps.RequiredTokenMissingException;
import com.raytheon.uf.edex.plugin.mpe.fieldgen.hpe.HPEFieldgenConstants;

/**
 * Utilizes {@link Netcdf} to generate a NetCDF file for a Field Gen Mosaic.
 * Based on: hpe_fieldgen/TEXT/save_netcdf.c.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 19, 2016 5631       bkowal      Initial creation
 *
 * </pre>
 *
 * @author bkowal
 */

public class MosaicNetcdfFileWrapper {

    private final Logger logger = LoggerFactory.getLogger(getClass());

    private static final DateFormat currentDateTimeNcFormat = new SimpleDateFormat(
            "MMddyyyyHHmmss");

    private final double[][] mosaic;

    private final String processFlag;

    private final Rectangle geoGridData;

    private final String formattedXmrgDate;

    public MosaicNetcdfFileWrapper(final double[][] mosaic,
            final String processFlag, final Rectangle geoGridData,
            final String formattedXmrgDate) {
        this.mosaic = mosaic;
        this.processFlag = processFlag;
        this.geoGridData = geoGridData;
        this.formattedXmrgDate = formattedXmrgDate;
    }

    public void generate(final Path netCdfFilePath) {
        /*
         * Read the Apps Defaults configuration.
         */
        NcConfig config = new NcConfig();
        try {
            AppsDefaultsConfigLoader.populateFromAppsDefaults(config);
        } catch (RequiredTokenMissingException | AppsDefaultsLoadException e) {
            logger.error("Failed to configure the Mosaic NetCdf File Wrapper.",
                    e);
            return;
        }

        /*
         * Determine which values to assign to various variables based on the
         * process flag.
         */
        String procName = HPENcConstants.VariableConstants.OTHER_PROC_NAME;
        String longName = HPENcConstants.VariableConstants.OTHER_LONG_NAME_VALUE;
        String units = HPENcConstants.VariableConstants.OTHER_UNITS_VALUE;
        String gridLabel = HPENcConstants.VariableConstants.OTHER_GRID_LABEL_VALUE;
        String resLabel = HPENcConstants.VariableConstants.OTHER_RES_LABEL_VALUE;
        if (HPEFieldgenConstants.DHR_PROC_FLAG.equals(processFlag)) {
            procName = HPENcConstants.VariableConstants.DHR_PROC_NAME;
            longName = HPENcConstants.VariableConstants.DHR_LONG_NAME_VALUE;
            units = HPENcConstants.VariableConstants.DHR_UNITS_VALUE;
            gridLabel = HPENcConstants.VariableConstants.DHR_GRID_LABEL_VALUE;
            resLabel = HPENcConstants.VariableConstants.DHR_RES_LABEL_VALUE;
        }

        final String formattedCurrentDate = currentDateTimeNcFormat
                .format(TimeUtil.newGmtCalendar().getTime());

        try (Netcdf mosaicNetCdf = new Netcdf(netCdfFilePath.toString(),
                NcConstants.NC_CLOBBER)) {
            ShortDimension hrapXDim = mosaicNetCdf.defineDim(
                    HPENcConstants.DimensionConstants.DIM_HRAPX,
                    geoGridData.width, ShortDimension.class, false);
            ShortDimension hrapYDim = mosaicNetCdf.defineDim(
                    HPENcConstants.DimensionConstants.DIM_HRAPY,
                    geoGridData.height, ShortDimension.class, false);
            IntDimension latLongDim = mosaicNetCdf.defineDim(
                    HPENcConstants.DimensionConstants.DIM_LATLONG, 4,
                    IntDimension.class, false);
            IntDimension datesDim = mosaicNetCdf.defineDim(
                    HPENcConstants.DimensionConstants.DIM_DATES, 11,
                    IntDimension.class, false);

            // proc name variable and attributes
            ShortVariable varProcName = mosaicNetCdf.defineVar(procName,
                    new NcDimension[] { hrapYDim, hrapXDim },
                    ShortVariable.class);
            varProcName.putStringAttribute(
                    HPENcConstants.VariableConstants.LONG_NAME, longName);
            varProcName.putStringAttribute(
                    HPENcConstants.VariableConstants.UNITS, units);
            varProcName.putStringAttribute(
                    HPENcConstants.VariableConstants.GRID, gridLabel);
            varProcName.putStringAttribute(
                    HPENcConstants.VariableConstants.RESOLUTION, resLabel);
            varProcName.putStringAttribute(
                    HPENcConstants.VariableConstants.DATEOFDATA,
                    formattedXmrgDate + "Z");
            varProcName.putStringAttribute(
                    HPENcConstants.VariableConstants.DATEOFCREATION,
                    formattedCurrentDate + "Z");
            varProcName.putStringAttribute(
                    HPENcConstants.VariableConstants.SOURCE,
                    config.getLocation());
            varProcName.putStringAttribute(
                    HPENcConstants.VariableConstants.COMMENTS,
                    HPENcConstants.VariableConstants.COMMENTS_PROCESS_VALUE);

            // lat variable
            FloatVariable varLat = mosaicNetCdf.defineVar(
                    HPENcConstants.VariableConstants.LAT,
                    new NcDimension[] { latLongDim }, FloatVariable.class);
            varLat.putStringAttribute(HPENcConstants.VariableConstants.COMMENTS,
                    HPENcConstants.VariableConstants.LAT_ORDER);

            // lon variable
            FloatVariable varLon = mosaicNetCdf.defineVar(
                    HPENcConstants.VariableConstants.LON,
                    new NcDimension[] { latLongDim }, FloatVariable.class);
            varLon.putStringAttribute(HPENcConstants.VariableConstants.COMMENTS,
                    HPENcConstants.VariableConstants.LON_ORDER);

            // true_lat variable
            FloatVariable varTrueLat = mosaicNetCdf.defineVar(
                    HPENcConstants.VariableConstants.TRUE_LAT,
                    new NcDimension[] {}, FloatVariable.class);

            // true_lon variable
            FloatVariable varTrueLon = mosaicNetCdf.defineVar(
                    HPENcConstants.VariableConstants.TRUE_LON,
                    new NcDimension[] {}, FloatVariable.class);

            // timeofdata variable
            ByteVariable varTimeOfData = mosaicNetCdf.defineVar(
                    HPENcConstants.VariableConstants.TIMEOFDATA,
                    new NcDimension[] { datesDim }, ByteVariable.class);

            // timeofcreation variable
            ByteVariable varTimeOfCreation = mosaicNetCdf.defineVar(
                    HPENcConstants.VariableConstants.TIMEOFCREATION,
                    new NcDimension[] { datesDim }, ByteVariable.class);

            // hrap_xor variable
            FloatVariable varHrapXor = mosaicNetCdf.defineVar(
                    HPENcConstants.VariableConstants.HRAP_XOR,
                    new NcDimension[] {}, FloatVariable.class);
            varHrapXor.putStringAttribute(
                    HPENcConstants.VariableConstants.COMMENTS,
                    HPENcConstants.VariableConstants.COMMENTS_HRAP_XOR_VALUE);

            // hrap_yor variable
            FloatVariable varHrapYor = mosaicNetCdf.defineVar(
                    HPENcConstants.VariableConstants.HRAP_YOR,
                    new NcDimension[] {}, FloatVariable.class);
            varHrapYor.putStringAttribute(
                    HPENcConstants.VariableConstants.COMMENTS,
                    HPENcConstants.VariableConstants.COMMENTS_HRAP_YOR_VALUE);

            mosaicNetCdf.endFileDefinition();

            // prepare the precip data
            short[] precip = new short[geoGridData.width * geoGridData.height];
            int k = 0;
            for (int j = 0; j < geoGridData.height; j++) {
                for (int i = 0; i < geoGridData.width; i++) {
                    precip[k] = (short) mosaic[j][i];
                    k++;
                }
            }

            // write data
            varLat.putVar(new int[] { 0 }, new int[] { 4 },
                    new float[] { config.getSwLat(), config.getSeLat(),
                            config.getNeLat(), config.getNwLat() });
            varLon.putVar(new int[] { 0 }, new int[] { 4 },
                    new float[] { config.getSwLon(), config.getSeLon(),
                            config.getNeLon(), config.getNwLon() });
            varTrueLat.putVar(new int[] { 0 }, new int[] { 1 },
                    new float[] { 60 });
            varTrueLon.putVar(new int[] { 0 }, new int[] { 1 },
                    new float[] { 105 });
            varHrapXor.putVar(new int[] { 0 }, new int[] { 1 },
                    new float[] { 0 });
            varHrapYor.putVar(new int[] { 0 }, new int[] { 1 },
                    new float[] { 0 });
            varProcName.putVar(new int[] { 0, 0 },
                    new int[] { geoGridData.height, geoGridData.width },
                    precip);
            varTimeOfData.putVar(new int[] { 0 }, new int[] { 9 },
                    stringToIntByteArray(formattedXmrgDate));
            varTimeOfCreation.putVar(new int[] { 0 }, new int[] { 9 },
                    stringToIntByteArray(formattedCurrentDate));
            logger.info("Successfully wrote NetCDF file: {}.",
                    netCdfFilePath.toString());
        } catch (NetcdfException e) {
            logger.error("Failed to write NetCDF file: "
                    + netCdfFilePath.toString() + ".", e);
        }
    }

    private byte[] stringToIntByteArray(final String value) {
        final int[] valueIntChars = value.chars().toArray();
        final byte[] valueBytes = new byte[valueIntChars.length];
        for (int i = 0; i < valueBytes.length; i++) {
            valueBytes[i] = (byte) valueIntChars[i];
        }

        return valueBytes;
    }
}