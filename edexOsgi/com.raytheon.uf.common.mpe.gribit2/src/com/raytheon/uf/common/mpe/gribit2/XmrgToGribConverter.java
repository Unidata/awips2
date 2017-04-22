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
package com.raytheon.uf.common.mpe.gribit2;

import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Date;
import java.util.Map;
import java.util.HashMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.awt.Rectangle;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Path;
import javax.xml.bind.JAXBException;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.mpe.gribit2.grib.BinaryDataSection;
import com.raytheon.uf.common.mpe.gribit2.grib.BitMapSection;
import com.raytheon.uf.common.mpe.gribit2.grib.GribFile;
import com.raytheon.uf.common.mpe.gribit2.grib.GridDefinitionLookupException;
import com.raytheon.uf.common.mpe.gribit2.grib.GridDefinitionSection;
import com.raytheon.uf.common.mpe.gribit2.grib.GridDefinitionSectionFactory;
import com.raytheon.uf.common.mpe.gribit2.grib.ProductDefinitionSection;
import com.raytheon.uf.common.mpe.gribit2.grib.Table11Flags;
import com.raytheon.uf.common.mpe.gribit2.grid.IGridDefinition;
import com.raytheon.uf.common.mpe.gribit2.inputs.XmrgToGridParameters;
import com.raytheon.uf.common.mpe.gribit2.inputs.XmrgToGridSubCenter;
import com.raytheon.uf.common.mpe.gribit2.lut.Center;
import com.raytheon.uf.common.mpe.gribit2.lut.Generator;
import com.raytheon.uf.common.mpe.gribit2.lut.LUTContentException;
import com.raytheon.uf.common.mpe.gribit2.lut.LUTReadException;
import com.raytheon.uf.common.mpe.gribit2.lut.Parameter;
import com.raytheon.uf.common.mpe.gribit2.lut.Qpf;
import com.raytheon.uf.common.mpe.gribit2.lut.QpfWMOS;
import com.raytheon.uf.common.mpe.gribit2.lut.Table0;
import com.raytheon.uf.common.mpe.gribit2.lut.Table128;
import com.raytheon.uf.common.mpe.gribit2.lut.Table2;
import com.raytheon.uf.common.mpe.gribit2.lut.TableA;
import com.raytheon.uf.common.mpe.gribit2.lut.TableC;
import com.raytheon.uf.common.mpe.gribit2.lut.XmrgToGridParameter;
import com.raytheon.uf.common.mpe.gribit2.lut.XmrgToGridParams;
import com.raytheon.uf.common.mpe.util.AppsDefaultsConversionWrapper;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.serialization.JAXBManager;
import com.raytheon.uf.common.site.SiteMap;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.xmrg.XmrgFile;
import com.raytheon.uf.common.xmrg.XmrgFile.XmrgHeader;
import com.raytheon.uf.common.xmrg.hrap.HrapConversionException;
import com.raytheon.uf.common.xmrg.hrap.HrapUtil;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Performs the xmrg to grib (specifically GRIB 1) conversion. Ported from:
 * /rary.ohd.pproc.gribit/TEXT/gbitmain.f.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 11, 2016 4619       bkowal      Initial creation
 * Jul 20, 2016 4619       bkowal      Implement grid definition lookup and header
 *                                     generation.
 * Aug 01, 2016 4619       bkowal      Gather additional required information.
 * Aug 10, 2016 4619       bkowal      Updated to use {@link GridDefinitionSectionFactory}.
 * Aug 11, 2016 4619       bkowal      Implemented proper bitmap handling.
 * Aug 18, 2016 4619       bkowal      Implemented grib file write.
 * 
 * </pre>
 * 
 * @author bkowal
 */

public class XmrgToGribConverter {

    private final Logger logger = LoggerFactory.getLogger(getClass());

    private static final String GRIBIT_LUT_ROOT = XmrgToGribConstants.GRIBIT_ROOT
            + IPathManager.SEPARATOR + "lut" + IPathManager.SEPARATOR;

    private static final String QPFWMOS_LUT_FILE = GRIBIT_LUT_ROOT
            + "mpe-qpfWMOS.xml";

    private static final String TABLE0_LUT_FILE = GRIBIT_LUT_ROOT
            + "mpe-Table0.xml";

    private static final String TABLE128_LUT_FILE = GRIBIT_LUT_ROOT
            + "mpe-Table128.xml";

    private static final String TABLE2_LUT_FILE = GRIBIT_LUT_ROOT
            + "mpe-Table2.xml";

    private static final String TABLEA_LUT_FILE = GRIBIT_LUT_ROOT
            + "mpe-TableA.xml";

    private static final String TABLEC_LUT_FILE = GRIBIT_LUT_ROOT
            + "mpe-TableC.xml";

    private static final String XMRGTOGRID_LUT_FILE = GRIBIT_LUT_ROOT
            + "mpe-xmrgToGridParams.xml";

    private static final String GRIB_HEADER_DATE_FMT = "ddHHmm";

    private static XmrgToGribConverter INSTANCE;

    private static final ThreadLocal<SimpleDateFormat> gribHeaderDF = new ThreadLocal<SimpleDateFormat>() {
        @Override
        protected SimpleDateFormat initialValue() {
            SimpleDateFormat sdf = new SimpleDateFormat(GRIB_HEADER_DATE_FMT);
            return sdf;
        }
    };

    private static final String GRIB_HEADER_FMT = "%-6s %-4s %s\r\r\n";

    private JAXBManager jaxbManager;

    /*
     * Lookup Tables
     */
    private final Map<Integer, Qpf> qpfWMOSLookupMap = new HashMap<>();

    private final Map<Integer, Center> table0LookupMap = new HashMap<>();

    private final Map<String, Parameter> table128LookupMap = new HashMap<>();

    private final Map<String, Parameter> table2LookupMap = new HashMap<>();

    private final Map<Integer, Generator> tableALookupMap = new HashMap<>();

    private final Map<String, Center> tableCLookupMap = new HashMap<>();

    private XmrgToGridParams xmrgToGridParams;

    /*
     * End Lookup Tables
     */

    protected XmrgToGribConverter() throws XmrgToGribInitializationException {
        initialize();
    }

    public static synchronized XmrgToGribConverter getInstance()
            throws XmrgToGribInitializationException {
        if (INSTANCE == null) {
            INSTANCE = new XmrgToGribConverter();
        }
        return INSTANCE;
    }

    public void execute(final Path xmrgInputPath, final Path gribOutputPath)
            throws XmrgToGribInitializationException,
            XmrgToGribConversionException {
        if (xmrgInputPath == null) {
            throw new IllegalArgumentException(
                    "Required argument 'xmrgInputPath' cannot be NULL.");
        }
        if (gribOutputPath == null) {
            throw new IllegalArgumentException(
                    "Required argument 'gribOutputPath' cannot be NULL.");
        }

        /*
         * Read the xmrg file.
         */
        GribitXmrgFile xmrgFile = new GribitXmrgFile(xmrgInputPath);
        try {
            /*
             * takes the place of: /rary.ohd.pproc.gribit/TEXT/rdxmrgg.f.
             */
            xmrgFile.load();
        } catch (IOException e) {
            throw new XmrgToGribConversionException(
                    "Failed to load xmrg file: " + xmrgInputPath.toString()
                            + ".", e);
        }

        /*
         * Grid Factor is based on Apps_defaults: hpe_hrap_grid_factor. Only pay
         * attention to grid factor if the process flag contains 'DHR' or 'DSP'.
         */
        int gridFactor = 1;
        final String processFlag = xmrgFile.getHeader().getProcessFlag();
        if (processFlag.contains(XmrgToGribConstants.PROC_CONTAINS_DHR)
                || processFlag.contains(XmrgToGribConstants.PROC_CONTAINS_DSP)) {
            final int hrapGridFactor = AppsDefaults.getInstance().getInt(
                    XmrgToGribConstants.AppsDefaults.HPE_HRAP_GRID_FACTOR,
                    gridFactor);
            if (hrapGridFactor == XmrgToGribConstants.QUARTER_HRAP_GRID_FACTOR) {
                gridFactor = XmrgToGribConstants.QUARTER_HRAP_GRID_FACTOR;
            }
        }

        /*
         * Check the grib_convert_data Apps_defaults property to determine if
         * data needs to be converted. When data is converted, data is converted
         * from hundredth of millimeters to millimeters. Either way the data
         * will be relocated to a float[].
         */
        final Boolean gribConvertData = AppsDefaultsConversionWrapper
                .getPropertyAsBoolean(XmrgToGribConstants.AppsDefaults.GRIB_CONVERT_DATA);
        // Variable 'ihfld' in the legacy code.
        short[] readXmrgData = xmrgFile.getData();
        // Variable 'fld' in the legacy code.
        final float[] xmrgData = new float[readXmrgData.length];
        /*
         * The property must indicate "OFF", otherwise 100.0 will be used as the
         * divisor.
         */
        final float convertDivisor = (Boolean.FALSE.equals(gribConvertData)) ? 1.0f
                : 100.0f;
        for (int i = 0; i < xmrgData.length; i++) {
            xmrgData[i] = (float) readXmrgData[i] / convertDivisor;
        }

        XmrgToGridParameters conversionParams = completeProcessLookups(
                xmrgFile.getHeader(), xmrgFile.getHrapExtent());
        logger.info("Determined conversion params: {}", conversionParams);

        IGridDefinition gridDefinition = null;
        try {
            gridDefinition = GridDefinitionManager.getInstance()
                    .retrieveGridDefinition(conversionParams.getNgrid());
        } catch (GridDefinitionNotFoundException e) {
            /*
             * Indicates that an unknown grid was encountered.
             */
            throw new XmrgToGribConversionException(
                    "Encountered an unexpected grid: "
                            + conversionParams.getNgrid() + ".", e);
        } catch (GridDefinitionException e) {
            /*
             * Indicates complete failure.
             */
            throw new XmrgToGribConversionException(
                    "Grid Definition retrieval failed.", e);
        }
        logger.info("Using grid definition: {}", gridDefinition.toString());

        final ProductDefinitionSection pds = constructGribPDS(conversionParams,
                xmrgFile.getHeader());
        logger.info("GRIB PDS = {}", pds.toString());

        logger.debug(
                "mwcol = {}, msrow = {}, ncol = {}, nrow = {}, mxval = {}",
                xmrgFile.getHrapExtent().x, xmrgFile.getHrapExtent().y,
                xmrgFile.getHrapExtent().width,
                xmrgFile.getHrapExtent().height, xmrgFile.getHeader()
                        .getMaxValue());
        /*
         * flag indicating whether or not the data should be "tossed" prior to
         * being packed. Data is only "tossed" when bitmaps are used. Flag will
         * be utilized for the binary data packing.
         */
        Boolean tossData = Boolean.FALSE;
        if (Boolean.TRUE.equals(pds.getIncludeBMS())) {
            tossData = Boolean.TRUE;
            convertData(xmrgFile, gridDefinition, conversionParams, gridFactor,
                    xmrgData);
        }

        /*
         * Generate the grib header. Will most likely become part of the grib
         * file write when fully implemented. However, legacy, generates the
         * grib header in a separate method before writing the file.
         */
        final String gribHeader = constructHeaderForUnformatted(
                conversionParams, xmrgFile.getHeader().getSaveDate());
        logger.info("GRIB Header: {}", gribHeader.trim());

        GridDefinitionSection<?> gds = null;
        if (Boolean.TRUE.equals(pds.getIncludeGDS())) {
            /*
             * Every Grid Definition Section generated during the xmrg -> grib
             * process are initialized to contain exactly the same content.
             */
            try {
                gds = GridDefinitionSectionFactory.getInstance()
                        .generateDefinitionSection(gridDefinition);
            } catch (GridDefinitionLookupException e) {
                throw new XmrgToGribConversionException(
                        "Failed to prepare the Grid Definition Section.", e);
            }
        }

        /*
         * Prepare the binary data section. All but two fields are
         * pre-initialized.
         */
        BinaryDataSection bds = new BinaryDataSection();
        Table11Flags table11Flags = new Table11Flags();
        table11Flags.setPacking(conversionParams.getPackageFlag());
        table11Flags.setOriginalDataType(conversionParams.getDataType());
        bds.setTable11Flags(table11Flags);

        BitPacking bitPacking = BitPackingCalculator.calculateBitPacking(
                pds.getIncludeBMS(), conversionParams.getBinf(),
                pds.getDecimalScaleFactor(), xmrgData);
        logger.info("Bit Packing = {}", bitPacking.toString());

        /*
         * SCALE THE DATA WITH D-SCALE FROM PDS(27-28). Within the legacy
         * gribit, the scaling value would either be applied to the xmrg data to
         * produce an integer array or a float array. However, the data type in
         * the baselined gribit is hard-coded to be floating point, so there is
         * no point in preparing an integer array that would never be used.
         */
        final int dScale = pds.getDecimalScaleFactor();
        final float scale = (float) Math.pow(10.0, dScale);
        for (int i = 0; i < xmrgData.length; i++) {
            xmrgData[i] *= scale;
        }

        /*
         * Pack the binary data.
         */
        BinaryDataSectionPacker bdsp = new BinaryDataSectionPacker();
        final PackBinaryDataResults pbds = bdsp.run(tossData, xmrgData,
                bitPacking);

        BitMapSection bms = null;
        if (Boolean.TRUE.equals(pds.getIncludeBMS())) {
            /*
             * Prepare the bitmap section.
             */
            bms = BitMapSection.generateBitMapSection(xmrgData);
        }

        /*
         * Finish populating the Binary Data Section (BDS).
         */
        bds.setNumberBytes(pbds.getLength()
                + BinaryDataSection.PREFIX_BYTES_LENGTH);
        /*
         * calculate the number of fill bits.
         */
        int numExpected = (bitPacking.getBitsToPack() * xmrgData.length)
                + (BinaryDataSection.PREFIX_BYTES_LENGTH * 8);
        int fillBits = 0;
        int nleft = numExpected % 16;
        if (nleft != 0) {
            numExpected = numExpected + 16 - nleft;
            fillBits = 16 - nleft;
        }
        bds.setNumberFillBits(fillBits);
        bds.setScaleFactor((int) pbds.getScale());
        bds.setBitsToPack(bitPacking.getBitsToPack());
        bds.setPackedData(pbds.getPackedData());

        /*
         * At this point, everything has been calculated and/or initialized and
         * we are now ready to start writing the grib file.
         */
        GribFile gribFile = new GribFile();
        gribFile.setHeader(gribHeader);
        /*
         * The indicator section will be constructed based on the definition of
         * all other sections.
         */
        gribFile.setPds(pds);
        gribFile.setGds(gds);
        gribFile.setBms(bms);
        gribFile.setBds(bds);
        gribFile.prepareIndicatorSection();
        try {
            final int bytesWritten = gribFile.writeGrib(gribOutputPath);
            logger.info(
                    "XMRG to Grib conversion was successful for xmrg file: {} -> grib file: {}. {} bytes written.",
                    xmrgInputPath.toString(), gribOutputPath.toString(),
                    bytesWritten);
        } catch (XmrgToGribConversionException e) {
            logger.error("XMRG to Grib conversion failed for xmrg file: "
                    + xmrgInputPath.toString() + " -> grib file: "
                    + gribOutputPath.toString() + ".", e);
            throw e;
        }
    }

    /**
     * This method converts data in HRAP format to the format associated with
     * the desired grid. Based on: /rary.ohd.pproc.gribit/TEXT/griddefg.f.
     * 
     * @param xmrgFile
     *            the {@link XmrgFile} that contains that data in HRAP format
     * @param gridDefinition
     *            attributes associated with the selected grid
     * @param conversionParams
     *            common xmrg -> grib conversion parameters
     * @param gridFactor
     *            indicates this is either a full grid or a quarter grid
     * @param xmrgData
     *            adjusted data read from the xmrg file
     * @throws XmrgToGribConversionException
     */
    private void convertData(final XmrgFile xmrgFile,
            final IGridDefinition gridDefinition,
            final XmrgToGridParameters conversionParams, final int gridFactor,
            final float[] xmrgData) throws XmrgToGribConversionException {
        final int mwcol = xmrgFile.getHrapExtent().x;
        final int msrow = xmrgFile.getHrapExtent().y;
        final int ncol = xmrgFile.getHrapExtent().width;
        final int nrow = xmrgFile.getHrapExtent().height;

        if (conversionParams.getNgrid() != XmrgToGribConstants.DEFAULT_NGRID
                && conversionParams.getNgrid() != XmrgToGribConstants.SUPPORTED_GRID_218) {
            throw new XmrgToGribConversionException("Grid "
                    + conversionParams.getNgrid() + " is not supported.");
        }

        /*
         * save off a few of the values because they may change under certain
         * conditions.
         */
        final int saveNcol = ncol;
        final int saveNrow = nrow;

        final int numPoints = saveNcol * saveNrow;
        // transfer the grid to a temporary work array - variable 'wfld' in the
        // legacy code.
        final float[] workGridArray = Arrays.copyOf(xmrgData, numPoints);

        if (conversionParams.getNgrid() == XmrgToGribConstants.DEFAULT_NGRID) {
            /*
             * GRIB grid number 240 - 4 km HRAP grid over contiguous United
             * States
             */

            // overwrite national values
            gridDefinition.setNumberXPoints(ncol);
            gridDefinition.setNumberYPoints(nrow);

            // convert lower left corner HRAP grid to lat/lon
            double addValue = 0.01;
            double xhrap = (double) mwcol + addValue;
            double yhrap = (double) msrow + addValue;
            Coordinate gridCoord = new Coordinate(xhrap, yhrap);
            Coordinate lowerLeftLatLonCoord = null;
            try {
                lowerLeftLatLonCoord = HrapUtil.hrapToLatLon(gridCoord);
            } catch (HrapConversionException e) {
                throw new XmrgToGribConversionException(
                        "Failed to determine the southwest corner lat/lon.", e);
            }
            /*
             * no need for a signed bit due to memory and storage limitations
             * (back when gribit was originally written). Necessary?
             */
            // swLatLonCoord.x = Math.abs(swLatLonCoord.x);
            logger.debug(
                    "in convertData - addval={}, xhrap={}, yhrap={}, elon={}, alat={} hrap to ll",
                    addValue, xhrap, yhrap, lowerLeftLatLonCoord.x,
                    lowerLeftLatLonCoord.y);

            /*
             * TODO: perform the reverse and validate the lat/lon converted to
             * hrap produces the original hrap inputs.
             */
            // latitude of origin
            gridDefinition
                    .setOriginLat((int) (lowerLeftLatLonCoord.y * 1000.0));
            // longitude of origin
            gridDefinition
                    .setOriginLon((int) (lowerLeftLatLonCoord.x * 1000.0));

            logger.debug("in convertData - Updated grid definition = {}",
                    gridDefinition.toString());

            /*
             * In Java, the 'bitmapArray' will already be initialized to 0. So,
             * initialize the xmrg data array to the xmrg ignore constant.
             */
            Arrays.fill(xmrgData, XmrgToGribConstants.XMRG_IGNORE_CONSTANT);

            logger.debug("in convertData - nrowh={}, ncolh={}", saveNrow,
                    saveNcol);
            for (int j = 0; j < saveNrow; j++) {
                for (int i = 0; i < saveNcol; i++) {
                    int kh = j * saveNcol + i;
                    int kg = j * ncol + i;
                    if (workGridArray[kh] >= 0.0) {
                        xmrgData[kg] = workGridArray[kh];
                    }
                }
            }
        } else if (conversionParams.getNgrid() == XmrgToGribConstants.SUPPORTED_GRID_218) {
            throw new XmrgToGribConversionException("Full support for Grid "
                    + XmrgToGribConstants.SUPPORTED_GRID_218
                    + " has not been implemented yet.");
            // TODO: implement support for Grid 218.
        }
    }

    /**
     * Constructs the Grib Product Definition Section (PDS) based on the
     * specified {@link XmrgToGridParameters} and {@link XmrgHeader}. Based on:
     * /rary.ohd.pproc.gribit/TEXT/engrib.f.
     * 
     * @param parameters
     *            the specified {@link XmrgToGridParameters}
     * @param xmrgHeader
     *            the specified {@link XmrgHeader}
     * @return the constructed {@link ProductDefinitionSection}.
     */
    private ProductDefinitionSection constructGribPDS(
            final XmrgToGridParameters parameters, final XmrgHeader xmrgHeader) {
        ProductDefinitionSection pds = new ProductDefinitionSection();
        pds.setParamTableVersionNum(parameters.getGribParamNumberFoundTable());
        pds.setOriginatingCenter(parameters.getOriginatingCenterId());
        pds.setModelId(parameters.getModelId());
        final int ngrid = (parameters.getNgrid() == XmrgToGribConstants.DEFAULT_NGRID) ? XmrgToGribConstants.PDS_NGRID
                : parameters.getNgrid();
        pds.setGrid(ngrid);
        pds.setParamUnitIndicator(parameters.getGribParamNumber());

        /*
         * Grib PDS Date/Time entries.
         */
        Calendar validCalendar = TimeUtil
                .newCalendar(xmrgHeader.getValidDate());
        final int year = validCalendar.get(Calendar.YEAR);
        final int yy = (int) (validCalendar.get(Calendar.YEAR) / 100);
        final int gribYear = year - (yy * 100);
        pds.setYear(gribYear);
        pds.setMonth(validCalendar.get(Calendar.MONTH) + 1);
        pds.setDay(validCalendar.get(Calendar.DATE));
        pds.setHour(validCalendar.get(Calendar.HOUR_OF_DAY));
        pds.setMinute(validCalendar.get(Calendar.MINUTE));

        pds.setForecastTimeUnit(parameters.getTunit());
        pds.setPeriodOfTimeP1(parameters.getNturef());
        pds.setPeriodOfTimeP2(parameters.getNtufc());
        pds.setTimeRangerIndicator(parameters.getTrang());
        /*
         * Since Java does not support natively retrieving the century ...
         */
        pds.setCentury(((validCalendar.get(Calendar.YEAR) - 1) / 100) + 1);
        pds.setSubCenter(parameters.getXmrgToGridSubCenter().getCode());
        pds.setDecimalScaleFactor(parameters.getDecimalPositions());
        return pds;
    }

    /**
     * Constructors the grib header. The Fortran code recognizes both formatted
     * and unformatted grib files. All xmrg to grib conversions produce an
     * unformatted grib file. Based on: /rary.ohd.pproc.gribit/TEXT/gpcomm.f.
     * 
     * @param parameters
     *            xmrg to grid parameters. Used to determine the wmo and office
     *            id.
     * @param saveDate
     *            the {@link Date} that the xmrg file was saved
     * @return the constructed grib header as a {@link String}.
     */
    private String constructHeaderForUnformatted(
            final XmrgToGridParameters parameters, final Date saveDate) {
        if (parameters == null) {
            throw new IllegalArgumentException(
                    "Required argument 'parameters' cannot be NULL.");
        }
        if (saveDate == null) {
            throw new IllegalArgumentException(
                    "Required argument 'saveDate' cannot be NULL.");
        }
        final String wmo = parameters.getWmo();
        if (wmo == null) {
            throw new IllegalArgumentException(
                    "Cannot generate a GRIB Header without the wmo; provided wmo = "
                            + wmo + ".");
        }
        final String office = (parameters.getXmrgToGridSubCenter() == null) ? StringUtils.EMPTY
                : parameters.getXmrgToGridSubCenter().getOffice();
        return String.format(GRIB_HEADER_FMT, wmo, office, gribHeaderDF.get()
                .format(saveDate));
    }

    /**
     * Retrieves and collects the needed xmrg to grib conversion information for
     * the specified {@link XmrgHeader} and the specified {@link Rectangle}
     * extents. Based on: /rary.ohd.pproc.gribit/TEXT/xm2gribg.f.
     * 
     * @param xmrgHeader
     *            the specified {@link XmrgHeader}
     * @param hrapExtent
     *            the specified {@link Rectangle} extents
     * @return a {@link XmrgToGridParameters} containing all of the conversion
     *         information that was retrieved and/or collected.
     * @throws XmrgToGribConversionException
     */
    private XmrgToGridParameters completeProcessLookups(
            final XmrgHeader xmrgHeader, final Rectangle hrapExtent)
            throws XmrgToGribConversionException {
        /*
         * Determine the sub-center code using Table C.
         */
        XmrgToGridSubCenter xmrgToGridSubCenter = lookupSubCenterCode(hrapExtent.x);
        if (xmrgToGridSubCenter == null) {
            throw new XmrgToGribConversionException(
                    "Failed to determine the Sub Center for: "
                            + xmrgHeader.getUserId() + ".");
        }
        final int originatingCenterId = XmrgToGribConstants.ORIGINATING_CENTER_ID;

        final String processFlag = xmrgHeader.getProcessFlag().trim();

        /*
         * Verify that the specified process flag is recognized.
         */
        XmrgToGridParameter foundParameter = null;

        for (XmrgToGridParameter parameter : xmrgToGridParams.getParameters()) {
            if (parameter.procMatches(processFlag)) {
                foundParameter = parameter;
                break;
            }
        }
        if (foundParameter == null) {
            throw new XmrgToGribConversionException(
                    "Xmrg file contained an unrecognized process flag: "
                            + processFlag + ".");
        }

        /*
         * Retrieve other corresponding parameters.
         */
        final int modelId = foundParameter.getModlid();
        final int ngrid = (foundParameter.getNgrid() == 0) ? XmrgToGribConstants.DEFAULT_NGRID
                : foundParameter.getNgrid();
        final int tunit = foundParameter.getTunit();
        final int trang = foundParameter.getTrang();

        /*
         * Determine the number of accumulation hours.
         */
        int accumulationHours = XmrgToGribConstants.DEFAULT_ACCUMULATION_HOURS;
        Matcher accumHourMatcher = XmrgToGribConstants.accumHourExtractPattern
                .matcher(processFlag);
        if (accumHourMatcher.matches()) {
            /*
             * regex will guarantee that it can be parsed. so, checking for a
             * parse exception will be omitted.
             */
            accumulationHours = Integer.parseInt(accumHourMatcher
                    .group(XmrgToGribConstants.ACCUM_HOURS_GROUP));
        }

        /*
         * Retrieve the valid hour (24 hour clock) for upcoming calculations.
         * And since most of Java Date has been deprecated, Calendar is used.
         */
        final Calendar validCalendar = TimeUtil.newCalendar(xmrgHeader
                .getValidDate());
        final int validHour = validCalendar.get(Calendar.HOUR_OF_DAY);
        /*
         * TODO: need a better, more descriptive name for these, as opposed to
         * the Fortran names, based on how they are used for the actual
         * conversion.
         */
        final int nturef = (foundParameter.getNturef() >= 0) ? foundParameter
                .getNturef() : validHour - accumulationHours;
        final int ntufc = (foundParameter.getNtufc() >= 0) ? foundParameter
                .getNtufc() : validHour;
        /*
         * Determine the wmo.
         */
        String wmo = null;
        if (!processFlag.startsWith(XmrgToGribConstants.PROC_STARTS_QPA)
                || !processFlag.startsWith(XmrgToGribConstants.PROC_STARTS_QPM)) {
            /*
             * Use the wmo found in the xmrg to grib lut.
             */
            wmo = foundParameter.getWmoid();
        } else {
            /*
             * Use the wmo id found in the qpfwmos lut.
             */
            final int num = ntufc / accumulationHours;
            Qpf qpf = qpfWMOSLookupMap.get(num);
            if (qpf != null) {
                wmo = qpf.getId();
            } else {
                throw new XmrgToGribConversionException(
                        "Failed to find a WMO in the qpf wmos lookup table for num: "
                                + num + ".");
            }
        }

        /*
         * Append the hours to the base GRIB parameter when the wildcard (*) is
         * present.
         */
        String gribParam = foundParameter.getGribParam();
        if (gribParam.endsWith(XmrgToGridParameter.WILDCARD_CHAR)) {
            gribParam = gribParam.replaceAll(Pattern
                    .quote(XmrgToGridParameter.WILDCARD_CHAR), StringUtils
                    .leftPad(Integer.toString(accumulationHours), 2, "0"));
        }

        /*
         * Determine grib parameter number. Will be ideally extracted from
         * either Table 128 or Table 2.
         */
        Integer gribParamNumberFoundTable = null;
        Integer gribParamNumber = null;
        Parameter parameter = table128LookupMap.get(gribParam);
        if (parameter == null) {
            /* Last chance. */
            parameter = table2LookupMap.get(gribParam);
            if (parameter == null) {
                throw new XmrgToGribConversionException(
                        "Unable to determine the grib parameter number for grib parameter: "
                                + gribParam + ".");
            } else {
                gribParamNumber = parameter.getNum();
                gribParamNumberFoundTable = Table2.TABLE_NUMBER;
            }
        } else {
            /*
             * If the parameter is also found in Table 2, the Table 2 parameter
             * will override the Table 128 parameter unless 'grib_ptbl_search'
             * has been set in Apps_defaults.
             */
            Parameter parameter2Override = table2LookupMap.get(gribParam);
            final Boolean ptblSearch = AppsDefaultsConversionWrapper
                    .getPropertyAsBoolean(XmrgToGribConstants.AppsDefaults.GRIB_PTBL_SEARCH);
            gribParamNumberFoundTable = Table128.TABLE_NUMBER;
            if (parameter2Override != null && !Boolean.TRUE.equals(ptblSearch)) {
                gribParamNumberFoundTable = Table2.TABLE_NUMBER;
                parameter = parameter2Override;
            }
            gribParamNumber = parameter.getNum();
        }

        /*
         * Retrieve flags corresponding to how the data should be written.
         */
        final int packageFlag = foundParameter.getPkflg();
        final int referenceValue = 0;
        final int binf = foundParameter.getBinf();
        /*
         * determines the number of decimal positions allowed.
         */
        final int decimalPositions = foundParameter.getDec();
        /*
         * data width.
         */
        final int width = foundParameter.getWidth();
        /*
         * possible data types: 0 = floating point, 1 = integer
         */
        final int dataType = 0;

        return new XmrgToGridParameters(xmrgToGridSubCenter,
                originatingCenterId, modelId, ngrid, tunit, trang,
                accumulationHours, nturef, ntufc, wmo, gribParam,
                gribParamNumberFoundTable, gribParamNumber, packageFlag,
                referenceValue, binf, decimalPositions, width, dataType);
    }

    /**
     * Looks up the sub center code based on the specified western most HRAP
     * column. Based on: /rary.ohd.pproc.gribit/TEXT/subctr.f.
     * 
     * @param mostWestHRAPColumn
     *            the specified western most HRAP column
     * @return the sub center code if one is found.
     */
    private XmrgToGridSubCenter lookupSubCenterCode(final int mostWestHRAPColumn) {
        /*
         * First, determine if the grib sub center should be set to 0.
         */
        final Boolean subCenter0 = AppsDefaultsConversionWrapper
                .getPropertyAsBoolean(XmrgToGribConstants.AppsDefaults.GRIB_SET_SUBCENTER_0);
        if (Boolean.TRUE.equals(subCenter0)) {
            return new XmrgToGridSubCenter(XmrgToGribConstants.SUB_CENTER_0);
        }

        String siteId = AppsDefaults.getInstance().getToken(
                XmrgToGribConstants.AppsDefaults.AWIPS_RFC_ID, null);
        if (siteId == null) {
            /*
             * Not found? Check the 'awips_send_id'.
             */
            siteId = AppsDefaults.getInstance().getToken(
                    XmrgToGribConstants.AppsDefaults.AWIPS_SEND_ID, null);
            if (siteId == null) {
                /*
                 * Still not found? It cannot be determined.
                 */
                return null;
            }
        }
        String fourCharSiteId = SiteMap.getInstance().getSite4LetterId(siteId);

        /*
         * Special case from the legacy code.
         */
        if ("KALR".equals(fourCharSiteId) && mostWestHRAPColumn > 1121) {
            fourCharSiteId = "TSJU";
        }
        /*
         * Attempt to lookup the sub-center code.
         */
        final Center center = tableCLookupMap.get(fourCharSiteId);
        if (center == null) {
            return null;
        }

        return new XmrgToGridSubCenter(center.getNum(), fourCharSiteId);
    }

    private void initialize() throws XmrgToGribInitializationException {
        try {
            jaxbManager = new JAXBManager(QpfWMOS.class, Table0.class,
                    Table128.class, Table2.class, TableA.class, TableC.class,
                    XmrgToGridParams.class);
        } catch (JAXBException e) {
            throw new XmrgToGribInitializationException(
                    "Failed to instantiate the JAXB Manager.", e);
        }

        try {
            initializeLookupTables();
        } catch (LUTReadException | LUTContentException e) {
            throw new XmrgToGribInitializationException(
                    "Failed to initialize the lookup tables.", e);
        }
    }

    /**
     * Reads the lookup tables and prepares mappings. Based on:
     * /rary.ohd.pproc.gribit/TEXT/loadtbl.f.
     * 
     * @throws LUTReadException
     * @throws LUTContentException
     */
    private void initializeLookupTables() throws LUTReadException,
            LUTContentException {
        QpfWMOS qpfWMOS = readLookupTable(QPFWMOS_LUT_FILE, QpfWMOS.class);
        Table0 table0 = readLookupTable(TABLE0_LUT_FILE, Table0.class);
        Table128 table128 = readLookupTable(TABLE128_LUT_FILE, Table128.class);
        Table2 table2 = readLookupTable(TABLE2_LUT_FILE, Table2.class);
        TableA tableA = readLookupTable(TABLEA_LUT_FILE, TableA.class);
        TableC tableC = readLookupTable(TABLEC_LUT_FILE, TableC.class);
        xmrgToGridParams = readLookupTable(XMRGTOGRID_LUT_FILE,
                XmrgToGridParams.class);

        qpfWMOSLookupMap.clear();
        table0LookupMap.clear();
        table128LookupMap.clear();
        table2LookupMap.clear();
        tableALookupMap.clear();
        tableCLookupMap.clear();

        if (CollectionUtils.isNotEmpty(qpfWMOS.getQpfs())) {
            for (Qpf qpf : qpfWMOS.getQpfs()) {
                qpfWMOSLookupMap.put(qpf.getNum(), qpf);
            }
        } else {
            throw new LUTContentException(
                    "The Table QPF WMOS lookup table is unexpectedly empty.");
        }

        if (CollectionUtils.isNotEmpty(table0.getCenters())) {
            for (Center center : table0.getCenters()) {
                table0LookupMap.put(center.getNum(), center);
            }
        } else {
            throw new LUTContentException(
                    "The Table 0 lookup table is unexpectedly empty.");
        }

        if (CollectionUtils.isNotEmpty(table128.getParameters())) {
            for (Parameter parameter : table128.getParameters()) {
                table128LookupMap.put(parameter.getId(), parameter);
            }
        } else {
            throw new LUTContentException(
                    "The Table 128 lookup table is unexpectedly empty.");
        }

        if (CollectionUtils.isNotEmpty(table2.getParameters())) {
            for (Parameter parameter : table2.getParameters()) {
                table2LookupMap.put(parameter.getId(), parameter);
            }
        } else {
            throw new LUTContentException(
                    "The Table 2 lookup table is unexpectedly empty.");
        }

        if (CollectionUtils.isNotEmpty(tableA.getGenerators())) {
            for (Generator generator : tableA.getGenerators()) {
                tableALookupMap.put(generator.getNum(), generator);
            }
        } else {
            throw new LUTContentException(
                    "The Table A lookup table is unexpectedly empty.");
        }

        if (CollectionUtils.isNotEmpty(tableC.getCenters())) {
            for (Center center : tableC.getCenters()) {
                tableCLookupMap.put(center.getId(), center);
            }
        } else {
            throw new LUTContentException(
                    "The Table C lookup table is unexpectedly empty.");
        }

        /*
         * Scan the xmrg to grid parameters for process ids that support
         * wildcards.
         */
        if (CollectionUtils.isNotEmpty(xmrgToGridParams.getParameters())) {
            /*
             * TODO: Java 8 streams.
             */
            for (XmrgToGridParameter parameter : xmrgToGridParams
                    .getParameters()) {
                parameter.checkForWildcard();
            }
        } else {
            throw new LUTContentException(
                    "The XMRG to Grid Parameters lookup table is unexpectedly empty.");
        }
    }

    private <T> T readLookupTable(final String lutFileName,
            final Class<T> lutClass) throws LUTReadException {
        logger.info("Reading lookup table file: " + lutFileName + " ...");

        IPathManager pathManager = PathManagerFactory.getPathManager();
        ILocalizationFile localizationFile = pathManager
                .getStaticLocalizationFile(lutFileName);
        if (localizationFile == null || !localizationFile.exists()) {
            throw new LUTReadException(lutFileName,
                    "Unable to find the lookup table.");
        }

        try (InputStream is = localizationFile.openInputStream()) {
            T lut = lutClass.cast(jaxbManager.unmarshalFromInputStream(
                    lutClass, is));
            logger.info("Successfully read lookup table file: " + lutFileName
                    + ".");
            return lut;
        } catch (Exception e) {
            throw new LUTReadException(lutFileName, e);
        }
    }
}