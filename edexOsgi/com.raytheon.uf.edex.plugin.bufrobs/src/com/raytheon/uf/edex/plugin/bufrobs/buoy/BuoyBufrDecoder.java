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
package com.raytheon.uf.edex.plugin.bufrobs.buoy;

import java.util.Set;

import com.raytheon.uf.common.dataplugin.sfcobs.ObsCommon;
import com.raytheon.uf.common.nc.bufr.BufrDataItem;
import com.raytheon.uf.common.nc.bufr.BufrParser;
import com.raytheon.uf.common.nc.bufr.util.BufrMapper;
import com.raytheon.uf.common.pointdata.spatial.SurfaceObsLocation;
import com.raytheon.uf.edex.plugin.bufrobs.AbstractBufrSfcObsDecoder;
import com.raytheon.uf.edex.plugin.bufrobs.BufrObsDecodeException;

/**
 * Buoy decoder for BUFR formatted sea sfc obs.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- ---------------------------------
 * Jun 11, 2014 3229       bclement     Initial creation
 * Jul 23, 2014 3410       bclement     location changed to floats
 * Sep 11, 2017 6406       bsteffen     Upgrade ucar
 * May 02, 2022 100863     smanoj       Add Null check to avoid location
 *                                      parsing problem.
 * Sep 15, 2023 2036140    tjensen      Fixed handling of fallback lat/lon
 *                                      fields.
 *
 * </pre>
 *
 * @author bclement
 */
public class BuoyBufrDecoder extends AbstractBufrSfcObsDecoder {

    public static final String BUOY_NAMESPACE = "buoy";

    public static final String ALIAS_FILE_NAME = BUOY_NAMESPACE + "-alias.xml";

    public static final String CATEGORY_FILE_NAME = BUOY_NAMESPACE
            + "-category.xml";

    public static final String PRECIP_FIELD = "precip";

    public static final String FALLBACK_LAT_FIELD = "Latitude_coarse_accuracy";

    public static final String FALLBACK_LON_FIELD = "Longitude_coarse_accuracy";

    public static final String WMO_SUB_AREA_FIELD = "WMO_Region_sub-area";

    public static final String BUOY_ID_FIELD = "Buoy-Platform_identifier";

    public static final String STATION_ID_FORMAT = "%d%d%03d";

    /**
     * @param pluginName
     * @throws BufrObsDecodeException
     */
    public BuoyBufrDecoder(String pluginName) throws BufrObsDecodeException {
        super(pluginName);
    }

    @Override
    protected void processField(ObsCommon record, BufrParser parser)
            throws BufrObsDecodeException {
        BufrMapper mapper = getMapper();
        String bufrName = parser.getFieldName();
        Set<String> baseNames = mapper.lookupBaseNamesOrEmpty(bufrName,
                BUOY_NAMESPACE);
        if (baseNames.isEmpty()) {
            /*
             * Fallback fields are not mapped, but we need to read them in as
             * the parser finds them to avoid reparsing the whole file
             */
            if (FALLBACK_LAT_FIELD.equals(bufrName)
                    || FALLBACK_LON_FIELD.equals(bufrName)) {
                processFallbackLocationField(record.getLocation(), parser,
                        bufrName);
            } else {
                log.debug("Skipping unmapped field: " + bufrName);
            }
        }
        for (String baseName : baseNames) {
            if (DEFAULT_LOCATION_FIELDS.contains(baseName)) {
                processLocationField(record.getLocation(), parser, baseName);
            } else if (PRECIP_FIELD.equalsIgnoreCase(baseName)) {
                processPrecip(record, parser);
            } else {
                processGeneralFields(record, parser, baseName);
            }
        }
    }

    /**
     * Read in the fallback location fields. If location information has not
     * already been set by the default fields, set the location information
     * using the fallback values. If the default location fields are later read
     * in, they should overwrite these fallback values.
     *
     * @param location
     *                     The record's location information
     * @param parser
     *                     BufrParser that contains the field information
     * @param bufrName
     *                     Name of the field being parsed
     * @throws BufrObsDecodeException
     */
    private void processFallbackLocationField(SurfaceObsLocation location,
            BufrParser parser, String bufrName) throws BufrObsDecodeException {
        /*
         * If latitude/longitude are already set using the default value, do not
         * override.
         */
        if (FALLBACK_LAT_FIELD.equals(bufrName)
                && location.getLatitude() == null) {
            Double lat = (Double) getFieldValue(parser, false);
            if (lat != null) {
                location.assignLatitude(lat.floatValue());
            }
        } else if (FALLBACK_LON_FIELD.equals(bufrName)
                && location.getLongitude() == null) {
            Double lon = (Double) getFieldValue(parser, false);
            if (lon != null) {
                location.assignLongitude(lon.floatValue());
            }
        }
    }

    @Override
    protected String createStationId(BufrParser parser)
            throws BufrObsDecodeException {
        /* WMO number is split into three parts */
        Number region = (Number) getFieldValue(parser, false);
        BufrDataItem subAreaData = parser.scanForStructField(WMO_SUB_AREA_FIELD,
                false);
        Number subArea = (Number) subAreaData.getValue();
        BufrDataItem buoyIdData = parser.scanForStructField(BUOY_ID_FIELD,
                false);
        Number bouyId = (Number) buoyIdData.getValue();
        if (region == null || subArea == null || bouyId == null) {
            String fields = parser.getFieldName() + ", " + WMO_SUB_AREA_FIELD
                    + ", or " + BUOY_ID_FIELD;
            throw new BufrObsDecodeException("BUFR file '" + parser.getFile()
                    + "' Missing one of the required station ID fields: "
                    + fields);
        }
        return String.format(STATION_ID_FORMAT, region.intValue(),
                subArea.intValue(), bouyId.intValue());
    }

    @Override
    protected ObsCommon finalizeRecord(BufrParser parser, ObsCommon record)
            throws BufrObsDecodeException {
        record = super.finalizeRecord(parser, record);
        finalizeLocation(parser, record);
        return record;
    }

    /**
     * Verify location information is present and assure location is set
     * correctly.
     *
     * @param parser
     * @param record
     * @throws BufrObsDecodeException
     */
    private void finalizeLocation(BufrParser parser, ObsCommon record)
            throws BufrObsDecodeException {
        SurfaceObsLocation location = record.getLocation();
        if (location.getLocation() == null) {
            throw new BufrObsDecodeException("BUFR file '" + parser.getFile()
                    + "' missing location information");
        }
        /*
         * Call assignLocation with the currently set latitude and longitude.
         * This is to handle the case where fallback lat/lon values were
         * overwritten by the default values. In those cases,
         * SurfaceObsLocation.assignLatitude() and assignLongitude() may not
         * have called assignLocation() with the newer information since
         * location was not null.
         */
        location.assignLocation(location.getLatitude(),
                location.getLongitude());

    }

    @Override
    protected String getAliasMapFile() {
        return ALIAS_FILE_NAME;
    }

    @Override
    protected String getCategoryFile() {
        return CATEGORY_FILE_NAME;
    }

}
