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
 * ------------ ---------- ----------- --------------------------
 * Jun 11, 2014 3229       bclement     Initial creation
 * Jul 23, 2014 3410       bclement    location changed to floats
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class BuoyBufrDecoder extends AbstractBufrSfcObsDecoder {

    public static final String BUOY_NAMESPACE = "buoy";

    public static final String ALIAS_FILE_NAME = BUOY_NAMESPACE
            + "-alias.xml";

    public static final String CATEGORY_FILE_NAME = BUOY_NAMESPACE
            + "-category.xml";

    public static final String PRECIP_FIELD = "precip";

    public static final String PRECIP_TIME_PERIOD_FIELD = "Time period or displacement-2";

    public static final String FALLBACK_LAT_FIELD = "Latitude (high accuracy)";

    public static final String FALLBACK_LON_FIELD = "Longitude (high accuracy)";

    public static final String WMO_SUB_AREA_FIELD = "WMO Region sub-area";

    public static final String BUOY_ID_FIELD = "Buoy/platform identifier";

    public static final String STATION_ID_FORMAT = "%d%d%03d";

    /**
     * @param pluginName
     * @throws BufrObsDecodeException
     */
    public BuoyBufrDecoder(String pluginName) throws BufrObsDecodeException {
        super(pluginName);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.plugin.bufrobs.AbstractBufrSfcObsDecoder#processField
     * (com.raytheon.uf.common.dataplugin.sfcobs.ObsCommon,
     * com.raytheon.uf.common.nc.bufr.BufrParser)
     */
    @Override
    protected void processField(ObsCommon record, BufrParser parser)
            throws BufrObsDecodeException {
        BufrMapper mapper = getMapper();
        String bufrName = parser.getFieldName();
        Set<String> baseNames = mapper.lookupBaseNamesOrEmpty(bufrName,
                BUOY_NAMESPACE);
        if (baseNames.isEmpty()) {
            log.debug("Skipping unmapped field: " + bufrName);
        }
        for (String baseName : baseNames) {
            if (DEFAULT_LOCATION_FIELDS.contains(baseName)) {
                processLocationField(record.getLocation(), parser, baseName);
            } else if (PRECIP_FIELD.equalsIgnoreCase(baseName)) {
                processPrecip(record, parser, PRECIP_TIME_PERIOD_FIELD);
            } else {
                processGeneralFields(record, parser, baseName);
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.plugin.bufrobs.AbstractBufrSfcObsDecoder#createStationId
     * (com.raytheon.uf.common.nc.bufr.BufrParser)
     */
    @Override
    protected String createStationId(BufrParser parser)
            throws BufrObsDecodeException {
        /* WMO number is split into three parts */
        Number region = (Number) getFieldValue(parser, false);
        BufrDataItem subAreaData = parser.scanForStructField(
                WMO_SUB_AREA_FIELD, false);
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

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.plugin.bufrobs.AbstractBufrSfcObsDecoder#finalizeRecord
     * (com.raytheon.uf.common.nc.bufr.BufrParser,
     * com.raytheon.uf.common.dataplugin.sfcobs.ObsCommon)
     */
    @Override
    protected ObsCommon finalizeRecord(BufrParser parser, ObsCommon record)
            throws BufrObsDecodeException {
        record = super.finalizeRecord(parser, record);
        finalizeLocation(parser, record);
        return record;
    }

    /**
     * @param parser
     * @param record
     * @throws BufrObsDecodeException
     */
    private void finalizeLocation(BufrParser parser, ObsCommon record)
            throws BufrObsDecodeException {
        SurfaceObsLocation location = record.getLocation();
        if (location.getLocation() == null) {
            /* Argos not available, fallback to coarse lon/lat */
            BufrDataItem lonData = parser.scanForStructField(
                    FALLBACK_LON_FIELD, false);
            Number lon = (Number) lonData.getValue();
            BufrDataItem latData = parser.scanForStructField(
                    FALLBACK_LAT_FIELD, false);
            Number lat = (Number) latData.getValue();
            if (lon == null || lat == null) {
                throw new BufrObsDecodeException("BUFR file '"
                        + parser.getFile() + "' missing location information");
            }
            location.assignLocation(lat.floatValue(), lon.floatValue());
        }
    }

    /* (non-Javadoc)
     * @see com.raytheon.uf.edex.plugin.bufrobs.AbstractBufrSfcObsDecoder#getAliasMapFile()
     */
    @Override
    protected String getAliasMapFile() {
        return ALIAS_FILE_NAME;
    }

    /* (non-Javadoc)
     * @see com.raytheon.uf.edex.plugin.bufrobs.AbstractBufrSfcObsDecoder#getCategoryFile()
     */
    @Override
    protected String getCategoryFile() {
        return CATEGORY_FILE_NAME;
    }
    
}
