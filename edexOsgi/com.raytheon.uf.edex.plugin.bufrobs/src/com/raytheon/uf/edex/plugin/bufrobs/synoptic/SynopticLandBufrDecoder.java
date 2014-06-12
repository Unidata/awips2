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
package com.raytheon.uf.edex.plugin.bufrobs.synoptic;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

import com.raytheon.edex.plugin.sfcobs.SfcObsPointDataTransform;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.sfcobs.ObsCommon;
import com.raytheon.uf.common.nc.bufr.BufrParser;
import com.raytheon.uf.common.nc.bufr.util.BufrMapper;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.common.pointdata.spatial.ObStation;
import com.raytheon.uf.common.pointdata.spatial.SurfaceObsLocation;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.decodertools.core.IDecoderConstants;
import com.raytheon.uf.edex.plugin.bufrobs.AbstractBufrSfcObsDecoder;
import com.raytheon.uf.edex.plugin.bufrobs.BufrObsDecodeException;
import com.raytheon.uf.edex.plugin.bufrobs.MissingRequiredDataException;
import com.raytheon.uf.edex.pointdata.spatial.ObStationDao;
import com.vividsolutions.jts.geom.Point;

/**
 * Synoptic Land type decoder for BUFR observations. Handles fixed and mobile
 * obs.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 21, 2014 2906       bclement     Initial creation
 * Jun 12, 2014 3229       bclement     moved location fields to parent class
 *                                      moved processGeneralFields() and processPrecip() to parent class
 *                                      override parent createStationId(), removed getTranslationFile() override
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class SynopticLandBufrDecoder extends AbstractBufrSfcObsDecoder {

    public static final Set<String> SUB_STRUCT_FIELDS = new HashSet<String>(
            Arrays.asList(SfcObsPointDataTransform.WIND_GUST));

    public static final String SYNOPTIC_LAND_NAMESPACE = "synoptic_land";

    public static final String ALIAS_FILE_NAME = SYNOPTIC_LAND_NAMESPACE
            + "-alias.xml";

    public static final String CATEGORY_FILE_NAME = SYNOPTIC_LAND_NAMESPACE
            + "-category.xml";

    public static final String PRECIP_FIELD = "precip";

    public static final String PRECIP_TIME_PERIOD_FIELD = "Time period or displacement";

    private final ObStationDao stationDao = new ObStationDao();

    /**
     * @param pluginName
     * @throws BufrObsDecodeException
     * @throws PluginException
     * @throws SerializationException
     */
    public SynopticLandBufrDecoder(String pluginName)
            throws BufrObsDecodeException {
        super(pluginName);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.plugin.bufrobs.AbstractBufrObsDecoder#processField
     * (com.raytheon.uf.common.dataplugin.sfcobs.ObsCommon,
     * com.raytheon.uf.common.nc.bufr.BufrParser, int)
     */
    @Override
    protected void processField(ObsCommon record, BufrParser parser)
            throws BufrObsDecodeException {
        BufrMapper mapper = getMapper();
        String bufrName = parser.getFieldName();
        Set<String> baseNames = mapper.lookupBaseNamesOrEmpty(bufrName,
                SYNOPTIC_LAND_NAMESPACE);
        if (baseNames.isEmpty()) {
            log.debug("Skipping unmapped field: " + bufrName);
        }
        int level = parser.getStructLevel();
        for (String baseName : baseNames) {
            if (level == 1) {
                /* process top level fields */
                if (DEFAULT_LOCATION_FIELDS.contains(baseName)) {
                    processLocationField(record.getLocation(), parser, baseName);
                } else {
                    processGeneralFields(record, parser, baseName);
                }
            } else if (level > 1) {
                /* process substructure fields */
                if (PRECIP_FIELD.equalsIgnoreCase(baseName)) {
                    processPrecip(record, parser, PRECIP_TIME_PERIOD_FIELD);
                } else if (SUB_STRUCT_FIELDS.contains(baseName)) {
                    processGeneralFields(record, parser, baseName);
                }
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
        /* WMO indexes have two parts, block and id */
        Number id = (Number) getFieldValue(parser, false);
        if (id == null) {
            log.debug("BUFR file " + parser.getFile()
                    + " missing station id field: " + parser.getFieldName());
            return null;
        }
        Number block = getWMOBlock(parser);
        if (block == null) {
            log.debug("BUFR file " + parser.getFile()
                    + " missing station id field: " + WMO_BLOCK_FIELD);
            return null;
        }

        return String.format(WMO_INDEX_FORMAT, block, id);
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
        finalizePresentWeather(parser, record);
        finalizeLocation(parser, record);
        return record;
    }

    /**
     * perform any finishing actions on the present weather field
     * 
     * @param parser
     * @param record
     */
    protected void finalizePresentWeather(BufrParser parser, ObsCommon record) {
        /* this code comes from the abstract synoptic text decoder */
        // Fixup the present weather string
        PointDataView pdv = record.getPointDataView();
        int pWx = pdv.getInt(SfcObsPointDataTransform.WX_PRESENT);
        if (pWx != SfcObsPointDataTransform.INT_DEFAULT) {
            if (pWx == 28 || (pWx > 41 && pWx < 48)) {
                float t = pdv.getFloat(SfcObsPointDataTransform.TEMPERATURE);
                if (t != SfcObsPointDataTransform.FLOAT_DEFAULT) {
                    String wx = (t > 273.15) ? "FG" : "FZFG";
                    pdv.setString(SfcObsPointDataTransform.PRES_WEATHER, wx);
                }
            }
        }
    }

    /**
     * perform any finishing actions on the location field
     * 
     * @param parser
     * @param record
     * @throws BufrObsDecodeException
     */
    protected void finalizeLocation(BufrParser parser, ObsCommon record)
            throws BufrObsDecodeException {
        SurfaceObsLocation location = record.getLocation();
        Point lonlat = location.getLocation();
        String stationId = location.getStationId();
        if (lonlat == null && stationId == null) {
            throw new MissingRequiredDataException(
                    "Record missing location information in BUFR file: "
                            + parser.getFile());
        } else if (lonlat == null) {
            log.debug("Getting station info from database for BUFR file: "
                    + parser.getFile());
            try {
                Integer type = ObStation.CAT_TYPE_SFC_FXD;
                if (record.getReportType() == IDecoderConstants.SYNOPTIC_MOBILE_LAND) {
                    type = ObStation.CAT_TYPE_SFC_MOB;
                }
                String gid = ObStation.createGID(type, stationId);
                ObStation station = stationDao.queryByGid(gid);
                if (station == null) {
                    throw new MissingRequiredDataException(
                            "Record missing location information in BUFR file: "
                                    + parser.getFile());
                }
                location.setElevation(station.getElevation());
                location.setLocation(station.getLocation());
            } catch (DataAccessLayerException e) {
                throw new BufrObsDecodeException(
                        "Problem querying the database for location", e);
            }
        } else if (stationId == null) {
            log.debug("Generating station id from location for BUFR file: "
                    + parser.getFile());
            location.generateCoordinateStationId();
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.plugin.bufrobs.AbstractBufrObsDecoder#getAliasMapFile
     * ()
     */
    @Override
    protected String getAliasMapFile() {
        return ALIAS_FILE_NAME;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.plugin.bufrobs.AbstractBufrObsDecoder#getCategoryFile
     * ()
     */
    @Override
    protected String getCategoryFile() {
        return CATEGORY_FILE_NAME;
    }

}
