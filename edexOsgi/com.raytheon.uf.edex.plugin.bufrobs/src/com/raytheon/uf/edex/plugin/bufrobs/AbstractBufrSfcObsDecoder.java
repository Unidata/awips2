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
package com.raytheon.uf.edex.plugin.bufrobs;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.NonSI;
import javax.measure.unit.SI;
import javax.measure.unit.Unit;
import javax.xml.bind.JAXBException;

import ucar.nc2.Attribute;
import ucar.nc2.NetcdfFile;
import ucar.nc2.Variable;

import com.raytheon.edex.plugin.sfcobs.SfcObsDao;
import com.raytheon.edex.plugin.sfcobs.SfcObsPointDataTransform;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.sfcobs.ObsCommon;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.nc.bufr.BufrDataItem;
import com.raytheon.uf.common.nc.bufr.BufrParser;
import com.raytheon.uf.common.nc.bufr.BufrParser.Event;
import com.raytheon.uf.common.nc.bufr.tables.MappedValue;
import com.raytheon.uf.common.nc.bufr.tables.ParsedTableUnit;
import com.raytheon.uf.common.nc.bufr.tables.TableEntry;
import com.raytheon.uf.common.nc.bufr.tables.TranslationTable;
import com.raytheon.uf.common.nc.bufr.tables.TranslationTable.TableType;
import com.raytheon.uf.common.nc.bufr.tables.TranslationTableManager;
import com.raytheon.uf.common.nc.bufr.time.BufrTimeFieldParser;
import com.raytheon.uf.common.nc.bufr.time.TimeFieldParseException;
import com.raytheon.uf.common.nc.bufr.util.BufrMapper;
import com.raytheon.uf.common.nc.bufr.util.TranslationTableGenerator;
import com.raytheon.uf.common.pointdata.ParameterDescription;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.pointdata.PointDataDescription;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.common.pointdata.spatial.SurfaceObsLocation;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.units.UnitLookupException;
import com.raytheon.uf.common.units.UnitMapper;
import com.raytheon.uf.common.wmo.WMOHeader;
import com.raytheon.uf.edex.decodertools.time.TimeTools;
import com.raytheon.uf.edex.plugin.bufrobs.category.CategoryKey;
import com.raytheon.uf.edex.plugin.bufrobs.category.CategoryParser;

/**
 * Abstract class for decoding BUFR formatted sfcobs. Contains general methods
 * that are oblivious to the specific fields that are in the BUFR structure.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 21, 2014 2906       bclement     Initial creation
 * Apr 21, 2014 2906       bclement     populated wmo header in record for consistency
 * Jun 12, 2014 3229       bclement     default implementation for getTranslationFile() and createStationId()
 *                                      moved processGeneralFields() and processPrecip() from synoptic land decoder
 * Jul 23, 2014 3410       bclement     location changed to floats
 * 
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public abstract class AbstractBufrSfcObsDecoder {

    public static final Set<String> DEFAULT_LOCATION_FIELDS = new HashSet<String>(
            Arrays.asList(SfcObsPointDataTransform.LATITUDE,
                    SfcObsPointDataTransform.LONGITUDE,
                    SfcObsPointDataTransform.STATION_ID,
                    SfcObsPointDataTransform.ELEVATION));

    public static final String localizationAliasDirectory = "bufrobs"
            + File.separator + "alias";

    public static final String UDUNITS_NAMESPACE = "udunits";

    public static final String WMO_HEADER_ATTRIB = "WMO Header";

    public static final String TIME_FIELD = "time";

    public static final String WMO_BLOCK_FIELD = "WMO block number";

    public static final String WMO_INDEX_FORMAT = "%02d%03d";

    protected static final IUFStatusHandler log = UFStatus
            .getHandler(AbstractBufrSfcObsDecoder.class);

    private volatile BufrMapper mapper;

    private final Object mapperMutex = new Object();

    private final SfcObsDao dao;

    private final String pluginName;

    private final PointDataDescription description;

    private final Map<String, ParameterDescription> parameterDescriptionMap;

    public static final String localizationTablesDirectory = "bufrobs"
            + File.separator + "tables";

    private final Map<String, TranslationTable> translationMap = new HashMap<String, TranslationTable>();

    private volatile Map<CategoryKey, Integer> reportTypeMap;

    private final Object reportTypeMutex = new Object();

    private final UnitMapper unitMapper = UnitMapper.getInstance();

    /**
     * @param pluginName
     * @throws BufrObsDecodeException
     */
    public AbstractBufrSfcObsDecoder(String pluginName)
            throws BufrObsDecodeException {
        try {
            this.dao = new SfcObsDao(pluginName);
            this.pluginName = pluginName;
            this.description = SfcObsPointDataTransform
                    .getDescription(pluginName);
            ParameterDescription[] params = description.parameters;
            this.parameterDescriptionMap = createParamDefMap(params);
        } catch (Exception e) {
            throw new BufrObsDecodeException("Unable to initialize decoder", e);
        }
    }

    /**
     * Create mapping from parameter name to description
     * 
     * @param params
     * @return
     */
    private static Map<String, ParameterDescription> createParamDefMap(
            ParameterDescription[] params) {
        Map<String, ParameterDescription> map = new HashMap<String, ParameterDescription>(
                params.length);
        for (ParameterDescription desc : params) {
            map.put(desc.getParameterName(), desc);
        }
        return Collections.unmodifiableMap(map);
    }

    /**
     * Parse structures from BUFR file and return results in obs common data
     * object
     * 
     * @param parser
     * @param key
     * @return
     * @throws BufrObsDecodeException
     */
    public PluginDataObject[] decode(BufrParser parser, CategoryKey key)
            throws BufrObsDecodeException {
        Map<File, PointDataContainer> pointMap = new HashMap<File, PointDataContainer>();
        Map<CategoryKey, Integer> rtypeMap = getReportTypeMap();
        Integer reportType = rtypeMap.get(key);
        WMOHeader header = getWmoHeader(parser);
        List<PluginDataObject> rval = new ArrayList<PluginDataObject>();
        boolean hasMore = true;
        while (hasMore) {
            try {
                try {
                    ObsCommon record = getNextRecord(parser, reportType,
                            header, pointMap);
                    if (record != null) {
                        rval.add(record);
                    } else {
                        hasMore = false;
                    }
                } catch (MissingRequiredDataException e) {
                    /* missing data required for record, no stack trace */
                    log.warn(e.getLocalizedMessage());
                    findEndOfStructure(parser);
                } catch (BufrObsDecodeException e) {
                    /* problem with individual structure, try to parse more */
                    log.error(e.getLocalizedMessage(), e);
                    findEndOfStructure(parser);
                }
            } catch (IOException e) {
                /* problem with the parser itself, give up on file */
                throw new BufrObsDecodeException("Problem parsing BUFR file: "
                        + parser.getFile(), e);
            }
        }
        return rval.toArray(new PluginDataObject[rval.size()]);
    }

    /**
     * Parse the next obs record or return null if there are no more records
     * 
     * @param parser
     * @param reportType
     * @param header
     * @param pointMap
     * @return null if there are no more records
     * @throws IOException
     * @throws BufrObsDecodeException
     */
    private ObsCommon getNextRecord(BufrParser parser, Integer reportType,
            WMOHeader header, Map<File, PointDataContainer> pointMap)
            throws IOException, BufrObsDecodeException {
        ObsCommon currentRecord = null;
        boolean done = false;
        while (parser.hasNext() && !done) {
            Event e = parser.next();
            switch (e) {
            case START_FILE:
                log.debug("Started processing BUFR file: " + parser.getFile());
                break;
            case START_STRUCTURE:
                if (parser.getStructLevel() == 1) {
                    /* started a new record */
                    currentRecord = createNewRecord(parser);
                    setTime(currentRecord, parser);
                    setWMOHeaderInfo(currentRecord, header);
                    currentRecord.setReportType(reportType);
                    PointDataContainer pdc = getPointDataContainer(pointMap,
                            currentRecord);
                    currentRecord.setPointDataView(pdc.append());
                }
                break;
            case FIELD:
                processField(currentRecord, parser);
                break;
            case END_STRUCTURE:
                if (parser.getStructLevel() == 0) {
                    /* ended record */
                    currentRecord = finalizeRecord(parser, currentRecord);
                    done = true;
                }
                break;
            case END_FILE:
                log.debug("Finished processing BUFR file");
                break;
            default:
                // no action
            }
        }
        return currentRecord;
    }

    /**
     * Run the parser until it is no longer in the current structure
     * 
     * @param parser
     * @throws IOException
     */
    private void findEndOfStructure(BufrParser parser) throws IOException {
        while (parser.hasNext() && parser.getStructLevel() > 0) {
            parser.next();
        }
    }

    /**
     * Set record fields that are sourced from WMO header
     * 
     * @param currentRecord
     * @param header
     */
    protected void setWMOHeaderInfo(ObsCommon currentRecord, WMOHeader header) {
        currentRecord.setWmoHeader(header.getWmoHeader());
        // Determine if this is a COR'd observation. Value must be in the
        // range of "CC[A..Z]"
        String cor = header.getBBBIndicator();
        // must have 3 characters.
        if ((cor != null) && (cor.length() == 2)) {
            if ("CC".equals(cor.substring(0, 2))) {
                char c = cor.charAt(2);
                if ((c >= 'A') && (c <= 'Z')) {
                    currentRecord.setCorIndicator(cor);
                }
            }
        }
        /* shef converter looks for WMO header in obs text */
        currentRecord.setObsText(header.getWmoHeader());
        currentRecord.setWmoHeader(header.getWmoHeader());
    }

    /**
     * Get WMO header object from NetCDF file
     * 
     * @param parser
     * @throws MissingRequiredDataException
     */
    protected WMOHeader getWmoHeader(BufrParser parser)
            throws MissingRequiredDataException {
        NetcdfFile ncfile = parser.getNcfile();
        Attribute attrib = ncfile
                .findGlobalAttributeIgnoreCase(WMO_HEADER_ATTRIB);
        if (attrib == null) {
            throw new MissingRequiredDataException(
                    "Missing WMO Header in BUFR file: " + parser.getFile());
        }
        return new WMOHeader(attrib.getStringValue().getBytes());
    }

    /**
     * Construct a new record instance
     * 
     * @param parser
     * 
     * @return
     */
    protected ObsCommon createNewRecord(BufrParser parser) {
        ObsCommon rval = new ObsCommon();
        rval.setLocation(new SurfaceObsLocation());
        return rval;
    }

    /**
     * Find point data container for record. Fields used in finding HDF file
     * (like time) need to be populated in record.
     * 
     * @param pointMap
     * @param record
     * @return
     */
    protected PointDataContainer getPointDataContainer(
            Map<File, PointDataContainer> pointMap, ObsCommon record) {
        File f = this.dao.getFullFilePath(record);
        PointDataContainer pdc = pointMap.get(f);
        if (pdc == null) {
            pdc = PointDataContainer.build(this.description);
            pointMap.put(f, pdc);
        }
        return pdc;
    }

    /**
     * Process BUFR field and place data in provided record
     * 
     * @param record
     * @param parser
     * @throws BufrObsDecodeException
     */
    abstract protected void processField(ObsCommon record, BufrParser parser)
            throws BufrObsDecodeException;

    /**
     * @param unitStr
     * @return true if unit specified a BUFR lookup table
     */
    protected boolean isTableLookup(String unitStr) {
        return ParsedTableUnit.TABLE_UNIT_PATTERN.matcher(unitStr).matches();
    }

    /**
     * Perform actions to finalize record after parsing is over
     * 
     * @param parser
     * 
     * @param record
     * @return
     * @throws BufrObsDecodeException
     */
    protected ObsCommon finalizeRecord(BufrParser parser, ObsCommon record)
            throws BufrObsDecodeException {
        return record;
    }

    /**
     * Lookup translation table for tableId. Results are cached.
     * 
     * @param tableId
     * @return
     * @throws BufrObsDecodeException
     */
    protected TranslationTable getTranslationTable(String tableId)
            throws BufrObsDecodeException {
        TranslationTable rval;
        synchronized (translationMap) {
            rval = translationMap.get(tableId);
            if (rval == null) {
                IPathManager pathMgr = PathManagerFactory.getPathManager();
                LocalizationContext edexStaticBase = pathMgr.getContext(
                        LocalizationContext.LocalizationType.EDEX_STATIC,
                        LocalizationContext.LocalizationLevel.BASE);
                String fileName = localizationTablesDirectory + File.separator
                        + getTranslationFile(tableId);
                LocalizationFile tableFile = pathMgr.getLocalizationFile(
                        edexStaticBase, fileName);
                if (tableFile == null) {
                    throw new BufrObsDecodeException(
                            "Unable to find localization file for BUFR translation table: "
                                    + tableId);
                }
                try {
                    TranslationTableManager instance = TranslationTableManager
                            .getInstance();
                    rval = (TranslationTable) instance
                            .unmarshalFromInputStream(tableFile
                                    .openInputStream());
                    translationMap.put(tableId, rval);
                } catch (Exception e) {
                    throw new BufrObsDecodeException(
                            "Problem creating BUFR translation table: "
                                    + tableId, e);
                }
            }
        }
        return rval;
    }

    /**
     * Get translation table file name
     * 
     * @param tableId
     * @return
     */
    protected String getTranslationFile(String tableId) {
        tableId = TranslationTableGenerator.replaceWhiteSpace(tableId, "_");
        return tableId + ".xml";
    }

    /**
     * Get obs-type specific parameter alias file name
     * 
     * @return
     */
    abstract protected String getAliasMapFile();

    /**
     * Get obs-type specific category file name
     * 
     * @return
     */
    abstract protected String getCategoryFile();

    /**
     * Get obs-type specific parameter alias mapper
     * 
     * @return
     * @throws BufrObsDecodeException
     */
    protected BufrMapper getMapper() throws BufrObsDecodeException {
        if (mapper == null) {
            synchronized (mapperMutex) {
                if (mapper == null) {
                    IPathManager pathMgr = PathManagerFactory.getPathManager();
                    File aliasFile = pathMgr
                            .getStaticFile(localizationAliasDirectory
                                    + File.separator + getAliasMapFile());
                    try {
                        mapper = new BufrMapper(Arrays.asList(aliasFile));
                    } catch (JAXBException e) {
                        throw new BufrObsDecodeException(
                                "Unable to create alias map", e);
                    }
                }
            }
        }
        return mapper;
    }

    /**
     * Locate time field in parser and populate time fields in record
     * 
     * @param record
     * @param parser
     * @throws BufrObsDecodeException
     */
    private void setTime(ObsCommon record, BufrParser parser)
            throws BufrObsDecodeException {
        BufrDataItem timeData = parser.scanForStructField(TIME_FIELD, false);
        if (timeData == null) {
            throw new MissingRequiredDataException(
                    "Missing time field value in BUFR file: "
                    + parser.getFile());
        }
        try {
            Variable var = timeData.getVariable();
            Calendar cal = BufrTimeFieldParser.processTimeField(
                    timeData.getValue(), var.getUnitsString());
            record.setDataTime(new DataTime(cal));
            record.setTimeObs(cal);
            record.setRefHour(TimeTools.copyToNearestHour(cal));
        } catch (TimeFieldParseException e) {
            throw new BufrObsDecodeException("Problem parsing time field: "
                    + e.getLocalizedMessage() + " in " + parser.getFile(), e);
        }
    }

    /**
     * Handles location-specific fields for obs
     * 
     * @param location
     * @param parser
     * @param baseName
     * @throws BufrObsDecodeException
     */
    protected void processLocationField(SurfaceObsLocation location,
            BufrParser parser, String baseName) throws BufrObsDecodeException {
        if (baseName.equalsIgnoreCase(SfcObsPointDataTransform.STATION_ID)) {
            location.setStationId(createStationId(parser));
        } else if (baseName.equalsIgnoreCase(SfcObsPointDataTransform.LATITUDE)) {
            Double lat = (Double) getFieldValue(parser, false);
            if (lat != null) {
                location.assignLatitude(lat.floatValue());
            }
        } else if (baseName
                .equalsIgnoreCase(SfcObsPointDataTransform.LONGITUDE)) {
            Double lon = (Double) getFieldValue(parser, false);
            if (lon != null) {
                location.assignLongitude(lon.floatValue());
            }
        } else if (baseName
                .equalsIgnoreCase(SfcObsPointDataTransform.ELEVATION)) {
            Number elevation = getInUnits(parser, SI.METER);
            if (elevation != null) {
                location.setElevation(elevation.intValue());
            }
        } else {
            log.warn("Unknown location base name: " + baseName);
        }
    }

    /**
     * Find and format stationId
     * 
     * @param parser
     * @return null if no station id found
     * @throws BufrObsDecodeException
     */
    protected String createStationId(BufrParser parser)
            throws BufrObsDecodeException {
        /* most subclasses will override this */
        Object obj = getFieldValue(parser, true);
        if (obj != null) {
            return obj.toString();
        } else {
            return null;
        }
    }

    /**
     * Find WMO block number in parser
     * 
     * @param parser
     * @return null if not found
     */
    protected Number getWMOBlock(BufrParser parser) {
        BufrDataItem data = parser.scanForStructField(WMO_BLOCK_FIELD, false);
        if (data == null) {
            return null;
        }
        return (Number) data.getValue();
    }

    /**
     * {@link BufrParser#getFieldScalarValue(boolean)}
     * 
     * @param parser
     * @param charArrayAsString
     * @return
     * @throws BufrObsDecodeException
     */
    protected Object getFieldValue(BufrParser parser, boolean charArrayAsString)
            throws BufrObsDecodeException {
        try {
            return parser.getFieldScalarValue(charArrayAsString);
        } catch (IOException e) {
            throw new BufrObsDecodeException(
                    "Problem getting value for field: " + parser.getFieldName(),
                    e);
        }
    }

    /**
     * Get field and convert to specified units
     * 
     * @param parser
     * @param toUnit
     * @return
     * @throws BufrObsDecodeException
     */
    protected Number getInUnits(BufrParser parser, Unit<?> toUnit)
            throws BufrObsDecodeException {
        Object obj = getFieldValue(parser, false);
        if (obj == null) {
            /* missing value */
            return null;
        }
        return getInUnits(parser.getFieldVariable(), obj, toUnit);
    }

    /**
     * Convert provided value to specified units
     * 
     * @param var
     * @param obj
     * @param toUnit
     * @return
     * @throws BufrObsDecodeException
     */
    protected Number getInUnits(Variable var, Object obj, Unit<?> toUnit)
            throws BufrObsDecodeException {
        Number rval;
        if (obj instanceof Number) {
            Number value = (Number) obj;
            Unit<?> fromUnit = getSourceUnit(var, toUnit);
            UnitConverter converter = fromUnit.getConverterTo(toUnit);
            rval = converter.convert(value.doubleValue());
        } else {
            log.error("Attempted to convert non-numeric value '" + obj
                    + "' for field: " + var.getFullName());
            rval = null;
        }
        return rval;
    }

    /**
     * Get units used in BUFR file
     * 
     * @param var
     * @param toUnit
     * @return
     * @throws BufrObsDecodeException
     */
    protected Unit<?> getSourceUnit(Variable var, Unit<?> toUnit)
            throws BufrObsDecodeException {
        String fieldUnits = var.getUnitsString();
        Set<Unit<?>> results;
        try {
            results = unitMapper.lookupCompatibleUnits(fieldUnits,
                    UDUNITS_NAMESPACE, toUnit);
            if (results.isEmpty()) {
                throw new UnitLookupException(
                        "No compatible unit mapping found for " + fieldUnits
                                + " to " + toUnit);
            }
        } catch (UnitLookupException e) {
            throw new BufrObsDecodeException(
                    "Unable to convert units for field " + var.getFullName()
                            + ": " + fieldUnits, e);
        }
        return results.iterator().next();
    }

    /**
     * Get mapping of category keys to report type integers. Caches result.
     * 
     * @return
     * @throws BufrObsDecodeException
     */
    public Map<CategoryKey, Integer> getReportTypeMap()
            throws BufrObsDecodeException {
        if (reportTypeMap == null) {
            synchronized (reportTypeMutex) {
                if (reportTypeMap == null) {
                    reportTypeMap = getReportMapInternal();
                }
            }
        }
        return reportTypeMap;
    }

    /**
     * Create mapping of category keys to report type integers
     * 
     * @return
     * @throws BufrObsDecodeException
     */
    private Map<CategoryKey, Integer> getReportMapInternal()
            throws BufrObsDecodeException {
        try {
            return CategoryParser.getReportTypeMap(getCategoryFile());
        } catch (Exception e) {
            throw new BufrObsDecodeException(e.getLocalizedMessage(), e);
        }
    }

    /**
     * Process fields that are indexes into BUFR code tables.
     * 
     * @param record
     * @param parser
     * @param baseName
     * @throws BufrObsDecodeException
     */
    protected void processLookupTableField(ObsCommon record, BufrParser parser,
            String baseName) throws BufrObsDecodeException {
        Map<String, ParameterDescription> paramMap = getParameterDescriptionMap();
        ParameterDescription desc = paramMap.get(baseName);
        if (desc == null) {
            log.error("Found field without parameter description: " + baseName);
            return;
        }
        Object value = getFieldValue(parser, false);
        String entryValue = null;
        if (value != null) {
            if (!(value instanceof Number)) {
                log.error("Found non-numeric table value. Field: "
                        + parser.getFieldName() + ", Table: "
                        + parser.getFieldUnits() + ", value: " + value);
                return;
            }
            entryValue = lookupTableEntry(parser, baseName,
                    ((Number) value).intValue());
        }
        PointDataView pdv = record.getPointDataView();
        switch (desc.getType()) {
        case INT:
            int ival;
            if (entryValue != null) {
                ival = Integer.valueOf(entryValue);
            } else {
                ival = SfcObsPointDataTransform.INT_DEFAULT;
            }
            pdv.setInt(baseName, ival);
            break;
        case STRING:
            pdv.setString(baseName, entryValue);
            break;
        default:
            log.error("Unsupported type for field: " + baseName);
        }
    }

    /**
     * Find parameter value for index key in translation table for field
     * 
     * @param parser
     * @param baseName
     * @param key
     * @return null if not found
     * @throws BufrObsDecodeException
     */
    protected String lookupTableEntry(BufrParser parser, String baseName,
            int key) throws BufrObsDecodeException {
        ParsedTableUnit tableUnit = ParsedTableUnit.parse(parser
                .getFieldUnits());
        TranslationTable transTable = getTranslationTable(tableUnit
                .getTableId());
        TableType type = tableUnit.getType();
        if (!TableType.CODE.equals(type)) {
            log.warn("Unsupported table type for field " + baseName + ": "
                    + type + ". Only first matched value will be used");
        }
        List<TableEntry> entries = transTable.lookupEntries(key, type);
        MappedValue match = null;
        /* only flag tables return more than one entry */
        for (TableEntry entry : entries) {
            /* there are usually less than 3 mapped values in an entry */
            for (MappedValue value : entry.getMappedValues()) {
                if (value.getParameter().equalsIgnoreCase(baseName)) {
                    match = value;
                    break;
                }
            }
        }
        String rval = null;
        if (match != null) {
            rval = match.getValue();
        } else {
            log.warn("Translation table " + transTable.getBufrTable()
                    + " missing mapped value for field: " + baseName);
        }
        return rval;
    }

    /**
     * Process fields that have quantitative values with associated units
     * 
     * @param record
     * @param parser
     * @param baseName
     * @throws BufrObsDecodeException
     */
    protected void processDirectField(ObsCommon record, BufrParser parser,
            String baseName) throws BufrObsDecodeException {
        Map<String, ParameterDescription> paramMap = getParameterDescriptionMap();
        ParameterDescription desc = paramMap.get(baseName);
        if (desc == null) {
            log.error("Found field without parameter description: " + baseName);
            return;
        }
        Object value = getParameterValue(desc, parser);
        PointDataView pdv = record.getPointDataView();
        switch (desc.getType()) {
        case FLOAT:
            float fval;
            if (value != null && value instanceof Number) {
                fval = ((Number) value).floatValue();
            } else {
                fval = SfcObsPointDataTransform.FLOAT_DEFAULT;
            }
            pdv.setFloat(baseName, fval);
            break;
        case INT:
            int ival;
            if (value != null && value instanceof Number) {
                ival = ((Number) value).intValue();
            } else {
                ival = SfcObsPointDataTransform.INT_DEFAULT;
            }
            pdv.setInt(baseName, ival);
            break;
        case STRING:
            pdv.setString(baseName, String.valueOf(value));
            break;
        default:
            log.error("Unsupported type for field: " + baseName);
        }
    }

    /**
     * Get field value given point data view parameter description details (ie
     * units)
     * 
     * @param desc
     * @param parser
     * @return
     * @throws BufrObsDecodeException
     */
    protected Object getParameterValue(ParameterDescription desc,
            BufrParser parser) throws BufrObsDecodeException {
        Unit<?> toUnit = null;
        if (desc.getUnit() != null) {
            toUnit = Unit.valueOf(desc.getUnit());
        }
        Object value;
        if (toUnit != null) {
            value = getInUnits(parser, toUnit);
        } else {
            value = getFieldValue(parser, false);
        }
        return value;
    }

    /**
     * Determines if field is table lookup or direct and handles accordingly
     * 
     * @param record
     * @param parser
     * @param baseName
     * @throws BufrObsDecodeException
     */
    protected void processGeneralFields(ObsCommon record, BufrParser parser,
            String baseName) throws BufrObsDecodeException {
        String unitStr = parser.getFieldUnits();
        if (unitStr != null && isTableLookup(unitStr)) {
            processLookupTableField(record, parser, baseName);
        } else {
            processDirectField(record, parser, baseName);
        }
    }

    /**
     * Precip doesn't have individual fields for different hour periods. One
     * structure contains the general precip field with a time period. This
     * method parses the time period to determine what field the precip data is
     * for.
     * 
     * @param record
     * @param parser
     * @throws BufrObsDecodeException
     */
    protected void processPrecip(ObsCommon record, BufrParser parser,
            String precipTimePeriodField) throws BufrObsDecodeException {
        Number precip = getInUnits(parser, SI.MILLIMETER);
        if (precip == null) {
            /* missing value */
            return;
        }
        BufrDataItem timeData = parser.scanForStructField(
                precipTimePeriodField, false);
        if (timeData == null || timeData.getValue() == null) {
            log.error("Found precipitation field missing time data. Field: "
                    + parser.getFieldName() + ", Table: "
                    + parser.getFieldUnits());
            return;
        }
        Number period = getInUnits(timeData.getVariable(), timeData.getValue(),
                NonSI.HOUR);
        PointDataView pdv = record.getPointDataView();
        switch (Math.abs(period.intValue())) {
        case 1:
            pdv.setFloat(SfcObsPointDataTransform.PRECIP1_HOUR,
                    precip.intValue());
            break;
        case 3:
            /* text synop obs doesn't have 3HR precip */
            log.debug("Received precip period not supported by point data view: "
                    + Math.abs(period.intValue()) + " HOUR");
            break;
        case 6:
            pdv.setFloat(SfcObsPointDataTransform.PRECIP6_HOUR,
                    precip.intValue());
            break;
        case 12:
            pdv.setFloat(SfcObsPointDataTransform.PRECIP12_HOUR,
                    precip.intValue());
            break;
        case 18:
            pdv.setFloat(SfcObsPointDataTransform.PRECIP18_HOUR,
                    precip.intValue());
            break;
        case 24:
            pdv.setFloat(SfcObsPointDataTransform.PRECIP24_HOUR,
                    precip.intValue());
            break;
        default:
            log.error("Unknown precipitation period '" + period.intValue()
                    + "' in BUFR file " + parser.getFile());
        }
    }

    /**
     * @return set of category keys that this parser handles
     * @throws BufrObsDecodeException
     */
    public Set<CategoryKey> getCategoryKeys() throws BufrObsDecodeException {
        return getReportTypeMap().keySet();
    }

    /**
     * @return the dao
     */
    public SfcObsDao getDao() {
        return dao;
    }

    /**
     * @return the pluginName
     */
    public String getPluginName() {
        return pluginName;
    }

    /**
     * @return the description
     */
    public PointDataDescription getDescription() {
        return description;
    }

    /**
     * @return the parameterDescriptionMap
     */
    public Map<String, ParameterDescription> getParameterDescriptionMap() {
        return parameterDescriptionMap;
    }

}
