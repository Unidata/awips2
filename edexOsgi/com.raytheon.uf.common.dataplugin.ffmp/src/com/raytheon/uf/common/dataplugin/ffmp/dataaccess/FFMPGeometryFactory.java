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
package com.raytheon.uf.common.dataplugin.ffmp.dataaccess;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.measure.unit.Unit;

import com.raytheon.uf.common.dataaccess.IDataRequest;
import com.raytheon.uf.common.dataaccess.exception.DataRetrievalException;
import com.raytheon.uf.common.dataaccess.exception.IncompatibleRequestException;
import com.raytheon.uf.common.dataaccess.exception.InvalidIdentifiersException;
import com.raytheon.uf.common.dataaccess.exception.TimeAgnosticDataException;
import com.raytheon.uf.common.dataaccess.geom.IGeometryData;
import com.raytheon.uf.common.dataaccess.impl.AbstractDataPluginFactory;
import com.raytheon.uf.common.dataaccess.impl.DefaultGeometryData;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPBasin;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPBasinData;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPGuidanceBasin;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPGuidanceInterpolation;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPRecord;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPTemplates;
import com.raytheon.uf.common.dataplugin.ffmp.HucLevelGeometriesFactory;
import com.raytheon.uf.common.dataplugin.ffmp.collections.FFMPDataCache;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.responses.DbQueryResponse;
import com.raytheon.uf.common.monitor.config.FFFGDataMgr;
import com.raytheon.uf.common.monitor.config.FFMPRunConfigurationManager;
import com.raytheon.uf.common.monitor.config.FFMPSourceConfigurationManager;
import com.raytheon.uf.common.monitor.config.FFMPSourceConfigurationManager.SOURCE_TYPE;
import com.raytheon.uf.common.monitor.config.FFMPTemplateConfigurationManager;
import com.raytheon.uf.common.monitor.xml.DomainXML;
import com.raytheon.uf.common.monitor.xml.FFMPRunXML;
import com.raytheon.uf.common.monitor.xml.ProductRunXML;
import com.raytheon.uf.common.monitor.xml.ProductXML;
import com.raytheon.uf.common.monitor.xml.SourceXML;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.BinOffset;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.vividsolutions.jts.geom.Geometry;

/**
 * A data factory for retrieving FFMP data.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Jan 24, 2013  1552     mpduff    Initial creation
 * Apr 16, 2013  1912     bsteffen  Initial bulk hdf5 access for ffmp
 * Jul 15, 2013  2184     dhladky   Remove all HUC's for storage except ALL
 * Aug 20, 2013  2250     mnash     Change some methods that were not working in
 *                                  all cases
 * Jan 14, 2014  2667     mnash     Remove getGridData method
 * May 01, 2014  3099     bkowal    No longer use an empty pfaf list when the
 *                                  data request locationNames list is empty.
 * Jun 24, 2014  3170     mnash     Get the accumulated time if multiple times
 *                                  are requested
 * Jul 14, 2014  3184     njensen   Overrode getAvailableLevels()
 * Jul 30, 2014  3184     njensen   Overrode required and optional identifiers
 * Feb 27, 2015  4180     mapeters  Overrode getAvailableParameters().
 * Jun 15, 2015  4560     ccody     Added support for configurable
 *                                  rate/accumulation calculation for
 *                                  getGeometryData
 * Jul 16, 2015  4658     dhladky   Expiration times fixed.
 * Oct 26, 2015  5056     dhladky   Re-wrote to take advantage of FFMP common
 *                                  data cache, fix general bugs.
 * Jun 07, 2016  5587     tgurney   Change get*Identifiers() to take
 *                                  IDataRequest
 * Jun 09, 2016  5587     tgurney   Return early from getGeometryData if no
 *                                  records returned from database
 * Jun 20, 2016  5587     tgurney   Implement getIdentifierValues()
 * Jul 01, 2016  5728     mapeters  Support RequestConstraint identifier values
 *                                  for siteKey and dataKey, only allow 1 param
 *                                  to be set since only 1 is retrieved, support
 *                                  getIdentifierValues() for huc, accumHrs
 * Aug 01, 2016  2416     tgurney   Add dataURI as optional identifier
 * Aug 08, 2016  5728     mapeters  Better error handling for invalid sourceName
 * 
 * </pre>
 * 
 * @author mpduff
 */

public class FFMPGeometryFactory extends AbstractDataPluginFactory {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(FFMPGeometryFactory.class);

    /** Site key constant */
    public static final String SITE_KEY = "siteKey";

    /** Data key constant */
    public static final String DATA_KEY = "dataKey";

    /** wfo constant */
    public static final String WFO = "wfo";

    /** plugin constant */
    public static final String PLUGIN_NAME = "ffmp";

    /** huc constant */
    public static final String HUC = "huc";

    /** source name constant */
    public static final String SOURCE_NAME = "sourceName";

    /** accumulation hours, needed for FFG interpolator */
    public static final String ACCUM_HRS = "accumHrs";

    /**
     * Constructor.
     */
    public FFMPGeometryFactory() {
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected IGeometryData[] getGeometryData(IDataRequest request,
            DbQueryResponse dbQueryResponse) {
        List<Map<String, Object>> results = dbQueryResponse.getResults();

        if (results.isEmpty()) {
            return new IGeometryData[0];
        }

        String cwa = extractIdentifierValue(request, WFO, String.class);
        String sourceName = null;
        Map<String, Set<String>> siteKeysToDataKeys = new HashMap<>();
        Map<String, Date> siteKeysToStartDates = new HashMap<>();
        Map<String, Date> siteKeysToEndDates = new HashMap<>();

        FFMPDataCache cache = getCache(cwa);

        // Ensures that all records have been added to the cache
        // before calculations
        for (Map<String, Object> map : results) {
            for (Map.Entry<String, Object> es : map.entrySet()) {
                FFMPRecord rec = (FFMPRecord) es.getValue();
                String siteKey = rec.getSiteKey();
                if (sourceName == null) {
                    sourceName = rec.getSourceName();
                }

                Set<String> dataKeys = siteKeysToDataKeys.get(siteKey);
                if (dataKeys == null) {
                    dataKeys = new HashSet<>();
                    siteKeysToDataKeys.put(siteKey, dataKeys);
                }
                dataKeys.add(rec.getDataKey());

                try {
                    cache.populateFFMPRecord(siteKey, rec, rec.getSourceName());
                } catch (Exception e) {
                    statusHandler.handle(
                            Priority.PROBLEM,
                            "Unable to populate FFMPRecord: "
                                    + rec.getDataURI(), e);
                }

                /*
                 * building a time range of the earliest FFMP time (based on
                 * each record) to the latest FFMP time (based on each record)
                 */
                Date recDate = rec.getDataTime().getRefTime();
                Date start = siteKeysToStartDates.get(siteKey);
                if (start == null || start.after(recDate)) {
                    siteKeysToStartDates.put(siteKey, recDate);
                }
                Date end = siteKeysToEndDates.get(siteKey);
                if (end == null || end.before(recDate)) {
                    siteKeysToEndDates.put(siteKey, recDate);
                }
            }
        }

        List<IGeometryData> geomRecords = new ArrayList<>();

        // Calculate separate set of records for each site
        for (String siteKey : siteKeysToDataKeys.keySet()) {
            // Time window for FFMP slides +- the 1/2 QPE expiration time.
            SourceXML source = FFMPSourceConfigurationManager.getInstance()
                    .getSource(sourceName);
            if (source == null) {
                String msg = "Invalid source name: "
                        + sourceName
                        + " (not configured in "
                        + FFMPSourceConfigurationManager.getInstance()
                                .getConfigFileName() + ")";
                throw new IncompatibleRequestException(msg);
            }
            int expirationTime = source.getExpirationMinutes(siteKey);
            Date start = siteKeysToStartDates.get(siteKey);
            start = new Date(start.getTime() - TimeUtil.MILLIS_PER_MINUTE
                    * (expirationTime / 2));
            Date end = siteKeysToEndDates.get(siteKey);

            /*
             * now that we have all the basin data in the cache, we can use the
             * methods on the FFMPDataCache class to get the accumulated value
             * in the case of a non-guidance basin
             */
            Map<Long, DefaultGeometryData> result = null;
            try {
                result = makeGeometryData(siteKey,
                        siteKeysToDataKeys.get(siteKey), sourceName, request,
                        start, end);
            } catch (Exception e) {
                String msg = "Unable to create Geometry Data: " + SOURCE_NAME
                        + "=" + sourceName + ", " + SITE_KEY + "=" + siteKey;
                throw new DataRetrievalException(msg, e);
            }

            geomRecords.addAll(result.values());
        }

        return geomRecords.toArray(new IGeometryData[0]);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected Map<String, RequestConstraint> buildConstraintsFromRequest(
            IDataRequest request) {
        Map<String, RequestConstraint> map = new HashMap<>();
        for (Map.Entry<String, Object> entry : request.getIdentifiers()
                .entrySet()) {
            String key = entry.getKey();
            // exclude these parameters
            if (!key.equals(ACCUM_HRS) && !key.equals(HUC)) {
                Object value = entry.getValue();
                RequestConstraint constraint;
                if (value instanceof RequestConstraint) {
                    constraint = (RequestConstraint) value;
                } else {
                    constraint = new RequestConstraint(value.toString());
                }
                map.put(key, constraint);
            }
        }

        /*
         * Most requests will have been validated to have exactly 1 param (for
         * requesting grid data or times), but other requests (e.g. for
         * available parameters) don't validate the number of params set, so
         * support any number
         */
        String[] params = request.getParameters();
        if (params.length > 0) {
            RequestConstraint paramConstraint = new RequestConstraint(params);
            map.put(SOURCE_NAME, paramConstraint);
        }

        return map;
    }

    /**
     * Get an identifier value that must be provided as an instance of the given
     * valueType (or may not be provided at all)
     * 
     * @param request
     * @param key
     * @param valueType
     * @return the identifier value (may be null)
     */
    private <T> T extractIdentifierValue(IDataRequest request, String key,
            Class<T> valueType) {
        Object value = request.getIdentifiers().get(key);
        if (value == null) {
            return null;
        } else if (valueType.isInstance(value)) {
            return valueType.cast(value);
        } else {
            throw new IncompatibleRequestException("Only "
                    + valueType.getSimpleName()
                    + " identifier values are valid for '" + key + "'");
        }
    }

    /**
     * Get the list of site keys that satisfy the identifier constraints on the
     * given request
     * 
     * @param request
     * @param key
     * @return the list of site keys
     */
    private String[] getSiteKeyValues(IDataRequest request) {
        Object idConstraint = request.getIdentifiers().get(SITE_KEY);
        if (idConstraint instanceof String) {
            // If directly provided as a string, just return it
            return new String[] { (String) idConstraint };
        } else if (idConstraint instanceof RequestConstraint) {
            // Automatically excludes values that don't satisfy identifiers
            return getAvailableValues(request, SITE_KEY, String.class);
        } else {
            throw new IncompatibleRequestException(
                    "Only string and RequestConstraint identifier values are valid for '"
                            + SITE_KEY + "'");
        }
    }

    /**
     * Create the IGeometryData objects.
     * 
     * @param sourceName
     * @param request
     * @param start
     * @param end
     * @return
     * @throws Exception
     */
    private Map<Long, DefaultGeometryData> makeGeometryData(String siteKey,
            Set<String> dataKeys, String sourceName, IDataRequest request,
            Date start, Date end) throws Exception {

        Map<Long, DefaultGeometryData> result = new HashMap<>();

        String cwa = extractIdentifierValue(request, WFO, String.class);
        String huc = extractIdentifierValue(request, HUC, String.class);

        FFMPDataCache cache = getCache(cwa);

        FFMPGuidanceInterpolation interpolation = null;
        FFMPBasinData basinData = null;
        SourceXML source = FFMPSourceConfigurationManager.getInstance()
                .getSource(sourceName);
        if (isGuidance(source)) {
            Number accumulationTime = extractIdentifierValue(request,
                    ACCUM_HRS, Number.class);
            basinData = cache.getSourceData(siteKey, source.getDisplayName())
                    .getRecord().getBasinData();
            interpolation = getGuidanceInterpolation(accumulationTime,
                    sourceName, cwa, siteKey);
        } else {
            basinData = cache.getSourceData(siteKey, sourceName).getRecord()
                    .getBasinData();
        }

        Map<Long, FFMPBasin> basinDataMap = basinData.getBasins();

        String rateOrAccum = null;
        if (source.getSourceType().equals(SOURCE_TYPE.QPE.getSourceType())) {
            rateOrAccum = source.getRateOrAccum(siteKey);
        }

        boolean isRate = true;
        /**
         * This is a misnomer that has caused loads of confusion. In actuality
         * when the FFMPBasin accumulates it checks that the Type is NOT a rate
         * and in fact should be accumulating. RATE == false. So in effect,
         * values that FFMP stores as RATE==true are actually NOT rates when
         * accumulated. So if, it is stored as RATE, it must use FALSE when
         * processing the accumulation.
         */
        if (rateOrAccum != null && !rateOrAccum.isEmpty()
                && rateOrAccum.compareToIgnoreCase("RATE") == 0) {
            isRate = false;
        }
        DefaultGeometryData data = null;

        HucLevelGeometriesFactory geomFactory = HucLevelGeometriesFactory
                .getInstance();

        Map<Long, Geometry> geomMap = geomFactory.getGeometries(
                cache.getTemplates(siteKey), siteKey, cwa, huc);

        List<Long> pfafList = getAvailableLocationPfafs(request);

        for (Long pfaf : geomMap.keySet()) {
            if (pfafList == null || pfafList.contains(pfaf)) {
                if (result.containsKey(pfaf)) {
                    data = result.get(pfaf);
                } else {
                    data = new DefaultGeometryData();
                    Map<String, Object> attrs = new HashMap<>();
                    attrs.put(DATA_KEY, buildDataKeysString(dataKeys));
                    attrs.put(SITE_KEY, siteKey);
                    attrs.put(WFO, cwa);
                    attrs.put(HUC, huc);
                    data.setAttributes(attrs);
                    data.setLocationName(String.valueOf(pfaf));
                    data.setGeometry(geomMap.get(pfaf));
                    data.setDataTime(new DataTime(start.getTime(),
                            new TimeRange(start, end)));
                    result.put(pfaf, data);
                }

                FFMPBasin basin = basinDataMap.get(pfaf);
                Float value = null;

                if (basin == null) {
                    continue;
                }

                /**
                 * Guidance Basins will use interpolation, need this to
                 * perfectly match FFMP table.
                 */
                if (basin instanceof FFMPGuidanceBasin) {

                    if (interpolation.isInterpolate()) {
                        // Interpolating between sources
                        value = ((FFMPGuidanceBasin) basin)
                                .getInterpolatedValue(interpolation,
                                        source.getExpirationMinutes(siteKey)
                                                * TimeUtil.MILLIS_PER_MINUTE);
                    } else {
                        value = ((FFMPGuidanceBasin) basin).getValue(
                                interpolation.getStandardSource(),
                                interpolation,
                                source.getExpirationMinutes(siteKey)
                                        * TimeUtil.MILLIS_PER_MINUTE);
                    }

                    // Will allow for any forcing to take precedence
                    FFFGDataMgr dman = FFFGDataMgr.getInstance();
                    if (!dman.isExpired()) {
                        value = dman.adjustValue(value, sourceName,
                                basin.getPfaf(),
                                ((FFMPGuidanceBasin) basin).getCountyFips());
                    }

                } else {
                    value = basin.getAccumValue(start, end,
                            source.getExpirationMinutes(siteKey)
                                    * TimeUtil.MILLIS_PER_MINUTE, isRate);
                }
                String parameter = sourceName;
                String unitStr = source.getUnit();

                Unit<?> unit = null;
                if (unitStr.equals(SourceXML.UNIT_TXT)) {
                    unit = Unit.valueOf("in");
                }

                if (unit != null) {
                    data.addData(parameter, value, unit);
                } else {
                    data.addData(parameter, value);
                }
            }
        }

        return result;
    }

    @Override
    public DataTime[] getAvailableTimes(IDataRequest request,
            boolean refTimeOnly) throws TimeAgnosticDataException {
        validateRequest(request);
        return super.getAvailableTimes(request, refTimeOnly);
    }

    @Override
    public DataTime[] getAvailableTimes(IDataRequest request,
            BinOffset binOffset) throws TimeAgnosticDataException {
        validateRequest(request);
        return super.getAvailableTimes(request, binOffset);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String[] getAvailableLocationNames(IDataRequest request) {
        // Requires most of required ids to get location pfafs
        validateRequest(request, false);
        // Changed this to query the active Templates, not the DB.
        List<Long> pfafs = getAvailableLocationPfafs(request);

        List<String> pfafList = new ArrayList<>(pfafs.size());
        for (Long pfaf : pfafs) {
            pfafList.add(String.valueOf(pfaf));
        }

        return pfafList.toArray(new String[0]);
    }

    /**
     * Gets the available location Pfafs. Note that the request must have been
     * validated to contain all required identifiers before calling this method.
     * 
     * @param request
     * @return
     */
    private List<Long> getAvailableLocationPfafs(IDataRequest request) {
        String wfo = extractIdentifierValue(request, WFO, String.class);
        String huc = extractIdentifierValue(request, HUC, String.class);
        String[] siteKeys = getSiteKeyValues(request);

        Set<Long> pfafs = new HashSet<>();

        FFMPDataCache cache = getCache(wfo);
        for (String siteKey : siteKeys) {
            FFMPTemplates templates = cache.getTemplates(siteKey);
            List<DomainXML> domains = templates.getDomains();
            pfafs.addAll(templates.getHucKeyList(siteKey, huc, domains));
        }

        return new ArrayList<>(pfafs);
    }

    @Override
    protected void validateParameters(IDataRequest request)
            throws IncompatibleRequestException {
        // Exactly 1 parameter must be set
        String[] params = request.getParameters();
        if (params == null || params.length != 1) {
            throw new IncompatibleRequestException("Requests of "
                    + request.getDatatype()
                    + " data must have exactly one parameter specified");
        }
    }

    @Override
    public String[] getAvailableParameters(IDataRequest request) {
        return this.getAvailableValues(request, SOURCE_NAME, String.class);
    }

    @Override
    public Level[] getAvailableLevels(IDataRequest request) {
        throw new IncompatibleRequestException(request.getDatatype()
                + " data does not support the concept of levels");
    }

    @Override
    public String[] getRequiredIdentifiers(IDataRequest request) {
        return new String[] { SITE_KEY, WFO, HUC };
    }

    @Override
    public String[] getOptionalIdentifiers(IDataRequest request) {
        return new String[] { DATA_KEY, ACCUM_HRS, PluginDataObject.DATAURI_ID };
    }

    @Override
    public String[] getIdentifierValues(IDataRequest request,
            String identifierKey) {
        if (!Arrays.asList(getRequiredIdentifiers(request)).contains(
                identifierKey)
                && !Arrays.asList(getOptionalIdentifiers(request)).contains(
                        identifierKey)) {
            throw new InvalidIdentifiersException(request.getDatatype(), null,
                    Arrays.asList(identifierKey));
        }
        if (identifierKey.equals(HUC)) {
            try {
                FFMPTemplateConfigurationManager templateConfig = FFMPTemplateConfigurationManager
                        .getInstance();
                templateConfig.readConfigXml();
                return templateConfig.getHucLevelsInArray();
            } catch (Exception e) {
                throw new DataRetrievalException(
                        "Failed to read available HUC levels from configuration file",
                        e);
            }
        } else if (identifierKey.equals(ACCUM_HRS)) {
            return getAccumHrsIdentifierValues(request);
        } else {
            return getAvailableValues(request, identifierKey, String.class);
        }
    }

    /**
     * Get the FFG Object need to interpolate between sources
     * 
     * @param accumulationTime
     *            if null, default value for given sourceName is retrieved from
     *            the interpolator
     * @param sourceName
     * @param wfo
     * @param siteKey
     * @return
     */
    private FFMPGuidanceInterpolation getGuidanceInterpolation(
            Number accumulationTime, String sourceName, String wfo,
            String siteKey) {

        FFMPSourceConfigurationManager sourceConfig = FFMPSourceConfigurationManager
                .getInstance();
        SourceXML source = sourceConfig.getSource(sourceName);
        String primarySourceName = sourceConfig.getPrimarySource(source);
        ProductXML product = sourceConfig.getProduct(primarySourceName);
        ProductRunXML productRun = FFMPRunConfigurationManager.getInstance()
                .getRunner(wfo).getProduct(siteKey);

        FFMPGuidanceInterpolation interpolator = new FFMPGuidanceInterpolation(
                sourceConfig, product, productRun, primarySourceName,
                source.getDisplayName(), siteKey);

        double time;
        if (accumulationTime != null) {
            // Use user-specified value if given
            time = accumulationTime.doubleValue();
        } else {
            /*
             * Otherwise get default accum hours from interpolator (e.g.
             * ARI6H2YR defaults to 6, ARI12H2YR to 12, etc.)
             */
            time = interpolator.getHour(sourceName);
        }
        interpolator.setInterpolationSources(time);

        return interpolator;
    }

    /**
     * Get the available accumHrs identifier values
     * 
     * @param request
     * @return the available accumHrs values
     */
    private String[] getAccumHrsIdentifierValues(IDataRequest request) {
        // Need parameter and required ids to get accumHrs values
        validateRequest(request);
        String sourceName = request.getParameters()[0];
        String wfo = extractIdentifierValue(request, WFO, String.class);
        String[] siteKeys = getSiteKeyValues(request);

        FFMPSourceConfigurationManager sourceConfig = FFMPSourceConfigurationManager
                .getInstance();
        SourceXML source = sourceConfig.getSource(sourceName);
        if (source == null || !isGuidance(source)) {
            // accumHrs id is only used for guidance sources
            return new String[0];
        }
        String primarySourceName = sourceConfig.getPrimarySource(source);
        ProductXML product = sourceConfig.getProduct(primarySourceName);
        FFMPRunXML runner = FFMPRunConfigurationManager.getInstance()
                .getRunner(wfo);

        /*
         * Available accumHrs values are all numbers between 0 and the largest
         * number of hours listed by interpolator for the given source, since
         * any number of hours <= max can be interpolated
         */
        Double maxHrs = -1.0;
        for (String siteKey : siteKeys) {
            ProductRunXML productRun = runner.getProduct(siteKey);
            FFMPGuidanceInterpolation interpolator = new FFMPGuidanceInterpolation(
                    sourceConfig, product, productRun, primarySourceName,
                    source.getDisplayName(), siteKey);

            List<Double> hrs = interpolator.orderedHours;
            if (hrs != null && !hrs.isEmpty()) {
                // Listed in increasing order, so last is max
                double currMaxHrs = hrs.get(hrs.size() - 1);
                maxHrs = Math.max(maxHrs, currMaxHrs);
            }
        }

        if (maxHrs < 0) {
            // No hours found
            return new String[0];
        } else {
            String hrsRange = 0 + "-" + maxHrs;
            return new String[] { "**any number of hours in range " + hrsRange };
        }
    }

    /**
     * The soft reference wrapped cache, if no longer needed, It will just fade
     * away.
     */
    private FFMPDataCache getCache(String wfo) {

        return FFMPDataCache.getInstance(wfo);
    }

    /**
     * Return whether the given source is a guidance source
     * 
     * @param source
     * @return true if the source is guidance, otherwise false
     */
    private boolean isGuidance(SourceXML source) {
        return source.getSourceType().equals(
                SOURCE_TYPE.GUIDANCE.getSourceType());
    }

    private String buildDataKeysString(Set<String> dataKeys) {
        // Comma-separate the dataKeys
        StringBuilder dataKeysString = new StringBuilder();
        Iterator<String> itr = dataKeys.iterator();
        while (itr.hasNext()) {
            dataKeysString.append(itr.next());
            if (itr.hasNext()) {
                dataKeysString.append(",");
            }
        }
        return dataKeysString.toString();
    }
}