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
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.measure.unit.Unit;

import com.raytheon.uf.common.dataaccess.IDataRequest;
import com.raytheon.uf.common.dataaccess.exception.DataRetrievalException;
import com.raytheon.uf.common.dataaccess.exception.IncompatibleRequestException;
import com.raytheon.uf.common.dataaccess.geom.IGeometryData;
import com.raytheon.uf.common.dataaccess.impl.AbstractDataPluginFactory;
import com.raytheon.uf.common.dataaccess.impl.DefaultGeometryData;
import com.raytheon.uf.common.dataaccess.util.DatabaseQueryUtil;
import com.raytheon.uf.common.dataaccess.util.DatabaseQueryUtil.QUERY_MODE;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPBasin;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPBasinData;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPGuidanceBasin;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPGuidanceInterpolation;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPRecord;
import com.raytheon.uf.common.dataplugin.ffmp.HucLevelGeometriesFactory;
import com.raytheon.uf.common.dataplugin.ffmp.collections.FFMPDataCache;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.dataquery.responses.DbQueryResponse;
import com.raytheon.uf.common.monitor.config.FFFGDataMgr;
import com.raytheon.uf.common.monitor.config.FFMPRunConfigurationManager;
import com.raytheon.uf.common.monitor.config.FFMPSourceConfigurationManager;
import com.raytheon.uf.common.monitor.config.FFMPSourceConfigurationManager.SOURCE_TYPE;
import com.raytheon.uf.common.monitor.xml.DomainXML;
import com.raytheon.uf.common.monitor.xml.ProductRunXML;
import com.raytheon.uf.common.monitor.xml.ProductXML;
import com.raytheon.uf.common.monitor.xml.SourceXML;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
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
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 24, 2013   1552     mpduff      Initial creation
 * Apr 16, 2013 1912       bsteffen    Initial bulk hdf5 access for ffmp
 * Jul 15, 2013 2184       dhladky     Remove all HUC's for storage except ALL
 * Aug,20, 2013 2250       mnash       Change some methods that were not working in all cases
 * Jan,14, 2014 2667       mnash       Remove getGridData method
 * May 1, 2014  3099       bkowal      No longer use an empty pfaf list when the
 *                                     data request locationNames list is empty.
 * Jun 24, 2014 3170       mnash       Get the accumulated time if multiple times are requested
 * Jul 14, 2014 3184       njensen     Overrode getAvailableLevels()
 * Jul 30, 2014 3184       njensen     Overrode required and optional identifiers
 * Feb 27, 2015 4180       mapeters    Overrode getAvailableParameters().
 * Jun 15, 2015 4560       ccody       Added support for configurable rate/accumulation calculation for getGeometryData
 * Jul 16, 2015 4658       dhladky     Expiration times fixed.
 * Oct 26, 2015 5056       dhladky     Re-wrote to take advantage of FFMP common data cache, fix general bugs.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
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
        String siteKey = (String) request.getIdentifiers().get(SITE_KEY);
        String cwa = (String) request.getIdentifiers().get(WFO);
        String sourceName = null;
        Date start = new Date(Long.MAX_VALUE);
        Date end = new Date(0);
        FFMPDataCache cache = getCache(cwa);

        // Ensures that all records have been added to the cache before calculations
        for (Map<String, Object> map : results) {
            for (Map.Entry<String, Object> es : map.entrySet()) {
                FFMPRecord rec = (FFMPRecord) es.getValue();
                if (sourceName == null) {
                    sourceName = rec.getSourceName();
                }
                try {
                    cache.populateFFMPRecord(siteKey, rec, rec.getSourceName());
                } catch (Exception e) {
                    statusHandler.handle(Priority.PROBLEM, "Unable to populate FFMPRecord: "+rec.getDataURI(), e);
                }
                
                // building a time range of the earliest FFMP time (based on
                // each record) to the latest FFMP time (based on each record)
                if (start.after(rec.getDataTime().getRefTime())) {
                    start = rec.getDataTime().getRefTime();
                }
                if (end.before(rec.getDataTime().getRefTime())) {
                    end = rec.getDataTime().getRefTime();
                }
            }
        }

        // Time window for FFMP slides +- the 1/2 QPE expiration time.
        int expirationTime = FFMPSourceConfigurationManager.getInstance()
                .getSource(sourceName).getExpirationMinutes(siteKey);
        start = new Date(start.getTime()
                - (TimeUtil.MILLIS_PER_MINUTE * (expirationTime/2)));
        /*
         * now that we have all the basin data in a single record (record), we
         * can use the methods on the FFMPRecord class to get the accumulated
         * value in the case of a non-guidance basin
         */
        Map<Long, DefaultGeometryData> result = null;
        

        try {
            result = makeGeometryData(sourceName, request, start, end);
        } catch (Exception e) {
            throw new DataRetrievalException("Unable to create Geometry Data: sourceName: "+sourceName, e);
        }
        
        return result.values().toArray(
                new DefaultGeometryData[result.values().size()]);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected Map<String, RequestConstraint> buildConstraintsFromRequest(
            IDataRequest request) {
        Map<String, RequestConstraint> map = new HashMap<String, RequestConstraint>();
        for (Map.Entry<String, Object> entry : request.getIdentifiers()
                .entrySet()) {
            String key = entry.getKey();
            // exclude this parameter
            if (!key.equals(ACCUM_HRS)) {
                String value = (String) entry.getValue();
                if (!key.equals(HUC)) {
                    RequestConstraint rc = new RequestConstraint(value);
                    map.put(key, rc);
                }
            }
        }

        RequestConstraint parameterConstraint = new RequestConstraint();
        parameterConstraint.setConstraintValueList(request.getParameters());
        parameterConstraint.setConstraintType(ConstraintType.IN);
        map.put(SOURCE_NAME, parameterConstraint);

        return map;
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
    private Map<Long, DefaultGeometryData> makeGeometryData(String sourceName,
            IDataRequest request, Date start, Date end) throws Exception {

        Map<Long, DefaultGeometryData> result = new HashMap<Long, DefaultGeometryData>();
        String huc = (String) request.getIdentifiers().get(HUC);
        String dataKey = (String) request.getIdentifiers().get(DATA_KEY);
        String siteKey = (String) request.getIdentifiers().get(SITE_KEY);
        String cwa = (String) request.getIdentifiers().get(WFO);
        Number accumulationTime = (Number) request.getIdentifiers().get(ACCUM_HRS);
        FFMPDataCache cache = getCache(cwa);

        if (dataKey == null) {
            dataKey = siteKey;
        }

        FFMPGuidanceInterpolation interpolation = null;
        FFMPBasinData basinData = null;
        SourceXML source = FFMPSourceConfigurationManager.getInstance()
                .getSource(sourceName);
        if (source.getSourceType().equals(SOURCE_TYPE.GUIDANCE.getSourceType())) {
            basinData = cache.getSourceData(siteKey, source.getDisplayName())
                    .getRecord().getBasinData();
            interpolation = getGuidanceInterpolation(
                    accumulationTime.doubleValue(), sourceName, cwa, siteKey);
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
         * This is a misnomer that has caused loads of confusion.
         * In actuality when the FFMPBasin accumulates it checks that the
         * Type is NOT a rate and in fact should be accumulating. RATE == false.
         * So in effect, values that FFMP stores as RATE==true are actually
         * NOT rates when accumulated.  So if, it is stored as RATE, it
         * must use FALSE when processing the accumulation.
         */
        if ((rateOrAccum != null) && (rateOrAccum.isEmpty() == false)
               && (rateOrAccum.compareToIgnoreCase("RATE") == 0)) {
            isRate = false;
        }
        DefaultGeometryData data = null;

        HucLevelGeometriesFactory geomFactory = HucLevelGeometriesFactory
                .getInstance();
        Map<Long, Geometry> geomMap = geomFactory.getGeometries(getCache(cwa).getTemplates(siteKey),
                siteKey, cwa, huc);

        List<Long> pfafList = getAvailableLocationPfafs(request);

        for (Long pfaf : geomMap.keySet()) {
            if (pfafList == null || pfafList.contains(pfaf)) {
                if (result.containsKey(pfaf)) {
                    data = result.get(pfaf);
                } else {
                    data = new DefaultGeometryData();
                    Map<String, Object> attrs = new HashMap<String, Object>();
                    attrs.put(DATA_KEY, dataKey);
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
                    if (dman.isExpired() == false) {
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

   
    /**
     * {@inheritDoc}
     */
    @Override
    public String[] getAvailableLocationNames(IDataRequest request) {
        // Changed this to query the active Templates, not the DB.
        List<Long> pfafs = getAvailableLocationPfafs(request);
        
        List<String> pfafList = new ArrayList<String>(pfafs.size());
        for (Long pfaf : pfafs) { 
            pfafList.add(String.valueOf(pfaf)); 
        }

        return pfafList.toArray(new String[0]);
    }
    
    /**
     * Gets the available location Pfafs.
     * @param request
     * @return
     */
    private List<Long> getAvailableLocationPfafs(IDataRequest request) {
        String siteKey = (String) request.getIdentifiers().get(SITE_KEY);
        String wfo = (String) request.getIdentifiers().get(WFO);
        FFMPDataCache cache = getCache(wfo);
        List<DomainXML> domains = cache.getTemplates(siteKey).getDomains();
        return cache.getTemplates(siteKey).getHucKeyList(siteKey, FFMPRecord.ALL, domains);
    }

    @Override
    public String[] getAvailableParameters(IDataRequest request) {
        StringBuilder sqlQuery = new StringBuilder("select distinct ")
                .append(SOURCE_NAME).append(" from ").append(PLUGIN_NAME);

        String keyWord = " where ";
        for (Map.Entry<String, Object> entry : request.getIdentifiers()
                .entrySet()) {
            String key = entry.getKey();
            String value = (String) entry.getValue();
            sqlQuery.append(keyWord).append(key).append(" = '").append(value)
                    .append("'");
            keyWord = " and ";
        }
        sqlQuery.append(";");

        List<Object[]> results = DatabaseQueryUtil.executeDatabaseQuery(
                QUERY_MODE.MODE_SQLQUERY, sqlQuery.toString(), "metadata",
                PLUGIN_NAME);

        List<String> params = new ArrayList<>(results.size());
        for (Object[] r : results) {
            params.add((String) r[0]);
        }
        return params.toArray(new String[0]);
    }

    @Override
    public Level[] getAvailableLevels(IDataRequest request) {
        throw new IncompatibleRequestException(request.getDatatype()
                + " data does not support the concept of levels");
    }

    @Override
    public String[] getRequiredIdentifiers() {
        return new String[] { SITE_KEY, WFO, HUC };
    }

    @Override
    public String[] getOptionalIdentifiers() {
        return new String[] { DATA_KEY, ACCUM_HRS };
    }

    /**
     * Get the FFG Object need to interpolate between sources
     * 
     * @param accumulationTime
     * @param sourceName
     * @param wfo
     * @param siteKey
     * @return
     */
    private FFMPGuidanceInterpolation getGuidanceInterpolation(
            Double accumulationTime, String sourceName, String wfo,
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
        interpolator.setInterpolationSources(accumulationTime);

        return interpolator;
    }



    /**
     * The soft reference wrapped cache, if no longer needed, 
     * It will just fade away.
     */
    private FFMPDataCache getCache(String wfo) {

        return FFMPDataCache.getInstance(wfo);
    }

}
