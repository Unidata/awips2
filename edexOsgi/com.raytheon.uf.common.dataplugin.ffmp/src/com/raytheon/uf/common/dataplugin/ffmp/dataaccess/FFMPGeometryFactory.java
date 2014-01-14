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
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.measure.unit.Unit;

import com.raytheon.uf.common.dataaccess.IDataRequest;
import com.raytheon.uf.common.dataaccess.exception.DataRetrievalException;
import com.raytheon.uf.common.dataaccess.geom.IGeometryData;
import com.raytheon.uf.common.dataaccess.impl.AbstractDataPluginFactory;
import com.raytheon.uf.common.dataaccess.impl.DefaultGeometryData;
import com.raytheon.uf.common.dataaccess.util.DatabaseQueryUtil;
import com.raytheon.uf.common.dataaccess.util.DatabaseQueryUtil.QUERY_MODE;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPBasin;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPBasinData;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPGuidanceBasin;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPRecord;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPTemplates;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPTemplates.MODE;
import com.raytheon.uf.common.dataplugin.ffmp.HucLevelGeometriesFactory;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.dataquery.responses.DbQueryResponse;
import com.raytheon.uf.common.monitor.config.FFMPRunConfigurationManager;
import com.raytheon.uf.common.monitor.config.FFMPSourceConfigurationManager;
import com.raytheon.uf.common.monitor.xml.DomainXML;
import com.raytheon.uf.common.monitor.xml.SourceXML;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
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

    /** FFMP Templates object */
    private FFMPTemplates templates;

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
        Map<Long, DefaultGeometryData> cache = new HashMap<Long, DefaultGeometryData>();

        for (Map<String, Object> map : results) {
            for (Map.Entry<String, Object> es : map.entrySet()) {
                FFMPRecord rec = (FFMPRecord) es.getValue();
                try {
                    rec.retrieveMapFromDataStore(templates);
                } catch (Exception e) {
                    throw new DataRetrievalException(
                            "Failed to retrieve the IDataRecord for PluginDataObject: "
                                    + rec.toString(), e);
                }

                try {
                    cache = makeGeometryData(rec, request, cache);
                } catch (Exception e) {
                    statusHandler.handle(Priority.PROBLEM,
                            e.getLocalizedMessage(), e);
                }
            }
        }

        return cache.values().toArray(
                new DefaultGeometryData[cache.values().size()]);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected Map<String, RequestConstraint> buildConstraintsFromRequest(
            IDataRequest request) {
        Map<String, RequestConstraint> map = new HashMap<String, RequestConstraint>();
        Map<String, Object> identifiers = request.getIdentifiers();
        String siteKey = (String) identifiers.get(SITE_KEY);
        for (Map.Entry<String, Object> entry : request.getIdentifiers()
                .entrySet()) {
            String key = entry.getKey();
            String value = (String) entry.getValue();
            if (!key.equals(HUC)) {
                RequestConstraint rc = new RequestConstraint(value);
                map.put(key, rc);
            }
        }

        RequestConstraint parameterConstraint = new RequestConstraint();
        parameterConstraint.setConstraintValueList(request.getParameters());
        parameterConstraint.setConstraintType(ConstraintType.IN);
        map.put(SOURCE_NAME, parameterConstraint);

        String domain = (String) request.getIdentifiers().get(WFO);
        DomainXML domainXml = FFMPRunConfigurationManager.getInstance()
                .getDomain(domain);

        templates = FFMPTemplates.getInstance(domainXml, siteKey, MODE.EDEX);

        return map;
    }

    /**
     * Create the IGeometryData objects.
     * 
     * @param rec
     *            The FFMPRecord
     * @param cache
     * @param huc
     *            The HUC level
     * @param siteKey
     *            The siteKey
     * @param cwa
     *            The CWA
     * @param dataKey
     *            The dataKey
     * @throws Exception
     */
    private Map<Long, DefaultGeometryData> makeGeometryData(FFMPRecord rec,
            IDataRequest request, Map<Long, DefaultGeometryData> cache)
            throws Exception {
        String huc = (String) request.getIdentifiers().get(HUC);
        String dataKey = (String) request.getIdentifiers().get(DATA_KEY);
        String siteKey = (String) request.getIdentifiers().get(SITE_KEY);
        String cwa = (String) request.getIdentifiers().get(WFO);

        if (dataKey == null) {
            dataKey = siteKey;
        }

        FFMPBasinData basinData = rec.getBasinData();

        Map<Long, FFMPBasin> basinDataMap = basinData.getBasins();

        HucLevelGeometriesFactory geomFactory = HucLevelGeometriesFactory
                .getInstance();
        // BAL - Switched to use siteKey instead of dataKey.
        // Map<Long, Geometry> geomMap = geomFactory.getGeometries(templates,
        // dataKey, cwa, huc);
        Map<Long, Geometry> geomMap = geomFactory.getGeometries(templates,
                siteKey, cwa, huc);

        FFMPSourceConfigurationManager srcConfigMan = FFMPSourceConfigurationManager
                .getInstance();
        SourceXML sourceXml = srcConfigMan.getSource(rec.getSourceName());

        DefaultGeometryData data = null;

        String[] locationNames = request.getLocationNames();

        List<Long> pfafList = null;
        if (locationNames != null) {
            pfafList = convertLocations(locationNames);
        }

        for (Long pfaf : geomMap.keySet()) {
            if (pfafList == null || pfafList.contains(pfaf)) {
                if (cache.containsKey(pfaf)) {
                    data = cache.get(pfaf);
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
                    cache.put(pfaf, data);
                }

                FFMPBasin basin = basinDataMap.get(pfaf);
                Float value = null;

                if (basin == null) {
                    continue;
                }

                if (basin instanceof FFMPGuidanceBasin) {
                    /*
                     * Bryon L - Added test for FFMPGuidanceBasin object.
                     * Couldn't use getValue(Date, Sourcename) here. Odd problem
                     * with date key reference.
                     */
                    value = ((FFMPGuidanceBasin) basin).getValue(
                            rec.getSourceName(), 1000);
                } else {
                    value = basin.getValue(rec.getDataTime().getRefTime());
                }
                String parameter = rec.getSourceName();
                String unitStr = sourceXml.getUnit();

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

        return cache;
    }

    /**
     * Convert list of PFAF strings to list of PFAF longs
     * 
     * @param locationNames
     * @return
     */
    private List<Long> convertLocations(String[] locationNames) {
        List<Long> pfafList = new ArrayList<Long>();
        for (String s : locationNames) {
            try {
                pfafList.add(Long.parseLong(s));
            } catch (NumberFormatException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error parsing pfaf id: " + s, e);
            }
        }

        return pfafList;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String[] getAvailableLocationNames(IDataRequest request) {
        List<String> pfafList = new ArrayList<String>();
        String domain = (String) request.getIdentifiers().get("wfo");
        String sql = "select pfaf_id from mapdata.ffmp_basins where cwa = '"
                + domain + "';";

        List<Object[]> results = DatabaseQueryUtil.executeDatabaseQuery(
                QUERY_MODE.MODE_SQLQUERY, sql, "metadata", "ffmp");

        for (Object[] oa : results) {
            pfafList.add((String) oa[0]);
        }

        return pfafList.toArray(new String[pfafList.size()]);
    }
}
