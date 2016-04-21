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
package com.raytheon.viz.warngen.gis;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.NonSI;
import javax.measure.unit.SI;

import org.apache.commons.lang.Validate;

import com.raytheon.uf.common.activetable.ActiveTableRecord;
import com.raytheon.uf.common.activetable.OperationalActiveTableRecord;
import com.raytheon.uf.common.activetable.PracticeActiveTableRecord;
import com.raytheon.uf.common.dataplugin.warning.WarningRecord.WarningAction;
import com.raytheon.uf.common.dataplugin.warning.config.AreaSourceConfiguration;
import com.raytheon.uf.common.dataplugin.warning.config.WarngenConfiguration;
import com.raytheon.uf.common.dataplugin.warning.gis.GeospatialData;
import com.raytheon.uf.common.dataquery.requests.DbQueryRequest;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.dataquery.responses.DbQueryResponse;
import com.raytheon.uf.common.status.IPerformanceStatusHandler;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.PerformanceStatus;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.util.Pair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.requests.ThriftClient;
import com.raytheon.viz.core.mode.CAVEMode;
import com.raytheon.viz.warngen.gis.MarineWordingConfiguration.MarineWordingEntry;
import com.raytheon.viz.warngen.gui.WarngenLayer;
import com.raytheon.viz.warngen.gui.WarngenLayer.GeoFeatureType;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.Polygon;

/**
 * Determines the valid watches related to the warning.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 17, 2014 3419       jsanchez     Initial creation
 * Aug 20, 2014 ASM #16703 D. Friedman  Ensure watches have a state attribute.
 * Aug 28, 2014 ASM #15658 D. Friedman  Add marine zones.
 * Aug 29, 2014 ASM #15551 Qinglu Lin   Sort watches by ETN and filter out ActiveTableRecord
 *                                      with act of CAN and EXP in processRecords().
 * Sep 25, 2014 ASM #15551 Qinglu Lin   Prevent a county's WOU from being used while its
 *                                      corresponding WCN is canceled or expired, prevent NEW
 *                                      from being used while CON/EXT is issued, and prevent duplicate
 *                                      /missing (part of state, state abbreviation) which resulted from 
 *                                      extension of a watch to counties which are of same/different fe_area.  
 * Sep 25, 2014 ASM #16783 D. Friedman  Do not use VTEC action to determine Watch uniqueness.
 * Apr 28, 2015 RODO #4027 randerso     Expunged Calendar from ActiveTableRecord
 * May  7, 2015 ASM #17438 D. Friedman  Clean up debug and performance logging.
 * Jun 04, 2015 RODO #4522 randerso     Added proper primary key to ActiveTableRecord
 * Jul 16, 2015 ASM #17741 D. Friedman  Use acceptable timestamp format in query
 * </pre>
 * 
 * @author jsanchez
 * @version 1.0
 */

public class WatchUtil {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(WatchUtil.class);

    private static final IPerformanceStatusHandler perfLog = PerformanceStatus
            .getHandler("WG:");

    private static final UnitConverter milesToKilometer = NonSI.MILE
            .getConverterTo(SI.KILOMETER);

    private static final double KmToDegrees = 111.12;

    private static final String ISSUE_TIME_FIELD = "issueTime";

    private static final String START_TIME_FIELD = "startTime";

    private static final String END_TIME_FIELD = "endTime";

    private static final String UGC_ZONE_FIELD = "key.ugcZone";

    private static final String PHEN_SIG_FIELD = "phensig";

    private static final String ETN_FIELD = "key.etn";

    private static final String ACTION_FIELD = "act";

    private static final String COUNTY_FIPS_FIELD = "FIPS";

    private static final String COUNTY_FE_AREA_FIELD = "FE_AREA";

    private static final Object MARINE_ZONE_UGC_FIELD = "ID";

    private static final Object MARINE_ZONE_NAME_FIELD = "NAME";

    private static final String STATE_FIELD = "STATE";

    private static final String COUNTY_TABLE = "County";

    private static final String MARINE_ZONE_TABLE = "MarineZones";

    private static final String PARENT_NAME_FIELD = "NAME";

    private static final String[] REQUEST_FIELDS = new String[] {
            ISSUE_TIME_FIELD, START_TIME_FIELD, END_TIME_FIELD, UGC_ZONE_FIELD,
            PHEN_SIG_FIELD, END_TIME_FIELD, ACTION_FIELD, ETN_FIELD };

    private GeospatialData[] countyGeoData;

    private GeospatialData[] marineGeoData;

    private WarngenLayer warngenLayer;

    private MarineWordingConfiguration marineWordingConfig;

    public WatchUtil(WarngenLayer warngenLayer) throws InstantiationException {
        countyGeoData = warngenLayer.getGeodataFeatures(COUNTY_TABLE,
                warngenLayer.getLocalizedSite());
        if ((countyGeoData == null) || (countyGeoData.length == 0)) {
            throw new InstantiationException("Cannot get geospatial data for "
                    + COUNTY_TABLE + "-based watches");
        }
        this.warngenLayer = warngenLayer;
    }

    /**
     * Retrieves valid watches based on the constraints in the config, the
     * warning polygon, and the current simulated time.
     * 
     * @param config
     * @param warningPolygon
     * @param simulatedTime
     * @return
     * @throws Exception
     */
    public List<Watch> getWatches(WarngenConfiguration config,
            Geometry warningPolygon, Date simulatedTime) throws Exception {
        List<Watch> watches = null;
        AreaSourceConfiguration hatchedAreaSourceConfig = config
                .getHatchedAreaSource();
        // Validation check
        Validate.notNull(hatchedAreaSourceConfig,
                "Cannot process watches: missing HATCHING area source configuration");

        double watchAreaBuffer = hatchedAreaSourceConfig
                .getIncludedWatchAreaBuffer();
        // Validation check
        Validate.isTrue(watchAreaBuffer >= 0,
                "'includedWatchAreaBuffer' can not be negative in .xml file");

        if (config.isIncludeMarineAreasInWatches()) {
            marineGeoData = warngenLayer.getGeodataFeatures(MARINE_ZONE_TABLE,
                    warngenLayer.getLocalizedSite());
            if (marineGeoData == null) {
                throw new VizException("Cannot get geospatial data for "
                        + MARINE_ZONE_TABLE + "-based watches");
            }

            marineWordingConfig = MarineWordingConfiguration.load(warngenLayer);
        }

        String[] includedWatches = config.getIncludedWatches();

        if ((includedWatches != null) && (includedWatches.length > 0)) {
            StringBuilder phenSigConstraint = new StringBuilder();
            Iterator<String> iterator = Arrays.asList(includedWatches)
                    .iterator();
            while (iterator.hasNext()) {
                phenSigConstraint.append(iterator.next());
                if (iterator.hasNext()) {
                    phenSigConstraint.append(",");
                }
            }

            // Determine entity class
            Class<? extends ActiveTableRecord> entityClass = OperationalActiveTableRecord.class;
            if (CAVEMode.getMode() != CAVEMode.OPERATIONAL) {
                entityClass = PracticeActiveTableRecord.class;
            }

            HashSet<String> allUgcs = new HashSet<String>(
                    warngenLayer.getAllUgcs(GeoFeatureType.COUNTY));
            Set<String> marineUgcs = null;
            if (config.isIncludeMarineAreasInWatches()) {
                marineUgcs = warngenLayer.getAllUgcs(GeoFeatureType.MARINE);
                allUgcs.addAll(marineUgcs);
            }

            DbQueryRequest request = buildRequest(simulatedTime,
                    phenSigConstraint.toString(), allUgcs, entityClass);
            DbQueryResponse response = (DbQueryResponse) ThriftClient
                    .sendRequest(request);

            List<ActiveTableRecord> records = convertReponse(entityClass,
                    response);

            if (records.isEmpty() == false) {
                try {
                    long t0 = System.currentTimeMillis();
                    Polygon watchArea = (Polygon) warningPolygon
                            .buffer(milesToKilometer.convert(watchAreaBuffer)
                                    / KmToDegrees);
                    perfLog.logDuration("Create watch area buffer time",
                            System.currentTimeMillis() - t0);
                    HashSet<String> validUgcZones = new HashSet<String>(
                            warngenLayer.getUgcsForWatches(watchArea,
                                    GeoFeatureType.COUNTY));
                    if (config.isIncludeMarineAreasInWatches()) {
                        validUgcZones.addAll(warngenLayer.getUgcsForWatches(
                                watchArea, GeoFeatureType.MARINE));
                    }
                    watches = processRecords(records, validUgcZones, marineUgcs);
                } catch (RuntimeException e) {
                    statusHandler
                            .handle(Priority.ERROR,
                                    "Error determining areas to search for watches.",
                                    e);
                }
            }
        }

        return watches;
    }

    /**
     * Builds a DBQueryRequest object.
     * 
     * @param simulatedTime
     * @param phenSig
     * @param ugcs
     * @param entityClass
     * @return
     */
    private static DbQueryRequest buildRequest(Date simulatedTime,
            String phenSig, Set<String> ugcs,
            Class<? extends ActiveTableRecord> entityClass) {
        // Create start constraint
        Calendar cal = Calendar.getInstance();
        cal.setTime(simulatedTime);
        cal.add(Calendar.MINUTE, 3);
        Date startConstraintTime = cal.getTime();

        DbQueryRequest request = new DbQueryRequest();
        request.setEntityClass(entityClass);
        request.addConstraint(START_TIME_FIELD,
                new RequestConstraint(TimeUtil.formatToSqlTimestamp(startConstraintTime),
                        ConstraintType.LESS_THAN_EQUALS));
        request.addConstraint(END_TIME_FIELD,
                new RequestConstraint(TimeUtil.formatToSqlTimestamp(simulatedTime),
                        ConstraintType.GREATER_THAN_EQUALS));
        request.addConstraint("phensig",
                new RequestConstraint(phenSig.toString(), ConstraintType.IN));

        /*
         * Get all UGCs in the CWA now so that the watches will be formatted
         * with all portions of the affected state(s).
         * 
         * Filtering for valid UGCs is performed in processATEntries
         */
        RequestConstraint ugcConstraint = new RequestConstraint("",
                ConstraintType.IN);
        ugcConstraint.setConstraintValueList(ugcs);
        request.addConstraint(UGC_ZONE_FIELD, ugcConstraint);

        // These are the only fields we need for processing watches
        request.addFields(REQUEST_FIELDS);

        return request;
    }

    /**
     * Converts the results of DbQueryResponse into a list of
     * ActiveTableRecords.
     * 
     * @param entityClass
     * @param response
     * @return
     * @throws IllegalAccessException
     * @throws InstantiationException
     */
    private static final List<ActiveTableRecord> convertReponse(
            Class<? extends ActiveTableRecord> entityClass,
            DbQueryResponse response) throws IllegalAccessException,
            InstantiationException {
        List<ActiveTableRecord> records = new ArrayList<ActiveTableRecord>(
                response.getNumResults());
        Map<Pair<String, String>, Set<String>> removedUgczones = new HashMap<Pair<String, String>, Set<String>>();
        Set<String> ugczones = null;
        for (Map<String, Object> result : response.getResults()) {
            WarningAction action = WarningAction.valueOf(String.valueOf(result
                    .get(ACTION_FIELD)));
            /*
             * TODO: Currently limited to filtering out one of ("CAN","EXP").
             * Could use "Act" in addition to "act", but this should really be
             * fixed the underlying system. request.addConstraint("act", new
             * RequestConstraint("CAN", ConstraintType.NOT_EQUALS));
             */
            if ((action != WarningAction.CAN) && (action != WarningAction.EXP)) {
                ActiveTableRecord record = entityClass.newInstance();
                record.setIssueTime((Date) result.get(ISSUE_TIME_FIELD));
                record.setStartTime((Date) result.get(START_TIME_FIELD));
                record.setEndTime((Date) result.get(END_TIME_FIELD));
                record.setEndTime((Date) result.get(END_TIME_FIELD));
                record.setUgcZone(String.valueOf(result.get(UGC_ZONE_FIELD)));
                record.setPhensig(String.valueOf(result.get(PHEN_SIG_FIELD)));
                record.setEtn(String.valueOf(result.get(ETN_FIELD)));
                record.setAct(String.valueOf(result.get(ACTION_FIELD)));
                records.add(record);
            } else {
                Pair<String, String> key = new Pair<String, String>(null, null);
                key.setFirst(String.valueOf(result.get(ETN_FIELD)));
                key.setSecond(String.valueOf(result.get(PHEN_SIG_FIELD)));
                ugczones = removedUgczones.get(key);
                if (ugczones == null) {
                    ugczones = new HashSet<String>();
                }
                ugczones.add(String.valueOf(result.get(UGC_ZONE_FIELD)));
                removedUgczones.put(key, ugczones);
            }
        }

        /*
         * remove ActiveTableRecord from records whose etn, ugcZone, and phensig
         * is same as canceled or expired.
         */
        String etn, ugczone, phensig;
        for (Pair<String, String> etnPhensig : removedUgczones.keySet()) {
            ugczones = removedUgczones.get(etnPhensig);
            etn = etnPhensig.getFirst();
            phensig = etnPhensig.getSecond();
            Iterator<String> iter = ugczones.iterator();
            while (iter.hasNext()) {
                ugczone = iter.next();
                Iterator<ActiveTableRecord> iterator = records.iterator();
                while (iterator.hasNext()) {
                    ActiveTableRecord atr = iterator.next();
                    if (atr.getEtn().equals(etn)
                            && atr.getUgcZone().equals(ugczone)
                            && atr.getPhensig().equals(phensig)) {
                        iterator.remove();
                    }
                }
            }
        }

        Collections.sort(records, PEUI);

        /*
         * Filters out extra ActiveTableRecords that have same phenSig, etn, and
         * ugcZone.
         */
        Map<String, ActiveTableRecord> atrMap = new LinkedHashMap<String, ActiveTableRecord>();
        for (ActiveTableRecord atr : records) {
            String key = atr.getPhensig() + atr.getEtn() + atr.getUgcZone();
            atrMap.put(key, atr);
        }
        records = new ArrayList<ActiveTableRecord>(atrMap.values());

        return records;
    }

    /**
     * Groups the activeTableRecords into Watch objects that share phenSig,
     * action, ETN, start time, and end time. It also determines the part of
     * state the watch covers.
     * 
     * @param activeTableRecords
     * @param validUgcZones
     * @param marineUgcs
     * 
     * @return
     */
    private List<Watch> processRecords(
            List<ActiveTableRecord> activeTableRecords,
            Set<String> validUgcZones, Set<String> marineUgcs) {
        List<Watch> watches = new ArrayList<Watch>();

        /*
         * Assumption 1: TO.A and SV.A UGC line will always be in county format
         * from WOU.
         */
        /*
         * Assumption 2: At least 1 warning for the issuing site supports county
         * based warnings. This will allow the county geo features to be cached.
         */

        Map<Watch, List<String>> map = new HashMap<Watch, List<String>>();
        // For each watch event, get the end time and list of active zones
        for (ActiveTableRecord ar : activeTableRecords) {
            /*
             * Currently reports all zones in the watch even if a given zone is
             * not in the warning polygon. If the logic is changed to only show
             * the portions of the watch near our warning polygon, filter on
             * validUgcZones here.
             */
            String ugcZone = ar.getUgcZone();
            String state = null;

            if ((marineUgcs != null) && marineUgcs.contains(ugcZone)) {
                // Just leave state == null
            } else {
                state = getStateName(ugcZone.substring(0, 2));
                if (state == null) {
                    continue;
                }
            }

            String phenSig = ar.getPhensig();
            String etn = ar.getEtn();
            Date startTime = ar.getStartTime();
            Date endTime = ar.getEndTime();

            if (validUgcZones.contains(ugcZone)) {
                Watch watch = new Watch(state, phenSig, etn, startTime, endTime);
                List<String> areas = map.get(watch);
                if (areas == null) {
                    areas = new ArrayList<String>();
                }
                areas.add(ugcZone);
                map.put(watch, areas);
            }
        }

        // Sets the areas for the watch
        for (Entry<Watch, List<String>> entry : map.entrySet()) {
            Watch watch = entry.getKey();
            watch.setAreas(entry.getValue());
            if (watch.getState() != null) {
                List<String> partOfState = new ArrayList<String>(
                        determineAffectedPortions(watch.getAreas()));
                watch.setPartOfState(partOfState);
                watches.add(watch);
            } else {
                watches.addAll(generateMarineWatchItems(watch,
                        determineMarineAreas(watch.getAreas())));
            }
        }

        /*
         * Sorts the watches based on ETN, then state. Marine areas have a null
         * state value so they appear at the end of each watch.
         */
        Collections.sort(watches, new Comparator<Watch>() {

            @Override
            public int compare(Watch watch1, Watch watch2) {
                String etn1 = watch1.getEtn();
                String etn2 = watch2.getEtn();
                int c;
                if (etn1 == etn2) {
                    c = 0;
                } else if (etn1 == null) {
                    return 1;
                } else if (etn2 == null) {
                    return -1;
                } else {
                    c = etn1.compareTo(etn2);
                }
                if (c != 0) {
                    return c;
                }

                String state1 = watch1.getState();
                String state2 = watch2.getState();
                if (state1 == state2) {
                    return 0;
                } else if (state1 == null) {
                    return 1; // null state is greater; put at end
                } else if (state2 == null) {
                    return -1;
                } else {
                    return state1.compareTo(state2);
                }
            }
        });

        /*
         * Filters out extra Watches that have different startTime but same
         * phenSig, etn, state, partOfState, endTime, and marineArea.
         */
        Map<String, Watch> watchMap = new LinkedHashMap<String, Watch>();
        for (Watch w : watches) {
            List<String> pos = w.getPartOfState() != null ? new ArrayList<String>(
                    w.getPartOfState()) : null;
            if (pos != null) {
                Collections.sort(pos);
            }
            String key = String.valueOf(w.getPhenSig())
                    + String.valueOf(w.getEtn()) + String.valueOf(w.getState())
                    + String.valueOf(pos) + String.valueOf(w.getEndTime());
            if (w.getMarineArea() != null) {
                key = key + '.' + w.getMarineArea();
            }
            watchMap.put(key, w);
        }
        watches = new ArrayList<Watch>(watchMap.values());

        return watches;
    }

    /**
     * Determines the directional set of a state.
     * 
     * @param ugcs
     * @return
     */
    private Set<String> determineAffectedPortions(List<String> ugcs) {
        Set<String> feAreas = new HashSet<String>();
        for (String ugc : ugcs) {
            // Want the first 2 letters
            String stateAbbrev = ugc.substring(0, 2);
            // Want the last 3 digits
            String fips = ugc.substring(ugc.length() - 3);
            String feArea = getFeArea(stateAbbrev, fips);
            // Checks to see if feArea in CWA
            if (feArea != null) {
                feAreas.add(feArea);
            }
        }

        Set<String> affectedPortions = new HashSet(
                Area.converFeAreaToPartList(mungeFeAreas(feAreas)));
        return affectedPortions;
    }

    private List<Watch> generateMarineWatchItems(Watch template,
            List<String> areas) {
        ArrayList<Watch> result = new ArrayList<Watch>();
        for (String area : areas) {
            Watch watch = new Watch(template.getState(), template.getPhenSig(),
                    template.getEtn(), template.getStartTime(),
                    template.getEndTime());
            watch.setMarineArea(area);
            result.add(watch);
        }
        return result;
    }

    private List<String> determineMarineAreas(List<String> areas) {
        HashSet<Pair<Integer, String>> groupedAreas = new HashSet<Pair<Integer, String>>();
        for (String area : areas) {
            int entryIndex = 0;
            for (MarineWordingEntry entry : marineWordingConfig.getEntries()) {
                if (entry.getUgcPattern().matcher(area).matches()) {
                    String replacement = entry.getReplacementText();
                    if (replacement != null) {
                        if (replacement.length() > 0) {
                            groupedAreas.add(new Pair<Integer, String>(
                                    entryIndex, entry.getReplacementText()));
                        }
                    } else {
                        groupedAreas.add(new Pair<Integer, String>(entryIndex,
                                getMarineZoneName(area)));
                    }
                    break;
                }
                entryIndex++;
            }
        }
        ArrayList<Pair<Integer, String>> sorted = new ArrayList<Pair<Integer, String>>(
                groupedAreas);
        Collections.sort(sorted, new Comparator<Pair<Integer, String>>() {
            @Override
            public int compare(Pair<Integer, String> o1,
                    Pair<Integer, String> o2) {
                int r = o1.getFirst().compareTo(o2.getFirst());
                return r != 0 ? r : o1.getSecond().compareTo(o2.getSecond());
            };
        });
        ArrayList<String> result = new ArrayList<String>(sorted.size());
        for (Pair<Integer, String> value : sorted) {
            result.add(value.getSecond());
        }
        return result;
    }

    /**
     * Returns the full state name from the state abbreviation.
     * 
     * @param stateAbrev
     * @return
     */
    private String getStateName(String stateAbrev) {
        for (GeospatialData g : countyGeoData) {
            if (stateAbrev.equals(g.attributes.get(STATE_FIELD))) {
                return (String) g.parent.attributes.get(PARENT_NAME_FIELD);
            }
        }
        return null;
    }

    /**
     * Returns the feArea field in the county table (i.e. n, s, e, w).
     * 
     * @param stateAbbrev
     * @param ugc
     * @return
     */
    private String getFeArea(String stateAbbrev, String ugc) {
        for (GeospatialData g : countyGeoData) {
            if (stateAbbrev.equals(g.attributes.get(STATE_FIELD))
                    && ((String) g.attributes.get(COUNTY_FIPS_FIELD))
                            .endsWith(ugc)) {
                return (String) g.attributes.get(COUNTY_FE_AREA_FIELD);
            }
        }

        return null;
    }

    private String getMarineZoneName(String ugc) {
        for (GeospatialData g : marineGeoData) {
            if (((String) g.attributes.get(MARINE_ZONE_UGC_FIELD))
                    .endsWith(ugc)) {
                return (String) g.attributes.get(MARINE_ZONE_NAME_FIELD);
            }
        }
        return null;
    }

    // Based on AWIPS 1 SELSparagraphs.C SELSparagraphs::processWOU().
    private String mungeFeAreas(Set<String> feAreas) {
        String abrev = "";
        // If eight or more portions, don't qualify area of state
        int m = feAreas.size();
        if (m < 8) {
            String partAbrev = "";
            /*
             * TODO: Unused variables should be removed if we are not going to
             * improve this in A2.
             */
            @SuppressWarnings("unused")
            int nw, nc, ne, wc, cc, ec, sw, sc, se, pa;
            int eee, www, nnn, sss, ee, ww, nn, ss;

            // Identify individual sub areas of this state affected
            nw = nc = ne = wc = cc = ec = sw = sc = se = pa = 0;
            eee = www = nnn = sss = ee = ww = nn = ss = 0;
            for (String part : feAreas) {
                if ("pa".equals(part)) {
                    pa = 1;
                    continue;
                } else if ("nn".equals(part)) {
                    nnn = nn = 1;
                } else if ("ss".equals(part)) {
                    sss = ss = 1;
                } else if ("ee".equals(part)) {
                    eee = ee = 1;
                } else if ("ww".equals(part)) {
                    www = ww = 1;
                } else if ("nw".equals(part)) {
                    nnn = www = nw = 1;
                } else if ("nc".equals(part)) {
                    nnn = nc = 1;
                } else if ("ne".equals(part)) {
                    nnn = eee = ne = 1;
                } else if ("wc".equals(part)) {
                    www = wc = 1;
                } else if ("cc".equals(part)) {
                    cc = 1;
                    continue;
                } else if ("ec".equals(part)) {
                    eee = ec = 1;
                } else if ("sw".equals(part)) {
                    sss = www = sw = 1;
                } else if ("sc".equals(part)) {
                    sss = sc = 1;
                } else if ("se".equals(part)) {
                    sss = eee = se = 1;
                }
                partAbrev = part;
            }
            // decide how to describe these subareas.
            if ((ne > 0) && (nw > 0)) {
                nn = 1;
            }
            if ((se > 0) && (sw > 0)) {
                ss = 1;
            }
            if ((se > 0) && (ne > 0)) {
                ee = 1;
            }
            if ((sw > 0) && (nw > 0)) {
                ww = 1;
            }
            if ((nnn > 0) && (sss > 0) && (eee > 0) && (www > 0)) {
                return abrev;
            }
            if (((nn > 0) && (ss > 0)) || ((ee > 0) && (ww > 0))) {
                return abrev;
            }
            if ((nnn + sss + eee + www) == 3) {
                if (www == 0) {
                    abrev = "e";
                } else if (eee == 0) {
                    abrev = "w";
                } else if (nnn == 0) {
                    abrev = "s";
                } else if (sss == 0) {
                    abrev = "n";
                }
                return abrev;
            }
            if (((nnn == sss) && (eee == www)) || (cc == m)) {
                abrev = "c";
                return abrev;
            }
            if ((pa != 0) && (cc == 0)) {
                abrev = "pa";
                if (--m <= 0) {
                    return abrev;
                }
            }
            if (m == (1 + cc)) {
                abrev += partAbrev + " ";
                return abrev;
            }
            if (nnn != sss) {
                abrev += nnn != 0 ? "n" : "s";
            }
            if (eee != www) {
                abrev += eee != 0 ? "e" : "w";
            }
        }
        return abrev;
    }

    // ActiveTableRecord: phenSig, etn, ugcZone, issueTime
    public static final Comparator<ActiveTableRecord> PEUI = new Comparator<ActiveTableRecord>() {
        @Override
        public int compare(ActiveTableRecord o1, ActiveTableRecord o2) {
            int i = o1.getPhensig().compareTo(o2.getPhensig());
            if (i == 0) {
                i = o1.getEtn().compareTo(o2.getEtn());
                if (i == 0) {
                    i = o1.getUgcZone().compareTo(o2.getUgcZone());
                    if (i == 0) {
                        i = o1.getIssueTime().compareTo(o2.getIssueTime());
                    }
                }
            }
            return i;
        }
    };

}
