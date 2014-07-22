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
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.viz.core.requests.ThriftClient;
import com.raytheon.viz.core.mode.CAVEMode;
import com.raytheon.viz.warngen.gui.WarngenLayer;
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
 * 
 * </pre>
 * 
 * @author jsanchez
 * @version 1.0
 */

public class WatchUtil {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(WatchUtil.class);

    private static final UnitConverter milesToKilometer = NonSI.MILE
            .getConverterTo(SI.KILOMETER);

    private static final double KmToDegrees = 111.12;

    private static final String ISSUE_TIME_FIELD = "issueTime";

    private static final String START_TIME_FIELD = "startTime";

    private static final String END_TIME_FIELD = "endTime";

    private static final String UGC_ZONE_FIELD = "ugcZone";

    private static final String PHEN_SIG_FIELD = "phensig";

    private static final String ETN = "etn";

    private static final String ACTION = "act";

    private static final String COUNTY_FIPS_FIELD = "FIPS";

    private static final String COUNTY_FE_AREA_FIELD = "FE_AREA";

    private static final String STATE_FIELD = "STATE";

    private static final String COUNTY_TABLE = "County";

    private static final String PARENT_NAME_FIELD = "NAME";

    private static final String[] REQUEST_FIELDS = new String[] {
            ISSUE_TIME_FIELD, START_TIME_FIELD, END_TIME_FIELD, UGC_ZONE_FIELD,
            PHEN_SIG_FIELD, END_TIME_FIELD, ACTION, ETN };

    private GeospatialData[] countyGeoData;

    private WarngenLayer warngenLayer;

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

            DbQueryRequest request = buildRequest(simulatedTime,
                    phenSigConstraint.toString(), warngenLayer.getAllUgcs(),
                    entityClass);
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
                    System.out.println("create watch area buffer time: "
                            + (System.currentTimeMillis() - t0));
                    Set<String> validUgcZones = warngenLayer
                            .getUgcsForWatches(watchArea);
                    watches = processRecords(records, validUgcZones);
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
                new RequestConstraint(TimeUtil.formatDate(startConstraintTime),
                        ConstraintType.LESS_THAN_EQUALS));
        request.addConstraint(END_TIME_FIELD,
                new RequestConstraint(TimeUtil.formatDate(simulatedTime),
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
        request.addConstraint("ugcZone", ugcConstraint);

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
        for (Map<String, Object> result : response.getResults()) {
            WarningAction action = WarningAction.valueOf(String.valueOf(result
                    .get(ACTION)));
            /*
             * TODO: Currently limited to filtering out one of ("CAN","EXP").
             * Could use "Act" in addition to "act", but this should really be
             * fixed the underlying system. request.addConstraint("act", new
             * RequestConstraint("CAN", ConstraintType.NOT_EQUALS));
             */
            if (action != WarningAction.CAN || action != WarningAction.EXP) {
                ActiveTableRecord record = entityClass.newInstance();
                record.setIssueTime((Calendar) result.get(ISSUE_TIME_FIELD));
                record.setStartTime((Calendar) result.get(START_TIME_FIELD));
                record.setEndTime((Calendar) result.get(END_TIME_FIELD));
                record.setEndTime((Calendar) result.get(END_TIME_FIELD));
                record.setUgcZone(String.valueOf(result.get(UGC_ZONE_FIELD)));
                record.setPhensig(String.valueOf(result.get(PHEN_SIG_FIELD)));
                record.setEtn(String.valueOf(result.get(ETN)));
                record.setAct(String.valueOf(result.get(ACTION)));
                records.add(record);
            }
        }

        return records;
    }

    /**
     * Groups the activeTableRecords into Watch objects that share phenSig,
     * action, ETN, start time, and end time. It also determines the part of
     * state the watch covers.
     * 
     * @param activeTableRecords
     * @param validUgcZones
     * 
     * @return
     */
    private List<Watch> processRecords(
            List<ActiveTableRecord> activeTableRecords,
            Set<String> validUgcZones) {
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
            String state = getStateName(ugcZone.substring(0, 2));
            String action = ar.getAct();
            String phenSig = ar.getPhensig();
            String etn = ar.getEtn();
            Date startTime = ar.getStartTime().getTime();
            Date endTime = ar.getEndTime().getTime();

            if (validUgcZones.contains(ugcZone)) {
                Watch watch = new Watch(state, action, phenSig, etn, startTime,
                        endTime);
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
            List<String> partOfState = new ArrayList<String>(
                    determineAffectedPortions(watch.getAreas()));
            watch.setPartOfState(partOfState);
            watches.add(watch);
        }

        // Sorts the watches based on state name.
        Collections.sort(watches, new Comparator<Watch>() {

            @Override
            public int compare(Watch watch1, Watch watch2) {
                return watch1.getState().compareTo(watch2.getState());
            }
        });

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
            if (nnn + sss + eee + www == 3) {
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
            if (m == 1 + cc) {
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

}
