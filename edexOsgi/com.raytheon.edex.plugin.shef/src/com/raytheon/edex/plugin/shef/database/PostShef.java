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
package com.raytheon.edex.plugin.shef.database;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.TimeZone;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.raytheon.edex.plugin.shef.data.ShefData;
import com.raytheon.edex.plugin.shef.data.ShefRecord;
import com.raytheon.edex.plugin.shef.data.ShefRecord.ShefType;
import com.raytheon.edex.plugin.shef.database.PostShefUtil.QualityControlCode;
import com.raytheon.edex.plugin.shef.util.ShefAdjustFactor;
import com.raytheon.edex.plugin.shef.util.ShefParm;
import com.raytheon.edex.plugin.shef.util.ShefStats;
import com.raytheon.edex.plugin.shef.util.ShefUtil;
import com.raytheon.edex.plugin.shef.util.StoreDisposition;
import com.raytheon.uf.common.dataplugin.persist.PersistableDataObject;
import com.raytheon.uf.common.dataplugin.shef.tables.Alertalarmval;
import com.raytheon.uf.common.dataplugin.shef.tables.AlertalarmvalId;
import com.raytheon.uf.common.dataplugin.shef.tables.Arealfcst;
import com.raytheon.uf.common.dataplugin.shef.tables.ArealfcstId;
import com.raytheon.uf.common.dataplugin.shef.tables.Arealobs;
import com.raytheon.uf.common.dataplugin.shef.tables.ArealobsId;
import com.raytheon.uf.common.dataplugin.shef.tables.Commentvalue;
import com.raytheon.uf.common.dataplugin.shef.tables.CommentvalueId;
import com.raytheon.uf.common.dataplugin.shef.tables.Contingencyvalue;
import com.raytheon.uf.common.dataplugin.shef.tables.ContingencyvalueId;
import com.raytheon.uf.common.dataplugin.shef.tables.Fcstheight;
import com.raytheon.uf.common.dataplugin.shef.tables.FcstheightId;
import com.raytheon.uf.common.dataplugin.shef.tables.Ingestfilter;
import com.raytheon.uf.common.dataplugin.shef.tables.IngestfilterId;
import com.raytheon.uf.common.dataplugin.shef.tables.Procvalue;
import com.raytheon.uf.common.dataplugin.shef.tables.ProcvalueId;
import com.raytheon.uf.common.dataplugin.shef.tables.Productlink;
import com.raytheon.uf.common.dataplugin.shef.tables.ProductlinkId;
import com.raytheon.uf.common.dataplugin.shef.tables.Rejecteddata;
import com.raytheon.uf.common.dataplugin.shef.tables.RejecteddataId;
import com.raytheon.uf.common.dataplugin.shef.tables.Stnclass;
import com.raytheon.uf.common.dataplugin.shef.tables.Unkstn;
import com.raytheon.uf.common.dataplugin.shef.tables.Unkstnvalue;
import com.raytheon.uf.common.dataplugin.shef.tables.UnkstnvalueId;
import com.raytheon.uf.common.dataplugin.shef.util.ParameterCode;
import com.raytheon.uf.common.dataplugin.shef.util.ParameterCode.DataType;
import com.raytheon.uf.common.dataplugin.shef.util.ParameterCode.Duration;
import com.raytheon.uf.common.dataplugin.shef.util.ParameterCode.Extremum;
import com.raytheon.uf.common.dataplugin.shef.util.ParameterCode.PhysicalElement;
import com.raytheon.uf.common.dataplugin.shef.util.ParameterCode.PhysicalElementCategory;
import com.raytheon.uf.common.dataplugin.shef.util.ParameterCode.TypeSource;
import com.raytheon.uf.common.dataplugin.shef.util.ShefConstants;
import com.raytheon.uf.common.dataplugin.shef.util.ShefConstants.IngestSwitch;
import com.raytheon.uf.common.dataplugin.shef.util.ShefQC;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.query.DatabaseQuery;
import com.raytheon.uf.edex.decodertools.time.TimeTools;

/**
 * Post the SHEF Data to the IHFS DB.
 *
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 04/21/2008   387        M. Duff     Initial Version.
 * 06/02/2008   1166       M. Duff     Added checks for null data objects.
 * 22Jul2008    1277       MW Fegan    Use CoreDao in checkIngest().
 * 10/16/2008   1548       jelkins     Integrated ParameterCode Types and misc fixes
 * 12/17/2008   1786       J. Sanchez  Replaced appDefaults.getToken with the default parameter
 * 01/07/2009   1722       J. Sanchez  Updated the tableName passed to postData methods
 *                                      Updated post method to increment validTime.
 * 01/08/2009   1846       J. Sanchez  Updated post method to allow B type records.
 *                                      Used dataValue as one of the params for populateDataObj
 * 01/13/2009   1802       askripsk    Fixed the reject_type for rejecteddata.
 * 01/26/2009   1927       J. Sanchez  Removed 'final' from postDate.
 * 02/02/2009   1943       J. Sanchez  Post fcst data to riverstatus table.
 *                                      Changed default value for alertAlarm
 * 05/28/2009   2410       J. Sanchez  Posted data for unknstnvalue.
 * 12/11/2009   2488       M. Duff     Fixed problem with storing text products.
 * 03/07/2013   15545      w. kwock    Added Observe time to log
 * 03/21/2013   15967      w. kwock    Fix the error in buildTsFcstRiv riverstatus table issue
 * 04/05/2013   16036      w. kwock    Fixed no ts=RZ in ingestfilter table but posted to height table
 * 10/28/2013   16711      lbousaidi   if the id is not in location table,but defined in geoarea table
 *                                     data can be posted to appropriate pe-based tables only if the data
 *                                     type is not READING like in A1 code.
 * 02/18/2014   16572      l. Bousaidi only apply adjust factor to non missing values.
 * 04/24/2014   16904      lbousaidi   gross check should be applied to adjusted value.
 * 04/29/2014   3088       mpduff      Change logging class, clean up/optimization.
 *                                     Updated with more performance fixes.
 * May 14, 2014 2536       bclement    removed TimeTools usage
 * 05/28/2014   3222       mpduff      Fix posting time to be processed time so db doesn't show all post times the same
 * 06/02/2014              mpduff      Fix for caching of range checks.
 * 06/26/2014   3321       mpduff      Fix ingestSwitchMap checks
 * 07/10/2014   3370       mpduff      Fix update/insert issue for riverstatus
 * 07/14/2014              mpduff      Fix data range checks
 * 08/05/2014   15671      snaples     Fixed check for posting when not found in ingestfilter and token is set for load_shef_ingest
 * 09/03/2014              mpduff      Fixed river status table updates.
 * 09/12/2014              mpduff      Fix for shef_load_ingest token
 * 09/18/2014   3627       mapeters    Updated deprecated {@link TimeTools} usage.
 * 06/26/2015   17420      xwei        Fix for : Application fails to act on defined locdatalimit range for parameter element SW
 * Aug 05, 2015 4486       rjpeter     Changed Timestamp to Date.
 * Dec 16, 2015 5166       kbisanz     Update logging to use SLF4J
 * 07/26/2016   19001      ksteinfeld  Remove a restriction that prevents the calling of the method checkIngest() when ShefData
 *                                     object data type is CONTINGENCY.
 * 07/07/2017   6344       mapeters    Remove alarm/alert data when overridden by new data
 * 01/10/2018   5049       mduff       ShefParm is now provided to this class.
 * 01/16/2018   6561       mduff       Added PEDTSEP to log statements.
 * 01/23/2018   6784       mduff       Changed check for missing value to use a numeric rather than a string.
 *                                     Refactored shef missing data var name.
 * 04/18/2018   DCS19644   jwu         Add column 'ts' (Type-Source) in locdatalimits.
 * 05/02/2018   7128       mduff       Moved adjust logging inside the block so it only logs when actually adjusting.
 * 07/12/2018   6991       mduff       Refactored out some db query methods, pass in any db classes.
 * 07/17/2018   7003       dgilling    Check obsTime against shef_winfuture and
 *                                     shef_winpast thresholds for processed data.
 * Sep 23, 2021 8608       mapeters    Handle PDO.traceId changes
 * </pre>
 *
 * @author mduff
 */
public class PostShef {
    /** The logger */
    private static final IUFStatusHandler log = UFStatus
            .getHandler(PostShef.class);

    /**
     * Location Enum
     */
    public enum Location {
        LOC_LOCATION, LOC_GEOAREA, LOC_NO_POST, LOC_UNDEFINED
    }

    /** Log entry separator */
    private static final String LOG_SEP = "========================================";

    /** Q code pattern */
    private static final Pattern Q_CODES = Pattern.compile("Q[^BEF]");

    /** Constant for ON */
    private static final String SHEF_ON = "ON";

    private static final int MISSING = -999;

    /** Questionable/bad threshold value */
    private static final int QUESTIONABLE_BAD_THRESHOLD = 1_073_741_824;

    /** Thread safe database date formatter */
    private final ThreadLocal<SimpleDateFormat> dbFormat = new ThreadLocal<SimpleDateFormat>() {
        @Override
        protected SimpleDateFormat initialValue() {
            SimpleDateFormat sdf = new SimpleDateFormat(
                    ShefConstants.POSTGRES_DATE_STRING);
            sdf.setTimeZone(TimeZone.getTimeZone("GMT"));
            return sdf;
        }
    };

    /** Instance of DAO object */
    private CoreDao dao;

    /** SHEF product id */
    private String prodId = null;

    /** SHEF product time */
    private Date prodTime = null;

    /** db posting time */
    private final Date postDate;

    /** SHEF data record */
    private ShefRecord shefRecord = null;

    /** SHEF decoder statistics object */
    private final ShefStats stats = new ShefStats();

    /** SHEF alert/alarm value */
    private int alertAlarm = ShefConstants.NO_ALERTALARM;

    /** AppsDefaults instance */
    private AppsDefaults appDefaults;

    /** Default basis hrs */
    private long basishrs = 72;

    /** Map of location identifiers to Location Objects */
    private final Map<String, Location> idLocations = new HashMap<>();

    /** number of milliseconds back for data to be considered valid */
    private java.time.Duration lookBackDataDuration;

    /** number of milliseconds forward for data to be considered valid */
    private java.time.Duration lookFwdDataDuration;

    /** Location DAO object */
    private CoreDao locDao;

    /** Instance of PostTables class */
    private final PostTables postTables;

    /** Map of adjustment factors for eacy data type */
    private final Map<String, ShefAdjustFactor> adjustmentMap = new HashMap<>();

    /** Map of location identifier to IngestSwitch */
    private final Map<IngestfilterId, IngestSwitch> ingestSwitchMap = new HashMap<>();

    // AppsDefaults tokens
    private String undefStation;

    private String shefPostDuplicate = null;

    private String shefPostDuplicateDef;

    private boolean shefAlertAlarm;

    private boolean locMess;

    private boolean postLink;

    private String postLatest = "";

    private boolean loadMaxFcst;

    private boolean postBadData;

    private boolean loadIngest;

    private boolean procObs;

    private boolean dataLog;

    private boolean perfLog;

    /** Type Source list */
    private final List<String> tsList = new ArrayList<>();

    /** Use latest value flag */
    private int useLatest = MISSING;

    private final Date basisTimeAnsi = new Date(System.currentTimeMillis()
            - (basishrs * ShefConstants.MILLIS_PER_HOUR));

    /** Type Source to use */
    private String useTs = null;

    /** basis time values from query */
    private Object[] basisTimeValues = null;

    /** Previous forecast query */
    private String previousQueryForecast;

    /** Forecast query results */
    private Object[] queryForecastResults;

    /** Cache of data limits and loc data limits */
    private final Map<String, ShefRangeData> dataRangeMap = new HashMap<>();

    /** Valid date range flag */
    private boolean validDateRange = false;

    private ShefParm shefParm;

    private HydroDataAccessor hydroDataAccessor;

    /**
     *
     * @param date
     * @param locDao
     * @param dao
     */
    public PostShef(Date date, ShefParm shefParm, AppsDefaults appsDefaults,
            HydroDataAccessor hydroDataAccessor, CoreDao dao, CoreDao locDao,
            PostTables postTables) {
        postDate = date;
        this.shefParm = shefParm;
        this.appDefaults = appsDefaults;
        this.hydroDataAccessor = hydroDataAccessor;
        this.dao = dao;
        this.locDao = locDao;
        this.postTables = postTables;
        getAppsDefaults();
    }

    private void getAppsDefaults() {
        undefStation = appDefaults.getToken(ShefConstants.SHEF_POST_UNKNOWN,
                ShefConstants.NONE);

        shefPostDuplicate = appDefaults.getToken(ShefConstants.SHEF_DUPLICATE);

        shefPostDuplicateDef = appDefaults.getToken(
                ShefConstants.SHEF_DUPLICATE, ShefConstants.IF_DIFFERENT);

        shefAlertAlarm = appDefaults.getBoolean(ShefConstants.SHEF_ALERTALARM,
                false);

        locMess = appDefaults.getBoolean(ShefConstants.LOCMESS, false);

        int lookBackDays = appDefaults.getInt(ShefConstants.SHEF_WINPAST, 10);
        lookBackDataDuration = java.time.Duration.ofDays(lookBackDays);

        int lookAheadMinutes = appDefaults.getInt(ShefConstants.SHEF_WINFUTURE,
                30);
        lookFwdDataDuration = java.time.Duration.ofMinutes(-lookAheadMinutes);

        postLink = appDefaults.getBoolean(ShefConstants.SHEF_POST_LINK, false);

        postLatest = appDefaults.getToken(ShefConstants.SHEF_POST_LATEST);

        loadMaxFcst = appDefaults
                .getToken(ShefConstants.SHEF_LOAD_MAXFCST, SHEF_ON)
                .equals(SHEF_ON);

        postBadData = "REJECT".equalsIgnoreCase(appDefaults
                .getToken(ShefConstants.SHEF_POST_BADDATA, "REJECT"));

        String basis_hours_str = appDefaults
                .getToken(ShefConstants.BASIS_HOURS_FILTER);
        try {
            if (basis_hours_str != null) {
                basishrs = Integer.parseInt(basis_hours_str);
            }
        } catch (NumberFormatException e) {
            log.info(ShefConstants.BASIS_HOURS_FILTER
                    + " invalid, using default value of 72", e);
        }

        loadIngest = appDefaults.getBoolean(ShefConstants.SHEF_LOAD_INGEST,
                false);

        procObs = appDefaults.getBoolean(ShefConstants.SHEF_PROCOBS, false);

        dataLog = appDefaults.getBoolean(ShefConstants.SHEF_DATA_LOG, false);
        // TODO need to implement this token and the performance logging
        perfLog = appDefaults.getBoolean(ShefConstants.SHEF_PERFLOG, false);
    }

    /**
     * Builds the DB Objects for storage in the IHFS
     *
     * The following tokens are used:
     *
     * location.ipost: <= 0 - no post > 0 - post to appropriate table
     * ingestfilter.ingest: T - process this PEDTSE F - do not process this
     * PEDTSE post_unk : 0 - ignore unknowns completely 1 - store station ids
     * only for unknowns 2 - store full data record for unknowns post_latest : 0
     * - don't post to latest 1 - post latest data regardless of quality check
     * or missing status 2 - post latest data only if passed quality check and
     * not msg 3 - post latest data only if passed quality check or msg proc_obs
     * : 0 - treat Processed data as Processed 1 - treat Processed data as
     * Observed
     *
     * @param shefRecord
     *            - The ShefRecord object containing all the data
     */
    public void post(ShefRecord shefRecord) {
        this.shefRecord = shefRecord;

        /* Make sure we have data, else return */
        if (shefRecord == null) {
            log.info("Not Posted:Report is null");
            return;
        }

        List<ShefData> dataValues = shefRecord.getDataValues();
        /* Make sure we have data, else return */
        if (dataValues == null) {
            log.info("Not Posted:No data records in decoded data");
            return;
        }

        long start;
        long end;

        String identifier = shefRecord.getIdentifier();
        if (identifier == null) {
            identifier = "MSGPRODID";
        }

        prodId = identifier;

        String locId = shefRecord.getLocationId();
        if (locId == null) {
            // Check for bad data
            log.warn("No data stored for " + prodId);
            return;
        }

        prodTime = shefRecord.getProductTime();

        try {

            /*
             * check to see whether this location should be posted. it checks if
             * the "location" is defined in the location table; also allow for
             * "locations" (i.e. areas) to be defined in the GeoArea table. this
             * if for data for counties, basins, etc.
             */
            Location postLocData = null;
            for (ShefData data : dataValues) {
                if (data.getObsTime() == null) {
                    log.error(data.toString());
                    log.error(
                            "Not posted:Record does not contain an observation time");
                    return;
                }

                postDate.setTime(
                        getToNearestSecond(TimeUtil.currentTimeMillis()));
                boolean same_lid_product = false;

                String dataValue = data.getStringValue();

                if (ShefConstants.SHEF_SKIPPED.equals(dataValue)) {
                    continue;
                } else if (data.isMissing()) {
                    dataValue = ShefConstants.SHEF_MISSING_STR;
                }

                // Per A1 code - set the creation date to Date(0) if missing.
                Date basis = data.getCreationDateObj();
                if (basis == null) {
                    Date d = new Date(0);
                    data.setCreationDateObj(d);
                    data.setCreationDate("1970-01-01 00:00:00");
                }

                locId = data.getLocationId();
                String key = locId + prodId + data.getObservationTime();
                if (idLocations.containsKey(key)) {
                    postLocData = idLocations.get(key);
                    same_lid_product = true;
                } else {
                    postLocData = hydroDataAccessor.checkLocation(locId,
                            shefRecord.getTraceId());
                    idLocations.put(key, postLocData);
                    same_lid_product = false;
                }

                if (dataLog) {
                    log.info(LOG_SEP);
                    log.info("Posting process started for LID [" + locId
                            + "] PEDTSEP [" + data.getPeDTsEP() + "] value ["
                            + dataValue + "]");
                }

                /*
                 * determine the type of data this is, based on the type-source
                 * code. areal data is separated from the point data. note that
                 * processed data can be labeled as observed data!!! also note
                 * that any type-sources which are not R,F, or C are assumed to
                 * be processed. This includes the numbered type-source codes.
                 */
                String dataQualifier = data.getQualifier();
                TypeSource typeSource = data.getTypeSource();

                if ((typeSource == null)
                        || (typeSource == TypeSource.UNKNOWN)) {
                    log.error("Unknown typesource code in data [" + data + "]");
                    continue;
                }

                /*
                 * Don't use the TypeSource directly because there are some
                 * cases where the "type" defaults.
                 */
                DataType dataType = ParameterCode.DataType
                        .getDataType(typeSource, procObs);

                /*
                 * if the station_id exists in location table and the data type
                 * is READING then the data doesn't get posted to the
                 * appropriate pe-based tables to match A1 logic. DR16711
                 */
                if ((DataType.READING.equals(dataType))
                        && (Location.LOC_GEOAREA.equals(postLocData))) {
                    postLocData = Location.LOC_UNDEFINED;
                }

                /*
                 * if location not defined, issue message and save the data if
                 * appropriate. now dispense of the unknown data in the
                 * appropriate manner. note for unknown data, any comments
                 * specified are not stored. also note, for unknown station
                 * data, don't bother posting if the data has not changed.
                 */
                StoreDisposition disposition = StoreDisposition.NONE;
                if (Location.LOC_UNDEFINED.equals(postLocData)) {
                    // Do logging here
                    if (locMess && !same_lid_product) {
                        StringBuilder sMsg = new StringBuilder();
                        if (ShefConstants.NONE.equalsIgnoreCase(undefStation)) {
                            sMsg.append("LID [").append(locId)
                                    .append("] not defined; no data posted");
                        } else if ("IDS_ONLY".equalsIgnoreCase(undefStation)) {
                            sMsg.append("LID [").append(locId).append(
                                    "] not defined; station info posting to UnkStn");
                        } else if ("IDS_AND_DATA"
                                .equalsIgnoreCase(undefStation)) {
                            sMsg.append("LID [").append(
                                    "] not defined; data posting to UnkStnValue");
                        }
                        if (sMsg.length() > 0) {
                            log.info(sMsg.toString());
                        }
                    }

                    // Only post an unknown once!
                    if ("IDS_ONLY".equalsIgnoreCase(undefStation)) {
                        if (!same_lid_product) {
                            Unkstn unknown = new Unkstn();
                            unknown.setIdentifier(identifier);
                            unknown.setLid(locId);
                            unknown.setPostingtime(postDate);
                            unknown.setProductId(prodId);
                            unknown.setProducttime(prodTime);
                            unknown.setSourceTraceId(shefRecord.getTraceId());
                            start = System.currentTimeMillis();
                            postTables.postUnknownStation(unknown, stats);
                            end = System.currentTimeMillis();
                            stats.addElapsedTimeUnknown(end - start);
                            stats.incrementUnknownStation();
                            disposition = StoreDisposition.UKN_STN_POSTING;
                        } else {
                            disposition = StoreDisposition.UKN_STN_POSTED;
                        }
                    } else if ("IDS_AND_DATA".equalsIgnoreCase(undefStation)) {
                        PersistableDataObject unknstnvalue = populateDataObj(
                                shefRecord, data, locId,
                                ShefConstants.UNKNOWN_STATION_VALUE, dataValue,
                                dataQualifier, 0);

                        start = System.currentTimeMillis();
                        postTables.postData(unknstnvalue,
                                ShefConstants.UNKNOWN_STATION_VALUE,
                                shefPostDuplicateDef, stats);
                        end = System.currentTimeMillis();
                        stats.incrementUnknownStation();
                        stats.addElapsedTimeUnknown(end - start);
                        disposition = StoreDisposition.UKN_STN_VALUE;
                    }
                    stats.incrementWarningMessages();
                } else if (Location.LOC_NO_POST.equals(postLocData)) {
                    stats.incrementNoPost();
                    // if the location is defined but was set to not post, then
                    // write message indicating this, if one hasn't been written
                    // already
                    if (locMess && !same_lid_product) {
                        log.info("Station [" + locId + "] is inactive");
                    }
                    disposition = StoreDisposition.INACTIVE_LID;
                }

                /*
                 * check if an lid-PEDTSE entry exists in the IngestFilter. this
                 * function can self-populate the IngestFilter table; if not in
                 * self-populate mode, then an error message is issued if there
                 * is no entry in the IngestFilter table and the data will not
                 * be posted.
                 */
                ShefConstants.IngestSwitch ingestSwitch = ShefConstants.IngestSwitch.POST_PE_ONLY;
                if (Location.LOC_LOCATION.equals(postLocData)
                        || (Location.LOC_GEOAREA.equals(postLocData))) {
                    ingestSwitch = checkIngest(locId, data);
                    if (ShefConstants.IngestSwitch.POST_PE_OFF
                            .equals(ingestSwitch)) {
                        stats.incrementNoPost();
                    }
                }

                /*
                 * if the location data should not be posted because either: 1)
                 * the location is not defined as a location or an area, or
                 * because the location post switch is off, or 2) the PEDTSE
                 * ingest switch is turned off; then no need to continue
                 */

                boolean postPeOffSwitch = ShefConstants.IngestSwitch.POST_PE_OFF
                        .equals(ingestSwitch);

                if ((!Location.LOC_LOCATION.equals(postLocData)
                        && !Location.LOC_GEOAREA.equals(postLocData))
                        || postPeOffSwitch) {
                    /*
                     * set the prev info for the next pass through this
                     * function. this is info is used for to prevent redundant
                     * messages
                     */
                    StringBuilder unkmsg = new StringBuilder();
                    switch (disposition) {
                    case UKN_STN_POSTING: {
                        unkmsg.append("Posting LID [").append(locId)
                                .append("] to [unkstn]");
                        break;
                    }
                    case UKN_STN_POSTED: {
                        unkmsg.append("LID [").append(locId)
                                .append("] already posted to [unkstn]");
                        break;
                    }
                    case UKN_STN_VALUE: {
                        unkmsg.append("Posting LID [").append(locId)
                                .append("] data [").append(dataValue)
                                .append("] to [unkstnvalue]");
                        break;
                    }
                    case INACTIVE_LID:
                        unkmsg.append("Not posting data [").append(dataValue)
                                .append("] for inactive LID [").append(locId)
                                .append("]");
                        break;
                    default: {
                        unkmsg.append("Not posting data [").append(dataValue)
                                .append("] for LID [").append(locId)
                                .append("] ").append(data.getPeDTsEP());
                        break;
                    }
                    }
                    log.warn(unkmsg.toString());
                    stats.incrementWarningMessages();
                    continue;
                }

                /*---------------------------------------------------------------------*/
                /*
                 * check for observed data too far in past or future if data is
                 * outside of this time window, then do not post. skip this
                 * check if data is monthly data
                 */
                Date obsTime = data.getObsTime().toCalendar().getTime();
                Date createTime = null;
                if (data.getCreateTime() != null) {
                    createTime = data.getCreateTime().toCalendar().getTime();
                }

                if ((DataType.READING == dataType)
                        || (DataType.PROCESSED == dataType)
                        || (DataType.AREAL_PROCESSED == dataType)) {
                    if ((java.time.Duration
                            .between(obsTime.toInstant(), postDate.toInstant())
                            .compareTo(lookBackDataDuration) > 0)
                            && (!Duration._1_MONTH
                                    .equals(data.getDuration()))) {
                        stats.incrementWarningMessages();
                        stats.incrementOutsideWindow();
                        log.warn(
                                locId + " " + data.getObsTime() + " obs time > "
                                        + lookBackDataDuration.abs().toDays()
                                        + " days old; data not posted");
                        continue;
                    } else if (java.time.Duration
                            .between(obsTime.toInstant(), postDate.toInstant())
                            .compareTo(lookFwdDataDuration) < 0) {
                        stats.incrementWarningMessages();
                        stats.incrementOutsideWindow();
                        log.warn(locId + " obs time (" + data.getObsTime()
                                + ") >" + " post time (" + postDate + "); "
                                + lookFwdDataDuration.abs().toMinutes()
                                + " minutes in the future; data not posted");
                        continue;
                    }
                }

                /*
                 * check for forecast basis times that are after the valid time,
                 * issue a warning message if this is the case - basis time is
                 * the creation date and valid time is the obs time
                 */
                if (DataType.FORECAST.equals(dataType)
                        || TypeSource.FORECAST_MEAN_AREAL_DATA
                                .equals(typeSource)) {

                    if (createTime != null) {
                        if (createTime.getTime() > obsTime.getTime()) {
                            stats.incrementWarningMessages();
                            log.warn(locId + " basis time (" + createTime
                                    + ") > valid time (" + obsTime
                                    + "); check encoding");
                        }
                    }
                }
                /*
                 * check to see if an adjustment factor should be applied to the
                 * raw SHEF value coming in and if so adjust that value in the
                 * shefrec structure
                 */
                if (!data.isMissing()) {
                    adjustRawValue(locId, data);
                }

                dataValue = data.getStringValue();
                /*
                 * multiply non-missing values of discharge values and
                 * unspecified height values by 1000 to change units
                 */
                String pe = data.getPhysicalElement().getCode();
                if ((pe != null) && (data.getValue() != -9999)) {
                    Matcher m = Q_CODES.matcher(pe);
                    if (m.matches()) {
                        data.adjustValue(1, 0, 1000.0, 0);
                        dataValue = data.getStringValue();
                    }
                    if ("HZ".equals(pe)) {
                        data.adjustValue(1, 0, 1000.0, 0);
                        dataValue = data.getStringValue();
                    }
                }

                /*---------------------------------------------------------------*/
                /*
                 * post data to the appropriate table(s). for the sake of
                 * uniformity, most of these functions have the same argument
                 * list even though some of the arguments are not used by some
                 * functions
                 *
                 * if instructed, post to the product link table, but only if
                 * the info has changed
                 */
                if (postLink && !same_lid_product) {
                    start = System.currentTimeMillis();
                    // Identifier has been set from the awipsHeader.
                    postProductLink(locId, identifier, obsTime);
                    stats.addElapsedTimeIngest(
                            System.currentTimeMillis() - start);

                    if (dataLog) {
                        log.info("Posted product link [" + identifier
                                + "] for LID [" + locId + "]");
                    }
                }

                /*
                 * Check the quality of the data if observed or forecast. note
                 * the posting may treat processed data as observed, including
                 * this manner.
                 *
                 * the quality_code defined contains information from two
                 * 'sources'. one, the qc checks performed by shef, and two,
                 * certain shef qualifier codes reflect the quality of the data.
                 * use the information in the quality_code field, which is based
                 * on these two sources, to help determine the dispensation of
                 * the value.
                 */
                boolean valueOk = false;
                long qualityCode = MISSING;
                Date validTime = new Date(obsTime.getTime());
                PostShefUtil util = new PostShefUtil();

                /* Don't perform the check if the value is a missing value */
                if (!data.isMissing()) {
                    qualityCode = checkQuality(locId, dataQualifier, dataValue,
                            data);
                    valueOk = util.checkQcCode(QualityControlCode.QC_NOT_FAILED,
                            qualityCode);
                } else {
                    qualityCode = ShefQC.setQcCode(ShefQC.QC_DEFAULT, 0L);
                    valueOk = true;
                }

                /*
                 * only attempt to post to the latestobsvalue table if meets
                 * certain conditions based on settings
                 */
                if (DataType.READING.equals(dataType)) {
                    if (SHEF_ON.equalsIgnoreCase(postLatest)
                            || (ShefConstants.VALID_ONLY
                                    .equalsIgnoreCase(postLatest) && valueOk
                                    && (data.getStringValue() != ShefConstants.SHEF_MISSING_STR))
                            || (ShefConstants.VALID_OR_MISSING
                                    .equalsIgnoreCase(postLatest) && valueOk)) {

                        postTables.postLatestObs(shefRecord, data, locId,
                                data.getStringValue(), data.getQualifier(),
                                qualityCode, prodId, prodTime,
                                shefPostDuplicateDef, stats, postDate);
                        if (dataLog) {
                            log.info("Data [" + dataValue + "] ObsTime["
                                    + data.getObservationTimeObj().toString()
                                    + "] for LID [" + locId
                                    + "] posted to the latestObsValue for PE ["
                                    + data.getPhysicalElement().getCode()
                                    + "]");
                        }
                    }
                }

                /*
                 * if the data is either observed or forecast, or if processed
                 * data is being treated as observed data, then invoke the
                 * procedure to post to the appropriate pe-based table. if data
                 * are bad, then don't post to pe-tables and instead post to
                 * reject data, as per user instructions.
                 */
                switch (dataType) {
                case READING:
                case AREAL_PROCESSED:
                case FORECAST:
                case AREAL_FORECAST: {
                    if (!valueOk && postBadData) {
                        PersistableDataObject rejectValue = populateDataObj(
                                shefRecord, data, locId,
                                ShefConstants.REJECTED_DATA, dataValue,
                                dataQualifier, qualityCode);

                        postTables.postData(rejectValue,
                                ShefConstants.REJECTED_DATA,
                                shefPostDuplicateDef, stats);
                        if (dataLog) {
                            log.info("Posting data [" + dataValue
                                    + "] for LID [" + locId
                                    + "] to rejectedData table");
                        }
                    } else {
                        if (DataType.READING.equals(dataType)
                                || DataType.FORECAST.equals(dataType)) {
                            if (checkIfPaired(data)) {
                                postTables.postPairedData(shefRecord, data,
                                        locId, dataValue, dataQualifier,
                                        qualityCode, prodId, prodTime,
                                        shefPostDuplicateDef, stats, postDate);
                                if (dataLog) {
                                    log.info("Posting data [" + dataValue
                                            + "] for LID [" + locId
                                            + "] to pairedValue table");
                                }
                            } else {
                                postTables.postPeData(shefRecord, data, locId,
                                        data.getStringValue(), dataQualifier,
                                        qualityCode, prodId, prodTime,
                                        shefPostDuplicateDef, ingestSwitch,
                                        stats, validTime, postDate, dataType);
                                if (dataLog) {
                                    log.info("Posting data ["
                                            + data.getStringValue()
                                            + "] for LID [" + locId
                                            + "] for PE ["
                                            + data.getPhysicalElement()
                                                    .getCode()
                                            + "]");
                                }
                            }
                        } else if (DataType.AREAL_PROCESSED.equals(dataType)) {
                            /*
                             * if a value is both areal and paired, then let the
                             * paired characteristic of the data take precedence
                             * over the areal nature of the data, so store the
                             * areal paired data in the pairedvalue table, not
                             * the areal tables.
                             */
                            if (checkIfPaired(data)) {
                                postTables.postPairedData(shefRecord, data,
                                        locId, dataValue, dataQualifier,
                                        qualityCode, prodId, prodTime,
                                        shefPostDuplicateDef, stats, postDate);
                                if (dataLog) {
                                    log.info("Posting areal obs data ["
                                            + dataValue + "] for LID [" + locId
                                            + "] to pairedValue table");
                                }
                            } else {
                                PersistableDataObject arealObs = populateDataObj(
                                        shefRecord, data, locId,
                                        ShefConstants.AREAL_OBS, dataValue,
                                        dataQualifier, qualityCode);
                                postTables.postData(arealObs,
                                        ShefConstants.AREAL_OBS,
                                        shefPostDuplicateDef, stats);

                                if (dataLog) {
                                    log.info("Posting areal obs data ["
                                            + dataValue + "] for LID [" + locId
                                            + "] to arealobs table");
                                }
                            }
                        } else if (DataType.AREAL_FORECAST.equals(dataType)) {
                            if (checkIfPaired(data)) {
                                postTables.postPairedData(shefRecord, data,
                                        locId, dataValue, dataQualifier,
                                        qualityCode, prodId, prodTime,
                                        shefPostDuplicateDef, stats, postDate);
                                if (dataLog) {
                                    log.info("Posting areal forecast data ["
                                            + dataValue + "] for LID [" + locId
                                            + "] to pairedValue table");
                                }
                            } else {
                                PersistableDataObject arealfcst = populateDataObj(
                                        shefRecord, data, locId,
                                        ShefConstants.AREAL_FCST, dataValue,
                                        dataQualifier, qualityCode);
                                postTables.postData(arealfcst,
                                        ShefConstants.AREAL_FCST,
                                        shefPostDuplicateDef, stats);
                                if (dataLog) {
                                    log.info("Posting areal forecast data ["
                                            + dataValue + "] for LID [" + locId
                                            + "] to arealfcst table");
                                }
                            }
                        }
                    }
                    break;
                }
                case CONTINGENCY: {
                    /*
                     * post to the Contingency and Processed tables; unless of
                     * course the PE is one of the special paired elements. note
                     * that we are only posting to the processed tables if not
                     * treating the processed data as observed.
                     */
                    if (checkIfPaired(data)) {
                        postTables.postPairedData(shefRecord, data, locId,
                                dataValue, dataQualifier, qualityCode, prodId,
                                prodTime, shefPostDuplicateDef, stats,
                                postDate);
                        if (dataLog) {
                            log.info("Posting contingency data [" + dataValue
                                    + "] for LID [" + locId
                                    + "] to pairedValue table");
                        }
                    } else {
                        PersistableDataObject contingency = populateDataObj(
                                shefRecord, data, locId,
                                ShefConstants.CONTINGENCY_VALUE, dataValue,
                                dataQualifier, qualityCode);
                        postTables.postData(contingency,
                                ShefConstants.CONTINGENCY_VALUE,
                                shefPostDuplicateDef, stats);
                        if (dataLog) {
                            log.info("Posting contingency data [" + dataValue
                                    + "] for LID [" + locId
                                    + "] to contingencyValue table");
                        }
                    }
                    break;
                }
                case PROCESSED: {
                    if (!valueOk && postBadData) {
                        PersistableDataObject rejectValue = populateDataObj(
                                shefRecord, data, locId,
                                ShefConstants.REJECTED_DATA, dataValue,
                                dataQualifier, qualityCode);

                        postTables.postData(rejectValue,
                                ShefConstants.REJECTED_DATA,
                                shefPostDuplicateDef, stats);
                        if (dataLog) {
                            log.info("Posting data [" + dataValue
                                    + "] for LID [" + locId
                                    + "] to rejectedData table");
                        }
                    } else {
                        if (checkIfPaired(data)) {
                            postTables.postPairedData(shefRecord, data, locId,
                                    dataValue, dataQualifier, qualityCode,
                                    prodId, prodTime, shefPostDuplicateDef,
                                    stats, postDate);
                            if (dataLog) {
                                log.info("Posting processed data [" + dataValue
                                        + "] for LID [" + locId
                                        + "] to pairedValue table");
                            }
                        } else {
                            PersistableDataObject procval = populateDataObj(
                                    shefRecord, data, locId,
                                    ShefConstants.PROC_VALUE, dataValue,
                                    dataQualifier, qualityCode);
                            postTables.postData(procval,
                                    ShefConstants.PROC_VALUE,
                                    shefPostDuplicateDef, stats);
                            if (dataLog) {
                                log.info("Posting processed data [" + dataValue
                                        + "] for LID [" + locId
                                        + "] to procValue table");
                            }
                        }
                    }
                    break;
                }
                case UNKNOWN:
                    break;
                default:
                    break;
                } // switch

                /*
                 * Update alert/alarm data as necessary. Don't update if the
                 * data is a ContingencyValue
                 */
                if (!DataType.CONTINGENCY.equals(dataType) && shefAlertAlarm) {
                    updateAlertAlarm(data, locId, dataValue, dataQualifier,
                            qualityCode);
                }

                /*
                 * now check if there is any comment data associated with this
                 * data. if so, then store in the comment table, where comments
                 * for all datatypes goes.
                 */
                String c = data.getRetainedComment();
                if ((c != null) && (c.length() > 0)) {
                    PersistableDataObject commentValue = populateDataObj(
                            shefRecord, data, locId,
                            ShefConstants.COMMENT_VALUE, dataValue,
                            dataQualifier, qualityCode);

                    postTables.postData(commentValue,
                            ShefConstants.COMMENT_VALUE, shefPostDuplicateDef,
                            stats);
                    if (dataLog) {
                        log.info("Posting comments for data [" + dataValue
                                + "] : LID [" + locId
                                + "] to commentValue table");
                    }
                }
            } // for

            postTables.executeBatchUpdates();

            if (!dataValues.isEmpty()) {
                ShefData data = dataValues.get(0);
                DataType dataType = ParameterCode.DataType
                        .getDataType(data.getTypeSource(), procObs);
                if ((DataType.FORECAST.equals(dataType)) && loadMaxFcst
                        && (data.getPhysicalElement().getCode().startsWith("H")
                                || data.getPhysicalElement().getCode()
                                        .startsWith("Q"))) {
                    postRiverStatus(data, locId);
                    log.info("Update RiverStatus for: " + locId + " "
                            + data.getPhysicalElement().getCode());
                }
            }
        } catch (Exception e) {
            log.error("An error occurred posting shef data.", e);
        }

        // Reset .E cache vars
        tsList.clear();
        useLatest = MISSING;
        useTs = null;
        basisTimeValues = null;
        previousQueryForecast = null;
        dataRangeMap.clear();
        validDateRange = false;
    }

    /**
     * Log the summary stats.
     *
     * @param traceId
     * @param totalTime
     */
    public void logStats(String traceId, long totalTime) {
        if (this.perfLog) {
            Logger perfLog = LoggerFactory.getLogger("ShefPerfLog");
            perfLog.info("********************************");
            perfLog.info("Performance Stats:  " + traceId);
            perfLog.info("Total Elapsed Time (ms): " + totalTime);
            if (prodTime != null) {
                SimpleDateFormat sdf = new SimpleDateFormat("yyMMddHHmmss");
                perfLog.info(prodId + ", " + sdf.format(prodTime));
            } else {
                perfLog.info(prodId + ", ------------");
            }
            logIt(perfLog, stats.getAlertAlarm(), " AlertAlarm");
            logIt(perfLog, stats.getArealFcstOverwrite(),
                    " Areal Fcst Overwrite");
            logIt(perfLog, stats.getArealFcstValues(), " Areal Fcst Values");
            logIt(perfLog, stats.getArealObsOverwrite(),
                    " Areal Obs Overwrite");
            logIt(perfLog, stats.getArealValues(), " Areal Obs Values");
            logIt(perfLog, stats.getCommentOverwrite(), " Comments Overwrite");
            logIt(perfLog, stats.getContingencyOverwrite(),
                    " Contingency Overwrite");
            logIt(perfLog, stats.getContingencyValues(), " Contingency Values");
            logIt(perfLog, stats.getElapsedTimeForecastIngest(),
                    " Elapsed Time (ms) Forecast Ingest");
            logIt(perfLog, stats.getElapsedTimeHeightIngest(),
                    " Elapsed Time (ms) Height Ingest");
            logIt(perfLog, stats.getElapsedTimeOtherIngest(),
                    " Elapsed Time (ms) Other Ingest");
            logIt(perfLog, stats.getElapsedTimePrecipIngest(),
                    " Elapsed Time (ms) Precip Ingest");
            logIt(perfLog, stats.getElapsedTimeUnknown(),
                    " Elapsed Time (ms) Unknown");
            logIt(perfLog, stats.getElapsedTimeIngest(),
                    " Elapsed Time (ms) All Ingest");
            logIt(perfLog, stats.getErrorMessages(), " Error Messages");
            logIt(perfLog, stats.getForecastPe(), " Forecast PE");
            logIt(perfLog, stats.getIgnored(), " Ignored");
            logIt(perfLog, stats.getLatestObs(), " Latest Obs");
            logIt(perfLog, stats.getMaxForecast(), " Max Forecast");
            logIt(perfLog, stats.getNoPost(), " No Post");
            logIt(perfLog, stats.getObsHeight(), " Obs Height");
            logIt(perfLog, stats.getObsPe(), " Obs PE");
            logIt(perfLog, stats.getObsPrecip(), " Obs Precip");
            logIt(perfLog, stats.getOutsideTimeWindow(),
                    " Outside Time Window");
            logIt(perfLog, stats.getPaired(), " Paired");
            logIt(perfLog, stats.getPairedOver(), " Paired Overwrite");
            logIt(perfLog, stats.getPostProcessedOverwrite(),
                    " Post Processd Overwrite");
            logIt(perfLog, stats.getPostProcessedValues(),
                    " Post Processed Values");
            logIt(perfLog, stats.getPrecipGpp(), " Precip GPP");
            logIt(perfLog, stats.getRejected(), " Rejected");
            logIt(perfLog, stats.getRejectedOverwrite(), " Rejected Overwrite");
            logIt(perfLog, stats.getSuccessMessages(), " Success Messages");
            logIt(perfLog, stats.getUnknownNoPost(), " Unknown No Post");
            logIt(perfLog, stats.getUnknownStationValue(), " Unknown Station");
            logIt(perfLog, stats.getUnknownStationValueOverwrite(),
                    " Unknown Station Overwrite");
            logIt(perfLog, stats.getWarningMessages(), " Warning Messages");
        }
    }

    private void logIt(Logger log, long value, String label) {
        if (value > 0) {
            log.info(value + label);
        }
    }

    /**
     * Update the alertalarmval table based on the new shef data.
     *
     * @param data
     * @param locId
     * @param value
     * @param qualifier
     * @param qualityCode
     */
    private void updateAlertAlarm(ShefData data, String locId, String value,
            String qualifier, long qualityCode) {
        String aaCategory = null;
        String aaCheck = null;

        PersistableDataObject<?> dataObj = populateDataObj(shefRecord, data,
                locId, ShefConstants.ALERTALARM_VALUE, data.getStringValue(),
                qualifier, qualityCode);
        Alertalarmval aaVal = (Alertalarmval) dataObj;

        if (alertAlarm == ShefConstants.NO_ALERTALARM) {
            /*
             * If no alert/alarm, delete any matching entries from alertalarmval
             * table since they are no longer valid
             */
            DatabaseQuery dbQuery = new DatabaseQuery(Alertalarmval.class);
            dbQuery.addQueryParam("id.lid", locId);
            dbQuery.addQueryParam("id.pe", aaVal.getId().getPe());
            dbQuery.addQueryParam("id.dur", aaVal.getId().getDur());
            dbQuery.addQueryParam("id.ts", aaVal.getId().getTs());
            dbQuery.addQueryParam("id.extremum", aaVal.getId().getExtremum());
            dbQuery.addQueryParam("id.validtime", aaVal.getId().getValidtime());
            try {
                dao.deleteByCriteria(dbQuery);
            } catch (DataAccessLayerException e) {
                log.error("Failed to delete " + ShefConstants.ALERTALARM_VALUE
                        + " entries matching criteria: " + dbQuery, e);
            }
        } else {
            /*
             * An alert/alarm has occurred, determine what type and post it to
             * the alertalarmval table. If duplicate found and new value should
             * be overwritten, then overwrite the data without saving the
             * replaced data. See post_tables.c: post_alertalarm
             */
            if (alertAlarm == ShefConstants.ALERT_UPPER_DETECTED) {
                aaCategory = ShefConstants.ALERT_CATEGSTR;
                aaCheck = ShefConstants.UPPER_CHECKSTR;
            } else if (alertAlarm == ShefConstants.ALARM_UPPER_DETECTED) {
                aaCategory = ShefConstants.ALARM_CATEGSTR;
                aaCheck = ShefConstants.UPPER_CHECKSTR;
            } else if (alertAlarm == ShefConstants.ALERT_LOWER_DETECTED) {
                aaCategory = ShefConstants.ALERT_CATEGSTR;
                aaCheck = ShefConstants.LOWER_CHECKSTR;
            } else if (alertAlarm == ShefConstants.ALARM_LOWER_DETECTED) {
                aaCategory = ShefConstants.ALARM_CATEGSTR;
                aaCheck = ShefConstants.LOWER_CHECKSTR;
            }

            aaVal.getId().setAaCateg(aaCategory);
            aaVal.getId().setAaCheck(aaCheck);

            postTables.postAAData(aaVal, ShefConstants.ALERTALARM_VALUE,
                    shefPostDuplicate, stats, aaCategory, aaCheck);

            if (dataLog) {
                log.info("Posting data [" + value + "] for LID [" + locId
                        + "] to " + ShefConstants.ALERTALARM_VALUE + " table");
            }
        }
    }

    /**
     * Post data to the riverstatus data table.
     */
    private void postRiverStatus(ShefData data, String locId) {
        long start = 0;

        String tableName = null;
        String pe = data.getPhysicalElement().getCode();

        if (data.getTimeSeriesId() <= ShefConstants.MAXFCST_INFO) {
            start = System.currentTimeMillis();
            if (pe.startsWith("H") || pe.startsWith("h")) {
                tableName = "FcstHeight";
            } else {
                tableName = "FcstDischarge";
            }
            loadMaxFcstData_lidpe(tableName, locId, pe);
        } else {
            /*
             * if there is a large number of reports that fill up the tracking
             * buffer, then process all forecast time series
             */
            loadMaxFcstData("FcstHeight");
            loadMaxFcstData("FcstDischarge");
        }

        if (dataLog) {
            log.info("H/Q lid-pe; updated RiverStatus, runtime = "
                    + (System.currentTimeMillis() - start) + " ms.");
        }

    }

    /**
     * Process forecast data for the given table name. Don't consider any
     * probabilistic values.
     **/
    private void loadMaxFcstData(String tableName) {
        Object[] oa = null;
        String lid = null;
        String pe = null;
        String ts = null;
        String dateStr = dbFormat.get().format(postDate);

        String query = "select lid,pe,ts from " + tableName
                + " where validtime > '" + dateStr + "' and probability < 0.0";

        try {
            oa = dao.executeSQLQuery(query);
            if (oa == null) {
                return;
            }

            Object[] row = null;
            for (Object element : oa) {
                row = (Object[]) element;
                if (row.length == 3) {
                    lid = ShefUtil.getString(row[0], null);
                    pe = ShefUtil.getString(row[1], null);
                    ts = ShefUtil.getString(row[2], null);

                    if ((lid != null) && (pe != null) && (ts != null)) {
                        loadMaxFcstItem(lid, pe, ts);
                    }
                }
            }
        } catch (Exception e) {
            log.error("Query = [" + query + "]");
            log.error(shefRecord.getTraceId()
                    + " - PostgresSQL error retrieving from " + tableName, e);
        }
    }

    /**
     * Process forecast data for the given tableName.
     */
    private void loadMaxFcstData_lidpe(String tableName, String locId,
            String pe) {
        Object[] oa = null;
        if ((tableName != null) && (locId != null) && (pe != null)) {
            if (shefRecord.getShefType() == ShefType.E) {
                // Only need to do this query once for each shef record for .E
                if (tsList.isEmpty()) {
                    String query = "select DISTINCT(ts) " + "from " + tableName
                            + " where lid = '" + locId + "' and pe = '" + pe
                            + "' and " + "validtime > CURRENT_TIMESTAMP and "
                            + "probability < 0.0";
                    try {
                        oa = dao.executeSQLQuery(query);
                        for (Object element : oa) {
                            String ts = ShefUtil.getString(element, null);
                            if (ts != null) {
                                tsList.add(ts);
                            }
                        }
                    } catch (Exception e) {
                        log.error("Query = [" + query + "]");
                        log.error(shefRecord.getTraceId()
                                + " - PostgresSQL error retrieving from "
                                + tableName, e);
                    }
                }
            } else {
                String query = "select DISTINCT(ts) " + "from " + tableName
                        + " where lid = '" + locId + "' and pe = '" + pe
                        + "' and " + "validtime > CURRENT_TIMESTAMP and "
                        + "probability < 0.0";

                try {
                    oa = dao.executeSQLQuery(query);

                    for (Object element : oa) {
                        String ts = ShefUtil.getString(element, null);
                        if (ts != null) {
                            tsList.add(ts);
                        }
                    }

                } catch (Exception e) {
                    log.error("Query = [" + query + "]");
                    log.error(shefRecord.getTraceId()
                            + " - PostgresSQL error retrieving from "
                            + tableName, e);
                }
            }

            for (String ts : tsList) {
                loadMaxFcstItem(locId, pe, ts);
            }
        }
    }

    /**
     * Loads the max fcst info into the RiverStatus table for the current
     * location and pe.
     */
    private void loadMaxFcstItem(String lid, String pe, String ts) {
        Object[] oa = null;
        int qcFilter = 1;
        List<ShefData> shefList = null;

        String riverStatQuery = "select use_latest_fcst from riverstat where lid = '"
                + lid + "'";
        String deleteQuery = "delete from riverstatus  " + "where lid= '" + lid
                + "' and pe= '" + pe + "' and ts= '" + ts + "'";
        if (shefRecord.getShefType() == ShefType.E) {
            if (useLatest == MISSING) {
                useLatest = 0;
                try {
                    oa = dao.executeSQLQuery(riverStatQuery);

                    /*
                     * get the setting for the use_latest_fcst field for the
                     * current location from the riverstat table.
                     */

                    if (oa == null) {
                        useLatest = 1;
                    } else {
                        if (oa.length > 0) {
                            if ("T".equals(ShefUtil.getString(oa[0], null))) {
                                useLatest = 1;
                            }
                        }
                    }
                } catch (Exception e) {
                    log.error("Query = [" + riverStatQuery + "]");
                    log.error(shefRecord.getTraceId()
                            + " - PostgresSQL error loading max forecast item",
                            e);
                }
            }
        } else {
            useLatest = 0;
            try {
                oa = dao.executeSQLQuery(riverStatQuery);

                /*
                 * get the setting for the use_latest_fcst field for the current
                 * location from the riverstat table.
                 */

                if (oa == null) {
                    useLatest = 1;
                } else {
                    if (oa.length > 0) {
                        if ("T".equals(ShefUtil.getString(oa[0], null))) {
                            useLatest = 1;
                        }
                    }
                }
            } catch (Exception e) {
                log.error("Query = [" + riverStatQuery + "]");
                log.error(shefRecord.getTraceId()
                        + " - PostgresSQL error loading max forecast item", e);
            }

        }
        /*
         * get the forecast time series for this location, pe, and ts using any
         * instructions on any type-source to screen and whether to use only the
         * latest basis time
         */
        /*
         * This code sets the time values
         */
        shefList = buildTsFcstRiv(lid, pe, ts, qcFilter, useLatest);
        if (shefList != null && !shefList.isEmpty()) {
            ShefData maxShefDataValue = findMaxFcst(shefList);
            boolean riverStatusUpdateValueFlag = hydroDataAccessor
                    .isUpdateRiverStatus(lid, pe, ts, shefRecord.getTraceId());
            postTables.postRiverStatus(shefRecord, maxShefDataValue,
                    riverStatusUpdateValueFlag);
        } else {
            /*
             * if no data were found, then delete any entries that may exist for
             * this key. this is needed if general applications are using this
             * function directly and delete all forecast data for a given key
             */
            dao.executeSQLUpdate(deleteQuery);
        }
    }

    /**
     *
     * This function assembles a forecast time series for a given location and
     * pe. The data are retrieved for: 1) either the specified type-source or
     * for the type-source defined in the ingest filter as the one to use, based
     * on its rank; and for 2) either all forecast values regardless of basis
     * time or only those forecasts with the latest basis time. 3) for
     * non-probabilistic values only.
     *
     * It returns a times series of values in an array of structures, and also
     * returns the count of values.
     *
     * Presuming that the duration and extremum values in the forecast table
     * never yield duplicates, then there can only be duplicates for the same
     * validtime due to multiple basis times.
     *
     * There is a passed in limit regarding how far in the future data is
     * considered, and how old the forecast (basistime) can be.
     *
     * This function is needed since some locations have short-term forecasts
     * and long-term forecasts, both of which are valid and do not prempt the
     * other. This avoids problems with the previous method where the software
     * always used the forecast with the latest creation time and ignored all
     * other forecasts, for certain purposes.
     *
     * The approach herein does NOT assume that the creation data corresponds to
     * the valid time covered - i.e. it does NOT require that long-term forecast
     * have the latest creation time. The heart of the logic for this function
     * is contained in the adjust_startend() function.
     **/
    private List<ShefData> buildTsFcstRiv(String lid, String pe,
            String tsFilter, int qcFilter, int useLatest) {
        int fcstCount = 0;
        String tableName = null;
        String query = null;
        StringBuilder queryForecast = null;

        boolean[] doKeep = null;

        Fcstheight[] fcstHead = null;
        Fcstheight fcstHght = null;

        List<ShefData> shefList = new ArrayList<>();
        ShefData shefDataValue = null;

        if (shefRecord.getShefType() != ShefType.E) {
            useTs = null;
            basisTimeValues = null;
        }
        if ((tsFilter == null)
                || ((tsFilter.length() == 0) && (useTs == null))) {
            useTs = hydroDataAccessor.getBestTs(lid, pe, "F%", 0,
                    shefRecord.getTraceId());
            if (useTs == null) {
                return null;
            }

        } else {
            useTs = tsFilter;
        }
        try {
            if (pe.startsWith("H") || pe.startsWith("h")) {
                tableName = "FcstHeight";
            } else {
                tableName = "FcstDischarge";
            }
            if (basisTimeValues == null) {
                /*
                 * retrieve a list of unique basis times; use descending sort.
                 * only consider forecast data before some ending time, and with
                 * some limited basis time ago
                 */
                query = "SELECT DISTINCT(basistime) FROM " + tableName + " "
                        + "WHERE lid = '" + lid + "' and " + "pe = '" + pe
                        + "' and " + "ts = '" + useTs + "' and "
                        + "validtime >= CURRENT_TIMESTAMP and "
                        + "basistime >= '" + basisTimeAnsi + "' and "
                        + "value != " + ShefConstants.SHEF_MISSING + " and "
                        + "quality_code >= " + QUESTIONABLE_BAD_THRESHOLD + " "
                        + "ORDER BY basistime DESC ";

                basisTimeValues = dao.executeSQLQuery(query);

                if ((basisTimeValues == null)
                        || (basisTimeValues.length <= 0)) {
                    return null;
                }
            }

            /*
             * retrieve the data; the ordering by validtime is important. as
             * before, limit the forecast time valid time window and as needed,
             * the age of the forecast (basistime).
             */
            queryForecast = new StringBuilder(
                    "SELECT lid,pe,dur,ts,extremum,probability,validtime,basistime,value ");
            queryForecast.append("FROM ").append(tableName)
                    .append(" WHERE lid = '").append(lid);
            queryForecast.append("' AND pe = '").append(pe)
                    .append("' AND ts = '").append(useTs);
            queryForecast.append(
                    "' AND validtime >= CURRENT_TIMESTAMP AND probability < 0.0 AND ");

            if ((useLatest == 1) || ((basisTimeValues != null)
                    && (basisTimeValues.length == 1))) {
                Date tempStamp = null;
                tempStamp = (Date) basisTimeValues[0];
                queryForecast.append("basistime >= '").append(tempStamp)
                        .append("' AND ");
            } else {
                queryForecast.append("basistime >= '").append(basisTimeAnsi)
                        .append("' AND ");

            }
            queryForecast.append("value != ")
                    .append(ShefConstants.SHEF_MISSING_STR)
                    .append(" AND quality_code >= ");
            queryForecast.append(ShefConstants.SHEF_MISSING_STR)
                    .append(" ORDER BY validtime ASC");

            if (!queryForecast.toString().equals(previousQueryForecast)) {
                previousQueryForecast = queryForecast.toString();
                queryForecastResults = dao
                        .executeSQLQuery(queryForecast.toString());
            }

            Object[] row = null;
            if ((queryForecastResults != null)
                    && (queryForecastResults.length > 0)) {
                fcstHead = new Fcstheight[queryForecastResults.length];
                for (int i = 0; i < queryForecastResults.length; i++) {
                    row = (Object[]) queryForecastResults[i];
                    fcstHght = new Fcstheight();
                    FcstheightId id = new FcstheightId();
                    Date tmpDate = null;

                    id.setLid(ShefUtil.getString(row[0], null)); // lid

                    id.setPe(ShefUtil.getString(row[1], null)); // pe

                    id.setDur(ShefUtil.getShort(row[2], (short) 0)); // dur

                    id.setTs(ShefUtil.getString(row[3], null)); // ts

                    id.setExtremum(ShefUtil.getString(row[4], null)); // extremum

                    id.setProbability(ShefUtil.getFloat(row[5], 0.0f));

                    tmpDate = ShefUtil.getDate(row[6], null);
                    id.setValidtime(tmpDate); // valid

                    tmpDate = ShefUtil.getDate(row[7], null);
                    id.setBasistime(tmpDate);// basis

                    fcstHght.setId(id);
                    fcstHght.setValue(ShefUtil.getDouble(row[8], 0.0)); // value
                    fcstHead[i] = fcstHght;
                }
                fcstCount = fcstHead.length;
            }

            /*
             * define a local array to determine which items in the time series
             * to keep and return
             */
            if (fcstCount > 0) {
                doKeep = new boolean[fcstCount];
            } else {
                return null;
            }

            /*
             * if only getting the latest basis time's data or only one basis
             * time was found, then consider all; otherwise, need to adjoin/butt
             * the time series together for the multiple basis times.
             */
            PostShefUtil util = new PostShefUtil();
            if ((useLatest == 1) || (basisTimeValues.length <= 1)) {
                Arrays.fill(doKeep, true);
            } else {
                doKeep = util.setFcstKeep(basisTimeValues, fcstHead);
            }

            /*
             * now load the values and info to return, knowing which items to
             * keep since all the values have been tagged. first get the count
             * of the number of values to keep and allocate the data
             */

            for (int y = 0; y < fcstCount; y++) {
                shefDataValue = new ShefData(shefParm);
                if (doKeep[y]) {
                    shefDataValue.setLocationId(fcstHead[y].getId().getLid());

                    shefDataValue.setPhysicalElement(PhysicalElement
                            .getEnum(fcstHead[y].getId().getPe()));
                    util.convertDur(fcstHead[y].getId().getDur(),
                            shefDataValue);

                    shefDataValue.setTypeSource(
                            TypeSource.getEnum(fcstHead[y].getId().getTs()));

                    shefDataValue.setExtremum(Extremum
                            .getEnum(fcstHead[y].getId().getExtremum()));
                    shefDataValue.setObservationTimeObj(
                            fcstHead[y].getId().getValidtime());
                    shefDataValue.setCreationDateObj(
                            fcstHead[y].getId().getBasistime());
                    shefDataValue.setValue(fcstHead[y].getValue());
                    shefList.add(shefDataValue);
                }
            }
        } catch (Exception e) {
            log.error("Query = [" + query + "]");
            log.error("Query = [" + queryForecast + "]");
            log.error(shefRecord.getTraceId()
                    + " - PostgresSQL error in buildTsFcstRiv", e);
        }
        return shefList;
    }

    /**
     * This gets the max forecast value from a forecast time-series that has
     * already been prepared. This function returns the ts, value, basistime,
     * and validtime for the maximum forecast value.
     */
    private ShefData findMaxFcst(List<ShefData> shefList) {
        double maxValue = -8888.0;
        ShefData maxFcstData = null;

        for (ShefData d : shefList) {
            Double x = d.getValue();
            if (x > maxValue) {
                maxValue = x;
                maxFcstData = d;
            }
        }
        return maxFcstData;
    }

    /**
     * Check whether this lid-PEDTSE combination has an entry in the
     * IngestFilter table which specifies it to be processed. This is checked
     * to:
     *
     * 1) allow issuance of an error message if an entry is not found, and 2) if
     * the load_ingest flag is set, then it will insert an entry into the table.
     *
     * @param locId
     *            - location id
     * @param data
     *            - ShefData
     * @param ingestSwitch
     *            ingest switch setting
     * @return
     */
    private IngestSwitch checkIngest(String locId, ShefData data) {
        StringBuilder errorMsg = new StringBuilder();
        boolean matchFound = false;
        int hNum = 0;
        int pNum = 0;
        int sNum = 0;
        int tNum = 0;
        int qNum = 0;
        int paNum = 0;
        int numPe = 0;
        boolean isOffriv = false;
        boolean isRes = false;
        boolean isRiv = false;
        boolean isPrecip = false;
        boolean isSnow = false;
        boolean isTemp = false;
        boolean isOther = false;
        boolean isUndef = false;
        boolean telemFound = false;
        boolean dcpFound = false;
        boolean obsFound = false;
        boolean fpFound = false;
        boolean resFound = false;
        String telem = null;
        String sql = null;
        Object[] oa = null;

        IngestfilterId key = data.getIngestFilterKey();// .getPeDTsE();

        // Default to off
        ShefConstants.IngestSwitch ingestSwitch = IngestSwitch.POST_PE_OFF;

        try {
            if (!ingestSwitchMap.containsKey(key)) {
                sql = "select lid, pe, dur, ts, extremum, ts_rank, ingest, ofs_input, stg2_input from IngestFilter where lid = '"
                        + locId + "'";
                errorMsg.append("Error requesting IngestFilter data:  " + sql);
                oa = dao.executeSQLQuery(sql);
                if (oa.length > 0) {
                    for (Object element : oa) {
                        Object[] oa2 = (Object[]) element;
                        String pe = ShefUtil.getString(oa2[1], "");
                        int dur = ShefUtil.getInt(oa2[2], -9999);
                        String ts = ShefUtil.getString(oa2[3], "");
                        String extremum = ShefUtil.getString(oa2[4], "");
                        String ingest = ShefUtil.getString(oa2[6], "");
                        String stg2_input = ShefUtil.getString(oa2[8], "");

                        if (pe.equals(data.getPhysicalElement().getCode())
                                && ts.equals(data.getTypeSource().getCode())
                                && extremum.equals(data.getExtremum().getCode())
                                && (dur == data.getDurationValue())) {
                            if ("T".equals(ingest)) {
                                if ("T".equals(stg2_input)) {
                                    ingestSwitch = ShefConstants.IngestSwitch.POST_PE_AND_HOURLY;
                                } else {
                                    ingestSwitch = ShefConstants.IngestSwitch.POST_PE_ONLY;
                                }
                            } else {
                                ingestSwitch = ShefConstants.IngestSwitch.POST_PE_OFF;
                            }
                            ingestSwitchMap.put(key, ingestSwitch);
                            break;
                        }
                    }
                }
            }

            matchFound = ingestSwitchMap.containsKey(key);
            ingestSwitch = ingestSwitchMap.get(key);

            /*
             * if there is no ingest record for this entry, then check if the
             * user options instruct the loading of the ingest info. if the user
             * wishes to automatically load this data PEDTSE information for
             * future use, then do so and allow this data to be processed.
             */
            if (!matchFound && loadIngest) {
                Ingestfilter ingestFilter = new Ingestfilter(
                        new IngestfilterId());
                ingestFilter.getId().setLid(locId);
                ingestFilter.getId().setPe(data.getPhysicalElement().getCode());

                Short n = data.getDurationValue();
                ingestFilter.getId().setDur((n != null) ? n : 0);
                ingestFilter.getId().setTs(data.getTypeSource().getCode());
                ingestFilter.getId().setExtremum(data.getExtremum().getCode());

                /*
                 * set the default values for the new entry. also set the ingest
                 * switch accordingly
                 */
                ingestFilter.setTsRank((short) 1);
                ingestFilter.setIngest("T");
                ingestFilter.setOfsInput("F");

                if ((data
                        .getPhysicalElement() == PhysicalElement.PRECIPITATION_ACCUMULATOR)
                        || (data.getPhysicalElement() == PhysicalElement.PRECIPITATION_INCREMENT)) {
                    ingestSwitch = ShefConstants.IngestSwitch.POST_PE_AND_HOURLY;
                    ingestFilter.setStg2Input("T");
                } else {
                    ingestSwitch = ShefConstants.IngestSwitch.POST_PE_ONLY;
                    ingestFilter.setStg2Input("F");
                }

                /* insert the record */
                errorMsg.setLength(0);
                errorMsg.append(
                        "PostgreSQL error putting data into IngestFilter");
                dao.saveOrUpdate(ingestFilter);

                /*
                 * since the elements defined in Ingest Filter have an impact on
                 * the stations 'class', redetermine the station class for this
                 * location if this id is for a location, and not a geo-area.
                 */

                sql = "select lid, type from telem where lid = '" + locId + "'";
                oa = dao.executeSQLQuery(sql);
                telemFound = false;
                dcpFound = false;
                obsFound = false;
                fpFound = false;
                resFound = false;

                if (oa.length > 0) {
                    telemFound = true;
                    Object[] oa2 = (Object[]) oa[0];
                    telem = (String) oa2[1];
                }

                /* set the sql where clause */
                String where = " where lid = '" + locId + "'";

                if (hydroDataAccessor.recordCount("Dcp", where) > 0) {
                    dcpFound = true;
                }

                if (hydroDataAccessor.recordCount("Observer", where) > 0) {
                    obsFound = true;
                }

                if (hydroDataAccessor.recordCount("RpfFcstPoint", where) > 0) {
                    fpFound = true;
                }

                if (hydroDataAccessor.recordCount("Reservoir", where) > 0) {
                    resFound = true;
                }

                /*
                 * an official forecast point is a station that is defined in
                 * the RpfFcstPoint table. a reservoir include points that have
                 * an entry in the reservoir table
                 */
                isOffriv = fpFound;
                isRes = resFound;

                /* get data elements defined for station */
                errorMsg.setLength(0);
                errorMsg.append("Error getting PE codes from IngestFilter: ")
                        .append(sql);
                sql = "select pe from IngestFilter where lid = '" + locId
                        + "' and ingest = 'T'";
                oa = dao.executeSQLQuery(sql);

                if (oa.length > 0) {
                    String[] sa = new String[oa.length];
                    for (int i = 0; i < oa.length; i++) {
                        sa[i] = ShefUtil.getString(oa[i], "");
                    }
                    hNum = checkPeMatch(sa,
                            PhysicalElementCategory.HEIGHT.getCode());
                    qNum = checkPeMatch(sa,
                            PhysicalElementCategory.DISCHARGE.getCode());

                    sNum = checkPeMatch(sa,
                            PhysicalElementCategory.SNOW.getCode());
                    tNum = checkPeMatch(sa,
                            PhysicalElementCategory.TEMPERATURE.getCode());

                    pNum = checkPeMatch(sa,
                            PhysicalElementCategory.PRECIPITATION.getCode());
                    paNum = checkPeMatch(sa,
                            PhysicalElement.PRESSURE_ATMOSPHERIC.getCode());
                    pNum = pNum - paNum;

                    numPe = sa.length;

                    /*
                     * also, a station is a reservoir if it has a param type of
                     * HP or HT or LS
                     */
                    if ((checkPeMatch(sa,
                            PhysicalElement.ELEVATION_POOL.getCode()) > 0)
                            || (checkPeMatch(sa,
                                    PhysicalElement.ELEVATION_PROJECT_TAIL
                                            .getCode()) > 0)
                            || (checkPeMatch(sa,
                                    PhysicalElement.LAKE_STORAGE_VOLUME
                                            .getCode()) > 0)) {
                        isRes = true;
                    }

                    /*
                     * a station is a river data point if it has an H or Q
                     * parameter and is not considered an official forecast
                     * point or reservoir station
                     */
                    if (!isOffriv && !isRes) {
                        if ((hNum > 0) || (qNum > 0)) {
                            isRiv = true;
                        }
                    }

                    /*
                     * check if the station is a precipitation station, snow, or
                     * temperature station
                     */
                    isPrecip = (pNum > 0);
                    isSnow = (sNum > 0);
                    isTemp = (tNum > 0);

                } else {
                    numPe = 0;
                }

                /* now check the special station classes */
                if ((numPe - (hNum + qNum + pNum + sNum + tNum)) > 0) {
                    isOther = true;
                }

                if (!isOffriv && !isRiv && !isRes && !isPrecip && !isSnow
                        && !isTemp && !isOther) {
                    isUndef = true;
                }

                /*
                 * now with all the information in hand, load the information
                 * into the StnClass table.
                 */
                Stnclass stnClass = new Stnclass();
                StringBuilder sb = new StringBuilder();

                if (isOffriv) {
                    sb.append("F");
                }
                if (isRes) {
                    sb.append("D");
                }
                if (isRiv) {
                    sb.append("R");
                }
                if (isPrecip) {
                    sb.append("P");
                }
                if (isSnow) {
                    sb.append("S");
                }
                if (isTemp) {
                    sb.append("T");
                }
                if (isOther) {
                    sb.append("O");
                }
                if (isUndef) {
                    sb.append("U");
                }
                stnClass.setDispClass(sb.toString());

                stnClass.setDcp((dcpFound) ? "T" : "F");

                if (telemFound) {
                    stnClass.setTelemType(telem);
                } else {
                    stnClass.setTelemType("");
                }

                if (obsFound) {
                    stnClass.setObserver("T");
                } else {
                    stnClass.setObserver("F");
                }

                stnClass.setLid(locId);

                List<String> fields = new ArrayList<>(1);
                List<Object> values = new ArrayList<>(1);
                fields.add("lid");
                values.add(locId);

                List<?> queryResult = locDao.queryByCriteria(fields, values);
                com.raytheon.uf.common.dataplugin.shef.tables.Location loc = null;
                if (!queryResult.isEmpty()) {
                    loc = (com.raytheon.uf.common.dataplugin.shef.tables.Location) queryResult
                            .get(0);
                }
                stnClass.setLocation(loc);
                stnClass.setSourceTraceId(shefRecord.getTraceId());

                errorMsg.setLength(0);
                errorMsg.append("Error on saveOrUpdate stnclass table: ")
                        .append(sql);
                dao.saveOrUpdate(stnClass);
                /* since a record was added, set the match_found variable */
                matchFound = true;
            }
        } catch (Exception e) {
            log.error("Query = [" + sql + "]");
            log.error(shefRecord.getTraceId() + " - " + errorMsg.toString(), e);
            stats.incrementErrorMessages();
        }

        // ***************************************************

        if (!matchFound) {
            log.warn(locId + " - " + data.getPhysicalElement() + "("
                    + data.getDuration() + ")" + data.getTypeSource()
                    + data.getExtremum() + " ingest filter not defined");
            stats.incrementWarningMessages();
            ingestSwitch = ShefConstants.IngestSwitch.POST_PE_OFF;
        }

        return ingestSwitch;
    }

    /**
     * Determines if a PE or PE category code is in the array passed in
     *
     * @param oa
     *            - array to search
     * @param pe
     *            - PE code or PE category code to search for
     * @return - number of matches found in the array
     */
    private int checkPeMatch(String[] sa, String findPeCode) {
        int retVal = 0;
        for (String s : sa) {
            if (s.startsWith(findPeCode)) {
                retVal++;
            }
        }
        return retVal;
    }

    /**
     * Adjust the data value using the adjustfactor table, if matched
     *
     * @param locId
     *            - location id of the record
     * @param data
     *            - data object
     */
    private void adjustRawValue(String locId, ShefData data) {
        String key = locId + data.getPhysicalElement().getCode()
                + data.getDurationValue() + data.getTypeSource().getCode()
                + data.getExtremum().getCode();
        // Check for existing adjust values
        if (!adjustmentMap.containsKey(key)) {
            // need to look up the adjust values
            double divisor = 1.0;
            double base = 0.0;
            double multiplier = 1.0;
            double adder = 0.0;

            StringBuilder sql = new StringBuilder();
            try {
                sql.append(
                        "select divisor, base, multiplier, adder from adjustfactor ");

                sql.append("where lid = '").append(locId)
                        .append("' and pe = '");
                sql.append(data.getPhysicalElement().getCode())
                        .append("' and dur = ");
                sql.append(data.getDurationValue()).append(" and ts = '");
                sql.append(data.getTypeSource().getCode())
                        .append("' and extremum = '");
                sql.append(data.getExtremum().getCode()).append("'");
                Object[] oa = dao.executeSQLQuery(sql.toString());
                if (oa.length > 0) {
                    Object[] oa2 = (Object[]) oa[0];

                    /*
                     * if Correction Factor divisor value is NULL, set it to 1.0
                     */
                    divisor = ShefUtil.getDouble(oa2[0], 1.0);
                    /*
                     * if divisor is ZERO, set it to 1.0, DON'T WANT TO DIVIDE
                     * BY ZERO
                     */
                    if (divisor == 0) {
                        log.warn("Divisor = 0.0 in adjustfactor "
                                + sql.toString());
                        divisor = 1;
                    }
                    base = ShefUtil.getDouble(oa2[1], 0.0);
                    multiplier = ShefUtil.getDouble(oa2[2], 1.0);
                    adder = ShefUtil.getDouble(oa2[3], 0.0);

                    ShefAdjustFactor af = new ShefAdjustFactor(divisor, base,
                            multiplier, adder);
                    adjustmentMap.put(key, af);
                } else {
                    adjustmentMap.put(key, null);
                }
            } catch (Exception e) {
                log.error("Query = [" + sql.toString() + "]");
                log.error(shefRecord.getTraceId()
                        + " - Error adjusting raw value", e);
                return;
            }
        }

        /*
         * calculate adjusted value using an equation similar to HydroMet
         */
        ShefAdjustFactor factor = adjustmentMap.get(key);
        if (factor != null) {
            data.adjustValue(factor.getDivisor(), factor.getBase(),
                    factor.getMultiplier(), factor.getAdder());
            if (dataLog) {
                log.info(
                        locId + " Adjusting Value for " + data.getLocationId());
            }
        }
    }

    /**
     * Insert the data into the table. if entry already exists then do nothing
     *
     * @param locId
     *            - the location id
     * @param productId
     *            - the product id
     * @param obsTime
     *            - The observation time
     */
    private void postProductLink(String locId, String productId, Date obsTime) {
        PersistableDataObject link = null;

        postDate.setTime(getToNearestSecond(TimeUtil.currentTimeMillis()));
        try {
            /* Get a Data Access Object */
            link = new Productlink(
                    new ProductlinkId(locId, productId, obsTime, postDate));

            dao.saveOrUpdate(link);
        } catch (Exception e) {
            log.error(shefRecord.getTraceId()
                    + " - Error writing to productlink table(" + locId + ", "
                    + productId + ", " + obsTime.toString() + ")", e);
        }
    }

    /**
     * Checks the quality of the data
     *
     * Note:
     *
     * When checking a station's qc and alert/alarm limits, this function uses
     * the full set of location limits if they exist in LocDataLimits, even if
     * for example, only the alert/alarm limits are defined and the qc limits
     * are not set. It will NOT attempt to get values for those null limits from
     * the general DataLimits tables.
     *
     * @param lid
     * @param dataQualifier
     * @param dataValue
     * @param data
     * @return qualityCode
     */
    private long checkQuality(String lid, String dataQualifier,
            String dataValue, ShefData data) {
        double missing = ShefConstants.SHEF_MISSING;

        long qualityCode = ShefConstants.DEFAULT_QC_VALUE;
        String monthdaystart = null;
        String monthdayend = null;

        alertAlarm = ShefConstants.NO_ALERTALARM;

        double dValue = 0;

        // if the dataValue = -9999 (missing data)
        if (dataValue.equals(ShefConstants.SHEF_MISSING_STR)) {
            return ShefConstants.QC_MANUAL_FAILED;
        }

        try {
            dValue = Double.parseDouble(dataValue);
        } catch (NumberFormatException e) {
            log.error("Double conversion failed for data value = '" + dataValue
                    + "'", e);
            return ShefConstants.QC_MANUAL_FAILED;
        }

        StringBuilder locLimitSql = new StringBuilder();
        StringBuilder defLimitSql = new StringBuilder();
        String key = lid + data.getPhysicalElement().getCode()
                + data.getDurationValue();
        try {
            if (!dataRangeMap.containsKey(key)) {
                String sqlStart = "select monthdaystart, monthdayend, gross_range_min, gross_range_max, reason_range_min, "
                        + "reason_range_max, roc_max, alert_upper_limit, alert_roc_limit, alarm_upper_limit, "
                        + "alarm_roc_limit, alert_lower_limit, alarm_lower_limit, alert_diff_limit, "
                        + "alarm_diff_limit, pe, dur";

                /*
                 * Grab all local data limits with matching lid/pe/dur.
                 */
                locLimitSql.append(sqlStart);
                locLimitSql.append(", ts from locdatalimits where ");
                locLimitSql.append("lid = '").append(lid).append("' and pe = '")
                        .append(data.getPhysicalElement().getCode())
                        .append("' and dur = ").append(data.getDurationValue());

                Object[] oa = dao.executeSQLQuery(locLimitSql.toString());

                if (oa.length > 0) {
                    /*
                     * Find the local data limit defined for the given TS. If
                     * not find, find the one with default TS ("NA").
                     */
                    List<Object> locLimitList = new ArrayList<>();
                    String dataTS;
                    String limitTS;
                    for (Object element : oa) {
                        Object[] oa1 = (Object[]) element;
                        dataTS = data.getTypeSource().getCode();
                        limitTS = ShefUtil.getString(oa1[17], "");
                        if (dataTS != null && dataTS.equals(limitTS)) {
                            locLimitList.add(element);
                        }
                    }

                    /*
                     * If the local data limit for the given TS cannot be found,
                     * find the default with TS="NA".
                     */
                    if (locLimitList.isEmpty()) {
                        for (Object element : oa) {
                            Object[] oa1 = (Object[]) element;
                            if ("NA".equals(ShefUtil.getString(oa1[17], ""))) {
                                locLimitList.add(element);
                            }
                        }
                    }

                    /*
                     * Note: if nothing defined for the given TS and "NA", this
                     * could still be empty. So default data limits are queried
                     * below.
                     */
                    oa = locLimitList.toArray();
                }

                /*
                 * No local data limit found, use the default data limit.
                 */
                if (oa.length == 0) {

                    if (dataRangeMap.containsKey(key)) {
                        dataRangeMap.remove(key);
                    }

                    // default range
                    defLimitSql = new StringBuilder(sqlStart);
                    defLimitSql.append(" from datalimits where pe = '")
                            .append(data.getPhysicalElement().getCode())
                            .append("' and dur = ")
                            .append(data.getDurationValue());

                    oa = dao.executeSQLQuery(defLimitSql.toString());
                    key = data.getPhysicalElement().getCode()
                            + data.getDurationValue();
                    if (oa.length == 0) {

                        if (dataRangeMap.containsKey(key)) {
                            dataRangeMap.remove(key);
                        }
                    }
                }

                PostShefUtil postShefUtil = new PostShefUtil();
                for (Object element : oa) {
                    Object[] oa2 = (Object[]) element;

                    /* Check the date range */
                    monthdaystart = ShefUtil.getString(oa2[0], "99-99");
                    monthdayend = ShefUtil.getString(oa2[1], "00-00");

                    validDateRange = postShefUtil.checkRangeDate(
                            data.getObservationTimeObj(), monthdaystart,
                            monthdayend);

                    if (validDateRange) {
                        /*
                         * if a range is found, then check the value and set the
                         * flag
                         */
                        ShefRangeData rangeData = new ShefRangeData();
                        rangeData.setGrossRangeMin(
                                ShefUtil.getDouble(oa2[2], missing));
                        rangeData.setGrossRangeMax(
                                ShefUtil.getDouble(oa2[3], missing));
                        rangeData.setReasonRangeMin(
                                ShefUtil.getDouble(oa2[4], missing));
                        rangeData.setReasonRangeMax(
                                ShefUtil.getDouble(oa2[5], missing));
                        rangeData.setAlarmLowerLimit(
                                ShefUtil.getDouble(oa2[12], missing));
                        rangeData.setAlarmUpperLimit(
                                ShefUtil.getDouble(oa2[9], missing));
                        rangeData.setAlertLowerLimit(
                                ShefUtil.getDouble(oa2[11], missing));
                        rangeData.setAlertUpperLimit(
                                ShefUtil.getDouble(oa2[7], missing));
                        this.dataRangeMap.put(key, rangeData);
                        break;
                    }
                }
            }

            ShefRangeData rangeData = dataRangeMap.get(key);
            if (rangeData != null) {
                /*
                 * if a range is found, then check the value and set the flag
                 */
                if (((rangeData.getGrossRangeMin() != missing)
                        && (dValue < rangeData.getGrossRangeMin()))
                        || ((rangeData.getGrossRangeMax() != missing)
                                && (dValue > rangeData.getGrossRangeMax()))) {
                    qualityCode = ShefQC.setQcCode(
                            (int) ShefConstants.QC_GROSSRANGE_FAILED,
                            qualityCode);

                    if (dataLog) {
                        log.info(lid + " failed gross range check: " + dValue
                                + " out of range "
                                + rangeData.getGrossRangeMin() + " - "
                                + rangeData.getGrossRangeMax());
                    }

                    /*
                     * don't do anything if it fails the gross range check
                     */
                } else {
                    if (((rangeData.getReasonRangeMin() != missing)
                            && (dValue < rangeData.getReasonRangeMin()))
                            || ((rangeData.getReasonRangeMax() != missing)
                                    && (dValue > rangeData
                                            .getReasonRangeMax()))) {
                        qualityCode = ShefQC.setQcCode(
                                (int) ShefConstants.QC_REASONRANGE_FAILED,
                                qualityCode);
                        if (dataLog) {
                            log.info(lid + " failed reasonable range check: "
                                    + dValue + " out of range "
                                    + rangeData.getReasonRangeMin() + " - "
                                    + rangeData.getReasonRangeMax());
                        }
                    }

                    /*
                     * if fails alert alarm then set a variable value which will
                     * later trigger a write of the record to AlertAlarmVal
                     * table.
                     */
                    if (shefAlertAlarm) {
                        if ((rangeData.getAlarmUpperLimit() != missing)
                                && (dValue >= rangeData.getAlarmUpperLimit())) {
                            alertAlarm = ShefConstants.ALARM_UPPER_DETECTED;
                        } else if ((rangeData.getAlertUpperLimit() != missing)
                                && (dValue >= rangeData.getAlertUpperLimit())) {
                            alertAlarm = ShefConstants.ALERT_UPPER_DETECTED;
                        } else if ((rangeData.getAlarmLowerLimit() != missing)
                                && (dValue <= rangeData.getAlarmLowerLimit())) {
                            alertAlarm = ShefConstants.ALARM_LOWER_DETECTED;
                        } else if ((rangeData.getAlertLowerLimit() != missing)
                                && (dValue <= rangeData.getAlertLowerLimit())) {
                            alertAlarm = ShefConstants.ALERT_LOWER_DETECTED;
                        }

                        if (dataLog) {
                            if ((alertAlarm >= ShefConstants.ALERT_UPPER_DETECTED)
                                    && (alertAlarm <= ShefConstants.ALARM_LOWER_DETECTED)) {
                                log.info(lid + " AlertAlarm value set: "
                                        + alertAlarm);
                            }
                        }
                    }
                }
            }

            /*
             * lastly, consider any influence that the shef qualifier code may
             * have on the quality code. for qualifiers G and M, do nothing,
             * since the default code embodies these values.
             */
            if ((dataQualifier != null)
                    && !"Z".equalsIgnoreCase(dataQualifier)) {
                if ("Q".equalsIgnoreCase(dataQualifier)
                        || "F".equalsIgnoreCase(dataQualifier)) {
                    qualityCode = ShefQC.setQcCode(
                            (int) ShefConstants.QC_EXTERN_QUEST, qualityCode);
                } else if (("R".equalsIgnoreCase(dataQualifier)
                        || "B".equalsIgnoreCase(dataQualifier))) {
                    qualityCode = ShefQC.setQcCode(
                            (int) ShefConstants.QC_EXTERN_FAILED, qualityCode);
                }
                if (dataLog) {
                    log.info(lid + " Data Qualifier of " + dataQualifier);
                }
            }
        } catch (Exception e) {
            log.info("locdatalimits query = [" + locLimitSql.toString() + "]");
            log.info("datalimits query  = [" + defLimitSql.toString() + "]");
            log.error("Error in checkQuality() for " + shefRecord.getTraceId(),
                    e);
            stats.incrementErrorMessages();
        }

        return qualityCode;
    }

    /**
     * check_if_paired()
     *
     * Check if the SHEF record being processed is for a physical element that
     * has data for a special paired-and-dependent set of data.
     */
    private boolean checkIfPaired(ShefData data) {
        boolean isPaired = false;
        PhysicalElement pe = data.getPhysicalElement();
        if (pe != null) {
            switch (pe) {
            case STAGE_ESTIMATE:
            case DIELECTRIC_CONSTANT:
            case SOIL_SALINITY:
            case SOIL_MOISTURE:
            case WATER_VOLUME:
            case DAM_GATE_OPENING:
            case SNOW_TEMPERATURE:
            case TEMPERATURE_BARE_SOIL_DEPTH:
            case TEMPERATURE_ELEVATION_ABOVE_MSL:
            case TEMPERATURE_VEGETAGED_SOIL_DEPTH: {
                isPaired = true;
            }
            }
        }
        return isPaired;
    }

    /**
     * Check if the data value's time is within the given day-of-the-year
     * window.
     *
     * @param obsTime
     *            - Data time
     * @param monthDayStart
     *            - Valid range start day
     * @param monthDayEnd
     *            - Valid range end day
     * @return - true if the data time is within the range
     */
    private static boolean checkRangeDate(Date obsTime, String monthDayStart,
            String monthDayEnd) {
        boolean valid = false;
        if ((obsTime != null) && (monthDayStart != null)
                && (monthDayEnd != null)) {
            if ((monthDayStart.length() == 5) && (monthDayEnd.length() == 5)) {

                int rangeStartDate = Integer
                        .parseInt(monthDayStart.substring(0, 2)) * 100;
                rangeStartDate += Integer.parseInt(monthDayStart.substring(3));

                int rangeEndDate = Integer.parseInt(monthDayEnd.substring(0, 2))
                        * 100;
                rangeEndDate += Integer.parseInt(monthDayEnd.substring(3));

                Calendar date = TimeUtil.newGmtCalendar(obsTime);

                int dataDate = (date.get(Calendar.MONTH) + 1) * 100;
                dataDate += date.get(Calendar.DAY_OF_MONTH);

                /* Compare the dates, don't check for straddling the year */
                valid = ((dataDate >= rangeStartDate)
                        && (dataDate <= rangeEndDate));
            }
        }
        return valid;
    }

    /**
     * Populates the data object for storage to the database.
     *
     * @param shefRecord
     *            - the shef record to store
     * @param data
     *            - the shef data object to store
     * @param locId
     *            - the location id
     * @param tableName
     *            - the table name to store to
     * @param dataValue
     *            - the value of the data
     * @param qualifier
     *            - the data qualifier
     * @param qualityCode
     *            - the data quality code
     * @return
     */
    private PersistableDataObject populateDataObj(ShefRecord shefRecord,
            ShefData data, String locId, String tableName, String dataValue,
            String qualifier, long qualityCode) {
        PersistableDataObject dataObj = null;
        postDate.setTime(getToNearestSecond(TimeUtil.currentTimeMillis()));

        if (ShefConstants.COMMENT_VALUE.equalsIgnoreCase(tableName)) {
            Commentvalue comment = new Commentvalue(new CommentvalueId());
            Date basisTime = data.getCreationDateObj();
            if (basisTime == null) {
                basisTime = new Date();
            }

            if (dataValue.isEmpty()) {
                dataValue = ShefConstants.SHEF_MISSING_STR;
            }
            short revision = 0;
            if (data.isRevisedRecord()) {
                revision = 1;
            }
            Short n = data.getDurationValue();
            comment.getId().setDur((n != null) ? n : 0);
            comment.getId().setExtremum(data.getExtremum().getCode());
            comment.setIdentifier(data.getLocationId());
            comment.getId().setLid(locId);
            comment.getId().setValidtime(data.getObservationTimeObj());
            comment.getId().setPe(data.getPhysicalElement().getCode());
            comment.setPostingtime(postDate);
            comment.setProductId(prodId);
            comment.setProducttime(prodTime);
            comment.setRevision(revision);
            comment.setShefQualCode(qualifier);
            comment.setSourceTraceId(shefRecord.getTraceId());
            comment.getId().setTs(data.getTypeSource().getCode());
            comment.setValue(Double.parseDouble(dataValue));
            comment.getId()
                    .setProbability((float) data.getProbability().getValue());
            comment.getId().setBasistime(basisTime);
            comment.setShefComment(data.getRetainedComment());
            dataObj = comment;
        } else if (ShefConstants.CONTINGENCY_VALUE
                .equalsIgnoreCase(tableName)) {
            Contingencyvalue contingency = new Contingencyvalue(
                    new ContingencyvalueId());
            Date basisTime = data.getCreationDateObj();
            if (basisTime == null) {
                basisTime = new Date();
            }

            short revision = 0;
            if (shefRecord.isRevisedRecord()) {
                revision = 1;
            }
            Short n = data.getDurationValue();
            contingency.getId().setDur((n != null) ? n : 0);
            contingency.getId().setExtremum(data.getExtremum().getCode());
            contingency.getId().setIdentifier(shefRecord.getIdentifier());
            contingency.getId().setLid(locId);
            contingency.getId().setValidtime(data.getObservationTimeObj());
            contingency.getId().setPe(data.getPhysicalElement().getCode());
            contingency.setPostingtime(postDate);
            contingency.setProductId(prodId);
            contingency.setProducttime(prodTime);
            contingency.setQualityCode((int) qualityCode);
            contingency.setRevision(revision);
            contingency.setShefQualCode(qualifier);
            contingency.setSourceTraceId(shefRecord.getTraceId());
            contingency.getId().setTs(data.getTypeSource().getCode());
            contingency.setValue(Double.parseDouble(dataValue));
            contingency.getId().setProbability(
                    Float.parseFloat(data.getProbability().getValue() + ""));
            contingency.getId().setBasistime(basisTime);
            dataObj = contingency;
        } else if (ShefConstants.PROC_VALUE.equalsIgnoreCase(tableName)) {
            Procvalue proc = new Procvalue(new ProcvalueId());

            short revision = 0;
            if (shefRecord.isRevisedRecord()) {
                revision = 1;
            }
            Short n = data.getDurationValue();
            proc.getId().setDur((n != null) ? n : 0);
            proc.getId().setExtremum(data.getExtremum().getCode());
            proc.getId().setLid(locId);
            proc.getId().setPe(data.getPhysicalElement().getCode());
            proc.setPostingtime(postDate);
            proc.setProductId(prodId);
            proc.setProducttime(prodTime);
            proc.setQualityCode((int) qualityCode);
            proc.setRevision(revision);
            proc.setShefQualCode(qualifier);
            proc.setSourceTraceId(shefRecord.getTraceId());
            proc.getId().setTs(data.getTypeSource().getCode());
            proc.setValue(Double.parseDouble(dataValue));
            proc.getId().setObstime(data.getObservationTimeObj());
            dataObj = proc;
        } else if (ShefConstants.REJECTED_DATA.equalsIgnoreCase(tableName)) {
            Rejecteddata rejectData = new Rejecteddata(new RejecteddataId());
            Date basisTime = data.getCreationDateObj();
            if (basisTime == null) {
                basisTime = new Date();
            }

            short revision = 0;
            if (shefRecord.isRevisedRecord()) {
                revision = 1;
            }
            Short n = data.getDurationValue();
            rejectData.getId().setDur((n != null) ? n : 0);
            rejectData.getId().setExtremum(data.getExtremum().getCode());
            rejectData.setIdentifier(shefRecord.getIdentifier());
            rejectData.getId().setLid(locId);
            rejectData.getId().setValidtime(data.getObservationTimeObj());
            rejectData.getId().setPe(data.getPhysicalElement().getCode());
            rejectData.getId().setPostingtime(postDate);
            rejectData.setProductId(prodId);
            rejectData.setProducttime(prodTime);
            rejectData.setQualityCode((int) qualityCode);
            rejectData.setRevision(revision);
            rejectData.setShefQualCode(qualifier);
            rejectData.setSourceTraceId(shefRecord.getTraceId());
            rejectData.getId().setTs(data.getTypeSource().getCode());
            rejectData.setValue(Double.parseDouble(dataValue));
            rejectData.getId().setProbability(
                    Float.parseFloat(data.getProbability().getValue() + ""));
            rejectData.getId().setBasistime(basisTime);
            rejectData.setRejectType("A");
            dataObj = rejectData;
        } else if (ShefConstants.ALERTALARM_VALUE.equalsIgnoreCase(tableName)) {
            Alertalarmval aa = new Alertalarmval(new AlertalarmvalId());
            Date basisTime = data.getCreationDateObj();
            if (basisTime == null) {
                basisTime = new Date();
            }

            short revision = 0;
            if (shefRecord.isRevisedRecord()) {
                revision = 1;
            }
            Short n = data.getDurationValue();
            aa.getId().setDur((n != null) ? n : 0);
            aa.getId().setExtremum(data.getExtremum().getCode());
            aa.getId().setLid(locId);
            aa.getId().setValidtime(data.getObservationTimeObj());
            aa.getId().setPe(data.getPhysicalElement().getCode());
            aa.setPostingtime(postDate);
            aa.setProductId(prodId);
            aa.setProducttime(prodTime);
            aa.setQualityCode((int) qualityCode);
            aa.setRevision(revision);
            aa.setShefQualCode(qualifier);
            aa.setSourceTraceId(shefRecord.getTraceId());
            aa.getId().setTs(data.getTypeSource().getCode());
            aa.setValue(Double.parseDouble(dataValue));
            aa.getId().setProbability(
                    Float.parseFloat(data.getProbability().getValue() + ""));
            aa.getId().setBasistime(basisTime);
            dataObj = aa;
        } else if (ShefConstants.AREAL_OBS.equalsIgnoreCase(tableName)) {
            Arealobs arealOb = new Arealobs(new ArealobsId());
            Date basisTime = data.getCreationDateObj();
            if (basisTime == null) {
                basisTime = new Date();
            }

            short revision = 0;
            if (shefRecord.isRevisedRecord()) {
                revision = 1;
            }
            Short n = data.getDurationValue();
            arealOb.getId().setDur((n != null) ? n : 0);
            arealOb.getId().setExtremum(data.getExtremum().getCode());
            arealOb.setIdentifier(shefRecord.getIdentifier());
            arealOb.getId().setLid(locId);
            arealOb.getId().setPe(data.getPhysicalElement().getCode());
            arealOb.setPostingtime(postDate);
            arealOb.setProductId(prodId);
            arealOb.setProducttime(prodTime);
            arealOb.setQualityCode((int) qualityCode);
            arealOb.setRevision(revision);
            arealOb.setShefQualCode(qualifier);
            arealOb.setSourceTraceId(shefRecord.getTraceId());
            arealOb.getId().setTs(data.getTypeSource().getCode());
            arealOb.setValue(Double.parseDouble(dataValue));
            arealOb.getId().setObstime(data.getObservationTimeObj());
            dataObj = arealOb;
        } else if (ShefConstants.AREAL_FCST.equalsIgnoreCase(tableName)) {
            Arealfcst arealfcst = new Arealfcst(new ArealfcstId());
            Date basisTime = data.getCreationDateObj();
            if (basisTime == null) {
                basisTime = new Date();
            }

            short revision = 0;
            if (shefRecord.isRevisedRecord()) {
                revision = 1;
            }
            Short n = data.getDurationValue();
            arealfcst.getId().setDur((n != null) ? n : 0);
            arealfcst.getId().setExtremum(data.getExtremum().getCode());
            arealfcst.setIdentifier(shefRecord.getIdentifier());
            arealfcst.getId().setLid(locId);
            arealfcst.getId().setValidtime(data.getObservationTimeObj());
            arealfcst.getId().setPe(data.getPhysicalElement().getCode());
            arealfcst.setPostingtime(postDate);
            arealfcst.setProductId(prodId);
            arealfcst.setProducttime(prodTime);
            arealfcst.setQualityCode((int) qualityCode);
            arealfcst.setRevision(revision);
            arealfcst.setShefQualCode(qualifier);
            arealfcst.setSourceTraceId(shefRecord.getTraceId());
            arealfcst.getId().setTs(data.getTypeSource().getCode());
            arealfcst.setValue(Double.parseDouble(dataValue));
            arealfcst.getId().setProbability(
                    Float.parseFloat(data.getProbability().getValue() + ""));
            arealfcst.getId().setBasistime(basisTime);
            dataObj = arealfcst;
        } else if (ShefConstants.UNKNOWN_STATION_VALUE
                .equalsIgnoreCase(tableName)) {
            Unkstnvalue unkstnvalue = new Unkstnvalue(new UnkstnvalueId());
            Date basisTime = data.getObservationTimeObj();
            if (basisTime == null) {
                basisTime = new Date();
            }

            short revision = 0;
            if (shefRecord.isRevisedRecord()) {
                revision = 1;
            }
            unkstnvalue.getId().setLid(locId);
            unkstnvalue.getId().setPe(data.getPhysicalElement().getCode());
            Short n = data.getDurationValue();
            unkstnvalue.getId().setDur((n != null) ? n : 0);
            unkstnvalue.getId().setTs(data.getTypeSource().getCode());
            unkstnvalue.getId().setExtremum(data.getExtremum().getCode());
            unkstnvalue.getId().setObstime(new Date(basisTime.getTime()));
            unkstnvalue.getId().setValue(Double.parseDouble(dataValue));
            unkstnvalue.setIdentifier(unkstnvalue.getId());
            unkstnvalue.getId().setIdentifier(unkstnvalue.getId());
            unkstnvalue.getId().setRevision(revision);
            unkstnvalue.getId().setShefQualCode("Z");
            unkstnvalue.getId().setProductId(prodId);
            unkstnvalue.getId().setProducttime(prodTime);
            unkstnvalue.getId().setPostingtime(postDate);
            unkstnvalue.setSourceTraceId(shefRecord.getTraceId());

            dataObj = unkstnvalue;
        }

        return dataObj;
    }

    /**
     * Convert the provided millisecond value to the nearest second.
     *
     * @param time
     *            time in milliseconds
     *
     * @return milliseconds rounded to the nearest second.
     */
    private long getToNearestSecond(long time) {
        // Force time to nearest second.
        return time - (time % 1000);
    }

    public void close() {
        postTables.close();
    }
}
