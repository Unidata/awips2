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

import java.sql.Timestamp;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.TimeZone;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.edex.plugin.shef.data.ShefData;
import com.raytheon.edex.plugin.shef.data.ShefRecord;
import com.raytheon.edex.plugin.shef.util.BitUtils;
import com.raytheon.edex.plugin.shef.util.SHEFDate;
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
import com.raytheon.uf.common.dataplugin.shef.util.SHEFTimezone;
import com.raytheon.uf.common.dataplugin.shef.util.ShefConstants;
import com.raytheon.uf.common.dataplugin.shef.util.ShefConstants.IngestSwitch;
import com.raytheon.uf.common.dataplugin.shef.util.ShefQC;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;
import com.raytheon.uf.edex.decodertools.time.TimeTools;

/**
 * Post the SHEF Data to the IHFS DB.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 04/21/2008	387        M. Duff     Initial Version.
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
 * 
 * </pre>
 * 
 * @author mduff
 * @version 1.0
 */
public class PostShef {
    /** The logger */
    private final Log log = LogFactory.getLog(getClass());

    /**
     * Location Enum
     */
    private enum Location {
        LOC_LOCATION, LOC_GEOAREA, LOC_NO_POST, LOC_UNDEFINED
    };

    /**
     * Quality Control Code Enum
     */
    private enum QualityControlCode {
        QC_DEFAULT, QC_GROSSRANGE_FAILED, QC_REASONRANGE_FAILED, QC_ROC_FAILED, QC_ROC_PASSED, QC_OUTLIER_FAILED, QC_OUTLIER_PASSED, QC_SCC_FAILED, QC_SCC_PASSED, QC_MSC_FAILED, QC_MSC_PASSED, QC_EXTERN_FAILED, QC_EXTERN_QUEST, QC_MANUAL_PASSED, QC_MANUAL_QUEST, QC_MANUAL_FAILED, QC_MANUAL_NEW, QC_PASSED, QC_QUESTIONABLE, QC_FAILED, QC_NOT_PASSED, QC_NOT_FAILED
    };

    private static final SimpleDateFormat DB_TIMESTAMP = new SimpleDateFormat(ShefConstants.POSTGRES_DATE_FORMAT.toPattern());
    static {
        DB_TIMESTAMP.setTimeZone(TimeZone.getTimeZone(ShefConstants.GMT));
    }
    
    private static final Pattern Q_CODES = Pattern.compile("Q[^BEF]");
    
    private static final String POST_START_MSG = "Posting process started for LID [%s] PEDTSEP [%s] value [%s]";
        
    private static final String LOV_POST_MSG = "Data [%s] ObsTime[%s] for LID [%s] posted to the latestObsValue for PE [%s]";
    
    private static final String SHEF_ON = "ON";

    private String prevLid = null;

    private String prevProdId = null;

    private Date prevProdTime = null;

    private String prodId = null;

    private Date prodTime = null;

    private Date postDate;

    private ShefRecord shefRecord = null;

    private final ShefStats stats = new ShefStats();

    private int alertAlarm = ShefConstants.NO_ALERTALARM;

    private AppsDefaults appDefaults = AppsDefaults.getInstance();

    private boolean isHoursLoad = false;

    private long obshrs = 72;

    private long fcsthrs = 72;

    private long basishrs = 72;

    // AppsDefaults tokens
    private String undefStation = ShefConstants.NONE;

    private String shefPostDuplicate = null;

    private String shefPostDuplicateDef = "IF_DIFFERENT";

    private boolean shefAlertAlarm = false;

    private boolean locMess = false;

    private int lookBackDays = 10;

    private int lookAheadMinutes = 30;

    private boolean postLink = false;

    private String postLatest = "";

    private boolean loadMaxFcst = false;

    private boolean postBadData = false;

    private String basis_hours_str = null;

    private boolean elgMess = false;

    private boolean loadIngest = false;

    private boolean ingestMess = false;

    private boolean procObs = false;

    private boolean dataLog = false;

    private boolean perfLog = false;

    private boolean archiveMode = false;
    
    private HashMap<String, Location> idLocations = new HashMap<String, Location>();

    /**
     * 
     * @param date
     */
    public PostShef(Date date) {
        postDate = date;
        getAppsDefaults();
        PostTables.PostTablesInit();
    }

    private void getAppsDefaults() {
        undefStation = appDefaults.getToken(ShefConstants.SHEF_POST_UNKNOWN,
                ShefConstants.NONE);

        shefPostDuplicate = appDefaults.getToken(ShefConstants.SHEF_DUPLICATE);

        shefPostDuplicateDef = appDefaults.getToken(
                ShefConstants.SHEF_DUPLICATE, "IF_DIFFERENT");

        shefAlertAlarm = appDefaults.getBoolean(ShefConstants.SHEF_ALERTALARM,
                false);

        locMess = appDefaults.getBoolean(ShefConstants.LOCMESS, false);

        lookBackDays = appDefaults.getInt(ShefConstants.SHEF_WINPAST, 10);
        lookAheadMinutes = appDefaults.getInt(ShefConstants.SHEF_WINFUTURE, 30);
        postLink = appDefaults.getBoolean(ShefConstants.SHEF_POST_LINK, false);

        postLatest = appDefaults.getToken(ShefConstants.SHEF_POST_LATEST);

        loadMaxFcst = appDefaults.getToken(ShefConstants.SHEF_LOAD_MAXFCST,
                SHEF_ON).equals(SHEF_ON);

        postBadData = appDefaults.getToken(ShefConstants.SHEF_POST_BADDATA,
                "REJECT").equalsIgnoreCase("REJECT");

        basis_hours_str = appDefaults
                .getToken(ShefConstants.BASIS_HOURS_FILTER);

        elgMess = appDefaults.getBoolean(ShefConstants.ELGMESS, false);

        loadIngest = appDefaults.getBoolean(ShefConstants.SHEF_LOAD_INGEST,
                false);

        ingestMess = appDefaults.getBoolean(ShefConstants.INGEST_MESS, false);

        procObs = appDefaults.getBoolean(ShefConstants.SHEF_PROCOBS, false);

        dataLog = appDefaults.getBoolean(ShefConstants.SHEF_DATA_LOG, false);
        // TODO need to implement this token and the performance logging
        perfLog = appDefaults.getBoolean(ShefConstants.SHEF_PERFLOG, false);
        
        archiveMode = appDefaults.getBoolean("ALLOW_ARCHIVE_DATA",false);
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

        if (log.isDebugEnabled()) {
            log.debug("PostShef.post() called...");
        }
        this.shefRecord = shefRecord;

        /* Make sure we have data, else return */
        if (shefRecord == null) {
            log.info("Not Posted:Report is null");
            return;
        } else {
            if (log.isDebugEnabled()) {
                log.debug("ShefRecord = " + shefRecord);
            }
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
        if(identifier == null) {
            identifier = "MSGPRODID";
        }

        prodId = identifier;

        // /* Build the text product */
        // storeTextProduct();

        String locId = shefRecord.getLocationId();
        prodTime = shefRecord.getProductTime();

        if (ShefRecord.ShefType.B.equals(shefRecord.getShefType())) {
            String locateId = null;
            // need to handle bType for bad data-- loop through it
            // for (ShefData data : dataValues) {
            // locateId = data.getLocationId();
            // log.info("Posting process started for shefrecord " + locateId);
            // }
        } else if ((locId == null) || (dataValues == null)) {
            // Check for bad data
            log.warn("No data stored for " + prodId);
            return;
        }
        prevLid = locId;

        /*
         * check to see whether this location should be posted. it checks if the
         * "location" is defined in the location table; also allow for
         * "locations" (i.e. areas) to be defined in the GeoArea table. this if
         * for data for counties, basins, etc.
         */
        prevLid = null;
        Location postLocData = null;
        for (ShefData data : dataValues) {

            boolean same_lid_product = false;

            String dataValue = data.getStringValue();

            if (ShefConstants.SHEF_SKIPPED.equals(dataValue)) {
                continue;
            } else if (ShefConstants.SHEF_MISSING_DEC.equals(dataValue)) {
                dataValue = ShefConstants.SHEF_MISSING;
            }

            // Per A1 code - set the creation date to Date(0) if missing.
            Date basis = data.getCreationDateObj();
            if(basis == null) {
                Date d = new Date(0);
                SimpleDateFormat sdf = new SimpleDateFormat(ShefConstants.POSTGRES_DATE_STRING);
                sdf.setTimeZone(TimeZone.getTimeZone("Zulu"));
                data.setCreationDateObj(d);
                try {
                    data.setCreationDate("1970-01-01 00:00:00");
                } catch (ParseException e) {
                    // Nothing - will not happen!
                }
            }
            
            locId = data.getLocationId();

            String key = String.format("%s%s%s", locId, prodId,
                    data.getObservationTime());
            if (idLocations.containsKey(key)) {
                postLocData = idLocations.get(key);
                same_lid_product = true;
            } else {
                postLocData = checkLocation(data.getLocationId());
                idLocations.put(key, postLocData);
                same_lid_product = false;
            }

            log.info("========================================");

            log.info(String.format(POST_START_MSG, locId, data.getPeTsE(), dataValue));
            
            /*
             * determine the type of data this is, based on the type-source
             * code. areal data is separated from the point data. note that
             * processed data can be labeled as observed data!!! also note that
             * any type-sources which are not R,F, or C are assumed to be
             * processed. This includes the numbered type-source codes.
             */
            String dataQualifier = data.getQualifier();
            TypeSource typeSource = data.getTypeSource();

            if (typeSource != null) {
                if (TypeSource.UNKNOWN.equals(typeSource)) {
                    log.error("Unknown typesource code in data [" + data + "]");
                    continue;
                }
            } else {
                log.error("Unknown typesource code in data [" + data + "]");
                continue;
            }

            // Don't use the TypeSource directly because there are some cases
            // where the "type" defaults.
            DataType dataType = ParameterCode.DataType.getDataType(typeSource,procObs);

            if (log.isDebugEnabled()) {
                log.debug("DataType = " + dataType);
            }
         
            /*
             *  if the station_id exists in location table and
             *  the data type is READING then the data doesn't get posted
             *  to the appropriate pe-based tables to match A1 logic.
             *  DR16711  
             */
            
            if ((DataType.READING.equals(dataType)) 
            		&&(Location.LOC_GEOAREA.equals(postLocData))) {
            	postLocData=Location.LOC_UNDEFINED;            	
            }
            
            SHEFDate d = data.getObsTime();
            if (d == null) {
                log.error(data);
                log.error("Not posted:Record does not contain an observation time");
                return;
            }
            Date obsTime = d.toCalendar().getTime();
            Date createTime = null;
            if (data.getCreateTime() != null) {
                createTime = data.getCreateTime().toCalendar().getTime();
            }

            /*
             * if location not defined, issue message and save the data if
             * appropriate. now dispense of the unknown data in the appropriate
             * manner. note for unknown data, any comments specified are not
             * stored. also note, for unknown station data, don't bother posting
             * if the data has not changed.
             */
            StoreDisposition disposition = StoreDisposition.NONE;
            if (Location.LOC_UNDEFINED.equals(postLocData)) {
                // Do logging here
                if (locMess && !same_lid_product) {
                    String sMsg = null; 
                    if (ShefConstants.NONE.equalsIgnoreCase(undefStation)) {
                        sMsg = String.format("LID [%s] not defined; no data posted", locId);
                    } else if ("IDS_ONLY".equalsIgnoreCase(undefStation)) {
                        sMsg = String.format("LID [%s] not defined; ; station info posting to UnkStn", locId);
                    } else if ("IDS_AND_DATA".equalsIgnoreCase(undefStation)) {
                        sMsg = String.format("LID [%s] not defined; ; data posting to UnkStnValue", locId);
                    }
                    if(sMsg != null) {
                        log.info(sMsg);
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
                        unknown.setTraceId(shefRecord.getTraceId());
                        start = System.currentTimeMillis();
                        PostTables.postUnknownStation(unknown, stats);
                        end = System.currentTimeMillis();
                        stats.addElapsedTimeUnknown(end - start);
                        disposition = StoreDisposition.UKN_STN_POSTING;
                    } else {
                        disposition = StoreDisposition.UKN_STN_POSTED;
                    }
                } else if ("IDS_AND_DATA".equalsIgnoreCase(undefStation)) {
                    stats.incrementUnknownStation();
                    PersistableDataObject unknstnvalue = populateDataObj(
                            shefRecord, data, locId,
                            ShefConstants.UNKNOWN_STATION_VALUE, dataValue,
                            dataQualifier, 0);

                    start = System.currentTimeMillis();
                    PostTables.postData(unknstnvalue,
                            ShefConstants.UNKNOWN_STATION_VALUE,
                            shefPostDuplicateDef, stats);
                    end = System.currentTimeMillis();
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
                    log.info(String.format("Station [%s] is inactive", locId));
                }
                disposition = StoreDisposition.INACTIVE_LID;
            }

            /*
             * check if an lid-PEDTSE entry exists in the IngestFilter. this
             * function can self-populate the IngestFilter table; if not in
             * self-populate mode, then an error message is issued if there is
             * no entry in the IngestFilter table and the data will not be
             * posted.
             */
            ShefConstants.IngestSwitch ingestSwitch = ShefConstants.IngestSwitch.POST_PE_ONLY;
            if (Location.LOC_LOCATION.equals(postLocData)
                    || (Location.LOC_GEOAREA.equals(postLocData))) {
                if (!DataType.CONTINGENCY.equals(dataType)) {
                    ingestSwitch = checkIngest(locId, data, ingestSwitch);
                }
                if (ShefConstants.IngestSwitch.POST_PE_OFF.equals(ingestSwitch)) {
                    stats.incrementNoPost();
                }
            }

            /*
             * if the location data should not be posted because either: 1) the
             * location is not defined as a location or an area, or because the
             * location post switch is off, or 2) the PEDTSE ingest switch is
             * turned off; then no need to continue
             */

            boolean t2 = ShefConstants.IngestSwitch.POST_PE_OFF
                    .equals(ingestSwitch);

            if ((!Location.LOC_LOCATION.equals(postLocData) && !Location.LOC_GEOAREA
                    .equals(postLocData)) || t2) {
                /*
                 * set the prev info for the next pass through this function.
                 * this is info is used for to prevent redundant messages
                 */
                prevLid = locId;
                prevProdId = identifier;
                prevProdTime = prodTime;
                String unkmsg = null;
                switch (disposition) {
                case UKN_STN_POSTING : {
                    unkmsg = String.format("Posting LID [%s] to [unkstn]", locId);
                    break;
                }
                case UKN_STN_POSTED : {
                    unkmsg = String.format("LID [%s] already posted to [unkstn]", locId);
                    break;
                }
                case UKN_STN_VALUE : {
                    unkmsg = String.format("Posting LID [%s] data [%s] to [unkstnvalue]", locId, dataValue);
                    break;
                }
                case INACTIVE_LID :
                    unkmsg = String.format("Not posting data [%s] for inactive LID [%s]", dataValue, locId);
                    break;
                default : {
                    unkmsg = String.format("Not posting data [%s] for LID [%s]", dataValue, locId);
                    break;
                }
                }
                log.warn(unkmsg);
                stats.incrementWarningMessages();
                continue;
            }

            /*---------------------------------------------------------------------*/
            /*
             * check for observed data too far in past or future if data is
             * outside of this time window, then do not post. skip this check if
             * data is monthly data
             */

            if (DataType.READING.equals(dataType)
                    || TypeSource.PROCESSED_MEAN_AREAL_DATA.equals(typeSource)) {

                long lookbackMillis = lookBackDays
                        * ShefConstants.MILLIS_PER_DAY;

                long lookfwdMillis = lookAheadMinutes
                        * ShefConstants.MILLIS_PER_MINUTE;

                if ((postDate.getTime() - obsTime.getTime() > lookbackMillis)
                        && (!Duration._1_MONTH.equals(data.getDuration()))) {
                    stats.incrementWarningMessages();
                    stats.incrementOutsideWindow();
                    log.warn(locId + " " + data.getObsTime() + " obs time > "
                            + lookBackDays + " days old; data not posted");
                    continue;
                } else if (obsTime.getTime() - postDate.getTime() > lookfwdMillis) {
                    stats.incrementWarningMessages();
                    stats.incrementOutsideWindow();
                    log.warn(locId + " obs time (" + data.getObsTime() + ") >"
                            + " post time (" + postDate + "); "
                            + lookAheadMinutes
                            + " minutes in the future; data not posted");
                    continue;
                }
            }
            if (log.isDebugEnabled()) {
                log.debug("Data in the window");
            }
            /*
             * check for forecast basis times that are after the valid time,
             * issue a warning message if this is the case - basis time is the
             * creation date and valid time is the obs time
             */
            if (DataType.FORECAST.equals(dataType)
                    || TypeSource.FORECAST_MEAN_AREAL_DATA.equals(typeSource)) {

                if (createTime == null) {
                    // stats.incrementWarning Messages();
                    if (log.isDebugEnabled()) {
                        log.debug("Creation date not present.");
                    }
                } else {
                    if (createTime.getTime() > obsTime.getTime()) {
                        stats.incrementWarningMessages();
                        log.warn(locId + " basis time (" + createTime
                                + ") > valid time (" + obsTime
                                + "); check encoding");
                    }
                }
            }
            /*
             * check to see if an adjustment factor should be applied to the raw
             * SHEF value coming in and if so adjust that value in the shefrec
             * structure
             */
            if (!dataValue.equals(ShefConstants.SHEF_MISSING)) {
                  adjustRawValue(locId, data);
            }
            
            /*
             * multiply non-missing values of discharge values and unspecified
             * height values by 1000 to change units
             */
            String pe = data.getPhysicalElement().getCode();
            if((pe != null)&&(data.getValue() != -9999)) {
                Matcher m = Q_CODES.matcher(pe);
                if(m.matches()) {
                    data.adjustValue(1, 0, 1000.0, 0);
                    dataValue = data.getStringValue();
                }
                if("HZ".equals(pe)) {
                    data.adjustValue(1, 0, 1000.0, 0);
                    dataValue = data.getStringValue();
                }
            }

            /*---------------------------------------------------------------*/
            /*
             * post data to the appropriate table(s). for the sake of
             * uniformity, most of these functions have the same argument list
             * even though some of the arguments are not used by some functions
             * 
             * if instructed, post to the product link table, but only if the
             * info has changed
             */
            if (postLink && !same_lid_product) {
                start = System.currentTimeMillis();
                // Identifier has been set from the awipsHeader.

                postProductLink(locId, identifier, obsTime);
                // postProductLink(locId, shefRecord.getIdentifier(), obsTime);
                stats.addElapsedTimeIngest(System.currentTimeMillis() - start);
                
                if (dataLog || log.isDebugEnabled()) {
                    String msg = String.format("Posted product link [%s] for LID [%s]", identifier, locId);
                    if (dataLog) {
                        log.info(msg);
                    } else if(log.isDebugEnabled()) {
                        log.debug(msg);
                    }
                }
            }


            /*
             * Check the quality of the data if observed or forecast. note the
             * posting may treat processed data as observed, including this
             * manner.
             * 
             * the quality_code defined contains information from two 'sources'.
             * one, the qc checks performed by shef, and two, certain shef
             * qualifier codes reflect the quality of the data. use the
             * information in the quality_code field, which is based on these
             * two sources, to help determine the dispensation of the value.
             */

            boolean valueOk = false;
            long qualityCode = -999;
            Date validTime = new Date(obsTime.getTime());

            /* Don't perform the check if the value is a missing value */
            if (!ShefConstants.SHEF_MISSING.equals(dataValue)) {
                qualityCode = checkQuality(locId, dataQualifier, dataValue,
                        data);
                valueOk = checkQcCode(QualityControlCode.QC_NOT_FAILED,
                        qualityCode);
            } else {
                qualityCode = ShefQC.setQcCode(ShefQC.QC_DEFAULT, 0L);
                valueOk = true;
            }

            /*
             * only attempt to post to the latestobsvalue table if meets certain
             * conditions based on settings
             */
            if (DataType.READING.equals(dataType)) {
                if (SHEF_ON.equalsIgnoreCase(postLatest)
                        || (ShefConstants.VALID_ONLY
                                .equalsIgnoreCase(postLatest) && valueOk && (data
                                .getStringValue() != ShefConstants.SHEF_MISSING))
                        || (ShefConstants.VALID_OR_MISSING
                                .equalsIgnoreCase(postLatest) && valueOk)) {
                    PostTables.postLatestObs(shefRecord, data, locId,
                            data.getStringValue(), data.getQualifier(),
                            qualityCode, prodId, prodTime,
                            shefPostDuplicateDef, stats, postDate);
                    if (dataLog) {
                        log.info(String.format(LOV_POST_MSG, dataValue, 
                                data.getObservationTimeObj(), locId,
                                data.getPhysicalElement().getCode()));
                    }
                }
            }

            /*
             * if the data is either observed or forecast, or if processed data
             * is being treated as observed data, then invoke the procedure to
             * post to the appropriate pe-based table. if data are bad, then
             * don't post to pe-tables and instead post to reject data, as per
             * user instructions.
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

                    PostTables.postData(rejectValue,
                            ShefConstants.REJECTED_DATA, shefPostDuplicateDef,
                            stats);
                    if (dataLog) {
                        log.info(String
                                .format("Posting data [%s] for LID [%s] to rejectedData table",
                                        dataValue, locId));
                    }
                } else {
                    if (DataType.READING.equals(dataType)
                            || DataType.FORECAST.equals(dataType)) {
                        if (checkIfPaired(data)) {
                            PostTables.postPairedData(shefRecord, data, locId,
                                    dataValue, dataQualifier, qualityCode,
                                    prodId, prodTime, shefPostDuplicateDef,
                                    stats, postDate);
                            if (dataLog) {
                                log.info(String
                                        .format("Posting data [%s] for LID [%s] to pairedValue table",
                                                dataValue, locId));
                            }
                        } else {                        	             	
                            PostTables.postPeData(shefRecord, data, locId,
                            		data.getStringValue(), dataQualifier, qualityCode,
                                    prodId, prodTime, shefPostDuplicateDef,
                                    ingestSwitch, stats, validTime, postDate,
                                    dataType);
                            if (dataLog) {
                                log.info(String
                                        .format("Posting data [%s] for LID [%s] for PE [%s]",
                                        		data.getStringValue(), locId, data
                                                        .getPhysicalElement()
                                                        .getCode()));
                            }
                        }
                    } else if (DataType.AREAL_PROCESSED.equals(dataType)) {
                        /*
                         * if a value is both areal and paired, then let the
                         * paired characteristic of the data take precedence
                         * over the areal nature of the data, so store the areal
                         * paired data in the pairedvalue table, not the areal
                         * tables.
                         */
                        if (checkIfPaired(data)) {
                            PostTables.postPairedData(shefRecord, data, locId,
                                    dataValue, dataQualifier, qualityCode,
                                    prodId, prodTime, shefPostDuplicateDef,
                                    stats, postDate);
                            if (dataLog) {
                                log.info(String
                                        .format("Posting areal obs data [%s] for LID [%s] to pairedValue table",
                                                dataValue, locId));
                            }
                        } else {
                            PersistableDataObject arealObs = populateDataObj(
                                    shefRecord, data, locId,
                                    ShefConstants.AREAL_OBS, dataValue,
                                    dataQualifier, qualityCode);

                            PostTables.postData(arealObs,
                                    ShefConstants.AREAL_OBS,
                                    shefPostDuplicateDef, stats);
                            if (dataLog) {
                                log.info(String
                                        .format("Posting areal obs data [%s] for LID [%s] to arealobs table",
                                                dataValue, locId));
                            }
                        }
                    } else if (DataType.AREAL_FORECAST.equals(dataType)) {
                        if (checkIfPaired(data)) {
                            PostTables.postPairedData(shefRecord, data, locId,
                                    dataValue, dataQualifier, qualityCode,
                                    prodId, prodTime, shefPostDuplicateDef,
                                    stats, postDate);
                            if (dataLog) {
                                log.info(String
                                        .format("Posting areal forecast data [%s] for LID [%s] to pairedValue table",
                                                dataValue, locId));
                            }
                        } else {
                            PersistableDataObject arealfcst = populateDataObj(
                                    shefRecord, data, locId,
                                    ShefConstants.AREAL_FCST, dataValue,
                                    dataQualifier, qualityCode);

                            PostTables.postData(arealfcst,
                                    ShefConstants.AREAL_FCST,
                                    shefPostDuplicateDef, stats);
                            if (dataLog) {
                                log.info(String
                                        .format("Posting areal forecast data [%s] for LID [%s] to arealfcst table",
                                                dataValue, locId));
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
                    PostTables.postPairedData(shefRecord, data, locId,
                            dataValue, dataQualifier, qualityCode, prodId,
                            prodTime, shefPostDuplicateDef, stats, postDate);
                    if (dataLog) {
                        log.info(String
                                .format("Posting contingency data [%s] for LID [%s] to pairedValue table",
                                        dataValue, locId));
                    }
                } else {
                    PersistableDataObject contingency = populateDataObj(
                            shefRecord, data, locId,
                            ShefConstants.CONTINGENCY_VALUE, dataValue,
                            dataQualifier, qualityCode);

                    PostTables.postData(contingency,
                            ShefConstants.CONTINGENCY_VALUE,
                            shefPostDuplicateDef, stats);
                    if (dataLog) {
                        log.info(String
                                .format("Posting contingency data [%s] for LID [%s] to contingencyValue table",
                                        dataValue, locId));
                    }
                }
                break;
            } // case CONTINGENCY:
            case PROCESSED: {
                if (checkIfPaired(data)) {
                    PostTables.postPairedData(shefRecord, data, locId,
                            dataValue, dataQualifier, qualityCode, prodId,
                            prodTime, shefPostDuplicateDef, stats, postDate);
                    if (dataLog) {
                        log.info(String
                                .format("Posting processed data [%s] for LID [%s] to pairedValue table",
                                        dataValue, locId));
                    }
                } else {
                    PersistableDataObject procval = populateDataObj(shefRecord,
                            data, locId, ShefConstants.PROC_VALUE, dataValue,
                            dataQualifier, qualityCode);

                    PostTables.postData(procval, ShefConstants.PROC_VALUE,
                            shefPostDuplicateDef, stats);
                    if (dataLog) {
                        log.info(String
                                .format("Posting processed data [%s] for LID [%s] to procValue table",
                                        dataValue, locId));
                    }
                }
                break;
            } // case PROCESSED:
            } // switch

            /*
             * post alertalarm data as necessary. Don't perform the alert/alarm
             * post if the data is a ContingencyValue
             */
            if (!DataType.CONTINGENCY.equals(dataType) && shefAlertAlarm
                    && (alertAlarm != ShefConstants.NO_ALERTALARM)) {

                // TODO: Ensure what is to be saved here!
                post_alertalarm(data, locId, dataValue, dataQualifier,
                        qualityCode);
                stats.incrementAlertAlarm();
                if (dataLog) {
                    log.info(String
                            .format("Posting data [%s] for LID [%s] to alertAlarmVal table",
                                    dataValue, locId));
                }
            }

            /*
             * now check if there is any comment data associated with this data.
             * if so, then store in the comment table, where comments for all
             * datatypes goes.
             */
            String c = data.getRetainedComment();
            if ((c != null) && (c.length() > 0)) {
                PersistableDataObject commentValue = populateDataObj(
                        shefRecord, data, locId, ShefConstants.COMMENT_VALUE,
                        dataValue, dataQualifier, qualityCode);

                PostTables.postData(commentValue, ShefConstants.COMMENT_VALUE,
                        shefPostDuplicateDef, stats);
                if (dataLog) {
                    log.info(String
                    .format("Posting comments for data [%s] : LID [%s] to commentValue table",
                            dataValue, locId));
                }
            }

            /*
             * if we just received some forecast height or discharge data, then
             * update the riverstatus table for those reports
             */
            if ((DataType.FORECAST.equals(dataType))
                    && loadMaxFcst
                    && (data.getPhysicalElement().getCode().startsWith("H") || data
                            .getPhysicalElement().getCode().startsWith("Q"))) {

                postRiverStatus(data, locId);
            }

            /*
             * very important to store this info to prevent redundant posting.
             */
            prevLid = locId;
            prevProdId = prodId;
            prevProdTime = prodTime;

        } // for
    }

    public void logStats(String traceId, long totalTime) {
        if (this.perfLog) {
            Log perfLog = LogFactory.getLog("ShefPerfLog");
            SimpleDateFormat sdf = new SimpleDateFormat("yyMMddHHmmss");
            perfLog.info("********************************");
            perfLog.info("Performance Stats:  " + traceId);
            perfLog.info("Total Elapsed Time (ms): " + totalTime);
            if(prodTime != null) {
                perfLog.info(prodId + ", " + sdf.format(prodTime));
            } else {
                perfLog.info(prodId + ", ------------");
            }
            logIt(perfLog, stats.getAlertAlarm(), " AlertAlarm");
            logIt(perfLog, stats.getArealFcstOverwrite(),
                    " Areal Fcst Overwrite");
            logIt(perfLog, stats.getArealFcstValues(), " Areal Fcst Values");
            logIt(perfLog, stats.getArealObsOverwrite(), " Areal Obs Overwrite");
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
            logIt(perfLog, stats.getOutsideTimeWindow(), " Outside Time Window");
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

    private void logIt(Log log, long value, String label) {
        if (value > 0) {
            log.info(value + label);
        }
    }

    /**
     * Post data to the alertalarmval data table. If duplicate found and new
     * value should be overwritten, then overwrite the data without saving the
     * replaced data. See post_tables.c: post_alertalarm
     */
    private void post_alertalarm(ShefData data, String locId, Object value,
            String qualifier, long qualityCode) {
        String aaCategory = null;
        String aaCheck = null;

        /*
         * these fields are particular to the AlertAlarm operations
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
        if (log.isDebugEnabled()) {
            log.debug("alertAlarm = " + alertAlarm);
        }

        PersistableDataObject aaValue = populateDataObj(shefRecord, data,
                locId, ShefConstants.ALERTALARM_VALUE, data.getStringValue(),
                qualifier, qualityCode);

        ((Alertalarmval) aaValue).getId().setAaCateg(aaCategory);
        ((Alertalarmval) aaValue).getId().setAaCheck(aaCheck);

        PostTables.postAAData(aaValue, ShefConstants.ALERTALARM_VALUE,
                shefPostDuplicate, stats, aaCategory, aaCheck);

    }

    /**
     * Post data to the riverstatus data table.
     */
    private void postRiverStatus(ShefData data, String locId) {

        long start = 0;
        long end = 0;
        long duration = 0;
        // int maxfcst = 0;
        String tableName = null;
        String pe = data.getPhysicalElement().getCode();
        log.info("Update RiverStatus for: " + locId + " "
                + data.getPhysicalElement());

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

        end = System.currentTimeMillis();
        ;
        duration = end - start;
        log.info("H/Q lid-pe; updated RiverStatus, runtime = " + duration
                + " ms.");

    }

    /**
     * Process forecast data for the given tablename. Don't consider any
     * probabilistic values.
     **/
    private void loadMaxFcstData(String tableName) {
        CoreDao dao = null;
        Object[] oa = null;
        String lid = null;
        String pe = null;
        String ts = null;

//      String query = "select lid,pe,ts " + "from " + tableName + " "
//      + "where validtime > CURRENT_TIMESTAMP and "
//      + "probability < 0.0";
        
        String query = String
                .format("select lid,pe,ts from %s where validtime > '%s' and probability < 0.0",
                        tableName, toTimeStamp(postDate));

        try {
            dao = new CoreDao(DaoConfig.forDatabase(ShefConstants.IHFS));

            oa = dao.executeSQLQuery(query);

            if (oa == null) {
                return;
            }

            Object[] row = null;
            for (int i = 0; i < oa.length; i++) {
                row = (Object[]) oa[i];
                if(row.length == 3) {
                    lid = ShefUtil.getString(row[0], null);
                    pe = ShefUtil.getString(row[1], null);
                    ts = ShefUtil.getString(row[2], null);

                    if ((lid != null) && (pe != null)&&(ts != null)) {
                        loadMaxFcstItem(lid, pe, ts);
                    }
                }
            }
        } catch (Exception e) {
            log.error("Query = [" + query + "]");
            log.error(shefRecord.getTraceId()
                    + " - PostgresSQL error retrieving from " + tableName);
            if(log.isDebugEnabled()) {
                log.error(e);
            }
        }
    }

    /**
     * Process forecast data for the given tableName.
     */
    private void loadMaxFcstData_lidpe(String tableName, String locId, String pe) {
        CoreDao dao = null;
        Object[] oa = null;
        if ((tableName != null) && (locId != null) && (pe != null)) {
            String query = "select DISTINCT(ts) " + "from " + tableName
                    + " where lid = '" + locId + "' and " + "pe = '" + pe
                    + "' and " + "validtime > CURRENT_TIMESTAMP and "
                    + "probability < 0.0";

            try {
                dao = new CoreDao(DaoConfig.forDatabase(ShefConstants.IHFS));
                oa = dao.executeSQLQuery(query);

                for (int i = 0; i < oa.length; i++) {
                    String ts = ShefUtil.getString(oa[i], null);
                    if(ts != null) {
                        loadMaxFcstItem(locId, pe, ts);
                    }
                }

            } catch (Exception e) {
                log.error("Query = [" + query + "]");
                log.error(shefRecord.getTraceId()
                        + " - PostgresSQL error retrieving from " + tableName);
                if(log.isDebugEnabled()) {
                    log.error(e);
                }
            }
        }
    }

    /**
     * Loads the max fcst info into the RiverStatus table for the current
     * location and pe.
     * */
    private void loadMaxFcstItem(String lid, String pe, String ts) {
        CoreDao dao = null;
        Object[] oa = null;
        String riverStatQuery = "select use_latest_fcst from riverstat where lid = '"
                + lid + "'";
        String hourQuery = "select obshrs,fcsthrs from RpfParams";
        String deleteQuery = "delete from riverstatus  " + "where lid= '" + lid
                + "' and pe= '" + pe + "' and ts= '" + ts + "'";
        int useLatest = 0;
        int qcFilter = 1;
        List<ShefData> shefList = null;
        try {
            dao = new CoreDao(DaoConfig.forDatabase(ShefConstants.IHFS));

            oa = dao.executeSQLQuery(riverStatQuery);

            /*
             * get the setting for the use_latest_fcst field for the current
             * location from the riverstat table.
             */

            if (oa == null) {
                useLatest = 1;
            } else {
                if (oa.length > 0) {
                    if ("T".equals(ShefUtil.getString(oa[0],null))) {
                        useLatest = 1;
                    }
                }
            }

            /*
             * get the forecast time series for this location, pe, and ts using
             * any instructions on any type-source to screen and whether to use
             * only the latest basis time
             */
            long currentTime = System.currentTimeMillis();
            // long obsTime = 0;
            // long endValidTime = 0;
            long basisBeginTime = 0;

            /*
             * This code sets the time values
             */
            if (!isHoursLoad) {
                oa = dao.executeSQLQuery(hourQuery);
                Object[] row = null;
                if (oa.length > 0) {
                    row = (Object[]) oa[0]; // first row
                    obshrs = ((Integer) row[0]).longValue();
                    fcsthrs = ((Integer) row[1]).longValue();
                } else {
                    log.error("No records in RpfParams table, using defaults");
                }

                if (basis_hours_str != null) {
                    basishrs = Long.parseLong(basis_hours_str);
                    if ((basishrs <= 0) || (basishrs > 480)) {
                        log.info("invalid value for basis_hours_filter token: "
                                + basishrs);
                        basishrs = 72;
                    }
                }
                isHoursLoad = true;
            }
            // obsTime = currentTime - (obshrs * 3600 * 1000);
            // endValidTime = currentTime + (fcsthrs * 3600 * 1000);
            basisBeginTime = currentTime - (basishrs * 3600 * 1000);

            shefList = buildTsFcstRiv(lid, pe, ts, qcFilter, useLatest,
                    basisBeginTime);

            if ((shefList != null) && (shefList.size() > 0)) {
                ShefData maxShefDataValue = findMaxFcst(shefList);
                boolean updateFlag = updateRiverStatus(lid, pe, ts);
                PostTables.postRiverStatus(shefRecord, maxShefDataValue,
                        updateFlag);

            } else {
                /*
                 * if no data were found, then delete any entries that may exist
                 * for this key. this is needed if general applications are
                 * using this function directly and delete all forecast data for
                 * a given key
                 */
                dao.executeSQLUpdate(deleteQuery);
            }

        } catch (Exception e) {
            log.error("Query = [" + riverStatQuery + "]");
            log.error("Query = [" + hourQuery + "]");
            log.error(shefRecord.getTraceId()
                    + " - PostgresSQL error loading max forecast item");
            if(log.isDebugEnabled()) {
                log.error(e);
            }
        }
    }

    /**
     * Determines if the lid,pe,ts already exists and whether or not the
     * riverstatus table needs to be updated.
     */
    private boolean updateRiverStatus(String lid, String pe, String ts) {
        boolean rval = false;
        CoreDao dao = null;
        Object[] oa = null;

        String query = "select lid " + "from riverstatus where lid = '" + lid
                + "' and " + "pe = '" + pe + "' and " + "ts = '" + ts + "'";

        try {
            dao = new CoreDao(DaoConfig.forDatabase(ShefConstants.IHFS));

            oa = dao.executeSQLQuery(query);

            if ((oa != null) && (oa.length > 0)) {
                rval = true;
            }

        } catch (Exception e) {
            log.error("Query = [" + query + "]");
            log.error(shefRecord.getTraceId()
                    + " - PostgresSQL error searching riverstatus");
            if(log.isDebugEnabled()) {
                log.error(e);
            }
        }
        return rval;
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
            String tsFilter, int qcFilter, int useLatest, long basisBegintime) {
        // int status = -1;
        int fcstCount = 0;
        int keepCount = 0;
        int QUESTIONABLE_BAD_THRESHOLD = 1073741824;

        String useTs = null;
        String tableName = null;
        String query = null;
        String queryForecast = null;

        java.sql.Timestamp basisTimeAnsi = null;

        int[] doKeep = null;
        Object[] ulHead = null;
        Object[] row = null;
        Fcstheight[] fcstHead = null;
        Fcstheight fcstHght = null;
        // List<ShefData> fcstList = new ArrayList<ShefData>();

        List<ShefData> shefList = new ArrayList<ShefData>();
        ShefData shefDataValue = null;
        CoreDao dao = null;

        if ((tsFilter == null) || (tsFilter.length() == 0)) {
            useTs = getBestTs(lid, pe, "F%", 0);
            if (useTs == null) {
                return null;
            }

        } else {
            useTs = tsFilter;
        }
        try {
            dao = new CoreDao(DaoConfig.forDatabase(ShefConstants.IHFS));

            if (pe.startsWith("H") || pe.startsWith("h")) {
                tableName = "FcstHeight";
            } else {
                tableName = "FcstDischarge";
            }

            basisTimeAnsi = new Timestamp(basisBegintime);
            /*
             * retrieve a list of unique basis times; use descending sort. only
             * consider forecast data before some ending time, and with some
             * limited basis time ago
             */

            query = "SELECT DISTINCT(basistime) FROM " + tableName + " "
                    + "WHERE lid = '" + lid + "' and " + "pe = '" + pe
                    + "' and " + "ts = '" + useTs + "' and "
                    + "validtime >= CURRENT_TIMESTAMP and " + "basistime >= '"
                    + basisTimeAnsi + "' and " + "value != "
                    + ShefConstants.SHEF_MISSING_INT + " and "
                    + "quality_code >= " + QUESTIONABLE_BAD_THRESHOLD + " "
                    + "ORDER BY basistime DESC ";

            ulHead = dao.executeSQLQuery(query);

            if ((ulHead == null) || (ulHead.length <= 0)) {
                return null;
            }

            /*
             * retrieve the data; the ordering by validtime is important. as
             * before, limit the forecast time valid time window and as needed,
             * the age of the forecast (basistime).
             */

            queryForecast = "SELECT lid,pe,dur,ts,extremum,probability,validtime,basistime,value "
                    + "FROM "
                    + tableName
                    + " "
                    + "WHERE lid = '"
                    + lid
                    + "' AND "
                    + "pe = '"
                    + pe
                    + "' AND "
                    + "ts = '"
                    + useTs
                    + "' AND "
                    + "validtime >= CURRENT_TIMESTAMP AND "
                    + "probability < 0.0 AND ";

            if ((useLatest == 1) || (ulHead.length == 1)) {
                java.sql.Timestamp tempStamp = null;
                tempStamp = (Timestamp) ulHead[0];
                queryForecast += "basistime >= '" + tempStamp + "' AND ";

            } else {
                queryForecast += "basistime >= '" + basisTimeAnsi + "' AND ";

            }
            queryForecast += "value != "
                    + Integer.parseInt(ShefConstants.SHEF_MISSING) + " AND "
                    + "quality_code >= "
                    + Integer.parseInt(ShefConstants.SHEF_MISSING) + " "
                    + "ORDER BY validtime ASC";

            Object[] oa = dao.executeSQLQuery(queryForecast);
            row = null;

            if ((oa != null) && (oa.length > 0)) {
                fcstHead = new Fcstheight[oa.length];
                for (int i = 0; i < oa.length; i++) {
                    row = (Object[]) oa[i];
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
            }

            if (fcstHead != null) {
                fcstCount = fcstHead.length;
            }

            /*
             * define a local array to determine which items in the time series
             * to keep and return
             */
            if (fcstCount > 0) {
                doKeep = new int[fcstCount];
            } else {
                return null;
            }

            /*
             * if only getting the latest basis time's data or only one basis
             * time was found, then consider all; otherwise, need to adjoin/butt
             * the time series together for the multiple basis times.
             */

            if ((useLatest == 1) || (ulHead.length <= 1)) {
                for (int i = 0; i < doKeep.length; i++) {
                    doKeep[i] = 1;
                }
            } else {
                doKeep = setFcstKeep(ulHead, fcstHead);
            }

            /*
             * now load the values and info to return, knowing which items to
             * keep since all the values have been tagged. first get the count
             * of the number of values to keep and allocate the data
             */

            for (int j = 0; j < fcstCount; j++) {
                if (doKeep[j] == 1) {
                    keepCount++;
                }
            }

            for (int y = 0; y < fcstCount; y++) {
                shefDataValue = new ShefData();
                if (doKeep[y] == 1) {
                    shefDataValue.setLocationId(fcstHead[y].getId().getLid());
                    shefDataValue.setPhysicalElement(PhysicalElement
                            .getEnum(fcstHead[y].getId().getPe()));

                    convertDur(fcstHead[y].getId().getDur(), shefDataValue);
                    shefDataValue.setTypeSource(TypeSource.getEnum(fcstHead[y]
                            .getId().getTs()));
                    shefDataValue.setExtremum(Extremum.getEnum(fcstHead[y]
                            .getId().getExtremum()));
                    shefDataValue.setObservationTimeObj(fcstHead[y].getId()
                            .getValidtime());
                    shefDataValue.setCreationDateObj(fcstHead[y].getId()
                            .getBasistime());
                    shefDataValue.setValue(fcstHead[y].getValue());
                    shefList.add(shefDataValue);
               }
            }
        } catch (Exception e) {
            log.error("Query = [" + query + "]");
            log.error("Query = [" + queryForecast + "]");
            log.error(shefRecord.getTraceId()
                    + " - PostgresSQL error in buildTsFcstRiv");
            if(log.isDebugEnabled()) {
                log.error(e);
            }
        }
        return shefList;
    }

    /**
     * Convert duration int to String character.
     * 
     * @param dur
     *            The duration value
     * @return The single character duration value
     */
    private void convertDur(short dur, ShefData data) {
        String value = null;
        String durationCode = null;

        switch (dur) {
        case 0:
            value = "I";
            break;
        case 1:
            value = "U";
            break;
        case 5:
            value = "E";
            break;
        case 10:
            value = "G";
            break;
        case 15:
            value = "C";
            break;
        case 30:
            value = "J";
            break;
        case 1001:
            value = "H";
            break;
        case 1002:
            value = "B";
            break;
        case 1003:
            value = "T";
            break;
        case 1004:
            value = "F";
            break;
        case 1006:
            value = "Q";
            break;
        case 1008:
            value = "A";
            break;
        case 1012:
            value = "K";
            break;
        case 1018:
            value = "L";
            break;
        case 2001:
            value = "D";
            break;
        case 2007:
            value = "W";
            break;
        // case 'N':
        // Not sure what to return. Shef maunal explanation:
        // N Mid month, duration for the period from the 1st day of the
        // month to and ending on the
        // 15th day of the same month
        // break;
        case 3001:
            value = "M";
            break;
        case 4001:
            value = "Y";
            break;
        case 5004:
            value = "P";
            break;
        case 5000: {
            value = "Z";
            break;
        }
        case 5001:
            value = "S";
            break;
        case 5002:
            value = "R";
            break;
        case 5005:
            value = "X";
            break;
        default: {
            // Anything that didn't get picked up above is
            // probably a variable duration.
            if (dur >= 7000) {
                value = "V";
                durationCode = "S";
            } else if (dur < 1000) {
                value = "V";
                durationCode = "N";
            } else if (dur < 2000) {
                value = "V";
                durationCode = "H";
            } else if (dur < 3000) {
                value = "V";
                durationCode = "D";
            } else if (dur < 4000) {
                value = "V";
                durationCode = "M";
            } else if (dur < 5000) {
                value = "V";
                durationCode = "Y";
            } else {
                // Not sure what value this would be.
                value = "Z";
            }
        }
        }
        data.setDuration(Duration.getEnum(value));
        data.setDurationCodeVariable(durationCode);
        data.setDurationValue(dur);
    }

    /**
     * Determine which items in the forecast time series to keep, as there may
     * be overlap due to multiple time_series.
     **/
    private int[] setFcstKeep(Object[] ulHead, Fcstheight[] fcstHead) {
        int fcstCount = fcstHead.length;
        int ulCount = ulHead.length;
        int[] doKeep = new int[fcstCount];
        int[] basisIndex = new int[fcstCount];
        int[] tsFirstChk = new int[ulCount];
        int MISSING = ShefConstants.SHEF_MISSING_INT;
        Timestamp[] startTime = new Timestamp[ulCount];
        Timestamp[] endTime = new Timestamp[ulCount];
        Timestamp[] basisTime = new Timestamp[ulCount];
        Timestamp fcstBasisTime = null;
        Timestamp fcstValidTime = null;
        Timestamp ulBasisTime = null;

        for (int i = 0; i < ulCount; i++) {
            tsFirstChk[i] = 0;
        }
        Timestamp row = null;
        Timestamp validTime = null;
        for (int i = 0; i < fcstCount; i++) {

            /* find out which basis time's time series this value belongs to */

            fcstBasisTime = new Timestamp(fcstHead[i].getId().getBasistime()
                    .getTime());

            basisIndex[i] = MISSING;

            for (int j = 0; ((j < ulCount) && (basisIndex[i] == MISSING)); j++) {
                row = (Timestamp) ulHead[j];
                ulBasisTime = row;

                if (ulBasisTime.compareTo(fcstBasisTime) == 0) {
                    basisIndex[i] = j;
                }
            }

            if (basisIndex[i] == MISSING) {
                log.info("Unexpected error assigning basis_index for " + i);
            }

            /*
             * check if the values constitute the start or end times for the
             * time series and record these times if they do
             */

            validTime = new Timestamp(fcstHead[i].getId().getValidtime()
                    .getTime());

            if (tsFirstChk[basisIndex[i]] == 1) {
                if (validTime.before(startTime[basisIndex[i]])) {
                    startTime[basisIndex[i]] = validTime;
                } else if (validTime.after(endTime[basisIndex[i]])) {
                    endTime[basisIndex[i]] = validTime;
                }
            } else {
                startTime[basisIndex[i]] = validTime;
                endTime[basisIndex[i]] = validTime;
                tsFirstChk[basisIndex[i]] = 1;
            }
        }

        /*
         * for each of the unique basis times, assign the basis time in a
         * convenient array for use in the adjust_startend function.
         */

        for (int j = 0; j < ulCount; j++) {
            row = (Timestamp) ulHead[j];
            basisTime[j] = row;
        }

        /*
         * knowing the actual start and end times for the multiple time series,
         * loop thru the time series and adjust the start and end times so that
         * they reflect the time span to use; i.e. there is no overlap. THIS IS
         * THE KEY STEP IN THE PROCESS OF DEFINING AN AGGREGATE VIRTUAL TIME
         * SERIES!!!
         */
        Object[] tmp = adjustStartEnd(ulCount, basisTime, startTime, endTime);
        startTime = (Timestamp[]) tmp[0];
        endTime = (Timestamp[]) tmp[1];

        /*
         * loop thru the complete retrieved time series and only keep the value
         * if it lies between the start and end time for this basis time
         */
        for (int i = 0; i < fcstCount; i++) {
            fcstValidTime = new Timestamp(fcstHead[i].getId().getValidtime()
                    .getTime());
            if ((fcstValidTime.compareTo(startTime[basisIndex[i]]) >= 0)
                    && (fcstValidTime.compareTo(endTime[basisIndex[i]]) <= 0)) {
                doKeep[i] = 1;
            } else {
                doKeep[i] = 0;
            }
        }
        return doKeep;
    }

    /**
     * This method uses the time series with the latest basis time first, and
     * uses it in its entirety. Then the time series with the next latest basis
     * time is used. If it overlaps portions of the already saved time series,
     * then only that portion which doesn't overlap is used. This process
     * continues until all time series have been considered. In essences, this
     * method adjoins adjacent time series.
     */
    private Object[] adjustStartEnd(int count, Timestamp[] basisTime,
            Timestamp[] startValidTime, Timestamp[] endValidTime) {
        boolean found = false;
        int currentIndex = 0;
        int[] basisOrder = new int[count];
        Timestamp fullStartValidTime = null;
        Timestamp fullEndValidTime = null;
        Timestamp tmpTime = null;
        Timestamp zero = new Timestamp((new Date(0)).getTime());
        Object[] rval = new Object[2]; // [startValidTime[]] [endValidTime[]]

        for (int i = 0; i < count; i++) {
            basisOrder[i] = -1;
        }

        /*
         * find the order of the time series by their latest basis time. if two
         * time series have the same basis time, use the one that has the
         * earlier starting time. note that the order is such that the latest
         * basis time is last in the resulting order array.
         */

        for (int i = 0; i < count; i++) {
            tmpTime = zero;
            currentIndex = 0;

            for (int j = 0; j < count; j++) {
                /*
                 * only consider the time series if it hasn't been accounted for
                 * in the order array
                 */
                found = false;

                for (int k = 0; k < i; k++) {
                    if (j == basisOrder[k]) {
                        found = true;
                        break;
                    }
                }

                if (!found) {
                    if (basisTime[j].compareTo(tmpTime) > 0) {
                        currentIndex = j;
                        tmpTime = basisTime[j];
                    } else if (basisTime[j].compareTo(tmpTime) == 0) {
                        if (startValidTime[j]
                                .compareTo(startValidTime[currentIndex]) < 0) {
                            currentIndex = j;
                            tmpTime = basisTime[j];
                        }
                    }
                }
            }

            basisOrder[i] = currentIndex;
        }

        /*
         * do NOT adjust the start and end time of the time series with the
         * latest ending time. loop through all the other time series and adjust
         * their start and end times as necessary so that they do not overlap
         * the time limits of the being-built aggregate time series.
         */

        currentIndex = basisOrder[0];
        fullStartValidTime = startValidTime[currentIndex];
        fullEndValidTime = endValidTime[currentIndex];

        for (int i = 1; i < count; i++) {
            currentIndex = basisOrder[i];

            /*
             * each additional time series being considered is checked to see if
             * it falls outside the time window already encompassed by the
             * assembled time series. there are four cases that can occur; each
             * is handled below.
             */

            if ((startValidTime[currentIndex].compareTo(fullStartValidTime) >= 0)
                    && (endValidTime[currentIndex].compareTo(fullEndValidTime) <= 0)) {
                /*
                 * if the basis time series being considered is fully within the
                 * time of the already existing time series, then ignore it
                 * completely, and reset its times.
                 */
                startValidTime[currentIndex] = zero;
                endValidTime[currentIndex] = zero;
            } else if ((startValidTime[currentIndex]
                    .compareTo(fullStartValidTime) <= 0)
                    && (endValidTime[currentIndex].compareTo(fullEndValidTime) >= 0)) {
                /*
                 * if the basis time series being considered covers time both
                 * before and after the existing time series, use the portion of
                 * it that is before the time series. it is not desirable to use
                 * both the before and after portion (this results in a
                 * non-contiguous time-series that is weird), and given a choice
                 * it is better to use the forecast data early on than the later
                 * forecast data, so use the before portion
                 */
                endValidTime[currentIndex] = new Timestamp(
                        fullStartValidTime.getTime() - 1000);
                fullStartValidTime = startValidTime[currentIndex];

            } else if ((startValidTime[currentIndex]
                    .compareTo(fullStartValidTime) <= 0)
                    && (endValidTime[currentIndex].compareTo(fullEndValidTime) <= 0)) {
                /*
                 * if the basis time series being considered straddles the
                 * beginning or is completely before the existing time series,
                 * then use the portion of it that is before the time series.
                 */
                endValidTime[currentIndex] = new Timestamp(
                        fullStartValidTime.getTime() - 1000);
                fullStartValidTime = startValidTime[currentIndex];
            } else if ((startValidTime[currentIndex]
                    .compareTo(fullStartValidTime) >= 0)
                    && (endValidTime[currentIndex].compareTo(fullEndValidTime) >= 0)) {
                /*
                 * if the basis time series being considered straddles the end
                 * or is completely after the existing time series, then use the
                 * portion of it that is after the time series.
                 */
                startValidTime[currentIndex] = new Timestamp(
                        fullEndValidTime.getTime() + 1000);
                fullEndValidTime = endValidTime[currentIndex];

            }
        } /* end for loop on the unique ordered basis times */

        // Need to find a better way to do this
        rval[0] = startValidTime;
        rval[1] = endValidTime;
        return rval;
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
     * For a given location and pe code and type-source prefix, this function
     * returns the type-source code with the lowest rank in IngestFilter.
     * Alternatively, if a specific ordinal number is passed, then the Nth
     * ranking ts is returned. If no (<= 0) ordinal number (i.e. 1st, 2nd) is
     * requested, then the highest rank (1st) is returned. The type-source
     * prefix is normally given as a one-character string, R for observed data
     * and F for forecast data.
     * 
     * The function argument returns a status variable indicating whether the
     * request was satisfied.
     * 
     */
    private String getBestTs(String lid, String pe, String tsPrefix, int ordinal) {
        int count = 0;
        String tsFound = null;
        String query = "SELECT ts_rank,ts FROM ingestfilter " + "WHERE lid = '"
                + lid + "' AND " + "pe = '" + pe + "' AND " + "ts like '"
                + tsPrefix + "' AND " + "ingest = 'T' "
                + "ORDER BY ts_rank, ts";
        CoreDao dao = null;
        Object[] oa = null;
        try {
            /*
             * get the ingest filter entries for this location. note that the
             * retrieval is ordered so that if multiple best ranks exist, there
             * is some predicatibility for the identified best one. also note
             * that this approach ignores the duration, extremum, and probabilty
             * code.
             */
            dao = new CoreDao(DaoConfig.forDatabase(ShefConstants.IHFS));

            oa = dao.executeSQLQuery(query);
            Object[] row = null;
            if ((oa != null) && (oa.length > 0)) {
                /*
                 * if no specific ordinal number was requested, return with the
                 * highest rank.
                 */
                if (ordinal <= 0) {
                    row = (Object[]) oa[0];
                    tsFound = ShefUtil.getString(row[1], null);
                } else {
                    /*
                     * get a count of the number of matching ts entries. if the
                     * requested ordinal number is greater than the number
                     * available then return with a not found status.
                     */

                    count = oa.length;

                    if (ordinal <= count) {
                        row = (Object[]) oa[ordinal - 1];
                        tsFound = ShefUtil.getString(row[1], null);
                    }
                }
            }
        } catch (Exception e) {
            log.error("Query = [" + query + "]");
            log.error(shefRecord.getTraceId()
                    + " - PostgresSQL error retrieving from ingestfilter");
            if(log.isDebugEnabled()) {
                log.error(e);
            }
        }
        return tsFound;
    }

    /**
     * Checks if location data should be posted. 4 possible return values:
     * Location defined as location - 0 Location defined as geoarea - 1 Location
     * defined but don't post - 2 Location undefined - 3
     * 
     * @param locId
     *            - Location Id to check
     * @return Location corresponding to 1 of 4 return values
     */
    private Location checkLocation(String locId) {
        if (log.isDebugEnabled()) {
            log.debug("PostShef.checkLocation() called...");
        }
        Location retVal = Location.LOC_UNDEFINED;
        CoreDao dao = null;
        String sql = null;
        try {
            dao = new CoreDao(DaoConfig.forDatabase(ShefConstants.IHFS));
            sql = "select lid, post from location where lid = '" + locId + "'";
            if (log.isDebugEnabled()) {
                log.debug("SQL Query = " + sql);
            }
            // TODO fix multiple results returned error
            Object[] oa = dao.executeSQLQuery(sql);
            if (log.isDebugEnabled()) {
                log.debug(oa.length + " elements in oa");
            }
            if (oa.length > 0) {
                Object[] oa2 = (Object[]) oa[0];
                int post = ShefUtil.getInt(oa2[1], 0);

                retVal = (post == 1) ? Location.LOC_LOCATION
                        : Location.LOC_NO_POST;

            } else {
                sql = "select area_id from GeoArea where area_id = '" + locId
                        + "'";
                if (log.isDebugEnabled()) {
                    log.debug("Sql Query = " + sql);
                }
                oa = dao.executeSQLQuery(sql);
                if (oa.length > 0) {
                    retVal = Location.LOC_GEOAREA;
                }
            }
        } catch (Exception e) {
            log.error("Query = [" + sql + "]");
            log.error(shefRecord.getTraceId() + " - Error checking location");
            if(log.isDebugEnabled()) {
                log.error(e);
            }
        }
        return retVal;
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
    private IngestSwitch checkIngest(String locId, ShefData data,
            ShefConstants.IngestSwitch ingestSwitch) {
        if (log.isDebugEnabled()) {
            log.debug("PostShef.checkIngest() called...");
        }
        StringBuffer errorMsg = new StringBuffer();
        CoreDao dao = null;
        CoreDao locDao = null;
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

        try {
            errorMsg.append("Error getting connection to IHFS Database");
            dao = new CoreDao(DaoConfig.forDatabase(ShefConstants.IHFS));
            sql = "select lid, pe, dur, ts, extremum, ts_rank, ingest, ofs_input, "
                    + "stg2_input from IngestFilter where lid = '"
                    + locId
                    + "'";
            if (log.isDebugEnabled()) {
                log.debug("SQL Query = " + sql);
            }
            errorMsg.setLength(0);
            errorMsg.append("Error requesting IngestFilter data:  " + sql);
            Object[] oa = dao.executeSQLQuery(sql);
            if (oa.length > 0) {
                for (int i = 0; i < oa.length; i++) {
                    Object[] oa2 = (Object[]) oa[i];
                    String pe = ShefUtil.getString(oa2[1],"");
                    int dur = ShefUtil.getInt(oa2[2],-9999);
                    String ts = ShefUtil.getString(oa2[3],"");
                    String extremum = ShefUtil.getString(oa2[4],"");
                    // int tsRank = (Short) oa2[5];
                    String ingest = ShefUtil.getString(oa2[6],"");
                    // String ofs_input = (String) oa2[7];
                    String stg2_input = ShefUtil.getString(oa2[8],"");

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
                            if (elgMess) {
                                if (log.isDebugEnabled()) {
                                    log.debug(locId + " - "
                                            + data.getPhysicalElement() + "("
                                            + data.getDuration() + ")"
                                            + data.getTypeSource()
                                            + data.getExtremum()
                                            + " ingest filter set to False");
                                }
                            }
                            ingestSwitch = ShefConstants.IngestSwitch.POST_PE_OFF;
                        }
                        matchFound = true;
                        break;
                    }
                }
            }
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

                if ((data.getPhysicalElement() == PhysicalElement.PRECIPITATION_ACCUMULATOR)
                        || (data.getPhysicalElement() == PhysicalElement.PRECIPITATION_INCREMENT)) {
                    ingestSwitch = ShefConstants.IngestSwitch.POST_PE_AND_HOURLY;
                    ingestFilter.setStg2Input("T");
                } else {
                    ingestSwitch = ShefConstants.IngestSwitch.POST_PE_ONLY;
                    ingestFilter.setStg2Input("F");
                }

                /* insert the record */
                errorMsg.setLength(0);
                errorMsg.append("PostgreSQL error putting data into IngestFilter");
                dao.saveOrUpdate(ingestFilter);
                prevLid = locId;

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

                if (recordCount("Dcp", where) > 0) {
                    dcpFound = true;
                }

                if (recordCount("Observer", where) > 0) {
                    obsFound = true;
                }

                if (recordCount("RpfFcstPoint", where) > 0) {
                    fpFound = true;
                }

                if (recordCount("Reservoir", where) > 0) {
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
                errorMsg.append("Error getting PE codes from IngestFilter: "
                        + sql);
                sql = "select pe from IngestFilter where lid = '" + locId
                        + "' and ingest = 'T'";
                oa = dao.executeSQLQuery(sql);

                if (oa.length > 0) {
                    hNum = checkPeMatch(oa,
                            PhysicalElementCategory.HEIGHT.getCode());
                    qNum = checkPeMatch(oa,
                            PhysicalElementCategory.DISCHARGE.getCode());

                    sNum = checkPeMatch(oa,
                            PhysicalElementCategory.SNOW.getCode());
                    tNum = checkPeMatch(oa,
                            PhysicalElementCategory.TEMPERATURE.getCode());

                    pNum = checkPeMatch(oa,
                            PhysicalElementCategory.PRECIPITATION.getCode());
                    paNum = checkPeMatch(oa,
                            PhysicalElement.PRESSURE_ATMOSPHERIC.getCode());
                    pNum = pNum - paNum;

                    numPe = oa.length;

                    /*
                     * also, a station is a reservoir if it has a param type of
                     * HP or HT or LS
                     */
                    if ((checkPeMatch(oa,
                            PhysicalElement.ELEVATION_POOL.getCode()) > 0)
                            || (checkPeMatch(oa,
                                    PhysicalElement.ELEVATION_PROJECT_TAIL
                                            .getCode()) > 0)
                            || (checkPeMatch(oa,
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
                StringBuffer sb = new StringBuffer();

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

                locDao = new CoreDao(DaoConfig.forClass(ShefConstants.IHFS,
                        com.raytheon.uf.common.dataplugin.shef.tables.Location.class));

                List<String> fields = new ArrayList<String>();
                List<Object> values = new ArrayList<Object>();
                fields.add("lid");
                values.add(locId);

                List<?> queryResult = locDao.queryByCriteria(fields, values);
                com.raytheon.uf.common.dataplugin.shef.tables.Location loc = null;
                if (queryResult.size() > 0) {
                    loc = (com.raytheon.uf.common.dataplugin.shef.tables.Location) queryResult
                            .get(0);
                }
                stnClass.setLocation(loc);
                // stnClass.setObserver();
                stnClass.setTraceId(shefRecord.getTraceId());

                errorMsg.setLength(0);
                errorMsg.append("Error on saveOrUpdate stnclass table: " + sql);
                dao.saveOrUpdate(stnClass);
                /* since a record was added, set the match_found variable */
                matchFound = true;
           }
        } catch (Exception e) {
            log.error("Query = [" + sql + "]");
            log.error(shefRecord.getTraceId() + " - " + errorMsg.toString());
            if(log.isDebugEnabled()) {
                log.error(e);
            }
            stats.incrementErrorMessages();
        }

        // ***************************************************

        if (!matchFound) {
            if (ingestMess) {
                log.warn(locId + " - " + data.getPhysicalElement() + "("
                        + data.getDuration() + ")" + data.getTypeSource()
                        + data.getExtremum() + " ingest "
                        + "filter not defined");
            }
            stats.incrementWarningMessages();
            ingestSwitch = ShefConstants.IngestSwitch.POST_PE_OFF;
        }

        return ingestSwitch;
    }

    /**
     * Retrieves the number of records in the table based on the where clause
     * 
     * @param table
     *            - table to search
     * @param where
     *            - where clause to narrow the search
     * @return - number of records in the table
     */
    private int recordCount(String table, String where) {
        if (log.isDebugEnabled()) {
            log.debug("PostShef.recordCount() called...");
        }
        int retVal = 0;
        CoreDao dao = null;
        StringBuffer sql = new StringBuffer("Select count(*) from " + table);
        if (where != null) {
            sql.append(where);
        }
        try {
            dao = new CoreDao(DaoConfig.forDatabase(ShefConstants.IHFS));
            Object[] oa = dao.executeSQLQuery(sql.toString());
            retVal = ShefUtil.getInt(oa[0],0);
        } catch (Exception e) {
            log.error("Query = [" + sql.toString() + "]");
            log.error(shefRecord.getTraceId()
                    + " - An error occurred in recordCount:  " + table + " - "
                    + sql);
            if(log.isDebugEnabled()) {
                log.error(e);
            }
        }
        return retVal;
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
    private int checkPeMatch(Object[] oa, String findPeCode) {
        if (log.isDebugEnabled()) {
            log.debug("PostShef.checkPeMatch() called...");
        }
        int retVal = 0;
        if (oa.length > 0) {

            for (Object o : oa) {

                String lookIn = ShefUtil.getString(o,"");
                if (lookIn.startsWith(findPeCode)) {
                    retVal++;
                }

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
        if (log.isDebugEnabled()) {
            log.debug("PostShef.adjustRawValue() called...");
        }
        double divisor = 1.0;
        double base = 0.0;
        double multiplier = 1.0;
        double adder = 0.0;

        CoreDao dao = null;
        String sql = null;
        try {
            /* Get a Data Access Object */
            dao = new CoreDao(DaoConfig.forDatabase(ShefConstants.IHFS));

            sql = "select divisor, base, multiplier, adder from "
                    + "adjustfactor ";
            
            String where = "where lid = '" + locId + "' and pe = '"
                    + data.getPhysicalElement().getCode() + "' and dur = "
                    + data.getDurationValue() + " and ts = '"
                    + data.getTypeSource().getCode() + "' and extremum = '"
                    + data.getExtremum().getCode() + "'";
            
            sql = sql + where;
            if (log.isDebugEnabled()) {
                log.debug("value adjustment query [" + sql + "]");
            }
            Object[] oa = dao.executeSQLQuery(sql);
            if (oa.length > 0) {
                Object[] oa2 = (Object[]) oa[0];

                /* if Correction Factor divisor value is NULL, set it to 1.0 */
                divisor = ShefUtil.getDouble(oa2[0], 1.0);
                // if divisor is ZERO, set it to 1.0, DON'T WANT TO DIVIDE BY
                // ZERO
                if (divisor == 0) {
                    log.error("Divisor = 0.0 in adjustfactor " + where);
                    divisor = 1;
                }
                base = ShefUtil.getDouble(oa2[1], 0.0);
                multiplier = ShefUtil.getDouble(oa2[2], 1.0);
                adder = ShefUtil.getDouble(oa2[3], 0.0);

                /*
                 * calculate adjusted value using an equation similar to
                 * HydroMet
                 */
                data.adjustValue(divisor, base, multiplier, adder);

                if (dataLog) {
                    log.info(locId + " Adjusting Value");
                }
            }
        } catch (Exception e) {
            log.error("Query = [" + sql + "]");
            log.error(shefRecord.getTraceId() + " - Error adjusting raw value");
            if(log.isDebugEnabled()) {
                log.error(e);
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
        if (log.isDebugEnabled()) {
            log.debug("PostShef.postProductLink() called...");
        }
        CoreDao dao = null;
        PersistableDataObject link = null;
        try {
            /* Get a Data Access Object */
            dao = new CoreDao(DaoConfig.forDatabase(ShefConstants.IHFS));

            link = new Productlink(new ProductlinkId(locId, productId, obsTime,
                    postDate));

            if (log.isDebugEnabled()) {
                log.debug("Storing data to Productlink table for ProductId "
                        + productId);
            }

            dao.saveOrUpdate(link);
        } catch (Exception e) {
            log.error(shefRecord.getTraceId()
                    + " - Error writing to productlink table(" + locId + ", "
                    + productId + ", " + obsTime.toString() + ")");
            if(log.isDebugEnabled()) {
                log.error(e);
            }
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
        if (log.isDebugEnabled()) {
            log.debug("PostShef.checkQuality() called...");
        }
        double missing = ShefConstants.SHEF_MISSING_INT;

        long qualityCode = ShefConstants.DEFAULT_QC_VALUE;
        String monthdaystart = null;
        String monthdayend = null;
        double grossRangeMin = missing;
        double grossRangeMax = missing;
        double reasonRangeMin = missing;
        double reasonRangeMax = missing;
        double alertUpperLimit = missing;
        double alarmUpperLimit = missing;
        double alertLowerLimit = missing;
        double alarmLowerLimit = missing;

        alertAlarm = ShefConstants.NO_ALERTALARM;

        if (log.isDebugEnabled()) {
            log.debug("DataValue = " + dataValue);
        }
        double dValue = 0;

        // if the dataValue = -9999 (missing data)
        if (dataValue.equals(ShefConstants.SHEF_MISSING)) {
            return ShefConstants.QC_MANUAL_FAILED;
        }

        try {
            dValue = Double.parseDouble(dataValue);
        } catch (NumberFormatException e) {
            log.error("Double conversion failed for data value = '" + dataValue
                    + "'");
        }

        
        boolean locRangeFound = false;
        boolean defRangeFound = false;
        boolean validDateRange = false;

        CoreDao dao = null;
        StringBuilder locLimitSql = new StringBuilder();
        StringBuilder defLimitSql = null;
        try {
            /* Get a Data Access Object */
            dao = new CoreDao(DaoConfig.forDatabase(ShefConstants.IHFS));

            String sqlStart = "select monthdaystart, monthdayend, gross_range_min, gross_range_max, reason_range_min, "
                    + "reason_range_max, roc_max, alert_upper_limit, alert_roc_limit, alarm_upper_limit, "
                    + "alarm_roc_limit, alert_lower_limit, alarm_lower_limit, alert_diff_limit, "
                    + "alarm_diff_limit, pe, dur from ";

            locLimitSql.append(sqlStart);
            locLimitSql.append("locdatalimits where ");
            locLimitSql.append("lid = '" + lid + "' and pe = '"
                    + data.getPhysicalElement().getCode() + "' and " + "dur = "
                    + data.getDurationValue());

            if (log.isDebugEnabled()) {
                log.debug("LocLimit query [" + locLimitSql.toString() + "]");
            }
            Object[] oa = dao.executeSQLQuery(locLimitSql.toString());

            if (oa.length > 0) { // Location specific range is defined
                if (log.isDebugEnabled()) {
                    log.debug("Found data in location specific range");
                }

                for (int i = 0; i < oa.length; i++) {
                    Object[] oa2 = (Object[]) oa[i];

                    /* Check the date range */
                    monthdaystart = ShefUtil.getString(oa2[0], "99-99");
                    monthdayend = ShefUtil.getString(oa2[1], "00-00");

                    validDateRange = checkRangeDate(
                            data.getObservationTimeObj(), monthdaystart,
                            monthdayend,log);

                    if (validDateRange) {
                        grossRangeMin =ShefUtil.getDouble(oa2[2], missing);
                        
                        grossRangeMax = ShefUtil.getDouble(oa2[3], missing);

                        reasonRangeMin = ShefUtil.getDouble(oa2[4], missing);

                        reasonRangeMax = ShefUtil.getDouble(oa2[5], missing);

                        alertUpperLimit = ShefUtil.getDouble(oa2[7], missing);

                        alertLowerLimit = ShefUtil.getDouble(oa2[11], missing);

                        alarmLowerLimit = ShefUtil.getDouble(oa2[12], missing);

                        alarmUpperLimit = ShefUtil.getDouble(oa2[9], missing);

                        locRangeFound = true;
                        break;
                    }
                }
            } else { // Location specific range is undefined, check the
                // default range
                defLimitSql = new StringBuilder(sqlStart);
                defLimitSql.append("datalimits where ");
                defLimitSql.append("pe = '" + data.getPhysicalElement().getCode()
                        + "' and " + "dur = " + data.getDurationValue());

                oa = dao.executeSQLQuery(defLimitSql.toString());
                if (oa.length > 0) { // Default range is defined
                    if (log.isDebugEnabled()) {
                        log.debug("Found data in default range");
                    }

                    for (int i = 0; i < oa.length; i++) {
                        Object[] oa2 = (Object[]) oa[i];

                        /* Check the date range */
                        monthdaystart = ShefUtil.getString(oa2[0], "99-99");
                        monthdayend = ShefUtil.getString(oa2[1], "00-00");

                        validDateRange = checkRangeDate(
                                data.getObservationTimeObj(), monthdaystart,
                                monthdayend,log);

                        if (validDateRange) {
                            /*
                             * if a range is found, then check the value and set
                             * the flag
                             */
                            grossRangeMin =ShefUtil.getDouble(oa2[2], missing);
                            
                            grossRangeMax = ShefUtil.getDouble(oa2[3], missing);

                            reasonRangeMin = ShefUtil.getDouble(oa2[4], missing);

                            reasonRangeMax = ShefUtil.getDouble(oa2[5], missing);

                            alertUpperLimit = ShefUtil.getDouble(oa2[7], missing);

                            alertLowerLimit = ShefUtil.getDouble(oa2[11], missing);

                            alarmLowerLimit = ShefUtil.getDouble(oa2[12], missing);

                            alarmUpperLimit = ShefUtil.getDouble(oa2[9], missing);

                            defRangeFound = true;
                            break;
                        }
                    }
                }
            }

            if (locRangeFound || defRangeFound) {
                /*
                 * if a range is found, then check the value and set the flag
                 */
                if (((grossRangeMin != missing) && (dValue < grossRangeMin))
                        || ((grossRangeMax != missing) && (dValue > grossRangeMax))) {
                    qualityCode = ShefQC.setQcCode(
                            (int) ShefConstants.QC_GROSSRANGE_FAILED,
                            qualityCode);

                    if (dataLog) {
                        log.info(lid + " failed gross range check: " + dValue
                                + " out of range " + grossRangeMin + " - "
                                + grossRangeMax);
                    }

                    /*
                     * don't do anything if it fails the gross range check
                     */
                } else {
                    if (((reasonRangeMin != missing) && (dValue < reasonRangeMin))
                            || ((reasonRangeMax != missing) && (dValue > reasonRangeMax))) {
                        qualityCode = ShefQC.setQcCode(
                                (int) ShefConstants.QC_REASONRANGE_FAILED,
                                qualityCode);
                        if (dataLog) {
                            log.info(lid + " failed reasonable range check: "
                                    + dValue + " out of range "
                                    + reasonRangeMin + " - " + reasonRangeMax);
                        }
                    }

                    /*
                     * if fails alert alarm then set a variable value which will
                     * later trigger a write of the record to AlertAlarmVal
                     * table.
                     */
                    if (shefAlertAlarm) {
                        if ((alarmUpperLimit != missing)
                                && (dValue >= alarmUpperLimit)) {
                            alertAlarm = ShefConstants.ALARM_UPPER_DETECTED;
                        } else if ((alertUpperLimit != missing)
                                && (dValue >= alertUpperLimit)) {
                            alertAlarm = ShefConstants.ALERT_UPPER_DETECTED;
                        } else if ((alarmLowerLimit != missing)
                                && (dValue <= alarmLowerLimit)) {
                            alertAlarm = ShefConstants.ALARM_LOWER_DETECTED;
                        } else if ((alertLowerLimit != missing)
                                && (dValue <= alertLowerLimit)) {
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
            if ((dataQualifier != null) && !"Z".equalsIgnoreCase(dataQualifier)) {
                if ("Q".equalsIgnoreCase(dataQualifier)
                        || "F".equalsIgnoreCase(dataQualifier)) {
                    qualityCode = ShefQC.setQcCode(
                            (int) ShefConstants.QC_EXTERN_QUEST, qualityCode);
                } else if ((dataQualifier != null)
                        && ("R".equalsIgnoreCase(dataQualifier) || "B"
                                .equalsIgnoreCase(dataQualifier))) {
                    qualityCode = ShefQC.setQcCode(
                            (int) ShefConstants.QC_EXTERN_FAILED, qualityCode);
                }
                if (dataLog) {
                    log.info(lid + " Data Qualifier of " + dataQualifier);
                }
            }
        } catch (Exception e) {
            log.error("Error in checkQuality() for " + shefRecord.getTraceId(),e);
            log.info("locdatalimits query = [" + locLimitSql.toString() + "]");
            log.info("datalimits query  = [" + defLimitSql.toString() + "]");
            if(log.isDebugEnabled()) {
                log.error(e);
            }
            stats.incrementErrorMessages();
        }

        return qualityCode;
    }

    /**
     * Determine if the qualityCode passed in is of "Higher" quality than the
     * checkCode passed in
     * 
     * @param checkCode
     *            - code to check against
     * @param qualityCode
     *            - code to check
     * @return true if the qualityCode is of "Higher" quality
     */
    private boolean checkQcCode(QualityControlCode checkCode, long qualityCode) {
        if (log.isDebugEnabled()) {
            log.debug("PostShef.checkQcCode() called...");
        }
        boolean returnValue = false;
        switch (checkCode) {
        case QC_DEFAULT:
            returnValue = (qualityCode == ShefConstants.DEFAULT_QC_VALUE);
            break;
        case QC_PASSED:
            returnValue = (qualityCode > ShefConstants.GOOD_QUESTIONABLE_THRESHOLD);
            break;
        case QC_QUESTIONABLE:
            returnValue = ((qualityCode >= ShefConstants.QUESTIONABLE_BAD_THRESHOLD) && (qualityCode < ShefConstants.GOOD_QUESTIONABLE_THRESHOLD));
            break;
        case QC_ROC_PASSED:
            returnValue = BitUtils
                    .checkQcBit(ShefConstants.ROC_QC, qualityCode);
            break;
        case QC_OUTLIER_PASSED:
            returnValue = BitUtils.checkQcBit(ShefConstants.OUTLIER_QC,
                    qualityCode);
            break;
        case QC_SCC_PASSED:
            returnValue = BitUtils
                    .checkQcBit(ShefConstants.SCC_QC, qualityCode);
            break;
        case QC_MSC_PASSED:
            returnValue = BitUtils
                    .checkQcBit(ShefConstants.MSC_QC, qualityCode);
            break;
        case QC_FAILED:
            returnValue = qualityCode < ShefConstants.QUESTIONABLE_BAD_THRESHOLD;
            break;
        case QC_NOT_FAILED:
            returnValue = (qualityCode >= ShefConstants.QUESTIONABLE_BAD_THRESHOLD);
            break;
        case QC_NOT_PASSED:
            returnValue = (qualityCode <= ShefConstants.GOOD_QUESTIONABLE_THRESHOLD);
            break;
        default:
            log.error("Invalid request made in checkQcCode() method.");
            // returnValue = ShefConstants.INVALID_QC_REQUEST;
            returnValue = false;
            break;
        }
        return returnValue;
    }

    /**
     * check_if_paired()
     * 
     * Check if the SHEF record being processed is for a physical element that
     * has data for a special paired-and-dependent set of data.
     */
    private boolean checkIfPaired(ShefData data) {
        if (log.isDebugEnabled()) {
            log.debug("PostShef.checkIfPaired() called...");
        }
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
            String monthDayEnd, Log log) {
        boolean valid = false;
        if (log != null && log.isDebugEnabled()) {
            log.debug("PostShef.checkRangeDate() ...");
        }
        if(obsTime != null) {
            if((monthDayStart != null)&&(monthDayEnd != null)) {
                if((monthDayStart.length() == 5)&&(monthDayEnd.length() == 5)) {
                    
                    int rangeStartDate = Integer.parseInt(monthDayStart.substring(0, 2)) * 100;
                    rangeStartDate += Integer.parseInt(monthDayStart.substring(3));
                    
                    int rangeEndDate = Integer.parseInt(monthDayEnd.substring(0, 2)) * 100;
                    rangeEndDate += Integer.parseInt(monthDayEnd.substring(3));
                    
                    Calendar date = TimeTools.getSystemCalendar();
                    date.setTime(obsTime);

                    int dataDate = (date.get(Calendar.MONTH) + 1) * 100;
                    dataDate += date.get(Calendar.DAY_OF_MONTH);

                    /* Compare the dates, don't check for straddling the year */
                    valid = ((dataDate >= rangeStartDate) && (dataDate <= rangeEndDate));
                }            
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

        if (ShefConstants.COMMENT_VALUE.equalsIgnoreCase(tableName)) {
            Commentvalue comment = new Commentvalue(new CommentvalueId());
            Date basisTime = data.getCreationDateObj();
            if (basisTime == null) {
                basisTime = new Date();
            }

            if (dataValue == "") {
                dataValue = "-9999";
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
            comment.setTraceId(shefRecord.getTraceId());
            comment.getId().setTs(data.getTypeSource().getCode());
            comment.setValue(Double.parseDouble(dataValue));
            comment.getId().setProbability(
                    (float) data.getProbability().getValue());
            comment.getId().setBasistime(basisTime);
            comment.setShefComment(data.getRetainedComment());
            dataObj = comment;
        } else if (ShefConstants.CONTINGENCY_VALUE.equalsIgnoreCase(tableName)) {
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
            contingency.setTraceId(shefRecord.getTraceId());
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
            proc.setIdentifier(shefRecord.getIdentifier());
            proc.getId().setLid(locId);
            proc.getId().setPe(data.getPhysicalElement().getCode());
            proc.setPostingtime(postDate);
            proc.setProductId(prodId);
            proc.setProducttime(prodTime);
            proc.setQualityCode((int) qualityCode);
            proc.setRevision(revision);
            proc.setShefQualCode(qualifier);
            proc.setTraceId(shefRecord.getTraceId());
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
            rejectData.setTraceId(shefRecord.getTraceId());
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
            aa.setIdentifier(shefRecord.getIdentifier());
            aa.getId().setLid(locId);
            aa.getId().setValidtime(data.getObservationTimeObj());
            aa.getId().setPe(data.getPhysicalElement().getCode());
            aa.setPostingtime(postDate);
            aa.setProductId(prodId);
            aa.setProducttime(prodTime);
            aa.setQualityCode((int) qualityCode);
            aa.setRevision(revision);
            aa.setShefQualCode(qualifier);
            aa.setTraceId(shefRecord.getTraceId());
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
            arealOb.setTraceId(shefRecord.getTraceId());
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
            arealfcst.setTraceId(shefRecord.getTraceId());
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
            unkstnvalue.getId().setObstime(new Timestamp(basisTime.getTime()));
            unkstnvalue.getId().setValue(Double.parseDouble(dataValue));
            unkstnvalue.setIdentifier(unkstnvalue.getId());
            unkstnvalue.getId().setIdentifier(unkstnvalue.getId());
            unkstnvalue.getId().setRevision(revision);
            // unkstnvalue.getId().setShefQualCode(qualifier);
            unkstnvalue.getId().setShefQualCode("Z");
            unkstnvalue.getId().setProductId(prodId);
            unkstnvalue.getId().setProducttime(prodTime);
            unkstnvalue.getId().setPostingtime(postDate);
            unkstnvalue.setTraceId(shefRecord.getTraceId());

            dataObj = unkstnvalue;
        }

        return dataObj;
    }

    
    /**
     * 
     * @param c
     * @return
     */
    private static String toTimeStamp(Date d) {
        String timeStamp = null;
        if(d != null) {
            timeStamp = DB_TIMESTAMP.format(d);
        }
        return timeStamp;
    }

    /**
     * 
     * @param c
     * @return
     */
    private static String toTimeStamp(Calendar c) {
        return toTimeStamp(c.getTime());
    }
    
    public static final void main(String[] args) {

        Calendar postDate = TimeTools.getBaseCalendar(2011, 1, 12);
        postDate.set(Calendar.HOUR_OF_DAY, 17);
        postDate.set(Calendar.MINUTE, 25);

        Calendar obsTimef = TimeTools.getBaseCalendar(2011, 1, 12);
        obsTimef.set(Calendar.HOUR_OF_DAY, 17);
        obsTimef.set(Calendar.MINUTE, 25);
        obsTimef.add(Calendar.DAY_OF_MONTH, -30);

        Calendar obsTimeb = TimeTools.getBaseCalendar(2011, 1, 12);
        obsTimeb.set(Calendar.HOUR_OF_DAY, 17);
        obsTimeb.set(Calendar.MINUTE, 25);
        obsTimeb.add(Calendar.MINUTE, 10);
        obsTimeb.set(Calendar.SECOND, 1);

        long lookbackMillis = 30 * ShefConstants.MILLIS_PER_DAY;

        long lookfwdMillis = 10 * ShefConstants.MILLIS_PER_MINUTE;

        long difff = postDate.getTimeInMillis() - obsTimef.getTimeInMillis();
        long diffb = obsTimeb.getTimeInMillis() - postDate.getTimeInMillis();

        System.out.println(difff + "  " + lookbackMillis);
        System.out.println(difff > lookbackMillis);

        System.out.println(diffb + "  " + lookfwdMillis);
        System.out.println(diffb > lookfwdMillis);
        
        
        SimpleDateFormat sdf = new SimpleDateFormat("yyyyMMDDhhmmssZ");
        sdf.setTimeZone(SHEFTimezone.GMT_TIMEZONE);
        try {
            Date d = sdf.parse("20110228102100-0000");
            
            System.out.println(sdf.format(d));
            System.out.println(checkRangeDate(d, "01-01", "12-31", null) + " expected true");
            System.out.println(checkRangeDate(d, "03-01", "10-01", null) + " expected false");
            System.out.println(checkRangeDate(d, "99-99", "00-00", null) + " expected false");
        
        } catch (ParseException e) {
            e.printStackTrace();
       }

    }
}
