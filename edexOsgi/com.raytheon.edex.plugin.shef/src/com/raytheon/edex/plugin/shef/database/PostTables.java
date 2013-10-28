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

import java.sql.CallableStatement;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.Timestamp;
import java.util.Date;
import java.util.List;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.hibernate.connection.ConnectionProvider;
import org.hibernate.engine.SessionFactoryImplementor;

import com.raytheon.edex.plugin.shef.data.ShefData;
import com.raytheon.edex.plugin.shef.data.ShefRecord;
import com.raytheon.edex.plugin.shef.data.precip.PrecipRecord;
import com.raytheon.edex.plugin.shef.ohdlib.GagePP;
import com.raytheon.edex.plugin.shef.ohdlib.GagePPOptions;
import com.raytheon.edex.plugin.shef.ohdlib.GagePPOptions.shef_dup;
import com.raytheon.edex.plugin.shef.ohdlib.PrecipUtils;
import com.raytheon.edex.plugin.shef.util.PrecipitationUtils;
import com.raytheon.edex.plugin.shef.util.ShefStats;
import com.raytheon.uf.common.dataplugin.persist.PersistableDataObject;
import com.raytheon.uf.common.dataplugin.shef.tables.Alertalarmval;
import com.raytheon.uf.common.dataplugin.shef.tables.Arealfcst;
import com.raytheon.uf.common.dataplugin.shef.tables.Arealobs;
import com.raytheon.uf.common.dataplugin.shef.tables.Commentvalue;
import com.raytheon.uf.common.dataplugin.shef.tables.Contingencyvalue;
import com.raytheon.uf.common.dataplugin.shef.tables.Pairedvalue;
import com.raytheon.uf.common.dataplugin.shef.tables.PairedvalueId;
import com.raytheon.uf.common.dataplugin.shef.tables.Procvalue;
import com.raytheon.uf.common.dataplugin.shef.tables.Rejecteddata;
import com.raytheon.uf.common.dataplugin.shef.tables.Unkstn;
import com.raytheon.uf.common.dataplugin.shef.tables.Unkstnvalue;
import com.raytheon.uf.common.dataplugin.shef.util.ParameterCode.DataType;
import com.raytheon.uf.common.dataplugin.shef.util.ParameterCode.Duration;
import com.raytheon.uf.common.dataplugin.shef.util.ParameterCode.PhysicalElement;
import com.raytheon.uf.common.dataplugin.shef.util.ParameterCode.PhysicalElementCategory;
import com.raytheon.uf.common.dataplugin.shef.util.ShefConstants;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;

/**
 * Post the data to the IHFS database.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 05/02/2008   387        M. Duff     Initial Creation.	
 * 22Jul2008    1277       MW Fegan    Corrected connection logic in execFunction(). 
 * 10/16/2008   1548       jelkins     Integrated ParameterCode Types
 * 12/17/2008   1722       J. Sanchez  Updated postPeData to handle Forecast type.
 *                                      Added execFcstFunction method.
 *                                      Updated determineUpdateAction with IF_DIFFERENT.
 * 01/08/2009   1846       J. Sanchez  Updated execFcstFunction to handle basisDate
 * 02/02/2009   1943       J. Sanchez  Add method postRiverStatus.
 * 05/29/2009   2410       J. Sanchez  Posted data for unknstnvalue.
 * 02/24/2012   14535      W. Kwock    Correct the duration value.
 * 11/29/2012   15530      lbousaidi   corrected posting and production time for
 * 									   latestobsvalue table.
 * 
 * </pre>
 * 
 * @author mduff
 * @version 1.0
 */

public class PostTables {

    private static final Log log = LogFactory
            .getLog(com.raytheon.edex.plugin.shef.database.PostTables.class);

    private static GagePPOptions gagePPOptions;

    public static void PostTablesInit() {
        gagePPSetup();
    }
    
    /**
     * Post data to the latest observed table, if appropriate. Only post if it
     * is the latest data.
     * 
     * The check of whether to even attempt to post data into the latestobsvalue
     * table based on whether the data are valid (passed qc and not missing) is
     * expected to be performed by the calling function. This function may still
     * need to determine if the data should be updated based on the post_latest
     * flag and the data attributes.
     * 
     * prior to OB2, the LatestObsValue table was posted directly. In OB2, the
     * above method of using a stored procedure was implemented, which roughly
     * cut the time spent dealing with LatestObsValue in half. !!!
     * 
     * @param record
     *            - the SHEF record object
     * @param shefData
     *            - the SHEF data object
     * @param locId
     *            - the location id
     * @param dataValue
     *            - the data value as a String
     * @param qualifier
     *            - the qualifier
     * @param qualityCode
     *            - the quality code
     * @param productId
     *            - the product id
     * @param productTime
     *            - the product time
     * @param duplicateOption
     *            - option indicating to post duplicate data or not
     * @param stats
     *            - stats object
     */
    public static synchronized void postLatestObs(ShefRecord record,
            ShefData shefData, String locId, String dataValue,
            String qualifier, long qualityCode, String productId,
            Date productTime, String duplicateOption, ShefStats stats,
            Date postTime) {
        if (log.isDebugEnabled()) {
            log.debug("PostTables.postLatestObs() called...");
        }
        long start = 0;
        long end = 0;

        Date basisTime = shefData.getCreationDateObj();
        if (basisTime == null) {
            basisTime = new Date();

            /* Sets time to Jan 1 1970 */
            basisTime.setTime(0);
        }

        String procName = "latestobs";
        if (dataValue.equals("")) {
            dataValue = "-9999.0";
        }

        /* now call the PostgreSQL function */
        start = System.currentTimeMillis();
        int status = execFunction(procName, record, shefData, locId, dataValue,
                qualifier, qualityCode, productId, productTime, postTime,
                duplicateOption, stats);
        end = System.currentTimeMillis();
        if (log.isDebugEnabled()) {
            log.debug("ExecFunction(" + procName + ") completed");
            log.debug("PE Store took " + (end - start) + " milliseconds");
        }
        if (status < 0) {
            log.error(record.getTraceId() + " - PostgresSQL error " + status
                    + " executing " + procName + " function for " + locId
                    + ", " + shefData.getObservationTimeObj().toString() + ", "
                    + productTime.toString() + ", " + productId + ", "
                    + postTime.toString());
            stats.incrementErrorMessages();
        } else {
            end = System.currentTimeMillis();
            stats.addElapsedTimeIngest(end - start);
            stats.incrementLatestObs();
            if (log.isDebugEnabled()) {
                log.debug("Latest obs store took " + (end - start)
                        + " milliseconds");
            }
        }
    }

    /**
     * Load special paired-and-dependent data into the PairedValue table.
     * 
     * @param record
     * @param shefData
     * @param locId
     * @param dataValue
     * @param qualifier
     * @param qualityCode
     * @param productId
     * @param productTime
     * @param duplicateOption
     * @param stats
     */
    public static synchronized void postPairedData(ShefRecord record,
            ShefData shefData, String locId, String dataValue,
            String qualifier, long qualityCode, String productId,
            Date productTime, String duplicateOption, ShefStats stats,
            Date postTime) {
        if (log.isDebugEnabled()) {
            log.debug("PostTables.postPairedData() called...");
        }

        int refValue = -9999;
        boolean isNegative = false;
        double value = -9999;
        String pe = shefData.getPhysicalElement().getCode();
        short dur = Short.parseShort(shefData.getDuration().getValue() + "");
        String ts = shefData.getTypeSource().getCode();
        String extremum = shefData.getExtremum().getCode();
        float probability = Float.parseFloat(shefData.getProbability()
                .getValue() + "");
        Date validTime = shefData.getObservationTimeObj();

        // CONCERN : Is this the correct behavior when the basisTime is missing?
        Date basisTime = shefData.getCreationDateObj();
        if (basisTime == null) {
            basisTime = new Date(postTime.getTime());
        }

        long start = 0;
        long end = 0;
        if (dataValue == "") {
            dataValue = ShefConstants.SHEF_MISSING;
        }

        /*
         * check for missing data when processing the data. either both values
         * can be missing (-9999.) or the dependent value can be missing
         * (-#.999), which is the more meaningful case, since the data is
         * missing for some independent value, such as the depth at which the
         * dependent soil temperature is measured.
         */

        int leftValue = 0;
        double rightValue = 0;

        if (dataValue.equals(ShefConstants.SHEF_MISSING + ".")) {
            // Set both values to missing.
            leftValue = ShefConstants.SHEF_MISSING_INT;
            rightValue = ShefConstants.SHEF_MISSING_INT;
        } else {
            /*
             * split the incoming value into its two parts. always assume the
             * format is given as independent.dependent - i.e. the independent
             * is the value as the matissa (left of the decimal point). also,
             * the mantissa's first (leftmost) value is assumed to be the
             * hundreth place, the 2nd from left the tenths, and so on. for
             * example, the number -34.23467 translates to 34 and -234.67
             */
            String[] parts = dataValue.split("\\.");

            if (parts.length == 2) {
                leftValue = Integer.parseInt(parts[0]);
                if (leftValue == -9999) {
                    rightValue = -9999.0;
                } else {
                    rightValue = Double.parseDouble("0." + parts[1]);
                    if (leftValue < 0) {
                        leftValue *= -1;
                        rightValue *= -1;
                    }
                    rightValue *= 1000;
                    if (rightValue < -999.0) {
                        rightValue = -9999.0;
                    }
                }
            }
        }

        /* build record for input into the table */
        PairedvalueId id = new PairedvalueId();

        Pairedvalue pairedValue = new Pairedvalue(id);

        id.setBasistime(basisTime);
        id.setDur(Short.parseShort(shefData.getDuration().getValue() + ""));
        id.setExtremum(shefData.getExtremum().getCode());
        id.setLid(locId);
        id.setPe(shefData.getPhysicalElement().getCode());
        id.setProbability(Float.parseFloat(shefData.getProbability().getValue()
                + ""));

        pairedValue.setPostingtime(postTime);
        pairedValue.setProductId(productId);
        pairedValue.setProducttime(productTime);
        pairedValue.setQualityCode((int) qualityCode);
        id.setRefValue(leftValue);
        pairedValue.setValue(rightValue);

        pairedValue.setRevision(((short) (shefData.isRevisedRecord() ? 1 : 0)));

        pairedValue.setShefQualCode(qualifier);
        id.setTs(shefData.getTypeSource().getCode());
        id.setValidtime(shefData.getObservationTimeObj());

        CoreDao dao = null;
        StringBuilder sql = new StringBuilder();
        try {
            dao = new CoreDao(DaoConfig.forDatabase(ShefConstants.IHFS));
            // lid, pe, dur, ts, extremum, probability, validtime, basistime,
            // ref_value
            sql.append("select value from pairedvalue where lid = '" + locId
                    + "' and pe = '" + pe + "' and ");
            sql.append("dur = " + dur + " and ts = '" + ts
                    + "' and extremum = '" + extremum + "' and ");
            sql.append("probability = " + probability + " and validtime = '"
                    + ShefConstants.POSTGRES_DATE_FORMAT.format(validTime)
                    + "' and ");
            sql.append("basistime = '"
                    + ShefConstants.POSTGRES_DATE_FORMAT.format(basisTime)
                    + "' and ");
            sql.append("ref_value = " + refValue);

            Object[] result = dao.executeSQLQuery(sql.toString());

            if (result.length <= 0) {
                start = System.currentTimeMillis();
                dao.persist(pairedValue);
                end = System.currentTimeMillis();
                if (log.isDebugEnabled()) {
                    log.debug("Paired Value store took " + (end - start)
                            + " milliseconds");
                }
                stats.incrementPaired();
            } else {
                Double tableValue = (Double) result[0];
                int doOverwrite = determineUpdateAction(duplicateOption,
                        shefData.isRevisedRecord());

                if (doOverwrite > 0) {
                    start = System.currentTimeMillis();
                    switch (doOverwrite) {
                    case ShefConstants.UPDATE_ACTION:
                        dao.saveOrUpdate(pairedValue);
                        break;
                    case ShefConstants.IF_DIFFERENT_UPDATE_ACTION:
                        if (tableValue != Double.parseDouble(dataValue)) {
                            dao.saveOrUpdate(pairedValue);
                        }
                        break;
                    }
                    end = System.currentTimeMillis();
                    if (log.isDebugEnabled()) {
                        log.debug("Paired store took " + (end - start)
                                + " milliseconds");
                    }
                    stats.incrementPairedOver();

                    /* data was properly added to table */
                    stats.incrementRejected();
                } else {
                    if (AppsDefaults.getInstance()
                            .getBoolean(ShefConstants.DUP_MESSAGE, false)) {
                        log.info("Ignoring duplicate PairedValue for " + locId
                                + ", " + productId + ", "
                                + shefData.getObservationTime());
                    }
                    stats.incrementIgnored();
                }
            }
        } catch (Exception e) {
            log.error(record.getTraceId() + " - Error posting paired data");
            log.error("Query = [" + sql.toString() + "]");
            if(log.isDebugEnabled()) {
                log.error(e);
            }
            stats.incrementErrorMessages();
        }
    }

    /**
     * Post the PE data to the proper tables if appropriate
     * 
     * @param record
     * @param shefData
     * @param locId
     * @param dataValue
     * @param qualifier
     * @param qualityCode
     * @param productId
     * @param productTime
     * @param duplicateOption
     * @param dataType
     * @param ingestSwitch
     * @param stats
     */
    public static synchronized void postPeData(ShefRecord record,
            ShefData shefData, String locId, String dataValue,
            String qualifier, long qualityCode, String productId,
            Date productTime, String duplicateOption,
            ShefConstants.IngestSwitch ingestSwitch, ShefStats stats,
            Date validTime, Date postTime, DataType type) {

        if (log.isDebugEnabled()) {
            log.debug("PostTables.postPeData() called...");
        }

        String procName = null;

        if (DataType.READING.equals(type)) {
            /* see if the data is a precip report of some kind */
            int precipIndex = PrecipitationUtils.getPrecipitationIndex(shefData
                    .getPhysicalElement());

            /* post all non-precip observed data */
            if (precipIndex == ShefConstants.NOT_PRECIP) {
                procName = "obs_pe";

                if (log.isDebugEnabled()) {
                    log.debug("postPeData() procName = " + procName);
                }

                /* now call the PostgreSQL function */
            } else {
                procName = "obs_precip";

                if (log.isDebugEnabled()) {
                    log.debug("postPeData() procName = " + procName);
                }

                /*
                 * if gpp is enabled, and the switch for this record dictates,
                 * write a copy of any precip report near the top-of-the-hour to
                 * a file that will be sent to the gpp server after the product
                 * is fully processed. if PP, only consider hourly data.
                 */
                boolean gage_pp_enable = AppsDefaults.getInstance().getBoolean("gage_pp_enable", false);
                if (log.isDebugEnabled()) {
                    log.debug("gage_pp_enable = " + gage_pp_enable);
                    log.debug("ingestSwitch   = " + ingestSwitch);
                }

                if (gage_pp_enable 
                        && (ingestSwitch == ShefConstants.IngestSwitch.POST_PE_AND_HOURLY)) {

                    if (log.isDebugEnabled()) {
                        log.debug("gage_pp_enable && POST_PE_AND_HOURLY");
                    }

                    PrecipRecord precip = new PrecipRecord(shefData);
                    precip.setPostingTime(postTime);
                    precip.setQualCode(qualityCode);
                    precip.setProductId(productId);
                    precip.setProductTime(productTime);

                    PhysicalElement pe = shefData.getPhysicalElement();
                    
                    if ((PhysicalElement.PRECIPITATION_INCREMENT.equals(pe))
                            && ((shefData.getDuration() == Duration._1_DAY)
                                    || (shefData.getDuration() == Duration._1_PERIOD) || (shefData
                                    .getDuration() == Duration._6_HOUR))) {

                        PrecipitationUtils.writePrecipGpp(shefData, record,
                                qualityCode, productId, productTime, postTime,
                                locId, qualifier, dataValue);
                        writePrecip(precip);
                        stats.incrementPrecipGpp();
                    }
                    if ((PhysicalElement.PRECIPITATION_ACCUMULATOR.equals(pe))
                            || ((PhysicalElement.PRECIPITATION_INCREMENT.equals(pe)) && ((shefData
                                    .getDuration() == Duration._60_MINUTES) || (shefData
                                    .getDuration() == Duration._1_HOUR)))) {

                        if (dataValue.equals("")) {
                            dataValue = "-9999.0";
                        }
                        
                        if(PrecipUtils.checkPrecipWindow(shefData.getObsTime(), pe, gagePPOptions)) {
                            PrecipitationUtils.writePrecipGpp(shefData, record,
                                    qualityCode, productId, productTime, postTime,
                                    locId, qualifier, dataValue);
                            writePrecip(precip);
                            stats.incrementPrecipGpp();
                        }
                    }
                }
            }

        } else if (DataType.FORECAST.equals(type)) {
            procName = "fcst_pe";
        }

        long start = 0;
        long end = 0;
        int status = -1;

        if (DataType.FORECAST.equals(type)) {

            start = System.currentTimeMillis();

            status = execFcstFunc(procName, record, shefData, locId, dataValue,
                    qualifier, qualityCode, productId, productTime, postTime,
                    duplicateOption, ingestSwitch, stats, validTime);

            end = System.currentTimeMillis();

            if (log.isDebugEnabled()) {
                log.debug("ExecFunction(" + procName + ") completed");
                log.debug("PE Store took " + (end - start) + " milliseconds");
            }

        } else {
            /* now call the PostgreSQL function */
            start = System.currentTimeMillis();
            status = execFunction(procName, record, shefData, locId, dataValue,
                    qualifier, qualityCode, productId, productTime, postTime,
                    duplicateOption, ingestSwitch, stats);
            end = System.currentTimeMillis();

            if (log.isDebugEnabled()) {
                log.debug("ExecFunction(" + procName + ") completed");
                log.debug("PE Store took " + (end - start) + " milliseconds");
            }
        }

        if (status < 0) {
            log.error(record.getTraceId() + " - PostgresSQL error " + status
                    + " executing " + procName + " function for " + locId
                    + ", " + shefData.getObservationTimeObj().toString() + ", "
                    + productTime.toString() + ", " + productId + ", "
                    + postTime.toString());
            stats.incrementErrorMessages();
        } else {
            if (log.isDebugEnabled()) {
                log.debug("ExecFunction(" + procName + ") completed normally");
            }
            if ((DataType.READING.equals(type))
                    || (DataType.PROCESSED.equals(type))) {
                stats.incrementObsPe();
                if (shefData.getPhysicalElement().getCategory() == PhysicalElementCategory.HEIGHT) {
                    stats.incrementObsHeight();
                    stats.setElapsedTimeHeightIngest(end - start);

                } else if ((shefData.getPhysicalElement() != PhysicalElement.PRESSURE_ATMOSPHERIC)
                        && (shefData.getPhysicalElement() != PhysicalElement.PRESSURE_ATMOSPHERIC_3HR)
                        && (shefData.getPhysicalElement() != PhysicalElement.PRESSURE_CHARACTERISTIC)
                        && (shefData.getPhysicalElement() != PhysicalElement.PRESSURE_SEA_LEVEL)) {
                    stats.incrementObsPrecip();
                    stats.addElapsedTimePrecipIngest(end - start);
                } else {
                    stats.addElapsedTimeOtherIngest(end - start);
                }
            } else if (DataType.FORECAST.equals(type)) {
                stats.incrementForecastPe();
                stats.addElapsedTimeForecastIngest(end - start);

                /*
                 * track which lid/pe combinations have been processed for
                 * forecast river data.
                 */
                if ((shefData.getPhysicalElement().getCategory() == PhysicalElementCategory.HEIGHT)
                        || (shefData.getPhysicalElement().getCategory() == PhysicalElementCategory.DISCHARGE)) {
                    loadForecastInfo(locId, shefData.getPhysicalElement(),
                            stats);
                }
            }
        }
    }

    /**
     * 
     * @param precip
     * @return
     */
    private static int writePrecip(PrecipRecord precip) {
        if (log.isDebugEnabled()) {
            log.debug("calling GagePP.gage_pp_process_file");
        }
        GagePP gpw = new GagePP();
        int status = gpw.gage_pp_process_file(precip, gagePPOptions);
        if (log.isDebugEnabled()) {
            log.debug("GagePP.gage_pp_process_file.status = "
                    + status);
        }
        return status;
    }
    
    /**
     * 
     * @param dataObj
     * @param tableName
     * @param duplicateOption
     * @param stats
     * @param aaCategory
     * @param aaCheck
     */
    public static synchronized void postAAData(PersistableDataObject dataObj,
            String tableName, String duplicateOption, ShefStats stats,
            String aaCategory, String aaCheck) {
        PostTables.postData(dataObj, tableName, duplicateOption, stats,
                aaCategory, aaCheck);
    }

    /**
     * 
     * @param dataObj
     * @param tableName
     * @param duplicateOption
     * @param stats
     */
    public static synchronized void postData(PersistableDataObject dataObj,
            String tableName, String duplicateOption, ShefStats stats) {
        PostTables.postData(dataObj, tableName, duplicateOption, stats, null,
                null);
    }

    private static synchronized void postData(PersistableDataObject dataObj,
            String tableName, String duplicateOption, ShefStats stats,
            String aaCategory, String aaCheck) {
        long start = 0;
        long end = 0;

        String locId = null;
        String pe = null;
        short dur = -999;
        String ts = null;
        String extremum = null;
        double probability = -999;
        String validTime = null;
        String basisTime = null;
        double dataValue = -999;
        boolean isRevised = false;
        short revision = -999;

        /* Build the sql query string */
        String sql = "select value from " + tableName + " ";
        String where = "";
        String appendStr = "";
        String update = "update " + tableName + " set value = ";

        if (dataObj instanceof Commentvalue) {
            Commentvalue value = (Commentvalue) dataObj;
            locId = value.getId().getLid();
            pe = value.getId().getPe();
            dur = value.getId().getDur();
            ts = value.getId().getTs();
            extremum = value.getId().getExtremum();
            probability = value.getId().getProbability();
            dataValue = value.getValue();
            revision = value.getRevision();
            validTime = ShefConstants.POSTGRES_DATE_FORMAT.format(value.getId()
                    .getValidtime());
            basisTime = ShefConstants.POSTGRES_DATE_FORMAT.format(value.getId()
                    .getBasistime());

            appendStr = "probability = " + probability + " and "
                    + "validtime = '" + validTime + "' and " + "basistime = '"
                    + basisTime + "'";
        } else if (dataObj instanceof Contingencyvalue) {
            Contingencyvalue value = (Contingencyvalue) dataObj;
            locId = value.getId().getLid();
            pe = value.getId().getPe();
            dur = value.getId().getDur();
            ts = value.getId().getTs();
            extremum = value.getId().getExtremum();
            probability = value.getId().getProbability();
            dataValue = value.getValue();
            revision = value.getRevision();
            validTime = ShefConstants.POSTGRES_DATE_FORMAT.format(value.getId()
                    .getValidtime());
            basisTime = ShefConstants.POSTGRES_DATE_FORMAT.format(value.getId()
                    .getBasistime());

            appendStr = "probability = " + probability + " and "
                    + "validtime = '" + validTime + "' and " + "basistime = '"
                    + basisTime + "'";
        } else if (dataObj instanceof Procvalue) {
            Procvalue value = (Procvalue) dataObj;
            locId = value.getId().getLid();
            pe = value.getId().getPe();
            dur = value.getId().getDur();
            ts = value.getId().getTs();
            extremum = value.getId().getExtremum();
            dataValue = value.getValue();
            revision = value.getRevision();
            validTime = ShefConstants.POSTGRES_DATE_FORMAT.format(value.getId()
                    .getObstime());

            appendStr = "obstime = '" + validTime + "'";
        } else if (dataObj instanceof Rejecteddata) {
            Rejecteddata value = (Rejecteddata) dataObj;
            value.setUserid("SHEFdec");
            locId = value.getId().getLid();
            pe = value.getId().getPe();
            dur = value.getId().getDur();
            ts = value.getId().getTs();
            extremum = value.getId().getExtremum();
            probability = value.getId().getProbability();
            dataValue = value.getValue();
            revision = value.getRevision();
            validTime = ShefConstants.POSTGRES_DATE_FORMAT.format(value.getId()
                    .getValidtime());
            basisTime = ShefConstants.POSTGRES_DATE_FORMAT.format(value.getId()
                    .getBasistime());
            String postingTime = ShefConstants.POSTGRES_DATE_FORMAT
                    .format(value.getId().getPostingtime());

            appendStr = "probability = " + probability + " and "
                    + "validtime = '" + validTime + "' and " + "basistime = '"
                    + basisTime + "' and " + "postingtime = '" + postingTime
                    + "'";
        } else if (dataObj instanceof Alertalarmval) {
            Alertalarmval value = (Alertalarmval) dataObj;
            locId = value.getId().getLid();
            pe = value.getId().getPe();
            dur = value.getId().getDur();
            ts = value.getId().getTs();
            extremum = value.getId().getExtremum();
            probability = value.getId().getProbability();
            dataValue = value.getValue();
            revision = value.getRevision();
            validTime = ShefConstants.POSTGRES_DATE_FORMAT.format(value.getId()
                    .getValidtime());
            basisTime = ShefConstants.POSTGRES_DATE_FORMAT.format(value.getId()
                    .getBasistime());

            appendStr = "probability = " + probability + " and "
                    + "validtime = '" + validTime + "' and " + "basistime = '"
                    + basisTime + "' and " + "aa_categ = '"
                    + value.getId().getAaCateg() + "' and " + "aa_check = '"
                    + value.getId().getAaCheck() + "'";
        } else if (dataObj instanceof Arealobs) {
            Arealobs value = (Arealobs) dataObj;
            locId = value.getId().getLid();
            pe = value.getId().getPe();
            dur = value.getId().getDur();
            ts = value.getId().getTs();
            extremum = value.getId().getExtremum();
            dataValue = value.getValue();
            revision = value.getRevision();
            validTime = ShefConstants.POSTGRES_DATE_FORMAT.format(value.getId()
                    .getObstime());

            appendStr = "obstime = '" + validTime + "'";
        } else if (dataObj instanceof Arealfcst) {
            Arealfcst value = (Arealfcst) dataObj;
            locId = value.getId().getLid();
            pe = value.getId().getPe();
            dur = value.getId().getDur();
            ts = value.getId().getTs();
            extremum = value.getId().getExtremum();
            dataValue = value.getValue();
            revision = value.getRevision();
            probability = value.getId().getProbability();
            validTime = ShefConstants.POSTGRES_DATE_FORMAT.format(value.getId()
                    .getValidtime());
            basisTime = ShefConstants.POSTGRES_DATE_FORMAT.format(value.getId()
                    .getBasistime());

            appendStr = "probability = " + probability + " and "
                    + "validtime = '" + validTime + "' and " + "basistime = '"
                    + basisTime + "'";
        } else if (dataObj instanceof Unkstnvalue) {
            Unkstnvalue value = (Unkstnvalue) dataObj;
            locId = value.getId().getLid();
            pe = value.getId().getPe();
            dur = value.getId().getDur();
            ts = value.getId().getTs();
            extremum = value.getId().getExtremum();
            dataValue = value.getId().getValue();
            revision = value.getId().getRevision();
            validTime = ShefConstants.POSTGRES_DATE_FORMAT.format(value.getId()
                    .getObstime());

            appendStr = "obstime = '" + validTime + "'";

        }

        where = "where lid = '" + locId + "' and pe = '" + pe + "' and "
                + "dur = " + dur + " and ts = '" + ts + "' and "
                + "extremum = '" + extremum + "' and " + appendStr;
        sql += where;
        update += "'" + dataValue + "' " + where;
        if (log.isDebugEnabled()) {
            log.debug("SQLQuery [" + sql + "]");
        }

        int doOverwrite = 0;
        CoreDao dao = null;
        try {
            dao = new CoreDao(DaoConfig.forDatabase(ShefConstants.IHFS));

            Object[] result = dao.executeSQLQuery(sql);
            if (result.length <= 0) {
                start = System.currentTimeMillis();
                dao.persist(dataObj);
                end = System.currentTimeMillis();
                if (log.isDebugEnabled()) {
                    log.debug(tableName + " store took " + (end - start)
                            + " milliseconds");
                }

                /* data was properly added to table */
                if (dataObj instanceof Commentvalue) {
                    stats.incrementComments();
                } else if (dataObj instanceof Contingencyvalue) {
                    stats.incrementContingencyValues();
                } else if (dataObj instanceof Procvalue) {
                    stats.incrementPostProcessedValues();
                } else if (dataObj instanceof Rejecteddata) {
                    stats.incrementRejected();
                } else if (dataObj instanceof Alertalarmval) {
                    stats.incrementAlertAlarm();
                } else if (dataObj instanceof Arealobs) {
                    stats.incrementArealValues();
                } else if (dataObj instanceof Arealfcst) {
                    stats.incrementArealFcst();
                } else if (dataObj instanceof Unkstnvalue) {
                    stats.incrementUnknownStation();
                }
            } else {
                double tableValue = ((Double) result[0]).doubleValue();

                if (revision == 1) {
                    isRevised = true;
                }
                doOverwrite = PostTables.determineUpdateAction(duplicateOption,
                        isRevised);

                /* if the record should be overwritten, then do so */
                if (doOverwrite > 0) {
                    start = System.currentTimeMillis();
                    switch (doOverwrite) {
                    case ShefConstants.UPDATE_ACTION:
                        dao.executeSQLUpdate(update);
                        break;
                    case ShefConstants.IF_DIFFERENT_UPDATE_ACTION:
                        if (tableValue != dataValue) {
                            dao.executeSQLUpdate(update);
                        }
                        break;
                    }
                    end = System.currentTimeMillis();
                    if (dataObj instanceof Commentvalue) {
                        stats.incrementCommentOverwrite();
                    } else if (dataObj instanceof Contingencyvalue) {
                        stats.incrementContingencyOverwrite();
                    } else if (dataObj instanceof Procvalue) {
                        stats.incrementPostProcessedOverwrite();
                    } else if (dataObj instanceof Rejecteddata) {
                        stats.incrementRejectedOverwrite();
                    } else if (dataObj instanceof Alertalarmval) {
                        stats.incrementAlertAlarm();
                    } else if (dataObj instanceof Arealobs) {
                        stats.incrementArealObsOverwrite();
                    } else if (dataObj instanceof Arealfcst) {
                        stats.incrementArealFcstOverwrite();
                    } else if (dataObj instanceof Unkstnvalue) {
                        stats.incrementUnknownStationOverwrite();
                    }
                } else {
                    /*
                     * don't perform the overwrite since conditions were not met
                     */
                    if (AppsDefaults.getInstance()
                            .getBoolean(ShefConstants.DUP_MESSAGE, false)) {
                        log.info("Ignoring duplicate " + tableName + " for "
                                + locId + ", " + validTime);
                    }
                    stats.incrementIgnored();
                }
            }
        } catch (Exception e) {
            log.error(dataObj.getTraceId() + " - PostgresSQL error updating "
                    + tableName + " for " + locId + ", " + validTime);
            if (doOverwrite > 0) {
                log.error("Query = [" + update + "]");
            } else {
                log.error("Query = [" + sql + "]");
            }
            if(log.isDebugEnabled()) {
                log.error(e);
            }
            stats.incrementErrorMessages();
        }
    }

    /**
     * 
     * @param unkstn
     * @param stats
     */
    public static synchronized void postUnknownStation(Unkstn unkstn,
            ShefStats stats) {
        /* Build the sql query string */
        StringBuilder sql = new StringBuilder();
        sql.append("select lid from unkstn where lid = '" + unkstn.getLid()
                + "'");

        long start = -999;
        long end = -999;
        CoreDao dao = null;
        try {
            dao = new CoreDao(DaoConfig.forDatabase(ShefConstants.IHFS));

            Object[] result = dao.executeSQLQuery(sql.toString());
            if (result.length <= 0) {
                start = System.currentTimeMillis();
                dao.persist(unkstn);
                end = System.currentTimeMillis();
                if (log.isDebugEnabled()) {
                    log.debug("Unkstn store took " + (end - start)
                            + " milliseconds");
                }

                /* data was properly added to table */
                stats.incrementUnknownStation();
            } else {
                dao.saveOrUpdate(unkstn);
                stats.incrementUnknownStationOverwrite();
            }
        } catch (Exception e) {
            log.error(unkstn.getTraceId()
                    + " - PostgresSQL error updating UnkStn for "
                    + unkstn.getLid() + ", "
                    + unkstn.getProducttime().toString() + ", "
                    + unkstn.getPostingtime().toString());
            log.error("Query = [" + sql.toString() + "]");
            if(log.isDebugEnabled()) {
                log.error(e);
            }
            stats.incrementErrorMessages();
        }
    }

    /**
     * Determines the update action to take.
     * 
     * @param option
     *            - Configured option
     * @param isRevised
     *            - is the data revised?
     * @return - int specifying what action to take
     */
    public static synchronized int determineUpdateAction(String option,
            boolean isRevised) {
        if (log.isDebugEnabled()) {
            log.debug("PostTables.determineUpdateAction() called...");
            log.debug("Revised: [" + isRevised + "]");
            log.debug("Option = [" + option + "]");
        }

        int updateAction = ShefConstants.DONT_UPDATE_ACTION;
        /*
         * Check if the existing value should be overwritten. This occurs under
         * any of the following conditions:
         * 
         * 1)if shef_duplicate = always_overwrite ie., optionsDuplicate =
         * ALWAYS_OVERWRITE 2)if shef_duplicate = use_revcode and revisions flag
         * is set in the SHEF record ie., optionsDuplicate = USE_REVCODE 3)if
         * shef_duplicate = if_different and the value is different ie.,
         * optionsDuplicate = IF_DIFFERENT 4)if shef_duplicate =
         * if_different_or_revcode and the revision flag is set in SHEF record
         * or if value is different ie., optionsDuplicate =
         * IF_DIFFERENT_UPDATE_ACTION 5)if shef_duplicate =
         * if_different_and_revcode and the revision flag is set in SHEF record
         * and value is different ie., optionsDuplicate =
         * IF_DIFFERENT_UPDATE_ACTION
         */

        if (ShefConstants.ALWAYS_OVERWRITE.equals(option)) {
            updateAction = ShefConstants.UPDATE_ACTION;
        } else if (ShefConstants.USE_REVCODE.equals(option)) {
            if (isRevised) {
                updateAction = ShefConstants.UPDATE_ACTION;
            } else {
                updateAction = ShefConstants.DONT_UPDATE_ACTION;
            }
        } else if (ShefConstants.IF_DIFFERENT_OR_REVCODE.equals(option)
                && isRevised) {
            updateAction = ShefConstants.UPDATE_ACTION;
        } else if (ShefConstants.IF_DIFFERENT_AND_REVCODE.equals(option)
                && isRevised) {
            updateAction = ShefConstants.IF_DIFFERENT_UPDATE_ACTION;
        } else if (ShefConstants.IF_DIFFERENT_AND_REVCODE.equals(option)
                || option.equals(ShefConstants.IF_DIFFERENT)) {

            updateAction = ShefConstants.IF_DIFFERENT_UPDATE_ACTION;
        } else if (!ShefConstants.IF_DIFFERENT_AND_REVCODE.equals(option)) {
            /*
             * This addresses the case where options_duplicate == IF_DIFFERENT
             * or options_duplicate == IF_DIFFERENT_OR_REVCODE and shefrec_rev
             * == 0
             */
            updateAction = ShefConstants.IF_DIFFERENT_UPDATE_ACTION;
        }

        if (log.isDebugEnabled()) {
            log.debug("updateAction = [" + updateAction + "]");
        }
        return updateAction;
    }

    /**
     * Load the lid/pe combinations for any data report which has not already
     * been loaded for this product.
     * 
     * @param lid
     *            - Location Identifier
     * @param pe
     *            - Physical Element
     * @param stats
     *            - Stats Object
     */
    private static synchronized void loadForecastInfo(String lid,
            PhysicalElement pe, ShefStats stats) {
        if (log.isDebugEnabled()) {
            log.debug("PostTables.loadForecastInfo() called...");
        }
        boolean matchFound = false;
        List<String> lidList = stats.getLidList();
        List<PhysicalElement> peList = stats.getPeList();
        for (int i = 0; i < lidList.size(); i++) {
            if (lidList.get(i).equals(lid) && peList.get(i).equals(pe)) {
                matchFound = true;
                break;
            }
        }

        /* load the combination if it is new and if there is room */
        if (!matchFound) {
            if (lidList.size() < ShefConstants.MAXFCST_INFO) {
                stats.addLidList(lid);
                stats.addPeList(pe);
                stats.incrementMaxForecast();
            }
        }
    }

    private static synchronized int execFunction(String functionName,
            ShefRecord record, ShefData shefData, String locId,
            String dataValue, String qualifier, long qualityCode,
            String productId, Date productTime, Date postTime,
            String duplicateOption, ShefStats stats) {
        if (log.isDebugEnabled()) {
            log.debug("PostTables.execFunction(1) called...");
        }
        CoreDao dao = null;
        Connection conn = null;
        CallableStatement cs = null;
        int status = -1;
        if (dataValue == "") {
            dataValue = ShefConstants.SHEF_MISSING;
        }
        try {
            dao = new CoreDao(DaoConfig.forDatabase(ShefConstants.IHFS));
            SessionFactoryImplementor impl = (SessionFactoryImplementor) dao.getSessionFactory();
            ConnectionProvider cp = impl.getConnectionProvider();
            conn = cp.getConnection();

            cs = conn.prepareCall("{call " + functionName
                    + "(?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)}");
            cs.setString(1, locId);
            cs.setString(2, shefData.getPhysicalElement().getCode());
            cs.setInt(3, shefData.getDurationValue());
            cs.setString(4, shefData.getTypeSource().getCode());
            cs.setString(5, shefData.getExtremum().getCode());
            cs.setTimestamp(6, new Timestamp(shefData.getObservationTimeObj()
                    .getTime()));
            cs.setFloat(7, Float.parseFloat(dataValue));
            cs.setString(8, qualifier);
            cs.setInt(9, (int) qualityCode);

            if (record.isRevisedRecord()) {
                cs.setInt(10, 1);
            } else {
                cs.setInt(10, 0);
            }

            cs.setString(11, productId);
            
            cs.setTimestamp(12, new java.sql.Timestamp(productTime.getTime()));
            cs.setTimestamp(13, new java.sql.Timestamp(postTime.getTime()));
            
            int doOverwrite = PostTables.determineUpdateAction(duplicateOption,
                    record.isRevisedRecord());
            cs.setInt(14, doOverwrite);
            cs.registerOutParameter(15, java.sql.Types.INTEGER);

            if (log.isDebugEnabled()) {
                log.debug("locId = [" + locId + "]");
                log.debug("PE = [" + shefData.getPhysicalElement() + "]");
                log.debug("duration = [" + shefData.getDuration().getValue() + "]");
                log.debug("TS = [" + shefData.getTypeSource() + "]");
                log.debug("extremum = [" + shefData.getExtremum() + "]");
                log.debug("timestamp = ["
                        + new Timestamp(shefData.getObservationTimeObj().getTime())
                        + "]");
                log.debug("data value = [" + dataValue + "]");
                log.debug("qualifier = [" + qualifier + "]");
                log.debug("qc = [" + qualityCode + "]");
                log.debug("productId = [" + productId + "]");
                log.debug("doOverwrite = [" + doOverwrite + "]");
                log.debug("Calling executeQuery for " + functionName
                        + " doOverwrite = " + doOverwrite);
                log.debug("Statement = [" + cs.toString() + "]");
            }
            boolean execStatus = cs.execute();
            status = cs.getInt(15);
            if (status == 0) {
                conn.commit();
            } else {
                throw new Exception("PostgresSQL error executing function "
                        + functionName);
            }
            if (log.isDebugEnabled()) {
                log.debug("Return status = " + status);
                log.debug("Completed PE insert for PE "
                        + shefData.getPhysicalElement());
                log.debug(functionName + " status = " + execStatus);
            }
        } catch (Exception e) {
            log.error(
                    record.getTraceId()
                            + " - PostgresSQL error executing function "
                            + functionName);
            log.error("Error updating/committing PE insert for PE "
                    + shefData.getPhysicalElement());
            log.error("Record Data: " + record);
            if(log.isDebugEnabled()) {
                log.error(e);
            }
        } finally {
            try {
                cs.close();
            } catch (Exception e) {
                // Intentionally empty
            }
            try {
                conn.close();
            } catch (Exception e) {
                // Intentionally empty
            }
            cs = null;
            conn = null;
        }
        return status;
    }

    /**
     * Call the database stored procedures
     * 
     * @param functionName
     *            - name of the procedure to call
     * @return - status of action, 1 is good, 0 is bad
     */
    private static synchronized int execFunction(String functionName,
            ShefRecord record, ShefData shefData, String locId,
            String dataValue, String qualifier, long qualityCode,
            String productId, Date productTime, Date postTime,
            String duplicateOption, ShefConstants.IngestSwitch ingestSwitch,
            ShefStats stats) {
        if (log.isDebugEnabled()) {
            log.debug("PostTables.execFunction(2) called...");

        }
        CoreDao dao = null;
        Connection conn = null;
        CallableStatement cs = null;
        int status = -1;
        if (dataValue == "") {
            dataValue = ShefConstants.SHEF_MISSING;
        }
        try {
            dao = new CoreDao(DaoConfig.forDatabase(ShefConstants.IHFS));
            SessionFactoryImplementor impl = (SessionFactoryImplementor) dao.getSessionFactory();
            ConnectionProvider cp = impl.getConnectionProvider();
            conn = cp.getConnection();

            cs = conn.prepareCall("{call " + functionName
                    + "(?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)}");
            cs.setString(1, locId);
            cs.setString(2, shefData.getPhysicalElement().getCode());
            cs.setInt(3, shefData.getDurationValue());
            cs.setString(4, shefData.getTypeSource().getCode());
            cs.setString(5, shefData.getExtremum().getCode());
            cs.setTimestamp(6, new java.sql.Timestamp(shefData
                    .getObservationTimeObj().getTime()));
            cs.setFloat(7, Float.parseFloat(dataValue));
            cs.setString(8, qualifier);
            cs.setInt(9, (int) qualityCode);

            if (record.isRevisedRecord()) {
                cs.setInt(10, 1);
            } else {
                cs.setInt(10, 0);
            }

            cs.setString(11, productId);
            cs.setTimestamp(12, new java.sql.Timestamp(productTime.getTime()));
            cs.setTimestamp(13, new java.sql.Timestamp(postTime.getTime()));

            int doOverwrite = PostTables.determineUpdateAction(duplicateOption,
                    record.isRevisedRecord());
            
            cs.setInt(14, doOverwrite);

            cs.registerOutParameter(15, java.sql.Types.INTEGER);

            if (log.isDebugEnabled()) {
                log.debug("Stored data : " + record);
                log.debug("doOverwrite = [" + doOverwrite + "]");
                log.debug("Calling executeQuery for " + functionName
                        + " doOverwrite = " + doOverwrite);
            }
            // TODO fix NullPointerException
            boolean execStatus = cs.execute();
            status = cs.getInt(15);
            if (status == 0) {
                conn.commit();
            } else {
                throw new Exception("PostgresSQL error executing function "
                        + functionName);
            }

            if (log.isDebugEnabled()) {
                log.debug("Completed PE insert for PE "
                        + shefData.getPhysicalElement());
                log.debug(functionName + " status = " + execStatus);
                log.debug("Return status = " + status);
            }
        } catch (Exception e) {
            log.error(
                    record.getTraceId()
                            + " - PostgresSQL error executing function "
                            + functionName);
            log.error("Error updating/committing PE insert for PE "
                    + shefData.getPhysicalElement());
            log.error("Record Data: " + record);
            if (log.isDebugEnabled()) {
                log.error(e);
            }
        } finally {
            try {
                cs.close();
            } catch (Exception e) {
                // Intentionally empty
            }
            try {
                conn.close();
            } catch (Exception e) {
                // Intentionally empty
            }
            cs = null;
            conn = null;
        }
        return status;
    }

    /**
     * Call the database stored procedures
     * 
     * @param functionName
     *            - name of the procedure to call
     * @return - status of action, 1 is good, 0 is bad
     */
    private static synchronized int execFcstFunc(String functionName,
            ShefRecord record, ShefData shefData, String locId,
            String dataValue, String qualifier, long qualityCode,
            String productId, Date productTime, Date postTime,
            String duplicateOption, ShefConstants.IngestSwitch ingestSwitch,
            ShefStats stats, Date validTime) {

        long start = System.currentTimeMillis();
        CoreDao dao = null;
        Connection conn = null;
        CallableStatement cs = null;
        java.sql.Timestamp timeStamp = null;
        int status = -1;
        if (dataValue == "") {
            dataValue = ShefConstants.SHEF_MISSING;
        }

        try {
            dao = new CoreDao(DaoConfig.forDatabase(ShefConstants.IHFS));
            SessionFactoryImplementor impl = (SessionFactoryImplementor) dao.getSessionFactory();
            ConnectionProvider cp = impl.getConnectionProvider();
            conn = cp.getConnection();
            
            cs = conn.prepareCall("{call " + functionName
                    + "(?, ?, ?, ?, ?, cast(? as real), ?, ?, ?, ?,"
                    + " ?, ?, ?, ?, ?, ?, ?)}");

            cs.setString(1, locId);
            if (log.isDebugEnabled()) {
            }
            cs.setString(2, shefData.getPhysicalElement().getCode());

            cs.setInt(3, shefData.getDurationValue());

            cs.setString(4, shefData.getTypeSource().getCode());

            cs.setString(5, shefData.getExtremum().getCode());

            float probability = new Double(shefData.getProbability().getValue())
                    .floatValue();
            cs.setFloat(6, probability);

            timeStamp = new java.sql.Timestamp(validTime.getTime());
            cs.setTimestamp(7, timeStamp);

            Date basisDate = shefData.getCreationDateObj();

            if (basisDate == null) {
                basisDate = shefData.getObservationTimeObj();
            }

            timeStamp = new java.sql.Timestamp(basisDate.getTime());
            cs.setTimestamp(8, timeStamp);

            cs.setFloat(9, Float.parseFloat(dataValue));

            cs.setString(10, qualifier);

            cs.setInt(11, (int) qualityCode);

            if (shefData.isRevisedRecord()) {
                cs.setInt(12, 1);
            } else {
                cs.setInt(12, 0);
            }

            cs.setString(13, productId);

            timeStamp = new java.sql.Timestamp(productTime.getTime());
            cs.setTimestamp(14, timeStamp);

            timeStamp = new java.sql.Timestamp(postTime.getTime());
            cs.setTimestamp(15, timeStamp);

            int doOverwrite = 0;

            doOverwrite = PostTables.determineUpdateAction(duplicateOption,
                    shefData.isRevisedRecord());
            cs.setInt(16, doOverwrite);

            cs.registerOutParameter(17, java.sql.Types.INTEGER);

            if (log.isDebugEnabled()) {
                log.debug("locId = [" + locId + "]");
                log.debug("PE = [" + shefData.getPhysicalElement() + "]");
                log.debug("Duration = [" + shefData.getDuration().getValue()
                        + "]");
                log.debug("TS = [" + shefData.getTypeSource() + "]");
                log.debug("Extremum = [" + shefData.getExtremum() + "]");
                log.debug("Probability = ["
                        + shefData.getProbability().getValue() + "]");
                log.debug("valid timestamp = [" + timeStamp + "]");
                log.debug("basis timestamp = [" + timeStamp + "]");
                log.debug("Data Value = [" + dataValue + "]");
                log.debug("Qualifier = [" + qualifier + "]");
                log.debug("qualityCode = [" + qualityCode + "]");
                log.debug("productId = [" + productId + "]");
                log.debug("productTime = [" + timeStamp + "]");
                log.debug("postTime = [" + timeStamp + "]");
                log.debug("doOverwrite = [" + doOverwrite + "]");

                log.debug("Calling executeQuery for " + functionName
                        + " doOverwrite = " + doOverwrite);
            }

            // TODO fix NullPointerException
            boolean execStatus = cs.execute();
            stats.incrementForecastPe();
            status = cs.getInt(17);

            if (status == 0) {
                conn.commit();
            } else {
                throw new Exception("PostgresSQL error executing function "
                        + functionName);
            }

            if (log.isDebugEnabled()) {
                log.debug("Completed PE insert for PE "
                        + shefData.getPhysicalElement());
                log.debug(functionName + " status = " + execStatus);
                log.debug("Return status = " + status);
            }
        } catch (Exception e) {
            log.error("Error updating/committing PE insert for PE "
                    + shefData.getPhysicalElement());
            log.error("Record Data: " + record);
            log.error(
                    record.getTraceId()
                            + " - PostgresSQL error executing function "
                            + functionName);
            if (log.isDebugEnabled()) {
                log.error(e);
            }
            stats.incrementErrorMessages();
        } finally {
            try {
                cs.close();
            } catch (Exception e) {
                // Intentionally empty
            }
            try {
                conn.close();
            } catch (Exception e) {
                // Intentionally empty
            }
            cs = null;
            conn = null;
        }

        return status;
    }

    private static int gagePPSetup() {
        String shef_duplicate_token = "shef_duplicate";

        gagePPOptions = new GagePPOptions();

        String token = AppsDefaults.getInstance()
                .getToken(shef_duplicate_token);

        StringBuilder message = new StringBuilder("shef_duplicate : ");
        if ("ALWAYS_OVERWRITE".equals(token)) {
            gagePPOptions.setShef_duplicate(shef_dup.ALWAYS_OVERWRITE);
            message.append("ALWAYS_OVERWRITE (always process duplicate reports)");
        } else if ("USE_REVCODE".equals(token)) {
            gagePPOptions.setShef_duplicate(shef_dup.USE_REVCODE);
            message.append("USE_REVCODE (only process duplicate reports if they are revisions");
        } else if ("IF_DIFFERENT_AND_REVCODE".equals(token)) {
            gagePPOptions.setShef_duplicate(shef_dup.IF_DIFFERENT_AND_REVCODE);
            message.append("IF_DIFFERENT_OR_REVCODE (only process duplicate reports if they are revisions or their values are different");
        } else {
            gagePPOptions.setShef_duplicate(shef_dup.IF_DIFFERENT);
            message.append("IF_DIFFERENT (only process duplicate reports if their values are different");
        }
        log.info(message.toString());

        PrecipUtils.get_precip_window(gagePPOptions);
        gagePPOptions.setIntppq(PrecipUtils.get_6hour_precip_window());

        // Output this information to the log

        String logMsg = String.format(
                "intpc [%d] intlppp [%d] intuppp [%d] intppq [%f]",
                gagePPOptions.getIntpc(), gagePPOptions.getIntlppp(),
                gagePPOptions.getIntuppp(), gagePPOptions.getIntppq());

        log.info(logMsg);

        return 0;
    }

    /**
     * Post data to the riverstatus table
     * 
     * 
     * @param record
     *            - the ShefRecord
     * @param shefDataValue
     *            - the shefDataValue to be posted to the table
     * @param updateFlag
     *            - the boolean value if an update versus an insert should be
     *            performed
     * @return - status of action, 1 is good, 0 is bad
     */
    public static synchronized int postRiverStatus(ShefRecord record,
            ShefData shefDataValue, boolean updateFlag) {

        CoreDao dao = null;
        Connection conn = null;
        PreparedStatement ps = null;
        java.sql.Timestamp timeStamp = null;
        java.sql.Timestamp timeStamp2 = null;
        String pe = null;
        String lid = null;
        String ts = null;
        float probability = -9999;
        String updateQuery = "UPDATE riverstatus SET lid = ? , " + "pe = ? , "
                + "dur = ? , " + "ts = ? , " + "extremum = ? ,"
                + "probability = ? , " + "validtime = ? , "
                + "basistime = ? , " + "value = ? "
                + "WHERE lid= ? AND pe= ? AND ts= ?";
        String insertQuery = "INSERT INTO riverstatus values(?,?,?,?,?,?,?,?,?)";
        int status = -1;

        try {
            dao = new CoreDao(DaoConfig.forDatabase(ShefConstants.IHFS));

            SessionFactoryImplementor impl = (SessionFactoryImplementor) dao.getSessionFactory();
            ConnectionProvider cp = impl.getConnectionProvider();

            conn = cp.getConnection();
            
            if (updateFlag) {
                ps = conn.prepareCall(updateQuery);
            } else {
                ps = conn.prepareCall(insertQuery);
            }

            lid = shefDataValue.getLocationId();
            ps.setString(1, lid);

            pe = shefDataValue.getPhysicalElement().getCode();
            ps.setString(2, pe);

            ps.setInt(3, shefDataValue.getDuration().getValue());

            ts = shefDataValue.getTypeSource().getCode();
            ps.setString(4, ts);

            ps.setString(5, shefDataValue.getExtremum().getCode());

            probability = new Double(shefDataValue.getProbability().getValue())
                    .floatValue();
            ps.setFloat(6, probability);

            timeStamp = new java.sql.Timestamp(shefDataValue
                    .getObservationTimeObj().getTime());
            ps.setTimestamp(7, timeStamp);

            Date basisDate = shefDataValue.getCreationDateObj();
            if (basisDate == null) {
                basisDate = record.getCreationDateObj();
                if (basisDate == null) {
                    basisDate = shefDataValue.getObservationTimeObj();
                }
            }

            timeStamp2 = new java.sql.Timestamp(basisDate.getTime());
            ps.setTimestamp(8, timeStamp2);

            // ps.setFloat(9, Float.parseFloat(shefDataValue.getStringValue()));
            ps.setFloat(9, shefDataValue.getValue().floatValue());

            if (updateFlag) {
                ps.setString(10, lid);
                ps.setString(11, pe);
                ps.setString(12, ts);
            }

            status = ps.executeUpdate();

            if (status != 0) {
                conn.commit();
            } else {
                throw new Exception(
                        "PostgresSQL error inserting into riverstatus");
            }

            if (log.isDebugEnabled()) {
                if (updateFlag) {
                    log.error(String.format("Completed updating into RiverStatus with [%s]", record));
                } else {
                    log.error(String.format("Completed inserting into RiverStatus with [%s]", record));
                }
            }
        } catch (Exception e) {
            if (updateFlag) {
                log.error(String.format("Error updating into RiverStatus with [%s]", record));
            } else {
                log.error(String.format("Error inserting into RiverStatus with [%s]", record));
            }
            if (log.isDebugEnabled()) {
                log.error(e);
            }
        } finally {
            try {
                ps.close();
            } catch (Exception e) {
                // Intentionally empty
            }
            try {
                conn.close();
            } catch (Exception e) {
                // Intentionally empty
            }
            ps = null;
            conn = null;
        }

        return status;
    }
}
