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

package com.raytheon.edex.plugin.shef;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.StringTokenizer;
import java.util.TimeZone;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.edex.plugin.shef.ShefSeparator.ShefDecoderInput;
import com.raytheon.edex.plugin.shef.data.ShefData;
import com.raytheon.edex.plugin.shef.data.ShefRecord;
import com.raytheon.edex.plugin.shef.util.ParserToken;
import com.raytheon.edex.plugin.shef.util.SHEFDate;
import com.raytheon.edex.plugin.shef.util.SHEFErrors;
import com.raytheon.edex.plugin.shef.util.ShefParm;
import com.raytheon.edex.plugin.shef.util.ShefUtil;
import com.raytheon.edex.plugin.shef.util.TokenType;
import com.raytheon.uf.common.dataplugin.shef.util.ParameterCode.Duration;
import com.raytheon.uf.common.dataplugin.shef.util.ParameterCode.Extremum;
import com.raytheon.uf.common.dataplugin.shef.util.ParameterCode.PhysicalElement;
import com.raytheon.uf.common.dataplugin.shef.util.SHEFErrorCodes;
import com.raytheon.uf.common.dataplugin.shef.util.SHEFTimezone;
import com.raytheon.uf.common.dataplugin.shef.util.ShefConstants;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.wmo.WMOHeader;

/**
 * The SHEFParser provides the text parsing for SHEF data. This class was
 * factored out of the original SHEFDecoder so that the parser code may be
 * tested in isolation.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 10, 2009            jkorman     Initial creation
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public class SHEFParser {

    private final Log log = LogFactory.getLog(getClass());

    private static final SHEFErrors ERR_LOGGER = SHEFErrors
            .registerLogger(SHEFParser.class);

    private static final HashMap<TokenType, Integer> STARTTYPES = new HashMap<TokenType, Integer>();
    static {
        STARTTYPES.put(TokenType.A_REC, 0);
        STARTTYPES.put(TokenType.A_REC_R, 0);
        STARTTYPES.put(TokenType.B_REC, 0);
        STARTTYPES.put(TokenType.B_REC_R, 0);
        STARTTYPES.put(TokenType.E_REC, 0);
        STARTTYPES.put(TokenType.E_REC_R, 0);
    }

    private static final HashSet<PhysicalElement> VALID_TRACE_PE = new HashSet<PhysicalElement>();
    static {
        VALID_TRACE_PE.add(PhysicalElement.PRECIPITATION_ACCUMULATOR);
        VALID_TRACE_PE.add(PhysicalElement.PRECIPITATION_INCREMENT);
        VALID_TRACE_PE.add(PhysicalElement.PRECIPITATION_INCREMENT_DAILY);
        VALID_TRACE_PE.add(PhysicalElement.SNOW_DEPTH);
        VALID_TRACE_PE.add(PhysicalElement.SNOW_NEW_SNOWFALL);
        VALID_TRACE_PE.add(PhysicalElement.SNOW_WATER_EQUIVALENT);

    }

    private static final HashSet<String> EOD_SENDCODES = new HashSet<String>();
    static {
        EOD_SENDCODES.add("HY");
        EOD_SENDCODES.add("PY");
        EOD_SENDCODES.add("QY");
    }
    
    private static final String CARRIAGECONTROL = "\r\n";

    private String message;

    private String traceId;

    private String awipsHeader;

    private WMOHeader wmoHeader;

    private Date productDate;

    private String recordType;

    private List<ParserToken> parts;

    private int partsIndex;

    // The location identifier is the first positional field in
    // each of the "A", "B", or "E" data records. It is either the
    // actual location of the data being processed (for "A" and "E"
    // records) or the message source for "B" records
    private String locationId = null;

    // The observation time is the second positional field in each of
    // the "A", "B", or "E" data records. This is a mandatory item and
    // will be completely evaluationed once the timezone has been set.
    // After that time the obsTime and timeZone must not change.
    private String obsTime = null;

    private SHEFDate masterDate = null;

    private SHEFDate obsDate = null;

    //
    private boolean was24Hour = false;

    // The timezone is the third positional field in a SHEF record. This
    // item is optional and if missing shall default to "Z". Do not set
    // this value until the timezone positional field has been seen or
    // it can be determined if the timezone should be set to default.
    private String timeZone = null;

    private TimeZone tz = null;

    //
    //
    //
    private SHEFDate adjObsDate = null;

    // The createTime may be evaluated at any time during the data scan. This
    // value is optional. If no value is specified all fields of the
    // createTime must be set to zero.
    private SHEFDate createTime = null;

    // The units code can change as we parse the input data.
    // currentUnits tracks these changes to be applied to the
    // ShefData being built.
    private String currentUnits = "E";

    private String currentDuration = "Z";

    private Short durationValue = 0;

    private String currentDurationOverride = null;

    private Short durationValueOverride = null;

    private String currentQualifier = null;

    private String currentExtremum = Extremum.NULL.getCode();

    private String bRecordDataSource = null;

    private AppsDefaults appDefaults = AppsDefaults.getInstance();

    private boolean emitSkippedValues = false;

    private String reportLead = null;
    
    /**
     * 
     * @param traceId
     */
    public SHEFParser(ShefDecoderInput sdi) {
        message = sdi.record;
        traceId = sdi.traceId;
        awipsHeader = sdi.awipsHeader;
        wmoHeader = sdi.wmoHeader;
        productDate = sdi.productDate;

        parts = tokenize(clearTabs(message));

        emitSkippedValues = appDefaults.getBoolean(
                ShefConstants.SHEF_EMIT_SKIPPED, false);

        partsIndex = 0;
    }

    /**
     * @return the locationId
     */
    public String getLocationId() {
        return locationId;
    }

    /**
     * @param locationId
     *            the locationId to set
     */
    public void setLocationId(String lid) {
        if (log.isDebugEnabled()) {
            log.debug(traceId + "- Setting locationId : " + lid);
        }
        locationId = lid;
    }

    /**
     * @return the obsTime
     */
    public String getObsTime() {
        return obsTime;
    }

    /**
     * @param obsTime
     *            the obsTime to set
     */
    public void setObsTime(String obsTime) {
        if (log.isDebugEnabled()) {
            log.debug(traceId + "- Setting obsTime : " + obsTime);
        }
        this.obsTime = obsTime;
    }

    /**
     * @return the obsDate
     */
    public SHEFDate getObsDate() {
        return obsDate;
    }

    /**
     * @param obsDate
     *            the obsDate to set
     */
    public void setObsDate(SHEFDate obsDate) {
        this.obsDate = obsDate;
    }

    /**
     * @return the timeZone
     */
    public String getTimeZone() {
        return timeZone;
    }

    /**
     * @param timeZone
     *            the timeZone to set
     */
    public void setTimeZone(String timeZone) {
        if (log.isDebugEnabled()) {
            log.debug(traceId + "- Setting timeZone : " + timeZone);
        }
        this.timeZone = timeZone;
    }

    /**
     * Checks to see if the timezone has been set. If not, a default timezone is
     * set.
     */
    public void checkForDefaultTimeZone() {
        if (timeZone == null) {
            timeZone = ShefConstants.Z;
        }
    }

    /**
     * Increments the adjusted observation date based on an "E" record
     * increment.
     * 
     * @param increment
     *            The "E" record increment;
     */
    public void incrementAdjObsDate(ParserToken increment) {
        if ((adjObsDate != null) && (increment != null)) {
            SHEFDate date = SHEFDate.increment(adjObsDate,
                    increment.getToken(), 2);
            if (date != null) {
                adjObsDate = date;
            }
        }
    }

    /**
     * Resets the adjusted obsdate to the current obsDate.
     */
    public void resetAdjObsDate() {
        setAdjObsDate(getObsDate());
    }

    /**
     * @return the adjusted date.
     */
    public SHEFDate getAdjObsDate() {
        return adjObsDate;
    }

    /**
     * @param adjDate
     *            the adjusted date to set
     */
    public void setAdjObsDate(SHEFDate adjDate) {
        if (log.isDebugEnabled()) {
            log.debug(traceId + "- Setting adjObsDate : " + adjDate);
        }
        if (adjDate != null) {
            adjObsDate = new SHEFDate(adjDate);
        }
    }

    /**
     * @return the createTime
     */
    public SHEFDate getCreateTime() {
        return createTime;
    }

    /**
     * @param createTime
     *            the createTime to set
     */
    public void setCreateTime(SHEFDate createTime) {
        if (log.isDebugEnabled()) {
            log.debug(traceId + "- Setting createTime : " + createTime);
        }
        this.createTime = createTime;
    }

    /**
     * @return the currentUnits
     */
    public String getCurrentUnits() {
        return currentUnits;
    }

    /**
     * @param currentUnits
     *            the currentUnits to set
     */
    public void setCurrentUnits(String currentUnits) {
        this.currentUnits = currentUnits;
    }

    /**
     * @return the currentDuration
     */
    public String getCurrentDuration() {
        return currentDuration;
    }

    /**
     * @param currentDuration
     *            the currentDuration to set
     */
    public void setCurrentDuration(String currentDuration) {
        this.currentDuration = currentDuration;
    }

    /**
     * @return the currentExtremum
     */
    public String getCurrentExtremum() {
        return currentExtremum;
    }

    /**
     * @param currentExtremum
     *            the currentExtremum to set
     */
    public void setCurrentExtremum(String currentExtremum) {
        this.currentExtremum = currentExtremum;
    }

    public void setCurrentQualifier(String qual) {
        if ("Z".equals(qual)) {
            currentQualifier = null;
        } else {
            currentQualifier = qual;
        }
    }

    public String getCurrentQualifier() {
        return currentQualifier;
    }

    /**
     * 
     * @return
     */
    public ShefRecord decode() {
        boolean revision = false;
        ShefRecord record = null;

        // Synchronize to the start of the data.
        ParserToken t = null;
        while (parts.size() > 0) {
            t = parts.remove(partsIndex);
            if (STARTTYPES.containsKey(t.getType())) {
                break;
            }
        }

        if (parts.size() <= 0) {
            log.error("No data parsed from report.\n" + message);
            return record;
        }
        record = new ShefRecord();
        setCurrentQualifier("Z");
        switch (t.getType()) {
        case A_REC_R: {
            revision = true;
        }
        case A_REC: {
            record.setShefType(ShefRecord.ShefType.A);
            record.setRevisedRecord(revision);
            if ((record = parseARecord(record)) != null) {
                record.setLocationId(locationId);
            }
            break;
        }
        case B_REC_R: {
            revision = true;
        }
        case B_REC: {
            record.setShefType(ShefRecord.ShefType.B);
            record.setRevisedRecord(revision);
            if ((record = parseBRecord(record)) != null) {
                record.setLocationId(locationId);
            }
            break;
        }
        case E_REC_R: {
            revision = true;
        }
        case E_REC: {
            record.setShefType(ShefRecord.ShefType.E);
            record.setRevisedRecord(revision);
            if ((record = parseERecord(record)) != null) {
                record.setLocationId(locationId);
            }
            break;
        }
        default: {
            // Could not determine the record type.
            record = null;
        }
        } // switch
        if (record != null) {
            if (productDate != null) {
                record.setProductTime(productDate);
            } else {
                record.setProductTime(new Date());
            }

            // Set up the default product id
            String identifier = "MSGPRODID";
            if (wmoHeader != null) {
                if (awipsHeader != null) {
                    if(awipsHeader.length() <= 6) {
                        identifier = wmoHeader.getCccc() + awipsHeader;
                    } else {
                        identifier = awipsHeader;
                    }
                }
            }
            record.setIdentifier(identifier);

            List<ShefData> data = record.getDataValues();
            if (data != null) {
                for (int i = 0; i < data.size();) {
                    ShefData d = data.get(i);
                    if (emitSkippedValues) {
                        d.toPostData();
                        i++;
                    } else {
                        if (ShefConstants.SHEF_SKIPPED.equals(d
                                .getStringValue())) {
                            data.remove(i);
                        } else {
                            d.toPostData();
                            i++;
                        }
                    }
                }
            }
        }
        return record;
    }

    // *********************************
    // * A Record specific methods.
    // *********************************

    /**
     * 
     * @param record
     * @return
     */
    private ShefRecord parseARecord(ShefRecord record) {
        reportLead = null;
        if (getPositionalData()) {
            StringBuilder sb = new StringBuilder();
            PRIMARY: for (int i = 0; i < parts.size();) {
                ParserToken t = parts.remove(i);
                sb.append(t.getRawToken());
                switch (t.getType()) {
                case TIMEZONE: {
                    break PRIMARY;
                }
                case OBS_DATE_4:
                case OBS_DATE_6:
                case OBS_DATE_8: {
                    record.setRecordDate(t.getDateData());
                    break;
                }
                }
                sb.append(" ");
            }
            reportLead = sb.toString();
        
            identifyUnknownToken(parts, false);

            if(!validateRecord(parts,record)) {
                return record;
            }
            int error = getObsDate().getError(); 
            if(error != 0) {
                statusReporting(record, ERR_LOGGER, SHEFErrors.HANDLERS.ERROR, error);
                return record;
            }
            masterDate = new SHEFDate(getObsDate());
            setAdjObsDate(masterDate);

            SHEFDate d = null;
            String pedtsep = null;
            String value = null;
            String qualifier = getCurrentQualifier();
            String retainedComment = null;
            boolean trace = false;
            int errorCode = 0;
            boolean reSync = false;
            boolean dateRelative = false;
            ShefData lastData = null;
            for (ParserToken token : parts) {
                int err = token.getError();
                if (err < ParserToken.ERR_NO_ERROR) {
                    switch (err) {
                    case ParserToken.ERR_INV_CREATE_DATE: {
                        statusReporting(record, ERR_LOGGER, SHEFErrors.HANDLERS.ERROR, SHEFErrorCodes.LOG_019);
                        value = null;
                        break;
                    }
                    case ParserToken.ERR_INV_JUL_DATE: {
                        statusReporting(record, ERR_LOGGER, SHEFErrors.HANDLERS.ERROR, SHEFErrorCodes.LOG_016);
                        value = null;
                        break;
                    }
                    case ParserToken.ERR_INVALID_QUAL: {
                        statusReporting(record, ERR_LOGGER, SHEFErrors.HANDLERS.ERROR, SHEFErrorCodes.LOG_021);
                        value = null;
                        break;
                    }
                    case ParserToken.ERR_INV_SECONDS: {
                        statusReporting(record, ERR_LOGGER, SHEFErrors.HANDLERS.ERROR, SHEFErrorCodes.LOG_016);
                        value = null;
                        break;
                    }
                    case ParserToken.ERR_INV_MINUTES: {
                        statusReporting(record, ERR_LOGGER, SHEFErrors.HANDLERS.ERROR, SHEFErrorCodes.LOG_016);
                        value = null;
                        break;
                    }
                    case ParserToken.ERR_INV_HOURS: {
                        statusReporting(record, ERR_LOGGER, SHEFErrors.HANDLERS.ERROR, SHEFErrorCodes.LOG_016);
                        value = null;
                        break;
                    }
                    case ParserToken.ERR_INV_DAY: {
                        statusReporting(record, ERR_LOGGER, SHEFErrors.HANDLERS.ERROR, SHEFErrorCodes.LOG_017);
                        value = null;
                        break;
                    }
                    case ParserToken.ERR_INV_MONTH: {
                        statusReporting(record, ERR_LOGGER, SHEFErrors.HANDLERS.ERROR, SHEFErrorCodes.LOG_017);
                        value = null;
                        break;
                    }
                    case ParserToken.ERR_LOG035 : {
                        statusReporting(record, ERR_LOGGER, SHEFErrors.HANDLERS.ERROR, SHEFErrorCodes.LOG_035);
                        value = null;
                        break;
                    }
                    case ParserToken.ERR_LOG044 : {
                        statusReporting(record, ERR_LOGGER, SHEFErrors.HANDLERS.ERROR, SHEFErrorCodes.LOG_044);
                        value = null;
                        reSync = true;
                        break;
                    }
                    case ParserToken.ERR_LOG079 : {
                        statusReporting(record, ERR_LOGGER, SHEFErrors.HANDLERS.WARNING, SHEFErrorCodes.LOG_035);
                        break;
                    }
                    }
                    break;
                } else if(err > 0) {
                    statusReporting(record, ERR_LOGGER, SHEFErrors.HANDLERS.ERROR, err);
                    value = null;
                    break;
                }
                if(reSync) {
                    break;
                }
                switch (token.getType()) {
                case UNITS_CODE: {

                    currentUnits = token.getToken().substring(2);
                    if(!isValidUnits(currentUnits)) {
                        // Handle the error condition
                        statusReporting(record, ERR_LOGGER, SHEFErrors.HANDLERS.ERROR, SHEFErrorCodes.LOG_022);
                        // and return with the legal data found so far.
                        return record;
                    }
                    break;
                }
                case QUAL_CODE: {
                    String q = token.getToken().substring(2);
                    if(isValidQualityCode(q)) {
                        setCurrentQualifier(q);
                        qualifier = getCurrentQualifier();
                    } else {
                        // Handle the error condition
                        statusReporting(record, ERR_LOGGER, SHEFErrors.HANDLERS.ERROR, SHEFErrorCodes.LOG_085);
                        // and return with the legal data found so far.
                        return record;
                    }
                    break;
                }
                case DUR_CODE: {
                    processDuration(token, false);
                    break;
                }
                case DATE_SEC:
                case DATE_MIN:
                case DATE_HOUR:
                case DATE_DAY:
                case DATE_MON:
                case DATE_YEAR:
                case DATE_DATE:
                case DATE_JUL: {
                    if ((d = masterDate.applyData(token)) != null) {
                        if(d.isDSTExclusion()) {
                            statusReporting(record, ERR_LOGGER, SHEFErrors.HANDLERS.ERROR, SHEFErrorCodes.LOG_044);
                            errorCode = 1; 
                        } else {
                            masterDate = d;
                            setObsDate(d);
                            resetAdjObsDate();
                        }
                    } else {
                        errorCode = 1;
                    }
                    break;
                }
                case DATE_REL: {
                    if ((d = getObsDate().applyData(token)) != null) {
                        if(d.isDSTExclusion()) {
                            statusReporting(record, ERR_LOGGER, SHEFErrors.HANDLERS.ERROR, SHEFErrorCodes.LOG_044);
                            errorCode = 1; 
                        } else {
                            setAdjObsDate(d);
                        }
                    } else {
                        errorCode = 1;
                    }
                    dateRelative = true;
                    break;
                }
                case DATE_CREATE: {
                    setCreateTime(token.getDateData());
                    break;
                }
                case EMPTY: {
                    value = ShefConstants.SHEF_SKIPPED;
                    retainedComment = null;
                    break;
                }
                case SLASH: {
                    // We only want to process a new data item if we've set
                    // the data.
                    retainedComment = null;
                    break;
                }
                case PEDTSEP: {
                    String s = null;
                    if(dateRelative) {
                        s = token.getSendCode();
                        if((s != null) && (s.length() >= 2)) {
                            s = s.substring(0,2);
                            if(EOD_SENDCODES.contains(s)) {
                                // this is an error condition
                                errorCode = SHEFErrorCodes.LOG_035;
                                statusReporting(record, ERR_LOGGER, SHEFErrors.HANDLERS.ERROR, errorCode);
                                trace = false;
                                reSync = false;
                                value = null;
                                pedtsep = null;
                                qualifier = getCurrentQualifier();
                                break;
                            }
                        }
                    }
                    
                    s = token.getToken();
                    int currError = ShefUtil.validatePEDTSEP(s);
                    if (currError == 0) {
                        PhysicalElement pe = PhysicalElement.getEnum(s
                                .substring(0, 2));
                        if (!PhysicalElement.UNKNOWN.equals(pe)) {
                            pedtsep = s;
                        } else {
                            pedtsep = null;
                        }
                    } else {
                        // Handle the error condition
                        statusReporting(record, ERR_LOGGER, SHEFErrors.HANDLERS.ERROR, currError);

                        pedtsep = null;
                        // Reset the qualifier back if it was overridden
                        reSync = true;
                    }
                    value = null;
                    qualifier = getCurrentQualifier();
                    break;
                }
                case QNUMERIC: {
                    if (!reSync) {
                        String s = token.getQualifier();
                        if(!isValidQualityCode(s)) {
                            statusReporting(record, ERR_LOGGER, SHEFErrors.HANDLERS.ERROR, SHEFErrorCodes.LOG_021);
                            value = null;
                        } else {
                            qualifier = s;
                            s = token.getToken();
                            value = s.substring(0, s.length() - 1);
                            trace = token.isTrace();
                        }
                    }
                    break;
                }
                case NUMERIC: {
                    if (!reSync) {
                        value = token.getToken();
                        trace = token.isTrace();
                    }
                    break;
                }
                case RETAINEDCOMMENT: {
                    if(lastData != null) {
                        lastData.setRetainedComment(token.getToken());
                        retainedComment = null;
                    } else {
                        retainedComment = token.getToken();
                    }
                    break;
                }
                case UNKNOWN: {
                    if (isMissingValue(token.getToken())) {
                        value = ShefConstants.SHEF_MISSING;
                        qualifier = getMissingQualifier(token.getToken());
                    } else if (isTraceValue(token.getToken())) {
                        value = ShefConstants.SHEF_TRACE;
                        trace = true;
                    } else {
                        value = null;
                    }
                    break;
                }
                
                default: {

                    // Handle the error condition
                    statusReporting(record, ERR_LOGGER, SHEFErrors.HANDLERS.ERROR, SHEFErrorCodes.LOG_064);

                    pedtsep = null;
                    value = null;
                    // Reset the qualifier back if it was overridden
                    qualifier = getCurrentQualifier();
                    retainedComment = null;
                    reSync = true;
                }
                } // switch
                if ((pedtsep != null) && (value != null)) {
                    ShefData data = new ShefData();
                    data.setParameterCodeString(pedtsep, currentDuration);
                    data.setLocationId(getLocationId());
                    data.setObservationTime(record.getRecordDate());
                    data.setObsTime(getAdjObsDate());
                    data.setCreateTime(getCreateTime());

                    data.setUnitsCode(getCurrentUnits());
                    data.setStringValue(value);
                    data.setQualifier(qualifier);
                    if(retainedComment != null) {
                        data.setRetainedComment(retainedComment);
                        retainedComment = null;
                    } else {
                        lastData = data;
                    }
                    data.setRevisedRecord(record.isRevisedRecord());
                    data.fixupDuration(durationValue);
                    if (trace) {
                        if (legalTraceValue(data.getPhysicalElement())) {
                            record.addDataValue(data);
                        } else {
                            statusReporting(record, ERR_LOGGER,
                                    SHEFErrors.HANDLERS.ERROR,
                                    SHEFErrorCodes.LOG_031);
                        }
                    } else {
                        record.addDataValue(data);
                    }

                    value = null;
                    pedtsep = null;
                    // Reset the qualifier back if it was overridden
                    qualifier = getCurrentQualifier();
                    trace = false;

                    reSync = false;
                }                
                if (errorCode > 0) {
                    // clear out the last value.
                    value = null;

                    break;
                }
            } // for
        }
        return record;
    }

    /**
     * Invalidate this record if PE in {HY QY PY} and the timezone is zulu.
     * 
     */
    private boolean validateRecord(List<ParserToken> tokens, ShefRecord record) {
        boolean isValid = true;
        int error = 0;
        for (ParserToken token : tokens) {

            if (token != null) {
                String pe = token.getSendCode();
                if (pe != null) {
                    if(pe != null) {
                        if(pe.startsWith("HY") || pe.startsWith("QY") || pe.startsWith("PY")) {
                            if ("Z".equals(timeZone)) {
                                isValid = false;
                                error = SHEFErrorCodes.LOG_035;
                            }
                        }
                    }
                }
            }
        }
        if (!isValid) {
            statusReporting(record, ERR_LOGGER, SHEFErrors.HANDLERS.ERROR, error);
        }
        return isValid;
    }
    
    // *********************************
    // * B Record specific methods.
    // *********************************

    /**
     * 
     * @param record
     * @return
     */
    private ShefRecord parseBRecord(ShefRecord record) {
        reportLead = null;

        parseLineData();

        int tStart = -1;
        int tEnd = -1;
        for (int i = 0; i < parts.size(); i++) {
            ParserToken t = parts.get(i);
            if (TokenType.B_PATTERN.equals(t.getType())) {
                tStart = i;
            } else if (TokenType.B_DATA.equals(t.getType())) {
                tEnd = i;
                break;
            }
        }
        List<ParserToken> patternList = null;
        if ((tStart >= 0) && (tEnd > tStart)) {
            patternList = parts.subList(tStart + 1, tEnd);
            List<ParserToken> saveList = parts;
            parts = patternList;
            partsIndex = 0;
            if (getPositionalData()) {
                identifyUnknownToken(parts, false);

                int error = getObsDate().getError(); 
                if(error != 0) {
                    statusReporting(record, ERR_LOGGER, SHEFErrors.HANDLERS.ERROR, error);
                    return record;
                }

                masterDate = new SHEFDate(getObsDate());
                bRecordDataSource = getLocationId();

                parts = saveList;
                fixupDates(parts, tz);

                List<ParserToken> pattern = new ArrayList<ParserToken>();
                List<List<ParserToken>> bdata = new ArrayList<List<ParserToken>>();
                int idx = 0;
                boolean end = false;
                boolean addToken = false;
                for (; !end && idx < parts.size(); idx++) {
                    ParserToken t = parts.get(idx);
                    if (TokenType.B_PATTERN.equals(t.getType())) {
                        addToken = true;
                        continue;
                    } else if (TokenType.B_DATA.equals(t.getType())) {
                        break;
                    }
                    if (addToken) {
                        pattern.add(t);
                    }
                }
                List<ParserToken> bLine = new ArrayList<ParserToken>();
                addToken = false;
                for (; !end && idx < parts.size(); idx++) {
                    ParserToken t = parts.get(idx);
                    if (TokenType.B_END.equals(t.getType())) {
                        if (bLine.size() > 0) {
                            bdata.add(bLine);
                        }
                        break;
                    } else if (TokenType.B_DATA.equals(t.getType())) {
                        if (bLine.size() > 0) {
                            bdata.add(bLine);
                        }
                        bLine = new ArrayList<ParserToken>();
                        addToken = true;
                        continue;
                    }
                    if (addToken) {
                        bLine.add(t);
                    }
                }
                // if the pattern is not valid, ditch the rest of the message.
                if (validatePattern(pattern)) {
                    for (List<ParserToken> subList : bdata) {
                        identifyBData(subList);
                        identifyUnknownToken(
                                subList.subList(1, subList.size()), true);
                        // Make a copy of the master date so that each data line
                        // gets a clean copy.
                        SHEFDate localMaster = new SHEFDate(masterDate);
                        try {
                            interpretData(record, pattern, subList, localMaster);
                        } catch (Exception e) {
                            ERR_LOGGER.error(getClass(), createRecordHeader(record, reportLead)
                                    + createDataLine(pattern));
                            ERR_LOGGER.error(getClass(), createDataLine(subList));
                            ERR_LOGGER.error(getClass(), "?");
                            ERR_LOGGER.error(getClass(), "Exception " + e.getLocalizedMessage());
                            ERR_LOGGER.error(getClass(), SHEFErrorCodes.LOG_090);
                        }
                        
                    }
                }
            }
        }

        return record;
    }

    /**
     * Iterate through a B record pattern and determine if all elements are
     * defined.
     * 
     * @param pattern
     * @return
     */
    private boolean validatePattern(List<ParserToken> pattern) {
        // Assume the pattern is ok until we prove otherwise.
        boolean valid = true;
        TokenType SLASH = TokenType.SLASH;
        int error = 0;

        int currPos = -1;
        for(ParserToken t : pattern) {
            currPos++;
            if(t.getError() != ParserToken.ERR_NO_ERROR) {
                valid = false;
                error = t.getError();
                break;
            }
        }
        if(valid) {
            TokenType type = TokenType.NIL;
            ParserToken currToken = null;
            TokenType lastType = TokenType.NIL;
            boolean peFound = false;
            currPos = -1;
            do {
                currPos++;
                valid = (currPos < pattern.size());
                if(valid) {
                    currToken = pattern.get(currPos);
                    lastType = type;
                    type = currToken.getType();
                }
            } while(valid && (!(SLASH.equals(type))));
            // Don't start other validation until we find the first /
            for (; (currPos < pattern.size()) && valid; currPos++) {
                currToken = pattern.get(currPos);
                TokenType currType = TokenType.NIL;
                valid = (currToken != null);
                if (valid) {
                    currType = currToken.getType();
                    valid = ((type != null) && (!TokenType.INVALID_B_PATTERN
                            .contains(currType)));
                    if (valid) {
                        // Current token is not a SLASH so last token should be
                        if (!SLASH.equals(currType)) {
                            valid = (SLASH.equals(lastType));
                        }
                    }
                }
                if(TokenType.PEDTSEP.equals(currType)) {
                    peFound = true;
                }
                lastType = currToken.getType();
            } // for
            // If we didn't find a pe, invalidate this pattern
            valid &= peFound;
            if(valid) {
                for (ParserToken token : pattern) {
                    if (token != null) {
                        String pe = token.getSendCode();
                        
                        if (pe != null) {
                            if(pe.startsWith("HY")||pe.startsWith("QY")||pe.startsWith("PY")) {
                                // if we found any of the above, examine the timezone
                                // to see if it is ZULU
                                if ("Z".equals(timeZone)) {
                                    token.setError(ParserToken.ERR_LOG035);
                                }
                            }
                        }
                    }
                }
            } else {
                // this isn't right, leave it for now
                error = SHEFErrorCodes.LOG_003;
            }
            if(valid) {
                for (ParserToken t : pattern) {

                    TokenType tt = t.getType();
                    switch (tt) {
                    case PEDTSEP: {
                        error = ShefUtil.validatePEDTSEP(t.getRawToken());
                        valid = (error == 0);
                        break;
                    }
                    case UNITS_CODE: {
                        valid = isValidUnits(t.getRawToken().substring(2));
                        error = SHEFErrorCodes.LOG_022;
                        break;
                    }
                    case QUAL_CODE: {
                        valid = isValidQualityCode(t.getRawToken().substring(2));
                        error = SHEFErrorCodes.LOG_085;
                        break;
                    }
                    } // switch
                    if(!valid) {
                        break;
                    }
                } // for
            }
        }
        if (!valid) {
            // points to the offending token
            StringBuilder sb = new StringBuilder();
            sb.append(pattern.get(0).getRawToken());
            int pos = 0;
            for (int i = 0; i < currPos; i++) {
                sb.append(" ");
                pos = sb.length();
                sb.append(pattern.get(i).getRawToken());
            }
            String s = String.format("%" + String.format("%d", pos + 1) + "s?",
                    " ");
            ERR_LOGGER.error(getClass(), createDataLine(pattern));
            ERR_LOGGER.error(getClass(), s);
            ERR_LOGGER.error(getClass(), error);
        }
        return valid;
    }

    /**
     * 
     * @param subList
     */
    private void identifyBData(List<ParserToken> subList) {

        TokenType last = TokenType.NIL;
        TokenType next = TokenType.LOC_ID;
        for (int i = 0; i < subList.size();) {
            ParserToken t = subList.get(i);
            switch (t.getType()) {
            case B_DATA: {
                next = TokenType.LOC_ID;
                i++;
                last = t.getType();
                break;
            } // case
            case UNKNOWN: {
                if (TokenType.LOC_ID.equals(next)) {
                    ParserToken tt = new ParserToken(t.getToken(),
                            TokenType.LOC_ID);
                    subList.set(i, tt);
                    last = TokenType.LOC_ID;
                    next = TokenType.UNKNOWN;
                    i++;
                } else {
                    i++;
                    last = t.getType();
                }
                break;
            } // case
            case SLASH: {
                if (TokenType.LOC_ID.equals(last)) {
                    subList.add(i, new ParserToken("", TokenType.EMPTY));
                    // Move past the added token.
                    i++;
                }
                last = t.getType();
                // Now move past the SLASH token.
                i++;
                break;
            }
            case SPACE: {
                if (i < subList.size() - 1) {
                    ParserToken tt = subList.get(i + 1);
                    if (TokenType.SLASH.equals(tt.getType())) {
                        if (TokenType.SLASH.equals(last)) {
                            // only set an empty if the last token was a SLASH
                            subList.set(i, new ParserToken("", TokenType.EMPTY));
                            i++;
                        } else {
                            subList.remove(i);
                        }
                    } else {
                        subList.remove(i);
                    }
                } else {
                    // Space at the end of the record, set to EMPTY
                    subList.set(i, new ParserToken("", TokenType.EMPTY));
                    i++;
                }
                break;
            }
            default: {
                last = t.getType();
                i++;
            }
            } // switch
        }
    }

    /**
     * 
     */
    private void parseLineData() {
        List<ParserToken> newTokens = new ArrayList<ParserToken>();
        newTokens.add(new ParserToken("", TokenType.B_PATTERN));
        TokenType last = TokenType.NIL;
        for (ParserToken t : parts) {
            switch (t.getType()) {
            case COMMA:
            case NEWLINE: {
                if (TokenType.SLASH.equals(last)) {
                    newTokens.add(new ParserToken("", TokenType.EMPTY));
                }
                newTokens.add(new ParserToken("", TokenType.B_DATA));
                last = TokenType.B_DATA;
                break;
            }
            case UNKNOWN: {
                String s = t.getToken();
                if (".END".equals(s)) {
                    newTokens.add(new ParserToken(s, TokenType.B_END));
                } else {
                    newTokens.add(t);
                }
                last = TokenType.NIL;
                break;
            }
            case SPACE: {
                if (!TokenType.B_DATA.equals(last)) {
                    newTokens.add(t);
                    last = TokenType.NIL;
                }
                break;
            }
            default: {
                newTokens.add(t);
                last = t.getType();
            }
            }
        }
        if (newTokens.size() > 0) {
            parts = newTokens;
        }
    }

    /**
     * 
     * @param record
     * @param pattern
     * @param bdata
     * @param localMaster
     */
    private void interpretData(ShefRecord record, List<ParserToken> pattern,
            List<ParserToken> bdata, SHEFDate localMaster) {

        SHEFDate d = null;
        String pedtsep = null;
        String value = null;
        String qualifier = getCurrentQualifier();
        String retainedComment = null;

        // clear any overrides for each bdata line
        durationValueOverride = null;
        currentDurationOverride = null;

        String unitsOverride = null;
        String qualifierOverride = null;

        String lid = bdata.get(0).getToken();
        if (ShefUtil.between(ShefConstants.LOWER_LID_LIMIT, lid.length(),
                ShefConstants.UPPER_LID_LIMIT)) {

            identifyNeededSlashes(parts.subList(1, bdata.size()));

            int errorCode = 0;

            int bDataPtr = 1;
            

            ParserToken drCode = null;
            ParserToken drCodeOverride = null;
            
            boolean createOverride = false;
            boolean reSync = false;
            boolean outOfData = false;
            boolean forceExit = false;
            boolean trace = false;
            boolean timeOverride = false;

            for (ParserToken pToken : pattern) {
                
                int exitStatus = tokenError(record, pattern, bdata, pToken); 
                if(exitStatus == 1) {
                    value = null;
                    forceExit = true;
                    break;
                } else if (exitStatus == 2) {
                    value = null;
                    break;
                }
                
                switch (pToken.getType()) {
                case UNITS_CODE: {
                    currentUnits = pToken.getToken().substring(2);
                    if(!isValidUnits(currentUnits)) {
                        ERR_LOGGER.error(getClass(), createRecordHeader(record, reportLead)
                                + createDataLine(pattern));
                        ERR_LOGGER.error(getClass(), createDataLine(bdata));
                        ERR_LOGGER.error(getClass(), " ?");
                        ERR_LOGGER.error(getClass(), SHEFErrorCodes.LOG_022);
                        errorCode = 1;
                    }
                    break;
                }
                case QUAL_CODE: {
                    setCurrentQualifier(pToken.getToken().substring(2));
                    if(!isValidQualityCode(getCurrentQualifier())) {
                        ERR_LOGGER.error(getClass(), createRecordHeader(record, reportLead)
                                + createDataLine(pattern));
                        ERR_LOGGER.error(getClass(), createDataLine(bdata));
                        ERR_LOGGER.error(getClass(), " ?");
                        ERR_LOGGER.error(getClass(), SHEFErrorCodes.LOG_021);
                        errorCode = 1;
                    }
                    break;
                }
                case DUR_CODE: {
                    processDuration(pToken, false);
                    break;
                }
                case DATE_SEC:
                case DATE_MIN:
                case DATE_HOUR:
                case DATE_DAY:
                case DATE_MON:
                case DATE_YEAR:
                case DATE_DATE:
                case DATE_JUL: {
                    if (!timeOverride) {
                        if ((d = localMaster.applyData(pToken)) != null) {
                            if(d.isDSTExclusion()) {
                                statusReporting(record, ERR_LOGGER, SHEFErrors.HANDLERS.ERROR, SHEFErrorCodes.LOG_044);
                                forceExit = true;
                                errorCode = 1; 
                            } else {
                                localMaster = d;
                                setObsDate(d);
                                resetAdjObsDate();
                            }
                            drCode = null;
                        } else {
                            errorCode = 1;
                        }
                    }
                    // Even though the time may not get used because of override,
                    // any date relative codes are cleared.
                    drCode = null;
                    if(!timeOverride) {
                        drCodeOverride = null;
                    }
                    break;
                }
                case DATE_REL: {
                    // just pickup the DR code
                    drCode = pToken;
                    break;
                }
                case DATE_CREATE: {
                    if (!createOverride) {
                        setCreateTime(pToken.getDateData());
                    }
                    break;
                }
                case PEDTSEP: {
                    if (outOfData) {
                        ERR_LOGGER.warning(getClass(),
                                createRecordHeader(record, reportLead)
                                        + createDataLine(pattern));
                        ERR_LOGGER.warning(getClass(), createDataLine(bdata));
                        ERR_LOGGER.warning(getClass(), " ?");
                        ERR_LOGGER.warning(getClass(), SHEFErrorCodes.LOG_042);
                        break;
                    }
                    String s = null;
                    // Need to check both Date Relative codes, so if either
                    // are not null...
                    if((drCode != null) || (drCodeOverride != null)) {
                        s = pToken.getSendCode();
                        if((s != null) && (s.length() >= 2)) {
                            s = s.substring(0,2);
                            if(EOD_SENDCODES.contains(s)) {
                                // this is an error condition
                                errorCode = SHEFErrorCodes.LOG_035;
                                statusReporting(record, ERR_LOGGER, SHEFErrors.HANDLERS.ERROR, errorCode);
                                forceExit = true;
                                reSync = false;
                                break;
                            }
                        }
                    }

                    s = pToken.getToken();
                    int currError = ShefUtil.validatePEDTSEP(s);
                    PhysicalElement pe = null;
                    if (currError == 0) {
                        pe = PhysicalElement.getEnum(s.substring(0, 2));
                        if (!PhysicalElement.UNKNOWN.equals(pe)) {
                            pedtsep = s;
                        }
                        // Is there a duration coded?
                        if(s.length() >= 3) {
                            if("V".equals(s.subSequence(2,3))) {
                                // do we have a variable duration defined?
                                if(currentDurationOverride == null) {
                                    if(currentDuration == null) {
                                        // No duration at all!
                                        currError = SHEFErrorCodes.LOG_032;
                                    } else {
                                        if(!"V".equals(currentDuration)) {
                                            currError = SHEFErrorCodes.LOG_032;
                                        }                                    
                                    }                                    
                                } else {
                                    if(!"V".equals(currentDurationOverride)) {
                                        currError = SHEFErrorCodes.LOG_032;
                                    }                                    
                                }
                                if(currError != 0) {
                                    errorCode = 1; 
                                    forceExit = true;
                                    pedtsep = null;
                                    value = null;
                                    // Reset the qualifier back if it was overridden
                                    qualifier = getCurrentQualifier();
                                    retainedComment = null;
                                    reSync = true;
                                }
                            }
                        }
                    } else {
                        pedtsep = null;
                        value = null;
                        // Reset the qualifier back if it was overridden
                        qualifier = getCurrentQualifier();
                        retainedComment = null;
                        reSync = true;
                    }
                    if(currError != 0) {
                        // Handle the error condition
                        ERR_LOGGER.error(getClass(), createRecordHeader(record, reportLead)
                                + createDataLine(pattern));
                        ERR_LOGGER.error(getClass(), createDataLine(bdata));
                        ERR_LOGGER.error(getClass(), " ?");
                        ERR_LOGGER.error(getClass(), currError);
                    }

                    if(reSync) {
                        break;
                    }
                    
                    qualifier = getCurrentQualifier();

                    boolean empty = false;
                    boolean badData = false;

                    ParserToken bToken = null;
                    boolean dataFound = false;
                    trace = false;
                    ShefData lastData = null;
                    INNER: do {
                        if (bDataPtr >= bdata.size()) {
                            // We only want to process a new data item if we've
                            // set the data. If we have a bad PEDTSEP then skip
                            // this
                            if (!reSync && (value != null)) {

                                ShefData data = new ShefData();
                                data.setParameterCodeString(
                                        pedtsep,
                                        (currentDurationOverride == null) ? currentDuration
                                                : currentDurationOverride);
                                data.setLocationId(lid);
                                data.setDataSource(bRecordDataSource);
                                data.setObservationTime(record.getRecordDate());

                                SHEFDate date = getRelativeDate(localMaster, drCode,
                                        drCodeOverride, record, timeOverride);
                                if(date != null) {
                                    data.setObsTime(date);

                                    data.setCreateTime(getCreateTime());

                                    if (unitsOverride == null) {
                                        data.setUnitsCode(getCurrentUnits());
                                    } else {
                                        data.setUnitsCode(unitsOverride);
                                    }
                                    data.setStringValue(value);
                                    if (qualifierOverride == null) {
                                        data.setQualifier(qualifier);
                                    } else {
                                        data.setQualifier(qualifierOverride);
                                    }
                                    data.setRetainedComment(retainedComment);
                                    data.setRevisedRecord(record.isRevisedRecord());
                                    data.fixupDuration((durationValueOverride == null) ? durationValue
                                            : durationValueOverride);

                                    if (trace) {
                                        if (legalTraceValue(data
                                                .getPhysicalElement())) {
                                            record.addDataValue(data);
                                        } else {
                                            ERR_LOGGER
                                                    .error(getClass(),
                                                            createRecordHeader(record, reportLead)
                                                                    + createDataLine(pattern));
                                            ERR_LOGGER.error(getClass(),
                                                    createDataLine(bdata));
                                            ERR_LOGGER.error(getClass(), " ?");
                                            ERR_LOGGER.error(getClass(),
                                                    SHEFErrorCodes.LOG_031);
                                        }
                                    } else {
                                        record.addDataValue(data);
                                    }
                                } else {
                                    errorCode = 1; 
                                    forceExit = true;
                                }

                                value = null;
                                retainedComment = null;
                                dataFound = true;
                            }
                            outOfData = true;
                            trace = false;
                            break;
                        }
                        bToken = bdata.get(bDataPtr++);

                        exitStatus = tokenError(record, pattern, bdata, bToken); 
                        if(exitStatus == 1) {
                            value = null;
                            forceExit = true;
                            dataFound = true;
                            break;
                        } else if (exitStatus == 2) {
                            value = null;
                            dataFound = true;
                            break;
                        }
                        
                        switch (bToken.getType()) {
                        case DATE_SEC:
                        case DATE_MIN:
                        case DATE_HOUR:
                        case DATE_DAY:
                        case DATE_MON:
                        case DATE_YEAR:
                        case DATE_DATE:
                        case DATE_JUL: {
                            timeOverride = true;
                            if ((d = localMaster.applyData(bToken)) != null) {
                                if(d.getError() == 0) {
                                    if(d.isDSTExclusion()) {
                                        statusReporting(record, ERR_LOGGER, SHEFErrors.HANDLERS.ERROR, SHEFErrorCodes.LOG_044);
                                        forceExit = true;
                                        errorCode = 1; 
                                        dataFound = true;
                                        timeOverride = false;
                                    } else {
                                        localMaster = d;
                                        setObsDate(d);
                                        resetAdjObsDate();
                                    }
                                } else {
                                    statusReporting(record, ERR_LOGGER, SHEFErrors.HANDLERS.ERROR, d.getError());
                                    break INNER;
                                }
                            } else {
                                errorCode = 1;
                            }
//                            // Remove the 'local' override, but leave the
//                            // outer override.
//                            drCodeOverride = null;
                            break;
                        }
                        case DATE_REL: {
                            drCode = null;
                            drCodeOverride = bToken;
                            break;
                        }
                        case DATE_CREATE: {
                            setCreateTime(bToken.getDateData());
                            createOverride = true;
                            break;
                        }
                        case UNITS_CODE: {
                            unitsOverride = bToken.getToken().substring(2);
                            if(!isValidUnits(unitsOverride)) {
                                ERR_LOGGER.error(getClass(), createRecordHeader(record, reportLead)
                                        + createDataLine(pattern));
                                ERR_LOGGER.error(getClass(), createDataLine(bdata));
                                ERR_LOGGER.error(getClass(), " ?");
                                ERR_LOGGER.error(getClass(), SHEFErrorCodes.LOG_022);
                                dataFound = true;
                                errorCode = 1;
                            }
                            break;
                        }
                        case QUAL_CODE: {
                            qualifierOverride = bToken.getToken().substring(2);
                            if(!isValidQualityCode(qualifierOverride)) {
                                ERR_LOGGER.error(getClass(), createRecordHeader(record, reportLead)
                                        + createDataLine(pattern));
                                ERR_LOGGER.error(getClass(), createDataLine(bdata));
                                ERR_LOGGER.error(getClass(), " ?");
                                ERR_LOGGER.error(getClass(), SHEFErrorCodes.LOG_021);
                                dataFound = true;
                                errorCode = 1;
                            }
                            break;
                        }
                        case DUR_CODE: {
                            processDuration(bToken, true);
                            break;
                        }
                        case QNUMERIC: {
                            String ss = bToken.getQualifier();
                            if(!isValidQualityCode(ss)) {
                                ERR_LOGGER.error(getClass(), createRecordHeader(record, reportLead)
                                        + createDataLine(pattern));
                                ERR_LOGGER.error(getClass(), createDataLine(bdata));
                                ERR_LOGGER.error(getClass(), " ?");
                                ERR_LOGGER.error(getClass(), SHEFErrorCodes.LOG_021);
                                value = null;
                            } else {
                                qualifier = ss;
                                ss = bToken.getToken();
                                value = ss.substring(0, ss.length() - 1);
                                trace = bToken.isTrace();
                            }
                            break;
                        }
                        case NUMERIC: {
                            value = bToken.getToken();
                            trace = bToken.isTrace();
                            break;
                        }

                        case RETAINEDCOMMENT: {
                            if(lastData != null) {
                                lastData.setRetainedComment(bToken.getToken());
                                retainedComment = null;
                            } else {
                                retainedComment = bToken.getToken();
                            }
                            break;
                        }
                        case EMPTY: {
                            value = ShefConstants.SHEF_SKIPPED;
                            retainedComment = null;
                            break;
                        }
                        case UNKNOWN: {
                            if (isMissingValue(bToken.getToken())) {
                                value = ShefConstants.SHEF_MISSING;
                                qualifier = getMissingQualifier(bToken.getToken());
                            } else if (isTraceValue(bToken.getToken())) {
                                value = ShefConstants.SHEF_TRACE;
                                trace = true;
                            } else {
                                badData = true;
                            }
                            break;
                        }
                        case SLASH: {
                            // We only want to process a new data item if we've
                            // set the data.
                            if (value != null) {
                                ShefData data = new ShefData();
                                data.setParameterCodeString(
                                        pedtsep,
                                        (currentDurationOverride == null) ? currentDuration
                                                : currentDurationOverride);
                                data.setLocationId(lid);
                                data.setDataSource(bRecordDataSource);
                                data.setObservationTime(record.getRecordDate());
                                SHEFDate date = getRelativeDate(localMaster,
                                        drCode, drCodeOverride, record, timeOverride);
                                if (date != null) {
                                    data.setObsTime(date);
                                    data.setCreateTime(getCreateTime());

                                    if (unitsOverride == null) {
                                        data.setUnitsCode(getCurrentUnits());
                                    } else {
                                        data.setUnitsCode(unitsOverride);
                                    }
                                    data.setStringValue(value);
                                    if (qualifierOverride == null) {
                                        data.setQualifier(qualifier);
                                    } else {
                                        data.setQualifier(qualifierOverride);
                                    }
                                    if (retainedComment != null) {
                                        data.setRetainedComment(retainedComment);
                                        retainedComment = null;
                                    } else {
                                        lastData = data;
                                    }
                                    data.setRevisedRecord(record
                                            .isRevisedRecord());

                                    data.fixupDuration((durationValueOverride == null) ? durationValue
                                            : durationValueOverride);
                                    if (trace
                                            && ShefConstants.SHEF_TRACE
                                                    .equals(value)) {
                                        if (legalTraceValue(data
                                                .getPhysicalElement())) {
                                            record.addDataValue(data);
                                        } else {
                                            ERR_LOGGER
                                                    .error(getClass(),
                                                            createRecordHeader(
                                                                    record,
                                                                    reportLead)
                                                                    + createDataLine(pattern));
                                            ERR_LOGGER.error(getClass(),
                                                    createDataLine(bdata));
                                            ERR_LOGGER.error(getClass(), " ?");
                                            ERR_LOGGER.error(getClass(),
                                                    SHEFErrorCodes.LOG_031);
                                        }
                                    } else {
                                        record.addDataValue(data);
                                    }
                                }

                                value = null;
                                dataFound = true;
                                outOfData = (bDataPtr >= bdata.size());
                            } else {
                                dataFound = (empty || badData);
                            }
                            break;
                        }
                        } // switch()
                    } while (!dataFound);
                    break;
                }
                case TIMEZONE:
                case OBS_DATE_4:
                case OBS_DATE_6:
                case OBS_DATE_8:
                case SLASH: {
                    // Nothing, just catch the token.
                    break;
                }
                case QNUMERIC:
                case NUMERIC:
                case RETAINEDCOMMENT: {
                    log.info(traceId + "- Illegal token [" + pToken
                            + "] in \"B\" Record pattern");
                    break;
                }
                default: {
                    if (log.isDebugEnabled()) {
                        log.debug(traceId + "- Invalid token ["
                                + pToken.getToken() + ":" + pToken.getType()
                                + "] in \"B\" record");
                    }
                }
                } // switch
                if (errorCode > 0) {
                    // clear out the last value.
                    value = null;

                    break;
                }
            } // for
            if (value != null) {
                ShefData data = new ShefData();
                data.setLocationId(lid);
                data.setDataSource(bRecordDataSource);
                data.setObservationTime(record.getRecordDate());
                SHEFDate date = getRelativeDate(localMaster, drCode,
                        drCodeOverride, record, timeOverride);
                if (date != null) {
                    data.setObsTime(date);
                    data.setCreateTime(getCreateTime());

                    if (unitsOverride == null) {
                        data.setUnitsCode(getCurrentUnits());
                    } else {
                        data.setUnitsCode(unitsOverride);
                    }
                    data.setStringValue(value);
                    if (qualifierOverride == null) {
                        data.setQualifier(qualifier);
                    } else {
                        data.setQualifier(qualifierOverride);
                    }
                    data.setParameterCodeString(pedtsep,
                            currentDurationOverride);
                    data.setRetainedComment(retainedComment);
                    data.setRevisedRecord(record.isRevisedRecord());
                    data.fixupDuration((durationValueOverride == null) ? durationValue
                            : durationValueOverride);
                    if (trace && ShefConstants.SHEF_TRACE.equals(value)) {
                        if (legalTraceValue(data.getPhysicalElement())) {
                            record.addDataValue(data);
                        } else {
                            ERR_LOGGER.error(getClass(),
                                    createRecordHeader(record, reportLead)
                                            + createDataLine(pattern));
                            ERR_LOGGER.error(getClass(), createDataLine(bdata));
                            ERR_LOGGER.error(getClass(), " ?");
                            ERR_LOGGER.error(getClass(), SHEFErrorCodes.LOG_031);
                        }
                    } else {
                        record.addDataValue(data);
                    }
                }
            }
            // Check to see if there was more data on the line than
            // there were in the pattern. But make sure we weren't forced
            // out by a different error.
            // add one to the bdata.size because the bDataPtr doesn't get
            // incremented the last time through
            if (bDataPtr+1 < bdata.size() && !forceExit) {
                ERR_LOGGER.warning(getClass(), createRecordHeader(record, reportLead)
                        + createDataLine(pattern));
                ERR_LOGGER.warning(getClass(), createDataLine(bdata));
                ERR_LOGGER.warning(getClass(), " ?");
                ERR_LOGGER.warning(getClass(), SHEFErrorCodes.LOG_041);
            }
        } else {
            ERR_LOGGER.error(getClass(), createRecordHeader(record, reportLead)
                    + createDataLine(pattern));
            ERR_LOGGER.error(getClass(), createDataLine(bdata));
            ERR_LOGGER.error(getClass(), " ?");
            ERR_LOGGER.error(getClass(), SHEFErrorCodes.LOG_047);
        }
    }

    private int tokenError(ShefRecord record, List<ParserToken> pattern,
            List<ParserToken> bdata, ParserToken token) {
        int errorCondition = 0;
        
        
        int err = token.getError();
        if (err < ParserToken.ERR_NO_ERROR) {
            ERR_LOGGER.error(getClass(), createDataLine(pattern));
            ERR_LOGGER.error(getClass(), createDataLine(bdata));
            ERR_LOGGER.error(getClass(), " ?");
            switch (err) {
            case ParserToken.ERR_INV_CREATE_DATE: {
                ERR_LOGGER.error(getClass(), SHEFErrorCodes.LOG_019);
                errorCondition = 1;
                break;
            }
            case ParserToken.ERR_INV_JUL_DATE: {
                ERR_LOGGER.error(getClass(), SHEFErrorCodes.LOG_016);
                errorCondition = 1;
                break;
            }
            case ParserToken.ERR_INVALID_QUAL: {
                ERR_LOGGER.error(getClass(), SHEFErrorCodes.LOG_021);
                errorCondition = 1;
                break;
            }
            case ParserToken.ERR_INV_SECONDS: {
                ERR_LOGGER.error(getClass(), SHEFErrorCodes.LOG_016);
                errorCondition = 1;
                break;
            }
            case ParserToken.ERR_INV_MINUTES: {
                ERR_LOGGER.error(getClass(), SHEFErrorCodes.LOG_016);
                errorCondition = 1;
                break;
            }
            case ParserToken.ERR_INV_HOURS: {
                ERR_LOGGER.error(getClass(), SHEFErrorCodes.LOG_016);
                errorCondition = 1;
                break;
            }
            case ParserToken.ERR_INV_DAY: {
                ERR_LOGGER.error(getClass(), SHEFErrorCodes.LOG_017);
                errorCondition = 1;
                break;
            }
            case ParserToken.ERR_INV_MONTH: {
                ERR_LOGGER.error(getClass(), SHEFErrorCodes.LOG_017);
                errorCondition = 1;
                break;
            }
            case ParserToken.ERR_LOG035 : {
                ERR_LOGGER.error(getClass(), SHEFErrorCodes.LOG_035);
                errorCondition = 1;
                break;
            }
            case ParserToken.ERR_LOG044 : {
                ERR_LOGGER.error(getClass(), SHEFErrorCodes.LOG_044);
                errorCondition = 1;
                break;
            }
            case ParserToken.ERR_LOG079 : {
                ERR_LOGGER.warning(getClass(), SHEFErrorCodes.LOG_079);
                errorCondition = 2;
                break;
            }
            }
        } else if(err > 0) {
            statusReporting(record, ERR_LOGGER, SHEFErrors.HANDLERS.ERROR, err);
            errorCondition = 1;
        }
       
        return errorCondition;
    }
    
    /**
     * 
     * @param baseTime
     * @param drOuter
     * @param drInner
     * @param record
     * @return
     */
    private SHEFDate getRelativeDate(SHEFDate baseTime, ParserToken drOuter,
            ParserToken drInner, ShefRecord record, boolean overRide) {
        SHEFDate date = null;
        ParserToken dateRelative = null;
        if ((drOuter != null)&&(TokenType.DATE_REL.equals(drOuter.getType()))) {
            if (drInner == null) {
                dateRelative = drOuter;
            } else {
                if(TokenType.DATE_REL.equals(drInner.getType())) {
                    dateRelative = drInner;
                } else {
                    date = new SHEFDate();
                    date.copyFrom(baseTime);
                }
            }
        } else {
            if ((drInner != null) && (TokenType.DATE_REL.equals(drInner.getType()))) {
                dateRelative = drInner;
            } else {
                date = new SHEFDate();
                date.copyFrom(baseTime);
            }
        }
        if (dateRelative != null) {
            if ((date = baseTime.applyData(dateRelative)) != null) {
                if (date.isDSTExclusion()) {
                    statusReporting(record, ERR_LOGGER,
                            SHEFErrors.HANDLERS.ERROR, SHEFErrorCodes.LOG_044);
                }
            }
        }

        return date;
    }

    // *********************************
    // * E Record specific methods.
    // *********************************

    /**
     * 
     * @param record
     * @return
     */
    private ShefRecord parseERecord(ShefRecord record) {
        reportLead = null;
        
        if (getPositionalData()) {
            record.setTimeZone(tz);
            correctMissingDelimiters();

            StringBuilder sb = new StringBuilder();
            PRIMARY: for (int i = 0; i < parts.size();) {
                ParserToken t = parts.remove(i);
                sb.append(t.getRawToken());
                
                switch (t.getType()) {
                case TIMEZONE: {
                    break PRIMARY;
                }
                case OBS_DATE_4:
                case OBS_DATE_6:
                case OBS_DATE_8: {
                    record.setRecordDate(t.getDateData());
                    break;
                }
                }
                sb.append(" ");
            }
            reportLead = sb.toString();
            
            identifyUnknownToken(parts, false);

            if(!validateERecord(record)) {
                return record;
            }
            
            int error = getObsDate().getError(); 
            if(error != 0) {
                statusReporting(record, ERR_LOGGER, SHEFErrors.HANDLERS.ERROR, error);
                return record;
            }

            masterDate = new SHEFDate(getObsDate());
            // The interval parameter is used within an "E" record to hold
            // the time increment "DI" value to be applied to the obsTime to
            // create the actual observation time.
            ParserToken interval = findInterval(parts);

            resetAdjObsDate();
            if (interval != null) {
                int intervalPos = parts.indexOf(interval);
                identifyNeededSlashes(parts.subList(intervalPos, parts.size()));

                // Now traverse the remaining token list and process out
                // each token.
                String pedtsep = null;
                record.setCreationDate(createTime);
                String value = null;
                String qualifier = getCurrentQualifier();
                String retainedComment = null;
                boolean reSync = false;
                boolean haveInt = false;
                boolean inData = false;
                boolean trace = false;

                int seriesSequence = 0;
                SHEFDate d = null;
                ShefData lastData = null;
                for (ParserToken token : parts) {
                    int err = token.getError();
                    if (err < ParserToken.ERR_NO_ERROR) {
                        switch (err) {
                        case ParserToken.ERR_INV_CREATE_DATE: {
                            statusReporting(record, ERR_LOGGER, SHEFErrors.HANDLERS.ERROR, SHEFErrorCodes.LOG_019);
                            break;
                        }
                        case ParserToken.ERR_INV_JUL_DATE: {
                            statusReporting(record, ERR_LOGGER, SHEFErrors.HANDLERS.ERROR, SHEFErrorCodes.LOG_079);
                            break;
                        }
                        case ParserToken.ERR_INVALID_QUAL: {
                            statusReporting(record, ERR_LOGGER, SHEFErrors.HANDLERS.ERROR, SHEFErrorCodes.LOG_021);
                            break;
                        }
                        case ParserToken.ERR_INV_SECONDS: {
                            statusReporting(record, ERR_LOGGER, SHEFErrors.HANDLERS.ERROR, SHEFErrorCodes.LOG_016);
                            break;
                        }
                        case ParserToken.ERR_INV_MINUTES: {
                            statusReporting(record, ERR_LOGGER, SHEFErrors.HANDLERS.ERROR, SHEFErrorCodes.LOG_016);
                            break;
                        }
                        case ParserToken.ERR_INV_HOURS: {
                            statusReporting(record, ERR_LOGGER, SHEFErrors.HANDLERS.ERROR, SHEFErrorCodes.LOG_016);
                            break;
                        }
                        case ParserToken.ERR_INV_DAY: {
                            statusReporting(record, ERR_LOGGER, SHEFErrors.HANDLERS.ERROR, SHEFErrorCodes.LOG_017);
                            break;
                        }
                        case ParserToken.ERR_INV_MONTH: {
                            statusReporting(record, ERR_LOGGER, SHEFErrors.HANDLERS.ERROR, SHEFErrorCodes.LOG_017);
                            break;
                        }
                        case ParserToken.ERR_LOG044 : {
                            statusReporting(record, ERR_LOGGER, SHEFErrors.HANDLERS.ERROR, SHEFErrorCodes.LOG_017);
                            break;
                        }
                        case ParserToken.ERR_LOG079 : {
                            statusReporting(record, ERR_LOGGER, SHEFErrors.HANDLERS.WARNING, SHEFErrorCodes.LOG_079);
                            break;
                        }
                        default : {
                            statusReporting(record, ERR_LOGGER, SHEFErrors.HANDLERS.WARNING, SHEFErrorCodes.LOG_090);
                        }
                        }
                        value = null;
                        break;
                    } else if(err > 0) {
                        statusReporting(record, ERR_LOGGER, SHEFErrors.HANDLERS.ERROR, err);
                        value = null;
                        break;
                    }
                    switch (token.getType()) {
                    case QUAL_CODE: {
                        qualifier = token.getToken().substring(2);
                        setCurrentQualifier(qualifier);
                        break;
                    }
                    case UNITS_CODE: {
                        setCurrentUnits(token.getToken().substring(2));
                        break;
                    }
                    case DUR_CODE: {
                        processDuration(token, false);
                        break;
                    }
                    case PEDTSEP: {
                        if (!inData) {
                            String s = token.getToken();
                            int currError = ShefUtil.validatePEDTSEP(s);

                            if (currError == 0) {
                                PhysicalElement pe = PhysicalElement.getEnum(s.substring(0, 2));
                                if (!PhysicalElement.UNKNOWN.equals(pe)) {
                                    pedtsep = s;
                                }
                                // Is there a duration coded?
                                if(s.length() >= 3) {
                                    if("V".equals(s.subSequence(2,3))) {
                                        // do we have a variable duration defined?
                                        if(!"Z".equals(currentDuration)) {
                                            if("Z".equals(currentDurationOverride)) {
                                                currError = SHEFErrorCodes.LOG_032;
                                            }
                                        } else {
                                            currError = SHEFErrorCodes.LOG_032;
                                        }
                                    }
                                }
                            }
                            if(currError != 0) {
                                // Handle the error condition
                                ERR_LOGGER.error(getClass(),
                                        createRecordHeader(record, reportLead)
                                                + createDataLine(parts));
                                ERR_LOGGER.error(getClass(), " ?");
                                ERR_LOGGER.error(getClass(), currError);

                                pedtsep = null;
                                value = null;
                                // Reset the qualifier back if it was overridden
                                qualifier = getCurrentQualifier();
                                retainedComment = null;
                                reSync = true;
                            }
                        } else {
                            // can't redeclare the PE once data processing has
                            // started.
                            // Handle the error condition
                            statusReporting(record, ERR_LOGGER, SHEFErrors.HANDLERS.ERROR, SHEFErrorCodes.LOG_101);
                            reSync = true;
                        }
                        break;
                    }
                    case INT_CODE: {
                        interval = token;
                        haveInt = true;
                        break;
                    }
                    case DATE_SEC:
                    case DATE_MIN:
                    case DATE_HOUR:
                    case DATE_DAY:
                    case DATE_MON:
                    case DATE_YEAR:
                    case DATE_DATE:
                    case DATE_JUL: {
                        if ((d = masterDate.applyData(token)) != null) {
                            if(d.isDSTExclusion()) {
                                statusReporting(record, ERR_LOGGER, SHEFErrors.HANDLERS.ERROR, SHEFErrorCodes.LOG_044);
                                pedtsep = null;
                                reSync = true;
                            } else {                            
                                masterDate = d;
                                setObsDate(d);
                                resetAdjObsDate();
                                // reset the seriesSequence also
                                seriesSequence = 0;
                            }
                        } else {
                            // something got very hosed
                            pedtsep = null;
                            reSync = true;
                        }
                        break;
                    }
                    case DATE_REL: {
                        if ((d = masterDate.applyData(token)) != null) {
                            if(d.isDSTExclusion()) {
                                statusReporting(record, ERR_LOGGER, SHEFErrors.HANDLERS.ERROR, SHEFErrorCodes.LOG_044);
                                pedtsep = null;
                                reSync = true;
                            } else {
                                setObsDate(d);
                                resetAdjObsDate();
                                // reset the seriesSequence also
                                seriesSequence = 0;
                            }
                        } else {
                            // something got very hosed
                            pedtsep = null;
                            reSync = true;
                        }
                        break;
                    }
                    case DATE_CREATE: {
                        setCreateTime(token.getDateData());
                        record.setCreationDate(createTime);

                        resetAdjObsDate();
                        seriesSequence = 0;
                        
                        break;
                    }
                    case QNUMERIC: {
                        if (!reSync) {
                            // Have to have seen a DIxxx prior to this
                            if (haveInt) {
                                // override the current qualifier.
                                String ss = token.getQualifier();
                                if(!isValidQualityCode(ss)) {
                                    statusReporting(record, ERR_LOGGER, SHEFErrors.HANDLERS.ERROR, SHEFErrorCodes.LOG_021);
                                    value = null;
                                    // But adjust the date
                                    incrementAdjObsDate(interval);
                                } else {
                                    qualifier = ss;
                                    ss = token.getToken();
                                    value = ss.substring(0, ss.length() - 1);
                                    trace = token.isTrace();
                                }
                                break;
                            } else {
                                reSync = true;
                            }
                        }
                        inData = true;
                        break;
                    }
                    case NUMERIC: {
                        if (!reSync) {
                            // Have to have seen a DIxxx prior to this
                            if (haveInt) {
                                value = token.getToken();
                                trace = token.isTrace();
                            } else {
                                reSync = true;
                            }
                        }
                        inData = true;
                        break;
                    }
                    case RETAINEDCOMMENT: {
                        if(lastData != null) {
                            lastData.setRetainedComment(token.getToken());
                            retainedComment = null;
                        } else {
                            retainedComment = token.getToken();
                        }
                        break;
                    }
                    case SLASH: {
                        retainedComment = null;
                        break;
                    }
                    case EMPTY: {
                        // Just ensure that the values are empty. This will
                        // ensure that the adjusted date is kept in sync.
                        value = ShefConstants.SHEF_SKIPPED;
                        retainedComment = null;
                        break;
                    }
                    case UNKNOWN: {
                        if (isMissingValue(token.getToken())) {
                            value = ShefConstants.SHEF_MISSING;
                            qualifier = getMissingQualifier(token.getToken());

                        } else if (isTraceValue(token.getToken())) {
                            value = ShefConstants.SHEF_TRACE;
                            trace = true;
                        } else {
                            statusReporting(record, ERR_LOGGER, SHEFErrors.HANDLERS.ERROR, SHEFErrorCodes.LOG_064);
                            value = null;
                            // Several things to check for
                            if(!haveInt || (pedtsep == null)) {
                                reSync = true;
                            }
                            break;
                        }
                        break;
                    }

                    default: {
                        // This is probably an error in the "E" Record.
                        // Shouldn't be anything
                        // else but the above token types on the tail of the
                        // report.
                        log.info(traceId + "- Invalid token ["
                                + token.getToken() + "] in \"E\" record");
                    }
                    } // switch
                    if ((pedtsep != null) && (value != null)) {
                        ShefData data = new ShefData();
                        data.setParameterCodeString(pedtsep,
                                currentDuration);
                        data.setLocationId(getLocationId());
                        data.setObservationTime(record.getRecordDate());
                        data.setObsTime(getAdjObsDate());
                        if (getCreateTime() != null) {
                            data.setCreateTime(new SHEFDate(getCreateTime()));
                        }
                        data.setStringValue(value);
                        data.setUnitsCode(getCurrentUnits());
                        data.setQualifier(qualifier);
                        if(retainedComment != null) {
                            data.setRetainedComment(retainedComment);
                            retainedComment = null;
                        } else {
                            lastData = data;
                        }
                        data.setTimeSeriesId((seriesSequence == 0) ? 1 : 2);
                        data.setRevisedRecord(record.isRevisedRecord());
                        data.fixupDuration(durationValue);

                        if (trace) {
                            if (legalTraceValue(data.getPhysicalElement())) {
                                record.addDataValue(data);
                            } else {
                                statusReporting(record, ERR_LOGGER, SHEFErrors.HANDLERS.ERROR, SHEFErrorCodes.LOG_031);
                            }
                        } else {
                            record.addDataValue(data);
                        }
                        value = null;
                        qualifier = getCurrentQualifier();

                        incrementAdjObsDate(interval);
                        if(getAdjObsDate().isDSTExclusion()) {
                            statusReporting(record, ERR_LOGGER, SHEFErrors.HANDLERS.ERROR, SHEFErrorCodes.LOG_044);
                            reSync = true;
                        }
                        seriesSequence++;
                        trace = false;
                    }                
                    
                      // For E records if we have a bad PEDTSEP or attempted
                      // re-declaration of
                      // PEDTSEP or the data time interval then we have to quit.
                    if (reSync) {
                        break;
                    }
                } // for
                  // Check to see if we have "trailing" data to pickup
            } else {
                statusReporting(record, ERR_LOGGER, SHEFErrors.HANDLERS.ERROR, SHEFErrorCodes.LOG_045);
                record = null;
            }
        } else {
            int error = 0;
            ParserToken t = null;
            for (int i = 0; (i < parts.size()) && (error == 0); i++) {
                t = parts.get(i);
                if (TokenType.ERROR.equals(t.getType())) {

                    switch (t.getError()) {
                    case ParserToken.ERR_LOCID_INVCHAR: {
                        error = SHEFErrorCodes.LOG_013;
                        break;
                    }
                    case ParserToken.ERR_LOCID_NULL: {
                        error = -9999;
                        break;
                    }
                    case ParserToken.ERR_LOCID_SHORT: {
                        error = SHEFErrorCodes.LOG_047;
                        break;
                    }
                    case ParserToken.ERR_LOCID_LONG: {
                        error = SHEFErrorCodes.LOG_014;
                        break;
                    }
                    }
                }
            }
            if (t != null) {
                if (error > -9999) {
                    statusReporting(record, ERR_LOGGER, SHEFErrors.HANDLERS.ERROR, error);
                } else {

                }
                record = null;
            }
        }
        return record;
    }

    /**
     * Any PE in {HY QY PY} will fail this record.
     */
    private boolean validateERecord(ShefRecord record) {
        boolean isValid = true;
        int error = 0;
        for(ParserToken token : parts) {
            
            if(token != null) {
                TokenType type = token.getType();
                switch(type) {
                case PEDTSEP : {
                    String pe = token.getSendCode();
                    if(pe != null) {
                        if(pe.startsWith("HY") || pe.startsWith("QY") || pe.startsWith("PY")) {
                            error = SHEFErrorCodes.LOG_035;
                            isValid = false;
                        }
                    }
                    break;
                }
                case UNITS_CODE : {
                    isValid = isValidUnits(token.getRawToken().substring(2));
                    error = SHEFErrorCodes.LOG_022;
                    break;
                }
                case QUAL_CODE : {
                    isValid = isValidQualityCode(token.getRawToken().substring(2));
                    error = SHEFErrorCodes.LOG_021;
                    break;
                }
                case UNKNOWN : {
                    
                    
                    
                }
                }
            }
        }
        if(!isValid) {
            statusReporting(record, ERR_LOGGER, SHEFErrors.HANDLERS.ERROR, error);
        }
        return isValid;
    }

    
    private void statusReporting(ShefRecord record, SHEFErrors logger, SHEFErrors.HANDLERS handler, int error) {
        switch(handler) {
        case DEBUG : {
            ERR_LOGGER.debug(getClass(), createRecordHeader(record, reportLead)
                    + createDataLine(parts));
            ERR_LOGGER.debug(getClass(), "?");
            ERR_LOGGER.debug(getClass(), error);

            break;
        }
        case WARNING : {
            ERR_LOGGER.warning(getClass(), createRecordHeader(record, reportLead)
                    + createDataLine(parts));
            ERR_LOGGER.warning(getClass(), "?");
            ERR_LOGGER.warning(getClass(), error);
            
            break;
        }
        case ERROR : {
            
            ERR_LOGGER.error(getClass(), createRecordHeader(record, reportLead)
                    + createDataLine(parts));
            ERR_LOGGER.error(getClass(), "?");
            ERR_LOGGER.error(getClass(), error);
            break;
        }
        }
    }
    
    /**
     * 
     * Note - This method must only be used for "E" records.
     * 
     */
    private void correctMissingDelimiters() {
        TokenType NUMERIC = TokenType.NUMERIC;
        TokenType QNUMERIC = TokenType.QNUMERIC;
        TokenType COMMA = TokenType.COMMA;
        TokenType SPACE = TokenType.SPACE;
        
        if ((parts != null) && (parts.size() > 0)) {
            ParserToken last = null;
            // First pass through we are going to look for possible commas in the data.
            for(int i = 0;i < parts.size();) {
                ParserToken t = getToken(parts,i);
             // do we have a comma?
                if(COMMA.equals(t.getType())) {
                    // ok remove it
                    parts.remove(i);
                    if(SPACE.equals(last.getType())) {
                        // preceeded by a space, then we check the
                        // next token.
                        t = getToken(parts,i);
                        if(t.isValueToken()) {
                            parts.remove(i);
                        }
                    } else {
                        if((last != null) && (last.getType() != null)) {
                            if(last.isValueToken()) {
                                parts.set(i-1,new ParserToken("",TokenType.EMPTY));
                                t = getToken(parts,i);
                                if(t.isValueToken()) {
                                    parts.remove(i);
                                }
                            }
                        }
                    }
                } else {
                    i++;
                }
                last = t;
            }
            last = null;
            for (int i = 0; i < parts.size();) {
                ParserToken t = parts.get(i);
                switch (t.getType()) {
                case NUMERIC:
                case QNUMERIC: {
                    if (NUMERIC.equals(last.getType())
                            || QNUMERIC.equals(last.getType())) {
                        ParserToken tt = new ParserToken("/", TokenType.SLASH);
                        parts.add(i, tt);
                        last = tt;
                    } else {
                        i++;
                        last = t;
                    }
                    break;
                }
                default: {
                    i++;
                    last = t;
                }
                }
            }
        }
    }

    /**
     * Find and remove all Interval Codes within the token list except for the
     * last Interval Code found which is returned. Note - This method must be
     * used for "E" records only.
     * 
     * @param tokens
     *            A list of tokens to process.
     * @return The Interval Code token found, or null if none were found.
     */
    private static ParserToken findInterval(List<ParserToken> tokens) {
        ParserToken token = null;
        int prevInt = -1;
        for (int i = 0; i < tokens.size();) {
            ParserToken t = tokens.get(i);
            if (TokenType.INT_CODE.equals(t.getType())) {
                token = t;
                if ((prevInt >= 0) && (prevInt < i)) {
                    tokens.remove(prevInt);
                    i--;
                    // Now get the next token.
                    t = tokens.get(prevInt);
                    // And check if it is a SLASH, if so remove it also.
                    if (TokenType.SLASH.equals(t.getType())) {
                        tokens.remove(prevInt);
                        i--;
                    }
                    prevInt = i;
                } else {
                    prevInt = i;
                    i++;
                }
            } else {
                i++;
            }
        }
        return token;
    }

    // *********************************
    // * General Parser methods.
    // *********************************

    /**
     * Tokenize the current message.
     * 
     * @param message
     * @return
     */
    private static List<ParserToken> tokenize(String message) {
        List<ParserToken> tokens = new ArrayList<ParserToken>();
        tokens.add(new ParserToken("", TokenType.START));
        if (message != null) {
            StringTokenizer st = new StringTokenizer(message, "/"
                    + CARRIAGECONTROL + ",", true);
            TokenType last = null;
            while (st.hasMoreTokens()) {
                String currToken = st.nextToken();
                // Constructor will attempt to determine the token type
                ParserToken t = new ParserToken(currToken.trim());
                if(TokenType.COMMA.equals(last) && currToken.startsWith(" ")) {
                    tokens.add(new ParserToken(" ", TokenType.SPACE));
                }
                if (TokenType.UNKNOWN.equals(t.getType()) || 
                		TokenType.SPACEINMIDDLE.equals(t.getType())) {
                    // check possible failures
                        List<ParserToken> subList = subTokenize(currToken);
                        if (subList != null) {
                            tokens.addAll(subList);
                        }
                } else {
                    tokens.add(t);
                }
                if(tokens.size() > 0) {
                    last = tokens.get(tokens.size() -1).getType();
                }
            }
            tokens = identifyEmpty(collapseSpaces(tokens));
        }

        return tokens;
    }

    /**
     * 
     * @param token
     * @return
     */
    private static List<ParserToken> subTokenize(String token) {
        TokenType UNKNOWN = TokenType.UNKNOWN;
        List<ParserToken> tokens = new ArrayList<ParserToken>();

        // Tokenize by spaces
        StringTokenizer st = new StringTokenizer(token, " ", true);
        String lastToken = null;
        while (st.hasMoreTokens()) {
            String currToken = st.nextToken();
            if ((" ".equals(currToken) && " ".equals(lastToken))) {
                continue;
            }
            ParserToken tt = new ParserToken(currToken);
            if(TokenType.UNKNOWN.equals(tt.getType())) {
                tt = tt.check_D_Directives();
            }
            tokens.add(tt);
            lastToken = currToken;
        }
        
        // Make a pass through the tokens to see if there are any
        // ill-formed retained comments
        for (int i = 0; i < tokens.size(); i++) {
            ParserToken t = tokens.get(i);
            if (t != null) {
                String s = t.getToken();
                int pos = s.indexOf("\"");
                if (pos < 0) {
                    pos = s.indexOf("\'");
                }
                // Do we need to split this token?
                if (pos > 0) {
                    // Let the system try to reidentify the token
                    ParserToken tt = new ParserToken(s.substring(0, pos));
                    tokens.set(i, tt);
                    tt = new ParserToken(s.substring(pos),
                            TokenType.RETAINEDCOMMENT);
                    i++;
                    tokens.add(i, tt);
                }
            }
        }

        // We may have split retained comments so attempt to put them
        // back together
        for (int i = 0; i < tokens.size(); i++) {
            ParserToken t = tokens.get(i);
            String quoteType = null;
            if (UNKNOWN.equals(t.getType())) {
                int startIndex = -1;
                int endIndex = -1;
                String s = t.getToken();
                if ((s != null) && (s.length() > 0)) {
                    if (s.startsWith("\"")) {
                        if (s.endsWith("\"")) {
                            continue;
                        } else {
                            quoteType = "\"";
                        }
                    } else if (s.startsWith("\'")) {
                        if (s.endsWith("\'")) {
                            continue;
                        } else {
                            quoteType = "\'";
                        }
                    }
                    // we found the beginning of a comment, but the end wasn't
                    // in the same token so we need to find it.
                    if (quoteType != null) {
                        startIndex = i;
                        // move to the next token
                        i++;
                        for (; i < tokens.size(); i++) {
                            s = tokens.get(i).getToken();
                            if (s.endsWith(quoteType)) {
                                endIndex = i + 1;
                                break;
                            }
                        }
                        // Check if we found an ending quote. End of line ends
                        // a comment so its legal to not find one.
                        if (endIndex < 0) {
                            endIndex = i + 1;
                        }
                        StringBuilder sb = new StringBuilder();
                        t = tokens.get(startIndex);
                        sb.append(t.getToken().substring(1));
                        for (int ii = startIndex + 1; ii < endIndex; endIndex--) {
                            ParserToken tt = tokens.remove(ii);
                            s = tt.getToken();
                            if (s.endsWith("\"")) {
                                sb.append(s.substring(0, s.length() - 1));
                            } else {
                                sb.append(s);
                            }
                        }
                        ParserToken newToken = new ParserToken(sb.toString()
                                .trim(), TokenType.RETAINEDCOMMENT);
                        tokens.set(startIndex, newToken);
                    }
                }
            }
        }

        return tokens;
    }

    private static List<ParserToken> identifyNeededSlashes(
            List<ParserToken> tokens) {
        TokenType SLASH = TokenType.SLASH;
        TokenType UNKNOWN = TokenType.UNKNOWN;
        TokenType NUMERIC = TokenType.NUMERIC;
        TokenType RETAINEDCOMMENT = TokenType.RETAINEDCOMMENT;
        
        TokenType last = UNKNOWN;
        for (int i = 0; i < tokens.size(); i++) {

            ParserToken currToken = tokens.get(i);
            switch (currToken.getType()) {
            case NUMERIC:
            case QNUMERIC: {
                if (last.equals(NUMERIC)) {
                    ParserToken tt = new ParserToken("/", SLASH);
                    tokens.add(i, tt);
                    i++;
                } else {
                    last = NUMERIC;
                }
                break;
            }
            case UNKNOWN: {
                if ((last.equals(NUMERIC)) || (last.equals(UNKNOWN))) {
                    ParserToken tt = new ParserToken("/", SLASH);
                    tokens.add(i, tt);
                    i++;
                }
                last = UNKNOWN;
                break;
            }
            case RETAINEDCOMMENT: {
                if(!NUMERIC.equals(last)) {
                    last = RETAINEDCOMMENT;
                }
                break;
            }
            default: {
                last = currToken.getType();
                break;
            }
            }
        }
        return tokens;
    }

    /**
     * 
     * @param tokens
     * @return
     */
    private static List<ParserToken> identifyEmpty(List<ParserToken> tokens) {

        TokenType SLASH = TokenType.SLASH;
        TokenType SPACE = TokenType.SPACE;
        TokenType COMMA = TokenType.COMMA;
        
        TokenType NIL = TokenType.NIL;

        List<ParserToken> newTokens = new ArrayList<ParserToken>();

        TokenType last = TokenType.NIL;

        for (ParserToken t : tokens) {
            if (SLASH.equals(t.getType())) {
                if (NIL.equals(last)) {
                    newTokens.add(new ParserToken("", TokenType.EMPTY));
                    newTokens.add(new ParserToken("/", SLASH));
                } else if (SLASH.equals(last)) {
                    newTokens.add(new ParserToken("", TokenType.EMPTY));
                    newTokens.add(new ParserToken("/", SLASH));
                } else if (SPACE.equals(last)) {
                    newTokens.add(new ParserToken("", TokenType.EMPTY));
                    newTokens.add(new ParserToken("/", SLASH));
                } else {
                    newTokens.add(t);
                }
            } else {
                if (SLASH.equals(last)) {
                    if (TokenType.NEWLINE.equals(t.getType())) {
                        newTokens.add(new ParserToken("", TokenType.EMPTY));
                        newTokens.add(t);
                    } else if (!SPACE.equals(t.getType())) {
                        newTokens.add(t);
                    }
                } else {
                    if(SPACE.equals(t.getType())) {
                        if(COMMA.equals(last)) {
                            newTokens.add(t);
                        }
                    } else {
                        newTokens.add(t);
                    }
                }
            }

            if (SLASH.equals(last)) {
                if (!SPACE.equals(t.getType())) {
                    last = t.getType();
                }
            } else {
                if(SPACE.equals(t.getType())) {
                    if(COMMA.equals(last)) {
                        last = t.getType();
                    }
                } else {
                    last = t.getType();
                }
            }
        } // for
        if (SLASH.equals(last)) {
            newTokens.add(new ParserToken("", TokenType.EMPTY));
        }
        return newTokens;
    }

    /**
     * Attempts to identify unknown tokens within a record token list.
     * 
     * @param tokens
     *            A list (or sublist) of tokenized data to analyzed.
     * @param isBData
     *            Is the list the data sublist from a B record?
     */
    private void identifyUnknownToken(List<ParserToken> tokens, boolean isBData) {
        if (tokens != null) {
            for (int i = 0; i < tokens.size();) {
                ParserToken t = tokens.get(i);
                if (TokenType.UNKNOWN.equals(t.getType())) {
                    String s = t.getToken().toUpperCase();
                    if (log.isDebugEnabled()) {
                        log.debug(traceId + "- Checking unknown token "
                                + t.getType() + " " + s);
                    }
                    if (s.length() >= 2) {
                        // Special check for MM, may be a PE or missing value.
                        if ("MM".equals(s)) {
                            if (isBData) {
                                ParserToken tt = new ParserToken(
                                        ShefConstants.SHEF_MISSING + "M",
                                        TokenType.QNUMERIC);
                                tokens.set(i, tt);
                            } else {
                                if (i > 0) {
                                    ParserToken tt = tokens.get(i - 1);
                                    if (TokenType.PEDTSEP.equals(tt.getType())) {
                                        // if the previous token is a PE token,
                                        // then
                                        // treat as missing
                                        tt = new ParserToken(
                                                ShefConstants.SHEF_MISSING,
                                                TokenType.NUMERIC);
                                        tokens.set(i, tt);
                                    } else if (TokenType.SLASH.equals(tt
                                            .getType())) {
                                        // if the previous token is a SLASH
                                        // token,
                                        // then
                                        // treat as a PE
                                        tt = new ParserToken(s,
                                                TokenType.PEDTSEP);
                                        tokens.set(i, tt);
                                    }
                                }
                            }
                        } else {
                            PhysicalElement pe = PhysicalElement.getEnum(s
                                    .substring(0, 2));
                            if (!PhysicalElement.UNKNOWN.equals(pe)) {
                                
                                int error = SHEFErrorCodes.LOG_000;
                                String sendCode = null;

                                String trans = ShefParm
                                        .getSendCodeDurationDefaults(pe
                                                .getCode());
                                if (trans != null) {
                                    if (trans.length() > 3) {
                                        // Handle the send code translation
                                        if(s.length() != 2) {
                                            error = SHEFErrorCodes.LOG_030;
                                        } else {
                                            // Only set the sendCode for true
                                            // send codes, not duration overrides.
                                            sendCode = pe.getCode();
                                            s = trans;
                                        }
                                    } else {
                                        if(s.length() == 2) {
                                            s = trans;
                                        }
                                    }
                                }
                                ParserToken tt = new ParserToken(s,
                                        TokenType.PEDTSEP);
                                tt.setError(error);
                                tt.setSendCode(sendCode);
                                tokens.set(i, tt);
                                // May be some other type of token
                            } else if (isMissingValue(t.getToken())) {
                                String q = getMissingQualifier(t
                                        .getToken());
                                if("M".equals(q)) {
                                    q = ShefConstants.SHEF_MISSING + "M";
                                } else {
                                    q = ShefConstants.SHEF_MISSING;
                                }
                                ParserToken tt = new ParserToken(q);
                                tokens.set(i, tt);
                            } else {
                                ParserToken tt = ParserToken.identifyUnknown(t);
                                tokens.set(i, tt);
                            }
                        }
                    } else if (isMissingValue(t.getToken())) {
                        String q = getMissingQualifier(t
                                .getToken());
                        if("M".equals(q)) {
                            q = ShefConstants.SHEF_MISSING + "M";
                        } else {
                            q = ShefConstants.SHEF_MISSING;
                        }
                        ParserToken tt = new ParserToken(q);
                        tokens.set(i, tt);
                    } else if (isTraceValue(t.getToken())) {
                        ParserToken tt = new ParserToken(
                                ShefConstants.SHEF_TRACE, TokenType.NUMERIC);
                        tt.setTrace(true);
                        tokens.set(i, tt);
                    } else {
                        // With the 
                        // We have a problem!
                        log.error(traceId + "- Could not identify token " + t);
                    }
                    i++;
                } else if (TokenType.SPACE.equals(t.getType())) {
                    if (isBData) {
                        if ((i < tokens.size() - 2)
                                && (TokenType.SLASH.equals(tokens.get(i + 1)
                                        .getType()))) {
                            tokens.set(i, new ParserToken("", TokenType.EMPTY));
                        } else {
                            tokens.remove(i);
                        }
                    } else {
                        tokens.remove(i);
                    }
                } else if (TokenType.DECIMAL.equals(t.getType())) {
                    ParserToken tt = new ParserToken(
                            ShefConstants.SHEF_MISSING, TokenType.NUMERIC);
                    tokens.set(i, tt);
                } else {
                    i++;
                }
                // re-get the token
//                t = tokens.get(ii);
//                if (TokenType.UNKNOWN.equals(t.getType())) {
//                    String s = t.getToken().toUpperCase();
//                    ParserToken tt = t.analyzeUnknown(s);
//                    tokens.set(ii, tt);
//                }
            }
        }
    }

    /**
     * Read the Location identifier, observation time and the optional timezone
     * information.
     * 
     * .X[R] lid date [tz] dcode/....
     * 
     * 
     * 
     */
    private boolean getPositionalData() {
        boolean foundPositionalData = false;
        if (movePastSpaces()) {
            ParserToken t = parts.get(partsIndex);

            if (ShefUtil.between(ShefConstants.LOWER_LID_LIMIT, t.getToken()
                    .length(), ShefConstants.UPPER_LID_LIMIT)) {
                setLocationId(t.getToken());

                // t = new ParserToken(getLocationId(), TokenType.LOC_ID);
                t = ParserToken.createLocIdToken(getLocationId());
                
                parts.set(partsIndex, t);
                if (t.getError() < 0) {
                    return foundPositionalData;
                }
                partsIndex++;
                movePastSpaces();
                if (partsIndex < parts.size()) {
                    t = parts.get(partsIndex++);
                    // This should be the observation time
                    int obsTimeIndex = -1;
                    if (TokenType.NUMERIC.equals(t.getType())) {
                        movePastSpaces();
                        obsTimeIndex = partsIndex - 1;
                        setObsTime(t.getToken());
                        TokenType obsType = null;
                        if (obsTime.length() == 4) {
                            obsType = TokenType.OBS_DATE_4;
                        } else if (obsTime.length() == 6) {
                            obsType = TokenType.OBS_DATE_6;
                        } else if (obsTime.length() == 8) {
                            obsType = TokenType.OBS_DATE_8;
                        }
                        // The next token may be the timezone, but it is
                        // optional. The timezone will be only 1 or 2
                        // characters.
                        movePastSpaces();
                        if (partsIndex < parts.size()) {
                            t = parts.get(partsIndex);
                            if (!TokenType.SLASH.equals(t.getType())) {
                                String tzc = t.getToken();
                                int len = tzc.length();
                                if ((len == 1) || (len == 2)) {
                                    setTimeZone(tzc);
                                }
                                checkForDefaultTimeZone();
                                // Now check to see if what attempted to set as
                                // the
                                // timezone was indeed the timezone. If so, set
                                // the
                                // token
                                // type to TIMEZONE
                                if (tzc.equals(getTimeZone())) {
                                    parts.set(partsIndex, new ParserToken(tzc,
                                            TokenType.TIMEZONE));
                                } else {
                                    parts.add(partsIndex, new ParserToken(
                                            timeZone, TokenType.TIMEZONE));
                                }
                                partsIndex++;
                            } else {
                                setTimeZone(ShefConstants.Z);
                                parts.add(partsIndex, new ParserToken(
                                        getTimeZone(), TokenType.TIMEZONE));
                                partsIndex++;
                            }
                            tz = SHEFTimezone.sysTimeZones.get(timeZone);
                            if (log.isDebugEnabled()) {
                                log.info("Timezone set to " + tz);
                            }
                            if (tz == null) {
                                // indicate error - really bad!
                                foundPositionalData = false;
                            } else {
                                if ((obsType != null) && (obsTimeIndex > 0)) {
                                    t = new ParserToken(obsTime, obsType);

                                    parts.set(obsTimeIndex, t);
                                    // We have a timezone so we can iterate over
                                    // all of
                                    // the tokens and update the date fields
                                    fixupDates(parts, tz);
                                    obsTime = parts.get(obsTimeIndex)
                                            .getDateData().toLocal();
                                    obsDate = parts.get(obsTimeIndex)
                                            .getDateData();
                                    foundPositionalData = true;
                                }
                            }
                        } else {
                            // out of data after positional data
                            foundPositionalData = false;
                        }
                    }
                } else {
                    // out of data looking for observation time.
                    ERR_LOGGER.error(getClass(), createDataLine(parts));
                    ERR_LOGGER.error(getClass(), " ?");
                    ERR_LOGGER.error(getClass(), SHEFErrorCodes.LOG_100);
                    foundPositionalData = false;
                }
            } else {
                ERR_LOGGER.error(getClass(), createDataLine(parts));
                ERR_LOGGER.error(getClass(), " ?");
                ERR_LOGGER.error(getClass(), SHEFErrorCodes.LOG_047);
            }
        }
        if (foundPositionalData) {
            for (ParserToken t : parts) {
                switch (t.getType()) {
                case OBS_DATE_4:
                case OBS_DATE_6:
                case OBS_DATE_8: {
                    obsTime = t.getDateData().toLocal();
                    break;
                }
                case TIMEZONE: {
                    break;
                }
                }
                if (!checkDates(t)) {
                    foundPositionalData = false;
                    break;
                }
            }
            movePastSpaces();
        }
        return foundPositionalData;
    } //


    /**
     * Move past any SPACE tokens in the data list.
     * 
     * @return Is more data available in the data list.
     */
    private boolean movePastSpaces() {
        boolean moreData = true;
        ParserToken t = null;
        while ((parts.size() > 0) && (partsIndex < parts.size())) {
            t = parts.get(partsIndex);
            if (TokenType.SPACE.equals(t.getType())) {
                parts.remove(partsIndex);
            } else {
                break;
            }
        }
        moreData = parts.size() > 0;
        return moreData;
    }

    /**
     * Collapse runs of spaces into a single space.
     * 
     * @param tokenList
     * @return
     */
    private static List<ParserToken> collapseSpaces(List<ParserToken> tokenList) {
        TokenType SPACE = TokenType.SPACE;
        List<ParserToken> newList = new ArrayList<ParserToken>();
        TokenType lastToken = null;
        for (ParserToken t : tokenList) {
            if (SPACE.equals(t.getType())) {
                if (!SPACE.equals(lastToken)) {
                    newList.add(t);
                }
            } else {
                newList.add(t);
            }
            lastToken = t.getType();
        }
        return newList;
    }

    /**
     * Adjust all date values in a token list to correspond to the given
     * timezone information.
     * 
     * @param tokens
     *            A list of ParserTokens to adjust.
     * @param tz
     *            The timezone to adjust time to.
     */
    private static void fixupDates(List<ParserToken> tokens, TimeZone tz) {
        for (ParserToken t : tokens) {
            switch(t.getType()) {
            case DATE_CREATE:
            case OBS_DATE_4:
            case OBS_DATE_6:
            case OBS_DATE_8: {
                if(t.getError() == ParserToken.ERR_NO_ERROR) {
                    t.adjustToTimezone(tz);
                    t.getDateData().validate();
                }
            }
            default: {
                // nothing
            }
            } // end switch
        }
    }

    /**
     * 
     * @param token
     * @return
     */
    private boolean checkDates(ParserToken token) {
        boolean valid = true;

        int err = token.getError();
        if (err < ParserToken.ERR_NO_ERROR) {
            ERR_LOGGER.error(getClass(), createDataLine(parts));
            ERR_LOGGER.error(getClass(), " ?");
            switch (err) {
            case ParserToken.ERR_INV_CREATE_DATE: {
                ERR_LOGGER.error(getClass(), SHEFErrorCodes.LOG_019);
                valid = false;
                break;
            }
            case ParserToken.ERR_INV_JUL_DATE: {
                ERR_LOGGER.error(getClass(), SHEFErrorCodes.LOG_079);
                valid = false;
                break;
            }
            case ParserToken.ERR_INVALID_QUAL: {
                ERR_LOGGER.error(getClass(), SHEFErrorCodes.LOG_021);
                valid = false;
                break;
            }
            case ParserToken.ERR_INV_SECONDS: {
                ERR_LOGGER.error(getClass(), SHEFErrorCodes.LOG_016);
                valid = false;
                break;
            }
            case ParserToken.ERR_INV_MINUTES: {
                ERR_LOGGER.error(getClass(), SHEFErrorCodes.LOG_016);
                valid = false;
                break;
            }
            case ParserToken.ERR_INV_HOURS: {
                ERR_LOGGER.error(getClass(), SHEFErrorCodes.LOG_016);
                valid = false;
                break;
            }
            case ParserToken.ERR_INV_DAY: {
                ERR_LOGGER.error(getClass(), SHEFErrorCodes.LOG_017);
                valid = false;
                break;
            }
            case ParserToken.ERR_INV_MONTH: {
                ERR_LOGGER.error(getClass(), SHEFErrorCodes.LOG_017);
                valid = false;
                break;
            }
            }
        }
        return valid;
    }

    /**
     * Process the Duration token "DVxvv".
     * 
     * @param token
     * @param override
     *            Used to signal B record overrides of a pattern level duration.
     */
    private void processDuration(ParserToken token, boolean override) {
        if (TokenType.DUR_CODE.equals(token.getType())) {
            String dUnit = token.getToken().substring(2, 3);
            if ("Z".equals(dUnit)) {
                // Turn off variable period.
                String cDuration = Duration.DEFAULT.getCode();
                if (override) {
                    currentDurationOverride = cDuration;
                    durationValueOverride = (short) Duration.INSTANTENOUS
                            .getValue();
                } else {
                    currentDuration = cDuration;
                    durationValue = (short) Duration.INSTANTENOUS.getValue();
                }
            } else {
                Short value = getDuration(token, dUnit);
                if (value != null) {
                    if (override) {
                        currentDurationOverride = Duration.VARIABLE_PERIOD
                                .getCode();
                        durationValueOverride = value;
                    } else {
                        currentDuration = Duration.VARIABLE_PERIOD.getCode();
                        durationValue = value;
                    }

                }
            }
        }
    }

    /**
     * 
     * @param value
     * @param units
     * @return
     */
    private static Short getDuration(ParserToken token, String units) {
        Short duration = null;
        try {
            Short value = Short.parseShort(token.getToken().substring(3));

            if ((units != null) && (units != null)) {
                if ((value >= 0) && (value < 100)) {
                    int idx = ShefConstants.DURATION_CODES.indexOf(units);
                    if (idx >= 0) {
                        short base = ShefConstants.DURATION_VALS[idx];
                        duration = (short) (base + value);
                    }
                }
            }
        } catch (NumberFormatException nfe) {
            // nothing.
        }
        return duration;
    }

    /**
     * Check to see if a candidate value represents a missing value.
     * 
     * @param value
     *            The value to check.
     * @return Is this value a possible missing value?
     */
    private static boolean isMissingValue(String value) {
        boolean isMissing = false;
        if ("+".equals(value)) {
            isMissing = true;
        } else if ("++".equals(value)) {
            isMissing = true;
        } else if ("-".equals(value)) {
            isMissing = true;
        } else if ("--".equals(value)) {
            isMissing = true;
        } else if ("m".equals(value)) {
            isMissing = true;
        } else if ("mm".equals(value)) {
            isMissing = true;
        } else if ("M".equals(value)) {
            isMissing = true;
        } else if ("MM".equals(value)) {
            isMissing = true;
        }
        return isMissing;
    }

    private static String getMissingQualifier(String value) {
        String qualifier = null;
        if ("mm".equals(value)) {
            qualifier = "M";
        } else if ("MM".equals(value)) {
            qualifier = "M";
        }
        return qualifier;
    }

    /**
     * Checks to see if the candidate value represents a trace value.
     * 
     * @param value
     *            The value to check.
     * @return Is this value a possible trace value?
     */
    private static boolean isTraceValue(String value) {
        boolean isTrace = false;
        if ("T".equals(value)) {
            isTrace = true;
        } else if ("t".equals(value)) {
            isTrace = true;
        }
        return isTrace;
    }

    /**
     * 
     * @param pe
     * @return
     */
    private static boolean legalTraceValue(PhysicalElement pe) {
        return VALID_TRACE_PE.contains(pe);
    }
    
    /**
     * 
     * @param qualCode
     * @return
     */
    private boolean isValidQualityCode(String qualCode) {
        // Set to false by exception
        boolean isValid = true;
        if(qualCode != null) {
            isValid = (ShefParm.getDataQualifierCodes(qualCode) != null);
        } else {
            isValid = false;
        }
        return isValid;
    }

    /**
     * Determine if the units code is valid.
     * @param unitsCode
     * @return
     */
    private static boolean isValidUnits(String unitsCode) {
        // Set to false by exception
        boolean isValid = true;
        if(unitsCode != null) {
            isValid = ShefConstants.VALID_UNITS.indexOf(unitsCode) > -1;
        } else {
            isValid = false;
        }
        return isValid;
    }

    /**
     * 
     * @param list
     * @param i
     * @return
     */
    private static ParserToken getToken(List<ParserToken> list, int i) {
        ParserToken t = null;
        if((list != null) && (i < list.size())) {
            t = list.get(i);
        }
        if(t == null) {
            t = new ParserToken("^^^", TokenType.UNKNOWN);
        }
        
        return t;
    }
    
    
    
    
    /**
     * 
     * @param msg
     *            The message data being parsed.
     * @param index
     *            The current position of the message pointer.
     * @param comment
     *            A not-null buffer to receive the comment.
     * @return The modified index position.
     */
    private static int getRetainedComment(String msg, int index,
            StringBuilder comment) {
        char retainedChar = msg.charAt(index++);
        for (; index < msg.length();) {
            if (retainedChar == msg.charAt(index)) {
                // move past the terminating quote
                index++;
                break;
            }
            if ('\n' == msg.charAt(index) || '\r' == msg.charAt(index)) {
                break;
            }
            comment.append(msg.charAt(index++));
        }
        return index;
    }

    /**
     * 
     * @param rec
     * @return
     */
    private static String createRecordHeader(ShefRecord rec, String reportLead) {
        StringBuilder recData = new StringBuilder(".");
        if (rec != null) {
            recData.append(rec.getShefType().name());
            recData.append(rec.isRevisedRecord() ? "R " : " ");
            if(reportLead != null) {
                recData.append(reportLead);
                recData.append(" ");
            }
        }
        return recData.toString();
    }

    /**
     * 
     * @param p
     * @return
     */
    private static String createDataLine(List<ParserToken> p) {
        StringBuilder sb = new StringBuilder();
        Iterator<ParserToken> it = p.iterator();
        while (it.hasNext()) {
            ParserToken t = it.next();
            if(t.getSendCode() != null) {
                sb.append(t.getSendCode());
            } else {
                sb.append(t.getRawToken());
            }
            if (it.hasNext()) {
                sb.append(" ");
            }
        }
        return sb.toString();
    }

    private static String clearTabs(String message) {
        if (message != null) {
            StringBuilder sb = new StringBuilder(message);
            for (int i = 0; i < sb.length(); i++) {
                if ('\t' == sb.charAt(i)) {
                    sb.setCharAt(i, ' ');
                }
            }
            message = sb.toString();
        }
        return message;
    }

    /**
     *  
     */
    public void dumpTokens() {
        for (ParserToken t : parts) {
            System.out.println(t);
        }
    }

    /**
     * @param args
     */
    public static void main(String[] args) {

//        List<ParserToken> list =
        // tokenize(".E  EE0165    0323 Z  DH01/HGI/DIH1 /\n" +
        // ".E1  1.0  2..0  3+0 \"comment 3\"  4.0 \"comment 4\"  5.0  6.0");

        // .A AA0447I 991216 Z DH09/ TX 10 \"Comm\" /
        // .A AA0447J 991216 Z DH09/ TX 30\"Comm\" /
        // .A AA0447K 991216 Z DH09/ TX 40M \"comment\" /
        // .A AA0447L 991216 Z DH09/ TX 20M\"comment\" /
        // .A AA0447M 991216 Z DH09/ TX 20A \"comment\" /
        // .A AA0447N 991216 Z DH09/ TX 20A\"comment\" /
        // .A AA0447P 991216 Z DH09/ TX 20R\'comment\' /

//        tokenize(".A  AA0447L  991216  Z  DH09/ TX  20M\"comment\"");

        // List<ParserToken> list =
        // tokenize(".E1  1.0  2..0  3+0 \"comment 3\"  4.0 \"comment 4\"  5.0  6.0 \"comment 5\"\n");

//        System.out
//                .println("------------------------------------------------------------");
//        for (ParserToken t : list) {
//            System.out.println(t);
//        }

         List<ParserToken> list =
         tokenize(".E1  1.0  2..0  3+0 \"comment 3\"  4.0 \"comment 4\"  5.0  6.0 \"comment 5 \"");
        
         System.out
         .println("------------------------------------------------------------");
         for (ParserToken t : list) {
         System.out.println(t);
         }

        ParserToken t = new ParserToken("HY");
        System.out.println(t + " " + t.getError());
        
        
        
    }

}
