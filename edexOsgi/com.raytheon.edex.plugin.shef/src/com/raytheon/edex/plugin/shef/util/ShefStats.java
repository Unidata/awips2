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
package com.raytheon.edex.plugin.shef.util;

import java.util.LinkedList;
import java.util.List;

import com.raytheon.uf.common.dataplugin.shef.util.ParameterCode.PhysicalElement;


/**
 * Class to hold all the SHEF decoding stats
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 25Apr2008	387        M. Duff     Initial Version.
 * 10/16/2008   1548       jelkins     Integrated ParameterCode Types
 * 
 * </pre>
 * 
 * @author mduff
 * @version 1.0
 */
public class ShefStats {
    /** Count of warning messages */
    private int warningMessages = 0;

    /** Count of error messages */
    private int errorMessages = 0;

    /** Count of successfully stored messages */
    private int successMessages = 0;

    /** Count of unknown stations posted */
    private int unknownNoPost = 0;

    /** Count of stations not posted */
    private int noPost = 0;

    /** Count of stations outside the configured time window */
    private int outsideTimeWindow = 0;

    /** Count of paired values posted */
    private int paired = 0;

    /** Count of paired values overwritten */
    private int pairedOver = 0;

    /** Count of values ignored */
    private int ignored = 0;

    /** Count of records sent to GPP server */
    private int precipGpp = 0;

    /** Count of Obs stored */
    private int obsPe = 0;

    /** Count of Obs Height stored */
    private int obsHeight = 0;

    /** Count of Obs Precip stored */
    private int obsPrecip = 0;

    /** Count of Forecast PEs stored */
    private int forecastPe = 0;

    /** Count of Max Forecast elements stored */
    private int maxForecast = 0;

    /** Count of Areal Obs overwritten */
    private int arealObsOverwrite = 0;

    /** Count of Areal Values added */
    private int arealValues = 0;

    /** Count of Areal Fcst overwritten */
    private int arealFcstOverwrite = 0;

    /** Count of Areal FcstValues added */
    private int arealFcstValues = 0;

    /** Count of Contingency Values overwritten */
    private int contingencyOverwrite = 0;

    /** Count of Contingency Values added */
    private int contingencyValues = 0;

    /** Count of Post Processed Overwritten */
    private int postProcessedOverwrite = 0;

    /** Count of Post Processed values added */
    private int postProcessedValues = 0;

    /** Count of Rejected Overwritten */
    private int rejectedOverwrite = 0;

    /** Count of Rejected values */
    private int rejected = 0;

    /** Count of Comments overwritten */
    private int commentOverwrite = 0;

    /** Count of Comments */
    private int comments = 0;

    /** Count of Alert/Alarm entries */
    private int alertAlarm = 0;

    /** Count of Latest Obs entries */
    private int latestObs = 0;

    /**
     * Count of unknown stations overwritten
     */
    private int unknownStationValueOverwrite = 0;

    /**
     * Count of unknown station entries
     */
    private int unknownStationValue = 0;

    /** Elapsed time for unknown stations to post, in milliseconds */
    private long elapsedTimeUnknown = 0;

    /** Elapsed time for ingesting, in milliseconds */
    private long elapsedTimeIngest = 0;

    /** Elapsed time for ingesting Height, in milliseconds */
    private long elapsedTimeHeightIngest = 0;

    /** Elapsed time for ingesting Precip, in milliseconds */
    private long elapsedTimePrecipIngest = 0;

    /** Elapsed time for ingesting other PEs, in milliseconds */
    private long elapsedTimeOtherIngest = 0;

    /** Elapsed time for ingesting Forecast PEs, in milliseconds */
    private long elapsedTimeForecastIngest = 0;

    /** Max Forecast Location ID list */
    private List<String> lidList = new LinkedList<String>();

    /** Max Forecast Physical Element list */
    private List<PhysicalElement> peList = new LinkedList<PhysicalElement>();

    /**
     * @return the warningMessages
     */
    public int getWarningMessages() {
        return warningMessages;
    }

    /**
     * @param warningMessages
     *            the warningMessages to set
     */
    public void setWarningMessages(int warningMessages) {
        this.warningMessages = warningMessages;
    }

    /**
     * increment warning messages
     */
    public void incrementWarningMessages() {
        this.warningMessages++;
    }

    /**
     * @return the errorMessages
     */
    public int getErrorMessages() {
        return errorMessages;
    }

    /**
     * @param errorMessages
     *            the errorMessages to set
     */
    public void setErrorMessages(int errorMessages) {
        this.errorMessages = errorMessages;
    }

    /**
     * increment error messages
     */
    public void incrementErrorMessages() {
        this.errorMessages++;
    }

    /**
     * @return the successMessages
     */
    public int getSuccessMessages() {
        return successMessages;
    }

    /**
     * @param successMessages
     *            the successMessages to set
     */
    public void setSuccessMessages(int successMessages) {
        this.successMessages = successMessages;
    }

    /**
     * increment success messages
     */
    public void incrementSuccessMessages() {
        this.successMessages++;
    }

    /**
     * @return the unknownNoPost
     */
    public int getUnknownNoPost() {
        return unknownNoPost;
    }

    /**
     * @param unknownNoPost
     *            the unknownNoPost to set
     */
    public void setUnknownNoPost(int unknownNoPost) {
        this.unknownNoPost = unknownNoPost;
    }

    /**
     * Increment unknown stations not posted
     */
    public void incrementUnknownNoPost() {
        this.unknownNoPost++;
    }

    /**
     * @return the elapsedTime
     */
    public long getElapsedTimeUnknown() {
        return elapsedTimeUnknown;
    }

    /**
     * @param elapsedTime
     *            the elapsedTime to set
     */
    public void setElapsedTimeUnknown(long elapsedTime) {
        this.elapsedTimeUnknown = elapsedTime;
    }

    /**
     * @param millis -
     *            Milliseconds to add to the elapsed time
     */
    public void addElapsedTimeUnknown(long millis) {
        this.elapsedTimeUnknown += millis;
    }

    /**
     * @return the noPost
     */
    public int getNoPost() {
        return noPost;
    }

    /**
     * @param noPost
     *            the noPost to set
     */
    public void setNoPost(int noPost) {
        this.noPost = noPost;
    }

    /**
     * Increment number of statiosn not posted
     */
    public void incrementNoPost() {
        this.noPost++;
    }

    /**
     * @return the elapsedTimeIngest
     */
    public long getElapsedTimeIngest() {
        return elapsedTimeIngest;
    }

    /**
     * @param elapsedTimeIngest
     *            the elapsedTimeIngest to set
     */
    public void setElapsedTimeIngest(long elapsedTimeIngest) {
        this.elapsedTimeIngest = elapsedTimeIngest;
    }

    /**
     * @param millis -
     *            Milliseconds to add to the elapsed time
     */
    public void addElapsedTimeIngest(long millis) {
        this.elapsedTimeIngest += millis;
    }

    /**
     * @return the outsideTimeWindow
     */
    public int getOutsideTimeWindow() {
        return outsideTimeWindow;
    }

    /**
     * @param outsideTimeWindow
     *            the outsideTimeWindow to set
     */
    public void setOutsideTimeWindow(int outsideTimeWindow) {
        this.outsideTimeWindow = outsideTimeWindow;
    }

    /**
     * Increment number of stations not posted because they fall outside the
     * configured time window
     */
    public void incrementOutsideWindow() {
        this.outsideTimeWindow++;
    }

    /**
     * @return the paired
     */
    public int getPaired() {
        return paired;
    }

    /**
     * @param paired
     *            the paired to set
     */
    public void setPaired(int paired) {
        this.paired = paired;
    }

    /**
     * Increment number of paired values stored
     */
    public void incrementPaired() {
        this.paired++;
    }

    /**
     * @return the paired
     */
    public int getPairedOver() {
        return pairedOver;
    }

    /**
     * @param pairedOver
     */
    public void setPairedOver(int pairedOver) {
        this.pairedOver = pairedOver;
    }

    /**
     * Increment number of paired values stored
     */
    public void incrementPairedOver() {
        this.pairedOver++;
    }

    /**
     * @return the ignored
     */
    public int getIgnored() {
        return ignored;
    }

    /**
     * @param ignored
     *            the ignored to set
     */
    public void setIgnored(int ignored) {
        this.ignored = ignored;
    }

    /**
     * Increment number of values ignored
     */
    public void incrementIgnored() {
        this.ignored++;
    }

    /**
     * @return the precipGpp
     */
    public int getPrecipGpp() {
        return precipGpp;
    }

    /**
     * @param precipGpp
     *            the precipGpp to set
     */
    public void setPrecipGpp(int precipGpp) {
        this.precipGpp = precipGpp;
    }

    /**
     * Increment number of gpp records sent
     */
    public void incrementPrecipGpp() {
        this.precipGpp++;
    }

    /**
     * @return the obsPe
     */
    public int getObsPe() {
        return obsPe;
    }

    /**
     * @param obsPe
     *            the obsPe to set
     */
    public void setObsPe(int obsPe) {
        this.obsPe = obsPe;
    }

    /**
     * Increment number of obs pe records stored
     */
    public void incrementObsPe() {
        this.obsPe++;
    }

    /**
     * @return the obsHeight
     */
    public int getObsHeight() {
        return obsHeight;
    }

    /**
     * @param obsHeight
     *            the obsHeight to set
     */
    public void setObsHeight(int obsHeight) {
        this.obsHeight = obsHeight;
    }

    /**
     * Increment number of obs pe records stored
     */
    public void incrementObsHeight() {
        this.obsHeight++;
    }

    /**
     * @return the obsPrecip
     */
    public int getObsPrecip() {
        return obsPrecip;
    }

    /**
     * @param obsPrecip
     *            the obsPrecip to set
     */
    public void setObsPrecip(int obsPrecip) {
        this.obsPrecip = obsPrecip;
    }

    /**
     * Increment number of obs pe records stored
     */
    public void incrementObsPrecip() {
        this.obsPrecip++;
    }

    /**
     * @return the elapsedTimeHeightIngest
     */
    public long getElapsedTimeHeightIngest() {
        return elapsedTimeHeightIngest;
    }

    /**
     * @param elapsedTimeHeightIngest
     *            the elapsedTimeHeightIngest to set
     */
    public void setElapsedTimeHeightIngest(long elapsedTimeHeightIngest) {
        this.elapsedTimeHeightIngest = elapsedTimeHeightIngest;
    }

    /**
     * 
     * @param millis -
     *            Milliseconds to add to the elapsed time
     */
    public void addElapsedTimeHeightIngest(long millis) {
        this.elapsedTimeHeightIngest += millis;
    }

    /**
     * @return the elapsedTimePrecipIngest
     */
    public long getElapsedTimePrecipIngest() {
        return elapsedTimePrecipIngest;
    }

    /**
     * @param elapsedTimePrecipIngest
     *            the elapsedTimePrecipIngest to set
     */
    public void setElapsedTimePrecipIngest(long elapsedTimePrecipIngest) {
        this.elapsedTimePrecipIngest = elapsedTimePrecipIngest;
    }

    /**
     * 
     * @param millis -
     *            Milliseconds to add to the elapsed time
     */
    public void addElapsedTimePrecipIngest(long millis) {
        this.elapsedTimePrecipIngest += millis;
    }

    /**
     * @return the elapsedTimeOtherIngest
     */
    public long getElapsedTimeOtherIngest() {
        return elapsedTimeOtherIngest;
    }

    /**
     * @param elapsedTimeOtherIngest
     *            the elapsedTimeOtherIngest to set
     */
    public void setElapsedTimeOtherIngest(long elapsedTimeOtherIngest) {
        this.elapsedTimeOtherIngest = elapsedTimeOtherIngest;
    }

    /**
     * 
     * @param millis -
     *            Milliseconds to add to the elapsed time
     */
    public void addElapsedTimeOtherIngest(long millis) {
        this.elapsedTimeOtherIngest += millis;
    }

    /**
     * @return the forecastPe
     */
    public int getForecastPe() {
        return forecastPe;
    }

    /**
     * @param forecastPe
     *            the forecastPe to set
     */
    public void setForecastPe(int forecastPe) {
        this.forecastPe = forecastPe;
    }

    /**
     * Increment number of forecast PEs to store
     */
    public void incrementForecastPe() {
        this.forecastPe++;
    }

    /**
     * @return the elapsedTimeForecastIngest
     */
    public long getElapsedTimeForecastIngest() {
        return elapsedTimeForecastIngest;
    }

    /**
     * @param elapsedTimeForecastIngest
     *            the elapsedTimeForecastIngest to set
     */
    public void setElapsedTimeForecastIngest(long elapsedTimeForecastIngest) {
        this.elapsedTimeForecastIngest = elapsedTimeForecastIngest;
    }

    /**
     * 
     * @param millis -
     *            Milliseconds to add to the elapsed time
     */
    public void addElapsedTimeForecastIngest(long millis) {
        this.elapsedTimeForecastIngest += millis;
    }

    /**
     * Get the Location ID list
     * 
     * @return the lidList
     */
    public List<String> getLidList() {
        return lidList;
    }

    /**
     * Set the Location ID list
     * 
     * @param lidList
     *            the lidList to set
     */
    public void setLidList(List<String> lidList) {
        this.lidList = lidList;
    }

    /**
     * Add the Location Id to the list if the list is 200 or less elements long
     * 
     * @param lid -
     *            the location id to add to the list
     */
    public void addLidList(String lid) {
        if (this.lidList.size() < 200) {
            this.lidList.add(lid);
        }
    }

    /**
     * Get the Physical Element list
     * 
     * @return the peList
     */
    public List<PhysicalElement> getPeList() {
        return peList;
    }

    /**
     * Set the Physical Element list
     * 
     * @param peList
     *            the peList to set
     */
    public void setPeList(List<PhysicalElement> peList) {
        this.peList = peList;
    }

    /**
     * Add the Physical Element to the list if the list is 200 or less elements
     * long
     * 
     * @param pe -
     *            the physical element to add to the list
     */
    public void addPeList(PhysicalElement pe) {
        if (this.peList.size() < 200) {
            this.peList.add(pe);
        }
    }

    /**
     * @return the maxForecast
     */
    public int getMaxForecast() {
        return maxForecast;
    }

    /**
     * @param maxForecast
     *            the maxForecast to set
     */
    public void setMaxForecast(int maxForecast) {
        this.maxForecast = maxForecast;
    }

    /**
     * Increment Max Forecast value
     */
    public void incrementMaxForecast() {
        this.maxForecast++;
    }

    /**
     * @return the arealObsOverwrite
     */
    public int getArealObsOverwrite() {
        return arealObsOverwrite;
    }

    /**
     * @param arealObsOverwrite
     *            the arealObsOverwrite to set
     */
    public void setArealObsOverwrite(int arealObsOverwrite) {
        this.arealObsOverwrite = arealObsOverwrite;
    }

    /**
     * Increment Areal Obs Overwrite
     */
    public void incrementArealObsOverwrite() {
        this.arealObsOverwrite++;
    }

    /**
     * @return the arealValues
     */
    public int getArealValues() {
        return arealValues;
    }

    /**
     * @param arealValues
     *            the arealValues to set
     */
    public void setArealValues(int arealValues) {
        this.arealValues = arealValues;
    }

    /**
     * Increment Areal Obs Added
     */
    public void incrementArealValues() {
        this.arealValues++;
    }

    /**
     * @return the contingencyOverwrite
     */
    public int getContingencyOverwrite() {
        return contingencyOverwrite;
    }

    /**
     * @param contingencyOverwrite
     *            the contingencyOverwrite to set
     */
    public void setContingencyOverwrite(int contingencyOverwrite) {
        this.contingencyOverwrite = contingencyOverwrite;
    }

    /**
     * Increment Contingency Values overwritten
     */
    public void incrementContingencyOverwrite() {
        this.contingencyOverwrite++;
    }

    /**
     * @return the contingencyValues
     */
    public int getContingencyValues() {
        return contingencyValues;
    }

    /**
     * @param contingencyValues
     *            the contingencyValues to set
     */
    public void setContingencyValues(int contingencyValues) {
        this.contingencyValues = contingencyValues;
    }

    /**
     * Increment Contingency Values
     */
    public void incrementContingencyValues() {
        this.contingencyValues++;
    }

    /**
     * @return the postProcessedOcerwrite
     */
    public int getPostProcessedOverwrite() {
        return postProcessedOverwrite;
    }

    /**
     * @param postProcessedOverwrite
     *            the postProcessedOcerwrite to set
     */
    public void setPostProcessedOverwrite(int postProcessedOverwrite) {
        this.postProcessedOverwrite = postProcessedOverwrite;
    }

    /**
     * Increment the post processed values
     */
    public void incrementPostProcessedOverwrite() {
        this.postProcessedOverwrite++;
    }

    /**
     * @return the postProcessedValues
     */
    public int getPostProcessedValues() {
        return postProcessedValues;
    }

    /**
     * @param postProcessedValues
     *            the postProcessedValues to set
     */
    public void setPostProcessedValues(int postProcessedValues) {
        this.postProcessedValues = postProcessedValues;
    }

    /**
     * Increment post processed values
     */
    public void incrementPostProcessedValues() {
        this.postProcessedValues++;
    }

    /**
     * @return the rejectedOverwrite
     */
    public int getRejectedOverwrite() {
        return rejectedOverwrite;
    }

    /**
     * @param rejectedOverwrite
     *            the rejectedOverwrite to set
     */
    public void setRejectedOverwrite(int rejectedOverwrite) {
        this.rejectedOverwrite = rejectedOverwrite;
    }

    /**
     * Increment rejected ovwewrite
     */
    public void incrementRejectedOverwrite() {
        this.rejectedOverwrite++;
    }

    /**
     * @return the rejected
     */
    public int getRejected() {
        return rejected;
    }

    /**
     * @param rejected
     *            the rejected to set
     */
    public void setRejected(int rejected) {
        this.rejected = rejected;
    }

    /**
     * Increment the Rejected values
     */
    public void incrementRejected() {
        this.rejected++;
    }

    /**
     * @return the commentOverwrite
     */
    public int getCommentOverwrite() {
        return commentOverwrite;
    }

    /**
     * @param commentOverwrite
     *            the commentOverwrite to set
     */
    public void setCommentOverwrite(int commentOverwrite) {
        this.commentOverwrite = commentOverwrite;
    }

    /**
     * Increment comment overwrite
     */
    public void incrementCommentOverwrite() {
        this.commentOverwrite++;
    }

    /**
     * @return the comments
     */
    public int getComments() {
        return comments;
    }

    /**
     * @param comments
     *            the comments to set
     */
    public void setComments(int comments) {
        this.comments = comments;
    }

    /**
     * Increment comment count
     */
    public void incrementComments() {
        this.comments++;
    }

    /**
     * @return the alertAlarm
     */
    public int getAlertAlarm() {
        return alertAlarm;
    }

    /**
     * @param alertAlarm
     *            the alertAlarm to set
     */
    public void setAlertAlarm(int alertAlarm) {
        this.alertAlarm = alertAlarm;
    }

    /**
     * Increment comment count
     */
    public void incrementAlertAlarm() {
        this.alertAlarm++;
    }

    /**
     * @return the latestObs
     */
    public int getLatestObs() {
        return latestObs;
    }

    /**
     * @param latestObs
     *            the latestObs to set
     */
    public void setLatestObs(int latestObs) {
        this.latestObs = latestObs;
    }

    /**
     * Increment latest obs count
     */
    public void incrementLatestObs() {
        this.latestObs++;
    }

    /**
     * @return the arealFcstOverwrite
     */
    public int getArealFcstOverwrite() {
        return arealFcstOverwrite;
    }

    /**
     * @param arealFcstOverwrite
     *            the arealFcstOverwrite to set
     */
    public void setArealFcstOverwrite(int arealFcstOverwrite) {
        this.arealFcstOverwrite = arealFcstOverwrite;
    }

    /**
     * Increment Areal Forecast Overwrite count
     */
    public void incrementArealFcstOverwrite() {
        this.arealFcstOverwrite++;
    }

    /**
     * @return the arealFcstValues
     */
    public int getArealFcstValues() {
        return arealFcstValues;
    }

    /**
     * @param arealFcstValues
     *            the arealFcstValues to set
     */
    public void setArealFcstValues(int arealFcstValues) {
        this.arealFcstValues = arealFcstValues;
    }

    /**
     * Increment Areal Forecast count
     */
    public void incrementArealFcst() {
        this.arealFcstValues++;
    }

    /**
     * @return the unknownStationValueOverwrite
     */
    public int getUnknownStationValueOverwrite() {
        return unknownStationValueOverwrite;
    }

    /**
     * @param unknownStationValueOverwrite
     *            the unknownStationValueOverwrite to set
     */
    public void setUnknownStationValueOverwrite(int unknownStationValueOverwrite) {
        this.unknownStationValueOverwrite = unknownStationValueOverwrite;
    }

    /**
     * Increment Unknown Station overwrite count
     */
    public void incrementUnknownStationOverwrite() {
        this.unknownStationValueOverwrite++;
    }

    /**
     * @return the unknownStationValue
     */
    public int getUnknownStationValue() {
        return unknownStationValue;
    }

    /**
     * @param unknownStationValue
     *            the unknownStationValue to set
     */
    public void setUnknownStationValue(int unknownStationValue) {
        this.unknownStationValue = unknownStationValue;
    }

    /**
     * Increment Unknown Station overwrite count
     */
    public void incrementUnknownStation() {
        this.unknownStationValue++;
    }
}
