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

package com.raytheon.uf.edex.database.purge;

import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;

import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * This class contains a rule used for purging data from the data store. This
 * class was derived from the AWIPS I purgeInfo.txt file.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 2/15/11      #2469       bphillip    Initial creation
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
@XmlAccessorType(XmlAccessType.NONE)
public class PurgeRule {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(PurgeRule.class);

    /** The serial number */
    private static final long serialVersionUID = 7121154611198571831L;

    /**
     * The pattern used for decoding time strings in the rule. Times are of the
     * format dd-hh:mm:ss where dd is the days, hh is the hours, mm is the
     * minutes, and ss is the seconds. Purge rules must provide an entire time
     * string (i.e 01-00:00:00 for one day) and not truncated time values
     */
    private static final String TIME_PATTERN_STRING = "([0-9]{1,2})-([0-9]{1,2}):([0-9]{1,2}):([0-9]{1,2})";

    /**
     * The Pattern object used for applying the TIME_PATTERN_STRING
     */
    private static final Pattern TIME_PATTERN = Pattern
            .compile(TIME_PATTERN_STRING);

    /**
     * The keys this rule is to match. Should be equal to a value based on the
     * key fields of the record specified in the PurgeRuleSet. Special cases are
     * made for keyMatch of value default.
     */
    @XmlElements({ @XmlElement(name = "keyValue", type = String.class) })
    private List<String> keyValues;

    /** The number of versions to keep */
    @XmlElement
    @DynamicSerializeElement
    private int versionsToKeep = 0;

    /**
     * Max period between the current time and oldest time stamp of files to
     * keep; defaults to 0 which means do not time purge. A leading tilde (~) on
     * the period means calculate from the latest file time stamp instead of the
     * current time.
     */
    @XmlElement
    @DynamicSerializeElement
    private String period = "0";

    /**
     * Data with a time stamp separated by less than this from the next newest
     * file will not be kept. Defaults to zero, which means do not consider time
     * separation. If a leading equals (=), keep only files an exact multiple of
     * this delta time, if a leading tilde (~), keep only the one file closest
     * to an exact multiple of this delta time.
     */
    @XmlElement
    @DynamicSerializeElement
    private String delta = "0";

    /**
     * Round times by this before deciding whether to purge. Defaults to zero,
     * which means do not round. The rounding time interacts with the delta, but
     * not the period. If a leading plus sign (+), add the time instead of
     * rounding by it. If consecutive files round to the same time, then if one
     * is kept they will all be kept.
     */
    @XmlElement
    @DynamicSerializeElement
    private String round = "0";

    /**
     * A non-blank entry means do not actually purge by this entry, only log
     * what would have been purged
     */
    @XmlElement
    @DynamicSerializeElement
    private boolean logOnly = false;

    /**
     * Time period to wait after the mod time of the latest file to purge
     * normally; this allows the most recent file to be completed before the
     * oldest is purged.
     */
    @XmlElement
    @DynamicSerializeElement
    private String modTimeToWait = "0";

    @XmlElement
    @DynamicSerializeElement
    private String ruleDescription;

    /**
     * Creates a new empty purge rule
     */
    public PurgeRule() {

    }

    /**
     * Gets the key values associated with the PurgeRuleSet keys.
     * 
     * @return
     */
    public List<String> getKeyValues() {
        return keyValues;
    }

    /**
     * Sets the key values associated with the PurgeRuleSet keys.
     * 
     * @param keyValues
     */
    public void setKeyValues(List<String> keyValues) {
        this.keyValues = keyValues;
    }

    /**
     * Gets the time period in milliseconds
     * 
     * @return The time period in milliseconds
     */
    public long getPeriodInMillis() {
        return parseTimePeriod(this.period);
    }

    /**
     * Gets the round time in milliseconds
     * 
     * @return The round time in milliseconds
     */
    public long getRoundInMillis() {
        return parseTimePeriod(this.round);
    }

    /**
     * Gets the last modification time in milliseconds
     * 
     * @return The last modification time in milliseconds
     */
    public long getModTimeToWaitInMillis() {
        return parseTimePeriod(this.modTimeToWait);
    }

    /**
     * Gets the delta time in milliseconds
     * 
     * @return The delta time in milliseconds
     */
    public long getDeltaTimeInMillis() {
        return parseTimePeriod(this.delta);
    }

    /**
     * Checks if this rule specifies versions to keep
     * 
     * @return True if versions to keep are specified
     */
    public boolean isVersionsToKeepSpecified() {
        return versionsToKeep != 0;
    }

    /**
     * Checks if this rule specifies a delta time
     * 
     * @return True if a delta time is specified
     */
    public boolean isDeltaSpecified() {
        return !this.delta.equals("0");
    }

    /**
     * Checks if this rule specifies a last modification time to wait period
     * 
     * @return True if one is specified
     */
    public boolean isModTimeToWaitSpecified() {
        return !modTimeToWait.equals("0");
    }

    /**
     * Checks if this rule specifies a rounding time
     * 
     * @return True if a rounding time is specified
     */
    public boolean isRoundSpecified() {
        return !round.equals("0");
    }

    /**
     * Checks if this rule specifies a period
     * 
     * @return True if a period is specified
     */
    public boolean isPeriodSpecified() {
        return !period.equals("0");
    }

    /**
     * Checks if the round value should be added
     * 
     * @return True if the round value should be added
     */
    public boolean isRoundAdditive() {
        return round.startsWith("+");
    }

    /**
     * Checks if the delta time specifies an exact multiple
     * 
     * @return True if the deta time is to be used as an exact multiple
     */
    public boolean isDeltaTimeMultiple() {
        return this.delta.startsWith("=");
    }

    /**
     * Checks if the delta time is to be an estimate
     * 
     * @return True if the delta time is to be used as an estimate
     */
    public boolean isDeltaTimeClosest() {
        return this.delta.startsWith("~");
    }

    /**
     * Checks if the period specified is to use the latest data time instead of
     * the current time
     * 
     * @return True if the period is to use the latest data time instead of the
     *         current time
     */
    public boolean isPeriodBasedOnLatestTime() {
        return period.startsWith("~");
    }

    /**
     * Gets the human readable description of the modification time to wait
     * 
     * @return The human readable description of the modification time to way
     */
    public String getModTimeToWaitDescription() {
        return getTimeDescription(this.modTimeToWait);
    }

    /**
     * Gets the rounded time for the given date based on the delta time
     * specified
     * 
     * @param refTime
     *            The time to round
     * @return The rounded time for the given date based on the delta time
     *         specified
     */
    public Date[] getRoundedDate(Date refTime) {
        Date timeToCompare = new Date(refTime.getTime());
        if (this.isRoundSpecified()) {
            if (this.isRoundAdditive()) {
                timeToCompare.setTime(refTime.getTime()
                        + this.getRoundInMillis());
            } else {
                /*
                 * Determine the upper and lower bounds based on the delta time
                 * specified which contain the time to be examined
                 */
                Date upperLimit = null;
                Date lowerLimit = null;
                long roundInMillis = getRoundInMillis();
                long timeInMillis = refTime.getTime();
                long quotient = timeInMillis / roundInMillis;
                lowerLimit = new Date(quotient * roundInMillis);
                upperLimit = new Date((quotient + 1) * roundInMillis);
                if ((timeInMillis - lowerLimit.getTime()) < (upperLimit
                        .getTime() - timeInMillis)) {
                    timeToCompare = lowerLimit;
                } else {
                    timeToCompare = upperLimit;
                }
            }
        }
        return new Date[] { refTime, timeToCompare };
    }

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        builder.append("[");

        for (String kv : keyValues) {
            builder.append("KeyValue: ").append(kv).append("  ");
        }

        builder.append("VersionToKeep: ").append(this.versionsToKeep)
                .append("  ");
        builder.append("Period: ").append(this.period).append("  ");
        builder.append("Delta: ").append(this.delta).append("  ");
        builder.append("Round: ").append(this.round).append("  ");
        builder.append("LogOnly: ").append(this.logOnly);
        builder.append("]");
        return builder.toString();
    }

    /**
     * Gets the human readable description of this rule
     * 
     * @return The human readable description of this rule
     */
    public String getRuleDescription(List<String> keys) {

        if (this.ruleDescription == null) {

            StringBuilder builder = new StringBuilder();

            if ((keys == null) || keys.isEmpty() || (keyValues == null)
                    || keyValues.isEmpty()) {
                builder.append("Default rule, ");
            } else {
                builder.append("For data matching ");
                Iterator<String> keyIter = keys.iterator();
                Iterator<String> valueIter = keyValues.iterator();
                while (keyIter.hasNext() && valueIter.hasNext()) {
                    builder.append(keyIter.next()).append("=")
                            .append(valueIter.next()).append(", ");
                }
            }

            if (isDeltaSpecified()) {
                getVersionsClause(builder);
                builder.append("at ").append(this.getTimeDescription(delta))
                        .append("intervals ");
                getRoundClause(builder);
                getPeriodClause(builder);
            } else if (!isDeltaSpecified() && isVersionsToKeepSpecified()) {
                getVersionsClause(builder);
                getPeriodClause(builder);
            } else if (!isDeltaSpecified() && !isVersionsToKeepSpecified()
                    && isPeriodSpecified()) {
                getVersionsClause(builder);
                getPeriodClause(builder);
            } else {
                builder.append("keep all data.");
            }
            if (isModTimeToWaitSpecified()) {
                builder.append(
                        " Do not purge if most recent version has been modified in the last ")
                        .append(this.getModTimeToWaitDescription());
            }
            ruleDescription = builder.toString();
        }
        return ruleDescription;

    }

    /**
     * @param ruleDescription
     *            the ruleDescription to set
     */
    public void setRuleDescription(String ruleDescription) {
        this.ruleDescription = ruleDescription;
    }

    /**
     * Gets the human readable version of the round time
     * 
     * @param builder
     *            The string builder to append to
     */
    private void getRoundClause(StringBuilder builder) {
        if (isRoundSpecified()) {
            if (isRoundAdditive()) {
                builder.append("plus ").append(
                        this.getTimeDescription(this.round));
            } else {
                builder.append("rounding to ").append(
                        this.getTimeDescription(this.round));
            }
        } else {
            builder.append("with no rounding applied ");
        }
    }

    /**
     * Gets the human readable version of the versions to keep
     * 
     * @param builder
     *            The string builder to append to
     */
    private void getVersionsClause(StringBuilder builder) {
        if (isVersionsToKeepSpecified()) {
            builder.append("keep the ").append(versionsToKeep)
                    .append(" most recent versions of data ");
        } else {
            builder.append("keep data ");
        }
    }

    /**
     * Gets the human readable version of the period
     * 
     * @param builder
     *            The string builder to append to
     */
    private void getPeriodClause(StringBuilder builder) {
        if (isPeriodSpecified()) {
            builder.append("newer than ").append(
                    getTimeDescription(this.period));
            if (isPeriodBasedOnLatestTime()) {
                builder.append("based on the most recent reference time.");
            } else {
                builder.append("based on the current time.");
            }
        }
    }

    /**
     * Parses a given formatted time into milliseconds
     * 
     * @param time
     *            The time to parse
     * @return The milliseconds represented by the given formatted time
     */
    private long parseTimePeriod(String time) {
        String timePeriod = time;
        if (timePeriod.startsWith("~")) {
            time = timePeriod.replace("~", "");
        }
        if (timePeriod.startsWith("=")) {
            time = timePeriod.replace("=", "");
        }
        Matcher timeMatcher = TIME_PATTERN.matcher(timePeriod);

        long days = 0;
        long hours = 0;
        long minutes = 0;
        long seconds = 0;

        // Default to 12 hours
        long milliseconds = 43200000;
        if (timeMatcher.find()) {
            days = Long.parseLong(timeMatcher.group(1));
            hours = Long.parseLong(timeMatcher.group(2));
            minutes = Long.parseLong(timeMatcher.group(3));
            seconds = Long.parseLong(timeMatcher.group(4));
            milliseconds = seconds * 1000 + minutes * 60000 + hours * 3600000
                    + days * 86400000;
        } else {
            statusHandler.handle(Priority.ERROR,
                    "Encountered incorrectly formatted time period: "
                            + timePeriod + ".  Using default of 12 hrs");
        }

        return milliseconds;
    }

    /**
     * Gets the human readable form of the given formatted time
     * 
     * @param time
     *            The time to translate
     * @return The human readable form of the given formatted time
     */
    private String getTimeDescription(String time) {

        StringBuilder builder = new StringBuilder();
        String timePeriod = time;
        if (timePeriod.startsWith("~")) {
            time = timePeriod.replace("~", "");
        }
        if (timePeriod.startsWith("=")) {
            time = timePeriod.replace("=", "");
        }
        Matcher timeMatcher = TIME_PATTERN.matcher(timePeriod);

        long days = 0;
        long hours = 0;
        long minutes = 0;
        long seconds = 0;

        if (timeMatcher.find()) {
            days = Long.parseLong(timeMatcher.group(1));
            hours = Long.parseLong(timeMatcher.group(2));
            minutes = Long.parseLong(timeMatcher.group(3));
            seconds = Long.parseLong(timeMatcher.group(4));

            if (days > 0) {
                builder.append(days).append(" day");
                if (days != 1) {
                    builder.append("s");
                }
                builder.append(" ");
            }

            if (hours > 0) {
                builder.append(hours).append(" hour");
                if (hours != 1) {
                    builder.append("s");
                }
                builder.append(" ");
            }

            if (minutes > 0) {
                builder.append(minutes).append(" minute");
                if (minutes != 1) {
                    builder.append("s");
                }
                builder.append(" ");
            }

            if (seconds > 0) {
                builder.append(seconds).append(" second");
                if (seconds != 1) {
                    builder.append("s");
                }
                builder.append(" ");
            }
        } else {
            builder.append("0 seconds");
        }
        return builder.toString();
    }

    /**
     * @return the versionsToKeep
     */
    public int getVersionsToKeep() {
        return versionsToKeep;
    }

    /**
     * @param versionsToKeep
     *            the versionsToKeep to set
     */
    public void setVersionsToKeep(int versionsToKeep) {
        this.versionsToKeep = versionsToKeep;
    }

    /**
     * @return the period
     */
    public String getPeriod() {
        return period;
    }

    /**
     * @param period
     *            the period to set
     */
    public void setPeriod(String period) {
        this.period = period;
    }

    /**
     * @return the delta
     */
    public String getDelta() {
        return delta;
    }

    /**
     * @param delta
     *            the delta to set
     */
    public void setDelta(String delta) {
        this.delta = delta;
    }

    /**
     * @return the round
     */
    public String getRound() {
        return round;
    }

    /**
     * @param round
     *            the round to set
     */
    public void setRound(String round) {
        this.round = round;
    }

    /**
     * @return the logOnly
     */
    public boolean isLogOnly() {
        return logOnly;
    }

    /**
     * @return the modTimeToWait
     */
    public String getModTimeToWait() {
        return modTimeToWait;
    }

    /**
     * @param modTimeToWait
     *            the modTimeToWait to set
     */
    public void setModTimeToWait(String modTimeToWait) {
        this.modTimeToWait = modTimeToWait;
    }

    /**
     * @param logOnly
     *            the logOnly to set
     */
    public void setLogOnly(boolean logOnly) {
        this.logOnly = logOnly;
    }
}
