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
package com.raytheon.uf.edex.plugin.svrwx.decoder;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * An InternalReport that contains an entire event report. The fields of a
 * report are parsed from the data, but not transformed in any way.
 *
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 25, 2014 3008       nabowle     Initial creation
 *
 * </pre>
 *
 * @author nabowle
 * @version 1.0
 */

public class EventReport extends InternalReport {

    private static final Pattern EVENT_KEY_PTRN = Pattern.compile(EVENT_KEY);

    private static final Pattern TIME_PTRN = Pattern.compile(TIME);

    private static final Pattern STATIONID_PTRN = Pattern.compile(STATIONID);

    private static final Pattern LATLON_PTRN = Pattern.compile(LATLON);

    private String event;

    private String time;

    private String key;

    private String remarks;

    private String latLon;

    private String stationId;

    /**
     * Constructor.
     *
     * @param event
     *            The event line.
     * @param pKey
     *            The event key.
     * @param pTime
     *            The event time.
     * @param station
     *            The station id.
     * @param latlon
     *            The latitude/longitude.
     * @param rmks
     *            The remarks.
     */
    private EventReport(String event, String pKey, String pTime,
            String station, String latlon, String rmks) {
        super(InternalType.EVENT_REPORT, event);
        this.key = pKey;
        this.time = pTime;
        this.stationId = station;
        this.latLon = latlon;
        this.remarks = rmks;
    }

    /**
     * @return the event
     */
    public String getEvent() {
        return event;
    }

    /**
     * @return the time
     */
    public String getTime() {
        return time;
    }

    /**
     * @return the key
     */
    public String getKey() {
        return key;
    }

    /**
     * @return the remarks
     */
    public String getRemarks() {
        return remarks;
    }

    /**
     * @return the latitude
     */
    public String getLatLon() {
        return latLon;
    }


    /**
     * @return the stationId
     */
    public String getStationId() {
        return stationId;
    }


    /**
     * Builder for the EventReport to facilitate creating an EventReport from
     * related data lines.
     */
    public static class Builder {

        private String eventLine;

        private List<String> remarks = new ArrayList<String>();

        /**
         * Constructor.
         */
        public Builder() {
            super();
        }

        /**
         * Creates an EventReport object from the event line and remarks.
         *
         * @return The built EventReport.
         */
        public EventReport build() {
            return new EventReport(buildEventString(), parseEventKey(),
                    parseTime(), parseStationId(), parseLatLon(),
                    parseRemarks());
        }

        /**
         * Sets the event line of the report.
         *
         * @param eventLine
         *            The event line.
         * @return This builder.
         */
        public Builder withEventLine(String eventLine) {
            this.eventLine = eventLine;
            return this;
        }

        /**
         * Adds a remarks line to the report.
         *
         * @param rmks
         *            The remarks line.
         * @return This builder.
         */
        public Builder withRemarks(String rmks) {
            this.remarks.add(rmks);
            return this;
        }

        /**
         * Builds a String of the entire event, including the event line and all
         * remarks lines.
         *
         * @return The event string.
         */
        private String buildEventString() {
            StringBuilder sb = new StringBuilder(this.eventLine);
            for (String rmk : this.remarks) {
                sb.append("\n").append(rmk);
            }

            return sb.toString();
        }

        /**
         * Parses the event key field.
         *
         * @return The event key field.
         */
        private String parseEventKey() {
            return parse(this.eventLine, EVENT_KEY_PTRN);
        }

        /**
         * Parses the time field.
         *
         * @return the time field.
         */
        private String parseTime() {
            return parse(this.eventLine, TIME_PTRN);
        }

        /**
         * Parses the station id field.
         *
         * @return The station id field.
         */
        private String parseStationId() {
            String station = parseFromRemarks(STATIONID_PTRN);

            if (station != null) {
                station = station.substring(0, 3);
            }

            return station;
        }

        /**
         * Parses the combined latitude/longitude field.
         *
         * @return The combined latitude/longitude field.
         */
        private String parseLatLon() {
            return parseFromRemarks(LATLON_PTRN);
        }



        /**
         * Parses the remarks field from the remarks lines. The first remarks
         * line contains other fields, starting with the station id, which are
         * not included in the remarks field.
         *
         * @return The parsed remarks fields.
         */
        private String parseRemarks() {
            String ret;
            if (this.remarks.isEmpty()) {
                ret = null;
            } else {

                StringBuilder sb = new StringBuilder();
                String remark = this.remarks.get(0);
                Matcher m = STATIONID_PTRN.matcher(remark);

                if (m.find()) {
                    sb.append(remark.substring(0, m.start()).trim());
                } else {
                    sb.append(remark.trim());
                }

                for (int i = 1; i < this.remarks.size(); i++) {
                    sb.append(" ").append(this.remarks.get(i).trim());
                }

                ret = sb.toString();
            }
            return ret;
        }

        /**
         * Parses a field from a line.
         *
         * @param line
         *            The line to pull the field from.
         * @param pattern
         *            The pattern of the field.
         * @return The matched group, or null if the pattern does not match the
         *         line.
         */
        private String parse(String line, Pattern pattern) {
            String ret;
            if (line == null) {
                ret = null;
            } else {
                Matcher m = pattern.matcher(line);
                if (m.find()) {
                    ret = m.group();
                } else {
                    ret = null;
                }
            }
            return ret;
        }

        /**
         * Parses a field from the first remarks line.
         *
         * @param pattern
         *            The pattern of the field.
         * @return The matched group, or null if the remarks or empty or the
         *         pattern does not match the remarks line.
         */
        private String parseFromRemarks(Pattern pattern) {
            String ret;
            if (this.remarks.isEmpty()) {
                ret = null;
            } else {
                ret = parse(this.remarks.get(0), pattern);
            }
            return ret;
        }
    }
}
