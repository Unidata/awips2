package gov.noaa.nws.ncep.staticdataprovider;

import gov.noaa.nws.ncep.common.dataplugin.aww.AwwFips;
import gov.noaa.nws.ncep.common.dataplugin.aww.AwwRecord;
import gov.noaa.nws.ncep.common.dataplugin.aww.AwwUgc;
import gov.noaa.nws.ncep.common.dataplugin.aww.AwwVtec;
import gov.noaa.nws.ncep.common.staticdata.SPCCounty;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TimeZone;

import com.raytheon.uf.common.dataquery.requests.DbQueryRequest;
import com.raytheon.uf.common.dataquery.requests.DbQueryRequest.OrderMode;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.dataquery.responses.DbQueryResponse;
import com.raytheon.uf.viz.core.requests.ThriftClient;

/**
 * Data retrieval from the AWW tables by report type (WCN) and by watch number.
 * 
 * <pre>
 * 
 *  SOFTWARE HISTORY
 *  
 *  Date         Ticket#    Engineer    Description
 *  ------------ ---------- ----------- --------------------------
 *  2104 Mar 21  TRAC 1112  srussell    Initial creation
 *  
 *  ----------------------------------------------------------------------------
 *  Possible aw_vtec.action values:
 *  ----------------------------------------------------------------------------
 *    NEW - Brand new product
 *    CON - Continues for a subset of counties (usually issued in segmented 
 *          fashion with CAN).
 *    CAN - Cancels part or all of the area in the product.
 *    EXP - Watch expiration statement, this is different than a CAN in the 
 *          sense that it does not stop immediately but at the initial 
 *          expiration time of the watch.
 *    EXT - Extends the time a watch is valid for
 *    EXA - Extends the area a watch is valid for
 *    EXB - Extends the area and time a watch is valid for
 * 
 *     http://www.nws.noaa.gov/directives/sym/pd01005011curr.pdf
 * -----------------------------------------------------------------------------
 * 
 * </pre>
 * 
 * @author srussell
 * @version 1.0
 */

public class WatchActiveCounties {

    private final static TimeZone utc = TimeZone.getTimeZone("UTC");

    /* Queries Aww tables by report type and by watch number. Once it gets the
     * AWW records, it will loop through the records and get the latest county
     * UGCs. It will skip the UGCs with the action code “CAN” (canceled). It
     * will return a list of active county UGCs. */

    public List<String> getActiveCounties(int watchNum, List<SPCCounty> watchBoxCountyList) {

        List<String> ugcList = new ArrayList<String>();
        List<WatchRecord> watchRecords = new ArrayList<WatchRecord>();
        HashMap<String, RequestConstraint> constraints = new HashMap<String, RequestConstraint>();

        String pluginName = "aww";
        String reportType = "WATCH COUNTY NOTIFICATION";
        String orderBy = "issueTime,issueOffice";

        String likeClause = "%" + watchNum + "%";
        ConstraintType like = ConstraintType.LIKE;

        constraints.put("pluginName", new RequestConstraint(pluginName));
        constraints.put("reportType", new RequestConstraint(reportType));
        constraints.put("watchNumber", new RequestConstraint(likeClause, like));

        DbQueryRequest request = new DbQueryRequest();
        request.setConstraints(constraints);
        request.setOrderByField(orderBy, OrderMode.ASC);

        try {

            DbQueryResponse response = (DbQueryResponse) ThriftClient.sendRequest(request);

            if (response == null) {
                return ugcList;
            }

            List<Map<String, Object>> responseList = response.getResults();
            AwwRecord awwRecord = null;

            // Retrieve the latest watch records from the database for this watch
            for (Map<String, Object> eachResponse : responseList) {

                Collection<Object> recordObj = eachResponse.values();

                for (Object pdo : recordObj) {

                    if (pdo instanceof AwwRecord) {
                        awwRecord = (AwwRecord) pdo;
                        addWatchRecord(watchRecords, awwRecord, watchNum);
                    }

                }
            }

            // Copy the county ID strings out of the watch box county list
            for (SPCCounty county : watchBoxCountyList) {
                ugcList.add(county.getUgcId());
            }

            // Update the list of counties from the watch box list, with values
            // the values retrieved from the database.
            updateWatchBoxCountyList(ugcList, watchRecords);

        } catch (Exception e) {
            e.printStackTrace();
        }

        return ugcList;
    }

    private static void updateWatchBoxCountyList(List<String> ugcList, List<WatchRecord> watchRecords) {

        String ugc = null;
        String action = null;
        Calendar eventEndTime = null;
        Calendar currentTime = null;

        for (WatchRecord wr : watchRecords) {
            ugc = wr.getUgc();
            action = wr.getAction();

            // New county put into the watch
            if (action.equalsIgnoreCase("NEW")) {
                if (!ugcList.contains(ugc)) {
                    ugcList.add(ugc);
                }
            }

            // County watch cancelled
            else if (action.equalsIgnoreCase("CAN")) {
                ugcList.remove(ugc);
            }

            // County watch "expired"
            else if (action.equalsIgnoreCase("EXP")) {

                eventEndTime = wr.getEventEndTime();
                eventEndTime.setTimeZone(utc);
                currentTime = Calendar.getInstance(utc);

                if (ugcList.contains(ugc) && eventEndTime.before(currentTime)) {
                    ugcList.remove(ugc);
                }
            }
        }
    }

    private void addWatchRecord(List<WatchRecord> watchRecords, AwwRecord aww, int watchNum) {

        if (aww == null) {
            return;
        }

        WatchRecord county = null;
        int watchNumInUGC = -1;
        Set<AwwUgc> ugcSet = aww.getAwwUGC();
        String wfo = aww.getIssueOffice();
        Calendar issueTime = aww.getIssueTime();

        // For each AwwUgc
        for (AwwUgc ugc : ugcSet) {
            watchNumInUGC = -1;

            try {
                // Get the watch number from the UGC
                watchNumInUGC = Integer.parseInt(ugc.getEventTrackingNumber());

            } catch (NumberFormatException e) {
                continue;
            }

            // If watch number in the UGC does NOT match the one in the query
            if (watchNumInUGC != watchNum) {
                continue;
            }

            Set<AwwVtec> vtec = ugc.getAwwVtecLine();
            if (vtec.isEmpty()) {
                continue;
            }

            // Get the vtec.action
            AwwVtec one_vtec = vtec.iterator().next();
            String action = one_vtec.getAction();

            // Get the UGC
            Set<AwwFips> fipsSet = ugc.getAwwFIPS();
            for (AwwFips fips : fipsSet) {
                county = this.new WatchRecord();
                county.setUgc(fips.getFips());
                county.setWfo(wfo);
                county.setAction(action);
                county.setIssueTIme(issueTime);
                if (action.equalsIgnoreCase("EXP")) {
                    county.setEventEndTime(one_vtec.getEventEndTime());
                }
                if (!watchRecords.contains(county)) {
                    watchRecords.add(county);
                }
            }
        }

    }// end function addAwwUgc

    private class WatchRecord {
        private String ugc = null;

        private String wfo = null;

        private String action = null;

        private Calendar eventEndTime = null;

        private Calendar issueTime = null;

        public WatchRecord() {
        }

        public String getUgc() {
            return ugc;
        }

        public void setUgc(String ugc) {
            this.ugc = ugc;
        }

        public String getWfo() {
            return wfo;
        }

        public void setWfo(String wfo) {
            this.wfo = wfo;
        }

        public String getAction() {
            return action;
        }

        public void setAction(String action) {
            this.action = action;
        }

        public Calendar getEventEndTime() {
            return eventEndTime;
        }

        public void setEventEndTime(Calendar eventEndTime) {
            this.eventEndTime = eventEndTime;
        }

        public Calendar getIssueTime() {
            return issueTime;
        }

        public void setIssueTIme(Calendar issueTime) {
            this.issueTime = issueTime;
        }

        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = prime * result + getOuterType().hashCode();
            result = prime * result + ((action == null) ? 0 : action.hashCode());
            result = prime * result + ((eventEndTime == null) ? 0 : eventEndTime.hashCode());
            result = prime * result + ((issueTime == null) ? 0 : issueTime.hashCode());
            result = prime * result + ((ugc == null) ? 0 : ugc.hashCode());
            result = prime * result + ((wfo == null) ? 0 : wfo.hashCode());
            return result;
        }

        @Override
        public boolean equals(Object obj) {
            if (this == obj) {
                return true;
            }
            if (obj == null) {
                return false;
            }
            if (!(obj instanceof WatchRecord)) {
                return false;
            }
            WatchRecord other = (WatchRecord) obj;
            if (!getOuterType().equals(other.getOuterType())) {
                return false;
            }
            if (action == null) {
                if (other.action != null) {
                    return false;
                }
            } else if (!action.equals(other.action)) {
                return false;
            }
            if (eventEndTime == null) {
                if (other.eventEndTime != null) {
                    return false;
                }
            } else if (!eventEndTime.equals(other.eventEndTime)) {
                return false;
            }
            if (issueTime == null) {
                if (other.issueTime != null) {
                    return false;
                }
            } else if (!issueTime.equals(other.issueTime)) {
                return false;
            }
            if (ugc == null) {
                if (other.ugc != null) {
                    return false;
                }
            } else if (!ugc.equals(other.ugc)) {
                return false;
            }
            if (wfo == null) {
                if (other.wfo != null) {
                    return false;
                }
            } else if (!wfo.equals(other.wfo)) {
                return false;
            }
            return true;
        }

        private WatchActiveCounties getOuterType() {
            return WatchActiveCounties.this;
        }

    }

}// end class WatchActiveCounties
