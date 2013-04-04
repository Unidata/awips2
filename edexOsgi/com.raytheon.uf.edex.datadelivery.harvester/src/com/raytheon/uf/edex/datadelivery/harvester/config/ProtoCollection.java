package com.raytheon.uf.edex.datadelivery.harvester.config;

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

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Map.Entry;

import com.raytheon.uf.common.datadelivery.registry.Collection.Periodicity;
import com.raytheon.uf.common.time.util.TimeUtil;

/**
 * Proto-Collection object.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 6, 2012  1038       dhladky     Initial creation
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public class ProtoCollection {

    private Map<Integer, ArrayList<Date>> dates = new LinkedHashMap<Integer, ArrayList<Date>>();

    private Map<Integer, SimpleDateFormat> formats = new LinkedHashMap<Integer, SimpleDateFormat>();

    private static Comparator<Date> dateComparator = new Comparator<Date>() {
        @Override
        public int compare(Date o1, Date o2) {
            // Null checks?
            return o2.before(o1) ? -1 : o1.equals(o2) ? 0 : 1;
        }
    };

    private String seedUrl;

    private String url;

    private String urlKey;

    // We grab the name and a sample url
    public ProtoCollection(String seedUrl, String url) {
        this.seedUrl = seedUrl;
        this.url = url;
    }

    /**
     * Standard collection naming format
     * 
     * @return
     */
    public String getCollectionName() {
        return getSeedUrl().replaceAll("/", "_");
    }

    /**
     * Build the expected date URL portions
     * 
     * @return
     */
    public String getDateFormatString() {
        StringBuilder sb = new StringBuilder();
        int i = 0;
        for (Entry<Integer, SimpleDateFormat> entry : getFormats().entrySet()) {
            SimpleDateFormat format = entry.getValue();
            sb.append(format.toPattern());
            if (i < formats.size() - 1) {
                sb.append("/");
            }
            i++;
        }

        return sb.toString();
    }

    public Map<Integer, ArrayList<Date>> getDates() {
        return dates;
    }

    public Date getFirstDate(int depth) {
        Collections.sort(getDates().get(depth), dateComparator);
        return getDates().get(depth).get(getDates().get(depth).size() - 1);
    }

    /**
     * formatted first date
     * 
     * @return
     */
    public String getFirstDateFormatted() {
        int maxDepth = getMaxDepthFormat();
        String format = null;
        if (maxDepth >= 0) {
            Date first = getFirstDate(getMaxDepthFormat());
            SimpleDateFormat sdf = getFormats().get(getMaxDepthFormat());
            format = sdf.format(first);
        }
        return format;
    }

    public Map<Integer, SimpleDateFormat> getFormats() {
        return formats;
    }

    public Date getLastDate(int depth) {
        Collections.sort(getDates().get(depth), dateComparator);
        return getDates().get(depth).get(0);
    }

    /**
     * Formatted last date
     * 
     * @return
     */
    public String getLastDateFormatted() {
        int maxDepth = getMaxDepthFormat();
        String format = null;
        if (maxDepth >= 0) {
            Date last = getLastDate(maxDepth);
            SimpleDateFormat sdf = null;
            if (last != null) {
                sdf = getFormats().get(getMaxDepthFormat());
            }
            format = sdf.format(last);
        }
        return format;
    }

    /**
     * Finds the maximum format depth
     * 
     * @return
     */
    public int getMaxDepthFormat() {
        int maxDepth = -1;
        for (Integer i : formats.keySet()) {
            if (i > maxDepth) {
                maxDepth = i;
            }
        }
        return maxDepth;
    }

    /**
     * Get the period of the directory structure for this provider
     * 
     * @return
     */
    public Periodicity getPeriodicity() {
        // grab two dates
        Periodicity p = Periodicity.NONE;
        if (getDates() != null && getDates().size() > 0) {
            Date date1 = getDates().get(getMaxDepthFormat()).get(0);
            Date date2 = getDates().get(getMaxDepthFormat()).get(1);

            long timeDiff = Math.abs(date1.getTime() - date2.getTime());

            // do checks
            if (timeDiff == TimeUtil.MILLIS_PER_DAY) {
                p = Periodicity.DAY;
            } else if (timeDiff == TimeUtil.MILLIS_PER_HOUR) {
                p = Periodicity.HOUR;
            } else if (timeDiff == TimeUtil.MILLIS_PER_WEEK) {
                p = Periodicity.WEEK;
            } else if (timeDiff > TimeUtil.MILLIS_PER_WEEK
                    && timeDiff < TimeUtil.MILLIS_PER_YEAR) {
                p = Periodicity.MONTH;
            } else if (timeDiff == TimeUtil.MILLIS_PER_YEAR) {
                p = Periodicity.YEAR;
            }
        }

        return p;
    }

    public String getSeedUrl() {
        return seedUrl;
    }

    public String getUrl() {
        return url;
    }

    public String getUrlKey() {
        return urlKey;
    }

    public void setDates(Map<Integer, ArrayList<Date>> dates) {
        this.dates = dates;
    }

    public void setFormats(Map<Integer, SimpleDateFormat> formats) {
        this.formats = formats;
    }

    public void setSeedUrl(String seedUrl) {
        this.seedUrl = seedUrl;
    }

    public void setUrl(String url) {
        this.url = url;
    }

    public void setUrlKey(String urlKey) {
        this.urlKey = urlKey;
    }
}
