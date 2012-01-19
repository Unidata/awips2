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
package com.raytheon.viz.aviation.utility;

import java.util.Calendar;
import java.util.TimeZone;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 27, 2009            avarani     Initial creation
 * 
 * </pre>
 * 
 * @author avarani
 * @version 1.0
 */

public class TafMessageData implements Comparable<TafMessageData> {
    private int forecasterId;

    private Calendar xmitTime;

    private String tafText;

    private String bbb;

    private String siteId;

    private String siteWmoId;

    private String siteNode;

    private String type;

    private Calendar headerTime;

    public TafMessageData(int forecasterId, Calendar xmitTime, String tafText,
            String bbb, String siteId, String siteWmoId, String siteNode,
            String type, Calendar headerTime) {
        this.forecasterId = forecasterId;
        this.xmitTime = xmitTime;
        this.tafText = tafText;
        this.bbb = bbb;
        this.siteId = siteId;
        this.siteWmoId = siteWmoId;
        this.siteNode = siteNode;
        this.type = type;
        this.headerTime = headerTime;
    }

    public String getInfo() {
        return String
                .format(
                        "%1$03d-%7$s%8$s%5$s-%6$s-%7$s-%2$ty%2$tm%2$td%2$tH%2$tM-%4$s-%9$d",
                        forecasterId, headerTime, tafText, bbb, siteId,
                        siteWmoId, siteNode, type,
                        (xmitTime.getTimeInMillis() / 1000));
    }

    // Update the xmitTime to the current time.
    public void updateTime() {
        xmitTime = Calendar.getInstance();
        xmitTime.setTimeInMillis(System.currentTimeMillis());
        xmitTime.setTimeZone(TimeZone.getTimeZone("GMT"));
    }

    public long getXmitTime() {
        return xmitTime.getTimeInMillis();
    }

    public String getTafText() {
        return tafText;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Comparable#compareTo(java.lang.Object)
     */
    @Override
    public int compareTo(TafMessageData o) {
        if (o.xmitTime.before(this.xmitTime)) {
            return -1;
        } else if (this.xmitTime.before(o.xmitTime)) {
            return 1;
        } else {
            return 0;
        }
    }

    public String getSiteId() {
        return siteId;
    }

    public String getWmoId() {
        return siteWmoId;
    }

    public String getBBB() {
        return bbb;
    }

    public void setSiteId(String siteId) {
        this.siteId = siteId;
    }

}
