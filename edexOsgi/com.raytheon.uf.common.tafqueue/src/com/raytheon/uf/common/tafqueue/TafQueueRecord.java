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
package com.raytheon.uf.common.tafqueue;

import java.util.Calendar;
import java.util.Date;
import java.util.TimeZone;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.Table;

import org.hibernate.annotations.Index;

import com.raytheon.uf.common.dataplugin.persist.IPersistableDataObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * A record corresponding to a TAF ready to be sent
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 27, 2009            avarani     Initial creation
 * Apr 30, 2012 14715      rferrel     Refactored and moved.
 * Mar 21, 2013 15375      zhao        Modified getInfo() to also handle VFT product
 * May 07, 2014 3091       rferrel     Use OUP authorization to bring up send dialog.
 * Feb 03, 2017 6065       tgurney     Add copy constructor
 * Mar 22, 2017 6065       tgurney     Add xml flag
 * Aug 27, 2019 21545   mgamazaychikov Add conversion of bbb from empty to underscores in getInfo
 *
 * </pre>
 *
 * @author avarani
 */
@Entity
@Table(name = "taf_queue")
@DynamicSerialize
public class TafQueueRecord
        implements IPersistableDataObject<Integer>, Comparable<TafQueueRecord> {

    private static final long serialVersionUID = 1L;

    public enum TafQueueState {
        PENDING, SENT, BAD
    }

    @DynamicSerializeElement
    @Id
    @GeneratedValue()
    private int id;

    @DynamicSerializeElement
    @Column
    private String forecasterId;

    @DynamicSerializeElement
    @Column(columnDefinition = "timestamp without time zone", nullable = false)
    @Index(name = "xmitTime_Index")
    private Date xmitTime;

    @DynamicSerializeElement
    @Column(columnDefinition = "text")
    private String tafText;

    @DynamicSerializeElement
    @Column(length = 3)
    private String bbb;

    @DynamicSerializeElement
    @Column(length = 3)
    private String siteId;

    @DynamicSerializeElement
    @Column(length = 6)
    private String wmoId;

    @DynamicSerializeElement
    @Column(length = 4)
    private String stationId;

    @DynamicSerializeElement
    @Column(columnDefinition = "timestamp without time zone", nullable = false)
    private Date headerTime;

    @DynamicSerializeElement
    @Column(nullable = false, length = 10)
    @Index(name = "state_Index")
    @Enumerated(EnumType.STRING)
    private TafQueueState state;

    @DynamicSerializeElement
    @Column(columnDefinition = "text")
    private String statusMessage;

    @DynamicSerializeElement
    @Column
    private boolean display;

    @DynamicSerializeElement
    @Column
    private boolean xml;

    public TafQueueRecord() {
        super();
    }

    /**
     * Construct a record with the desired values.
     *
     * @param forecasterId
     * @param xmitTime
     * @param tafText
     * @param bbb
     * @param siteId
     * @param wmoId
     * @param stationId
     * @param headerTime
     */
    public TafQueueRecord(String forecasterId, Date xmitTime, String tafText,
            String bbb, String siteId, String wmoId, String stationId,
            Date headerTime) {
        this.forecasterId = forecasterId;
        this.xmitTime = xmitTime;
        this.tafText = tafText;
        this.bbb = bbb;
        this.siteId = siteId;
        this.wmoId = wmoId;
        this.stationId = stationId;
        this.headerTime = headerTime;

        // New records should always have the following default values.
        this.state = TafQueueState.PENDING;
        this.statusMessage = "";
        this.display = true;
        this.xml = false;
    }

    /**
     * Copy constructor
     *
     * @param other
     */
    public TafQueueRecord(TafQueueRecord other) {
        this.forecasterId = other.forecasterId;
        this.xmitTime = other.xmitTime;
        this.tafText = other.tafText;
        this.bbb = other.bbb;
        this.siteId = other.siteId;
        this.wmoId = other.wmoId;
        this.stationId = other.stationId;
        this.headerTime = other.headerTime;
        this.state = other.state;
        this.statusMessage = other.statusMessage;
        this.display = other.display;
        this.xml = other.xml;
    }

    /**
     * Format the information "file name" string.
     *
     * @return info
     */
    public String getInfo() {
        String bbbInfo;
        if ("   ".equals(bbb)) {
           bbbInfo = "___";
        } else {
          bbbInfo = bbb;
        }
        return String.format(
                "%1$s-%7$s%8$s%5$s-%6$s-%7$s-%2$ty%2$tm%2$td%2$tH%2$tM-%4$s-%9$d",
                forecasterId, headerTime, tafText, bbbInfo, siteId, wmoId,
                stationId, getProductTag(), xmitTime.getTime() / 1000);
    }

    public String getAwipsWanPil() {
        return stationId + getProductTag() + siteId;
    }

    public String getProductTag() {
        String rval = "TAF";
        if (xml) {
            rval = "TML";
        }
        // for VFT product (DR15375)
        if (forecasterId != null && forecasterId
                .equals(TafQueueVftConfigMgr.getInstance().getFcstid())) {
            rval = "VFT";
        }
        return rval;
    }

    /**
     * Update the xmitTime to the current time.
     */
    public void updateTime() {
        Calendar cal = Calendar.getInstance();
        cal.setTimeInMillis(System.currentTimeMillis());
        cal.setTimeZone(TimeZone.getTimeZone("GMT"));
        xmitTime = cal.getTime();
    }

    public Date getXmitTime() {
        return xmitTime;
    }

    public String getTafText() {
        return tafText;
    }

    @Override
    public int compareTo(TafQueueRecord o) {
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

    public TafQueueState getState() {
        return state;
    }

    public String getWmoId() {
        return wmoId;
    }

    public String getBBB() {
        return bbb;
    }

    public boolean isDisplay() {
        return display;
    }

    public void setSiteId(String siteId) {
        this.siteId = siteId;
    }

    public void setState(TafQueueState state) {
        this.state = state;
    }

    public int getId() {
        return id;
    }

    public void setId(int id) {
        this.id = id;
    }

    public String getForecasterId() {
        return forecasterId;
    }

    public void setForecasterId(String forecasterId) {
        this.forecasterId = forecasterId;
    }

    public String getBbb() {
        return bbb;
    }

    public void setBbb(String bbb) {
        this.bbb = bbb;
    }

    public void setDisplay(boolean display) {
        this.display = display;
    }

    public String getStationId() {
        return stationId;
    }

    public String getStatusMessage() {
        return statusMessage;
    }

    public void setStationId(String stationId) {
        this.stationId = stationId;
    }

    public Date getHeaderTime() {
        return headerTime;
    }

    public void setHeaderTime(Date headerTime) {
        this.headerTime = headerTime;
    }

    public void setXmitTime(Date xmitTime) {
        this.xmitTime = xmitTime;
    }

    public void setStatusMessage(String statusMessage) {
        this.statusMessage = statusMessage;
    }

    public void setTafText(String tafText) {
        this.tafText = tafText;
    }

    public void setWmoId(String wmoId) {
        this.wmoId = wmoId;
    }

    @Override
    public Integer getIdentifier() {
        return getId();
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("TafQueueRecord, id:").append(id).append(", forecasterId:")
                .append(forecasterId);
        sb.append("\tbbb:").append(bbb).append(", siteId:").append(siteId)
                .append(", wmoId:").append(wmoId).append(", stationId:")
                .append(stationId).append(", state:").append(state);
        sb.append("\n\txmitTime:").append(xmitTime);
        sb.append("\n\theaderTime:").append(headerTime);
        sb.append("\n\ttafText:\"").append(tafText).append("\"");
        sb.append("\n\tstatusMessage:\"").append(statusMessage).append("\"\n");
        return sb.toString();
    }

    public boolean isXml() {
        return xml;
    }

    public void setXml(boolean xml) {
        this.xml = xml;
    }
}
