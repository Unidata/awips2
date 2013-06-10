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

package com.raytheon.uf.common.dataplugin.gfe;

import java.util.Date;

import javax.persistence.Column;
import javax.persistence.Embedded;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.PrimaryKeyJoinColumn;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;

import org.hibernate.annotations.Index;
import org.hibernate.annotations.Type;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.GFERecord;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.message.WsId;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.common.time.util.TimeUtil;

/**
 * GridDataHistory
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 *                         randerso    Initial creation
 * 02/27/2008   879        rbell       Added clone()
 * 04/18/2008   #875       bphillip    Changed date fields to use java.util.Calendar
 * Feb 15, 2013 1638       mschenke    Moved Util.getUnixTime into TimeUtil
 * 03/28/2013   1949       rjpeter     Normalized database structure.
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */
@Entity
@Table(name = "gfe_gridhistory")
@DynamicSerialize
public class GridDataHistory implements Cloneable {

    private static final long serialVersionUID = 1L;

    /**
     * Auto-generated surrogate key
     */
    @Id
    @SequenceGenerator(name = "GFE_HISTORY_GENERATOR", sequenceName = "gfe_history_seq", allocationSize = 1)
    @GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "GFE_HISTORY_GENERATOR")
    private int id;

    public enum OriginType {
        INITIALIZED("Populated"), TIME_INTERPOLATED("Interpolated"), SCRATCH(
                "Scratch"), CALCULATED("Calculated"), OTHER("Other");

        String display;

        OriginType(String display) {
            this.display = display;
        }

        /*
         * (non-Javadoc)
         * 
         * @see java.lang.Enum#toString()
         */
        @Override
        public String toString() {
            return display;
        }

    };

    @Column
    @Enumerated(value = EnumType.STRING)
    @DynamicSerializeElement
    private OriginType origin;

    // DO NOT LINK TO PARMID TABLE. The ParmId table may be purged out
    // independent of the history of a forecast grid. Need to keep the history
    // of where the grid came from.
    @Column
    @Type(type = "com.raytheon.uf.common.dataplugin.gfe.db.type.ParmIdType")
    @DynamicSerializeElement
    private ParmID originParm;

    @Embedded
    @DynamicSerializeElement
    private TimeRange originTimeRange;

    @Column
    @DynamicSerializeElement
    private Date timeModified;

    @Column
    @Type(type = "com.raytheon.uf.common.dataplugin.gfe.db.type.WsIdType")
    @DynamicSerializeElement
    private WsId whoModified;

    @Column
    @DynamicSerializeElement
    private Date updateTime;

    @Column
    @DynamicSerializeElement
    private Date publishTime;

    @Column
    @DynamicSerializeElement
    private Date lastSentTime;

    /**
     * Used only for hibernate mappings to allow a look up of GridDataHistory by
     * a given parmId/timeRange. Do not set cascade options.
     */
    @ManyToOne(fetch = FetchType.LAZY, optional = false)
    @PrimaryKeyJoinColumn
    @JoinColumn(updatable = false)
    @Index(name = "gfe_gridhistory_history_idx")
    private GFERecord parent;

    /**
     * Default constructor (all fields initialized null)
     */
    public GridDataHistory() {
        origin = OriginType.OTHER;
    }

    /**
     * Initializer constructor
     * 
     * @param originType
     * @param originParmID
     * @param originTimeRange
     * @param timeModified
     * @param whoModified
     */
    public GridDataHistory(OriginType originType, ParmID originParmID,
            TimeRange originTimeRange, Date timeModified, WsId whoModified) {
        this.origin = originType;
        this.originParm = originParmID;
        this.originTimeRange = originTimeRange;
        this.timeModified = timeModified;
        this.whoModified = whoModified;
    }

    /**
     * Initializer constructor
     * 
     * @param originType
     * @param originParmID
     * @param originTimeRange
     */
    public GridDataHistory(OriginType originType, ParmID originParmID,
            TimeRange originTimeRange) {
        this.origin = originType;
        this.originParm = originParmID;
        this.originTimeRange = originTimeRange;
    }

    /**
     * Initializer constructor
     * 
     * @param originType
     * @param originParmID
     * @param originTimeRange
     * @param lastSentTime
     * @param lastStored
     */
    public GridDataHistory(OriginType originType, ParmID originParmID,
            TimeRange originTimeRange, Date lastSent, Date lastStored) {
        this.origin = originType;
        this.originParm = originParmID;
        this.originTimeRange = originTimeRange;
        this.lastSentTime = lastSent;
        this.updateTime = lastStored;
    }

    /**
     * Constructor taking an encoded string
     * 
     * @param codedString
     */
    public GridDataHistory(String codedString) {

        int index = 0;
        String[] codedTokens = codedString.split(" ");

        // analyze origin
        this.origin = OriginType.values()[Integer
                .parseInt(codedTokens[index++])];

        // read origin parmID
        this.originParm = new ParmID(codedTokens[index++]);

        // read origin time range
        this.originTimeRange = new TimeRange(
                Long.parseLong(codedTokens[index++]) * 1000l,
                Long.parseLong(codedTokens[index++]) * 1000l);

        // read modification time
        String modTime = codedTokens[index++];
        if (!modTime.equals("0")) { // modification time exists
            // process modification time and who modified
            this.timeModified = new Date(Long.parseLong(modTime) * 1000l);
            this.whoModified = new WsId(codedTokens[index++]);
        }

        long updateTime = Long.parseLong(codedTokens[index++]) * 1000l;
        long publishTime = Long.parseLong(codedTokens[index++]) * 1000l;
        long lastSentTime = Long.parseLong(codedTokens[index++]) * 1000l;

        // read the update time (allow for none), ignore any errors
        if (updateTime != 0) {
            this.updateTime = new Date(updateTime);
        }

        if (publishTime != 0) {
            this.publishTime = new Date(publishTime);
        }

        // read the last sent time (allow for none), ignore any errors
        if (lastSentTime != 0) {
            this.lastSentTime = new Date(lastSentTime);
        }
    }

    /**
     * @return the lastSentTime
     */
    public Date getLastSentTime() {
        return lastSentTime;
    }

    /**
     * @param lastSentTime
     *            the lastSentTime to set
     */
    public void setLastSentTime(Date lastSentTime) {
        this.lastSentTime = lastSentTime;
    }

    /**
     * @return the origin
     */
    public OriginType getOrigin() {
        return origin;
    }

    /**
     * @param origin
     *            the origin to set
     */
    public void setOrigin(OriginType origin) {
        this.origin = origin;
    }

    /**
     * @return the publishTime
     */
    public Date getPublishTime() {
        return publishTime;
    }

    /**
     * @param publishTime
     *            the publishTime to set
     */
    public void setPublishTime(Date publishTime) {
        this.publishTime = publishTime;
    }

    /**
     * @return the timeModified
     */
    public Date getTimeModified() {
        return timeModified;
    }

    /**
     * @param timeModified
     *            the timeModified to set
     */
    public void setTimeModified(Date timeModified) {
        this.timeModified = timeModified;
    }

    /**
     * @return the updateTime
     */
    public Date getUpdateTime() {
        return updateTime;
    }

    /**
     * @param updateTime
     *            the updateTime to set
     */
    public void setUpdateTime(Date updateTime) {
        this.updateTime = updateTime;
    }

    /**
     * @param whoModified
     *            the whoModified to set
     */
    public void setModified(WsId whoModified) {
        this.whoModified = whoModified;
    }

    public WsId getModified() {
        return this.whoModified;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#clone()
     */
    @Override
    public GridDataHistory clone() throws CloneNotSupportedException {
        GridDataHistory gdh = new GridDataHistory();

        gdh.origin = this.origin;
        gdh.originParm = this.originParm;
        gdh.originTimeRange = this.originTimeRange.clone();
        if (this.timeModified != null) {
            gdh.timeModified = (Date) this.timeModified.clone();
        }
        gdh.whoModified = this.whoModified;
        if (this.updateTime != null) {
            gdh.updateTime = (Date) this.updateTime.clone();
        }
        if (this.publishTime != null) {
            gdh.publishTime = (Date) this.publishTime.clone();
        }
        if (this.lastSentTime != null) {
            gdh.lastSentTime = (Date) this.lastSentTime.clone();
        }

        return gdh;
    }

    /**
     * @return the originParm
     */
    public ParmID getOriginParm() {
        return originParm;
    }

    /**
     * @param originParm
     *            the originParm to set
     */
    public void setOriginParm(ParmID originParm) {
        this.originParm = originParm;
    }

    /**
     * @return the originTimeRange
     */
    public TimeRange getOriginTimeRange() {
        return originTimeRange;
    }

    /**
     * @param originTimeRange
     *            the originTimeRange to set
     */
    public void setOriginTimeRange(TimeRange originTimeRange) {
        this.originTimeRange = originTimeRange;
    }

    /**
     * @return the whoModified
     */
    public WsId getWhoModified() {
        return whoModified;
    }

    /**
     * @param whoModified
     *            the whoModified to set
     */
    public void setWhoModified(WsId whoModified) {
        this.whoModified = whoModified;
    }

    @Override
    public String toString() {
        StringBuffer buffer = new StringBuffer();
        buffer.append("Origin:").append(this.origin).append("\n");
        buffer.append("Origin Parm:").append(this.originParm.toString())
                .append("\n");
        buffer.append("Origin Time Range: ").append(this.originTimeRange)
                .append(" Time Modified: ").append(this.timeModified)
                .append(" Who Modified: ").append(this.whoModified)
                .append("\n");
        buffer.append("Update Time: ").append(this.updateTime).append("\n");
        buffer.append("Publish Time: ").append(this.publishTime).append("\n");
        buffer.append("Last Sent Time: ").append(this.lastSentTime)
                .append("\n");
        return buffer.toString();
    }

    /**
     * Returns the coded string for GridDataHistory.
     * 
     * @return The coded String
     */
    public String getCodedString() {
        StringBuffer buffer = new StringBuffer();

        buffer.append(this.origin.ordinal()).append(" ");
        buffer.append(this.originParm.toString()).append(" ");
        buffer.append(TimeUtil.getUnixTime(this.originTimeRange.getStart()))
                .append(" ");
        buffer.append(TimeUtil.getUnixTime(this.originTimeRange.getEnd())).append(
                " ");

        if (this.timeModified == null) {
            buffer.append("0");
        } else if (TimeUtil.getUnixTime(this.timeModified) == 0) {
            buffer.append("0");
        } else {
            buffer.append(TimeUtil.getUnixTime(this.timeModified)).append(" ");
            buffer.append(this.whoModified);
        }
        if (this.updateTime == null) {
            buffer.append(" 0");
        } else {
            buffer.append(" ").append(TimeUtil.getUnixTime(this.updateTime));
        }
        if (this.publishTime == null) {
            buffer.append(" 0");
        } else {
            buffer.append(" ").append(TimeUtil.getUnixTime(this.publishTime));
        }
        if (this.lastSentTime == null) {
            buffer.append(" 0");
        } else {
            buffer.append(" ").append(TimeUtil.getUnixTime(this.lastSentTime));
        }

        return buffer.toString();
    }

    /**
     * @return the key
     */
    public int getId() {
        return id;
    }

    /**
     * @param key
     *            the key to set
     */
    public void setId(int id) {
        this.id = id;
    }

    public void replaceValues(GridDataHistory replacement) {
        this.setLastSentTime(replacement.getLastSentTime());
        this.setWhoModified(replacement.getWhoModified());
        this.setOrigin(replacement.getOrigin());
        this.setOriginParm(replacement.getOriginParm());
        this.setOriginTimeRange(replacement.getOriginTimeRange());
        this.setPublishTime(replacement.getPublishTime());
        this.setTimeModified(replacement.getTimeModified());
        this.setUpdateTime(replacement.getUpdateTime());
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#hashCode()
     */
    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result
                + ((lastSentTime == null) ? 0 : lastSentTime.hashCode());
        result = prime * result + ((origin == null) ? 0 : origin.hashCode());
        result = prime * result
                + ((originParm == null) ? 0 : originParm.hashCode());
        result = prime * result
                + ((originTimeRange == null) ? 0 : originTimeRange.hashCode());
        result = prime * result
                + ((publishTime == null) ? 0 : publishTime.hashCode());
        result = prime * result
                + ((timeModified == null) ? 0 : timeModified.hashCode());
        result = prime * result
                + ((updateTime == null) ? 0 : updateTime.hashCode());
        result = prime * result
                + ((whoModified == null) ? 0 : whoModified.hashCode());
        return result;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        GridDataHistory other = (GridDataHistory) obj;
        if (lastSentTime == null) {
            if (other.lastSentTime != null) {
                return false;
            }
        } else if (!lastSentTime.equals(other.lastSentTime)) {
            return false;
        }
        if (origin != other.origin) {
            return false;
        }
        if (originParm == null) {
            if (other.originParm != null) {
                return false;
            }
        } else if (!originParm.equals(other.originParm)) {
            return false;
        }
        if (originTimeRange == null) {
            if (other.originTimeRange != null) {
                return false;
            }
        } else if (!originTimeRange.equals(other.originTimeRange)) {
            return false;
        }
        if (publishTime == null) {
            if (other.publishTime != null) {
                return false;
            }
        } else if (!publishTime.equals(other.publishTime)) {
            return false;
        }
        if (timeModified == null) {
            if (other.timeModified != null) {
                return false;
            }
        } else if (!timeModified.equals(other.timeModified)) {
            return false;
        }
        if (updateTime == null) {
            if (other.updateTime != null) {
                return false;
            }
        } else if (!updateTime.equals(other.updateTime)) {
            return false;
        }
        if (whoModified == null) {
            if (other.whoModified != null) {
                return false;
            }
        } else if (!whoModified.equals(other.whoModified)) {
            return false;
        }
        return true;
    }

    /**
     * @return the parent
     */
    public GFERecord getParent() {
        return parent;
    }

    /**
     * @param parent
     *            the parent to set
     */
    public void setParent(GFERecord parent) {
        this.parent = parent;
    }
}
