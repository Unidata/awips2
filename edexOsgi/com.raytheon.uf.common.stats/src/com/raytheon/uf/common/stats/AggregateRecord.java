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
package com.raytheon.uf.common.stats;

import java.util.Calendar;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Table;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.dataplugin.persist.PersistableDataObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Record class for an aggregate result.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 21, 2012            jsanchez     Initial creation
 * Nov 12, 2012            dhladky      Updates some things for stats
 * Jan 15, 2013 1487       djohnson     Increase length of grouping to 1024.
 * 
 * </pre>
 * 
 * @author jsanchez
 * @version 1.0
 */
@Entity
@Table(name = "aggregate", schema = "events")
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class AggregateRecord extends PersistableDataObject<Integer> {
    private static final long serialVersionUID = -4553588456131256014L;

    @GeneratedValue(strategy = GenerationType.AUTO)
    @Id
    @DynamicSerializeElement
    private Integer id;

    @Column(nullable = false)
    @DynamicSerializeElement
    private Calendar startDate;

    @Column(nullable = false)
    @DynamicSerializeElement
    private Calendar endDate;

    @Column(nullable = false)
    @DynamicSerializeElement
    private String eventType;

    @DynamicSerializeElement
    @Column(length = 1024)
    private String grouping;

    @Column(nullable = false)
    @DynamicSerializeElement
    private String field;

    @DynamicSerializeElement
    private double max;

    @DynamicSerializeElement
    private double min;

    @DynamicSerializeElement
    private double sum;

    @DynamicSerializeElement
    private double count;

    public AggregateRecord() {

    }

    public AggregateRecord(String eventType, Calendar startDate,
            Calendar endDate, String groupings, String field) {
        this.eventType = eventType;
        this.startDate = startDate;
        this.endDate = endDate;
        grouping = groupings;
        this.field = field;
    }

    public double getCount() {
        return count;
    }

    public Calendar getEndDate() {
        return endDate;
    }

    public String getEventType() {
        return eventType;
    }

    public String getField() {
        return field;
    }

    public String getGrouping() {
        return grouping;
    }

    public Integer getId() {
        return id;
    }

    public double getMax() {
        return max;
    }

    public double getMin() {
        return min;
    }

    public Calendar getStartDate() {
        return startDate;
    }

    public double getSum() {
        return sum;
    }

    public void setCount(double count) {
        this.count = count;
    }

    public void setEndDate(Calendar endDate) {
        this.endDate = endDate;
    }

    public void setEventType(String eventType) {
        this.eventType = eventType;
    }

    public void setField(String field) {
        this.field = field;
    }

    public void setGrouping(String grouping) {
        this.grouping = grouping;
    }

    public void setId(Integer id) {
        this.id = id;
    }

    public void setMax(double max) {
        this.max = max;
    }

    public void setMin(double min) {
        this.min = min;
    }

    public void setStartDate(Calendar startDate) {
        this.startDate = startDate;
    }

    public void setSum(double sum) {
        this.sum = sum;
    }

    /* (non-Javadoc)
     * @see java.lang.Object#hashCode()
     */
    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        long temp;
        temp = Double.doubleToLongBits(count);
        result = prime * result + (int) (temp ^ (temp >>> 32));
        result = prime * result + ((endDate == null) ? 0 : endDate.hashCode());
        result = prime * result
                + ((eventType == null) ? 0 : eventType.hashCode());
        result = prime * result + ((field == null) ? 0 : field.hashCode());
        result = prime * result
                + ((grouping == null) ? 0 : grouping.hashCode());
        temp = Double.doubleToLongBits(max);
        result = prime * result + (int) (temp ^ (temp >>> 32));
        temp = Double.doubleToLongBits(min);
        result = prime * result + (int) (temp ^ (temp >>> 32));
        result = prime * result
                + ((startDate == null) ? 0 : startDate.hashCode());
        temp = Double.doubleToLongBits(sum);
        result = prime * result + (int) (temp ^ (temp >>> 32));
        return result;
    }

    /* (non-Javadoc)
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (!super.equals(obj)) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        AggregateRecord other = (AggregateRecord) obj;
        if (Double.doubleToLongBits(count) != Double
                .doubleToLongBits(other.count)) {
            return false;
        }
        if (endDate == null) {
            if (other.endDate != null) {
                return false;
            }
        } else if (!endDate.equals(other.endDate)) {
            return false;
        }
        if (eventType == null) {
            if (other.eventType != null) {
                return false;
            }
        } else if (!eventType.equals(other.eventType)) {
            return false;
        }
        if (field == null) {
            if (other.field != null) {
                return false;
            }
        } else if (!field.equals(other.field)) {
            return false;
        }
        if (grouping == null) {
            if (other.grouping != null) {
                return false;
            }
        } else if (!grouping.equals(other.grouping)) {
            return false;
        }
        if (Double.doubleToLongBits(max) != Double.doubleToLongBits(other.max)) {
            return false;
        }
        if (Double.doubleToLongBits(min) != Double.doubleToLongBits(other.min)) {
            return false;
        }
        if (startDate == null) {
            if (other.startDate != null) {
                return false;
            }
        } else if (!startDate.equals(other.startDate)) {
            return false;
        }
        if (Double.doubleToLongBits(sum) != Double.doubleToLongBits(other.sum)) {
            return false;
        }
        return true;
    }
}
