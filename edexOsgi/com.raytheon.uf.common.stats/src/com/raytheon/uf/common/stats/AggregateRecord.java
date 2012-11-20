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
public class AggregateRecord extends PersistableDataObject {

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

}
