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
package com.raytheon.uf.edex.stats.xml;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;

/**
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 21, 2012            jsanchez     Made serializable.
 * 
 * </pre>
 * 
 * @author jsanchez
 * 
 */
@XmlRootElement(name = "statistics")
@XmlAccessorType(XmlAccessType.NONE)
public class Statistics implements ISerializableObject {

    /** the start time of the period YYYY-MM-DD HH:MM:SS */
    @XmlAttribute
    private String start;

    /** the end time of the period YYYY-MM-DD HH:MM:SS */
    @XmlAttribute
    private String end;

    /** the event type that will be monitored */
    @XmlElement(name = "eventType")
    private String eventType;

    @XmlElement(name = "groupBy")
    private GroupBy groupBy;

    @XmlElement(name = "aggregate")
    private Aggregate[] aggregates;

    /**
     * default constructor
     */
    public Statistics() {

    }

    public Statistics(StatsConfig config) {
        this.eventType = config.getEventType();
        this.groupBy = config.getGroupBy();
        this.aggregates = config.getAggregates();
    }

    public String getEventType() {
        return eventType;
    }

    public void setEventType(String eventType) {
        this.eventType = eventType;
    }

    public GroupBy getGroupBy() {
        return groupBy;
    }

    public void setGroupBy(GroupBy groupBy) {
        this.groupBy = groupBy;
    }

    public Aggregate[] getAggregates() {
        return aggregates;
    }

    public void setAggregates(Aggregate[] aggregates) {
        this.aggregates = aggregates;
    }

    public String getStart() {
        return start;
    }

    public void setStart(String start) {
        this.start = start;
    }

    public String getEnd() {
        return end;
    }

    public void setEnd(String end) {
        this.end = end;
    }
}
