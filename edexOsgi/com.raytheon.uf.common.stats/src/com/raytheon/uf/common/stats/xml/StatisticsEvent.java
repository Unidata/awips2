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
package com.raytheon.uf.common.stats.xml;

import java.lang.reflect.Method;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.event.Event;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Statistics Configuration Event xml element.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 6, 2012    728      mpduff      Initial creation.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */
@DynamicSerialize
@XmlRootElement(name = "event")
@XmlAccessorType(XmlAccessType.NONE)
public class StatisticsEvent {

    @XmlAttribute
    @DynamicSerializeElement
    private String type;

    @XmlAttribute
    @DynamicSerializeElement
    private String displayName;

    @XmlAttribute
    @DynamicSerializeElement
    private String category;

    @XmlElements({ @XmlElement(name = "statisticsGroup", type = StatisticsGroup.class) })
    @DynamicSerializeElement
    private List<StatisticsGroup> groupList;

    @XmlElements({ @XmlElement(name = "statisticsAggregate", type = StatisticsAggregate.class) })
    @DynamicSerializeElement
    private List<StatisticsAggregate> aggregateList;

    private Class<? extends Event> typeClass = null;

    private List<Method> groupByMethods = null;

    private List<Method> aggregateMethods = null;

    /**
     * @return the aggregateList
     */
    public List<StatisticsAggregate> getAggregateList() {
        return aggregateList;
    }

    /**
     * @return the category
     */
    public String getCategory() {
        return category;
    }

    /**
     * @return the displayName
     */
    public String getDisplayName() {
        return displayName;
    }

    /**
     * @return the groupList
     */
    public List<StatisticsGroup> getGroupList() {
        return groupList;
    }

    /**
     * @return the type
     */
    public String getType() {
        return type;
    }

    /**
     * @param aggregateList
     *            the aggregateList to set
     */
    public void setAggregateList(List<StatisticsAggregate> aggregateList) {
        this.aggregateList = aggregateList;
    }

    /**
     * @param category
     *            the category to set
     */
    public void setCategory(String category) {
        this.category = category;
    }

    /**
     * @param displayName
     *            the displayName to set
     */
    public void setDisplayName(String displayName) {
        this.displayName = displayName;
    }

    /**
     * @param groupList
     *            the groupList to set
     */
    public void setGroupList(List<StatisticsGroup> groupList) {
        this.groupList = groupList;
    }

    /**
     * @param type
     *            the type to set
     */
    public void setType(String type) {
        this.type = type;
    }

    public Class<? extends Event> getTypeClass() {
        return typeClass;
    }

    public void setTypeClass(Class<? extends Event> typeClass) {
        this.typeClass = typeClass;
    }

    public List<Method> getGroupByMethods() {
        return groupByMethods;
    }

    public void setGroupByMethods(List<Method> groupByMethods) {
        this.groupByMethods = groupByMethods;
    }

    public List<Method> getAggregateMethods() {
        return aggregateMethods;
    }

    public void setAggregateMethods(List<Method> aggregateMethods) {
        this.aggregateMethods = aggregateMethods;
    }

}
