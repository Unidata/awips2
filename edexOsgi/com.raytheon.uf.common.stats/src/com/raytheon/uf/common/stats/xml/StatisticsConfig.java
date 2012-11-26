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

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Statistical Configuration File.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 6, 2012      728    mpduff      Initial creation.
 *
 * </pre>
 *
 * @author mpduff
 * @version 1.0
 */
@DynamicSerialize
@XmlRootElement(name = "statisticsConfig")
@XmlAccessorType(XmlAccessType.NONE)
public class StatisticsConfig implements ISerializableObject {
    @XmlElements({ @XmlElement(name = "statisticsEvent", type = StatisticsEvent.class) })
    @DynamicSerializeElement
    private List<StatisticsEvent> events;

    /**
     * @return the events
     */
    public List<StatisticsEvent> getEvents() {
        return events;
    }

    /**
     * @param events
     *            the events to set
     */
    public void setEvents(List<StatisticsEvent> events) {
        this.events = events;
    }

    /**
     * Return the list of categories in this config file.
     *
     * @return List<String> of categories
     */
    public List<String> getCategories() {
        Set<String> categories = new HashSet<String>();
        if (events != null && events.size() > 0) {
            for (StatisticsEvent event : events) {
                categories.add(event.getCategory());
            }
        }

        return new ArrayList<String>(categories);
    }
}
