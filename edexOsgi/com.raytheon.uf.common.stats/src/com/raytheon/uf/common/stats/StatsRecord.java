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
 * Record class for stats waiting to be stored in the appropriate bucket.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 21, 2012            jsanchez     Initial creation
 * 
 * </pre>
 * 
 * @author jsanchez
 * 
 */
@Entity
@Table(name = "stats", schema = "events")
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class StatsRecord extends PersistableDataObject {
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Id
    @DynamicSerializeElement
    private Integer id;

    @Column(nullable = false)
    @DynamicSerializeElement
    private Calendar date;

    @Column(nullable = false)
    @DynamicSerializeElement
    private String eventType;

    @Column(nullable = false)
    @DynamicSerializeElement
    private byte[] event;

    public Calendar getDate() {
        return date;
    }

    public byte[] getEvent() {
        return event;
    }

    public String getEventType() {
        return eventType;
    }

    public Integer getId() {
        return id;
    }

    public void setDate(Calendar date) {
        this.date = date;
    }

    public void setEvent(byte[] event) {
        this.event = event;
    }

    public void setEventType(String eventType) {
        this.eventType = eventType;
    }

    public void setId(Integer id) {
        this.id = id;
    }

    @Override
    public String toString() {

        StringBuilder sb = new StringBuilder();
        sb.append(getEventType() + " ");
        sb.append(getDate() + " ");
        sb.append(getId() + " ");
        return sb.toString();

    }

}
