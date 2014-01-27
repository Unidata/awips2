package com.raytheon.uf.edex.datadelivery.bandwidth.registry;

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

import java.io.Serializable;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

import com.raytheon.uf.common.dataplugin.persist.IPersistableDataObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Provider Key Record
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 15, 2013 1736       dhladky     Store bandwidth entries
 *
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */
@Entity
@Table(name = "dataDeliveryRegistryBandwidth")
@DynamicSerialize
public class RegistryBandwidthRecord implements IPersistableDataObject<Long>,
        Serializable {

    private static final long serialVersionUID = 177884683888461814L;

    @Id
    @DynamicSerializeElement
    private Long timePeriod;

    @Column(nullable = false)
    @DynamicSerializeElement
    private int bytes;

    public RegistryBandwidthRecord() {

    }

    public RegistryBandwidthRecord(long timePeriod, int bytes) {
        this.timePeriod = timePeriod;
        this.bytes = bytes;
    }

    public int getBytes() {
        return bytes;
    }

    public void setBytes(int bytes) {
        this.bytes = bytes;
    }

    @Override
    public Long getIdentifier() {
        return timePeriod;
    }

    public Long getTimePeriod() {
        return timePeriod;
    }

    public void setTimePeriod(long timePeriod) {
        this.timePeriod = timePeriod;
    }

}
