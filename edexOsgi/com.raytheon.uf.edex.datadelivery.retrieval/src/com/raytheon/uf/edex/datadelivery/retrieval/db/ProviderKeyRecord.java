package com.raytheon.uf.edex.datadelivery.retrieval.db;

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
import com.raytheon.uf.common.serialization.ISerializableObject;
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
 * Jul 13, 2012 2180       dhladky     Provider Key storage
 * Aug 08, 2013 2180       mpduff      Added default constructor for serialization
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */
@Entity
@Table(name = "dataDeliveryProviderKey")
@DynamicSerialize
public class ProviderKeyRecord implements IPersistableDataObject<String>,
        Serializable, ISerializableObject {

    private static final long serialVersionUID = 177884683888461814L;

    @Id
    @DynamicSerializeElement
    private String providerName;

    @Column(nullable = false)
    @DynamicSerializeElement
    private String providerKey;

    public ProviderKeyRecord() {

    }

    public ProviderKeyRecord(String providerName, String providerKey) {
        this.providerName = providerName;
        this.providerKey = providerKey;
    }

    public String getProviderKey() {
        return providerKey;
    }

    public void setProviderKey(String providerKey) {
        this.providerKey = providerKey;
    }

    @Override
    public String getIdentifier() {
        return providerName;
    }

    public String getProviderName() {
        return providerName;
    }

    public void setProviderName(String providerName) {
        this.providerName = providerName;
    }

}
