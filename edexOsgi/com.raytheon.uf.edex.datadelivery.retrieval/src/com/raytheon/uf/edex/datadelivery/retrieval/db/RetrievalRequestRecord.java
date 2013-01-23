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
package com.raytheon.uf.edex.datadelivery.retrieval.db;

import java.io.Serializable;
import java.util.Date;

import javax.persistence.Column;
import javax.persistence.EmbeddedId;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.Table;

import com.raytheon.uf.common.datadelivery.registry.Network;
import com.raytheon.uf.common.datadelivery.retrieval.xml.Retrieval.SubscriptionType;
import com.raytheon.uf.common.dataplugin.persist.IPersistableDataObject;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Retrieval Request Record
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 09, 2012            rjpeter     Initial creation
 * Oct 10, 2012 0726       djohnson    Add {@link #subRetrievalKey}.
 * Nov 26, 2012 1340       dhladky     Added additional fields for tracking subscriptions
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */
@Entity
@Table(name = "subscription_retrieval")
@DynamicSerialize
public class RetrievalRequestRecord implements
        IPersistableDataObject<RetrievalRequestRecordPK>, Serializable,
        ISerializableObject {

    // TODO: Need COMPLETED state?
    public enum State {
        PENDING, RUNNING, FAILED, COMPLETED
    };

    private static final long serialVersionUID = 1L;

    @EmbeddedId
    @DynamicSerializeElement
    private RetrievalRequestRecordPK id;

    @DynamicSerializeElement
    @Column(nullable = false)
    @Enumerated(EnumType.STRING)
    private State state;

    @Column(nullable = false)
    @DynamicSerializeElement
    private int priority = Integer.MAX_VALUE;

    @Column(nullable = false)
    @DynamicSerializeElement
    private String owner;

    @Column(nullable = false)
    @DynamicSerializeElement
    private String plugin;

    @Column(nullable = false)
    @DynamicSerializeElement
    private SubscriptionType subscriptionType;

    @Column(nullable = false)
    @DynamicSerializeElement
    private Network network;

    @Column(nullable = false)
    @DynamicSerializeElement
    private String provider;

    @Column(nullable = false)
    @DynamicSerializeElement
    private Date insertTime;

    @Column(nullable = false)
    @DynamicSerializeElement
    private byte[] retrieval;

    @Column(nullable = false)
    @DynamicSerializeElement
    private Long subRetrievalKey;

    public RetrievalRequestRecord() {
    }

    public RetrievalRequestRecord(String subscriptionName, int index,
            Long subRetrievalKey) {
        id = new RetrievalRequestRecordPK(subscriptionName, index);
        this.subRetrievalKey = subRetrievalKey;
    }

    public RetrievalRequestRecordPK getId() {
        return id;
    }

    @Override
    public RetrievalRequestRecordPK getIdentifier() {
        return id;
    }

    public Date getInsertTime() {
        return insertTime;
    }

    public String getOwner() {
        return owner;
    }

    public String getPlugin() {
        return plugin;
    }

    public int getPriority() {
        return priority;
    }

    public byte[] getRetrieval() {
        return retrieval;
    }

    public State getState() {
        return state;
    }

    public Long getSubRetrievalKey() {
        return subRetrievalKey;
    }

    public void setId(RetrievalRequestRecordPK id) {
        this.id = id;
    }

    public void setInsertTime(Date insertTime) {
        this.insertTime = insertTime;
    }

    public void setOwner(String owner) {
        this.owner = owner;
    }

    public void setPlugin(String plugin) {
        this.plugin = plugin;
    }

    public void setPriority(int priority) {
        this.priority = priority;
    }

    public void setRetrieval(byte[] retrieval) {
        this.retrieval = retrieval;
    }

    public void setState(State state) {
        this.state = state;
    }

    public void setSubRetrievalKey(Long subRetrievalKey) {
        this.subRetrievalKey = subRetrievalKey;
    }

    public void setSubscriptionType(SubscriptionType subscriptionType) {
        this.subscriptionType = subscriptionType;
    }

    public SubscriptionType getSubscriptionType() {
        return subscriptionType;
    }

    public void setNetwork(Network network) {
        this.network = network;
    }

    public Network getNetwork() {
        return network;
    }

    public void setProvider(String provider) {
        this.provider = provider;
    }

    public String getProvider() {
        return provider;
    }
}
