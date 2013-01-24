package com.raytheon.uf.edex.datadelivery.bandwidth.dao;

import java.io.Serializable;
import java.util.Calendar;

import javax.persistence.Column;
import javax.persistence.DiscriminatorColumn;
import javax.persistence.DiscriminatorType;
import javax.persistence.DiscriminatorValue;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Inheritance;
import javax.persistence.InheritanceType;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;

import com.raytheon.uf.common.datadelivery.registry.Network;
import com.raytheon.uf.common.dataplugin.persist.IPersistableDataObject;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.edex.datadelivery.bandwidth.retrieval.RetrievalStatus;
import com.raytheon.uf.edex.datadelivery.bandwidth.util.BandwidthUtil;

/**
 * 
 * A bandwidth allocation.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 12, 2012 0726       djohnson     Add SW history, use string version of enum.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
@Entity
@Table(name = "bandwidth_allocation")
@Inheritance(strategy = InheritanceType.SINGLE_TABLE)
@DiscriminatorColumn(name = "dtype", discriminatorType = DiscriminatorType.STRING)
@DiscriminatorValue("BandwidthAllocation")
@DynamicSerialize
@SequenceGenerator(name = "BANDWIDTH_SEQ", sequenceName = "bandwidth_seq", allocationSize = 1)
public class BandwidthAllocation implements IPersistableDataObject<Long>,
        ISerializableObject, Serializable {

    private static final long serialVersionUID = 743702044231376839L;

    @Column(nullable = true)
    @DynamicSerializeElement
    private Calendar actualEnd;

    @Column(nullable = true)
    @DynamicSerializeElement
    private Calendar actualStart;

    @Column(nullable = false)
    @DynamicSerializeElement
    private String agentType;

    @Column(nullable = true)
    @DynamicSerializeElement
    private long bandwidthBucket;

    @Column(nullable = false)
    @DynamicSerializeElement
    private Calendar endTime;

    @Column(nullable = false)
    @DynamicSerializeElement
    private long estimatedSize;

    @Id
    @Column(name = "identifier")
    @GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "BANDWIDTH_SEQ")
    @DynamicSerializeElement
    private long id = BandwidthUtil.DEFAULT_IDENTIFIER;

    @Column(nullable = false)
    @DynamicSerializeElement
    private double priority;

    @Column(nullable = false)
    @Enumerated(EnumType.STRING)
    @DynamicSerializeElement
    private Network network;

    @Column(nullable = false)
    @DynamicSerializeElement
    private Calendar startTime;

    @Column(nullable = true)
    @DynamicSerializeElement
    @Enumerated(EnumType.STRING)
    private RetrievalStatus status;

    public BandwidthAllocation() {
        // Constructor for bean interface.
    }

    public BandwidthAllocation(BandwidthAllocation request, long size) {
        this.id = request.getId();
        this.startTime = request.getStartTime();
        this.endTime = request.getEndTime();
        this.priority = request.getPriority();
        this.network = request.getNetwork();
        this.estimatedSize = size;
        this.bandwidthBucket = request.getBandwidthBucket();
    }

    /**
     * @return the actualEnd
     */
    public Calendar getActualEnd() {
        return actualEnd;
    }

    /**
     * @return the actualStart
     */
    public Calendar getActualStart() {
        return actualStart;
    }

    /**
     * @return the agentType
     */
    public String getAgentType() {
        return agentType;
    }

    public long getBandwidthBucket() {
        return bandwidthBucket;
    }

    public Calendar getEndTime() {
        return this.endTime;
    }

    /**
     * Get the estimated size in kilobytes (kB).
     * 
     * @return
     */
    public long getEstimatedSize() {
        return this.estimatedSize;
    }

    public long getEstimatedSizeInBytes() {
        return getEstimatedSize() * BandwidthUtil.BYTES_PER_KILOBYTE;
    }

    public long getId() {
        return id;
    }

    @Override
    public Long getIdentifier() {
        return Long.valueOf(id);
    }

    public double getPriority() {
        return priority;
    }

    public Network getNetwork() {
        return network;
    }

    public Calendar getStartTime() {
        return startTime;
    }

    public RetrievalStatus getStatus() {
        return status;
    }

    /**
     * @param actualEnd
     *            the actualEnd to set
     */
    public void setActualEnd(Calendar actualEnd) {
        this.actualEnd = actualEnd;
    }

    /**
     * @param actualStart
     *            the actualStart to set
     */
    public void setActualStart(Calendar actualStart) {
        this.actualStart = actualStart;
    }

    /**
     * @param agentType
     *            the agentType to set
     */
    public void setAgentType(String agentType) {
        this.agentType = agentType;
    }

    public void setBandwidthBucket(long bandwidthBucket) {
        this.bandwidthBucket = bandwidthBucket;
    }

    public void setEndTime(Calendar endTime) {
        this.endTime = endTime;
    }

    /**
     * Set the estimated size in kilobytes (kB).
     * 
     * @param estimatedSize
     *            the estimated size
     */
    public void setEstimatedSize(long estimatedSize) {
        this.estimatedSize = estimatedSize;
    }

    public void setId(long id) {
        this.id = id;
    }

    /**
     * @param id
     *            the id to set
     */
    public void setIdentifier(Long identifier) {
        setId(identifier.longValue());
    }

    public void setPriority(double priority) {
        this.priority = priority;
    }

    public void setNetwork(Network network) {
        this.network = network;
    }

    public void setStartTime(Calendar startTime) {
        this.startTime = startTime;
    }

    public void setStatus(RetrievalStatus status) {
        this.status = status;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + (int) (id ^ (id >>> 32));
        return result;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        BandwidthAllocation other = (BandwidthAllocation) obj;
        if (id != other.id)
            return false;
        return true;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("allocation id: [").append(getId()).append("] ");
        sb.append("path [").append(getNetwork()).append("] ");
        sb.append("priority [").append(getPriority()).append("] ");
        sb.append("size (bytes) [").append(getEstimatedSizeInBytes())
                .append("] ");
        sb.append("status [").append(getStatus()).append("] ");
        sb.append("startTime [").append(BandwidthUtil.format(getStartTime()))
                .append("] ");
        sb.append("endTime [").append(BandwidthUtil.format(getEndTime()))
                .append("]");
        return sb.toString();
    }

}