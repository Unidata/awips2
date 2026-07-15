/**
 * This software was developed and / or modified by NOAA/NWS/OCP/ASDT
 **/
package com.raytheon.edex.plugin.gfe.isc;

import java.io.IOException;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import jakarta.persistence.CollectionTable;
import jakarta.persistence.Column;
import jakarta.persistence.ElementCollection;
import jakarta.persistence.Entity;
import jakarta.persistence.FetchType;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.SequenceGenerator;
import jakarta.persistence.Table;

import org.hibernate.annotations.ForeignKey;
import org.hibernate.annotations.Index;

import com.fasterxml.jackson.databind.ObjectMapper;

/**
 * Record for storing a job managed by {@link ISCMosaicJobManager}. Initially,
 * the job is stored as a single record with {@code prepared=false}. After
 * preparation, {@code preparation=true} and there may be one or more records
 * related by the same {@code leader} which is set to the ID of the original,
 * single record.
 *
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 08, 2018 19452      dfriedman    Initial creation
 * Sep 09, 2022 23257      dgilling     Implement toString.
 *
 * </pre>
 *
 * @author dfriedman
 */
@Entity
@Table(name = "iscmosaicjob")
@SequenceGenerator(initialValue = 1, name = IscMosaicJobRecord.ID_GEN, sequenceName = "iscmosaicjobseq")
public class IscMosaicJobRecord {

    public static final String ID_GEN = "idgen";

    @Id
    @GeneratedValue(strategy = GenerationType.SEQUENCE, generator = ID_GEN)
    private int id;

    /**
     * ID of the job record representing an overall request. This groups
     * together job records for various input file/argument combinations of the
     * request.
     */
    @Column(nullable = false)
    @Index(name = "iscmosaicjob_leader")
    private int leader;

    @Column(length = 4, nullable = false)
    private String site;

    /** Args passed to the ISC mosaic script in JSON format */
    @Column(columnDefinition = "text")
    private String args;

    @Column(nullable = false)
    @Index(name = "iscmosaicjob_prepared")
    private boolean prepared;

    /**
     * Used to control exclusive access to the job for the preparation and
     * cleanup steps
     */
    @Column(nullable = false)
    private boolean inUse;

    /**
     * Reservation time used with @{code inUse}
     */
    @Column(nullable = false)
    private Date lastUse;

    /**
     * Set of lock names that indicate the set of remaining parms to process.
     * <p>
     * CollectionTable.indexes not supported so the index is created in
     * res/scripts/iscmosaicparm-index.sql.
     */
    @ElementCollection(fetch = FetchType.EAGER)
    @CollectionTable(name = "iscmosaicparm", joinColumns = {
            @JoinColumn(name = "job_id", nullable = false, referencedColumnName = "id") })
    @Column(name = "lockName", nullable = false)
    @ForeignKey(name = "fk_iscmosaicparm_to_iscmosaicjob")
    private Set<String> parms;

    public IscMosaicJobRecord() {

    }

    public int getId() {
        return id;
    }

    public void setId(int id) {
        this.id = id;
    }

    public int getLeader() {
        return leader;
    }

    public void setLeader(int leader) {
        this.leader = leader;
    }

    public String getSite() {
        return site;
    }

    public void setSite(String site) {
        this.site = site;
    }

    public String getArgs() {
        return args;
    }

    public void setArgs(String args) {
        this.args = args;
    }

    public boolean isPrepared() {
        return prepared;
    }

    public void setPrepared(boolean prepared) {
        this.prepared = prepared;
    }

    public boolean isInUse() {
        return inUse;
    }

    public void setInUse(boolean inUse) {
        this.inUse = inUse;
    }

    public Date getLastUse() {
        return lastUse;
    }

    public void setLastUse(Date lastUse) {
        this.lastUse = lastUse;
    }

    public Set<String> getParms() {
        if (parms == null) {
            parms = new HashSet<>();
        }
        return parms;
    }

    public void setParms(Set<String> parms) {
        this.parms = parms;
    }

    public Map<String, Object> getArgsMap() throws IOException {
        return args != null ? new ObjectMapper().readValue(args, HashMap.class)
                : null;
    }

    public void setArgsMap(Map<String, Object> args) throws IOException {
        this.args = args != null ? new ObjectMapper().writeValueAsString(args)
                : null;
    }

    @Override
    public String toString() {
        return "IscMosaicJobRecord [site=" + site + ", args=" + args
                + ", parms=" + parms + "]";
    }
}
