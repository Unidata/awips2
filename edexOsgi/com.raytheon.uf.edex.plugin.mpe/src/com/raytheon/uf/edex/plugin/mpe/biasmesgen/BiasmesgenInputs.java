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
package com.raytheon.uf.edex.plugin.mpe.biasmesgen;

import java.nio.file.Path;
import java.util.Collection;
import java.util.Collections;
import java.util.Map;
import java.util.List;
import java.util.ArrayList;

import com.raytheon.uf.common.dataplugin.shef.tables.Radarloc;
import com.raytheon.uf.common.dataplugin.shef.tables.Radarresp;
import com.raytheon.uf.common.dataplugin.shef.tables.Rwbiasstat;

/**
 * POJO to store inputs gathered by {@link Biasmesgen}. Presently, everything is
 * local to an execute method. However, at a later time everything may be
 * pipelined through Camel to allow certain processes to trigger others at any
 * stage of execution. Additionally, the existence of this class and others like
 * it will allow for the segmentation of specific functionality in the new
 * implementation.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 13, 2016 5576       bkowal      Initial creation
 * May 20, 2016 5576       bkowal      Added {@link #memorySpanList} and
 *                                     {@link #radIdLocMap}.
 * 
 * </pre>
 * 
 * @author bkowal
 */

public class BiasmesgenInputs {

    public static final int MEMORY_SPAN_COUNT = 10;

    private String siteId;

    private Short siteIdNumber;

    private Path biasOutputRoot;

    private String fxaLocalSite;

    private Boolean sendLocalBias;

    private Rwbiasstat rwbiasstat;

    /**
     * 10 Memory Span values are defined in a {@link Rwbiasstat} record.
     */
    private final List<Float> memorySpanList = new ArrayList<>(
            MEMORY_SPAN_COUNT);

    private Collection<Radarresp> radarrespCollection;

    private Map<String, Radarloc> radIdLocMap = Collections.emptyMap();

    /**
     * Determines whether or not it will actually be possible to generate output
     * based on the inputs that have been provided. It is important to note that
     * this method will only be checked on successful input retrieval. Any
     * failed input retrieval attempts will generate an Exception that will need
     * to be handled by the driver class.
     * 
     * @return {@code true}, if output can be generated; {code false},
     *         otherwise.
     */
    public boolean generationPossible() {
        return (radarrespCollection != null && !radarrespCollection.isEmpty());
    }

    public String getSiteId() {
        return siteId;
    }

    public void setSiteId(String siteId) {
        this.siteId = siteId;
    }

    public Short getSiteIdNumber() {
        return siteIdNumber;
    }

    public void setSiteIdNumber(Short siteIdNumber) {
        this.siteIdNumber = siteIdNumber;
    }

    public Path getBiasOutputRoot() {
        return biasOutputRoot;
    }

    public void setBiasOutputRoot(Path biasOutputRoot) {
        this.biasOutputRoot = biasOutputRoot;
    }

    public String getFxaLocalSite() {
        return fxaLocalSite;
    }

    public void setFxaLocalSite(String fxaLocalSite) {
        this.fxaLocalSite = fxaLocalSite;
    }

    public Boolean getSendLocalBias() {
        return sendLocalBias;
    }

    public void setSendLocalBias(Boolean sendLocalBias) {
        this.sendLocalBias = sendLocalBias;
    }

    public Rwbiasstat getRwbiasstat() {
        return rwbiasstat;
    }

    public void setRwbiasstat(Rwbiasstat rwbiasstat) {
        this.rwbiasstat = rwbiasstat;
        /*
         * Populate the memory span list for future access.
         */
        memorySpanList.clear();
        if (rwbiasstat == null) {
            return;
        }
        memorySpanList.add(rwbiasstat.getMemSpan1());
        memorySpanList.add(rwbiasstat.getMemSpan2());
        memorySpanList.add(rwbiasstat.getMemSpan3());
        memorySpanList.add(rwbiasstat.getMemSpan4());
        memorySpanList.add(rwbiasstat.getMemSpan5());
        memorySpanList.add(rwbiasstat.getMemSpan6());
        memorySpanList.add(rwbiasstat.getMemSpan7());
        memorySpanList.add(rwbiasstat.getMemSpan8());
        memorySpanList.add(rwbiasstat.getMemSpan9());
        memorySpanList.add(rwbiasstat.getMemSpan10());
    }

    public List<Float> getMemorySpanList() {
        return memorySpanList;
    }

    public Collection<Radarresp> getRadarrespCollection() {
        return radarrespCollection;
    }

    public void setRadarrespCollection(Collection<Radarresp> radarrespCollection) {
        if (radarrespCollection == null) {
            throw new IllegalArgumentException(
                    "Required argument 'radarrespCollection' cannot be NULL.");
        }
        this.radarrespCollection = radarrespCollection;
    }

    public Map<String, Radarloc> getRadIdLocMap() {
        return radIdLocMap;
    }

    public void setRadIdLocMap(Map<String, Radarloc> radIdLocMap) {
        this.radIdLocMap = radIdLocMap;
    }
}