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
package com.raytheon.uf.common.dataplugin.text.request;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.serialization.comm.IServerRequest;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 4, 2011            rferrel     Initial creation
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */
@DynamicSerialize
public class ExecuteAwipsCmdRequest implements IServerRequest {

    @DynamicSerializeElement
    private String nnnXxx;

    @DynamicSerializeElement
    private String wmoId = null;

    @DynamicSerializeElement
    private String site = null;

    @DynamicSerializeElement
    private String lastHrs = null;

    @DynamicSerializeElement
    private String hdrTime = null;

    @DynamicSerializeElement
    private String bbb = null;

    @DynamicSerializeElement
    private boolean fullDataRead;

    @DynamicSerializeElement
    private boolean operationalMode;

    public String getLastHrs() {
        return lastHrs;
    }

    public void setLastHrs(String lastHrs) {
        this.lastHrs = lastHrs;
    }

    public boolean isFullDataRead() {
        return fullDataRead;
    }

    public void setFullDataRead(boolean fullDataRead) {
        this.fullDataRead = fullDataRead;
    }

    public String getNnnXxx() {
        return this.nnnXxx;
    }

    public void setNnnXxx(String nnnXxx) {
        this.nnnXxx = nnnXxx;
    }

    public String getWmoId() {
        return wmoId;
    }

    public void setWmoId(String wmoId) {
        this.wmoId = wmoId;
    }

    public String getSite() {
        return this.site;
    }

    public void setSite(String site) {
        this.site = site;
    }

    public String getHdrTime() {
        return this.hdrTime;
    }

    public void setHdrTime(String hdrTime) {
        this.hdrTime = hdrTime;
    }

    public String getBbb() {
        return bbb;
    }

    public void setBbb(String bbb) {
        this.bbb = bbb;
    }

    public boolean isOperationalMode() {
        return this.operationalMode;
    }

    public void setOperationalMode(boolean operationalMode) {
        this.operationalMode = operationalMode;
    }
}
