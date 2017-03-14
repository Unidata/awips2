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
package com.raytheon.viz.aviation.monitor;

import java.util.Map;

import com.raytheon.uf.viz.core.jobs.QueueJobRequest;
import com.raytheon.viz.aviation.xml.MonitorCfg;

/**
 * Request for monitoring
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 24, 2009            njensen     Initial creation
 * May 13, 2011 8611       rferrel     Added type
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class MonitorRequest extends QueueJobRequest<Map<?, ?>> {

    private String siteID;

    private MonitorCfg cfg;

    private String taf;

    private String type;

    /** the taf's wmo header **/
    private String wmoHeader;

    private Map<String, Object> args;

    public String getSiteID() {
        return siteID;
    }

    public void setSiteID(String siteID) {
        this.siteID = siteID;
    }

    public String getTaf() {
        return taf;
    }

    public void setTaf(String taf) {
        this.taf = taf;
    }

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }

    public String getWmoHeader() {
        return wmoHeader;
    }

    public void setWmoHeader(String wmoHeader) {
        this.wmoHeader = wmoHeader;
    }

    public MonitorCfg getCfg() {
        return cfg;
    }

    public void setCfg(MonitorCfg cfg) {
        this.cfg = cfg;
    }

    public Map<String, Object> getArgs() {
        return args;
    }

    public void setArgs(Map<String, Object> args) {
        this.args = args;
    }

}
