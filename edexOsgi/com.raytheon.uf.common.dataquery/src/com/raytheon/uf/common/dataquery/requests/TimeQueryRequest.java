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
package com.raytheon.uf.common.dataquery.requests;

import java.util.Date;
import java.util.Map;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.serialization.comm.IServerRequest;
import com.raytheon.uf.common.time.BinOffset;

/**
 * Request to query available times. A bin offset can be included and will be
 * applied to the resulting times by the server handler.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 5, 2011            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

@DynamicSerialize
public class TimeQueryRequest implements IServerRequest {

    @DynamicSerializeElement
    private String pluginName;

    @DynamicSerializeElement
    private boolean maxQuery;

    @DynamicSerializeElement
    private Date simDate;

    @DynamicSerializeElement
    private Map<String, RequestConstraint> queryTerms;

    @DynamicSerializeElement
    private BinOffset binOffset;

    public String getPluginName() {
        return pluginName;
    }

    public void setPluginName(String pluginName) {
        this.pluginName = pluginName;
    }

    public void setQueryTerms(Map<String, RequestConstraint> queryTerms) {
        this.queryTerms = queryTerms;
    }

    public BinOffset getBinOffset() {
        return binOffset;
    }

    public void setBinOffset(BinOffset binOffset) {
        this.binOffset = binOffset;
    }

    public boolean isMaxQuery() {
        return maxQuery;
    }

    public void setMaxQuery(boolean maxQuery) {
        this.maxQuery = maxQuery;
    }

    public Map<String, RequestConstraint> getQueryTerms() {
        return queryTerms;
    }

    public void setSimDate(Date simDate) {
        this.simDate = simDate;
    }

    public Date getSimDate() {
        return simDate;
    }

    @Override
    public String toString() {
        return "TimeQueryRequest [pluginName=" + pluginName + ", maxQuery="
                + maxQuery + ", queryTerms=" + queryTerms + ", binOffset="
                + binOffset + " simDate=" + simDate + " ]";
    }

}
