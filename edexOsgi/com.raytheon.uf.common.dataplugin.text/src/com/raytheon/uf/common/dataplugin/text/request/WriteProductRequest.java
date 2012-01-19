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
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 2, 2010            njensen     Initial creation
 * 01Jun2010               cjeanbap    Added operational mode functionality.
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

@DynamicSerialize
public class WriteProductRequest implements IServerRequest {

    @DynamicSerializeElement
    private String productId;

    @DynamicSerializeElement
    private String reportData;
    
    @DynamicSerializeElement
    private boolean operationalMode;
    
    @DynamicSerializeElement
    private boolean notifyAlarmAlert = false;

    public String getProductId() {
        return productId;
    }

    public void setProductId(String productId) {
        this.productId = productId;
    }

    public String getReportData() {
        return reportData;
    }

    public void setReportData(String reportData) {
        this.reportData = reportData;
    }

    public boolean getOperationalMode() {
        return operationalMode;
    }
    
    public void setOperationalMode(boolean operationalMode) {
       this.operationalMode = operationalMode;
    }

    public boolean isNotifyAlarmAlert() {
        return notifyAlarmAlert;
    }

    public void setNotifyAlarmAlert(boolean notifyAlarmAlert) {
        this.notifyAlarmAlert = notifyAlarmAlert;
    }
}
