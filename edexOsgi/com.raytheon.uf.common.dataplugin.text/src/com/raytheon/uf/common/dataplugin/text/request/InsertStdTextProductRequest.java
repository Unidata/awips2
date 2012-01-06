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
 * Nov 12, 2009            mschenke     Initial creation
 * 01Jun2010               cjeanbap     Added operational mode functionality.
 * 02Aug2010     2187      cjeanbap     Update variable/method signature to be consistent.    
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */
@DynamicSerialize
public class InsertStdTextProductRequest implements IServerRequest {

    @DynamicSerializeElement
    private String afosId;

    @DynamicSerializeElement
    private String product;

    @DynamicSerializeElement
    private boolean notifySubscriptions = false;

    @DynamicSerializeElement
    private boolean notifyAlarmAlert = false;
    
    @DynamicSerializeElement
    private boolean operationalMode = true;

    public InsertStdTextProductRequest() {

    }

    public InsertStdTextProductRequest(String afosId, String product) {
        this.afosId = afosId;
        this.product = product;
    }
    
    public InsertStdTextProductRequest(String afosId, String product, boolean operationalMode) {
        this.afosId = afosId;
        this.product = product;
        this.operationalMode = operationalMode; 
    }

    public boolean isNotifySubscriptions() {
        return notifySubscriptions;
    }

    public void setNotifySubscriptions(boolean notifySubscriptions) {
        this.notifySubscriptions = notifySubscriptions;
    }

    public boolean isNotifyAlarmAlert() {
        return notifyAlarmAlert;
    }

    public void setNotifyAlarmAlert(boolean notifyAlarmAlert) {
        this.notifyAlarmAlert = notifyAlarmAlert;
    }

    public String getAfosId() {
        return afosId;
    }

    public void setAfosId(String afosId) {
        this.afosId = afosId;
    }

    public String getProduct() {
        return product;
    }

    public void setProduct(String product) {
        this.product = product;
    }

    public boolean getOperationalMode() {
        return operationalMode; 
    }
    
    public void setOperationalMode(boolean operationalMode) {
        this.operationalMode = operationalMode;
    }
}
