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
 * Nov 19, 2009            avarani     Initial creation
 * 
 * </pre>
 * 
 * @author avarani
 * @version 1.0
 */
@DynamicSerialize
public class GetPartialAfosIdRequest implements IServerRequest {
    @DynamicSerializeElement
    private String cccc;

    @DynamicSerializeElement
    private String nnn;

    @DynamicSerializeElement
    private String xxx;

    public String getCccc() {
        return cccc;
    }

    public void setCccc(String cccc) {
        this.cccc = cccc;
    }

    public String getNnn() {
        return nnn;
    }

    public void setNnn(String nnn) {
        this.nnn = nnn;
    }

    public String getXxx() {
        return xxx;
    }

    public void setXxx(String xxx) {
        this.xxx = xxx;
    }
}
