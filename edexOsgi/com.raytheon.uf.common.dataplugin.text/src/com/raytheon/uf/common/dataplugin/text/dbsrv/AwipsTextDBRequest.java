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
package com.raytheon.uf.common.dataplugin.text.dbsrv;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.serialization.comm.IServerRequest;

/**
 * Object used by thrift clients to make awips textdb requests
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 12, 2016 4716       rferrel     Initial creation
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */
@DynamicSerialize
public class AwipsTextDBRequest implements IServerRequest {

    @DynamicSerializeElement
    private String nnnxxx;

    public AwipsTextDBRequest() {
    }

    /**
     * @param message
     */
    public AwipsTextDBRequest(String nnnxxx) {
        setNnnxxx(nnnxxx);
    }

    public String getNnnxxx() {
        return nnnxxx;
    }

    public void setNnnxxx(String nnnxxx) {
        if (nnnxxx.length() == 4) {
            this.nnnxxx = nnnxxx + "  ";
        } else if (nnnxxx.length() == 5) {
            this.nnnxxx = nnnxxx + " ";
        } else {
            this.nnnxxx = nnnxxx;
        }
    }
}
