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
package com.raytheon.uf.common.serialization.comm;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Web service request object, includes service interface, method to execute,
 * and objects for arguments
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 22, 2011            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */
@DynamicSerialize
public class RemoteServiceRequest implements IServerRequest {

    @DynamicSerializeElement
    private String interfaceToUse;

    @DynamicSerializeElement
    private Object[] arguments;

    @DynamicSerializeElement
    private String method;

    /**
     * @return the interfaceToUse
     */
    public String getInterfaceToUse() {
        return interfaceToUse;
    }

    /**
     * @param interfaceToUse
     *            the interfaceToUse to set
     */
    public void setInterfaceToUse(String interfaceToUse) {
        this.interfaceToUse = interfaceToUse;
    }

    /**
     * @return the arguments
     */
    public Object[] getArguments() {
        return arguments;
    }

    /**
     * @param arguments
     *            the arguments to set
     */
    public void setArguments(Object[] arguments) {
        this.arguments = arguments;
    }

    /**
     * @return the method
     */
    public String getMethod() {
        return method;
    }

    /**
     * @param method
     *            the method to set
     */
    public void setMethod(String method) {
        this.method = method;
    }

}
