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
package com.raytheon.uf.common.datastorage.remote.requests;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.serialization.comm.IServerRequest;

/**
 * Request for invoking a method on IDataStore using thrift. This class uses
 * reflection heavily
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 3, 2009            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */
@DynamicSerialize
public class ThriftDataRequest implements IServerRequest {

    @DynamicSerializeElement
    private Object[] parameters;

    @DynamicSerializeElement
    private String[] parameterTypes;

    @DynamicSerializeElement
    private String file;

    @DynamicSerializeElement
    private String method;

    @DynamicSerializeElement
    private boolean useLocking;

    public Object[] getParameters() {
        return parameters;
    }

    public void setParameters(Object[] parameters) {
        this.parameters = parameters;
    }

    public String getFile() {
        return file;
    }

    public void setFile(String file) {
        this.file = file;
    }

    public String getMethod() {
        return method;
    }

    public void setMethod(String method) {
        this.method = method;
    }

    public boolean isUseLocking() {
        return useLocking;
    }

    public void setUseLocking(boolean useLocking) {
        this.useLocking = useLocking;
    }

    public String[] getParameterTypes() {
        return parameterTypes;
    }

    /**
     * Sets the parameter types, these types are turned into classes using
     * Class.forName on the server side. Needed for reflective method invocation
     * 
     * @param parameterTypes
     *            should match up 1-1 with parameters
     */
    public void setParameterTypes(String[] parameterTypes) {
        this.parameterTypes = parameterTypes;
    }

}
