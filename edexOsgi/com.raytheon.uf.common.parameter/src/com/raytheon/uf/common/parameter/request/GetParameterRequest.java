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
package com.raytheon.uf.common.parameter.request;

import com.raytheon.uf.common.parameter.Parameter;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.serialization.comm.IServerRequest;

/**
 * Request to lookup a parameter that has the same abbreviation as the provided
 * parameter, also provides option for creating the parameter in the database if
 * it does not already exist.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 26, 2012            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
@DynamicSerialize
public class GetParameterRequest implements IServerRequest {

    @DynamicSerializeElement
    private Parameter parameter;

    @DynamicSerializeElement
    private boolean create = false;

    public GetParameterRequest() {
    }

    public GetParameterRequest(Parameter parameter) {
        this.parameter = parameter;
        this.create = false;
    }

    public GetParameterRequest(Parameter parameter, boolean create) {
        this.parameter = parameter;
        this.create = create;
    }

    public Parameter getParameter() {
        return parameter;
    }

    public void setParameter(Parameter parameter) {
        this.parameter = parameter;
    }

    public boolean getCreate() {
        return create;
    }

    public void setCreate(boolean create) {
        this.create = create;
    }

}
