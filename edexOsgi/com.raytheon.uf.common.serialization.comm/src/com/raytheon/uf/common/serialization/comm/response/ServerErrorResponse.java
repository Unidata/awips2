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
package com.raytheon.uf.common.serialization.comm.response;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.SerializableExceptionWrapper;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Response returned by RemoteRequestServer in case of an exception. Right now
 * has Exception type and message but not the actually exception. TODO: need to
 * write custom serializer for exceptions so you can retrieve the stack traces
 * The ThriftClient checks for this return value and throws exception if found
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
public class ServerErrorResponse implements ISerializableObject {

    @DynamicSerializeElement
    private SerializableExceptionWrapper exception;

    public ServerErrorResponse() {

    }

    public SerializableExceptionWrapper getException() {
        return exception;
    }

    public void setException(SerializableExceptionWrapper exception) {
        this.exception = exception;
    }

}
