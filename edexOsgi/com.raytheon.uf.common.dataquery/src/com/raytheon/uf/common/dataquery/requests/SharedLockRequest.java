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

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.serialization.comm.IServerRequest;

/**
 * The request class to coordinate with the shared locks in the
 * awips.custer_task table.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 1, 2014  2862       rferrel     Initial creation
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */

@DynamicSerialize
public class SharedLockRequest implements IServerRequest {
    /** The types of requests. */
    public static enum RequestType {
        READER_LOCK, READER_UNLOCK, READER_UPDATE_TIME, WRITER_LOCK, WRITER_UNLOCK, WRITER_UPDATE_TIME
    }

    /** The name column entry. */
    @DynamicSerializeElement
    private String name;

    /** The details column entry. */
    @DynamicSerializeElement
    private String details;

    /* The desired request. */
    @DynamicSerializeElement
    private RequestType requestType;

    /**
     * Default constructor should only be used for serialization.
     */
    public SharedLockRequest() {
    }

    /**
     * Desired constructor.
     * 
     * @param details
     * @param requestType
     */
    public SharedLockRequest(String name, String details,
            RequestType requestType) {
        setName(name);
        setDetails(details);
        setRequestType(requestType);
    }

    /**
     * Getter.
     * 
     * @return requestType
     */
    public RequestType getRequestType() {
        return requestType;
    }

    /**
     * Setter should only be used for serialization.
     * 
     * @param requestType
     */
    public void setRequestType(RequestType requestType) {
        this.requestType = requestType;
    }

    /**
     * Getter.
     * 
     * @return name
     */
    public String getName() {
        return name;
    }

    /**
     * Setter.
     * 
     * @param name
     */
    public void setName(String name) {
        this.name = name;
    }

    /**
     * Getter.
     * 
     * @return details
     */
    public String getDetails() {
        return details;
    }

    /**
     * Setter should only be used for serialization.
     * 
     * @param details
     */
    public void setDetails(String details) {
        this.details = details;
    }
}
