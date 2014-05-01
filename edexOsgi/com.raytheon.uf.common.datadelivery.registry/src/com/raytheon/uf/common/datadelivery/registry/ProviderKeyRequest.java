package com.raytheon.uf.common.datadelivery.registry;

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
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlEnumValue;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.serialization.comm.IServerRequest;

/**
 * A request providerKey.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 14, 2013 2184      dhladky      Initial creation.
 * Aug 08, 2013 2180      mpduff       Made serializable.
 * Aug 23, 2013 2180      mpduff       Added message.
 * 
 * @author dhladky
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class ProviderKeyRequest implements IServerRequest {

    @DynamicSerializeElement
    private Provider provider;

    @DynamicSerializeElement
    private String providerKey;

    @DynamicSerializeElement
    private RequestType requestType;

    @DynamicSerializeElement
    private Status status;

    @DynamicSerializeElement
    private String message;

    public ProviderKeyRequest() {

    }

    public ProviderKeyRequest(String providerKey, Provider provider,
            RequestType requestType) {
        this.providerKey = providerKey;
        this.provider = provider;
        this.requestType = requestType;
    }

    public Provider getProvider() {
        return provider;
    }

    public void setProvider(Provider provider) {
        this.provider = provider;
    }

    public String getProviderKey() {
        return providerKey;
    }

    public void setProviderKey(String providerKey) {
        this.providerKey = providerKey;
    }

    public RequestType getRequestType() {
        return requestType;
    }

    public void setRequestType(RequestType requestType) {
        this.requestType = requestType;
    }

    /**
     * Request Type
     * 
     * <pre>
     * 
     * SOFTWARE HISTORY
     * 
     * Date         Ticket#    Engineer    Description
     * ------------ ---------- ----------- --------------------------
     * July 16, 2013 2184       dhladky     Initial creation
     * 
     * </pre>
     * 
     * @author dhladky
     * @version 1.0
     */
    public enum RequestType {
        @XmlEnumValue(RequestType.save)
        SAVE("SAVE"), @XmlEnumValue(RequestType.retrieve)
        RETRIEVE("RETRIEVE"), @XmlEnumValue(RequestType.delete)
        DELETE("DELETE");

        private static final String save = "SAVE";

        private static final String retrieve = "RETRIEVE";

        private static final String delete = "DELETE";

        private final String requestType;

        private RequestType(String name) {
            requestType = name;
        }

        @Override
        public String toString() {
            return requestType;
        }

    }

    /**
     * Transaction Status
     * 
     * <pre>
     * 
     * SOFTWARE HISTORY
     * 
     * Date         Ticket#    Engineer    Description
     * ------------ ---------- ----------- --------------------------
     * July 16, 2013 2184       dhladky     Initial creation
     * 
     * </pre>
     * 
     * @author dhladky
     * @version 1.0
     */
    public enum Status {
        @XmlEnumValue(Status.success)
        SUCCESS("SUCCESS"), @XmlEnumValue(Status.fail)
        FAILURE("FAILURE");

        private static final String success = "SUCCESS";

        private static final String fail = "FAILURE";

        private final String status;

        private Status(String name) {
            status = name;
        }

        @Override
        public String toString() {
            return status;
        }

    }

    public Status getStatus() {
        return status;
    }

    public void setStatus(Status status) {
        this.status = status;
    }

    /**
     * Set the status message
     * 
     * @param message
     *            The status message
     */
    public void setMessage(String message) {
        this.message = message;
    }

    /**
     * Get the status message.
     * 
     * @return The status message
     */
    public String getMessage() {
        return this.message;
    }
}
