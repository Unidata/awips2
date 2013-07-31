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
 * July 14, 2013 2184      dhladky     Initial creation
 *  
 * @author dhladky
 * @version 1.0
 */
public class ProviderKeyRequest implements IServerRequest {
    
    @DynamicSerializeElement
    private Provider provider;

    @DynamicSerializeElement
    private String providerKey;
    
    @DynamicSerializeElement
    private RequestType requestType;
    
    @DynamicSerializeElement
    private Status status;
    
    public ProviderKeyRequest() {
        
    }
    
    public ProviderKeyRequest(String providerKey, Provider provider, RequestType requestType) {
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

        SAVE("SAVE"), RETRIEVE("RETRIEVE");
        
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

        SUCCESS("SUCCESS"), FAILURE("FAILURE");
        
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

}
