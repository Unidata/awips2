package com.raytheon.uf.common.dataplugin.text.request;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.serialization.comm.IServerRequest;

@DynamicSerialize
public class RemoteRetrievalRequest implements IServerRequest {
    
    @DynamicSerializeElement
    private String requestType = "AFOS";

    @DynamicSerializeElement
    private String dataType = "TEXT";

    // Adapted types (such as Date) have too many serialization issues in C++.
    @DynamicSerializeElement
    private long validTime;

    @DynamicSerializeElement
    private String addressee;

    @DynamicSerializeElement
    private String wmoHeader; // TTAAii cccc 

    @DynamicSerializeElement
    private long mostRecentTime;
    
    @DynamicSerializeElement
    private String afosID;

    /**
     * @return the requestType
     */
    public String getRequestType() {
        return requestType;
    }

    /**
     * @param requestType the requestType to set
     */
    public void setRequestType(String requestType) {
        this.requestType = requestType;
    }

    /**
     * @return the dataType
     */
    public String getDataType() {
        return dataType;
    }

    /**
     * @param dataType the dataType to set
     */
    public void setDataType(String dataType) {
        this.dataType = dataType;
    }

    /**
     * @return the addressee
     */
    public String getAddressee() {
        return addressee;
    }

    /**
     * @param addressee the addressee to set
     */
    public void setAddressee(String addressee) {
        this.addressee = addressee;
    }

    /**
     * @return the wmoHeader
     */
    public String getWmoHeader() {
        return wmoHeader;
    }

    /**
     * @param wmoHeader the wmoHeader to set
     */
    public void setWmoHeader(String wmoHeader) {
        this.wmoHeader = wmoHeader;
    }

    /**
     * @return the afosID
     */
    public String getAfosID() {
        return afosID;
    }

    /**
     * @param afosID the afosID to set
     */
    public void setAfosID(String afosID) {
        this.afosID = afosID;
    }
    
    /**
     * @return the validTime
     */
    public long getValidTime() {
        return validTime;
    }

    /**
     * @param validTime the validTime to set
     */
    public void setValidTime(long validTime) {
        this.validTime = validTime;
    }

    /**
     * @return the mostRecentTime
     */
    public long getMostRecentTime() {
        return mostRecentTime;
    }

    /**
     * @param mostRecentTime the mostRecentTime to set
     */
    public void setMostRecentTime(long mostRecentTime) {
        this.mostRecentTime = mostRecentTime;
    }

    // Follows the same logic as (TODO)
    public String getMatchKey() {
        // AWIPS 1 does not take the fime fields or WMO heading into account...
        return String.format("%s:%s:%s", requestType, dataType, afosID);
    }

}
