package gov.noaa.nws.ncep.common.dataplugin.gempak.request;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.serialization.comm.IServerRequest;
import com.raytheon.uf.common.time.DataTime;

@DynamicSerialize
public class StationDataRequest implements IServerRequest {

    @DynamicSerializeElement
    private String pluginName;

    @DynamicSerializeElement
    private String stationId;

    @DynamicSerializeElement
    private DataTime refTime;
    
    @DynamicSerializeElement
    private String parmList;

    @DynamicSerializeElement
    private String partNumber;

    public StationDataRequest() {
    }

    public String getPluginName() {
        return pluginName;
    }

    public void setPluginName(String pluginName) {
        this.pluginName = pluginName;
    }

    public String getStationId() {
        return stationId;
    }

    public void setStationId(String stationId) {
        this.stationId = stationId;
    }

    public DataTime getRefTime() {
        return refTime;
    }

    public void setRefTime(DataTime refTime) {
        this.refTime = refTime;
    }
    
    public String getParmList() {
    	return parmList;
    }
    
    public void setParmList(String parmList) {
    	this.parmList = parmList;
    }
    
    public String getPartNumber() {
    	return partNumber;
    }
    
    public void setPartNumber(String partNumber) {
    	this.partNumber = partNumber;
    }

}
