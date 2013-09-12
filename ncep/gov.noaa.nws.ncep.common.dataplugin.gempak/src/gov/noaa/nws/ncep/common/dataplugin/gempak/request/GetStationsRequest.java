package gov.noaa.nws.ncep.common.dataplugin.gempak.request;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.serialization.comm.IServerRequest;

@DynamicSerialize
public class GetStationsRequest implements IServerRequest {

    @DynamicSerializeElement
    private String pluginName;

    public GetStationsRequest() {
    }

    public String getPluginName() {
        return pluginName;
    }

    public void setPluginName(String pluginName) {
        this.pluginName = pluginName;
    }

}
