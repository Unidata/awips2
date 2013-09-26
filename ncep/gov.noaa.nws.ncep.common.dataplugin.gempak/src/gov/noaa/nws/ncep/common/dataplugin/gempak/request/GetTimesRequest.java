package gov.noaa.nws.ncep.common.dataplugin.gempak.request;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.serialization.comm.IServerRequest;

@DynamicSerialize
public class GetTimesRequest implements IServerRequest {

    @DynamicSerializeElement
    private String pluginName;

    @DynamicSerializeElement
    private String timeField;

    public GetTimesRequest() {
    }

    public String getPluginName() {
        return pluginName;
    }

    public void setPluginName(String pluginName) {
        this.pluginName = pluginName;
    }

    public String getTimeField() {
        return timeField;
    }

    public void setTimeField(String timeField) {
        this.timeField = timeField;
    }

}
