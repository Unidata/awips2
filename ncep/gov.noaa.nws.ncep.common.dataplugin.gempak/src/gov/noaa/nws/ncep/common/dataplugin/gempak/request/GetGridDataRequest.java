package gov.noaa.nws.ncep.common.dataplugin.gempak.request;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.serialization.comm.IServerRequest;

@DynamicSerialize
public class GetGridDataRequest implements IServerRequest {

    @DynamicSerializeElement
    private String pluginName;

    @DynamicSerializeElement
    private String modelId;

    @DynamicSerializeElement
    private String reftime;

    @DynamicSerializeElement
    private String fcstsec;

    @DynamicSerializeElement
    private String level1;

    @DynamicSerializeElement
    private String level2;

    @DynamicSerializeElement
    private String vcoord;

    @DynamicSerializeElement
    private String parm;

    public GetGridDataRequest() {
    }

    public String getPluginName() {
        return pluginName;
    }

    public void setPluginName(String pluginName) {
        this.pluginName = pluginName;
    }

    public String getModelId() {
        return modelId;
    }

    public void setModelId(String modelId) {
        this.modelId = modelId;
    }

	public String getReftime() {
		return reftime;
	}

	public void setReftime(String reftime) {
		this.reftime = reftime;
	}

	public String getFcstsec() {
		return fcstsec;
	}

	public void setFcstsec(String fcstsec) {
		this.fcstsec = fcstsec;
	}

	public String getLevel1() {
		return level1;
	}

	public void setLevel1(String level1) {
		this.level1 = level1;
	}

	public String getLevel2() {
		return level2;
	}

	public void setLevel2(String level2) {
		this.level2 = level2;
	}

	public String getVcoord() {
		return vcoord;
	}

	public void setVcoord(String vcoord) {
		this.vcoord = vcoord;
	}

	public String getParm() {
		return parm;
	}

	public void setParm(String parm) {
		this.parm = parm;
	}

}
