package gov.noaa.nws.ncep.common.dataplugin.gempak.request;

import java.util.List;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

@DynamicSerialize
public class GetTimesResponse {

    @DynamicSerializeElement
    private List<?> times;

    public GetTimesResponse() {
    }

    public List<?> getTimes() {
        return times;
    }

    public void setTimes(List<?> times) {
        this.times = times;
    }

}
