package gov.noaa.nws.ncep.common.dataplugin.geomag.request;

import java.util.Date;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.serialization.comm.IServerRequest;

/**
 * 
 * Request for a GeoMagAvg for the given dataURI
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 2014/02/12   #1110      qzhou       Init
 * 
 * </pre>
 * 
 * @author qzhou
 * @version 1.0
 */
@DynamicSerialize
public class RetrieveHrAvgRequest implements IServerRequest {

    @DynamicSerializeElement
    private String stationCode;

    @DynamicSerializeElement
    private Date startTime;

    @DynamicSerializeElement
    private Date endTime;

    public RetrieveHrAvgRequest() {
    }

    public RetrieveHrAvgRequest(String stationCode, Date startTime, Date endTime) {
        super();
        this.stationCode = stationCode;
        this.startTime = startTime;
        this.endTime = endTime;
    }

    public String getStationCode() {
        return stationCode;
    }

    public void setStationCode(String stationCode) {
        this.stationCode = stationCode;
    }

    public Date getStartTime() {
        return startTime;
    }

    public void setStartTime(Date startTime) {
        this.startTime = startTime;
    }

    public Date getEndTime() {
        return endTime;
    }

    public void setEndTime(Date endTime) {
        this.endTime = endTime;
    }

}
