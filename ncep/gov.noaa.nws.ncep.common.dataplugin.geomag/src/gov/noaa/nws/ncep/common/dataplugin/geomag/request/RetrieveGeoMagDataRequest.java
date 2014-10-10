package gov.noaa.nws.ncep.common.dataplugin.geomag.request;

import java.util.Date;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.serialization.comm.IServerRequest;

/**
 * 
 * Request for GeoMag data
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 07/31/2014   R4078      sgurung     Initial creation.
 * 
 * </pre>
 * 
 * @author sgurung
 * @version 1.0
 */
@DynamicSerialize
public class RetrieveGeoMagDataRequest implements IServerRequest {

    public static enum RetrieveGeoMagDataRequestType {
        DATA_LIST, COUNT
    }

    @DynamicSerializeElement
    private String stationCode;

    @DynamicSerializeElement
    private Date startTime;

    @DynamicSerializeElement
    private Date endTime;

    @DynamicSerializeElement
    private int sourceId;

    @DynamicSerializeElement
    private RetrieveGeoMagDataRequestType requestType = RetrieveGeoMagDataRequestType.DATA_LIST;

    public RetrieveGeoMagDataRequest() {
    }

    public RetrieveGeoMagDataRequest(String stationCode, Date startTime,
            Date endTime, int sourceId,
            RetrieveGeoMagDataRequestType requestType) {
        super();
        this.stationCode = stationCode;
        this.startTime = startTime;
        this.endTime = endTime;
        this.sourceId = sourceId;
        this.requestType = requestType;
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

    public int getSourceId() {
        return sourceId;
    }

    public void setSourceId(int sourceId) {
        this.sourceId = sourceId;
    }

    public RetrieveGeoMagDataRequestType getRequestType() {
        return requestType;
    }

    public void setRequestType(RetrieveGeoMagDataRequestType requestType) {
        this.requestType = requestType;
    }

}
