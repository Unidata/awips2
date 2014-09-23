package gov.noaa.nws.ncep.common.dataplugin.geomag.request;

import java.util.Date;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.serialization.comm.IServerRequest;

/**
 * 
 * Request for a GeoMagk1min for the given dataURI
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 2014/07/01   #1136      qzhou       Init
 * 
 * </pre>
 * 
 * @author qzhou
 * @version 1.0
 */
@DynamicSerialize
public class RetrieveSingleK1minRequest implements IServerRequest {

    @DynamicSerializeElement
    private String stationCode;

    @DynamicSerializeElement
    private Date refTime;

    public RetrieveSingleK1minRequest() {
    }

    public RetrieveSingleK1minRequest(String stationCode, Date refTime) {
        super();
        this.stationCode = stationCode;
        this.refTime = refTime;
    }

    public String getStationCode() {
        return stationCode;
    }

    public void setStationCode(String stationCode) {
        this.stationCode = stationCode;
    }

    public Date getRefTime() {
        return refTime;
    }

    public void setRefTime(Date refTime) {
        this.refTime = refTime;
    }
}
