/**
 * This code has unlimited rights, and is provided "as is" by the National Centers 
 * for Environmental Prediction, without warranty of any kind, either expressed or implied, 
 * including but not limited to the implied warranties of merchantability and/or fitness 
 * for a particular purpose.
 * 
 * This code has been developed by the NCEP-SIB for use in the AWIPS2 system.
 * 
 **/
package gov.noaa.nws.ncep.common.dataplugin.geomag.request;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.serialization.comm.IServerRequest;

/**
 * 
 * Request for a GeoMagk1min
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 2014/02/12   #1110      qzhou       Init
 * 03/05/2014   R4078      sgurung     Added requestType and stationCodeList.
 * 
 * </pre>
 * 
 * @author qzhou
 * @version 1.0
 */
@DynamicSerialize
public class RetrieveK1minRequest implements IServerRequest {

    public static enum RetrieveK1minRequestType {
        K, KP, LATEST_K, LAST_DATA_DATE
    }

    @DynamicSerializeElement
    private String stationCode;

    @DynamicSerializeElement
    private Date startTime;

    @DynamicSerializeElement
    private Date endTime;

    @DynamicSerializeElement
    private List<String> stationCodeList;

    @DynamicSerializeElement
    private RetrieveK1minRequestType requestType = RetrieveK1minRequestType.K;

    public RetrieveK1minRequest() {
    }

    public RetrieveK1minRequest(String stationCode, Date startTime, Date endTime) {
        super();
        this.stationCode = stationCode;
        this.startTime = startTime;
        this.endTime = endTime;
        stationCodeList = new ArrayList<String>();
    }

    public RetrieveK1minRequest(List<String> stationCodeList, Date startTime,
            Date endTime, RetrieveK1minRequestType reqType) {
        super();
        this.stationCode = null;
        this.startTime = startTime;
        this.endTime = endTime;
        this.stationCodeList = stationCodeList;
        requestType = reqType;
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

    public List<String> getStationCodeList() {
        return stationCodeList;
    }

    public void setStationCodeList(List<String> stationCodeList) {
        this.stationCodeList = stationCodeList;
    }

    public RetrieveK1minRequestType getRequestType() {
        return requestType;
    }

    public void setRequestType(RetrieveK1minRequestType requestType) {
        this.requestType = requestType;
    }

}
