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

import java.util.Date;
import java.util.List;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.serialization.comm.IServerRequest;

/**
 * 
 * RetrieveStationStateRequest
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 07/01/2014   R4078      sgurung     Initial Creation
 * 
 * </pre>
 * 
 * @author sgurung
 * @version 1.0
 */
@DynamicSerialize
public class RetrieveStationStateRequest implements IServerRequest {

    @DynamicSerializeElement
    private String stationCode;

    @DynamicSerializeElement
    private Date refTime;

    @DynamicSerializeElement
    private List<String> stationCodeList;

    public RetrieveStationStateRequest() {
    }

    public RetrieveStationStateRequest(String stationCode, Date timeTag) {
        super();
        this.stationCode = stationCode;
        this.refTime = timeTag;
        this.stationCodeList = null;
    }

    public RetrieveStationStateRequest(List<String> stationCodeList,
            Date timeTag) {
        super();
        this.stationCode = null;
        this.refTime = timeTag;
        this.stationCodeList = stationCodeList;
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

    public List<String> getStationCodeList() {
        return stationCodeList;
    }

    public void setStationCodeList(List<String> stationCodeList) {
        this.stationCodeList = stationCodeList;
    }

}
