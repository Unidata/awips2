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

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.serialization.comm.IServerRequest;

/**
 * 
 * ChangeStationStateRequest
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
public class ChangeStationStateRequest implements IServerRequest {

    @DynamicSerializeElement
    private String stationCode;

    @DynamicSerializeElement
    private Date synopticTime;

    @DynamicSerializeElement
    private Integer prevPeriod;

    public ChangeStationStateRequest() {
    }

    public ChangeStationStateRequest(String stationCode, Integer prevPeriod,
            Date timeTag) {
        super();
        this.stationCode = stationCode;
        this.synopticTime = timeTag;
        this.prevPeriod = prevPeriod;
    }

    public Integer getPrevPeriod() {
        return prevPeriod;
    }

    public void setPrevPeriod(Integer prevPeriod) {
        this.prevPeriod = prevPeriod;
    }

    public String getStationCode() {
        return stationCode;
    }

    public void setStationCode(String stationCode) {
        this.stationCode = stationCode;
    }

    public Date getSynopticTime() {
        return synopticTime;
    }

    public void setSynopticTime(Date synopticTime) {
        this.synopticTime = synopticTime;
    }
}
