package gov.noaa.nws.ncep.common.dataplugin.pgen.request;

import gov.noaa.nws.ncep.common.dataplugin.pgen.ActivityInfo;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.serialization.comm.IServerRequest;

/**
 * 
 * Request to store a PGEN Activity
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 22, 2013            sgilbert     Initial creation
 * 
 * </pre>
 * 
 * @author sgilbert
 * @version 1.0
 */
@DynamicSerialize
public class StoreActivityRequest implements IServerRequest {

    @DynamicSerializeElement
    private ActivityInfo activityInfo;

    @DynamicSerializeElement
    private String activityXML;

    public StoreActivityRequest() {
    }

    public StoreActivityRequest(ActivityInfo activityInfo, String activityXML) {
        this.activityInfo = activityInfo;
        this.activityXML = activityXML;
    }

    public ActivityInfo getActivityInfo() {
        return activityInfo;
    }

    public void setActivityInfo(ActivityInfo activityInfo) {
        this.activityInfo = activityInfo;
    }

    public String getActivityXML() {
        return activityXML;
    }

    public void setActivityXML(String activityXML) {
        this.activityXML = activityXML;
    }

}
