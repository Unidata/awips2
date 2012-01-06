package gov.noaa.nws.ncep.viz.rsc.plotdata.rsc;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;

import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.alerts.AbstractAlertMessageParser;
import com.raytheon.uf.viz.core.alerts.AlertMessage;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData;
import com.raytheon.viz.pointdata.PlotInfo;

/**
 * AlertMessage parser for the H5 Uair Plot data
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 7/04/11        #441      ghull      Initial creation
 * 
 * </pre>
 * 
 * @author ghull
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class H5UairAlertParser extends AbstractAlertMessageParser {

	// TODO :
	// Currently the URI for the H5UAIR doesn't have the lat/lon in it.
	// We will either need to query the data using the URI or modify the
	// URI's for H5UAIR.
	// For now we will return null and not auto update for H5Uair.
	//
    @Override
    public Object parseAlertMessage(AlertMessage message,
            AbstractRequestableResourceData resourceData) throws VizException {
        String stationId = (String) message.decodedAlert
                .get("location.stationId");
        Double lat = (Double) message.decodedAlert.get("location.latitude");
        Double lon = (Double) message.decodedAlert.get("location.longitude");
        DataTime dataTime = (DataTime) message.decodedAlert.get("dataTime");
        
//        return new PlotInfo(message.id, stationId, lat, lon, dataTime,
//                message.dataURI);
        return null;
    }
}