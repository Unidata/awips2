package gov.noaa.nws.ncep.viz.rsc.lightning.rsc;

import java.util.HashMap;
import java.util.Map;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.viz.core.RecordFactory;
import com.raytheon.uf.viz.core.alerts.AbstractAlertMessageParser;
import com.raytheon.uf.viz.core.alerts.AlertMessage;
import com.raytheon.uf.viz.core.comm.Loader;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData;

// This object will convert the auto update alert message into a LightningRecord and
// then 
public class NcLtngAlertParser extends AbstractAlertMessageParser {

	@Override
	public Object parseAlertMessage(AlertMessage message,
			AbstractRequestableResourceData reqResourceData) throws VizException {

        Object objectToSend = null;
        Map<String, Object> attribs = new HashMap<String, Object>(
                message.decodedAlert);
        String dataURI = message.dataURI;
        attribs.put("dataURI", message.dataURI);
        objectToSend = Loader.loadData(attribs);
        
        return objectToSend;
	}
}
