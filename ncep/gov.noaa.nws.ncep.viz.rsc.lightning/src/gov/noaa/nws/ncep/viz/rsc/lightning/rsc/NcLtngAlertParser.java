package gov.noaa.nws.ncep.viz.rsc.lightning.rsc;

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
        return Loader.loadData(message.dataURI);
	}
}
