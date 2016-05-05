package com.raytheon.viz.warnings.rsc;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.warning.AbstractWarningRecord;
import com.raytheon.uf.common.dataquery.requests.DbQueryRequest;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.responses.DbQueryResponse;
import com.raytheon.uf.viz.core.RecordFactory;
import com.raytheon.uf.viz.core.alerts.AbstractAlertMessageParser;
import com.raytheon.uf.viz.core.alerts.AlertMessage;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.requests.ThriftClient;
import com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;

/**
 *    SOFTWARE HISTORY
 *   
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Apr 21, 2016  DR 18905 Qinglu Lin  Added code to handle no SPS auto-update issue.
 * 
 */
@XmlAccessorType(XmlAccessType.NONE)
public class CWASPSResourceData extends WWAResourceData {

    private static AlertMessageToPDOParserSPS alertParser = new AlertMessageToPDOParserSPS();

    /*
     * (non-Javadoc)
     * 
     * @seecom.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData#
     * constructResource(com.raytheon.uf.viz.core.comm.LoadProperties,
     * com.raytheon.edex.db.objects.PluginDataObject[])
     */
    @Override
    protected AbstractVizResource<?, ?> constructResource(
            LoadProperties loadProperties, PluginDataObject[] objects)
            throws VizException {
        // add records
        records = new ArrayList<AbstractWarningRecord>(objects.length);
        for (int i = 0; i < objects.length; i++) {
            AbstractWarningRecord r = (AbstractWarningRecord) objects[i];
            records.add(r);
        }

        return new CWASPSResource(this, loadProperties);
    }

    @Override
    public AbstractAlertMessageParser getAlertParser() {
        return alertParser;
    }

    private static class AlertMessageToPDOParserSPS extends AbstractAlertMessageParser {

        @Override
        public Object parseAlertMessage(AlertMessage message,
                AbstractRequestableResourceData reqResourceData) throws VizException {
            Object objectToSend = null;
            Map<String, Object> attribs = new HashMap<>(message.decodedAlert);

            if (reqResourceData.isUpdatingOnMetadataOnly()) {
                PluginDataObject record = RecordFactory.getInstance()
                        .loadRecordFromMap(attribs);
                objectToSend = record;
            } else {
                /*
                 * TODO avoid requesting data that will not be used, for example
                 * when time matching won't allow the frame to be displayed.
                 */
                attribs.remove(PluginDataObject.DATAURI_ID);
                DbQueryRequest request = new DbQueryRequest(
                        RequestConstraint.toConstraintMappingExcludeNull(attribs));
                request.setLimit(1);
                DbQueryResponse response = (DbQueryResponse) ThriftClient
                        .sendRequest(request);
                PluginDataObject[] pdos = response
                        .getEntityObjects(PluginDataObject.class);
                if (pdos.length > 0) {
                    objectToSend = pdos[0];
                }
            }
            return objectToSend;
        }
    }
}
