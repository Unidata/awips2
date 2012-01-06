/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/

package com.raytheon.edex.uengine.tasks.process;

import javax.jms.TextMessage;

import com.raytheon.edex.msg.Command;
import com.raytheon.edex.uengine.exception.MicroEngineException;
import com.raytheon.edex.uengine.tasks.ScriptTask;
import com.raytheon.edex.uengine.util.MEUtils;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.edex.core.props.EnvProperties;
import com.raytheon.uf.edex.core.props.PropertiesFactory;

/**
 * ExecuteCommand task derived from old uEngine ExecuteCommand task. Executes
 * the command line command contained in the body. The command is executed by
 * forwarding the command to the {@link com.raytheon.edex.adapterSrv.AdapterSrv
 * AdapterSrv} service for processing.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date             PR#             Engineer            Description
 * -----------      ----------      ------------        --------------------------
 * Apr 12, 2007                     njensen             Initial Creation
 * 02Oct2007        345             MW Fegan            execute() returns ProgramOutput object.
 * </PRE>
 * 
 */
public class ExecuteCommand extends ScriptTask {

    private String body = "";

    private long timeOut = 60 * 1000;

    private static EnvProperties environment = PropertiesFactory.getInstance()
            .getEnvProperties();

    public ExecuteCommand(String aBody) {
        body = aBody;
    }

    public ExecuteCommand(String aBody, long aTimeOut) {
        body = aBody;
        timeOut = aTimeOut;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.uengine.js.tasks.ScriptTask#execute()
     */
    @Override
    public Object execute() {
        String responseStr = null;

        try {

            String queue = environment.getEnvValue("ADAPTERSRVQUE");

            String corrID = java.util.UUID.randomUUID().toString();
            Command cmd = Command.initialize(body);
            String command = SerializationUtil.marshalToXml(cmd);
            TextMessage prodResponse = (TextMessage) MEUtils.handleTextMessage(
                    command, queue, queue + "Rsp", timeOut, corrID,
                    "uEngineWeb", "tcp://localhost:5672");
            responseStr = prodResponse.getText();
            return SerializationUtil.unmarshalFromXml(responseStr);
        } catch (Exception e) {
            throw new MicroEngineException("unable to execute ", e);
        }
    }

    public String getBody() {
        return body;
    }

    public void setBody(String aBody) {
        body = aBody;
    }

    public long getTimeOut() {
        return timeOut;
    }

    public void setTimeOut(long aTimeOut) {
        timeOut = aTimeOut;
    }

}
