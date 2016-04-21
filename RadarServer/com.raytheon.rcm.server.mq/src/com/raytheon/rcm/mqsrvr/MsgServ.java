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
package com.raytheon.rcm.mqsrvr;

import java.io.StringReader;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.Map;

import javax.jms.Destination;
import javax.jms.JMSException;
import javax.jms.Message;
import javax.jms.MessageListener;
import javax.jms.Queue;
import javax.jms.QueueConnection;
import javax.jms.QueueConnectionFactory;
import javax.jms.QueueReceiver;
import javax.jms.QueueSender;
import javax.jms.QueueSession;
import javax.jms.Session;
import javax.jms.TextMessage;
import javax.jms.Topic;
import javax.jms.TopicConnection;
import javax.jms.TopicConnectionFactory;
import javax.jms.TopicPublisher;
import javax.jms.TopicSession;
import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;

import com.raytheon.rcm.event.ConfigEvent;
import com.raytheon.rcm.event.NotificationEvent;
import com.raytheon.rcm.event.OtrEvent;
import com.raytheon.rcm.event.RadarEvent;
import com.raytheon.rcm.event.RadarEventListener;
import com.raytheon.rcm.message.GSM;
import com.raytheon.rcm.message.GraphicProduct;
import com.raytheon.rcm.mqsrvr.ReplyObj.ROStatus;
import com.raytheon.rcm.mqsrvr.ReqObj.ActivateRMR;
import com.raytheon.rcm.mqsrvr.ReqObj.CancelRMR;
import com.raytheon.rcm.mqsrvr.ReqObj.DebugCommand;
import com.raytheon.rcm.mqsrvr.ReqObj.DebugHandleMessage;
import com.raytheon.rcm.mqsrvr.ReqObj.GetActiveRMRs;
import com.raytheon.rcm.mqsrvr.ReqObj.GetAlertRequest;
import com.raytheon.rcm.mqsrvr.ReqObj.GetGlobalConfig;
import com.raytheon.rcm.mqsrvr.ReqObj.GetRadarConfig;
import com.raytheon.rcm.mqsrvr.ReqObj.GetRadarList;
import com.raytheon.rcm.mqsrvr.ReqObj.GetRadarStatusMessages;
import com.raytheon.rcm.mqsrvr.ReqObj.GetRpsList;
import com.raytheon.rcm.mqsrvr.ReqObj.SendAlertRequest;
import com.raytheon.rcm.mqsrvr.ReqObj.SendConfigFile;
import com.raytheon.rcm.mqsrvr.ReqObj.SendMessageToRPG;
import com.raytheon.rcm.mqsrvr.ReqObj.SendOneTimeRequests;
import com.raytheon.rcm.mqsrvr.ReqObj.SendRpsList;
import com.raytheon.rcm.mqsrvr.ReqObj.SendRpsListData;
import com.raytheon.rcm.mqsrvr.ReqObj.SetGlobalConfig;
import com.raytheon.rcm.mqsrvr.ReqObj.SetRadarConfig;
import com.raytheon.rcm.otrmgr.OTRHandler;
import com.raytheon.rcm.request.RpsList;
import com.raytheon.rcm.rmr.RmrEvent;
import com.raytheon.rcm.server.Log;
import com.raytheon.rcm.server.RadarServer;
import com.raytheon.rcm.server.StatusManager.RadarStatus;

/**
 * JMS-based RadarServer request message handler.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * ...
 * 2014-02-03   DR 14762   D. Friedman Handle NDM config files.
 * </pre>
 *
 */
public class MsgServ implements RadarEventListener, MessageListener {

    QueueConnection queueConn;

    QueueSession queueSession;

    QueueSender queueSender;

    TopicConnection topicConn;

    TopicSession topicSession;

    TopicPublisher topicPublisher;

    JAXBContext jaxbCtx;

    Marshaller m;

    Unmarshaller u;

    Serv serv;

    public MsgServ(RadarServer server) {
        serv = new Serv(server);
        try {
            jaxbCtx = JAXBContext.newInstance(ReqObj.class, ReplyObj.class,
                    EventObj.class, RmrEvent.class);
            m = jaxbCtx.createMarshaller();
            u = jaxbCtx.createUnmarshaller();
        } catch (JAXBException e) {
            Log.errorf("MsgServ could not create JAXBContext: %s", e);
        }
    }

    public void start(QueueConnectionFactory qConnFac,
            TopicConnectionFactory tConnFac) {
        try {
            queueConn = qConnFac.createQueueConnection();
            queueSession = queueConn.createQueueSession(false,
                    Session.AUTO_ACKNOWLEDGE);
            Queue queue = queueSession.createQueue("RadarServer");
            queueSender = queueSession.createSender(null);
            QueueReceiver qr = queueSession.createReceiver(queue);
            qr.setMessageListener(this);

            topicConn = tConnFac.createTopicConnection();
            topicSession = topicConn.createTopicSession(false,
                    Session.AUTO_ACKNOWLEDGE);
            Topic topic = topicSession.createTopic("RadarEvents");
            topicPublisher = topicSession.createPublisher(topic);

            queueConn.start();
        } catch (JMSException e) {
            Log.errorf("MsgServ could not start connections: %s", e);
        }
    }

    @Override
    public void handleRadarEvent(RadarEvent event) {
        RadarEvent eventToSend = EventObj.filterRadarEvent(event);
        if (eventToSend != null)
            publishEvent(eventToSend);
    }

    @Override
    public void onMessage(Message msg) {
        try {
            ReplyObj po = null;
            String error = null;
            Exception exc = null;

            Destination replyToDestination = msg.getJMSReplyTo();

            if (msg instanceof TextMessage) {
                TextMessage tms = (TextMessage) msg;
                StringReader sr = new StringReader(tms.getText());
                Object o = null;
                try {
                    o = u.unmarshal(sr);
                } catch (JAXBException e) {
                    exc = e;
                }

                if (o instanceof ReqObj) {
                    ReqObj ro = (ReqObj) o;
                    try {
                        po = handleRequest(replyToDestination, ro);
                    } catch (RuntimeException e) {
                        error = "Unexpected error while processing request: "
                                + e.toString();
                        exc = e;
                    }
                } else if (o != null)
                    error = String.format("Invalid request class '%s'",
                            o.getClass());
            } else
                error = "Invalid JMS message type";

            if (error == null && exc != null)
                error = exc.toString();
            if (po == null)
                po = ReplyObj.error(error != null ? error : "Unknown error");

            if (exc != null)
                Log.errorf("Error processing remote request: %s", exc);
            else if (error != null)
                Log.errorf("Error processing remote request: %s", error);

            if (po != null) {
                StringWriter sw = new StringWriter();
                try {
                    synchronized (m) {
                        m.marshal(po, sw);
                    }
                } catch (JAXBException e) {
                    Log.errorf("Error processing remote request: %s", e);
                    return;
                }
                TextMessage rtm = queueSession.createTextMessage(sw.toString());

                String id = msg.getJMSCorrelationID();
                if (id == null)
                    id = msg.getJMSMessageID();
                if (id != null)
                    rtm.setJMSCorrelationID(id);
                if (replyToDestination != null)
                    queueSender.send(replyToDestination, rtm);
                else
                    Log.errorf("Client did not specify reply-to destination");
            }

        } catch (JMSException e) {
            Log.errorf("Error while processing JMS message: %s", e);
        }
    }

    private ReplyObj handleRequest(Destination replyToDestination, ReqObj ro) {
        ReplyObj po = null;
        String error = null;

        Log.eventf("Got remote request %s", ro);

        if (ro instanceof GetRadarList)
            po = ReplyObj.toGetRadarList(serv.getRadarList());
        else if (ro instanceof GetRadarConfig) {
            GetRadarConfig grc = (GetRadarConfig) ro;
            if (grc.radarID != null)
                po = ReplyObj
                        .toGetRadarConfig(serv.getRadarConfig(grc.radarID));
            else
                po = ReplyObj.toGetRadarConfig(serv.getAllRadarConfigs());
        } else if (ro instanceof SetRadarConfig) {
            SetRadarConfig src = (SetRadarConfig) ro;
            error = serv.setRadarConfig(src.config);
        } else if (ro instanceof GetRadarStatusMessages) {
            GetRadarStatusMessages grs = (GetRadarStatusMessages) ro;
            if (grs.radarID != null)
                po = ReplyObj.toGetRadarStatusMessages(createROStatus(
                        grs.radarID, serv.getRadarStatus(grs.radarID)));
            else {
                ArrayList<ROStatus> status = new ArrayList<ROStatus>();
                for (Map.Entry<String, ? extends RadarStatus> e : serv
                        .getAllRadarStatus().entrySet()) {
                    status.add(createROStatus(e.getKey(), e.getValue()));
                }
                po = ReplyObj.toGetRadarStatusMessages(status);
            }
        } else if (ro instanceof SendOneTimeRequests) {
            MsgServOtrHandler handler = null;

            /*
             * Correlation ID should be null so as to not intefere with the
             * handler list on the client side.
             */
            /*
             * String id = msg.getJMSCorrelationID(); if (id == null) id =
             * msg.getJMSMessageID();
             */
            String id = null;
            if (replyToDestination != null)
                handler = new MsgServOtrHandler(replyToDestination, id);

            SendOneTimeRequests r = (SendOneTimeRequests) ro;
            serv.sendOTRs(r.radarIDs, r.requests, handler);
        } else if (ro instanceof SendRpsList) {
            SendRpsList r = (SendRpsList) ro;
            boolean store = r.store != null && r.store.booleanValue();
            error = serv.sendRpsList(r.radarIDs, r.vcp, r.requests, store);
        } else if (ro instanceof SendRpsListData) {
            SendRpsListData r = (SendRpsListData) ro;
            boolean store = r.store != null && r.store.booleanValue();
            error = serv.sendRpsListData(r.radarIDs, r.vcp, r.listData, store);
        } else if (ro instanceof GetRpsList) {
            GetRpsList r = (GetRpsList) ro;
            if (r.radarID != null) {
                int vcp = r.vcp != null ? r.vcp : -1;
                int opMode = r.opMode != null ? r.opMode
                        : GSM.OP_MODE_MAINTENANCE;
                RpsList rpsList = serv.getRpsList(r.radarID, opMode, vcp);
                po = new ReplyObj.RpsListReply(rpsList);
                /*
                 * if (rpsList != null) po = ReplyObj.toGetRpsList(rpsList);
                 * else error =
                 * String.format("Could not retrieve RPS list for radar %s%s",
                 * r.radarID, vcp != -1 ? ", VCP " + vcp : "");
                 */
            } else
                error = "Must specify a radar name";
        } else if (ro instanceof GetGlobalConfig) {
            ReplyObj.GlobalConfigReply r = new ReplyObj.GlobalConfigReply();
            r.global = serv.getGlobalConfig();
            po = r;
        } else if (ro instanceof SetGlobalConfig) {
            error = serv.setGlobalConfig(((SetGlobalConfig) ro).global);
        } else if (ro instanceof ActivateRMR) {
            error = serv.activateRMR(((ActivateRMR) ro).multipleRequest);
        } else if (ro instanceof CancelRMR) {
            error = serv.cancelRMR(((CancelRMR) ro).requestName);
        } else if (ro instanceof GetActiveRMRs) {
            ReplyObj.RmrReply r = new ReplyObj.RmrReply();
            r.list = serv.getActiveRMRs();
            po = r;
        } else if (ro instanceof GetAlertRequest) {
            GetAlertRequest ro2 = (GetAlertRequest) ro;
            ReplyObj.AlertReqReply r = new ReplyObj.AlertReqReply();
            r.alertRequest = serv.getAlertRequest(ro2.radarID, ro2.areaIndex);
            po = r;
        } else if (ro instanceof SendAlertRequest) {
            SendAlertRequest ro2 = (SendAlertRequest) ro;
            error = serv.sendAlertRequest(ro2.radarID, ro2.areaIndex,
                    ro2.alertRequest);
        } else if (ro instanceof SendMessageToRPG) {
            SendMessageToRPG ro2 = (SendMessageToRPG) ro;
            error = serv.sendMessageToRPG(ro2.radarID, ro2.message);
        } else if (ro instanceof SendConfigFile) {
            SendConfigFile ro2 = (SendConfigFile) ro;
            error = serv.sendConfigFile(ro2.fileName, ro2.fileData);
        } else if (ro instanceof DebugCommand) {
            DebugCommand.Command c = ((DebugCommand) ro).command;
            if (c == DebugCommand.Command.LOG_OTR_STATUS)
                serv.logOtrStatus();
            else
                error = String.format("Unknown debug command '%s'", c);
        } else if (ro instanceof DebugHandleMessage) {
            DebugHandleMessage r = (DebugHandleMessage) ro;
            error = serv.debugHandleMessage(r.radarID, r.message);
        } else
            error = String.format("Unsupported request '%s'", ro.toString());

        if (po == null) {
            if (error == null)
                po = new ReplyObj();
            else
                po = ReplyObj.error(error);
        }

        return po;
    }

    private ROStatus createROStatus(String radarID, RadarStatus rs) {
        ROStatus ros = new ROStatus();
        ros.radarID = radarID;
        ros.currentAAP = rs.getCurrentAAP();
        ros.currentGSM = rs.getCurrentGSM();
        ros.currentPTL = rs.getCurrentPTL();
        ros.lastAAP = rs.getLastAAP();
        ros.lastGSM = rs.getLastGSM();
        ros.lastPTL = rs.getLastPTL();
        return ros;
    }

    @Override
    public void handleConfigEvent(ConfigEvent event) {
        publishEvent(event);
    }

    private void publishEvent(Object obj) {
        // Can get events before mq is set up.
        if (topicPublisher == null)
            return;

        StringWriter sw = new StringWriter();
        try {
            synchronized (m) {
                m.marshal(obj, sw);
            }
        } catch (JAXBException e) {
            Log.errorf("Error serializing event: %s", e);
            return;
        }
        try {
            TextMessage tm = topicSession.createTextMessage(sw.toString());
            topicPublisher.publish(tm);
        } catch (JMSException e) {
            Log.errorf("Error sending message: %s", e);
        }
    }

    @Override
    public void handleNotificationEvent(NotificationEvent event) {
        publishEvent(event);
    }

    class MsgServOtrHandler implements OTRHandler {
        Destination destination;

        String correlationID;

        public MsgServOtrHandler(Destination destination, String correlationID) {
            this.destination = destination;
            this.correlationID = correlationID;
        }

        @Override
        public void handleOtrEvent(OtrEvent event) {
            OtrEvent eventToSend = event.clone();
            /*
             * This OTR notification capability is currently only used to
             * display alerts in CAVE. Thus, there is no need to send the actual
             * product data. Given that some products can be larger than one
             * megabyte and the notification is in XML format, this is a useful
             * optimization.
             */
            if (event.product != null
                    && com.raytheon.rcm.message.Message
                            .messageCodeOf(event.product) > 16)
                eventToSend.product = GraphicProduct
                        .extractHeaderAndPDB(event.product);

            StringWriter sw = new StringWriter();
            try {
                synchronized (m) {
                    m.marshal(eventToSend, sw);
                }
            } catch (JAXBException e) {
                Log.errorf("Error processing remote request: %s", e);
                return;
            }
            try {
                TextMessage rtm = queueSession.createTextMessage(sw.toString());
                if (correlationID != null)
                    rtm.setJMSCorrelationID(correlationID);
                queueSender.send(destination, rtm);
            } catch (JMSException e) {
                Log.errorf("Error sending message: %s", e);
            }
        }

    }
}
