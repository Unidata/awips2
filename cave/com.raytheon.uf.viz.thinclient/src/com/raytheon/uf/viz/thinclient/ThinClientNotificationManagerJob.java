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
package com.raytheon.uf.viz.thinclient;

import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;

import com.raytheon.uf.common.localization.msgs.GetServersRequest;
import com.raytheon.uf.common.localization.msgs.GetServersResponse;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.notification.jobs.NotificationManagerJob;
import com.raytheon.uf.viz.core.requests.ThriftClient;
import com.raytheon.uf.viz.thinclient.preferences.ThinClientPreferenceConstants;

/**
 * Listens to changes to the "Disable JMS" option in the Thin Client
 * Preferences. Will automatically connect to and disconnect from the JMS Server
 * as the option is updated.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 29, 2011            bsteffen     Initial creation
 * Aug 27, 2013 2295       bkowal       The entire jms connection string is now
 *                                      provided by EDEX.
 * Feb 08, 2016 5281       tjensen      Replaced disableJms with dataRefreshMethod
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class ThinClientNotificationManagerJob extends NotificationManagerJob
        implements IPropertyChangeListener {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ThinClientNotificationManagerJob.class, "ThinClient");

    private static ThinClientNotificationManagerJob instance;

    private String dataRefreshMethod;

    public static synchronized ThinClientNotificationManagerJob getInstance() {
        if (instance == null) {
            instance = new ThinClientNotificationManagerJob();
            setCustomInstance(instance);
        }
        return instance;
    }

    public ThinClientNotificationManagerJob() {
        super();
        IPreferenceStore store = Activator.getDefault().getPreferenceStore();
        dataRefreshMethod = store
                .getString(ThinClientPreferenceConstants.P_DATA_REFRESH_METHOD);

        store.addPropertyChangeListener(this);
        if (ThinClientPreferenceConstants.P_DATA_REFRESH_METHOD_PUSH
                .equals(dataRefreshMethod)) {
            connect(true);
        }
    }

    @Override
    protected void connect(boolean notifyError) {
        if (ThinClientPreferenceConstants.P_DATA_REFRESH_METHOD_POLL
                .equals(dataRefreshMethod)) {
            return;
        } else {
            super.connect(notifyError);
        }
    }

    @Override
    public void propertyChange(PropertyChangeEvent event) {
        if (ThinClientPreferenceConstants.P_DATA_REFRESH_METHOD.equals(event
                .getProperty())) {
            dataRefreshMethod = String.valueOf(event.getNewValue());

            if (ThinClientPreferenceConstants.P_DATA_REFRESH_METHOD_POLL
                    .equals(dataRefreshMethod)) {
                disconnect(true);
            } else {
                if (VizApp.getJmsConnectionString() == null) {
                    GetServersRequest req = new GetServersRequest();
                    GetServersResponse resp;
                    try {
                        resp = (GetServersResponse) ThriftClient
                                .sendLocalizationRequest(req);
                        VizApp.setJmsConnectionString(resp
                                .getJmsConnectionString());
                    } catch (VizException e) {
                        statusHandler.handle(Priority.PROBLEM,
                                e.getLocalizedMessage(), e);
                    }
                }
                connect(true);
            }
        }
    }

}
