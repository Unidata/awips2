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
package com.raytheon.uf.viz.collaboration.ui.actions;

import java.lang.ref.Reference;
import java.lang.ref.WeakReference;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IPartListener;
import org.eclipse.ui.IViewReference;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.jivesoftware.smack.XMPPConnection;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.xmpp.iq.FeedVenueConfig;
import com.raytheon.uf.viz.collaboration.comm.identity.CollaborationException;
import com.raytheon.uf.viz.collaboration.comm.identity.ISession;
import com.raytheon.uf.viz.collaboration.comm.identity.IVenueSession;
import com.raytheon.uf.viz.collaboration.comm.provider.connection.CollaborationConnection;
import com.raytheon.uf.viz.collaboration.comm.provider.session.FeedVenueConfigManager;
import com.raytheon.uf.viz.collaboration.comm.provider.session.VenueSession;
import com.raytheon.uf.viz.collaboration.comm.provider.user.VenueId;
import com.raytheon.uf.viz.collaboration.ui.Activator;
import com.raytheon.uf.viz.collaboration.ui.prefs.HandleUtil;
import com.raytheon.uf.viz.collaboration.ui.session.SessionFeedView;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.icon.IconUtil;
import com.raytheon.viz.ui.views.CaveWorkbenchPageManager;

/**
 * Display the feed view
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul  5, 2012            bsteffen    Initial creation
 * Dec 19, 2013 2563       bclement    added check for feed venue existence
 * Jan 28, 2014 2698       bclement    changed feed venue filter to match whole name
 * Jan 30, 2014 2698       bclement    added default handle of username
 * Feb  3, 2014 2699       bclement    use preference handle default, display error if handle taken
 * Mar 06, 2014 2848       bclement    removed CollaborationConnection.joinTextOnlyVenue()
 * Apr 10, 2014 2937       bgonzale    Connect to the venue after the feed view is available
 *                                     to display messages.
 * Jun 16, 2014 3288       bclement    feed venue configuration changes
 * Oct 08, 2014 3705       bclement    moved venue joining code to CollaborationConnection
 * Mar 10, 2015 4238       njensen     null check in getSessionId()
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class DisplayFeedAction extends Action {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(DisplayFeedAction.class);

    public DisplayFeedAction() {
        super("Display Feed", SWT.TOGGLE);
        setImageDescriptor(IconUtil.getImageDescriptor(Activator.getDefault()
                .getBundle(), "feed.gif"));
        setEnabled(CollaborationConnection.getConnection() != null);
        IWorkbenchPage page = PlatformUI.getWorkbench()
                .getActiveWorkbenchWindow().getActivePage();
        page.addPartListener(new PartListener(this));
        if (isEnabled()) {
            String sessionId = getSessionId();
            if (sessionId != null) {
                IViewReference ref = page.findViewReference(SessionFeedView.ID,
                        sessionId);
                setChecked(ref != null);
            }
        }
    }

    /**
     * @return session ID of feed venue session or null if not found
     */
    private static String getSessionId() {
        CollaborationConnection connection = CollaborationConnection
                .getConnection();
        String sessionId = null;
        // connection can be null in rare cases
        if (connection != null) {
            for (ISession session : connection.getSessions()) {
                if (session instanceof IVenueSession) {
                    FeedVenueConfig config = FeedVenueConfigManager.getConfig();
                    if (((IVenueSession) session).getVenueName()
                            .equalsIgnoreCase(config.getName())) {
                        sessionId = session.getSessionId();
                    }
                }
            }
        }
        return sessionId;
    }

    /**
     * Attempt to join the feed venue on server using the handle set in
     * preferences. Displays an error and returns null if the join wasn't
     * successful.
     * 
     * @return the joined VenueSession; null if failed to join
     */
    private VenueSession joinFeedVenue() {
        CollaborationConnection connection = CollaborationConnection
                .getConnection();
        String defaultHandle = HandleUtil.getDefaultHandle();
        FeedVenueConfig config = FeedVenueConfigManager.getConfig();
        VenueId venueId = createVenueId(config);
        try {
            VenueSession session = connection.joinTextOnlyVenue(venueId,
                    defaultHandle);
            return session;
        } catch (CollaborationException e) {
            final String msg = e.getLocalizedMessage()
                    + "\n\nDefault handle options can be set in the Collaboration Preferences page.";
            VizApp.runAsync(new Runnable() {
                @Override
                public void run() {
                    Shell shell = new Shell(Display.getCurrent());
                    MessageDialog.openError(shell,
                            "Unable to join collaboration feed", msg);
                }
            });
            return null;
        }
    }

    /**
     * Create venue ID using configuration and the server name of the XMPP
     * server currently connected
     * 
     * @param config
     * @return
     */
    private static VenueId createVenueId(FeedVenueConfig config) {
        CollaborationConnection conn = CollaborationConnection.getConnection();
        XMPPConnection xmpp = conn.getXmppConnection();
        return new VenueId(config.getSubdomain(), xmpp.getServiceName(),
                config.getName());
    }

    @Override
    public void run() {
        CollaborationConnection connection = CollaborationConnection
                .getConnection();
        FeedVenueConfig config = FeedVenueConfigManager.getConfig();
        if (!connection.venueExistsOnServer(config.getSubdomain(),
                config.getName())) {
            statusHandler.info("Feed venue doesn't exist on server: "
                    + config.getName());
            setChecked(false);
            return;
        }

        if (isChecked()) {
            VenueSession session = joinFeedVenue();
            if (session == null) {
                // we couldn't join, stop action
                setChecked(false);
                return;
            }
            // handle if it is clicked to close or open the view as
            // necessary
            CaveWorkbenchPageManager page = CaveWorkbenchPageManager
                    .getActiveInstance();
            try {
                page.showView(SessionFeedView.ID, session.getSessionId(),
                        IWorkbenchPage.VIEW_ACTIVATE);
                // Connect to the room after opening the feed view.
                session.connectToRoom();
            } catch (PartInitException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Unable to join collaboration feed", e);
            } catch (CollaborationException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Unable to join collaboration feed", e);
            }
        } else {
            String sessionId = getSessionId();
            // handle if it is clicked to close or open the view as
            // necessary
            CaveWorkbenchPageManager page = CaveWorkbenchPageManager
                    .getActiveInstance();
            IViewReference ref = page.findViewReference(SessionFeedView.ID,
                    sessionId);
            if (ref != null) {
                page.hideView(ref);
            }
        }
    }

    private static class PartListener implements IPartListener {

        private final Reference<Action> actionRef;

        public PartListener(DisplayFeedAction action) {
            actionRef = new WeakReference<Action>(action);
        }

        private void clean(IWorkbenchPart part) {
            if (actionRef.get() == null) {
                part.getSite().getPage().removePartListener(this);
            }
        }

        private void setChecked(boolean checked) {
            Action action = actionRef.get();
            if (action != null) {
                action.setChecked(checked);
            }
        }

        @Override
        public void partActivated(IWorkbenchPart part) {
            clean(part);
        }

        @Override
        public void partBroughtToTop(IWorkbenchPart part) {
            clean(part);
        }

        @Override
        public void partClosed(IWorkbenchPart part) {
            clean(part);
            if (part instanceof SessionFeedView) {
                SessionFeedView view = (SessionFeedView) part;
                if (view.getRoom().equals(getSessionId())) {
                    setChecked(false);
                }
            }
        }

        @Override
        public void partDeactivated(IWorkbenchPart part) {
            clean(part);
        }

        @Override
        public void partOpened(IWorkbenchPart part) {
            clean(part);
            if (part instanceof SessionFeedView) {
                SessionFeedView view = (SessionFeedView) part;
                if (view.getRoom().equals(getSessionId())) {
                    setChecked(true);
                }
            }
        }

    }

}
