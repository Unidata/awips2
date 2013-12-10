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
import org.eclipse.swt.SWT;
import org.eclipse.ui.IPartListener;
import org.eclipse.ui.IViewReference;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.collaboration.comm.identity.CollaborationException;
import com.raytheon.uf.viz.collaboration.comm.identity.ISession;
import com.raytheon.uf.viz.collaboration.comm.identity.IVenueSession;
import com.raytheon.uf.viz.collaboration.comm.provider.session.CollaborationConnection;
import com.raytheon.uf.viz.collaboration.ui.Activator;
import com.raytheon.uf.viz.collaboration.ui.session.SessionFeedView;
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
 * Jul 5, 2012            bsteffen     Initial creation
 * Dec 19, 2013 2563      bclement     added check for feed venue existence
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class DisplayFeedAction extends Action {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(DisplayFeedAction.class);

    // TODO make this configurable?
    public static final String FEED_VENUE = "nws-collaboration";

    public DisplayFeedAction() {
        super("Display Feed", SWT.TOGGLE);
        setImageDescriptor(IconUtil.getImageDescriptor(Activator.getDefault()
                .getBundle(), "feed.gif"));
        setEnabled(CollaborationConnection.getConnection() != null);
        IWorkbenchPage page = PlatformUI.getWorkbench()
                .getActiveWorkbenchWindow().getActivePage();
        page.addPartListener(new PartListener(this));
        if (isEnabled()) {
            String sessionId = getSessionId(false);
            if (sessionId != null) {
                IViewReference ref = page.findViewReference(SessionFeedView.ID,
                        sessionId);
                setChecked(ref != null);
            }
        }
    }

    private static String getSessionId(boolean create) {
        CollaborationConnection connection = CollaborationConnection
                .getConnection();
        String sessionId = null;
        for (ISession session : connection.getSessions()) {
            if (session instanceof IVenueSession) {
                if (((IVenueSession) session).getVenue().getName()
                        .startsWith(FEED_VENUE)) {
                    sessionId = session.getSessionId();
                }
            }
        }
        if (sessionId == null && create) {
            try {
                IVenueSession session = connection
                        .joinTextOnlyVenue(FEED_VENUE);
                sessionId = session.getSessionId();
            } catch (CollaborationException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Unable to join the collaboration feed", e);
            }
        }
        return sessionId;
    }

    @Override
    public void run() {
        CollaborationConnection connection = CollaborationConnection
                .getConnection();
        if (!connection.venueExistsOnServer(FEED_VENUE)) {
            statusHandler.info("Feed venue doesn't exist on server: "
                    + FEED_VENUE);
            return;
        }

        // handle if it is clicked to close or open the view as
        // necessary
        CaveWorkbenchPageManager page = CaveWorkbenchPageManager
                .getActiveInstance();
        String sessionId = getSessionId(isChecked());
        if (!isChecked()) {
            IViewReference ref = page.findViewReference(SessionFeedView.ID,
                    sessionId);
            if (ref != null) {
                page.hideView(ref);
            }
        } else {
            try {
                page.showView(SessionFeedView.ID, sessionId,
                        IWorkbenchPage.VIEW_ACTIVATE);
            } catch (PartInitException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Unable to join collaboration feed", e);
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
                if (view.getRoom().equals(getSessionId(false))) {
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
                if (view.getRoom().equals(getSessionId(false))) {
                    setChecked(true);
                }
            }
        }

    }

}
