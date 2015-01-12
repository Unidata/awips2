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
package com.raytheon.uf.viz.collaboration.display.roles;

import org.eclipse.swt.graphics.RGB;

import com.google.common.eventbus.Subscribe;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.collaboration.comm.identity.CollaborationException;
import com.raytheon.uf.viz.collaboration.comm.identity.ISharedDisplaySession;
import com.raytheon.uf.viz.collaboration.comm.identity.event.IVenueParticipantEvent;
import com.raytheon.uf.viz.collaboration.comm.identity.event.ParticipantEventType;
import com.raytheon.uf.viz.collaboration.comm.identity.invite.ColorPopulator;
import com.raytheon.uf.viz.collaboration.display.data.ColorChangeEvent;
import com.raytheon.uf.viz.collaboration.display.data.SessionColorManager;
import com.raytheon.uf.viz.collaboration.display.data.SharedDisplaySessionMgr;
import com.raytheon.uf.viz.collaboration.display.editor.ActivateRemoteDisplay;
import com.raytheon.uf.viz.collaboration.display.roles.dataprovider.SharedEditorsManager;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.editor.AbstractEditor;

/**
 * TODO: This class is in severe need of a refactor!
 * 
 * Handles the events of a shared display session that are specific to the Data
 * Provider role.
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 26, 2012            njensen     Initial creation
 * Feb 13, 2014 2751       bclement    VenueParticipant refactor
 * Feb 13, 2014 2751       njensen     Renamed container to displayContainer
 * Mar 06, 2014 2848       bclement    removed check for self from participantChanged
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class DataProviderEventController extends
        AbstractRoleEventController<SharedEditorsManager> {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(DataProviderEventController.class);

    public DataProviderEventController(ISharedDisplaySession session) {
        super(session);
    }


    @Subscribe
    public void participantChanged(IVenueParticipantEvent event) {
        /*
         * arrived events only trigger when others join the venue, no need to
         * check if the event is about us
         */
        if (event.getEventType().equals(ParticipantEventType.ARRIVED)) {
            try {
                AbstractEditor active = displayContainer
                        .getActiveSharedEditor();
                if (active != null) {
                    IDisplayPane activePane = active.getActiveDisplayPane();
                    if (activePane != null) {
                        ActivateRemoteDisplay arde = new ActivateRemoteDisplay();
                        arde.setDisplayId(displayContainer
                                .getDisplayId(activePane.getRenderableDisplay()));
                        session.sendObjectToPeer(event.getParticipant(), arde);
                    }
                }

                // new color for each user
                SessionColorManager scm = SharedDisplaySessionMgr
                        .getSessionContainer(session.getSessionId())
                        .getColorManager();
                RGB color = scm.getColorForUser(event.getParticipant());

                ColorChangeEvent cce = new ColorChangeEvent(
                        event.getParticipant(), color);
                session.sendObjectToPeer(event.getParticipant(),
                        new ColorPopulator(scm.getColors()));
                session.sendObjectToVenue(cce);
            } catch (CollaborationException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error sending initialization data to new participant "
                                + event.getParticipant().getName(), e);
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.collaboration.ui.role.AbstractRoleEventController
     * #startup()
     */
    @Override
    public void startup() {
        super.startup();
        AbstractEditor active = EditorUtil
                .getActiveEditorAs(AbstractEditor.class);
        if (active != null
                && SharedEditorsManager.isBeingShared(active) == false) {
            try {
                displayContainer.shareEditor(active);
            } catch (CollaborationException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.collaboration.display.roles.AbstractRoleEventController
     * #createDisplayContainer()
     */
    @Override
    protected SharedEditorsManager createDisplayContainer() {
        return SharedEditorsManager.getManager(session);
    }

}
