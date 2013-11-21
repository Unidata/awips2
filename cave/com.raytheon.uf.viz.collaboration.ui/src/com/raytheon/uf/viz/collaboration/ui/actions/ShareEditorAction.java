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

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.ActionContributionItem;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.action.IMenuCreator;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Menu;

import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.collaboration.comm.identity.CollaborationException;
import com.raytheon.uf.viz.collaboration.comm.identity.ISharedDisplaySession;
import com.raytheon.uf.viz.collaboration.comm.identity.info.IVenueInfo;
import com.raytheon.uf.viz.collaboration.display.data.SessionContainer;
import com.raytheon.uf.viz.collaboration.display.data.SharedDisplaySessionMgr;
import com.raytheon.uf.viz.collaboration.display.roles.dataprovider.SharedEditorsManager;
import com.raytheon.uf.viz.collaboration.ui.Activator;
import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.actions.ContributedEditorMenuAction;
import com.raytheon.viz.ui.editor.AbstractEditor;

/**
 * Action to share an editor with a chosen session
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 21, 2012            mnash     Initial creation
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class ShareEditorAction extends ContributedEditorMenuAction implements
        IMenuCreator {

    private Menu menu;

    public ShareEditorAction() {
        super("Share with", IAction.AS_DROP_DOWN_MENU);
    }

    @Override
    public boolean shouldBeVisible() {
        return getActiveSharableEditor() != null && getSessions().size() > 0;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.action.IMenuCreator#dispose()
     */
    @Override
    public void dispose() {
        if (menu != null) {
            menu.dispose();
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.action.IMenuCreator#getMenu(org.eclipse.swt.widgets
     * .Control)
     */
    @Override
    public Menu getMenu(Control parent) {
        if (menu != null) {
            menu.dispose();
        }

        menu = new Menu(parent);

        fillMenu(menu);
        return menu;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.action.IMenuCreator#getMenu(org.eclipse.swt.widgets
     * .Menu)
     */
    @Override
    public Menu getMenu(Menu parent) {
        if (menu != null) {
            menu.dispose();
        }

        menu = new Menu(parent);

        fillMenu(menu);

        return menu;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.action.Action#getMenuCreator()
     */
    @Override
    public IMenuCreator getMenuCreator() {
        return this;
    }

    /**
     * @param menu2
     */
    private void fillMenu(Menu menu) {
        final AbstractEditor editor = getActiveSharableEditor();
        if (editor != null) {
            List<ISharedDisplaySession> sessions = getSessions();
            for (final ISharedDisplaySession session : sessions) {
                String sessionName;
                try {
                    IVenueInfo sessionInfo = session.getVenue().getInfo();
                    sessionName = sessionInfo.getVenueDescription();
                } catch (CollaborationException e1) {
                    Activator.statusHandler.error(e1.getLocalizedMessage(), e1);
                    sessionName = session.getVenue().getName();
                }
                ActionContributionItem aci = new ActionContributionItem(
                        new Action(sessionName) {
                            @Override
                            public void run() {
                                try {
                                    SharedEditorsManager.getManager(session)
                                            .shareEditor(editor);
                                } catch (CollaborationException e) {
                                    Activator.statusHandler.handle(
                                            Priority.PROBLEM,
                                            e.getLocalizedMessage(), e);
                                }
                            }
                        });
                aci.fill(menu, -1);
            }
        }
    }

    private AbstractEditor getActiveSharableEditor() {
        AbstractEditor editor = EditorUtil
                .getActiveEditorAs(AbstractEditor.class);
        if (editor != null
                && SharedEditorsManager.isBeingShared(editor) == false) {
            return editor;
        }
        return null;
    }

    private List<ISharedDisplaySession> getSessions() {
        Collection<String> sessionIds = SharedDisplaySessionMgr
                .getActiveSessionIds();
        List<ISharedDisplaySession> sessions = new ArrayList<ISharedDisplaySession>();
        for (String sessionId : sessionIds) {
            SessionContainer container = SharedDisplaySessionMgr
                    .getSessionContainer(sessionId);
            if (container != null) {
                ISharedDisplaySession session = container.getSession();
                if (session != null
                        && session.getUserID() == session
                                .getCurrentDataProvider()) {
                    sessions.add(container.getSession());
                }
            }
        }
        return sessions;
    }
}
