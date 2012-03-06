package com.raytheon.uf.viz.collaboration.ui.session;

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

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.ActionContributionItem;
import org.eclipse.jface.action.IToolBarManager;

import com.raytheon.uf.viz.collaboration.data.CollaborationDataManager;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 1, 2012            rferrel     Initial creation
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */
public class CollaborationSessionView extends SessionView {
    public static final String ID = "com.raytheon.uf.viz.collaboration.CollaborationSession";

    private static final String COLLABORATION_SESSION_IMAGE_NAME = "add_correction.gif";

    private Action switchDataProviderAction;

    private Action switchLeaderAction;

    public void createSwitchDataProviderAction() {
        switchDataProviderAction = new Action() {
            public void run() {
                switchDataProvider();
            }
        };
        // TODO find image and use instead of text
        // switchDataProviderAction.setImageDescriptor(CollaborationUtils
        // .getImageDescriptor("browser.gif"));
        switchDataProviderAction.setText("Data");
        switchDataProviderAction.setToolTipText("Switch Data Provider Request");
    }

    public void createSwitchLeaderAction() {
        switchLeaderAction = new Action() {
            public void run() {
                switchLeader();
            }
        };
        // TODO find image and use instead of text
        // switchLeaderAction.setImageDescriptor(CollaborationUtils
        // .getImageDescriptor("browser.gif"));
        switchLeaderAction.setText("Leader");
        switchLeaderAction.setToolTipText("Switch Leader Request");
    }

    protected void createToolBar() {
        IToolBarManager mgr = getViewSite().getActionBars().getToolBarManager();
        createSwitchDataProviderAction();
        createSwitchLeaderAction();
        ActionContributionItem item = null;
        item = new ActionContributionItem(switchDataProviderAction);
        item.setMode(ActionContributionItem.MODE_FORCE_TEXT);
        mgr.add(item);
        item = new ActionContributionItem(switchLeaderAction);
        item.setMode(ActionContributionItem.MODE_FORCE_TEXT);
        mgr.add(switchLeaderAction);
        // item = new ActionContributionItem(sendMessageAction);
        // item.setMode(ActionContributionItem.MODE_FORCE_TEXT);
        super.createToolBar();
    }

    public void switchDataProvider() {
        System.out.println("Send switchDataProvider request.");
    }

    public void switchLeader() {
        System.out.println("Send switchLeader request");
    }

    @Override
    protected String getSessionImageName() {
        return COLLABORATION_SESSION_IMAGE_NAME;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.collaboration.ui.session.SessionView#sendMessage()
     */
    @Override
    public void sendMessage() {
        String message = null;
        message = composeText.getText().trim();
        composeText.setText("");
        composeText.setCaretOffset(0);
        if (message.length() == 0) {
            // Do not send empty messages.
            return;
        }
        CollaborationDataManager.getInstance().getSession(sessionId)
                .sendMessageToVenue(message);
    }
    // @Override
    // public void dispose() {
    // super.dispose();
    // }
}
