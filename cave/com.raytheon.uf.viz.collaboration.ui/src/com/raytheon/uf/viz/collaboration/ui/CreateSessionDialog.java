package com.raytheon.uf.viz.collaboration.ui;

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

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.events.VerifyEvent;
import org.eclipse.swt.events.VerifyListener;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IPartListener;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.collaboration.comm.identity.CollaborationException;
import com.raytheon.uf.viz.collaboration.comm.identity.ISharedDisplaySession;
import com.raytheon.uf.viz.collaboration.comm.identity.IVenueSession;
import com.raytheon.uf.viz.collaboration.comm.identity.info.IVenueInfo;
import com.raytheon.uf.viz.collaboration.comm.identity.user.SharedDisplayRole;
import com.raytheon.uf.viz.collaboration.comm.provider.Tools;
import com.raytheon.uf.viz.collaboration.comm.provider.session.CollaborationConnection;
import com.raytheon.uf.viz.collaboration.comm.provider.session.PeerToPeerCommHelper;
import com.raytheon.uf.viz.collaboration.display.data.SharedDisplaySessionMgr;
import com.raytheon.uf.viz.collaboration.display.roles.dataprovider.ISharedEditorsManagerListener;
import com.raytheon.uf.viz.collaboration.display.roles.dataprovider.SharedEditorsManager;
import com.raytheon.uf.viz.collaboration.ui.editor.CollaborationEditor;
import com.raytheon.uf.viz.collaboration.ui.prefs.CollabPrefConstants;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.maps.display.MapRenderableDisplay;
import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;
import com.raytheon.viz.ui.editor.AbstractEditor;
import com.raytheon.viz.ui.editor.IMultiPaneEditor;

/**
 * Collaboration creation dialog for sessions.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 15, 2012            rferrel     Initial creation
 * Dec 19, 2013 2563       bclement    disable shared display option if not supported by server
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */
public class CreateSessionDialog extends CaveSWTDialog {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(CreateSessionDialog.class);

    private Text nameTF;

    private Text subjectTF;

    private Button sharedSessionDisplay;

    private Button inviteUsers;

    private StyledText inviteMessageTF;

    private Label inviteLabel;

    private IPartListener editorChangeListener;

    private ISharedEditorsManagerListener sharedEditorsListener;

    public CreateSessionDialog(Shell parentShell) {
        super(parentShell);
        setText("Create Session");
    }

    private Control createDialogArea(Composite parent) {
        Composite body = new Composite(parent, SWT.NONE);
        body.setLayout(new GridLayout(2, false));
        // body.setLayoutData(new GridData(GridData.GRAB_HORIZONTAL
        // | GridData.HORIZONTAL_ALIGN_FILL));
        Label label = null;
        label = new Label(body, SWT.NONE);
        label.setText("Name: ");
        nameTF = new Text(body, SWT.BORDER);
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.minimumWidth = 200;
        nameTF.setLayoutData(gd);
        nameTF.addVerifyListener(new VerifyListener() {

            @Override
            public void verifyText(VerifyEvent e) {
                if (" \t\"&'/,<>@".indexOf(e.character) >= 0) {
                    e.doit = false;
                    // Toolkit.getDefaultToolkit().beep();
                }
            }
        });

        label = new Label(body, SWT.NONE);
        label.setText("Subject: ");
        subjectTF = new Text(body, SWT.BORDER);
        subjectTF.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
        sharedSessionDisplay = new Button(body, SWT.CHECK);
        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT, false, false);
        gd.horizontalSpan = 2;
        sharedSessionDisplay.setLayoutData(gd);
        updateSharedSessionDisplay();

        inviteUsers = new Button(body, SWT.CHECK);
        inviteUsers.setSelection(true);
        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT, false, false);
        gd.horizontalSpan = 2;
        inviteUsers.setLayoutData(gd);
        inviteUsers.setText("Invite Selected Users");
        // inviteUsers.setSelection(true);
        inviteUsers.setVisible(true);
        // label = new Label(body, SWT.NONE);
        // label.setText("");
        // label.setVisible(showInvite);
        inviteLabel = new Label(body, SWT.NONE);
        inviteLabel.setText("Message: ");
        inviteLabel.setToolTipText("Message to send to invited users");
        inviteMessageTF = new StyledText(body, SWT.BORDER | SWT.MULTI
                | SWT.WRAP | SWT.V_SCROLL);
        inviteMessageTF.setLayoutData(new GridData(GridData.FILL_BOTH));
        inviteMessageTF.pack();
        inviteMessageTF.setToolTipText("Message to send to invited users");
        Point p = inviteMessageTF.getSize();
        gd = (GridData) inviteMessageTF.getLayoutData();
        gd.heightHint = p.y * 3;
        inviteUsers.addSelectionListener(new SelectionListener() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                boolean selected = ((Button) e.widget).getSelection();
                inviteLabel.setVisible(selected);
                inviteMessageTF.setVisible(selected);
            }

            @Override
            public void widgetDefaultSelected(SelectionEvent e) {
                boolean selected = ((Button) e.widget).getSelection();
                inviteLabel.setVisible(selected);
                inviteMessageTF.setVisible(selected);

            }
        });
        inviteLabel.setVisible(true);
        inviteMessageTF.setVisible(true);

        IWorkbenchPage page = PlatformUI.getWorkbench()
                .getActiveWorkbenchWindow().getActivePage();
        if (page != null) {
            editorChangeListener = new IPartListener() {

                @Override
                public void partOpened(IWorkbenchPart part) {
                    // not used
                }

                @Override
                public void partDeactivated(IWorkbenchPart part) {
                    // not used
                }

                @Override
                public void partClosed(IWorkbenchPart part) {
                    // not used
                }

                @Override
                public void partBroughtToTop(IWorkbenchPart part) {
                    // not used
                }

                @Override
                public void partActivated(IWorkbenchPart part) {
                    if (part instanceof IEditorPart) {
                        updateSharedSessionDisplay();
                    }
                }
            };
            page.addPartListener(editorChangeListener);
        }

        AbstractEditor editor = EditorUtil
                .getActiveEditorAs(AbstractEditor.class);
        if (editor != null) {
            ISharedDisplaySession session = SharedEditorsManager
                    .getSharedEditorSession(editor);
            if (session != null) {
                sharedEditorsListener = new ISharedEditorsManagerListener() {
                    @Override
                    public void shareEditor(AbstractEditor editor) {
                        updateSharedSessionDisplay();
                    }

                    @Override
                    public void removeEditor(AbstractEditor editor) {
                        updateSharedSessionDisplay();
                    }
                };
                SharedEditorsManager.getManager(session).addListener(
                        sharedEditorsListener);
            }
        }
        return body;
    }

    private static boolean isShareable(IWorkbenchPart part) {
        if (part instanceof AbstractEditor) {
            AbstractEditor ed = (AbstractEditor) part;
            IDisplayPane pane = ed.getActiveDisplayPane();
            IRenderableDisplay display = pane.getRenderableDisplay();
            IDisplayPaneContainer container = display.getContainer();
            boolean isMapDisplay = display instanceof MapRenderableDisplay;
            boolean hasMultiplePanes = container instanceof IMultiPaneEditor
                    && ((IMultiPaneEditor) container).getNumberofPanes() > 1;
            return (isMapDisplay && !hasMultiplePanes);
        }
        return false;
    }

    private void updateSharedSessionDisplay() {
        IEditorPart editor = EditorUtil.getActiveEditorAs(IEditorPart.class);

        if (!sharedSessionDisplay.isDisposed()) {
            if (!serverSupportsSharing()) {
                disableShareOption(
                        "Not Supported By Server",
                        "Unable to create a shared display session because"
                                + " the server doesn't support shared display sessions.");
            } else if (editor instanceof CollaborationEditor) {
                disableShareOption("Client Session",
                        "Unable to create a shared display session because"
                                + " the active editor is a client session.");
            } else if (!isShareable(editor)) {
                disableShareOption("Not Shareable",
                        "Unable to create a shared display session because"
                                + " the active editor is not shareable.");
            } else if (editor != null
                    && editor instanceof AbstractEditor
                    && SharedEditorsManager
                            .isBeingShared((AbstractEditor) editor)) {
                disableShareOption("Already Shared",
                        "Unable to create a shared display session because"
                                + " the active editor is already "
                                + "in a shared display session.");
            } else {
                sharedSessionDisplay.setText("Create Shared Display Session");
                sharedSessionDisplay.getParent().setToolTipText("");
                this.enableOrDisableSharedDisplays();
            }
        }
    }

    /**
     * Disable create shared display checkbox
     * 
     * @param shortReason
     * @param longReason
     */
    private void disableShareOption(String shortReason, String longReason) {
        String text = String.format("Create Shared Display Session *%s*",
                shortReason);
        sharedSessionDisplay.setText(text);
        sharedSessionDisplay.setEnabled(false);
        sharedSessionDisplay.setSelection(false);
        sharedSessionDisplay.getParent().setToolTipText(longReason);
    }

    /**
     * @return true if the server supports shared display sessions
     */
    private boolean serverSupportsSharing() {
        return PeerToPeerCommHelper.getCollaborationHttpServer() != null;
    }

    @Override
    protected void disposed() {
        super.disposed();
        if (editorChangeListener != null) {
            IWorkbenchPage page = PlatformUI.getWorkbench()
                    .getActiveWorkbenchWindow().getActivePage();
            page.removePartListener(editorChangeListener);
        }
        if (sharedEditorsListener != null) {
            AbstractEditor editor = EditorUtil
                    .getActiveEditorAs(AbstractEditor.class);
            if (editor != null) {
                ISharedDisplaySession session = SharedEditorsManager
                        .getSharedEditorSession(editor);
                if (session != null) {
                    SharedEditorsManager.getManager(session).removeListener(
                            sharedEditorsListener);
                }
            }
        }
    }

    @Override
    protected void initializeComponents(Shell shell) {
        shell.setLayout(new GridLayout(1, false));
        createDialogArea(shell);
        createButtonBar(shell);
    }

    private void createButtonBar(Composite parent) {
        GridData gd = null;
        Composite bar = new Composite(parent, SWT.NONE);

        // set up to center buttons.
        bar.setLayout(new GridLayout(0, true));
        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        bar.setLayoutData(gd);
        createButton(bar, IDialogConstants.OK_ID, "Create", true);

        createButton(bar, IDialogConstants.CANCEL_ID,
                IDialogConstants.CANCEL_LABEL, false);
    }

    @Override
    protected void preOpened() {
        super.preOpened();
    }

    /**
     * Creates a new button with the given id.
     * <p>
     * The <code>Dialog</code> implementation of this framework method creates a
     * standard push button, registers it for selection events including button
     * presses, and registers default buttons with its shell. The button id is
     * stored as the button's client data. If the button id is
     * <code>IDialogConstants.CANCEL_ID</code>, the new button will be
     * accessible from <code>getCancelButton()</code>. If the button id is
     * <code>IDialogConstants.OK_ID</code>, the new button will be accesible
     * from <code>getOKButton()</code>. Note that the parent's layout is assumed
     * to be a <code>GridLayout</code> and the number of columns in this layout
     * is incremented. Subclasses may override.
     * </p>
     * 
     * @param parent
     *            the parent composite
     * @param id
     *            the id of the button (see <code>IDialogConstants.*_ID</code>
     *            constants for standard dialog button ids)
     * @param label
     *            the label from the button
     * @param defaultButton
     *            <code>true</code> if the button is to be the default button,
     *            and <code>false</code> otherwise
     * 
     * @return the new button
     * 
     * @see #getCancelButton
     * @see #getOKButton()
     */
    protected Button createButton(Composite parent, int id, String label,
            boolean defaultButton) {
        // increment the number of columns in the button bar
        ((GridLayout) parent.getLayout()).numColumns++;
        Button button = new Button(parent, SWT.PUSH);
        button.setText(label);
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.minimumWidth = 70;
        button.setLayoutData(gd);
        button.setData(new Integer(id));
        button.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                Integer val = (Integer) event.widget.getData();
                if (val != IDialogConstants.OK_ID) {
                    setReturnValue(null);
                    CreateSessionDialog.this.getShell().dispose();
                } else {
                    Text focusField = null;
                    List<String> errorMessages = new ArrayList<String>();
                    String subject = subjectTF.getText().trim();
                    String err = validateVenueName();
                    String name = nameTF.getText();
                    if (err != null) {
                        focusField = nameTF;
                        errorMessages.add(err);
                    }

                    if (focusField == null) {
                        CreateSessionData result = new CreateSessionData();
                        result.setName(name);
                        result.setSubject(subject);
                        result.setCollaborationSessioh(sharedSessionDisplay
                                .getSelection());
                        if (inviteUsers == null) {
                            result.setInviteUsers(false);
                        } else {
                            result.setInviteUsers(inviteUsers.getSelection());
                            result.setInviteMessage(inviteMessageTF.getText());
                        }

                        IVenueSession session = null;
                        try {
                            CollaborationConnection connection = CollaborationConnection
                                    .getConnection();
                            if (result.isCollaborationSession()) {
                                session = connection.createCollaborationVenue(
                                        result.getName(), result.getSubject());
                                ISharedDisplaySession displaySession = (ISharedDisplaySession) session;
                                SharedDisplaySessionMgr.joinSession(
                                        displaySession,
                                        SharedDisplayRole.DATA_PROVIDER, null);
                            } else {
                                session = connection.createTextOnlyVenue(
                                        result.getName(), result.getSubject());
                            }
                            result.setSessionId(session.getSessionId());
                            setReturnValue(result);
                            CreateSessionDialog.this.getShell().dispose();
                        } catch (CollaborationException ex) {
                            statusHandler.handle(Priority.ERROR,
                                    "Session Creation Error", ex);
                            event.doit = false;
                            setReturnValue(null);
                        }
                    } else {
                        StringBuilder sb = new StringBuilder();
                        String prefix = "";
                        for (String msg : errorMessages) {
                            sb.append(prefix).append(msg);
                            prefix = "\n";
                        }
                        statusHandler.handle(Priority.ERROR,
                                "Session Creation Error");
                        event.doit = false;
                        setReturnValue(null);
                        focusField.setFocus();
                        focusField.selectAll();
                    }
                }
            }
        });
        if (defaultButton) {
            Shell shell = parent.getShell();
            if (shell != null) {
                shell.setDefaultButton(button);
            }
        }
        return button;
    }

    private String validateVenueName() {
        String name = nameTF.getText().trim();
        nameTF.setText(name);
        String err = null;
        if (name.length() <= 0) {
            err = "Must have session name.";
        } else if (!Tools.isValidId(name)) {
            err = "Name contains invalid characters.";
        } else {
            Collection<IVenueInfo> info = CollaborationConnection
                    .getConnection().getVenueInfo();
            for (IVenueInfo i : info) {
                if (name.equals(i.getVenueName())) {
                    err = "Session already exists. Pick a different name.";
                    break;
                }
            }
        }
        return err;
    }

    private void enableOrDisableSharedDisplays() {
        boolean sharedSessionsEnabled = Activator
                .getDefault()
                .getPreferenceStore()
                .getBoolean(
                        CollabPrefConstants.HttpCollaborationConfiguration.P_SESSION_CONFIGURED);
        this.sharedSessionDisplay.setSelection(sharedSessionsEnabled);
        this.sharedSessionDisplay.setEnabled(sharedSessionsEnabled);
    }

}