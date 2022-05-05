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

package com.raytheon.uf.viz.d2d.ui.dialogs.procedures;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import org.eclipse.jface.dialogs.InputDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IPartListener2;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchListener;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPartReference;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.SaveableOutputStream;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.DescriptorMap;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.VizConstants;
import com.raytheon.uf.viz.core.drawables.AbstractDescriptor;
import com.raytheon.uf.viz.core.drawables.AbstractRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.globals.VizGlobalsManager;
import com.raytheon.uf.viz.core.maps.display.MapRenderableDisplay;
import com.raytheon.uf.viz.core.procedures.Bundle;
import com.raytheon.uf.viz.core.procedures.Procedure;
import com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.d2d.core.procedures.AlterBundleFactory;
import com.raytheon.uf.viz.d2d.core.procedures.IAlterBundleContributor;
import com.raytheon.uf.viz.d2d.ui.dialogs.procedures.ProcedureComm.BundlePair;
import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.HistoryList;
import com.raytheon.viz.ui.IRenameablePart;
import com.raytheon.viz.ui.UiUtil;
import com.raytheon.viz.ui.actions.SaveBundle;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;
import com.raytheon.viz.ui.dialogs.ICloseCallback;
import com.raytheon.viz.ui.dialogs.localization.VizLocalizationFileListDlg;
import com.raytheon.viz.ui.editor.AbstractEditor;
import com.raytheon.viz.ui.views.PartAdapter2;

/**
 *
 * Dialog for loading or modifying procedures.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer     Description
 * ------------- -------- ------------ -----------------------------------------
 *                                     Initial Creation
 * Oct 16, 2012  1229     rferrel      Changes for non-blocking AlterBundleDlg.
 * Oct 16, 2012  1229     rferrel      Changes to have displayDialog method.
 * Oct 16, 2012  1229     rferrel      Changes for non-blocking
 *                                     VizLocalizationFileListDlg.
 * Jan 15, 2013  15699    D. Friedman  Prompt for save when close button
 *                                     clicked.
 * Jan 16, 2013  15367    D. Friedman  Enable save button for Up/Down changes.
 * Feb 25, 2013  1640     bsteffen     Dispose old display in BundleLoader
 * Jun 07, 2013  2074     mnash        Remove resource if doesn't instantiate
 *                                     correctly
 * Aug 11, 2014  3480     bclement     added info logging when procedure is
 *                                     loaded
 * Jan 06, 2015  3879     nabowle      Disallow copy-in when the view is empty.
 * Mar 02, 2015  4204     njensen      Copy In uses tab name if applicable
 * Mar 12, 2015  4204     njensen      Ensure renamed bundle goes into tab name
 *                                     on next load
 * Apr 08, 2015  4185     mapeters     Disable Copy In when not applicable for
 *                                     active editor
 * Jun 02, 2015  4401     bkowal       Updated to use {@link
 *                                     VizLocalizationFileListDlg}.
 * Jan 29, 2016  5289     tgurney      Add missing minimize/maximize buttons in
 *                                     trim
 * Feb 12, 2016  5242     dgilling     Remove calls to deprecated Localization
 *                                     APIs.
 * Feb 24, 2017  6116     mapeters     Don't hardcode size of buttons
 * Nov 13, 2017  6044     mapeters     Added null checks in disposed()
 * Feb 20, 2018  6883     tgurney      Prompt for save on CAVE close
 * Jul 25, 2018  6748     randerso     Fixed to work with changes in
 *                                     CaveSWTDialog.shouldClose(). Code
 *                                     cleanup.
 * Sep 21, 2018  7470     dgilling     Force LoopTool state update when loading
 *                                     bundles.
 * Dec 13, 2018  6883     tgurney      Remove workbench listener when the dialog is closed
 *
 * </pre>
 *
 * @author unknown
 */
public class ProcedureDlg extends CaveSWTDialog implements IWorkbenchListener {

    private final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ProcedureDlg.class);

    public static final String ORIGINAL = "Original Location";

    public static final String CURRENT = "Current Location";

    public static final String PROCEDURES_DIR = "/procedures";

    private static final String FIRST = "First";

    private static final String NEXT = "Next";

    private static final Map<String, ProcedureDlg> openDialogs = new HashMap<>();

    private Font font;

    private List dataList;

    private Button currentRdo;

    private Button firstNextBtn;

    private Button loadBtn;

    private Button alterBtn;

    private Button copyInBtn;

    private Button copyOutBtn;

    private Button saveBtn;

    private String fileName;

    private Boolean frozen;

    private boolean saved = true;

    private static Shell lastShell = null;

    private static Point lastPoint = null;

    private static int initialPosX = -1;

    private static int initialPosY = -1;

    private final java.util.List<BundlePair> bundles;

    private AlterBundleDlg alterDlg;

    private ProcedureListFileDlg saveAsDlg;

    private IPartListener2 activeEditorListener;

    private ProcedureDlg(String fileName, Procedure p, Shell parent) {
        // Win32
        super(parent, SWT.DIALOG_TRIM | SWT.RESIZE | SWT.MIN | SWT.MAX,
                CAVE.INDEPENDENT_SHELL | CAVE.DO_NOT_BLOCK);
        String titleBarStr = fileName;
        if (titleBarStr == null) {
            titleBarStr = "(untitled)";
        }
        setText("Procedure - " + titleBarStr);

        this.fileName = fileName;
        this.bundles = new ArrayList<>();
        frozen = false;
        if (p != null && p.getBundles() != null) {
            Bundle[] bund = p.getBundles();
            for (Bundle b : bund) {
                // remove any bad resource pairs from each display
                for (AbstractRenderableDisplay display : b.getDisplays()) {
                    ResourceList rList = display.getDescriptor()
                            .getResourceList();
                    // modify the resource list so that we remove any null
                    // resource datas
                    for (ResourcePair rp : rList) {
                        if (rp.getResourceData() == null) {
                            rList.remove(rp);
                        }
                    }
                }
                // Check to see if frozen
                for (AbstractRenderableDisplay display : b.getDisplays()) {
                    ResourceList rList = display.getDescriptor()
                            .getResourceList();
                    for (ResourcePair rp : rList) {
                        if (rp.getResourceData().isFrozen()) {
                            frozen = true;
                            break;
                        }
                    }
                    if (frozen) {
                        break;
                    }
                }
                BundlePair bp = new BundlePair();
                try {
                    bp.xml = b.toXML();
                } catch (VizException e) {
                    statusHandler.error(
                            "Error marshalling bundle to XML: " + b.getName(),
                            e);
                }
                bp.name = (b.getName() != null ? b.getName() : " ");
                this.bundles.add(bp);
            }
        }

        PlatformUI.getWorkbench().addWorkbenchListener(this);
    }

    @Override
    protected Layout constructShellLayout() {
        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, true);
        mainLayout.marginHeight = 1;
        mainLayout.marginWidth = 1;
        return mainLayout;
    }

    @Override
    protected void disposed() {
        font.dispose();
        IWorkbench workbench = PlatformUI.getWorkbench();
        workbench.removeWorkbenchListener(this);
        if (activeEditorListener != null) {
            IWorkbenchWindow window = workbench.getActiveWorkbenchWindow();
            if (window != null) {
                IWorkbenchPage page = window.getActivePage();
                if (page != null) {
                    page.removePartListener(activeEditorListener);
                }
            }
        }
        synchronized (openDialogs) {
            openDialogs.remove(fileName);
        }
    }

    @Override
    protected synchronized void preOpened() {
        Point currentPoint = null;
        if (lastShell != null) {
            if (!lastShell.isDisposed()) {
                currentPoint = lastShell.getLocation();
            } else {
                currentPoint = lastPoint;
            }

            if (initialPosX == -1) {
                initialPosX = currentPoint.x;
            }
            if (initialPosY == -1) {
                initialPosY = currentPoint.y;
            }

            Composite parent = shell.getParent();
            if (parent instanceof Shell) {
                Shell parentShell = (Shell) parent;
                int parentSizeX = parentShell.getSize().x;
                int parentSizeY = parentShell.getSize().y;

                if (parentSizeY > (currentPoint.y + shell.getSize().y)) {
                    shell.setLocation(currentPoint.x, currentPoint.y + 90);
                } else if (parentSizeX > (currentPoint.x + shell.getSize().x)) {
                    shell.setLocation(currentPoint.x + 100, initialPosY);
                } else {
                    initialPosX += 30;
                    shell.setLocation(initialPosX, initialPosY);
                }
            }
        }
        lastShell = shell;
        lastPoint = shell.getLocation();
    }

    @Override
    protected void opened() {
        addHistoryAndShellListeners();
    }

    @Override
    protected void initializeComponents(Shell shell) {
        font = new Font(shell.getDisplay(), "Courier", 11, SWT.BOLD);

        createListBoxAndControls();
        createBottomControls();
    }

    private void addHistoryAndShellListeners() {
        final ProcedureDlg thisDlg = this;

        final ProcedureComm.ICopyOutStateChangeListener changeListener = new ProcedureComm.ICopyOutStateChangeListener() {

            @Override
            public void copyOutStateChange() {
                VizApp.runAsync(new Runnable() {

                    @Override
                    public void run() {
                        if (!copyOutBtn.isDisposed()) {
                            copyOutBtn.setEnabled(ProcedureComm.getInstance()
                                    .getCopyListenerCount() > 1
                                    && bundles != null && !bundles.isEmpty()
                                    && dataList.getSelectionIndex() >= 0);
                        }
                    }
                });
            }
        };

        ProcedureComm.getInstance().addCopyOutStateListener(changeListener);
        final ProcedureComm.ICopyOutListener copyOutListener = new ProcedureComm.ICopyOutListener() {

            @Override
            public void copyOut(BundlePair b, Object src) {
                if (src != thisDlg) {
                    bundles.add(b);
                    saved = false;
                    saveBtn.setEnabled(true);
                }

                resyncProcedureAndList();
            }

        };

        ProcedureComm.getInstance().addCopyOutListener(copyOutListener);

        addListener(SWT.Dispose, new Listener() {
            @Override
            public void handleEvent(Event event) {
                ProcedureComm.getInstance()
                        .removeCopyOutListener(copyOutListener);
                ProcedureComm.getInstance()
                        .removeCopyOutStateListener(changeListener);
            }
        });
    }

    private void resyncProcedureAndList() {
        VizApp.runAsync(new Runnable() {

            @Override
            public void run() {
                if (!dataList.isDisposed()) {
                    if (bundles != null && !bundles.isEmpty()) {
                        String[] list = new String[bundles.size()];
                        int i = 0;
                        for (BundlePair b : bundles) {
                            list[i] = (b.name != null ? b.name : " ");
                            i++;
                        }
                        int currIdx = dataList.getSelectionIndex();
                        dataList.setItems(list);
                        if (!firstNextBtn.isEnabled()) {
                            if (currIdx == -1 && firstNextBtn.getText()
                                    .toString().equals(FIRST)) {
                                firstNextBtn.setEnabled(true);
                                alterBtn.setEnabled(false);
                                loadBtn.setEnabled(false);
                            }
                        }
                        if (currIdx > -1) {
                            if (currIdx > dataList.getItemCount() - 1) {
                                currIdx = dataList.getItemCount() - 1;
                            }
                            dataList.setSelection(currIdx);
                        }
                    } else {
                        dataList.setItems(new String[0]);
                        firstNextBtn.setEnabled(false);
                        firstNextBtn.setText(FIRST);
                        loadBtn.setEnabled(false);
                        alterBtn.setEnabled(false);
                    }
                }
            }

        });

    }

    private void saveProcedure(boolean closeAfterSave) {
        try {
            Procedure procedure = new Procedure();
            BundlePair[] bp = bundles.toArray(new BundlePair[bundles.size()]);
            Bundle[] bundlesToSave = new Bundle[bp.length];
            int i = 0;
            for (BundlePair b : bp) {
                Bundle bundle = Bundle.unmarshalBundle(b.xml, null);

                if (!frozen) {
                    for (AbstractRenderableDisplay display : bundle
                            .getDisplays()) {
                        for (ResourcePair rp : display.getDescriptor()
                                .getResourceList()) {
                            if (rp.getResourceData() != null && rp
                                    .getResourceData() instanceof AbstractRequestableResourceData) {
                                ((AbstractRequestableResourceData) rp
                                        .getResourceData()).setFrozenTime(null);
                            }
                        }
                    }
                }
                bundlesToSave[i] = bundle;
                bundlesToSave[i].setName(b.name);
                i++;
            }
            procedure.setBundles(bundlesToSave);

            IPathManager pm = PathManagerFactory.getPathManager();
            LocalizationContext context = pm.getContext(
                    LocalizationType.CAVE_STATIC, LocalizationLevel.USER);
            ILocalizationFile localizationFile = pm.getLocalizationFile(context,
                    PROCEDURES_DIR + IPathManager.SEPARATOR + fileName);
            String procedureXML = procedure.toXML();
            try (SaveableOutputStream outStream = localizationFile
                    .openOutputStream()) {
                outStream.write(procedureXML.getBytes());
                outStream.save();
            }

            shell.setText("Procedure - " + fileName);
            saved = true;
            saveBtn.setEnabled(false);

            if (closeAfterSave) {
                MessageDialog.openInformation(shell, "Procedure Saved",
                        "Procedure was saved to " + fileName + ".");
                this.close();
            }

        } catch (Exception e) {
            final String errMsg = "Error occurred during procedure save.";
            statusHandler.handle(Priority.PROBLEM, errMsg, e);
        }
    }

    private void createListBoxAndControls() {
        Composite listComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(2, false);
        listComp.setLayout(gl);
        listComp.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.widthHint = 175;
        gd.heightHint = 125;
        dataList = new List(listComp,
                SWT.BORDER | SWT.SINGLE | SWT.V_SCROLL | SWT.H_SCROLL);
        dataList.setLayoutData(gd);
        dataList.setFont(font);
        dataList.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                loadBtn.setEnabled(true);
                alterBtn.setEnabled(true);
                firstNextBtn.setText(NEXT);
                ProcedureComm.getInstance().refreshCopyOutState();
            }
        });
        dataList.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseDoubleClick(MouseEvent e) {
                load();
            }
        });

        Composite listControlComp = new Composite(listComp, SWT.NONE);
        RowLayout rl = new RowLayout(SWT.VERTICAL);
        rl.spacing = 20;
        rl.pack = false;
        listControlComp.setLayout(rl);

        Button upBtn = new Button(listControlComp, SWT.PUSH);
        upBtn.setText("Up");
        upBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                ProcedureComm.getInstance().refreshCopyOutState();
                int idx = dataList.getSelectionIndex();
                if (idx < 0) {
                    return;
                }

                BundlePair b = bundles.remove(idx);
                if (idx - 1 < 0) {
                    idx = bundles.size() + 1;
                }
                bundles.add(idx - 1, b);
                dataList.setSelection(idx - 1);
                resyncProcedureAndList();
                saved = false;
                saveBtn.setEnabled(true);
            }
        });

        Button downBtn = new Button(listControlComp, SWT.PUSH);
        downBtn.setText("Down");
        downBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                ProcedureComm.getInstance().refreshCopyOutState();
                int idx = dataList.getSelectionIndex();
                if (idx < 0) {
                    return;
                }

                BundlePair b = bundles.remove(idx);
                if (idx + 1 > bundles.size()) {
                    idx = -1;
                }
                bundles.add(idx + 1, b);
                dataList.setSelection(idx + 1);
                resyncProcedureAndList();
                saved = false;
                saveBtn.setEnabled(true);
            }
        });

        Button renameBtn = new Button(listControlComp, SWT.PUSH);
        renameBtn.setText("Rename...");
        renameBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                int idx = dataList.getSelectionIndex();
                if (idx < 0) {
                    return;
                }

                BundlePair b = bundles.get(idx);
                boolean done = false;
                while (!done) {
                    InputDialog id = new InputDialog(shell, "Enter Bundle Name",
                            "Enter bundle name:", b.name, null);
                    if (Window.OK == id.open()) {
                        String newName = id.getValue();

                        if (newName != null && !newName.trim().isEmpty()) {
                            b.name = newName;
                            resyncProcedureAndList();
                            saved = false;
                            saveBtn.setEnabled(true);
                            done = true;
                        } else {
                            MessageDialog.openWarning(shell,
                                    "Error Setting Name",
                                    "The bundle name must contain at least one alphanumeric character.");
                        }
                    } else {
                        done = true;
                    }
                }
            }
        });
        resyncProcedureAndList();
    }

    private void createBottomControls() {
        Composite buttonComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(3, true);
        gl.horizontalSpacing = 10;
        buttonComp.setLayout(gl);
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        buttonComp.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalSpan = 2;
        Button originalRdo = new Button(buttonComp, SWT.RADIO);
        originalRdo.setText("Original");
        originalRdo.setSelection(true);
        originalRdo.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        currentRdo = new Button(buttonComp, SWT.RADIO);
        currentRdo.setText("Current");
        currentRdo.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        firstNextBtn = new Button(buttonComp, SWT.PUSH);
        firstNextBtn.setText(FIRST);
        firstNextBtn.setLayoutData(gd);
        firstNextBtn.setEnabled(false);
        firstNextBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                String text = firstNextBtn.getText().toString();
                if (text.equals(FIRST)) {
                    dataList.setSelection(0);
                    firstNextBtn.setText(NEXT);
                    loadBtn.setEnabled(true);
                    alterBtn.setEnabled(true);
                    load();
                    ProcedureComm.getInstance().refreshCopyOutState();
                } else if (text.equals(NEXT)) {
                    int nextIdx = (dataList.getSelectionIndex() + 1)
                            % dataList.getItemCount();
                    dataList.setSelection(nextIdx);
                    load();
                    ProcedureComm.getInstance().refreshCopyOutState();
                }
            }
        });

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        loadBtn = new Button(buttonComp, SWT.PUSH);
        loadBtn.setText("Load");
        loadBtn.setLayoutData(gd);
        loadBtn.setEnabled(false);
        loadBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                load();
            }

        });

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        alterBtn = new Button(buttonComp, SWT.PUSH);
        alterBtn.setText("Alter...");
        alterBtn.setLayoutData(gd);
        alterBtn.setEnabled(false);
        alterBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                alterBundle();
            }
        });

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        copyInBtn = new Button(buttonComp, SWT.PUSH);
        copyInBtn.setText("Copy In");
        copyInBtn.setLayoutData(gd);
        copyInBtn.setEnabled(
                EditorUtil.getActiveEditor() instanceof AbstractEditor);

        activeEditorListener = new PartAdapter2() {
            @Override
            public void partDeactivated(IWorkbenchPartReference partRef) {
                updateCopyInBtnEnabled(partRef);
            }

            @Override
            public void partActivated(IWorkbenchPartReference partRef) {
                updateCopyInBtnEnabled(partRef);
            }

            private void updateCopyInBtnEnabled(
                    IWorkbenchPartReference partRef) {
                copyInBtn.setEnabled(
                        EditorUtil.getActiveEditor() instanceof AbstractEditor);
            }
        };
        IWorkbenchPage page = PlatformUI.getWorkbench()
                .getActiveWorkbenchWindow().getActivePage();
        page.addPartListener(activeEditorListener);

        copyInBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                copyIn();
            }
        });

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        copyOutBtn = new Button(buttonComp, SWT.PUSH);
        copyOutBtn.setText("Copy Out");
        copyOutBtn.setLayoutData(gd);
        if (ProcedureComm.getInstance().getCopyListenerCount() > 1
                && bundles != null && !bundles.isEmpty()
                && dataList.getSelectionIndex() >= 0) {
            copyOutBtn.setEnabled(true);
        } else {
            copyOutBtn.setEnabled(false);
        }
        final ProcedureDlg thisDlg = this;

        copyOutBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {

                int idx = dataList.getSelectionIndex();
                if (idx > -1) {

                    BundlePair b = bundles.get(idx);

                    ProcedureComm.getInstance().copyOut(b, thisDlg);
                }
            }
        });

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Button deleteBtn = new Button(buttonComp, SWT.PUSH);
        deleteBtn.setText("Delete");
        deleteBtn.setLayoutData(gd);
        deleteBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                saved = false;
                saveBtn.setEnabled(true);
                int idx = dataList.getSelectionIndex();
                if (idx < 0) {
                    return;
                }
                if (!bundles.isEmpty()) {
                    bundles.remove(idx);
                }
                resyncProcedureAndList();
                ProcedureComm.getInstance().refreshCopyOutState();
            }
        });

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        saveBtn = new Button(buttonComp, SWT.PUSH);
        saveBtn.setEnabled(false);
        saveBtn.setText("Save");
        saveBtn.setLayoutData(gd);
        saveBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                handleSaveRequest(false);
            }
        });

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Button saveAsBtn = new Button(buttonComp, SWT.PUSH);
        saveAsBtn.setText("Save As...");
        saveAsBtn.setLayoutData(gd);
        saveAsBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                showSaveAsDlg(false);
            }
        });
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Button closeBtn = new Button(buttonComp, SWT.PUSH);
        closeBtn.setText("Close");
        closeBtn.setLayoutData(gd);
        closeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                ProcedureDlg.this.close();
            }
        });
    }

    private void load() {
        int index = dataList.getSelectionIndex();
        if (index < 0) {
            return;
        }
        try {
            BundlePair selected = bundles.get(index);
            Bundle b = Bundle.unmarshalBundle(selected.xml);
            b.setName(selected.name);
            if (currentRdo.getSelection()) {
                for (IAlterBundleContributor contributor : AlterBundleFactory
                        .getContributors()) {
                    Map<String, String[]> alterables = contributor
                            .getAlterables();
                    for (Entry<String, String[]> entry : alterables
                            .entrySet()) {
                        Set<String> values = new HashSet<>(
                                Arrays.asList(entry.getValue()));
                        if (values.contains(CURRENT)) {
                            contributor.alterBundle(b, entry.getKey(), CURRENT);
                        }
                    }
                }
            }
            load(b);
        } catch (VizException e) {
            final String err = "Error loading bundle";
            statusHandler.handle(Priority.PROBLEM, err, e);
        }

    }

    private void load(final Bundle b) {
        statusHandler.info("Loading bundle: " + b.getName());
        String editorName = null;
        if (b.getDisplays().length > 0) {
            editorName = DescriptorMap.getEditorId(
                    b.getDisplays()[0].getDescriptor().getClass().getName());
        }
        final AbstractEditor editor = UiUtil.createOrOpenEditor(editorName,
                b.getDisplays());

        // set the loop properties
        if (b.getLoopProperties() != null) {
            editor.setLoopProperties(b.getLoopProperties());
            VizGlobalsManager.getCurrentInstance().updateChange(
                    VizConstants.LOOPING_ID, b.getLoopProperties().isLooping());
        }

        ProcedureLoadJob.getInstance().enqueue(b, editor);

        // add to history
        try {
            HistoryList.getInstance().addBundle(b);
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error adding bundle to history list", e);
        }
    }

    private void alterBundle() {
        if (dataList.getSelectionIndex() < 0) {
            return;
        }
        try {
            if (mustCreate(alterDlg)) {
                final BundlePair bp = new BundlePair();
                bp.name = bundles.get(dataList.getSelectionIndex()).name;
                bp.xml = bundles.get(dataList.getSelectionIndex()).xml;
                Bundle b = Bundle.unmarshalBundle(bp.xml, null);

                alterDlg = new AlterBundleDlg(b, getShell());
                alterDlg.addCloseCallback(new ICloseCallback() {

                    @Override
                    public void dialogClosed(Object returnValue) {
                        if (returnValue instanceof Bundle) {
                            Bundle b = (Bundle) returnValue;
                            try {
                                // Load was issued in alterBundleDlg
                                bp.xml = b.toXML();
                                saveProcedure(false);
                                load(b);
                            } catch (VizException e) {
                                final String err = "Error altering bundle";
                                statusHandler.handle(Priority.PROBLEM, err, e);
                            }
                        }
                    }
                });
                alterDlg.open();
            } else {
                alterDlg.bringToTop();
            }
        } catch (VizException e) {
            final String err = "Error altering bundle";
            statusHandler.handle(Priority.PROBLEM, err, e);
        }
    }

    private void copyIn() {
        try {
            /*
             * This seems counter intuitive, but going through the history list
             * ensures that a fresh serialized copy of the bundle is made rather
             * than a shallow copy of the display.
             */
            Bundle b = SaveBundle.extractCurrentBundle();
            HistoryList.getInstance().refreshLatestBundle(b);
            if (b.getName() == null || b.getName().isEmpty()) {
                MessageDialog.openWarning(shell, "Error Copying Resources",
                        "You must have at least one resource displayed to copy in.");
                return;
            }

            saved = false;
            saveBtn.setEnabled(true);

            /*
             * TODO: copy latest time in, potential threading issue if update
             * comes in after freeze is called but before thaw is called,
             * resource will not get update
             */
            for (AbstractRenderableDisplay display : b.getDisplays()) {
                for (ResourcePair rp : display.getDescriptor()
                        .getResourceList()) {
                    if (rp.getResource() != null) {
                        AbstractVizResource<?, ?> rsc = rp.getResource();
                        AbstractResourceData resourceData = rp.getResource()
                                .getResourceData();
                        if (resourceData != null) {
                            AbstractDescriptor desc = (AbstractDescriptor) rsc
                                    .getDescriptor();
                            DataTime[] times = desc.getTimeMatchingMap()
                                    .get(this);
                            if (times != null && times.length > 0) {
                                resourceData
                                        .setFrozenTime(times[times.length - 1]);
                            }
                        }
                    }
                }
            }

            String sb = b.toXML();

            // restore to not be frozen
            for (AbstractRenderableDisplay display : b.getDisplays()) {
                for (ResourcePair rp : display.getDescriptor()
                        .getResourceList()) {
                    if (rp.getResource() != null) {
                        AbstractResourceData ard = rp.getResource()
                                .getResourceData();
                        if (ard != null) {
                            ard.setFrozenTime(null);
                        }
                    }
                }
            }

            BundlePair bp = new BundlePair();
            if (!IRenameablePart.DEFAULT_PART_NAME.equals(b.getName())
                    && b.getDisplays()[0] instanceof MapRenderableDisplay) {
                /*
                 * This is a horrible hack to get a renamed editor's name
                 * instead of the default of Map.
                 */
                bp.name = b.getName();
            } else {
                bp.name = HistoryList.getInstance().getLabels()[0];
            }
            bp.xml = sb;
            bundles.add(bp);
            resyncProcedureAndList();
        } catch (VizException e) {
            final String err = "Error copying in";
            statusHandler.handle(Priority.PROBLEM, err, e);
        }
    }

    private void handleSaveRequest(boolean closeAfterSave) {
        if (fileName == null) {
            showSaveAsDlg(closeAfterSave);
        } else {
            saveProcedure(closeAfterSave);
        }
    }

    private void showSaveAsDlg(final boolean closeAfterSave) {
        if (mustCreate(saveAsDlg)) {
            saveAsDlg = new ProcedureListFileDlg("Save Procedure As...", shell,
                    VizLocalizationFileListDlg.Mode.SAVE, PROCEDURES_DIR);

            saveAsDlg.addCloseCallback(new ICloseCallback() {

                @Override
                public void dialogClosed(Object returnValue) {
                    String fn = saveAsDlg.getSelectedFileName();
                    if (fn != null) {
                        ProcedureDlg oldDlg = getDialog(fn);

                        if (oldDlg != null) {
                            oldDlg.close();
                        }

                        // Update mapping to new file name.
                        synchronized (openDialogs) {
                            openDialogs.remove(fileName);
                            openDialogs.put(fn, ProcedureDlg.this);
                        }

                        frozen = saveAsDlg.isFrozen();
                        fileName = fn;
                        saveProcedure(closeAfterSave);
                    }
                }
            });
            saveAsDlg.open();
        } else {
            saveAsDlg.bringToTop();
        }
    }

    @Override
    public boolean shouldClose() {
        if (saved) {
            return true;
        } else {
            return showConfirmSaveDlg();
        }
    }

    /**
     * Confirm save dialog, for if the user hasn't saved the procedure but tries
     * to close it
     *
     * @return true on "Yes" or "No". false on "Cancel"
     */
    private boolean showConfirmSaveDlg() {

        MessageDialog dlg = new MessageDialog(shell, "Confirm Save", null,
                "The procedure \"(" + (fileName != null ? fileName : "untitled")
                        + ")\"\ncontains unsaved data.\n\nSave before closing?",
                MessageDialog.QUESTION, 2, "Yes", "No", "Cancel");

        int returnCode = dlg.open();

        boolean closeDialog = false;
        switch (returnCode) {
        case 0:
            // YES
            handleSaveRequest(true);
            closeDialog = false;
            break;
        case 1:
            // NO
            closeDialog = true;
            break;

        default:
            // CANCEL
            closeDialog = false;
            break;
        }

        return closeDialog;
    }

    /**
     * If there is a procedure dialog open for the given filename, return it,
     * otherwise null.
     *
     * @param fileName
     * @return the dialog if it's open for for the filename
     */
    public static ProcedureDlg getDialog(String fileName) {
        synchronized (ProcedureDlg.openDialogs) {
            ProcedureDlg dialog = openDialogs.get(fileName);
            return dialog;
        }
    }

    /**
     * Get the ProcedureDlg for the given fileName and display it.
     *
     * @param fileName
     * @param p
     * @param parent
     */
    public static void displayDialog(String fileName, Procedure p,
            Shell parent) {
        synchronized (openDialogs) {
            ProcedureDlg dialog = getDialog(fileName);
            if (dialog == null || dialog.getShell() == null
                    || dialog.isDisposed()) {
                dialog = new ProcedureDlg(fileName, p, parent);
                openDialogs.put(fileName, dialog);
                dialog.open();
            } else {
                dialog.bringToTop();
            }
        }
    }

    @Override
    public boolean preShutdown(IWorkbench workbench, boolean forced) {
        if (!saved) {
            return showConfirmSaveDlg();
        }
        return true;
    }

    @Override
    public void postShutdown(IWorkbench workbench) {
    }

}
