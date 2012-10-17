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

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import org.eclipse.jface.dialogs.InputDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.ShellAdapter;
import org.eclipse.swt.events.ShellEvent;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.layout.RowData;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.viz.core.DescriptorMap;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.drawables.AbstractDescriptor;
import com.raytheon.uf.viz.core.drawables.AbstractRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.procedures.AlterBundleFactory;
import com.raytheon.uf.viz.core.procedures.Bundle;
import com.raytheon.uf.viz.core.procedures.IAlterBundleContributor;
import com.raytheon.uf.viz.core.procedures.Procedure;
import com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.d2d.ui.dialogs.procedures.ProcedureComm.BundlePair;
import com.raytheon.viz.ui.HistoryList;
import com.raytheon.viz.ui.UiUtil;
import com.raytheon.viz.ui.actions.SaveBundle;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;
import com.raytheon.viz.ui.dialogs.ICloseCallback;
import com.raytheon.viz.ui.editor.AbstractEditor;

/**
 * 
 * Dialog for loading or modifying procedures.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 *                                     Initial Creation
 * Oct 16, 2012 1229       rferrel     Changes for non-blocking AlterBundleDlg.
 * Oct 16, 2012 1229       rferrel     Changes to have displayDialog method.
 * Oct 16, 2012 1229       rferrel     Changes for non-blocking ProcedureListDlg.
 * 
 * </pre>
 * 
 * @author unknown
 * @version 1.0
 */
public class ProcedureDlg extends CaveSWTDialog {

    private final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ProcedureDlg.class);

    public static final String ORIGINAL = "Original Location";

    public static final String CURRENT = "Current Location";

    public static final String PROCEDURES_DIR = "/procedures";

    private static final Map<String, ProcedureDlg> openDialogs = new HashMap<String, ProcedureDlg>();

    private Font font;

    private List dataList;

    private final int BOTTOM_BTN_WIDTH = 90;

    private Button upBtn;

    private Button downBtn;

    private Button renameBtn;

    private Button originalRdo;

    private Button currentRdo;

    private Button firstNextBtn;

    private final String FIRST = "First";

    private final String NEXT = "Next";

    private Button loadBtn;

    private Button alterBtn;

    private Button copyInBtn;

    private Button copyOutBtn;

    private Button deleteBtn;

    private Button saveBtn;

    private Button saveAsBtn;

    private Button closeBtn;

    private String fileName;

    private Boolean frozen;

    private boolean saved = true;

    private static Shell lastShell = null;

    private static Point lastPoint = null;

    private static int initialPosX = -1;

    private static int initialPosY = -1;

    private final java.util.List<BundlePair> bundles;

    private AlterBundleDlg alterDlg;

    private ProcedureListDlg saveAsDlg;

    private ProcedureDlg(String fileName, Procedure p, Shell parent) {
        // Win32
        super(parent, SWT.DIALOG_TRIM | SWT.RESIZE, CAVE.INDEPENDENT_SHELL
                | CAVE.DO_NOT_BLOCK);
        String titleBarStr = fileName;
        if (titleBarStr == null) {
            titleBarStr = "(untitled)";
        }
        setText("Procedure - " + titleBarStr);

        this.fileName = fileName;
        this.bundles = new ArrayList<BundlePair>();
        frozen = false;
        if (p != null && p.getBundles() != null) {
            Bundle[] bund = p.getBundles();
            for (Bundle b : bund) {
                // Check to see if frozen
                for (AbstractRenderableDisplay display : b.getDisplays()) {
                    for (ResourcePair rp : display.getDescriptor()
                            .getResourceList()) {
                        if (rp.getResourceData() != null) {
                            if (rp.getResourceData().isFrozen()) {
                                frozen = true;
                                break;
                            }
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
                    e.printStackTrace();
                }
                bp.name = (b.getName() != null ? b.getName() : " ");
                this.bundles.add(bp);
            }
        }
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
        synchronized (openDialogs) {
            openDialogs.remove(fileName);
        }
    }

    @Override
    protected void preOpened() {
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
                                    && bundles != null
                                    && bundles.size() > 0
                                    && dataList.getSelectionIndex() >= 0);
                        }
                    }
                });
            }
        };

        ProcedureComm.getInstance().addCopyOutStateListener(changeListener);
        final ProcedureComm.ICopyOutListener copyOutListener = new ProcedureComm.ICopyOutListener() {

            /*
             * (non-Javadoc)
             * 
             * @see
             * com.raytheon.viz.ui.dialogs.procedure.ProcedureComm.ICopyOutListener
             * #copyOut(com.raytheon.viz.ui.dialogs.procedure.ProcedureComm.
             * BundlePair, java.lang.Object)
             */
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

        shell.addShellListener(new ShellAdapter() {

            @Override
            public void shellClosed(ShellEvent arg0) {
                ProcedureComm.getInstance().removeCopyOutListener(
                        copyOutListener);
                ProcedureComm.getInstance().removeCopyOutStateListener(
                        changeListener);

                font.dispose();
            }
        });
    }

    private void resyncProcedureAndList() {
        VizApp.runAsync(new Runnable() {

            @Override
            public void run() {
                if (!dataList.isDisposed()) {
                    if (bundles != null && bundles.size() > 0) {
                        String[] list = new String[bundles.size()];
                        int i = 0;
                        for (BundlePair b : bundles) {
                            list[i] = (b.name != null ? b.name : " ");
                            i++;
                        }
                        int currIdx = dataList.getSelectionIndex();
                        dataList.setItems(list);
                        if (firstNextBtn.isEnabled() == false) {
                            if (currIdx == -1
                                    && firstNextBtn.getText().toString()
                                            .equals(FIRST)) {
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

    private void saveProcedure() {
        try {
            IPathManager pm = PathManagerFactory.getPathManager();

            LocalizationContext context = pm.getContext(
                    LocalizationType.CAVE_STATIC, LocalizationLevel.USER);

            LocalizationFile localizationFile = pm.getLocalizationFile(context,
                    PROCEDURES_DIR + File.separator + fileName);

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
                            if (rp.getResourceData() != null
                                    && rp.getResourceData() instanceof AbstractRequestableResourceData) {
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

            String procedureXML = procedure.toXML();
            FileUtil.bytes2File(procedureXML.getBytes(),
                    localizationFile.getFile());
            localizationFile.save();

            shell.setText("Procedure - " + fileName);
            saved = true;
            saveBtn.setEnabled(false);
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
        dataList = new List(listComp, SWT.BORDER | SWT.SINGLE | SWT.V_SCROLL
                | SWT.H_SCROLL);
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
        listControlComp.setLayout(rl);

        RowData rd = new RowData(80, SWT.DEFAULT);
        upBtn = new Button(listControlComp, SWT.PUSH);
        upBtn.setText("Up");
        upBtn.setLayoutData(rd);
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
            }
        });

        rd = new RowData(80, SWT.DEFAULT);
        downBtn = new Button(listControlComp, SWT.PUSH);
        downBtn.setText("Down");
        downBtn.setLayoutData(rd);
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
            }
        });

        rd = new RowData(80, SWT.DEFAULT);
        renameBtn = new Button(listControlComp, SWT.PUSH);
        renameBtn.setText("Rename...");
        renameBtn.setLayoutData(rd);
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
                    InputDialog id = new InputDialog(shell,
                            "Enter Bundle Name", "Enter bundle name:", b.name,
                            null);
                    if (InputDialog.OK == id.open()) {
                        String newName = id.getValue();

                        if (newName != null
                                && "".equals(newName.trim()) == false) {
                            b.name = newName;
                            resyncProcedureAndList();
                            saved = false;
                            saveBtn.setEnabled(true);
                            done = true;
                        } else {
                            MessageDialog
                                    .openWarning(shell, "Error Setting Name",
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
        GridLayout gl = new GridLayout(3, false);
        gl.horizontalSpacing = 10;
        buttonComp.setLayout(gl);
        buttonComp.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true,
                false));

        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = BOTTOM_BTN_WIDTH;
        originalRdo = new Button(buttonComp, SWT.RADIO);
        originalRdo.setText("Original");
        originalRdo.setSelection(true);
        originalRdo.setLayoutData(gd);

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = BOTTOM_BTN_WIDTH;
        Label fillerLbl = new Label(buttonComp, SWT.NONE);
        fillerLbl.setLayoutData(gd);

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = BOTTOM_BTN_WIDTH;
        currentRdo = new Button(buttonComp, SWT.RADIO);
        currentRdo.setText("Current");
        currentRdo.setLayoutData(gd);

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = BOTTOM_BTN_WIDTH;
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

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = BOTTOM_BTN_WIDTH;
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

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = BOTTOM_BTN_WIDTH;
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

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = BOTTOM_BTN_WIDTH;
        copyInBtn = new Button(buttonComp, SWT.PUSH);
        copyInBtn.setText("Copy In");
        copyInBtn.setLayoutData(gd);
        copyInBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                try {

                    // This seems counter intuitive, but going through
                    // the history list ensures that a fresh serialized copy
                    // of the bundle is made rather than a shallow copy of
                    // the display.
                    saved = false;
                    saveBtn.setEnabled(true);
                    Bundle b = SaveBundle.extractCurrentBundle();
                    HistoryList.getInstance().refreshLatestBundle(b);

                    // TODO: copy latest time in, potential threading issue if
                    // update
                    // comes in after freeze is called but before thaw is
                    // called, resource will not get update
                    for (AbstractRenderableDisplay display : b.getDisplays()) {
                        for (ResourcePair rp : display.getDescriptor()
                                .getResourceList()) {
                            if (rp.getResource() != null) {
                                AbstractVizResource<?, ?> rsc = rp
                                        .getResource();
                                AbstractResourceData resourceData = rp
                                        .getResource().getResourceData();
                                if (resourceData != null) {
                                    AbstractDescriptor desc = (AbstractDescriptor) rsc
                                            .getDescriptor();
                                    DataTime[] times = desc
                                            .getTimeMatchingMap().get(this);
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
                    bp.name = (HistoryList.getInstance().getLabels()[0]);
                    bp.xml = sb;
                    bundles.add(bp);
                    resyncProcedureAndList();
                } catch (VizException e) {
                    final String err = "Error copying in";
                    statusHandler.handle(Priority.PROBLEM, err, e);

                }

            }
        });

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = BOTTOM_BTN_WIDTH;
        copyOutBtn = new Button(buttonComp, SWT.PUSH);
        copyOutBtn.setText("Copy Out");
        copyOutBtn.setLayoutData(gd);
        if (ProcedureComm.getInstance().getCopyListenerCount() > 1
                && bundles != null && bundles.size() > 0
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

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = BOTTOM_BTN_WIDTH;
        deleteBtn = new Button(buttonComp, SWT.PUSH);
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
                if (bundles.size() > 0) {
                    bundles.remove(idx);
                }
                resyncProcedureAndList();
                ProcedureComm.getInstance().refreshCopyOutState();
            }
        });

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = BOTTOM_BTN_WIDTH;
        saveBtn = new Button(buttonComp, SWT.PUSH);
        saveBtn.setEnabled(false);
        saveBtn.setText("Save");
        saveBtn.setLayoutData(gd);
        saveBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (fileName == null) {
                    showSaveAsDlg();
                } else {
                    saveProcedure();
                }
            }
        });

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = BOTTOM_BTN_WIDTH;
        saveAsBtn = new Button(buttonComp, SWT.PUSH);
        saveAsBtn.setText("Save As...");
        saveAsBtn.setLayoutData(gd);
        saveAsBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                showSaveAsDlg();
            }
        });
        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = BOTTOM_BTN_WIDTH;
        closeBtn = new Button(buttonComp, SWT.PUSH);
        closeBtn.setText("Close");
        closeBtn.setLayoutData(gd);
        closeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (saved) {
                    shell.close();
                } else {
                    showConfirmSaveDlg();
                }
            }
        });
    }

    private void load() {
        if (dataList.getSelectionIndex() < 0) {
            return;
        }
        try {
            Bundle b = Bundle.unmarshalBundle(
                    bundles.get(dataList.getSelectionIndex()).xml, null);
            if (currentRdo.getSelection()) {
                for (IAlterBundleContributor contributor : AlterBundleFactory
                        .getContributors()) {
                    Map<String, String[]> alterables = contributor
                            .getAlterables();
                    for (Entry<String, String[]> entry : alterables.entrySet()) {
                        Set<String> values = new HashSet<String>(
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
        String editorName = null;
        if (b.getDisplays().length > 0) {
            editorName = DescriptorMap.getEditorId(b.getDisplays()[0]
                    .getDescriptor().getClass().getName());
        }
        final AbstractEditor editor = UiUtil.createOrOpenEditor(editorName,
                b.getDisplays());

        // set the loop properties
        if (b.getLoopProperties() != null) {
            editor.setLoopProperties(b.getLoopProperties());
        }

        /*
         * Check if the bundle was used to open the editor. If it was then we do
         * not want to clear the display.
         */
        if (editor.getDisplayPanes()[0].getDescriptor() != b.getDisplays()[0]
                .getDescriptor()) {
            for (IDisplayPane pane : editor.getDisplayPanes()) {
                pane.getRenderableDisplay().getDescriptor().getResourceList()
                        .clear();
            }
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
                alterDlg.setCloseCallback(new ICloseCallback() {

                    @Override
                    public void dialogClosed(Object returnValue) {
                        if (returnValue instanceof Bundle) {
                            Bundle b = (Bundle) returnValue;
                            try {
                                // Load was issued in alterBundleDlg
                                bp.xml = b.toXML();
                                saveProcedure();
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

    private void showSaveAsDlg() {
        if (mustCreate(saveAsDlg)) {
            saveAsDlg = new ProcedureListDlg("Save Procedure As...", shell,
                    ProcedureListDlg.Mode.SAVE);

            saveAsDlg.setCloseCallback(new ICloseCallback() {

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
                        saveProcedure();
                    }
                }
            });
            saveAsDlg.open();
        } else {
            saveAsDlg.bringToTop();
        }
    }

    /**
     * Confirm save dialog, for if the user hasn't saved the procedure but tries
     * to close it
     */
    private void showConfirmSaveDlg() {
        CaveSWTDialog dlg = new CaveSWTDialog(shell, SWT.DIALOG_TRIM, CAVE.NONE) {

            @Override
            protected void initializeComponents(Shell shell) {
                shell.setText("Confirm Save");

                /*
                 * Image and label
                 */
                Composite imageLblComp = new Composite(shell, SWT.NONE);
                GridLayout layout = new GridLayout(2, false);
                GridData gridData = new GridData(SWT.FILL, SWT.DEFAULT, true,
                        false);
                imageLblComp.setLayout(layout);
                imageLblComp.setLayoutData(gridData);

                Label imageLbl = new Label(imageLblComp, SWT.NONE);
                imageLbl.setImage(getDisplay()
                        .getSystemImage(SWT.ICON_QUESTION));

                Label label = new Label(imageLblComp, SWT.NONE);

                if (fileName == null) {
                    fileName = "untitled";
                }
                label.setText("The procedure \"(" + fileName
                        + ")\"\ncontains unsaved data.\nSave before closing?");
                label.setData(gridData);

                /*
                 * Button composite
                 */
                layout = new GridLayout(3, true);
                gridData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
                Composite buttonComp = new Composite(shell, SWT.NONE);
                buttonComp.setLayout(layout);
                buttonComp.setLayoutData(gridData);

                /*
                 * Yes button
                 */
                Button yes = new Button(buttonComp, SWT.PUSH);
                gridData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
                yes.setText("Yes");
                yes.setFocus();
                yes.setLayoutData(gridData);

                yes.addSelectionListener(new SelectionAdapter() {
                    @Override
                    public void widgetSelected(SelectionEvent event) {
                        Display.getCurrent().getActiveShell().dispose();
                        showSaveAsDlg();
                    }
                });

                /*
                 * No button
                 */
                Button no = new Button(buttonComp, SWT.PUSH);
                no.setText("No");
                gridData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
                no.setLayoutData(gridData);
                no.addSelectionListener(new SelectionAdapter() {
                    @Override
                    public void widgetSelected(SelectionEvent event) {
                        Composite composite = Display.getCurrent()
                                .getActiveShell().getParent();
                        if (composite instanceof Shell) {
                            Shell shell = (Shell) composite;
                            shell.close();
                        } else {
                            Display.getCurrent().getActiveShell().getParent()
                                    .dispose();
                        }
                    }
                });

                /*
                 * Cancel
                 */
                Button cancel = new Button(buttonComp, SWT.PUSH);
                cancel.setText("Cancel");
                gridData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
                cancel.setLayoutData(gridData);
                cancel.addSelectionListener(new SelectionAdapter() {
                    @Override
                    public void widgetSelected(SelectionEvent event) {
                        Display.getCurrent().getActiveShell().dispose();
                    }
                });
            }
        };
        dlg.open();
    }

    /**
     * If there is a procedure dialog open for the given filename, return it,
     * otherwise null.
     * 
     * @param fileName
     * @return
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
    public static void displayDialog(String fileName, Procedure p, Shell parent) {
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
}
