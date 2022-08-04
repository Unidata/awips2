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
package com.raytheon.viz.gfe.dialogs.formatterlauncher;

import java.util.ArrayList;
import java.util.List;
import java.util.Timer;
import java.util.TimerTask;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.resource.ImageRegistry;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StackLayout;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.ImageData;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.ProgressBar;
import org.eclipse.swt.widgets.TabFolder;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.viz.gfe.Activator;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.dialogs.FormatterLauncherDialog;
import com.raytheon.viz.gfe.dialogs.formatterlauncher.ConfigData.ProductStateEnum;
import com.raytheon.viz.gfe.tasks.AbstractGfeTask;
import com.raytheon.viz.gfe.tasks.TaskManager;
import com.raytheon.viz.gfe.textformatter.FormatterUtil;
import com.raytheon.viz.gfe.textformatter.TextProductFinishListener;
import com.raytheon.viz.gfe.textformatter.TextProductManager;

/**
 * Composite containing the product area and its controls.
 *
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Apr 18, 2008           lvenable  Initial creation
 * Jan 19, 2010  4085     ryu       Save and load draft.
 * Dec 01, 2010  6130     ryu       Show output log when formatter fails.
 * Sep 02, 2011  10654    gzhou     Delete running/pending task and close tab.
 * May 23, 2012  14859    ryu       Select VTEC formatting in practice mode
 *                                  based on VTECMessageType setting.
 * Aug 10, 2012  15178    mli       Add autoWrite and autoStore capability
 * Sep 26, 2012  15423    ryu       Fix product correction in practice mode
 * May 15, 2013  1842     dgilling  Change constructor signature to accept a
 *                                  DataManager instance.
 * Sep 05, 2013  2329     randerso  Added call to
 *                                  ZoneCombinerComp.applyZoneCombo when when
 *                                  run formatter button is clicked.
 * Feb 05, 2014  2591     randerso  Added dataManager to ZoneCombinerComp
 *                                  constructor Passed dataMgr instance to
 *                                  FormatterUtil.runFormatterScript
 * Feb 12, 2014  2801     randerso  Added prompting if formatter is run against
 *                                  non-normal database
 * Apr 20, 2015  4027     randerso  Fixes for GFE formatter auto tests to
 *                                  support mixed case WA
 * Aug 24, 2015  4749     dgilling  Ensure TextProductFinishListener callbacks
 *                                  execute on UI thread, override dispose to
 *                                  aid perspective shutdown.
 * Sep 15, 2015  4858     dgilling  Handle exception from runFormatterScript.
 * Nov 03, 2015  14813    ryu       Fix missing VTEC code in generated product.
 *                                  VTEC mode is set based on the pil of the
 *                                  product rather than the disply name.
 * Feb 18, 2016  13033    yteng     Improve error message for bad characters in
 *                                  text formatter definitions.
 * Apr 14, 2016  5578     dgilling  Support changes to
 *                                  FormatterUtil.runFormatterScript.
 * Jul 02, 2020  7597     randerso  Fix GUI issues exposed by Eclipse 4.16
 *                                  upgrade
 * Feb 22, 2022  8782     randerso  Reorganized code to use StackLayout.
 *                                  Made disabled buttons more obvious.
 *                                  Fixed progress bar which only worked the
 *                                  first time in gtk3.
 * Mar 09, 2022  8782     randerso  Fix editor correction mode.
 *
 * </pre>
 *
 * @author lvenable
 *
 */
public class ProductAreaComp extends Composite
        implements TextProductFinishListener, ITransmissionState {

    private static final String DISABLED_TAG = "Disabled";

    private static final int DISABLED_ALPHA = 96;

    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(getClass());

    private class ProgressUpdateTask extends TimerTask {
        private static final int DELTA = 5;

        private int delta = DELTA;

        @Override
        public void run() {
            VizApp.runAsync(() -> {
                int min = progressBar.getMinimum();
                int max = progressBar.getMaximum();
                int sel = progressBar.getSelection();

                sel += delta;
                if (sel > max) {
                    sel -= DELTA;
                    delta = -DELTA;
                } else if (sel < min) {
                    sel += DELTA;
                    delta = DELTA;
                }
                progressBar.setSelection(sel);
            });
        }
    }

    private List<Button> viewSelectionButtons;

    /**
     * Product editor button.
     */
    private Button productEditorBtn;

    /**
     * Output log button.
     */
    private Button outputLogBtn;

    /**
     * Run formatter button.
     */
    private Button runFormatterBtn;

    /**
     * Abort formatter button.
     */
    private Button abortFormatterBtn;

    /**
     * Formatting combo box.
     */
    private Combo formattingCbo;

    /**
     * Product name.
     */
    private String productName;

    /**
     * Composite with a GridLayout that simulates a StackLayout.
     */
    private Composite stackGridComp;

    /**
     * Composite containing the Zone Combiner controls.
     */

    private ZoneCombinerComp zoneCombinerComp;

    /**
     * Composite containing the Product Editor controls.
     */
    private ProductEditorComp productEditorComp;

    /**
     * Composite containing the Output Log controls.
     */
    private OutputLogComp outputLogComp;

    /**
     * Flag indicating the product is in error correction mode.
     */
    private boolean editorCorrectionMode;

    /**
     * Product tab callback interface.
     */
    private IProductTab productTabCB;

    private ImageRegistry registry;

    private ProgressBar progressBar;

    private Timer progressTimer;

    private ProgressUpdateTask progressUpdateTask;

    private TextProductManager textProductMgr;

    private final DataManager dataMgr;

    private boolean practiceMode;

    private boolean isTabClosed = false;

    /**
     * Constructor.
     *
     * @param parent
     *            Parent composite.
     */
    public ProductAreaComp(TabFolder parent, IProductTab productTabCB,
            String productName, boolean editorCorrectionMode,
            TextProductManager textProductMgr, DataManager dataMgr,
            boolean practiceMode) {
        super(parent, SWT.NONE);

        this.productName = productName;
        this.editorCorrectionMode = editorCorrectionMode;
        this.productTabCB = productTabCB;
        this.textProductMgr = textProductMgr;
        this.dataMgr = dataMgr;
        this.practiceMode = practiceMode;

        init();
    }

    /**
     * Initialize the composite.
     */
    private void init() {
        GridLayout gl = new GridLayout(1, false);
        gl.marginHeight = 1;
        gl.marginWidth = 1;
        setLayout(gl);

        registry = new ImageRegistry();
        loadImage("icons/gfepmunuon.gif", "zoneCombiner");
        loadImage("icons/gfeedit.gif", "productEditor");
        loadImage("icons/gfeterm.gif", "outputLog");
        loadImage("icons/loadDraft.gif", "loadDraft");
        loadImage("icons/gferun.gif", "runFormatter");
        loadImage("icons/gfecancel.gif", "abortFormatter");
        loadImage("icons/gfestop.gif", "closeTab");

        initializeComponents();
    }

    private void loadImage(String imagePath, String imageId) {
        ImageDescriptor imageDescriptor = Activator
                .imageDescriptorFromPlugin(Activator.PLUGIN_ID, imagePath);
        registry.put(imageId, imageDescriptor);

        ImageData imageData = imageDescriptor.getImageData(100);
        for (int i = 0; i < imageData.alphaData.length; i++) {
            imageData.alphaData[i] *= -DISABLED_ALPHA;
        }
        registry.put(imageId + DISABLED_TAG,
                new Image(getDisplay(), imageData));
    }

    private Image getImage(String imageId, boolean enabled) {
        String id = imageId;
        if (!enabled) {
            id += DISABLED_TAG;
        }
        return registry.get(id);
    }

    /**
     * Initialize the controls on the composite.
     */
    private void initializeComponents() {
        Composite buttonBarComp = new Composite(this, SWT.BORDER);
        buttonBarComp.setLayout(new GridLayout(10, false));
        buttonBarComp.setLayoutData(
                new GridData(SWT.FILL, SWT.DEFAULT, true, false));

        stackGridComp = new Composite(this, SWT.NONE);
        StackLayout sl = new StackLayout();
        sl.marginHeight = 0;
        sl.marginWidth = 0;
        stackGridComp.setLayout(sl);
        stackGridComp
                .setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

        createIconButtonsComp(buttonBarComp);
    }

    /**
     * Create the control buttons (no text, just icons).
     */
    private void createIconButtonsComp(Composite buttonBarComp) {
        viewSelectionButtons = new ArrayList<>(3);

        if (editorCorrectionMode) {
            /*
             * For correction mode just create a productEditorComp with no
             * viewSelectionButtons
             */
            productEditorComp = new ProductEditorComp(stackGridComp,
                    textProductMgr.getProductDefinition(productName),
                    productName, editorCorrectionMode, this, dataMgr);

            ((StackLayout) stackGridComp
                    .getLayout()).topControl = productEditorComp;
            stackGridComp.layout();
        } else {
            /*
             * Create the left side buttons.
             */
            Button zoneCombinerBtn = new Button(buttonBarComp, SWT.TOGGLE);
            viewSelectionButtons.add(zoneCombinerBtn);
            zoneCombinerBtn.setData("imageId", "zoneCombiner");
            enableButton(zoneCombinerBtn, true);
            zoneCombinerBtn.setSelection(true);
            zoneCombinerBtn.setToolTipText("Zone Combiner");
            zoneCombinerBtn.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent event) {
                    selectView((Button) event.widget);
                }
            });
            zoneCombinerComp = new ZoneCombinerComp(stackGridComp, productTabCB,
                    productName, textProductMgr, this.dataMgr);
            zoneCombinerBtn.setData("viewComp", zoneCombinerComp);

            productEditorBtn = new Button(buttonBarComp, SWT.TOGGLE);
            viewSelectionButtons.add(productEditorBtn);
            productEditorBtn.setData("imageId", "productEditor");
            enableButton(productEditorBtn, true);
            productEditorBtn.setToolTipText("Product Editor");
            productEditorBtn.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent event) {
                    selectView((Button) event.widget);
                }
            });
            productEditorComp = new ProductEditorComp(stackGridComp,
                    textProductMgr.getProductDefinition(productName),
                    productName, editorCorrectionMode, this, dataMgr);
            productEditorBtn.setData("viewComp", productEditorComp);

            outputLogBtn = new Button(buttonBarComp, SWT.TOGGLE);
            viewSelectionButtons.add(outputLogBtn);
            outputLogBtn.setData("imageId", "outputLog");
            enableButton(outputLogBtn, true);
            outputLogBtn.setToolTipText("View Output");
            enableButton(outputLogBtn, false);
            outputLogBtn.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent event) {
                    selectView((Button) event.widget);
                }
            });
            outputLogComp = new OutputLogComp(stackGridComp);
            outputLogBtn.setData("viewComp", outputLogComp);

            if (textProductMgr.mapRequired(productName)) {
                selectView(zoneCombinerBtn);
            } else {
                enableButton(zoneCombinerBtn, false);
                selectView(productEditorBtn);
            }

            Label fillerLbl = new Label(buttonBarComp, SWT.NONE);
            fillerLbl.setLayoutData(new GridData(20, SWT.DEFAULT));

            Button loadDraftBtn = new Button(buttonBarComp, SWT.PUSH);
            loadDraftBtn.setData("imageId", "loadDraft");
            enableButton(loadDraftBtn, true);
            loadDraftBtn.setToolTipText("Load Draft");
            loadDraftBtn.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent event) {
                    productEditorComp.loadDraft();
                    ProductAreaComp.this.selectView(productEditorBtn);
                }
            });

            runFormatterBtn = new Button(buttonBarComp, SWT.PUSH);
            runFormatterBtn.setData("imageId", "runFormatter");
            enableButton(runFormatterBtn, true);
            runFormatterBtn.setToolTipText("Run Formatter");
            runFormatterBtn.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent event) {
                    runFormatter();
                }
            });

            abortFormatterBtn = new Button(buttonBarComp, SWT.PUSH);
            abortFormatterBtn.setData("imageId", "abortFormatter");
            enableButton(abortFormatterBtn, true);
            abortFormatterBtn.setToolTipText("Cancel Run");
            enableButton(abortFormatterBtn, false);
            abortFormatterBtn.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent e) {
                    abortFormatter();
                }
            });
        }

        /*
         * Create the formatting combo box controls.
         */
        Composite formatComp = new Composite(buttonBarComp, SWT.NONE);
        GridLayout gl = new GridLayout(2, false);
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        formatComp.setLayout(gl);
        formatComp
                .setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, true));

        Label formattingLbl = new Label(formatComp, SWT.RIGHT);
        formattingLbl.setText("Formatting: ");
        formattingLbl.setLayoutData(new GridData(120, SWT.DEFAULT));
        formattingLbl.setVisible(practiceMode);

        formattingCbo = new Combo(formatComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        formattingCbo.add("Normal: NoVTEC");
        formattingCbo.add("Normal: O-Vtec");
        formattingCbo.add("Normal: E-Vtec");
        formattingCbo.add("Normal: X-Vtec");
        formattingCbo.add("Test: NoVTEC");
        formattingCbo.add("Test: T-Vtec");

        int pracType = 0;
        if (practiceMode) {
            String pil = null;
            if (textProductMgr.getProductDefinition(productName) != null) {
                pil = (String) textProductMgr.getProductDefinition(productName)
                        .get("pil");
            }
            if (pil != null) {
                String vtecMode = textProductMgr
                        .getVtecMessageType(pil.substring(0, 3));
                if (vtecMode == null) {
                    pracType = 0;
                } else if ("O".equals(vtecMode)) {
                    pracType = 1;
                } else if ("E".equals(vtecMode)) {
                    pracType = 2;
                } else if ("X".equals(vtecMode)) {
                    pracType = 3;
                } else if ("T".equals(vtecMode)) {
                    pracType = 5;
                } else {
                    pracType = 4;
                }
            }
        }
        formattingCbo.select(pracType);

        formattingCbo.setVisible(practiceMode);

        // create the progress bar
        progressBar = new ProgressBar(buttonBarComp, SWT.HORIZONTAL);
        progressBar.setVisible(false);
        progressTimer = new Timer();

        /*
         * Create the right side close button.
         */
        Button closeTabBtn = new Button(buttonBarComp, SWT.PUSH);
        closeTabBtn.setData("imageId", "closeTab");
        enableButton(closeTabBtn, true);
        closeTabBtn.setToolTipText("Close Tab");
        closeTabBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                closeTab();
            }
        });
    }

    private void enableButton(Button button, boolean enabled) {
        String imageId = (String) button.getData("imageId");
        button.setImage(getImage(imageId, enabled));
        button.setEnabled(enabled);
    }

    private boolean okToLoseText() {
        boolean run = true;
        String currentText = productEditorComp.getProductText();
        if ((currentText != null) && (currentText.length() > 0)
                && (!productEditorComp.isEditorDisabled())) {
            String question = "Your action will delete existing text "
                    + "in Product Editor.\nContinue anyway?";
            run = MessageDialog.openConfirm(ProductAreaComp.this.getShell(),
                    "Product Text Delete?", question);
        }
        return run;
    }

    private void selectView(Button selectedButton) {
        for (Button button : viewSelectionButtons) {
            button.setSelection(button.equals(selectedButton));
        }

        // Re-layout the stack grid composite
        ((StackLayout) stackGridComp
                .getLayout()).topControl = (Composite) selectedButton
                        .getData("viewComp");
        stackGridComp.layout();
    }

    private void runFormatter() {
        enableButton(outputLogBtn, false);
        selectView(productEditorBtn);

        if (okToLoseText()) {
            DatabaseID dbId = ((FormatterLauncherDialog) productTabCB)
                    .getSelectedDataSource(productName);

            if (dbId != null) {
                productEditorComp.clearProductText();
                enableButton(abortFormatterBtn, true);
                enableButton(runFormatterBtn, false);
                String vtecMode = "";
                if (formattingCbo.isVisible()) {
                    vtecMode = formattingCbo.getText();
                } else {
                    String pil = "";
                    try {
                        pil = (String) textProductMgr
                                .getDefinitionValue(productName, "pil");
                    } catch (ClassCastException e) {
                        statusHandler.error(
                                "Invalid pil value: " + textProductMgr
                                        .getDefinitionValue(productName, "pil"),
                                e);
                    }
                    if (pil != null) {
                        pil = pil.substring(0, 3);
                        vtecMode = textProductMgr.getVtecMessageType(pil);
                    }
                }

                // Get the source database
                zoneCombinerComp.applyZoneCombo();
                FormatterUtil.runFormatterScript(dataMgr, textProductMgr,
                        productName, dbId.toString(), vtecMode,
                        ProductAreaComp.this);
            }
        }
    }

    private void abortFormatter() {
        String taskName = textProductMgr.getModuleName(productName);
        AbstractGfeTask task = TaskManager.getInstance().getTask(taskName);
        if (task != null) {
            TaskManager.getInstance().cancelTask(task);
        }
    }

    private void closeTab() {
        if (okToLoseText()) {
            String taskName = textProductMgr.getModuleName(productName);
            if (taskName != null) {
                AbstractGfeTask task = TaskManager.getInstance()
                        .getTask(taskName);
                if (task != null) {
                    TaskManager.getInstance().cancelTask(task);
                }
            }

            productTabCB.removeProductTab(productName);
            isTabClosed = true;
        }
    }

    /**
     *
     * @return the textProductManager
     */
    public TextProductManager getTextProductManager() {
        return textProductMgr;
    }

    /**
     * Sets the textProductManager
     *
     * @param textProductManager
     */
    public void setTextProductManager(TextProductManager textProductManager) {
        textProductMgr = textProductManager;
    }

    @Override
    public void textProductQueued(final ConfigData.ProductStateEnum state) {
        if (isTabClosed) {
            return;
        }

        VizApp.runSyncIfWorkbench(
                () -> productTabCB.setTabState(state, productName));
    }

    @Override
    public void textProductFinished(final String productText,
            final ConfigData.ProductStateEnum state) {
        if (isTabClosed) {
            return;
        }

        VizApp.runSyncIfWorkbench(() -> {
            enableButton(abortFormatterBtn, false);
            enableButton(runFormatterBtn, true);
            outputLogComp.showLog(textProductMgr.getModuleName(productName));
            enableButton(outputLogBtn, true);
            if (state == ConfigData.ProductStateEnum.Finished) {
                if (productText != null) {
                    productEditorComp.retrieveActiveVTEC();
                    productEditorComp.setProductText(productText);

                    // handle autoWrite and autoStore...
                    productEditorComp.doAutoStuff();
                }

                productEditorBtn.setSelection(true);
                selectView(productEditorBtn);
            } else if (state == ConfigData.ProductStateEnum.Failed) {
                selectView(outputLogBtn);
            }
        });
    }

    @Override
    public void startProgressBar(final ConfigData.ProductStateEnum state) {
        if (isTabClosed) {
            return;
        }

        VizApp.runSyncIfWorkbench(() -> {
            progressBar.setSelection(progressBar.getMinimum());
            progressBar.setVisible(true);
            progressUpdateTask = new ProgressUpdateTask();
            progressTimer.scheduleAtFixedRate(progressUpdateTask, 0, 100);
            productTabCB.setTabState(state, productName);
        });
    }

    @Override
    public void stopProgressBar(final ConfigData.ProductStateEnum state) {
        if (isTabClosed) {
            return;
        }

        VizApp.runSyncIfWorkbench(() -> {
            progressBar.setVisible(false);
            progressUpdateTask.cancel();
            productTabCB.setTabState(state, productName);
        });
    }

    @Override
    public void setTransmissionState(ProductStateEnum state) {
        productTabCB.setTabState(state, productName);
    }

    @Override
    public void dispose() {
        isTabClosed = true;
        registry.dispose();
        super.dispose();
    }
}
