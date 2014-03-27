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

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.resource.ImageRegistry;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.ProgressBar;
import org.eclipse.swt.widgets.TabFolder;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID;
import com.raytheon.viz.gfe.Activator;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.dialogs.FormatterLauncherDialog;
import com.raytheon.viz.gfe.dialogs.formatterlauncher.ConfigData.productStateEnum;
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
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 18 APR 2008  ###        lvenable    Initial creation
 * 19 JAN 2010  4085       ryu         Save and load draft.
 *  1 DEC 2010  6130       ryu         Show output log when formatter fails.
 *  2 SEP 2011 10654       gzhou       Delete running/pending task and close tab.
 * 23 MAY 2012 14859       ryu         Select VTEC formatting in practice mode
 *                                     based on VTECMessageType setting.
 * 10 AUG 2012 15178  	   mli		   Add autoWrite and autoStore capability
 * 26 SEP 2012 15423       ryu         Fix product correction in practice mode
 * 15 MAY 2013  1842       dgilling    Change constructor signature to accept a
 *                                     DataManager instance.
 * 05 SEP 2013  2329       randerso    Added call to ZoneCombinerComp.applyZoneCombo when
 *                                     when run formatter button is clicked.
 * 05 FEB 2014  2591       randerso    Added dataManager to ZoneCombinerComp constructor
 *                                     Passed dataMgr instance to FormatterUtil.runFormatterScript
 * 12 FEB 2014  2801       randerso    Added prompting if formatter is run against non-normal database
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class ProductAreaComp extends Composite implements
        TextProductFinishListener, ITransmissionState {
    /**
     * Zone combiner image.
     */
    private Image zoneCombinerImg;

    /**
     * Product editor image.
     */
    private Image productEditorImg;

    /**
     * Output log image.
     */
    private Image outputLogImg;

    /**
     * Load draft image.
     */
    private Image loadDraftImg;

    /**
     * Run formatter image.
     */
    private Image runFormatterImg;

    /**
     * Abort image.
     */
    private Image abortFormatterImg;

    /**
     * Close tab image.
     */
    private Image closeTabImg;

    /**
     * Zone Combiner image.
     */
    private Button zoneCombinerBtn;

    /**
     * Product editor button.
     */
    private Button productEditorBtn;

    /**
     * Output log button.
     */
    private Button outputLogBtn;

    /**
     * Load draft button.
     */
    private Button loadDraftBtn;

    /**
     * Run formatter button.
     */
    private Button runFormatterBtn;

    /**
     * Abort formatter button.
     */
    private Button abortFormatterBtn;

    /**
     * Close tab button.
     */
    private Button closeTabBtn;

    /**
     * Formatting combo box.
     */
    private Combo formattingCbo;

    /**
     * Formatting label.
     */
    private Label formattingLbl;

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

    private ZoneCombinerComp zoneCombiner;

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

    private ImageRegistry getImageRegistry() {
        if (registry == null) {
            registry = new ImageRegistry();
            registry.put("gfepmunuon", Activator.imageDescriptorFromPlugin(
                    Activator.PLUGIN_ID, "icons/gfepmunuon.gif"));
            registry.put("gfeedit", Activator.imageDescriptorFromPlugin(
                    Activator.PLUGIN_ID, "icons/gfeedit.gif"));
            registry.put("gfeterm", Activator.imageDescriptorFromPlugin(
                    Activator.PLUGIN_ID, "icons/gfeterm.gif"));
            registry.put("loadDraft", Activator.imageDescriptorFromPlugin(
                    Activator.PLUGIN_ID, "icons/loadDraft.gif"));
            registry.put("gferun", Activator.imageDescriptorFromPlugin(
                    Activator.PLUGIN_ID, "icons/gferun.gif"));
            registry.put("gfecancel", Activator.imageDescriptorFromPlugin(
                    Activator.PLUGIN_ID, "icons/gfecancel.gif"));
            registry.put("gfestop", Activator.imageDescriptorFromPlugin(
                    Activator.PLUGIN_ID, "icons/gfestop.gif"));

        }

        return registry;
    }

    /**
     * Initialize the composite.
     */
    private void init() {
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        GridLayout gl = new GridLayout(1, false);
        gl.marginHeight = 1;
        gl.marginWidth = 1;
        setLayout(gl);
        setLayoutData(gd);

        zoneCombinerImg = getImageRegistry().get("gfepmunuon");
        productEditorImg = getImageRegistry().get("gfeedit");
        outputLogImg = getImageRegistry().get("gfeterm");
        loadDraftImg = getImageRegistry().get("loadDraft");
        runFormatterImg = getImageRegistry().get("gferun");
        abortFormatterImg = getImageRegistry().get("gfecancel");
        closeTabImg = getImageRegistry().get("gfestop");

        initializeComponents();

        this.pack();

    }

    /**
     * Initialize the controls on the composite.
     */
    private void initializeComponents() {
        createIconButtonsComp();

        createGridStackComposite();
    }

    /**
     * Create the control buttons (no text, just icons).
     */
    private void createIconButtonsComp() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite buttonBarComp = new Composite(this, SWT.BORDER);
        buttonBarComp.setLayout(new GridLayout(10, false));
        buttonBarComp.setLayoutData(gd);

        if (editorCorrectionMode == false) {
            // -------------------------------------------
            // Create the left side buttons.
            // -------------------------------------------
            zoneCombinerBtn = new Button(buttonBarComp, SWT.TOGGLE);
            zoneCombinerBtn.setImage(zoneCombinerImg);
            zoneCombinerBtn.setSelection(true);
            zoneCombinerBtn.setToolTipText("Zone Combiner");
            zoneCombinerBtn.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent event) {
                    productEditorBtn.setSelection(false);
                    outputLogBtn.setSelection(false);

                    // Hide the Output Log composite
                    ((GridData) outputLogComp.getLayoutData()).exclude = true;
                    outputLogComp.setVisible(false);

                    // Hide the Product Editor composite
                    ((GridData) productEditorComp.getLayoutData()).exclude = true;
                    productEditorComp.setVisible(false);

                    // Show the Zone Combiner composite
                    ((GridData) zoneCombiner.getLayoutData()).exclude = false;
                    zoneCombiner.setVisible(true);
                    // Re-layout the stack grid composite
                    stackGridComp.layout();
                }
            });

            productEditorBtn = new Button(buttonBarComp, SWT.TOGGLE);
            productEditorBtn.setImage(productEditorImg);
            productEditorBtn.setToolTipText("Product Editor");
            productEditorBtn.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent event) {
                    productEditorBtnSelected();
                }
            });

            outputLogBtn = new Button(buttonBarComp, SWT.TOGGLE);
            outputLogBtn.setImage(outputLogImg);
            outputLogBtn.setToolTipText("View Output");
            outputLogBtn.setEnabled(false);
            outputLogBtn.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent event) {
                    outputLogBtnSelected();
                }
            });

            gd = new GridData(20, SWT.DEFAULT);
            Label fillerLbl = new Label(buttonBarComp, SWT.NONE);
            fillerLbl.setLayoutData(gd);

            loadDraftBtn = new Button(buttonBarComp, SWT.PUSH);
            loadDraftBtn.setImage(loadDraftImg);
            loadDraftBtn.setToolTipText("Load Draft");
            loadDraftBtn.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent event) {
                    productEditorComp.loadDraft();
                    ProductAreaComp.this.productEditorBtnSelected();
                }
            });

            runFormatterBtn = new Button(buttonBarComp, SWT.PUSH);
            runFormatterBtn.setImage(runFormatterImg);
            runFormatterBtn.setToolTipText("Run Formatter");
            runFormatterBtn.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent event) {
                    outputLogBtn.setEnabled(false);
                    productEditorBtnSelected();

                    if (okToLoseText()) {
                        DatabaseID dbId = ((FormatterLauncherDialog) productTabCB)
                                .getSelectedDataSource(productName);

                        if (dbId != null) {
                            productEditorComp.clearProductText();
                            abortFormatterBtn.setEnabled(true);
                            // closeTabBtn.setEnabled(false);
                            runFormatterBtn.setEnabled(false);
                            String vtecMode = "";
                            if (formattingCbo.isVisible()) {
                                vtecMode = formattingCbo.getText();
                            } else {
                                int hazIndex = productName.indexOf("Hazard_");
                                if (hazIndex > -1) {
                                    String category = productName.substring(
                                            hazIndex + 7, hazIndex + 10);
                                    vtecMode = textProductMgr
                                            .getVtecMessageType(category);
                                    if (vtecMode == null) {
                                        vtecMode = "";
                                    }
                                }
                            }

                            // Get the source database
                            zoneCombiner.applyZoneCombo();
                            FormatterUtil.runFormatterScript(dataMgr,
                                    textProductMgr, productName,
                                    dbId.toString(), vtecMode,
                                    ProductAreaComp.this);
                        }
                    }
                }
            });

            abortFormatterBtn = new Button(buttonBarComp, SWT.PUSH);
            abortFormatterBtn.setImage(abortFormatterImg);
            abortFormatterBtn.setToolTipText("Cancel Run");
            abortFormatterBtn.setEnabled(false);
            abortFormatterBtn.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent e) {
                    String taskName = textProductMgr.getModuleName(productName);
                    AbstractGfeTask task = TaskManager.getInstance().getTask(
                            taskName);
                    if (task != null) {
                        TaskManager.getInstance().cancelTask(task);
                    }
                }
            });
        }

        // -------------------------------------------
        // Create the formatting combo box controls.
        // -------------------------------------------
        gd = new GridData(SWT.FILL, SWT.CENTER, true, true);
        Composite formatComp = new Composite(buttonBarComp, SWT.NONE);
        GridLayout gl = new GridLayout(2, false);
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        formatComp.setLayout(gl);
        formatComp.setLayoutData(gd);

        gd = new GridData(120, SWT.DEFAULT);
        formattingLbl = new Label(formatComp, SWT.RIGHT);
        formattingLbl.setText("Formatting: ");
        formattingLbl.setLayoutData(gd);
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
                String vtecMode = textProductMgr.getVtecMessageType(pil
                        .substring(0, 3));
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
        progressBar = new ProgressBar(buttonBarComp, SWT.INDETERMINATE);
        progressBar.setVisible(false);

        // -------------------------------------------
        // Create the right side close button.
        // -------------------------------------------

        closeTabBtn = new Button(buttonBarComp, SWT.PUSH);
        closeTabBtn.setImage(closeTabImg);
        closeTabBtn.setToolTipText("Close Tab");
        closeTabBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
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
        });
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

    private void productEditorBtnSelected() {
        zoneCombinerBtn.setSelection(false);
        outputLogBtn.setSelection(false);

        // Hide the Zone Combiner composite
        ((GridData) zoneCombiner.getLayoutData()).exclude = true;
        zoneCombiner.setVisible(false);

        // Hide the Output Log composite
        ((GridData) outputLogComp.getLayoutData()).exclude = true;
        outputLogComp.setVisible(false);

        // Show the Product Editor composite
        ((GridData) productEditorComp.getLayoutData()).exclude = false;
        productEditorComp.setVisible(true);

        // Re-layout the stack grid composite
        stackGridComp.layout();

    }

    private void outputLogBtnSelected() {
        zoneCombinerBtn.setSelection(false);
        productEditorBtn.setSelection(false);

        // Hide the Zone Combiner composite
        ((GridData) zoneCombiner.getLayoutData()).exclude = true;
        zoneCombiner.setVisible(false);

        // Hide the Product Editor composite
        ((GridData) productEditorComp.getLayoutData()).exclude = true;
        productEditorComp.setVisible(false);

        // Show the Output Log composite
        ((GridData) outputLogComp.getLayoutData()).exclude = false;
        outputLogComp.showLog(textProductMgr.getModuleName(productName));
        outputLogComp.setVisible(true);

        // Re-layout the stack grid composite
        stackGridComp.layout();
    }

    /**
     * Create a composite with a GridLayout that will act like a StackLayout.
     */
    private void createGridStackComposite() {
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        stackGridComp = new Composite(this, SWT.NONE);
        GridLayout gl = new GridLayout(1, false);
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        stackGridComp.setLayout(gl);
        stackGridComp.setLayoutData(gd);

        if (!editorCorrectionMode) {
            createZoneCombinerComp();
        }

        createProductEditorComp();

        if (!editorCorrectionMode) {
            createOutputLogComp();
        }

        if (editorCorrectionMode) {
            // Show the Product Editor composite
            ((GridData) productEditorComp.getLayoutData()).exclude = false;
            productEditorComp.setVisible(true);

            // Re-layout the stack grid composite
            stackGridComp.layout();
        }
    }

    /**
     * Create the Zone Combiner composite.
     */
    private void createZoneCombinerComp() {
        zoneCombiner = new ZoneCombinerComp(stackGridComp, productTabCB,
                productName, getTextProductManager(), this.dataMgr);
    }

    /**
     * Create the Product Editor composite.
     */
    private void createProductEditorComp() {
        productEditorComp = new ProductEditorComp(stackGridComp,
                textProductMgr.getProductDefinition(productName), productName,
                editorCorrectionMode, this, dataMgr);
        ((GridData) productEditorComp.getLayoutData()).exclude = true;
        productEditorComp.setVisible(false);
        stackGridComp.layout();
    }

    /**
     * Create the Output Log composite.
     */
    private void createOutputLogComp() {
        outputLogComp = new OutputLogComp(stackGridComp);
        ((GridData) outputLogComp.getLayoutData()).exclude = true;
        outputLogComp.setVisible(false);
        stackGridComp.layout();
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
    public void textProductQueued() {
        productTabCB.setTabState(ConfigData.productStateEnum.Queued,
                productName);
    }

    @Override
    public void textProductFinished(String productText,
            ConfigData.productStateEnum state) {

        if (isTabClosed == true) {
            return;
        }

        abortFormatterBtn.setEnabled(false);
        runFormatterBtn.setEnabled(true);
        // closeTabBtn.setEnabled(true);
        outputLogBtn.setEnabled(true);
        if (state == ConfigData.productStateEnum.Finished) {
            if (productText != null) {
                productEditorComp.retrieveActiveVTEC();
                productEditorComp.setProductText(productText);

                // handle autoWrite and autoStore...
                productEditorComp.doAutoStuff();
            }

            productEditorBtn.setSelection(true);
            productEditorBtnSelected();
        } else if (state == ConfigData.productStateEnum.Failed) {
            outputLogBtn.setSelection(true);
            outputLogBtnSelected();
        }
    }

    @Override
    public void startProgressBar(ConfigData.productStateEnum state) {
        if (isTabClosed == true) {
            return;
        }

        progressBar.setVisible(true);
        productTabCB.setTabState(state, productName);
    }

    @Override
    public void stopProgressBar(ConfigData.productStateEnum state) {
        if (isTabClosed == true) {
            return;
        }

        progressBar.setVisible(false);
        productTabCB.setTabState(state, productName);
    }

    @Override
    public void setTransmissionState(productStateEnum state) {
        productTabCB.setTabState(state, productName);
    }

}
