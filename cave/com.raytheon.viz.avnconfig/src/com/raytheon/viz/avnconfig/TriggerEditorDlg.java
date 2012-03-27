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
package com.raytheon.viz.avnconfig;

import java.io.FileNotFoundException;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.commons.configuration.ConfigurationException;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.ScrolledComposite;
import org.eclipse.swt.events.ControlAdapter;
import org.eclipse.swt.events.ControlEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Dialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.viz.core.catalog.DirectDbQuery;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.avnconfig.AvnConfigConstants.triggerType;

/**
 * Trigger editor dialog. There are no longer any references to this class in
 * any of the java and python code or in the xml configuration files. Consider
 * removing this class from the base line.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 22 MAY 2008  1119       lvenable    Initial creation.
 * 6/16/2008    937        grichard    Updated dialog title.
 *  9 JUL 2010  5078       rferrel     Updated to handle File not Found.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
@Deprecated
public class TriggerEditorDlg extends Dialog {
    /**
     * Dialog shell.
     */
    private Shell shell;

    /**
     * The display control.
     */
    private Display display;

    /**
     * Composite containing message status controls.
     */
    private MessageStatusComp msgStatusComp;

    /**
     * TAF scrolled composite.
     */
    private ScrolledComposite tafScrolledComp;

    /**
     * METAR scrolled composite.
     */
    private ScrolledComposite metarScrolledComp;

    /**
     * CCFP scrolled composite.
     */
    private ScrolledComposite ccfpScrolledComp;

    /**
     * TAF label and text composite.
     */
    private LabelTextComp tafLabelTextComp;

    /**
     * METAR label and text composite.
     */
    private LabelTextComp metarLabelTextComp;

    /**
     * CCFP label and text composite.
     */
    private LabelTextComp ccfpLabelTextComp;

    /**
     * Map of TAF sites.
     */
    private Map<String, String> tafDataMap;

    /**
     * Map of METAR sites.
     */
    private Map<String, String> metarDataMap;

    /**
     * Map of CCFP data.
     */
    private Map<String, String> ccfpDataMap;

    /**
     * Scrolled composite width.
     */
    private final int SCROLLED_COMP_WIDTH = 190;

    /**
     * Scrolled composite height.
     */
    private final int SCROLLED_COMP_HEIGHT = 400;

    /**
     * Background color of a control that has invalid data.
     */
    private Color incorrectColor;

    /**
     * Text control font.
     */
    private Font textFont;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     */
    public TriggerEditorDlg(Shell parent) {
        super(parent, 0);
    }

    /**
     * Open method used to display the dialog.
     * 
     * @return Null.
     */
    public Object open() {
        Shell parent = getParent();
        display = parent.getDisplay();
        shell = new Shell(parent, SWT.DIALOG_TRIM);
        shell.setText("AvnFPS Trigger Editor");

        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 3;
        mainLayout.marginWidth = 3;
        mainLayout.verticalSpacing = 5;
        shell.setLayout(mainLayout);

        // Initialize all of the controls and layouts
        initializeComponents();

        shell.pack();

        shell.open();
        while (!shell.isDisposed()) {
            if (!display.readAndDispatch()) {
                display.sleep();
            }
        }
        incorrectColor.dispose();
        textFont.dispose();

        return null;
    }

    /**
     * Initialize the components on the display.
     */
    private void initializeComponents() {
        incorrectColor = new Color(display, new RGB(255, 215, 220));
        textFont = new Font(display, "Monospace", 10, SWT.NORMAL);

        tafDataMap = new LinkedHashMap<String, String>();
        metarDataMap = new LinkedHashMap<String, String>();
        ccfpDataMap = new LinkedHashMap<String, String>();

        populateDataMaps();

        createTopControls();

        createScrolledCompositeLists();

        createBottomMessageControls();
    }

    /**
     * Create the buttons at the top of the display.
     */
    private void createTopControls() {
        // ------------------------------------------
        // Create the composite for the controls.
        // ------------------------------------------
        Composite controlComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(1, false);
        controlComp.setLayout(gl);
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        controlComp.setLayoutData(gd);

        // -------------------------------------------
        // Create a button composite for the buttons
        // -------------------------------------------
        Composite buttonComp = new Composite(controlComp, SWT.NONE);
        gl = new GridLayout(4, true);
        buttonComp.setLayout(gl);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        buttonComp.setLayoutData(gd);

        int buttonWidth = 100;

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = buttonWidth;
        Button updateBtn = new Button(buttonComp, SWT.PUSH);
        updateBtn.setText("Update");
        updateBtn
                .setToolTipText("Updates missing values using the afos2awips file");
        updateBtn.setLayoutData(gd);
        updateBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                update();
            }
        });

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = buttonWidth;
        Button makeBtn = new Button(buttonComp, SWT.PUSH);
        makeBtn.setText("Make");
        makeBtn.setToolTipText("Creates trigger file and updates watchwarn table");
        makeBtn.setLayoutData(gd);
        makeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                makeTriggers();
            }
        });

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = buttonWidth;
        Button closeBtn = new Button(buttonComp, SWT.PUSH);
        closeBtn.setText("Close");
        closeBtn.setToolTipText("Closes this dialog");
        closeBtn.setLayoutData(gd);
        closeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                shell.dispose();
            }
        });

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = buttonWidth;
        Button helpBtn = new Button(buttonComp, SWT.PUSH);
        helpBtn.setText("Help");
        helpBtn.setToolTipText("Shows help");
        helpBtn.setLayoutData(gd);
        helpBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                String description = "AvnFPS - Trigger Editor Help";
                String helpText = "This dialog is used to create a trigger file to get various\ntext products from the FXA text decoder that are important\nto the proper functioning of AvnFPS monitoring\ncapabilities.\n\nThe contents of latest trigger file is loaded when this\ndialog is displayed.  Use the \"Update\" button to update any\nmissing entries from AWIPS's afos2awips text file.\n\nYou can modify or add missing PILs in any of the text\nfields as needed. Pressing the\"Make\" button will write the\ntriggers to disk and update the watchwarn table in the\nfxatext database.\n\nThe \"Close\" button quits this application.\n\nThe \"Help\" button displays this help dialog.";
                HelpUsageDlg usageDlg = new HelpUsageDlg(shell, description,
                        helpText);
                usageDlg.open();
            }
        });
    }

    /**
     * Create the main composite that will contain the scrolled composites for
     * TAF, METAR, and CCFP.
     */
    private void createScrolledCompositeLists() {
        // ------------------------------------------
        // Create the composite for the controls.
        // ------------------------------------------
        Composite mainListComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(3, false);
        mainListComp.setLayout(gl);
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        mainListComp.setLayoutData(gd);

        // ------------------------------
        // Create the labels
        // ------------------------------

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Label tafLbl = new Label(mainListComp, SWT.CENTER);
        tafLbl.setText("TAF");
        tafLbl.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Label metarLbl = new Label(mainListComp, SWT.CENTER);
        metarLbl.setText("METAR");
        metarLbl.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Label ccfpLbl = new Label(mainListComp, SWT.CENTER);
        ccfpLbl.setText("CCFP");
        ccfpLbl.setLayoutData(gd);

        createTafSrolledComp(mainListComp);
        createMetarSrolledComp(mainListComp);
        createCcfpSrolledComp(mainListComp);
    }

    /**
     * Create the TAF scrolled composite.
     * 
     * @param mainListComp
     *            Parent composite.
     */
    private void createTafSrolledComp(Composite mainListComp) {
        tafScrolledComp = new ScrolledComposite(mainListComp, SWT.V_SCROLL
                | SWT.BORDER);
        GridLayout gl = new GridLayout(1, false);
        gl.verticalSpacing = 1;
        tafScrolledComp.setLayout(gl);
        GridData gd = new GridData(SCROLLED_COMP_WIDTH, SCROLLED_COMP_HEIGHT);
        gd.heightHint = SCROLLED_COMP_HEIGHT;
        tafScrolledComp.setLayoutData(gd);

        gl = new GridLayout(1, false);
        gl.verticalSpacing = 1;
        tafLabelTextComp = new LabelTextComp(tafScrolledComp, tafDataMap,
                incorrectColor, textFont);
        tafLabelTextComp.setLayout(gl);

        tafLabelTextComp.layout();

        tafScrolledComp.setContent(tafLabelTextComp);
        tafScrolledComp.setExpandHorizontal(true);
        tafScrolledComp.setExpandVertical(true);

        tafScrolledComp.addControlListener(new ControlAdapter() {
            @Override
            public void controlResized(ControlEvent e) {
                Rectangle r = tafScrolledComp.getClientArea();
                tafScrolledComp.setMinSize(tafLabelTextComp.computeSize(
                        r.width, SWT.DEFAULT));
            }
        });

        tafScrolledComp.layout();
    }

    /**
     * Create the METAR scrolled composite.
     * 
     * @param mainListComp
     *            Parent composite.
     */
    private void createMetarSrolledComp(Composite mainListComp) {
        metarScrolledComp = new ScrolledComposite(mainListComp, SWT.V_SCROLL
                | SWT.BORDER);
        GridLayout gl = new GridLayout(1, false);
        gl.verticalSpacing = 1;
        metarScrolledComp.setLayout(gl);
        GridData gd = new GridData(SCROLLED_COMP_WIDTH, SCROLLED_COMP_HEIGHT);
        gd.heightHint = SCROLLED_COMP_HEIGHT;
        metarScrolledComp.setLayoutData(gd);

        gl = new GridLayout(1, false);
        gl.verticalSpacing = 1;
        metarLabelTextComp = new LabelTextComp(metarScrolledComp, metarDataMap,
                incorrectColor, textFont);
        metarLabelTextComp.setLayout(gl);

        metarLabelTextComp.layout();

        metarScrolledComp.setContent(metarLabelTextComp);
        metarScrolledComp.setExpandHorizontal(true);
        metarScrolledComp.setExpandVertical(true);

        metarScrolledComp.addControlListener(new ControlAdapter() {
            @Override
            public void controlResized(ControlEvent e) {
                Rectangle r = metarScrolledComp.getClientArea();
                metarScrolledComp.setMinSize(metarLabelTextComp.computeSize(
                        r.width, SWT.DEFAULT));
            }
        });

        metarScrolledComp.layout();
    }

    /**
     * Create the CCFP scrolled composite.
     * 
     * @param mainListComp
     *            Parent composite.
     */
    private void createCcfpSrolledComp(Composite mainListComp) {
        ccfpScrolledComp = new ScrolledComposite(mainListComp, SWT.V_SCROLL
                | SWT.BORDER);
        GridLayout gl = new GridLayout(1, false);
        gl.verticalSpacing = 1;
        ccfpScrolledComp.setLayout(gl);
        GridData gd = new GridData(SCROLLED_COMP_WIDTH, SCROLLED_COMP_HEIGHT);
        gd.heightHint = SCROLLED_COMP_HEIGHT;
        ccfpScrolledComp.setLayoutData(gd);

        gl = new GridLayout(1, false);
        gl.verticalSpacing = 1;
        ccfpLabelTextComp = new LabelTextComp(ccfpScrolledComp, ccfpDataMap,
                incorrectColor, textFont);
        ccfpLabelTextComp.setLayout(gl);

        ccfpLabelTextComp.layout();

        ccfpScrolledComp.setContent(ccfpLabelTextComp);
        ccfpScrolledComp.setExpandHorizontal(true);
        ccfpScrolledComp.setExpandVertical(true);

        ccfpScrolledComp.addControlListener(new ControlAdapter() {
            @Override
            public void controlResized(ControlEvent e) {
                Rectangle r = ccfpScrolledComp.getClientArea();
                ccfpScrolledComp.setMinSize(ccfpLabelTextComp.computeSize(
                        r.width, SWT.DEFAULT));
            }
        });

        ccfpScrolledComp.layout();
    }

    /**
     * Create the message status composite.
     */
    private void createBottomMessageControls() {
        msgStatusComp = new MessageStatusComp(shell, null, null);
    }

    private void populateDataMaps() {
        try {
            ITafSiteConfig config = TafSiteConfigFactory.getInstance();
            Map<String, String> tafMap = config.getTriggers(triggerType.TAF);
            Map<String, String> mtrMap = config.getTriggers(triggerType.METAR);
            Map<String, String> cfpMap = config.getTriggers(triggerType.CCFP);
            List<String> ids = config.getSiteList();

            for (String id : ids) {
                String tafPil = tafMap.get(id);
                String mtrPil = mtrMap.get(id);

                if (tafPil == null) {
                    tafPil = "";
                }

                if (mtrPil == null) {
                    mtrPil = "";
                }

                tafDataMap.put(id, tafPil);
                metarDataMap.put(id, mtrPil);
            }

            Set<String> keys = cfpMap.keySet();

            for (String key : keys) {
                ccfpDataMap.put(key, cfpMap.get(key));
            }

            for (int i = 1; i < 4; i++) {
                String n = Integer.toBinaryString(i);

                if (n.length() == 1) {
                    n = "0" + n;
                }

                if (!ccfpDataMap.containsValue("MKCCFP" + n)) {
                    ccfpDataMap.put("CFP" + n, "MKCCFP" + n);
                }
            }
        } catch (FileNotFoundException e) {
            msgStatusComp.setMessageText(e.getMessage(), new RGB(255, 0, 0));
        } catch (ConfigurationException e) {
            msgStatusComp.setMessageText("An error occured.",
                    new RGB(255, 0, 0));
        }
    }

    private void makeTriggers() {
        try {
            ITafSiteConfig config = TafSiteConfigFactory.getInstance();
            config.setTriggers(tafDataMap, metarDataMap, ccfpDataMap);
        } catch (FileNotFoundException e) {
            msgStatusComp.setMessageText(e.getMessage(), new RGB(255, 0, 0));
        } catch (ConfigurationException e) {
            msgStatusComp.setMessageText("An error occured.",
                    new RGB(255, 0, 0));
        }
    }

    private void update() {
        try {
            ITafSiteConfig config = TafSiteConfigFactory.getInstance();
            List<String> ids = config.getSiteList();
            Map<String, String> tafMap = new HashMap<String, String>();
            Map<String, String> mtrMap = new HashMap<String, String>();
            String SQL = "SELECT afosid FROM afos_to_awips";
            List<Object[]> data;
            data = DirectDbQuery.executeQuery(SQL, "metadata",
                    DirectDbQuery.QueryLanguage.SQL);

            for (Object[] obj : data) {
                String afos = (String) obj[0];

                if (afos.length() < 6) {
                    continue;
                }

                String nnn = afos.substring(3, 6);

                if (nnn.equals("TAF")) {
                    tafMap.put(afos.substring(6, afos.length()), afos);
                }

                String tmp = afos.substring(0, 3) + "MTR" + afos.substring(6);
                mtrMap.put(afos.substring(6, afos.length()), tmp);
            }

            for (String site : ids) {
                if (tafDataMap.get(site) == null
                        || tafDataMap.get(site).equals("")) {
                    tafDataMap.put(site, tafMap.get(site.substring(1)));
                    tafLabelTextComp.updateTextControl(site,
                            tafMap.get(site.substring(1)));
                }

                if (metarDataMap.get(site) == null
                        || metarDataMap.get(site).equals("")) {
                    metarDataMap.put(site, mtrMap.get(site.substring(1)));
                    metarLabelTextComp.updateTextControl(site,
                            mtrMap.get(site.substring(1)));
                }
            }
        } catch (FileNotFoundException e) {
            msgStatusComp.setMessageText(e.getMessage(), new RGB(255, 0, 0));
        } catch (ConfigurationException e) {
            msgStatusComp.setMessageText("An error occured.",
                    new RGB(255, 0, 0));
        } catch (VizException e) {
            msgStatusComp.setMessageText("An error occured.",
                    new RGB(255, 0, 0));
        }
    }
}
