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
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.configuration.ConfigurationException;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Spinner;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.dataplugin.text.AfosWmoIdDataContainer;
import com.raytheon.uf.common.dataplugin.text.db.AfosToAwips;
import com.raytheon.uf.common.dataplugin.text.request.GetPartialAfosIdRequest;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.exception.LocalizationOpFailedException;
import com.raytheon.uf.common.pointdata.spatial.ObStation;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.catalog.LayerProperty;
import com.raytheon.uf.viz.core.catalog.ScriptCreator;
import com.raytheon.uf.viz.core.comm.Loader;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.requests.ThriftClient;
import com.raytheon.viz.avncommon.AvnMessageMgr.StatusMessageType;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;
import com.raytheon.viz.ui.dialogs.ICloseCallback;
import com.vividsolutions.jts.geom.Point;

/**
 * TAF site information editor dialog.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 22 MAY 2008  1119       lvenable    Initial creation
 *  9 Jul 2010  5078       rferrel     Handle File Not Found.
 *  7 Dec 2010  7621       rferrel     Changes to get the template's
 *                                     Make and Edit buttons to work.
 * 10 Dec 2010  7662       rferrel     Create and use getObStation.
 *  9 May 2011  8856       rferrel     Code cleanup
 * 12 Oct 2012  1229       rferrel     Now a subclass of CaveSWTDialog
 *                                      and made non-blocking.
 * 15 Oct 2012  1229       rferrel     Changes for non-blocking TextEditorSetupDlg.
 * 15 OCT 2012  1229       rferrel     Changes for non-blocking HelpUsageDlg.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class TafSiteInfoEditorDlg extends CaveSWTDialog {
    private final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(TafSiteInfoEditorDlg.class);

    /**
     * A site's template hours.
     */
    private final String[] START_HOURS = { "00", "06", "12", "18" };

    /**
     * Composite containing message status controls.
     */
    private MessageStatusComp msgStatusComp;

    /**
     * Site ID text control.
     */
    private Text siteIdTF;

    /**
     * WMO text control.
     */
    private Text wmoTF;

    /**
     * AFOS text control.
     */
    private Text afosTF;

    /**
     * Hours spinner control.
     */
    private Spinner hoursSpnr;

    /**
     * Visibility text control.
     */
    private Text visibilityTF;

    /**
     * Ceiling text control.
     */
    private Text ceilingTF;

    /**
     * Radar cutoff text control.
     */
    private Text radarCutoffTF;

    /**
     * Profiler cutoff text control.
     */
    private Text profilerCutoffTF;

    /**
     * Latitude text control.
     */
    private Text latitudeTF;

    /**
     * Longitude text control.
     */
    private Text longitudeTF;

    /**
     * Elevation text control.
     */
    private Text elevationTF;

    /**
     * Runway text control.
     */
    private Text runwayTF;

    /**
     * METAR text control.
     */
    private Text metarTF;

    /**
     * ETAMOS text control.
     */
    private Text nammosTF;

    /**
     * Radars text control.
     */
    private Text radarsTF;

    /**
     * GFSLAMP text control.
     */
    private Text gfslampTF;

    /**
     * Profiler text control.
     */
    private Text profilersTF;

    /**
     * GFSMOS text control.
     */
    private Text gfsmosTF;

    /**
     * ETA text control.
     */
    private Text namTF;

    /**
     * ACARS text control.
     */
    private Text acarsTF;

    /**
     * Issue combo box.
     */
    private Combo issueCbo;

    /**
     * Impact QC check box.
     */
    private Button impactQcChk;

    /**
     * Climate QC check box.
     */
    private Button climateQcChk;

    /**
     * Current Wx QC check box.
     */
    private Button currentWxQcChk;

    /**
     * Incorrect color used when there are incorrect values entered in the text
     * control.
     */
    private Color incorrectColor;

    /**
     * Correct color used when there are correct values entered in the text
     * control.
     */
    private Color correctColor;

    /**
     * Possible to have a different dialog for each issue time.
     */
    private Map<String, TextEditorSetupDlg> editorDlgMap;

    private HelpUsageDlg usageDlg;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     */
    public TafSiteInfoEditorDlg(Shell parent) {
        super(parent, SWT.DIALOG_TRIM, CAVE.DO_NOT_BLOCK);
        setText("AvnFPS TAF Site Info Editor");
        editorDlgMap = new HashMap<String, TextEditorSetupDlg>();
    }

    @Override
    protected void initializeComponents(Shell shell) {

        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 3;
        mainLayout.marginWidth = 3;
        mainLayout.verticalSpacing = 5;
        shell.setLayout(mainLayout);

        // Initialize all of the controls and layouts
        initializeComponents();
    }

    @Override
    protected void disposed() {
        if (incorrectColor != null) {
            incorrectColor.dispose();
        }

        if (correctColor != null) {
            correctColor.dispose();
        }
    }

    /**
     * Initialize the components on the display.
     */
    private void initializeComponents() {
        Display display = getParent().getDisplay();

        incorrectColor = new Color(display, new RGB(255, 215, 220));

        createTopControls();

        createTafHeaderDuration();

        createThresholds();

        createGeography();

        createAlternateIds();

        createQC();

        createTemplates();

        createBottomMessageControls();
    }

    /**
     * Converts an array into a comma separated string
     * 
     * @param array
     * @return string
     */
    private String arrayToString(String[] array) {
        // TODO convert to use StringBuilder.
        String s = "";

        for (int i = 0; i < array.length; i++) {
            s += array[i];

            if (i < (array.length - 1)) {
                s += ", ";
            }
        }

        return s;
    }

    /**
     * Clear all the text fields.
     */
    private void clearTF() {
        wmoTF.setText("");
        afosTF.setText("");
        visibilityTF.setText("");
        ceilingTF.setText("");
        radarCutoffTF.setText("");
        profilerCutoffTF.setText("");
        latitudeTF.setText("");
        longitudeTF.setText("");
        elevationTF.setText("");
        runwayTF.setText("");
        metarTF.setText("");
        gfslampTF.setText("");
        gfsmosTF.setText("");
        nammosTF.setText("");
        namTF.setText("");
        acarsTF.setText("");
        radarsTF.setText("");
        profilersTF.setText("");
    }

    /**
     * Create the controls at the top of the display.
     */
    private void createTopControls() {
        // ------------------------------------------
        // Create the composite for the controls.
        // ------------------------------------------
        Composite controlComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(7, false);
        controlComp.setLayout(gl);
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        controlComp.setLayoutData(gd);

        Label siteIdLbl = new Label(controlComp, SWT.NONE);
        siteIdLbl.setText("Site ID:");

        gd = new GridData(40, SWT.DEFAULT);
        siteIdTF = new Text(controlComp, SWT.BORDER);
        siteIdTF.setBackground(incorrectColor);
        siteIdTF.setLayoutData(gd);
        siteIdTF.addKeyListener(new KeyAdapter() {
            @Override
            public void keyReleased(KeyEvent e) {
                if (e.character == SWT.CR) {
                    loadSite();
                }
            }
        });

        int buttonWidth = 80;

        gd = new GridData(SWT.RIGHT, SWT.DEFAULT, true, false);
        gd.widthHint = buttonWidth;
        Button loadBtn = new Button(controlComp, SWT.PUSH);
        loadBtn.setText("Load");
        loadBtn.setToolTipText("Retrieves data for selected site");
        loadBtn.setLayoutData(gd);
        loadBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                loadSite();
            }
        });

        gd = new GridData(buttonWidth, SWT.DEFAULT);
        Button updateBtn = new Button(controlComp, SWT.PUSH);
        updateBtn.setText("Update");
        updateBtn.setToolTipText("Retrieves site info from fxa data files");
        updateBtn.setLayoutData(gd);
        updateBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                // loadDefaultData();
                validateData();
                loadDefaultData();
            }
        });

        gd = new GridData(buttonWidth, SWT.DEFAULT);
        Button saveBtn = new Button(controlComp, SWT.PUSH);
        saveBtn.setText("Save");
        saveBtn.setToolTipText("Saves site info to a file");
        saveBtn.setLayoutData(gd);
        saveBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (validateData()) {
                    TafSiteData site = new TafSiteData();
                    site.wmo = wmoTF.getText();
                    site.afos = afosTF.getText();
                    site.longitude = longitudeTF.getText();
                    site.latitude = latitudeTF.getText();
                    site.elevation = elevationTF.getText();
                    site.hours = Integer.toString(hoursSpnr.getSelection());
                    site.visibility = visibilityTF.getText().split(",");
                    site.ceiling = ceilingTF.getText().split(",");
                    site.radarCutoff = radarCutoffTF.getText().split(",");
                    site.profilerCutoff = profilerCutoffTF.getText().split(",");
                    site.runway = runwayTF.getText().split(",");
                    site.acars = acarsTF.getText();
                    site.metar = metarTF.getText().split(",");
                    site.nam = namTF.getText();
                    site.nammos = nammosTF.getText();
                    site.gfsmos = gfsmosTF.getText();
                    site.gfslamp = gfslampTF.getText();
                    site.radars = radarsTF.getText().split(",");
                    site.profilers = profilersTF.getText().split(",");
                    site.impactQc = impactQcChk.getSelection();
                    site.climateQc = climateQcChk.getSelection();
                    site.currentWxQc = currentWxQcChk.getSelection();
                    ITafSiteConfig config;

                    try {
                        config = TafSiteConfigFactory.getInstance();
                        config.setSite(siteIdTF.getText(), site);

                        msgStatusComp.setMessageText("Site data saved for "
                                + siteIdTF.getText(), new RGB(0, 255, 0));
                    } catch (ConfigurationException e) {
                        msgStatusComp.setMessageText("Error saving data for "
                                + siteIdTF.getText(), new RGB(255, 0, 0));
                    } catch (LocalizationOpFailedException e) {
                        msgStatusComp.setMessageText("Error saving data for "
                                + siteIdTF.getText(), new RGB(255, 0, 0));
                    } catch (IOException e) {
                        msgStatusComp.setMessageText("Error saving data for "
                                + siteIdTF.getText(), new RGB(255, 0, 0));
                    }
                }
            }
        });

        gd = new GridData(buttonWidth, SWT.DEFAULT);
        Button closeBtn = new Button(controlComp, SWT.PUSH);
        closeBtn.setText("Close");
        closeBtn.setToolTipText("Closes this dialog");
        closeBtn.setLayoutData(gd);
        closeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                shell.dispose();
            }
        });

        gd = new GridData(buttonWidth, SWT.DEFAULT);
        Button helpBtn = new Button(controlComp, SWT.PUSH);
        helpBtn.setText("Help");
        helpBtn.setToolTipText("Shows help");
        helpBtn.setLayoutData(gd);
        helpBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (mustCreate(usageDlg)) {
                    String description = "AvnFPS - Site Info Editor Help";

                    String helpText = "This dialog is used to define TAF site information.\n\nTo add a new site, enter site id and press \"Update\". Edit\ndisplayed entries and press \"Save\". Create default template\nfiles.\n\nTo change an existing TAF site attribute, enter site id and\npress the \"Load\" button.\n\nTo create template files, press \"Make\" in the \"Templates\"\narea.\n\nTo edit template file, select issue hour, then press \"Edit\"\nin the \"Templates\" area.\n\nYou can use \"Update\" button to extract information from\nAWIPS configuration files. Only non-empty fields will be\noverwritten.";
                    usageDlg = new HelpUsageDlg(shell, description,
                            helpText);
                    usageDlg.open();
                } else {
                    usageDlg.bringToTop();
                }
            }
        });
    }

    /**
     * Create the TAF Headers and TAF Duration groups and controls.
     */
    private void createTafHeaderDuration() {
        // ------------------------------------------
        // Create the composite for the controls.
        // ------------------------------------------
        Composite controlComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(2, false);
        controlComp.setLayout(gl);
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        controlComp.setLayoutData(gd);

        // -----------------------------------------------
        // Create the TAF headers group and controls
        // -----------------------------------------------
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Group tafHeadersGroup = new Group(controlComp, SWT.NONE);
        tafHeadersGroup.setText(" TAF Headers ");
        gl = new GridLayout(4, false);
        tafHeadersGroup.setLayout(gl);
        tafHeadersGroup.setLayoutData(gd);

        gd = new GridData(60, SWT.DEFAULT);
        Label wmoLbl = new Label(tafHeadersGroup, SWT.RIGHT);
        wmoLbl.setText("WMO");
        wmoLbl.setLayoutData(gd);

        gd = new GridData(150, SWT.DEFAULT);
        wmoTF = new Text(tafHeadersGroup, SWT.BORDER);
        wmoTF.setBackground(incorrectColor);
        wmoTF.setLayoutData(gd);

        gd = new GridData(60, SWT.DEFAULT);
        Label afosLbl = new Label(tafHeadersGroup, SWT.RIGHT);
        afosLbl.setText("AFOS");
        afosLbl.setLayoutData(gd);

        gd = new GridData(150, SWT.DEFAULT);
        afosTF = new Text(tafHeadersGroup, SWT.BORDER);
        afosTF.setBackground(incorrectColor);
        afosTF.setLayoutData(gd);

        // -----------------------------------------------
        // Create the TAF duration group and controls
        // -----------------------------------------------
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Group tafDurationGroup = new Group(controlComp, SWT.NONE);
        tafDurationGroup.setText(" TAF Duration ");
        gl = new GridLayout(2, false);
        tafDurationGroup.setLayout(gl);
        tafDurationGroup.setLayoutData(gd);

        gd = new GridData(80, SWT.DEFAULT);
        Label hoursLbl = new Label(tafDurationGroup, SWT.RIGHT);
        hoursLbl.setText("Hours");
        hoursLbl.setLayoutData(gd);

        gd = new GridData(30, SWT.DEFAULT);
        hoursSpnr = new Spinner(tafDurationGroup, SWT.BORDER);
        hoursSpnr.setDigits(0);
        hoursSpnr.setIncrement(6);
        hoursSpnr.setPageIncrement(6);
        hoursSpnr.setMinimum(24);
        hoursSpnr.setMaximum(30);
        hoursSpnr.setSelection(24);
        hoursSpnr.setLayoutData(gd);
    }

    /**
     * Create the Thresholds group and controls.
     */
    private void createThresholds() {
        // -----------------------------------------------
        // Create the thresholds group and controls
        // -----------------------------------------------
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Group tafHeadersGroup = new Group(shell, SWT.NONE);
        tafHeadersGroup.setText(" Thresholds ");
        GridLayout gl = new GridLayout(5, false);
        tafHeadersGroup.setLayout(gl);
        tafHeadersGroup.setLayoutData(gd);

        int textWidth = 250;

        Label visibilityLbl = new Label(tafHeadersGroup, SWT.NONE);
        visibilityLbl.setText("Visibility");

        gd = new GridData(textWidth, SWT.DEFAULT);
        visibilityTF = new Text(tafHeadersGroup, SWT.BORDER);
        visibilityTF.setBackground(incorrectColor);
        visibilityTF.setLayoutData(gd);

        gd = new GridData(10, SWT.DEFAULT);
        Label filler1 = new Label(tafHeadersGroup, SWT.NONE);
        filler1.setLayoutData(gd);

        Label ceilingLbl = new Label(tafHeadersGroup, SWT.NONE);
        ceilingLbl.setText("Ceiling");

        gd = new GridData(textWidth, SWT.DEFAULT);
        ceilingTF = new Text(tafHeadersGroup, SWT.BORDER);
        ceilingTF.setBackground(incorrectColor);
        ceilingTF.setLayoutData(gd);

        Label radarCutoffLbl = new Label(tafHeadersGroup, SWT.NONE);
        radarCutoffLbl.setText("Radar Cutoff");

        gd = new GridData(textWidth, SWT.DEFAULT);
        radarCutoffTF = new Text(tafHeadersGroup, SWT.BORDER);
        radarCutoffTF.setLayoutData(gd);

        gd = new GridData(10, SWT.DEFAULT);
        Label filler2 = new Label(tafHeadersGroup, SWT.NONE);
        filler2.setLayoutData(gd);

        Label profilerCutoffLbl = new Label(tafHeadersGroup, SWT.NONE);
        profilerCutoffLbl.setText("Profiler Cutoff");

        gd = new GridData(textWidth, SWT.DEFAULT);
        profilerCutoffTF = new Text(tafHeadersGroup, SWT.BORDER);
        profilerCutoffTF.setLayoutData(gd);
    }

    /**
     * Create Geography group and controls.
     */
    private void createGeography() {
        // -----------------------------------------------
        // Create the geography group and controls
        // -----------------------------------------------
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Group geographyGroup = new Group(shell, SWT.NONE);
        geographyGroup.setText(" Geography ");
        GridLayout gl = new GridLayout(8, false);
        geographyGroup.setLayout(gl);
        geographyGroup.setLayoutData(gd);

        gd = new GridData(60, SWT.DEFAULT);
        Label latLbl = new Label(geographyGroup, SWT.RIGHT);
        latLbl.setText("Latitude");
        latLbl.setLayoutData(gd);

        gd = new GridData(110, SWT.DEFAULT);
        latitudeTF = new Text(geographyGroup, SWT.BORDER);
        latitudeTF.setBackground(incorrectColor);
        latitudeTF.setLayoutData(gd);

        gd = new GridData(70, SWT.DEFAULT);
        Label lonLbl = new Label(geographyGroup, SWT.RIGHT);
        lonLbl.setText("Longitude");
        lonLbl.setLayoutData(gd);

        gd = new GridData(110, SWT.DEFAULT);
        longitudeTF = new Text(geographyGroup, SWT.BORDER);
        longitudeTF.setBackground(incorrectColor);
        longitudeTF.setLayoutData(gd);

        gd = new GridData(70, SWT.DEFAULT);
        Label elevationLbl = new Label(geographyGroup, SWT.RIGHT);
        elevationLbl.setText("Elevation");
        elevationLbl.setLayoutData(gd);

        gd = new GridData(50, SWT.DEFAULT);
        elevationTF = new Text(geographyGroup, SWT.BORDER);
        elevationTF.setBackground(incorrectColor);
        elevationTF.setLayoutData(gd);

        gd = new GridData(70, SWT.DEFAULT);
        Label runwaysLbl = new Label(geographyGroup, SWT.RIGHT);
        runwaysLbl.setText("Runway(s)");
        runwaysLbl.setLayoutData(gd);

        gd = new GridData(100, SWT.DEFAULT);
        runwayTF = new Text(geographyGroup, SWT.BORDER);
        runwayTF.setBackground(incorrectColor);
        runwayTF.setLayoutData(gd);
    }

    /**
     * Create the Alternate IDs group and controls.
     */
    private void createAlternateIds() {
        // -----------------------------------------------
        // Create the geography group and controls
        // -----------------------------------------------
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Group altIdsGroup = new Group(shell, SWT.NONE);
        altIdsGroup.setText(" Alternate Ids ");
        GridLayout gl = new GridLayout(8, false);
        altIdsGroup.setLayout(gl);
        altIdsGroup.setLayoutData(gd);

        int leadingLabelWidth = 120;
        int textWidth = 100;

        // METAR
        gd = new GridData(leadingLabelWidth, SWT.DEFAULT);
        Label metarsLbl = new Label(altIdsGroup, SWT.RIGHT);
        metarsLbl.setText("METAR");
        metarsLbl.setLayoutData(gd);

        gd = new GridData(textWidth, SWT.DEFAULT);
        metarTF = new Text(altIdsGroup, SWT.BORDER);
        metarTF.setBackground(incorrectColor);
        metarTF.setLayoutData(gd);

        gd = new GridData(10, SWT.DEFAULT);
        Label filler1 = new Label(altIdsGroup, SWT.NONE);
        filler1.setLayoutData(gd);

        // GFSLAMP
        Label gfslampLbl = new Label(altIdsGroup, SWT.RIGHT);
        gfslampLbl.setText("GFSLAMP");

        gd = new GridData(textWidth, SWT.DEFAULT);
        gfslampTF = new Text(altIdsGroup, SWT.BORDER);
        gfslampTF.setLayoutData(gd);

        gd = new GridData(10, SWT.DEFAULT);
        Label filler2 = new Label(altIdsGroup, SWT.NONE);
        filler2.setLayoutData(gd);

        // GFSMOS
        Label gfsmosLbl = new Label(altIdsGroup, SWT.RIGHT);
        gfsmosLbl.setText("GFSMOS");

        gd = new GridData(textWidth, SWT.DEFAULT);
        gfsmosTF = new Text(altIdsGroup, SWT.BORDER);
        gfsmosTF.setLayoutData(gd);

        // ETAMOS
        gd = new GridData(leadingLabelWidth, SWT.DEFAULT);
        Label etamosLbl = new Label(altIdsGroup, SWT.RIGHT);
        etamosLbl.setText("NAM-MOS");
        etamosLbl.setLayoutData(gd);

        gd = new GridData(textWidth, SWT.DEFAULT);
        nammosTF = new Text(altIdsGroup, SWT.BORDER);
        nammosTF.setLayoutData(gd);

        gd = new GridData(10, SWT.DEFAULT);
        Label filler3 = new Label(altIdsGroup, SWT.NONE);
        filler3.setLayoutData(gd);

        // ETA
        Label etaLbl = new Label(altIdsGroup, SWT.RIGHT);
        etaLbl.setText("NAM");

        gd = new GridData(textWidth, SWT.DEFAULT);
        namTF = new Text(altIdsGroup, SWT.BORDER);
        namTF.setLayoutData(gd);

        gd = new GridData(10, SWT.DEFAULT);
        Label filler4 = new Label(altIdsGroup, SWT.NONE);
        filler4.setLayoutData(gd);

        // ACARS
        Label acarsLbl = new Label(altIdsGroup, SWT.RIGHT);
        acarsLbl.setText("ACARS");

        gd = new GridData(textWidth, SWT.DEFAULT);
        acarsTF = new Text(altIdsGroup, SWT.BORDER);
        acarsTF.setLayoutData(gd);

        // Radars
        gd = new GridData(leadingLabelWidth, SWT.DEFAULT);
        Label radarsLbl = new Label(altIdsGroup, SWT.RIGHT);
        radarsLbl.setText("Radars");
        radarsLbl.setLayoutData(gd);

        gd = new GridData(textWidth, SWT.DEFAULT);
        radarsTF = new Text(altIdsGroup, SWT.BORDER);
        radarsTF.setLayoutData(gd);

        gd = new GridData(10, SWT.DEFAULT);
        Label filler5 = new Label(altIdsGroup, SWT.NONE);
        filler5.setLayoutData(gd);

        // Profilers
        Label profilersLbl = new Label(altIdsGroup, SWT.RIGHT);
        profilersLbl.setText("Profilers");

        gd = new GridData(textWidth, SWT.DEFAULT);
        profilersTF = new Text(altIdsGroup, SWT.BORDER);
        profilersTF.setLayoutData(gd);

        gd = new GridData(10, SWT.DEFAULT);
        Label filler6 = new Label(altIdsGroup, SWT.NONE);
        filler6.setLayoutData(gd);

        gd = new GridData(10, SWT.DEFAULT);
        Label filler7 = new Label(altIdsGroup, SWT.NONE);
        filler7.setLayoutData(gd);

        gd = new GridData(10, SWT.DEFAULT);
        Label filler8 = new Label(altIdsGroup, SWT.NONE);
        filler8.setLayoutData(gd);
    }

    private void createQC() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Group qcGroup = new Group(shell, SWT.NONE);
        qcGroup.setText(" QC Checks ");
        GridLayout gl = new GridLayout(1, false);
        qcGroup.setLayout(gl);
        qcGroup.setLayoutData(gd);

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        Composite controlComp = new Composite(qcGroup, SWT.NONE);
        gl = new GridLayout(6, false);
        controlComp.setLayout(gl);
        controlComp.setLayoutData(gd);

        Group impactGroup = new Group(controlComp, SWT.NONE);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gl = new GridLayout(1, false);
        impactGroup.setLayout(gl);
        impactGroup.setLayoutData(gd);
        impactQcChk = new Button(impactGroup, SWT.CHECK);
        impactQcChk.setText("Impact");

        Group climateGroup = new Group(controlComp, SWT.NONE);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gl = new GridLayout(1, false);
        climateGroup.setLayout(gl);
        climateGroup.setLayoutData(gd);
        climateQcChk = new Button(climateGroup, SWT.CHECK);
        climateQcChk.setText("Climate");

        Group currentWxGroup = new Group(controlComp, SWT.NONE);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gl = new GridLayout(1, false);
        currentWxGroup.setLayout(gl);
        currentWxGroup.setLayoutData(gd);
        currentWxQcChk = new Button(currentWxGroup, SWT.CHECK);
        currentWxQcChk.setText("Current Wx");
    }

    /**
     * Create the Templates group and controls.
     */
    private void createTemplates() {
        // -----------------------------------------------
        // Create the geography group and controls
        // -----------------------------------------------
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Group templatesGroup = new Group(shell, SWT.NONE);
        templatesGroup.setText(" Templates ");
        GridLayout gl = new GridLayout(1, false);
        templatesGroup.setLayout(gl);
        templatesGroup.setLayoutData(gd);

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        Composite controlComp = new Composite(templatesGroup, SWT.NONE);
        gl = new GridLayout(6, false);
        controlComp.setLayout(gl);
        controlComp.setLayoutData(gd);

        Label issueLbl = new Label(controlComp, SWT.NONE);
        issueLbl.setText("Issue");

        issueCbo = new Combo(controlComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        for (String startHour : START_HOURS) {
            issueCbo.add(startHour + "Z");
        }
        issueCbo.select(0);

        gd = new GridData(10, SWT.DEFAULT);
        Label filler1 = new Label(controlComp, SWT.NONE);
        filler1.setLayoutData(gd);

        gd = new GridData(80, SWT.DEFAULT);
        Button editBtn = new Button(controlComp, SWT.PUSH);
        editBtn.setText("Edit");
        editBtn.setToolTipText("Template editor");
        editBtn.setLayoutData(gd);
        editBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (isSiteIdValid()) {
                    try {
                        final String issueTime = issueCbo.getText().substring(
                                0, 2);
                        TextEditorSetupDlg editorDlg = editorDlgMap
                                .get(issueTime);
                        if (mustCreate(editorDlg)) {
                            ITafSiteConfig config = TafSiteConfigFactory
                                    .getInstance();
                            String siteId = siteIdTF.getText();
                            LocalizationFile lFile = config.getTemplateFile(
                                    siteId, issueTime);
                            editorDlg = new TextEditorSetupDlg(shell, lFile);
                            editorDlgMap.put(issueTime, editorDlg);
                            editorDlg.setCloseCallback(new ICloseCallback() {

                                @Override
                                public void dialogClosed(Object returnValue) {
                                    editorDlgMap.remove(issueTime);
                                }
                            });

                            editorDlg.open();
                        } else {
                            editorDlg.bringToTop();
                        }
                    } catch (FileNotFoundException e) {
                        msgStatusComp.setMessageText(e.getMessage(), new RGB(
                                255, 0, 0));
                    } catch (ConfigurationException e) {
                        msgStatusComp
                                .setMessageText(
                                        "An error occured when loading template to edit.",
                                        new RGB(255, 0, 0));
                    }
                }
            }
        });

        gd = new GridData(10, SWT.DEFAULT);
        Label filler2 = new Label(controlComp, SWT.NONE);
        filler2.setLayoutData(gd);

        gd = new GridData(80, SWT.DEFAULT);
        Button makeBtn = new Button(controlComp, SWT.PUSH);
        makeBtn.setText("Make");
        editBtn.setToolTipText("Makes default templates");
        makeBtn.setLayoutData(gd);
        makeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (isSiteIdValid()) {
                    try {
                        ITafSiteConfig config = TafSiteConfigFactory
                                .getInstance();

                        int duration = Integer.parseInt(hoursSpnr.getText());

                        for (String startHour : START_HOURS) {
                            int end = (Integer.parseInt(startHour) + duration) % 24;
                            String line = null;

                            if (end == 0) {
                                end = 24;
                            }

                            if (duration > 24) {
                                line = String.format(
                                        "%1$s DD%2$s00Z D1%2$s/D2%3$02d =",
                                        siteIdTF.getText(), startHour, end);
                            } else {
                                line = String.format(
                                        "%1$s DD%2$s00Z DD%2$s%3$02d =",
                                        siteIdTF.getText(), startHour, end);
                            }

                            config.saveTafTemplate(siteIdTF.getText(),
                                    startHour, line);
                        }

                        msgStatusComp.setMessageText(
                                "Default templates created for "
                                        + siteIdTF.getText(),
                                new RGB(0, 255, 0));
                    } catch (FileNotFoundException e) {
                        msgStatusComp.setMessageText(e.getMessage(), new RGB(
                                255, 0, 0));
                    } catch (ConfigurationException e) {
                        msgStatusComp.setMessageText(
                                "Error creating templates for "
                                        + siteIdTF.getText(),
                                new RGB(255, 0, 0));
                    } catch (LocalizationOpFailedException e) {
                        msgStatusComp.setMessageText(
                                "Error creating templates for "
                                        + siteIdTF.getText(),
                                new RGB(255, 0, 0));
                    }
                }
            }
        });
    }

    /**
     * Create the message status composite.
     */
    private void createBottomMessageControls() {
        msgStatusComp = new MessageStatusComp(shell,
                StatusMessageType.TafSiteInfo, null, null);
    }

    /**
     * Query for a icao station's information.
     * 
     * @param icao
     *            - station ID
     * @return obStation
     * @throws VizException
     *             - query failed or no data found
     */
    // TODO: If this query is every needed by another classes this method can be
    // made static and public. However, it should also be refactored to a common
    // package/class.
    private ObStation getObStation(String icao) throws VizException {
        // New up request constraint for table request and
        // also of the classname of the table for the request
        String tablePluginName = "table";
        String tableDatabaseName = "metadata";
        String tableClassName = ObStation.class.getName();
        RequestConstraint rcTable = new RequestConstraint(tablePluginName);
        RequestConstraint rcDatabase = new RequestConstraint(tableDatabaseName);
        RequestConstraint rcClass = new RequestConstraint(tableClassName);
        RequestConstraint rcGid = new RequestConstraint(ObStation.createGID(
                ObStation.CAT_TYPE_ICAO, icao));

        // New up hash map and populate with entry query
        // parameters before new'ng up layer property
        HashMap<String, RequestConstraint> query = new HashMap<String, RequestConstraint>();
        query.put("pluginName", rcTable);
        query.put("databasename", rcDatabase);
        query.put("classname", rcClass);
        query.put("gid", rcGid);

        // New up layer property and set the entry parameters
        LayerProperty lpParm = new LayerProperty();
        lpParm.setEntryQueryParameters(query, false);
        // Create Image <Any> Script for table request
        String tableScript = ScriptCreator.createScript(lpParm);
        List<Object> list = Loader.loadData(tableScript, 10000);

        if (list.size() == 0) {
            throw new VizException("Station not found: " + icao);
        }
        return (ObStation) list.get(0);
    }

    /**
     * Load the default values for a new TAF site.
     */
    private void loadDefaultData() {
        String icao = siteIdTF.getText().trim();
        try {
            ObStation obstation = getObStation(icao);
            Point p = obstation.getLocation();
            String longitude = Double.toString(p.getX());
            String latitude = Double.toString(p.getY());
            String elevation = Integer.toString(obstation.getElevation());
            GetPartialAfosIdRequest afosRequest = new GetPartialAfosIdRequest();
            afosRequest.setCccc(null);
            afosRequest.setNnn("TAF");
            afosRequest.setXxx(siteIdTF.getText(1, 3));
            Object afosResponse = ThriftClient.sendRequest(afosRequest);

            if (afosResponse == null) {
                throw new Exception("afosResponse is null");
            }
            if (!(afosResponse instanceof AfosWmoIdDataContainer)) {
                throw new Exception(
                        "afosResponse is not an instance of AfosWmoIdDataContainer");
            }

            AfosWmoIdDataContainer container = (AfosWmoIdDataContainer) afosResponse;

            if (container.getErrorMessage() != null) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error occurred looking up WMO IDs\nMessage from server["
                                + container.getErrorMessage() + "]");
            }

            java.util.List<AfosToAwips> list = container.getIdList();

            String afos = "";
            String wmo = "";

            if (list.size() < 1) {
                String msg = "AFOS ID TAF" + afosRequest.getXxx()
                        + " not found in afos_to_awips table.";
                msgStatusComp.setMessageText(msg, new RGB(255, 0, 0));
            } else {
                AfosToAwips a2a = list.get(0);
                afos = a2a.getAfosid();
                wmo = a2a.getWmottaaii() + " " + a2a.getWmocccc();
            }

            wmoTF.setText(wmo);
            afosTF.setText(afos);
            longitudeTF.setText(longitude);
            latitudeTF.setText(latitude);
            elevationTF.setText(elevation);
            hoursSpnr.setSelection(24);
            visibilityTF.setText("0.5,1.0,2.0,3.0,6.0");
            ceilingTF.setText("200,600,1000,2000,3100");
            runwayTF.setText("0");
            acarsTF.setText(siteIdTF.getText(1, 3));
            metarTF.setText(siteIdTF.getText());
            namTF.setText(siteIdTF.getText());
            nammosTF.setText(siteIdTF.getText());
            gfsmosTF.setText(siteIdTF.getText());
            gfslampTF.setText(siteIdTF.getText());
            radarCutoffTF.setText("");
            profilerCutoffTF.setText("");
            radarsTF.setText("");
            profilersTF.setText("");
            impactQcChk.setSelection(true);
            climateQcChk.setSelection(true);
            currentWxQcChk.setSelection(true);

            msgStatusComp.setMessageText("Default data successfuly loaded for "
                    + siteIdTF.getText(), new RGB(0, 255, 0));
        } catch (VizException e) {
            msgStatusComp.setMessageText(e.getMessage(), new RGB(255, 0, 0));
            clearTF();
            siteIdTF.setText(icao);
        } catch (Exception e) {
            msgStatusComp.setMessageText(
                    "Failed to find geography for " + icao, new RGB(255, 0, 0));
            clearTF();
            siteIdTF.setText(icao);
        }

        validateData();
    }

    /**
     * Validate the data and color the text fields accordingly.
     * 
     * @return boolean - True if all the data is valid.
     */
    private boolean validateData() {
        Display display = getParent().getDisplay();
        boolean isValid = true;
        correctColor = new Color(display, new RGB(255, 255, 255));

        // afos
        if (isAfosValid()) {
            afosTF.setBackground(correctColor);
        } else {
            afosTF.setBackground(incorrectColor);
            isValid = false;
        }
        // siteId
        if (isSiteIdValid()) {
            siteIdTF.setBackground(correctColor);
        } else {
            siteIdTF.setBackground(incorrectColor);
            isValid = false;
        }
        // wmo
        if (isWmoValid()) {
            wmoTF.setBackground(correctColor);
        } else {
            wmoTF.setBackground(incorrectColor);
            isValid = false;
        }
        // visibility
        if (isVsbyValid()) {
            visibilityTF.setBackground(correctColor);
        } else {
            visibilityTF.setBackground(incorrectColor);
            isValid = false;
        }
        // ceiling
        if (isCigValid()) {
            ceilingTF.setBackground(correctColor);
        } else {
            ceilingTF.setBackground(incorrectColor);
            isValid = false;
        }
        // radar cutoff
        if (isRadarHeightValid(radarCutoffTF.getText())) {
            radarCutoffTF.setBackground(correctColor);
        } else {
            radarCutoffTF.setBackground(incorrectColor);
            isValid = false;
        }
        // profiler cutoff
        if (isProfilerHeightValid(profilerCutoffTF.getText())) {
            profilerCutoffTF.setBackground(correctColor);
        } else {
            profilerCutoffTF.setBackground(incorrectColor);
            isValid = false;
        }
        // latitude
        if (isLatitudeValid()) {
            latitudeTF.setBackground(correctColor);
        } else {
            latitudeTF.setBackground(incorrectColor);
            isValid = false;
        }
        // longitude
        if (isLongitudeValid()) {
            longitudeTF.setBackground(correctColor);
        } else {
            longitudeTF.setBackground(incorrectColor);
            isValid = false;
        }
        // elevation
        if (isElevationValid()) {
            elevationTF.setBackground(correctColor);
        } else {
            elevationTF.setBackground(incorrectColor);
            isValid = false;
        }
        // runways
        if (isRunwayValid()) {
            runwayTF.setBackground(correctColor);
        } else {
            runwayTF.setBackground(incorrectColor);
            isValid = false;
        }
        // metar
        if (isMetarValid()) {
            metarTF.setBackground(correctColor);
        } else {
            metarTF.setBackground(incorrectColor);
            isValid = false;
        }
        // radars
        if (isRadarValid()) {
            radarsTF.setBackground(correctColor);
        } else {
            radarsTF.setBackground(incorrectColor);
            isValid = false;
        }
        // profilers
        if (isProfilerValid()) {
            profilersTF.setBackground(correctColor);
        } else {
            profilersTF.setBackground(incorrectColor);
            isValid = false;
        }
        // acars
        if (isOtherValid(acarsTF.getText())) {
            acarsTF.setBackground(correctColor);
        } else {
            acarsTF.setBackground(incorrectColor);
            isValid = false;
        }
        // eta
        if (isOtherValid(namTF.getText())) {
            namTF.setBackground(correctColor);
        } else {
            namTF.setBackground(incorrectColor);
            isValid = false;
        }
        // etamos
        if (isOtherValid(nammosTF.getText())) {
            nammosTF.setBackground(correctColor);
        } else {
            nammosTF.setBackground(incorrectColor);
            isValid = false;
        }
        // gfslamp
        if (isOtherValid(gfslampTF.getText())) {
            gfslampTF.setBackground(correctColor);
        } else {
            gfslampTF.setBackground(incorrectColor);
            isValid = false;
        }
        // gfsmos
        if (isOtherValid(gfsmosTF.getText())) {
            gfsmosTF.setBackground(correctColor);
        } else {
            gfsmosTF.setBackground(incorrectColor);
            isValid = false;
        }

        return isValid;
    }

    /**
     * Validate AFOS
     * 
     * @return boolean
     */
    private boolean isAfosValid() {
        return afosTF.getText().matches("^[A-Z]{3,3}TAF[0-9[A-Z]]{2,3}$");
    }

    /**
     * Validate Site ID
     * 
     * @return boolean
     */
    private boolean isSiteIdValid() {
        // mmaron DR 4344
        String siteIdTF_ = siteIdTF.getText().trim().toUpperCase();
        siteIdTF.setText(siteIdTF_);
        return siteIdTF.getText().matches("^[A-Z[0-9]]{4}$");
    }

    /**
     * Validate WMO
     * 
     * @return boolean
     */
    private boolean isWmoValid() {
        return wmoTF.getText().matches("^[A-Z]{4}[0-9]{2} [KPTI][A-Z]{3}$");
    }

    /**
     * Validate runway list
     * 
     * @return boolean
     */
    private boolean isRunwayValid() {
        boolean isValid = true;
        String[] runways = runwayTF.getText().split(",");

        if (runways.length == 0) {
            isValid = false;
        }

        try {
            for (String runway : runways) {
                int runwayInt = Integer.parseInt(runway.trim());

                if (runwayInt < 0 || runwayInt > 360) {
                    throw new Exception();
                }
            }
        } catch (Exception e) {
            isValid = false;
        }

        return isValid;
    }

    /**
     * Validate Height for Radar Cutoff list.
     * 
     * @return boolean
     */
    private boolean isRadarHeightValid(String text) {
        String radarsTxt = radarsTF.getText();

        if (text.equals("") && radarsTxt.equals("")) {
            return true;
        } else if ((text.equals("") && !radarsTxt.equals(""))
                || (!text.equals("") && radarsTxt.equals(""))) {
            return false;
        }

        String[] heights = text.split(",");
        String[] radars = radarsTxt.split(",");

        if (heights.length != radars.length) {
            return false;
        }

        try {
            for (String height : heights) {
                @SuppressWarnings("unused")
                int i = Integer.parseInt(height.trim());
            }
        } catch (Exception e) {
            return false;
        }

        return true;
    }

    /**
     * Validate Height for Profiler Cutoff list.
     * 
     * @return boolean
     */
    private boolean isProfilerHeightValid(String text) {
        String profilersTxt = profilersTF.getText();

        if (text.equals("") && profilersTxt.equals("")) {
            return true;
        } else if ((text.equals("") && !profilersTxt.equals(""))
                || (!text.equals("") && profilersTxt.equals(""))) {
            return false;
        }

        String[] heights = text.split(",");
        String[] profilers = profilersTxt.split(",");

        if (heights.length != profilers.length) {
            return false;
        }

        try {
            for (String height : heights) {
                @SuppressWarnings("unused")
                int i = Integer.parseInt(height.trim());
            }
        } catch (Exception e) {
            return false;
        }

        return true;
    }

    /**
     * Validate METAR
     * 
     * @return boolean
     */
    private boolean isMetarValid() {
        boolean isValid = true;
        String[] metars = metarTF.getText().split(",");

        if (metars.length == 0) {
            isValid = false;
        }

        for (String metar : metars) {
            if (!metar.matches("^[A-Z[0-9]]{4}$")) {
                isValid = false;
                break;
            }
        }

        return isValid;
    }

    /**
     * Validate Profilers list
     * 
     * @return boolean
     */
    private boolean isProfilerValid() {
        boolean isValid = true;

        if (profilersTF.getText().equals("")) {
            return true;
        }

        String[] profilers = profilersTF.getText().split(",");

        for (String profiler : profilers) {
            if (!profiler.matches("^[A-Z[0-9]]{5}$")) {
                isValid = false;
                break;
            }
        }

        return isValid;
    }

    /**
     * Validate Radars list
     * 
     * @return boolean
     */
    private boolean isRadarValid() {
        boolean isValid = true;

        if (radarsTF.getText().equals("")) {
            return true;
        }

        String[] radars = radarsTF.getText().split(",");

        for (String radar : radars) {
            if (!radar.matches("^[A-Z]{3,4}$")) {
                isValid = false;
                break;
            }
        }

        return isValid;
    }

    /**
     * Validate Visibility list
     * 
     * @return boolean
     */
    private boolean isVsbyValid() {
        boolean isValid = true;
        String[] vsbys = visibilityTF.getText().split(",");
        ArrayList<Double> list = new ArrayList<Double>();

        if (vsbys.length == 0) {
            return false;
        }

        try {
            for (String vsby : vsbys) {
                double d = Double.parseDouble(vsby.trim());

                if (d < 0.0 || d > 10.0) {
                    throw new Exception();
                }

                list.add(d);
            }

            Collections.sort(list);
            String vis = list.toString();
            vis = vis.substring(1, (vis.length() - 1));
            visibilityTF.setText(vis);
        } catch (Exception e) {
            isValid = false;
        }

        return isValid;
    }

    /**
     * Validate Ceiling list
     * 
     * @return boolean
     */
    private boolean isCigValid() {
        boolean isValid = true;
        String[] cigs = ceilingTF.getText().split(",");
        ArrayList<Integer> list = new ArrayList<Integer>();

        if (cigs.length == 0) {
            return false;
        }

        try {
            for (String cig : cigs) {
                int i = Integer.parseInt(cig.trim());

                if (i < 0 || i > 30000) {
                    throw new Exception();
                }

                list.add(i);
            }

            Collections.sort(list);
            String cig = list.toString();
            cig = cig.substring(1, (cig.length() - 1));
            ceilingTF.setText(cig);
        } catch (Exception e) {
            isValid = false;
        }

        return isValid;
    }

    /**
     * Validate ACARS, ETA, ETAMOS, GFSLAMP, GFSMOS, and NGMMOS
     * 
     * @return boolean
     */
    private boolean isOtherValid(String text) {
        boolean isValid = true;

        if (text.length() > 0) {
            isValid = text.matches("^[A-Z[0-9]]{3,4}$");
        }

        return isValid;
    }

    /**
     * Validate Latitude
     * 
     * @return boolean
     */
    private boolean isLatitudeValid() {
        boolean isValid = true;

        try {
            double lat = Double.parseDouble(latitudeTF.getText().trim());

            if (lat < -90.0 || lat > 90.0) {
                throw new Exception();
            }
        } catch (Exception e) {
            isValid = false;
        }

        return isValid;
    }

    /**
     * Validate Longitude
     * 
     * @return boolean
     */
    private boolean isLongitudeValid() {
        boolean isValid = true;

        try {
            double lon = Double.parseDouble(longitudeTF.getText().trim());

            if (lon < -180.0 || lon > 180.0) {
                throw new Exception();
            }
        } catch (Exception e) {
            isValid = false;
        }

        return isValid;
    }

    /**
     * Validate Elevation
     * 
     * @return boolean
     */
    private boolean isElevationValid() {
        return elevationTF.getText().matches("^-?[0-9]+$");
    }

    /**
     * Obtain the site information for the site user entered in site ID text
     * field and when valid populate the fields with the information.
     */
    private void loadSite() {
        boolean loadFailed = false;
        String loadFailedMsg = null;
        try {
            String tmp;
            ITafSiteConfig config = TafSiteConfigFactory.getInstance();
            // // mmaron - DR 4344
            // String siteIdTF_ = siteIdTF.getText().toUpperCase();
            // siteIdTF.setText(siteIdTF_);
            if (isSiteIdValid()) {
                siteIdTF.setBackground(correctColor);
            } else {
                siteIdTF.setBackground(incorrectColor);
                loadFailedMsg = "Invalid Site ID";
                throw new IllegalArgumentException(loadFailedMsg);
            }

            TafSiteData site = config.getSite(siteIdTF.getText());
            wmoTF.setText(site.wmo);
            afosTF.setText(site.afos);
            longitudeTF.setText(site.longitude);
            latitudeTF.setText(site.latitude);
            elevationTF.setText(site.elevation);
            hoursSpnr.setSelection(Integer.parseInt(site.hours));
            visibilityTF.setText(arrayToString(site.visibility));
            ceilingTF.setText(arrayToString(site.ceiling));

            tmp = arrayToString(site.radarCutoff);
            if (tmp.equals("\"\"")) {
                tmp = "";
            }
            radarCutoffTF.setText(tmp);

            tmp = arrayToString(site.profilerCutoff);
            if (tmp.equals("\"\"")) {
                tmp = "";
            }
            profilerCutoffTF.setText(tmp);

            runwayTF.setText(arrayToString(site.runway));
            acarsTF.setText(site.acars);
            metarTF.setText(arrayToString(site.metar));
            namTF.setText(site.nam);
            nammosTF.setText(site.nammos);
            gfsmosTF.setText(site.gfsmos);
            gfslampTF.setText(site.gfslamp);

            tmp = arrayToString(site.radars);
            if (tmp.equals("\"\"")) {
                tmp = "";
            }
            radarsTF.setText(tmp);

            tmp = arrayToString(site.profilers);
            if (tmp.equals("\"\"")) {
                tmp = "";
            }
            profilersTF.setText(tmp);

            impactQcChk.setSelection(site.impactQc);
            climateQcChk.setSelection(site.climateQc);
            currentWxQcChk.setSelection(site.currentWxQc);
            validateData();

            msgStatusComp.setMessageText(siteIdTF.getText()
                    + " data loaded successfuly.", new RGB(0, 255, 0));
        } catch (ConfigurationException e) {
            loadFailed = true;
        } catch (IllegalArgumentException e) {
            loadFailed = true;
        } catch (IOException e) {
            loadFailed = true;
        } finally {
            if (loadFailed == true) {
                String siteId = siteIdTF.getText();
                if (loadFailedMsg == null) {
                    loadFailedMsg = "Failed to retrieve configuration for "
                            + siteId;
                }
                msgStatusComp.setMessageText(loadFailedMsg, new RGB(255, 0, 0));
                clearTF();
                siteIdTF.setText(siteId);
            }
            validateData();
        }
    }
}
