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
package com.raytheon.viz.radar.ui;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Scale;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Spinner;

import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.viz.radar.IRadarConfigListener;
import com.raytheon.viz.radar.rsc.image.RadarSRMResource;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * This class displays the Radar Display Control dialog.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 04 DEC 2007  373        lvenable    Initial creation
 * 06 Nov 2014  DCS 16776  zwang       Add control for MBA
 * May 13, 2015 4461       bsteffen    Add option for sails.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class RadarDisplayControlDlg extends CaveSWTDialog {

    /**
     * Current values.
     */
    private RadarDisplayControls values;

    /**
     * Label font.
     */
    private Font labelFont;

    /**
     * Show storm scale.
     */
    private Scale showStormScale;

    /**
     * Show storm label.
     */
    private Label showStormLbl;

    /**
     * STI track to show combo box.
     */
    private Combo stiTrackToShowCbo;

    /**
     * Low POH combo box.
     */
    private Combo lowPohCbo;

    /**
     * Low POSH combo box.
     */
    private Combo lowPoshCbo;

    /**
     * High POH combo box.
     */
    private Combo highPohCbo;

    /**
     * High POSH combo box.
     */
    private Combo highPoshCbo;

    /**
     * Show elevated TVS check box.
     */
    private Button showElevatedTvsChk;

    /**
     * Show extrapolated features check box.
     */
    private Button showExtrapolatedChk;

    /**
     * Minimum feature scale label.
     */
    private Label minFeatureScaleLbl;

    /**
     * Minimum feature scale.
     */
    private Scale minFeatureScale;

    /**
     * Show MBA Wind Shear check box.
     */
    private Button showMbaWindShear;

    /**
     * Overlap Mesos check box.
     */
    private Button overlapMesosChk;

    /**
     * DMD track to show combo box.
     */
    private Combo dmdTrackToShowCbo;

    /**
     * Storm motion radio button.
     */
    private Button stormMotionRdo;

    /**
     * Average storm motion from STI radio button.
     */
    private Button averageStormRdo;

    /**
     * Custom storm motion radio button.
     */
    private Button customStormRdo;

    /**
     * Custom storm group container.
     */
    private Group customStormGroup;

    /**
     * Direction label.
     */
    private Label dirLbl;

    /**
     * Direction scale.
     */
    private Scale dirScale;

    /**
     * Direction spinner.
     */
    private Spinner dirSpnr;

    /**
     * Speed label.
     */
    private Label speedLbl;

    /**
     * Speed scale.
     */
    private Scale speedScale;

    /**
     * Speed spinner.
     */
    private Spinner speedSpnr;

    /**
     * enable sails frame coordinator check box.
     */
    private Button enableSails;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     */
    public RadarDisplayControlDlg(Shell parent) {
        super(parent);
        setText("Radar Display Controls");

        this.values = RadarDisplayManager.getInstance().getCurrentSettings();
    }

    @Override
    protected void disposed() {
        labelFont.dispose();
        setReturnValue(values);
    }

    @Override
    protected void initializeComponents(Shell shell) {
        setReturnValue(false);

        labelFont = new Font(shell.getDisplay(), "Arial", 10, SWT.ITALIC
                | SWT.BOLD);

        // Initialize all of the menus, controls, and layouts
        createStiControls();
        addSeparator();
        createHiControls();
        addSeparator();
        createTvsControls();
        addSeparator();
        createDmdMdTvsControls();
        addSeparator();
        createDmdControls();
        addSeparator();
        createMbaControls();
        addSeparator();
        createSrmControls();
        createCustomStormMotionGroup();
        addSeparator();
        createSailsControls();
        createCloseButton();

        updateDialogFromValues();
        // When something changes updateDialogValues in the UI Thread
        RadarDisplayManager.getInstance().addListener(
                new IRadarConfigListener() {
                    @Override
                    public void updateConfig() {
                        if (isDisposed()) {
                            RadarDisplayManager.getInstance().removeListener(
                                    this);
                            return;
                        }
                        VizApp.runAsync(new Runnable() {
                            @Override
                            public void run() {
                                if (!isDisposed()) {
                                    updateDialogFromValues();
                                }

                            }
                        });
                    }
                });
    }

    private void updateDialogFromValues() {
        showStormScale.setSelection(values.getStiNumStorms());
        showStormLbl.setText(String.valueOf(values.getStiNumStorms()));
        stiTrackToShowCbo.select(stiTrackToShowCbo.indexOf(values
                .getStiTrackType().toString()));
        lowPohCbo
                .select(lowPohCbo.indexOf(String.valueOf(values.getHiPOHLow())));
        lowPoshCbo.select(lowPoshCbo.indexOf(String.valueOf(values
                .getHiPOSHLow())));
        highPohCbo.select(highPohCbo.indexOf(String.valueOf(values
                .getHiPOHHigh())));
        highPoshCbo.select(highPoshCbo.indexOf(String.valueOf(values
                .getHiPOSHHigh())));
        showElevatedTvsChk.setSelection(values.isTvsShowElevated());
        showExtrapolatedChk.setSelection(values.isDmdMdTvsShowExtrapolated());
        minFeatureScale.setSelection(values.getDmdMinFeatureStrength());
        minFeatureScaleLbl.setText(String.valueOf(values
                .getDmdMinFeatureStrength()));
        overlapMesosChk.setSelection(values.isDmdShowOverlapping());
        dmdTrackToShowCbo.select(dmdTrackToShowCbo.indexOf(values
                .getDmdTrackType().toString()));
        showMbaWindShear.setSelection(values.isMbaShowWindShear());
        stormMotionRdo.setSelection(values.getSrmSource().equals(
                RadarSRMResource.SRMSource.WARNGEN));
        averageStormRdo.setSelection(values.getSrmSource().equals(
                RadarSRMResource.SRMSource.STI));
        customStormRdo.setSelection(values.getSrmSource().equals(
                RadarSRMResource.SRMSource.CUSTOM));
        dirScale.setSelection(values.getSrmDir());
        dirSpnr.setSelection(values.getSrmDir());
        speedScale.setSelection(values.getSrmSpeed());
        speedSpnr.setSelection(values.getSrmSpeed());
        enableCustomStormControls(values.getSrmSource().equals(
                RadarSRMResource.SRMSource.CUSTOM));
        enableSails.setSelection(values.isSailsFrameCoordinator());
    }

    /**
     * Create the STI controls.
     */
    private void createStiControls() {
        Composite stiComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(4, false);
        stiComp.setLayout(gl);

        GridData gd = new GridData(40, SWT.DEFAULT);
        Label stiLbl = new Label(stiComp, SWT.NONE);
        stiLbl.setFont(labelFont);
        stiLbl.setForeground(getDisplay().getSystemColor(SWT.COLOR_BLUE));
        stiLbl.setText("STI");
        stiLbl.setLayoutData(gd);

        gd = new GridData(160, SWT.DEFAULT);
        Label numStormsLbl = new Label(stiComp, SWT.RIGHT);
        numStormsLbl.setText("Num storms to show:");
        numStormsLbl.setLayoutData(gd);

        gd = new GridData(130, SWT.DEFAULT);
        showStormScale = new Scale(stiComp, SWT.HORIZONTAL);
        showStormScale.setMinimum(0);
        showStormScale.setMaximum(100);
        showStormScale.setPageIncrement(1);
        showStormScale.setLayoutData(gd);
        showStormScale.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                showStormLbl.setText(String.valueOf(showStormScale
                        .getSelection()));

            }
        });
        showStormScale.addKeyListener(new KeyAdapter() {

            @Override
            public void keyReleased(KeyEvent e) {
                if (e.keyCode == SWT.ARROW_LEFT || e.keyCode == SWT.ARROW_RIGHT) {
                    values.setStiNumStorms(showStormScale.getSelection());
                }
            }

        });
        showStormScale.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseUp(MouseEvent e) {
                values.setStiNumStorms(showStormScale.getSelection());
            }
        });

        gd = new GridData(30, SWT.DEFAULT);
        showStormLbl = new Label(stiComp, SWT.NONE);
        showStormLbl.setLayoutData(gd);

        // Filler label
        new Label(stiComp, SWT.NONE);

        gd = new GridData(160, SWT.DEFAULT);
        Label trackShowLbl = new Label(stiComp, SWT.RIGHT);
        trackShowLbl.setText("Type of track to show:");
        trackShowLbl.setLayoutData(gd);

        gd = new GridData();
        gd.horizontalSpan = 2;
        stiTrackToShowCbo = new Combo(stiComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        for (RadarDisplayManager.TrackTypes type : RadarDisplayManager.TrackTypes
                .values()) {
            stiTrackToShowCbo.add(type.toString());
        }
        stiTrackToShowCbo.setLayoutData(gd);
        stiTrackToShowCbo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                values.setStiTrackType(RadarDisplayManager.TrackTypes
                        .fromString(stiTrackToShowCbo.getText()));
            }
        });
    }

    /**
     * Create the HI controls.
     */
    private void createHiControls() {
        Composite hiComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(7, false);
        hiComp.setLayout(gl);

        GridData gd = new GridData(60, SWT.DEFAULT);
        Label hiLbl = new Label(hiComp, SWT.NONE);
        hiLbl.setFont(labelFont);
        hiLbl.setForeground(getDisplay().getSystemColor(SWT.COLOR_BLUE));
        hiLbl.setText("HI");
        hiLbl.setLayoutData(gd);

        // Filler labels
        new Label(hiComp, SWT.NONE);
        new Label(hiComp, SWT.NONE);

        gd = new GridData(70, SWT.DEFAULT);
        Label pohLbl = new Label(hiComp, SWT.CENTER);
        pohLbl.setText("POH");
        pohLbl.setLayoutData(gd);

        // Filler label
        new Label(hiComp, SWT.NONE);

        gd = new GridData(20, SWT.DEFAULT);
        Label filler4 = new Label(hiComp, SWT.NONE);
        filler4.setLayoutData(gd);

        gd = new GridData(70, SWT.DEFAULT);
        Label poshLbl = new Label(hiComp, SWT.CENTER);
        poshLbl.setText("POSH");
        poshLbl.setLayoutData(gd);

        // Filler label
        new Label(hiComp, SWT.NONE);

        Label lowLbl = new Label(hiComp, SWT.CENTER);
        lowLbl.setText("Low");

        new PohPoshCanvas(hiComp, true);

        gd = new GridData(70, SWT.DEFAULT);
        lowPohCbo = new Combo(hiComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        fillPohPoshComboBox(lowPohCbo);
        lowPohCbo.setLayoutData(gd);
        lowPohCbo.setLayoutData(gd);
        lowPohCbo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (lowPohCbo.getSelectionIndex() > highPohCbo
                        .getSelectionIndex()) {
                    MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR
                            | SWT.OK);
                    mb.setText("Error");
                    mb.setMessage("POH low value cannot be greater than POH high value.");
                    mb.open();

                    lowPohCbo.select(lowPohCbo.indexOf(String.valueOf(values
                            .getHiPOHLow())));
                } else {
                    values.setHiPOHLow(Integer.parseInt(lowPohCbo.getText()));
                }
            }
        });

        gd = new GridData(20, SWT.DEFAULT);
        Label filler6 = new Label(hiComp, SWT.NONE);
        filler6.setLayoutData(gd);

        new PohPoshCanvas(hiComp, true);

        gd = new GridData(70, SWT.DEFAULT);
        lowPoshCbo = new Combo(hiComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        fillPohPoshComboBox(lowPoshCbo);

        lowPoshCbo.setLayoutData(gd);
        lowPoshCbo.setLayoutData(gd);
        lowPoshCbo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (lowPoshCbo.getSelectionIndex() > highPoshCbo
                        .getSelectionIndex()) {
                    MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR
                            | SWT.OK);
                    mb.setText("Error");
                    mb.setMessage("POSH low value cannot be greater than POSH high value.");
                    mb.open();

                    lowPoshCbo.select(lowPoshCbo.indexOf(String.valueOf(values
                            .getHiPOSHLow())));
                } else {
                    values.setHiPOSHLow(Integer.parseInt(lowPoshCbo.getText()));
                }
            }
        });

        // Filler label
        new Label(hiComp, SWT.NONE);

        Label highLbl = new Label(hiComp, SWT.CENTER);
        highLbl.setText("High");

        new PohPoshCanvas(hiComp, false);

        gd = new GridData(70, SWT.DEFAULT);
        highPohCbo = new Combo(hiComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        fillPohPoshComboBox(highPohCbo);
        highPohCbo.setLayoutData(gd);
        highPohCbo.setLayoutData(gd);
        highPohCbo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (lowPohCbo.getSelectionIndex() > highPohCbo
                        .getSelectionIndex()) {
                    MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR
                            | SWT.OK);
                    mb.setText("Error");
                    mb.setMessage("POH high value cannot be less than POH low value.");
                    mb.open();

                    highPohCbo.select(highPohCbo.indexOf(String.valueOf(values
                            .getHiPOHHigh())));
                } else {
                    values.setHiPOHHigh(Integer.parseInt(highPohCbo.getText()));
                }
            }
        });

        gd = new GridData(20, SWT.DEFAULT);
        Label filler8 = new Label(hiComp, SWT.NONE);
        filler8.setLayoutData(gd);

        new PohPoshCanvas(hiComp, false);

        gd = new GridData(70, SWT.DEFAULT);
        highPoshCbo = new Combo(hiComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        fillPohPoshComboBox(highPoshCbo);
        highPoshCbo.setLayoutData(gd);
        highPoshCbo.setLayoutData(gd);
        highPoshCbo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (lowPoshCbo.getSelectionIndex() > highPoshCbo
                        .getSelectionIndex()) {
                    MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR
                            | SWT.OK);
                    mb.setText("Error");
                    mb.setMessage("POSH high value cannot be less than POSH low value.");
                    mb.open();

                    highPoshCbo.select(highPoshCbo.indexOf(String
                            .valueOf(values.getHiPOSHHigh())));
                } else {
                    values.setHiPOSHHigh(Integer.parseInt(highPoshCbo.getText()));
                }
            }
        });
    }

    /**
     * Create the TVS controls.
     */
    private void createTvsControls() {
        Composite tvsComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(2, false);
        tvsComp.setLayout(gl);

        GridData gd = new GridData(60, SWT.DEFAULT);
        Label tvsLbl = new Label(tvsComp, SWT.NONE);
        tvsLbl.setFont(labelFont);
        tvsLbl.setForeground(getDisplay().getSystemColor(SWT.COLOR_BLUE));
        tvsLbl.setText("TVS");
        tvsLbl.setLayoutData(gd);

        showElevatedTvsChk = new Button(tvsComp, SWT.CHECK);
        showElevatedTvsChk.setText("Show elevated TVS");
        showElevatedTvsChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                values.setTvsShowElevated(showElevatedTvsChk.getSelection());
            }
        });
    }

    /**
     * Create the DMD, MD, and TVS control.
     */
    private void createDmdMdTvsControls() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite dmdMdTvsComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(2, false);
        dmdMdTvsComp.setLayout(gl);
        dmdMdTvsComp.setLayoutData(gd);

        gd = new GridData();
        gd.horizontalSpan = 2;
        Label dmdMdTvsLbl = new Label(dmdMdTvsComp, SWT.NONE);
        dmdMdTvsLbl.setFont(labelFont);
        dmdMdTvsLbl.setForeground(getDisplay().getSystemColor(SWT.COLOR_BLUE));
        dmdMdTvsLbl.setText("DMD,MD,TVS");
        dmdMdTvsLbl.setLayoutData(gd);

        gd = new GridData(60, SWT.DEFAULT);
        Label filler = new Label(dmdMdTvsComp, SWT.NONE);
        filler.setLayoutData(gd);

        showExtrapolatedChk = new Button(dmdMdTvsComp, SWT.CHECK);
        showExtrapolatedChk.setText("Show extrapolated features");
        showExtrapolatedChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                values.setDmdMdTvsShowExtrapolated(showExtrapolatedChk
                        .getSelection());
            }
        });
    }

    /**
     * Create the DMD controls.
     */
    private void createDmdControls() {
        Composite dmdComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(4, false);
        dmdComp.setLayout(gl);

        GridData gd = new GridData(60, SWT.DEFAULT);
        Label dmdMdTvsLbl = new Label(dmdComp, SWT.NONE);
        dmdMdTvsLbl.setFont(labelFont);
        dmdMdTvsLbl.setForeground(getDisplay().getSystemColor(SWT.COLOR_BLUE));
        dmdMdTvsLbl.setText("DMD");
        dmdMdTvsLbl.setLayoutData(gd);

        Label minFeatureLbl = new Label(dmdComp, SWT.NONE);
        minFeatureLbl.setText("Min feature strength:");

        gd = new GridData(80, SWT.DEFAULT);
        minFeatureScale = new Scale(dmdComp, SWT.HORIZONTAL);
        minFeatureScale.setMinimum(0);
        minFeatureScale.setMaximum(15);
        minFeatureScale.setLayoutData(gd);
        minFeatureScale.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                minFeatureScaleLbl.setText(String.valueOf(minFeatureScale
                        .getSelection()));
            }
        });
        minFeatureScale.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseUp(MouseEvent e) {
                values.setDmdMinFeatureStrength(minFeatureScale.getSelection());
            }
        });

        gd = new GridData(30, SWT.DEFAULT);
        minFeatureScaleLbl = new Label(dmdComp, SWT.NONE);
        minFeatureScaleLbl.setLayoutData(gd);

        // Filler label
        new Label(dmdComp, SWT.NONE);

        gd = new GridData();
        gd.horizontalSpan = 3;
        overlapMesosChk = new Button(dmdComp, SWT.CHECK);
        overlapMesosChk.setText("Show overlapping Mesos");
        overlapMesosChk.setLayoutData(gd);
        overlapMesosChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                values.setDmdShowOverlapping(overlapMesosChk.getSelection());
            }
        });

        // Filler label
        new Label(dmdComp, SWT.NONE);

        Label typeTrackLbl = new Label(dmdComp, SWT.NONE);
        typeTrackLbl.setText("Type of track to show:");

        gd = new GridData();
        gd.horizontalSpan = 2;
        dmdTrackToShowCbo = new Combo(dmdComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        for (RadarDisplayManager.TrackTypes type : RadarDisplayManager.TrackTypes
                .values()) {
            dmdTrackToShowCbo.add(type.toString());
        }
        dmdTrackToShowCbo.setLayoutData(gd);
        dmdTrackToShowCbo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                values.setDmdTrackType(RadarDisplayManager.TrackTypes
                        .fromString(dmdTrackToShowCbo.getText()));
            }
        });
    }

    /**
     * Create the MBA controls.
     */
    private void createMbaControls() {
        Composite mbaComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(2, false);
        mbaComp.setLayout(gl);

        GridData gd = new GridData(60, SWT.DEFAULT);
        Label mbaLbl = new Label(mbaComp, SWT.NONE);
        mbaLbl.setFont(labelFont);
        mbaLbl.setForeground(getDisplay().getSystemColor(SWT.COLOR_BLUE));
        mbaLbl.setText("MBA");
        mbaLbl.setLayoutData(gd);

        showMbaWindShear = new Button(mbaComp, SWT.CHECK);
        showMbaWindShear.setText("Show Wind Shear");
        showMbaWindShear.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                values.setMbaShowWindShear(showMbaWindShear.getSelection());
            }
        });

    }

    /**
     * Create the SRM radio button controls.
     */
    private void createSrmControls() {
        Composite srmComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(2, false);
        srmComp.setLayout(gl);

        // Filler label
        new Label(srmComp, SWT.NONE);

        stormMotionRdo = new Button(srmComp, SWT.RADIO);
        stormMotionRdo.setText("Storm Motion from WarnGen Track");
        stormMotionRdo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                values.setSrmSource(RadarSRMResource.SRMSource.WARNGEN);
                enableCustomStormControls(false);
            }
        });

        GridData gd = new GridData(60, SWT.DEFAULT);
        Label srmLbl = new Label(srmComp, SWT.NONE);
        srmLbl.setFont(labelFont);
        srmLbl.setForeground(getDisplay().getSystemColor(SWT.COLOR_BLUE));
        srmLbl.setText("SRM");
        srmLbl.setLayoutData(gd);

        averageStormRdo = new Button(srmComp, SWT.RADIO);
        averageStormRdo.setText("Average Storm Motion from STI");
        averageStormRdo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                values.setSrmSource(RadarSRMResource.SRMSource.STI);
                enableCustomStormControls(false);
            }
        });

        // Filler label
        new Label(srmComp, SWT.NONE);

        customStormRdo = new Button(srmComp, SWT.RADIO);
        customStormRdo.setText("Custom Storm Motion");
        customStormRdo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                values.setSrmSource(RadarSRMResource.SRMSource.CUSTOM);
                enableCustomStormControls(true);
            }
        });
    }

    /**
     * Create the Custom Storm Motion group & controls.
     */
    private void createCustomStormMotionGroup() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        customStormGroup = new Group(shell, SWT.NONE);
        GridLayout gl = new GridLayout(3, false);
        customStormGroup.setLayout(gl);
        customStormGroup.setLayoutData(gd);
        customStormGroup.setText("Custom Storm Motion");

        int scaleWidth = 250;

        // ---------------------------------------
        // Direction controls
        // ---------------------------------------
        dirLbl = new Label(customStormGroup, SWT.NONE);
        dirLbl.setText("Dir:");

        gd = new GridData(scaleWidth, SWT.DEFAULT);
        dirScale = new Scale(customStormGroup, SWT.HORIZONTAL);
        dirScale.setMinimum(0);
        dirScale.setMaximum(359);
        dirScale.setIncrement(5);
        dirScale.setLayoutData(gd);
        dirScale.addMouseListener(new MouseAdapter() {

            @Override
            public void mouseUp(MouseEvent e) {
                values.setSrmDir(dirScale.getSelection());
                dirSpnr.setSelection(dirScale.getSelection());
            }

        });

        gd = new GridData(30, SWT.DEFAULT);
        dirSpnr = new Spinner(customStormGroup, SWT.BORDER);
        dirSpnr.setLayoutData(gd);
        dirSpnr.setMinimum(dirScale.getMinimum());
        dirSpnr.setMaximum(dirScale.getMaximum());
        dirSpnr.addMouseListener(new MouseAdapter() {

            @Override
            public void mouseUp(MouseEvent e) {
                values.setSrmDir(dirSpnr.getSelection());
                dirScale.setSelection(dirSpnr.getSelection());
            }

        });

        // Filler label
        new Label(customStormGroup, SWT.NONE);

        gd = new GridData(scaleWidth, SWT.DEFAULT);
        Composite dirComp = new Composite(customStormGroup, SWT.NONE);
        gl = new GridLayout(3, false);
        dirComp.setLayout(gl);
        dirComp.setLayoutData(gd);

        gd = new GridData(SWT.LEFT, SWT.DEFAULT, true, false);
        Label dirMinLbl = new Label(dirComp, SWT.NONE);
        dirMinLbl.setText(String.valueOf(dirScale.getMinimum()));
        dirMinLbl.setLayoutData(gd);

        gd = new GridData(SWT.RIGHT, SWT.DEFAULT, false, false);
        Label dirMaxLbl = new Label(dirComp, SWT.NONE);
        dirMaxLbl.setText(String.valueOf(dirScale.getMaximum()));
        dirMaxLbl.setLayoutData(gd);

        // Filler label
        new Label(customStormGroup, SWT.NONE);

        // ---------------------------------------
        // Speed controls
        // ---------------------------------------
        speedLbl = new Label(customStormGroup, SWT.NONE);
        speedLbl.setText("Spd:");

        gd = new GridData(scaleWidth, SWT.DEFAULT);
        speedScale = new Scale(customStormGroup, SWT.HORIZONTAL);
        speedScale.setMinimum(0);
        speedScale.setMaximum(99);
        speedScale.setIncrement(5);
        speedScale.setLayoutData(gd);
        speedScale.addMouseListener(new MouseAdapter() {

            @Override
            public void mouseUp(MouseEvent e) {
                values.setSrmSpeed(speedScale.getSelection());
                speedSpnr.setSelection(speedScale.getSelection());
            }

        });

        gd = new GridData(30, SWT.DEFAULT);
        speedSpnr = new Spinner(customStormGroup, SWT.BORDER);
        speedSpnr.setLayoutData(gd);
        speedSpnr.setMinimum(speedScale.getMinimum());
        speedSpnr.setMaximum(speedScale.getMaximum());
        speedSpnr.addMouseListener(new MouseAdapter() {

            @Override
            public void mouseUp(MouseEvent e) {
                values.setSrmSpeed(speedSpnr.getSelection());
                speedScale.setSelection(speedSpnr.getSelection());
            }

        });

        // Filler label
        new Label(customStormGroup, SWT.NONE);

        gd = new GridData(scaleWidth, SWT.DEFAULT);
        Composite spdComp = new Composite(customStormGroup, SWT.NONE);
        gl = new GridLayout(3, false);
        spdComp.setLayout(gl);
        spdComp.setLayoutData(gd);

        gd = new GridData(SWT.LEFT, SWT.DEFAULT, true, false);
        Label spdMinLbl = new Label(spdComp, SWT.NONE);
        spdMinLbl.setText(String.valueOf(speedScale.getMinimum()));
        spdMinLbl.setLayoutData(gd);

        gd = new GridData(SWT.RIGHT, SWT.DEFAULT, false, false);
        Label spdMaxLbl = new Label(spdComp, SWT.NONE);
        spdMaxLbl.setText(String.valueOf(speedScale.getMaximum()));
        spdMaxLbl.setLayoutData(gd);
    }

    private void createSailsControls() {
        Composite sailsComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(2, false);
        sailsComp.setLayout(gl);

        GridData gd = new GridData(60, SWT.DEFAULT);
        Label mbaLbl = new Label(sailsComp, SWT.NONE);
        mbaLbl.setFont(labelFont);
        mbaLbl.setForeground(getDisplay().getSystemColor(SWT.COLOR_BLUE));
        mbaLbl.setText("SAILS");
        mbaLbl.setLayoutData(gd);

        enableSails = new Button(sailsComp, SWT.CHECK);
        enableSails.setText("Enable SAILS Frame Coordinator");
        enableSails
                .setToolTipText("The SAILS frame coordinator enables custom actions for the up/down arrows and the last frame button that");
        enableSails.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                values.setSailsFrameCoordinator(enableSails.getSelection());
            }
        });

    }

    /**
     * Create the close button.
     */
    private void createCloseButton() {
        Composite centeredComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(1, false);
        centeredComp.setLayout(gl);
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.horizontalSpan = 2;
        centeredComp.setLayoutData(gd);

        gd = new GridData(90, SWT.DEFAULT);
        Button closeBtn = new Button(centeredComp, SWT.NONE);
        closeBtn.setText("Close");
        closeBtn.setLayoutData(gd);
        closeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                shell.dispose();
            }
        });
    }

    /**
     * Add a horizontal separator to the display.
     */
    private void addSeparator() {
        GridData gd = new GridData(GridData.FILL_HORIZONTAL);
        Label sepLbl = new Label(shell, SWT.SEPARATOR | SWT.HORIZONTAL);
        sepLbl.setLayoutData(gd);
    }

    /**
     * Enables the Custom Storm Motion controls.
     * 
     * @param flag
     *            If true the controls are enabled, disable if false.
     */
    private void enableCustomStormControls(boolean flag) {
        customStormGroup.setEnabled(flag);

        dirLbl.setEnabled(flag);
        dirScale.setEnabled(flag);
        dirSpnr.setEnabled(flag);

        speedLbl.setEnabled(flag);
        speedScale.setEnabled(flag);
        speedSpnr.setEnabled(flag);
    }

    /**
     * Fill a combo box with value from 0 to 100 with an increment of 10.
     * 
     * @param comboBox
     *            Combo box to fill with values.
     */
    private void fillPohPoshComboBox(Combo comboBox) {
        for (int x = 0; x <= 100; x += 10) {
            comboBox.add(String.valueOf(x));
        }
    }
}
