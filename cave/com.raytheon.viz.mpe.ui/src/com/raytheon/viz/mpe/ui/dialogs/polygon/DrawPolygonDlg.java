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
package com.raytheon.viz.mpe.ui.dialogs.polygon;

import java.awt.Point;
import java.util.Date;
import java.util.List;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Cursor;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Scale;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Spinner;

import com.raytheon.viz.mpe.ui.DisplayFieldData;
import com.raytheon.viz.mpe.ui.MPEDisplayManager;
import com.raytheon.viz.mpe.ui.dialogs.polygon.RubberPolyData.PolygonEditAction;
import com.raytheon.viz.mpe.ui.rsc.MPEPolygonResource;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * Draw Polygon Dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 30, 2009 2685       mpduff      Initial creation
 * Jan 26, 2011 7761       bkowal      The value associated with polygons
 *                                     with the "scale" action will no
 *                                     longer be divided by 100.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class DrawPolygonDlg extends CaveSWTDialog {
    private static final String ADJUST_PRECIP_TEXT = "Adjust Precipitation Value";

    private static final String SUBSTITUTE_VALUE_TEXT = "Select Field To Substitute";

    private static final String MAKE_PERSISTENT = "Make Persistent";

    /**
     * Bold Font.
     */
    private Font boldFont = null;

    /**
     * Normal font.
     */
    private Font font = null;

    /**
     * The precip value spinner control.
     */
    private Spinner precipSpinner = null;

    /**
     * The precip slider control.
     */
    private Scale precipSlider = null;

    /**
     * The points to perform operations on
     */
    private List<Point> points;

    /**
     * The wait mouse pointer.
     */
    private Cursor waitCursor = null;

    /** The substitute type */
    private DisplayFieldData subType = null;

    /** Checkbox for persistent. */
    private Button persistentChk = null;

    /** The polygon resource */
    private final MPEPolygonResource resource;

    /**
     * Constructor.
     * 
     * @param parentShell
     *            The parent shell for this dialog.
     */
    public DrawPolygonDlg(Shell parentShell, MPEPolygonResource resource) {
        super(parentShell, SWT.DIALOG_TRIM, CAVE.DO_NOT_BLOCK);
        setText("Edit Precipitation");
        this.resource = resource;
        waitCursor = new Cursor(parentShell.getDisplay(), SWT.CURSOR_WAIT);
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
        boldFont.dispose();
        resource.clearPolygons();
    }

    @Override
    protected void initializeComponents(Shell shell) {
        setReturnValue(false);

        boldFont = new Font(shell.getDisplay(), "Monospace", 10, SWT.BOLD);
        font = new Font(shell.getDisplay(), "Monospace", 10, SWT.NORMAL);
        // Initialize all of the controls and layoutsendCal
        initializeComponents();
    }

    /**
     * Initialize the dialog components.
     */
    private void initializeComponents() {
        createPersistentGroup();
        createSubGroup();
        createCloseBtn();
    }

    /**
     * Create the persistent group.
     */
    private void createPersistentGroup() {
        // Create adjust group
        Group persistentGroupComp = new Group(shell, SWT.NONE);
        persistentGroupComp.setFont(boldFont);
        persistentGroupComp.setText(ADJUST_PRECIP_TEXT);
        persistentGroupComp.setLayout(new GridLayout(1, true));
        GridData gd = new GridData(345, SWT.DEFAULT);
        persistentGroupComp.setLayoutData(gd);

        getPersistentChk(persistentGroupComp);
        getSliderComp(persistentGroupComp);
        getButtonComp(persistentGroupComp);
    }

    /**
     * Create the substitute group.
     */
    private void createSubGroup() {
        // Create substitute group
        Group subGroup = new Group(shell, SWT.NONE);
        subGroup.setFont(boldFont);
        subGroup.setText(SUBSTITUTE_VALUE_TEXT);
        subGroup.setLayout(new GridLayout(2, false));
        GridData gd = new GridData(345, SWT.DEFAULT);
        subGroup.setLayoutData(gd);

        getSubChecks(subGroup);

        // Create Substitute button
        final Button subBtn = new Button(subGroup, SWT.PUSH);
        subBtn.setData(PolygonEditAction.SUB);
        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT, false, false, 2, 1);
        subBtn.setText("Substitute");
        subBtn.setLayoutData(gd);
        subBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                processDrawPrecipValue(subBtn);
            }
        });
    }

    /**
     * Create the close button.
     */
    private void createCloseBtn() {
        Button closeBtn = new Button(shell, SWT.PUSH);
        closeBtn.setText("Close");
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, false, false, 1, 1);
        closeBtn.setAlignment(SWT.CENTER);
        closeBtn.setLayoutData(gd);
        closeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                shell.close();
            }
        });
    }

    /**
     * Build the persistent check box.
     * 
     * @param groupComp
     *            the parent composite
     */
    private void getPersistentChk(Group groupComp) {
        persistentChk = new Button(groupComp, SWT.CHECK);
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, false, false);
        persistentChk.setLayoutData(gd);
        persistentChk.setText(MAKE_PERSISTENT);
        persistentChk.setFont(font);
    }

    /**
     * Build the slider/spinner composite.
     * 
     * @param groupComp
     *            the parent composite
     */
    private void getSliderComp(Group groupComp) {
        Composite comp = new Composite(groupComp, SWT.NONE);
        comp.setLayout(new GridLayout(2, false));

        GridData gd = new GridData(250, 30);
        precipSlider = new Scale(comp, SWT.HORIZONTAL);
        precipSlider.setMinimum(0);
        precipSlider.setMaximum(500);
        precipSlider.setIncrement(1);
        precipSlider.setLayoutData(gd);
        precipSlider.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                precipSpinner.setSelection(precipSlider.getSelection());
            }
        });

        // Create the Red color spinner.
        precipSpinner = new Spinner(comp, SWT.BORDER);
        gd = new GridData(30, SWT.DEFAULT);
        precipSpinner.setLayoutData(gd);
        precipSpinner.setMinimum(0);
        precipSpinner.setMaximum(500);
        precipSpinner.setSelection(precipSlider.getSelection());
        precipSpinner.setDigits(2);

        precipSpinner.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                precipSlider.setSelection(precipSpinner.getSelection());
            }
        });
    }

    /**
     * Build the 5 button composite.
     * 
     * @param groupComp
     *            the parent composite
     */
    private void getButtonComp(Group groupComp) {
        Composite comp = new Composite(groupComp, SWT.NONE);
        comp.setLayout(new GridLayout(5, false));

        PolygonEditAction[] editBtns = new PolygonEditAction[] {
                PolygonEditAction.SET, PolygonEditAction.RAISE,
                PolygonEditAction.LOWER, PolygonEditAction.SCALE,
                PolygonEditAction.SNOW };

        for (PolygonEditAction action : editBtns) {
            Button editBtn = new Button(comp, SWT.PUSH);
            editBtn.setText(action.toPrettyName());
            editBtn.setData(action);
            editBtn.setLayoutData(new GridData(60, SWT.DEFAULT));
            editBtn.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent event) {
                    processDrawPrecipValue((Button) event.getSource());
                }
            });
        }
    }

    /**
     * Create the substitute check box widgets
     * 
     * @param groupComp
     *            The group composite
     */
    private void getSubChecks(Group groupComp) {
        // Spacer
        Label spaceLabel = new Label(groupComp, SWT.NONE);
        spaceLabel.setText("    ");

        Button radarMosaicChk = new Button(groupComp, SWT.RADIO);
        GridData gd = new GridData(SWT.LEFT, SWT.DEFAULT, true, false);
        radarMosaicChk.setLayoutData(gd);
        radarMosaicChk.setText("Radar Mosaic");
        radarMosaicChk.setFont(font);
        radarMosaicChk.setLayoutData(gd);
        // Default to radar mosaic on dialog creation
        radarMosaicChk.setSelection(true);
        subType = DisplayFieldData.rMosaic;
        radarMosaicChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                subType = DisplayFieldData.rMosaic;
            }
        });

        spaceLabel = new Label(groupComp, SWT.NONE);
        spaceLabel.setText("    ");

        Button avgRadarMosaicChk = new Button(groupComp, SWT.RADIO);
        gd = new GridData(SWT.LEFT, SWT.DEFAULT, true, false);
        avgRadarMosaicChk.setLayoutData(gd);
        avgRadarMosaicChk.setText("Average Radar Mosaic");
        avgRadarMosaicChk.setFont(font);
        avgRadarMosaicChk.setLayoutData(gd);
        avgRadarMosaicChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                subType = DisplayFieldData.avgrMosaic;
            }
        });

        spaceLabel = new Label(groupComp, SWT.NONE);
        spaceLabel.setText("    ");

        Button maxRadarMosaicChk = new Button(groupComp, SWT.RADIO);
        gd = new GridData(SWT.LEFT, SWT.DEFAULT, true, false);
        maxRadarMosaicChk.setLayoutData(gd);
        maxRadarMosaicChk.setText("Max Radar Mosaic");
        maxRadarMosaicChk.setFont(font);
        maxRadarMosaicChk.setLayoutData(gd);
        maxRadarMosaicChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                subType = DisplayFieldData.maxrMosaic;
            }
        });

        spaceLabel = new Label(groupComp, SWT.NONE);
        spaceLabel.setText("    ");

        Button fieldBiasRadarMosaicChk = new Button(groupComp, SWT.RADIO);
        gd = new GridData(SWT.LEFT, SWT.DEFAULT, true, false);
        fieldBiasRadarMosaicChk.setLayoutData(gd);
        fieldBiasRadarMosaicChk.setText("Field Bias Radar Mosaic");
        fieldBiasRadarMosaicChk.setFont(font);
        fieldBiasRadarMosaicChk.setLayoutData(gd);
        fieldBiasRadarMosaicChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                subType = DisplayFieldData.bMosaic;
            }
        });

        spaceLabel = new Label(groupComp, SWT.NONE);
        spaceLabel.setText("    ");

        Button localBiasRadarMosaicChk = new Button(groupComp, SWT.RADIO);
        gd = new GridData(SWT.LEFT, SWT.DEFAULT, true, false);
        localBiasRadarMosaicChk.setLayoutData(gd);
        localBiasRadarMosaicChk.setText("Local Bias Radar Mosaic");
        localBiasRadarMosaicChk.setFont(font);
        localBiasRadarMosaicChk.setLayoutData(gd);
        localBiasRadarMosaicChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                subType = DisplayFieldData.lMosaic;
            }
        });

        spaceLabel = new Label(groupComp, SWT.NONE);
        spaceLabel.setText("    ");

        Button gageOnlyChk = new Button(groupComp, SWT.RADIO);
        gd = new GridData(SWT.LEFT, SWT.DEFAULT, true, false);
        gageOnlyChk.setLayoutData(gd);
        gageOnlyChk.setText("Gage Only Analysis");
        gageOnlyChk.setFont(font);
        gageOnlyChk.setLayoutData(gd);
        gageOnlyChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                subType = DisplayFieldData.gageOnly;
            }
        });

        spaceLabel = new Label(groupComp, SWT.NONE);
        spaceLabel.setText("    ");

        Button multiSensorMosaicChk = new Button(groupComp, SWT.RADIO);
        gd = new GridData(SWT.LEFT, SWT.DEFAULT, true, false);
        multiSensorMosaicChk.setLayoutData(gd);
        multiSensorMosaicChk.setText("Multisensor Mosaic");
        multiSensorMosaicChk.setFont(font);
        multiSensorMosaicChk.setLayoutData(gd);
        multiSensorMosaicChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                subType = DisplayFieldData.mMosaic;
            }
        });

        spaceLabel = new Label(groupComp, SWT.NONE);
        spaceLabel.setText("    ");

        Button localBiasMultiSensorMosaicChk = new Button(groupComp, SWT.RADIO);
        gd = new GridData(SWT.LEFT, SWT.DEFAULT, true, false);
        localBiasMultiSensorMosaicChk.setLayoutData(gd);
        localBiasMultiSensorMosaicChk.setText("Local Bias Multisensor Mosaic");
        localBiasMultiSensorMosaicChk.setFont(font);
        localBiasMultiSensorMosaicChk.setLayoutData(gd);
        localBiasMultiSensorMosaicChk
                .addSelectionListener(new SelectionAdapter() {
                    @Override
                    public void widgetSelected(SelectionEvent event) {
                        subType = DisplayFieldData.mlMosaic;
                    }
                });

        spaceLabel = new Label(groupComp, SWT.NONE);
        spaceLabel.setText("    ");

        Button satPrecipChk = new Button(groupComp, SWT.RADIO);
        gd = new GridData(SWT.LEFT, SWT.DEFAULT, true, false);
        satPrecipChk.setLayoutData(gd);
        satPrecipChk.setText("Satellite Precip");
        satPrecipChk.setFont(font);
        satPrecipChk.setLayoutData(gd);
        satPrecipChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                subType = DisplayFieldData.satPre;
            }
        });

        spaceLabel = new Label(groupComp, SWT.NONE);
        spaceLabel.setText("    ");

        Button localBiasSatPrecipChk = new Button(groupComp, SWT.RADIO);
        gd = new GridData(SWT.LEFT, SWT.DEFAULT, true, false);
        localBiasSatPrecipChk.setLayoutData(gd);
        localBiasSatPrecipChk.setText("Local Bias Satellite Precip");
        localBiasSatPrecipChk.setFont(font);
        localBiasSatPrecipChk.setLayoutData(gd);
        localBiasSatPrecipChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                subType = DisplayFieldData.lsatPre;
            }
        });

        spaceLabel = new Label(groupComp, SWT.NONE);
        spaceLabel.setText("    ");

        Button satRadarMosaicChk = new Button(groupComp, SWT.RADIO);
        gd = new GridData(SWT.LEFT, SWT.DEFAULT, true, false);
        satRadarMosaicChk.setLayoutData(gd);
        satRadarMosaicChk.setText("Satellite Radar Mosaic");
        satRadarMosaicChk.setFont(font);
        satRadarMosaicChk.setLayoutData(gd);
        satRadarMosaicChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                subType = DisplayFieldData.srMosaic;
            }
        });

        spaceLabel = new Label(groupComp, SWT.NONE);
        spaceLabel.setText("    ");

        Button satGageMosaicChk = new Button(groupComp, SWT.RADIO);
        gd = new GridData(SWT.LEFT, SWT.DEFAULT, true, false);
        satGageMosaicChk.setLayoutData(gd);
        satGageMosaicChk.setText("Satellite Gage Mosaic");
        satGageMosaicChk.setFont(font);
        satGageMosaicChk.setLayoutData(gd);
        satGageMosaicChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                subType = DisplayFieldData.sgMosaic;
            }
        });

        spaceLabel = new Label(groupComp, SWT.NONE);
        spaceLabel.setText("    ");

        Button satRadarGageMosaicChk = new Button(groupComp, SWT.RADIO);
        gd = new GridData(SWT.LEFT, SWT.DEFAULT, true, false);
        satRadarGageMosaicChk.setLayoutData(gd);
        satRadarGageMosaicChk.setText("Satellite Radar Gage Mosaic");
        satRadarGageMosaicChk.setFont(font);
        satRadarGageMosaicChk.setLayoutData(gd);
        satRadarGageMosaicChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                subType = DisplayFieldData.srgMosaic;
            }
        });

        spaceLabel = new Label(groupComp, SWT.NONE);
        spaceLabel.setText("    ");

        Button triangulatedLocalBiasMosaicChk = new Button(groupComp, SWT.RADIO);
        gd = new GridData(SWT.LEFT, SWT.DEFAULT, true, false);
        triangulatedLocalBiasMosaicChk.setLayoutData(gd);
        triangulatedLocalBiasMosaicChk
                .setText("Triangulated Local Bias Mosaic");
        triangulatedLocalBiasMosaicChk.setFont(font);
        triangulatedLocalBiasMosaicChk.setLayoutData(gd);
        triangulatedLocalBiasMosaicChk
                .addSelectionListener(new SelectionAdapter() {
                    @Override
                    public void widgetSelected(SelectionEvent event) {
                        subType = DisplayFieldData.p3lMosaic;
                    }
                });

        spaceLabel = new Label(groupComp, SWT.NONE);
        spaceLabel.setText("    ");

        Button bestEstQPEChk = new Button(groupComp, SWT.RADIO);
        gd = new GridData(SWT.LEFT, SWT.DEFAULT, true, false);
        bestEstQPEChk.setLayoutData(gd);
        bestEstQPEChk.setText("Best Estimate QPE");
        bestEstQPEChk.setFont(font);
        bestEstQPEChk.setLayoutData(gd);
        bestEstQPEChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                subType = DisplayFieldData.Xmrg;
            }
        });

        spaceLabel = new Label(groupComp, SWT.NONE);
        spaceLabel.setText("    ");

        Button rfcFieldBiasMosaic = new Button(groupComp, SWT.RADIO);
        gd = new GridData(SWT.LEFT, SWT.DEFAULT, true, false);
        rfcFieldBiasMosaic.setLayoutData(gd);
        rfcFieldBiasMosaic.setText("RFC Field Bias Mosaic");
        rfcFieldBiasMosaic.setFont(font);
        rfcFieldBiasMosaic.setLayoutData(gd);
        rfcFieldBiasMosaic.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                subType = DisplayFieldData.rfcbMosaic;
            }
        });

        spaceLabel = new Label(groupComp, SWT.NONE);
        spaceLabel.setText("    ");

        Button rfcMultiSensorMosaic = new Button(groupComp, SWT.RADIO);
        gd = new GridData(SWT.LEFT, SWT.DEFAULT, true, false);
        rfcMultiSensorMosaic.setLayoutData(gd);
        rfcMultiSensorMosaic.setText("RFC Multisensor Mosaic");
        rfcMultiSensorMosaic.setFont(font);
        rfcMultiSensorMosaic.setLayoutData(gd);
        rfcMultiSensorMosaic.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                subType = DisplayFieldData.rfcmMosaic;
            }
        });

        spaceLabel = new Label(groupComp, SWT.NONE);
        spaceLabel.setText("    ");

        Button rawQ2Mosaic = new Button(groupComp, SWT.RADIO);
        gd = new GridData(SWT.LEFT, SWT.DEFAULT, true, false);
        rawQ2Mosaic.setLayoutData(gd);
        rawQ2Mosaic.setText("Raw Q2 Mosaic");
        rawQ2Mosaic.setFont(font);
        rawQ2Mosaic.setLayoutData(gd);
        rawQ2Mosaic.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                subType = DisplayFieldData.qmosaic;
            }
        });

        spaceLabel = new Label(groupComp, SWT.NONE);
        spaceLabel.setText("    ");

        Button localBQ2Mosaic = new Button(groupComp, SWT.RADIO);
        gd = new GridData(SWT.LEFT, SWT.DEFAULT, true, false);
        localBQ2Mosaic.setLayoutData(gd);
        localBQ2Mosaic.setText("Local Bias Q2 Mosaic");
        localBQ2Mosaic.setFont(font);
        localBQ2Mosaic.setLayoutData(gd);
        localBQ2Mosaic.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                subType = DisplayFieldData.lqmosaic;
            }
        });

        spaceLabel = new Label(groupComp, SWT.NONE);
        spaceLabel.setText("    ");

        Button mQ2Mosaic = new Button(groupComp, SWT.RADIO);
        gd = new GridData(SWT.LEFT, SWT.DEFAULT, true, false);
        mQ2Mosaic.setLayoutData(gd);
        mQ2Mosaic.setText("Multisensor Q2 Mosaic");
        mQ2Mosaic.setFont(font);
        mQ2Mosaic.setLayoutData(gd);
        mQ2Mosaic.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                subType = DisplayFieldData.mlqmosaic;
            }
        });

        spaceLabel = new Label(groupComp, SWT.NONE);
        spaceLabel.setText("    ");

        Button local1Mosaic = new Button(groupComp, SWT.RADIO);
        gd = new GridData(SWT.LEFT, SWT.DEFAULT, true, false);
        local1Mosaic.setLayoutData(gd);
        local1Mosaic.setText("Local Field #1");
        local1Mosaic.setFont(font);
        local1Mosaic.setLayoutData(gd);
        local1Mosaic.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                subType = DisplayFieldData.localField1;
            }
        });

        spaceLabel = new Label(groupComp, SWT.NONE);
        spaceLabel.setText("    ");

        Button local2Mosaic = new Button(groupComp, SWT.RADIO);
        gd = new GridData(SWT.LEFT, SWT.DEFAULT, true, false);
        local2Mosaic.setLayoutData(gd);
        local2Mosaic.setText("Local Field #2");
        local2Mosaic.setFont(font);
        local2Mosaic.setLayoutData(gd);
        local2Mosaic.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                subType = DisplayFieldData.localField2;
            }
        });

        spaceLabel = new Label(groupComp, SWT.NONE);
        spaceLabel.setText("    ");

        Button local3Mosaic = new Button(groupComp, SWT.RADIO);
        gd = new GridData(SWT.LEFT, SWT.DEFAULT, true, false);
        local3Mosaic.setLayoutData(gd);
        local3Mosaic.setText("Local Field #3");
        local3Mosaic.setFont(font);
        local3Mosaic.setLayoutData(gd);
        local3Mosaic.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                subType = DisplayFieldData.localField3;
            }
        });

        spaceLabel = new Label(groupComp, SWT.NONE);
        spaceLabel.setText("    ");

        Button rfcQpeMosaic = new Button(groupComp, SWT.RADIO);
        gd = new GridData(SWT.LEFT, SWT.DEFAULT, true, false);
        rfcQpeMosaic.setLayoutData(gd);
        rfcQpeMosaic.setText("RFC QPE Mosaic");
        rfcQpeMosaic.setFont(font);
        rfcQpeMosaic.setLayoutData(gd);
        rfcQpeMosaic.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                subType = DisplayFieldData.rfcMosaic;
            }
        });
    }

    /**
     * Process the selection.
     * 
     * @param btnSource
     *            Source object
     */
    private void processDrawPrecipValue(Button editBtn) {
        MPEDisplayManager dispMgr = MPEDisplayManager.getInstance(resource
                .getDescriptor().getRenderableDisplay());
        Cursor prevCursor = shell.getCursor();
        try {
            /* Apply the polygon and save it to the Polygon file. */
            shell.setCursor(waitCursor);

            // Divide precipSpinner selection by 100 since we have 2 decimal
            // digits when created. This give actual precip value
            double precipValue = precipSpinner.getSelection() / 100.0;
            Point[] editPoints = points.toArray(new Point[0]);
            PolygonEditAction action = (PolygonEditAction) editBtn.getData();
            boolean persistent = persistentChk.getSelection();
            DisplayFieldData subType = null;
            if (action == PolygonEditAction.SUB) {
                subType = this.subType;
            }

            DisplayFieldData displayedField = dispMgr.getDisplayFieldType();
            Date editDate = dispMgr.getCurrentEditDate();

            RubberPolyData newEdit = new RubberPolyData(action, subType,
                    precipValue, editPoints, true, persistent);

            List<RubberPolyData> polygonEdits = PolygonEditManager
                    .getPolygonEdits(displayedField, editDate);
            polygonEdits.add(newEdit);
            PolygonEditManager.writePolygonEdits(displayedField, editDate,
                    polygonEdits);
        } finally {
            shell.setCursor(prevCursor);
        }
    }

    /**
     * Set the rubber banded polygon data
     * 
     * @param polyData
     *            The RubberPolyData object to set
     */
    public void setPolygonPoints(List<Point> points) {
        this.points = points;
    }

}
