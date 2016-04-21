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

package com.raytheon.viz.hydro.bestestimateqpe;

import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.TimeZone;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.layout.RowData;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Scale;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IFrameCoordinator.FrameChangeMode;
import com.raytheon.uf.viz.core.drawables.IFrameCoordinator.FrameChangeOperation;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydro.Activator;
import com.raytheon.viz.hydrocommon.HydroDisplayManager;
import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * This class displays the Best Estimate QPE dialog for Hydroview.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 29 NOV 2007  373        lvenable    Initial creation.
 * 21 May 2009             mpduff      Fixed typo in window title.
 * 24 Aug 2009  2258       mpduff      Implemented dialog functionality.
 * 07 Dec 2012  1353       rferrel     Make dialog non-blocking.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class BestEstimateQpeDlg extends CaveSWTDialog {
    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(BestEstimateQpeDlg.class);

    /**
     * QPE source Local radio button.
     */
    private Button localRdo;

    /**
     * QPE source RFC radio button.
     */
    private Button rfcRdo;

    /**
     * Font used for text controls.
     */
    private Font font;

    /**
     * Arrow button to increase the day.
     */
    private Button upDayBtn;

    /**
     * Arrow button to decrease the day.
     */
    private Button dnDayBtn;

    /**
     * Date & time text control.
     */
    private Text dateTimeTF;

    /**
     * Arrow button to increase the hours.
     */
    private Button upHoursBtn;

    /**
     * Arrow button to decrease the hours.
     */
    private Button dnHoursBtn;

    /**
     * Accumulate radio button.
     */
    private Button accumRdo;

    /**
     * Time lapse radio button.
     */
    private Button timeLapseRdo;

    /**
     * First button.
     */
    private Button firstBtn;

    /**
     * Previous button.
     */
    private Button previousBtn;

    /**
     * Next button.
     */
    private Button nextBtn;

    /**
     * Last button.
     */
    private Button lastBtn;

    /**
     * Looping button.
     */
    private Button loopBtn;

    /**
     * Image for the first button.
     */
    private Image firstImg;

    /**
     * Image for the previous button.
     */
    private Image previousImg;

    /**
     * Image for the next button.
     */
    private Image nextImg;

    /**
     * Image for the last button.
     */
    private Image lastImg;

    /**
     * Image for the looping button.
     */
    private Image loopImg;

    /**
     * Scale for selecting a duration.
     */
    private Scale durationScale;

    /**
     * Scale label for indicating the selected a duration.
     */
    private Label durationNumLbl;

    /**
     * Display options combo box.
     */
    private Combo displayAsCbo;

    /**
     * Annotation ID check box.
     */
    private Button idChk;

    /**
     * Annotation labels check box.
     */
    private Button labelsChk;

    /**
     * Show data button.
     */
    private Button showDataBtn;

    /**
     * End Lapse button.
     */
    private Button endLapseBtn;

    /**
     * Clear Data button.
     */
    private Button clearDataBtn;

    /**
     * Close button.
     */
    private Button closeBtn;

    /**
     * Format for the date/time control.
     */
    private SimpleDateFormat dateTimeFmt = new SimpleDateFormat("yyyy-MM-dd HH");

    /**
     * Data Date.
     */
    private Calendar cal = new GregorianCalendar();

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     */
    public BestEstimateQpeDlg(Shell parent) {
        super(parent, SWT.DIALOG_TRIM, CAVE.DO_NOT_BLOCK);
        setText("Display Best Estimate QPE");
        cal.setTimeZone(TimeZone.getTimeZone("GMT"));
        dateTimeFmt.setTimeZone(TimeZone.getTimeZone("GMT"));
    }

    @Override
    protected Layout constructShellLayout() {
        GridLayout mainLayout = new GridLayout(1, true);
        mainLayout.marginHeight = 1;
        mainLayout.marginWidth = 1;
        return mainLayout;
    }

    @Override
    protected void disposed() {
        font.dispose();
        firstImg.dispose();
        previousImg.dispose();
        nextImg.dispose();
        lastImg.dispose();
        loopImg.dispose();
    }

    @Override
    protected void initializeComponents(Shell shell) {
        setReturnValue(false);

        font = new Font(shell.getDisplay(), "Courier", 10, SWT.NORMAL);

        // Initialize all of the controls and layouts
        getImages();

        createQpeSource();
        createDateTimeAdjust();
        createAccumTimeLapse();
        createDurationControl();
        createDisplayAsControl();
        createAnnotateControls();
        createBottomButtons();

        Date dataDate = SimulatedTime.getSystemTime().getTime();
        cal.setTime(dataDate);
        dateTimeTF.setText(dateTimeFmt.format(dataDate));
    }

    /**
     * Create the QPE Source radio buttons.
     */
    private void createQpeSource() {
        Group qpeSourceGroup = new Group(shell, SWT.NONE);
        qpeSourceGroup.setText(" QPE Source ");
        RowLayout qpeSourceLayout = new RowLayout();
        qpeSourceLayout.spacing = 5;
        qpeSourceGroup.setLayout(qpeSourceLayout);

        localRdo = new Button(qpeSourceGroup, SWT.RADIO);
        localRdo.setText("Local");
        localRdo.setSelection(true);

        rfcRdo = new Button(qpeSourceGroup, SWT.RADIO);
        rfcRdo.setText("RFC");
    }

    /**
     * Create the date and time controls.
     */
    private void createDateTimeAdjust() {
        Composite timeComp = new Composite(shell, SWT.NONE);
        GridLayout timeGl = new GridLayout(5, false);
        timeComp.setLayout(timeGl);

        Label dayAdjustLbl = new Label(timeComp, SWT.CENTER);
        dayAdjustLbl.setText("Day\nAdjust");

        // Add the time arrow buttons
        Composite timeArrowsComp = new Composite(timeComp, SWT.NONE);
        RowLayout timeArrowRl = new RowLayout(SWT.VERTICAL);
        timeArrowsComp.setLayout(timeArrowRl);

        RowData rd = new RowData(25, 25);
        upDayBtn = new Button(timeArrowsComp, SWT.ARROW);
        upDayBtn.setLayoutData(rd);
        upDayBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                cal.add(Calendar.DAY_OF_MONTH, 1);

                if (cal.getTimeInMillis() > SimulatedTime.getSystemTime()
                        .getTime().getTime()) {
                    cal.add(Calendar.DAY_OF_MONTH, -1);
                    return;
                }
                dateTimeTF.setText(dateTimeFmt.format(cal.getTime()));
            }
        });

        rd = new RowData(25, 25);
        dnDayBtn = new Button(timeArrowsComp, SWT.ARROW | SWT.DOWN);
        dnDayBtn.setLayoutData(rd);
        dnDayBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                Calendar tmpCal = Calendar.getInstance(TimeZone
                        .getTimeZone("GMT"));
                Date d = SimulatedTime.getSystemTime().getTime();
                tmpCal.setTime(d);
                tmpCal.add(Calendar.DAY_OF_MONTH, -3);

                cal.add(Calendar.DAY_OF_MONTH, -1);

                if (cal.getTimeInMillis() < tmpCal.getTimeInMillis()) {
                    cal.add(Calendar.DAY_OF_MONTH, 1);
                    return;
                }
                dateTimeTF.setText(dateTimeFmt.format(cal.getTime()));
            }
        });

        // Add the time text field
        GridData gd = new GridData(160, SWT.DEFAULT);
        dateTimeTF = new Text(timeComp, SWT.BORDER);
        dateTimeTF.setFont(font);
        dateTimeTF.setLayoutData(gd);

        // Add the hours arrows button
        Composite hoursArrowsComp = new Composite(timeComp, SWT.NONE);
        RowLayout hoursArrowRl = new RowLayout(SWT.VERTICAL);
        hoursArrowsComp.setLayout(hoursArrowRl);

        rd = new RowData(25, 25);
        upHoursBtn = new Button(hoursArrowsComp, SWT.ARROW);
        upHoursBtn.setLayoutData(rd);
        upHoursBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                cal.add(Calendar.HOUR_OF_DAY, 1);
                Date d = SimulatedTime.getSystemTime().getTime();
                Calendar tmpCal = Calendar.getInstance(TimeZone
                        .getTimeZone("GMT"));
                tmpCal.setTime(d);
                if (cal.getTimeInMillis() > tmpCal.getTimeInMillis()) {
                    cal.add(Calendar.HOUR_OF_DAY, -1);
                    return;
                }

                dateTimeTF.setText(dateTimeFmt.format(cal.getTime()));
            }
        });

        rd = new RowData(25, 25);
        dnHoursBtn = new Button(hoursArrowsComp, SWT.ARROW | SWT.DOWN);
        dnHoursBtn.setLayoutData(rd);
        dnHoursBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                Date d = SimulatedTime.getSystemTime().getTime();
                Calendar tmpCal = Calendar.getInstance(TimeZone
                        .getTimeZone("GMT"));
                tmpCal.setTime(d);

                tmpCal.add(Calendar.DAY_OF_MONTH, -3);

                cal.add(Calendar.HOUR_OF_DAY, -1);

                if (cal.getTimeInMillis() < tmpCal.getTimeInMillis()) {
                    cal.add(Calendar.HOUR_OF_DAY, 1);
                    return;
                }
                dateTimeTF.setText(dateTimeFmt.format(cal.getTime()));
            }
        });

        Label hoursAdjustLbl = new Label(timeComp, SWT.CENTER);
        hoursAdjustLbl.setText("Hour\nAdjust");
    }

    /**
     * Create the accumulation and time lapse controls.
     */
    private void createAccumTimeLapse() {
        Composite accumTimeLapseComp = new Composite(shell, SWT.NONE);
        GridLayout accumTimeGl = new GridLayout(7, false);
        accumTimeGl.horizontalSpacing = 2;
        accumTimeLapseComp.setLayout(accumTimeGl);

        // ----------------------------------------------
        // Create the Accumulate radio button & label
        // ----------------------------------------------
        Composite accumComp = new Composite(accumTimeLapseComp, SWT.NONE);
        RowLayout accumRl = new RowLayout(SWT.VERTICAL);
        accumRl.marginRight = 10;
        accumComp.setLayout(accumRl);

        accumRdo = new Button(accumComp, SWT.RADIO);
        accumRdo.setText("Accumulate");
        accumRdo.setSelection(true);
        accumRdo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                displayAsCbo.setEnabled(true);
                timeLapseRdo.setSelection(false);
                durationScale.setMaximum(72);
                durationScale.setSelection(1);
                durationNumLbl.setText(String.valueOf(durationScale
                        .getSelection()));
            }
        });

        Label max72Hr = new Label(accumComp, SWT.NONE);
        max72Hr.setText("(max 72 hours)");

        // ----------------------------------------------
        // Create the Time Lapse radio button & label
        // ----------------------------------------------
        Composite timeLapComp = new Composite(accumTimeLapseComp, SWT.NONE);
        RowLayout timeLapRl = new RowLayout(SWT.VERTICAL);
        timeLapRl.marginRight = 10;
        timeLapComp.setLayout(timeLapRl);

        timeLapseRdo = new Button(timeLapComp, SWT.RADIO);
        timeLapseRdo.setText("Time Lapse");
        timeLapseRdo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                displayAsCbo.setEnabled(false);
                accumRdo.setSelection(false);
                durationScale.setMaximum(24);
                durationScale.setSelection(1);
                durationNumLbl.setText(String.valueOf(durationScale
                        .getSelection()));
            }
        });

        Label max24Hr = new Label(timeLapComp, SWT.NONE);
        max24Hr.setText("(max 24 hours)");

        // ----------------------------------------------
        // Create the Time Lapse control buttons
        // ----------------------------------------------

        firstBtn = new Button(accumTimeLapseComp, SWT.PUSH);
        firstBtn.setImage(firstImg);
        firstBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                IDisplayPaneContainer container = EditorUtil
                        .getActiveVizContainer();
                if (container != null) {
                    container.getLoopProperties().setLooping(false);
                }
                IDescriptor descriptor = HydroDisplayManager.getInstance()
                        .getPane().getDescriptor();
                descriptor.getFrameCoordinator().changeFrame(
                        FrameChangeOperation.FIRST, FrameChangeMode.TIME_ONLY);
                HydroDisplayManager.getInstance().getPane().refresh();
            }
        });

        previousBtn = new Button(accumTimeLapseComp, SWT.PUSH);
        previousBtn.setImage(previousImg);
        previousBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                IDisplayPaneContainer container = EditorUtil
                        .getActiveVizContainer();
                if (container != null) {
                    container.getLoopProperties().setLooping(false);
                }
                IDescriptor descriptor = HydroDisplayManager.getInstance()
                        .getPane().getDescriptor();
                descriptor.getFrameCoordinator().changeFrame(
                        FrameChangeOperation.PREVIOUS,
                        FrameChangeMode.TIME_ONLY);
                HydroDisplayManager.getInstance().getPane().refresh();
            }
        });

        nextBtn = new Button(accumTimeLapseComp, SWT.PUSH);
        nextBtn.setImage(nextImg);
        nextBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                IDisplayPaneContainer container = EditorUtil
                        .getActiveVizContainer();
                if (container != null) {
                    container.getLoopProperties().setLooping(false);
                }
                IDescriptor descriptor = HydroDisplayManager.getInstance()
                        .getPane().getDescriptor();
                descriptor.getFrameCoordinator().changeFrame(
                        FrameChangeOperation.NEXT, FrameChangeMode.TIME_ONLY);
                HydroDisplayManager.getInstance().getPane().refresh();
            }
        });

        lastBtn = new Button(accumTimeLapseComp, SWT.PUSH);
        lastBtn.setImage(lastImg);
        lastBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                IDisplayPaneContainer container = EditorUtil
                        .getActiveVizContainer();
                if (container != null) {
                    container.getLoopProperties().setLooping(false);
                }
                IDescriptor descriptor = HydroDisplayManager.getInstance()
                        .getPane().getDescriptor();
                descriptor.getFrameCoordinator().changeFrame(
                        FrameChangeOperation.LAST, FrameChangeMode.TIME_ONLY);
                HydroDisplayManager.getInstance().getPane().refresh();
            }
        });

        loopBtn = new Button(accumTimeLapseComp, SWT.PUSH);
        loopBtn.setImage(loopImg);
        loopBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (timeLapseRdo.getSelection()) {
                    HydroDisplayManager displayManager = HydroDisplayManager
                            .getInstance();
                    if (displayManager.isTimeLapseMode()
                            && displayManager.getAccumInterval() == durationScale
                                    .getSelection()) {
                        IDisplayPaneContainer container = EditorUtil
                                .getActiveVizContainer();
                        if (container != null) {
                            container.getLoopProperties().setLooping(true);
                        }
                    } else {
                        showData();
                    }
                }
            }
        });

    }

    /**
     * Create the duration slider and label controls.
     */
    private void createDurationControl() {
        Composite durationComp = new Composite(shell, SWT.NONE);
        GridLayout durationGl = new GridLayout(3, false);
        durationGl.horizontalSpacing = 10;
        durationComp.setLayout(durationGl);

        Label durationLbl = new Label(durationComp, SWT.NONE);
        durationLbl.setText("Duration:");

        GridData gd = new GridData(250, SWT.DEFAULT);
        durationScale = new Scale(durationComp, SWT.HORIZONTAL);
        durationScale.setMinimum(1);
        durationScale.setMaximum(72);
        durationScale.setLayoutData(gd);
        durationScale.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                durationNumLbl.setText(String.valueOf(durationScale
                        .getSelection()));
            }
        });

        gd = new GridData(30, SWT.DEFAULT);
        durationNumLbl = new Label(durationComp, SWT.NONE);
        durationNumLbl.setText(String.valueOf(durationScale.getSelection()));
        durationNumLbl.setLayoutData(gd);
    }

    /**
     * Create the "Display As" combo box.
     */
    private void createDisplayAsControl() {
        Composite displayAsComp = new Composite(shell, SWT.NONE);
        GridLayout displayAsGl = new GridLayout(2, false);
        displayAsComp.setLayout(displayAsGl);

        Label displayAsLbl = new Label(displayAsComp, SWT.NONE);
        displayAsLbl.setText("Display As:");

        GridData gd = new GridData(130, SWT.DEFAULT);
        displayAsCbo = new Combo(displayAsComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        displayAsCbo.add("Grid");
        displayAsCbo.add("Basin");
        displayAsCbo.add("County");
        displayAsCbo.add("Zone");
        displayAsCbo.select(0);
        displayAsCbo.setLayoutData(gd);
        displayAsCbo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (!displayAsCbo.getItem(displayAsCbo.getSelectionIndex())
                        .equalsIgnoreCase("Grid")) {
                    idChk.setEnabled(true);
                    labelsChk.setEnabled(true);
                } else {
                    idChk.setEnabled(false);
                    labelsChk.setEnabled(false);
                }
            }
        });

    }

    /**
     * Create the annotation controls.
     */
    private void createAnnotateControls() {
        Composite annotateComp = new Composite(shell, SWT.NONE);
        GridLayout annotateGl = new GridLayout(3, false);
        annotateGl.horizontalSpacing = 15;
        annotateComp.setLayout(annotateGl);

        Label annotateLbl = new Label(annotateComp, SWT.NONE);
        annotateLbl.setText("Annotate:");

        idChk = new Button(annotateComp, SWT.CHECK);
        idChk.setText("Ids");
        idChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                showData();
            }
        });

        labelsChk = new Button(annotateComp, SWT.CHECK);
        labelsChk.setText("Labels");
        labelsChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                showData();
            }
        });
    }

    /**
     * Create the buttons located at the bottom of the dialog.
     */
    private void createBottomButtons() {
        Composite buttonComp = new Composite(shell, SWT.NONE);
        RowLayout layout = new RowLayout();
        layout.spacing = 18;
        layout.marginLeft = 5;
        buttonComp.setLayout(layout);

        RowData rd = new RowData(90, SWT.DEFAULT);
        showDataBtn = new Button(buttonComp, SWT.PUSH);
        showDataBtn.setText("Show Data");
        showDataBtn.setLayoutData(rd);
        showDataBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                showData();
            }
        });

        rd = new RowData(90, SWT.DEFAULT);
        endLapseBtn = new Button(buttonComp, SWT.PUSH);
        endLapseBtn.setText("End Lapse");
        endLapseBtn.setLayoutData(rd);
        endLapseBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                IDisplayPaneContainer container = EditorUtil
                        .getActiveVizContainer();
                if (container != null) {
                    container.getLoopProperties().setLooping(false);
                }
            }
        });

        rd = new RowData(90, SWT.DEFAULT);
        clearDataBtn = new Button(buttonComp, SWT.PUSH);
        clearDataBtn.setText("Clear Data");
        clearDataBtn.setLayoutData(rd);
        clearDataBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                HydroDisplayManager.getInstance().clearQPEData();
            }
        });

        rd = new RowData(90, SWT.DEFAULT);
        closeBtn = new Button(buttonComp, SWT.PUSH);
        closeBtn.setText("Close");
        closeBtn.setLayoutData(rd);
        closeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                close();
            }
        });
    }

    /**
     * Get the images for the looping buttons.
     */
    private void getImages() {
        ImageDescriptor id;

        id = Activator.imageDescriptorFromPlugin(
                "com.raytheon.viz.hydrocommon", "icons/first0.gif");
        firstImg = id.createImage();

        id = Activator.imageDescriptorFromPlugin(
                "com.raytheon.viz.hydrocommon", "icons/back0.gif");
        previousImg = id.createImage();

        id = Activator.imageDescriptorFromPlugin(
                "com.raytheon.viz.hydrocommon", "icons/fwd0.gif");
        nextImg = id.createImage();

        id = Activator.imageDescriptorFromPlugin(
                "com.raytheon.viz.hydrocommon", "icons/last0.gif");
        lastImg = id.createImage();

        id = Activator.imageDescriptorFromPlugin(
                "com.raytheon.viz.hydrocommon", "icons/looping0.gif");
        loopImg = id.createImage();
    }

    private void showData() {
        HydroDisplayManager displayManager = HydroDisplayManager.getInstance();
        if (localRdo.getSelection()) {
            /* Set the coloruse scheme. */
            displayManager.setCvUse("xmrg");
        } else {
            /* Set the coloruse scheme. */
            displayManager.setCvUse("RFCMOSAIC");
        }

        displayManager.setAccumInterval(durationScale.getSelection());
        displayManager.setDataDate(cal.getTime());
        displayManager.setDisplayType(displayAsCbo.getItem(displayAsCbo
                .getSelectionIndex()));
        displayManager.setTimeLapseMode(timeLapseRdo.getSelection());
        displayManager.setAccumulate(accumRdo.getSelection());
        displayManager.setIds(idChk.getSelection());
        displayManager.setLabels(labelsChk.getSelection());
        try {
            displayManager.displayQPE();
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM, e.getMessage(), e);
        }
    }
}
