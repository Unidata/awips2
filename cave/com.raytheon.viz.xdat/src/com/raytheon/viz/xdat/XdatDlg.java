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
package com.raytheon.viz.xdat;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.Locale;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.events.FocusAdapter;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.printing.PrintDialog;
import org.eclipse.swt.printing.Printer;
import org.eclipse.swt.printing.PrinterData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.viz.hydrocommon.HydroConstants;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * This class displays the main XDAT dialog.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 10 Nov 2008             lvenable    Initial creation.
 * 16 Jan 2009  1883       lvenable    Added code updates/fixes for displaying data
 *                                     on the dialog.
 * 10 Feb 2009             wkwock      Added functions and clean up.
 * 31 Mar 2009  2141       mduff       Fixed cancel save error.
 * 23 Nov 2010  6244       lbousaidi   cleared idTF box after search
 * 16 Mar 2011  6251	   lbousaidi   activate [Enter] for idTF box
 * 02 Jun 2011  9150       mduff       xdat_flood_hours needs to be negative on the calendar.add
 * 22 May 2015  4501       skorolev    Removed old DB connection commands. Got rid of Vector.
 * 26 Jan 2016  5054       randerso    Made XdatDlg parented to display
 * 04 Aug 2016  5800       mduff       Fixed widget disposed errors, cleanup.
 * 
 * </pre>
 * 
 * @author lvenable
 * 
 */
public class XdatDlg extends CaveSWTDialog implements ITextDisplay {

    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(XdatDlg.class);

    private final String NEWLINE = "\n";

    /**
     * Control font.
     */
    private Font controlFont;

    /**
     * Version.
     */
    private final String version = "TaskOrder 10";

    /**
     * ID text control.
     */
    private Text idTF;

    /**
     * HG radio button.
     */
    private Button hgRdo;

    /**
     * HP radio button.
     */
    private Button hpRdo;

    /**
     * HT radio button.
     */
    private Button htRdo;

    /**
     * QT radio button.
     */
    private Button qtRdo;

    /**
     * PC radio button.
     */
    private Button pcRdo;

    /**
     * PP radio button.
     */
    private Button ppRdo;

    /**
     * TA radio button.
     */
    private Button taRdo;

    /**
     * Physical Element text control.
     */
    private Text peTF;

    /**
     * Start date list control.
     */
    private List startDateList;

    /**
     * End date list control.
     */
    private List endDateList;

    /**
     * Main XDAT data styled text control.
     */
    private StyledText textArea;

    /**
     * Edit Data button
     */
    private Button editDataBtn;

    /**
     * AppsDefaults instance.
     */
    private final AppsDefaults appsDefaults;

    /**
     * Database manager class.
     */
    private final XdatDB databaseMgr;

    /**
     * Array of Physical Element radio buttons.
     */
    private ArrayList<Button> peButtons;

    /**
     * Printer class.
     */
    private Printer printer;

    private GroupDataDlg groupDlg = null;

    /**
     * Constructor.
     * 
     * @param display
     * 
     */
    public XdatDlg(Display display) {
        super(display, SWT.DIALOG_TRIM | SWT.RESIZE | CAVE.DO_NOT_BLOCK);
        databaseMgr = new XdatDB();
        appsDefaults = AppsDefaults.getInstance();

        setText("xdat (Version: " + version + ") (db_name = " + XdatDB.IHFS
                + ")");
    }

    @Override
    protected Layout constructShellLayout() {
        return new GridLayout(1, false);
    }

    @Override
    protected void disposed() {
        controlFont.dispose();
    }

    @Override
    protected void initializeComponents(Shell shell) {
        setReturnValue(false);

        controlFont = new Font(shell.getDisplay(), "Monospace", 10, SWT.NORMAL);
        createTopButtons();
        createMiddleControls();
        createQuitButton();
    }

    @Override
    protected void preOpened() {
        shell.setMinimumSize(shell.getBounds().width, shell.getBounds().height);
    }

    /**
     * Create the buttons at the top of the dialog.
     */
    private void createTopButtons() {
        Composite topComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(5, false);
        gl.horizontalSpacing = 30;
        topComp.setLayout(gl);

        int smlWidth = 110;
        int medWidth = 150;
        int lrgWidth = 220;

        GridData gd = new GridData(smlWidth, SWT.DEFAULT);
        Button listIdBtn = new Button(topComp, SWT.PUSH);
        listIdBtn.setText("List IDs");
        listIdBtn.setLayoutData(gd);

        listIdBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                displaySiteListDlg();
            }
        });

        gd = new GridData(medWidth, SWT.DEFAULT);
        Button searchBtn = new Button(topComp, SWT.PUSH);
        searchBtn.setText("Search");
        searchBtn.setLayoutData(gd);
        searchBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                displaySearchData();
                idTF.setText(idTF.getText().trim().toUpperCase());
            }
        });

        gd = new GridData(lrgWidth, SWT.DEFAULT);
        Button siteAbvFldStgBtn = new Button(topComp, SWT.PUSH);
        siteAbvFldStgBtn.setText("Sites Above Flood Stage");
        siteAbvFldStgBtn.setLayoutData(gd);
        siteAbvFldStgBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                displaySiteAboveFldStg();
            }
        });

        gd = new GridData(medWidth, SWT.DEFAULT);
        Button unknownSitesBtn = new Button(topComp, SWT.PUSH);
        unknownSitesBtn.setText("Unknown Sites");
        unknownSitesBtn.setLayoutData(gd);
        unknownSitesBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                displayUnknownSitesData();
            }
        });

        gd = new GridData(medWidth, SWT.DEFAULT);
        Button printDataBtn = new Button(topComp, SWT.PUSH);
        printDataBtn.setText("Print Data");
        printDataBtn.setLayoutData(gd);
        printDataBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                printDisplayText();
            }
        });

        Label enterIdLbl = new Label(topComp, SWT.NONE);
        enterIdLbl.setText("Enter ID:");

        gd = new GridData(medWidth, SWT.DEFAULT);
        Button insertDataBtn = new Button(topComp, SWT.PUSH);
        insertDataBtn.setText("Insert Data...");
        insertDataBtn.setLayoutData(gd);
        final ITextDisplay displayCB = this;
        insertDataBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                String PeType = getPeType();
                idTF.setText(idTF.getText().trim().toUpperCase());
                InsertDataDlg insertDlg = new InsertDataDlg(shell, PeType,
                        databaseMgr, displayCB);
                insertDlg.open();
            }
        });

        gd = new GridData(lrgWidth, SWT.DEFAULT);
        Button precipDataBtn = new Button(topComp, SWT.PUSH);
        precipDataBtn.setText("Precipitation Data...");
        precipDataBtn.setLayoutData(gd);
        precipDataBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                displayPrecipDataDlg();
            }
        });

        gd = new GridData(medWidth, SWT.DEFAULT);
        Button rejectedDataBtn = new Button(topComp, SWT.PUSH);
        rejectedDataBtn.setText("Rejected Data");
        rejectedDataBtn.setLayoutData(gd);
        rejectedDataBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                displayRejectedData();
            }
        });

        gd = new GridData(medWidth, SWT.DEFAULT);
        Button saveDataBtn = new Button(topComp, SWT.PUSH);
        saveDataBtn.setText("Save Data");
        saveDataBtn.setLayoutData(gd);
        saveDataBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                saveData();
            }
        });

        gd = new GridData(smlWidth, SWT.DEFAULT);
        idTF = new Text(topComp, SWT.BORDER);
        idTF.setLayoutData(gd);
        idTF.addKeyListener(new KeyAdapter() {
            @Override
            public void keyPressed(KeyEvent e) {
                if ((e.keyCode == SWT.KEYPAD_CR) || (e.keyCode == SWT.CR)) {
                    displaySearchData();
                    idTF.setText(idTF.getText().trim().toUpperCase());
                }
            }
        });

        gd = new GridData(medWidth, SWT.DEFAULT);
        editDataBtn = new Button(topComp, SWT.PUSH);
        editDataBtn.setText("Edit Data...");
        editDataBtn.setLayoutData(gd);
        editDataBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                idTF.setText(idTF.getText().trim().toUpperCase());
                XdatEditDataDlg editDataDlg = new XdatEditDataDlg(shell,
                        displayCB, databaseMgr);
                editDataDlg.open();
            }
        });

        gd = new GridData(lrgWidth, SWT.DEFAULT);
        Button groupDataBtn = new Button(topComp, SWT.PUSH);
        groupDataBtn.setText("Group Data");
        groupDataBtn.setLayoutData(gd);
        groupDataBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                displayGroupDataDlg();
            }
        });

        gd = new GridData(medWidth, SWT.DEFAULT);
        Button sitesTurnedOffBtn = new Button(topComp, SWT.PUSH);
        sitesTurnedOffBtn.setText("Sites Turned Off");
        sitesTurnedOffBtn.setLayoutData(gd);
        sitesTurnedOffBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                displaySitesTurnedOffData();
            }
        });
    }

    /**
     * Create the composite for the controls in the middle of the dialog.
     */
    private void createMiddleControls() {
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        Composite mainComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(2, false);
        gl.horizontalSpacing = 20;
        mainComp.setLayout(gl);
        mainComp.setLayoutData(gd);

        createPeDateControls(mainComp);
        createTextArea(mainComp);

    }

    /**
     * Create the physical element controls.
     * 
     * @param parentComp
     *            Parent composite.
     */
    private void createPeDateControls(Composite parentComp) {
        peButtons = new ArrayList<>();

        GridData gd = new GridData(SWT.DEFAULT, SWT.TOP, false, true);
        Composite peDateComp = new Composite(parentComp, SWT.NONE);
        peDateComp.setLayout(new GridLayout(2, false));
        peDateComp.setLayoutData(gd);

        // ------------------------------------------------------
        // Create the PE & Date list controls on the left side
        // ------------------------------------------------------
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.horizontalSpan = 2;
        Group selectPeGrp = new Group(peDateComp, SWT.NONE);
        selectPeGrp.setLayout(new GridLayout(1, false));
        selectPeGrp.setLayoutData(gd);
        selectPeGrp.setText(" Select PE ");
        // ------------------------------------------------------
        // Create a composite for the HG control.
        // ------------------------------------------------------

        hgRdo = new Button(selectPeGrp, SWT.RADIO);
        hgRdo.setText("HG");
        hgRdo.setSelection(true);
        peButtons.add(hgRdo);

        hpRdo = new Button(selectPeGrp, SWT.RADIO);
        hpRdo.setText("HP");
        peButtons.add(hpRdo);

        htRdo = new Button(selectPeGrp, SWT.RADIO);
        htRdo.setText("HT");
        peButtons.add(htRdo);

        qtRdo = new Button(selectPeGrp, SWT.RADIO);
        qtRdo.setText("QT");
        peButtons.add(qtRdo);

        pcRdo = new Button(selectPeGrp, SWT.RADIO);
        pcRdo.setText("PC");
        peButtons.add(pcRdo);

        ppRdo = new Button(selectPeGrp, SWT.RADIO);
        ppRdo.setText("PP");
        peButtons.add(ppRdo);

        taRdo = new Button(selectPeGrp, SWT.RADIO);
        taRdo.setText("TA");
        peButtons.add(taRdo);

        gd = new GridData(SWT.DEFAULT, SWT.CENTER, false, true);
        gd.verticalIndent = 10;
        Label enterPeLbl = new Label(peDateComp, SWT.NONE);
        enterPeLbl.setText("Enter PE:");
        enterPeLbl.setLayoutData(gd);

        gd = new GridData(40, SWT.DEFAULT);
        gd.verticalIndent = 10;
        peTF = new Text(peDateComp, SWT.BORDER);
        peTF.setLayoutData(gd);
        peTF.setTextLimit(2);

        peTF.addFocusListener(new FocusAdapter() {
            @Override
            public void focusLost(FocusEvent e) {
                if (peTF.getText().trim().length() > 0) {
                    peTF.setText(peTF.getText().trim().toUpperCase());
                }
            }

        });

        gd = new GridData();
        gd.horizontalSpan = 2;
        gd.verticalIndent = 20;
        Label startDateLbl = new Label(peDateComp, SWT.NONE);
        startDateLbl.setText("Starting Date:");
        startDateLbl.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.horizontalSpan = 2;
        gd.widthHint = 100;
        gd.heightHint = 130;
        startDateList = new List(peDateComp, SWT.BORDER | SWT.SINGLE
                | SWT.V_SCROLL);
        startDateList.setLayoutData(gd);
        startDateList.setFont(controlFont);
        startDateList.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                validateStartEndTimeSelection();
            }
        });

        gd = new GridData();
        gd.horizontalSpan = 2;
        gd.verticalIndent = 20;
        Label endDateLbl = new Label(peDateComp, SWT.NONE);
        endDateLbl.setText("Ending Date:");
        endDateLbl.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.horizontalSpan = 2;
        gd.widthHint = 100;
        gd.heightHint = 130;
        endDateList = new List(peDateComp, SWT.BORDER | SWT.SINGLE
                | SWT.V_SCROLL);
        endDateList.setLayoutData(gd);
        endDateList.setFont(controlFont);
        endDateList.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                validateStartEndTimeSelection();
            }
        });

        SimpleDateFormat dateFmt = new SimpleDateFormat("MM/dd/yyyy", Locale.US);
        dateFmt.setTimeZone(TimeUtil.GMT_TIME_ZONE);
        Calendar calendar = null;
        try {
            String today = appsDefaults.getToken("xdat_settoday");
            if (today == null) {
                calendar = TimeUtil.newGmtCalendar();
            } else {
                Date sDate = dateFmt.parse(today);
                calendar = TimeUtil.newGmtCalendar();
                calendar.setTime(sDate);
            }
        } catch (ParseException e) {
            calendar = TimeUtil.newGmtCalendar();
        }

        dateFmt = new SimpleDateFormat("yyyy-MM-dd", Locale.US);
        dateFmt.setTimeZone(TimeUtil.GMT_TIME_ZONE);

        int obsDays = databaseMgr.get_obs_days();
        for (int i = 0; i < obsDays; i++) {
            String dateStr = dateFmt.format(calendar.getTime());
            startDateList.add(dateStr);
            endDateList.add(dateStr);
            calendar.add(Calendar.DAY_OF_YEAR, -1);
        }

        if (startDateList.getItemCount() > 0) {
            startDateList.select(0);
        }

        if (endDateList.getItemCount() > 0) {
            endDateList.select(0);
        }
    }

    /**
     * Create the styled text control that will display the data.
     * 
     * @param parentComp
     *            Parent composite.
     */
    private void createTextArea(Composite parentComp) {
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.widthHint = 750;
        gd.heightHint = 500;
        textArea = new StyledText(parentComp, SWT.BORDER | SWT.MULTI
                | SWT.V_SCROLL | SWT.H_SCROLL);
        textArea.setFont(controlFont);
        textArea.setEditable(false);
        textArea.setLayoutData(gd);
    }

    /**
     * Create the Quit button at the bottom of the dialog.
     */
    private void createQuitButton() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite buttonComp = new Composite(shell, SWT.NONE);
        buttonComp.setLayout(new GridLayout(1, false));
        buttonComp.setLayoutData(gd);

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = 100;
        Button quitBtn = new Button(buttonComp, SWT.PUSH);
        quitBtn.setText("Quit");
        quitBtn.setLayoutData(gd);
        quitBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                shell.dispose();
            }
        });
    }

    /**
     * Set the text on the display.
     * 
     * @param textArray
     *            String array to be displayed in the main text area.
     */
    @Override
    public void setDisplayText(String textArray[]) {
        if (textArray != null) {
            textArea.setText("");

            for (int i = 0; i < textArray.length; i++) {
                textArea.append(textArray[i] + NEWLINE);
            }
        }
    }

    /**
     * Set the text on the display.
     * 
     * @param text
     *            String to be displayed in the main text area.
     */
    @Override
    public void setDisplayText(String text) {
        textArea.setText(text);
    }

    /**
     * Save the data (text on the display) to a file.
     */
    private void saveData() {

        /*
         * Check if there is text to be saved.
         */
        if (textArea.getText().trim().length() == 0) {
            MessageBox mb = new MessageBox(getShell(), SWT.ICON_WARNING
                    | SWT.OK);
            mb.setText("Save Warning");
            mb.setMessage("There is no text to be saved.");
            mb.open();
            return;
        }

        /*
         * Filter on files ending in ".dat". Filter extensions are used to
         * filter which files are displayed.
         */
        final String[] FILTER_NAMES = { "Data File (*.dat)" };

        final String[] FILTER_EXTS = { "*.dat" };

        /*
         * Display the Save dialog.
         */
        FileDialog fileDlg = new FileDialog(shell, SWT.SAVE);
        fileDlg.setFilterNames(FILTER_NAMES);
        fileDlg.setFilterExtensions(FILTER_EXTS);
        String fileName = fileDlg.open();

        /*
         * If the save file is null, the user cancel the dialog so we return.
         */
        if (fileName == null) {
            return;
        }

        File saveFile = new File(fileName);

        /*
         * Check if the file exists. If it does the prompt the user to
         * overwrite.
         */
        if (saveFile.exists() == true) {

            MessageBox mb = new MessageBox(getShell(), SWT.ICON_WARNING
                    | SWT.YES | SWT.NO);
            mb.setText("Save Warning");
            mb.setMessage("The file to save already exists.\n\n"
                    + "Do you want to overwrite the file?");
            int answer = mb.open();

            if (answer == SWT.NO) {
                return;
            }
        }

        /*
         * Write the data (text) out to file.
         */
        try (FileOutputStream fos = new FileOutputStream(saveFile)) {
            OutputStreamWriter out = new OutputStreamWriter(fos);
            String buf = textArea.getText();
            out.write(buf);
            out.close();
        } catch (IOException e) {
            statusHandler.error("Error writing data to file. ", e);
        }
    }

    /**
     * Get the Physical Element. If one is entered in the PE text control then
     * return it. If not then return the selected PE (radio button).
     * 
     * @return PE type
     */
    public String getPeType() {
        String peType = peTF.getText().trim().toUpperCase();

        if (peType.isEmpty()) {
            peType = getSelectedPE();
        }

        return peType;
    }

    /**
     * Get the start date.
     * 
     * @return The start date in a string format.
     */
    @Override
    public String getStartDate() {
        String startDate = null;
        if (startDateList.getSelectionIndex() >= 0) {
            startDate = startDateList
                    .getItem(startDateList.getSelectionIndex());
        } else {
            startDate = startDateList.getItem(0);
        }

        return startDate;
    }

    /**
     * Get the end date.
     * 
     * @return The end date in a string format.
     */
    @Override
    public String getEndDate() {
        String endDate = null;

        if (endDateList.getSelectionIndex() >= 0) {
            endDate = endDateList.getItem(endDateList.getSelectionIndex());
        } else {
            endDate = endDateList.getItem(0);
        }

        return endDate;
    }

    /**
     * Display the rejected data on the display.
     */
    private void displayRejectedData() {
        String startDateStr = "";
        String endDateStr = "";

        if (startDateList.getSelectionIndex() == -1) {
            startDateStr = startDateList.getItem(0);
        } else {
            startDateStr = startDateList.getItem(startDateList
                    .getSelectionIndex());
        }

        if (endDateList.getSelectionIndex() == -1) {
            endDateStr = startDateList.getItem(0);
        } else {
            endDateStr = startDateList.getItem(endDateList.getSelectionIndex());
        }

        if (validateStartEndTimeSelection() == false) {
            return;
        }

        String peStr = getSelectedPE();

        java.util.List<String[]> rejectedDataBuf = databaseMgr.getRejectedData(
                peStr, startDateStr, endDateStr);

        if (rejectedDataBuf == null) {
            textArea.setText("No rejected " + peStr
                    + " data found in the rejecteddata table.\n");
        } else {

            String formattedStr = formatRejectedData(rejectedDataBuf, peStr);
            textArea.setText(formattedStr);
        }
    }

    /**
     * Format the rejected data for displaying.
     * 
     * @param rejectedDataBuf
     *            Array of rejected data.
     * @param peStr
     *            Physical Element.
     * @return The formatted data.
     */
    private String formatRejectedData(java.util.List<String[]> rejectedDataBuf,
            String peStr) {
        StringBuilder strBld = new StringBuilder();

        String titleFmt = "                                   %S %S %S";
        String headerFmt = "   %S      %S    %S    %S    %S        %S                 %S          %S";
        String dataFmt = " %-9S %S     %-4S  %S    %S    %S    %-23S  %8.1f";

        /*
         * Add the Title text
         */
        strBld.append(String.format(titleFmt, "REJECTED", peStr, "DATA"))
                .append("\n\n");

        /*
         * Add the data header
         */
        strBld.append(
                String.format(headerFmt, "ID", "PE", "DUR", "TS", "E",
                        "OBSTIME", "POSTING TIME", "VALUE")).append(NEWLINE);

        /*
         * Add a dash separator.
         */
        for (int i = 0; i < 91; i++) {
            strBld.append("-");
        }
        strBld.append(NEWLINE);

        /*
         * Add the data
         */
        for (String[] strArray : rejectedDataBuf) {

            double dblVal = Double.valueOf(strArray[7]);
            double rndVal = (Math.round(dblVal * 10.0)) / 10.0;

            strBld.append(
                    String.format(dataFmt, strArray[0], strArray[1],
                            strArray[2], strArray[3], strArray[4], strArray[5],
                            strArray[6], rndVal)).append(NEWLINE);
        }

        return strBld.toString();
    }

    /**
     * Display the site above flood stage data on the display.
     */
    private void displaySiteAboveFldStg() {
        SimpleDateFormat dbFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
        dbFormat.setTimeZone(TimeUtil.GMT_TIME_ZONE);
        SimpleDateFormat startDate = new SimpleDateFormat("mm/dd/yyy",
                Locale.US);
        startDate.setTimeZone(TimeUtil.GMT_TIME_ZONE);

        Calendar cal = TimeUtil.newGmtCalendar();
        Date sDate = cal.getTime();

        String setToday = appsDefaults.getToken("xdat_settoday");
        String floodHours = appsDefaults.getToken("xdat_flood_hours", "6");

        try {
            if ((setToday != null) && !setToday.isEmpty()
                    && !setToday.equals(" ")) {
                sDate = startDate.parse(setToday);
            }
        } catch (ParseException e) {
            MessageBox mb = new MessageBox(getShell(), SWT.ICON_WARNING
                    | SWT.OK);
            mb.setText("Error with xdat_settoday token");
            mb.setMessage("Invalid value entered as token value.");
            mb.open();
        }

        cal.setTime(sDate);
        cal.add(Calendar.HOUR_OF_DAY, -1 * (Integer.parseInt(floodHours)));
        sDate = cal.getTime();
        String dateStr = dbFormat.format(sDate);

        java.util.List<String[]> aboveFsBuf = databaseMgr
                .getAboveFsSearch(dateStr);

        if (aboveFsBuf == null) {
            textArea.setText("No sites above flood stage");
        } else {
            String formattedStr = formatSiteAboveFldStageData(aboveFsBuf, sDate);
            textArea.setText(formattedStr);
        }
    }

    /**
     * Format the site above flood stage data for displaying.
     * 
     * @param aboveFsBuf
     *            Array of flood stage data.
     * @return The formatted data.
     */
    private String formatSiteAboveFldStageData(
            java.util.List<String[]> aboveFsBuf, Date sDate) {
        SimpleDateFormat displayDate = new SimpleDateFormat(
                "yyyy-MM-dd HH:00:00", Locale.US);
        StringBuilder strBld = new StringBuilder();

        String titleFmt = "         %s %s";
        String headerFmt = "   %S      %S    %S    %S    %S         %S             %S       %S     %S";
        String dataFmt = " %-9S %S     %-4S  %S    %S    %S     %8S  %8.2f    %3d";

        /*
         * Add the Title text
         */
        Calendar cal = TimeUtil.newGmtCalendar(sDate);
        String floodHours = appsDefaults.getToken("xdat_flood_hours");
        if ((floodHours != null) && (floodHours.length() > 0)) {
            cal.add(Calendar.HOUR_OF_DAY, -1 * (Integer.parseInt(floodHours)));
        }

        strBld.append(
                String.format(titleFmt, "Site Above Flood Stage since",
                        displayDate.format(new Date(cal.getTimeInMillis()))))
                .append(NEWLINE).append(NEWLINE);

        /*
         * Add the data header
         */
        strBld.append(
                String.format(headerFmt, "ID", "PE", "DUR", "TS", "E",
                        "OBSTIME", "PRODUCT", "VALUE", "FS")).append(NEWLINE);

        /*
         * Add a dash separator.
         */
        for (int i = 0; i < 86; i++) {
            strBld.append("-");
        }
        strBld.append(NEWLINE);

        /*
         * Add the data
         */
        for (String[] dataArray : aboveFsBuf) {

            double dblVal = Double.valueOf(dataArray[6]);
            double rndVal = (Math.round(dblVal * 100.0)) / 100.0;

            int fsVal = Math.round(Double.valueOf(dataArray[8]).floatValue());

            String productID = dataArray[7];
            if (productID == null) {
                productID = "";
            }

            strBld.append(
                    String.format(dataFmt, dataArray[0], dataArray[1],
                            dataArray[2], dataArray[3], dataArray[4],
                            dataArray[5], productID, rndVal, fsVal)).append(
                    NEWLINE);
        }

        return strBld.toString();
    }

    /**
     * Display the site turned off data on the display.
     */
    private void displaySitesTurnedOffData() {

        java.util.List<String[]> sitesTurnedOffBuf = databaseMgr
                .getSitesTurnedOffData();

        if (sitesTurnedOffBuf == null) {
            textArea.setText("No location data found with post = 0");
        } else {
            String formattedStr = formatSitesTurnedOffData(sitesTurnedOffBuf);
            textArea.setText(formattedStr);
        }
    }

    /**
     * Format the sites turned off data for displaying.
     * 
     * @param sitesTurnedOffBuf
     *            Array of data.
     * @return The formatted data.
     */
    private String formatSitesTurnedOffData(
            java.util.List<String[]> sitesTurnedOffBuf) {
        StringBuilder strBld = new StringBuilder();

        String titleFmt = "%50s";
        String headerFmt = "   %S          %S                 %S       %S";
        String dataFmt = " %-9S     %-20S     %2S        %S";

        /*
         * Add the Title text
         */
        strBld.append(String.format(titleFmt, "SITES WITH LOCATION POST = 0"))
                .append(NEWLINE).append(NEWLINE);

        /*
         * Add the data header
         */
        strBld.append(
                String.format(headerFmt, "ID", "County", "State", "Description"))
                .append(NEWLINE);

        /*
         * Add a dash separator.
         */
        for (int i = 0; i < 70; i++) {
            strBld.append("-");
        }
        strBld.append(NEWLINE);

        /*
         * Add the data
         */
        for (String[] dataArray : sitesTurnedOffBuf) {

            strBld.append(
                    String.format(dataFmt, dataArray[0], dataArray[1],
                            dataArray[2], dataArray[3])).append(NEWLINE);
        }

        return strBld.toString();
    }

    /**
     * Display the unknown sites data on the display.
     */
    private void displayUnknownSitesData() {
        String postFlag = appsDefaults.getToken("xdat_post_unk");

        if (postFlag.equalsIgnoreCase("NONE")) {
            textArea.setText("Shefdecoder not posting unknown data.");
            return;
        }

        String unknownTable = "unkstnvalue";
        if (postFlag.compareTo("IDS_ONLY") == 0) {
            unknownTable = "unkstn";
        }

        java.util.List<String[]> unknownSiteBuf = databaseMgr
                .getUnknownSites(unknownTable);

        if (unknownSiteBuf == null) {
            textArea.setText("No unknown station data found in table "
                    + unknownTable + ".\n");
        } else {
            String formattedStr = formatUnknownSitesData(unknownSiteBuf);
            textArea.setText(formattedStr);
        }
    }

    /**
     * Format the unknown sites data for displaying.
     * 
     * @param unknownSiteBuf
     *            Array of data.
     * @return The formatted data.
     */
    private String formatUnknownSitesData(
            java.util.List<String[]> unknownSiteBuf) {
        StringBuilder strBld = new StringBuilder();

        String titleFmt = "%40s";
        String headerFmt = "   %S         %S    %S";
        String dataFmt = " %-8S     %-10S    %S";

        /*
         * Add the Title text
         */
        strBld.append(String.format(titleFmt, "Unknown Station IDs")).append(
                "\n\n");

        /*
         * Add the data header
         */
        strBld.append(
                String.format(headerFmt, "ID", "Product ID", "Product Time"))
                .append(NEWLINE);

        /*
         * Add a dash separator.
         */
        for (int i = 0; i < 70; i++) {
            strBld.append("-");
        }
        strBld.append(NEWLINE);

        /*
         * Add the data
         */
        for (String[] dataArray : unknownSiteBuf) {
            String productID = dataArray[1];
            if (productID == null) {
                productID = "";
            }

            String productTime = dataArray[2];
            if (productTime == null) {
                productTime = "";
            }

            strBld.append(
                    String.format(dataFmt, dataArray[0], productID, productTime))
                    .append(NEWLINE);
        }

        return strBld.toString();
    }

    /**
     * Display the search data on the display.
     */
    private void displaySearchData() {
        /*
         * code very similar to SiteListDlg.displayListSelection(). need
         * refactor
         */

        String selectedId = idTF.getText().trim().toUpperCase();

        if (selectedId.trim().compareTo("") == 0) {
            MessageBox mb = new MessageBox(getShell(), SWT.ICON_WARNING
                    | SWT.OK);
            mb.setText("Search Warning");
            mb.setMessage("You need to enter an ID into the ID text field.");
            mb.open();
            return;
        }

        String peType = getSelectedPE();

        java.util.List<String[]> dataVec = databaseMgr.getListData(selectedId,
                peType, getStartDate(), getEndDate());

        if (dataVec == null) {
            String[] msg = new String[] { "No data available." };
            setDisplayText(msg);
            return;
        }

        String locationDes = databaseMgr.getLocationDes(selectedId);

        if (locationDes.equals("null")) {
            locationDes = "";
        }

        String[] displayData = new String[dataVec.size() + 3];
        // 1st three lines for header
        String dataFmt = "%-8S %2s   %-4S %2S %1S %19S %13S % 6.2f   % 6.2f";
        String displayHeader = " ID      PE  DUR   TS E       OBSTIME           PRODUCT    VALUE   CHANGE";
        String dashLine = "---------------------------------------------------------------------------";
        displayData[0] = "\t\t" + selectedId + "\t  " + locationDes;
        displayData[1] = displayHeader;
        displayData[2] = dashLine;

        for (int i = 0; i < dataVec.size(); i++) {
            String[] rowData = dataVec.get(i);

            double dblVal = Double.valueOf(rowData[5]);
            double rndVal = (Math.round(dblVal * 100.0)) / 100.0;
            double change = 0;
            /*
             * if this is not the last one, then calculate the changes
             */
            if (i < (dataVec.size() - 1)) {
                String[] nextRowData = dataVec.get(i + 1);
                if ((Double.valueOf(rowData[5]) != HydroConstants.MISSING_VALUE)
                        && (Double.valueOf(nextRowData[5]) != HydroConstants.MISSING_VALUE)) {
                    change = Double.valueOf(rowData[5])
                            - Double.valueOf(nextRowData[5]);
                    change = (Math.round(change * 100.0)) / 100.0;
                }
            }

            String productID = rowData[0];
            if (productID == null) {
                productID = "";
            }

            displayData[i + 3] = String.format(dataFmt, selectedId, peType,
                    rowData[1], rowData[2], rowData[3], rowData[4], productID,
                    rndVal, change);
        }

        setDisplayText(displayData);
    }

    /**
     * Validate the start and end date/time. Checks if the start date/time is
     * later that the end date/time.
     * 
     * @return True if the start date/time is before the end date/time.
     */
    private boolean validateStartEndTimeSelection() {

        String start = startDateList.getItem(startDateList.getSelectionIndex());
        String end = endDateList.getItem(endDateList.getSelectionIndex());

        if (start.compareTo(end) <= 0) {
            return true;
        }

        MessageBox mb = new MessageBox(getShell(), SWT.ICON_WARNING | SWT.OK);
        mb.setText("Date Warning");
        mb.setMessage("The selected end time is before the selected start time.");
        mb.open();

        return false;
    }

    /**
     * Get the user selected physical element.
     * 
     * @return The selected physical element.
     */
    @Override
    public String getSelectedPE() {
        String peType = peTF.getText().trim().toUpperCase();

        if (peType.compareTo("") != 0) {
            return peType;
        }

        for (Button peBtn : peButtons) {
            if (peBtn.getSelection() == true) {
                peType = peBtn.getText();
                break;
            }
        }

        return peType;
    }

    /**
     * Get the highlighted text in the main panel. This returns an array of
     * strings. Each element in the array is one line of data. This will return
     * the full line of data even if there is a partial selection.
     */
    @Override
    public java.util.List<String> getSelectedText() {

        java.util.List<String> array = new ArrayList<>();

        // If there is no selection then return an empty array.
        if (textArea.getSelectionText().length() == 0) {
            return array;
        }
        Point pt = textArea.getSelection();
        int startLine = textArea.getLineAtOffset(pt.x);
        int endLine = textArea.getLineAtOffset(pt.y - 1);

        /*
         * If the selection ends on a line that is in the header then return an
         * empty array.
         */
        if (endLine < 3) {
            return array;
        }

        /*
         * If the start line starts in the header area then set the start line
         * to the index of the first data line.
         */
        if (startLine < 3) {
            startLine = 3;
        }

        for (int i = startLine; i <= endLine; i++) {
            array.add(textArea.getLine(i));
        }

        return array;
    }

    /**
     * Display the site list dialog.
     */
    private void displaySiteListDlg() {

        SiteListDlg siteDlg = new SiteListDlg(shell, databaseMgr, this);
        siteDlg.open();
    }

    /**
     * Display the precipitation data dialog.
     */
    private void displayPrecipDataDlg() {

        PrecipDataDlg precipDlg = new PrecipDataDlg(shell, this, databaseMgr);
        precipDlg.open();
    }

    /**
     * Display the group data dialog.
     */
    private void displayGroupDataDlg() {
        groupDlg = new GroupDataDlg(shell, databaseMgr, this);
        groupDlg.open();
        groupDlg = null;
    }

    /**
     * Print the formatted text on the display.
     */
    private void printDisplayText() {

        final String text = textArea.getText();

        if (text.trim().length() == 0) {
            MessageBox mb = new MessageBox(getShell(), SWT.ICON_WARNING
                    | SWT.OK);
            mb.setText("Print Warning");
            mb.setMessage("There is no text to be printed.");
            mb.open();
            return;
        }

        if (text != null) {
            PrintDialog dialog = new PrintDialog(shell, SWT.NONE);
            PrinterData data = dialog.open();

            if (data == null) {
                return;
            }

            printer = new Printer(data);

            /*
             * Do the printing in a background thread so that spooling does not
             * freeze the UI.
             */
            Thread printingThread = new Thread("PrintTable") {
                @Override
                public void run() {
                    PrintManager printMgr = new PrintManager(printer);
                    printMgr.print(text);
                    printer.dispose();
                }
            };
            printingThread.start();
        }
    }

    @Override
    public String getSelectedSite() {
        return idTF.getText();
    }
}
