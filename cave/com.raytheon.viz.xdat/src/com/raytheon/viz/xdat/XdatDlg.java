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
import java.util.Collections;
import java.util.Date;
import java.util.Iterator;
import java.util.Locale;
import java.util.Map;
import java.util.TimeZone;

import org.apache.commons.lang.math.NumberUtils;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.events.FocusAdapter;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
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
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.viz.hydrocommon.HydroConstants;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * This class displays the main XDAT dialog.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#   Engineer     Description
 * ------------- --------- ------------ ----------------------------------------
 * Nov 10, 2008            lvenable     Initial creation.
 * Jan 16, 2009  1883      lvenable     Added code updates/fixes for displaying
 *                                      data on the dialog.
 * Feb 10, 2009            wkwock       Added functions and clean up.
 * Mar 31, 2009  2141      mduff        Fixed cancel save error.
 * Nov 23, 2010  6244      lbousaidi    cleared idTF box after search
 * Mar 16, 2011  6251      lbousaidi    activate [Enter] for idTF box
 * Jun 02, 2011  9150      mduff        xdat_flood_hours needs to be negative on
 *                                      the calendar.add
 * May 22, 2015  4501      skorolev     Removed old DB connection commands. Got
 *                                      rid of Vector.
 * Jan 26, 2016  5054      randerso     Made XdatDlg parented to display
 * Aug 04, 2016  5800      mduff        Fixed widget disposed errors, cleanup.
 * Mar 12, 2018  DCS18260  astrakovsky  Changed rows to be selected in one click
 *                                      and fixed GUI not updating.
 * May 01, 2018  7027      mduff        Change to only have a single instance so
 *                                      only one window can be open.
 * Dec 09, 2019  7992      bhurley      Allow editing values in 24 Hour COOP
 *                                      Precipitation and Sites Above Flood
 *                                      Stage displays
 * 
 * </pre>
 * 
 * @author lvenable
 * 
 */
public class XdatDlg extends CaveSWTDialog implements ITextDisplay {

    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(XdatDlg.class);

    private static final String NEWLINE = "\n";

    private static XdatDlg instance;

    /**
     * Control font.
     */
    private Font controlFont;

    /**
     * Version.
     */
    private static final String version = "TaskOrder 10";

    /**
     * Search data format.
     */
    private static final String ID_DATA_FORMAT = "%-8S %2s   %-4S %2S %1S %19S %13S % 6.2f   % 6.2f";

    /**
     * 24 Hour COOP Precipitation data format.
     */
    private static final String COOP_PRECIP_DATA_FORMAT = "%-8s %2s  %4s %2s %1s %19s %13s %6.2f";

    /**
     * Sites Above Flood Stage data format.
     */
    private static final String SITES_ABOVE_FLOOD_STAGE_FORMAT = " %-9S %S     %-4S  %S    %S    %S     %8S  %8.2f    %3d";

    /**
     * Regex for splitting columns in data lines.
     */
    private static final String SPLIT_REGEX = "[ ]++";

    /**
     * Enum for types of updates to data in the text area.
     */
    protected enum UpdateType {
        REJECTED_DATA,
        SITE_ABOVE_FLOOD_STAGE,
        SITES_TURNED_OFF,
        UNKNOWN_SITES,
        SEARCH_DATA,
        LIST_SELECTION,
        COOP_PRECIP,
        PRECIP_ACCUMULATION,
        GROUP_DATA
    }

    /**
     * The last type of data displayed.
     */
    private UpdateType lastUpdate = null;

    /**
     * Last selected ID for search and list selection refresh.
     */
    private String lastSelectedId = null;

    /**
     * Last hour and duration for precipitation refresh.
     */
    private int lastPrecipHour = 12;

    private int lastPrecipDuration = 24;

    /**
     * Last selected group for group refresh.
     */
    private String lastSelectedGroup = null;

    /**
     * Last line selected.
     */
    private int lastLine = -1;

    /**
     * Start and end lines of saved selection.
     */
    private int savedTopLine = -1;

    private int savedBottomLine = -1;

    /**
     * ID text control.
     */
    private Text idTF;

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

    public static final synchronized XdatDlg getInstance(Display display) {
        if (instance == null || !instance.isOpen()) {
            instance = new XdatDlg(display);
        }

        return instance;
    }

    /**
     * Constructor.
     * 
     * @param display
     * 
     */
    private XdatDlg(Display display) {
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
        Button editDataBtn = new Button(topComp, SWT.PUSH);
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

        Button hgRdo = new Button(selectPeGrp, SWT.RADIO);
        hgRdo.setText("HG");
        hgRdo.setSelection(true);
        peButtons.add(hgRdo);

        Button hpRdo = new Button(selectPeGrp, SWT.RADIO);
        hpRdo.setText("HP");
        peButtons.add(hpRdo);

        Button htRdo = new Button(selectPeGrp, SWT.RADIO);
        htRdo.setText("HT");
        peButtons.add(htRdo);

        Button qtRdo = new Button(selectPeGrp, SWT.RADIO);
        qtRdo.setText("QT");
        peButtons.add(qtRdo);

        Button pcRdo = new Button(selectPeGrp, SWT.RADIO);
        pcRdo.setText("PC");
        peButtons.add(pcRdo);

        Button ppRdo = new Button(selectPeGrp, SWT.RADIO);
        ppRdo.setText("PP");
        peButtons.add(ppRdo);

        Button taRdo = new Button(selectPeGrp, SWT.RADIO);
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
        startDateList = new List(peDateComp,
                SWT.BORDER | SWT.SINGLE | SWT.V_SCROLL);
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
        endDateList = new List(peDateComp,
                SWT.BORDER | SWT.SINGLE | SWT.V_SCROLL);
        endDateList.setLayoutData(gd);
        endDateList.setFont(controlFont);
        endDateList.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                validateStartEndTimeSelection();
            }
        });

        SimpleDateFormat dateFmt = new SimpleDateFormat("MM/dd/yyyy",
                Locale.US);
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
        textArea = new StyledText(parentComp,
                SWT.BORDER | SWT.MULTI | SWT.V_SCROLL | SWT.H_SCROLL);
        textArea.setFont(controlFont);
        textArea.setEditable(false);
        textArea.setLayoutData(gd);

        // select the line clicked
        textArea.addMouseListener(new MouseAdapter() {

            @Override
            public void mouseDown(MouseEvent e) {
                /*
                 * Select clicked line - start on mouse down to ensure immediate
                 * selection.
                 */
                selectLine();
            }

            @Override
            public void mouseUp(MouseEvent e) {
                // Select all lines highlighted while mouse was down.
                appendLineSelection();
            }

        });

        /*
         * Also do selection when keys are pressed to account for line changing
         * on some key presses.
         */
        textArea.addKeyListener(new KeyAdapter() {

            @Override
            public void keyPressed(KeyEvent e) {
                /*
                 * Select line with cursor - start on key down to ensure
                 * immediate selection.
                 */
                selectLine();
            }

            @Override
            public void keyReleased(KeyEvent e) {
                /*
                 * Repeat selection to prevent multiple lines being highlighted
                 * while key is down.
                 */
                selectLine();
            }

        });

    }

    /**
     * Select the current line only.
     */
    private void selectLine() {
        int line = textArea.getLineAtOffset(textArea.getCaretOffset());
        textArea.setSelection(textArea.getOffsetAtLine(line),
                getEndOfLine(line));
        lastLine = line;
    }

    /**
     * Select the area between the previous line selected and the current one
     * (inclusive).
     */
    private void appendLineSelection() {
        if (textArea.getLineCount() > lastLine && lastLine > -1) {
            int line = textArea.getLineAtOffset(textArea.getCaretOffset());
            int selectionStart;
            int selectionEnd;
            if (line > lastLine) {
                selectionStart = textArea.getOffsetAtLine(lastLine);
                selectionEnd = getEndOfLine(line);
            } else {
                selectionStart = textArea.getOffsetAtLine(line);
                selectionEnd = getEndOfLine(lastLine);
            }
            textArea.setSelection(selectionStart, selectionEnd);
        }
    }

    /**
     * Get the cursor offset of the end of the specified line in the text area.
     */
    private int getEndOfLine(int lineNum) {
        int lineEnd;
        if (textArea.getLineCount() > lineNum + 1) {
            lineEnd = textArea.getOffsetAtLine(lineNum + 1) - 1;
        } else {
            lineEnd = textArea.getCharCount() - 1;
        }
        return lineEnd;
    }

    /**
     * Save the current text area selection.
     */
    @Override
    public void saveSelection() {
        if (textArea.isTextSelected()) {
            savedBottomLine = getCurrentLineNumber();
            savedTopLine = savedBottomLine - (getSelectedText().size() - 1);
        } else {
            savedBottomLine = -1;
            savedTopLine = -1;
        }
    }

    /**
     * Restore the last saved text area selection.
     */
    @Override
    public void restoreSelection() {
        if (savedTopLine > -1 && savedBottomLine > -1) {
            if (savedBottomLine < getLineCount() - 1) {
                setSelection(getOffsetAtLine(savedTopLine),
                        getOffsetAtLine(savedBottomLine + 1) - 1);
            } else if (savedTopLine < getLineCount() - 1) {
                setSelection(getOffsetAtLine(savedTopLine),
                        textArea.getCharCount() - 1);
            } else {
                setSelection(0, textArea.getCharCount() - 1);
            }
        }
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

            for (String element : textArray) {
                textArea.append(element + NEWLINE);
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
     * Get the text on the display.
     * 
     * @return the text area content.
     */
    @Override
    public String getDisplayText() {
        return textArea.getText();
    }

    /**
     * Get the currently selected line.
     * 
     * @return the line number.
     */
    @Override
    public int getCurrentLineNumber() {
        return textArea.getLineAtOffset(textArea.getCaretOffset());
    }

    /**
     * Get the offset at line.
     * 
     * @return the offset.
     */
    @Override
    public int getOffsetAtLine(int line) {
        return textArea.getOffsetAtLine(line);
    }

    /**
     * Get the line indicated
     */
    @Override
    public String getLine(int line) {
        return textArea.getLine(line);
    }

    /**
     * Get the line count
     */
    @Override
    public int getLineCount() {
        return textArea.getLineCount();
    }

    /**
     * Set the top index
     * 
     * @param int
     *            the top index to set.
     */
    @Override
    public void setTopIndex(int index) {
        textArea.setTopIndex(index);
    }

    /**
     * Get the top index
     */
    @Override
    public int getTopIndex() {
        return textArea.getTopIndex();
    }

    /**
     * Set the text selection
     * 
     * @param point
     *            the selection point to set.
     */
    @Override
    public void setSelection(Point point) {
        textArea.setSelection(point);
    }

    /**
     * Set the text selection
     * 
     * @param int
     *            the selection coordinates to set.
     */
    @Override
    public void setSelection(int x, int y) {
        textArea.setSelection(x, y);
    }

    /**
     * Get the text selection
     */
    @Override
    public Point getSelection() {
        return textArea.getSelection();
    }

    /**
     * Get the last update
     * 
     * @return
     */
    @Override
    public UpdateType getLastUpdate() {
        return lastUpdate;
    }

    /**
     * Set the last update
     * 
     * @param lastUpdate
     */
    @Override
    public void setLastUpdate(UpdateType lastUpdate) {
        this.lastUpdate = lastUpdate;
    }

    /**
     * Refresh the contents of the text area, preserving selection and scroll
     * position.
     */
    @Override
    public void refreshTextArea() {
        if (lastUpdate != null) {
            int index = getTopIndex();
            saveSelection();
            switch (lastUpdate) {
            case REJECTED_DATA:
                displayRejectedData();
                break;
            case SITE_ABOVE_FLOOD_STAGE:
                displaySiteAboveFldStg();
                break;
            case SITES_TURNED_OFF:
                displaySitesTurnedOffData();
                break;
            case UNKNOWN_SITES:
                displayUnknownSitesData();
                break;
            case SEARCH_DATA:
                displaySearchData();
                break;
            case LIST_SELECTION:
                displayIdSelection(lastSelectedId);
                break;
            case COOP_PRECIP:
                displayCoopPrecip();
                break;
            case PRECIP_ACCUMULATION:
                displayPrecipAccumulation(lastPrecipHour, lastPrecipDuration);
                break;
            case GROUP_DATA:
                retrieveAndDisplayGroupData(lastSelectedGroup);
                break;
            }
            setTopIndex(index);
            restoreSelection();
        }
    }

    /**
     * Save the data (text on the display) to a file.
     */
    private void saveData() {

        /*
         * Check if there is text to be saved.
         */
        if (textArea.getText().trim().length() == 0) {
            MessageBox mb = new MessageBox(getShell(),
                    SWT.ICON_WARNING | SWT.OK);
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
        if (saveFile.exists()) {

            MessageBox mb = new MessageBox(getShell(),
                    SWT.ICON_WARNING | SWT.YES | SWT.NO);
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
            startDateStr = startDateList
                    .getItem(startDateList.getSelectionIndex());
        }

        if (endDateList.getSelectionIndex() == -1) {
            endDateStr = startDateList.getItem(0);
        } else {
            endDateStr = startDateList.getItem(endDateList.getSelectionIndex());
        }

        if (!validateStartEndTimeSelection()) {
            return;
        }

        String peStr = getSelectedPE();

        java.util.List<String[]> rejectedDataBuf = databaseMgr
                .getRejectedData(peStr, startDateStr, endDateStr);

        if (rejectedDataBuf == null) {
            textArea.setText("No rejected " + peStr
                    + " data found in the rejecteddata table.\n");
        } else {

            String formattedStr = formatRejectedData(rejectedDataBuf, peStr);
            textArea.setText(formattedStr);
        }
        setLastUpdate(UpdateType.REJECTED_DATA);
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
        strBld.append(String.format(headerFmt, "ID", "PE", "DUR", "TS", "E",
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

            strBld.append(String.format(dataFmt, strArray[0], strArray[1],
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
            if ((setToday != null) && !setToday.trim().isEmpty()) {
                sDate = startDate.parse(setToday);
            }
        } catch (ParseException e) {
            MessageBox mb = new MessageBox(getShell(),
                    SWT.ICON_WARNING | SWT.OK);
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
            String formattedStr = formatSiteAboveFldStageData(aboveFsBuf,
                    sDate);
            textArea.setText(formattedStr);
        }
        setLastUpdate(UpdateType.SITE_ABOVE_FLOOD_STAGE);
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

        /*
         * Add the Title text
         */
        Calendar cal = TimeUtil.newGmtCalendar(sDate);
        String floodHours = appsDefaults.getToken("xdat_flood_hours");
        if ((floodHours != null) && (floodHours.length() > 0)) {
            cal.add(Calendar.HOUR_OF_DAY, -1 * (Integer.parseInt(floodHours)));
        }

        strBld.append(String.format(titleFmt, "Site Above Flood Stage since",
                displayDate.format(new Date(cal.getTimeInMillis()))))
                .append(NEWLINE).append(NEWLINE);

        /*
         * Add the data header
         */
        strBld.append(String.format(headerFmt, "ID", "PE", "DUR", "TS", "E",
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

            strBld.append(String.format(SITES_ABOVE_FLOOD_STAGE_FORMAT,
                    dataArray[0], dataArray[1], dataArray[2], dataArray[3],
                    dataArray[4], dataArray[5], productID, rndVal, fsVal))
                    .append(NEWLINE);
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
        setLastUpdate(UpdateType.SITES_TURNED_OFF);
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
        strBld.append(String.format(headerFmt, "ID", "County", "State",
                "Description")).append(NEWLINE);

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

            strBld.append(String.format(dataFmt, dataArray[0], dataArray[1],
                    dataArray[2], dataArray[3])).append(NEWLINE);
        }

        return strBld.toString();
    }

    /**
     * Display the unknown sites data on the display.
     */
    private void displayUnknownSitesData() {
        String postFlag = appsDefaults.getToken("xdat_post_unk");

        if ("NONE".equalsIgnoreCase(postFlag)) {
            textArea.setText("Shefdecoder not posting unknown data.");
            setLastUpdate(UpdateType.UNKNOWN_SITES);
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
        setLastUpdate(UpdateType.UNKNOWN_SITES);
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
        strBld.append(String.format(titleFmt, "Unknown Station IDs"))
                .append("\n\n");

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

            strBld.append(String.format(dataFmt, dataArray[0], productID,
                    productTime)).append(NEWLINE);
        }

        return strBld.toString();
    }

    /**
     * Display the search data on the display.
     */
    private void displaySearchData() {

        String selectedId = idTF.getText().trim().toUpperCase();

        if (selectedId.trim().compareTo("") == 0) {
            MessageBox mb = new MessageBox(getShell(),
                    SWT.ICON_WARNING | SWT.OK);
            mb.setText("Search Warning");
            mb.setMessage("You need to enter an ID into the ID text field.");
            mb.open();
            return;
        }

        displayIdSelection(selectedId);

        setLastUpdate(UpdateType.SEARCH_DATA);
    }

    /**
     * Display the data associated with the selected ID.
     */
    @Override
    public void displayIdSelection(String selectedId) {

        if (selectedId != null) {

            // Get the PE type from main window
            String peType = getSelectedPE();

            java.util.List<String[]> dataList = databaseMgr.getListData(
                    selectedId, peType, getStartDate(), getEndDate());

            if (dataList == null) {
                String[] msg = new String[] { "No data available." };
                setDisplayText(msg);
                return;
            }

            /*
             * Format the data.
             */
            String locationDes = databaseMgr.getLocationDes(selectedId);

            if ("null".equals(locationDes)) {
                locationDes = "";
            }

            /*
             * Add 3 for the first 3 lines, which are header rows
             */
            String[] displayData = new String[dataList.size() + 3];
            String displayHeader = " ID      PE  DUR   TS E       OBSTIME           PRODUCT    VALUE   CHANGE";
            String dashLine = "---------------------------------------------------------------------------";
            displayData[0] = "\t\t" + selectedId + "\t  " + locationDes;
            displayData[1] = displayHeader;
            displayData[2] = dashLine;

            for (int i = 0; i < dataList.size(); i++) {
                String[] rowData = dataList.get(i);

                double dblVal = Double.valueOf(rowData[5]);
                double rndVal = (Math.round(dblVal * 100.0)) / 100.0;
                double change = 0;

                /*
                 * If this is not the last one then calculate the change
                 */
                if (i < (dataList.size() - 1)) {
                    String[] nextRowData = dataList.get(i + 1);
                    double nextDblVal = Double.valueOf(nextRowData[5]);
                    if ((dblVal != HydroConstants.MISSING_VALUE)
                            && (nextDblVal != HydroConstants.MISSING_VALUE)) {
                        change = dblVal - nextDblVal;
                        change = (Math.round(change * 100.0)) / 100.0;
                    }
                }

                String productID = rowData[0];
                if (productID == null) {
                    productID = "";
                }

                displayData[i + 3] = String.format(ID_DATA_FORMAT, selectedId,
                        peType, rowData[1], rowData[2], rowData[3], rowData[4],
                        productID, rndVal, change);
            }

            setDisplayText(displayData);
            lastSelectedId = selectedId;
            idTF.setText(selectedId);
        }

    }

    /**
     * Update the GUI with the new value and changes without reloading from DB.
     */
    @Override
    public void updateTextAreaValue(String newValue) {
        switch (getLastUpdate()) {
        case LIST_SELECTION:
            updateTextAreaListOrSearchValue(newValue);
            break;
        case SEARCH_DATA:
            updateTextAreaListOrSearchValue(newValue);
            break;
        case COOP_PRECIP:
            updateTextAreaPrecipOrFloodValue(newValue);
            break;
        case GROUP_DATA:
            break;
        case PRECIP_ACCUMULATION:
            break;
        case REJECTED_DATA:
            break;
        case SITES_TURNED_OFF:
            break;
        case SITE_ABOVE_FLOOD_STAGE:
            updateTextAreaPrecipOrFloodValue(newValue);
            break;
        case UNKNOWN_SITES:
            break;
        default:
            break;
        }
    }

    /**
     * Update value and changes in List or Search display
     */
    private void updateTextAreaListOrSearchValue(String newValue) {
        /*
         * Need to deal with lines above and below to adjust change Note: line
         * variable naming refers to the timestamp, so the "next" line appears
         * earlier in the text area.
         */
        int bottomLineNum = getCurrentLineNumber();
        int topLineNum = bottomLineNum - (getSelectedText().size() - 1);
        String bottomLine = getSelectedText().get(getSelectedText().size() - 1);
        String nextLine = "";
        String previousLine = "";
        if (topLineNum > 3) {
            // only get next line if non-header lines above exist
            nextLine = getLine(topLineNum - 1);
        }
        if (bottomLineNum < getLineCount() - 1) {
            // only get last line if lines below exist
            previousLine = getLine(bottomLineNum + 1);
        }

        // decode lines
        String[] bottomLineSplit = bottomLine.trim().split(SPLIT_REGEX);
        double bottomValue = Double.parseDouble(bottomLineSplit[8]);
        String[] nextLineSplit = null;
        double nextValue = 0;
        String[] previousLineSplit = null;
        double previousValue = 0;
        if (!nextLine.isEmpty()) {
            // selection is not top line - line above selection exists
            nextLineSplit = nextLine.trim().split(SPLIT_REGEX);
            nextValue = Double.parseDouble(nextLineSplit[8]);
        }
        if (!previousLine.isEmpty()) {
            // selection is not bottom line - line below selection exists
            previousLineSplit = previousLine.trim().split(SPLIT_REGEX);
            previousValue = Double.parseDouble(previousLineSplit[8]);
        }

        // decode changes
        double bottomChange = Double.parseDouble(bottomLineSplit[9]);
        double newCurrentValue = 0;
        double newMidChange = 0;
        double newBottomChange = 0;
        double newNextChange = 0;

        // update value and change for selection
        if (NumberUtils.isNumber(newValue)) {
            // if number, parse
            newCurrentValue = Double.parseDouble(newValue);
            if (previousLineSplit != null
                    && bottomValue != HydroConstants.MISSING_VALUE) {
                /*
                 * If old value was not missing, calculate change and add to old
                 * change
                 */
                newBottomChange = bottomChange
                        + (newCurrentValue - bottomValue);
            } else if (previousLineSplit != null
                    && previousValue != HydroConstants.MISSING_VALUE) {
                /*
                 * if old value was missing, calculate change from previous line
                 * if the value exists
                 */
                newBottomChange = newCurrentValue - previousValue;
            }
            // change remains 0 if no way to calculate or last line was selected
        } else {
            // interpret non-numerical values as missing
            newCurrentValue = HydroConstants.MISSING_VALUE;
            // current and next change should be 0 if value is missing
        }

        // update change for next line if next line exists
        if (nextLineSplit != null && nextValue != HydroConstants.MISSING_VALUE
                && newCurrentValue != HydroConstants.MISSING_VALUE) {
            newNextChange = nextValue - newCurrentValue;
        }
        // change remains 0 if no way to calculate

        // round value and changes
        newCurrentValue = (Math.round(newCurrentValue * 100.0)) / 100.0;
        newMidChange = (Math.round(newMidChange * 100.0)) / 100.0;
        newBottomChange = (Math.round(newBottomChange * 100.0)) / 100.0;
        newNextChange = (Math.round(newNextChange * 100.0)) / 100.0;

        // rebuild text contents
        StringBuilder builder = new StringBuilder("");
        for (String s : getSelectedText()) {
            builder.append(s);
            builder.append(NEWLINE);
        }
        String textContent = getDisplayText();
        String[] unchangedTextContents = textContent
                .split(nextLine + NEWLINE + builder.toString());
        builder = new StringBuilder("");

        if (unchangedTextContents.length == 2
                || unchangedTextContents.length == 1) {
            /*
             * If no duplicate lines, result will contain the surrounding text,
             * or just what was in front if this is the bottom line
             * 
             * Add unmodified text preceding the edited line and the one above
             * it (if applicable)
             */
            builder.append(unchangedTextContents[0]);

            // add next line with change affected by edited line
            if (nextLineSplit != null) {
                builder.append(formatIdDataLine(nextLineSplit, nextValue,
                        newNextChange));
            }
            builder.append(NEWLINE);

            /*
             * Add all edited lines except for the bottom one, which has a
             * different change
             */
            for (int ii = 0; ii < getSelectedText().size() - 1; ii++) {
                String[] lineSplit = getSelectedText().get(ii).trim()
                        .split(SPLIT_REGEX);
                builder.append(formatIdDataLine(lineSplit, newCurrentValue,
                        newMidChange));
                builder.append(NEWLINE);
            }

            // add the edited bottom line with new value and change
            builder.append(formatIdDataLine(bottomLineSplit, newCurrentValue,
                    newBottomChange));
            builder.append(NEWLINE);

            if (unchangedTextContents.length == 2) {
                /*
                 * If edited bottom line was not the last line, add the rest of
                 * the unmodified text
                 */
                builder.append(unchangedTextContents[1]);
            }

        } else if (unchangedTextContents.length > 2) {
            // duplicate lines exist, need to handle line by line

            // split text by lines
            unchangedTextContents = textContent.split(NEWLINE);
            // add lines above selection (and next line if applicable)
            for (int ii = 0; ii < topLineNum - 1; ii++) {
                builder.append(unchangedTextContents[ii]);
                builder.append(NEWLINE);
            }

            // add next line with change affected by edited line
            if (nextLineSplit != null) {
                builder.append(formatIdDataLine(nextLineSplit, nextValue,
                        newNextChange));
            } else {
                builder.append(unchangedTextContents[topLineNum - 1]);
            }
            builder.append(NEWLINE);

            /*
             * Add all edited lines except for the bottom one, which has a
             * different change
             */
            for (int ii = 0; ii < getSelectedText().size() - 1; ii++) {
                String[] lineSplit = getSelectedText().get(ii).trim()
                        .split(SPLIT_REGEX);
                builder.append(formatIdDataLine(lineSplit, newCurrentValue,
                        newMidChange));
                builder.append(NEWLINE);
            }

            // add the edited bottom line with new value and change
            builder.append(formatIdDataLine(bottomLineSplit, newCurrentValue,
                    newBottomChange));
            builder.append(NEWLINE);

            // add lines below selection
            for (int ii = bottomLineNum + 1; ii < getLineCount() - 1; ii++) {
                builder.append(unchangedTextContents[ii]);
                builder.append(NEWLINE);
            }

        }

        // update display and preserve scroll position and selection
        int index = getTopIndex();
        setDisplayText(builder.toString());
        setTopIndex(index);
        setSelection(getOffsetAtLine(topLineNum),
                getOffsetAtLine(bottomLineNum + 1) - 1);

    }

    /**
     * Update value in 24 Hour COOP Precipitation or Sites Above Flood Stage
     * display
     */
    private void updateTextAreaPrecipOrFloodValue(String newValue) {
        int bottomLineNum = getCurrentLineNumber();
        int topLineNum = bottomLineNum - (getSelectedText().size() - 1);

        // update value and change for selection
        double newCurrentValue = 0;
        if (NumberUtils.isNumber(newValue)) {
            // if number, parse
            newCurrentValue = Double.parseDouble(newValue);
        } else {
            // interpret non-numerical values as missing
            newCurrentValue = HydroConstants.MISSING_VALUE;
        }

        // round value
        newCurrentValue = (Math.round(newCurrentValue * 100.0)) / 100.0;

        // rebuild text contents
        StringBuilder builder = new StringBuilder("");
        for (String s : getSelectedText()) {
            builder.append(s);
            builder.append(NEWLINE);
        }
        String textContent = getDisplayText();
        String[] unchangedTextContents = textContent
                .split(NEWLINE + builder.toString());
        builder = new StringBuilder("");

        if (unchangedTextContents.length == 2
                || unchangedTextContents.length == 1) {
            /*
             * If no duplicate lines, result will contain the surrounding text,
             * or just what was in front if this is the bottom line
             * 
             * Add unmodified text preceding the edited line
             */
            builder.append(unchangedTextContents[0]);
            builder.append(NEWLINE);

            /*
             * Add all edited lines
             */
            for (int ii = 0; ii < getSelectedText().size(); ii++) {
                String[] lineSplit = getSelectedText().get(ii).trim()
                        .split(SPLIT_REGEX);
                builder.append(formatPrecipOrFloodDataLine(lineSplit,
                        newCurrentValue));
                builder.append(NEWLINE);
            }

            if (unchangedTextContents.length == 2) {
                /*
                 * If edited bottom line was not the last line, add the rest of
                 * the unmodified text
                 */
                builder.append(unchangedTextContents[1]);
            }

        } else if (unchangedTextContents.length > 2) {
            // duplicate lines exist, need to handle line by line

            // split text by lines
            unchangedTextContents = textContent.split(NEWLINE);
            // add lines above selection
            for (int ii = 0; ii < topLineNum; ii++) {
                builder.append(unchangedTextContents[ii]);
                builder.append(NEWLINE);
            }

            /*
             * Add all edited lines
             */
            for (int ii = 0; ii < getSelectedText().size(); ii++) {
                String[] lineSplit = getSelectedText().get(ii).trim()
                        .split(SPLIT_REGEX);
                builder.append(formatPrecipOrFloodDataLine(lineSplit,
                        newCurrentValue));
                builder.append(NEWLINE);
            }

            // add lines below selection
            for (int ii = bottomLineNum + 1; ii < getLineCount() - 1; ii++) {
                builder.append(unchangedTextContents[ii]);
                builder.append(NEWLINE);
            }
        }

        // update display and preserve scroll position and selection
        int index = getTopIndex();
        setDisplayText(builder.toString());
        setTopIndex(index);
        setSelection(getOffsetAtLine(topLineNum),
                getOffsetAtLine(bottomLineNum + 1) - 1);
    }

    /**
     * Format data line from selected ID display.
     */
    private String formatIdDataLine(String[] lineSplit, double value,
            double change) {
        if (lineSplit.length > 7) {
            return String.format(ID_DATA_FORMAT, lineSplit[0], lineSplit[1],
                    lineSplit[2], lineSplit[3], lineSplit[4],
                    lineSplit[5] + " " + lineSplit[6], lineSplit[7], value,
                    change);
        } else {
            return "";
        }
    }

    /**
     * Format data line for 24 Hour COOP Precipitation or Sites Above Flood
     * Stage display.
     */
    private String formatPrecipOrFloodDataLine(String[] lineSplit,
            double value) {
        if (lineSplit.length == 9 && lastUpdate == UpdateType.COOP_PRECIP) {
            return String.format(COOP_PRECIP_DATA_FORMAT, lineSplit[0],
                    lineSplit[1], lineSplit[2], lineSplit[3], lineSplit[4],
                    lineSplit[5] + " " + lineSplit[6], lineSplit[7], value);
        } else if (lineSplit.length == 10
                && lastUpdate == UpdateType.SITE_ABOVE_FLOOD_STAGE) {
            return String.format(SITES_ABOVE_FLOOD_STAGE_FORMAT, lineSplit[0],
                    lineSplit[1], lineSplit[2], lineSplit[3], lineSplit[4],
                    lineSplit[5] + " " + lineSplit[6], lineSplit[7], value,
                    Integer.parseInt(lineSplit[9]));
        } else {
            return "";
        }
    }

    /**
     * Display COOP Precipitation
     */
    @Override
    public void displayCoopPrecip() {
        String startDate = getEndDate();
        String header1 = "\t\t\t 24 Hour Precipitation Ending at "
                + getEndDate() + " 12Z";
        String header2 = "  ID     PE  DUR  TS E       OBSTIME        PRODUCT       VALUE";
        String dashLine = "-------------------------------------------------------------------";
        java.util.List<String[]> coopPrecipData = databaseMgr
                .getCoopPrecipData(startDate);

        if (coopPrecipData == null) {
            setDisplayText("No data available.");
        } else {
            String[] displayText = new String[coopPrecipData.size() + 3];
            displayText[0] = header1;
            displayText[1] = header2;
            displayText[2] = dashLine;

            int i = 3;
            for (String[] dataText : coopPrecipData) {
                String productID = dataText[5];
                if (productID == null) {
                    productID = "";
                }

                double value = HydroConstants.MISSING_VALUE;
                try {
                    value = Double.parseDouble(dataText[6]);
                } catch (NumberFormatException nfe) {
                    value = HydroConstants.MISSING_VALUE;
                    statusHandler.handle(Priority.ERROR,
                            "Fail to parse " + dataText[6] + ".");
                }
                displayText[i] = String.format(COOP_PRECIP_DATA_FORMAT,
                        dataText[0], "PP", dataText[1], dataText[2],
                        dataText[3], dataText[4], productID, value);
                i++;
            }
            setDisplayText(displayText);
        }
        setLastUpdate(UpdateType.COOP_PRECIP);
    }

    /**
     * Display Precipitation Accumulation
     */
    @Override
    public void displayPrecipAccumulation(int hour, int duration) {
        SimpleDateFormat obsDate = new SimpleDateFormat("yyyy-MM-dd",
                Locale.US);
        Calendar date = null;
        try {
            Date sDate = obsDate.parse(getEndDate());
            date = TimeUtil.newGmtCalendar(sDate);
        } catch (ParseException e) {
            date = TimeUtil.newGmtCalendar();
            statusHandler.handle(Priority.ERROR,
                    "Fail to parse " + getEndDate() + ".");
        }

        obsDate = new SimpleDateFormat("yyyy-MM-dd HH:00:00", Locale.US);
        obsDate.setTimeZone(TimeZone.getTimeZone("GMT"));

        String obsTimeStr = obsDate.format(date.getTime());
        String displayTime = obsTimeStr;
        Map<String, Double> precipLidAndValue = databaseMgr
                .getPrecipLidAndValue(obsTimeStr);

        date.add(Calendar.HOUR_OF_DAY, 0 - duration);
        obsTimeStr = obsDate.format(date.getTime());

        Map<String, Double> precipLidAndValue2 = databaseMgr
                .getPrecipLidAndValue(obsTimeStr);

        java.util.List<String> precipLidAndDiffBuf = new ArrayList<>(
                precipLidAndValue.size());

        if (precipLidAndValue.isEmpty()) {
            precipLidAndDiffBuf
                    .add("No " + duration + " Hour Precipitation data found.");
        } else {
            precipLidAndDiffBuf.add("\t\t\t " + duration
                    + " Hour PC Accumulation Ending at " + displayTime);
            precipLidAndDiffBuf.add("  ID     VALUE");
            precipLidAndDiffBuf.add("--------------");

            Iterator<String> iter = precipLidAndValue.keySet().iterator();
            String lid = null;
            java.util.List<XdatPcData> dataList = new ArrayList<>();
            while (iter.hasNext()) {
                lid = iter.next();

                double value = precipLidAndValue.get(lid);
                double value2 = -999;
                if (precipLidAndValue2.containsKey(lid)) {
                    value2 = precipLidAndValue2.get(lid);
                }
                // TODO determine how A1 handles missing data
                double valDiff = value - value2;
                int valint = (int) (valDiff * 100);
                valDiff = Math.floor(valint) / 100.0;

                if (valDiff < 0) {
                    valDiff = 0;
                }

                XdatPcData data = new XdatPcData(lid, valDiff);
                dataList.add(data);
            }

            Collections.sort(dataList);
            for (int i = 0; i < dataList.size(); i++) {
                XdatPcData data = dataList.get(i);

                precipLidAndDiffBuf.add(String.format("%-8s %4.2f",
                        data.getLid(), data.getValue()));
            }

        }
        setDisplayText(precipLidAndDiffBuf
                .toArray(new String[precipLidAndDiffBuf.size()]));
        lastPrecipHour = hour;
        lastPrecipDuration = duration;
        setLastUpdate(UpdateType.PRECIP_ACCUMULATION);
    }

    /**
     * Retrieve and display the group data.
     */
    @Override
    public void retrieveAndDisplayGroupData(String selectedGroup) {

        if (selectedGroup != null) {

            String groupsDir = AppsDefaults.getInstance()
                    .getToken("xdat_groups_dir");

            File file = new File(groupsDir + File.separator + selectedGroup);

            ReadIDsList idsList = new ReadIDsList(file);
            java.util.List<String[]> idList = idsList.getIDsList();

            StringBuilder strBld = new StringBuilder();
            for (int i = 0; i < idList.size(); i++) {
                java.util.List<String[]> results = databaseMgr.getGroupData(
                        idList.get(i), getStartDate(), getEndDate());
                if (results == null) {
                    return;
                }

                // Format the group data and display the data on the screen.
                strBld.append(formatGroupData(results));
                strBld.append("\n\n");
            }
            setDisplayText(strBld.toString());
            lastSelectedGroup = selectedGroup;
            setLastUpdate(UpdateType.GROUP_DATA);
        }

    }

    /**
     * Format the group data.
     * 
     * @param results
     *            Array of data to be formatted for the display.
     * @return StringBuilder class containing the formatted data.
     */
    private StringBuilder formatGroupData(java.util.List<String[]> results) {

        StringBuilder strBld = new StringBuilder();

        boolean dataLine = false;

        String idDesFmt = "\t\t\t %-10S  %S";

        String dataLineFmt = "%-9S  %2S  %4S %2S  %1S   %19S   %12S %6S   %7S";

        String hdr = "  ID       PE  DUR  TS  E       OBSTIME               PRODUCT   VALUE    CHANGE";
        String dashLine = "-------------------------------------------------------------------------------";

        for (String[] rowData : results) {

            if (rowData.length == 2) {

                if (dataLine) {
                    dataLine = false;
                    strBld.append(hdr).append("\n");
                }

                strBld.append(String.format(idDesFmt, rowData[0], rowData[1]))
                        .append("\n\n");
            } else {
                if (!dataLine) {
                    strBld.append(hdr).append("\n");
                    strBld.append(dashLine).append("\n");
                }

                dataLine = true;

                strBld.append(String.format(dataLineFmt, rowData[0], rowData[1],
                        rowData[2], rowData[3], rowData[4], rowData[5],
                        rowData[6], rowData[7], rowData[8])).append("\n");
            }
        }

        return strBld;
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
        mb.setMessage(
                "The selected end time is before the selected start time.");
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
            if (peBtn.getSelection()) {
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

        PrecipDataDlg precipDlg = new PrecipDataDlg(shell, this);
        precipDlg.open();
    }

    /**
     * Display the group data dialog.
     */
    private void displayGroupDataDlg() {
        GroupDataDlg groupDlg = new GroupDataDlg(shell, this);
        groupDlg.open();
        groupDlg = null;
    }

    /**
     * Print the formatted text on the display.
     */
    private void printDisplayText() {

        final String text = textArea.getText();

        if (text.trim().length() == 0) {
            MessageBox mb = new MessageBox(getShell(),
                    SWT.ICON_WARNING | SWT.OK);
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
