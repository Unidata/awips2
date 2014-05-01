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
package com.raytheon.viz.aviation.editor;

import java.util.ArrayList;
import java.util.HashMap;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.ExtendedModifyEvent;
import org.eclipse.swt.custom.ExtendedModifyListener;
import org.eclipse.swt.custom.ST;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.custom.VerifyKeyListener;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.VerifyEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Cursor;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Caret;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Text;

import com.raytheon.viz.aviation.editor.TafViewerEditorDlg.TafSettings;
import com.raytheon.viz.aviation.model.ForecastModel;
import com.raytheon.viz.aviation.resource.ResourceConfigMgr;
import com.raytheon.viz.aviation.resource.ResourceConfigMgr.ResourceTag;

/**
 * EditorTafTabComp class creates the TAF tab composite for the TAF editor.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 28 FEB 2008  938         lvenable    Initial creation.
 * 4/2/2008     934         grichard    Added setters for fields.
 * 6/16/2008    937         grichard    Improved viewer/editor interaction.
 * 8/20/2009    2839        grichard    Check for syntax performed before sending TAF.
 * 9/28/2010    2846        rferrel     Now use resources to determine editor
 *                                      text height and width.
 * 12/9/2010    7380        rferrel     Adjust text size to be more like AWIPS I.
 * 1/17/2011    7782        rferrel     Added qcSkipCheck to mimic A1.
 * 3/18/2011    7888        rferrel     Added getLargeTF method. 
 * 02/19/2014   16980       zhao        added getter and setter for the Alt flag
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class EditorTafTabComp extends Composite {

    /**
     * WMO ID label.
     */
    private Label wmoIdLbl;

    /**
     * WMO Site ID label.
     */
    private Label wmoSiteIdLbl;

    /**
     * Large text control.
     */
    private Text largeTF;

    /**
     * Small text control.
     */
    private Text smallTF;

    /**
     * Routine radio button.
     */
    private Button rtnRdo;

    /**
     * Amended radio button.
     */
    private Button amdRdo;

    /**
     * Routine Delay radio button.
     */
    private Button rtdRdo;

    /**
     * Correction radio button.
     */
    private Button corRdo;

    /**
     * Clear button.
     */
    private Button clearBtn;

    /**
     * TAF editor text control.
     */
    private StyledText tafEditorTxt;

    /**
     * Editor cursor.
     */
    private Cursor textEditorCursor;

    /**
     * Caret image.
     */
    private Image caretImage;

    /**
     * The callback to use to open the Taf Viewer/Editor.
     */
    private ITafSettable tveDlg;

    /**
     * Callback when making cut, copy, paste, undo, and redo edits.
     */
    private IEditActions tveEditCallback;

    /**
     * Popup menu for cut, copy, paste, undo, and redo.
     */
    private Menu popupMenu;

    private boolean ctrl = false;

    private boolean shft = false;

    private boolean alt = false;

    private boolean altPlusKey = false;

    private ArrayList<HashMap<String, Object>> undoStack;

    private ArrayList<HashMap<String, Object>> redoStack;

    private final int UNDO_STACK_SIZE = 10;

    private boolean modifyFlag = true;

    private boolean syntaxChecked = false;

    private boolean qcSkipCheck = false;

    private int errorLevel;

    private boolean tafSent;

    // Enumeration for amendment type of report
    public static enum AmendmentType {
        RTN, AMD, RTD, COR
    };

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent composite.
     */
    public EditorTafTabComp(Composite parent, ITafSettable tveDlg,
            IEditActions tveEditCb) {
        super(parent, SWT.NONE);

        this.tveDlg = tveDlg;
        this.tveEditCallback = tveEditCb;

        init();
    }

    /**
     * Initialize the composite.
     */
    private void init() {

        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        GridLayout gl = new GridLayout(1, false);
        gl.verticalSpacing = 1;
        gl.marginHeight = 1;
        this.setLayout(gl);
        this.setLayoutData(gd);

        initializeComponents();

        this.pack();

        this.addDisposeListener(new DisposeListener() {
            @Override
            public void widgetDisposed(DisposeEvent e) {
                textEditorCursor.dispose();
                caretImage.dispose();
            }
        });
    }

    /**
     * Initialize the controls on the composite.
     */
    private void initializeComponents() {

        ResourceConfigMgr configMgr = ResourceConfigMgr.getInstance();

        configMgr.setDefaultColors(this);

        createTopControls(configMgr);
        createTafEditorTextControl(configMgr);
    }

    /**
     * Create the controls at the top of the display.
     */
    private void createTopControls(ResourceConfigMgr configMgr) {

        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite controlsComp = new Composite(this, SWT.NONE);
        GridLayout gl = new GridLayout(9, false);
        controlsComp.setLayout(gl);
        controlsComp.setLayoutData(gd);
        configMgr.setDefaultColors(controlsComp);

        gd = new GridData(65, SWT.DEFAULT);
        wmoIdLbl = new Label(controlsComp, SWT.NONE);
        configMgr.setDefaultFontAndColors(wmoIdLbl, "WWWWWW", gd);
        wmoIdLbl.setText("");

        gd = new GridData(60, SWT.DEFAULT);
        wmoSiteIdLbl = new Label(controlsComp, SWT.NONE);
        configMgr.setDefaultFontAndColors(wmoSiteIdLbl, "WWWW", gd);
        wmoSiteIdLbl.setText("");

        gd = new GridData(80, SWT.DEFAULT);
        largeTF = new Text(controlsComp, SWT.BORDER);
        configMgr.setDefaultFontAndColors(largeTF, "000000", gd);
        largeTF.setText("");

        gd = new GridData(40, SWT.DEFAULT);
        smallTF = new Text(controlsComp, SWT.BORDER);
        configMgr.setDefaultFontAndColors(smallTF, "WWW", gd);
        smallTF.setText("");

        rtnRdo = new Button(controlsComp, SWT.RADIO);
        rtnRdo.setText("Rtn");
        rtnRdo.setSelection(true);
        configMgr.setDefaultFontAndColors(rtnRdo);
        rtnRdo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                smallTF.setText("");
                amendTaf(AmendmentType.RTN);
            }
        });

        amdRdo = new Button(controlsComp, SWT.RADIO);
        amdRdo.setText("Amd");
        configMgr.setDefaultFontAndColors(amdRdo);
        amdRdo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                smallTF.setText("AAX");
                amendTaf(AmendmentType.AMD);
            }
        });

        rtdRdo = new Button(controlsComp, SWT.RADIO);
        rtdRdo.setText("Rtd");
        configMgr.setDefaultFontAndColors(rtdRdo);
        rtdRdo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                smallTF.setText("RRX");
                amendTaf(AmendmentType.RTD);
            }
        });

        corRdo = new Button(controlsComp, SWT.RADIO);
        corRdo.setText("Cor");
        configMgr.setDefaultFontAndColors(corRdo);
        corRdo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                smallTF.setText("CCX");
                amendTaf(AmendmentType.COR);
            }
        });

        gd = new GridData(SWT.RIGHT, SWT.DEFAULT, true, false);
        gd.widthHint = 90;
        clearBtn = new Button(controlsComp, SWT.PUSH);
        configMgr.setDefaultFontAndColors(clearBtn, "Clear", gd);
        clearBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                tveDlg.updateSettings(TafSettings.CLEAR, null);
                tveDlg.showDialog();
            }
        });
    }

    /**
     * Create the TAF editor text control.
     */
    private void createTafEditorTextControl(ResourceConfigMgr configMgr) {
        GridLayout gl = new GridLayout(1, false);
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        Composite editorTextComp = new Composite(this, SWT.NONE);
        editorTextComp.setLayout(gl);
        editorTextComp.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true,
                true));

        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);

        gd.heightHint = configMgr.getDataAsInt(ResourceTag.TextEditorHeight);
        gd.widthHint = configMgr.getDataAsInt(ResourceTag.TextEditorWidth);
        tafEditorTxt = new StyledText(editorTextComp, SWT.BORDER | SWT.MULTI
                | SWT.V_SCROLL | SWT.H_SCROLL);
        tafEditorTxt.setEditable(true);
        tafEditorTxt.setLayoutData(gd);
        configMgr.setTextEditorFontAndColors(tafEditorTxt);

        undoStack = new ArrayList<HashMap<String, Object>>(UNDO_STACK_SIZE);
        redoStack = new ArrayList<HashMap<String, Object>>(UNDO_STACK_SIZE);

        // Check if word wrap is turned on.
        String wrapStr = configMgr.getDataAsString(ResourceTag.Wrap);

        if (wrapStr.compareTo("word") == 0) {
            tafEditorTxt.setWordWrap(true);
        } else {
            tafEditorTxt.setWordWrap(false);
        }

        String cursorStr = configMgr
                .getResourceAsString(ResourceTag.TextEditorCursor);
        int cursorInt = configMgr.getCursorAsInt(cursorStr);
        textEditorCursor = new Cursor(this.getDisplay(), cursorInt);
        tafEditorTxt.setCursor(textEditorCursor);

        int insertWidth = configMgr
                .getResourceAsInt(ResourceTag.TextEditorInsWidth);

        Caret caret = new Caret(tafEditorTxt, SWT.NONE);
        Point size = tafEditorTxt.getCaret().getSize();
        createCaretImage(insertWidth, size.y);
        caret.setImage(caretImage);
        tafEditorTxt.setCaret(caret);

        /*
         * NOTE:
         * 
         * The following code sets the TOGGLE_OVERWRITE for the text editor in
         * the editor tabs. In the TafViewerEditorDlg, after the controls are
         * initialized it is also set (if insert is false). Removing this code
         * or the code in the TafViewerEditorDlg will cause the text editor
         * control to not insert/overwrite properly. I do not understand why
         * this is but that is why I am documenting this. This may be fixed in
         * the future. --- L. Venable
         */
        if (configMgr.getResourceAsBoolean(ResourceTag.Insert) == false) {
            tafEditorTxt.invokeAction(ST.TOGGLE_OVERWRITE);
        }

        tafEditorTxt.addMouseListener(new MouseAdapter() {

            @Override
            public void mouseDown(MouseEvent e) {
                if (e.button == 3) {
                    popupMenu.setVisible(true);
                }
            }

        });

        tafEditorTxt.addKeyListener(new KeyAdapter() {
            @Override
            public void keyPressed(KeyEvent e) {
                if (!altPlusKey) {
                    if (alt) {
                        altPlusKey = true;
                    }
                }

                if (e.keyCode == SWT.ALT) {
                    alt = true;
                }

                if (e.keyCode == SWT.CTRL) {
                    ctrl = true;
                }

                if (e.keyCode == SWT.SHIFT) {
                    shft = true;
                }

                // Ctrl+Up Arrow
                if (ctrl && !shft && e.keyCode == SWT.ARROW_UP) {
                    int initialIndex = tafEditorTxt.getCaretOffset();
                    int currentLine = tafEditorTxt
                            .getLineAtOffset(initialIndex);

                    if (currentLine == 0) {
                        // Do nothing
                    } else if (tafEditorTxt.getLine(currentLine).trim()
                            .startsWith("TAF")) {
                        currentLine--;

                        while (currentLine > 0
                                && !tafEditorTxt.getLine(currentLine).trim()
                                        .endsWith("=")) {
                            currentLine--;
                        }

                        while (currentLine > 0
                                && !tafEditorTxt.getLine(currentLine).trim()
                                        .startsWith("TAF")) {
                            currentLine--;
                        }
                    } else {
                        currentLine--;

                        while (currentLine > 0
                                && !tafEditorTxt.getLine(currentLine).trim()
                                        .startsWith("TAF")) {
                            currentLine--;
                        }
                    }

                    int finalIndex = tafEditorTxt.getOffsetAtLine(currentLine);
                    tafEditorTxt.setCaretOffset(finalIndex);
                    tafEditorTxt.setTopIndex(currentLine);
                }

                // Ctrl+Shift+Up Arrow
                if (shft && ctrl && e.keyCode == SWT.ARROW_UP) {
                    int initialIndex = tafEditorTxt.getCaretOffset();
                    int currentLine = tafEditorTxt
                            .getLineAtOffset(initialIndex);
                    initialIndex = tafEditorTxt.getSelection().y;

                    if (currentLine == 0) {
                        // Do nothing
                    } else if (tafEditorTxt.getLine(currentLine).trim()
                            .startsWith("TAF")) {
                        currentLine--;

                        while (currentLine > 0
                                && !tafEditorTxt.getLine(currentLine).trim()
                                        .endsWith("=")) {
                            currentLine--;
                        }

                        while (currentLine > 0
                                && !tafEditorTxt.getLine(currentLine).trim()
                                        .startsWith("TAF")) {
                            currentLine--;
                        }
                    } else {
                        currentLine--;

                        while (currentLine > 0
                                && !tafEditorTxt.getLine(currentLine).trim()
                                        .startsWith("TAF")) {
                            currentLine--;
                        }
                    }

                    int finalIndex = tafEditorTxt.getOffsetAtLine(currentLine);
                    tafEditorTxt.setCaretOffset(finalIndex);
                    tafEditorTxt.setSelection(initialIndex, finalIndex);
                }

                // Ctrl+Down Arrow
                if (!shft && ctrl && e.keyCode == SWT.ARROW_DOWN) {
                    int initialIndex = tafEditorTxt.getCaretOffset();
                    int currentLine = tafEditorTxt
                            .getLineAtOffset(initialIndex);

                    if (currentLine == (tafEditorTxt.getLineCount() - 1)) {
                        // Do nothing
                    } else {
                        if (tafEditorTxt.getLine(currentLine).endsWith("=")) {
                            currentLine++;
                        }

                        while (currentLine < (tafEditorTxt.getLineCount() - 1)
                                && !tafEditorTxt.getLine(currentLine).endsWith(
                                        "=")) {
                            currentLine++;
                        }

                        int finalIndex;

                        if (currentLine == (tafEditorTxt.getLineCount() - 1)) {
                            finalIndex = tafEditorTxt.getCharCount();
                        } else {
                            currentLine++;
                            finalIndex = tafEditorTxt
                                    .getOffsetAtLine(currentLine) - 1;
                        }

                        tafEditorTxt.setCaretOffset(finalIndex);

                        // Scroll the visible area if necessary
                        int lineHeight = tafEditorTxt.getLineHeight();
                        int numLines = Math.round(tafEditorTxt.getBounds().height
                                / lineHeight);
                        int topLine = tafEditorTxt.getTopIndex();

                        if ((currentLine - topLine) > numLines) {
                            if (currentLine == (tafEditorTxt.getLineCount() - 1)) {
                                currentLine++;
                            }

                            tafEditorTxt
                                    .setTopIndex(currentLine - numLines + 1);
                        }

                    }
                }

                // Ctrl+Shift+Down Arrow
                if (shft && ctrl && e.keyCode == SWT.ARROW_DOWN) {
                    int initialIndex = tafEditorTxt.getCaretOffset();
                    int currentLine = tafEditorTxt
                            .getLineAtOffset(initialIndex);
                    initialIndex = tafEditorTxt.getSelection().x;

                    if (currentLine == (tafEditorTxt.getLineCount() - 1)) {
                        // Do nothing
                    } else {
                        if (tafEditorTxt.getLine(currentLine).endsWith("=")) {
                            currentLine++;
                        }

                        while (currentLine < (tafEditorTxt.getLineCount() - 1)
                                && !tafEditorTxt.getLine(currentLine).endsWith(
                                        "=")) {
                            currentLine++;
                        }

                        int finalIndex;

                        if (currentLine == (tafEditorTxt.getLineCount() - 1)) {
                            finalIndex = tafEditorTxt.getCharCount();
                        } else {
                            currentLine++;
                            finalIndex = tafEditorTxt
                                    .getOffsetAtLine(currentLine) - 1;
                        }

                        tafEditorTxt.setCaretOffset(finalIndex);
                        tafEditorTxt.setSelection(initialIndex, finalIndex);
                    }
                }

                // Ctrl+u
                if (ctrl && e.keyCode == 'u') {
                    undo();
                }

                // Ctrl+r
                if (ctrl && e.keyCode == 'r') {
                    redo();
                }

                // Ctrl+b
                if (ctrl && e.keyCode == 'b') {
                    int idx = tafEditorTxt.getCaretOffset();

                    if (idx > 0) {
                        idx--;
                        tafEditorTxt.setCaretOffset(idx);
                    }
                }

                // Ctrl+f
                if (ctrl && e.keyCode == 'f') {
                    int idx = tafEditorTxt.getCaretOffset();

                    if (idx < tafEditorTxt.getCharCount()) {
                        idx++;
                        tafEditorTxt.setCaretOffset(idx);
                    }
                }

                // Ctrl+p
                if (ctrl && e.keyCode == 'p') {
                    tafEditorTxt.invokeAction(SWT.ARROW_UP);
                }

                // Ctrl+n
                if (ctrl && e.keyCode == 'n') {
                    tafEditorTxt.invokeAction(SWT.ARROW_DOWN);
                }

                // Ctrl+e
                if (ctrl && e.keyCode == 'e') {
                    int idx = tafEditorTxt.getCaretOffset();
                    int line = tafEditorTxt.getLineAtOffset(idx);

                    if (line < (tafEditorTxt.getLineCount() - 1)) {
                        line++;
                        idx = tafEditorTxt.getOffsetAtLine(line) - 1;
                    } else {
                        idx = tafEditorTxt.getCharCount();
                    }

                    tafEditorTxt.setCaretOffset(idx);
                }

                // Ctrl+/
                if (ctrl && e.keyCode == '/') {
                    tafEditorTxt.setSelection(0, tafEditorTxt.getCharCount());
                    tafEditorTxt.setCaretOffset(tafEditorTxt.getCharCount());
                }

                // Ctrl+\
                if (ctrl && e.keyCode == '\\') {
                    tafEditorTxt.setSelection(tafEditorTxt.getCaretOffset());
                }

                // Ctrl+h
                if (ctrl && e.keyCode == 'h') {
                    tafEditorTxt.invokeAction(SWT.BS);
                }

                // Ctrl+d
                if (ctrl && e.keyCode == 'd') {
                    tafEditorTxt.invokeAction(SWT.DEL);
                }

                // Ctrl+k
                if (ctrl && e.keyCode == 'k') {
                    int idx = tafEditorTxt.getCaretOffset();
                    int line = tafEditorTxt.getLineAtOffset(idx);

                    if (line < (tafEditorTxt.getLineCount() - 1)) {
                        line++;
                        int idx2 = tafEditorTxt.getOffsetAtLine(line) - 1;
                        tafEditorTxt.setSelection(idx, idx2);
                        tafEditorTxt.invokeAction(SWT.DEL);
                    } else {
                        tafEditorTxt.setSelection(idx,
                                tafEditorTxt.getCharCount());
                        tafEditorTxt.invokeAction(SWT.DEL);
                    }
                }

                // Ctrl+t
                if (ctrl && e.keyCode == 't') {
                    int idx = tafEditorTxt.getCaretOffset();
                    String s = tafEditorTxt.getText();
                    String s1 = s.substring(0, idx);
                    String c1 = s1.substring(s1.length() - 1);
                    String s2 = s.substring(idx);
                    String c2 = s2.substring(0, 1);
                    tafEditorTxt.replaceTextRange(idx - 1, 1, c2);
                    tafEditorTxt.replaceTextRange(idx, 1, c1);
                }

                // Ctrl+o
                if (ctrl && e.keyCode == 'o') {
                    int idx = tafEditorTxt.getCaretOffset();
                    tafEditorTxt.replaceTextRange(idx, 0, "\n");
                }

                // Alt+b
                if (alt && e.keyCode == 'b') {
                    int idx = tafEditorTxt.getCaretOffset();
                    String s = tafEditorTxt.getText();
                    String c = s.substring(idx, idx + 1);

                    while (c.trim().length() > 0) {
                        idx--;
                        c = s.substring(idx, idx + 1);
                    }

                    while (c.trim().length() == 0) {
                        idx--;
                        c = s.substring(idx, idx + 1);
                    }

                    while (c.trim().length() > 0) {
                        idx--;
                        c = s.substring(idx, idx + 1);
                    }

                    tafEditorTxt.setCaretOffset(idx + 1);
                }

                // Alt+d
                if (alt && e.keyCode == 'd') {
                    int count = tafEditorTxt.getCharCount();
                    int idx1 = tafEditorTxt.getCaretOffset();
                    int idx = idx1;

                    if (idx == count) {
                        return;
                    }

                    String s = tafEditorTxt.getText();
                    String c = s.substring(idx, idx + 1).trim();

                    if (c.length() == 0) {
                        while (c.length() == 0 && idx < count) {
                            idx++;

                            if (idx == count) {
                                break;
                            }

                            c = s.substring(idx, idx + 1).trim();
                        }

                        while (c.length() > 0 && idx < count) {
                            idx++;

                            if (idx == count) {
                                break;
                            }

                            c = s.substring(idx, idx + 1).trim();
                        }
                    } else {
                        while (c.length() > 0 && idx < count) {
                            idx++;

                            if (idx == count) {
                                break;
                            }

                            c = s.substring(idx, idx + 1).trim();
                        }

                        while (c.length() == 0 && idx < count) {
                            idx++;

                            if (idx == count) {
                                break;
                            }

                            c = s.substring(idx, idx + 1).trim();
                        }
                    }

                    tafEditorTxt.replaceTextRange(idx1, idx - idx1, "");
                }

                // Alt+Del
                if (alt && e.keyCode == SWT.DEL) {
                    int idx1 = tafEditorTxt.getCaretOffset();

                    if (idx1 == 0) {
                        return;
                    }

                    int idx = idx1 - 1;
                    String s = tafEditorTxt.getText();
                    String c = s.substring(idx, idx + 1).trim();

                    if (c.length() == 0) {
                        while (c.length() == 0 && idx > 0) {
                            idx--;

                            if (idx == 0) {
                                break;
                            }

                            c = s.substring(idx, idx + 1).trim();
                        }

                        while (c.length() > 0 && idx > 0) {
                            idx--;

                            if (idx == 0) {
                                break;
                            }

                            c = s.substring(idx, idx + 1).trim();
                        }
                    } else {
                        while (c.length() > 0 && idx > 0) {
                            idx--;

                            if (idx == 0) {
                                break;
                            }

                            c = s.substring(idx, idx + 1).trim();
                        }

                        while (c.length() == 0 && idx > 0) {
                            idx--;

                            if (idx == 0) {
                                break;
                            }

                            c = s.substring(idx, idx + 1).trim();
                        }
                    }

                    if (idx > 0) {
                        idx++;
                    }

                    tafEditorTxt.replaceTextRange(idx, idx1 - idx, "");
                }
            }

            @Override
            public void keyReleased(KeyEvent e) {
                if (e.keyCode == SWT.ALT) {
                    alt = false;

                    // Alt (solo)
                    if (!altPlusKey) {
                        int idx = tafEditorTxt.getCaretOffset();
                        String s = tafEditorTxt.getText();
                        String c = s.substring(idx, idx + 1);

                        while (c.trim().length() == 0) {
                            idx++;
                            c = s.substring(idx, idx + 1);
                        }

                        while (c.trim().length() > 0) {
                            idx++;
                            c = s.substring(idx, idx + 1);
                        }

                        while (c.trim().length() == 0) {
                            idx++;
                            c = s.substring(idx, idx + 1);
                        }

                        tafEditorTxt.setCaretOffset(idx);
                    }

                    altPlusKey = false;
                }

                if (e.keyCode == SWT.CTRL) {
                    ctrl = false;
                }

                if (e.keyCode == SWT.SHIFT) {
                    shft = false;
                }

                if (e.keyCode == SWT.INSERT) {
                    e.doit = false;
                    tveDlg.updateInsert(true);
                }
            }
        });

        tafEditorTxt.addVerifyKeyListener(new VerifyKeyListener() {
            @Override
            public void verifyKey(VerifyEvent ve) {
                if (ve.keyCode == SWT.INSERT) {
                    ve.doit = false;
                }
            }
        });

        tafEditorTxt.addExtendedModifyListener(new ExtendedModifyListener() {
            @Override
            public void modifyText(ExtendedModifyEvent e) {
                // Something changed, we need to check syntax again.
                syntaxChecked = false;
                qcSkipCheck = false;

                // Check the modifyFlag, it will be set to false if we're
                // currently in the process of an undo or redo action, it should
                // be true otherwise.
                if (modifyFlag) {
                    int start = e.start;
                    int length = e.length;
                    String replacedText = e.replacedText;
                    HashMap<String, Object> undoData = new HashMap<String, Object>();
                    undoData.put("start", start);
                    undoData.put("length", length);
                    undoData.put("replacedText", replacedText);

                    if (undoStack.size() == UNDO_STACK_SIZE) {
                        undoStack.remove(0);
                    }

                    undoStack.add(undoData);
                    redoStack.clear();
                }
            }
        });

        createEditorPopupMenu(editorTextComp);
    }

    private void createEditorPopupMenu(Composite editorTextComp) {
        popupMenu = new Menu(editorTextComp);

        MenuItem cutMI = new MenuItem(popupMenu, SWT.NONE);
        cutMI.setText("Cut");
        cutMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                tveEditCallback.cutText();
            }
        });

        MenuItem copyMI = new MenuItem(popupMenu, SWT.NONE);
        copyMI.setText("Copy");
        copyMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                tveEditCallback.copyText();
            }
        });

        MenuItem pasteMI = new MenuItem(popupMenu, SWT.NONE);
        pasteMI.setText("Paste");
        pasteMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                tveEditCallback.pasteText();
            }
        });

        MenuItem undoMI = new MenuItem(popupMenu, SWT.NONE);
        undoMI.setText("Undo");
        undoMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                tveEditCallback.undoText();
            }
        });

        MenuItem redoMI = new MenuItem(popupMenu, SWT.NONE);
        redoMI.setText("Redo");
        redoMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                tveEditCallback.redoText();
            }
        });

        editorTextComp.setMenu(popupMenu);
    }

    private void createCaretImage(int imgWidth, int imgHeight) {
        ResourceConfigMgr configMgr = ResourceConfigMgr.getInstance();
        RGB insertRGB = configMgr.getInsertBackgroundRgb();

        caretImage = new Image(this.getParent().getDisplay(), imgWidth,
                imgHeight);

        GC gc = new GC(caretImage);
        drawImage(gc, imgWidth, imgHeight, insertRGB);

        gc.dispose();
    }

    private void drawImage(GC gc, int imgWidth, int imgHeight, RGB imageRGB) {
        RGB backRGB = tafEditorTxt.getBackground().getRGB();

        int red = backRGB.red ^ imageRGB.red;
        int green = backRGB.green ^ imageRGB.green;
        int blue = backRGB.blue ^ imageRGB.blue;

        // Draw a solid rectangle.
        Color c = new Color(this.getParent().getDisplay(), red, green, blue);
        gc.setBackground(c);
        gc.fillRectangle(0, 0, imgWidth, imgHeight);
        c.dispose();
    }

    /**
     * Get the TAF editor text control.
     * 
     * @return The TAF editor text control.
     */
    public StyledText getTextEditorControl() {
        return tafEditorTxt;
    }

    /**
     * Get routine radio button selection.
     * 
     * @return the selection predicate
     */
    public boolean isRtnRdoSelected() {
        return rtnRdo.getSelection();
    }

    /**
     * Set routine radio button selection.
     * 
     * @param -- the selection predicate
     */
    public void setRtnRdo(boolean select) {
        rtnRdo.setSelection(select);
    }

    /**
     * Get amended radio button selection.
     * 
     * @return the selection predicate
     */
    public boolean isAmdRdoSelected() {
        return amdRdo.getSelection();
    }

    /**
     * set amended radio button selection.
     * 
     * @param -- the selection predicate
     */
    public void setAmdRdo(boolean select) {
        amdRdo.setSelection(select);
    }

    /**
     * Get routine delay radio button selection.
     * 
     * @return the selection predicate
     */
    public boolean isRtdRdoSelected() {
        return rtdRdo.getSelection();
    }

    /**
     * Set routine delay radio button selection.
     * 
     * @param -- the selection predicate
     */
    public void setRtdRdo(boolean select) {
        rtdRdo.setSelection(select);
    }

    /**
     * Get correction radio button selection.
     * 
     * @return the selection predicate
     */
    public boolean isCorRdoSelected() {
        return corRdo.getSelection();
    }

    /**
     * Set correction radio button selection.
     * 
     * @param -- the selection predicate
     */
    public void setCorRdo(boolean select) {
        corRdo.setSelection(select);
    }

    /**
     * Set WMO ID Label
     * 
     * @param -- the text to put in the label
     * 
     */
    public void setWmoIdLbl(String s) {
        wmoIdLbl.setText(s);
    }

    /**
     * Set WMO Site Label
     * 
     * @param -- the text to put in the label
     * 
     */
    public void setWmoSiteLbl(String s) {
        wmoSiteIdLbl.setText(s);
    }

    /**
     * Set large text control
     * 
     * @param -- the text to put in the control
     * 
     */
    public void setLargeTF(String s) {
        largeTF.setText(s);
    }

    /**
     * Set small text control
     * 
     * @param -- the text to put in the control
     * 
     */
    public void setSmallTF(String s) {
        smallTF.setText(s);
    }

    /**
     * Amend the TAF with the designated amendment type
     * 
     * @param amd
     *            -- the type of amendment
     */
    public void amendTaf(AmendmentType amd) {
        switch (amd) {
        case RTN:
            amendIt("");
            break;

        case AMD:
            amendIt(amd.toString());
            break;

        case RTD:
            amendIt("");
            break;

        case COR:
            amendIt(amd.toString());
            break;
        }
    }

    /**
     * Method that updates the TAF with the amendment
     * 
     * @param amd
     *            -- the amendment
     */
    private void amendIt(String amd) {
        tafEditorTxt.setText(tafEditorTxt.getText()
                .replaceAll("TAF COR", "TAF"));
        tafEditorTxt.setText(tafEditorTxt.getText()
                .replaceAll("TAF AMD", "TAF"));
        tafEditorTxt.setText(tafEditorTxt.getText().replaceAll("TAF",
                ("TAF" + " " + amd)));

    }

    /**
     * Regex that captures the issue time and beginning of valid time of TAF.
     */
    public final String ISSUE_TIME = "\\d{6}Z \\d{4}/";

    /**
     * Update times in TAF during syntax checking provided the update times on
     * format checkbox is selected in the taf viewer/editor dialog.
     */
    public void updateTimes() {
        tafEditorTxt.setText(tafEditorTxt.getText().replaceAll(
                ISSUE_TIME,
                ForecastModel.getInstance().getTimeNowInDdHhMmFormat(0) + "Z "
                        + ForecastModel.getInstance().getTimeNowInDdHhFormat(0)
                        + "/"));
    }

    public boolean isSyntaxChecked() {
        return syntaxChecked;
    }

    public void setSyntaxChecked() {
        syntaxChecked = true;
    }

    public boolean isQcSkipCheck() {
        return qcSkipCheck;
    }

    public void setQcSkipCheck() {
        qcSkipCheck = true;
    }

    /**
     * Method to obtain indicator of errors in bulletin within TAF editor text
     * contents.
     * 
     * @return the indicator
     */
    public boolean isErrorsInBulletin() {
        return (errorLevel > 0);
    }

    /**
     * Method to set indicator of errors in bulletin within TAF editor text
     * contents.
     */
    public void setErrorsInBulletin(int errorLevel) {
        if (errorLevel > this.errorLevel) {
            this.errorLevel = errorLevel;
        }
    }

    /**
     * Method to clear indicator of errors in bulletin within TAF editor text
     * contents.
     */
    public void clearErrorsInBulletin() {
        this.errorLevel = 0;
    }

    public int getErrorLevel() {
        return errorLevel;
    }

    public String getWmoId() {
        return wmoIdLbl.getText();
    }

    public String getWmoSiteId() {
        return wmoSiteIdLbl.getText();
    }

    public String getLargeTF() {
        return largeTF.getText().trim();
    }

    public String getBBB() {
        String bbb = smallTF.getText().trim().toUpperCase();
        smallTF.setText(bbb);

        // DR #6023 rferrel
        // Assume routine (Rtn) TAF.
        if (bbb == null || bbb.isEmpty()) {
            bbb = "   ";
        }

        return bbb;
    }

    public void setBBB(String newBbb) {
        String bbb = newBbb.toUpperCase();
        setRtnRdo(false);
        setAmdRdo(false);
        setRtdRdo(false);
        setCorRdo(false);

        if (bbb.startsWith("AA")) {
            setAmdRdo(true);
            amendTaf(AmendmentType.AMD);
        } else if (bbb.startsWith("RR")) {
            setRtdRdo(true);
            amendTaf(AmendmentType.RTD);
        } else if (bbb.startsWith("CC")) {
            setCorRdo(true);
            amendTaf(AmendmentType.COR);
        } else {
            setRtnRdo(true);
            bbb = "";
            amendTaf(AmendmentType.RTN);
        }

        smallTF.setText(bbb);
    }

    public boolean isTafSent() {
        return tafSent;
    }

    public void setTafSent(boolean tafSent) {
        this.tafSent = tafSent;
    }

    public void undo() {
        if (undoStack.size() > 0) {
            HashMap<String, Object> undoData = undoStack.remove(undoStack
                    .size() - 1);
            int start = (Integer) undoData.get("start");
            int length = (Integer) undoData.get("length");
            String text = (String) undoData.get("replacedText");
            undoData.clear();
            undoData.put("start", start);
            undoData.put("length", text.length());
            undoData.put("replacedText",
                    tafEditorTxt.getTextRange(start, length));
            redoStack.add(undoData);
            // Set the modifyFlag to false so the action of undoing the last
            // modification does not update the undoStack.
            modifyFlag = false;
            tafEditorTxt.replaceTextRange(start, length, text);
            tafEditorTxt.setCaretOffset(start + text.length());
            // Reset the modifyFlag so that subsequent modifications do update
            // the undoStack.
            modifyFlag = true;
        }
    }

    public void redo() {
        if (redoStack.size() > 0) {
            HashMap<String, Object> redoData = redoStack.remove(redoStack
                    .size() - 1);
            int start = (Integer) redoData.get("start");
            int length = (Integer) redoData.get("length");
            String text = (String) redoData.get("replacedText");
            redoData.clear();
            redoData.put("start", start);
            redoData.put("length", text.length());
            redoData.put("replacedText",
                    tafEditorTxt.getTextRange(start, length));
            undoStack.add(redoData);
            // Set the modifyFlag to false so the action of redoing the last
            // modification does not update the undoStack
            modifyFlag = false;
            tafEditorTxt.replaceTextRange(start, length, text);
            tafEditorTxt.setCaretOffset(start + text.length());
            // Reset the modifyFlag so that subsequent modifications do update
            // the undoStack.
            modifyFlag = true;
        }
    }

    void updateTafSent(boolean sent) {
        if (tafSent != sent) {
            tafSent = sent;
        }
        setEditableControls(!sent);
        ResourceConfigMgr configMgr = ResourceConfigMgr.getInstance();
        if (sent) {
            configMgr.setTextEditorFontAndReverseColors(tafEditorTxt);
        } else {
            configMgr.setTextEditorFontAndColors(tafEditorTxt);
        }
        this.layout();
        this.pack(true);
        this.redraw();
    }

    private void setEditableControls(boolean editable) {
        tafEditorTxt.setEditable(editable);
        largeTF.setEditable(editable);
        smallTF.setEditable(editable);
        rtnRdo.setEnabled(editable);
        amdRdo.setEnabled(editable);
        rtdRdo.setEnabled(editable);
        corRdo.setEnabled(editable);
    }

	public boolean getAlt() {
		return alt;
	}

	public void setAlt(boolean b) {
		alt = b;
	}
}
