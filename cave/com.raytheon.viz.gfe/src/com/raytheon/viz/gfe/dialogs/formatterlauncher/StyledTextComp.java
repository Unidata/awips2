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

import static com.raytheon.viz.gfe.product.StringUtil.stringJoin;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import jep.JepException;

import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.resource.StringConverter;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.ExtendedModifyEvent;
import org.eclipse.swt.custom.ExtendedModifyListener;
import org.eclipse.swt.custom.StyleRange;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.VerifyEvent;
import org.eclipse.swt.events.VerifyListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.FontData;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Listener;

import com.raytheon.uf.common.dataplugin.gfe.python.GfePyIncludeUtil;
import com.raytheon.uf.common.python.PythonScript;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.RGBColors;
import com.raytheon.viz.gfe.Activator;
import com.raytheon.viz.gfe.dialogs.formatterlauncher.ProductEditorComp.PTypeCategory;
import com.raytheon.viz.gfe.rsc.GFEFonts;
import com.raytheon.viz.gfe.textformatter.TextFmtParserUtil;

/**
 * Composite containing the product editor.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 05 Jan 2008  1784       lvenable    Initial creation
 * 19 Feb 2010  4132       ryu         Product correction.
 * 30 Jul 2010  6719       jnjanga     Placed cursor at the end of inserted CTA
 * 26 Sep 2012  15423      ryu         Avoid resetting text when possible.
 * 03 Dec 2012  15620      ryu         Unlock framed cities list for editing.
 * 30 APR 2013  16095      ryu         Modified updateTextStyle() to not lock edited text.
 * 29 AUG 2013  #2250      dgilling    Better error handling for parseProductText().
 * 04 SEP 2013  16534      ryu         Fixed word wrap to not insert duplicate text; refactor.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class StyledTextComp extends Composite {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(StyledTextComp.class);

    private static final String DFT_BG = "#ffffff";

    private static final String DFT_FG = "#000000";

    private static final String DFT_FRAME = "#ff0000";

    private static final String DFT_INSERT = "cyan";

    private static final String DFT_LOCK = "#0000ff";

    private static final String SETTING_BG = "ProductOutputDialog_bgColor";

    private static final String SETTING_FG = "ProductOutputDialog_fgColor";

    private static final String SETTING_FRAME = "ProductOutputDialog_frameColor";

    private static final String SETTING_INSERT = "ProductOutputDialog_insertColor";

    private static final String SETTING_LOCK = "ProductOutputDialog_lockColor";

    private static final String EMPTY = "";

    private static final String SPC = " ";

    private static final String PRODUCT_PARSE_ERROR = "An unhandled exception was encountered trying to parse your text product. Please cancel and run your formatter again.";

    /**
     * Parent composite.
     */
    private Composite parent;

    /**
     * Styled text editor.
     */
    private StyledText textEditorST;

    /**
     * Text font.
     */
    private Font textFont;

    /**
     * Parsed data in a structure.
     */
    private ProductDataStruct prodDataStruct;

    private boolean corMode;

    /**
     * Mouse listener.
     */
    private Listener mouseListener;

    private boolean highlight = Activator.getDefault().getPreferenceStore()
            .getBoolean("HighlightFramingCodes");

    private boolean newProduct = false;

    private Set<String> unlockCitySegs;

    private boolean autoWrapMode;

    private int wrapColumn = 80; // TODO: get from external

    private boolean dirty = false;

    private PythonScript python = null;

    protected Color bgColor;

    protected Color fgColor;

    protected Color frameColor;

    protected Color insertColor;

    protected Color lockColor;

    private boolean updatingForCor = false;

    private static final String NORM_SEP = "^\\s*$";

    private static final String FUNNY_SEP = "^(\\s*)\\*(\\s*)";

    private static final String NWS_SEP = "^\\..*\\.{3}";

    private static final String PARA_SEP_STRING = "(" + NORM_SEP + ")|("
            + FUNNY_SEP + ")|(" + NWS_SEP + ")";

    public boolean isAutoWrapMode() {
        return autoWrapMode;
    }

    public void setAutoWrapMode(boolean autoWrapMode) {
        this.autoWrapMode = autoWrapMode;
    }

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent composite.
     */
    public StyledTextComp(Composite parent) {
        super(parent, SWT.BORDER);

        this.parent = parent;

        init();
    }

    /**
     * Initialize method.
     */
    private void init() {
        String fontSetting = Activator.getDefault().getPreferenceStore()
                .getString("ProductOutputDialog_font");
        FontData fontData;
        if (fontSetting.isEmpty()) {
            fontData = GFEFonts.getFontData(2);
        } else {
            fontData = StringConverter.asFontData(fontSetting);
        }
        textFont = new Font(parent.getDisplay(), fontData);

        createMouseListner();

        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        GridLayout gl = new GridLayout(1, false);
        gl.marginHeight = 1;
        gl.marginWidth = 1;
        this.setLayout(gl);
        this.setLayoutData(gd);

        getColorSettings();

        createTextControl();

        this.addDisposeListener(new DisposeListener() {
            @Override
            public void widgetDisposed(DisposeEvent arg0) {
                textFont.dispose();
                bgColor.dispose();
                fgColor.dispose();
                frameColor.dispose();
                insertColor.dispose();
                lockColor.dispose();
                if (python != null) {
                    python.dispose();
                    python = null;
                }
            }
        });

        List<String> preEvals = new ArrayList<String>();
        preEvals.add("import textwrap");
        try {
            python = new PythonScript(
                    GfePyIncludeUtil.getCommonGfeIncludePath(), this.getClass()
                            .getClassLoader(), preEvals);
        } catch (JepException je) {
            if (python != null) {
                python.dispose();
            }
        }
    }

    /**
     * Create the editor text control.
     */
    private void createTextControl() {
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        textEditorST = new StyledText(this, SWT.BORDER | SWT.MULTI
                | SWT.V_SCROLL | SWT.H_SCROLL);
        textEditorST.setWordWrap(false);
        textEditorST.setFont(textFont);
        textEditorST.setEditable(true);
        textEditorST.setLayoutData(gd);
        textEditorST.setBackground(bgColor);
        textEditorST.setForeground(fgColor);

        textEditorST.addKeyListener(new KeyAdapter() {
            @Override
            public void keyReleased(KeyEvent ke) {
                handleKeyRelease(ke);
            }
        });

        textEditorST.addVerifyListener(new VerifyListener() {
            @Override
            public void verifyText(VerifyEvent e) {
                handleVerifyEvent(e);
            }
        });

        textEditorST.addExtendedModifyListener(new ExtendedModifyListener() {

            @Override
            public void modifyText(ExtendedModifyEvent event) {
                updateTextStyle(event);
                checkAutoWrap(event);

                if (corMode && !updatingForCor) {
                    updatingForCor = true;
                    try {
                        makeCorrections();
                    } finally {
                        updatingForCor = false;
                    }
                }
            }

        });

        textEditorST.addListener(SWT.MouseDown, mouseListener);
        textEditorST.addListener(SWT.MouseUp, mouseListener);
    }

    /**
     * Get the StyledText editor.
     * 
     * @return The StyledText editor.
     */
    public StyledText getTextEditorST() {
        return textEditorST;
    }

    /**
     * Set the product text.
     * 
     * @param text
     *            The product text.
     */
    public void setProductText(String text) {
        newProduct = true;
        textEditorST.setText(EMPTY);
        textEditorST.setStyleRange(null);

        try {
            parseProductText(text);
            textEditorST.setText(text);
            lockText();
            findFramingCodes();
            textEditorST.getVerticalBar().setSelection(0);
            newProduct = false;
        } catch (JepException e) {
            statusHandler.error(PRODUCT_PARSE_ERROR, e);
        }
    }

    /**
     * Lock the parts of the text that needs to be uneditable.
     */
    private void lockText() {
        int startLine = 0;
        int endLine = 0;

        // NOTE: For the endline variables we need to exclude the last line
        // in the for loop because the the parsed index is at line x
        // and column 0. This occurs for the ciblock, mnd, header, and
        // term indexes. We are locking entire lines of text.

        String[] productTextArray = prodDataStruct.getProductTextArray();

        /*
         * Lock the ci block text.
         */
        HashMap<String, TextIndexPoints> ciMap = prodDataStruct.getCiMap();
        TextIndexPoints ciBlockTip = ciMap.get("ciblock");

        if (ciBlockTip != null) {
            startLine = ciBlockTip.getStartIndex().x;
            endLine = ciBlockTip.getEndIndex().x;
            lockLines(productTextArray, startLine, endLine);
        }

        /*
         * Lock the mnd text.
         */
        HashMap<String, TextIndexPoints> mndMap = prodDataStruct.getMndMap();
        TextIndexPoints mndTip = mndMap.get("mnd");

        if (mndTip != null) {
            startLine = mndTip.getStartIndex().x;
            endLine = mndTip.getEndIndex().x;
            lockLines(productTextArray, startLine, endLine);
        }

        /*
         * Lock content in the UGC text.
         */
        if (newProduct) {
            unlockCitySegs = new HashSet<String>();
        }

        List<SegmentData> segArray = prodDataStruct.getSegmentsArray();
        TextIndexPoints segTip, cityTip;

        for (SegmentData segmentData : segArray) {
            /*
             * Lock header.
             */
            segTip = segmentData.getSegmentDataIndexPoints("header");
            cityTip = segmentData.getSegmentDataIndexPoints("city");

            if (segTip != null) {
                startLine = segTip.getStartIndex().x;
                endLine = segTip.getEndIndex().x;

                String ugc = segTip.getText().substring(0, 6);

                // Check if this is a segment for which the cities list
                // should be unlocked. Cities list is unlocked for editing
                // when framing codes are present.
                if (newProduct) {
                    if (cityTip != null && cityTip.getText().indexOf("|*") > 0) {
                        unlockCitySegs.add(ugc);
                    }
                }

                if (unlockCitySegs.contains(ugc)) {
                    // Lock the segment header but skip the cities list.
                    int cityStart = cityTip.getStartIndex().x;
                    int cityEnd = cityTip.getEndIndex().x;

                    lockLines(productTextArray, startLine, cityStart);
                    lockLines(productTextArray, cityEnd, endLine);
                } else {
                    lockLines(productTextArray, startLine, endLine);
                }
            }

            /*
             * Lock term.
             */
            segTip = segmentData.getSegmentDataIndexPoints("term");

            if (segTip != null) {
                startLine = segTip.getStartIndex().x;
                endLine = segTip.getEndIndex().x;

                // system.out.println("startLine = " + startLine);
                // system.out.println("endLine = " + endLine);

                // system.out.println("productTextArray.length = "
                // + productTextArray.length);

                /*
                 * Due to the way the parser parses the text we have to check
                 * the endline for the TERM ('$$'). We have to have an offset to
                 * correct the end value.
                 */
                int endLineOffset = 0;
                if (endLine == productTextArray.length - 1) {
                    ++endLineOffset;
                }

                lockLines(productTextArray, startLine, endLine + endLineOffset);
            }
        }
    }

    private void lockLines(String[] productTextArray, int startLine, int endLine) {
        int strLen = 0;
        for (int i = startLine; i < endLine; i++) {
            // Add 1 to the length to account for the \n character
            strLen += productTextArray[i].length() + 1;
        }

        StyleRange sr = new StyleRange(textEditorST.getOffsetAtLine(startLine),
                strLen, lockColor, null);
        textEditorST.setStyleRange(sr);
    }

    /**
     * Find the framing codes in the text.
     */
    private void findFramingCodes() {
        StyleRange sr;

        /*
         * Color the framing codes.
         */
        List<TextIndexPoints> framingCodes = prodDataStruct.getFramingCodes();
        String tmpStr;
        int offset;

        for (TextIndexPoints fcTip : framingCodes) {
            tmpStr = fcTip.getText();
            offset = textEditorST.getOffsetAtLine(fcTip.getStartIndex().x);
            int index = prodDataStruct.getStringIndexInProduct(tmpStr, offset);

            sr = new StyleRange(index, tmpStr.length(), frameColor, null);
            textEditorST.setStyleRange(sr);
        }
    }

    /**
     * Parse the product text string.
     * 
     * @param productText
     *            Complete product text.
     * @throws JepException
     *             If python throws an Error trying to parse the product.
     */
    private void parseProductText(String productText) throws JepException {
        HashMap<String, Object> fmtResult = TextFmtParserUtil
                .parseText(productText);
        prodDataStruct = new ProductDataStruct(fmtResult, productText);

        prodDataStruct.printData();
    }

    public void patchMND(String tag) {
        patchMND(tag, false);
    }

    public void patchMND(String tag, boolean strip) {
        if (prodDataStruct == null) {
            return;
        }
        TextIndexPoints tip = prodDataStruct.getProductType();
        if (tip == null) {
            return;
        }
        String pline = tip.getText();
        String[] tokens = pline.split("\\.\\.\\.");
        int start;
        if (tokens[0].equals("TEST") || tokens[0].equals("EXPERIMENTAL")) {
            start = 2;
        } else {
            start = 1;
        }

        int i = start;
        for (; i < tokens.length; i++) {
            if (tokens[i].equals(tag)) {
                return;
            }
        }

        if (strip) {
            ArrayList<String> new_tokens = new ArrayList<String>();
            for (i = 0; i < start; i++) {
                new_tokens.add(tokens[i]);
            }
            for (i = start; i < tokens.length; i++) {
                if (tokens[i].equals("TEST")) {
                    new_tokens.add(tokens[i]);
                }
            }
            if (!tag.isEmpty()) {
                new_tokens.add(start, tag);
            }

            replaceText(tip, stringJoin(new_tokens, "..."));
        }
    }

    public void updatePType(String newfield) {
        if (prodDataStruct == null) {
            return;
        }
        // Only upper case 'codes' get put in text
        if (!Character.isUpperCase(newfield.charAt(0))) {
            newfield = EMPTY;
        }

        // Find the code and the pit
        TextIndexPoints ff = prodDataStruct.getFunnyFiled();
        TextIndexPoints pit = prodDataStruct.getPIT();

        if (ff == null) {
            if (pit == null || newfield.length() == 0) {
                return; // No typecode or ci block found
            } else {
                ff = new TextIndexPoints();
                Point p = pit.getEndIndex();
                ff.addIndexPointsAndText(p.x, p.y, p.x, p.y, EMPTY);
                replaceText(ff, SPC + newfield);
            }
        } else {
            String s = SPC + newfield;
            if (!ff.getText().equals(s)) {
                replaceText(ff, s);
            }
        }
    }

    public void replaceText(TextIndexPoints tip, String text) {
        int start = prodDataStruct.positionToOffset(tip.getStartIndex());
        if (!tip.getText().equals(text)) {
            StyleRange[] ranges = textEditorST.getStyleRanges(start, tip
                    .getText().length());
            textEditorST.replaceTextRange(start, tip.getText().length(), text);

            // only reparse if we replaced with different length text
            // else, replace StyleRanges since the operation is safe
            if (tip.getText().length() != text.length()) {
                dirty = true;
            } else {
                for (StyleRange range : ranges) {
                    textEditorST.setStyleRange(range);
                }
            }
        }
    }

    private void makeCorrections() {
        ((ProductEditorComp) parent).setPTypeCategory(PTypeCategory.COR);
        List<SegmentData> segs = prodDataStruct.getSegmentsArray();
        for (SegmentData seg : segs) {
            if (seg.getSementMap().keySet().contains("vtec")) {
                correctVTEC();
                break;
            }
        }

    }

    private void correctVTEC() {
        try {
            startUpdate();
            List<SegmentData> segs = prodDataStruct.getSegmentsArray();
            if (segs == null || segs.size() == 0) {
                return;
            }

            int offset = textEditorST.getCaretOffset();
            Pattern codePattern = Pattern.compile("\\.([A-Z]{3})\\.");
            for (SegmentData segData : segs) {
                HashMap<String, TextIndexPoints> segMap = segData
                        .getSementMap();
                TextIndexPoints tipUgc = segMap.get("ugc");
                int start = prodDataStruct.positionToOffset(tipUgc
                        .getStartIndex());
                int end = prodDataStruct.positionToOffset(tipUgc.getEndIndex());
                if (offset <= start || offset >= end) {
                    continue;
                }
                TextIndexPoints tipVtec = segMap.get("vtec");
                if (tipVtec == null) {
                    break;
                }
                start = prodDataStruct
                        .positionToOffset(tipVtec.getStartIndex());
                end = prodDataStruct.positionToOffset(tipVtec.getEndIndex());
                int lineCount = tipVtec.getEndIndex().x
                        - tipVtec.getStartIndex().x;
                String[] newVtec = new String[lineCount];
                boolean changed = false;
                for (int i = 0; i < lineCount; i++) {
                    String vtec = prodDataStruct.getProductTextArray()[i
                            + tipVtec.getStartIndex().x];
                    if (vtec.indexOf("-") < 0) {
                        newVtec[i] = vtec;
                    } else {
                        Matcher matcher = codePattern.matcher(vtec);
                        if (matcher.find()) {
                            String code = matcher.group();
                            if (code.equals(".UPG.") || code.equals(".COR.")) {
                                newVtec[i] = vtec;
                            } else if ((code.equals(".EXP.") || code
                                    .equals(".CAN.")) && (lineCount > 1)) {
                                newVtec[i] = vtec;
                            } else {
                                newVtec[i] = vtec.substring(0, matcher.start())
                                        + ".COR."
                                        + vtec.substring(matcher.end());
                                changed = true;
                            }
                        }
                    }
                }

                if (changed) {
                    replaceText(tipVtec, stringJoin(newVtec, "\n"));
                }
                break;
            }
        } finally {
            endUpdate();
        }
    }

    /**
     * Handle the verify key event. This event fires after a change has been
     * made to the control (after the text has been updated, for example)
     * 
     * @param event
     *            Verify event that was fired.
     */
    private void handleVerifyEvent(VerifyEvent event) {
        // we're going to implicitly trust programmatic updates to the text
        // control and trust they correctly set the dirty flag
        if (isSystemTextChange()) {
            return;
        }

        int offset = event.start;
        if (offset >= textEditorST.getCharCount()) {
            return;
        }

        // Check if the selected text contains locked text.
        boolean selectTextLocked = selectionHasLockedText();

        // Get the StyleRange at the cursor offset.
        int length = event.end - offset;
        if (length == 0) {
            length = 1;
        }
        boolean editingLockedText = rangeHasLockedText(offset, length);

        if (selectTextLocked || editingLockedText) {
            if (!isNonEditKey(event) && !isSystemTextChange()) {
                event.doit = false;
                return;
            }
        }

        // this is specifically to handle the case of deleting line breaks
        // between two separate locked sections so a locked section cannot be
        // moved onto the end of an unlocked line
        if (length == 1 && event.text.length() == 0) {
            if (offset + 2 < textEditorST.getCharCount()
                    && rangeHasLockedText(offset, 2)) {
                event.doit = false;
                return;
            }
        }

        // allow edit to go through
        dirty = true;
    }

    private void updateTextStyle(ExtendedModifyEvent event) {
        if (event.start + event.length + 1 < textEditorST.getCharCount()) {
            int start = Math.max(0, event.start - 1);
            int end = Math.min(textEditorST.getCharCount() - 1, event.start
                    + event.length + 1);
            StyleRange startRange = textEditorST.getStyleRangeAtOffset(start);
            StyleRange endRange = textEditorST.getStyleRangeAtOffset(end);
            // StyleRange startRange = textEditorST
            // .getStyleRangeAtOffset(event.start - 1);
            // StyleRange endRange = textEditorST
            // .getStyleRangeAtOffset(event.start + event.length + 1);

            // if it's in a framing code, turn it red
            if (startRange != null && endRange != null
                    && event.start > startRange.start
                    && event.start + event.length < endRange.start
                    && startRange.similarTo(endRange)
                    && startRange.foreground.equals(frameColor)) {
                StyleRange style = (StyleRange) startRange.clone();
                style.start = event.start;
                style.length = event.length;
                textEditorST.setStyleRange(style);
            }

            // framing code was deleted, need to turn it black
            boolean framingCodeChange = false;
            if (("*").equals(event.replacedText)
                    || ("|").equals(event.replacedText)) {
                framingCodeChange = true;
            }
            // framing code was added, need to turn it red
            char newText = textEditorST.getText().charAt(event.start);
            if (newText == '*' || newText == '|') {
                framingCodeChange = true;
            }

            if (framingCodeChange) {
                reParse();
            }
        }
    }

    /**
     * Handle the key event when a key is released.
     * 
     * @param ke
     *            Key event.
     */
    private void handleKeyRelease(KeyEvent ke) {
        int offset = textEditorST.getCaretOffset();

        StyleRange[] srArray = textEditorST.getStyleRanges(true);

        for (int i = 0; i < srArray.length; i++) {
            if (srArray[i].start <= offset
                    && offset <= srArray[i].start + srArray[i].length) {
                if (srArray[i].foreground == frameColor) {
                    inFramingCode(srArray[i]);
                }
            }
        }
    }

    /**
     * Check if there is selected text and if there is locked text in the
     * selected text.
     * 
     * @return True if there is selected text that contains locked text.
     */
    private boolean selectionHasLockedText() {
        if (textEditorST.getSelectionCount() == 0) {
            return false;
        }

        Point selPt = textEditorST.getSelectionRange();
        return rangeHasLockedText(selPt.x, selPt.y);
    }

    /**
     * Check if there is locked text in the specified range of text.
     * 
     * @param offset
     *            The starting point of the locked text search.
     * @param length
     *            The length of the search.
     * 
     * @return Whether or not there is text in the range that contains locked
     *         text.
     */
    protected boolean rangeHasLockedText(int offset, int length) {
        StyleRange[] ranges = textEditorST.getStyleRanges(offset, length);

        for (StyleRange range : ranges) {
            if (range.foreground == lockColor) {
                return true;
            }
        }

        return false;
    }

    /**
     * Select the framing code and the text contained in the framing code.
     * 
     * @param sr
     *            StyleRange.
     */
    private void inFramingCode(StyleRange sr) {
        if (highlight) {
            textEditorST.setSelection(sr.start, sr.start + sr.length);
        }
    }

    /**
     * Check if the key being pressed is a "non-edit" key.
     * 
     * @param event
     *            Verify event.
     * @return True if the key is an arrow or "non-edit" key.
     */
    private boolean isNonEditKey(KeyEvent event) {
        if (event.keyCode == SWT.ARROW_UP || event.keyCode == SWT.ARROW_DOWN
                || event.keyCode == SWT.ARROW_LEFT
                || event.keyCode == SWT.ARROW_RIGHT
                || event.keyCode == SWT.SHIFT) {
            return true;
        }

        return false;
    }

    /**
     * Create a mouse listener for the StyledText editor.
     */
    private void createMouseListner() {
        mouseListener = new Listener() {

            @Override
            public void handleEvent(Event e) {
                if (e.type == SWT.MouseDown) {
                    handleMouseDown(e);
                } else if (e.type == SWT.MouseUp) {
                    handleMouseUp(e);
                }
            }
        };
    }

    /**
     * Handle the mouse down event.
     * 
     * @param e
     *            Event fired.
     */
    private void handleMouseDown(Event e) {

        // Check if the second mouse button was pressed.
        if (e.button == 2) {
            e.doit = false;
            int offset = textEditorST.getCaretOffset();

            if (offset >= textEditorST.getCharCount()) {
                return;
            }

            boolean selectTextLocked = selectionHasLockedText();

            if (selectTextLocked == true) {
                e.doit = false;
                return;
            }

            StyleRange sr = textEditorST.getStyleRangeAtOffset(offset);

            if (sr != null) {
                if (sr.foreground == lockColor) {
                    e.doit = false;
                    return;
                }
            }
        }
    }

    /**
     * Handle the mouse up event
     * 
     * @param e
     *            Event fired.
     */
    private void handleMouseUp(Event e) {
        if (e.button == 1) {
            int offset = textEditorST.getCaretOffset();

            StyleRange[] sr = textEditorST.getStyleRanges(true);

            for (int i = 0; i < sr.length; i++) {
                if (sr[i].start <= offset
                        && offset <= sr[i].start + sr[i].length) {
                    if (sr[i].foreground == frameColor) {
                        inFramingCode(sr[i]);
                    }
                }
            }
        }
    }

    public void setFramingCodeState(boolean highlight) {
        this.highlight = highlight;
    }

    /**
     * Cut the selected text if the selection does not contain locked text.
     */
    public void cutText() {
        // Do not cut text if the selection contains locked text.
        if (selectionHasLockedText() == true) {
            return;
        }

        // Cut the selected text. If text is not selected, then StyledText
        // will not perform the cut operation.
        textEditorST.cut();
    }

    /**
     * Copy the selected text.
     */
    public void copyText() {
        textEditorST.copy();
    }

    /**
     * Paste text into the StyledText editor.
     */
    public void pasteText() {
        // Do not paste text if the there is selected text and it contains
        // locked text.
        if (selectionHasLockedText() == true) {
            return;
        }

        // There is a selection so we want to paste the contents into the
        // selection and then return.
        if (textEditorST.getSelectionCount() >= 0) {
            textEditorST.paste();
            return;
        }

        // Get the offset of the cursor and determine if it is in a locked area.
        int offset = textEditorST.getCaretOffset();

        if (offset >= textEditorST.getCharCount()) {
            return;
        }

        StyleRange sr = textEditorST.getStyleRangeAtOffset(offset);

        if (sr != null) {
            // If the cursor is in a locked area then return;
            if (sr.foreground == lockColor) {
                return;
            }
        }

        textEditorST.paste();
    }

    public String getProductText() {
        return textEditorST.getText();
    }

    public ProductDataStruct getProductDataStruct() {
        return prodDataStruct;
    }

    public void startUpdate() {
        textEditorST.setEditable(false);
        if (dirty) {
            reParse();
        }
    }

    public void endUpdate() {
        if (dirty) {
            reParse();
        }
        textEditorST.setEditable(true);
    }

    /**
     * Checks if the system is editing, e.g. updating the issue time every
     * minute, vs a user typing text in the text area
     * 
     * @return
     */
    private boolean isSystemTextChange() {
        return !textEditorST.getEditable();
    }

    protected void reParse() {
        textEditorST.setStyleRange(null);
        // mmaron #6718: append "\n \n" to the end of forecast text
        // to make line after the last $$ editable
        String _text = textEditorST.getText();

        if (!_text.endsWith("\n \n")) {
            _text += "\n \n";
        }

        try {
            parseProductText(_text);
            findFramingCodes();
            lockText();
            dirty = false;
        } catch (JepException e) {
            statusHandler.error(PRODUCT_PARSE_ERROR, e);
        }
    }

    protected boolean isUpperCase(final String word) {
        for (int index = word.length() - 1; index >= 0; index--) {
            if (Character.isLowerCase(word.charAt(index))) {
                return false;
            }
        }
        return true;
    }

    protected void upper() {
        String text = textEditorST.getText();
        if (isUpperCase(text)) {
            return;
        }
        int topIdx = textEditorST.getTopIndex();
        setProductText(textEditorST.getText().toUpperCase());
        textEditorST.setTopIndex(topIdx);
    }

    protected void setCorMode(boolean corMode) {
        this.corMode = corMode;
    }

    protected boolean isCorMode() {
        return corMode;
    }

    protected void checkAutoWrap(ExtendedModifyEvent event) {
        if (!autoWrapMode) {
            return;
        }

        int cursorOffset = textEditorST.getCaretOffset();

        // Modifications made within the routine apparently re-invoke it.
        // Since we know wrap mode was on, turn it off, do processing, then turn
        // it back on.
        try {
            autoWrapMode = false;

            int totalLines = textEditorST.getLineCount();
            int lineNum = textEditorST.getLineAtOffset(event.start);
            int lineLength = textEditorST.getLine(lineNum).length();

            if (lineLength < wrapColumn
                    && event.length >= event.replacedText.length()) {
                return;
            }

            String NL = textEditorST.getLineDelimiter();

            String line = textEditorST.getLine(lineNum)
                    + ((lineNum + 1 < totalLines) ? NL : EMPTY);
            lineLength = line.length();
            if (NL.equals(line) || EMPTY.equals(line)) {
                return;
            }

            int lineOff = textEditorST.getOffsetAtLine(lineNum);
            int index = line.lastIndexOf(SPC, wrapColumn);
            if (index < 0) {
                // No space w/index <= wrapColumn; look for one after
                if (lineLength > wrapColumn) {
                    index = line.substring(wrapColumn).indexOf(SPC);
                    if (index >= 0) {
                        index += wrapColumn;
                    }
                }
                if (index < 0) {
                    index = lineLength - 1;
                    if (NL.equals(line.substring(index))) {
                        // treat as normal NL and don't join lines
                    } else {
                        // last line; nothing to join with
                        return;
                    }
                }
            }

            // check for locked text
            StyleRange styleRange = textEditorST.getStyleRangeAtOffset(lineOff
                    + index);
            if (styleRange != null && styleRange.foreground == lockColor) {
                return;
            }

            // deal with programmatic changes distant from the cursor
            int eventCursor = cursorOffset;
            if (eventCursor < event.start
                    || eventCursor > event.start + event.length) {
                eventCursor = event.start + event.length;
            }

            wordWrap(textEditorST, eventCursor, wrapColumn);

            if (cursorOffset != eventCursor) {
                // restore cursor position for programmatic changes
                if (cursorOffset > event.start) {
                    int diff = textEditorST.getCaretOffset() - eventCursor;
                    cursorOffset += diff;
                }
                textEditorST.setCaretOffset(cursorOffset);
            }

        } finally {
            autoWrapMode = true;
        }
    }

    /**
     * @param color
     * @param start
     *            The starting offset of the region in textEditorST
     * @param length
     *            The length of the region
     * @return true if the foreground of any style range touching
     */
    protected boolean textHasColor(Color color, int start, int length) {
        StyleRange[] styleRanges = textEditorST.getStyleRanges(start, length);
        for (StyleRange range : styleRanges) {
            if (range.foreground.equals(color)) {
                return true;
            }
        }
        return false;
    }

    /**
     * Getter for the column at which wrap and auto-wrap will wrap the text.
     * 
     * @return the column number
     */
    public int getWrapColumn() {
        return wrapColumn;
    }

    /**
     * Getter for the column at which wrap and auto-wrap will wrap the text.
     * 
     * @param wrapColumn
     *            the column number
     */
    public void setWrapColumn(int wrapColumn) {
        this.wrapColumn = wrapColumn;
    }

    /**
     * Get the foreground and background color settings.
     */
    protected void getColorSettings() {

        IPreferenceStore prefs = Activator.getDefault().getPreferenceStore();
        Display display = getDisplay();

        // get color settings
        bgColor = loadColor(prefs, display, SETTING_BG, DFT_BG);
        fgColor = loadColor(prefs, display, SETTING_FG, DFT_FG);
        frameColor = loadColor(prefs, display, SETTING_FRAME, DFT_FRAME);
        insertColor = loadColor(prefs, display, SETTING_INSERT, DFT_INSERT);
        lockColor = loadColor(prefs, display, SETTING_LOCK, DFT_LOCK);

        // It's BAD if any two editor colors are exactly the same.
        // If it happens, scold the user. The component may be unusable.
        Color[] colors = new Color[] { bgColor, fgColor, frameColor,
                insertColor, lockColor };
        String[] labels = new String[] { "Background", "Foreground", "Frame",
                "Insert", "Locked" };
        for (int i = 0; i < colors.length - 1; i++) {
            for (int j = i + 1; j < colors.length; j++) {
                warnIfEqual(colors[i], colors[j], labels[i], labels[j]);
            }
        }
    }

    /**
     * Query the prefs for setting. If it does not exist, use colorDft as its
     * value. Create an SWT Color for display from the value and return it.
     * 
     * @param prefs
     *            A preference store which might have config values.
     * @param display
     *            The SWT display on which the color will appear
     * @param setting
     *            The name of the config setting to look up
     * @param colorDft
     *            The vaule to use if the config setting is missing
     * @return The SWT color.
     */
    protected Color loadColor(IPreferenceStore prefs, Display display,
            String setting, String colorDft) {
        Color color = null;
        String colorStr = prefs.getString(setting);
        colorStr = "".equals(colorStr) ? colorDft : colorStr;
        RGB colorRGB = RGBColors.getRGBColor(colorStr);
        color = new Color(display, colorRGB);
        return color;
    }

    /**
     * Send a PROBLEM message if color1 is exactly equal to color2.
     * 
     * @param color1
     *            the first color
     * @param color2
     *            the second color
     * @param label1
     *            the name for color1
     * @param label2
     *            the name for color2
     */
    protected void warnIfEqual(Color color1, Color color2, String label1,
            String label2) {
        if (color1.equals(color2)) {
            String msg = String
                    .format("%s color is identical to %s color. Editing may not work properly.",
                            label1, label2);
            statusHandler.handle(Priority.PROBLEM, msg);
        }
    }

    /**
     * Get the foreground color of the StyledTextComp. This is the actual color,
     * not a copy. It will be disposed when the StyledTextComp is, and should
     * not be disposed before then.
     * <p>
     * The getter name is different to avoid confusion with the getFgColor()
     * method of Control.
     * 
     * @return the foreground Color
     */
    public Color getFgndColor() {
        return fgColor;
    }

    /**
     * Get the framed text color of the StyledTextComp. This is the actual
     * color, not a copy. It will be disposed when the StyledTextComp is, and
     * should not be disposed before then.
     * 
     * @return the frameColor
     */
    public Color getFrameColor() {
        return frameColor;
    }

    /**
     * Get the insert color of the StyledTextComp. This is the actual color, not
     * a copy. It will be disposed when the StyledTextComp is, and should not be
     * disposed before then.
     * 
     * @return the insertColor
     */
    public Color getInsertColor() {
        return insertColor;
    }

    /**
     * Get the locked text color of the StyledTextComp. This is the actual
     * color, not a copy. It will be disposed when the StyledTextComp is, and
     * should not be disposed before then.
     * 
     * @return the lockColor
     */
    public Color getLockColor() {
        return lockColor;
    }

    /**
     * Word wrap the text in the block around cursorIndex. Adjust the cursor
     * position to account for inserted or deleted whitespace.
     * 
     * @param st
     *            The StyledText in which word wrap is to be performed
     * @param cursorIndex
     *            The cursor index
     * @param width
     *            The width to which to word wrap
     * @return An array of integers giving:
     *         <ol>
     *         <li value=0>The index in the old content of the first character</li>
     *         <li>The index in the old content of the last character</li>
     *         <li>The length of the replacement text</li>
     *         </ol>
     */
    public int[] wordWrap(StyledText st, int cursorIndex, int width) {

        final Pattern PARA_SEP_PATTERN = Pattern.compile(PARA_SEP_STRING);
        final Matcher PARA_MATCHER = PARA_SEP_PATTERN.matcher("");

        final Pattern FUNNY_SEP_PATTERN = Pattern.compile(FUNNY_SEP,
                Pattern.MULTILINE);
        final Matcher FUNNY_SEP_MATCHER = FUNNY_SEP_PATTERN.matcher("");

        final String NL = st.getLineDelimiter();

        String line;

        int startIndex = -1;
        String initialIndent = "";
        // search backwards for a paragraph separator
        for (int searchLine = st.getLineAtOffset(cursorIndex); searchLine >= 0; searchLine--) {
            line = st.getLine(searchLine);
            int lineOffset = st.getOffsetAtLine(searchLine);

            // if line contains locked text, quit looking.
            if (rangeHasLockedText(lineOffset, line.length())) {
                break;
            }

            // The paragraph separator before the caret ends our search.
            PARA_MATCHER.reset(line);
            if (PARA_MATCHER.lookingAt()) {

                // If it's FUNNY_SEP, use it as the start index
                FUNNY_SEP_MATCHER.reset(line);
                if (FUNNY_SEP_MATCHER.lookingAt()) {
                    startIndex = lineOffset;
                }
                break;
            }
        }

        if (startIndex < 0) {
            startIndex = st.getOffsetAtLine(st.getLineAtOffset(cursorIndex));
        }

        // Find the end index of the wrap
        int endIndex = -1;
        int cursorLine = st.getLineAtOffset(cursorIndex);
        int lineCursorPos = cursorIndex - st.getOffsetAtLine(cursorLine);
        for (int searchLine = cursorLine; searchLine <= st.getLineAtOffset(st
                .getCharCount()); searchLine++) {

            int lineStartOffset = st.getOffsetAtLine(searchLine);
            line = st.getLine(searchLine);

            // don't use locked text
            if (rangeHasLockedText(lineStartOffset, line.length())) {
                break;
            }

            // don't include paragraph separators
            // start from cursor position on first line
            PARA_MATCHER.reset(line);
            if (PARA_MATCHER.find(lineCursorPos)) {
                break;
            }

            lineCursorPos = 0;

            // remember last good endIndex
            endIndex = lineStartOffset + line.length() - 1;
        }

        if (endIndex < 0) {
            endIndex = startIndex - 1;
        }

        if (endIndex >= st.getCharCount()) {
            endIndex = st.getCharCount() - 1;
        }

        if (endIndex < startIndex) {
            return new int[] { startIndex, endIndex, 0 };
        }

        // get the block text before the cursor
        String pre = "";
        if (startIndex < cursorIndex) {
            pre = st.getText(startIndex, cursorIndex - 1);
        }

        FUNNY_SEP_MATCHER.reset(pre);
        if (FUNNY_SEP_MATCHER.lookingAt()) {
            initialIndent = FUNNY_SEP_MATCHER.group(0);
            pre = pre.substring(initialIndent.length());
        }

        // get the text from the caret to the end of the block
        String post = "";
        if (endIndex >= cursorIndex && cursorIndex < st.getCharCount()) {
            post = st.getText(cursorIndex, endIndex);
        }

        // fudge characters for newlines next to the caret
        String lchar = "";
        if (cursorIndex > 0) {
            lchar = st.getTextRange(cursorIndex - 1, 1);
            char lchar0 = lchar.charAt(0);
            if (Character.isSpaceChar(lchar0) && lchar0 != NL.charAt(0)) {
                lchar = " ";
            } else {
                lchar = "";
            }
        }

        String rchar = "";
        if (post.length() > 0) {
            char post0 = post.charAt(0);
            if (Character.isSpaceChar(post0) && post0 != NL.charAt(0)) {
                rchar = " ";
            }
        }

        // normalize whitespace in pre and post
        pre = pre.replaceAll("(?m)\\s+", " ");
        post = post.replaceAll("(?m)\\s+", " ");

        Map<String, Object> args = new HashMap<String, Object>();
        args.put("initial_indent", initialIndent);
        args.put("subsequent_indent", initialIndent.replaceAll(".", " "));
        args.put("break_long_words", Boolean.FALSE);
        args.put("width", Integer.valueOf(width));

        Map<String, Object> preArgs = new HashMap<String, Object>();
        preArgs.put("text", pre);

        try {
            python.instantiatePythonClass("_wrapper", "textwrap.TextWrapper",
                    args);
            pre = (String) python.execute("fill", "_wrapper", preArgs);

        } catch (JepException e) {
            statusHandler.error("Python error wrapping text preceding cursor:",
                    e);
        }

        if ("".equals(pre) && !"".equals(initialIndent)) {
            pre = initialIndent;
            lchar = "";
        }

        pre += lchar;

        String postInitialIndent = "";
        int lastNL = pre.lastIndexOf(NL);
        postInitialIndent = " "
                + pre.substring(lastNL + NL.length()).replaceAll(".", " ");

        args.put("initial_indent", postInitialIndent);

        Map<String, Object> postArgs = new HashMap<String, Object>();
        postArgs.put("text", post);

        try {
            python.instantiatePythonClass("_wrapper", "textwrap.TextWrapper",
                    args);
            post = (String) python.execute("fill", "_wrapper", postArgs);
        } catch (JepException e) {
            statusHandler.error("Python error wrapping text after cursor:", e);
        }

        // post = post.lstrip()
        post = post.replaceAll("^\\s*", "");

        String text = pre + rchar + post;
        st.replaceTextRange(startIndex, 1 + endIndex - startIndex, text);
        int newCaretOffset = startIndex + pre.length();
        st.setCaretOffset(newCaretOffset);

        return new int[] { startIndex, endIndex, text.length() };
    }

}
