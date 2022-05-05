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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.jface.resource.StringConverter;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.ExtendedModifyEvent;
import org.eclipse.swt.custom.ExtendedModifyListener;
import org.eclipse.swt.custom.StyleRange;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
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

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.ProductEditorLogger;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.RGBColors;
import com.raytheon.viz.gfe.GFEPreference;
import com.raytheon.viz.gfe.dialogs.formatterlauncher.ProductEditorComp.PTypeCategory;
import com.raytheon.viz.gfe.rsc.GFEFonts;
import com.raytheon.viz.gfe.textformatter.TextFmtParserUtil;

import jep.JepException;

/**
 * Composite containing the product editor.
 *
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Jan 05, 2008  1784     lvenable  Initial creation
 * Feb 19, 2010  4132     ryu       Product correction.
 * Jul 30, 2010  6719     jnjanga   Placed cursor at the end of inserted CTA
 * Sep 26, 2012  15423    ryu       Avoid resetting text when possible.
 * Dec 03, 2012  15620    ryu       Unlock framed cities list for editing.
 * Apr 30, 2013  16095    ryu       Modified updateTextStyle() to not lock
 *                                  edited text.
 * Aug 29, 2013  2250     dgilling  Better error handling for
 *                                  parseProductText().
 * Sep 04, 2013  16534    ryu       Fixed word wrap to not insert duplicate
 *                                  text; refactor.
 * Dec 20, 2013  16854    ryu       Force re-parsing of text on type change.
 * Jan 28, 2015  4018     randerso  Code cleanup. Fixed reparsing when framing
 *                                  codes are cut or pasted instead of just
 *                                  typed over. Added logging of text changes to
 *                                  help diagnose future issues.
 * Feb 04, 2015  17039    ryu       Removed HighlightFramingCodes feature which
 *                                  prevented editing of framing codes.
 * Jul 02, 2015  13753    lshi      Update times for products in Product Editor
 * Aug 06, 2015  13753    lshi      use isSystemTextChange instead of
 *                                  isUpdateTime
 * Oct 14, 2015  4959     dgilling  Use WordWrapperPythonExecutor to get python
 *                                  calls off UI thread.
 * Dec 04, 2015  13753    lshi      revert 13753
 * Dec 22, 2015  18428    lshi      Issuing a Correction of a corrected product
 *                                  via an existing Product Editor in GFE throws
 *                                  and error and unlocks text, wordWrap
 * Mar 10, 2016  5479     randerso  Use improved GFEFonts API
 * Jun 17, 2016  18940    arickert  Remove startIndex check to allow word
 *                                  wrapping on the first line
 * Aug 08, 2016  5787     dgilling  Prevent product header line from wrapping.
 * Aug 09, 2016  5685     dgilling  Tweak word wrapping to prevent pulling up
 *                                  words from the previous line if it will make
 *                                  the line too long.
 * Nov 07, 2016  5984     dgilling  Tweak Pattern used to strip leading spaces
 *                                  on lines.
 * Dec 22, 2016  19616    ryu       Added logic for dashed-bullets and
 *                                  colon-terminated paragraphs. Fixed
 *                                  indentation for bulleted text below the
 *                                  first line.
 * Feb 08, 2017  6127     dgilling  Remove errant calls to startUpdate/endUpdate
 *                                  added under 5787.
 * Jan 24, 2018  7153     randerso  Changes to allow new GFE config file to be
 *                                  selected when perspective is re-opened.
 * Mar 20, 2018 20585      ryu         Fix verify listener to prevent joining of normal text
 *                                     and locked text causing unlocking and garble.
 * Apr 05, 2018  6775     dgilling  Fix drifting cursor when deleting text for
 *                                  a CORRECTED product.
 *
 * </pre>
 *
 * @author lvenable
 */

public class StyledTextComp extends Composite {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(StyledTextComp.class);

    private static final String DFT_BG = "#ffffff";

    private static final String DFT_FG = "#000000";

    private static final String DFT_FRAME = "#ff0000";

    private static final String DFT_LOCK = "#0000ff";

    private static final String SETTING_BG = "ProductOutputDialog_bgColor";

    private static final String SETTING_FG = "ProductOutputDialog_fgColor";

    private static final String SETTING_FRAME = "ProductOutputDialog_frameColor";

    private static final String SETTING_LOCK = "ProductOutputDialog_lockColor";

    private static final String EMPTY = StringUtils.EMPTY;

    private static final String SPC = StringUtils.SPACE;

    private static final String PRODUCT_PARSE_ERROR = "An unhandled exception was encountered trying to parse your text product. Please cancel and run your formatter again.";

    private static final String NORM_SEP = "^\\s*$";

    private static final String FUNNY_SEP = "^(\\s*)(\\*|\\-)(\\s*)";

    private static final String NWS_SEP = "^\\..*\\.{3}";

    private static final String COLON_END_SEP = ":\\s*$";

    private static final String PARA_SEP_STRING = "(" + NORM_SEP + ")|("
            + FUNNY_SEP + ")|(" + NWS_SEP + ")";

    private static final Pattern PARA_SEP_PATTERN = Pattern
            .compile(PARA_SEP_STRING);

    private static final Pattern FUNNY_SEP_PATTERN = Pattern.compile(FUNNY_SEP,
            Pattern.MULTILINE);

    private static final Pattern COLON_END_PATTERN = Pattern
            .compile(COLON_END_SEP);

    /**
     * Parent composite.
     */
    private final ProductEditorComp parent;

    private final WordWrapPythonExecutor wrapper;

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

    private boolean newProduct = false;

    private Set<String> unlockCitySegs = new HashSet<>();

    private boolean autoWrapMode;

    private int wrapColumn;

    private boolean dirty = false;

    protected Color bgColor;

    protected Color fgColor;

    protected Color frameColor;

    protected Color lockColor;

    private boolean updatingForCor = false;

    private final ProductEditorLogger peLog;

    /**
     * Constructor.
     *
     * @param parent
     *            Parent composite.
     * @param wrapMode
     * @param wrapColumn
     */
    public StyledTextComp(ProductEditorComp parent, int wrapColumn,
            boolean wrapMode) {
        super(parent, SWT.BORDER);

        this.parent = parent;
        this.wrapColumn = wrapColumn;
        this.autoWrapMode = wrapMode;
        this.wrapper = new WordWrapPythonExecutor();

        this.peLog = new ProductEditorLogger(parent.getProductName());

        init();
    }

    /**
     * Initialize method.
     */
    private void init() {
        String fontSetting = GFEPreference
                .getString("ProductOutputDialog_font");
        if (fontSetting.isEmpty()) {
            textFont = GFEFonts.getFont(parent.getDisplay(), 2);
        } else {
            FontData fontData = StringConverter.asFontData(fontSetting);
            textFont = new Font(parent.getDisplay(), fontData);
        }

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
                wrapper.dispose();
                textFont.dispose();
                bgColor.dispose();
                fgColor.dispose();
                frameColor.dispose();
                lockColor.dispose();
            }
        });
    }

    /**
     * @return the autoWrapMode
     */
    public boolean isAutoWrapMode() {
        return autoWrapMode;
    }

    /**
     * @param autoWrapMode
     *            the autoWrapMode to set
     */
    public void setAutoWrapMode(boolean autoWrapMode) {
        this.autoWrapMode = autoWrapMode;
    }

    /**
     * Create the editor text control.
     */
    private void createTextControl() {
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        textEditorST = new StyledText(this,
                SWT.BORDER | SWT.MULTI | SWT.V_SCROLL | SWT.H_SCROLL);
        textEditorST.setWordWrap(false);
        textEditorST.setFont(textFont);
        textEditorST.setEditable(true);
        textEditorST.setLayoutData(gd);
        textEditorST.setBackground(bgColor);
        textEditorST.setForeground(fgColor);

        textEditorST.addVerifyListener(new VerifyListener() {
            @Override
            public void verifyText(VerifyEvent e) {
                handleVerifyEvent(e);
            }
        });

        textEditorST.addExtendedModifyListener(new ExtendedModifyListener() {

            @Override
            public void modifyText(ExtendedModifyEvent event) {
                logTextChange(event);
                updateTextStyle(event);

                if (corMode && !updatingForCor && !isSystemTextChange()) {
                    updatingForCor = true;
                    try {
                        makeCorrections();
                    } finally {
                        updatingForCor = false;
                    }
                }

                checkAutoWrap(event);
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
        try {
            parseProductText(text);
            textEditorST.setStyleRange(null);
            textEditorST.setText(text);
            lockText();
            findFramingCodes();
            textEditorST.getVerticalBar().setSelection(0);
            newProduct = false;
        } catch (JepException e) {
            statusHandler.error(PRODUCT_PARSE_ERROR, e);
            textEditorST.setText(EMPTY);
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
        Map<String, TextIndexPoints> ciMap = prodDataStruct.getCiMap();
        TextIndexPoints ciBlockTip = ciMap.get("ciblock");

        if (ciBlockTip != null) {
            startLine = ciBlockTip.getStartIndex().x;
            endLine = ciBlockTip.getEndIndex().x;
            lockLines(productTextArray, startLine, endLine);
        }

        /*
         * Lock the mnd text.
         */
        Map<String, TextIndexPoints> mndMap = prodDataStruct.getMndMap();
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
            unlockCitySegs = new HashSet<>();
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
                    if (cityTip != null
                            && cityTip.getText().indexOf("|*") > 0) {
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

    private void lockLines(String[] productTextArray, int startLine,
            int endLine) {
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
        Map<String, Object> fmtResult = TextFmtParserUtil
                .parseText(productText);
        prodDataStruct = new ProductDataStruct(fmtResult, productText);
    }

    /**
     * Patch the MND header
     *
     * @param tag
     * @param strip
     */
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
        if ("TEST".equals(tokens[0]) || "EXPERIMENTAL".equals(tokens[0])) {
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
            List<String> new_tokens = new ArrayList<>();
            for (i = 0; i < start; i++) {
                new_tokens.add(tokens[i]);
            }
            for (i = start; i < tokens.length; i++) {
                if ("TEST".equals(tokens[i])) {
                    new_tokens.add(tokens[i]);
                }
            }
            if (!tag.isEmpty()) {
                new_tokens.add(start, tag);
            }

            /*
             * Because StyledText won't allow us to replace the pline and
             * maintain the StyleRange we use for the locked text, we'll
             * temporarily disable word wrapping and lock the text control to
             * make the change.
             */
            boolean prevWrapMode = autoWrapMode;
            autoWrapMode = false;
            replaceText(tip, StringUtils.join(new_tokens, "..."));
            autoWrapMode = prevWrapMode;
        }
    }

    /**
     * @param newfield
     */
    public void updatePType(String newfield) {
        if (prodDataStruct == null) {
            return;
        }
        // Only upper case 'codes' get put in text
        if (!Character.isUpperCase(newfield.charAt(0))) {
            newfield = EMPTY;
        }

        // Find the code and the pit
        TextIndexPoints ff = prodDataStruct.getFunnyField();
        TextIndexPoints pit = prodDataStruct.getPIT();

        if (ff == null) {
            if (pit == null || newfield.length() == 0) {
                // No typecode or ci block found
                return;
            } else {
                ff = new TextIndexPoints();
                Point p = pit.getEndIndex();
                ff.addIndexPointsAndText(p.x, p.y, p.x, p.y, EMPTY);
                replaceText(ff, SPC + newfield);
            }
        } else {
            String s = SPC + newfield;
            if (!ff.getText().equals(s)) {
                replaceText(ff, s, true);
            }
        }
    }

    /**
     * Replace text
     *
     * @param tip
     *            the point at which to replace the text
     * @param text
     */
    public void replaceText(TextIndexPoints tip, String text) {
        replaceText(tip, text, false);
    }

    /**
     * Replacement of the text in the given range with new text.
     *
     * @param tip
     *            the range of text to be replaced
     * @param text
     *            the replacement text
     * @param forceReparse
     *            if true, the product text will be forced to be re-parsed.
     */
    public void replaceText(TextIndexPoints tip, String text,
            boolean forceReparse) {
        int start = prodDataStruct.positionToOffset(tip.getStartIndex());
        if (!tip.getText().equals(text)) {
            StyleRange[] ranges = textEditorST.getStyleRanges(start,
                    tip.getText().length());
            textEditorST.replaceTextRange(start, tip.getText().length(), text);

            // only reparse if we replaced with different length text or forced
            // else, replace StyleRanges since the operation is safe
            if (tip.getText().length() != text.length() || forceReparse) {
                dirty = true;
            } else {
                for (StyleRange range : ranges) {
                    textEditorST.setStyleRange(range);
                }
            }
        }
    }

    private void makeCorrections() {
        parent.setPTypeCategory(PTypeCategory.COR);
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
            if (segs == null || segs.isEmpty()) {
                return;
            }

            int offset = textEditorST.getCaretOffset();
            Pattern codePattern = Pattern.compile("\\.([A-Z]{3})\\.");
            for (SegmentData segData : segs) {
                Map<String, TextIndexPoints> segMap = segData.getSementMap();
                TextIndexPoints tipUgc = segMap.get("ugc");
                int start = prodDataStruct
                        .positionToOffset(tipUgc.getStartIndex());
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
                    if (vtec.indexOf('-') < 0) {
                        newVtec[i] = vtec;
                    } else {
                        Matcher matcher = codePattern.matcher(vtec);
                        if (matcher.find()) {
                            String code = matcher.group();
                            if (".UPG.".equals(code) || ".COR.".equals(code)) {
                                newVtec[i] = vtec;
                            } else if ((".EXP.".equals(code)
                                    || ".CAN.".equals(code)) && lineCount > 1) {
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
                    replaceText(tipVtec, String.join("\n", newVtec));
                }
                break;
            }
        } finally {
            endUpdate();
        }
    }

    /**
     * Handle the verify key event. Sent when the text is about to be modified.
     * A verify event occurs after the user has done something to modify the
     * text (typically typed a key), but before the text is modified. The doit
     * field in the verify event indicates whether or not to modify the text.
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

        int length = event.end - offset;

        boolean editingLockedText = false;
        if (length > 0) {
            editingLockedText = rangeHasLockedText(offset, length);
        } else if (event.text.length() > 0) {
            // Should not allow inserting within locked text
            editingLockedText = offsetIsWithinLockedText(offset);
        }

        if (editingLockedText && !isNonEditKey(event)) {
            event.doit = false;
            return;
        }
        
        // edit range ends right before a block of locked text
        if (rangeHasLockedText(event.end, 1)) {
            if (event.end == 0) {
                // not allow any insert
                event.doit = false;
                return;
            } else if (event.text.length() == 0) {
                // make sure text does not merge with locked text
                // and keep a line between two locked blocks
                if (!textEditorST.getTextRange(offset-1, 1).equals("\n") ||
                        rangeHasLockedText(offset-1, 1)) {
                    event.text = "\n";
                }
            } else {
                // do not allow prepending onto locked text
                if (!event.text.endsWith("\n")) {
                    event.doit = false;
                    return;
                }
            }
        }
        
        // do nothing if no change
        if (textEditorST.getTextRange(event.start, length)
                .equals(event.text)) {
            event.doit = false;
            return;
        }

        // allow edit to go through
        dirty = true;
    }

    private void updateTextStyle(ExtendedModifyEvent event) {
        if (event.start + event.length + 1 < textEditorST.getCharCount()) {
            int start = Math.max(0, event.start - 1);
            int end = Math.min(textEditorST.getCharCount() - 1,
                    event.start + event.length + 1);
            StyleRange startRange = textEditorST.getStyleRangeAtOffset(start);
            StyleRange endRange = textEditorST.getStyleRangeAtOffset(end);

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
            if (event.replacedText.contains("*")
                    || event.replacedText.contains("|")) {
                framingCodeChange = true;
            }
            // framing code was added, need to turn it red
            String newText = textEditorST.getText().substring(event.start,
                    event.start + event.length);
            if (newText.contains("*") || newText.contains("|")) {
                framingCodeChange = true;
            }

            if (framingCodeChange) {
                reParse();
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
        if (offset >= textEditorST.getCharCount()) {
            return false;
        }
        
        StyleRange[] ranges = textEditorST.getStyleRanges(offset, length);

        for (StyleRange range : ranges) {
            if (range.foreground == lockColor) {
                return true;
            }
        }

        return false;
    }
    
    /**
     * Check if offset is located within locked text.
     * 
     * @param offset
     *            The text position to check.
     * 
     * @return Whether or not offset is located within locked text.
     */
    protected boolean offsetIsWithinLockedText(int offset) {
        if (offset >= textEditorST.getCharCount())
            return false;

        StyleRange[] srs = textEditorST.getStyleRanges();

        for (StyleRange sr : srs) {
            if ((sr.foreground == lockColor) && 
                    (offset > sr.start) && 
                    (offset < (sr.start + sr.length))) {
                return true;
            }
        }
        return false;
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

            if (selectTextLocked) {
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
     * Cut the selected text if the selection does not contain locked text.
     */
    public void cutText() {
        // Do not cut text if the selection contains locked text.
        if (selectionHasLockedText()) {
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
        if (selectionHasLockedText()) {
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

    /**
     * @return the product text
     */
    public String getProductText() {
        return textEditorST.getText();
    }

    /**
     * @return the ProductDataStruct
     */
    public ProductDataStruct getProductDataStruct() {
        return prodDataStruct;
    }

    /**
     * Start update of this text comp
     *
     * Disables editing from the GUI, forces a reparse.
     */
    public void startUpdate() {
        textEditorST.setEditable(false);
        if (dirty) {
            reParse();
        }
    }

    /**
     * End update of this text comp
     *
     * Forces a reparse, enables editing from the GUI
     */
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
            String line = textEditorST.getLine(lineNum);
            int lineLength = line.length();

            if (lineLength < wrapColumn
                    && event.length >= event.replacedText.length()) {
                return;
            }

            String NL = textEditorST.getLineDelimiter();

            line += lineNum + 1 < totalLines ? NL : EMPTY;
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
            StyleRange styleRange = textEditorST
                    .getStyleRangeAtOffset(lineOff + index);
            if (styleRange != null && styleRange.foreground == lockColor) {
                return;
            }

            // deal with programmatic changes distant from the cursor
            int eventCursor = cursorOffset;
            if (eventCursor < event.start
                    || eventCursor > event.start + event.length) {
                eventCursor = event.start + event.length;
            }

            wordWrap(eventCursor, wrapColumn);

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

        Display display = getDisplay();

        // get color settings
        bgColor = loadColor(display, SETTING_BG, DFT_BG);
        fgColor = loadColor(display, SETTING_FG, DFT_FG);
        frameColor = loadColor(display, SETTING_FRAME, DFT_FRAME);
        lockColor = loadColor(display, SETTING_LOCK, DFT_LOCK);

        // It's BAD if any two editor colors are exactly the same.
        // If it happens, scold the user. The component may be unusable.
        Color[] colors = new Color[] { bgColor, fgColor, frameColor,
                lockColor };
        String[] labels = new String[] { "Background", "Foreground", "Frame",
                "Locked" };
        for (int i = 0; i < colors.length - 1; i++) {
            for (int j = i + 1; j < colors.length; j++) {
                warnIfEqual(colors[i], colors[j], labels[i], labels[j]);
            }
        }
    }

    /**
     * Query the GFEPreferences for setting. If it does not exist, use
     * colorDefault as its value. Create an SWT Color for display from the value
     * and return it.
     *
     * @param display
     *            The SWT display on which the color will appear
     * @param setting
     *            The name of the config setting to look up
     * @param colorDefault
     *            The value to use if the config setting is missing
     * @return The SWT color.
     */
    protected Color loadColor(Display display, String setting,
            String colorDefault) {
        Color color = null;
        String colorStr = GFEPreference.getString(setting, colorDefault);
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
            String msg = String.format(
                    "%s color is identical to %s color. Editing may not work properly.",
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
     * @param cursorIndex
     *            The cursor index
     * @param width
     *            The width to which to word wrap
     * @return An array of integers giving:
     *         <ol>
     *         <li value=0>The index in the old content of the first character
     *         </li>
     *         <li>The index in the old content of the last character</li>
     *         <li>The length of the replacemetruent text</li>
     *         </ol>
     */
    public int[] wordWrap(int cursorIndex, int width) {
        StyledText st = getTextEditorST();

        final Matcher PARA_MATCHER = PARA_SEP_PATTERN.matcher("");
        final Matcher FUNNY_SEP_MATCHER = FUNNY_SEP_PATTERN.matcher("");
        final Matcher COLON_END_MATCHER = COLON_END_PATTERN.matcher("");
        final String NL = st.getLineDelimiter();

        String line;

        int startIndex = -1;
        int lastLineOffset = -1;
        String initialIndent = "";
        // search backwards for a paragraph separator
        for (int searchLine = st
                .getLineAtOffset(cursorIndex); searchLine >= 0; searchLine--) {
            line = st.getLine(searchLine);
            int lineOffset = st.getOffsetAtLine(searchLine);

            // if line contains locked text, quit looking.
            if (rangeHasLockedText(lineOffset, line.length())) {
                break;
            }

            // if a colon-terminated line is found, start from next line
            if (lastLineOffset != -1) {
                COLON_END_MATCHER.reset(line);
                if (COLON_END_MATCHER.find()) {
                    startIndex = lastLineOffset;
                    break;
                }
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

            lastLineOffset = lineOffset;
        }

        if (startIndex < 0) {
            startIndex = st.getOffsetAtLine(st.getLineAtOffset(cursorIndex));
        }

        // Find the end index of the wrap
        int endIndex = -1;
        int cursorLine = st.getLineAtOffset(cursorIndex);
        int lineCursorPos = cursorIndex - st.getOffsetAtLine(cursorLine);
        for (int searchLine = cursorLine; searchLine <= st
                .getLineAtOffset(st.getCharCount()); searchLine++) {

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

            // colon terminated line ends paragraph
            COLON_END_MATCHER.reset(line);
            if (COLON_END_MATCHER.find()) {
                break;
            }
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

        String subsequentIndent = initialIndent.replaceAll(".", " ");

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

        Map<String, Object> args = new HashMap<>();
        args.put("initial_indent", initialIndent);
        args.put("subsequent_indent", subsequentIndent);
        args.put("break_long_words", Boolean.FALSE);
        args.put("width", Integer.valueOf(width));

        Map<String, Object> preArgs = new HashMap<>();
        preArgs.put("text", pre);

        try {
            pre = wrapper.callWordWrapPython(args, preArgs);
        } catch (Exception e) {
            statusHandler.error("Python error wrapping text preceding cursor:",
                    e);
        }

        if (pre.isEmpty() && !initialIndent.isEmpty()) {
            pre = initialIndent;
            lchar = "";
        }

        pre += lchar;

        String postInitialIndent = "";
        int lastNL = pre.lastIndexOf(NL);
        postInitialIndent = " "
                + pre.substring(lastNL + NL.length()).replaceAll(".", " ");

        args.put("initial_indent", postInitialIndent);
        args.put("drop_whitespace", Boolean.FALSE);

        Map<String, Object> postArgs = new HashMap<>();
        postArgs.put("text", post);

        try {
            post = wrapper.callWordWrapPython(args, postArgs);
        } catch (Exception e) {
            statusHandler.error("Python error wrapping text after cursor:", e);
        }

        /*
         * We'll strip any leading whitespace but specifically not any new lines
         * in case the text before the cursor was exactly the length of our wrap
         * limit.
         */
        Pattern stripLeadingSpaces = Pattern.compile("^\\p{Blank}+",
                Pattern.MULTILINE);
        post = stripLeadingSpaces.matcher(post).replaceAll("");
        if (!subsequentIndent.isEmpty()) {
            post = post.replaceAll(NL, NL + subsequentIndent);
        }

        String text = pre + rchar + post;
        st.replaceTextRange(startIndex, 1 + endIndex - startIndex, text);

        int newCaretOffset = startIndex + pre.length();
        st.setCaretOffset(newCaretOffset);

        return new int[] { startIndex, endIndex, text.length() };
    }

    protected void logTextChange(ExtendedModifyEvent event) {
        StyledText st = (StyledText) event.widget;
        String oldText = event.replacedText;
        String newText = "";
        if (event.length > 0) {
            newText = st.getText(event.start, event.start + event.length - 1);
        }

        if (!newText.equals(oldText)) {
            peLog.logEdit(event.start, oldText, newText);
        }

    }
}
