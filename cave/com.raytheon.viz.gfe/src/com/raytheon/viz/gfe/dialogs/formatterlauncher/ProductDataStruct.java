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
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.eclipse.swt.graphics.Point;

/**
 * This class takes the information from the parsed product text and stores the
 * information in separate, more organized containers.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 05 Jan 2008  1784       lvenable    Initial creation
 * 28 Jan 2015  4018       randerso    Code cleanup
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class ProductDataStruct {
    /**
     * Array of framing codes text index points.
     */
    private List<TextIndexPoints> frames;

    /**
     * HashMap of ci text index points.
     */
    private Map<String, TextIndexPoints> ci;

    /**
     * HashMap of mnd text index points.
     */
    private Map<String, TextIndexPoints> mnd;

    /**
     * Array of segment data.
     */
    private List<SegmentData> segments;

    /**
     * Parsed map containing the parsed product text.
     */
    private Map<String, Object> parsedMap;

    /**
     * A String array containing the product text. Each element in the array is
     * one line from the product text.
     */
    private String[] productTextArray;

    /**
     * The product text as one string.
     */
    private String productText;

    private final String nwsTimeStr = "nwstime";

    /**
     * Constructor.
     * 
     * @param parsedMap
     *            The parsed product text.
     * @param productText
     *            Product text.
     */
    public ProductDataStruct(Map<String, Object> parsedMap, String productText) {
        this.parsedMap = parsedMap;

        this.productText = productText;

        if (productText != null) {
            productTextArray = productText.split("\n");
        }

        extractData();
    }

    /**
     * Extract the data from the parsed map.
     */
    private void extractData() {
        extractFrames();
        extractCi();
        extractMnd();
        extractSegments();
    }

    /**
     * Extracted the framing codes.
     */
    @SuppressWarnings("unchecked")
    private void extractFrames() {
        if (parsedMap.containsKey("frames") == false) {
            return;
        }

        if ((parsedMap.get("frames") instanceof List) == false) {
            return;
        }

        frames = new ArrayList<TextIndexPoints>();

        List<Map<String, List<List<Integer>>>> parseFrames = (List<Map<String, List<List<Integer>>>>) parsedMap
                .get("frames");

        for (Map<String, List<List<Integer>>> frameMap : parseFrames) {
            Set<String> keys = frameMap.keySet();

            for (String key : keys) {
                List<List<Integer>> frameIndexes = frameMap.get(key);

                TextIndexPoints tip = createTextIndexPoints(frameIndexes);

                frames.add(tip);
            }
        }
    }

    /**
     * Extracted the CI information.
     */
    @SuppressWarnings("unchecked")
    private void extractCi() {

        ci = new HashMap<String, TextIndexPoints>();

        if (parsedMap.containsKey("ci") == false) {
            return;
        }

        List<List<Integer>> tmpArray;
        TextIndexPoints tip;

        Map<String, List<List<Integer>>> parsedCi = (Map<String, List<List<Integer>>>) parsedMap
                .get("ci");

        Set<String> keys = parsedCi.keySet();

        for (String key : keys) {
            tmpArray = parsedCi.get(key);
            tip = createTextIndexPoints(tmpArray);
            ci.put(key, tip);
        }
    }

    /**
     * Extract the MND information.
     */
    @SuppressWarnings("unchecked")
    private void extractMnd() {
        mnd = new HashMap<String, TextIndexPoints>();

        if (parsedMap.containsKey("mnd") == false) {
            return;
        }

        List<List<Integer>> tmpArray;
        TextIndexPoints tip;

        Map<String, List<List<Integer>>> parsedMnd = (Map<String, List<List<Integer>>>) parsedMap
                .get("mnd");

        Set<String> keys = parsedMnd.keySet();

        for (String key : keys) {
            tmpArray = parsedMnd.get(key);
            tip = createTextIndexPoints(tmpArray);

            mnd.put(key, tip);
        }
    }

    /**
     * Extract the segments.
     */
    @SuppressWarnings("unchecked")
    private void extractSegments() {
        segments = new ArrayList<SegmentData>();

        if (parsedMap.containsKey("segs") == false) {
            return;
        }

        TextIndexPoints tip;
        List<List<Integer>> tmpArray;

        // Get the Array of segments from the parsed map.
        List<Map<String, Object>> parsedSegs = (List<Map<String, Object>>) parsedMap
                .get("segs");

        // Loop through each segment.
        for (Map<String, Object> curSegMap : parsedSegs) {

            Set<String> keys = curSegMap.keySet();

            SegmentData segData = new SegmentData();

            for (String key : keys) {
                if (key.equals("headInfo")) {
                    List<Map<String, List<List<Integer>>>> headInfoArray = (List<Map<String, List<List<Integer>>>>) curSegMap
                            .get(key);

                    for (Object hiObj : headInfoArray) {
                        Map<String, List<List<Integer>>> headInfoMap = (Map<String, List<List<Integer>>>) hiObj;

                        Set<String> headInfoKeys = headInfoMap.keySet();
                        for (String hiKey : headInfoKeys) {
                            tmpArray = headInfoMap.get(hiKey);
                            tip = createTextIndexPoints(tmpArray);
                            segData.addToHeadInfoMap(hiKey, tip);
                        }
                    }
                } else {
                    tmpArray = (List<List<Integer>>) curSegMap.get(key);
                    tip = createTextIndexPoints(tmpArray);
                    segData.addToSegmentMap(key, tip);
                }
            }

            segments.add(segData);
        }
    }

    /**
     * Create Text Index Points data from the array of indexes passed in.
     * 
     * @param tmpArray
     *            Array of indexes.
     * @return TextIndexPoint data.
     */
    private TextIndexPoints createTextIndexPoints(List<List<Integer>> tmpArray) {
        TextIndexPoints tip = new TextIndexPoints();

        // Get the starting index
        List<Integer> startPoints = tmpArray.get(0);
        int startLine = (startPoints.get(0)) - 1;
        int startCol = startPoints.get(1);

        // Get the ending index
        List<Integer> endPoints = tmpArray.get(1);
        int endLine = (endPoints.get(0)) - 1;
        int endCol = (endPoints.get(1));

        String text = getIndexString(startLine, startCol, endLine, endCol);

        tip.addIndexPointsAndText(startLine, startCol, endLine, endCol, text);

        return tip;
    }

    /**
     * Get the string from the product text array given the starting line,
     * starting column, ending line, and ending column.
     * 
     * @param startLine
     *            Starting line.
     * @param startCol
     *            Starting column.
     * @param endLine
     *            Ending line.
     * @param endCol
     *            Ending column.
     * @return The string from the product text array.
     */
    private String getIndexString(int startLine, int startCol, int endLine,
            int endCol) {
        StringBuilder str = new StringBuilder();
        boolean getToEndOfLine = false;

        if (endCol == 0) {
            --endLine;
        }

        for (int i = startLine; i <= endLine; i++) {
            if (i != startLine) {
                startCol = 0;
            }

            if (i != endLine) {
                getToEndOfLine = true;
            } else {
                if (endCol == 0) {
                    getToEndOfLine = true;
                } else {
                    getToEndOfLine = false;
                }
            }

            if (getToEndOfLine == true) {
                str.append(productTextArray[i].substring(startCol));
            } else {

                int endColOffset = 0;
                if ((endCol - 1) == productTextArray[i].length()) {
                    endColOffset = -1;
                }

                str.append(productTextArray[i].substring(startCol, endCol
                        + endColOffset));
            }

            if (i != endLine) {
                str.append("\n");
            }
        }

        return str.toString();
    }

    /**
     * Get the index of a string in the product text.
     * 
     * @param str
     *            String to get the index of.
     * @param offset
     *            Offset into the string.
     * @return The index of the string from the given offset.
     */
    public int getStringIndexInProduct(String str, int offset) {
        return productText.indexOf(str, offset);
    }

    /**
     * Get the framing codes array.
     * 
     * @return The framing codes array.
     */
    public List<TextIndexPoints> getFramingCodes() {
        return frames;
    }

    /**
     * Get the CI map.
     * 
     * @return The CI map.
     */
    public Map<String, TextIndexPoints> getCiMap() {
        return ci;
    }

    /**
     * Get the MND map.
     * 
     * @return The MND map.
     */
    public Map<String, TextIndexPoints> getMndMap() {
        return mnd;
    }

    /**
     * Get the array of segments.
     * 
     * @return
     */
    public List<SegmentData> getSegmentsArray() {
        return segments;
    }

    /**
     * Get the product text in array format. Each element of the array is one
     * line of text from the product text.
     * 
     * @return String array of line from the product text.
     */
    public String[] getProductTextArray() {
        return productTextArray;
    }

    public boolean hasFramingCodes() {
        if (frames == null) {
            return false;
        }

        if (frames.size() == 0) {
            return false;
        }

        return true;
    }

    /**
     * Print the data from the maps/array. This is for debugging purposes.
     */
    @SuppressWarnings("unused")
    public void printData() {
        System.out.println("**** PRINT START ********************************");
        System.out.println("****");

        /*
         * Print the Frames information.
         */
        System.out.println("");
        System.out.println("--- frames");
        for (TextIndexPoints frameData : frames) {
            System.out.println("frame text = " + frameData.getText());
        }

        /*
         * Print the CI information.
         */
        System.out.println("");
        System.out.println("--- ci");
        Set<String> ciKeys = ci.keySet();

        for (String key : ciKeys) {
            System.out.println("key = " + key);
            System.out.println("text = " + ci.get(key).getText());
        }

        /*
         * Print the MND information.
         */
        System.out.println("");
        System.out.println("--- mnd");
        Set<String> mndKeys = mnd.keySet();

        for (String key : mndKeys) {
            System.out.println("key = " + key);
            System.out.println("text = " + mnd.get(key).getText());
        }

        /*
         * Print the Segments information.
         */
        System.out.println("");
        System.out.println("--- segments");
        for (SegmentData segData : segments) {
            System.out.println("++++++ segment map");
            Map<String, TextIndexPoints> segMap = segData.getSementMap();

            Set<String> segMapKeys = segMap.keySet();
            for (String segMapKey : segMapKeys) {
                TextIndexPoints tip = segMap.get(segMapKey);
                System.out.println("SegMapKey = " + segMapKey);
                System.out.println("Text = " + segMap.get(segMapKey).getText());
            }

            System.out.println("++++++ headinfo map");
            Map<String, TextIndexPoints> headInfoMap = segData.getHeadInfoMap();

            Set<String> headInfoKeys = headInfoMap.keySet();
            for (String headInfoKey : headInfoKeys) {
                System.out.println("headInfoKey = " + headInfoKey);
                System.out.println("Text = "
                        + headInfoMap.get(headInfoKey).getText());
            }
        }

        System.out.println("**** PRINT END **********************************");
    }

    public String getWmoId() {
        if (ci.get("wmoid") != null) {
            return ci.get("wmoid").getText();
        }
        return null;
    }

    public String getFullStationId() {
        if (ci.get("fsid") != null) {
            return ci.get("fsid").getText();
        }
        return null;
    }

    public String getPil() {
        if (ci.get("pil") != null) {
            return ci.get("pil").getText();
        }
        return null;
    }

    public TextIndexPoints getProductType() {
        return mnd.get("pline");
    }

    public TextIndexPoints getFunnyField() {
        return ci.get("funnyfield");
    }

    public TextIndexPoints getPIT() {
        return ci.get("pit");
    }

    public int positionToOffset(int line, int column) {
        if (line >= productTextArray.length) {
            return -1;
        }

        int index = 0;
        for (int i = 0; i < line; i++) {
            index += productTextArray[i].length() + 1;
        }
        index += column;
        return index;
    }

    public int positionToOffset(Point p) {
        return positionToOffset(p.x, p.y);
    }

    public Point offsetToPosition(int offset) {
        int line = 0;
        int column = 0;
        for (line = 0; line < productTextArray.length; line++) {
            int llen = productTextArray[line].length() + 1;
            if (offset < llen) {
                break;
            }
            offset -= llen;
        }
        column = offset;
        return new Point(line, column);
    }

    public String getMndNwsTimeStr() {
        return nwsTimeStr;
    }
}
