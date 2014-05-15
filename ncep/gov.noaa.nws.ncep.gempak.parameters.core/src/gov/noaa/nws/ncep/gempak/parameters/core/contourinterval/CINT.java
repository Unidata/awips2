package gov.noaa.nws.ncep.gempak.parameters.core.contourinterval;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * CINT accepts strings (in its overloaded constructor) matching one of the two
 * formats below:
 * 
 * <pre>
 * <tt> contourInterval/minimumContourValue/maximumContourValue/numPaddingDigits<p>
 *  contourVal1;contourVal2;...;contourValn
 *  Multiple zoom levels (up to 5 levels) can be entered by separating the above input string formats with a  ">"
 *  sign 
 *  </tt>
 * 
 * <tt>CINT</tt> returns a list of contour levels (and/or contour interval,
 * minimum and maximum contour levels) only if the method <tt>isCINTStringParsed()</tt> returns true.
 * Hence before attempting to use a numeric value returned by any method in <tt>CINT</tt>, it is strongly
 * advised that the user checks the result of the method <tt>isCINTStringParsed()</tt>.
 * </pre>
 * <p>
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date          Ticket#     Engineer     Description
 * ------------ ---------- ----------- --------------------------
 * 12-Nov-2009    174        Archana.S   Initial Creation
 * 07-Jun-2010    174        Archana.S   Renamed the package as
 *                                                            gov.noaa.nws.ncep.gempak.parameters.contourinterval
 * 10-Jun-2010    174        Archana.S   Added methods generateContourValueListAsString(),
 *                                                            setContourValueListAsStringList() and 
 *                                                            getContourValueListAsStringList()  
 * 11-Jun-2010    174        Archana.S   Added method addLeadingZeros() and 
 *                                                           updated generateContourValueListAsString() accordingly
 * 14-Jun-2010    174        Archana.S   Redesigned class to support multiple zoom levels                                          
 *  30-Jul-2010    174      Archana.S     Added the zoom-level as an enum. Updated design to
 *                                                          access the list of contour values, the contour interval, 
 *                                                          min and max contour values separately from each zoom level.
 *                                                          Implemented each zoom level as a CNT object in a list of CINT objects.
 * 17-May-2011				M. Li		 Created a parseCINT to simplify CINT parsing.
 * 07-Apr-2014   TTR-938   D.Sushon      Added check for null string to constructor, fixing NullPointerException
 *                                       thrown when attempting to initialize with null String.
 * 
 * 
 * </pre>
 * 
 * @author Archana.S
 * @version 1
 * @see $GEMPAK/help/hlx/cint.hl2
 */
public class CINT {

    /** The object responsible for parsing the CINT string */
    private ContourStringParser cintParser;

    /** The input CINT string */
    private String contourIntervalString;

    /** A real number that represents the contour interval */
    private Double contourInterval;

    /** A real number that represents the minimum contour level */
    private Double minContourValue;

    /** A real number that represents the maximum contour level */
    private Double maxContourValue;

    /** Boolean flag to validate that the CINT string was parsed correctly */
    private boolean isCINTStringParsed;

    /**
     * An integer that decides the minimum number of digits in an integer
     * contour label
     */
    private Integer numPaddingDigits;

    /** The unparsed CINT string entered by the user */
    String userInputString;

    public String getUserInputString() {
        return userInputString;
    }

    private void setUserInputString(String userInputString) {
        this.userInputString = new String(userInputString);
    }

    /** A list of CINT objects where each CINT object represents one zoom level */
    private List<CINT> listOfCINTObjects = new ArrayList<CINT>(0);

    /** The list of contour values */
    private List<Double> contourValuesList;

    /** The list of contour values represented as String objects */
    private List<String> contourValuesListAsString;

    /**
     * The list of extracted contour labels. For CINT strings without labels, it
     * is the contour value stored as its String equivalent
     */
    private List<String> contourLabelList;

    private List<String> getContourLabelList() {
        return contourLabelList;
    }

    private void setContourLabelList(List<String> contourLabelList) {
        this.contourLabelList = contourLabelList;
    }

    /** A HashMap of the contour values and their labels */
    private Map<Double, String> cintHashMap;

    /** @return a list of CINT objects, each of which represents one zoom level */
    public List<CINT> getListOfCINTObjects() {
        return (new ArrayList<CINT>(listOfCINTObjects));
    }

    /**
     * Gets the {@code HashMap<Double, String>} of the contour values and labels
     * for a specific zoom level
     * 
     * @param The
     *            zoom level
     * @return The {@code HashMap<Double, String>} of the contour values and
     *         labels for the input zoom level if the CINT object for that zoom
     *         level exists or an empty map otherwise.
     * */
    public Map<Double, String> getCintHashMap(ZoomLevel zLevel) {
        int listSize = this.listOfCINTObjects.size();
        if (listSize >= zLevel.zoomLevel) {
            Map<Double, String> thisMap = new HashMap<Double, String>(
                    this.listOfCINTObjects.get(zLevel.zoomLevel - 1).cintHashMap);
            if (thisMap.size() > 0) {
                return (thisMap);
            }
        }
        return Collections.emptyMap();
    }

    /***
     * 
     * @param cintHashMap
     */
    private void setCintHashMap(Map<Double, String> cintHashMap) {
        this.cintHashMap = cintHashMap;
    }

    /**
     * Gets the {@code List<Double>} of the contour values for a specific zoom
     * level
     * 
     * @param the
     *            zoom level
     * @return The {@code List<Double>} of the contour values for the input zoom
     *         level if the CINT object for that zoom level exists or an empty
     *         list otherwise.
     * */
    public List<Double> getContourValuesListAsDouble(ZoomLevel zLevel) {
        int listSize = this.listOfCINTObjects.size();

        if (listSize >= zLevel.zoomLevel) {
            List<Double> cList = new ArrayList<Double>(
                    this.listOfCINTObjects.get(zLevel.zoomLevel - 1).contourValuesList);
            if (cList.size() > 0) {
                return (new ArrayList<Double>(cList));
            }
        }
        return Collections.emptyList();
    }

    /***
     * 
     * @param contourValuesList
     */
    private void setContourValuesList(List<Double> contourValuesList) {
        this.contourValuesList = new ArrayList<Double>(contourValuesList);
    }

    /** An enumeration to define the 5 zoom levels allowed in CINT */
    public static enum ZoomLevel {
        FIRST(1), SECOND(2), THIRD(3), FOURTH(4), FIFTH(5);
        private int zoomLevel;

        private ZoomLevel(int index) {
            this.zoomLevel = index;
        }

        public int getZoomLevelAsInt() {
            return zoomLevel;
        }
    }

    /** Zoom constant representing the first zoom level */
    public static final ZoomLevel FIRST_ZOOM_LEVEL = ZoomLevel.FIRST;

    /** Zoom constant representing the second zoom level */
    public static final ZoomLevel SECOND_ZOOM_LEVEL = ZoomLevel.SECOND;

    /** Zoom constant representing the third zoom level */
    public static final ZoomLevel THIRD_ZOOM_LEVEL = ZoomLevel.THIRD;

    /** Zoom constant representing the fourth zoom level */
    public static final ZoomLevel FOURTH_ZOOM_LEVEL = ZoomLevel.FOURTH;

    /** Zoom constant representing the fifth zoom level */
    public static final ZoomLevel FIFTH_ZOOM_LEVEL = ZoomLevel.FIFTH;

    /**
     * Zoom constant representing the first zoom level as the minimum level of
     * zoom
     */
    public static final ZoomLevel MIN_ZOOM_LEVEL = ZoomLevel.FIRST;

    /**
     * Zoom constant representing the fifth zoom level as the maximum level of
     * zoom
     */
    public static final ZoomLevel MAX_ZOOM_LEVEL = ZoomLevel.FIFTH;

    /**
     * Gets the {@code List<String>} of the contour values for a specific zoom
     * level
     * 
     * @param the
     *            zoom level
     * @return The {@code List<String>} of the contour values for the input zoom
     *         level if the CINT object for that zoom level exists or an empty
     *         list otherwise.
     * */
    public List<String> getContourValuesListAsString(ZoomLevel zLevel) {
        List<String> cvList = Collections.emptyList();
        int listSize = this.listOfCINTObjects.size();
        if (listSize > 0 && listSize >= zLevel.zoomLevel) {
            cvList = new ArrayList<String>(
                    this.listOfCINTObjects.get(zLevel.zoomLevel - 1).contourValuesListAsString);
        }
        return cvList;
    }

    /**
     * Gets the {@code List<String>} of the contour labels for a specific zoom
     * level
     * 
     * @param the
     *            zoom level
     * @return The {@code List<String>} of the contour labels for the input zoom
     *         level if the CINT object for that zoom level exists or an empty
     *         list otherwise.
     * */
    public List<String> getContourLabelsForZoomLevel(ZoomLevel zLevel) {
        List<String> cintLabelList = new ArrayList<String>(0);
        int listSize = this.listOfCINTObjects.size();
        if (listSize >= zLevel.zoomLevel) {
            cintLabelList = new ArrayList<String>(this.listOfCINTObjects.get(
                    zLevel.zoomLevel - 1).getContourLabelList());
        }
        return cintLabelList;
    }

    /**
     * The default constructor initializes the instance variables to their
     * defaults
     **/
    public CINT() {
        setContourInterval(Double.NaN);
        setMinContourValue(Double.NaN);
        setMaxContourValue(Double.NaN);
        isCINTStringParsed = false;
        cintParser = new ContourStringParser();
        cintHashMap = new HashMap<Double, String>(0);
        contourValuesList = new ArrayList<Double>(0);
        contourValuesListAsString = new ArrayList<String>(0);
        contourLabelList = new ArrayList<String>(0);
    }

    /**
     * The overloaded constructor accepts the CINT string as an input and calls
     * the parse method of the ContourStringParser on it. If the parsing is
     * successful, the contour interval, minimum contour level, maximum contour
     * level and the list of contour values will be populated by the
     * corresponding parsed data from the ContourStringParser object.
     * 
     **/
    public CINT(String contourIntervalString) {

        /* Initialize instance variables */
        setContourInterval(Double.NaN);
        setMinContourValue(Double.NaN);
        setMaxContourValue(Double.NaN);
        contourValuesList = new ArrayList<Double>(0);
        contourValuesListAsString = new ArrayList<String>(0);
        contourLabelList = new ArrayList<String>(0);

        if (null == contourIntervalString) {
            isCINTStringParsed = false;
            cintParser = new ContourStringParser();
            cintHashMap = new HashMap<Double, String>(0);
        } else {
            setUserInputString(contourIntervalString);
            cintParser = new ContourStringParser();
        }

        parseAndSetAttributes(contourIntervalString);

    }

    public static List<Double> parseCINT(String cint, int zoomLevelIndex,
            float minValue, float maxValue) {

        List<Double> cvalues = null;
        Double dcint = null;
        int comparison = 0;

        /*
         * Check cint for a zero value
         */

        try {
            dcint = new Double(cint);
            comparison = dcint.compareTo(new Double(0));

            if (comparison == 0) {
                // cint is zero, return an empty list, no contours
                cvalues = new ArrayList<Double>(0);
                return cvalues;
            }

        } catch (java.lang.NumberFormatException e) {
            // do nothing
        }

        /*
         * Convert zoomLevel index
         */
        ZoomLevel zoomLevel = CINT.FIRST_ZOOM_LEVEL;

        switch (zoomLevelIndex) {
        case 1:
            zoomLevel = CINT.FIRST_ZOOM_LEVEL;
            break;
        case 2:
            zoomLevel = CINT.SECOND_ZOOM_LEVEL;
            break;
        case 3:
            zoomLevel = CINT.THIRD_ZOOM_LEVEL;
            break;
        case 4:
            zoomLevel = CINT.FOURTH_ZOOM_LEVEL;
            break;
        case 5:
            zoomLevel = CINT.FIFTH_ZOOM_LEVEL;
            break;
        }

        /*
         * Get contour values from CINT
         */

        Double cmin = new Double(minValue);
        Double cmax = new Double(maxValue);
        Double interval = null;

        if (cint == null || cint.trim().length() < 1) {
            interval = (cmax - cmin) / 10.0;
            CINT contourInfo = new CINT(interval.toString() + "/"
                    + cmin.toString() + "/" + cmax.toString());
            cvalues = contourInfo
                    .getUniqueSortedContourValuesFromAllZoomLevels();
        } else {
            CINT contourInfo = new CINT(cint);
            cvalues = contourInfo.getContourValuesListAsDouble(zoomLevel);

            if (cvalues == null || cvalues.size() < 1 /*
                                                       * || contourInfo.
                                                       * getContourInterval
                                                       * (zoomLevel) == 0.0
                                                       */) {

                cmin = contourInfo.getMinContourValue(zoomLevel);
                if (cmin == null || cmin.isNaN())
                    cmin = new Double(minValue);

                cmax = contourInfo.getMaxContourValue(zoomLevel);
                if (cmax == null || cmax.isNaN())
                    cmax = new Double(maxValue);

                interval = contourInfo.getContourInterval(zoomLevel);
                if (interval == null || interval.isNaN()) {
                    interval = (cmax - cmin) / 10.0;
                }

                // Only allow less than 50 contour levels
                if ((cmax - cmin) / interval > 50)
                    interval = (cmax - cmin) / 50;

                // System.out.println("  cmax=="+cmax);
                contourInfo = new CINT(interval.toString() + "/"
                        + cmin.toString() + "/" + cmax.toString());
                cvalues = contourInfo
                        .getUniqueSortedContourValuesFromAllZoomLevels();
            }
        }

        return cvalues;
    }

    /** @return boolean isCINTStringParsed */
    public boolean isCINTStringParsed() {
        return isCINTStringParsed;
    }

    /**
     * @return The portion of the parsed CINT string specific to a zoom level if
     *         the CINT object for that zoom level exists or an empty String
     *         otherwise.
     * */
    public String getCINTString(ZoomLevel zLevel) {
        String currentCINTString = "";
        int listSize = this.listOfCINTObjects.size();
        if (listSize >= zLevel.zoomLevel) {
            currentCINTString = new String(
                    this.listOfCINTObjects.get(zLevel.zoomLevel - 1).contourIntervalString);
        }
        return currentCINTString;
    }

    /**
     * @return The contour interval specific to a zoom level if the CINT object
     *         for that zoom level exists or NaN otherwise.
     * */
    public Double getContourInterval(ZoomLevel zLevel) {
        Double currentContourInterval = Double.NaN;
        int listSize = this.listOfCINTObjects.size();
        if (listSize >= zLevel.zoomLevel) {
            currentContourInterval = new Double(
                    this.listOfCINTObjects.get(zLevel.zoomLevel - 1).contourInterval);
        }
        return currentContourInterval;
    }

    /**
     * @return The minimum contour value specific to a zoom level if the CINT
     *         object for that zoom level exists or NaN otherwise.
     * */
    public Double getMinContourValue(ZoomLevel zLevel) {
        Double currentMinContourValue = Double.NaN;
        int listSize = this.listOfCINTObjects.size();
        if (listSize >= zLevel.zoomLevel) {
            currentMinContourValue = new Double(
                    this.listOfCINTObjects.get(zLevel.zoomLevel - 1).minContourValue);
        }
        return currentMinContourValue;
    }

    /**
     * @return The maximum contour value specific to a zoom level if the CINT
     *         object for that zoom level exists or NaN otherwise.
     * */
    public Double getMaxContourValue(ZoomLevel zLevel) {
        Double currentMaxContourValue = Double.NaN;
        int listSize = this.listOfCINTObjects.size();
        if (listSize >= zLevel.zoomLevel) {
            currentMaxContourValue = new Double(
                    this.listOfCINTObjects.get(zLevel.zoomLevel - 1).maxContourValue);
        }
        return currentMaxContourValue;
    }

    /**
     * @return The minimum digits in the label for contour values specific to a
     *         zoom level if the CINT object for that zoom level exists or 0
     *         otherwise.
     * */
    public Integer getNumPaddingDigits(ZoomLevel zLevel) {
        Integer currentNumPaddingDigits = new Integer(0);
        int listSize = this.listOfCINTObjects.size();
        if (listSize >= zLevel.zoomLevel) {
            currentNumPaddingDigits = new Integer(
                    this.listOfCINTObjects.get(zLevel.zoomLevel - 1).numPaddingDigits);
        }
        return currentNumPaddingDigits;
    }

    /***
     * 
     * @return a list String objects representing the unique contour values from
     *         all zoom levels
     */
    public List<String> getUniqueSortedContourValuesFromAllZoomLevelsAsString() {
        List<Double> sortedKeySet = getUniqueSortedContourValuesFromAllZoomLevels();
        List<String> contourValList = new ArrayList<String>(0);
        if (sortedKeySet.size() > 0) {
            for (Double contourValue : sortedKeySet) {
                contourValList.add(contourValue.toString());
            }
        }
        return contourValList;
    }

    /***
     * 
     * @return a list of Double objects representing the unique contour values
     *         from all zoom levels
     */
    public List<Double> getUniqueSortedContourValuesFromAllZoomLevels() {
        Set<Double> setOfUnqiueContourValues = new HashSet<Double>(0);
        List<Double> sortedList = new ArrayList<Double>(0);

        int length = this.listOfCINTObjects.size();
        for (int index = 0; index < length; index++) {
            CINT currentCINTObj = this.listOfCINTObjects.get(index);
            setOfUnqiueContourValues.addAll(currentCINTObj.contourValuesList);

            sortedList = new ArrayList<Double>(setOfUnqiueContourValues);
            Collections.sort(sortedList);
        }
        return sortedList;
    }

    private void setContourValuesListAsString(
            List<String> contourValuesListAsString) {
        this.contourValuesListAsString = contourValuesListAsString;
    }

    /** @param boolean isCINTStringParsed */
    private void setCINTStringParsed(boolean isCINTStringParsed) {
        this.isCINTStringParsed = isCINTStringParsed;
    }

    /**
     * @param String
     *            cintString
     */
    private void setCINTString(String cintString) {
        contourIntervalString = new String(cintString);
    }

    /**
     * @param Double
     *            contourInterval
     */
    private void setContourInterval(Double contourInterval) {
        this.contourInterval = new Double(contourInterval);
    }

    /**
     * @param Double
     *            minContourValue
     */
    private void setMinContourValue(Double minContourValue) {
        this.minContourValue = new Double(minContourValue);
    }

    /**
     * @param Double
     *            maxContourValue
     */
    private void setMaxContourValue(Double maxContourValue) {
        this.maxContourValue = new Double(maxContourValue);
    }

    /**
     * @param Integer
     *            numPaddingDigits
     **/
    private void setNumPaddingDigits(Integer numPaddingDigits) {
        this.numPaddingDigits = new Integer(numPaddingDigits);
    }

    /***
     * 
     * @param inputStr
     */
    private void parseAndSetAttributes(String inputStr) {
        boolean isParsed = false;
        if (inputStr != null) {
            String contourLevelStringsArray[] = inputStr.split(">");
            int lengthOfContourLevelStringsArray = contourLevelStringsArray.length;
            if (lengthOfContourLevelStringsArray > CINT.MAX_ZOOM_LEVEL.zoomLevel) {
                lengthOfContourLevelStringsArray = CINT.MAX_ZOOM_LEVEL.zoomLevel;
            }

            listOfCINTObjects = new ArrayList<CINT>(
                    lengthOfContourLevelStringsArray);

            for (int index = 0; index < lengthOfContourLevelStringsArray; index++) {

                // /*Invoke the parse method of ContourStringParser*/
                cintParser.parse(contourLevelStringsArray[index]);

                // create the CINT object for the current zoom level
                CINT currentCINTObj = new CINT();
                currentCINTObj.setCINTStringParsed(cintParser
                        .isContourStringParsed());

                /*
                 * If the parse operation was successful, extract the numeric
                 * data and set the corresponding instance variables of
                 * currentCINTObj
                 */

                if (currentCINTObj.isCINTStringParsed()) {
                    currentCINTObj
                            .setCINTString(contourLevelStringsArray[index]);
                    currentCINTObj.setContourInterval(cintParser
                            .getContourInterval());
                    currentCINTObj.setMinContourValue(cintParser
                            .getMinContourLevel());
                    currentCINTObj.setMaxContourValue(cintParser
                            .getMaxContourLevel());
                    currentCINTObj.setNumPaddingDigits(cintParser
                            .getNumPaddingDigits());
                    currentCINTObj.setContourValuesList(cintParser
                            .getContourValuesList());
                    currentCINTObj.setCintHashMap(cintParser
                            .getLabeledContourValuesHashMap());
                    Set<Double> tempKeySet = new LinkedHashSet<Double>(
                            currentCINTObj.cintHashMap.keySet());
                    currentCINTObj.setContourValuesList(new ArrayList<Double>(
                            tempKeySet));
                    for (Double contourValue : tempKeySet) {
                        currentCINTObj.contourValuesListAsString
                                .add(contourValue.toString());
                    }
                    currentCINTObj.contourLabelList = new ArrayList<String>(
                            new LinkedHashSet<String>(
                                    currentCINTObj.cintHashMap.values()));
                }

                if (index == 0) {
                    isParsed = currentCINTObj.isCINTStringParsed();
                } else {
                    isParsed = isParsed & currentCINTObj.isCINTStringParsed();
                }

                // Sets the status of the parse operations across all zoom
                // levels (i.e. for all CINT objects in the list)
                this.setCINTStringParsed(isParsed);

                // finally add currentCINTObj to the list of CINT objects
                listOfCINTObjects.add(currentCINTObj);
            }
        }
    }

}
