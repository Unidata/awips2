package gov.noaa.nws.ncep.gempak.parameters.core.contourinterval;

import gov.noaa.nws.ncep.gempak.parameters.core.util.StringUtil;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;

/**
 * 
 * <tt>ContourStringParser</tt> should be used only in conjunction with a
 * <tt>FINT</tt> or a <tt>CINT</tt> object. It serves the purpose of parsing the
 * contour interval string passed to the constructor of a <tt>FINT</tt> or a
 * <tt>CINT</tt>object.Thereafter the instance variables of each of these two
 * classes is populated by the data extracted by <tt>ContourStringParser</tt>,
 * from the parsed string.
 * 
 * <pre>
 *  SOFTWARE HISTORY
 *  Date          Ticket#     Engineer     Description
 *  ------------ ---------- ----------- --------------------------
 *  09-Nov-2009    174        Archana.S   Refactored and updated the actual parsing
 *                                        logic from ContourDataStringParser 
 *  12-Nov-2009    174        Archana.S   Removed commented print statements
 *                                        Commented out logic to process labels
 *                                        Updated the method parseContourStringContainingLabel
 *                                        to set the boolean flag to false, when no number is found
 * 13-Nov-2009     174        Archana.S   Updated per review comments: 
 *                                        1.Updated regular expression for   
 *                                        checking labeled contour values
 *                                        2.Removed a separate check to see if the contour value string 
 *                                        matches an Integer, since the regex for real numbers, takes care of it. 
 *  07-Jun-2010     174        Archana.S    Updated the method call to 
 *                                         the private method removeBlanksWithinString()
 *                                         to use the static method from the StringUtilities
 *                                         class and deleted the private method
 *                                         removeBlanksWithinString() from this class.                                                                      
 *                                        Renamed the package as
 *                                        gov.noaa.nws.ncep.gempak.parameters.contourinterval
 *                                        
 *  08-Jun-2010    174        Archana.S   Updated the method 
 *                                        extractContourValueFromStringAndAddToContourList(String)
 *                                        to set the contour interval, the minimum and the maximum
 *                                        contour value in a labeled contour string.  
 *  09-Jun-2010    174       Archana.S    Added and updated the logic to parse labeled contour
 *                                        values into a HashMap(String,Double).
 *  14-Jun-2010    174        Archana.S   Redesigned class to support multiple zoom levels 
 *  02-Aug-2010   174        Archana.S   Updated code to parse a single real number as the contour interval
 *  07-Apr-2014  TTR-938      D.Sushon   Fixes to parseContourString:
 *                                         correctly parse CINTs of form: 1/-23.5/-22.5   1/22.5/23.5   (TTR-938)
 *                                         single numbers (i.e. 0.7) not parsed correctly, fixed to follow same execution path as #// (i.e. 0.7//)
 * 
 * </pre>
 * 
 * @author Archana.S
 * @version 1.0
 * @see $GEMPAK/help/hlx/cint.hl2
 * @see $GEMPAK/help/hlx/fint.hl2
 *      <p>
 */
public class ContourStringParser {

    /** A real number that represents the contour interval */
    private Double contourInterval;

    /** A real number that represents the minimum contour level */
    private Double minContourLevel;

    /** A real number that represents the maximum contour level */
    private Double maxContourLevel;

    /**
     * An integer that decides the minimum number of digits in an integer
     * contour label
     */
    private Integer numPaddingDigits;

    /** The generated list of contour levels */
    private List<Double> contourValuesList;

    /** A HashMap of the labels and their respective contour values */
    private Map<Double, String> labeledContourValuesHashMap;

    /**
     * A string that contains the extracted contour label from a single contour
     * level
     */
    private String contourIntervalLabel;

    /**
     * Boolean flag to validate that the contour data string was parsed
     * correctly
     */
    private boolean isContourStringParsed;

    /**
     * The LIST_OF_CONTOUR_VALUES must have at least 2 numbers separated by a
     * semicolon
     */
    private String LIST_OF_CONTOUR_VALUES = "(-?\\d*\\.?\\d*;-?\\d*\\.?\\d*)+";

    private String REAL_NUMBER = "-?\\d*\\.?\\d*;?";

    private String INTEGER = "-?\\d+";

    private String LABELLED_CONTOUR_VALUE = "(-?\\d*\\.?\\d*=[\\w\\p{Punct}]*;*)+";

    /**
     * The default constructor just initializes the instance variables to their
     * defaults
     */
    public ContourStringParser() {
        contourInterval = Double.NaN;
        minContourLevel = Double.NaN;
        maxContourLevel = Double.NaN;
        numPaddingDigits = new Integer(0);
        isContourStringParsed = false;
        contourValuesList = null;
    }

    /**
     * @return the labeledContourValuesHashMap
     */
    public Map<Double, String> getLabeledContourValuesHashMap() {
        return labeledContourValuesHashMap;
    }

    /**
     * @param labeledContourValuesHashMap
     *            the labeledContourValuesHashMap to set
     */
    private void setLabeledContourValuesHashMap(
            Map<Double, String> labeledContourValuesHashMap) {
        this.labeledContourValuesHashMap = labeledContourValuesHashMap;
    }

    public boolean isContourStringParsed() {
        return isContourStringParsed;
    }

    /** @return List of Double - contourValuesList */
    public List<Double> getContourValuesList() {
        return contourValuesList;
    }

    /**
     * The method getContourInterval returns the contour interval value
     * 
     * @return Double contourInterval
     **/
    public Double getContourInterval() {
        return contourInterval;
    }

    /**
     * The method getContourIntervalLabel returns the contour interval value
     * 
     * @return String contourIntervalLabel
     **/
    public String getContourIntervalLabel() {
        return contourIntervalLabel;
    }

    /**
     * The method getMinContourLevel returns the value of the minimum contour
     * level
     * 
     * @return Double minContourLevel
     **/
    public Double getMinContourLevel() {
        return minContourLevel;
    }

    /**
     * The method getMaxContourLevel returns the value of the maximum contour
     * level
     * 
     * @return Double maxContourLevel
     **/
    public Double getMaxContourLevel() {
        return maxContourLevel;
    }

    /**
     * The method getNumPaddingDigits returns the value of the Integer
     * numPaddingDigits
     * 
     * @return Integer numPaddingDigits
     **/
    public Integer getNumPaddingDigits() {
        return numPaddingDigits;
    }

    /**
     * The method <tt>parse</tt> checks if the input string contains blanks. If
     * so it invokes the method <tt>removeBlanksWithinString </tt> on the string
     * and then invokes the method <tt>parseContourString</tt> on it. Else, the
     * method <tt>parseContourString</tt> is directly invoked on the input
     * string
     * <p>
     * 
     * @param String
     *            contourStringToParse
     **/
    public void parse(String contourStringToParse) {

        /*
         * Initialize the list and the hash map
         */
        setContourValuesList(new ArrayList<Double>(0));
        setLabeledContourValuesHashMap(new LinkedHashMap<Double, String>(0));

        if (contourStringToParse != null && !(contourStringToParse.isEmpty())) {
            if (contourStringToParse.contains(" ")) {
                String strWithoutBlanks = StringUtil
                        .removeBlanksWithinString(contourStringToParse);
                isContourStringParsed = this
                        .parseContourString(strWithoutBlanks);
            } else {
                isContourStringParsed = this
                        .parseContourString(contourStringToParse);
            }
        }
    }

    /**
     * The method <tt>parseContourString</tt>
     * 
     * @param String
     *            contourStringToParse
     * @return boolean isInputContourStringParsed
     *         <p>
     *         The method <tt>parseContourString</tt> accepts the contour data
     *         string fed as an input from the method <tt>parse()</tt>. A valid
     *         contour string can take only one of the formats listed below:
     *         <P>
     *         1. contourInterval/minimumContourLevel/maximumContourLevel/
     *         numPaddingdigits
     *         <P>
     *         2. contourValue1;contourValue2;contourValue3;contourValue4;
     *         contourValue5.......
     *         <p>
     *         3. contourValue1=label1/contourValue1/contourValue1
     *         <p>
     *         4.
     *         contourValue1=label1;contourValue2=label2;contourValue3=label3;
     *         ............
     *         <p>
     *         5. contourInterval/minimumContourLevel/maximumContourLevel
     *         <p>
     *         <p>
     *         If the input string is entered in the first or the fifth format,
     *         <tt>contourStringToParse</tt> is parsed using the "/" character
     *         as a delimiter. All numeric data is extracted and stored.
     *         <p>
     *         If the second format is chosen, the string contourStringParam is
     *         parsed using the ";" character as a delimiter.
     *         <p>
     *         The method<tt>generateContourValuesList</tt> is invoked to
     *         compute the range of contour values. The method
     *         setContourValuesList is invoked to store the computed range of
     *         contour values.
     *         <p>
     *         If the parsing and subsequent numeric data retrieval is
     *         successful, the boolean isInputContourStringParsed is set to
     *         true.
     *         <P>
     ***/
    private boolean parseContourString(String contourStringToParse) {
        String contourStringsTokens[];
        boolean isInputContourStringParsed = false;
        boolean possibleFallThrough = false;
        String tempForReprocess = contourStringToParse;
        /*
         * If the input string contains a list of contour values separated by a
         * ';' character as a delimiter
         */
        if (Pattern.matches(REAL_NUMBER, contourStringToParse)) {
            try {
                String[] tempStr = contourStringToParse.split(";");
                if (tempStr != null && tempStr.length > 0) {
                    Double d = Double.parseDouble(tempStr[0]);
                    setContourInterval(d.doubleValue());
                    isInputContourStringParsed = true;
                    // System.out.println(tempStr[0]);
                    possibleFallThrough = true;
                }
            } catch (Exception e) {
                isInputContourStringParsed = false;
            }
        } else if (Pattern
                .matches(LABELLED_CONTOUR_VALUE, contourStringToParse)) {
            if (contourStringToParse.contains(";")) {
                contourStringsTokens = contourStringToParse.split(";");
                for (String s : contourStringsTokens) {
                    isInputContourStringParsed = this
                            .parseContourStringContainingLabel(s);

                    /*
                     * TODO add code to store the labels in a list or in a
                     * HashMap with the corresponding contour values
                     */

                    if (!isInputContourStringParsed) {
                        System.out.println("INVALID STRING");
                        break;
                    }
                }
            } else {
                isInputContourStringParsed = this
                        .parseContourStringContainingLabel(contourStringToParse);
            }
        } else if (Pattern
                .matches(LIST_OF_CONTOUR_VALUES, contourStringToParse)) {
            if (contourStringToParse.contains(";")) {

                /*
                 * Split the input string into tokens using the ';' character as
                 * a delimiter
                 */
                contourStringsTokens = contourStringToParse.split(";");
                for (String s : contourStringsTokens) {
                    /*
                     * for each string token, attempt to extract the contour
                     * value from it to the map of contour values
                     */
                    isInputContourStringParsed = this
                            .extractContourValueFromStringAndAddToHashMap(s);
                    if (!isInputContourStringParsed) {
                        break;
                    }
                }
            } else {
                isInputContourStringParsed = this
                        .extractContourValueFromStringAndAddToHashMap(contourStringToParse);
            }

        } else {
            if (contourStringToParse.contains("/")) {
                contourStringsTokens = contourStringToParse.split("/");

                Double tempContourParam[] = new Double[3];
                int i = 0;
                for (i = 0; i < 3; i++) {
                    try {
                        tempContourParam[i] = Double
                                .parseDouble(contourStringsTokens[i]);
                        isInputContourStringParsed = true;

                    } catch (NumberFormatException nfe) {
                        tempContourParam[i] = Double.NaN;
                        // exceptions should be used to handle exceptional
                        // situations rather than as a branching mechanism
                    } catch (Exception e) {
                        tempContourParam[i] = Double.NaN;
                        // exceptions should be used to handle exceptional
                        // situations rather than as a branching mechanism
                    }
                }

                /*
                 * Store extracted numeric data as minContourLevel and
                 * maxContouLevel
                 */
                setMinContourLevel(tempContourParam[1]);
                setMaxContourLevel(tempContourParam[2]);

                /*
                 * If the maxContourLevel value is different from the
                 * minContourLevel value,store the contourInterval value as
                 * retrieved from the corresponding string token.
                 */
                if (this.getMaxContourLevel().doubleValue() != this
                        .getMinContourLevel().doubleValue()) {
                    setContourInterval(tempContourParam[0]);
                } else {

                    /* Special case: check for a label in a single contour value */
                    if (contourStringsTokens[0].contains("=")) {
                        isInputContourStringParsed = parseContourStringContainingLabel(contourStringsTokens[0]);

                        if (isInputContourStringParsed) {
                            setContourInterval(this.getMinContourLevel());
                        }
                    } else {
                        /*
                         * Since both maxContourLevel and minContourLevel are
                         * equal, and there is no contour label, the contour
                         * interval is set to 0??. seems like should be
                         * Double.NaN..
                         */
                        setContourInterval(0.0);
                        setContourInterval(Double.NaN);
                    }
                }

                if (Double.isNaN(this.getContourInterval())
                        && Double.isNaN(this.getMinContourLevel())
                        && Double.isNaN(this.getMaxContourLevel())) {
                    isInputContourStringParsed = false; /*
                                                         * All three parameters
                                                         * cannot be undefined
                                                         * in a successful parse
                                                         * operation
                                                         */
                } else {
                    isInputContourStringParsed = true;
                }

                /*
                 * If the parsing has been successful, generate the list of
                 * contour values
                 */
                if ((isInputContourStringParsed)) {

                    /*
                     * For CINT alone, a digit defining the minimum number of
                     * digits in a label is to be extracted from the 4th token,
                     * is it exists
                     */

                    if (contourStringsTokens.length >= 4) {
                        try {
                            /*
                             * Check that the 4th string token is an integer:
                             * 
                             * INTEGER is a regular expression of the form: \\d+
                             */
                            if (Pattern.matches(INTEGER,
                                    contourStringsTokens[3])) {
                                setNumPaddingDigits(Integer
                                        .parseInt(contourStringsTokens[3]));
                            }
                        } catch (Exception e) {
                            isInputContourStringParsed = false;
                        }
                    } else {
                        setNumPaddingDigits(0);
                    }

                    Map<Double, String> tempContourValMap = generateContourValuesMap(
                            this.getContourInterval(),
                            this.getMinContourLevel(),
                            this.getMaxContourLevel(),
                            this.getNumPaddingDigits());

                    setLabeledContourValuesHashMap(tempContourValMap);
                }
            }
        }

        /*
         * case for lone contour interval entered without "//", retry parse as
         * "contourInterval//"
         */
        if (true == possibleFallThrough
                && (false == tempForReprocess.contains(";"))) {
            Double foo = Double.NaN;
            try {

                foo = Double.parseDouble(tempForReprocess);
            } catch (NumberFormatException nfe) {
                //
            } catch (Exception genericException) {
                //
            }

            String reparse = new String(foo + "//");
            isInputContourStringParsed = false;

            contourStringsTokens = reparse.split("/");

            Double tempContourParam[] = new Double[3];
            for (int i = 0; i < 3; i++) {
                try {
                    tempContourParam[i] = Double
                            .parseDouble(contourStringsTokens[i]);
                    isInputContourStringParsed = true;

                } catch (NumberFormatException nfe) {
                    tempContourParam[i] = Double.NaN;
                    // exceptions should be used to handle exceptional
                    // situations rather than as a branching mechanism
                } catch (Exception e) {
                    tempContourParam[i] = Double.NaN;
                    // exceptions should be used to handle exceptional
                    // situations rather than as a branching mechanism
                }
            }

            /*
             * Store extracted numeric data as minContourLevel and
             * maxContouLevel
             */
            setMinContourLevel(tempContourParam[1]);
            setMaxContourLevel(tempContourParam[2]);

            /*
             * If the maxContourLevel value is different from the
             * minContourLevel value,store the contourInterval value as
             * retrieved from the corresponding string token.
             */
            if (this.getMaxContourLevel().doubleValue() != this
                    .getMinContourLevel().doubleValue()) {
                setContourInterval(tempContourParam[0]);
            } else {

                /* Special case: check for a label in a single contour value */
                if (contourStringsTokens[0].contains("=")) {
                    isInputContourStringParsed = parseContourStringContainingLabel(contourStringsTokens[0]);

                    if (isInputContourStringParsed) {
                        setContourInterval(this.getMinContourLevel());
                    }
                } else {
                    /*
                     * Since both maxContourLevel and minContourLevel are equal,
                     * and there is no contour label, the contour interval is
                     * set to 0??. seems like should be Double.NaN..
                     */
                    // setContourInterval(0.0);
                    setContourInterval(Double.NaN);
                }
            }

            if (Double.isNaN(this.getContourInterval())
                    && Double.isNaN(this.getMinContourLevel())
                    && Double.isNaN(this.getMaxContourLevel())) {
                isInputContourStringParsed = false; /*
                                                     * All three parameters
                                                     * cannot be undefined in a
                                                     * successful parse
                                                     * operation
                                                     */
            } else {
                isInputContourStringParsed = true;
            }

            /*
             * If the parsing has been successful, generate the list of contour
             * values
             */
            if ((isInputContourStringParsed)) {
                setNumPaddingDigits(0);

                Map<Double, String> tempContourValMap = generateContourValuesMap(
                        this.getContourInterval(), this.getMinContourLevel(),
                        this.getMaxContourLevel(), this.getNumPaddingDigits());

                setLabeledContourValuesHashMap(tempContourValMap);

            }
        }

        return isInputContourStringParsed;
    }

    /**
     * @param String
     *            contourString
     * @return boolean isContourValueExists
     *         <p>
     *         The method <tt>extractContourValueFromStringAndAddToHashMap</tt>
     *         is used to extract a real number (the contour value) from the
     *         input string. If the extraction is successful, the real number is
     *         added to a HashMap of contour values. If the extraction is
     *         successful, the method returns <tt>true</tt>, else <tt>false</tt>.
     * */
    private boolean extractContourValueFromStringAndAddToHashMap(
            String contourString) {
        Double tempContourValue;
        boolean isContourValueExists = false;
        try {
            tempContourValue = Double.parseDouble(contourString);
            /*
             * If the parse is successful, add the contour value to the list of
             * contour values
             */
            isContourValueExists = true;
        } catch (Exception e) {
            tempContourValue = Double.NaN;
            isContourValueExists = false;
        }

        if (isContourValueExists) {
            // this.getContourValuesList().add(tempContourValue);
            this.getLabeledContourValuesHashMap().put(tempContourValue,
                    contourString);
            this.contourInterval = tempContourValue;
        }

        return isContourValueExists;
    }

    /**
     * @param String
     *            contourValueWithLabel
     * @return boolean isLabelExists
     *         <p>
     *         The method <tt>parseContourStringContainingLabel</tt> is used to
     *         parse the input string into it constituent contour value(a real
     *         number) and a string representing its label. If the extraction is
     *         successful, the method returns true, else false.
     **/
    private boolean parseContourStringContainingLabel(
            String contourValueWithLabel) {
        String singleContourLevelToken[];
        boolean isLabelAndContourValueExists = false;
        Double key = Double.NaN;
        String value = "";
        /*
         * check for the "=" delimiter, to see if the string contains a label
         * for the contour value
         */
        if (contourValueWithLabel.contains("=")) {

            /* If yes, split the string using the delimiter "=" */
            singleContourLevelToken = contourValueWithLabel.split("=");

            /*
             * Extract the contour value from the numeric data in the first
             * string token
             */
            if (Pattern.matches(REAL_NUMBER, singleContourLevelToken[0])) {

                if (singleContourLevelToken.length > 1) {
                    isLabelAndContourValueExists = true;
                    String contourLabelIntervalValue[] = singleContourLevelToken[1]
                            .split("/");
                    value = new String(contourLabelIntervalValue[0]);
                    try {
                        key = Double.parseDouble(singleContourLevelToken[0]);
                    } catch (Exception e) {
                        key = Double.NaN;
                        isLabelAndContourValueExists = false;
                    }
                }

                if (isLabelAndContourValueExists) {
                    /*
                     * If the parse is successful, add the contour value to the
                     * list of contour values
                     */
                    this.getContourValuesList().add(key);
                    labeledContourValuesHashMap.put(key, value);
                }

            }
        } else {
            /*
             * No numeric data was found in the string token Set the boolean
             * flag to false, to indicate an error.
             */
            isLabelAndContourValueExists = false;
        }
        if (labeledContourValuesHashMap.size() == 1) {
            setContourInterval(0.0);
            setMaxContourLevel(key);
            setMinContourLevel(key);
        }

        return isLabelAndContourValueExists;
    }

    /**
     * Generates a Map of Contour values of the form Map(Double, String)
     * 
     * @param Double
     *            contourInterval
     * @param Double
     *            minContourValue
     * @param Double
     *            maxContourValue
     * @param numLeadingZeros
     * @return a Map of type (Double, String)
     *         <p>
     *         The method <tt>generateContourValuesMap</tt> accepts as input the
     *         <tt>contourInterval</tt> and the minimum and maximum contour
     *         values. It generates a Map (Double, String) representing the
     *         contour levels that lie in the range specified by
     *         <tt>minContourValue</tt> and <tt>maxContourValue</tt>. The String
     *         value of each entry in the map represents the contour value's
     *         String equivalent, after appending it with leading zeros
     *         depending on the value of numLeadingZeros
     *         <p>
     *         Each contour level is separated from its neighboring contour
     *         values by the value of the contourInterval. If
     *         <tt>minContourValue</tt> and <tt>maxContourValue</tt> are not
     *         exact multiples of <tt>contourInterval</tt>, then they are set to
     *         the next neighboring values that are exact multiples of the
     *         <tt>contourInterval</tt>.
     *         <p>
     * 
     ***/
    private Map<Double, String> generateContourValuesMap(
            Double contourInterval, Double minContourValue,
            Double maxContourValue, Integer numLeadingZeros) {

        Map<Double, String> contourValMap = new LinkedHashMap<Double, String>();
        /* If contourInterval is negative, multiply it by -1 */
        if (contourInterval < 0) {
            contourInterval *= -1;
        }

        /* If the contourInterval is defined */
        if (!(Double.isNaN(contourInterval)) && (contourInterval != 0)) {

            if (!(Double.isNaN(maxContourValue))
                    && !(Double.isNaN(minContourValue))) {

                /*
                 * Even if the values of minContourValue and maxContourValue
                 * parameters are exchangedSwap them and then generate the list
                 */
                if (minContourValue.compareTo(maxContourValue) > 0) {

                    Double tempContourVal = new Double(minContourValue);
                    minContourValue = new Double(maxContourValue);
                    maxContourValue = new Double(tempContourVal);
                    setMinContourLevel(minContourValue);
                    setMaxContourLevel(maxContourValue);
                }

                /*
                 * If minContourValue and maxContourValue are not exact
                 * multiples of contourInterval they are set to neighboring
                 * values that are exact multiples of the contourInterval.
                 */
                // if(Pattern.matches(INTEGER, ))

                Double d1 = minContourValue / contourInterval;
                Double d2 = maxContourValue / contourInterval;
                int intMinContourValueAdjFactor = 0;
                int intMaxContourValueAdjFactor = 0;

                // if (!Pattern.matches(INTEGER, d1.toString())
                // && d1.doubleValue() != 0) {
                // intMinContourValueAdjFactor = (int) d1.doubleValue();
                // }
                //
                // if (!Pattern.matches(INTEGER, d2.toString())
                // && d2.doubleValue() != 0) {
                // intMaxContourValueAdjFactor = (int) d2.doubleValue();
                // }

                if (!Pattern.matches(INTEGER, d1.toString())
                        && d1.doubleValue() != 0) {
                    intMinContourValueAdjFactor = (int) StrictMath.ceil(d1
                            .doubleValue());
                }

                if (!Pattern.matches(INTEGER, d2.toString())
                        && d2.doubleValue() != 0) {
                    intMaxContourValueAdjFactor = (int) StrictMath.floor(d2
                            .doubleValue());
                }

                if (intMinContourValueAdjFactor != 0) {
                    minContourValue = new Double(intMinContourValueAdjFactor
                            * contourInterval);
                }

                if (intMaxContourValueAdjFactor != 0) {
                    maxContourValue = new Double(intMaxContourValueAdjFactor
                            * contourInterval);
                }
                /*
                 * Starting from minContourValue, until maxContourValue is
                 * reached, each contourValue is added to the mapand the next
                 * contourValue in the range is computed by adding the
                 * contourInterval to the current contour value.Finally, the
                 * last element in the range - maxContourValue is also added to
                 * the map.
                 */
                while (minContourValue.compareTo(maxContourValue) <= 0) {
                    String contourLabel = addLeadingZerosToContourValue(
                            numLeadingZeros, minContourValue);
                    contourValMap.put(minContourValue, contourLabel);
                    minContourValue = new Double(contourInterval
                            + minContourValue.doubleValue());
                }
                // String contourLabel = addLeadingZerosToContourValue(
                // numLeadingZeros, maxContourValue);
                // contourValMap.put(maxContourValue, contourLabel);
            }
        } else {
            if (minContourValue.doubleValue() == maxContourValue.doubleValue()) {
                /*
                 * When minContourValue is equal to maxContourValue, even if the
                 * contour interval is undefined the contour values list will
                 * contain one element namely the contour level specified by
                 * either minContourValue or maxContourValue(since they are the
                 * same).
                 */
                String contourLabel = addLeadingZerosToContourValue(
                        numLeadingZeros, minContourValue);
                contourValMap.put(minContourValue, contourLabel);
            } else {
                /*
                 * TODO Add code to generate the contour values list when
                 * contourInterval is NAN or 0 but minContourValue is not equal
                 * to maxContourValue
                 */
            }

        }
        return contourValMap;
    }

    /**
     * @param List
     *            of Double - contourValuesList
     */
    private void setContourValuesList(List<Double> contourValuesList) {
        this.contourValuesList = contourValuesList;
    }

    /**
     * @param Double
     *            contourInterval
     **/
    private void setContourInterval(Double contourInterval) {
        this.contourInterval = contourInterval;
    }

    /**
     * @param Double
     *            minContourLevel
     **/
    private void setMinContourLevel(Double minContourLevel) {
        this.minContourLevel = minContourLevel;
    }

    /**
     * @param Double
     *            maxContourLevel
     **/
    private void setMaxContourLevel(Double maxContourLevel) {
        this.maxContourLevel = maxContourLevel;
    }

    /**
     * @param Integer
     *            numPaddingDigits
     **/
    private void setNumPaddingDigits(Integer numPaddingDigits) {
        this.numPaddingDigits = new Integer(numPaddingDigits);
    }

    /**
     * Creates and returns a String representation of the input contour value
     * after adding leading zeros to it.
     * 
     * @param nPaddingDigits
     *            - the number of leading zeros to add
     * @param inputDblVal
     *            - the contour value to which leading zeros might need to be
     *            added
     * 
     * @return The String representation of the input Double value, after
     *         padding it with leading zeros if its integer portion has less
     *         number of digits than the number of padding digits
     */
    private String addLeadingZerosToContourValue(int nPaddingDigits,
            Double inputDblVal) {
        Integer intVal = new Integer(inputDblVal.intValue());
        boolean isNegative = false;
        if (inputDblVal < 0) {
            isNegative = true;
            inputDblVal = new Double(inputDblVal.doubleValue() * -1);
        }
        int lengthOfInputStr = intVal.toString().length();
        int numZerosToAdd = nPaddingDigits - lengthOfInputStr;
        StringBuilder stringBuilder = new StringBuilder(lengthOfInputStr);
        for (int i = 0; i < numZerosToAdd; i++) {
            stringBuilder.insert(0, "0");
        }

        if (isNegative) {
            stringBuilder.insert(0, "-");
        }

        String inputString = inputDblVal.toString();
        stringBuilder.append(inputString);

        return stringBuilder.toString();
    }

}
