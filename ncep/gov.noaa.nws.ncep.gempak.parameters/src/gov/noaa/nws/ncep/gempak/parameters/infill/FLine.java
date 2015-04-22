package gov.noaa.nws.ncep.gempak.parameters.infill;

import gov.noaa.nws.ncep.gempak.parameters.core.util.StringUtil;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;

/**
 *<pre>
 * SOFTWARE HISTORY
 * Date          Ticket#     Engineer     Description
 * ------------ ---------- ----------- --------------------------
 * 04-Nov-2009    185        Archana.S   Initial Creation
 *  07-Jun-2010    185        Archana.S   Updated the method call to 
 *                                        the private method removeBlanksWithinString()
 *                                        to use the static method from the StringUtilities
 *                                        class and deleted the private method
 *                                        removeBlanksWithinString() from this class.
 *                                        Renamed the package as
 *                                        gov.noaa.nws.ncep.gempak.parameters.infill    
 * </pre>
 * @author Archana.S
 * @version 1
 * @see $GEMPAK/help/hlx/fline.hl2
 */

/*
 * TODO 1. Update documentation for patterns after they are added.
 * TODO 2. Update the documentation if the colors are to be stored as
 * a list of Color objects instead of integers.
 */
/**
 * <code>FLine</code> supplies the color and pattern information for filling a
 * contour. A string containing the color and/or pattern information is entered
 * as an input to the constructor. The input can be given in either one of the
 * formats listed below:
 * <p>
 * color1;color2;...;colorn/type1;...typen
 * <p>
 * color1-colorn
 * <p>
 * (Note: The second format is used to generate a list of colors from color1 to
 * colorn).
 * <p>
 * The status of the parse operation can be tested using the method
 * <code>isFLineStringParsed()</code>. The colors are stored as a list of
 * Integers and can be retrieved using the method
 * <code>getFillColorList()</code>. The patterns are also stored as a list of
 * Integers and can be retrieved using the method <code>getFillTypeList()</code>.
 */

public class FLine {

    /** A boolean flag to indicate if the input string was parsed */
    private boolean isFLineStringParsed;

    /**
     * The input string that contains the color and pattern information for
     * filling the contour
     */
    private String fLineString;

    /** The Error Message */
    private String errorMessage;

    /** The starting value for the color range */
    private Integer startColorRange;

    /** The ending value for the color range */
    private Integer endColorRange;

    /** The list of integers representing the parsed color values */
    private List<Integer> fillColorList;

    /** The list of integers representing the parsed color values */
    private List<Integer> fillTypeList;

    /** The default color */
    private Integer defaultFillColor;

    /** The default fill pattern */
    private Integer defaultFillType;

    /**
     * A regular expression to represent repeating groups in a list of
     * color/pattern values
     */
    private String LIST_OF_INTEGERS = "(-?;?\\d;*)*";

    /**
     * The default constructor generates 2 lists: The list of fill colors
     * contains 1 element set to 0, to indicate no color.
     * <p>
     * Currently, the color list is a list of integers.
     */

    public FLine() {
        setIsFLineStringParsed(false);
        fillColorList = new ArrayList<Integer>();
        fillTypeList = new ArrayList<Integer>();
        defaultFillColor = 1; /* Default fill color is no color */
        defaultFillType = 1; /* Default fill pattern is set to solid */
        fillTypeList.add(defaultFillType);
        fillColorList.add(defaultFillColor);
        setStartColorRange(defaultFillColor);
        setEndColorRange(defaultFillColor);

    }

    /**
     * @param String
     *            fLineStringInput
     *            <p>
     *            The input string containing the color and/or pattern
     *            information is parsed when the constructor invokes the private
     *            method <code>parseFLineString()</code>.
     */

    public FLine(String fLineStringInput) {

        // if the input string contains "-" as well as "/", ignore the fill type
        // list after "/"
        // e.g.: 0;20-32/1;2;3 => parse 0-20-32 and ignore the rest
        String[] fLineTmpstr = { fLineStringInput };
        if (fLineStringInput.trim().contains("-")
                && fLineStringInput.trim().contains("/")) {
            fLineTmpstr = fLineStringInput.trim().split("/");
            fLineStringInput = fLineTmpstr[0];
        }

        // process '-'
        String newFline = null;
        String[] tmpstr = fLineStringInput.trim().split(";");
        for (int i = 0; i < tmpstr.length; i++) {
            if (tmpstr[i].contains("-")) {
                String[] twoends = tmpstr[i].split("-", 2);
                int start = Integer.parseInt(twoends[0]);
                int end = Integer.parseInt(twoends[1]);
                String newstr = twoends[0] + ";";

                if (start < end) {
                    for (int j = start + 1; j <= end; j++) {
                        newstr = newstr.concat(String.valueOf(j));
                        if (j != end)
                            newstr = newstr + ";";
                    }
                } else {
                    for (int j = start - 1; j >= end; j--) {
                        newstr = newstr.concat(String.valueOf(j));
                        if (j != end)
                            newstr = newstr + ";";
                    }
                }

                tmpstr[i] = newstr;
            }

            if (i == 0)
                newFline = tmpstr[i];
            else
                newFline = newFline.concat(tmpstr[i]);

            if (i != (tmpstr.length - 1))
                newFline = newFline + ";";
        }
        fLineStringInput = newFline;

        /* Set default values and initialize instance variables */

        setIsFLineStringParsed(false);

        fillColorList = new ArrayList<Integer>();
        fillTypeList = new ArrayList<Integer>();

        /* The actual color represented by the integer 1,is based on user input */
        defaultFillColor = 1;

        /* Default fill pattern is set to solid */
        defaultFillType = 1;

        setStartColorRange(defaultFillColor);
        setEndColorRange(defaultFillColor);

        /*
         * If the input string is not empty or null, the method
         * parseFLineString() is invoked to parse it. The boolean value returned
         * by this method is used to set the boolean state variable
         * isFLineStringParsed.
         */
        if (((fLineStringInput != null))
                && (!fLineStringInput.trim().isEmpty())) {

            this.setFLineString(fLineStringInput);
            if (fLineStringInput.contains(" ")) {
                String inputStringWithoutBlanks = StringUtil
                        .removeBlanksWithinString(fLineStringInput);
                setIsFLineStringParsed(this
                        .parseFLineString(inputStringWithoutBlanks));
            } else {
                setIsFLineStringParsed(this.parseFLineString(fLineStringInput));
            }

            /*
             * If both the colors and the patterns have been parsed, update the
             * list ofpatterns to matcht the number of colors
             */
            if (this.getFillTypeList().size() > 0
                    && this.getFillColorList().size() > 0) {
                this.setMissingAttributes(this.getFillColorList(),
                        this.getFillTypeList());
            }
            /*
             * Else, if only the colors were entered as input and parsed, set
             * the default pattern toSOLID (i.e the Integer 1) and repeat it to
             * match the number of colors
             */
            else if (this.getFillTypeList().size() == 0
                    && this.getFillColorList().size() > 0) {
                this.getFillTypeList().add(defaultFillType);
                this.setMissingAttributes(this.getFillColorList(),
                        this.getFillTypeList());
            }
        } else {
            /*
             * If the input string is empty or null, then the color list is
             * created with a single element - the default color (integer object
             * set to 1)
             */

            fillColorList.add(defaultFillColor);
            /*
             * Set the default pattern variable to SOLID and add it to the list
             * of patterns
             */

            fillTypeList.add(defaultFillType);

        }
    }

    /**
     * @return String fLineString
     */
    public String getFLineString() {
        return fLineString;
    }

    /**
     * @return String errorMessage
     */
    public String getErrorMessage() {
        return errorMessage;
    }

    /**
     * @return List (of Integer) fillColorList
     */
    public List<Integer> getFillColorList() {
        return fillColorList;
    }

    /**
     * 
     * @return List of Integer fillTypeList
     */
    public List<Integer> getFillTypeList() {
        return fillTypeList;
    }

    /**
     * @return Integer startColorRange
     */

    public Integer getStartColorRange() {
        return startColorRange;
    }

    /**
     * @return Integer endColorRange
     */
    public Integer getEndColorRange() {
        return endColorRange;
    }

    /**
     * @param boolean isFLineStringParsed
     */
    public void setIsFLineStringParsed(boolean isFLineStringParsed) {
        this.isFLineStringParsed = isFLineStringParsed;
    }

    /**
     * @return boolean isFLineStringParsed
     */
    public boolean isFLineStringParsed() {
        return isFLineStringParsed;
    }

    /**
     * @param String
     *            lineString
     */
    private void setFLineString(String lineString) {
        fLineString = lineString;
    }

    /**
     * @param String
     *            errorMessage
     */
    private void setErrorMessage(String errorMessage) {
        this.errorMessage = errorMessage;
    }

    /**
     * @param List
     *            (of Integer) lineColorList
     */
    private void setFillColorList(List<Integer> lineColorList) {
        fillColorList = lineColorList;
    }

    /**
     * @param List
     *            of Integer fillPatternsList
     */
    private void setFillTypeList(List<Integer> fillPatternsList) {
        fillTypeList = fillPatternsList;
    }

    /**
     * @param Integer
     *            startColorRange
     */
    private void setStartColorRange(Integer startColorRange) {
        this.startColorRange = startColorRange;
    }

    /**
     * @param Integer
     *            endColorRange
     */
    private void setEndColorRange(Integer endColorRange) {
        this.endColorRange = endColorRange;
    }

    /**
     * 
     * @param String
     *            fLineStringInput
     * @return boolean isFLineStringParsedCorrectly
     */
    private boolean parseFLineString(String fLineStringInput) {
        boolean isFLineStringParsedCorrectly = false;
        boolean isColorToken = false;
        String[] fLineTokens;

        /* It is checked if both the colors and the patterns are entered */
        if (fLineStringInput.contains("/")) {

            /*
             * If yes, then the colors and patterns are parsed into tokens using
             * the delimiter character "/"
             */
            fLineTokens = fLineStringInput.split("/");

            /*
             * If the token contains only 1 element, it is checked if a list of
             * colors have been entered
             */
            if (fLineTokens.length >= 1) {

                /*
                 * If the token matches the regular expression for a list of
                 * integersit is construed to be a list of color values.The
                 * integer values for the colors are then extracted and stored
                 * in the list of colors.
                 */

                if (Pattern.matches(LIST_OF_INTEGERS, fLineTokens[0])) {
                    isColorToken = true;
                    isFLineStringParsedCorrectly = this
                            .parseColorOrPatternTokens(fLineTokens[0],
                                    isColorToken);
                }
            }
            /*
             * If the token contains two elements, the second element is
             * processed as the list of patterns
             */
            isColorToken = false;
            if (fLineTokens.length == 2) {
                if (Pattern.matches(LIST_OF_INTEGERS, fLineTokens[1])) {
                    isFLineStringParsedCorrectly = this
                            .parseColorOrPatternTokens(fLineTokens[1],
                                    isColorToken);
                }
            }
        }
        /*
         * Else it is checked if a range of colors have been entered. If so, the
         * method parseRangeOfColors() is invoked to process the string tokens
         * generated by splitting the input string into tokens using the
         * character '-' as a delimiter.
         */
        else if (fLineStringInput.contains("-")) {
            fLineTokens = fLineStringInput.split("-");
            isFLineStringParsedCorrectly = this.parseRangeOfColors(fLineTokens);
        } else {
            /*
             * It is checked if only a list of color values (colr1;..;colrn)
             * have been entered.If so, then the method
             * parseColorOrPatternTokens() is invoked toprocess the color tokens
             * generated by splitting the input string usingthe '-' character as
             * a delimiter.s
             */
            if (Pattern.matches(LIST_OF_INTEGERS, fLineStringInput.trim())) {
                isColorToken = true;

                isFLineStringParsedCorrectly = this.parseColorOrPatternTokens(
                        fLineStringInput, isColorToken);
            }
        }
        return isFLineStringParsedCorrectly;
    }

    /**
     * @param String
     *            inputToken
     * @param boolean isColorToken
     * @return boolean isTokenParsed
     *         <p>
     *         The method <code>parseColorOrPatternTokens()</code> can be used
     *         to process the string tokens containing either the list of colors
     *         or the list of patterns. It returns true if the parse operation
     *         was successful.
     */
    private boolean parseColorOrPatternTokens(String inputToken,
            boolean isColorToken) {
        boolean isTokenParsed = false;
        boolean multipleTokensFound = false;
        String[] tokenStringArray;
        List<Integer> integerTokenList = new ArrayList<Integer>();
        /*
         * If the input string contains a ';', it is split using this character
         * as a delimiter.
         */
        if (inputToken.contains(";")) {
            tokenStringArray = inputToken.split(";");
            /*
             * If more than one token is generated as a result of the split
             * operation a boolean flag multipleTokensFound is set to true.
             */
            if (tokenStringArray.length > 1) {
                multipleTokensFound = true;
            } else {
                /*
                 * Else it is assumed that a single token containing a single
                 * integer was generated.
                 */
                try {
                    /*
                     * An attempt is made to extract the integer from the string
                     * and store it.
                     */

                    integerTokenList.add(Integer.parseInt(tokenStringArray[0]));
                    isTokenParsed = true;
                    /*
                     * If the input boolean isColorToken is set to true, then
                     * the extracted integer is stored in the color list.
                     */
                    if (isColorToken) {
                        // TODO: check the single Integer in the list for valid
                        // color values?
                        this.setFillColorList(removeInvalidColors(integerTokenList));
                    } else {
                        this.setFillTypeList(integerTokenList);
                    }
                } catch (Exception e) {
                    integerTokenList.add(0);/*
                                             * A blank input string token
                                             * represents no color or no pattern
                                             */
                    this.setErrorMessage("INVALID STRING FORMAT");
                }
            }
            /*
             * If several tokens are generated, then using a for-loop,, an
             * attempt is made to extract an integer from each string token. If
             * it is successful, then integers are added one by one to a list of
             * integers.
             */
            if (multipleTokensFound) {
                for (int currentIndex = 0; currentIndex < tokenStringArray.length; currentIndex++) {
                    try {
                        integerTokenList.add(Integer
                                .parseInt(tokenStringArray[currentIndex]));
                        isTokenParsed = true;
                    } catch (Exception e) {
                        integerTokenList.add(0);
                        this.setErrorMessage("INVALID STRING FORMAT");
                    }
                }

            }

            /*
             * If it is a color token, the list of integers extracted is stored
             * as a list of colors.
             */
            if (isColorToken) {
                // TODO: check each Integer in the list for valid color values?
                this.setFillColorList(removeInvalidColors(integerTokenList));
            } else {
                // TODO
                /*
                 * It is a list of patterns -check if each integer in the list
                 * belongs to one of the valid patternvalues and if so, add it
                 * to the list of patterns.
                 */
                this.setFillTypeList(integerTokenList);
            }
        }
        /*
         * Else, if the input string token does not contain the ';' delimiter,
         * it is assumed that it contains a single integer and an attempt is
         * made to extract this integer. Of the attempt is successful, then the
         * integer is stored as a single member of the list of colors.
         */
        else {
            try {
                integerTokenList.add(Integer.parseInt(inputToken));
                isTokenParsed = true;

                if (isColorToken) {
                    // TODO: check the single Integer for valid color values?
                    this.setFillColorList(integerTokenList);
                } else {
                    this.setFillTypeList(integerTokenList);
                }
            } catch (NumberFormatException e) {
                this.setErrorMessage("INVALID STRING FORMAT");
            }
        }
        return isTokenParsed;
    }

    /**
     * @param String
     *            [] colorRangeStringTokens
     * @return boolean isColorRangeStringParsed
     *         <p>
     *         The method <code>parseRangeOfColors</code> accepts as input a
     *         string array containing two tokens - the starting and ending
     *         values of the color range, in that order. If the two integer
     *         values are successfully parsed from the string tokens, the method
     *         <code>generateColorRange()</code> is invoked to populate the list
     *         with all the color values in the range.
     */
    private boolean parseRangeOfColors(String[] colorRangeStringTokens) {

        boolean isColorRangeStringParsed = false;
        Integer colorValue = 0;
        for (int currentIndex = 0; currentIndex <= 1; currentIndex++) {

            try {
                colorValue = Integer
                        .parseInt(colorRangeStringTokens[currentIndex]);
                if (currentIndex == 0) {
                    /*
                     * TODO: Check if colorValue lies in the permissible range
                     * for color values
                     */
                    this.setStartColorRange(colorValue);
                } else {
                    this.setEndColorRange(colorValue);
                }
                isColorRangeStringParsed = true;
            } catch (Exception e) {
                this.setErrorMessage("INVALID STRING FORMAT");
            }

        }

        if (isColorRangeStringParsed) {
            this.generateColorRange(this.getStartColorRange(),
                    this.getEndColorRange());
        }
        return isColorRangeStringParsed;
    }

    /**
     * @param Integer
     *            startColorValue
     * @param Integer
     *            endColorValue The method <code>generateColorRange()</code>
     *            accepts as input, two integers that represent the starting and
     *            ending values of the color range. A loop is used to generate
     *            every integer (representing a color value) in between the
     *            range.
     */
    private void generateColorRange(Integer startColorValue,
            Integer endColorValue) {
        int currentIndex = 0;

        /*
         * If the starting and ending color values in the range are equalonly a
         * single element - the starting color value of the range is added to
         * the list.
         */
        if (startColorValue == endColorValue) {
            this.getFillColorList().add(startColorValue);
        }
        /*
         * Else if the starting color value in the range is less than ending
         * color value,a loop is used to generate all the intermediate color
         * values from the starting valueto the ending value by incrementing the
         * loop counter
         */
        else if (startColorValue < endColorValue) {
            for (currentIndex = startColorValue; currentIndex <= endColorValue; currentIndex++) {
                this.getFillColorList().add(currentIndex);
            }
        }
        /*
         * Else if the starting color value in the range is greater than ending
         * color value,a loop is used to generate all the intermediate color
         * values from the starting valueto the ending value by decrementing the
         * loop counter
         */
        else {
            for (currentIndex = startColorValue; currentIndex >= endColorValue; currentIndex--) {
                this.getFillColorList().add(currentIndex);
            }
        }
    }

    /**
     * The method <code>setMissingAttributes</code>checks if the list of the
     * list of patterns is less than that of the colors. If so, it invokes the
     * method <code>setMissingPatterns</code> to repeat the patterns in the list
     * till both the lists have the same number of elements
     * 
     * @param List
     *            of Integer - colorsList
     * @param List
     *            of Integer - patternsList
     */
    private void setMissingAttributes(List<Integer> colorsList,
            List<Integer> patternsList) {
        int colorsListSize = colorsList.size();
        int patternsListSize = patternsList.size();
        int numElementsToAdd = 0;

        if (patternsListSize < colorsListSize) {
            numElementsToAdd = colorsListSize - patternsListSize;
            this.setMissingPatterns(patternsList, numElementsToAdd);
        }

    }

    /**
     * The method <code>setMissingPatterns</code> uses the input integer
     * <code>numElementsToAdd</code> to decide how many elements to add, so as
     * to repeat the patterns in the input list of patterns.
     * 
     * @param List
     *            of Integer - patternsList
     * @param int numElementsToAdd
     */
    private void setMissingPatterns(List<Integer> patternsList,
            int numElementsToAdd) {
        int currentIndex = 0;
        int count = 1;
        while (count <= numElementsToAdd) {
            patternsList.add(patternsList.get(currentIndex));
            currentIndex++;
            count++;
        }
        this.setFillTypeList(patternsList);
    }

    private List<Integer> removeInvalidColors(List<Integer> integerList) {

        // Only allow integers 0 through 32 so that it does not exceed
        // the number of colors available (refer to class GempakColor)

        List<Integer> newIntegerList = new ArrayList<Integer>();

        int size = (integerList != null) ? integerList.size() : 0;
        for (int i = 0; i < size; i++) {
            int tmp = integerList.get(i);
            if (tmp >= 0 && tmp <=  32)
                newIntegerList.add(tmp);
        }

        return newIntegerList;
    }
}
