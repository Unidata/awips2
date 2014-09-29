/**
 **/
package gov.noaa.nws.ncep.viz.resources.manager;

/**
 * XMLSpecialCharacter - Class (enum) to encapsulate knowledge of predefined
 * character entity references in XML. Provides methods to take an arbitrary
 * string, and (1) detect presence of any characters requiring such special
 * handling in XML, or (2) return a suitably encoded (XML-safe) string.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 9, 2014            bhebbard     Initial creation
 * 
 * </pre>
 * 
 * @author bhebbard
 * @version 1.0
 */

public enum XMLSpecialCharacter {

    // @formatter:off
    AMPERSAND         ( '&',  "&amp;"  ),
    LESS_THAN_SIGN    ( '<',  "&lt;"   ),
    GREATER_THAN_SIGN ( '>',  "&gt;"   ),
    DOUBLE_QUOTE      ( '"',  "&quot;" ),
    APOSTROPHE        ( '\'', "&apos;" );
    // @formatter:on

    private char character;

    private String characterEntityReference;

    private XMLSpecialCharacter(char c, String cer) {
        character = c;
        characterEntityReference = cer;
    }

    // Non-static methods apply only to "this" character

    private String encodeMe(String inString) {
        return inString.replaceAll(Character.toString(character),
                characterEntityReference);
    }

    private String decodeMe(String inString) {
        return inString.replaceAll(characterEntityReference,
                Character.toString(character));
    }

    // Static convenience methods apply to all values()

    /**
     * Returns a boolean, indicating whether the source string contains at least
     * one of the XML-sensitive characters: & < > " '
     * 
     * @param testString
     *            a String to be tested for XML-sensitive characters
     * @return boolean; true iff testString contains any of: & < > " '
     */
    public static boolean in(String testString) {
        for (XMLSpecialCharacter xsc : values()) {
            if (testString.indexOf(xsc.character) != -1) {
                return true;
            }
        }
        return false;
    }

    /**
     * Returns a String that contains character entity references substituted
     * into the source String for any of the predefined entities defined in XML
     * (for handling special XML-sensitive characters). Operation is idempotent,
     * i.e., can be applied repeatedly with no harm (e.g., an originalString
     * already containing "&lt;" will result in a string containing "&lt;", and
     * not invalid string "&amp;lt;").
     * 
     * @param originalString
     *            a String which may contain (or not): & < > " '
     * @return String with XML-safe substitutions: &amp; &lt; &gt; &quot; &apos;
     */
    public static String encode(String originalString) {
        // Because '&' is a special character AND is itself a substring of all
        // character entity references, we start the encoding by DEcoding the
        // string, to avoid iterative/recursive &-->&amp; substitutions.
        String codedString = decode(originalString);
        // For each special character...
        for (XMLSpecialCharacter xsc : values()) {
            // ...encode all its occurrences in the string)
            codedString = xsc.encodeMe(codedString);
        }
        return codedString;
    }

    private static String decode(String originalString) {
        String decodedString = originalString;
        for (XMLSpecialCharacter xsc : values()) {
            decodedString = xsc.decodeMe(decodedString);
        }
        return decodedString;
    }

}
