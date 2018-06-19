package gov.noaa.nws.ncep.viz.tools.contourinterval;

import static org.junit.Assert.assertEquals;

import java.util.ArrayList;
import java.util.List;

import org.junit.Ignore;
import org.junit.Test;

/**
 * This Junit file tests the methods of the class ContourDataStringParser
 * <P>
 * A single string containing the contour data is fed to the overloaded
 * constructor of ContourDataStringParser. The method getContourValuesList
 * returns a list (of type Double) of the contour values. Prior to invoking the
 * method getContourValuesList, it should be checked that the input string was
 * parsed correctly, using the method isContourStringParsed().
 * <P>
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date          Ticket#     Engineer     Description
 * ------------ ---------- ----------- --------------------------
 * 19-Oct-2009    174        Archana.S   Initial Creation
 * </pre>
 * 
 * @author Archana.S
 * @version 1
 */
// TODO fix?
@Ignore
public class ContourDataStringParserTest {

    @Test
    /*
     * Test for valid contour data string of the form
     * contourInterval/minContourValue/maxContourValue
     */
    public void testPositiveContourIntervalWithMinMaxValues() {
        ContourDataStringParser cd = new ContourDataStringParser("10/0.5/9");
        List<Double> tempContourValList = new ArrayList<Double>();
        tempContourValList.add(0.0);
        assertEquals(cd.isContourStringParsed(), true);
        assertEquals(cd.getContourValuesList(), tempContourValList);
        System.out.println("=============================================");
        System.out.println("Is the contour data string parsed correctly? "
                + cd.isContourStringParsed());
        System.out
                .println("Contour Values List = " + cd.getContourValuesList());

        ContourDataStringParser cd2 = new ContourDataStringParser("10/-57/86");
        System.out.println("=============================================");
        System.out.println("Is the contour data string parsed correctly? "
                + cd2.isContourStringParsed());
        System.out.println("Contour Values List = "
                + cd2.getContourValuesList());
    }

    @Test
    /*
     * Test for valid contour data string of the form
     * contourInterval/minContourValue/maxContourValue with a negative
     * contourInterval
     */
    public void testNegativeContourIntervalWithMinMaxValues() {
        ContourDataStringParser cd = new ContourDataStringParser("-5/-11/23");
        assertEquals(cd.isContourStringParsed(), true);
        System.out.println("=============================================");
        System.out.println("Is the contour data string parsed correctly? "
                + cd.isContourStringParsed());
        System.out
                .println("Contour Values List = " + cd.getContourValuesList());
    }

    @Test
    /*
     * Test for valid contour data string of the form
     * contourInterval/minContourValue//numPaddingDigits
     */
    public void testContourIntervalWithMinValueOnly() {
        ContourDataStringParser cd = new ContourDataStringParser("-0.345/0//2");
        assertEquals(cd.getContourInterval().doubleValue(), -0.345, 0.0);
        assertEquals(cd.getMinContourLevel().doubleValue(), 0.0, 0.0);
        assertEquals(cd.getMaxContourLevel().doubleValue(), Double.NaN, 0.0);

        System.out.println("=============================================");
        System.out.println("Is the contour data string parsed correctly? "
                + cd.isContourStringParsed());
        System.out.println("Contour Interval = " + cd.getContourInterval());
        System.out
                .println("Minimum Contour Level = " + cd.getMinContourLevel());
        System.out
                .println("Maximum Contour Level = " + cd.getMaxContourLevel());
    }

    @Test
    /*
     * Test for valid contour data string of the form
     * contourInterval//maxContourValue
     */
    public void testContourIntervalWithMaxValueOnly() {
        ContourDataStringParser cd = new ContourDataStringParser("15//30");
        assertEquals(cd.getContourInterval().doubleValue(), 15, 0.0);
        assertEquals(cd.getMinContourLevel().doubleValue(), Double.NaN, 0.0);
        assertEquals(cd.getMaxContourLevel().doubleValue(), 30, 0.0);

        System.out.println("=============================================");
        System.out.println("Is the contour data string parsed correctly? "
                + cd.isContourStringParsed());
        System.out.println("Contour Interval = " + cd.getContourInterval());
        System.out
                .println("Minimum Contour Level = " + cd.getMinContourLevel());
        System.out
                .println("Maximum Contour Level = " + cd.getMaxContourLevel());
    }

    @Test
    /* Test input string containing a single contour value */
    public void testLessNumArgsContourIntervalString() {
        ContourDataStringParser cd = new ContourDataStringParser("-0.5/");
        assertEquals(cd.isContourStringParsed(), true);
        System.out.println("=============================================");
        System.out.println("List with single contour value = "
                + cd.getContourValuesList());

        ContourDataStringParser cd2 = new ContourDataStringParser("-0.6");
        assertEquals(cd2.isContourStringParsed(), true);
        System.out.println("=============================================");
        System.out.println("List with single contour value = "
                + cd2.getContourValuesList());

        ContourDataStringParser cd3 = new ContourDataStringParser("0.7;");
        assertEquals(cd3.isContourStringParsed(), true);
        System.out.println("=============================================");
        System.out.println("List with single contour value = "
                + cd3.getContourValuesList());

    }

    @Test
    /*
     * Test for contour data string of the form
     * contourInterval/minContourValue/maxContourValue
     * /numPaddingDigits/extraNumbers/extraNumbers
     */
    public void testMoreNumArgsContourIntervalString() {
        ContourDataStringParser cd = new ContourDataStringParser(
                "-0.5/0/700/30/40");
        assertEquals(cd.isContourStringParsed(), true);
        System.out.println("=============================================");
        System.out.println("Contour Interval = " + cd.getContourInterval());
        System.out
                .println("Minimum Contour Level = " + cd.getMinContourLevel());
        System.out
                .println("Maximum Contour Level = " + cd.getMaxContourLevel());
    }

    @Test
    /* Test for non-numeric values in contour interval string */
    public void testNonNumericContourIntervalString() {
        ContourDataStringParser cd = new ContourDataStringParser(
                "-def/abc/%^&/30/40");
        assertEquals(cd.isContourStringParsed(), false);

        System.out.println("=============================================");
        System.out.println("Is the contour data string parsed correctly? "
                + cd.isContourStringParsed());
        System.out.println("Contour Interval = " + cd.getContourInterval());
        System.out
                .println("Minimum Contour Level = " + cd.getMinContourLevel());
        System.out
                .println("Maximum Contour Level = " + cd.getMaxContourLevel());
    }

    @Test
    /* Test contour interval string with invalid delimiters */
    public void testInvalidDelimitersInContourIntervalString() {
        ContourDataStringParser cd = new ContourDataStringParser("5.10.60.9");
        assertEquals(cd.isContourStringParsed(), false);
        assertEquals(cd.getErrorMessage(), "Invalid String Format");
        assertEquals(cd.getContourInterval().doubleValue(), Double.NaN, 0.0);
        assertEquals(cd.getMinContourLevel().doubleValue(), Double.NaN, 0.0);
        assertEquals(cd.getMaxContourLevel().doubleValue(), Double.NaN, 0.0);

        System.out.println("=============================================");
        System.out.println("Error message = " + cd.getErrorMessage());
        System.out.println("Contour Interval = " + cd.getContourInterval());
        System.out
                .println("Minimum Contour Level = " + cd.getMinContourLevel());
        System.out
                .println("Maximum Contour Level = " + cd.getMaxContourLevel());
    }

    @Test
    /*
     * Test contour interval string by interchanging minContourValue and
     * maxContourValue
     */
    public void testMinMaxValuesInterchangedContourIntervalString() {
        ContourDataStringParser cd = new ContourDataStringParser("-5/20/5/1");
        assertEquals(cd.isContourStringParsed(), true);
        System.out.println("=============================================");
        System.out.println("Is the contour data string parsed correctly? "
                + cd.isContourStringParsed());
        System.out.println("Contour Interval = " + cd.getContourInterval());
        System.out
                .println("Minimum Contour Level = " + cd.getMinContourLevel());
        System.out
                .println("Maximum Contour Level = " + cd.getMaxContourLevel());
        System.out
                .println("Contour Values List = " + cd.getContourValuesList());

        ContourDataStringParser cd2 = new ContourDataStringParser("5/20/5/1");
        assertEquals(cd2.isContourStringParsed(), true);

        System.out.println("=============================================");
        System.out.println("Is the contour data string parsed correctly? "
                + cd.isContourStringParsed());
        System.out.println("Contour Interval      = "
                + cd2.getContourInterval());
        System.out.println("Minimum Contour Level = "
                + cd2.getMinContourLevel());
        System.out.println("Maximum Contour Level = "
                + cd2.getMaxContourLevel());
        System.out.println("Contour Values List   = "
                + cd2.getContourValuesList());
    }

    @Test
    /* Test for valid contour value string of the form val1;val2;val3;val4..... */
    public void testValidContourLevelValuesString() {
        ContourDataStringParser cd = new ContourDataStringParser(
                "66.1;0.1;5000;76;-.999;12233459390;0.00009988;1234.567890");
        assertEquals(cd.isContourStringParsed(), true);
        System.out.println("=============================================");
        System.out.println("Is the contour data string parsed correctly? "
                + cd.isContourStringParsed());
        System.out.println("Contour Values List   = "
                + cd.getContourValuesList());
    }

    @Test
    /* Test for non-numeric vales in contour values' string */
    public void testInvalidContourLevelValuesString() {
        ContourDataStringParser cd = new ContourDataStringParser(
                "66.1;abc;5000;76;;@#$%;12233459390");
        assertEquals(cd.isContourStringParsed(), false);

        System.out.println("=============================================");
        System.out.println("Is the contour data string parsed correctly? "
                + cd.isContourStringParsed());
        System.out.println("Contour Values List   = "
                + cd.getContourValuesList());
    }

    @Test
    /* Test for invalid delimiters in contour values' string */
    public void testInvalidDelimiterContourValuesString() {
        ContourDataStringParser cd = new ContourDataStringParser(
                "66.1,0,1,5000,76,-.999,12233459390");
        assertEquals(cd.isContourStringParsed(), false);
        assertEquals(cd.getErrorMessage(), "Invalid String Format");

        System.out.println("=============================================");
        System.out.println("Is the contour data string parsed correctly? "
                + cd.isContourStringParsed());
        System.out.println("Error message = " + cd.getErrorMessage());
    }

    @Test
    /* Test for empty contour data string */
    public void testEmptyString() {
        ContourDataStringParser cd = new ContourDataStringParser();
        assertEquals(cd.isContourStringParsed(), false);
        assertEquals(cd.getErrorMessage(), "Contour data string is empty");

        System.out.println("=============================================");
        System.out.println("Error message = " + cd.getErrorMessage());

        ContourDataStringParser cd2 = new ContourDataStringParser("");
        assertEquals(cd2.isContourStringParsed(), false);
        assertEquals(cd2.getErrorMessage(), "Contour data string is empty");

        System.out.println("=============================================");
        System.out.println("Error message = " + cd2.getErrorMessage());
    }

    @Test
    /* Test for contour data string with blank-spaces */
    public void testContourDataStringWithBlankSpaces() {

        ContourDataStringParser cd = new ContourDataStringParser("     ");
        assertEquals(cd.isContourStringParsed(), false);
        assertEquals(cd.getErrorMessage(), "Contour data string is empty");

        System.out.println("=============================================");
        System.out.println("Error message = " + cd.getErrorMessage());

    }

}
