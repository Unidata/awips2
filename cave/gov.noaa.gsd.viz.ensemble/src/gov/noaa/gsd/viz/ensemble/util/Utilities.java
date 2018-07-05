package gov.noaa.gsd.viz.ensemble.util;

import java.io.BufferedWriter;
import java.io.File;
import java.io.IOException;
import java.io.PrintStream;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Iterator;
import java.util.Map;
import java.util.Random;
import java.util.Set;

import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.RGB;
import org.geotools.filter.expression.ThisPropertyAccessorFactory;

import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.procedures.Bundle;

/**
 * Generic Utilities class to contain a hodge-podge of utility capabilties.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 08, 2014  5056       polster     Initial creation
 * Nov 19, 2016  19443      polster     Add a dump bundle method
 * Jan 10, 2018  20525      polster     High contrast color accessor now works
 * 
 * </pre>
 * 
 * @author polster
 * @version 1.0
 */
public class Utilities {

    private static int NEXT_HUE_POSITION = 0;

    private static final int DARKER_LUMINOSITY = 50;

    private static final int MEDIUM_LUMINOSITY = 63;

    private static final int LIGHTER_LUMINOSITY = 77;

    private static final int NUM_HUES = 20;

    private static final int HUE_MAX_IN_HSL = 359;

    private static final int MAX_TINGE_CHANGE_AMOUNT = 12;

    private Utilities() {
        super();
    }

    public static String padRight(String s, int n) {
        return String.format("%1$-" + n + "s", s);
    }

    public static String padLeft(String s, int n) {
        return String.format("%1$" + n + "s", s);
    }

    public static String removeExtraSpaces(String source) {
        String result = source.replaceAll("\\s+", " ");
        return result;
    }

    public static String trimQuotes(String pqs) {
        if (pqs.startsWith("\"")) {
            pqs = pqs.substring(1);
        }
        if (pqs.endsWith("\"")) {
            pqs = pqs.substring(0, pqs.length() - 1);
        }
        return pqs;
    }

    /**
     * This method returns a randmomly generated color that is guaranteed to be
     * visually (color/hue) different between calls.
     * 
     * @return RGB of the produced color
     */
    synchronized public static RGB getRandomNiceContrastColor() {

        HSLColor chosenColor = null;

        RGB niceColor = null;

        float hue = 0.0f; // 0 - 359 always separated by roughly 20
        float saturation = 100.0f; // always 100%
        float luminosity = 0.0f; // 50% (darker), 63% (medium), or 70% (lighter)

        switch (NEXT_HUE_POSITION) {
        case 0:
            hue = 0.0f;
            break;
        case 1:
            hue = 28.0f;
            break;
        case 2:
            hue = 85.0f;
            break;
        case 3:
            hue = 145.0f;
            break;
        case 4:
            hue = 205.0f;
            break;
        case 5:
            hue = 270.0f;
            break;
        case 6:
            hue = 335.0f;
            break;
        case 7:
            hue = 60.0f;
            break;
        case 8:
            hue = 120.0f;
            break;
        case 9:
            hue = 180.0f;
            break;
        case 10:
            hue = 240.0f;
            break;
        case 11:
            hue = 300.0f;
            break;
        case 12:
            hue = (float) HUE_MAX_IN_HSL;
            break;
        case 13:
            hue = 36.0f;
            break;
        case 14:
            hue = 142.0f;
            break;
        case 15:
            hue = 208.0f;
            break;
        case 16:
            hue = 256.0f;
            break;
        case 17:
            hue = 286.0f;
            break;
        case 18:
            hue = 322.0f;
            break;
        case (NUM_HUES - 1):
            hue = 195.0f;
            break;
        }

        /*
         * Randomize the hue +/- by some tinge factor. Increase the hue when the
         * result isn't greater than the max hue. Otherwise decrease hue.
         */
        Random rn = new Random();
        int randomHueTinger = rn.nextInt(MAX_TINGE_CHANGE_AMOUNT + 1);
        if (hue < (HUE_MAX_IN_HSL - MAX_TINGE_CHANGE_AMOUNT)) {
            hue += randomHueTinger;
        } else {
            hue -= randomHueTinger;
        }

        /*
         * bump the index for next time into this method to guarantee different
         * color on subsequent calls
         */
        NEXT_HUE_POSITION++;
        if (NEXT_HUE_POSITION == NUM_HUES) {
            NEXT_HUE_POSITION = 0;
        }

        int randomLuminosity = rn.nextInt(3);
        switch (randomLuminosity) {
        case 0:
            luminosity = DARKER_LUMINOSITY;
            break;
        case 1:
            luminosity = MEDIUM_LUMINOSITY;
            break;
        case 2:
            luminosity = LIGHTER_LUMINOSITY;
            break;
        }

        chosenColor = new HSLColor(hue, saturation, luminosity);

        niceColor = new RGB(chosenColor.getRGB().getRed(),
                chosenColor.getRGB().getGreen(),
                chosenColor.getRGB().getBlue());

        return niceColor;
    }

    /**
     * Create a gradient between a color and White.
     * 
     * @param color1
     *            The color to gradiate.
     * 
     * @param ratio
     *            Blend ratio. 0.5 will give even blend, 1.0 will return color1,
     *            0.0 will return color2 and so on.
     * @return Blended color.
     */
    public static Color gradient(Color color1, double ratio) {

        float r = (float) ratio;
        float ir = (float) 1.0 - r;

        RGB rgb1 = color1.getRGB();
        RGB rgb2 = GlobalColor.get(GlobalColor.WHITE).getRGB();

        int r1 = rgb1.red;
        int b1 = rgb1.blue;
        int g1 = rgb1.green;
        int red = 255;
        int blue = 255;
        int green = 255;

        if ((b1 > r1) && (b1 > g1)) {
            red = (int) (rgb1.red * ir + rgb2.red * r);
            green = (int) (rgb1.green * ir + rgb2.green * r);
            blue = b1;
        } else if ((r1 > b1) && (r1 > g1)) {
            red = r1;
            blue = (int) (rgb1.blue * ir + rgb2.blue * r);
            green = (int) (rgb1.green * ir + rgb2.green * r);
        } else if ((g1 > b1) && (g1 > r1)) {
            red = (int) (rgb1.red * ir + rgb2.red * r);
            blue = (int) (rgb1.blue * ir + rgb2.blue * r);
            green = g1;
        } else {
            red = (int) (rgb1.red * ir + rgb2.red * r);
            blue = (int) (rgb1.blue * ir + rgb2.blue * r);
            green = (int) (rgb1.green * ir + rgb2.green * r);
        }

        RGB blend = new RGB(red, blue, green);

        Color color = SWTResourceManager.getColor(blend);
        return color;

    }

    public static RGB desaturate(RGB c) {

        float[] hsb = c.getHSB();
        hsb[1] = 0.15f;
        RGB nc = new RGB(hsb[0], hsb[1], hsb[2]);
        return nc;
    }

    public static RGB brighten(RGB c) {

        float[] hsb = c.getHSB();
        hsb[2] = 1.0f;
        RGB nc = new RGB(hsb[0], hsb[1], hsb[2]);
        return nc;
    }

    /**
     * This method prints an Object reference as a series of eight (8) upper
     * case hexidecimal values.
     * 
     * @param o
     *            - An object reference
     * @return - A string containing the hex representation of an Object
     *         reference
     */
    public static String getReference(Object o) {

        String es = null;
        if (o == null) {
            es = "@00000000";
        } else {
            es = o.toString();
        }
        int atSignIndex = es.indexOf("@");
        if (atSignIndex > 0) {
            es = es.substring(atSignIndex);
        }
        es = es.toUpperCase();
        return es;
    }

    public static void dumpMap(PrintStream out, Map<String, String> map,
            String keyDescr, String valueDescr) {

        String variable = null;
        String value = null;

        out.println("");
        out.println(
                "----------------------------------------------------------");
        Set<String> keySet = map.keySet();
        Iterator<String> variablesIter = keySet.iterator();
        while (variablesIter.hasNext()) {
            variable = variablesIter.next();
            value = map.get(variable);
            out.println(">>>>>>>>> " + keyDescr + " " + variable + " "
                    + valueDescr + ": " + value);
        }
        out.println(
                "----------------------------------------------------------");
        out.println("");

    }

    public static void echo(PrintStream out, String str) {

        out.println("");
        out.println(
                "----------------------------------------------------------");
        out.println(">>>>>>>>>>>>>>>>>  " + str);
        out.println(
                "----------------------------------------------------------");
        out.println("");

    }

    public static String bytesIntoHumanReadable(long bytes) {
        final float kilobyte = 1024.0f;
        final float megabyte = kilobyte * 1024.0f;
        final float gigabyte = megabyte * 1024.0f;
        final float terabyte = gigabyte * 1024.0f;
        String result = null;
        if ((bytes >= 0) && (bytes < kilobyte)) {
            result = String.format("%.1f %s", Float.valueOf(bytes), "B");

        } else if ((bytes >= kilobyte) && (bytes < megabyte)) {
            result = String.format("%.1f %s",
                    Float.valueOf(((float) bytes / kilobyte)), "KB");

        } else if ((bytes >= megabyte) && (bytes < gigabyte)) {
            result = String.format("%.1f %s",
                    Float.valueOf(((float) bytes / megabyte)), "MB");

        } else if ((bytes >= gigabyte) && (bytes < terabyte)) {
            result = String.format("%.1f %s",
                    Float.valueOf(((float) bytes / gigabyte)), "GB");

        } else if (bytes >= terabyte) {
            result = String.format("%.1f %s",
                    Float.valueOf(((float) bytes / terabyte)), "TB");
        } else {
            return bytes + " Bytes";
        }
        return result;
    }

    public static void dumpStackTrace(int traceCount, String header) {

        Exception e = new Exception();
        StackTraceElement[] traces = e.getStackTrace();
        System.out.println(">>>>>>>>>>>>>>> " + header + " >>>>>> begin");
        int count = 0;
        for (StackTraceElement ste : traces) {
            if (ste.toString().indexOf("dumpStackTrace") >= 0) {
                continue;
            }
            System.out.println(">>>>> " + ste.toString());
            count++;
            if (count == traceCount)
                break;
        }
        System.out.println(">>>>>>>>>>>>>>> " + header + " >>>>>> end");
        System.out.println(
                "__________________________________________________________________");

    }

    public static void dumpBundleToFile(Bundle b, String fileLocation,
            String prefix) {
        String bundleAsXML = null;
        try {
            bundleAsXML = b.toXML();
        } catch (VizException e) {
            return;
        }

        if (!fileLocation.endsWith(File.separator)) {
            fileLocation.concat(File.separator);
        }

        if (bundleAsXML != null && bundleAsXML.length() > 0) {
            Path outFile = Paths.get(fileLocation + prefix + "-bundle.xml");
            Charset charset = Charset.forName("UTF-8");
            try (BufferedWriter writer = Files.newBufferedWriter(outFile,
                    charset)) {
                writer.write(bundleAsXML, 0, bundleAsXML.length());
            } catch (IOException x) {
                /* ignore */
            }
        }
    }
}