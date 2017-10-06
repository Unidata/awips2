package gov.noaa.gsd.viz.ensemble.util;

import java.io.PrintStream;
import java.util.Iterator;
import java.util.Map;
import java.util.Random;
import java.util.Set;

import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.RGB;

/**
 * Generic Utilities class to contain a hodge-podge of utility capabilties.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 8, 2014     5056     polster     Initial creation
 * 
 * </pre>
 * 
 * @author polster
 * @version 1.0
 */
public class Utilities {

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
     * generate a random color that is visbily not too dark nor light.
     * 
     * @return
     */
    public static RGB getRandomNiceContrastColor() {

        final int darkestShadeLowerThreshold = 160;
        final int darkestShadeUpperThreshold = 185;
        final int lightestShadeLowerThreshold = 195;
        final int lightestShadeUpperThreshold = 235;

        /**
         * For each of Red, Green, and Bluse components:
         * 
         * Find a random lower and upper thresholds to calculate the darkest and
         * lightest possible values of the color's component.
         */
        Random rand = new Random();
        final int r_darkestShade = Math.max(darkestShadeLowerThreshold,
                rand.nextInt(darkestShadeUpperThreshold));
        final int r_lightestShade = Math.max(lightestShadeLowerThreshold,
                rand.nextInt(lightestShadeUpperThreshold));
        final int g_darkestShade = Math.max(darkestShadeLowerThreshold,
                rand.nextInt(darkestShadeUpperThreshold));
        final int g_lightestShade = Math.max(lightestShadeLowerThreshold,
                rand.nextInt(lightestShadeUpperThreshold));
        final int b_darkestShade = Math.max(darkestShadeLowerThreshold,
                rand.nextInt(darkestShadeUpperThreshold));
        final int b_lightestShade = Math.max(lightestShadeLowerThreshold,
                rand.nextInt(lightestShadeUpperThreshold));

        /**
         * Now use the thresholds to create the random color value for each rgb
         * component.
         */
        int r = Math.min(r_darkestShade, rand.nextInt(r_lightestShade));
        int g = Math.min(g_darkestShade, rand.nextInt(g_lightestShade));
        int b = Math.min(b_darkestShade, rand.nextInt(b_lightestShade));

        return new RGB(r, g, b);

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
        hsb[2] = 0.75f;
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

    /* TODO: diagnostic only: uses standard-out file descriptor */
    public static void dumpMap(PrintStream out, Map<String, String> map,
            String keyDescr, String valueDescr) {

        String variable = null;
        String value = null;

        out.println("");
        out.println("----------------------------------------------------------");
        Set<String> keySet = map.keySet();
        Iterator<String> variablesIter = keySet.iterator();
        while (variablesIter.hasNext()) {
            variable = variablesIter.next();
            value = map.get(variable);
            out.println(">>>>>>>>> " + keyDescr + " " + variable + " "
                    + valueDescr + ": " + value);
        }
        out.println("----------------------------------------------------------");
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

}