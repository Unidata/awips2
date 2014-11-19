package gov.noaa.gsd.viz.ensemble.util;

/**
 <pre>
 * 
 *  SOFTWARE HISTORY
 * 
 *  Date         Ticket#     Engineer    Description
 *  ------------ ----------  ----------- --------------------------
 *  Nov-20-2011              epolster    Initial Creation. 
 *  
 * </pre>
 * 
 * @author epolster
 * @version 1 
 * 
 *
 */

import java.util.Random;

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

    public static RGB getRandomColor() {

        Random rand = new Random();
        final int lowerFilter = 80;
        final int upperFilter = 200;
        final int skewToBrightness = 256 - upperFilter;

        int r = rand.nextInt(upperFilter);
        if (r < lowerFilter)
            r = lowerFilter;

        int g = rand.nextInt(upperFilter);
        if (g < lowerFilter)
            g = lowerFilter;

        int b = rand.nextInt(upperFilter);
        if (b < lowerFilter)
            b = lowerFilter;

        r += skewToBrightness;
        g += skewToBrightness;
        b += skewToBrightness;

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
        RGB rgb2 = SWTResourceManager.WHITE.getRGB();

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

}
