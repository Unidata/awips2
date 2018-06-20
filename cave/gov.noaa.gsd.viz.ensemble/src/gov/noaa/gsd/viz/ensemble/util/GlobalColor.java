package gov.noaa.gsd.viz.ensemble.util;

import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.RGB;

/**
 * The GlobalColor class houses enumerations for the set of constant colors that
 * are available for global use. The caller is guaranteed that the Color, when
 * obtained, is not disposed.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 18, 2015    11727     polster     Initial creation
 * 
 * </pre>
 * 
 * @author polster
 * @version 1.0
 */

public enum GlobalColor {

    ATOMIC_TANGERINE(new RGB(255, 164, 116)), //
    BLACK(new RGB(0, 0, 0)), //
    BLUE(new RGB(31, 117, 254)), //
    BRIGHT_YELLOW(new RGB(255, 255, 0)), //
    BURNT_ORANGE(new RGB(255, 127, 73)), //
    CARIBBEAN_GREEN(new RGB(0, 204, 153)), //
    CERULEAN(new RGB(29, 172, 214)), //
    CHERRY_RED(new RGB(255, 0, 31)), //
    DARKER_GRAY(new RGB(130, 130, 130)), //
    DEEP_BLUE(new RGB(3, 2, 255)), //
    GRAY(new RGB(190, 190, 190)), //
    GREEN(new RGB(0, 204, 0)), //
    HOT_MAGENTA(new RGB(252, 29, 206)), //
    LIGHT_CARIBBEAN_GREEN(new RGB(0, 224, 173)), //
    LIGHTER_GRAY(new RGB(234, 234, 234)), //
    LIGHTER_YELLOW(new RGB(255, 255, 240)), //
    LIGHTISH_GRAY(new RGB(220, 220, 220)), //
    LIGHT_GRAY(new RGB(210, 210, 210)), //
    LIGHT_OLIVE(new RGB(153, 204, 0)), //
    LIGHT_RED(new RGB(255, 134, 134)), //
    LIGHT_YELLOW(new RGB(255, 255, 220)), //
    MEDIUM_LIGHT_GRAY(new RGB(175, 175, 175)), //
    MEDIUM_GRAY(new RGB(160, 160, 160)), //
    NEAR_WHITE(new RGB(240, 240, 240)), //
    NEON_CARROT(new RGB(255, 163, 67)), //
    NEON_PURPLE(new RGB(217, 0, 255)), //
    PALE_DULL_ORANGE(new RGB(255, 204, 153)), //
    PALE_DULL_VIOLET(new RGB(204, 204, 255)), //
    PALE_DULL_YELLOW(new RGB(255, 255, 150)), //
    PALE_DULL_AZURE(new RGB(141, 182, 221)), //
    PALE_DARK_AZURE(new RGB(174, 196, 228)), //
    PALE_LIGHT_AZURE(new RGB(196, 220, 249)), //
    PALE_EXTRA_LIGHT_AZURE(new RGB(214, 233, 253)), //
    PALE_WEAK_BLUE(new RGB(203, 229, 255)), //
    PALE_WEAK_GREEN(new RGB(204, 255, 204)), //
    PALE_WEAK_MAGENTA(new RGB(255, 204, 255)), //
    PASTEL_LIGHT_BLUE(new RGB(134, 181, 245)), //
    PEACH(new RGB(255, 204, 153)), //
    PINK_FLAMINGO(new RGB(252, 116, 253)), //
    PURPLE(new RGB(153, 51, 255)), //
    RED(new RGB(255, 51, 0)), //
    SEA_GREEN(new RGB(51, 204, 204)), //
    SEPIA(new RGB(165, 105, 79)), //
    SLATE_BLUE(new RGB(0, 153, 255)), //
    SLATE_GREEN(new RGB(51, 153, 102)), //
    SLATE_RED(new RGB(255, 153, 153)), //
    SPRING_GREEN(new RGB(0, 204, 68)), //
    WHITE(new RGB(255, 255, 255)), //
    WILD_WATERMELON(new RGB(252, 108, 133)), //
    YELLOW(new RGB(255, 255, 102)); //

    private RGB components;

    private GlobalColor(RGB comp) {
        components = comp;
    }

    public static Color get(GlobalColor gc) {
        return SWTResourceManager.getColor(gc.components.red,
                gc.components.green, gc.components.blue);
    }
}
