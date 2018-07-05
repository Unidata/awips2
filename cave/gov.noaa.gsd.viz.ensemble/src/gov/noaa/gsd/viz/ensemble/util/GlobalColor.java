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

    ATOMIC_TANGERINE(new RGB(255, 164, 116), true), //
    BLACK(new RGB(0, 0, 0), false), //
    BLUE(new RGB(31, 117, 254), false), //
    BRIGHT_YELLOW(new RGB(255, 255, 0), true), //
    BURNT_ORANGE(new RGB(255, 127, 73), true), //
    CARIBBEAN_GREEN(new RGB(0, 204, 153), true), //
    CERULEAN(new RGB(29, 172, 214), true), //
    CHERRY_RED(new RGB(255, 0, 31), true), //
    DARKER_GRAY(new RGB(130, 130, 130), false), //
    DARKISH_GRAY(new RGB(160, 160, 160), false), //
    DEEP_BLUE(new RGB(3, 2, 255), false), //
    GRAY(new RGB(190, 190, 190), false), //
    GREEN(new RGB(0, 204, 0), false), //
    HOT_MAGENTA(new RGB(252, 29, 206), true), //
    LIGHT_CARIBBEAN_GREEN(new RGB(0, 224, 173), true), //
    LIGHTER_GRAY(new RGB(234, 234, 234), true), //
    LIGHTER_YELLOW(new RGB(255, 255, 240), true), //
    LIGHTISH_GRAY(new RGB(220, 220, 220), true), //
    LIGHT_GRAY(new RGB(210, 210, 210), true), //
    LIGHT_OLIVE(new RGB(153, 204, 0), true), //
    LIGHT_RED(new RGB(255, 134, 134), true), //
    LIGHT_YELLOW(new RGB(255, 255, 220), true), //
    MEDIUM_LIGHT_GRAY(new RGB(175, 175, 175), false), //
    MEDIUM_GRAY(new RGB(160, 160, 160), false), //
    NEAR_WHITE(new RGB(240, 240, 240), true), //
    NEON_CARROT(new RGB(255, 163, 67), true), //
    NEON_PURPLE(new RGB(217, 0, 255), true), //
    PALE_DULL_ORANGE(new RGB(255, 204, 153), true), //
    PALE_DULL_VIOLET(new RGB(204, 204, 255), true), //
    PALE_DULL_YELLOW(new RGB(255, 255, 150), true), //
    PALE_DULL_AZURE(new RGB(141, 182, 221), true), //
    PALE_DARK_AZURE(new RGB(174, 196, 228), true), //
    PALE_LIGHT_AZURE(new RGB(196, 220, 249), true), //
    PALE_EXTRA_LIGHT_AZURE(new RGB(214, 233, 253), true), //
    PALE_LIGHT_BLUE(new RGB(203, 229, 255), true), //
    PALE_LIGHT_GREEN(new RGB(204, 255, 204), true), //
    PALE_LIGHT_MAGENTA(new RGB(255, 204, 255), true), //
    PALE_LIGHT_RED(new RGB(255, 204, 204), true), //
    PASTEL_LIGHT_BLUE(new RGB(134, 181, 245), true), //
    PEACH(new RGB(255, 204, 153), true), //
    PINK_FLAMINGO(new RGB(252, 116, 253), true), //
    PURPLE(new RGB(153, 51, 255), true), //
    RED(new RGB(255, 51, 0), true), //
    SEA_GREEN(new RGB(51, 204, 204), true), //
    SEPIA(new RGB(165, 105, 79), false), //
    SLATE_BLUE(new RGB(0, 153, 255), true), //
    SLATE_GREEN(new RGB(51, 153, 102), false), //
    SLATE_RED(new RGB(255, 153, 153), true), //
    SPRING_GREEN(new RGB(0, 204, 68), false), //
    WHITE(new RGB(255, 255, 255), true), //
    WILD_WATERMELON(new RGB(252, 108, 133), true), //
    YELLOW(new RGB(255, 255, 102), true), //

    PapayaWhip(new RGB(255, 239, 213), true), //
    BlanchedAlmond(new RGB(255, 235, 205), true), //
    Bisque(new RGB(255, 228, 196), true), //
    PeachPuff(new RGB(255, 218, 185), true), //
    NavajoWhite(new RGB(255, 222, 173), true), //
    Moccasin(new RGB(255, 228, 181), true), //
    Cornsilk(new RGB(255, 248, 220), true), //
    Ivory(new RGB(255, 255, 240), true), //
    LemonChiffon(new RGB(255, 250, 205), true), //
    Seashell(new RGB(255, 245, 238), true), //
    Honeydew(new RGB(240, 255, 240), true), //
    MintCream(new RGB(245, 255, 250), true), //
    Azure(new RGB(240, 255, 255), true), //
    AliceBlue(new RGB(240, 248, 255), true), //
    Lavender(new RGB(230, 230, 250), true), //
    LavenderBlush(new RGB(255, 240, 245), true), //
    MistyRose(new RGB(255, 228, 225), true), //
    LightBlue(new RGB(173, 216, 230), true), //
    CornflowerBlue(new RGB(100, 149, 237), true), //
    SlateBlue(new RGB(106, 90, 205), true), //
    MediumSlateBlue(new RGB(123, 104, 238), true), //
    LightSlateBlue(new RGB(132, 112, 255), true), //
    RoyalBlue(new RGB(65, 105, 225), true), //
    DodgerBlue(new RGB(30, 144, 255), true), //
    DeepSkyBlue(new RGB(0, 191, 255), true), //
    SkyBlue(new RGB(135, 206, 235), true), //
    LightSkyBlue(new RGB(135, 206, 250), true), //
    SteelBlue(new RGB(70, 130, 180), false), //
    LightSteelBlue(new RGB(176, 196, 222), true), //
    PowderBlue(new RGB(176, 224, 230), true), //
    PaleTurquoise(new RGB(175, 238, 238), true), //
    DarkTurquoise(new RGB(0, 206, 209), true), //
    MediumTurquoise(new RGB(72, 209, 204), true), //
    Turquoise(new RGB(64, 224, 208), true), //
    LightCyan(new RGB(224, 255, 255), true), //
    MediumAquamarine(new RGB(102, 205, 170), true), //
    Aquamarine(new RGB(127, 255, 212), true), //
    DarkSeaGreen(new RGB(143, 188, 143), false), //
    MediumSeaGreen(new RGB(60, 179, 113), false), //
    LightSeaGreen(new RGB(32, 178, 170), true), //
    PaleGreen(new RGB(152, 251, 152), true), //
    SpringGreen(new RGB(0, 255, 127), true), //
    LawnGreen(new RGB(124, 252, 0), true), //
    Chartreuse(new RGB(127, 255, 0), true), //
    MediumSpringGreen(new RGB(0, 250, 154), true), //
    GreenYellow(new RGB(173, 255, 47), true), //
    LimeGreen(new RGB(50, 205, 50), true), //
    YellowGreen(new RGB(154, 205, 50), true), //
    Khaki(new RGB(240, 230, 140), true), //
    PaleGoldenrod(new RGB(238, 232, 170), true), //
    LightGoldenrodYellow(new RGB(250, 250, 210), true), //
    LightYellow(new RGB(255, 255, 224), true), //
    Gold(new RGB(255, 215, 0), true), //
    LightGoldenrod(new RGB(238, 221, 130), true), //
    Goldenrod(new RGB(218, 165, 32), true), //
    IndianRed(new RGB(205, 92, 92), true), //
    Peru(new RGB(205, 133, 63), true), //
    Beige(new RGB(245, 245, 220), true), //
    Wheat(new RGB(245, 222, 179), true), //
    SandyBrown(new RGB(244, 164, 96), true), //
    DarkSalmon(new RGB(233, 150, 122), true), //
    LightSalmon(new RGB(255, 160, 122), true), //
    Orange(new RGB(255, 165, 0), true), //
    Coral(new RGB(255, 127, 80), true), //
    LightCoral(new RGB(240, 128, 128), true), //
    HotPink(new RGB(255, 105, 180), true), //
    DeepPink(new RGB(255, 20, 147), true), //
    Pink(new RGB(255, 192, 203), true), //
    LightPink(new RGB(255, 182, 193), true), //
    PaleVioletRed(new RGB(219, 112, 147), true), //
    Maroon(new RGB(176, 48, 96), true), //
    VioletRed(new RGB(208, 32, 144), true), //
    Magenta(new RGB(255, 0, 255), true), //
    Violet(new RGB(238, 130, 238), true), //
    Plum(new RGB(221, 160, 221), true), //
    Orchid(new RGB(218, 112, 214), true), //
    BlueViolet(new RGB(138, 43, 226), true), //
    Purple(new RGB(160, 32, 240), true), //
    Thistle(new RGB(216, 191, 216), true), //
    Seashell1(new RGB(255, 245, 238), true), //
    Seashell2(new RGB(238, 229, 222), true), //
    Seashell3(new RGB(205, 197, 191), true), //
    Seashell4(new RGB(139, 134, 130), true), //
    Bisque2(new RGB(238, 213, 183), true), //
    Bisque3(new RGB(205, 183, 158), true), //
    PeachPuff1(new RGB(255, 218, 185), true), //
    PeachPuff2(new RGB(238, 203, 173), true), //
    NavajoWhite2(new RGB(238, 207, 161), true), //
    NavajoWhite3(new RGB(205, 179, 139), true), //
    LemonChiffon1(new RGB(255, 250, 205), true), //
    LemonChiffon2(new RGB(238, 233, 191), true), //
    LemonChiffon3(new RGB(205, 201, 165), true), //
    Cornsilk2(new RGB(238, 232, 205), true), //
    Cornsilk3(new RGB(205, 200, 177), true), //
    Ivory2(new RGB(238, 238, 224), true), //
    Ivory3(new RGB(205, 205, 193), true), //
    Honeydew2(new RGB(224, 238, 224), true), //
    LavenderBlush2(new RGB(238, 224, 229), true), //
    MistyRose2(new RGB(238, 213, 210), true), //
    MistyRose3(new RGB(205, 183, 181), true), //
    Azure2(new RGB(224, 238, 238), true), //
    Azure3(new RGB(193, 205, 205), true), //
    SlateBlue1(new RGB(131, 111, 255), true), //
    SlateBlue2(new RGB(122, 103, 238), true), //
    SlateBlue3(new RGB(105, 89, 205), true), //
    RoyalBlue1(new RGB(72, 118, 255), true), //
    RoyalBlue2(new RGB(67, 110, 238), true), //
    DodgerBlue1(new RGB(30, 144, 255), true), //
    DodgerBlue2(new RGB(28, 134, 238), true), //
    DodgerBlue3(new RGB(24, 116, 205), true), //
    SteelBlue1(new RGB(99, 184, 255), true), //
    SteelBlue2(new RGB(92, 172, 238), true), //
    DeepSkyBlue1(new RGB(0, 191, 255), true), //
    DeepSkyBlue2(new RGB(0, 178, 238), true), //
    SkyBlue1(new RGB(135, 206, 255), true), //
    SkyBlue2(new RGB(126, 192, 238), true), //
    SkyBlue3(new RGB(108, 166, 205), true), //
    LightSkyBlue1(new RGB(176, 226, 255), true), //
    LightSkyBlue2(new RGB(164, 211, 238), true), //
    LightSkyBlue3(new RGB(141, 182, 205), true), //
    LightSteelBlue2(new RGB(188, 210, 238), true), //
    LightSteelBlue3(new RGB(162, 181, 205), true), //
    LightBlue1(new RGB(191, 239, 255), true), //
    LightBlue2(new RGB(178, 223, 238), true), //
    LightBlue3(new RGB(154, 192, 205), true), //
    PaleTurquoise1(new RGB(187, 255, 255), true), //
    PaleTurquoise2(new RGB(174, 238, 238), true), //
    PaleTurquoise3(new RGB(150, 205, 205), true), //
    CadetBlue2(new RGB(142, 229, 238), true), //
    CadetBlue3(new RGB(122, 197, 205), true), //
    LightGoldenrod1(new RGB(255, 236, 139), true), //
    LightGoldenrod2(new RGB(238, 220, 130), true), //
    LightGoldenrod3(new RGB(205, 190, 112), true); //

    private RGB components;

    /* is this color look good against a black background like a map? */
    private boolean highContrast = false;

    private GlobalColor(RGB comp, boolean hiContrast) {
        components = comp;
        highContrast = hiContrast;
    }

    public static Color get(GlobalColor gc) {
        return SWTResourceManager.getColor(gc.components.red,
                gc.components.green, gc.components.blue);
    }

    public RGB getRGB() {
        return components;
    }

    public static int size() {
        return GlobalColor.values().length;
    }

    public static GlobalColor getAtOrdinal(int index) {
        return GlobalColor.values()[index];
    }

    public boolean isHighContrast() {
        return highContrast;
    }

}
