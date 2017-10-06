package gov.noaa.gsd.viz.ensemble.util;

import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.RGB;

/**
 * This class is used to map unique colors to given GFS ensemble perturbation
 * members, which are identified by their hard-coded perturbation memeber names
 * (ctl1, ctl2, n1, n2 ... p4, p5). It has been created in support of
 * simplifying the process of allowing the user to color an entire ensemble set
 * of members using a color gradient.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 8, 2014    5056     polster     Initial creation
 * 
 * </pre>
 * 
 * @author polster
 * @version 1.0
 */
public class ChosenGEFSColors {

    static public ChosenGEFSColors getInstance() {
        if (SINGLETON == null) {
            SINGLETON = new ChosenGEFSColors();
        }
        return SINGLETON;
    }

    static private ChosenGEFSColors SINGLETON = null;

    private ChosenGEFSColors() {

    }

    private Color color = GlobalColor.get(GlobalColor.NEON_PURPLE);

    public Color getColor() {
        if (color.isDisposed()) {
            color = GlobalColor.get(GlobalColor.NEON_PURPLE);
        }
        return color;
    }

    public void setColor(Color c) {
        color = c;
    }

    public Color getGradientByEnsembleId(String name) {

        Color c = null;
        final int totalSteps = 20;

        int pertNumber = getPerturbationIndex(name);
        c = getGradientColor(color, totalSteps, pertNumber);
        return c;

    }

    private Color getGradientColor(Color c, int totalSteps, int currStep) {

        RGB rgb = c.getRGB();
        float[] hsb = rgb.getHSB();
        hsb[1] = (float) currStep / (float) totalSteps;
        RGB nrgb = new RGB(hsb[0], hsb[1], hsb[2]);
        return SWTResourceManager.getColor(nrgb);
    }

    public String getSrefPerturbationPrefix(String member) {
        return member.replaceAll("[0-9]", "");
    }

    /*
     * TODO: this is a poor man's solution working against hard-coded names.
     */
    public int getPerturbationIndex(String member) {

        int pert = 0;
        if (member.startsWith("ctll0")) {
            pert = 1;
        } else if (member.startsWith("p1")) {
            pert = 2;
        } else if (member.startsWith("p2")) {
            pert = 3;
        } else if (member.startsWith("p3")) {
            pert = 4;
        } else if (member.startsWith("p4")) {
            pert = 5;
        } else if (member.startsWith("p5")) {
            pert = 6;
        } else if (member.startsWith("p6")) {
            pert = 7;
        } else if (member.startsWith("p7")) {
            pert = 8;
        } else if (member.startsWith("p8")) {
            pert = 9;
        } else if (member.startsWith("p9")) {
            pert = 10;
        } else if (member.startsWith("p10")) {
            pert = 11;
        } else if (member.startsWith("p11")) {
            pert = 12;
        } else if (member.startsWith("p12")) {
            pert = 13;
        } else if (member.startsWith("p13")) {
            pert = 14;
        } else if (member.startsWith("p14")) {
            pert = 15;
        } else if (member.startsWith("p15")) {
            pert = 16;
        } else if (member.startsWith("p16")) {
            pert = 17;
        } else if (member.startsWith("p17")) {
            pert = 18;
        } else if (member.startsWith("p18")) {
            pert = 19;
        } else if (member.startsWith("p19")) {
            pert = 20;
        }

        return pert;
    }

}
