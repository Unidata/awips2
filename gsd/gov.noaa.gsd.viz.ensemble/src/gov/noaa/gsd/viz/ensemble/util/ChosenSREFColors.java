package gov.noaa.gsd.viz.ensemble.util;

import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.RGB;

/**
 * This class is used to map unique colors to given SREF ensemble perturbation
 * members, which are identified by their hard-coded perturbation memeber names.
 * It has been created in support of simplifying the process of allowing the
 * user to color the different groups of perturbation members (NMM, NMB, EM)
 * with their own color gradient using a base color for each group (e.g. NMM =
 * RED, NMB = BLUE, EM = GREEN).
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 8, 2014    5056    polster     Initial creation
 * 
 * </pre>
 * 
 * @author polster
 * @version 1.0
 */
public class ChosenSREFColors {

    static public ChosenSREFColors getInstance() {
        if (SINGLETON == null) {
            SINGLETON = new ChosenSREFColors();
        }
        return SINGLETON;
    }

    static private ChosenSREFColors SINGLETON = null;

    private ChosenSREFColors() {

    }

    private Color nmm_color = SWTResourceManager.CHERRY_RED;

    private Color nmb_color = SWTResourceManager.SPRING_GREEN;

    private Color em_color = SWTResourceManager.DEEP_BLUE;

    public Color get_EM_color() {
        return em_color;
    }

    public void set_EM_color(Color em_color) {
        this.em_color = em_color;
    }

    public Color get_NMM_color() {
        return nmm_color;
    }

    public void set_NMM_color(Color nmm_color) {
        this.nmm_color = nmm_color;
    }

    public Color get_NMB_color() {
        return nmb_color;
    }

    public void set_NMB_color(Color nmb_color) {
        this.nmb_color = nmb_color;
    }

    // TODO: this is a poor man's solution wokring against hard-coded names:
    // .... such as ctll1, ctll8, ctll15, n1, n2, n3, n4, p5, p6, p7 etc.
    public Color getGradientByEnsembleId(String name) {

        Color c = null;

        float[] saturationSchedule = { 1.0f, 0.70f, 0.60f, 0.50f, 0.40f, 0.25f,
                0.15f };
        int pertNumber = getPerturbationIndex(name);
        int shiftedPertNum = 0;

        Color startColor = null;
        // int totalSteps = 7;

        // first model is NMM ...
        if ((pertNumber >= 1) && (pertNumber <= 7)) {
            startColor = nmm_color;
            c = getGradientColor(startColor, saturationSchedule[pertNumber - 1]);
        }

        // second model is NMB ...
        if ((pertNumber >= 8) && (pertNumber <= 14)) {
            startColor = nmb_color;
            shiftedPertNum = pertNumber - 7;
            c = getGradientColor(startColor,
                    saturationSchedule[(shiftedPertNum - 1)]);
        }

        // third model is EM ...
        if ((pertNumber >= 15) && (pertNumber <= 21)) {
            startColor = em_color;
            shiftedPertNum = pertNumber - 14;
            c = getGradientColor(startColor,
                    saturationSchedule[(shiftedPertNum - 1)]);
        }
        return c;

    }

    @SuppressWarnings("unused")
    private Color getGradientColor(Color c, int totalSteps, int currStep) {

        RGB rgb = c.getRGB();
        float[] hsb = rgb.getHSB();
        hsb[1] = (float) currStep / (float) totalSteps;
        RGB nrgb = new RGB(hsb[0], hsb[1], hsb[2]);
        return SWTResourceManager.getColor(nrgb);
    }

    private Color getGradientColor(Color c, float ratio) {

        RGB rgb = c.getRGB();
        float[] hsb = rgb.getHSB();
        hsb[1] = ratio;
        RGB nrgb = new RGB(hsb[0], hsb[1], hsb[2]);
        return SWTResourceManager.getColor(nrgb);
    }

    public String getPerturbationPrefix(String member) {
        return member.replaceAll("[0-9]", "");
    }

    public int getPerturbationIndex(String member) {

        String pertNumStr = member.replaceAll("[^0-9]", "");
        int pert = Integer.parseInt(pertNumStr);
        return pert;

    }

}
