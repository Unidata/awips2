/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.viz.aviation.climatology;

import java.io.File;
import java.io.IOException;

import org.apache.commons.configuration.ConfigurationException;
import org.apache.commons.configuration.INIConfiguration;
import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.viz.core.RGBColors;

/**
 * WindRoseConfigData class the configuration data for wind rose.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 18 JUN 2008  1119       lvenable    Initial creation.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class WindRoseConfigData implements Cloneable {
    /**
     * Calm RGB value.
     */
    private RGB calmRgb = new RGB(255, 255, 255);

    /**
     * Variable RGB value.
     */
    private RGB variableRgb = new RGB(141, 69, 20);

    /**
     * Variable 1 RGB value (defined as 0 to v1 knots).
     */
    private RGB var1Rgb = new RGB(0, 0, 255);

    /**
     * Variable 2 RGB value (defined as v1 to v2 knots).
     */
    private RGB var2Rgb = new RGB(0, 255, 0);

    /**
     * Variable 3 RGB value (defined as v2 to v3 knots).
     */
    private RGB var3Rgb = new RGB(255, 0, 0);

    /**
     * Above RGB value (defined as above v3 knots).
     */
    private RGB aboveRgb = new RGB(175, 0, 230);

    /**
     * Default Calm RGB value.
     */
    private RGB defaultCalmRgb = new RGB(255, 255, 255);

    /**
     * Default Variable RGB value.
     */
    private RGB defaultVariableRgb = new RGB(141, 69, 20);

    /**
     * Default Variable 1 RGB value (defined as 0 to v1 knots).
     */
    private RGB defaultVar1Rgb = new RGB(0, 0, 255);

    /**
     * Default Variable 2 RGB value (defined as v1 to v2 knots).
     */
    private RGB defaultVar2Rgb = new RGB(0, 255, 0);

    /**
     * Default Variable 3 RGB value (defined as v2 to v3 knots).
     */
    private RGB defaultVar3Rgb = new RGB(255, 0, 0);

    /**
     * Default Above RGB value (defined as above v3 knots).
     */
    private RGB defaultAboveRgb = new RGB(175, 0, 230);

    /**
     * Default maximum value for wind variable 1.
     */
    private int var1Max = 5;

    /**
     * Default maximum value for wind variable 2.
     */
    private int var2Max = 12;

    /**
     * Default maximum value for wind variable 3.
     */
    private int var3Max = 20;

    /**
     * Number of wind direction points on the wind rose diagram.
     */
    private int points = 36;

    /**
     * Constructor.
     */
    public WindRoseConfigData() {
        loadDataFromFile();
    }

    private void loadDataFromFile() {
        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationFile lFile = pm
                .getStaticLocalizationFile("aviation/config/windrose.cfg");

        File file = lFile.getFile();

        try {
            if (!file.exists()) {
                throw new IOException("Error: file " + file.getPath()
                        + " does not exist.");
            } else {
                INIConfiguration config = new INIConfiguration(file);
                calmRgb = RGBColors.getRGBColor(config
                        .getString("wind_calm.color"));
                variableRgb = RGBColors.getRGBColor(config
                        .getString("wind_vrb.color"));
                var1Rgb = RGBColors.getRGBColor(config
                        .getString("wind_1.color"));
                var2Rgb = RGBColors.getRGBColor(config
                        .getString("wind_2.color"));
                var3Rgb = RGBColors.getRGBColor(config
                        .getString("wind_3.color"));
                aboveRgb = RGBColors.getRGBColor(config
                        .getString("wind_4.color"));
                var1Max = config.getInt("wind_1.value");
                var2Max = config.getInt("wind_2.value");
                var3Max = config.getInt("wind_3.value");
                points = config.getInt("wind.num_dir");
            }
        } catch (IOException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        } catch (ConfigurationException e) {
            // TODO Auto-generated catch block
            System.err.println(e.getStackTrace());
        }
    }

    /**
     * Constructor.
     * 
     * @param calmRgb
     *            Calm RGB value.
     * @param variableRgb
     *            Variable RGB value.
     * @param var1Rgb
     *            Wind Variable 1 RGB value.
     * @param var2Rgb
     *            Wind Variable 2 RGB value.
     * @param var3Rgb
     *            Wind Variable 3 RGB value.
     * @param aboveRgb
     *            Above Wind Variable 3 RGB value.
     * @param var1Max
     *            Maximum value for wind variable 1.
     * @param var2Max
     *            Maximum value for wind variable 2.
     * @param var3Max
     *            Maximum value for wind variable 3.
     * @param points
     *            Number of wind direction points on the wind rose diagram.
     */
    public WindRoseConfigData(RGB calmRgb, RGB variableRgb, RGB var1Rgb,
            RGB var2Rgb, RGB var3Rgb, RGB aboveRgb, int var1Max, int var2Max,
            int var3Max, int points) {
        this.calmRgb = calmRgb;
        this.variableRgb = variableRgb;
        this.var1Rgb = var1Rgb;
        this.var2Rgb = var2Rgb;
        this.var3Rgb = var3Rgb;
        this.aboveRgb = aboveRgb;
        this.var1Max = var1Max;
        this.var2Max = var2Max;
        this.var3Max = var3Max;
        this.points = points;
    }

    /**
     * Get the Calm RGB value.
     * 
     * @return The Calm RGB value.
     */
    public RGB getCalmRgb() {
        return calmRgb;
    }

    /**
     * Set the Calm RGB value.
     * 
     * @param The
     *            Calm RGB value.
     */
    public void setCalmRgb(RGB calmRgb) {
        this.calmRgb = calmRgb;
    }

    /**
     * Get the Variable RGB value.
     * 
     * @return The Variable RGB value.
     */
    public RGB getVariableRgb() {
        return variableRgb;
    }

    /**
     * Set the Variable RGB value.
     * 
     * @param variableRgb
     *            The Variable RGB value.
     */
    public void setVariableRgb(RGB variableRgb) {
        this.variableRgb = variableRgb;
    }

    /**
     * Get the wind variable 1 RGB value (defined as 0 to v1 knots).
     * 
     * @return The wind variable 1 RGB value.
     */
    public RGB getVar1Rgb() {
        return var1Rgb;
    }

    /**
     * Set the wind variable 1 RGB value (defined as 0 to v1 knots).
     * 
     * @param var1Rgb
     *            The wind variable 1 RGB value.
     */
    public void setVar1Rgb(RGB var1Rgb) {
        this.var1Rgb = var1Rgb;
    }

    /**
     * Get the wind variable 2 RGB value (defined as v1 to v2 knots).
     * 
     * @return The wind variable 2 RGB value.
     */
    public RGB getVar2Rgb() {
        return var2Rgb;
    }

    /**
     * Set the wind variable 2 RGB value (defined as v1 to v2 knots).
     * 
     * @param var2Rgb
     *            The wind variable 2 RGB value.
     */
    public void setVar2Rgb(RGB var2Rgb) {
        this.var2Rgb = var2Rgb;
    }

    /**
     * Get the wind variable 3 RGB value (defined as v2 to v3 knots).
     * 
     * @return The wind variable 3 RGB value.
     */
    public RGB getVar3Rgb() {
        return var3Rgb;
    }

    /**
     * Set the wind variable 3 RGB value (defined as v2 to v3 knots).
     * 
     * @param var3Rgb
     *            The wind variable 3 RGB value.
     */
    public void setVar3Rgb(RGB var3Rgb) {
        this.var3Rgb = var3Rgb;
    }

    /**
     * Get the above wind variable 3 RGB value (defined as wind greater than v3
     * knots).
     * 
     * @return The above wind variable 3 RGB value.
     */
    public RGB getAboveRgb() {
        return aboveRgb;
    }

    /**
     * Set the above wind variable 3 RGB value (defined as wind greater than v3
     * knots).
     * 
     * @param aboveRgb
     *            The above wind variable 3 RGB value.
     */
    public void setAboveRgb(RGB aboveRgb) {
        this.aboveRgb = aboveRgb;
    }

    /**
     * Get the maximum value for the wind variable 1 value (defined as 0 to v1
     * knots).
     * 
     * @return The maximum value for the wind variable 1.
     */
    public int getVar1Max() {
        return var1Max;
    }

    /**
     * Set the maximum value for the wind variable 1 value (defined as 0 to v1
     * knots).
     * 
     * @param var1Max
     *            The maximum value for the wind variable 1.
     */
    public void setVar1Max(int var1Max) {
        this.var1Max = var1Max;
    }

    /**
     * Get the maximum value for the wind variable 2 value (defined as v1 to v2
     * knots).
     * 
     * @return The maximum value for the wind variable 2.
     */
    public int getVar2Max() {
        return var2Max;
    }

    /**
     * Set the maximum value for the wind variable 2 value (defined as v1 to v2
     * knots).
     * 
     * @param var2Max
     *            The maximum value for the wind variable 2.
     */
    public void setVar2Max(int var2Max) {
        this.var2Max = var2Max;
    }

    /**
     * Get the maximum value for the wind variable 3 value (defined as v2 to v3
     * knots).
     * 
     * @return The maximum value for the wind variable 3.
     */
    public int getVar3Max() {
        return var3Max;
    }

    /**
     * Set the maximum value for the wind variable 3 value (defined as v2 to v3
     * knots).
     * 
     * @param var3Max
     *            The maximum value for the wind variable 3.
     */
    public void setVar3Max(int var3Max) {
        this.var3Max = var3Max;
    }

    /**
     * Get the number of wind directions to be displayed on the wind rose
     * diagram. The possible values are 8, 16, or 32.
     * 
     * @return The number of wind directions (points).
     */
    public int getPoints() {
        return points;
    }

    /**
     * Set the number of wind directions to be displayed on the wind rose
     * diagram. The possible values are 8, 16, or 32. If other values are
     * specified then the points will default to 36.
     * 
     * @param points
     *            Number of wind directions (points).
     */
    public void setPoints(int points) {
        if (points == 8 || points == 16 || points == 36) {
            this.points = points;
        } else {
            this.points = 36;
        }
    }

    /**
     * Check if the Calm color is the default Calm color.
     * 
     * @return True if the colors are the same, false otherwise.
     */
    public boolean calmDefaultColor() {
        if (defaultCalmRgb.equals(calmRgb)) {
            return true;
        }

        return false;
    }

    /**
     * Check if the Variable color is the default Variable color.
     * 
     * @return True if the colors are the same, false otherwise.
     */
    public boolean variableDefaultColor() {
        if (defaultVariableRgb.equals(variableRgb)) {
            return true;
        }

        return false;
    }

    /**
     * Check if the Wind Variable 1 color is the default Wind Variable 1 color.
     * 
     * @return True if the colors are the same, false otherwise.
     */
    public boolean var1DefaultColor() {
        if (defaultVar1Rgb.equals(var1Rgb)) {
            return true;
        }

        return false;
    }

    /**
     * Check if the Wind Variable 2 color is the default Wind Variable 2 color.
     * 
     * @return True if the colors are the same, false otherwise.
     */
    public boolean var2DefaultColor() {
        if (defaultVar2Rgb.equals(var2Rgb)) {
            return true;
        }

        return false;
    }

    /**
     * Check if the Wind Variable 3 color is the default Wind Variable 3 color.
     * 
     * @return True if the colors are the same, false otherwise.
     */
    public boolean var3DefaultColor() {
        if (defaultVar3Rgb.equals(var3Rgb)) {
            return true;
        }

        return false;
    }

    /**
     * Check if the Above Wind Variable 3 color is the default Above Wind
     * Variable 3 color.
     * 
     * @return True if the colors are the same, false otherwise.
     */
    public boolean aboveDefaultColor() {
        if (defaultAboveRgb.equals(aboveRgb)) {
            return true;
        }

        return false;
    }

    /**
     * Return a clone of WindRoseConfigData data.
     * 
     * @return A clone of the WindRoseConfigData data.
     */
    public WindRoseConfigData cloneData() {
        try {
            return (WindRoseConfigData) super.clone();
        } catch (CloneNotSupportedException cnse) {
            System.out.println("Clone not supported...");
            cnse.printStackTrace();
            return this;
        }
    }
}