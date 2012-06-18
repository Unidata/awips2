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
package com.raytheon.uf.viz.collaboration.ui.data;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 16, 2012            mnash     Initial creation
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

@DynamicSerialize
public class AlertWord {
    @DynamicSerializeElement
    private String text;

    @DynamicSerializeElement
    private Integer red;

    @DynamicSerializeElement
    private Integer green;

    @DynamicSerializeElement
    private Integer blue;

    @DynamicSerializeElement
    private String soundPath;

    @DynamicSerializeElement
    private String font;

    public AlertWord() {

    }

    public AlertWord(String text, RGB color) {
        this.text = text;
        red = color.red;
        green = color.green;
        blue = color.blue;
    }

    /**
     * @return the text
     */
    public String getText() {
        return text;
    }

    /**
     * @param text
     *            the text to set
     */
    public void setText(String text) {
        this.text = text;
    }

    /**
     * @return the red
     */
    public Integer getRed() {
        return red;
    }

    /**
     * @param red
     *            the red to set
     */
    public void setRed(Integer red) {
        this.red = red;
    }

    /**
     * @return the green
     */
    public Integer getGreen() {
        return green;
    }

    /**
     * @param green
     *            the green to set
     */
    public void setGreen(Integer green) {
        this.green = green;
    }

    /**
     * @return the blue
     */
    public Integer getBlue() {
        return blue;
    }

    /**
     * @param blue
     *            the blue to set
     */
    public void setBlue(Integer blue) {
        this.blue = blue;
    }

    /**
     * @return the soundPath
     */
    public String getSoundPath() {
        return soundPath;
    }

    /**
     * @param soundPath
     *            the soundPath to set
     */
    public void setSoundPath(String soundPath) {
        this.soundPath = soundPath;
    }

    /**
     * @return the font
     */
    public String getFont() {
        return font;
    }

    /**
     * @param font
     *            the font to set
     */
    public void setFont(String font) {
        this.font = font;
    }
}
