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
package com.raytheon.viz.gfe.ui;

import java.awt.geom.Rectangle2D;

import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.IFont;

/**
 * GfeUiUtil is a utility class that contains a collection of useful functions
 * for working with GFE's UI. This class cannot be instantiated.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 15, 2010            dgilling     Initial creation
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

public class GfeUiUtil {

    /**
     * A private constructor so that Java does not attempt to create one for us.
     * As this class should not be instantiated, do not attempt to ever call
     * this constructor; it will simply throw an AssertionError.
     * 
     */
    private GfeUiUtil() {
        throw new AssertionError();
    }

    /**
     * Accepts a string and, given some rendering parameters, truncates the
     * string so that it will fit in the provided maximum number of pixels. If
     * desired, an ellipsis will be added to the end of the truncated string.
     * 
     * @param drawTarget
     *            The IGraphicsTarget object representing where the truncated
     *            string will be rendered.
     * @param font
     *            The IFont object representing the font used to render the
     *            truncated string.
     * @param label
     *            The String to be truncated.
     * @param maxLengthPixels
     *            The maximum number of pixels lengthwise this string must fit
     *            into.
     * @param useEllipsis
     *            Whether or not an ellipsis (...) will be appended to this
     *            truncated string.
     * @return The longest portion of label that will fit into maxLengthPixels.
     *         If useEllipsis is true, an ellipsis will be appended to the
     *         truncated string. If needed, even the ellipsis may be truncated
     *         to fit into the desired width.
     */
    public static String truncateLabelToFit(IGraphicsTarget drawTarget,
            IFont font, String label, int maxLengthPixels, boolean useEllipsis) {
        if (maxLengthPixels < 1) {
            return "";
        }

        // okay as is?
        if (textSize(drawTarget, font, label) < maxLengthPixels) {
            return label;
        }

        StringBuilder truncatedLabel = new StringBuilder(label);

        if (useEllipsis) {
            truncatedLabel.append("...");

            // remove the last character before the dots
            if (!label.isEmpty()) {
                truncatedLabel.deleteCharAt(truncatedLabel.length() - 4);
            }

            while ((textSize(drawTarget, font, truncatedLabel.toString()) > maxLengthPixels)
                    && (truncatedLabel.length() > 0)) {
                // remove the last char before the dots
                if (truncatedLabel.length() > 3) {
                    truncatedLabel.deleteCharAt(truncatedLabel.length() - 4);
                } else { // remove dots 1 by 1
                    truncatedLabel.deleteCharAt(0);
                }
            }

        } else { // no dots wanted
            // remove the last character
            truncatedLabel.deleteCharAt(truncatedLabel.length() - 1);

            while ((textSize(drawTarget, font, truncatedLabel.toString()) > maxLengthPixels)
                    && (truncatedLabel.length() > 0)) {
                // remove last character
                truncatedLabel.deleteCharAt(truncatedLabel.length() - 1);
            }
        }

        return truncatedLabel.toString();
    }

    /**
     * Given a string and some rendering parameters, determines the number of
     * pixels length-wise that the string will occupy.
     * 
     * @param drawTarget
     *            The IGraphicsTarget object representing where the string will
     *            be rendered.
     * @param font
     *            The IFont object representing the font used to render the
     *            string.
     * @param label
     *            The string to be rendered.
     * @return The number of pixels that it will take in the x-axis to display
     *         label.
     */
    private static int textSize(IGraphicsTarget drawTarget, IFont font,
            String label) {
        DrawableString string = new DrawableString(label, null);
        string.font = font != null ? font : drawTarget.getDefaultFont();
        Rectangle2D bounds = drawTarget.getStringsBounds(string);

        return (int) Math.ceil(bounds.getWidth());
    }
}
