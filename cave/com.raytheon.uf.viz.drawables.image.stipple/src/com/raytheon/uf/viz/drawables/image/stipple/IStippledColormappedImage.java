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
package com.raytheon.uf.viz.drawables.image.stipple;

import java.util.List;

import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.viz.core.drawables.IColormappedImage;

/**
 * 
 * An extension of {@link IColormappedImage} that also supports using a
 * stipple(fill pattern) to differentiate different areas of the image based off
 * the data values. For example it is possible to render low values as
 * horizontal lines and high values as vertical lines. Commonly used patterns
 * include different sizes or densities of dots or different widths or angles of
 * lines but the pattern is specified as a 32x32 bitmask to provide flexibility
 * for custom patterns. The different fill patterns are applied to the data
 * values using the ranges in the {@link ColorMapParameters} in the same way
 * that colors are applied. If there is the same number of patterns and colors
 * then then each color will be rendered in a different pattern. It is valid to
 * have more or less patterns than colors to achieve different color/pattern
 * combination.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -----------------
 * Nov 01, 2016  5957     bsteffen  Initial creation
 * 
 * </pre>
 *
 * @author bsteffen
 */
public interface IStippledColormappedImage
        extends IColormappedImage {

    /**
     * Specify the fill patterns to use. Each pattern must by a byte array of
     * length 128 where each bit represents a single pixel in the pattern. The
     * List may contain null elements to indicate a solid, completely filled
     * pattern. If the list only contains one element, that pattern will be
     * applied to the entire image. The List of patterns may also be null which
     * will display exactly like a standard {@link IColormappedImage}.
     * 
     * @param pattern
     *            the 32x32 bitpattern to use for filling
     */
    public void setFillPatterns(List<byte[]> fillPatterns);

}