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
package com.raytheon.viz.gfe.rsc;

import org.eclipse.jface.resource.StringConverter;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Device;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.FontData;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.viz.gfe.Activator;
import com.raytheon.viz.gfe.GFEPreference;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 14, 2010            randerso     Initial creation
 * Apr 27, 2011 #9250      bkowal       getStyle and getName are now used to
 *                                      get the style and name associated with
 *                                      a FontData object.
 * Nov 20, 2013 #2488      randerso     Changed to use DejaVu fonts
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class GFEFonts {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(GFEFonts.class);

    private static final String DEFAULT_FONT_NAME = "DejaVu Sans Mono";

    private static final int[] DEFAULT_FONT_SIZE = { 9, 9, 12, 14, 20 };

    private static final int DEFAULT_FONT_STYLE = SWT.BOLD;

    private static final int NUM_FONTS = DEFAULT_FONT_SIZE.length;

    private static FontData[] fontData;

    /**
     * Retrieves FontData for one of the five predefined GFE fonts. These are
     * defined in gfeConfig in the TextFontn settings
     * 
     * @param size
     *            GFE font size 0-4
     * @return FontData for the requested font
     */
    public static FontData getFontData(int size) {
        int i = size;
        if (i < 0) {
            i = 0;
        } else if (i > NUM_FONTS - 1) {
            i = NUM_FONTS - 1;
        }
        return getFontData()[i];
    }

    private static synchronized FontData[] getFontData() {
        if (fontData == null) {
            fontData = new FontData[NUM_FONTS];

            for (int i = 0; i < NUM_FONTS; i++) {
                String s = "TextFont" + i;
                try {
                    String fontString = Activator.getDefault()
                            .getPreferenceStore().getString(s);
                    fontData[i] = StringConverter.asFontData(fontString);
                } catch (Throwable e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Error loading GFE font " + s
                                    + " using default font.", e);
                    fontData[i] = new FontData(DEFAULT_FONT_NAME,
                            DEFAULT_FONT_SIZE[i], DEFAULT_FONT_STYLE);
                }
            }
        }
        return fontData;
    }

    /**
     * Returns the desired GFE font for the specified device. The caller is
     * responsible for disposing the font.
     * 
     * @param device
     *            SWT graphics device
     * @param size
     *            GFE font size 0-4
     * @return desired GFE font
     */
    public static Font getFont(Device device, int size) {
        return new Font(device, getFontData(size));
    }

    /**
     * Returns the desired GFE font for the specified target. The caller is
     * responsible for disposing the font.
     * 
     * @param target
     *            IGraphicsTarget
     * @param size
     *            GFE font size 0-4
     * @return desired GFE font
     */
    public static IFont getFont(IGraphicsTarget target, int size) {
        FontData fd = getFontData(size);

        IFont.Style[] style;
        switch (fd.getStyle()) {
        case SWT.BOLD:
            style = new IFont.Style[] { IFont.Style.BOLD };
            break;
        case SWT.ITALIC:
            style = new IFont.Style[] { IFont.Style.ITALIC };
            break;
        case SWT.BOLD | SWT.ITALIC:
            style = new IFont.Style[] { IFont.Style.BOLD, IFont.Style.ITALIC };
            break;
        case SWT.NORMAL:
        default:
            style = new IFont.Style[0];
        }

        IFont font = target.initializeFont(fd.getName(), fd.height * 1.2f,
                style);
        font.setSmoothing(false);
        font.setScaleFont(false);
        return font;
    }

    /**
     * The map uses font settings like other GFE components, but can't use the
     * fonts returned by GFEFonts directly because they're SWT fonts and it
     * needs IFonts. This method does the conversion, taking a target to build
     * the IFont, a configuration setting to look up, and a default font number
     * to use if the setting isn't found.
     * <p>
     * It is the caller's responsibility to call dispose() on the returned font
     * when it is no longer needed.
     * 
     * @param target
     *            The graphics target that builds the IFont.
     * @param setting
     *            The configuration setting name to check for a font number
     *            override.
     * @param fontNum
     *            The default GFE font number.
     * @return an IFont
     */
    public static IFont makeGFEIFont(IGraphicsTarget target, String setting,
            int fontNum) {
        if (GFEPreference.contains(setting)) {
            fontNum = GFEPreference.getIntPreference(setting);
        }

        return getFont(target, fontNum);
    }

    /**
     * Get the number of predefined GFE fonts.
     * 
     * @return The number of GFE fonts
     */
    public static int getNumFonts() {
        return NUM_FONTS;
    }
}
