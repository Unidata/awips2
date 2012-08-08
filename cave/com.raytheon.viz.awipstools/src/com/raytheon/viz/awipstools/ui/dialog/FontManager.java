/*****************************************************************************************
 * COPYRIGHT (c), 2007, RAYTHEON COMPANY
 * ALL RIGHTS RESERVED, An Unpublished Work 
 *
 * RAYTHEON PROPRIETARY
 * If the end user is not the U.S. Government or any agency thereof, use
 * or disclosure of data contained in this source code file is subject to
 * the proprietary restrictions set forth in the Master Rights File.
 *
 * U.S. GOVERNMENT PURPOSE RIGHTS NOTICE
 * If the end user is the U.S. Government or any agency thereof, this source
 * code is provided to the U.S. Government with Government Purpose Rights.
 * Use or disclosure of data contained in this source code file is subject to
 * the "Government Purpose Rights" restriction in the Master Rights File.
 *
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * Use or disclosure of data contained in this source code file is subject to
 * the export restrictions set forth in the Master Rights File.
 ******************************************************************************************/
package com.raytheon.viz.awipstools.ui.dialog;

import org.eclipse.jface.resource.FontRegistry;
import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.FontData;

/**
 * Class with a static method for obtaining a font. This handles registering the
 * requested font so disposing of the font is taken care of.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 31 2012  #875       rferrel     Initial creation
 * 
 * </pre>
 * 
 * @author rferrel
 */
public class FontManager {
    private FontManager() {

    }

    /**
     * Returns the font based on its name, height and style.
     * 
     * @param name
     *            the name of the font (must not be null)
     * @param height
     *            the font height in points
     * @param style
     *            a bitwise combination of NORMAL, ITALIC and BOLD
     * @return font
     */
    public static Font getFont(String name, int size, int style) {
        FontRegistry registry = JFaceResources.getFontRegistry();
        String symbolicName = name + '|' + size + '|' + style;
        Font font = registry.get(symbolicName);
        if (font == null) {
            FontData fontData = new FontData(name, size, style);
            registry.put(symbolicName, new FontData[] { fontData });
            font = registry.get(symbolicName);
        }
        return font;
    }
}