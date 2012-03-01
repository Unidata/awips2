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
package com.raytheon.viz.core.gl.internal;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.jface.resource.FontRegistry;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.FontData;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.viz.core.gl.IGLFont;

/**
 * Class for construction fonts that are based on editable preferences
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 19, 2010            mschenke     Initial creation
 * Apr 27, 2011 #9250      bkowal       getStyle and getName are now used
 *                                      to get the style and name associated
 *                                      with a FontData object.
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class FontFactory {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(FontFactory.class);

    public static final String DEFAULT_FONT_ID = "com.raytheon.uf.viz.core.defaultFont";

    private static FontFactory instance;

    private FontRegistry registry;

    public synchronized static FontFactory getInstance() {
        if (instance == null) {
            instance = new FontFactory();
        }
        return instance;
    }

    public FontFactory() {
        registry = PlatformUI.getWorkbench().getThemeManager()
                .getCurrentTheme().getFontRegistry();
    }

    public boolean hasId(String fontId) {
        return registry.hasValueFor(fontId);
    }

    /**
     * Given the fontId, intialize a font using the application preferences
     * 
     * @param fontId
     * @param target
     * @return The font to use, never null
     */
    public IGLFont getFont(String fontId) {
        if (!registry.hasValueFor(fontId)) {
            statusHandler.handle(Priority.PROBLEM,
                    "No font registered with id: " + fontId);
        }
        FontData[] data = registry.getFontData(fontId);
        if (data.length == 0) {
            statusHandler.handle(Priority.PROBLEM,
                    "No font data found for id: " + fontId);
        }
        FontData fd = data[0];
        float size = fd.height;
        String name = fd.getName();
        List<IFont.Style> styles = new ArrayList<IFont.Style>();

        int style = fd.getStyle();
        if ((style & SWT.BOLD) != 0) {
            styles.add(IFont.Style.BOLD);
        }
        if ((style & SWT.ITALIC) != 0) {
            styles.add(IFont.Style.ITALIC);
        }

        IGLFont font = new GLFont(name, size,
                styles.toArray(new IFont.Style[styles.size()]));
        if (fontId.equals(DEFAULT_FONT_ID)) {
            font = new UnmodifiableGLFont(font);
        }

        return new PreferenceBasedGLFont(font, fontId, registry);
    }
}
