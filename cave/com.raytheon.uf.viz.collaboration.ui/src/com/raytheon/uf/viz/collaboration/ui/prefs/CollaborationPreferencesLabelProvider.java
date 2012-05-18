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
package com.raytheon.uf.viz.collaboration.ui.prefs;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.jface.resource.StringConverter;
import org.eclipse.jface.viewers.ColumnLabelProvider;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.FontData;
import org.eclipse.swt.widgets.Display;

import com.raytheon.uf.viz.collaboration.data.AlertWord;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 17, 2012            mnash     Initial creation
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class CollaborationPreferencesLabelProvider extends ColumnLabelProvider {

    private List<Color> colors = null;

    private List<Font> fonts = null;

    /**
     * 
     */
    public CollaborationPreferencesLabelProvider() {
        colors = new ArrayList<Color>();
        fonts = new ArrayList<Font>();
    }

    @Override
    public String getText(Object element) {
        AlertWord word = (AlertWord) element;
        return word.getText();
    }

    @Override
    public Color getForeground(Object element) {
        AlertWord word = (AlertWord) element;
        Color color = new Color(Display.getCurrent(), word.getRed(),
                word.getGreen(), word.getBlue());
        colors.add(color);
        return color;
    }

    @Override
    public Font getFont(Object element) {
        AlertWord word = (AlertWord) element;
        String fontString = word.getFont();
        Font font = null;
        if (fontString != null) {
            FontData data = StringConverter.asFontData(fontString);
            font = new Font(Display.getCurrent(), data);
            fonts.add(font);
        }
        return font;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.viewers.BaseLabelProvider#dispose()
     */
    @Override
    public void dispose() {
        for (Font font : fonts) {
            font.dispose();
        }
        for (Color color : colors) {
            color.dispose();
        }
        super.dispose();
    }
}
