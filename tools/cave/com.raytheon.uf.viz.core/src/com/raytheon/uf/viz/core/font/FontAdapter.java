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
package com.raytheon.uf.viz.core.font;

import java.awt.Font;

import com.raytheon.uf.viz.core.drawables.IFont;

/**
 * Adapter that transforms IFonts to AWT Fonts
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 16, 2010            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class FontAdapter {

    public static Font getAWTFont(IFont font) {
        int style = Font.PLAIN;
        if (font.getStyle() != null) {
            for (IFont.Style s : font.getStyle()) {
                if (s == IFont.Style.BOLD) {
                    style = (Font.BOLD | style);
                } else if (s == IFont.Style.ITALIC) {
                    style = (Font.ITALIC | style);
                }
            }
        }

        Font awtfont = new Font(font.getFontName(), style,
                (int) font.getFontSize());
        return awtfont;
    }

}
