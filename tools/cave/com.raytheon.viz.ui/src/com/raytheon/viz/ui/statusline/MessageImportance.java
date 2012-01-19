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
package com.raytheon.viz.ui.statusline;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.viz.core.RGBColors;
import com.raytheon.viz.ui.statusline.StatusMessage.Importance;

/**
 * TODO Add Description MessageImportance.java May 19, 2008
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 	May 19, 2008					Eric Babin Initial Creation
 * 
 * </pre>
 * 
 * @author ebabin
 * @version 1.0
 */

public class MessageImportance {

    // TODO: Get timeout from gfeconfig
    private static long DEFAULT_TIMEOUT = 30000;

    public static Map<Importance, MessageImportance> defaultImportanceDict() {
        Map<Importance, MessageImportance> d = new HashMap<Importance, MessageImportance>();

        d.put(Importance.REGULAR, new MessageImportance(RGBColors
                .getRGBColor("green"), RGBColors.getRGBColor("gray40"),
                RGBColors.getRGBColor("gray80"), true, DEFAULT_TIMEOUT, null,
                null, null));

        d.put(Importance.SIGNIFICANT, new MessageImportance(RGBColors
                .getRGBColor("yellow"), RGBColors.getRGBColor("black"),
                RGBColors.getRGBColor("gray80"), false, DEFAULT_TIMEOUT,
                "Significant Messages", RGBColors.getRGBColor("black"),
                new RGB(0xEB, 0xEB, 0x00)));

        d.put(Importance.URGENT, new MessageImportance(RGBColors
                .getRGBColor("red2"), RGBColors.getRGBColor("white"), RGBColors
                .getRGBColor("gray40"), false, 0, "Urgent Messages", RGBColors
                .getRGBColor("white"), RGBColors.getRGBColor("red2")));

        d.put(Importance.ALERT, new MessageImportance(RGBColors
                .getRGBColor("orange"), RGBColors.getRGBColor("black"),
                RGBColors.getRGBColor("gray80"), false, DEFAULT_TIMEOUT, null,
                null, null));

        return d;
    }

    private final RGB foregroundColor;

    private final RGB backgroundColor;

    private final RGB dimmedColor;

    private final boolean defaultImportance;

    private final long flashingTimeout;

    private final String bannerName;

    private final RGB bannerFgColor;

    private final RGB bannerBgColor;

    public MessageImportance(RGB foregroundColor, RGB backgroundColor,
            RGB dimmedColor, boolean isDefault, long flashingTimeout,
            String bannerName, RGB bannerFg, RGB bannerBg) {
        this.foregroundColor = foregroundColor;
        this.backgroundColor = backgroundColor;
        this.dimmedColor = dimmedColor;

        this.defaultImportance = isDefault;

        this.flashingTimeout = flashingTimeout;

        this.bannerName = bannerName;
        this.bannerFgColor = bannerFg;
        this.bannerBgColor = bannerBg;
    }

    public RGB getForegroundColor() {
        return foregroundColor;
    }

    public RGB getBackgroundColor() {
        return backgroundColor;
    }

    public RGB getDimmedColor() {
        return dimmedColor;
    }

    public long getFlashingTimeout() {
        return flashingTimeout;
    }

    public boolean isDefaultImportance() {
        return defaultImportance;
    }

    public String getBannerName() {
        return bannerName;
    }

    public RGB getBannerFgColor() {
        return bannerFgColor;
    }

    public RGB getBannerBgColor() {
        return bannerBgColor;
    }

}
