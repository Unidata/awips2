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
package com.raytheon.viz.gfe.core.msgs;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.viz.core.RGBColors;
import com.raytheon.viz.gfe.GFEPreference;

/**
 * Reference Set Appearance Changed Message
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Aug 24, 2009           randerso  Initial creation
 * Jan 05, 2018  7178     randerso  Added default constructor for
 *                                  Message.InquireLastMessage
 * Jan 24, 2018  7153     randerso  Changes to allow new GFE config file to be
 *                                  selected when perspective is re-opened.
 *
 * </pre>
 *
 * @author randerso
 */

public class RefSetAppearanceChangedMsg extends Message {

    private final RGB color;

    private final int lineWidth;

    /**
     * Default constructor for use in {@link Message#inquireLastMessage(Class)}
     */
    public RefSetAppearanceChangedMsg() {
        String colorStr = GFEPreference.getString("ReferenceSet_color",
                "Gray80");
        this.color = RGBColors.getRGBColor(colorStr);

        int lineWidth = GFEPreference.getInt("ReferenceSet_width", 1);
        if (lineWidth < 1) {
            lineWidth = 1;
        }
        this.lineWidth = lineWidth;
    }

    /**
     * Constructor
     *
     * @param color
     * @param lineWidth
     */
    public RefSetAppearanceChangedMsg(RGB color, int lineWidth) {
        super();
        this.color = color;
        this.lineWidth = lineWidth;
    }

    /**
     * @return the color
     */
    public RGB getColor() {
        return color;
    }

    /**
     * @return the lineWidth
     */
    public int getLineWidth() {
        return lineWidth;
    }
}
