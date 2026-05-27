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
package com.raytheon.uf.viz.alertviz.ui.dialogs;

import java.util.EnumMap;
import java.util.Map;

/**
 * AlertViz ToolTip text container
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -----------------------------
 * Mar 12, 2019  7763     randerso  Initial creation
 * Jan 29, 2020  8017     randerso  Fixed typo in TIP.CATEGORIES
 *
 * </pre>
 *
 * @author randerso
 */

public class AlertVizTips {
    public static enum TIP {
        LAYOUT,
        COMMON,
        SOURCES,
        PRIORITIES,
        CATEGORIES,
        TEXT,
        BLINK,
        POPUP,
        AUDIO,
        ACTION,
        COLOR,
        IMAGE
    };

    private static final Map<TIP, String> tipMap = createMap();

    private static Map<TIP, String> createMap() {
        Map<TIP, String> map = new EnumMap<>(TIP.class);

        map.put(TIP.LAYOUT,
                "This is the Layout section, where you choose which "
                        + "layout you want the AlertViz bar to use and "
                        + "which Categories' text messages will be put into "
                        + "which Text Cell. Select a Category in the list on "
                        + "the left, then click in a cell under Message Text "
                        + "Layout: on the right to assign the Category to that "
                        + "box. The selected cell will be displayed in yellow. "
                        + "Click Remove Selection to remove a cell selection.");

        map.put(TIP.COMMON,
                "These are general settings. The Text Message section describes "
                        + "how the text message representations will be affected "
                        + "in the AlertViz bar (if text is turned on for the "
                        + "key/priority) and the Pop-ups and how long the "
                        + "text blinking and system audio execution will "
                        + "last (again, if turned on). The lower area defines "
                        + "other, general behavior.\n"
                        + "NOTE: to make blinking or audio responses perpetual, "
                        + "set the duration to 0.");

        map.put(TIP.SOURCES,
                "This is the list of Source Keys that AlertViz "
                        + "recognizes. Click on one to see its Priority "
                        + "settings to the right. To create a new Source, "
                        + "select 'New...'.\n"
                        + "The color coding is explained in the Legend below.");

        map.put(TIP.PRIORITIES,
                "These are the Priority settings for the selected "
                        + "Source Key. This is where you control how "
                        + "messages of various priorities for the various "
                        + "Source Keys get communicated to you.\n"
                        + "Remember, zero is highest priority and five is "
                        + "lowest priority.\n"
                        + "Most are toggles, but some allow you to enter "
                        + "a file name or select a color.\n"
                        + "For definitions of the priority numbers, click "
                        + "the Info (i) button in the AlertViz bar.");

        map.put(TIP.CATEGORIES,
                "This is the list of message Categories that AlertViz "
                        + "recognizes. Select a category in this list "
                        + "and then click in the Layout Boxes on the right "
                        + "where you want that category to be displayed. "
                        + "To create a new Category, select 'New...'.");

        map.put(TIP.TEXT,
                "For the various Priorities, turning text on means "
                        + "that messages from the selected Source Key will be "
                        + "sent to the AlertViz bar's text section, "
                        + "if the Category of the message has been placed "
                        + "in that text box in the Layout section.");

        map.put(TIP.BLINK,
                "For the various Priorities, if text is turned on and "
                        + "the message's Category is included in one of the "
                        + "text boxes in the AlertViz bar, then the text "
                        + "will blink when displayed. The duration of the blink is "
                        + "defined in the Common Settings section.");

        map.put(TIP.POPUP, "For the various Priorities, turning popup on means "
                + "an incoming message from the selected Source will "
                + "yield a popup box with the message in it. This popup "
                + "can be normal or expanded, based on the toggle set in "
                + "the Common Settings section. If you click the button "
                + "to the right of the check box you can select an image "
                + "to be displayed in the popup window. By default the "
                + "AlertViz image is displayed");

        map.put(TIP.AUDIO, "For the various Priorities, turning audio on means "
                + "an incoming message from the selected Source will "
                + "produce a system beep. You can also click on the button to "
                + "the right of the check box to select an audio file name (*.wav) "
                + "that will be played instead of the system beep. The duration of "
                + "the system audio is defined in the Common Settings section.");

        map.put(TIP.ACTION, "For the various Priorities, if you turn action on "
                + "and supply an executable, that executable will be run. This is "
                + "a rather powerful tool and should be used carefully.");

        map.put(TIP.COLOR,
                "For the various Priorities, you can right click the "
                        + "button next to the colored boxes to choose the "
                        + "foreground/background color that gets used in "
                        + "the text section of the AlertViz bar and in the "
                        + "popup window.");

        map.put(TIP.IMAGE,
                "This is where a monitor defines the image for "
                        + "use on its button in the AlertViz bar.\n"
                        + "If you create a new Source Key and leave this "
                        + "entry blank, AlertViz will think the new Source "
                        + "Key is NOT a monitor. If this entry has contents, "
                        + "AlertViz will identify the new Source Key as a "
                        + "monitor.");

        return map;
    }

    /**
     * @param key
     * @return the tool tip text for the specified key
     */
    public static String getTip(TIP key) {
        return tipMap.get(key);
    }
}
