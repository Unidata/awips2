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
package com.raytheon.viz.ui.input.preferences;

/**
 * 
 * All the mouse events that can be set in the mouse preferences page.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 27, 2009            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public enum MouseEvent {
    DISABLED, HOVER, LEFT_CLICK, MIDDLE_CLICK, RIGHT_CLICK, LONG_LEFT_CLICK, LONG_MIDDLE_CLICK, LONG_RIGHT_CLICK, LEFT_DRAG, MIDDLE_DRAG, RIGHT_DRAG, SCROLL_FORWARD, SCROLL_BACK;

    public String toString() {
        StringBuilder pretty = new StringBuilder();
        String words[] = super.toString().split("_");
        for (int i = 0; i < words.length; i++) {
            if (i != 0) {
                pretty.append(' ');
            }
            pretty.append(words[i].substring(0, 1)
                    + words[i].substring(1).toLowerCase());
        }
        return pretty.toString();
    }
    
    public static MouseEvent fromString(String pretty) {
        if (pretty == null || pretty.isEmpty()) {
            return null;
        }
        return MouseEvent.valueOf(pretty.replace(' ', '_')
                .toUpperCase());
    }
    
    public static String toListString(MouseEvent[] preferences) {
        StringBuilder pretty = new StringBuilder();
        for (MouseEvent pref : preferences) {
            if (pref == null) {
                continue;
            }
            pretty.append(pref.toString());
            pretty.append(',');
        }
        return pretty.toString().replaceAll(",$", "");
    }
    
    public static MouseEvent[] fromListString(String pretty) {
        String[] strings = pretty.split(",");
        return fromStringArray(strings);
    }
    
    public static String[] toStringArray(MouseEvent[] prefs) {
        String[] strings = new String[prefs.length];
        for (int i = 0; i < strings.length; i++) {
            strings[i] = prefs[i].toString();
        }
        return strings;
    }

    public static MouseEvent[] fromStringArray(String[] strings) {
        MouseEvent[] prefs = new MouseEvent[strings.length];
        for (int i = 0; i < strings.length; i++) {
            prefs[i] = fromString(strings[i].trim());
        }
        return prefs;
    }
}

