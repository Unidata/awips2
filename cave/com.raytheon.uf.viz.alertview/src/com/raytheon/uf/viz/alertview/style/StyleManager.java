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
package com.raytheon.uf.viz.alertview.style;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Device;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.FontData;
import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.viz.alertview.Alert;
import com.raytheon.uf.viz.alertview.filter.AlertFilter;
import com.raytheon.uf.viz.alertview.filter.FilterManager;
import com.raytheon.uf.viz.alertview.prefs.PreferenceFile;

/**
 * Converts {@link AlertStyle}s into SWT colors and fonts.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------
 * Jun 18, 2015  4474     bsteffen  Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class StyleManager {

    private static final String CONFIG_FILE_LOC = "alert_styles.xml";

    private static final String REGULAR = "Regular";

    private static final String BOLD = "Bold";

    private static final String ITALIC = "Italic";

    private static final String BOLD_ITALIC = BOLD + "-" + ITALIC;

    private final FilterManager filterManager = new FilterManager();

    private List<AlertStyle> styles = new ArrayList<AlertStyle>();

    public StyleManager() {
        readConfig();
    }

    public void close() {

    }

    private void readConfig() {
        PreferenceFile<StyleList> file = new PreferenceFile<>(CONFIG_FILE_LOC,
                StyleList.class,
                new PreferenceFile.Listener<StyleList>() {

                    @Override
                    public void update(StyleList t) {

                    }

                });
        styles = file.get().getStyles();
    }

    public Color getForegroundColor(Device device, Alert alert) {
        for (AlertStyle style : styles) {
            if (style.getForegroundColor() != null) {
                AlertFilter filter = filterManager.getFilter(style
                        .getFilter());
                if (filter.filter(alert)) {
                    return parseColor(device, style.getForegroundColor());
                }
            }
        }
        return null;
    }

    public Color getBackgroundColor(Device device, Alert alert) {
        for (AlertStyle style : styles) {
            if (style.getBackgroundColor() != null) {
                AlertFilter filter = filterManager.getFilter(style
                        .getFilter());
                if (filter.filter(alert)) {
                    return parseColor(device, style.getBackgroundColor());
                }
            }
        }
        return null;
    }

    public Font getFont(Device device, Alert alert) {
        String name = null;
        Integer size = null;
        String fstyle = null;
        for (AlertStyle style : styles) {
            if (name == null && style.getFontName() != null) {
                AlertFilter filter = filterManager.getFilter(style
                        .getFilter());
                if (filter.filter(alert)) {
                    name = style.getFontName();
                }
            }
            if (size == null && style.getFontSize() != null) {
                AlertFilter filter = filterManager.getFilter(style
                        .getFilter());
                if (filter.filter(alert)) {
                    size = style.getFontSize();
                }
            }
            if (fstyle == null && style.getFontStyle() != null) {
                AlertFilter filter = filterManager.getFilter(style
                        .getFilter());
                if (filter.filter(alert)) {
                    fstyle = style.getFontStyle();
                }
            }
            if (name != null && size != null && fstyle != null) {
                break;
            }
        }
        return getFont(device, name, size, fstyle);
    }

    public static Font getFont(Device device, AlertStyle style) {
        return getFont(device, style.getFontName(), style.getFontSize(),
                style.getFontStyle());
    }

    public static Font getFont(Device device, String name, Integer size,
            String style) {
        Integer fstyle = parseFontStyle(style);
        if (name != null && size != null && fstyle != null) {
            return new Font(device, new FontData(name, size, fstyle));
        }
        if (name == null && size == null && fstyle == null) {
            return device.getSystemFont();
        }
        FontData systemFontData = device.getSystemFont().getFontData()[0];
        if (name == null) {
            name = systemFontData.getName();
        }
        if (size == null) {
            size = systemFontData.getHeight();
        }
        if (fstyle == null) {
            fstyle = systemFontData.getStyle();
        }
        return new Font(device, new FontData(name, size, fstyle));
    }

    public static void setFont(AlertStyle style, FontData data) {
        style.setFontName(data.getName());
        style.setFontSize(data.getHeight());
        style.setFontStyle(formatFontStyle(data.getStyle()));
    }

    private static Integer parseFontStyle(String style) {
        if (style == null) {
            return null;
        } else if (style.equalsIgnoreCase(REGULAR)) {
            return SWT.NORMAL;
        } else if (style.equalsIgnoreCase(BOLD)) {
            return SWT.BOLD;
        } else if (style.equals(ITALIC)) {
            return SWT.ITALIC;
        } else if (style.equals(BOLD_ITALIC)) {
            return SWT.BOLD | SWT.ITALIC;
        }
        return null;
    }

    private static String formatFontStyle(Integer style) {
        if (style == null) {
            return null;
        } else if ((style & SWT.BOLD) == SWT.BOLD
                && (style & SWT.ITALIC) == SWT.ITALIC) {
            return BOLD_ITALIC;
        } else if ((style & SWT.BOLD) == SWT.BOLD) {
            return BOLD;
        } else if ((style & SWT.ITALIC) == SWT.ITALIC) {
            return ITALIC;
        } else if ((style & SWT.NORMAL) == SWT.NORMAL) {
            return REGULAR;
        }
        return null;
    }

    public static Color parseColor(Device device, String str) {
        if (str == null) {
            return null;
        }
        try {
            int red = Integer.parseInt(str.substring(1, 3), 16);
            int green = Integer.parseInt(str.substring(3, 5), 16);
            int blue = Integer.parseInt(str.substring(5, 7), 16);
            return new Color(device, red, green, blue);
        } catch (NumberFormatException e) {
            e.printStackTrace();
        }
        return null;
    }

    public static String formatColor(RGB colorValue) {
        return String.format("#%02x%02x%02x", colorValue.red, colorValue.green,
                colorValue.blue);
    }

    public List<AlertStyle> getStyles() {
        return styles;
    }



}
