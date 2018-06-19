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
package com.raytheon.viz.aviation.resource;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StyleRange;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.FontData;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.viz.core.RGBColors;
import com.raytheon.viz.aviation.resource.ResourceConfigMgr.ResourceTag;

/**
 * Data class to manage the resource editor data.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 19, 2009            lvenable     Initial creation
 * Oct 27, 2010 7387       rferrel      Added ResourceMap to allow changes to
 *                                      values without changing the resourceCB values.
 * Feb  7, 1011 7783       rferrel      setTextEditorFontAndReverseColors no 
 *                                      longer reverses highlighted text's
 *                                      foreground color
 * 16 Aug 2013  #2256      lvenable     Fixed font array out of bounds issue that may
 *                                      occasionally occur.
 * 30 Aug 2013  #2164      bkowal       Add default case statement for MSFT Windows
 *                                      Java. Replaced platform-dependent code with
 *                                      code that is not platform-dependent.
 * Oct 20, 2015 17445      yteng        Add audio alert interval.
 * May 24, 2016 18352      zhao         Added "ViewerAlertColor"   
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class ResourceDataManager {
    /**
     * Color offset.
     */
    final int colorOffset = 127;

    /**
     * Indicates loaded and non-disposed resources.
     */
    private boolean haveLoadedResources = false;

    /**
     * Default background offset color.
     */
    private Color defaultBackgroundOffsetColor;

    /**
     * Default font.
     */
    private Font defaultFont;

    /**
     * Default foreground color.
     */
    private Color defaultFgColor;

    /**
     * Default background color.
     */
    private Color defaultBgColor;

    /**
     * Text font.
     */
    private Font textFont;

    /**
     * Text foreground color.
     */
    private Color textFgColor;

    /**
     * Text background color.
     */
    private Color textBgColor;

    /**
     * Text editor font.
     */
    private Font textEditorFont;

    /**
     * Text editor foreground color.
     */
    private Color textEditorFgColor;

    /**
     * Text background color.
     */
    private Color textEditorBgColor;

    /**
     * Insert background color.
     */
    private Color insertBgColor;

    /**
     * Font for the list box controls.
     */
    private Font listBoxFont;

    /**
     * Text entry font.
     */
    private Font entryFont;

    /**
     * Entry background color.
     */
    private Color entryBgColor;

    /**
     * Message bar color.
     */
    private Color msgBarEntryBgColor;

    private Color alertLevel0Color;

    private Color alertLevel1Color;

    private Color alertLevel2Color;

    private Color alertLevel3Color;

    private Color alertLevel4Color;

    private Color alertLevel5Color;

    private Color alertLevel6Color;

    private Color viewerAlertColor;

    private Color lifrColor;

    private Color ifrColor;

    private Color mvfrColor;

    private Color vfrColor;

    private Display display;

    private IResourceAction resourceCB;

    private Map<ResourceTag, Object> resourceMap = new HashMap<ResourceConfigMgr.ResourceTag, Object>();

    public ResourceDataManager(Display display, IResourceAction resourceCB) {
        this.display = display;
        this.resourceCB = resourceCB;
    }

    public boolean isResourcesLoaded() {
        return haveLoadedResources;
    }

    public void reloadData() {
        initFontAndColors();
        initRemainingResourceData();
        haveLoadedResources = true;
    }

    public void setDefaultFontAndColors(Control ctrl) {
        ctrl.setFont(defaultFont);
        ctrl.setForeground(defaultFgColor);
        ctrl.setBackground(defaultBgColor);
    }

    public void setDefaultFontAndColors(Control ctrl, String text, GridData gd) {
        ctrl.setFont(defaultFont);
        ctrl.setForeground(defaultFgColor);
        ctrl.setBackground(defaultBgColor);
        if (ctrl instanceof Button) {
            ((Button) ctrl).setText(text);
        } else if (ctrl instanceof Label) {
            ((Label) ctrl).setText(text);
        } else if (ctrl instanceof Text) {
            ((Text) ctrl).setText(text);
        } else {
            System.err.println("Unable to set text");
        }
        Point p = ctrl.computeSize(SWT.DEFAULT, SWT.DEFAULT);
        if (gd.widthHint < p.x) {
            gd.widthHint = p.x;
            gd.minimumWidth = p.x;
        }
        ctrl.setLayoutData(gd);
    }

    public void setDefaultColors(Control ctrl) {
        ctrl.setForeground(defaultFgColor);
        ctrl.setBackground(defaultBgColor);
    }

    public void setDefaultFontAndForegroundColor(Control ctrl) {
        ctrl.setFont(defaultFont);
        ctrl.setForeground(defaultFgColor);
    }

    public void setTextFontAndColors(Control ctrl) {
        ctrl.setFont(textFont);
        ctrl.setForeground(textFgColor);
        ctrl.setBackground(textBgColor);
    }

    public void setEntryFontBgColor(Control ctrl) {
        ctrl.setFont(entryFont);
        ctrl.setBackground(entryBgColor);
    }

    public void setVfrColor(Control ctrl) {
        ctrl.setBackground(vfrColor);
    }

    public void setIfrColor(Control ctrl) {
        ctrl.setBackground(ifrColor);
    }

    public void setLifrColor(Control ctrl) {
        ctrl.setBackground(lifrColor);
    }

    public void setMvfrColor(Control ctrl) {
        ctrl.setBackground(mvfrColor);
    }

    public RGB getMsgBarBackground() {
        return msgBarEntryBgColor.getRGB();
    }

    public Color getFlightCatColor(ResourceTag tag) {
        if (tag == ResourceTag.MvfrColor) {
            return mvfrColor;
        } else if (tag == ResourceTag.VfrColor) {
            return vfrColor;
        } else if (tag == ResourceTag.LifrColor) {
            return lifrColor;
        } else if (tag == ResourceTag.IfrColor) {
            return ifrColor;
        }

        return null;
    }

    public void setTextEditorFontAndColors(Control ctrl) {
        ctrl.setFont(textEditorFont);
        ctrl.setForeground(textEditorFgColor);
        ctrl.setBackground(textEditorBgColor);
    }

    public void setTextEditorFontAndReverseColors(Control ctrl) {
        ctrl.setFont(textEditorFont);
        ctrl.setForeground(textEditorBgColor);
        ctrl.setBackground(textEditorFgColor);
        // Preserve foreground color in highlighted text
        if (ctrl instanceof StyledText) {
            StyledText st = (StyledText) ctrl;
            StyleRange[] srArray = st.getStyleRanges();
            for (StyleRange sr : srArray) {
                sr.foreground = textEditorFgColor;
            }
            st.setStyleRange(null);
            st.setStyleRanges(srArray);
        }
    }

    public void setListBoxFont(Control ctrl) {
        ctrl.setFont(listBoxFont);
    }

    public RGB getInsertBackgroundRgb() {
        return insertBgColor.getRGB();
    }

    /**
     * This sets up GUI display components and should only invoked one time
     * otherwise it is possible to have disposed components on active windows.
     */
    private void initFontAndColors() {
        this.dispose();

        /*
         * Default font, foreground color, background color
         */
        String fontStr = resourceCB.getResourceAsString(ResourceTag.Font);
        String[] stringArray = fontStr.split("-");

        /*
         * If the font back from the resource is null or doesn't have the
         * correct font data then create the default font with a system font.
         */
        if (stringArray == null || stringArray.length < 3) {
            FontData fd = display.getSystemFont().getFontData()[0];
            defaultFont = new Font(display, new FontData(fd.getName(),
                    fd.getHeight(), fd.getStyle()));
        } else {
            defaultFont = new Font(display, new FontData(stringArray[0],
                    Integer.valueOf(stringArray[1]),
                    getStyleInt(stringArray[2])));
        }

        RGB colorRGB = RGBColors.getRGBColor(resourceCB
                .getResourceAsString(ResourceTag.Foreground));
        defaultFgColor = new Color(display, colorRGB);

        colorRGB = RGBColors.getRGBColor(resourceCB
                .getResourceAsString(ResourceTag.Background));
        defaultBgColor = new Color(display, colorRGB);

        computeDefaultBgOffsetColor();

        /*
         * Text font, foreground color, background color
         */
        fontStr = resourceCB.getResourceAsString(ResourceTag.TextFont);
        stringArray = fontStr.split("-");
        textFont = new Font(display, new FontData(stringArray[0],
                Integer.valueOf(stringArray[1]), getStyleInt(stringArray[2])));

        colorRGB = RGBColors.getRGBColor(resourceCB
                .getResourceAsString(ResourceTag.TextForeground));
        textFgColor = new Color(display, colorRGB);

        colorRGB = RGBColors.getRGBColor(resourceCB
                .getResourceAsString(ResourceTag.TextBackground));
        textBgColor = new Color(display, colorRGB);

        /*
         * Text editor font, foreground color, background color
         */
        fontStr = resourceCB.getResourceAsString(ResourceTag.TextEditorFont);
        stringArray = fontStr.split("-");
        textEditorFont = new Font(display, new FontData(stringArray[0],
                Integer.valueOf(stringArray[1]), getStyleInt(stringArray[2])));

        colorRGB = RGBColors.getRGBColor(resourceCB
                .getResourceAsString(ResourceTag.TextEditorForeground));
        textEditorFgColor = new Color(display, colorRGB);

        colorRGB = RGBColors.getRGBColor(resourceCB
                .getResourceAsString(ResourceTag.TextEditorBackground));
        textEditorBgColor = new Color(display, colorRGB);

        /*
         * Insert background color
         */
        colorRGB = RGBColors.getRGBColor(resourceCB
                .getResourceAsString(ResourceTag.InsertBackground));
        insertBgColor = new Color(display, colorRGB);

        /*
         * List box font
         */
        fontStr = resourceCB.getResourceAsString(ResourceTag.ListBoxFont);
        stringArray = fontStr.split("-");
        listBoxFont = new Font(display, new FontData(stringArray[0],
                Integer.valueOf(stringArray[1]), getStyleInt(stringArray[2])));

        /*
         * Entry font and background color
         */
        fontStr = resourceCB.getResourceAsString(ResourceTag.EntryFont);
        stringArray = fontStr.split("-");
        entryFont = new Font(display, new FontData(stringArray[0],
                Integer.valueOf(stringArray[1]), getStyleInt(stringArray[2])));

        colorRGB = RGBColors.getRGBColor(resourceCB
                .getResourceAsString(ResourceTag.EntryBackground));
        entryBgColor = new Color(display, colorRGB);

        /*
         * Message bar entry background color
         */
        colorRGB = RGBColors.getRGBColor(resourceCB
                .getResourceAsString(ResourceTag.MsgBarEntryBgnd));
        msgBarEntryBgColor = new Color(display, colorRGB);

        /*
         * Alert level colors.
         */
        colorRGB = RGBColors.getRGBColor(resourceCB
                .getResourceAsString(ResourceTag.AlertLevel0));
        alertLevel0Color = new Color(display, colorRGB);

        colorRGB = RGBColors.getRGBColor(resourceCB
                .getResourceAsString(ResourceTag.AlertLevel1));
        alertLevel1Color = new Color(display, colorRGB);

        colorRGB = RGBColors.getRGBColor(resourceCB
                .getResourceAsString(ResourceTag.AlertLevel2));
        alertLevel2Color = new Color(display, colorRGB);

        colorRGB = RGBColors.getRGBColor(resourceCB
                .getResourceAsString(ResourceTag.AlertLevel3));
        alertLevel3Color = new Color(display, colorRGB);

        colorRGB = RGBColors.getRGBColor(resourceCB
                .getResourceAsString(ResourceTag.AlertLevel4));
        alertLevel4Color = new Color(display, colorRGB);

        colorRGB = RGBColors.getRGBColor(resourceCB
                .getResourceAsString(ResourceTag.AlertLevel5));
        alertLevel5Color = new Color(display, colorRGB);

        colorRGB = RGBColors.getRGBColor(resourceCB
                .getResourceAsString(ResourceTag.AlertLevel6));
        alertLevel6Color = new Color(display, colorRGB);

        colorRGB = RGBColors.getRGBColor(resourceCB
                .getResourceAsString(ResourceTag.ViewerAlertColor));
        viewerAlertColor = new Color(display, colorRGB);

        /*
         * LIFR, IFR, MVFR, VFR colors
         */
        colorRGB = RGBColors.getRGBColor(resourceCB
                .getResourceAsString(ResourceTag.LifrColor));
        lifrColor = new Color(display, colorRGB);

        colorRGB = RGBColors.getRGBColor(resourceCB
                .getResourceAsString(ResourceTag.IfrColor));
        ifrColor = new Color(display, colorRGB);

        colorRGB = RGBColors.getRGBColor(resourceCB
                .getResourceAsString(ResourceTag.MvfrColor));
        mvfrColor = new Color(display, colorRGB);

        colorRGB = RGBColors.getRGBColor(resourceCB
                .getResourceAsString(ResourceTag.VfrColor));
        vfrColor = new Color(display, colorRGB);
    }

    private void initRemainingResourceData() {
        resourceMap.clear();
        int textWidth = resourceCB.getResourceAsInt(ResourceTag.TextWidth);
        resourceMap.put(ResourceTag.TextWidth, textWidth);
        int textHeight = resourceCB.getResourceAsInt(ResourceTag.TextHeight);
        resourceMap.put(ResourceTag.TextHeight, textHeight);
        String textCursor = resourceCB
                .getResourceAsString(ResourceTag.TextCursor);
        resourceMap.put(ResourceTag.TextCursor, textCursor);

        int textEditorWidth = resourceCB
                .getResourceAsInt(ResourceTag.TextEditorWidth);
        resourceMap.put(ResourceTag.TextEditorWidth, textEditorWidth);
        int textEditorHeight = resourceCB
                .getResourceAsInt(ResourceTag.TextEditorHeight);
        resourceMap.put(ResourceTag.TextEditorHeight, textEditorHeight);
        String textEditorCursor = resourceCB
                .getResourceAsString(ResourceTag.TextEditorCursor);
        resourceMap.put(ResourceTag.TextEditorCursor, textEditorCursor);
        int textEditorInsWidth = resourceCB
                .getResourceAsInt(ResourceTag.TextEditorInsWidth);
        resourceMap.put(ResourceTag.TextEditorInsWidth, textEditorInsWidth);

        int textViewerWidth = resourceCB
                .getResourceAsInt(ResourceTag.TextViewerWidth);
        resourceMap.put(ResourceTag.TextViewerWidth, textViewerWidth);
        int textViewerHeight = resourceCB
                .getResourceAsInt(ResourceTag.TextViewerHeight);
        resourceMap.put(ResourceTag.TextViewerHeight, textViewerHeight);
        String orientation = resourceCB
                .getResourceAsString(ResourceTag.Orientation);
        resourceMap.put(ResourceTag.Orientation, orientation);

        String impactPlacement = resourceCB
                .getResourceAsString(ResourceTag.ImpactPlacement);
        resourceMap.put(ResourceTag.ImpactPlacement, impactPlacement);
        boolean confirmClose = resourceCB
                .getResourceAsBoolean(ResourceTag.ConfirmClose);
        resourceMap
                .put(ResourceTag.ConfirmClose, Boolean.valueOf(confirmClose));

        boolean transientDialogs = resourceCB
                .getResourceAsBoolean(ResourceTag.TransientDialogs);
        resourceMap.put(ResourceTag.TransientDialogs,
                Boolean.valueOf(transientDialogs));

        boolean confirmSend = resourceCB
                .getResourceAsBoolean(ResourceTag.ConfirmSend);
        resourceMap.put(ResourceTag.ConfirmSend, Boolean.valueOf(confirmSend));

        String notifyDeiconify = resourceCB
                .getResourceAsString(ResourceTag.NotifyDeiconify);
        resourceMap.put(ResourceTag.NotifyDeiconify, notifyDeiconify);
        String notifyRaise = resourceCB
                .getResourceAsString(ResourceTag.NotifyRaise);
        resourceMap.put(ResourceTag.NotifyRaise, notifyRaise);
        String notifyPlay = resourceCB
                .getResourceAsString(ResourceTag.NotifyPlay);
        resourceMap.put(ResourceTag.NotifyPlay, notifyPlay);
        String playFile = resourceCB.getResourceAsString(ResourceTag.PlayFile);
        resourceMap.put(ResourceTag.PlayFile, playFile);
        int alertIntervalMinutes = resourceCB
                .getResourceAsInt(ResourceTag.TextEditorInsWidth);
        resourceMap.put(ResourceTag.AlertIntervalMinutes, alertIntervalMinutes);
        boolean blink = resourceCB.getResourceAsBoolean(ResourceTag.Blink);
        resourceMap.put(ResourceTag.Blink, Boolean.valueOf(blink));
        String disalowSend = resourceCB
                .getResourceAsString(ResourceTag.DisallowSend);
        resourceMap.put(ResourceTag.DisallowSend, disalowSend);
        String loadOrder = resourceCB
                .getResourceAsString(ResourceTag.LoadOrder);
        resourceMap.put(ResourceTag.LoadOrder, loadOrder);

        boolean autoSave = resourceCB
                .getResourceAsBoolean(ResourceTag.AutoSave);
        resourceMap.put(ResourceTag.AutoSave, Boolean.valueOf(autoSave));
        boolean updateTimes = resourceCB
                .getResourceAsBoolean(ResourceTag.UpdateTimes);
        resourceMap.put(ResourceTag.UpdateTimes, Boolean.valueOf(updateTimes));
        boolean autoPrint = resourceCB
                .getResourceAsBoolean(ResourceTag.AutoPrint);
        resourceMap.put(ResourceTag.AutoPrint, Boolean.valueOf(autoPrint));
        boolean insert = resourceCB.getResourceAsBoolean(ResourceTag.Insert);
        resourceMap.put(ResourceTag.Insert, Boolean.valueOf(insert));

        String wrap = resourceCB.getResourceAsString(ResourceTag.Wrap);
        resourceMap.put(ResourceTag.Wrap, wrap);
        boolean amdButtons = resourceCB
                .getResourceAsBoolean(ResourceTag.AmdButtons);
        resourceMap.put(ResourceTag.AmdButtons, Boolean.valueOf(amdButtons));
        String numTafs = resourceCB.getResourceAsString(ResourceTag.NumTafs);
        resourceMap.put(ResourceTag.NumTafs, numTafs);
        String numHours = resourceCB.getResourceAsString(ResourceTag.NumHours);
        resourceMap.put(ResourceTag.NumHours, numHours);
        String numHoursAll = resourceCB
                .getResourceAsString(ResourceTag.NumHoursAll);
        resourceMap.put(ResourceTag.NumHoursAll, numHoursAll);

        boolean showHeaders = resourceCB
                .getResourceAsBoolean(ResourceTag.ShowHeaders);
        resourceMap.put(ResourceTag.ShowHeaders, Boolean.valueOf(showHeaders));
        boolean showDecoded = resourceCB
                .getResourceAsBoolean(ResourceTag.ShowDecoded);
        resourceMap.put(ResourceTag.ShowDecoded, Boolean.valueOf(showDecoded));

        boolean showRoutine = resourceCB
                .getResourceAsBoolean(ResourceTag.ShowRoutine);
        resourceMap.put(ResourceTag.ShowRoutine, Boolean.valueOf(showRoutine));

        boolean showProbs = resourceCB
                .getResourceAsBoolean(ResourceTag.ShowProbs);
        resourceMap.put(ResourceTag.ShowProbs, Boolean.valueOf(showProbs));
        String showFormatted = resourceCB
                .getResourceAsString(ResourceTag.ShowFormatted);
        resourceMap.put(ResourceTag.ShowFormatted, showFormatted);
        boolean highlightFlightCat = resourceCB
                .getResourceAsBoolean(ResourceTag.HighlightFlightCat);
        resourceMap.put(ResourceTag.HighlightFlightCat,
                Boolean.valueOf(highlightFlightCat));
    }

    private void computeDefaultBgOffsetColor() {
        // Get the current color and change the RGB values using the color
        // offset
        // so the cursor color can be updated.
        RGB rgb = defaultBgColor.getRGB();
        rgb.red += (rgb.red > 127 ? -colorOffset : colorOffset);
        rgb.green += (rgb.green > 127 ? -colorOffset : colorOffset);
        rgb.blue += (rgb.blue > 127 ? -colorOffset : colorOffset);

        // Dispose of the cursor color (if not null);
        if (defaultBackgroundOffsetColor != null) {
            defaultBackgroundOffsetColor.dispose();
        }

        // Set the new cursor color.
        defaultBackgroundOffsetColor = new Color(display, rgb);
    }

    /**
     * Set resource tag data to an integer value.
     * 
     * @param resourceTag
     * @param value
     */
    public void setResourceAsInt(ResourceTag resourceTag, int value) {
        resourceMap.put(resourceTag, Integer.valueOf(value));
    }

    /**
     * Obtain resource tags value as an integer
     * 
     * @param resourceTag
     * @return value - The integer value otherwise 0
     */
    public int getResourceAsInt(ResourceTag resourceTag) {
        Object obj = resourceMap.get(resourceTag);
        if (obj instanceof Integer) {
            return ((Integer) obj).intValue();
        }
        return 0;
    }

    /**
     * Set the resource tag data value to the string.
     * 
     * @param resourceTag
     * @param value
     */
    public void setResourceAsString(ResourceTag resourceTag, String value) {
        resourceMap.put(resourceTag, value);
    }

    /**
     * Obtain the string data value for the resource tag.
     * 
     * @param resourceTag
     * @return value - If a string otherwise an empty string
     */
    public String getResourceAsString(ResourceTag resourceTag) {
        Object obj = resourceMap.get(resourceTag);
        if (obj instanceof String) {
            return (String) obj;
        }
        return "";
    }

    /**
     * Set the data value for the resource tag as a boolean.
     * 
     * @param resourceTag
     * @param value
     */
    public void setResourceAsBoolean(ResourceTag resourceTag, boolean value) {
        resourceMap.put(resourceTag, Boolean.valueOf(value));
    }

    /**
     * Obtain the data value for the resource tag as a boolean.
     * 
     * @param resourceTag
     * @return value - If a boolean otherwise false
     */
    public boolean getResourceAsBoolean(ResourceTag resourceTag) {
        Object obj = resourceMap.get(resourceTag);
        if (obj instanceof Boolean) {
            return ((Boolean) obj).booleanValue();
        }
        return false;
    }

    public int getStyleInt(String styleStr) {
        if (styleStr.compareTo("Normal") == 0) {
            return SWT.NORMAL;
        } else if (styleStr.compareTo("Bold") == 0) {
            return SWT.BOLD;
        } else if (styleStr.compareTo("Italic") == 0) {
            return SWT.ITALIC;
        } else if (styleStr.compareTo("BoldItalic") == 0) {
            return SWT.BOLD | SWT.ITALIC;
        }

        return 0;
    }

    public String getStyleString(int styleInt) {
        if (styleInt == SWT.NORMAL) {
            return "Normal";
        } else if (styleInt == SWT.BOLD) {
            return "Bold";
        } else if (styleInt == SWT.ITALIC) {
            return "Italic";
        } else if (styleInt == (SWT.BOLD | SWT.ITALIC)) {
            return "BoldItalic";
        }

        return "Normal";
    }

    public void dispose() {
        haveLoadedResources = false;

        if (defaultFont != null) {
            defaultFont.dispose();
        }

        if (defaultBgColor != null) {
            defaultBgColor.dispose();
        }

        if (defaultFgColor != null) {
            defaultFgColor.dispose();
        }

        if (textFont != null) {
            textFont.dispose();
        }

        if (textBgColor != null) {
            textBgColor.dispose();
        }

        if (textFgColor != null) {
            textFgColor.dispose();
        }

        if (textEditorFont != null) {
            textEditorFont.dispose();
        }

        if (textEditorBgColor != null) {
            textEditorBgColor.dispose();
        }

        if (textEditorFgColor != null) {
            textEditorFgColor.dispose();
        }

        if (insertBgColor != null) {
            insertBgColor.dispose();
        }

        if (listBoxFont != null) {
            listBoxFont.dispose();
        }

        if (entryFont != null) {
            entryFont.dispose();
        }

        if (entryBgColor != null) {
            entryBgColor.dispose();
        }

        if (msgBarEntryBgColor != null) {
            msgBarEntryBgColor.dispose();
        }

        if (alertLevel0Color != null) {
            alertLevel0Color.dispose();
        }

        if (alertLevel1Color != null) {
            alertLevel1Color.dispose();
        }

        if (alertLevel2Color != null) {
            alertLevel2Color.dispose();
        }

        if (alertLevel3Color != null) {
            alertLevel3Color.dispose();
        }

        if (alertLevel4Color != null) {
            alertLevel4Color.dispose();
        }

        if (alertLevel5Color != null) {
            alertLevel5Color.dispose();
        }

        if (alertLevel6Color != null) {
            alertLevel6Color.dispose();
        }

        if (viewerAlertColor != null) {
            viewerAlertColor.dispose();
        }

        if (lifrColor != null) {
            lifrColor.dispose();
        }

        if (ifrColor != null) {
            ifrColor.dispose();
        }

        if (mvfrColor != null) {
            mvfrColor.dispose();
        }

        if (vfrColor != null) {
            vfrColor.dispose();
        }
    }

    public RGB getDefaultBackgroundRGB() {
        return defaultBgColor.getRGB();
    }

    public Color getDefaultBackgroundColor() {
        return defaultBgColor;
    }

    public Color getDefaultBackgroundOffsetColor() {
        return defaultBackgroundOffsetColor;
    }

    public Font getDefaultFont() {
        return defaultFont;
    }

    public Font getFont(ResourceTag tag) {
        switch (tag) {
        case Font:
            return defaultFont;

        case TextFont:
            return textFont;

        case TextEditorFont:
            return textEditorFont;

        case ListBoxFont:
            return listBoxFont;

        case EntryFont:
            return entryFont;

        default:
            return null;
        }
    }

    public Color getAlertLevel0Color() {
        return alertLevel0Color;
    }

    public Color getAlertLevel1Color() {
        return alertLevel1Color;
    }

    public Color getAlertLevel2Color() {
        return alertLevel2Color;
    }

    public Color getAlertLevel3Color() {
        return alertLevel3Color;
    }

    public Color getAlertLevel4Color() {
        return alertLevel4Color;
    }

    public Color getAlertLevel5Color() {
        return alertLevel5Color;
    }

    public Color getAlertLevel6Color() {
        return alertLevel6Color;
    }

    public Color getViwerAlertColor() {
        return viewerAlertColor;
    }

    public Color[] getAlertLevelColors() {
        return new Color[] { alertLevel0Color, alertLevel1Color,
                alertLevel2Color, alertLevel3Color, alertLevel4Color,
                alertLevel5Color, alertLevel6Color };
    }
}
