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

import java.io.IOException;
import java.util.LinkedHashMap;
import java.util.Set;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.FontData;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.MenuItem;

import com.raytheon.uf.viz.core.localization.HierarchicalPreferenceStore;
import com.raytheon.viz.aviation.activator.Activator;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 28, 2010            ?           Initial creation
 * Sep 28, 2010 2846       rferrel     Change range values in getSpinnerData
 * Oct 27, 2010 7383       rferrel     Added setter methods for the getData...
 *                                     methods and method to reset data values.
 * Dec 9, 2010  7380       rferrel     Changed spinner values for text fields.
 * Dec 14, 2010 5782       rferrel     Fixed numTafs combo string array.
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */
public class ResourceConfigMgr implements IResourceAction {
    private Display display;

    private ResourceDataManager resrcDataMgr;

    private static ResourceConfigMgr classInstance;

    private LinkedHashMap<ResourceTag, ResourceInfo> resourceTypeMap;

    public static enum ResourceType {
        FONT, COLOR, COMBO, SPINNER, CHECK, FILE
    };

    public static enum ResourceTag {
        Font("font"), Background("background"), Foreground("foreground"), TextFont(
                "textFont"), TextBackground("textBackground"), TextForeground(
                "textForeground"), TextWidth("textWidth"), TextHeight(
                "textHeight"), TextCursor("textCursor"), TextEditorFont(
                "textEditorFont"), TextEditorBackground("textEditorBackground"), TextEditorForeground(
                "textEditorForeground"), TextEditorWidth("textEditorWidth"), TextEditorHeight(
                "textEditorHeight"), TextEditorCursor("textEditorCursor"), TextEditorInsWidth(
                "textEditorInsWidth"), InsertBackground("insertBackground"), TextViewerWidth(
                "textViewerWidth"), TextViewerHeight("textViewerHeight"), Orientation(
                "orientation"), ListBoxFont("listBoxFont"), EntryFont(
                "entryFont"), EntryBackground("entryBackground"), MsgBarEntryBgnd(
                "msgBarEntryBgnd"), ImpactPlacement("impactPlacement"), TransientDialogs(
                "transientDialogs"), ConfirmClose("confirmClose"), ConfirmSend(
                "confirmSend"), AlertLevel0("alertLevel0"), AlertLevel1(
                "alertLevel1"), AlertLevel2("alertLevel2"), AlertLevel3(
                "alertLevel3"), AlertLevel4("alertLevel4"), AlertLevel5(
                "alertLevel5"), AlertLevel6("alertLevel6"), NotifyDeiconify(
                "notifyDeiconify"), NotifyRaise("notifyRaise"), NotifyPlay(
                "notifyPlay"), PlayFile("playFile"), Blink("blink"), DisallowSend(
                "disallowSend"), LoadOrder("loadOrder"), AutoSave("autoSave"), UpdateTimes(
                "updateTimes"), AutoPrint("autoPrint"), Insert("insert"), Wrap(
                "wrap"), AmdButtons("amdbuttons"), NumTafs("numTafs"), NumHours(
                "numHours"), NumHoursAll("numHoursAll"), ShowHeaders(
                "showHeaders"), ShowDecoded("showDecoded"), ShowRoutine(
                "showRoutine"), ShowProbs("showProbs"), ShowFormatted(
                "showFormatted"), HighlightFlightCat("highlightFlightCat"), LifrColor(
                "lifrColor"), IfrColor("ifrColor"), MvfrColor("mvfrColor"), VfrColor(
                "vfrColor");

        private String tagName;

        ResourceTag(String name) {
            tagName = name;
        }

        public String getTagName() {
            return tagName;
        }
    };

    private ResourceConfigMgr() {
        initData();
    }

    public static synchronized ResourceConfigMgr getInstance() {
        if (classInstance == null) {
            classInstance = new ResourceConfigMgr();
        }

        return classInstance;
    }

    private void initData() {
        display = Display.getDefault();
        resrcDataMgr = new ResourceDataManager(display, this);

        display.addListener(SWT.Dispose, new Listener() {
            @Override
            public void handleEvent(Event event) {
                resrcDataMgr.dispose();
            }
        });

        createResourceTypeMap();
    }

    /**
     * Generates GUI resources. This method should only be called once to
     * prevent active windows from having components that are disposed.
     */
    private void createResourceTypeMap() {
        resourceTypeMap = new LinkedHashMap<ResourceTag, ResourceInfo>();

        resourceTypeMap.put(ResourceTag.Font, new ResourceInfo(
                ResourceType.FONT, "Default font"));
        resourceTypeMap.put(ResourceTag.Background, new ResourceInfo(
                ResourceType.COLOR, "Default background"));
        resourceTypeMap.put(ResourceTag.Foreground, new ResourceInfo(
                ResourceType.COLOR, "Default foreground"));
        resourceTypeMap.put(ResourceTag.TextFont, new ResourceInfo(
                ResourceType.FONT, "Text window font"));
        resourceTypeMap.put(ResourceTag.TextBackground, new ResourceInfo(
                ResourceType.COLOR, "Text window background"));
        resourceTypeMap.put(ResourceTag.TextForeground, new ResourceInfo(
                ResourceType.COLOR, "Text window foreground"));
        resourceTypeMap.put(ResourceTag.TextWidth, new ResourceInfo(
                ResourceType.SPINNER, "Text window width"));
        resourceTypeMap.put(ResourceTag.TextHeight, new ResourceInfo(
                ResourceType.SPINNER, "Text window height"));
        resourceTypeMap.put(ResourceTag.TextCursor, new ResourceInfo(
                ResourceType.COMBO, "Mouse cursor in Text window"));
        resourceTypeMap.put(ResourceTag.TextEditorFont, new ResourceInfo(
                ResourceType.FONT, "Forecast Editor font"));
        resourceTypeMap.put(ResourceTag.TextEditorBackground, new ResourceInfo(
                ResourceType.COLOR, "Forecast Editor background"));
        resourceTypeMap.put(ResourceTag.TextEditorForeground, new ResourceInfo(
                ResourceType.COLOR, "Forecast Editor foreground"));
        resourceTypeMap.put(ResourceTag.TextEditorWidth, new ResourceInfo(
                ResourceType.SPINNER, "Forecast Editor window width"));
        resourceTypeMap.put(ResourceTag.TextEditorHeight, new ResourceInfo(
                ResourceType.SPINNER, "Forecast Editor window height"));
        resourceTypeMap.put(ResourceTag.TextEditorCursor, new ResourceInfo(
                ResourceType.COMBO, "Mouse cursor in Forecast Editor window"));
        resourceTypeMap.put(ResourceTag.TextEditorInsWidth, new ResourceInfo(
                ResourceType.SPINNER, "Insertion cursor width"));
        resourceTypeMap.put(ResourceTag.InsertBackground, new ResourceInfo(
                ResourceType.COLOR, "Insertion cursor color"));
        resourceTypeMap.put(ResourceTag.TextViewerWidth, new ResourceInfo(
                ResourceType.SPINNER, "Text Viewer window width"));
        resourceTypeMap.put(ResourceTag.TextViewerHeight, new ResourceInfo(
                ResourceType.SPINNER, "Text Viewer window height"));
        resourceTypeMap.put(ResourceTag.Orientation, new ResourceInfo(
                ResourceType.COMBO, "Forecast Editor layout"));
        resourceTypeMap.put(ResourceTag.ListBoxFont, new ResourceInfo(
                ResourceType.FONT, "Listbox font"));
        resourceTypeMap.put(ResourceTag.EntryFont, new ResourceInfo(
                ResourceType.FONT, "Entry field font"));
        resourceTypeMap.put(ResourceTag.EntryBackground, new ResourceInfo(
                ResourceType.COLOR, "Entry field background"));
        resourceTypeMap.put(ResourceTag.MsgBarEntryBgnd, new ResourceInfo(
                ResourceType.COLOR,
                "Message Bar (on the bottom of windows) background"));
        resourceTypeMap.put(ResourceTag.ImpactPlacement, new ResourceInfo(
                ResourceType.COMBO, "Placement of impact messages"));
        resourceTypeMap.put(ResourceTag.TransientDialogs, new ResourceInfo(
                ResourceType.CHECK,
                "May change behavior of dialog windows placement"));
        resourceTypeMap.put(ResourceTag.ConfirmClose,
                new ResourceInfo(ResourceType.CHECK,
                        "Ask for confirmation while closing editor"));
        resourceTypeMap
                .put(ResourceTag.ConfirmSend,
                        new ResourceInfo(ResourceType.CHECK,
                                "Ask for confirmation while sending amendments and corrections"));
        resourceTypeMap.put(ResourceTag.AlertLevel0, new ResourceInfo(
                ResourceType.COLOR, "AvnWatch Status OK"));
        resourceTypeMap.put(ResourceTag.AlertLevel1, new ResourceInfo(
                ResourceType.COLOR, "AvnWatch Status Missing"));
        resourceTypeMap.put(ResourceTag.AlertLevel2, new ResourceInfo(
                ResourceType.COLOR, "AvnWatch Lowest Alert"));
        resourceTypeMap.put(ResourceTag.AlertLevel3, new ResourceInfo(
                ResourceType.COLOR, ""));
        resourceTypeMap.put(ResourceTag.AlertLevel4, new ResourceInfo(
                ResourceType.COLOR, ""));
        resourceTypeMap.put(ResourceTag.AlertLevel5, new ResourceInfo(
                ResourceType.COLOR, ""));
        resourceTypeMap.put(ResourceTag.AlertLevel6, new ResourceInfo(
                ResourceType.COLOR, "AvnWatch Highest Alert"));
        resourceTypeMap.put(ResourceTag.NotifyDeiconify, new ResourceInfo(
                ResourceType.COMBO, "Alert level to deiconify monitor GUI"));
        resourceTypeMap.put(ResourceTag.NotifyRaise, new ResourceInfo(
                ResourceType.COMBO, "Alert level to raise monitor GUI"));
        resourceTypeMap.put(ResourceTag.NotifyPlay, new ResourceInfo(
                ResourceType.COMBO, "Alert level to play file"));
        resourceTypeMap.put(ResourceTag.PlayFile, new ResourceInfo(
                ResourceType.FILE, "Sound to play on TAF alert"));
        resourceTypeMap.put(ResourceTag.Blink, new ResourceInfo(
                ResourceType.CHECK, "Blink on new notification"));
        resourceTypeMap.put(ResourceTag.DisallowSend, new ResourceInfo(
                ResourceType.COMBO, "Disallow transmission error level"));
        resourceTypeMap.put(ResourceTag.LoadOrder, new ResourceInfo(
                ResourceType.COMBO, "How to initialize forecast"));
        resourceTypeMap.put(ResourceTag.AutoSave, new ResourceInfo(
                ResourceType.CHECK, "Autosave bulletin in a backup file"));
        resourceTypeMap.put(ResourceTag.UpdateTimes, new ResourceInfo(
                ResourceType.CHECK, "Update issue and valid time on QC"));
        resourceTypeMap.put(ResourceTag.AutoPrint, new ResourceInfo(
                ResourceType.CHECK, "Print transmitted forecast"));
        resourceTypeMap.put(ResourceTag.Insert, new ResourceInfo(
                ResourceType.CHECK, "Insert/Overwrite"));
        resourceTypeMap.put(ResourceTag.Wrap, new ResourceInfo(
                ResourceType.COMBO, "Wrap long lines"));
        resourceTypeMap.put(ResourceTag.AmdButtons, new ResourceInfo(
                ResourceType.CHECK, "Show editor shortcuts"));
        resourceTypeMap.put(ResourceTag.NumTafs, new ResourceInfo(
                ResourceType.COMBO, "Number of TAFs to display"));
        resourceTypeMap.put(ResourceTag.NumHours, new ResourceInfo(
                ResourceType.COMBO, "Number of hours of METARs to display"));
        resourceTypeMap.put(ResourceTag.NumHoursAll, new ResourceInfo(
                ResourceType.COMBO,
                "Number of hours of METARs to display when All selected"));
        resourceTypeMap.put(ResourceTag.ShowHeaders, new ResourceInfo(
                ResourceType.CHECK,
                "Show WMO headers while displaying TAFs and Metars"));
        resourceTypeMap.put(ResourceTag.ShowDecoded, new ResourceInfo(
                ResourceType.CHECK, "Show decoded METARs ARONET style"));
        resourceTypeMap.put(ResourceTag.ShowRoutine, new ResourceInfo(
                ResourceType.CHECK,
                "Show Show TAF as formatted for next routine issuance"));
        resourceTypeMap.put(ResourceTag.ShowProbs, new ResourceInfo(
                ResourceType.CHECK,
                "Show category probabilities for MOS reports"));
        resourceTypeMap.put(ResourceTag.ShowFormatted, new ResourceInfo(
                ResourceType.COMBO, "Format guidance reports"));
        resourceTypeMap.put(ResourceTag.HighlightFlightCat, new ResourceInfo(
                ResourceType.CHECK,
                "Highlight flight categories in TAF editor display window"));
        resourceTypeMap.put(ResourceTag.LifrColor, new ResourceInfo(
                ResourceType.COLOR, "background color for LIFR"));
        resourceTypeMap.put(ResourceTag.IfrColor, new ResourceInfo(
                ResourceType.COLOR, "background color for IFR"));
        resourceTypeMap.put(ResourceTag.MvfrColor, new ResourceInfo(
                ResourceType.COLOR, "background color for MVFR"));
        resourceTypeMap.put(ResourceTag.VfrColor, new ResourceInfo(
                ResourceType.COLOR, "background color for VFR"));
    }

    public Set<ResourceTag> getResourceTags() {
        return resourceTypeMap.keySet();
    }

    public ResourceType getResourceType(ResourceTag resourceTag) {
        return resourceTypeMap.get(resourceTag).getResourceType();
    }

    public String getResourceDesc(ResourceTag resourceTag) {
        return resourceTypeMap.get(resourceTag).getDescription();
    }

    public SpinnerData getSpinnerData(ResourceTag resourceTag) {
        final int wdMin = 200;
        final int wdMax = 1500;
        final int htMin = 50;
        final int htMax = 1200;
        final int inc = 50;

        if (resourceTag == ResourceTag.TextWidth) {
            return new SpinnerData(wdMin, wdMax, inc);
        } else if (resourceTag == ResourceTag.TextHeight) {
            return new SpinnerData(htMin, htMax, inc);
        } else if (resourceTag == ResourceTag.TextEditorWidth) {
            return new SpinnerData(wdMin, wdMax, inc);
        } else if (resourceTag == ResourceTag.TextEditorHeight) {
            return new SpinnerData(htMin, htMax, inc);
        } else if (resourceTag == ResourceTag.TextEditorInsWidth) {
            return new SpinnerData(1, 10, 2);
        } else if (resourceTag == ResourceTag.TextViewerWidth) {
            return new SpinnerData(wdMin, wdMax, inc);
        } else if (resourceTag == ResourceTag.TextViewerHeight) {
            return new SpinnerData(htMin, htMax, inc);
        }

        return new SpinnerData(0, 100, 10);
    }

    public String[] getComboValues(ResourceTag resourceTag) {
        if (resourceTag == ResourceTag.TextCursor) {
            return new String[] { "Arrow", "Cross", "Hand", "Help", "I-Beam",
                    "X" };
        } else if (resourceTag == ResourceTag.TextEditorCursor) {
            return new String[] { "Arrow", "Cross", "Hand", "Help", "I-Beam",
                    "X" };
        } else if (resourceTag == ResourceTag.Orientation) {
            return new String[] { "vertical", "horizontal" };
        } else if (resourceTag == ResourceTag.ImpactPlacement) {
            return new String[] { "top", "split", "bottom" };
        } else if (resourceTag == ResourceTag.NotifyDeiconify) {
            return new String[] { "disabled", "alertLevel2", "alertLevel3",
                    "alertLevel4", "alertLevel5", "alertLevel6" };
        } else if (resourceTag == ResourceTag.NotifyRaise) {
            return new String[] { "disabled", "alertLevel2", "alertLevel3",
                    "alertLevel4", "alertLevel5", "alertLevel6" };
        } else if (resourceTag == ResourceTag.NotifyPlay) {
            return new String[] { "disabled", "alertLevel2", "alertLevel3",
                    "alertLevel4", "alertLevel5", "alertLevel6" };
        } else if (resourceTag == ResourceTag.DisallowSend) {
            return new String[] { "always", "warning", "error", "fatal" };
        } else if (resourceTag == ResourceTag.LoadOrder) {
            return new String[] { "template", "merge", "latest" };
        } else if (resourceTag == ResourceTag.Wrap) {
            return new String[] { "none", "word" };
        } else if (resourceTag == ResourceTag.NumTafs) {
            return new String[] { "1", "3", "99" };
        } else if (resourceTag == ResourceTag.NumHours) {
            return new String[] { "1", "3", "6", "12", "24", "99" };
        } else if (resourceTag == ResourceTag.NumHoursAll) {
            return new String[] { "1", "3", "6", "12", "24", "99" };
        } else if (resourceTag == ResourceTag.ShowFormatted) {
            return new String[] { "raw", "long", "short" };
        }

        return new String[] {};
    }

    public String getResourceAsString(ResourceTag resourceTag) {
        HierarchicalPreferenceStore hStore = Activator.getDefault()
                .getPreferenceStore();

        return hStore.getString(resourceTag.tagName);
    }

    public boolean getResourceAsBoolean(ResourceTag resourceTag) {
        HierarchicalPreferenceStore hStore = Activator.getDefault()
                .getPreferenceStore();

        return hStore.getBoolean(resourceTag.tagName);
    }

    public int getResourceAsInt(ResourceTag resourceTag) {
        HierarchicalPreferenceStore hStore = Activator.getDefault()
                .getPreferenceStore();

        return hStore.getInt(resourceTag.tagName);
    }

    /**
     * Set a string data value for resource tag.
     * 
     * @param resourceTag
     * @param value
     */
    public void setDataAsString(ResourceTag resourceTag, String value) {
        resrcDataMgr.setResourceAsString(resourceTag, value);
    }

    /**
     * Obtain current string value for resource tag.
     * 
     * @param resourceTag
     * @return value
     */
    public String getDataAsString(ResourceTag resourceTag) {
        return resrcDataMgr.getResourceAsString(resourceTag);
    }

    /**
     * Set and integer data value for resource tag.
     * 
     * @param resourceTag
     * @param value
     */
    public void setDataAsInt(ResourceTag resourceTag, int value) {
        resrcDataMgr.setResourceAsInt(resourceTag, value);
    }

    /**
     * Get current integer data value for resource tag.
     * 
     * @param resourceTag
     * @return int
     */
    public int getDataAsInt(ResourceTag resourceTag) {
        return resrcDataMgr.getResourceAsInt(resourceTag);
    }

    /**
     * Set a boolean data value for resource tag.
     * 
     * @param resourceTag
     * @param value
     */
    public void setDataAsBoolean(ResourceTag resourceTag, boolean value) {
        resrcDataMgr.setResourceAsBoolean(resourceTag, value);
    }

    /**
     * Obtain the current data value for resource tag as a boolean.
     * 
     * @param resourceTag
     * @return value
     */
    public boolean getDataAsBoolean(ResourceTag resourceTag) {
        return resrcDataMgr.getResourceAsBoolean(resourceTag);
    }

    /**
     * Set the resource tag value in data store.
     * 
     * @param resourceTag
     * @param resource
     */
    public void setResourceString(ResourceTag resourceTag, String resource) {
        HierarchicalPreferenceStore hStore = Activator.getDefault()
                .getPreferenceStore();

        hStore.setValue(resourceTag.tagName, resource);
    }

    /**
     * Have the data manager reload its non-GUI component values.
     */
    public void reloadResourceData() {
        resrcDataMgr.reloadData();
    }

    public boolean isResourceLoaded() {
        return resrcDataMgr.isResourcesLoaded();
    }

    public void saveResources() {
        HierarchicalPreferenceStore hStore = Activator.getDefault()
                .getPreferenceStore();

        try {
            hStore.save();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    public String resetResource(ResourceTag resourceTag) {
        HierarchicalPreferenceStore hStore = Activator.getDefault()
                .getPreferenceStore();

        String defaultRes = hStore.getDefaultString(resourceTag.tagName);

        hStore.setValue(resourceTag.tagName, defaultRes);

        return defaultRes;
    }

    public void setDefaultFontAndColors(Control ctrl) {
        resrcDataMgr.setDefaultFontAndColors(ctrl);
    }

    public void setDefaultFontAndColors(Control ctrl, String text, GridData gd) {
        resrcDataMgr.setDefaultFontAndColors(ctrl, text, gd);
    }

    public void setDefaultFontAndForegroundColor(Control ctrl) {
        resrcDataMgr.setDefaultFontAndForegroundColor(ctrl);
    }

    public void setTextFontAndColors(Control ctrl) {
        resrcDataMgr.setTextFontAndColors(ctrl);
    }

    public void setTextEditorFontAndColors(Control ctrl) {
        resrcDataMgr.setTextEditorFontAndColors(ctrl);
    }

    public void setTextEditorFontAndReverseColors(Control ctrl) {
        resrcDataMgr.setTextEditorFontAndReverseColors(ctrl);
    }

    public void setEntryFontBgColor(Control ctrl) {
        resrcDataMgr.setEntryFontBgColor(ctrl);
    }

    public void setVfrColor(Control ctrl) {
        resrcDataMgr.setVfrColor(ctrl);
    }

    public void setIfrColor(Control ctrl) {
        resrcDataMgr.setIfrColor(ctrl);
    }

    public void setLifrColor(Control ctrl) {
        resrcDataMgr.setLifrColor(ctrl);
    }

    public void setMvfrColor(Control ctrl) {
        resrcDataMgr.setMvfrColor(ctrl);
    }

    public void setDefaultFont(Control ctrl) {
        // Set the default font
        String fontStr = getResourceAsString(ResourceTag.Font);
        String[] string = fontStr.split("-");

        Font tmpFont = new Font(ctrl.getParent().getDisplay(), new FontData(
                string[0], Integer.valueOf(string[1]), getStyleInt(string[2])));

        ctrl.setFont(tmpFont);
        tmpFont.dispose();
    }

    public Font getDefaultFont() {
        return resrcDataMgr.getDefaultFont();
    }

    public void setDefaultColors(Control ctrl) {
        resrcDataMgr.setDefaultColors(ctrl);
    }

    public void setMenuSetting(MenuItem ctrl, ResourceTag tag) {
        ctrl.setSelection(getResourceAsBoolean(tag));
    }

    public void setListBoxFont(Control ctrl) {
        resrcDataMgr.setListBoxFont(ctrl);
    }

    public RGB getMsgBarBackground() {
        return resrcDataMgr.getMsgBarBackground();
    }

    public Font getFont(ResourceTag tag) {
        return resrcDataMgr.getFont(tag);
    }

    public int getStyleInt(String styleStr) {
        return resrcDataMgr.getStyleInt(styleStr);
    }

    public String getStyleString(int styleInt) {
        return resrcDataMgr.getStyleString(styleInt);
    }

    public RGB getDefaultBackgroundRGB() {
        return resrcDataMgr.getDefaultBackgroundRGB();
    }

    public Color getDefaultBackgroundColor() {
        return resrcDataMgr.getDefaultBackgroundColor();
    }

    public Color getDefaultBackgroundOffsetColor() {
        return resrcDataMgr.getDefaultBackgroundOffsetColor();
    }

    public RGB getInsertBackgroundRgb() {
        return resrcDataMgr.getInsertBackgroundRgb();
    }

    public int getCursorAsInt(String cursorString) {
        if (cursorString.compareTo("Arrow") == 0) {
            return SWT.CURSOR_ARROW;
        } else if (cursorString.compareTo("Cross") == 0) {
            return SWT.CURSOR_CROSS;
        } else if (cursorString.compareTo("Hand") == 0) {
            return SWT.CURSOR_HAND;
        } else if (cursorString.compareTo("Help") == 0) {
            return SWT.CURSOR_HELP;
        } else if (cursorString.compareTo("I-Beam") == 0) {
            return SWT.CURSOR_IBEAM;
        } else if (cursorString.compareTo("X") == 0) {
            return SWT.CURSOR_NO;
        }

        return SWT.CURSOR_ARROW;
    }

    public Color getFlightCatColor(ResourceTag tag) {
        return resrcDataMgr.getFlightCatColor(tag);
    }

    public Color getAlertLevel0Color() {
        return resrcDataMgr.getAlertLevel0Color();
    }

    public Color getAlertLevel1Color() {
        return resrcDataMgr.getAlertLevel1Color();
    }

    public Color getAlertLevel2Color() {
        return resrcDataMgr.getAlertLevel2Color();
    }

    public Color getAlertLevel3Color() {
        return resrcDataMgr.getAlertLevel3Color();
    }

    public Color getAlertLevel4Color() {
        return resrcDataMgr.getAlertLevel4Color();
    }

    public Color getAlertLevel5Color() {
        return resrcDataMgr.getAlertLevel5Color();
    }

    public Color getAlertLevel6Color() {
        return resrcDataMgr.getAlertLevel6Color();
    }

    public Color getViwerAlertColor() {
        return resrcDataMgr.getViwerAlertColor();
    }

    public Color[] getAlertLevelColors() {
        return resrcDataMgr.getAlertLevelColors();
    }
}
