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
package com.raytheon.viz.aviation.xml;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

@XmlRootElement(name = "resourceConfig")
@XmlAccessorType(XmlAccessType.NONE)
public class ResourceConfigData {
    @XmlElement(name = "font")
    private String font;

    @XmlElement(name = "background")
    private String background;

    @XmlElement(name = "foreground")
    private String foreground;

    @XmlElement(name = "textFont")
    private String textFont;

    @XmlElement(name = "textBackground")
    private String textBackground;

    @XmlElement(name = "textForeground")
    private String textForeground;

    @XmlElement(name = "textWidth")
    private int textWidth;

    @XmlElement(name = "textHeight")
    private int textHeight;

    @XmlElement(name = "textCursor")
    private String textCursor;

    @XmlElement(name = "textEditorFont")
    private String textEditorFont;

    @XmlElement(name = "textEditorBackground")
    private String textEditorBackground;

    @XmlElement(name = "textEditorForeground")
    private String textEditorForeground;

    @XmlElement(name = "textEditorWidth")
    private int textEditorWidth;

    @XmlElement(name = "textEditorHeight")
    private int textEditorHeight;

    @XmlElement(name = "textEditorCursor")
    private String textEditorCursor;

    @XmlElement(name = "textEditorInsWidth")
    private int textEditorInsWidth;

    @XmlElement(name = "insertBackground")
    private String insertBackground;

    @XmlElement(name = "textViewerWidth")
    private int textViewerWidth;

    @XmlElement(name = "textViewerHeight")
    private int textViewerHeight;

    @XmlElement(name = "orientation")
    private String orientation;

    @XmlElement(name = "listBoxFont")
    private String listBoxFont;

    @XmlElement(name = "entryFont")
    private String entryFont;

    @XmlElement(name = "entryBackground")
    private String entryBackground;

    @XmlElement(name = "msgBarEntryBgnd")
    private String msgBarEntryBgnd;

    @XmlElement(name = "toolTipFont")
    private String toolTipFont;

    @XmlElement(name = "impactPlacement")
    private String impactPlacement;

    @XmlElement(name = "transientDialogs")
    private boolean transientDialogs = true;

    @XmlElement(name = "confirmClose")
    private boolean confirmClose;

    @XmlElement(name = "confirmSend")
    private boolean confirmSend;

    @XmlElement(name = "alertLevel0")
    private String alertLevel0;

    @XmlElement(name = "alertLevel1")
    private String alertLevel1;

    @XmlElement(name = "alertLevel2")
    private String alertLevel2;

    @XmlElement(name = "alertLevel3")
    private String alertLevel3;

    @XmlElement(name = "alertLevel4")
    private String alertLevel4;

    @XmlElement(name = "alertLevel5")
    private String alertLevel5;

    @XmlElement(name = "alertLevel6")
    private String alertLevel6;

    @XmlElement(name = "notifyDeiconify")
    private String notifyDeiconify;

    @XmlElement(name = "notifyRaise")
    private String notifyRaise;

    @XmlElement(name = "notifyPlay")
    private String notifyPlay;

    @XmlElement(name = "playFile")
    private String playFile;

    @XmlElement(name = "blink")
    private boolean blink;

    @XmlElement(name = "disallowSend")
    private String disallowSend;

    @XmlElement(name = "loadOrder")
    private String loadOrder;

    @XmlElement(name = "autoSave")
    private boolean autoSave;

    @XmlElement(name = "updateTimes")
    private boolean updateTimes;

    @XmlElement(name = "autoPrint")
    private boolean autoPrint;

    @XmlElement(name = "insert")
    private boolean insert;

    @XmlElement(name = "wrap")
    private String wrap;

    @XmlElement(name = "showUnique")
    private boolean showUnique;

    @XmlElement(name = "amdbuttons")
    private boolean amdbuttons;

    @XmlElement(name = "numTafs")
    private int numTafs;

    @XmlElement(name = "numHours")
    private int numHours;

    @XmlElement(name = "numHoursAll")
    private int numHoursAll;

    @XmlElement(name = "showHeaders")
    private boolean showHeaders;

    @XmlElement(name = "showDecoded")
    private boolean showDecoded;

    @XmlElement(name = "showProbs")
    private boolean showProbs;

    @XmlElement(name = "showFormatted")
    private String showFormatted;

    @XmlElement(name = "highlightFlightCat")
    private boolean highlightFlightCat;

    @XmlElement(name = "lifrColor")
    private String lifrColor;

    @XmlElement(name = "ifrColor")
    private String ifrColor;

    @XmlElement(name = "mvfrColor")
    private String mvfrColor;

    @XmlElement(name = "vfrColor")
    private String vfrColor;

    public ResourceConfigData() {
    }

    public String getFont() {
        return font;
    }

    public void setFont(String font) {
        this.font = font;
    }

    public String getBackground() {
        return background;
    }

    public void setBackground(String background) {
        this.background = background;
    }

    public String getForeground() {
        return foreground;
    }

    public void setForeground(String foreground) {
        this.foreground = foreground;
    }

    public String getTextFont() {
        return textFont;
    }

    public void setTextFont(String textFont) {
        this.textFont = textFont;
    }

    public String getTextBackground() {
        return textBackground;
    }

    public void setTextBackground(String textBackground) {
        this.textBackground = textBackground;
    }

    public String getTextForeground() {
        return textForeground;
    }

    public void setTextForeground(String textForeground) {
        this.textForeground = textForeground;
    }

    public int getTextWidth() {
        return textWidth;
    }

    public void setTextWidth(int textWidth) {
        this.textWidth = textWidth;
    }

    public int getTextHeight() {
        return textHeight;
    }

    public void setTextHeight(int textHeight) {
        this.textHeight = textHeight;
    }

    public String getTextCursor() {
        return textCursor;
    }

    public void setTextCursor(String textCursor) {
        this.textCursor = textCursor;
    }

    public String getTextEditorFont() {
        return textEditorFont;
    }

    public void setTextEditorFont(String textEditorFont) {
        this.textEditorFont = textEditorFont;
    }

    public String getTextEditorBackground() {
        return textEditorBackground;
    }

    public void setTextEditorBackground(String textEditorBackground) {
        this.textEditorBackground = textEditorBackground;
    }

    public String getTextEditorForeground() {
        return textEditorForeground;
    }

    public void setTextEditorForeground(String textEditorForeground) {
        this.textEditorForeground = textEditorForeground;
    }

    public int getTextEditorWidth() {
        return textEditorWidth;
    }

    public void setTextEditorWidth(int textEditorWidth) {
        this.textEditorWidth = textEditorWidth;
    }

    public int getTextEditorHeight() {
        return textEditorHeight;
    }

    public void setTextEditorHeight(int textEditorHeight) {
        this.textEditorHeight = textEditorHeight;
    }

    public String getTextEditorCursor() {
        return textEditorCursor;
    }

    public void setTextEditorCursor(String textEditorCursor) {
        this.textEditorCursor = textEditorCursor;
    }

    public int getTextEditorInsWidth() {
        return textEditorInsWidth;
    }

    public void setTextEditorInsWidth(int textEditorInsWidth) {
        this.textEditorInsWidth = textEditorInsWidth;
    }

    public String getInsertBackground() {
        return insertBackground;
    }

    public void setInsertBackground(String insertBackground) {
        this.insertBackground = insertBackground;
    }

    public int getTextViewerWidth() {
        return textViewerWidth;
    }

    public void setTextViewerWidth(int textViewerWidth) {
        this.textViewerWidth = textViewerWidth;
    }

    public int getTextViewerHeight() {
        return textViewerHeight;
    }

    public void setTextViewerHeight(int textViewerHeight) {
        this.textViewerHeight = textViewerHeight;
    }

    public String getOrientation() {
        return orientation;
    }

    public void setOrientation(String orientation) {
        this.orientation = orientation;
    }

    public String getListBoxFont() {
        return listBoxFont;
    }

    public void setListBoxFont(String listBoxFont) {
        this.listBoxFont = listBoxFont;
    }

    public String getEntryFont() {
        return entryFont;
    }

    public void setEntryFont(String entryFont) {
        this.entryFont = entryFont;
    }

    public String getEntryBackground() {
        return entryBackground;
    }

    public void setEntryBackground(String entryBackground) {
        this.entryBackground = entryBackground;
    }

    public String getMsgBarEntryBgnd() {
        return msgBarEntryBgnd;
    }

    public void setMsgBarEntryBgnd(String msgBarEntryBgnd) {
        this.msgBarEntryBgnd = msgBarEntryBgnd;
    }

    public String getToolTipFont() {
        return toolTipFont;
    }

    public void setToolTipFont(String toolTipFont) {
        this.toolTipFont = toolTipFont;
    }

    public String getImpactPlacement() {
        return impactPlacement;
    }

    public void setImpactPlacement(String impactPlacement) {
        this.impactPlacement = impactPlacement;
    }

    public boolean getTransientDialogs() {
        return transientDialogs;
    }

    public void setTransientDialogs(boolean transientDialogs) {
        this.transientDialogs = transientDialogs;
    }

    public boolean getConfirmClose() {
        return confirmClose;
    }

    public void setConfirmClose(boolean confirmClose) {
        this.confirmClose = confirmClose;
    }

    public boolean getConfirmSend() {
        return confirmSend;
    }

    public void setConfirmSend(boolean confirmSend) {
        this.confirmSend = confirmSend;
    }

    public String getAlertLevel0() {
        return alertLevel0;
    }

    public void setAlertLevel0(String alertLevel0) {
        this.alertLevel0 = alertLevel0;
    }

    public String getAlertLevel1() {
        return alertLevel1;
    }

    public void setAlertLevel1(String alertLevel1) {
        this.alertLevel1 = alertLevel1;
    }

    public String getAlertLevel2() {
        return alertLevel2;
    }

    public void setAlertLevel2(String alertLevel2) {
        this.alertLevel2 = alertLevel2;
    }

    public String getAlertLevel3() {
        return alertLevel3;
    }

    public void setAlertLevel3(String alertLevel3) {
        this.alertLevel3 = alertLevel3;
    }

    public String getAlertLevel4() {
        return alertLevel4;
    }

    public void setAlertLevel4(String alertLevel4) {
        this.alertLevel4 = alertLevel4;
    }

    public String getAlertLevel5() {
        return alertLevel5;
    }

    public void setAlertLevel5(String alertLevel5) {
        this.alertLevel5 = alertLevel5;
    }

    public String getAlertLevel6() {
        return alertLevel6;
    }

    public void setAlertLevel6(String alertLevel6) {
        this.alertLevel6 = alertLevel6;
    }

    public String getNotifyDeiconify() {
        return notifyDeiconify;
    }

    public void setNotifyDeiconify(String notifyDeiconify) {
        this.notifyDeiconify = notifyDeiconify;
    }

    public String getNotifyRaise() {
        return notifyRaise;
    }

    public void setNotifyRaise(String notifyRaise) {
        this.notifyRaise = notifyRaise;
    }

    public String getNotifyPlay() {
        return notifyPlay;
    }

    public void setNotifyPlay(String notifyPlay) {
        this.notifyPlay = notifyPlay;
    }

    public String getPlayFile() {
        return playFile;
    }

    public void setPlayFile(String playFile) {
        this.playFile = playFile;
    }

    public boolean getBlink() {
        return blink;
    }

    public void setBlink(boolean blink) {
        this.blink = blink;
    }

    public String getDisallowSend() {
        return disallowSend;
    }

    public void setDisallowSend(String disallowSend) {
        this.disallowSend = disallowSend;
    }

    public String getLoadOrder() {
        return loadOrder;
    }

    public void setLoadOrder(String loadOrder) {
        this.loadOrder = loadOrder;
    }

    public boolean getAutoSave() {
        return autoSave;
    }

    public void setAutoSave(boolean autoSave) {
        this.autoSave = autoSave;
    }

    public boolean getUpdateTimes() {
        return updateTimes;
    }

    public void setUpdateTimes(boolean updateTimes) {
        this.updateTimes = updateTimes;
    }

    public boolean getAutoPrint() {
        return autoPrint;
    }

    public void setAutoPrint(boolean autoPrint) {
        this.autoPrint = autoPrint;
    }

    public boolean getInsert() {
        return insert;
    }

    public void setInsert(boolean insert) {
        this.insert = insert;
    }

    public String getWrap() {
        return wrap;
    }

    public void setWrap(String wrap) {
        this.wrap = wrap;
    }

    public boolean getShowUnique() {
        return showUnique;
    }

    public void setShowUnique(boolean showUnique) {
        this.showUnique = showUnique;
    }

    public boolean getAmdbuttons() {
        return amdbuttons;
    }

    public void setAmdbuttons(boolean amdbuttons) {
        this.amdbuttons = amdbuttons;
    }

    public int getNumTafs() {
        return numTafs;
    }

    public void setNumTafs(int numTafs) {
        this.numTafs = numTafs;
    }

    public int getNumHours() {
        return numHours;
    }

    public void setNumHours(int numHours) {
        this.numHours = numHours;
    }

    public int getNumHoursAll() {
        return numHoursAll;
    }

    public void setNumHoursAll(int numHoursAll) {
        this.numHoursAll = numHoursAll;
    }

    public boolean getShowHeaders() {
        return showHeaders;
    }

    public void setShowHeaders(boolean showHeaders) {
        this.showHeaders = showHeaders;
    }

    public boolean getShowDecoded() {
        return showDecoded;
    }

    public void setShowDecoded(boolean showDecoded) {
        this.showDecoded = showDecoded;
    }

    public boolean getShowProbs() {
        return showProbs;
    }

    public void setShowProbs(boolean showProbs) {
        this.showProbs = showProbs;
    }

    public String getShowFormatted() {
        return showFormatted;
    }

    public void setShowFormatted(String showFormatted) {
        this.showFormatted = showFormatted;
    }

    public boolean getHighlightFlightCat() {
        return highlightFlightCat;
    }

    public void setHighlightFlightCat(boolean highlightFlightCat) {
        this.highlightFlightCat = highlightFlightCat;
    }

    public String getLifrColor() {
        return lifrColor;
    }

    public void setLifrColor(String lifrColor) {
        this.lifrColor = lifrColor;
    }

    public String getIfrColor() {
        return ifrColor;
    }

    public void setIfrColor(String ifrColor) {
        this.ifrColor = ifrColor;
    }

    public String getMvfrColor() {
        return mvfrColor;
    }

    public void setMvfrColor(String mvfrColor) {
        this.mvfrColor = mvfrColor;
    }

    public String getVfrColor() {
        return vfrColor;
    }

    public void setVfrColor(String vfrColor) {
        this.vfrColor = vfrColor;
    }
}
