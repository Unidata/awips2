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
package com.raytheon.viz.texteditor.dialogs;

import java.io.InputStream;
import java.util.ArrayList;

import javax.xml.bind.JAXB;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * Text Editor Configuration
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 15, 2013  15733     Xiaochuan   Initial creation
 * Jul 25, 2013  15733     Greg Hull   Combined FontSizeCfg, elements from TextColorsCfg 
 *                                      and added defaultNumEditors and  maxNumEditors
 * Mar 10, 2016  5411      randerso    Added flags to disable comma replacement,
 *                                      enable character set validation, and configure
 *                                      valid character sets
 * 
 * 
 * </pre>
 * 
 * @author XHuang
 * @version 1.0
 */
@XmlRootElement(name = "textEditorCfg")
@XmlAccessorType(XmlAccessType.NONE)
public class TextEditorCfg implements ISerializableObject {

    @XmlElement
    private Integer defaultNumEditors = 4;

    @XmlElement
    private Integer maxNumEditors = 8;

    @XmlElement(required = false)
    private Boolean replaceCommasWithEllipses = true;

    @XmlElement(required = false)
    private Boolean validateCharacterSet = false;

    @XmlElement(required = false)
    private String mixedCaseValidCharacters = "\r !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~";

    @XmlElement(required = false)
    private String upperCaseValidCharacters = "\r $%&*+-./0123456789>ABCDEFGHIJKLMNOPQRSTUVWXYZ_";

    @XmlElement
    private FontSizeCfg fontSizeCfg = new FontSizeCfg();

    private static SizeButtonCfg selectedFontButton = null;

    @XmlElement
    @XmlJavaTypeAdapter(RGBColorAdapter.class)
    private RGB textForegroundColor = new RGB(0, 0, 0);

    @XmlElement
    @XmlJavaTypeAdapter(RGBColorAdapter.class)
    private RGB textBackgroundColor = new RGB(255, 255, 255);

    @XmlElement
    @XmlJavaTypeAdapter(RGBColorAdapter.class)
    private RGB highlightTextForegroundColor = new RGB(0, 0, 0);

    @XmlElement
    @XmlJavaTypeAdapter(RGBColorAdapter.class)
    private RGB highlightTextBackgroundColor = new RGB(85, 152, 215);

    public Integer getDefaultNumEditors() {
        return defaultNumEditors;
    }

    public void setDefaultNumEditors(Integer defaultNumEditors) {
        if ((defaultNumEditors > 0) && (defaultNumEditors < 100)) { // sanity
                                                                    // check
            this.defaultNumEditors = defaultNumEditors;
        }
    }

    public Integer getMaxNumEditors() {
        return maxNumEditors;
    }

    public void setMaxNumEditors(Integer maxNumEditors) {
        if ((maxNumEditors > 0) && (maxNumEditors < 200)) { // sanity check
            this.maxNumEditors = maxNumEditors;
        }
    }

    public Boolean getReplaceCommasWithEllipses() {
        return replaceCommasWithEllipses;
    }

    public void setReplaceCommasWithEllipses(Boolean replaceCommasWithEllipses) {
        this.replaceCommasWithEllipses = replaceCommasWithEllipses;
    }

    public Boolean getValidateCharacterSet() {
        return validateCharacterSet;
    }

    public void setValidateCharacterSet(Boolean checkCharacterSet) {
        this.validateCharacterSet = checkCharacterSet;
    }

    /**
     * @return the mixedCaseValidCharacters
     */
    public String getMixedCaseValidCharacters() {
        return mixedCaseValidCharacters;
    }

    /**
     * @param mixedCaseValidCharacters
     *            the mixedCaseValidCharacters to set
     */
    public void setMixedCaseValidCharacters(String mixedCaseValidCharacters) {
        this.mixedCaseValidCharacters = mixedCaseValidCharacters;
    }

    /**
     * @return the upperCaseValidCharcters
     */
    public String getUpperCaseValidCharcters() {
        return upperCaseValidCharacters;
    }

    /**
     * @param upperCaseValidCharcters
     *            the upperCaseValidCharcters to set
     */
    public void setUpperCaseValidCharcters(String upperCaseValidCharcters) {
        this.upperCaseValidCharacters = upperCaseValidCharcters;
    }

    public FontSizeCfg getFontSizeCfg() {
        return fontSizeCfg;
    }

    public void setFontSizeCfg(FontSizeCfg fontSizeCfg) {
        this.fontSizeCfg = fontSizeCfg;
    }

    public static SizeButtonCfg getSelectedFontButton() {
        return selectedFontButton;
    }

    public RGB getTextForegroundColor() {
        return textForegroundColor;
    }

    public void setTextForegroundColor(RGB textForegroundColor) {
        this.textForegroundColor = textForegroundColor;
    }

    public RGB getTextBackgroundColor() {
        return textBackgroundColor;
    }

    public void setTextBackgroundColor(RGB textBackgroundColor) {
        this.textBackgroundColor = textBackgroundColor;
    }

    public RGB getHighlightTextForegroundColor() {
        return highlightTextForegroundColor;
    }

    public void setHighlightTextForegroundColor(RGB highlightTextForegroundColor) {
        this.highlightTextForegroundColor = highlightTextForegroundColor;
    }

    public RGB getHighlightTextBackgroundColor() {
        return highlightTextBackgroundColor;
    }

    public void setHighlightTextBackgroundColor(RGB highlightTextBackgroundColor) {
        this.highlightTextBackgroundColor = highlightTextBackgroundColor;
    }

    private static TextEditorCfg textEditorCfg = null;

    public static TextEditorCfg getTextEditorCfg() {

        if (textEditorCfg == null) {
            try {
                IPathManager pm = PathManagerFactory.getPathManager();
                LocalizationFile lf = pm
                        .getStaticLocalizationFile("textws/gui/TextEditorCfg.xml");
                if (lf == null) {
                    throw new Exception(
                            "localization file textws/gui/TextEditorCfg.xml not found");
                }

                try (InputStream is = lf.openInputStream()) {
                    textEditorCfg = JAXB.unmarshal(is, TextEditorCfg.class);
                }
            } catch (Exception ex) {
                IUFStatusHandler statusHandler = UFStatus
                        .getHandler(TextEditorDialog.class);
                statusHandler.handle(Priority.ERROR,
                        "Error with TextEditorCfg.xml file. Using defaults: ",
                        ex);
                textEditorCfg = new TextEditorCfg();
            }

            FontSizeCfg fontSizeCfg = textEditorCfg.getFontSizeCfg();

            if (fontSizeCfg == null) {
                // set meaningful dflt values
                FontSizeCfg fscfg = new FontSizeCfg();
                fscfg.setButtons(new ArrayList<SizeButtonCfg>());
                textEditorCfg.setFontSizeCfg(fscfg);
            }

            // do some sanity checking
            if ((fontSizeCfg.getButtons() == null)
                    || fontSizeCfg.getButtons().isEmpty()) {
                // default to 1 medium button
                selectedFontButton = new SizeButtonCfg();
                fontSizeCfg.setButtons(new ArrayList<SizeButtonCfg>());
                fontSizeCfg.getButtons().add(selectedFontButton);
            } else {
                for (SizeButtonCfg buttonCfg : fontSizeCfg.getButtons()) {
                    if (buttonCfg.isSelected()) {
                        if (selectedFontButton == null) {
                            selectedFontButton = buttonCfg;
                        } else {
                            buttonCfg.setSelected(false);
                            System.out
                                    .println("Sanity check in textEditorCfg.xml file:"
                                            + " only 1 font button can be selected");
                        }
                    }
                }
                if (selectedFontButton == null) {
                    System.out
                            .println("Sanity check in textEditorCfg.xml file:"
                                    + " no font button set asselected. Defaulting to the first");
                    selectedFontButton = fontSizeCfg.getButtons().get(0);
                }
            }
        }
        return textEditorCfg;
    }
}
