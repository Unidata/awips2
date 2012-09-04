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
package com.raytheon.uf.viz.d2d.ui.dialogs;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;

/**
 * Allows user printer settings to be persisted to an XML file.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 13, 2012       1053 jkorman     Initial creation
 *
 * </pre>
 *
 * @author jkorman
 * @version 1.0	
 */

@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
public class UserPrintSettings  implements ISerializableObject {

    public enum PRINT_ORIENTATION {
        PORTRAIT(false),
        LANDSCAPE(true);
        
        private final boolean printLandscape;
        
        private PRINT_ORIENTATION(boolean orientation) {
            this.printLandscape = orientation;
        }

        /**
         * Is this enum set to landscape?
         * @return Is this enum set to landscape?
         */
        public boolean isPrintLandscape() {
            return printLandscape;
        }

        /**
         * Get the proper enum instance depending on the orientation.
         * @param landscape The landscape mode. True return LANDSCAPE.
         * @return
         */
        public static PRINT_ORIENTATION getPrintOrientation(boolean landscape) {
            return (landscape) ? LANDSCAPE : PORTRAIT;
        }
    }

    @XmlElement
    private String printerUsed;
    
    @XmlElement
    private String printerFile;
    
    @XmlElement
    private boolean usePrinterFile = false;
    
    @XmlElement
    private PRINT_ORIENTATION orientation = PRINT_ORIENTATION.PORTRAIT;

    @XmlElement
    private boolean invertBlackWhite = false;
    
    @XmlElement
    private boolean printGrayScale = false;
    
    @XmlElement
    private Integer copies = 1;

    @XmlElement
    private Integer density = 0;
    
    @XmlElement
    private Integer mag = 0;

    @XmlElement
    private Integer scale = 100;
    
    /**
     * Construct an instance with defaults values.
     */
    public UserPrintSettings() {
    }

    /**
     * Get the name of the printer that was used.
     * @return The printer name.
     */
    public String getPrinterUsed() {
        return printerUsed;
    }

    /**
     * Set the name of the printer that was used.
     * @param printerUsed  The printer name.
     */
    public void setPrinterUsed(String printerUsed) {
        this.printerUsed = printerUsed;
    }

    /**
     * Get the name of the print file that was used.
     * @return The print filename.
     */
    public String getPrinterFile() {
        return printerFile;
    }

    /**
     * Set the name of the print file that was used.
     * @param printerFile The print filename.
     */
    public void setPrinterFile(String printerFile) {
        this.printerFile = printerFile;
    }

    /**
     * Should print to file be used?
     * @return Should print to file be used?
     */
    public boolean isUsePrinterFile() {
        return usePrinterFile;
    }

    /**
     * Set whether print to file was used.
     * @param usePrinterFile Was print to file used?
     */
    public void setUsePrinterFile(boolean usePrinterFile) {
        this.usePrinterFile = usePrinterFile;
    }

    /**
     * Get the print page orientation.
     * @return The print orientation.
     */
    public PRINT_ORIENTATION getOrientation() {
        return orientation;
    }

    /**
     * Set the print page orientation.
     * @param orientation The print orientation.
     */
    public void setOrientation(PRINT_ORIENTATION orientation) {
        this.orientation = orientation;
    }
    
    /**
     * Should black and white be inverted?
     * @return Should black and white be inverted?
     */
    public boolean getInvertBlackWhite() {
        return invertBlackWhite;
    }

    /**
     * Set whether black and white should be inverted?
     * @param invertBlackWhite Should black and white be inverted.
     */
    public void setInvertBlackWhite(boolean invertBlackWhite) {
        this.invertBlackWhite = invertBlackWhite;
    }

    /**
     * Should the print be converted to gray scale?
     * @return Should the print be converted to gray scale?
     */
    public boolean isPrintGrayScale() {
        return printGrayScale;
    }

    /**
     * Set whether the print should be converted to grayscale.
     * @param printGrayScale Should the print be converted to grayscale.
     */
    public void setPrintGrayScale(boolean printGrayScale) {
        this.printGrayScale = printGrayScale;
    }

    /**
     * Get the number of copies that should be printed.
     * @return The number of print copies.
     */
    public Integer getCopies() {
        return copies;
    }

    /**
     * Set the number of copies that should be printed.
     * @param copies The number of copies that should be printed.
     */
    public void setCopies(Integer copies) {
        this.copies = copies;
    }

    /**
     * Get the print density that should be used. NOTE : This value is the ordinal value
     * returned by the control. The value does not correspond to an actual density value.
     * @return the density
     */
    public Integer getDensity() {
        return density;
    }

    /**
     * @param density the density to set
     */
    public void setDensity(Integer density) {
        this.density = density;
    }

    /**
     * Get the print magnification that should be used. NOTE : This value is the ordinal value
     * returned by the control. The value does not correspond to an actual magnification value.
     * @return the density
     */
    public Integer getMag() {
        return mag;
    }

    /**
     * @param mag the mag to set
     */
    public void setMag(Integer mag) {
        this.mag = mag;
    }

    /**
     * Get the print scaling that should be used. This is a percent value i.e. 100% = 100.
     * @return The print scaling factor.
     */
    public Integer getScale() {
        return scale;
    }

    /**
     * Set the print scaling that should be used. This is a percent value i.e. 100% = 100.
     * @param scale The print scaling factor.
     */
    public void setScale(Integer scale) {
        this.scale = scale;
    }
}
