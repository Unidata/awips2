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

import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;

import javax.xml.bind.JAXBException;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.adapters.XmlAdapter;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import org.eclipse.swt.printing.Printer;
import org.eclipse.swt.printing.PrinterData;

import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SingleTypeJAXBManager;
import com.raytheon.uf.viz.d2d.ui.Activator;

/**
 * Allows user printer settings to be persisted to an XML file.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Aug 13, 2012  1053     jkorman   Initial creation
 * May 13, 2016  5653     randerso  Added Fit to Page setting
 * Feb 19, 2020  8039     randerso  Updated to store PrinterData settings. Added
 *                                  load/store methods to store under user's
 *                                  home directory instead of localization since
 *                                  print settings are not portable between
 *                                  Linux/Windows.
 *
 * </pre>
 *
 * @author jkorman
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
public class UserPrintSettings {

    private static class PrinterDataXML {
        @XmlElement
        public String driver;

        @XmlElement
        public String name;

        @XmlElement
        public int scope;

        @XmlElement
        public int startPage;

        @XmlElement
        public int endPage;

        @XmlElement
        public boolean printToFile;

        @XmlElement
        public String fileName;

        @XmlElement
        public int copyCount;

        @XmlElement
        public boolean collate;

        @XmlElement
        public int orientation;

        @XmlElement
        public int duplex;

        /**
         * Nullary constructor required for jaxb serialization
         */
        @SuppressWarnings("unused")
        private PrinterDataXML() {
        }

        public PrinterDataXML(PrinterData printerData) {
            driver = printerData.driver;
            name = printerData.name;
            scope = printerData.scope;
            startPage = printerData.startPage;
            endPage = printerData.endPage;
            printToFile = printerData.printToFile;
            fileName = printerData.fileName;
            copyCount = printerData.copyCount;
            collate = printerData.collate;
            orientation = printerData.orientation;
            duplex = printerData.duplex;
        }

        public PrinterData getPrinterData() {
            PrinterData printerData = new PrinterData();
            printerData.driver = driver;
            printerData.name = name;
            printerData.scope = scope;
            printerData.startPage = startPage;
            printerData.endPage = endPage;
            printerData.printToFile = printToFile;
            printerData.fileName = fileName;
            printerData.copyCount = copyCount;
            printerData.collate = collate;
            printerData.orientation = orientation;
            printerData.duplex = duplex;

            return printerData;
        }
    }

    private static class PrinterDataAdapter
            extends XmlAdapter<PrinterDataXML, PrinterData> {

        @Override
        public PrinterData unmarshal(PrinterDataXML v) throws Exception {
            return v.getPrinterData();
        }

        @Override
        public PrinterDataXML marshal(PrinterData v) throws Exception {
            return new PrinterDataXML(v);
        }

    }

    @XmlElement
    private boolean invertBlackWhite = true;

    @XmlElement
    private Integer density;

    @XmlElement
    private Integer mag;

    @XmlElement
    private boolean fitToPage = true;

    @XmlJavaTypeAdapter(value = PrinterDataAdapter.class)
    private PrinterData printerData;

    /**
     * Default constructor
     */
    public UserPrintSettings() {
        this.printerData = Printer.getDefaultPrinterData();
    }

    /**
     * Store this UserPrinterSettings object directory
     *
     * @param filename
     * @throws IOException
     * @throws LocalizationException
     * @throws JAXBException
     * @throws SerializationException
     */
    public void store(String filename) throws IOException,
            LocalizationException, JAXBException, SerializationException {

        Path path = Path.of(
                Activator.getDefault().getStateLocation().toString(), filename);
        try (FileOutputStream out = new FileOutputStream(path.toFile())) {
            SingleTypeJAXBManager<UserPrintSettings> jaxb = new SingleTypeJAXBManager<>(
                    UserPrintSettings.class);
            jaxb.marshalToStream(this, out);
        }
    }

    /**
     * Load UserPrinterSettings
     *
     * @param filename
     * @return default settings if file does not exist
     *
     * @throws IOException
     * @throws LocalizationException
     * @throws JAXBException
     * @throws SerializationException
     */
    public static UserPrintSettings load(String filename) throws IOException,
            LocalizationException, JAXBException, SerializationException {

        Path path = Path.of(
                Activator.getDefault().getStateLocation().toString(), filename);

        UserPrintSettings ups;
        if (Files.exists(path)) {
            try (InputStream in = new FileInputStream(path.toFile())) {
                SingleTypeJAXBManager<UserPrintSettings> jaxb = new SingleTypeJAXBManager<>(
                        UserPrintSettings.class);
                ups = jaxb.unmarshalFromInputStream(in);

            }
        } else {
            ups = new UserPrintSettings();
        }

        return ups;
    }

    /**
     * @return the invertBlackWhite
     */
    public boolean isInvertBlackWhite() {
        return invertBlackWhite;
    }

    /**
     * @param invertBlackWhite
     *            the invertBlackWhite to set
     */
    public void setInvertBlackWhite(boolean invertBlackWhite) {
        this.invertBlackWhite = invertBlackWhite;
    }

    /**
     * @return the density
     */
    public Integer getDensity() {
        return density;
    }

    /**
     * @param density
     *            the density to set
     */
    public void setDensity(Integer density) {
        this.density = density;
    }

    /**
     * @return the mag
     */
    public Integer getMag() {
        return mag;
    }

    /**
     * @param mag
     *            the mag to set
     */
    public void setMag(Integer mag) {
        this.mag = mag;
    }

    /**
     * @return the fitToPage
     */
    public boolean isFitToPage() {
        return fitToPage;
    }

    /**
     * @param fitToPage
     *            the fitToPage to set
     */
    public void setFitToPage(boolean fitToPage) {
        this.fitToPage = fitToPage;
    }

    /**
     * @return the printerData
     */
    public PrinterData getPrinterData() {
        return printerData;
    }

    /**
     * @param printerData
     *            the printerData to set
     */
    public void setPrinterData(PrinterData printerData) {
        this.printerData = printerData;
    }
}
