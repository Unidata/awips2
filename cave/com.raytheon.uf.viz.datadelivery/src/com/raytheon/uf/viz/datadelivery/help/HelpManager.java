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
package com.raytheon.uf.viz.datadelivery.help;

import java.util.HashMap;
import java.util.Map;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;

import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.viz.ui.dialogs.ICloseCallback;

/**
 * Data Delivery Help Dialog manager class.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 4, 2013            mpduff     Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class HelpManager {
    /** Status Handler */
    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(HelpManager.class);

    /** Class instance */
    private static HelpManager instance = null;

    /** JAXB context */
    private JAXBContext jax;

    /** Unmarshaller object */
    private Unmarshaller unmarshaller;

    /** Help dialog map for managing open help dialogs */
    private final Map<String, DataDeliveryHelpDlg> helpMap = new HashMap<String, DataDeliveryHelpDlg>();

    /**
     * Private constructor.
     * 
     * @throws JAXBException
     */
    private HelpManager() throws JAXBException {
        createContext();
    }

    /**
     * Get the only instance of this class.
     * 
     * @return The instance
     * @throws Exception
     */
    public static final synchronized HelpManager getInstance() throws Exception {
        if (instance == null) {
            instance = new HelpManager();
        }
        return instance;
    }

    /**
     * Create the JAXB context
     * 
     * @throws JAXBException
     */
    private void createContext() throws JAXBException {
        Class[] classes = new Class[] { DataDeliveryHelpXML.class,
                HelpEntryXML.class };

        jax = JAXBContext.newInstance(classes);
        this.unmarshaller = jax.createUnmarshaller();
    }

    /**
     * Display a help dialog.
     * 
     * @param shell
     *            The parent shell
     * @param helpFile
     *            The file containing the text
     */
    public void displayHelpDialog(Shell shell, final String helpFile) {
        if (helpMap.containsKey(helpFile)) {
            helpMap.get(helpFile).bringToTop();
            return;
        }

        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationFile locFile = pm.getStaticLocalizationFile(helpFile);
        DataDeliveryHelpXML xml = new DataDeliveryHelpXML();

        if (locFile != null && locFile.exists()) {
            try {
                xml = (DataDeliveryHelpXML) unmarshaller.unmarshal(locFile
                        .getFile());
                DataDeliveryHelpDlg helpDlg = new DataDeliveryHelpDlg(shell, xml);
                helpDlg.setCloseCallback(new ICloseCallback() {
                    @Override
                    public void dialogClosed(Object returnValue) {
                        helpMap.remove(helpFile);
                    }
                });

                helpMap.put(helpFile, helpDlg);
                helpDlg.open();
            } catch (JAXBException e) {
                statusHandler.handle(Priority.ERROR,
                        "Error displaying help dialog for file " + helpFile, e);
            }
        }
    }
}
