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
package com.raytheon.viz.texteditor.alarmalert.util;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.JAXB;

import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.dataplugin.text.alarms.AlarmAlertProduct;
import com.raytheon.uf.common.dataplugin.text.db.StdTextProduct;
import com.raytheon.uf.common.localization.ILocalizationFileObserver;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationOpFailedException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.viz.texteditor.alarmalert.dialogs.AlarmAlertBell;
import com.raytheon.viz.texteditor.command.CommandFactory;
import com.raytheon.viz.texteditor.command.CommandFailedException;
import com.raytheon.viz.texteditor.command.ICommand;
import com.raytheon.viz.texteditor.util.TextEditorUtil;

/**
 * This class is used for some of the calculation work used in the alarm/alert
 * functionality
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 18, 2009            mnash       Initial creation
 * 03/19/2012              D. Friedman Fix determination of "Alarm" entries.
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class AlarmAlertFunctions {

    private static final AlarmAlertProduct.ProductType AA = AlarmAlertProduct.ProductType.Alarm_Alert;

    private static final AlarmAlertProduct.ProductType PA = AlarmAlertProduct.ProductType.Proximity_Alarm;

    private static final Object configFileLock = new Object();

    private static final String ALARM_ALERT_PATH = "alarms" + File.separator;

    private static final String CONFIG_FILE = "settings.cfg";

    private static final String SITE_FILE = "DefaultSiteAlarms.xml";

    private static final AlarmAlertBell alarmAlertBell = new AlarmAlertBell(
            new Shell());

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(AlarmAlertFunctions.class);

    protected void getGIS() {

    }

    /**
     * Create the string for distance in the dialog
     * 
     * @param prod
     * @return
     */
    public static String buildDistance(AlarmAlertProduct prod) {
        StringBuilder string = new StringBuilder();
        if (prod.isAor()) {
            string.append("AOR");
        }
        if (!"".equals(prod.getAorDistance())) {
            string.append("+" + prod.getAorDistance() + prod.getAorLabel());
        } else if (!"".equals(prod.getUgcList())) {
            string.append("UGC-" + prod.getUgcList());
        }
        return string.toString();
    }

    protected void print(String textToPrint) {

    }

    protected static void ringBell(boolean sound) {
        alarmAlertBell.open(sound);
    }

    /**
     * Decides if the product is in the filtered alarm list and then tells the
     * application to notify or not
     * 
     * @param prod
     */
    public static void isInAlarmList(AlarmAlertProduct prod) {
        AlarmAlertLists instance = AlarmAlertLists.getInstance();

        List<AlarmAlertProduct> currentAlarms = instance.getFilteredProducts();
        boolean alarm = false;
        List<AlarmAlertProduct> prods = findMatches(prod.getProductId(),
                currentAlarms);
        // did we match anything?
        boolean alertAlarm = (prods.size() > 0);
        if (alertAlarm) {
            String pId = prods.get(0).getProductId();
            // first go get the product. All of the matching product identifiers
            // are
            // the same so just get the first.
            List<StdTextProduct> prodList = getProduct(pId);
            AlarmAlertProduct productFound = null;
            if (prodList.size() > 0) {
                String s = prodList.get(0).getProduct();
                for (AlarmAlertProduct p : prods) {
                    String search = p.getSearchString();

                    boolean match = false;
                    if ((search != null) && (search.length() > 0)) {
                        if (s.indexOf(search) >= 0) {
                            match = true;
                        }
                    } else {
                        match = true;
                    }
                    if (match) {
                        if (productFound == null)
                            productFound = p;
                        if ("Alarm".equals(p.getAlarmType()) && p.isAlarm()) {
                            alarm = true;
                            productFound = p;
                        }
                        if (alarm)
                            break;
                    }
                }
            }
            if (productFound != null) {
                prod.setAlarm(productFound.isAlarm());
                prod.setAlarmType(productFound.getAlarmType());

                instance.getCurrentAlarms().add(prod);
                instance.fireNewCurrentAlarmEvent(prod);

                ringBell(alarm);
            }
        }
    }

    public static void main(String[] args) {
        ringBell(true);
    }

    /**
     * Retrieve a text product from the text database based on its productId.
     * 
     * @param productId
     *            AFOS ProductId to retrieve from the text database.
     * @return A list of text products. Will always return a not null reference.
     */
    private static List<StdTextProduct> getProduct(String productId) {
        List<StdTextProduct> productList = null;

        ICommand command = CommandFactory.getAfosCommand(productId);
        try {
            productList = command.executeCommand(TextEditorUtil
                    .getTextDbsrvTransport());
        } catch (CommandFailedException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }
        if (productList == null) {
            productList = new ArrayList<StdTextProduct>();
        }
        return productList;
    }

    /**
     * Return a list of all alarms that match the incoming product identifier.
     * 
     * @param productId
     * @param currentAlarms
     * @return
     */
    private static List<AlarmAlertProduct> findMatches(String productId,
            List<AlarmAlertProduct> currentAlarms) {
        List<AlarmAlertProduct> prods = new ArrayList<AlarmAlertProduct>();
        if (productId != null) {
            productId = productId.trim().toUpperCase();
            for (AlarmAlertProduct a : currentAlarms) {
                // **************
                // TODO : For now disable Proximity Alerts
                // **************
                if (AA.equals(a.getProductType())) {
                    String s = a.getProductId();
                    if (s != null) {
                        s = s.trim().toUpperCase();
                        if (s.equals(productId)) {
                            // Reset the productId so we know we're dealing with
                            // uppercase
                            a.setProductId(s);
                            prods.add(a);
                        }
                    }
                }
            }
        }
        return prods;
    }

    /**
     * initialize the localization for user with the save/load functions
     * 
     * @return the initialized localization
     */
    public static LocalizationContext initUserLocalization() {
        return initLocalization(LocalizationLevel.USER);
    }

    /**
     * initialize the localization for site with the save/load functions
     * 
     * @return the initialized localization
     */
    public static LocalizationContext initSiteLocalization() {
        return initLocalization(LocalizationLevel.SITE);
    }

    /**
     * Initialize a LocalizationContext for the given LocalizationLevel.
     * 
     * @return the initialized localization
     */
    public static LocalizationContext initLocalization(LocalizationLevel level) {
        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationContext localization = pm.getContext(
                LocalizationType.COMMON_STATIC, level);
        return localization;
    }

    public static LocalizationFile getFile(LocalizationContext lc, String name) {
        LocalizationFile loc = PathManagerFactory.getPathManager()
                .getLocalizationFile(lc, ALARM_ALERT_PATH + name);
        return loc;
    }

    /*
     * Try to load the workstation file. If there is no workstation file then
     * try to load the site file and create a new workstation file from it. If
     * there is no site file, then create a new default workstation file.
     */
    public static AAPACombined loadSiteAlarms(ILocalizationFileObserver listener) {
        LocalizationFile workstationFile = getFile(
                initLocalization(LocalizationLevel.WORKSTATION), SITE_FILE);
        AAPACombined aapaCombined = null;

        if (workstationFile == null || !workstationFile.exists()) {
            // no workstation file found. try the site file
            LocalizationFile siteFile = getFile(initSiteLocalization(),
                    SITE_FILE);
            if (siteFile == null) {
                aapaCombined = createDefaultAAPACombined();
            } else {
                try {
                    aapaCombined = loadFile(siteFile.getFile());
                } catch (FileNotFoundException e) {
                    aapaCombined = createDefaultAAPACombined();
                }
            }
            // save work file
            if (workstationFile != null) {
                saveAlarms(aapaCombined, workstationFile);
            }
        } else {
            try {
                aapaCombined = loadFile(workstationFile.getFile());
            } catch (FileNotFoundException e) {
                aapaCombined = createDefaultAAPACombined();
            }
        }

        if (workstationFile != null) {
            workstationFile.addFileUpdatedObserver(listener);
        }
        return aapaCombined;
    }

    public static AAPACombined loadFile(File file) throws FileNotFoundException {
        AAPACombined rval = null;
        try {
            if (file.exists()) {
                rval = JAXB.unmarshal(file, AAPACombined.class);
                if (rval.getAaList() == null) {
                    rval.setAaList(new ArrayList<AlarmAlertProduct>());
                }
                if (rval.getPaList() == null) {
                    rval.setPaList(new ArrayList<AlarmAlertProduct>());
                }
                return rval;
            } else {
                StringBuilder sb = new StringBuilder("File ");
                sb.append(file);
                sb.append(" not found.");
                throw new FileNotFoundException(sb.toString());
            }
        } catch (RuntimeException e) {
            statusHandler.handle(Priority.CRITICAL, e.getLocalizedMessage(), e);
        }
        return rval;
    }

    public static AAPACombined createDefaultAAPACombined() {
        AAPACombined rval = new AAPACombined();
        rval.setAaList(new ArrayList<AlarmAlertProduct>());
        rval.setPaList(new ArrayList<AlarmAlertProduct>());
        return rval;
    }

    public static void saveAlarms(List<AlarmAlertProduct> aaList,
            List<AlarmAlertProduct> paList, LocalizationFile file) {
        AAPACombined combined = new AAPACombined();
        combined.setAaList(aaList);
        combined.setPaList(paList);
        saveAlarms(combined, file);
    }

    public static void saveAlarms(AAPACombined alarms, LocalizationFile file) {
        try {
            // Serialize
            JAXB.marshal(alarms, file.getFile());
            try {
                file.save();
            } catch (LocalizationOpFailedException e) {
                statusHandler.handle(Priority.SIGNIFICANT,
                        e.getLocalizedMessage(), e);
            }

        } catch (RuntimeException e) {
            statusHandler.handle(Priority.CRITICAL, e.getLocalizedMessage(), e);
        }
    }

    public static AAPACombined loadUserAlarmsFromConfig() {
        AAPACombined rval = null;
        // load from default file
        LocalizationFile lFile = getCurrentFileFromConfig();
        if (lFile != null) {
            File aaFile = lFile.getFile();
            if (aaFile != null && aaFile.exists()) {
                try {
                    rval = AlarmAlertFunctions.loadFile(aaFile);
                } catch (FileNotFoundException e) {
                    rval = createDefaultAAPACombined();
                }
            } else {
                rval = createDefaultAAPACombined();
            }
        }
        return rval;

    }

    public static LocalizationFile getCurrentFileFromConfig() {
        String filename = CONFIG_FILE;

        // load settings file
        synchronized (configFileLock) {
            LocalizationContext lc = AlarmAlertFunctions.initUserLocalization();
            LocalizationFile lFile = AlarmAlertFunctions.getFile(lc, filename);
            try {
                if (lFile != null && lFile.getFile().exists()) {

                    BufferedReader reader = null;
                    FileReader r = new FileReader(lFile.getFile());
                    reader = new BufferedReader(r);
                    filename = reader.readLine();
                    reader.close();

                } else if (lFile != null) {
                    lFile.getFile().createNewFile();
                    try {
                        lFile.save();
                    } catch (LocalizationOpFailedException e) {
                        statusHandler.handle(Priority.SIGNIFICANT,
                                e.getLocalizedMessage(), e);
                    }
                    filename = null;
                } else {
                    filename = null;
                }

            } catch (FileNotFoundException e) {
                statusHandler.handle(Priority.CRITICAL,
                        e.getLocalizedMessage(), e);
                filename = null;
            } catch (IOException e) {
                statusHandler.handle(Priority.CRITICAL,
                        e.getLocalizedMessage(), e);
                filename = null;
            }
            if (filename != null) {
                return AlarmAlertFunctions.getFile(lc, filename);
            } else {
                return null;
            }
        }
    }

    public static void setLastFileInConfig(String fname) {
        try {
            synchronized (configFileLock) {
                LocalizationFile lFile = getFile(initUserLocalization(),
                        CONFIG_FILE);
                if (lFile != null) {
                    File cfgFile = lFile.getFile();
                    cfgFile.delete();
                    cfgFile.createNewFile();
                    FileWriter w = new FileWriter(cfgFile);
                    w.write(fname);
                    w.close();
                    try {
                        lFile.save();
                    } catch (LocalizationOpFailedException e) {
                        statusHandler.handle(Priority.SIGNIFICANT,
                                e.getLocalizedMessage(), e);
                    }
                }
            }
        } catch (IOException e) {
            statusHandler.handle(Priority.CRITICAL, e.getLocalizedMessage(), e);
        }
    }

    public static List<AlarmAlertProduct> loadAllAlarms() {
        List<AlarmAlertProduct> alarms = new ArrayList<AlarmAlertProduct>();
        AAPACombined combined = loadUserAlarmsFromConfig();
        alarms.addAll(combined.getAaList());
        alarms.addAll(combined.getPaList());
        return alarms;
    }

    /**
     * @return the alarmalertbell
     */
    public static AlarmAlertBell getAlarmalertbell() {
        return alarmAlertBell;
    }
}
