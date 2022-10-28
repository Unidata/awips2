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
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Scanner;
import java.util.StringTokenizer;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.xml.bind.JAXB;

import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.dataplugin.text.alarms.AlarmAlertProduct;
import com.raytheon.uf.common.dataplugin.text.alarms.AlarmAlertProduct.ProductType;
import com.raytheon.uf.common.dataplugin.text.db.StdTextProduct;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.geospatial.SpatialException;
import com.raytheon.uf.common.geospatial.SpatialQueryFactory;
import com.raytheon.uf.common.geospatial.SpatialQueryResult;
import com.raytheon.uf.common.localization.ILocalizationFileObserver;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.localization.LocalizationManager;
import com.raytheon.viz.texteditor.alarmalert.dialogs.AlarmAlertBell;
import com.raytheon.viz.texteditor.command.CommandFactory;
import com.raytheon.viz.texteditor.command.CommandFailedException;
import com.raytheon.viz.texteditor.command.ICommand;
import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.geom.GeometryFactory;

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
 * 12/07/2012   15555  m.gamazaychikov Added methods and constants for
 *                                     the implementation of proximity alarm
 * 07/24/2014    3423      randerso    Ensure ringBell is called on UI thread
 * 09/09/2014    3580      mapeters    Removed IQueryTransport usage (no longer exists).
 * 12/03/2014   16829      D. Friedman Lazy initialization of alarmAlertBell
 * Nov 12, 2015 4834       njensen     Changed LocalizationOpFailedException to LocalizationException
 * 11/29/2015   14995  m.gamazaychikov Made sure that non-standard latlons in
 *                                     LAT...LON string did not result in error.
 * 15/01/2016    5054      randerso    Use proper parent shell
 * 03/30/2016    5513      randerso    Fixed AlarmAlertBell to display on same monitor as parent
 *                                     code cleanup
 * 07/05/2016   19153   mgamazaychikov Fix disappearance of AlarmAlert Bell after initial close
 * Jan 24, 2018  7132      tgurney     Add destroyAlarmAlertBell()
 *
 *
 * </pre>
 *
 * @author mnash
 */
public class AlarmAlertFunctions {

    private static final AlarmAlertProduct.ProductType AA = AlarmAlertProduct.ProductType.Alarm_Alert;

    private static final AlarmAlertProduct.ProductType PA = AlarmAlertProduct.ProductType.Proximity_Alarm;

    private static final Object configFileLock = new Object();

    private static final String ALARM_ALERT_PATH = "alarms" + File.separator;

    private static final String CONFIG_FILE = "settings.cfg";

    private static final String SITE_FILE = "DefaultSiteAlarms.xml";

    private static AlarmAlertBell alarmAlertBell;

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(AlarmAlertFunctions.class);

    private static final Pattern UGC_NEW_PATTERN = Pattern
            .compile("^(([A-Z]{3})(\\d{3}))$");

    private static final Pattern UGC_FOLLOW_PATTERN = Pattern
            .compile("^(\\d{3})$");

    private static String DEFAULT_DISTANCE = "3000";

    private static final String HYPHEN = Pattern.quote("-");

    public static final Pattern UGC = Pattern.compile(
            "(^(\\w{2}[CZ]\\d{3}\\S*-\\d{6}-)$|((\\d{3}-)*\\d{6}-)$|((\\d{3}-)+))");

    private static final Pattern LAT_LON_PATTERN = Pattern
            .compile("LAT...LON+(\\s\\d{3,4}\\s\\d{3,5}){1,}");

    private static final Pattern SUB_LAT_LON_PATTERN = Pattern
            .compile("\\s(\\d{3,4})\\s(\\d{3,5})");

    private static final double ONE_DEGREE_MI = 69.09;

    private static final double ONE_DEGREE_KM = 111.20;

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
            /*
             * DR15555 - check the text content, if it is not a valid number set
             * the text to default 3000 mi
             */
            try (Scanner scn = new Scanner(prod.getAorDistance())) {
                while (scn.hasNext()) {
                    if (!scn.hasNextInt()) {
                        prod.setAorDistance(DEFAULT_DISTANCE);
                        break;
                    } else {
                        scn.next();
                    }
                }
            }
            string.append("AOR+" + prod.getAorDistance() + prod.getAorLabel());
        } else if (!"".equals(prod.getUgcList())) {
            string.append("UGC-" + prod.getUgcList());
        }
        return string.toString();
    }

    private static synchronized void ringBell(boolean sound) {
        if (alarmAlertBell != null) {
            alarmAlertBell.open(sound);
        }
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
        boolean alertAlarm = !prods.isEmpty();
        if (alertAlarm) {
            String pId = prods.get(0).getProductId();
            // first go get the product. All of the matching product identifiers
            // are the same so just get the first.
            List<StdTextProduct> prodList = getProduct(pId);
            AlarmAlertProduct productFound = null;
            if (!prodList.isEmpty()) {
                String s = prodList.get(0).getProduct();
                for (AlarmAlertProduct p : prods) {
                    String search = p.getSearchString();

                    boolean match = false;
                    if (search != null && search.length() > 0) {
                        if (s.indexOf(search) >= 0) {
                            match = true;
                        }
                    } else {
                        match = true;
                    }
                    if (match) {
                        if (productFound == null) {
                            productFound = p;
                        }
                        if ("Alarm".equals(p.getAlarmType()) && p.isAlarm()) {
                            alarm = true;
                            productFound = p;
                        }
                        if (alarm) {
                            break;
                        }
                    }
                }
            }
            if (productFound != null) {
                prod.setAlarm(productFound.isAlarm());
                prod.setAlarmType(productFound.getAlarmType());

                instance.getCurrentAlarms().add(prod);
                instance.fireNewCurrentAlarmEvent(prod);

                final boolean sound = alarm;
                VizApp.runAsync(new Runnable() {

                    @Override
                    public void run() {
                        ringBell(sound);
                    }
                });
            }
        }
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
            productList = command.executeCommand();
        } catch (CommandFailedException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }
        if (productList == null) {
            productList = new ArrayList<>();
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
        List<AlarmAlertProduct> prods = new ArrayList<>();
        if (productId != null) {
            productId = productId.trim().toUpperCase();
            for (AlarmAlertProduct a : currentAlarms) {
                ProductType pt = a.getProductType();
                /*
                 * Alarm_Alert
                 */
                if (AA.equals(pt)) {
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
                /*
                 * DR1555 - Proximity_Alarm
                 */
                else if (PA.equals(pt)) {
                    String s = a.getProductId();
                    if (s != null) {
                        s = s.trim().toUpperCase();
                        if (s.equals(productId)) {
                            List<StdTextProduct> productList = getProduct(
                                    a.getProductId());
                            if (!productList.isEmpty()) {
                                StdTextProduct stp = productList.get(0);
                                if (stp != null) {
                                    Geometry messagePolygon = getMessagePolygon(
                                            stp);
                                    if (a.isAor()) {
                                        /*
                                         * Check if polygon in the message is
                                         * within the AOR
                                         */
                                        if (messagePolygon != null
                                                && matchAOR(messagePolygon)) {
                                            prods.add(a);
                                        }
                                    } else if (!"".equals(a.getAorDistance())) {
                                        /*
                                         * Check if polygon in the message is
                                         * within the AOR+distance
                                         */
                                        if (messagePolygon != null
                                                && matchAORExtention(
                                                        a.getAorDistance(),
                                                        a.getAorLabel(),
                                                        messagePolygon)) {
                                            prods.add(a);
                                        }
                                    } else if (!"".equals(a.getUgcList())) {
                                        /*
                                         * Check if UGCs in the message match
                                         * the UGCs in the alarm
                                         */
                                        String messageUGCs = getMessageUGCs(
                                                stp.getProduct());
                                        String alarmUGCs = a.getUgcList();
                                        if (matchUGCList(alarmUGCs,
                                                messageUGCs)) {
                                            prods.add(a);
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        return prods;
    }

    /**
     * Return a String containing UGCs specified in the message
     *
     * @param productText
     * @return
     */
    private static String getMessageUGCs(String productText) {
        StringBuilder ugcLine = new StringBuilder();
        for (String line : productText.replaceAll("\r", "").trim()
                .split("\n")) {
            Matcher m = UGC.matcher(line);
            if (m.find()) {
                ugcLine.append(line);
                continue;
            } else if (ugcLine.length() > 0) {
                break;
            }
        }
        return ugcLine.toString();
    }

    /** @return true if the polygon intersects the CWA */
    private static boolean matchAOR(Geometry polygon) {
        Geometry cwa = null;
        String site = LocalizationManager.getInstance().getCurrentSite();
        try {
            cwa = readCountyWarningArea(site);
        } catch (SpatialException e) {
            statusHandler.warn("Failed to get the CWA for site " + site, e);
        }
        if (cwa != null) {
            if (polygon.intersects(cwa)) {
                return true;
            }
        }
        return false;
    }

    /**
     * Returns true if a UGC specified in the alarmUGCs is present in the
     * messageUGCs
     *
     * @param alarmUGCs
     * @param messageUGCs
     * @return
     */
    private static boolean matchUGCList(String alarmUGCs, String messageUGCs) {
        List<String> alarmUGCList = getUGCs(alarmUGCs);
        for (String alarmUGC : alarmUGCList) {
            if (messageUGCs.contains(alarmUGC)) {
                return true;
            }
        }
        return false;
    }

    /**
     * Return a List of strings of UGCs
     *
     * @param ugcString
     * @return
     */
    private static List<String> getUGCs(String ugcString) {
        String[] ugcList = ugcString.split(HYPHEN);
        // Process the list of UGC lines into a list of UGCs in full form
        // matching edit area names
        List<String> finalUGCList = new ArrayList<>(ugcList.length);
        String state = null;
        for (String ugc : ugcList) {
            Matcher newGroup = UGC_NEW_PATTERN.matcher(ugc);
            if (newGroup.matches()) {
                state = newGroup.group(2);
                finalUGCList.add(newGroup.group(1));
            } else {
                Matcher followGroup = UGC_FOLLOW_PATTERN.matcher(ugc);
                if (followGroup.matches()) {
                    finalUGCList.add(state + followGroup.group(1));
                }
            }
        }
        return finalUGCList;
    }

    /**
     * Return Geometry representing the site's CWA
     *
     * @param site
     * @return
     * @throws SpatialException
     */
    private static Geometry readCountyWarningArea(String site)
            throws SpatialException {
        Map<String, RequestConstraint> map = new HashMap<>();
        map.put("cwa", new RequestConstraint(site));
        SpatialQueryResult[] result = SpatialQueryFactory.create().query("cwa",
                null, null, map, null);
        if (result == null || result.length == 0) {
            return null;
        }
        return result[0].geometry;
    }

    /**
     * Returns true if the polygon intersects the CWA+distance
     *
     * @param distanceStr
     * @param distanceUnits
     * @param polygon
     * @return
     */
    private static boolean matchAORExtention(String distanceStr,
            String distanceUnits, Geometry polygon) {
        Geometry cwa = null;
        String site = LocalizationManager.getInstance().getCurrentSite();
        try {
            cwa = readCountyWarningArea(site);
        } catch (SpatialException e) {
            statusHandler.error("Failed to get the CWA for site " + site, e);
            return false;
        }
        Geometry CWAConvex = cwa.convexHull();

        double d0 = 0.0d;
        if ("mi".equalsIgnoreCase(distanceUnits)) {
            d0 = ONE_DEGREE_MI;
        } else {
            d0 = ONE_DEGREE_KM;
        }
        Double distance = Double.parseDouble(distanceStr);
        double centerLat = Math.toRadians(CWAConvex.getCentroid().getY());
        Double deltaX = distance / (Math.cos(centerLat) * d0);
        Double deltaY = distance / d0;
        Geometry expandedCWA = expandCWABy(CWAConvex, deltaX, deltaY);

        if (expandedCWA != null) {
            if (polygon.intersects(expandedCWA)) {
                return true;
            }
        }
        return false;
    }

    /**
     * Return expanded by deltaX-deltaY geometry
     *
     * @param CWAConvex
     * @param deltaX
     * @param deltaY
     * @return
     */
    private static Geometry expandCWABy(Geometry CWAConvex, Double deltaX,
            Double deltaY) {
        Coordinate[] coords = CWAConvex.getCoordinates();
        Coordinate[] coordsExpanded = new Coordinate[coords.length];
        Double centerLat = CWAConvex.getCentroid().getY();
        Double centerLon = CWAConvex.getCentroid().getX();
        for (int i = 0; i < coords.length; i++) {
            double latE = coords[i].y;
            double lonE = coords[i].x;
            if (coords[i].x < centerLon) {
                lonE = coords[i].x - deltaX;
            } else if (coords[i].x > centerLon) {
                lonE = coords[i].x + deltaX;
            } else if (coords[i].x == centerLon) {
                lonE = coords[i].x;
            }
            if (coords[i].y < centerLat) {
                latE = coords[i].y - deltaY;
            } else if (coords[i].y > centerLat) {
                latE = coords[i].y + deltaY;
            } else if (coords[i].y == centerLat) {
                latE = coords[i].y;
            }
            coordsExpanded[i] = new Coordinate(lonE, latE);
        }
        GeometryFactory gf = new GeometryFactory();
        return gf.createLinearRing(coordsExpanded).convexHull();
    }

    /**
     * Return the polygon contained in message
     *
     * @param stp
     * @return
     */
    private static Geometry getMessagePolygon(StdTextProduct stp) {
        String body = stp.getProduct();
        if (body.contains("LAT...LON")) {
            Coordinate[] coords = getLatLonCoords(body);
            if (coords.length == 0) {
                return null;
            }
            GeometryFactory gf = new GeometryFactory();
            return gf.createLinearRing(coords);
        }
        return null;
    }

    /**
     * Return an array of Coordinate[] contained in the message
     *
     * @param body
     * @return
     */
    private static Coordinate[] getLatLonCoords(String body) {
        StringBuilder latLon = new StringBuilder();
        boolean insideLatLon = false;
        List<Coordinate> coordinates = new ArrayList<>();
        String[] separatedLines = body.split("\n");
        for (String line : separatedLines) {
            Matcher m = LAT_LON_PATTERN.matcher(line);
            if (m.find()) {
                latLon = new StringBuilder(line);
                insideLatLon = true;
                continue;
            }
            if (insideLatLon) {
                m = SUB_LAT_LON_PATTERN.matcher(line);
                if (!line.startsWith("TIME...") && m.find()) {
                    latLon.append(" ").append(line.trim());
                    continue;
                } else {
                    insideLatLon = false;
                }
            }
        }
        coordinates = processLatlons(latLon.toString());
        Coordinate[] coords = new Coordinate[coordinates.size()];
        coords = coordinates.toArray(coords);
        return coords;
    }

    /**
     * Process the extracted from the message latlon coordinates
     *
     * @param latLon
     * @return
     */
    private static ArrayList<Coordinate> processLatlons(String latLon) {
        ArrayList<Coordinate> coordinates = new ArrayList<>();
        String currentToken = null;
        String latlon = "LAT...LON";
        String latitude = null;
        String longitude = null;
        boolean pair = false;
        Double dlat, dlong;
        StringTokenizer latlonTokens = new StringTokenizer(latLon);
        if (latLon.length() == 0) {
            return coordinates;
        }
        while (latlonTokens.hasMoreTokens()) {
            currentToken = latlonTokens.nextToken();
            if (!currentToken.equals(latlon)) {
                if (pair) {
                    longitude = currentToken;

                    pair = false;

                    dlat = (double) (Integer.parseInt(latitude) / 100.0);
                    dlong = (double) (Integer.parseInt(longitude) / 100.0
                            * -1.0);
                    coordinates.add(new Coordinate(dlong, dlat));
                } else {
                    latitude = currentToken;
                    pair = true;
                }
            }
        }
        Double dlong0 = coordinates.get(0).x;
        Double dlat0 = coordinates.get(0).y;
        coordinates.add(new Coordinate(dlong0, dlat0));
        return coordinates;
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
    public static LocalizationContext initLocalization(
            LocalizationLevel level) {
        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationContext localization = pm
                .getContext(LocalizationType.COMMON_STATIC, level);
        return localization;
    }

    public static LocalizationFile getFile(LocalizationContext lc,
            String name) {
        LocalizationFile loc = PathManagerFactory.getPathManager()
                .getLocalizationFile(lc, ALARM_ALERT_PATH + name);
        return loc;
    }

    /*
     * Try to load the site file.
     */
    public static AAPACombined loadSiteAlarms(ILocalizationFileObserver listener) {
        AAPACombined aapaCombined = null;

        LocalizationFile siteFile = getFile(initSiteLocalization(), SITE_FILE);
        if (siteFile == null) {
            aapaCombined = createDefaultAAPACombined();
            try {
                aapaCombined = loadFile(siteFile.getFile());
            } catch (FileNotFoundException e) {
                aapaCombined = createDefaultAAPACombined();
            }
        }

        return aapaCombined;
    }

    public static AAPACombined loadFile(File file)
            throws FileNotFoundException {
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
            } catch (LocalizationException e) {
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
                    try (FileReader r = new FileReader(lFile.getFile());
                            BufferedReader reader = new BufferedReader(r)) {
                        filename = reader.readLine();
                    }

                } else if (lFile != null) {
                    lFile.getFile().createNewFile();
                    try {
                        lFile.save();
                    } catch (LocalizationException e) {
                        statusHandler.handle(Priority.SIGNIFICANT,
                                e.getLocalizedMessage(), e);
                    }
                    filename = null;
                } else {
                    filename = null;
                }

            } catch (IOException e) {
                statusHandler.handle(Priority.CRITICAL, e.getLocalizedMessage(),
                        e);
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
                    try (FileWriter w = new FileWriter(cfgFile)) {
                        w.write(fname);
                    }
                    try {
                        lFile.save();
                    } catch (LocalizationException e) {
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
        List<AlarmAlertProduct> alarms = new ArrayList<>();
        AAPACombined combined = loadUserAlarmsFromConfig();
        alarms.addAll(combined.getAaList());
        alarms.addAll(combined.getPaList());
        return alarms;
    }

    /**
     * @param parent
     *            parent shell for alarm alert bell
     */
    public static synchronized void initAlarmAlertBell(Shell parent) {
        if (alarmAlertBell == null) {
            alarmAlertBell = new AlarmAlertBell(parent);
        }
    }

    /**
     * Close the alarm alert bell
     */
    public static synchronized void closeAlarmAlertBell() {
        if (alarmAlertBell != null) {
            alarmAlertBell.close();
        }
    }

    /**
     * Close alarm alert bell and set to null, so that a new one must be
     * instantiated next time
     */
    public static synchronized void destroyAlarmAlertBell() {
        if (alarmAlertBell != null) {
            alarmAlertBell.close();
            alarmAlertBell = null;
        }
    }
}
