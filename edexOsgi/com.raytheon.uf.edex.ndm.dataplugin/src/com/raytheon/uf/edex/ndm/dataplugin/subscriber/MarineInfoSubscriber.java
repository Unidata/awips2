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
package com.raytheon.uf.edex.ndm.dataplugin.subscriber;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.HashMap;
import java.util.Map;

import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.SaveableOutputStream;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;
import com.raytheon.uf.edex.ndm.ingest.INationalDatasetSubscriber;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.Point;
import com.vividsolutions.jts.geom.PrecisionModel;

/**
 * Marine information NDM subscriber.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Apr 11, 2011           bfarmer   Initial creation
 * Mar 06, 2014  2876     mpduff    New NDM plugin.
 * Jul 13, 2015  4500     rjpeter   Fix SQL Injection concerns.
 * Mar 02, 2016  5434     bkowal    Relocated to ndm dataplugin.
 * Aug 01, 2016  5744     mapeters  Fix DB reloading, save file to 
 *                                  common_static.configured instead of 
 *                                  edex_static.base
 * 
 * </pre>
 * 
 * @author bfarmer
 */

public class MarineInfoSubscriber implements INationalDatasetSubscriber {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(MarineInfoSubscriber.class);

    private String pgPort = null;

    private static String DBSCHEMA = "mapdata";

    private static String DBTABLE = "marinesites";

    private String awipsHome = "/awips2";

    private String edexHome = "/awips2/edex";

    private static String setupOne = "DELETE FROM public.geometry_columns WHERE f_table_schema = 'mapdata' AND f_table_name = 'marinesites';";

    private static String setupTwo = "DROP TABLE IF EXISTS mapdata.marinesites";

    private static String setupThree = "CREATE TABLE \"mapdata\".\"marinesites\" (gid serial PRIMARY KEY, \"st\" character varying (3), \"name\" character varying(50), \"prog_disc\" bigint, \"warngenlev\" character varying(14));";

    private static String setupFour = "DELETE FROM mapdata.map_version WHERE table_name='marinesites';";

    private static String setupFive = "INSERT INTO mapdata.map_version (table_name, filename) values ('marinesites', 'MarineInfo.txt');";

    private static String setupSix = "SELECT AddGeometryColumn('mapdata', 'marinesites', 'the_geom', '4326', 'POINT', 2);";

    private static String setupSeven = "CREATE INDEX \"marinesites_the_geom_gist\" ON \"mapdata\".\"marinesites\" USING gist (the_geom);";

    public MarineInfoSubscriber(String pgPort, String edexHome) {
        super();
        setPgPort(pgPort);
        setEdexHome(edexHome);
    }

    @Override
    public void notify(String fileName, File file) {
        if ("MarineInfo.txt".equals(fileName)) {
            IPathManager pathMgr = PathManagerFactory.getPathManager();
            LocalizationContext lc = pathMgr.getContext(
                    LocalizationType.COMMON_STATIC,
                    LocalizationLevel.CONFIGURED);
            ILocalizationFile outFile = pathMgr.getLocalizationFile(lc,
                    "infofiles" + IPathManager.SEPARATOR + "marinesites"
                            + IPathManager.SEPARATOR + "MarineInfo.txt");

            saveFile(file, outFile);
            triggerDBReload(outFile);
        }
    }

    private void triggerDBReload(ILocalizationFile outFile) {
        if ((outFile != null) && outFile.exists()) {
            CoreDao dao = new CoreDao(DaoConfig.forDatabase("maps"));
            try {
                dao.executeSQLUpdate(setupOne);
                dao.executeSQLUpdate(setupTwo);
                dao.executeSQLUpdate(setupThree);
                dao.executeSQLUpdate(setupFour);
                dao.executeSQLUpdate(setupFive);
                // Can't use executeSQLUpdate() with SELECT statement
                dao.executeSQLQuery(setupSix);
                dao.executeSQLUpdate(setupSeven);
            } catch (Exception e) {
                statusHandler.handle(Priority.CRITICAL,
                        "Error resetting the MarineInfo DB table, ", e);
                return;
            }
            String line = null;
            String[] splitOne = null;
            String[] splitTwo = null;
            Map<String, Object> paramMap = new HashMap<>(8, 1);
            GeometryFactory geomFactory = new GeometryFactory(
                    new PrecisionModel(), 4326);

            StringBuilder queryBuilder = new StringBuilder();
            queryBuilder.append("INSERT INTO \"");
            queryBuilder.append(DBSCHEMA);
            queryBuilder.append("\".\"");
            queryBuilder.append(DBTABLE);
            queryBuilder
                    .append("\"(st, name, prog_disc, warngenlev, the_geom) VALUES(");
            queryBuilder.append(":st, :name, :prog_disc, :warngenlev, :geom)");
            String query = queryBuilder.toString();

            try (BufferedReader fis = new BufferedReader(new InputStreamReader(
                    outFile.openInputStream()))) {

                while ((line = fis.readLine()) != null) {
                    splitOne = line.split("\\s+", 5);
                    if (splitOne.length == 5) {
                        splitTwo = splitOne[4].split("\\|");
                        splitTwo[0] = splitTwo[0].replaceAll("'", "''");
                    } else {
                        String msg = "Terminating DB reload due to invalid "
                                + "line in " + outFile.getPath()
                                + " (expected 5 columns): " + line;
                        statusHandler.warn(msg);
                        return;
                    }

                    Long progDisc = null;
                    try {
                        progDisc = Long.valueOf(splitOne[2]);
                    } catch (NumberFormatException e) {
                        String msg = "Terminating DB reload due to invalid "
                                + "prog_disc value in " + outFile.getPath()
                                + " (expected integer): " + splitOne[2];
                        statusHandler.handle(Priority.PROBLEM, msg, e);
                        return;
                    }

                    // Build geometry from coordinate values
                    String lat = splitOne[0];
                    String lon = splitOne[1];
                    Point point = null;
                    try {
                        // Reverse lat/lon order to match Coordinate(x,y)
                        Coordinate coord = new Coordinate(Double.valueOf(lon),
                                Double.valueOf(lat));
                        point = geomFactory.createPoint(coord);
                    } catch (NumberFormatException e) {
                        String msg = "Terminating DB reload due to invalid "
                                + "coordinate values in " + outFile.getPath()
                                + ": " + lat + " " + lon;
                        statusHandler.handle(Priority.PROBLEM, msg, e);
                        return;
                    }

                    paramMap.put("st", splitOne[3]);
                    paramMap.put("name", splitTwo[0]);
                    paramMap.put("prog_disc", progDisc);
                    paramMap.put("warngenlev", splitTwo[1]);
                    paramMap.put("geom", point);
                    dao.executeSQLUpdate(query, paramMap);
                }
            } catch (IOException e) {
                statusHandler.handle(Priority.PROBLEM, "Could not read file: "
                        + outFile.getPath(), e);
            } catch (LocalizationException e) {
                statusHandler.handle(
                        Priority.PROBLEM,
                        "Error opening output stream for file: "
                                + outFile.getPath(), e);
            } catch (Exception e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Could not insert record into mapdata.marineinfo table: "
                                + query, e);
            }
        }
    }

    private void saveFile(File file, ILocalizationFile outFile) {
        if ((file != null) && file.exists()) {
            try (InputStream is = new FileInputStream(file);
                    SaveableOutputStream os = outFile.openOutputStream()) {
                byte[] buf = new byte[2048];
                int len = is.read(buf);
                while (len > 0) {
                    os.write(buf, 0, len);
                    len = is.read(buf);
                }

                os.save();
            } catch (FileNotFoundException e) {
                statusHandler.handle(Priority.PROBLEM, "Failed to find file: "
                        + file.getAbsolutePath(), e);
            } catch (IOException e) {
                statusHandler.handle(Priority.PROBLEM, "Failed to save file "
                        + file.getAbsolutePath() + " to localization file "
                        + outFile.getPath());
            } catch (LocalizationException e) {
                statusHandler.handle(
                        Priority.PROBLEM,
                        "Failed to open output stream for file: "
                                + outFile.getPath(), e);
            }
        }
    }

    /**
     * @return the pgPort
     */
    public String getPgPort() {
        return pgPort;
    }

    /**
     * @param pgPort
     *            the pgPort to set
     */
    public void setPgPort(String pgPort) {
        this.pgPort = pgPort;
    }

    /**
     * @return the awipsHome
     */
    public String getAwipsHome() {
        return awipsHome;
    }

    /**
     * @param awipsHome
     *            the awipsHome to set
     */
    public void setAwipsHome(String awipsHome) {
        this.awipsHome = awipsHome;
    }

    /**
     * @return the edexHome
     */
    public String getEdexHome() {
        return edexHome;
    }

    /**
     * @param edexHome
     *            the edexHome to set
     */
    public void setEdexHome(String edexHome) {
        this.edexHome = edexHome;
        awipsHome = edexHome.substring(0, edexHome.lastIndexOf("/"));
    }
}
