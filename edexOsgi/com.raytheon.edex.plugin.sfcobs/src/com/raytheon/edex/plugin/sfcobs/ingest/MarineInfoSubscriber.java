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
package com.raytheon.edex.plugin.sfcobs.ingest;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.database.tasks.SqlStatementTask;
import com.raytheon.uf.edex.ndm.ingest.INationalDatasetSubscriber;

/**
 * Marine information NDM subscriber.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 11, 2011            bfarmer     Initial creation
 * Mar 06, 2014   2876     mpduff      New NDM plugin.
 * 
 * </pre>
 * 
 * @author bfarmer
 * @version 1.0
 */

public class MarineInfoSubscriber implements INationalDatasetSubscriber {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(MarineInfoSubscriber.class);

    private String pgPort = null;

    private static String DBSCHEMA = "mapdata";

    private static String DBTABLE = "marinesites";

    private String awipsHome = "/awips2";

    private String edexHome = "/awips2/edex";

    /*
     * " DELETE FROM public.geometry_columns WHERE f_table_schema = '${SCHEMA}'
     * AND f_table_name = '${TABLE}';
     * 
     * DROP TABLE IF EXISTS ${SCHEMA}.${TABLE};
     * 
     * CREATE TABLE "${SCHEMA}"."${TABLE}" (gid serial PRIMARY KEY, "st"
     * character varying(3), "name" character varying(50), "prog_disc" bigint,
     * "warngenlev" character varying(14));
     * 
     * DELETE from ${SCHEMA}.map_version WHERE table_name='${TABLE}';
     * 
     * INSERT INTO ${SCHEMA}.map_version (table_name, filename) values
     * ('${TABLE}','${FILENAME}');
     * 
     * SELECT
     * AddGeometryColumn('${SCHEMA}','${TABLE}','the_geom','4326','POINT',2);
     * 
     * CREATE INDEX "${TABLE}_the_geom_gist" ON "${SCHEMA}"."${TABLE}" USING
     * gist (the_geom); "
     */
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
                    LocalizationType.EDEX_STATIC, LocalizationLevel.BASE);
            File outFile = pathMgr.getFile(lc, "infofiles/MarineInfo.txt");
            if (!outFile.exists()) {
                try {
                    outFile.createNewFile();
                } catch (IOException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Could not create MarineInfo file. ", e);
                }
            }
            saveFile(file, outFile);
            triggerDBReload(outFile);
        }
    }

    private void triggerDBReload(File outFile) {
        if ((outFile != null) && outFile.exists()) {
            BufferedReader fis = null;
            try {
                fis = new BufferedReader(new InputStreamReader(
                        new FileInputStream(outFile)));
                try {
                    SqlStatementTask task = new SqlStatementTask(setupOne,
                            "maps");
                    task.execute();
                    task = new SqlStatementTask(setupTwo, "maps");
                    task.execute();
                    task = new SqlStatementTask(setupThree, "maps");
                    task.execute();
                    task = new SqlStatementTask(setupFour, "maps");
                    task.execute();
                    task = new SqlStatementTask(setupFive, "maps");
                    task.execute();
                    task = new SqlStatementTask(setupSix, "maps");
                    task.execute();
                    task = new SqlStatementTask(setupSeven, "maps");
                    task.execute();
                } catch (Exception e) {
                    statusHandler.handle(Priority.CRITICAL,
                            "Error resetting the MarineInfo DB table, ", e);
                    return;
                }
                String line = null;
                String[] splitOne = null;
                String[] splitTwo = null;
                StringBuilder query = null;
                try {
                    while ((line = fis.readLine()) != null) {
                        splitOne = line.split("\\s+", 5);
                        if (splitOne.length == 5) {
                            splitTwo = splitOne[4].split("\\|");
                            splitTwo[0] = splitTwo[0].replaceAll("'", "''");
                        } else {
                            return;
                        }
                        // "INSERT INTO" + DBSCHEMA + "." + DBTABLE
                        // "(st, name, prog_disc, warngenlev,the_geom) "
                        // "VALUES('3','4',2,5,GeomFromText('POINT(1, 0)', 4326));"
                        query = new StringBuilder("INSERT INTO \"");
                        query.append(DBSCHEMA);
                        query.append("\".\"");
                        query.append(DBTABLE);
                        query.append("\"(st, name, prog_disc, warngenlev, the_geom) VALUES('");
                        query.append(splitOne[3]); // st
                        query.append("', '");
                        query.append(splitTwo[0]); // name
                        query.append("', ");
                        query.append(splitOne[2]); // prog_disc
                        query.append(", ");
                        query.append(splitTwo[1]); // warngenlev
                        query.append(", ");
                        query.append("GeomFromText('POINT(");
                        query.append(splitOne[1]); // the_geom 1
                        query.append(" ");
                        query.append(splitOne[0]); // the_geom 2
                        query.append(")', 4326));"); // End query
                        SqlStatementTask task = new SqlStatementTask(
                                query.toString(), "maps");
                        task.execute();
                    }
                } catch (IOException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Could not read file: " + outFile, e);

                } catch (Exception e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Could not insert record into mapdata.marineinfo table"
                                    + query.toString(), e);
                }
            } catch (FileNotFoundException e) {
                statusHandler.handle(Priority.PROBLEM, "Failed to find file: "
                        + outFile, e);

            } finally {
                if (fis != null) {
                    try {
                        fis.close();
                    } catch (IOException e) {
                        // ignore
                    }
                }
            }
        }
    }

    private void saveFile(File file, File outFile) {
        if ((file != null) && file.exists()) {
            BufferedReader fis = null;
            BufferedWriter fos = null;
            try {
                fis = new BufferedReader(new InputStreamReader(
                        new FileInputStream(file)));
                fos = new BufferedWriter(new OutputStreamWriter(
                        new FileOutputStream(outFile)));
                String line = null;
                try {
                    while ((line = fis.readLine()) != null) {
                        fos.write(line);
                        fos.newLine();
                    }
                    fos.close();
                } catch (IOException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Could not read file: " + file.getName(), e);

                }
            } catch (FileNotFoundException e) {
                statusHandler.handle(Priority.PROBLEM, "Failed to find file: "
                        + file.getName(), e);
            } finally {
                if (fis != null) {
                    try {
                        fis.close();
                    } catch (IOException e) {
                        // ignore
                    }
                }
                if (fos != null) {
                    try {
                        fos.close();
                    } catch (IOException e) {
                        // ignore
                    }
                }
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
