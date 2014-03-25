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
package com.raytheon.uf.edex.plugin.cwa.util;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Loads symbols from svg into IImage
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 02, 2010             jsanchez    Initial creation
 * Mar 26, 2014             skorolev    Updated logger.
 * 
 * </pre>
 * 
 * @author jsanchez
 * @version 1.0
 */
public class TableLoader {

    private static HashMap<String, Coordinate> pirepTable = new HashMap<String, Coordinate>();

    /** The logger */
    private IUFStatusHandler logger = UFStatus.getHandler(TableLoader.class);

    private static final String KEY = "\\w{3,3}";

    private static final String LAT = "\\d{4,4}";

    private static final String LON = "(-\\d{4,5})";

    public TableLoader() {
        BufferedReader input = null;
        File file = PathManagerFactory.getPathManager().getStaticFile(
                "cwa" + File.separator + "pirepsTable.txt");
        int count = 0;
        String key = null;
        float latitude = 0;
        float longitude = 0;
        Matcher m;
        Pattern p1 = Pattern.compile(KEY);
        Pattern p2 = Pattern.compile(LAT);
        Pattern p3 = Pattern.compile(LON);
        try {
            input = new BufferedReader(new FileReader(file));
            String line = null;
            while ((line = input.readLine()) != null) {
                try {
                    m = p1.matcher(line);
                    if (m.find()) {
                        key = m.group();
                        count++;
                    }

                    m = p2.matcher(line);
                    if (m.find()) {
                        latitude = Float.parseFloat(m.group()) / 100;
                        count++;
                    }

                    m = p3.matcher(line);
                    if (m.find()) {
                        longitude = Float.parseFloat(m.group()) / 100;
                        count++;
                    }

                    if (count == 3) {
                        pirepTable
                                .put(key, new Coordinate(longitude, latitude));
                    }
                } catch (NumberFormatException ex) {

                } finally {
                    count = 0;
                }
            }
            input.close();
        } catch (FileNotFoundException ex) {
            logger.error("Failed to load pirepsTable.txt");
            ex.printStackTrace();
        } catch (IOException ex) {
            logger.error("Failed to load pirepsTable.txt");
            ex.printStackTrace();
        }
    }

    /**
     * @param key
     * @return
     */
    public Coordinate get(String key) {
        return pirepTable.get(key);
    }

    /**
     * @param key
     * @return
     */
    public boolean contains(String key) {
        return pirepTable.containsKey(key);
    }
}
