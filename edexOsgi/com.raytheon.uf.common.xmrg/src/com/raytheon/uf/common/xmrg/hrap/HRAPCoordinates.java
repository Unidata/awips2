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
package com.raytheon.uf.common.xmrg.hrap;

import java.awt.Rectangle;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;

import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.ohd.AppsDefaultsDirKeys;
import com.raytheon.uf.common.util.FileUtil;

/**
 * Returns HRAP Coordinates
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 18, 2014  17067      snaples     Added trim to readline to prevent parsing issues.
 * Jan 26, 2016  5264       bkowal      Use the apps defaults dir constant.
 * Jul 18, 2016  4600       njensen     Improved exception handling
 * 
 * </pre>
 * 
 * @author snaples
 */

public class HRAPCoordinates {

    public static Rectangle getHRAPCoordinates() throws FileNotFoundException,
            IOException, NumberFormatException {
        // read the coord file
        AppsDefaults appsDefaults = AppsDefaults.getInstance();

        String geoDataDir = appsDefaults.getToken(AppsDefaultsDirKeys.GEO_DATA);
        String officeId = appsDefaults.getToken("st3_rfc");
        String coordFilename = FileUtil.join(geoDataDir, officeId, "ascii",
                "coord_" + officeId + ".dat");

        File coordFile = new File(coordFilename);
        int Hrap_XOR = 0;
        int Hrap_YOR = 0;
        int Hrap_MAXX = 0;
        int Hrap_MAXY = 0;

        String line = null;
        try (BufferedReader in = new BufferedReader(new FileReader(coordFile))) {
            try {
                line = in.readLine().trim();
                Hrap_XOR = Integer.parseInt(line);
            } catch (NumberFormatException e) {
                throw new NumberFormatException("Invalid integer " + line
                        + " for HRAP origin x in coordinate file: "
                        + coordFile.getPath());
            }
            try {
                line = in.readLine().trim();
                Hrap_YOR = Integer.parseInt(line);
            } catch (NumberFormatException e) {
                throw new NumberFormatException("Invalid integer " + line
                        + " for HRAP origin y in coordinate file: "
                        + coordFile.getPath());
            }
            try {
                line = in.readLine().trim();
                Hrap_MAXX = Integer.parseInt(line);
            } catch (NumberFormatException e) {
                throw new NumberFormatException("Invalid integer " + line
                        + " for HRAP max x in coordinate file: "
                        + coordFile.getPath());
            }
            try {
                line = in.readLine().trim();
                Hrap_MAXY = Integer.parseInt(line);
            } catch (NumberFormatException e) {
                throw new NumberFormatException("Invalid integer " + line
                        + " for HRAP max y in coordinate file: "
                        + coordFile.getPath());
            }
        }

        Rectangle HRAPExtent = new Rectangle(Hrap_XOR, Hrap_YOR, Hrap_MAXX,
                Hrap_MAXY);

        return HRAPExtent;
    }
}
