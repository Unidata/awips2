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
package com.raytheon.uf.common.hydro.spatial;

import java.awt.Rectangle;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;

import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.util.FileUtil;

public class HRAPCoordinates {
    public static Rectangle getHRAPCoordinates() throws Exception {
        // read the coord file
        AppsDefaults appsDefaults = AppsDefaults.getInstance();

        String geoDataDir = appsDefaults.getToken("geo_data");
        String officeId = appsDefaults.getToken("st3_rfc");
        String coordFilename = FileUtil.join(geoDataDir, officeId, "ascii",
                "coord_" + officeId + ".dat");

        File coordFile = new File(coordFilename);
        int Hrap_XOR = 0;
        int Hrap_YOR = 0;
        int Hrap_MAXX = 0;
        int Hrap_MAXY = 0;

        BufferedReader in = null;
        String line = null;
        try {
            in = new BufferedReader(new FileReader(coordFile));
            line = in.readLine();
            Hrap_XOR = Integer.parseInt(line);
            line = in.readLine();
            Hrap_YOR = Integer.parseInt(line);
            line = in.readLine();
            Hrap_MAXX = Integer.parseInt(line);
            line = in.readLine();
            Hrap_MAXY = Integer.parseInt(line);
        } catch (FileNotFoundException e) {
            throw new Exception("unable to find coordinate file: "
                    + coordFile.getAbsolutePath(), e);
        } catch (IOException e) {
            throw new Exception("error reading coordinate file: "
                    + coordFile.getAbsolutePath(), e);
        } catch (NumberFormatException e) {
            throw new Exception("invalid integer " + line
                    + " in coordinate file: " + coordFile.getAbsolutePath(), e);
        } finally {
            try {
                if (in != null) {
                    in.close();
                }
            } catch (IOException e) {
                e.printStackTrace();
            }
        }

        Rectangle HRAPExtent = new Rectangle(Hrap_XOR, Hrap_YOR, Hrap_MAXX,
                Hrap_MAXY);

        return HRAPExtent;
    }
}
