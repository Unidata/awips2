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
package com.raytheon.uf.common.dataplugin.radar.util;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;

/**
 * TODO Add Description
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

class AngleBin {
    double primary;

    double min;

    public AngleBin(String s) {
        Scanner sr = new Scanner(s);
        primary = sr.nextDouble();
        min = sr.nextDouble();
        sr.close();
    }
}

public class TiltAngleBin {
    private static final List<AngleBin> lookUp;
    static {
        lookUp = new ArrayList<AngleBin>();
        try {
            IPathManager pathManager = PathManagerFactory.getPathManager();
            LocalizationContext context = pathManager.getContext(
                    LocalizationType.COMMON_STATIC, LocalizationLevel.BASE);
            File file = pathManager.getFile(context, "radar" + File.separator
                    + "tiltAngleGroups.txt");

            BufferedReader in = new BufferedReader(new FileReader(file));

            String s = in.readLine();
            while (s != null) {
                if (s.charAt(0) != '/') {
                    AngleBin bin = new AngleBin(s);
                    if (bin != null) {
                        lookUp.add(bin);
                    }
                }
                s = in.readLine();
            }
            in.close();
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    public static double getPrimaryElevationAngle(double elev) {
        // round to nearest tenth of a degree
        double rounded = Math.floor(elev * 10.0 + 0.5) / 10.0;

        int i = lookUp.size() - 1;
        while (i > 0 && rounded < lookUp.get(i).min) {
            i--;
        }
        return lookUp.get(i).primary;
    }

    public static void main(String[] args) {
        for (int a = -9; a <= 900; a++) {
            float elev = a * 0.1f;
            System.out.println(String.format("%1.1f: %1.1f", elev,
                    TiltAngleBin.getPrimaryElevationAngle(elev)));
        }
    }
}
