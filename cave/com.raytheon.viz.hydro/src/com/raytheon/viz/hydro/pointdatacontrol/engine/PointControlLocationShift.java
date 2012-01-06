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
package com.raytheon.viz.hydro.pointdatacontrol.engine;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;

import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.viz.hydro.pointdatacontrol.data.PDCLocationShift;

/**
 * Handles the PDC location shift feature.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 17, 2010 2635       mpduff      Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class PointControlLocationShift {

    /**
     * Loads the shift configuration data file.
     * @return
     *      List of PDCLocationShift objects, one per shift record
     */
    public static ArrayList<PDCLocationShift> loadShiftData() {
        ArrayList<PDCLocationShift> list = new ArrayList<PDCLocationShift>();

        File cfgFile = PathManagerFactory.getPathManager().getStaticFile(
                "hydro/config/pdc_loc_shift.txt");

        try {
            BufferedReader in = new BufferedReader(new FileReader(cfgFile));
            String str;
            while ((str = in.readLine()) != null) {
                if (str.startsWith("#") || (str.length() == 0) || str.equals(" ")) {
                    continue;
                }
                
                PDCLocationShift shift = new PDCLocationShift();
                String[] parts = str.split("\\s+");
                
                shift.setLid(parts[0]);
                shift.setParamCode(parts[1]);
                shift.setXShift(Integer.parseInt(parts[2]));
                shift.setYShift(Integer.parseInt(parts[3]));
                
                list.add(shift);
            }
            in.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
        
        return list;
    }
}
