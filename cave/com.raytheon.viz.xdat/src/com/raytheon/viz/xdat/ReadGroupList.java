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

package com.raytheon.viz.xdat;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;

import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * 
 * Class use to read the groups data file.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 16, 2009 1883       lvenable     Initial creation
 * 10 Feb 2009             wkwock      Added functions.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class ReadGroupList {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ReadGroupList.class);

    /**
     * Array of groups.
     */
    private ArrayList<String> groupsList;

    /**
     * Constructor.
     */
    public ReadGroupList() {

        InputStream is = null;
        BufferedReader br = null;
        String line;
        groupsList = new ArrayList<String>();

        // Get the groups file to read in.
        String localdata_dir = AppsDefaults.getInstance()
            .getToken("xdat_groups_dir");

        File file = new File(localdata_dir + File.separatorChar + "groups");

        try {
            br = new BufferedReader(new FileReader(file));

            /*
             * The readLine call reads in the first line of the file which is
             * ignored because we don't care how many lines are in the file.
             */
            line = br.readLine();

            // Read the file and put the lines in an array.
            while (null != (line = br.readLine())) {
                groupsList.add(line);
            }
        } catch (Exception e) {
            String msg = "Error reading " + file.getAbsolutePath();
            statusHandler.handle(Priority.ERROR, msg, e);
        } finally {
            try {
                if (br != null) {
                    br.close();
                }

                if (is != null) {
                    is.close();
                }
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
    }

    /**
     * Get the array of groups.
     * 
     * @return Array of groups.
     */
    ArrayList<String> getGroups() {
        return groupsList;
    }
}
