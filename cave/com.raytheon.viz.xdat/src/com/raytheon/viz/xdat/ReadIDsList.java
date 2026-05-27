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
import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;

/**
 * 
 * Class use to read the user selected group file.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 16, 2009 1883       lvenable     Initial creation
 * 10 Feb 2009             wkwocked     Add functions.
 * 13 Dec 2017  6778       mduff        Fixed reading of the groups file to handle comments and white space.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class ReadIDsList {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ReadIDsList.class);

    /**
     * Array of IDs.
     */
    private List<String[]> idsList = new ArrayList<>();

    /**
     * Constructor
     * 
     * @param groupFileName
     */
    public ReadIDsList(File groupFile) {
        if (groupFile == null) {
            statusHandler.error("Null group file provided.");
            return;
        }
        try (BufferedReader in = new BufferedReader(
                new FileReader(groupFile))) {
            String str;
            while ((str = in.readLine()) != null) {
                // Skip comment lines that start with $
                if (str.startsWith("$")) {
                    continue;
                }
                String IDandPEStr[] = str.trim().split("\\s+");
                if (IDandPEStr.length >= 2) {
                    idsList.add(IDandPEStr);
                }
            }
        } catch (IOException e) {
            statusHandler.error("Error reading " + groupFile.getAbsolutePath(),
                    e);
        }
    }

    /**
     * Get the array of IDs
     * 
     * @return ArrayList<String[]> Array of IDs.
     */
    public List<String[]> getIDsList() {
        return idsList;
    }
}
