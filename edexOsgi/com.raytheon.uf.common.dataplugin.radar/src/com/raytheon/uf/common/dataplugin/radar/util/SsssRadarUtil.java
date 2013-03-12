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
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManager;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * TODO Add Description
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 03/07/13     DR15495    zwang       Handle SSSS radars
 * 
 * </pre>
 * 
 * @author zwang
 * @version 1.0
 */

public class SsssRadarUtil {
    
	private static final transient IUFStatusHandler statusHandler = UFStatus
    .getHandler(RadarsInUseUtil.class);
	
	private static final IUFStatusHandler handler = UFStatus
    .getHandler(RadarsInUseUtil.class);
	
	private static List<String> ssssRadars;

	private static boolean parsed = false;

    private static synchronized void parseFile() throws IOException{
        PathManager pm = (PathManager) PathManagerFactory.getPathManager();
        LocalizationContext context = pm.getContext(LocalizationType.COMMON_STATIC,
                  LocalizationLevel.BASE);
       
        if (parsed) {
            return;
        }

        parsed = true;
        ssssRadars = new ArrayList<String>();
        LocalizationFile file = pm.getLocalizationFile(context, "radar"
                + File.separator + "ssssRadars.txt");

        if (!file.exists()) {
            statusHandler.info("File ssssRadars.txt not found ");
        }
        else{
            BufferedReader buf = new BufferedReader(new FileReader(
                    file.getFile()));
            String temp = buf.readLine();
            while (temp != null) {
                temp = temp.trim();
                if (temp.startsWith("#")) {
                    // Skip comment lines if any
                	break;
                } else if (!temp.trim().isEmpty()) {
                    ssssRadars.add(temp);
                }
                temp = buf.readLine();
            }
            buf.close();
        }
    }

    public static List<String> getSsssRadars() {
        if (parsed)
        	return ssssRadars;
        else {
    	    try {
    	    	parseFile();
    	    } catch (IOException e) {
    	    	handler.handle(Priority.ERROR, "Error occurred looking up radars",
    	    			e);
    	    }
        }
        return ssssRadars;
    }

    public static boolean isSsssRadar(String radar) {
    	if (!parsed) {
    		try {
    	    	parseFile();
    	    } catch (IOException e) {
    	    	handler.handle(Priority.ERROR, "Error occurred looking up radars",
    	    			e);
    	    }
    	}
    	
    	return ssssRadars.contains(radar);
    }

}
