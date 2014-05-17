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
package com.raytheon.rcm.server;

import java.io.File;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;

import org.apache.log4j.DailyRollingFileAppender;

public class DailyPurgingRollingFileAppender extends DailyRollingFileAppender {
    
    private int maxFiles = -1; 

    @Override
    protected void closeFile() {
        File path = new File(getFile());
        super.closeFile();
        
        if (maxFiles < 0)
            return;
        
        try {
            String pattern = getDatePattern();
            final String logBaseName = path.getName();
            SimpleDateFormat sdf = new SimpleDateFormat(pattern);
            
            File directory = path.getParentFile();
            File[] files = directory.listFiles();
            ArrayList<Date> dates = new ArrayList<Date>(files.length);
            HashMap<Date, File> datedFiles = new HashMap<Date, File>(files.length);
            for (File f : files) {
                if (! f.isFile())
                    continue;
                String fileName = f.getName();
                if (fileName.startsWith(logBaseName)) {
                    String rest = fileName.substring(logBaseName.length());
                    Date date = null;
                    try {
                        date = sdf.parse(rest);
                    } catch (ParseException e) {
                        // not a match...
                    }
                    if (date != null) {
                        dates.add(date);
                        datedFiles.put(date, f);
                    }
                }
            }
            
            Collections.sort(dates);
            if (dates.size() > maxFiles) {
                int limit = dates.size() - maxFiles;
                for (int i = 0; i < limit; ++i)
                    datedFiles.get(dates.get(i)).delete();
            }
                        
        } catch (Exception e) {
            Log.errorf("Cannot purge logs: %s", e);
        }
    }

    public int getMaxFiles() {
        return maxFiles;
    }

    public void setMaxFiles(int maxFiles) {
        this.maxFiles = maxFiles;
    }
}
