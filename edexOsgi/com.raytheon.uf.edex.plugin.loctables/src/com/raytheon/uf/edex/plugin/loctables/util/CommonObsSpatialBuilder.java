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
package com.raytheon.uf.edex.plugin.loctables.util;

import static com.raytheon.uf.common.localization.LocalizationContext.LocalizationType.EDEX_STATIC;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.edex.plugin.loctables.ingest.LocationTablesIngest;
import com.raytheon.uf.edex.plugin.loctables.util.handlers.AbstractTableHandler;
import com.raytheon.uf.edex.plugin.loctables.util.store.ObStationRow;


/**
 * TODO Add Description
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 19, 2010            jkorman     Initial creation
 *
 * </pre>
 *
 * @author jkorman
 * @version 1.0	
 */

public class CommonObsSpatialBuilder implements TableHandler {

    private Log logger = LogFactory.getLog(getClass());

    private static final String TABLES_DIR = "spatialTables";

    private static final String COS_DIRECTIVE = "COMMON_OBS_SPATIAL";
    
    private static final String COMMON_OBS_SPATIAL = AbstractTableHandler.DIRECTIVE + COS_DIRECTIVE;

    private static final Pattern P_DIRECTIVE = Pattern.compile("^" + COMMON_OBS_SPATIAL + "$");

    private static final String [] COMMON_TABLES = {
 "maritimeStationInfo.txt",
            "metarStationInfo.txt", "pirepsTable.txt",
            "synopticStationInfo.txt", "raobStationInfo.txt",
            "mesonetStationInfo.txt"
    };
    
    private LocationTablesIngest ingest = null;
    
    public CommonObsSpatialBuilder(LocationTablesIngest ingestor) {
        ingest = ingestor;
    }
    
    
    /**
     *
     */
    @Override
    public String findDirective(String data) {
        String directive = null;
        
        Matcher m = P_DIRECTIVE.matcher(data);
        if(m.matches()) {
            directive = COS_DIRECTIVE;
        }
        return directive;
    }

    /**
     *
     */
    @Override
    public void handleDirective(String directive) {
        if(COS_DIRECTIVE.equals(directive)) {
            processCommonObsSpatial();
        }
    }

    /**
     *
     */
    @Override
    public void processFile(File file) {
        logger.info("Creating new CommonObsSpatial files");
        
        BufferedReader reader = null;
        try {
            reader = new BufferedReader(new FileReader(file));
            String line = null;
            while((line = reader.readLine()) != null) {
                handleDirective(findDirective(line));
            }
        } catch(IOException ioe) {
            logger.error("Error processing data", ioe);
        } finally {
            if(reader != null) {
                try {
                    reader.close();
                } catch(IOException ioe) {
                    logger.error("Error closing file",ioe);
                }
            }
        }
    }

    /**
     * No implementation in the class.
     */
   @Override
   public ObStationRow parseLine(String data) {
       return null;
   }

    /**
     * No implementation in the class.
     */
    @Override
    public boolean processObStationRow(ObStationRow row) {
        return false;
    }

    @Override
    public void setStatus(Integer status) {
    }

    @Override
    public void setErrorPos(Integer pos) {
    }

    @Override
    public void setStatusMsg(String errMsg) {
    }

    private void processCommonObsSpatial() {

        try {
            File fileDir = null;

            IPathManager manager = PathManagerFactory.getPathManager();
            if (manager != null) {
                LocalizationContext context = manager.getContext(EDEX_STATIC,
                        LocalizationLevel.BASE);
                if (context != null) {
                    fileDir = manager.getFile(context, TABLES_DIR);
                    if (fileDir.exists()) {
                        for(String table : COMMON_TABLES) {
                            processTable(fileDir,table);
                        }
                    } else {
                        logger.error("");
                    }
                } else {
                    logger.error(String.format(" "));
                }
            } else {
                // Could not create PathManager
            }
        } catch (Exception e) {
            logger.error(" ", e);
        }
    }
    
    private void processTable(File fileDir, String table) {
        BufferedReader reader = null;

        TableHandler tblStrategy = ingest.getHandlers().get(table);
        if (tblStrategy != null) {
            try {
                File file = new File(fileDir, table);
                if (file.exists()) {
                    reader = new BufferedReader(new FileReader(file));
                    String line = null;
                    while ((line = reader.readLine()) != null) {
                        ObStationRow row = tblStrategy.parseLine(line);
                        if(row != null) {
                            System.out.println(row.toSQLInsertString());
                        }
                    }
                } else {
                    logger.error("File " + table + " not processed");
                }
            } catch (Exception e) {
                logger.error("Error processing file " + table, e);
            } finally {
                if (reader != null) {
                    try {
                        reader.close();
                    } catch (IOException ioe) {
                        logger.error(" ", ioe);
                    }
                }
            }

        } else {
            logger.error("No table handler strategy for " + table + " was found");
        }
    }
    
    public static final void main(String [] args) {
        
        String data = COMMON_OBS_SPATIAL;
        
        Matcher m = P_DIRECTIVE.matcher(data);
        if(m.matches()) {
            System.out.println("Found directive");
        }
        
    }
}
