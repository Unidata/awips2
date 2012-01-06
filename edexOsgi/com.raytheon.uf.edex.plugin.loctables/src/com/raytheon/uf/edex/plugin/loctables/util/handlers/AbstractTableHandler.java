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
package com.raytheon.uf.edex.plugin.loctables.util.handlers;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.PrintStream;
import java.util.Calendar;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.uf.edex.decodertools.time.TimeTools;
import com.raytheon.uf.edex.plugin.loctables.util.TableHandler;
import com.raytheon.uf.edex.plugin.loctables.util.store.ObStationRow;
import com.raytheon.uf.edex.plugin.loctables.util.store.RowStoreStrategy;

/**
 * TODO Add Description
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 16, 2010            jkorman     Initial creation
 *
 * </pre>
 *
 * @author jkorman
 * @version 1.0	
 */

public abstract class AbstractTableHandler implements TableHandler {

    public static final int STA_NORMAL = 0;
    
    public static final int ERR_RES_DISPOSED = -100;

    public static final String DIRECTIVE_STATUS_DIR = "#!!STATUS_DIR=";
    
    private static Pattern LATLON = Pattern.compile("(\\d{1,3})(( +\\d{2})( +\\d{2})?)?([NESW])");
    
    public static final String COMMENT = "#";
    
    public static final String DIRECTIVE = "#!!";
    
    Log logger = LogFactory.getLog(getClass());
    
    private int status = STA_NORMAL;
    
    private int errorPos = -1;

    private String statusMessage = null;
    
    private RowStoreStrategy storeStrategy;

    private PrintStream statusFile = null;
    
    private File processFile = null;
    
    final String handlerName;
    
    /**
     * 
     * @param name
     * @param storeStrategy
     */
    AbstractTableHandler (String name, RowStoreStrategy storeStrategy) {
        handlerName = name;
        this.storeStrategy = storeStrategy;
        if(storeStrategy != null) {
            storeStrategy.setParent(this);
        }
    }
    
    /**
     * 
     * @param file
     */
    @Override
    public void processFile(File file) {
        
        if(file != null) {
            logger.info(handlerName + "Handler [" + file.getName() + "]");

            BufferedReader reader = null;
            try {
                processFile = file;
                reader = new BufferedReader(new FileReader(file));
                String line = null;
                while((line = reader.readLine()) != null) {
                    clearStatus();
                    try {
                        // If a directive was found a null reference is returned.
                        if(findDirective(line) != null) {
                            ObStationRow row = parseLine(line);
                            if(!processObStationRow(row)) {
                                String msg = null;
                                if(statusMessage != null) {
                                    if(errorPos >= 0) {
                                        msg = statusMessage + " at position " + errorPos;
                                    } else {
                                        msg = statusMessage;
                                    }
                                } else {
                                    msg = "Error processing [" + line + "]";
                                }
                                writeStatus(msg);
                            } else {
                                if(statusMessage != null) {
                                    writeStatus(statusMessage);
                                }
                            }
                        }
                    } catch(Exception e) {
                        writeStatus("Error processing [" + line + "]", e);
                    }
                }
            } catch(IOException ioe) {
                logger.error("Error processing " + handlerName + " data", ioe);
            } finally {
                if(reader != null) {
                    try {
                        reader.close();
                    } catch(IOException ioe) {
                        logger.error("Error closing " + handlerName + " file",ioe);
                    }
                }
                if(statusFile != null) {
                    statusFile.close();
                    if(statusFile.checkError()) {
                        logger.error("Error closing status file");
                    }
                }
            }
        } else {
            logger.error("Cannot process null file reference.");
        }
    }

    /**
     * Determine if the specified data is either a directive or comment
     * line. Directive data is passed to a specified directive strategy.
     * Directive and Comment lines are set to null and returned. 
     * @param data A potential directive or comment line.
     * @return The original data if not a directive or comment, null otherwise.
     */
    public String findDirective(String data) {
        if(data != null) {
            if(data.startsWith(DIRECTIVE)) {
                handleDirective(data);
                data = null;
            } else if(data.startsWith(COMMENT)) {
                data = null;
            } else if(data.length() == 0) {
                data = null;
            }
        }
        return data;
    }

    /**
     * Handle any directives
     * @param data A line of data containing a directive. 
     */
    @Override
    public void handleDirective(String data) {
        if(data != null) {
            if(data.startsWith(DIRECTIVE_STATUS_DIR)) {
                
                String fs = data.substring(DIRECTIVE_STATUS_DIR.length()).trim();
                
                Calendar c = TimeTools.getSystemCalendar();
                fs = String.format("%s.%2$tY%<te%<td%<tH%<tM%<tS.jnl", fs, c);
                try {
                    statusFile = new PrintStream(fs);
                } catch(IOException ioe) {
                    logger.error("Could not create statusFile " + fs);
                    statusFile = null;
                }
            }
        }
    }
    
    /**
     * 
     * @param row
     * @return
     */
    public boolean processObStationRow(ObStationRow row) {
        boolean success = false;
        if(storeStrategy != null) {
            success = storeStrategy.store(row);
        }
        return success;
    }

    /**
     * Set a status to this handler.
     * @param status Current status.
     */
    public void setStatus(Integer status) {
        this.status = status;
    }
    
    
    /**
     * Set the position of the last error encountered.
     * @param pos Position of the last error.
     */
    public void setErrorPos(Integer pos) {
        errorPos = pos;
    }

    /**
     * Set the error message for the last error encountered.
     * @param errorMsg The error message to be displayed.
     */
    public void setStatusMsg(String statusMsg) {
        statusMessage = statusMsg;
    }
    
    /**
     * Convert a latitude or longitude value in degrees, minutes, seconds (EWNS)
     * to a double value.
     * @param value
     * @return
     */
    public final Double cvtLatLon(String value) {
        Double latlon = null;
        if(value != null) {
            Matcher m = LATLON.matcher(value);
            if(m.find()) {
                double lalo = -9999;
                String s = m.group(1);
                lalo = Double.parseDouble(s);
                 s = m.group(3);
                 if(s != null) {
                     double mm = Double.parseDouble(s);
                     lalo += (mm / 60);
                     s = m.group(4);
                     if(s != null) {
                         mm = Double.parseDouble(s);
                         lalo += (mm / 3600);
                     }
                 }
                 s = m.group(5);
                 if("N".equals(s)) {
                     latlon = lalo;
                 } else if("E".equals(s)) {
                     latlon = lalo;
                 } else if("S".equals(s)) {
                     latlon = lalo * -1;
                 } else if("W".equals(s)) {
                     latlon = lalo * -1;
                 }
            }
        }
        return latlon;
    }
    
    /**
     * 
     * @param value
     * @return
     */
    public static final Integer getInt(String value) {
        Integer retValue = null;
        try {
            retValue = new Integer(value);
        } catch(NumberFormatException nfe) {
            // Nothing - return null
        }
        return retValue;
    }

    /**
     * 
     * @param value
     * @param defaultValue
     * @return
     */
    public static final Integer getInt(String value, Integer defaultValue) {
        Integer retValue = getInt(value);
        if(retValue == null) {
            retValue = defaultValue;
        }
        return retValue;
    }

    /**
     * 
     * @param value
     * @return
     */
    public static final Double getDouble(String value) {
        Double retValue = null;
        try {
            retValue = new Double(value);
        } catch(NumberFormatException nfe) {
            // Nothing - return null
        }
        return retValue;
    }
    
    /**
     * 
     * @param value
     * @return
     */
    public static final Double getDouble(String value, Double defaultValue) {
        Double retValue = getDouble(value);
        if(retValue == null) {
            retValue = defaultValue;
        }
        return retValue;
    }

    /**
     * Write a status message to the status file only if the status file
     * has been opened.
     * @param message A status message to write.
     */
    private void writeStatus(String message) {
        if(status < STA_NORMAL) {
            logger.error(message);
        } else {
            logger.info(message);
        }
        if(statusFile != null) {
            statusFile.println(message);
        }
    }

    /**
     * Write a status message to the status file only if the status file
     * has been opened.
     * @param message A status message to write.
     */
    private void writeStatus(String message, Throwable t) {
        if(status < STA_NORMAL) {
            logger.error(message, t);
        } else {
            logger.info(message, t);
        }
        if(statusFile != null) {
            statusFile.println(message + " " + t);
        }
    }
    
    /**
     * Reset the status to normal.
     */
    public void clearStatus() {
        status = STA_NORMAL;
        statusMessage = null;
        errorPos = -1;
    }

}
