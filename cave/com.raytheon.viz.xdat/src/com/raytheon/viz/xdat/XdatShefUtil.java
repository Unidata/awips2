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

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.Locale;
import java.util.TimeZone;

import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.RunProcess;

/**
 * XDAT Shef File manager.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 2, 2011  9210       mpduff      Initial creation.
 * July 21, 2011 9210	   lbousaidi   changed the token xdat_shefdata_dir
 * 						 			   to shefdecode_input
 * Sep 19, 2011 10955      rferrel     Use RunProcess
 * Mar 02, 2015 14538      J Wei       Shef file created by XDat will not be over written
 * Mar 04, 2015 116        J Wei       Shef file created by XDat will not be deleted
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class XdatShefUtil {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(XdatShefUtil.class);

    /** Shef message footer */
    private static final String SHEF_FOOTER = ":\n:\tEND OF MESSAGE\n:\n$$\nNNNN\n";

    // Apps Defaults tokens
    private String pil;

    private String route;

    private String hdr;

    private String userId;

    private String send;

    private String xmitCmd;

    private String shefDir;

    private String localDataDir;

    private boolean sendAction = false;

    private File shefFile;

    /**
     * Constructor.
     * 
     * @param shell
     *            The parent shell
     */
    public XdatShefUtil() {
        getAppsDefaults();
    }

    /**
     * Create the insert shef file.
     * 
     * @param id
     * @param pe
     * @param value
     * @param hour
     * @param minute
     * @param startDateStr
     */
    public void createInsertFile(String id, String pe, double value, int hour,
            int minute, String startDateStr) {
        shefFile = this.getShefFile();

        if (shefFile != null) {
            Calendar cal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
            SimpleDateFormat startDateFormat = new SimpleDateFormat("ddHHmm",
                    Locale.US);
            String timeStr = startDateFormat.format(cal.getTime());

            startDateFormat = new SimpleDateFormat("yyyy-MM-dd", Locale.US);
            try {
                Date sDate = startDateFormat.parse(startDateStr);
                cal.setTime(sDate);
            } catch (ParseException e) {
                // use current date for cal
            }

            startDateFormat = new SimpleDateFormat("yyyyMMdd", Locale.US);
            String post_date = startDateFormat.format(cal.getTime());

            StringBuilder messageBuf = new StringBuilder();
            messageBuf.append(this.getShefHeader(timeStr));
            messageBuf.append(this.getShefComment());
            messageBuf.append(String.format(".AR %s %s DH%02d%02d/%s %s\n", id,
                    post_date, hour, minute, pe, value));
            messageBuf.append(SHEF_FOOTER);

            writeFile(messageBuf.toString());
        }
    }

    public void createEditFile(ArrayList<DecodedMessage> decodedMsg,
            XdatDB databaseMgr, String newValue) {
        shefFile = this.getShefFile();
        if (shefFile != null) {
            Calendar cal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
            SimpleDateFormat dateFmt = new SimpleDateFormat("ddHHmm", Locale.US);
            String currentTime = dateFmt.format(cal.getTime());

            StringBuilder messageBuf = new StringBuilder();
            messageBuf.append(this.getShefHeader(currentTime));
            messageBuf.append(this.getShefComment());

            String postDate;
            Calendar obsTime;
            String hour;

            for (DecodedMessage msg : decodedMsg) {
                String durCode = databaseMgr.getDurcode(msg.getDur());
                obsTime = msg.getObsTime();

                dateFmt = new SimpleDateFormat("yyyyMMdd", Locale.US);
                postDate = dateFmt.format(obsTime.getTime());

                dateFmt = new SimpleDateFormat("HHmmss", Locale.US);
                hour = dateFmt.format(obsTime.getTime());

                messageBuf.append(String.format(".AR %s %s DH%s/%s%s%s%s %s\n",
                        msg.getId(), postDate, hour, msg.getPe(), durCode,
                        msg.getTS(), msg.getExtremum(), newValue));
            }

            messageBuf.append(SHEF_FOOTER);

            writeFile(messageBuf.toString());
        }
    }

    /**
     * Send the file out via XDAT xmit command or shef ingest. This method must
     * be called after the createFile method.
     */
    public void sendFile() {
        
    	if (!sendAction) {
        	return;
        }
           
        if (xmitCmd.length() == 0) {
        	String msg = "ERROR: xdat_xmit_cmd not specified. \n" + "File "
                        + shefFile.getAbsolutePath() + " not sent";
            statusHandler.handle(Priority.ERROR, msg);
        } else {
            String cmd = xmitCmd + " " + shefFile.getAbsolutePath();
            try {
            	// DR#10955
                RunProcess.getRunProcess().exec(cmd);
            } catch (IOException e) {
                String msg = "ERROR:  Send Failed.";
                statusHandler.handle(Priority.ERROR, msg);
            }

        }
        
    }

    private void writeFile(String message) {
        try {
            FileWriter fw = new FileWriter(shefFile);
            fw.write(message);
            fw.close();
        } catch (IOException e) {
            String msg = "ERROR: unable write to " + shefFile.getAbsolutePath();
            statusHandler.handle(Priority.ERROR, msg, e);
        }
    }

    /**
     * Get the shef filename.
     * 
     * @return File object
     */
    private File getShefFile() {
    	
    	String aDir ="";
    	
    	if ( (send.compareTo("1") == 0) || (send.compareToIgnoreCase("yes") == 0) ) {
            sendAction = true;
            aDir = localDataDir;
        }
    	
    	if ( (send.compareTo("0") == 0) || (send.compareToIgnoreCase("no") == 0) ) {
            sendAction = false;
            aDir = shefDir;
            
            File sd = new File(shefDir);
            
            if (!sd.exists()) {
                String msg = "ERROR: " + shefDir + " does not exist.  Check "
                        + "the EDEX server for the directory";
                
                statusHandler.handle(Priority.ERROR, msg);
                return null;
            }

            if (!sd.canWrite()) {
                    
                String msg = "ERROR: " + shefDir + " does not have write " + "permissions.  " +
                    		"Check the EDEX server for proper permissions.";
                    
                statusHandler.handle(Priority.ERROR, msg);
                return null;    
            }
        }
    	
        File xdatrrFile = null;
        int i = 0;
        for (i = 1; i < 100; i++) {
            String fileName = aDir + "/xdatrr" + i;
            xdatrrFile = new File(fileName);
            try {
                if (xdatrrFile.createNewFile()) {
                    break;
                }
            } catch (IOException e) {
                continue;
            }
        }

        if (i == 100) {
            String msg = "ERROR: creating SHEF product filename because all filenames used.";
            statusHandler.handle(Priority.ERROR, msg);
            return null;
        }

        return xdatrrFile;
    }

    /**
     * Get the header line of the shef file.
     * 
     * @param timeStr
     *            The time in String format
     * 
     * @return The shef header line
     */
    private String getShefHeader(String timeStr) {
        String headerLine = null;
        // Send the file out
        if (sendAction) {
            headerLine = String.format("ZCZC %s %s\nTTAA00 %s %s\n", pil,
                    route, hdr, timeStr);
        } else {
            // Copy to the manual directory for shef ingest
            headerLine = String.format("ZCZC %s %s\nTTAA00 %s %s\n%s\n", pil,
                    route, hdr, timeStr, pil);
        }

        return headerLine;
    }

    /**
     * Get the shef file comment.
     * 
     * @return The XDAT comment
     */
    private String getShefComment() {
        StringBuilder sb = new StringBuilder(":\n");
        sb.append(String.format(":\tGENERATED BY THE RFC XDAT PROGRAM (%s)\n",
                userId));
        sb.append(":\n");

        return sb.toString();
    }

    /**
     * Get the needed Apps_defaults values.
     */
    private void getAppsDefaults() {
        AppsDefaults appsDefaults = AppsDefaults.getInstance();

        pil = appsDefaults.getToken("xdat_afosid");
        route = appsDefaults.getToken("xdat_route_code");
        hdr = appsDefaults.getToken("xdat_office_hdr");
        userId = appsDefaults.getToken("LOGNAME");
        send = appsDefaults.getToken("xdat_send_shef");
        xmitCmd = appsDefaults.getToken("xdat_xmit_cmd");
        shefDir = appsDefaults.getToken("shefdecode_input");
        localDataDir = appsDefaults.getToken("xdat_localdata_dir");
    }
}
