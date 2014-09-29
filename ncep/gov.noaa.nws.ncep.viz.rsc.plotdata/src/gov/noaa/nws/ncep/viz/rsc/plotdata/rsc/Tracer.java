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
package gov.noaa.nws.ncep.viz.rsc.plotdata.rsc;

/**
 * TODO Add Description
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 16, 2013            bhebbard     Initial creation
 * Jul 07, 2014            bhebbard     Assorted updates
 *
 * </pre>
 *
 * @author bhebbard
 * @version 1.0	
 */

import gov.noaa.nws.ncep.viz.rsc.plotdata.rsc.NcPlotResource2.Station;

import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.io.UnsupportedEncodingException;
import java.lang.reflect.Method;
import java.text.DateFormat;
import java.text.DecimalFormat;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collection;
import java.util.Date;
import java.util.HashSet;

import com.raytheon.uf.common.time.DataTime;

public class Tracer {

    // ACTIVATE HERE ------------------v
    private static boolean enabled = false;

    // save it static to have it available on every call

    private static Method m;

    private static long startTime = -1;

    DateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd-HH-mm-ss");

    Calendar cal = Calendar.getInstance();

    String logFileName = "" // optional custom directory
            + dateFormat.format(cal.getTime());

    private static PrintWriter writer = null;

    static {
        try {
            m = Throwable.class.getDeclaredMethod("getStackTraceElement",
                    int.class);
            m.setAccessible(true);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public static String getMethodName(final int depth) {
        try {
            StackTraceElement element = (StackTraceElement) m.invoke(
                    new Throwable(), depth + 1);
            // Yes, following could be one line...
            // Just did line-by-line so can add/remove parts easily...
            String returnString = "";
            returnString = element.getClassName();
            returnString = returnString.replace(
                    "gov.noaa.nws.ncep.viz.rsc.plotdata.", "...");
            returnString += "." + element.getMethodName();
            return returnString;
        } catch (Exception e) {
            e.printStackTrace();
            return null;
        }
    }

    public static void print(String message) {
        if (enabled) {
            long elapsedTime = 0;
            if (startTime < 0) {
                DateFormat dateFormat = new SimpleDateFormat(
                        "yyyy-MM-dd-HH-mm-ss");
                Calendar cal = Calendar.getInstance();
                String logFileName = "/export/cdbsrv/bhebbard/pointdatadisplay/"
                        + dateFormat.format(cal.getTime());
                try {
                    writer = new PrintWriter(logFileName, "UTF-8");
                } catch (FileNotFoundException e) {
                    // TODO Auto-generated catch block. Please revise as
                    // appropriate.
                    // statusHandler.handle(Priority.PROBLEM,
                    // e.getLocalizedMessage(), e);
                    System.out.println("[" + "FileNotFoundException" + "] "
                            + message);
                } catch (UnsupportedEncodingException e) {
                    // TODO Auto-generated catch block. Please revise as
                    // appropriate.
                    // statusHandler.handle(Priority.PROBLEM,
                    // e.getLocalizedMessage(), e);
                    System.out.println("[" + "UnsupportedEncodingException"
                            + "] " + message);
                }
                System.out.println("[Logging to " + logFileName + "]");
                startTime = System.nanoTime();
            } else {
                elapsedTime = (System.nanoTime() - startTime) / 1000000;
            }
            System.out.println("[" + elapsedTime + getMethodName(1) + "] "
                    + message);
            writer.println("[" + elapsedTime + getMethodName(1) + "] "
                    + message);
        }
    }

    public static void printX(String message) {
        // System.out.println("[" + getMethodName(1) + "] " + message);
    }

    public static String shortTimeString(DataTime dt) {
        // temporary -- combine with
        // NcPlotResource2.FrameData.getShortFrameTime(), somehow
        @SuppressWarnings("deprecation")
        Date frameDate = dt.getRefTime();
        int date = frameDate.getDate();
        int hours = frameDate.getHours();
        hours += 4; // TODO use actual current local time offset from UTC
        if (hours > 23) {
            date += 1; // TODO: eom condition
            hours -= 24;
        }
        int minutes = frameDate.getMinutes();
        DecimalFormat df = new DecimalFormat("00");
        String returnString = Integer.toString(date) + "/" + df.format(hours)
                + df.format(minutes);
        if (dt.getFcstTime() != 0) {
            returnString += "(" + dt.getFcstTime() + ")";
        }
        return returnString;
    }

    public static String printableStationList(Collection<Station> stations) {
        String returnString = "";
        for (Station s : stations) {
            if (s != null && s.info != null) {
                returnString += s.info.stationId + " ";
            }
        }
        return returnString;
    }

    public static String printableStationList(Station[] stations) {
        return printableStationList(new HashSet<Station>(
                Arrays.asList(stations)));
    }
}
