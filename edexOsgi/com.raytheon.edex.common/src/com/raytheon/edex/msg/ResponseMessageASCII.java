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

package com.raytheon.edex.msg;

import java.util.Calendar;
import java.util.Date;

import com.raytheon.edex.util.Util;
import com.raytheon.uf.common.message.response.AbstractResponseMessage;
import com.raytheon.uf.common.util.ConvertUtil;

/**
 * Represents an ASCII product response from EDEX.
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 22Aug2006      #18       MW Fegan    Initial creation.
 * 23Oct2006      TO4       MW Fegan    Provide safe values for missing arguments.
 * 06Mar2007      #115      MW Fegan    Use ob time when available for valid time.
 * 
 * </pre>
 *
 * @author mfegan
 * @version 1
 */

public class ResponseMessageASCII extends AbstractResponseMessage {
    private String filename = null;
    private String contents = null;
    private String type = null;
    private String station = null;
    private String time = null;
    /**
     * Constructor. No argument constructor.
     */
    public ResponseMessageASCII() {
        // intentionall empty.
    }
    /**
     * Private constructor. Creates a fully populated object. This allows a static
     * method to create an object.
     * 
     * @param filename name of the original ASCII file.
     * @param contents the message contents.
     * @param type the ASCII message type (METAR, TAF, etc).
     * @param station the station ICAO.
     * @param time the record time.
     * @param uri the data uri for the object
     */
    private ResponseMessageASCII(String filename,
                                 String contents,
                                 String type, 
                                 String station,
                                 String time, 
                                 String uri) {
        this.fileType = "ascii";
        this.filename = Util.isEmptyString(filename)?"":filename;
        this.contents = contents;
        this.type = Util.isEmptyString(type)?"":type;
        this.station = Util.isEmptyString(station)?"":station;
        this.time = Util.isEmptyString(time)?"":time;

        this.dataURI = Util.isEmptyString(uri)?"":uri;
        try {
            Calendar valid = (Calendar)ConvertUtil.convertObject(time, Calendar.class);
            this.validTime = valid.getTime();
        } catch (Exception e) {
            this.validTime = new Date();            
        }
    }
    /**
     * Static method that creates an ASCII Response Message. Uses the
     * private constructor to create the object.
     * 
     * @param filename name of the original ASCII file.
     * @param contents the message contents.
     * @param type the ASCII message type (METAR, TAF, etc).
     * @param station the station ICAO.
     * @param time the record time.
     * @param uri the data uri for the object
     * 
     * @return a fully populated {@code ResponseMessageASCII} object.
     */
    public static ResponseMessageASCII generateASCIIResponse(String filename,
                                                             String contents,
                                                             String type,
                                                             String station,
                                                             String time,
                                                             String uri) {
        return new ResponseMessageASCII(filename, contents, type, station, time, uri);
    }
    /**
     * @return the contents
     */
    public String getContents() {
        return contents;
    }
    /**
     * @param contents the contents to set
     */
    public void setContents(String contents) {
        this.contents = contents;
    }
    /**
     * @return the filename
     */
    public String getFilename() {
        return filename;
    }
    /**
     * @param filename the filename to set
     */
    public void setFilename(String filename) {
        this.filename = filename;
    }
    /**
     * @return the station
     */
    public String getStation() {
        return station;
    }
    /**
     * @param station the station to set
     */
    public void setStation(String station) {
        this.station = station;
    }
    /**
     * @return the time
     */
    public String getTime() {
        return time;
    }
    /**
     * @param time the time to set
     */
    public void setTime(String time) {
        this.time = time;
    }
    /**
     * @return the type
     */
    public String getType() {
        return type;
    }
    /**
     * @param type the type to set
     */
    public void setType(String type) {
        this.type = type;
    }
    
}
