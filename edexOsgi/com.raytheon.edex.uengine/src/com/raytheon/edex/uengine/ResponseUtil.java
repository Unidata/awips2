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

package com.raytheon.edex.uengine;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.sql.Date;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.GregorianCalendar;
import java.util.Hashtable;
import java.util.List;
import java.util.TimeZone;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import org.w3c.dom.Document;

import com.raytheon.edex.util.Util;
import com.raytheon.edex.util.XMLUtils;
import com.raytheon.uf.common.message.Body;
import com.raytheon.uf.common.message.Header;
import com.raytheon.uf.common.message.Message;
import com.raytheon.uf.common.message.Property;
import com.raytheon.uf.common.message.response.AbstractResponseMessage;
import com.raytheon.uf.common.message.response.ResponseMessageError;

/**
 * Util class for responses. Originally extracted from original uEngine's
 * AMakeResponse.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date             PR#             Engineer            Description
 * -----------      ----------      ------------        --------------------------
 * Mar 29, 2007                     njensen             Initial Creation
 * </PRE>
 * 
 */
public class ResponseUtil {

    public static String EMPTY_DATA_URI = "";

    public static String NO_SCRIPT_ID = "N/A";

    /**
     * Converts an object containing a time value into a {@link Calendar}. The
     * time value must be either a YYYYMMDDhhmmss formatted String, a Calendar,
     * or a Date. For other object types, a current Calendar is returned.
     * 
     * @param timeObject
     *            the time value to change
     * 
     * @return the time value as a calendar
     */
    public static GregorianCalendar getValidTime(Object timeObject) {
        GregorianCalendar calendar = null;
        if (timeObject instanceof GregorianCalendar) {
            calendar = (GregorianCalendar) timeObject;
        } else if (timeObject instanceof Calendar) {
            calendar = new GregorianCalendar(TimeZone.getTimeZone("GMT"));
            calendar.setTime(((Calendar) timeObject).getTime());
        } else if (timeObject instanceof Date) {
            calendar = new GregorianCalendar(TimeZone.getTimeZone("GMT"));
            calendar.setTime((Date) timeObject);
        } else if (timeObject instanceof String) {
            calendar = Util.convertStr14ToCal((String) timeObject);
        } else {
            calendar = new GregorianCalendar(TimeZone.getTimeZone("GMT"));
        }
        return calendar;
    }

    /**
     * performs a (possibly recursive) zip of a directory.
     * 
     * @param zos
     *            the output stream.
     * @param directory
     *            the directory/file.
     * @param subPath
     *            the subdirectory.
     * @throws IOException
     *             if an error occurs.
     */
    public static void recursiveZip(ZipOutputStream zos, File directory,
            String subPath) throws IOException {

        if (!directory.isDirectory()) {
            addToZip(zos, subPath, directory);
            return;
        }

        File[] files = directory.listFiles();
        for (File f : files) {
            if (f.isDirectory()) {
                recursiveZip(zos, f, subPath + "/" + f.getName());
            } else {
                addToZip(zos, subPath, f);
            }
        }
    }

    /**
     * Adds a file to a zip file
     * 
     * @param zos
     *            the zip stream
     * @param subPath
     *            the path within the zip file
     * @param f
     *            the file to add
     * @throws IOException
     * @throws FileNotFoundException
     */
    private static void addToZip(ZipOutputStream zos, String subPath, File f)
            throws IOException, FileNotFoundException {
        ZipEntry ze = new ZipEntry(subPath + "/" + f.getName());

        zos.putNextEntry(ze);
        FileInputStream fis = new FileInputStream(f);
        try {
            byte[] temp = new byte[1024];
            int sz = fis.read(temp);
            while (sz > 0) {
                zos.write(temp, 0, sz);
                sz = fis.read(temp);
            }
            zos.closeEntry();
        } finally {
            // ensure file handles are not leaked
            if (fis != null)
                fis.close();
        }
    }

    /**
     * Wraps the output in the canonical XML message format.
     * <P>
     * <DL>
     * <DT><B>Note:</B>
     * <DD>If an error occurs in creating the message, the original contents
     * are returned.
     * </DL>
     * 
     * @deprecated use createMessage instead
     * @param contents
     *            contents to wrap
     * @param dataURI
     *            the data URI associated with the message
     * @param scriptID
     *            the client script ID associated with the message
     * 
     * @return the contents wrapped in the conanical XML message
     */
    public static String createCanonicalMessage(String dataURI,
            String scriptID, String[] contents) {
        try {
            Hashtable<String, String> properties = new Hashtable<String, String>();
            String now = String.format("%1$tY%1$tm%1$td%1$tH%1$tM%1$tS",
                    Calendar.getInstance());
            String stringURI = (dataURI == null) ? "" : dataURI.toString();
            properties.put("id", scriptID);
            properties.put("uri", stringURI);
            properties.put("time", now);
            properties.put("function", "response");
            Document document = XMLUtils.createXMLMessage(properties, contents);
            return XMLUtils.transformXMLDocument(document);
        } catch (Exception e) {
            // logger.warn("unable to create response message. Exception is
            // ",e);
            return contents[0];
        }
    }

    /**
     * Wraps the AbstractResponseMessages in a Message object
     * 
     * @param dataURI
     *            the data URI associated with the message
     * @param scriptID
     *            the client script ID associated with the message
     * @param contents
     *            contents to wrap
     * @return the contents wrapped in the Message
     */
    public static Message createMessageObject(String dataURI, String scriptID,
            List<AbstractResponseMessage> contents) {
        Message msg = new Message();
        Header header = new Header();
        String stringURI = (dataURI == null) ? "" : dataURI.toString();
        String id = (scriptID == null) ? "" : scriptID.toString();
        String now = String.format("%1$tY%1$tm%1$td%1$tH%1$tM%1$tS", Calendar
                .getInstance());
        Property[] properties = new Property[4];
        properties[0] = new Property("id", id);
        properties[1] = new Property("uri", stringURI);
        properties[2] = new Property("time", now);
        properties[3] = new Property("function", "response");
        header.setProperties(properties);
        msg.setHeader(header);
        Body body = new Body();
        body.setResponses(contents.toArray(new AbstractResponseMessage[contents
                .size()]));
        msg.setBody(body);
        return msg;
    }

    /**
     * Wraps a ResponseMessageError in a Message
     * 
     * @param error
     *            the error to wrap
     * @return
     */
    public static Message createErrorMessage(ResponseMessageError error) {
        ArrayList<AbstractResponseMessage> errorList = new ArrayList<AbstractResponseMessage>();
        errorList.add(error);
        Message response = createMessageObject(EMPTY_DATA_URI, NO_SCRIPT_ID,
                errorList);
        return response;
    }

}
