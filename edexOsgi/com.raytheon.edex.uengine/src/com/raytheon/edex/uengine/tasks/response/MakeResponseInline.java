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

package com.raytheon.edex.uengine.tasks.response;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.util.ArrayList;
import java.util.GregorianCalendar;
import java.util.List;
import java.util.UUID;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import com.raytheon.edex.msg.ResponseMessageInline;
import com.raytheon.edex.uengine.ResponseUtil;
import com.raytheon.edex.uengine.exception.MicroEngineException;
import com.raytheon.edex.uengine.tasks.ScriptTask;
import com.raytheon.edex.util.Util;

/**
 * Makes an inline response message. Derived from old uEngine
 * MakeResponseInline.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date             PR#             Engineer            Description
 * -----------      ----------      ------------        --------------------------
 * Apr 12, 2007                     njensen             Initial Creation
 * </PRE>
 * 
 */
public class MakeResponseInline extends ScriptTask {

    private Object dataItem;

    private Object validTime;

    private String format;

    private String dataUri;

    public MakeResponseInline(Object aDataItem, Object aValidTime,
            String aFormat, String aDataUri) {
        dataItem = aDataItem;
        validTime = aValidTime;
        format = aFormat;
        dataUri = aDataUri;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.uengine.js.tasks.ScriptTask#execute()
     */
    @Override
    public Object execute() {
        // set up the data and uri lists
        List<Object> imageData = null;
        URI[] productURIs = null;
        if (dataItem instanceof URI) {
            productURIs = new URI[1];
            productURIs[0] = (URI) dataItem;
        } else if (dataItem instanceof byte[]) {
            imageData = new ArrayList<Object>();
            imageData.add(dataItem);
        } else {
            throw new MicroEngineException("unknown data type "
                    + Util.printString(dataItem.getClass().getName())
                    + " unable to continue.");
        }
        // create the return object - need to call the create method.
        return generateInlineResponse(validTime, imageData, productURIs,
                imageData.size(), format, dataUri);
    }

    /**
     * Creates an inline response message.
     * 
     * @param metaData
     *            the meta data to use for the response.
     * @param imageData
     *            the image buffers
     * @param productURIs
     *            the image uris
     * @param imageCount
     *            the number of images
     * 
     * @return the response message
     */
    private ResponseMessageInline generateInlineResponse(Object aTime,
            List<Object> imageData, URI[] productURIs, int imageCount,
            String aFormat, String aDataURI) throws MicroEngineException {
        ResponseMessageInline response = new ResponseMessageInline();
        // get the valid time
        GregorianCalendar validTime = null;
        if (aTime == null) {
            validTime = new GregorianCalendar();
        } else {
            validTime = ResponseUtil.getValidTime(aTime);
        }
        // get the Data URI
        if (aDataURI == null) {
            aDataURI = "";
        }

        String[] fileNames = new String[imageCount];

        for (int counter = 0; counter < imageCount; counter++) {
            fileNames[counter] = UUID.randomUUID() + "." + aFormat;
        }

        response.setFileName(fileNames);
        response.setFileType(aFormat);
        response.setValidTime(validTime.getTime());
        response.setDataURI(aDataURI);

        ArrayList<byte[]> zippedData = new ArrayList<byte[]>();

        if (productURIs != null) {

            try {
                for (URI productURI : productURIs) {
                    File file = new File(productURI.getPath());
                    if (file.exists()) {
                        ByteArrayOutputStream fileData = new ByteArrayOutputStream();
                        ZipOutputStream zipOutputStream = new ZipOutputStream(
                                fileData);
                        ResponseUtil.recursiveZip(zipOutputStream, file, "");
                        zipOutputStream.close();
                        zippedData.add(fileData.toByteArray());

                        fileData.close();
                    } else {
                        throw new MicroEngineException("Invalid file passed: "
                                + file.toString());
                    }
                }
            } catch (Exception e) {
                throw new MicroEngineException(
                        "Unable to package procduct files.", e);
            }

        }
        // Handle images passed in as byte arrays
        else if (imageData != null) {
            try {
                for (int i = 0; i < imageData.size(); i++) {
                    ByteArrayOutputStream fileData = new ByteArrayOutputStream();
                    ZipOutputStream zipOutputStream = new ZipOutputStream(
                            fileData);

                    ZipEntry entry = new ZipEntry(fileNames[i]);
                    zipOutputStream.putNextEntry(entry);
                    zipOutputStream.write((byte[]) imageData.get(i));
                    zipOutputStream.closeEntry();

                    zipOutputStream.close();

                    zippedData.add(fileData.toByteArray());
                    fileData.close();

                }
            } catch (IOException e) {
                throw new MicroEngineException(
                        "Unable to package product images.", e);
            }
        }

        response.setZippedData(zippedData);
        return response;
    }

    public Object getDataItem() {
        return dataItem;
    }

    public void setDataItem(Object aDataItem) {
        dataItem = aDataItem;
    }

    public String getDataUri() {
        return dataUri;
    }

    public void setDataUri(String aDataUri) {
        dataUri = aDataUri;
    }

    public String getFormat() {
        return format;
    }

    public void setFormat(String aFormat) {
        format = aFormat;
    }

    public Object getValidTime() {
        return validTime;
    }

    public void setValidTime(Object aValidTime) {
        validTime = aValidTime;
    }

}
