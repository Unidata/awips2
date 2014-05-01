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
package com.raytheon.uf.common.dissemination;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * OfficialUserProduct (OUP), representing a text product to be disseminated.
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 29, 2009            njensen     Initial creation
 * Dec 15, 2009 DR3778     M. Huang    Add acknowledgment handling
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

@DynamicSerialize
public class OfficialUserProduct implements ISerializableObject {

    /** optional, corresponds to -w in legacy perl script, aka bbb **/
    @DynamicSerializeElement
    protected String wmoType;

    /**
     * optional address to send to, if null product will be sent to DEFAULTNCF
     * and NWWS if not found in exclude lists
     **/
    @DynamicSerializeElement
    protected String address;

    /**
     * optional String of format DDHHMM, corresponds to -d in legacy perl script
     **/
    @DynamicSerializeElement
    protected String userDateTimeStamp;

    /** required in the format CCCCNNNXXX **/
    @DynamicSerializeElement
    protected String awipsWanPil;

    /** required, just the filename, not the full path **/
    @DynamicSerializeElement
    protected String filename;

    /** required, the the text that will go in the file at filename **/
    @DynamicSerializeElement
    protected String productText;

    /**
     * optional, the source application that sent the product, if not specified,
     * default handling codes will be used
     **/
    @DynamicSerializeElement
    protected String source;

    /** product sending priority based on awipsPriority.txt file **/
    @DynamicSerializeElement
    protected int priority;

    /**
     * whether the service should add the wmo header to the beginning of the
     * productText, defaults to true
     **/
    @DynamicSerializeElement
    protected boolean needsWmoHeader = true;

    @DynamicSerializeElement
    protected String attachedFilename;

    @DynamicSerializeElement
    protected byte[] attachedFile;

    public String getWmoType() {
        return wmoType;
    }

    public void setWmoType(String wmoType) {
        this.wmoType = wmoType;
    }

    public String getUserDateTimeStamp() {
        return userDateTimeStamp;
    }

    public void setUserDateTimeStamp(String userDateTimeStamp) {
        this.userDateTimeStamp = userDateTimeStamp;
    }

    public String getFilename() {
        return filename;
    }

    public void setFilename(String filename) {
        this.filename = filename;
    }

    public String getProductText() {
        return productText;
    }

    public void setProductText(String productText) {
        this.productText = productText;
    }

    public String getAddress() {
        return address;
    }

    public void setAddress(String address) {
        this.address = address;
    }

    public String getSource() {
        return source;
    }

    public void setSource(String source) {
        this.source = source;
    }

    public int getPriority() {
        return priority;
    }

    public void setPriority(int priority) {
        this.priority = priority;
    }

    public boolean isNeedsWmoHeader() {
        return needsWmoHeader;
    }

    public void setNeedsWmoHeader(boolean needsWmoHeader) {
        this.needsWmoHeader = needsWmoHeader;
    }

    public String getAwipsWanPil() {
        return awipsWanPil;
    }

    public void setAwipsWanPil(String awipsWanPil) {
        this.awipsWanPil = awipsWanPil;
    }

    public String getAttachedFilename() {
        return attachedFilename;
    }

    public void setAttachedFilename(String attachedFilename) {
        this.attachedFilename = attachedFilename;
    }

    public byte[] getAttachedFile() {
        return attachedFile;
    }

    public void setAttachedFile(byte[] attachedFile) {
        this.attachedFile = attachedFile;
    }

}
