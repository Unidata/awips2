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
package com.raytheon.uf.common.wmo;

import java.io.Serializable;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 14, 2009            jkorman     Initial creation
 * Nov 08, 2013 2506       bgonzale    Setting messageBody is done only through setter method.
 * May 14, 2014 2536       bclement    moved WMO Header to common, added init()
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public class WMOMessage implements Serializable {

    private static final long serialVersionUID = 1L;

    private AFOSProductId productId;

    private WMOHeader wmoHeader;

    private String bodyText;

    private byte[] messageBody;

    private boolean strictWMOHeader = false;

    /**
     * Create an empty WMOMessage.
     */
    public WMOMessage() {
    }

    /**
     * Create a WMO Message instance from text.
     * 
     * @param wmoMessage
     */
    public WMOMessage(String wmoMessage, String fileName) {
        this(wmoMessage.getBytes(), fileName);
    }

    /**
     * Create a WMO Message instance from text.
     * 
     * @param wmoMessage
     */
    public WMOMessage(byte[] wmoMessage, String fileName) {
        if (wmoMessage != null) {
            WMOHeader header = new WMOHeader(wmoMessage, fileName);
            init(header, wmoMessage);
        }
    }

    /**
     * @param wmoMessage
     */
    public WMOMessage(byte[] wmoMessage) {
        if (wmoMessage != null) {
            WMOHeader header = new WMOHeader(wmoMessage);
            init(header, wmoMessage);
        }
    }

    /**
     * @param header
     * @param wmoMessage
     */
    private void init(WMOHeader header, byte[] wmoMessage) {
        if (header.isValid()) {
            wmoHeader = header;
            int bodyLen = wmoMessage.length - header.getMessageDataStart();
            byte[] messageBodyData = new byte[bodyLen];
            System.arraycopy(wmoMessage, header.getMessageDataStart(),
                    messageBodyData, 0, bodyLen);
            setMessageBody(messageBodyData);
        }
    }

    /**
     * @return the productId
     */
    public AFOSProductId getProductId() {
        return productId;
    }

    /**
     * @param productId
     *            the productId to set
     */
    public void setProductId(AFOSProductId productId) {
        this.productId = productId;
    }

    /**
     * 
     * @param xxx
     */
    public void setCcc(String ccc) {
        if (productId == null) {
            productId = new AFOSProductId();
        }
        productId.setCcc(ccc);
    }

    /**
     * 
     * @return
     */
    public String getCCC() {
        String ccc = null;
        if (productId != null) {
            ccc = productId.getCcc();
        }
        return ccc;
    }

    /**
     * 
     * @param xxx
     */
    public void setNnn(String nnn) {
        if (productId == null) {
            productId = new AFOSProductId();
        }
        productId.setNnn(nnn);
    }

    /**
     * 
     * @return
     */
    public String getNNN() {
        String nnn = null;
        if (productId != null) {
            nnn = productId.getNnn();
        }
        return nnn;
    }

    /**
     * 
     * @param xxx
     */
    public void setXxx(String xxx) {
        if (productId == null) {
            productId = new AFOSProductId();
        }
        productId.setXxx(xxx);
    }

    /**
     * 
     * @return
     */
    public String getXxx() {
        String xxx = null;
        if (productId != null) {
            xxx = productId.getXxx();
        }
        return xxx;
    }

    public String getTTAAii() {
        String ttaaii = null;
        if ((wmoHeader != null) && wmoHeader.isValid()) {
            StringBuilder sb = new StringBuilder();
            sb.append(wmoHeader.getT1());
            sb.append(wmoHeader.getT2());
            sb.append(wmoHeader.getA1());
            sb.append(wmoHeader.getA2());
            if (strictWMOHeader) {
                if (wmoHeader.getIi() < 10) {
                    sb.append('0');
                }
                sb.append(wmoHeader.getIi());
            } else {
                sb.append(wmoHeader.getIi());
            }
            ttaaii = sb.toString();
        }
        return ttaaii;
    }

    /**
     * 
     * @return
     */
    public String getCCCC() {
        String cccc = null;
        if ((wmoHeader != null) && wmoHeader.isValid()) {
            cccc = wmoHeader.getCccc();
        }
        return cccc;
    }

    /**
     * 
     * @return
     */
    public String getDDHHMM() {
        String ddHHmm = null;
        if ((wmoHeader != null) && wmoHeader.isValid()) {
            ddHHmm = wmoHeader.getYYGGgg();
        }
        return ddHHmm;
    }

    /**
     * 
     * @return
     */
    public String getBBB() {
        String bbb = null;
        if ((wmoHeader != null) && wmoHeader.isValid()) {
            bbb = wmoHeader.getBBBIndicator();
        }
        return bbb;
    }

    /**
     * Get the WMOHeader for this object.
     * 
     * @return The wmoHeader.
     */
    public WMOHeader getWmoHeader() {
        return wmoHeader;
    }

    /**
     * Set the WMOHeader for this object.
     * 
     * @param wmoHeader
     *            The wmoHeader to set.
     */
    public void setWmoHeader(WMOHeader wmoHeader) {
        this.wmoHeader = wmoHeader;
    }

    /**
     * @return the bodyText
     */
    public String getBodyText() {
        return bodyText;
    }

    /**
     * @param bodyText
     *            the bodyText to set
     */
    public void setBodyText(String bodyText) {
        this.bodyText = bodyText;
    }

    /**
     * Get the binary data that comprises the body of this message.
     * 
     * @return The binary data.
     */
    public byte[] getMessageBody() {
        return messageBody;
    }

    /**
     * Set the binary data that comprises the body of this message.
     * 
     * @return The binary data.
     */
    public void setMessageBody(byte[] binaryData) {
        messageBody = binaryData;
        if (messageBody != null) {
            bodyText = new String(messageBody);
        }
    }

    /**
     * @return the strictWMOHeader
     */
    public boolean isStrictWMOHeader() {
        return strictWMOHeader;
    }

    /**
     * @param strictWMOHeader
     *            the strictWMOHeader to set
     */
    public void setStrictWMOHeader(boolean strictWMOHeader) {
        this.strictWMOHeader = strictWMOHeader;
    }

}
