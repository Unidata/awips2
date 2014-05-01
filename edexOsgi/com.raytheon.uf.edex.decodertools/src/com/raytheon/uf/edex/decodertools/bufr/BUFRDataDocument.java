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
package com.raytheon.uf.edex.decodertools.bufr;

import java.util.List;

import com.raytheon.uf.edex.decodertools.bufr.descriptors.BUFRDescriptor;
import com.raytheon.uf.edex.decodertools.bufr.packets.IBUFRDataPacket;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 	
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public class BUFRDataDocument {

    private List<IBUFRDataPacket> documentData;

    private BUFRDocument enclosingDocument;
    
    public BUFRDataDocument(List<IBUFRDataPacket> data) {
        documentData = data;
    }

    /**
     * @return the enclosingDocument
     */
    public BUFRDocument getEnclosingDocument() {
        return enclosingDocument;
    }


    /**
     * @param enclosingDocument the enclosingDocument to set
     */
    public void setEnclosingDocument(BUFRDocument enclosingDocument) {
        this.enclosingDocument = enclosingDocument;
    }


    public List<IBUFRDataPacket> getList() {
        return documentData;
    }

    public IBUFRDataPacket get(int descriptor) {
        IBUFRDataPacket data = null;

        return data;
    }

    /**
     * Get the first instance of the specified descriptor if it exists.
     * 
     * @param f
     * @param x
     * @param y
     * @return
     */
    public IBUFRDataPacket get(int f, int x, int y) {
        return get(BUFRDescriptor.createDescriptor(f, x, y));
    }

    /**
     * Get the first instance of a data packet that references the specified
     * descriptor.
     * 
     * @param descriptor
     * @return
     */
    public IBUFRDataPacket getFirst(int descriptor) {
        IBUFRDataPacket data = null;

        return data;
    }

    /**
     * Get the first instance of the specified descriptor if it exists.
     * 
     * @param f
     * @param x
     * @param y
     * @return
     */
    public IBUFRDataPacket getFirst(int f, int x, int y) {
        return getFirst(BUFRDescriptor.createDescriptor(f, x, y));
    }

    public BUFRDataDocument getFirstSubList(int descriptor) {
        BUFRDataDocument doc = null;

        return doc;
    }

    /**
     * Get the first instance of the specified descriptor if it exists.
     * 
     * @param f
     * @param x
     * @param y
     * @return
     */
    public BUFRDataDocument getFirstSubList(int f, int x, int y) {
        return getFirstSubList(BUFRDescriptor.createDescriptor(f, x, y));
    }

    public BUFRDataDocument getNextSubList() {
        BUFRDataDocument doc = null;

        return doc;
    }

}
