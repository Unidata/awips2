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
package com.raytheon.uf.edex.decodertools.bufr.exceptions;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.edex.decodertools.bufr.descriptors.BUFRDescriptor;

/**
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 20071127            382 jkorman     Initial Coding.
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public class BUFRDecoderException extends RuntimeException {

    private static final long serialVersionUID = 1000001L;

    private ArrayList<BUFRDescriptor> descriptorTrace;
    
    public BUFRDecoderException() {
        super();
    }

    public BUFRDecoderException(String message) {
        super(message);
    }

    public BUFRDecoderException(String message, Throwable cause) {
        super(message, cause);
    }

    public BUFRDecoderException(Throwable cause) {
        super(cause);
    }

    public void addDescriptorToTrace(BUFRDescriptor descriptor) {
        if(descriptorTrace == null) {
            descriptorTrace = new ArrayList<BUFRDescriptor>();
        }
        descriptorTrace.add(descriptor);
    }

    public List<BUFRDescriptor> getTrace() {
        return descriptorTrace;
    }
}
