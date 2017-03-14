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
package com.raytheon.uf.edex.plugin.mpe.biasmesgen;

import java.nio.file.Path;

/**
 * Signals that reading/writing a Bias Table has failed.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 16, 2016 5576       bkowal      Initial creation
 * 
 * </pre>
 * 
 * @author bkowal
 */

public class BiasTableIOException extends Exception {

    public enum ACTION {
        READ("read"), WRITE("write"), UNSPECIFIED("?");

        private final String msgText;

        private ACTION(String msgText) {
            this.msgText = msgText;
        }

        public String getMsgText() {
            return msgText;
        }
    }

    private static final long serialVersionUID = -7227909051130386783L;

    public BiasTableIOException(Path biasTablePath, ACTION action,
            Throwable cause) {
        super(generateExMsg(biasTablePath, action), cause);
    }

    private static String generateExMsg(Path biasTablePath, ACTION action) {
        if (action == null) {
            action = ACTION.UNSPECIFIED;
        }
        return String.format("Failed to %s bias table file: %s.",
                action.getMsgText(), biasTablePath.toString());
    }
}