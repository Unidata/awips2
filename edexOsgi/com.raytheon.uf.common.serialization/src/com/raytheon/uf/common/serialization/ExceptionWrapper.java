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
package com.raytheon.uf.common.serialization;

/**
 * Class used to wrap/unwrap throwables in a serializable form to be transported
 * over thrift
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 10, 2009            mschenke     Initial creation
 * Sep 14, 2012 1169       djohnson     Moved to com.raytheon.uf.common.serialization
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class ExceptionWrapper {
    public static SerializableExceptionWrapper wrapThrowable(Throwable t) {
        return buildWrapper(t);
    }

    private static SerializableExceptionWrapper buildWrapper(Throwable t) {
        if (t == null) {
            return null;
        }
        SerializableExceptionWrapper wrapper = new SerializableExceptionWrapper();
        wrapper.setExceptionClass(t.getClass().getName());
        wrapper.setMessage(t.getLocalizedMessage());
        wrapper.setStackTrace(t.getStackTrace());
        wrapper.setWrapper(buildWrapper(t.getCause()));
        return wrapper;
    }

    public static Throwable unwrapThrowable(SerializableExceptionWrapper sew) {
        return buildThrowable(sew);
    }

    private static Throwable buildThrowable(SerializableExceptionWrapper wrapper) {
        if (wrapper == null) {
            return null;
        }
        Throwable t = new WrappedException(wrapper.getExceptionClass(), wrapper
                .getMessage(), buildThrowable(wrapper.getWrapper()));
        t.setStackTrace(wrapper.getStackTrace());
        return t;
    }
}
