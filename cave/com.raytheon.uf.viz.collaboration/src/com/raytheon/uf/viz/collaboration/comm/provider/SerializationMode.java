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
package com.raytheon.uf.viz.collaboration.comm.provider;

import java.io.Serializable;
import java.lang.annotation.Annotation;

import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 23, 2012            jkorman     Initial creation
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public enum SerializationMode {
    THRIFT, JAXB, JAVA, STRING, NONE, ISNULL;

    public static SerializationMode getMode(Object object) {
        SerializationMode mode = ISNULL;
        if (object != null) {
            if (object instanceof String) {
                mode = STRING;
            } else if (object instanceof Serializable) {
                mode = JAVA;
            } else {
                // We may override the serialization type
                Class<?> clazz = object.getClass();
                Annotation a = clazz.getAnnotation(DynamicSerialize.class);
                if (a != null) {
                    mode = THRIFT;
                } else {
                    a = clazz.getAnnotation(XmlRootElement.class);
                    if (a != null) {
                        mode = JAXB;
                    } else {
                        mode = NONE;
                    }
                }
            }
        }
        return mode;
    }
}
