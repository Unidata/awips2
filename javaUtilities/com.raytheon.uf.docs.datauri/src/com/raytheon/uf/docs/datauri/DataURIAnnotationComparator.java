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
package com.raytheon.uf.docs.datauri;

import java.lang.reflect.Field;
import java.util.Comparator;

import com.raytheon.uf.common.dataplugin.annotations.DataURI;

/**
 * Shamelessly stolen from DataURIUtil.java. I didn't want to change that to be
 * public since this plugin is only a utility.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 27, 2014            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class DataURIAnnotationComparator implements Comparator<Field> {

    @Override
    public int compare(Field f1, Field f2) {
        int i1 = f1.getAnnotation(DataURI.class).position();
        int i2 = f2.getAnnotation(DataURI.class).position();
        return (i1 < i2 ? -1 : (i1 == i2 ? 0 : 1));
    }

}
