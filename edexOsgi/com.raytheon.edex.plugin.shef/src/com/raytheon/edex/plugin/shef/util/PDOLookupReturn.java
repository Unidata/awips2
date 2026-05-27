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
package com.raytheon.edex.plugin.shef.util;

import java.io.Serializable;

/**
 * Basic POJO used to encapsulate the DAO class and {@link Serializable} id
 * associated with an ihfs data record.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 12, 2017 6554       bkowal      Initial creation
 *
 * </pre>
 *
 * @author bkowal
 */

public final class PDOLookupReturn {

    private final Class<?> daoClass;

    private final Serializable id;

    public PDOLookupReturn(final Class<?> daoClass, final Serializable id) {
        this.daoClass = daoClass;
        this.id = id;
    }

    public Class<?> getDaoClass() {
        return daoClass;
    }

    public Serializable getId() {
        return id;
    }
}