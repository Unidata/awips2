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
package com.raytheon.uf.edex.plugin.mpe.dao;

import java.io.Serializable;

/**
 * Abstract DAO implementation for interacting with the hydro/mpe damcat
 * database. The damcat database will typically be a database with a name based
 * on the following convention: dc_ob{VERSION}{WFO}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 28, 2016 5614       bkowal      Initial creation
 * May 19, 2016 5576       bkowal      JavaDoc generic type params
 * 
 * </pre>
 * 
 * @author bkowal
 * @param <T>
 *            the Entity (annotated with the @Entity) tag that the DAO has been
 *            created for. Must exist in the damcat database.
 * @param <I>
 *            the identifier of the Entity that the DAO has been created for.
 *            Must extend Serializable.
 */

public abstract class AbstractDamcatDbDao<T, I extends Serializable> extends
        AbstractMPEDao<T, I> {

    private static final String GENERIC_DB_NAME = "damcat";

    protected AbstractDamcatDbDao(Class<T> entityClass) {
        super(GENERIC_DB_NAME, entityClass);
    }
}