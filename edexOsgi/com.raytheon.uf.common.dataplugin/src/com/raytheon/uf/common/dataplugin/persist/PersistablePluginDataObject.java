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
package com.raytheon.uf.common.dataplugin.persist;

import java.util.Calendar;
import java.util.Date;
import java.util.TimeZone;

import javax.persistence.Inheritance;
import javax.persistence.InheritanceType;
import javax.persistence.MappedSuperclass;
import javax.persistence.SequenceGenerator;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * Provides an abstract implementation of plugindataobject with clustered file
 * support.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 16, 2008            chammack     Initial creation
 * Apr 8, 2013  1293       bkowal       Removed references to hdffileid.
 * Apr 12, 2013 1857       bgonzale     Changed to MappedSuperclass.
 * Mar 02, 2013 1970       bgonzale     Added SequenceGenerator and Inheritance Strategy
 *                                      annotations.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */
@MappedSuperclass
@SequenceGenerator(name = PluginDataObject.ID_GEN)
@Inheritance(strategy = InheritanceType.TABLE_PER_CLASS)
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public abstract class PersistablePluginDataObject extends PluginDataObject
        implements IPersistable {

    private static final long serialVersionUID = 1L;

    /**
     * Constructor
     */
    public PersistablePluginDataObject(String uri) {
        super(uri);
    }

    /**
     * Constructor
     */
    public PersistablePluginDataObject() {
        super();
    }

    /**
     * Set the time to be used for the persistence time for this object.
     * 
     * @param persistTime
     *            The persistence time to be used.
     */
    @Override
    public void setPersistenceTime(Date persistTime) {
        Calendar c = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        c.setTime(persistTime);
        setInsertTime(c);
    }

    /**
     * Get the time to use for persisting this data.
     * 
     * @return The persistence time for this data.
     */
    @Override
    public Date getPersistenceTime() {
        Calendar c = getInsertTime();
        if (c == null) {
            return null;
        }

        return c.getTime();
    }
}
