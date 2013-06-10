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
package com.raytheon.uf.common.activetable;

import javax.persistence.Entity;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;

import org.hibernate.annotations.Index;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * Practice Active Table, separated so that practice and operational data go to separate tables.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 10, 2010            njensen     Initial creation
 * May 10, 2013 1951       rjpeter     Added own id sequence tagging and new index.
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

@Entity
@SequenceGenerator(initialValue = 1, name = ActiveTableRecord.ID_GEN, sequenceName = "practice_activetableseq")
@Table(name = "practice_activetable")
@DynamicSerialize
@org.hibernate.annotations.Table(appliesTo = "practice_activetable", indexes = { @Index(name = "practice_activetable_officeid_phensig_idx", columnNames = {
        "officeid", "phensig" }) })
public class PracticeActiveTableRecord extends ActiveTableRecord implements
        Cloneable {

    private static final long serialVersionUID = 1L;

    @Override
    public Object clone() {
        return super.internalClone(new PracticeActiveTableRecord());
    }

}
