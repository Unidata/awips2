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

package com.raytheon.uf.common.dataplugin.warning;

import java.util.HashMap;
import java.util.Map;

import javax.persistence.Access;
import javax.persistence.AccessType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import javax.persistence.UniqueConstraint;

import org.hibernate.annotations.Index;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * 
 * Warning Record
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 03/12/2007   1003        bwoodle     initial creation
 * Apr 4, 2013  1846        bkowal      Added an index on refTime and forecastTime
 * Apr 12, 2013 1857        bgonzale    Added SequenceGenerator annotation.
 * May 02, 2013 1949        rjpeter     Removed ugcZones.
 * May 07, 2013 1869        bsteffen    Remove dataURI column from
 *                                      PluginDataObject.
 * </pre>
 * 
 * @author bwoodle
 * @version 1
 */
@Entity
@SequenceGenerator(initialValue = 1, name = PluginDataObject.ID_GEN, sequenceName = "warningseq")
@Table(name = "warning", uniqueConstraints = { @UniqueConstraint(columnNames = { "dataURI" }) })
/*
 * Both refTime and forecastTime are included in the refTimeIndex since
 * forecastTime is unlikely to be used.
 */
@org.hibernate.annotations.Table(appliesTo = "warning", indexes = {
        @Index(name = "warning_refTimeIndex", columnNames = { "refTime",
                "forecastTime" }),
        @Index(name = "warning_office_phensig_index", columnNames = {
                "officeid", "phensig" }) })
@DynamicSerialize
public class WarningRecord extends AbstractWarningRecord {

    public static class WarningAction {
        public static final WarningAction CON = new WarningAction("CON");

        public static final WarningAction CAN = new WarningAction("CAN");

        public static final WarningAction EXP = new WarningAction("EXP");

        public static final WarningAction COR = new WarningAction("COR");

        public static final WarningAction NEW = new WarningAction("NEW");

        // Not sure what these ones do...
        public static final WarningAction EXA = new WarningAction("EXA");

        public static final WarningAction EXB = new WarningAction("EXB");

        public static final WarningAction UPG = new WarningAction("UPG");

        // /end

        public static final WarningAction EXT = new WarningAction("EXT");

        public static final WarningAction CANCON = new WarningAction("CANCON");

        private static final WarningAction[] values = new WarningAction[] {
                CON, CAN, EXP, COR, NEW, EXT, EXA, EXB, UPG, CANCON };

        private static Map<String, WarningAction> unknownMap = new HashMap<String, WarningAction>();

        private final String text;

        private WarningAction(String text) {
            this.text = text;
        }

        public static WarningAction[] values() {
            return values;
        }

        public static WarningAction valueOf(String act) {
            for (WarningAction action : values) {
                if (action.text.equalsIgnoreCase(act)) {
                    return action;
                }
            }
            WarningAction rval = unknownMap.get(act);
            if (rval == null) {
                rval = new WarningAction(act);
                unknownMap.put(act, rval);
            }
            return rval;
        }

        @Override
        public String toString() {
            return text;
        }

    }

    private static final long serialVersionUID = 1L;

    /**
     * Constructor.
     */
    public WarningRecord() {
        super();
    }

    /**
     * Constructor to duplicate record.
     * 
     * @param message
     *            The text of the message
     */
    public WarningRecord(WarningRecord old) {
        super(old);
    }

    /**
     * Constructs a warning record from a dataURI
     * 
     * @param uri
     *            The dataURI
     * @param tableDef
     *            The table definition associated with this class
     */
    public WarningRecord(String uri) {
        super(uri);
    }
    @Override
    @Column
    @Access(AccessType.PROPERTY)
    public String getDataURI() {
        return super.getDataURI();
    }
}
