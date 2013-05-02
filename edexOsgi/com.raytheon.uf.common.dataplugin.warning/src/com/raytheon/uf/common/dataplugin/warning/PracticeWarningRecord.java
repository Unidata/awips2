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

import java.util.List;

import javax.persistence.Entity;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import javax.persistence.UniqueConstraint;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import org.hibernate.annotations.Index;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * 
 * Operational Warning Record
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 10/04/2011   10049        bgonzale    initial creation
 * Apr 4, 2013        1846 bkowal      Added an index on refTime and forecastTime
 * Apr 12, 2013 1857        bgonzale     Added SequenceGenerator annotation.
 * 
 * </pre>
 * 
 * @author bgonzale
 * @version 1
 */
@Entity
@SequenceGenerator(initialValue = 1, name = PluginDataObject.ID_GEN, sequenceName = "practicewarningseq")
@Table(name = "practicewarning", uniqueConstraints = { @UniqueConstraint(columnNames = { "dataURI" }) })
/*
 * Both refTime and forecastTime are included in the refTimeIndex since
 * forecastTime is unlikely to be used.
 */
@org.hibernate.annotations.Table(
		appliesTo = "practicewarning",
		indexes = {
				@Index(name = "practicewarning_refTimeIndex", columnNames = { "refTime", "forecastTime" } )
		}
)
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class PracticeWarningRecord extends AbstractWarningRecord {

    private static final long serialVersionUID = 1L;

    /**
     * Constructor.
     */
    public PracticeWarningRecord() {
        super();
    }

    /**
     * Constructor to duplicate record.
     * 
     * @param message
     *            The text of the message
     */
    public PracticeWarningRecord(PracticeWarningRecord old) {
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
    public PracticeWarningRecord(String uri) {
        super(uri);
        identifier = java.util.UUID.randomUUID().toString();
    }

    @Override
    public void setUgcs(List<String> list) {
        ugczones.clear();
        for (String s : list) {
            ugczones.add(new UGCZone(s, this));
        }
    }

}
