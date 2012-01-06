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

package com.raytheon.edex.util.satellite;

import java.io.Serializable;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

import com.raytheon.uf.common.dataplugin.persist.PersistableDataObject;
import com.raytheon.uf.common.serialization.ISerializableObject;

/**
 * A satellite source i.e. NESDIS
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 *                         bphillip    Initial Creation
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */

@Entity
@Table(name = "satellite_sources")
public class SatelliteSource extends PersistableDataObject implements
        Serializable, ISerializableObject {

    private static final long serialVersionUID = 5855029407853840979L;

    /** The source id number */
    @Id
    private int sourceId;

    /** The source name */
    @Column(length = 64)
    private String sourceName;

    /**
     * Constructs an empty SatelliteSource
     */
    public SatelliteSource() {

    }

    /**
     * Constructs a new SatelliteSource
     * 
     * @param id
     *            The source id
     * @param name
     *            The source name
     */
    public SatelliteSource(int id, String name) {
        this.sourceId = id;
        this.sourceName = name;
    }

    public int getSourceId() {
        return sourceId;
    }

    public void setSourceId(int sourceId) {
        this.sourceId = sourceId;
    }

    public String getSourceName() {
        return sourceName;
    }

    public void setSourceName(String sourceName) {
        this.sourceName = sourceName;
    }

    public String toString() {
        return sourceName;
    }
}
