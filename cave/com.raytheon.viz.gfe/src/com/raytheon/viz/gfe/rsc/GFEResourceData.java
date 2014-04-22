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
package com.raytheon.viz.gfe.rsc;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.DataManagerUIFactory;
import com.raytheon.viz.gfe.core.IParmManager;
import com.raytheon.viz.gfe.core.parm.Parm;

/**
 * Resource data for {@link GFEResource}. Saves information about the parm.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Jan 23, 2014  2703     bsteffen    Rewrite to enable JAXB serialization and
 *                                    resource construction.
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 2.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class GFEResourceData extends AbstractResourceData {

    @XmlAttribute
    private String parmName;

    @XmlAttribute
    private String parmLevel;

    @XmlAttribute
    private String dbId;

    public GFEResourceData() {
        /* Empty constructor for JAXB */
    }

    public GFEResourceData(ParmID parmId) {
        this.parmName = parmId.getParmName();
        this.parmLevel = parmId.getParmLevel();
        this.dbId = parmId.getDbId().toString();
    }

    @Override
    public GFEResource construct(LoadProperties loadProperties,
            IDescriptor descriptor) throws VizException {
        DataManager dataManager = DataManagerUIFactory.getCurrentInstance();
        IParmManager parmManager = dataManager.getParmManager();
        DatabaseID dbId = new DatabaseID(this.dbId);
        ParmID parmId = new ParmID(this.parmName, dbId, this.parmLevel);
        Parm parm = parmManager.getParm(parmId);
        if (parm == null) {
            parm = parmManager.addParm(parmId, false, true);
        }
        return new GFEResource(this, loadProperties, parm, dataManager);
    }

    @Override
    public void update(Object updateData) {
        /* Nothing uses updates */
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((dbId == null) ? 0 : dbId.hashCode());
        result = prime * result
                + ((parmLevel == null) ? 0 : parmLevel.hashCode());
        result = prime * result
                + ((parmName == null) ? 0 : parmName.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        GFEResourceData other = (GFEResourceData) obj;
        if (dbId == null) {
            if (other.dbId != null)
                return false;
        } else if (!dbId.equals(other.dbId))
            return false;
        if (parmLevel == null) {
            if (other.parmLevel != null)
                return false;
        } else if (!parmLevel.equals(other.parmLevel))
            return false;
        if (parmName == null) {
            if (other.parmName != null)
                return false;
        } else if (!parmName.equals(other.parmName))
            return false;
        return true;
    }

}
