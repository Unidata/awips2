package com.raytheon.viz.warnings.rsc;

import java.util.ArrayList;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.warning.AbstractWarningRecord;
import com.raytheon.uf.common.dataplugin.warning.WarningRecord.WarningAction;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;

/**
 * Displays WOUs updated by WCNs
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 2014-08-28   ASM #15682 D. Friemdan Initial creation
 * </pre>
 * 
 */
@XmlAccessorType(XmlAccessType.NONE)
public class WouWcnWatchesResourceData extends WWAResourceData {

    @Override
    protected AbstractVizResource<?, ?> constructResource(
            LoadProperties loadProperties, PluginDataObject[] objects)
            throws VizException {
        // add records
        records = new ArrayList<AbstractWarningRecord>(objects.length);
        for (int i = 0; i < objects.length; i++) {
            AbstractWarningRecord r = (AbstractWarningRecord) objects[i];
            records.add(r);
        }

        return new WouWcnWatchesResource(this, loadProperties);
    }

    @Override
    protected boolean isRecordTimeImportant(AbstractWarningRecord warnRec) {
        WarningAction act = WarningAction.valueOf(warnRec.getAct());
        if (("WOU".equals(warnRec.getPil()) && WarningAction.CON == act) ||
                ("WCN".equals(warnRec.getPil()) && WarningAction.NEW == act)) {
            return false;
        } else {
            return super.isRecordTimeImportant(warnRec);
        }
    }
}

