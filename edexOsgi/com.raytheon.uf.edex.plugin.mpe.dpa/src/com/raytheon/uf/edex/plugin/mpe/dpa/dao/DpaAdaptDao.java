package com.raytheon.uf.edex.plugin.mpe.dpa.dao;

import com.raytheon.uf.common.dataplugin.shef.tables.Dpaadapt;
import com.raytheon.uf.edex.plugin.mpe.dao.AbstractIHFSDbDao;

/**
 * IHFS Database Dao for interacting with the {@link Dpaadapt} entity.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 31, 2016 4662       jschmid     Initial creation
 * 
 * </pre>
 * 
 * @author jschmid
 */
public class DpaAdaptDao extends AbstractIHFSDbDao<Dpaadapt, String> {

    public DpaAdaptDao() {
        super(Dpaadapt.class);
    }

}
