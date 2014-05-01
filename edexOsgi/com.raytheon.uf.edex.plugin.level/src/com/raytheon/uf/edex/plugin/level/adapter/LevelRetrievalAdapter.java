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
package com.raytheon.uf.edex.plugin.level.adapter;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataplugin.level.LevelContainer;
import com.raytheon.uf.common.dataplugin.level.MasterLevel;
import com.raytheon.uf.common.dataplugin.level.MasterLevelContainer;
import com.raytheon.uf.common.dataplugin.level.request.GetAllLevelsForMasterLevelRequest;
import com.raytheon.uf.common.dataplugin.level.request.GetLevelByIdRequest;
import com.raytheon.uf.common.dataplugin.level.request.GetLevelRequest;
import com.raytheon.uf.common.dataplugin.level.request.GetMasterLevelRequest;
import com.raytheon.uf.common.dataplugin.level.request.ILevelRetrievalAdapter;
import com.raytheon.uf.edex.plugin.level.dao.LevelDao;

public class LevelRetrievalAdapter implements ILevelRetrievalAdapter {
    private Log logger = LogFactory.getLog(getClass());

    @Override
    public Level getLevel(GetLevelRequest request) {
        Level rval = null;

        try {
            LevelDao dao = new LevelDao();
            rval = dao.lookupLevel(request.getLevel());
        } catch (Exception e) {
            logger.error("Error occurred looking up level [" + request + "]", e);
        }

        return rval;
    }

    @Override
    public Level getLevel(GetLevelByIdRequest request) {
        Level rval = null;

        try {
            LevelDao dao = new LevelDao();
            rval = dao.lookupLevel(request.getId());
        } catch (Exception e) {
            logger.error("Error occurred looking up level [" + request + "]", e);
        }

        return rval;
    }

    @Override
    public MasterLevel getMasterLevel(GetMasterLevelRequest request) {
        MasterLevel rval = null;

        try {
            LevelDao dao = new LevelDao();
            rval = dao.lookupMasterLevel(request.getMasterLevel(),
                    request.isCreate());
        } catch (Exception e) {
            logger.error("Error occurred looking up level [" + request + "]", e);
        }
        return rval;
    }

    @Override
    public LevelContainer getAllLevelsForMasterLevel(
            GetAllLevelsForMasterLevelRequest request) {
        LevelContainer rval = null;
        try {
            LevelDao dao = new LevelDao();
            rval = dao.lookupAllLevels(request.getLevel());
        } catch (Exception e) {
            logger.error(
                    "Error occurred looking up all levels for master level ["
                            + request + "]", e);
        }
        return rval;
    }

    @Override
    public LevelContainer getAllLevels() {
        LevelContainer rval = null;
        try {
            LevelDao dao = new LevelDao();
            rval = dao.lookupAllLevels();
        } catch (Exception e) {
            logger.error("Error occurred looking up all levels", e);
        }
        return rval;
    }

    @Override
    public MasterLevelContainer getAllMasterLevels() {
        MasterLevelContainer rval = null;
        try {
            LevelDao dao = new LevelDao();
            rval = dao.lookupAllMasterLevels();
        } catch (Exception e) {
            logger.error("Error occurred looking up all levels", e);
        }
        return rval;
    }
}
