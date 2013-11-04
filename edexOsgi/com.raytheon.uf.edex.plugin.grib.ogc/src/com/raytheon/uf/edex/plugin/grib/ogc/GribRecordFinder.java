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
package com.raytheon.uf.edex.plugin.grib.ogc;

import java.util.Comparator;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.SortedSet;

import org.hibernate.Criteria;
import org.hibernate.SessionFactory;
import org.hibernate.classic.Session;
import org.hibernate.criterion.Conjunction;
import org.hibernate.criterion.Criterion;
import org.hibernate.criterion.Disjunction;
import org.hibernate.criterion.Restrictions;

import com.raytheon.uf.common.dataplugin.grid.GridInfoRecord;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.edex.database.plugin.PluginDao;
import com.raytheon.uf.edex.database.plugin.PluginFactory;
import com.raytheon.uf.edex.ogc.common.OgcException;
import com.raytheon.uf.edex.ogc.common.OgcException.Code;
import com.raytheon.uf.edex.ogc.common.OgcLayer;
import com.raytheon.uf.edex.ogc.common.db.LayerTransformer;
import com.raytheon.uf.edex.ogc.common.db.SimpleDimension;
import com.raytheon.uf.edex.ogc.common.level.LevelDimUtil;
import com.raytheon.uf.edex.ogc.common.time.ForecastTimeUtil;
import com.raytheon.uf.edex.plugin.dataset.urn.CFNameLookup;

/**
 * 
 * @author bclement
 * @version 1.0
 */
public class GribRecordFinder {

    public static final String INFO_ALIAS = "info_alias";

    public static final String LEVEL_ALIAS = "level_alias";

    public static final String MASTER_LEVEL_ALIAS = "master_level_alias";

    public static final String COV_ALIAS = "cov_alias";

    public static final String INFO = "info";

    public static final String LEVEL = "info_alias.level";

    public static final String LEVLE_ONE = LEVEL_ALIAS + ".levelonevalue";

    public static final String LEVLE_TWO = LEVEL_ALIAS + ".leveltwovalue";

    public static final String MASTER_LEVEL = LEVEL_ALIAS + ".masterLevel";

    public static final String LEVEL_UNIT = MASTER_LEVEL_ALIAS + ".unitString";

    public static final String LEVEL_NAME = MASTER_LEVEL_ALIAS + ".name";

    public static final String DS_NAME = INFO_ALIAS + ".datasetId";

    public static final String COVERAGE = INFO_ALIAS + ".location";

    public static final String COVERAGE_NAME = COV_ALIAS + ".name";

    public static final String PARAM = INFO_ALIAS + ".parameter";

    public static final String PARAM_ALIAS = "param_alias";

    public static final String PARAM_ABBV = PARAM_ALIAS + ".abbreviation";

    public static final String REF_TIME = "dataTime.refTime";

    public static final String FCST_TIME = "dataTime.fcstTime";

    public static class Comp implements Comparator<GridRecord> {
        /*
         * (non-Javadoc)
         * 
         * @see java.util.Comparator#compare(java.lang.Object, java.lang.Object)
         */
        @Override
        public int compare(GridRecord left, GridRecord right) {
            Date rightRef = right.getDataTime().getRefTime();
            Date leftRef = left.getDataTime().getRefTime();
            int rval = rightRef.compareTo(leftRef);
            if (rval == 0) {
                GridInfoRecord leftInfo = left.getInfo();
                GridInfoRecord rightInfo = right.getInfo();
                // FIXME this doesn't take units into account
                Level leftLevel = leftInfo.getLevel();
                Level rightLevel = rightInfo.getLevel();
                rval = (int) (leftLevel.getLevelonevalue() - rightLevel
                        .getLevelonevalue());
                if (rval == 0) {
                    if (leftInfo.getEnsembleId() != null) {
                        if (rightInfo.getEnsembleId() != null) {
                            rval = leftInfo.getEnsembleId().compareTo(
                                    rightInfo.getEnsembleId());
                        } else {
                            rval = 1;
                        }
                    } else {
                        if (rightInfo.getEnsembleId() != null) {
                            rval = -1;
                        } else {
                            rval = 0;
                        }
                    }
                }
            }
            return rval;
        }
    }

    public static List<GridRecord> findWms(
            LayerTransformer<GribDimension, GridParamLayer> transformer,
            String key, String layerName, Date time,
            Map<String, String> dimensions) throws OgcException {
        GridParamLayer l = getLayer(transformer, layerName);
        SortedSet<DataTime> times = new ForecastTimeUtil().getDataTimes(l,
                time, dimensions);
        return findInternal(l, transformer, key, times, dimensions,
                parseWmsId(layerName));
    }

    public static List<GridRecord> findWms(
            LayerTransformer<GribDimension, GridParamLayer> transformer,
            String key, String layerName, String time,
            Map<String, String> dimensions) throws OgcException {
        GridParamLayer l = getLayer(transformer, layerName);
        SortedSet<DataTime> times = new ForecastTimeUtil().getDataTimes(l,
                time, dimensions);
        return findInternal(l, transformer, key, times, dimensions,
                parseWmsId(layerName));
    }

    protected static List<GridRecord> findInternal(GridParamLayer l,
            LayerTransformer<GribDimension, GridParamLayer> transformer,
            String key, SortedSet<DataTime> times,
            Map<String, String> dimensions, Criterion idCrit)
            throws OgcException {
        Criterion levelCrit = getLevel(dimensions, l);
        String param = ogcToDbParameter(l.getParameter());
        // TODO ensure consistency in which level gets returned
        // TODO add support for more dimensions
        return query(key, times, param, idCrit, levelCrit);
    }

    public static GridParamLayer getLayer(
            LayerTransformer<GribDimension, GridParamLayer> transformer,
            String layerName) throws OgcException {
        GridParamLayer rval;
        try {
            rval = transformer.find(layerName);
        } catch (OgcException e) {
            throw new OgcException(Code.InternalServerError, e);
        }
        if (rval == null) {
            throw new OgcException(Code.LayerNotDefined);
        }
        return rval;
    }

    protected static String getIntDim(Map<String, String> dimensions,
            String key, GribLayer layer) throws OgcException {
        String rval = getDim(dimensions, key, layer);
        if (rval != null) {
            try {
                Integer i = Integer.parseInt(rval);
                rval = i.toString();
            } catch (Exception e) {
                throw new OgcException(Code.InvalidDimensionValue, key
                        + " must be a bare integer");
            }
        }
        return rval;
    }

    protected static String getDim(Map<String, String> dimensions, String key,
            GribLayer layer) {
        String rval = dimensions.get(key);
        if (rval == null) {
            SimpleDimension dim = layer.getDimension(key);
            rval = dim.getDefaultValue(layer);
        }
        return rval;
    }

    /**
     * @param dimensions
     * @param l
     * @return null if level value not in dimensions and not in layer
     * @throws OgcException
     */
    protected static Criterion getLevel(Map<String, String> dimensions,
            GribLayer layer) throws OgcException {
        String dimName = null;
        // find level dimension in request
        for (String dim : dimensions.keySet()) {
            String lower = dim.toLowerCase();
            if (lower.startsWith(LevelDimUtil.LEVEL_DIM_PREFIX.toLowerCase())) {
                dimName = dim;
                break;
            }
        }
        SimpleDimension dim;
        String value;
        if (dimName == null) {
            return null;
        } else {
            dim = layer.getDimension(dimName);
            if (dim == null) {
                return null;
            }
            value = dimensions.get(dimName);
        }
        // undo any case changes
        dimName = dim.getName();
        return parseLevel(dimName, value, dim.getUnits());
    }

    @SuppressWarnings("unchecked")
    protected static List<GridRecord> query(String key,
            SortedSet<DataTime> times, String param, Criterion idCrit,
            Criterion levelCrit) throws OgcException {
        Session sess = null;
        try {
            PluginDao dao = PluginFactory.getInstance().getPluginDao(key);
            SessionFactory sessFact = dao.getSessionFactory();
            sess = sessFact.openSession();
            Criteria criteria = sess.createCriteria(GridRecord.class);
            criteria.setResultTransformer(Criteria.DISTINCT_ROOT_ENTITY);
            Disjunction or = Restrictions.disjunction();
            Criterion paramCrit = Restrictions.eq(PARAM_ABBV, param);
            for (DataTime dt : times) {
                Conjunction and = Restrictions.conjunction();
                and.add(idCrit).add(paramCrit);
                if (levelCrit != null) {
                    and.add(levelCrit);
                }
                and.add(getTimeCrit(dt));
                or.add(and);
            }
            criteria.add(or);
            modCriteria(criteria);
            List<GridRecord> res = (List<GridRecord>) criteria.list();
            return res;
        } catch (Exception e) {
            throw new OgcException(Code.InternalServerError);
        } finally {
            if (sess != null) {
                sess.close();
            }
        }
    }

    /**
     * @param dt
     * @return
     */
    private static Criterion getTimeCrit(DataTime dt) {
        return Restrictions.and(Restrictions.eq(FCST_TIME, dt.getFcstTime()),
                Restrictions.eq(REF_TIME, dt.getRefTime()));
    }

    /**
     * @param dimName
     * @param value
     * @param defaultUnits
     * @return
     * @throws OgcException
     */
    public static Criterion parseLevel(String dimName, String value,
            String defaultUnits) throws OgcException {
        Level level = LevelDimUtil.parseLevel(dimName, value, defaultUnits);
        if (level == null) {
            return Restrictions.eq(LEVEL_NAME,
                    dimName.substring(LevelDimUtil.LEVEL_DIM_PREFIX.length()));
        }
        Criterion nameEq = Restrictions.eq(LEVEL_NAME, level.getMasterLevel()
                .getName());
        Conjunction and = Restrictions.conjunction();
        and.add(nameEq);
        and.add(Restrictions.eq(LEVLE_ONE, level.getLevelonevalue()));
        and.add(Restrictions.eq(LEVLE_TWO, level.getLeveltwovalue()));

        String units = level.getMasterLevel().getUnitString();
        if (units != null) {
            and.add(Restrictions.eq(LEVEL_UNIT, units));
        } else {
            and.add(Restrictions.isNull(LEVEL_UNIT));
        }

        return and;
    }

    /**
     * @param id
     * @return
     * @throws OgcException
     */
    public static Criterion parseWcsId(String id) throws OgcException {
        String[] parts = id.split(OgcLayer.keySeparator);
        String model;
        String coverage;
        String level;
        if (parts.length < 3) {
            throw new OgcException(Code.InvalidFormat, "Invalid id format");
        } else {
            level = parts[2];
            coverage = parts[1];
            model = parts[0];
        }
        Conjunction and = Restrictions.conjunction();
        and.add(Restrictions.eq(DS_NAME, model));
        and.add(Restrictions.eq(COVERAGE_NAME, coverage));
        and.add(Restrictions.eq(LEVEL_NAME, level));
        return and;
    }

    /**
     * @param id
     * @return
     * @throws OgcException
     */
    public static Criterion parseWmsId(String id) throws OgcException {
        String[] parts = id.split(OgcLayer.keySeparator);
        String model;
        String coverage;
        String level;
        String param;
        if (parts.length < 4) {
            throw new OgcException(Code.InvalidFormat, "Invalid id format");
        } else {
            level = parts[3];
            coverage = parts[1];
            model = parts[0];
            param = ogcToDbParameter(parts[2]);
        }
        Conjunction and = Restrictions.conjunction();
        and.add(Restrictions.eq(DS_NAME, model));
        and.add(Restrictions.eq(COVERAGE_NAME, coverage));
        and.add(Restrictions.eq(LEVEL_NAME, level));
        and.add(Restrictions.eq(PARAM_ABBV, param));
        return and;
    }

    /**
     * Add grid aliases to criteria
     * 
     * @param criteria
     * @return
     */
    public static Criteria modCriteria(Criteria criteria) {
        criteria = criteria.createAlias(INFO, INFO_ALIAS);
        criteria = criteria.createAlias(PARAM, PARAM_ALIAS);
        criteria = criteria.createAlias(LEVEL, LEVEL_ALIAS);
        criteria = criteria.createAlias(MASTER_LEVEL, MASTER_LEVEL_ALIAS);
        return criteria.createAlias(COVERAGE, COV_ALIAS);
    }

    /**
     * Convert OGC parameter name to name stored in database
     * 
     * @param parameter
     * @return
     */
    public static String ogcToDbParameter(String parameter) {
        return CFNameLookup.getInstance().getNCEPFromCF(parameter);
    }

    /**
     * Convert parameter name stored in database to OGC name
     * 
     * @param parameter
     * @return
     */
    public static String dbToOgcParameter(String parameter) {
        return CFNameLookup.getInstance().getCFFromNCEP(parameter);
    }
}
