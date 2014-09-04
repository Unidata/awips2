/**
 * This code has unlimited rights, and is provided "as is" by the National Centers 
 * for Environmental Prediction, without warranty of any kind, either expressed or implied, 
 * including but not limited to the implied warranties of merchantability and/or fitness 
 * for a particular purpose.
 * 
 * This code has been developed by the NCEP-SIB for use in the AWIPS2 system.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * -------		------- 	-------- 	-----------
 * 05/30/2013				Chin J. Chen	Initial coding
 *
 * </pre>
 * 
 * @author Chin J. Chen
 * @version 1.0
 */
package gov.noaa.nws.ncep.common.dataplugin.gpd.dao;

import gov.noaa.nws.ncep.common.dataplugin.gpd.GenericPointDataConstants;
import gov.noaa.nws.ncep.common.dataplugin.gpd.GenericPointDataRecord;
import gov.noaa.nws.ncep.common.dataplugin.gpd.product.GenericPointDataLevel;
import gov.noaa.nws.ncep.common.dataplugin.gpd.product.GenericPointDataParameter;
import gov.noaa.nws.ncep.common.dataplugin.gpd.product.GenericPointDataProductContainer;
import gov.noaa.nws.ncep.common.dataplugin.gpd.product.GenericPointDataProductInfo;
import gov.noaa.nws.ncep.common.dataplugin.gpd.product.GenericPointDataStationProduct;
import gov.noaa.nws.ncep.common.dataplugin.gpd.query.GenericPointDataQuery;
import gov.noaa.nws.ncep.common.dataplugin.gpd.query.GenericPointDataReqMsg.GenericPointDataQueryKey;
import gov.noaa.nws.ncep.common.dataplugin.gpd.query.GenericPointDataReqMsg.GenericPointDataReqType;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingStnInfo;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingStnInfoCollection;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingTimeLines;

import java.io.File;
import java.sql.Timestamp;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.xml.bind.JAXBException;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.hibernate.Criteria;
import org.hibernate.Session;
import org.hibernate.Transaction;
import org.hibernate.criterion.Criterion;
import org.hibernate.criterion.Restrictions;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.level.MasterLevel;
import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.common.parameter.Parameter;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.pointdata.PointDataDescription;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.common.pointdata.spatial.ObStation;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.purge.PurgeLogger;
import com.raytheon.uf.edex.database.query.DatabaseQuery;
import com.raytheon.uf.edex.pointdata.PointDataPluginDao;

public class GenericPointDataDao extends
        PointDataPluginDao<GenericPointDataRecord> {

    private Log logger = LogFactory.getLog(getClass());

    private PointDataDescription pdd;

    private SimpleDateFormat hdfFileDateFormat, dbRefTimeFormat;

    public GenericPointDataDao(String pluginName) throws PluginException {
        super(pluginName);
        hdfFileDateFormat = new SimpleDateFormat("-yyyy-MM-dd-HH-mm");
        dbRefTimeFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
    }

    @Override
    public String[] getKeysRequiredForFileName() {
        return new String[] { GenericPointDataConstants.DB_REF_TIME,
                GenericPointDataConstants.DB_PROD_NAME,
                GenericPointDataConstants.DB_MASTER_LEVEL_NAME };
    }

    @Override
    public GenericPointDataRecord newObject() {
        return new GenericPointDataRecord();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.pointdata.PointDataPluginDao#getPointDataFileName
     * (com.raytheon.uf.common.dataplugin.PluginDataObject) Chin: in GPD
     * implementation this is only called from Purge
     */
    @Override
    public String getPointDataFileName(GenericPointDataRecord p) {
        Date refTime = (p.getDataTime().getRefTime());
        int forecasttime = p.getDataTime().getFcstTime();
        String dateStr = hdfFileDateFormat.format(refTime) + "-f"
                + forecasttime;
        // System.out.println("gpd getPointDataFileName1 called and return: "+"gpd-"+p.getProductInfo().getName()+dateStr+/*"-"+p.getReportType().getMasterLevel().getName()+*/".h5");
        return "gpd-" + p.getProductInfo().getName() + dateStr + ".h5";
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.pointdata.PointDataPluginDao#getPointDataFileName
     * (java.util.Map) return a full HDF5 point data file name (including path)
     * Chin:::This is called when requesting data Currently one report is
     * defined with only one master level, therefore file path will be like
     * this.
     * /awips2/edex/data/hdf5/gpd/productName/gpd-productName-yyyy-mm-dd-HH
     * -MM.h5 E.g. /awips2/edex/data/hdf5/gpd/sib1/gpd-sib1-2013-05-08-19-00.h5
     * Note:::future ??? If, we want to defined more than one master level for
     * one report type, then file path will like this,
     * /awips2/edex/data/hdf5/gpd
     * /reportName/masterLevelName/gpd-reportName-masterLevelName
     * -yyyy-mm-dd-HH-MM.h5 E.g.
     * /awips2/edex/data/hdf5/gpd/sib1/MB/gpd-sib1-MB-2013-05-08-19-00.h5
     */
    @Override
    public String getPointDataFileName(Map<String, Object> dbResults) {
        String reportname = (String) dbResults
                .get(GenericPointDataConstants.DB_PROD_NAME);
        int forecasttime = (Integer) dbResults
                .get(GenericPointDataConstants.DB_FORECAST_TIME);
        String dateStr = hdfFileDateFormat.format(dbResults
                .get(GenericPointDataConstants.DB_REF_TIME))
                + "-f"
                + forecasttime;
        String filename = PLUGIN_HDF5_DIR + reportname + File.separator
                // + lmName + File.separator
                + this.pluginName + "-" + reportname + /* "-"+lmName+ */dateStr
                + ".h5";
        // System.out.println("GenericPointDataDao getPointDataFileName2 called and return: "+
        // filename);

        return filename;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.pointdata.PointDataPluginDao#getFullFilePath(com
     * .raytheon.uf.common.dataplugin.PluginDataObject) return a full HDF5 point
     * data file name (including path) This is called when decoding data
     */
    @Override
    public File getFullFilePath(PluginDataObject persistable) {
        File file;
        GenericPointDataRecord rec = (GenericPointDataRecord) persistable;
        String directory = PLUGIN_HDF5_DIR /*
                                            * + File.separator +
                                            * persistable.getPluginName() +
                                            * File.separator
                                            */
                + rec.getProductInfo().getName();

        Date refTime = ((PluginDataObject) persistable).getDataTime()
                .getRefTime();
        int forecasttime = ((PluginDataObject) persistable).getDataTime()
                .getFcstTime();
        String dateStr = hdfFileDateFormat.format(refTime) + "-f"
                + forecasttime;
        String fileName = persistable.getPluginName() + "-"
                + rec.getProductInfo().getName() + /*
                                                    * "-"+rec.getReportType().
                                                    * getMasterLevel
                                                    * ().getName()+
                                                    */dateStr + ".h5";
        file = new File(directory + File.separator + fileName);
        // System.out.println("GenericPointDataDao getFullFilePath return "+
        // file.getPath()+" "+ file.getName());
        return file;
    }

    /*
     * This function is for development testing.. not used in production code
     */
    public PointDataDescription getPointDataDescription() throws JAXBException {
        if (pdd == null) {
            try {
                pdd = PointDataDescription.fromStream(this.getClass()
                        .getResourceAsStream("/res/pointdata/gpd.xml"));
            } catch (SerializationException e) {
                // TODO Auto-generated catch block. Please revise as
                // appropriate.
                // statusHandler.handle(Priority.PROBLEM,
                // e.getLocalizedMessage(), e);
            }
        }
        return pdd;
    }

    // look up target location in gpd_location table. If not present and if
    // createLocation = TRUE, insert it to table.
    public boolean lookupGpdLocation(ObStation location, boolean createLocation) {
        boolean status = true;
        Session sess = null;
        Transaction trans = null;
        try {
            sess = getSessionFactory().openSession();
            trans = sess.beginTransaction();

            Criteria crit = sess.createCriteria(ObStation.class);

            Criterion nameCrit = Restrictions.eq("stationId",
                    location.getStationId());
            crit.add(nameCrit);
            Criterion nameCrit1 = Restrictions.eq("catalogType",
                    location.getCatalogType());
            crit.add(nameCrit1);
            // querying...
            List<?> vals = crit.list();

            if (vals.size() <= 0) {
                // not in database
                if (createLocation) {
                    sess.saveOrUpdate(location);
                    trans.commit();
                } else {
                    status = false;
                }
            }

        } catch (Exception e) {
            logger.error("lookupGpdLocation:Error occurred looking up ["
                    + location.getStationId() + "]", e);
            status = false;
            if (trans != null) {
                try {
                    trans.rollback();
                } catch (Exception e1) {
                    logger.error(
                            "lookupGpdLocation: Error occurred rolling back transaction",
                            e);
                }
            }
        } finally {
            if (sess != null) {
                try {
                    sess.close();
                } catch (Exception e) {
                    logger.error(
                            "lookupGpdLocation: Error occurred closing session",
                            e);
                }
            }
        }

        return status;
    }

    // look up target parm in parameter table. If not present and if createParm
    // = TRUE, insert it to table.
    public boolean lookupParameter(Parameter parm, boolean createParm) {
        boolean status = true;
        Session sess = null;
        Transaction trans = null;
        try {
            sess = getSessionFactory().openSession();
            trans = sess.beginTransaction();

            Criteria crit = sess.createCriteria(Parameter.class);

            Criterion nameCrit = Restrictions.eq("abbreviation",
                    parm.getAbbreviation());
            crit.add(nameCrit);
            // querying...
            List<?> vals = crit.list();
            if (vals.size() <= 0) {
                if (createParm) {
                    sess.saveOrUpdate(parm);
                    trans.commit();
                } else {
                    status = false;
                }
            }

        } catch (Exception e) {
            logger.error("lookupParameter: Error occurred looking up parm["
                    + parm.getAbbreviation() + "]", e);
            status = false;
            if (trans != null) {
                try {
                    trans.rollback();
                } catch (Exception e1) {
                    logger.error(
                            "lookupParameter: Error occurred rolling back transaction",
                            e);
                }
            }
        } finally {
            if (sess != null) {
                try {
                    sess.close();
                } catch (Exception e) {
                    logger.error(
                            "lookupParameter: Error occurred closing session",
                            e);
                }
            }
        }

        return status;
    }

    // look up target masterLvl in level_master table. If not present and if
    // createMl = TRUE, insert it to table.
    public boolean lookupMasterlevel(MasterLevel masterLvl, boolean createMl) {
        boolean status = true;
        Session sess = null;
        Transaction trans = null;
        try {
            sess = getSessionFactory().openSession();
            trans = sess.beginTransaction();

            Criteria crit = sess.createCriteria(Parameter.class);

            Criterion nameCrit = Restrictions.eq("name", masterLvl.getName());
            crit.add(nameCrit);
            // querying...
            List<?> vals = crit.list();
            if (vals.size() <= 0) {
                if (createMl) {
                    sess.saveOrUpdate(masterLvl);
                    trans.commit();
                } else {
                    status = false;
                }
            }

        } catch (Exception e) {
            logger.error("lookupMasterlevel: Error occurred looking up parm["
                    + masterLvl.getName() + "]", e);
            status = false;
            if (trans != null) {
                try {
                    trans.rollback();
                } catch (Exception e1) {
                    logger.error(
                            "lookupMasterlevel: Error occurred rolling back transaction",
                            e);
                }
            }
        } finally {
            if (sess != null) {
                try {
                    sess.close();
                } catch (Exception e) {
                    logger.error(
                            "lookupMasterlevel: Error occurred closing session",
                            e);
                }
            }
        }

        return status;
    }

    private boolean mergeProductInfoParameterList(
            GenericPointDataProductInfo targetProdInfo,
            GenericPointDataProductInfo sourceProdInfo) {
        List<Parameter> srcParmList = sourceProdInfo.getParameterLst();
        List<Parameter> tarParmList = targetProdInfo.getParameterLst();
        boolean merged = false;
        /*
         * for(Parameter p: srcParmList){
         * System.out.println("srcA p="+p.getAbbreviation()); } for(Parameter p:
         * tarParmList){ System.out.println("tarA p="+p.getAbbreviation()); }
         */
        for (int index = srcParmList.size() - 1; index >= 0; index--) {
            Parameter pm = srcParmList.get(index);
            boolean found = false;
            for (Parameter p : tarParmList) {
                if (p.getAbbreviation().equals(pm.getAbbreviation())) {
                    found = true;
                    break;
                }
            }
            if (found == false) {
                Parameter newPm = srcParmList.remove(index);
                tarParmList.add(newPm);
                merged = true;
                // System.out.println("not found add newPM="+
                // newPm.getAbbreviation());
            }
            /*
             * else { System.out.println("found srcP="+pm.getAbbreviation()); }
             */
        }
        // System.out.println("merged = "+ merged);
        /*
         * for(Parameter p: srcParmList){
         * System.out.println("srcB p="+p.getAbbreviation()); } for(Parameter p:
         * tarParmList){ System.out.println("tarB p="+p.getAbbreviation()); }
         */
        return merged;
    }

    // 1. look up target product in gpd_productinfo table. If not present and if
    // createReport = TRUE, insert it to table.
    // 2. A complete product is returned using the contents found in DB, as user
    // may just use product name in input XML file for
    // decoding request, when a same product information had been already saved
    // in DB earlier.
    // 3. If prodInfo contains new parameters, if version number is 0, than
    // update DB, otherwise, reject the update
    // 4. copy input prod's "number of level" to return prodInfo
    public GenericPointDataProductInfo lookupUpdateGpdProdInfo(
            GenericPointDataProductInfo prod, boolean createProd,
            int prodVersion) {
        GenericPointDataProductInfo returnProdInfo = null;
        boolean status = true;
        Session sess = null;
        Transaction trans = null;
        int maxnumLvl = prod.getMaxNumberOfLevel();
        try {
            sess = getSessionFactory().openSession();
            trans = sess.beginTransaction();

            Criteria crit = sess
                    .createCriteria(GenericPointDataProductInfo.class);

            Criterion nameCrit = Restrictions.eq("name", prod.getName());
            crit.add(nameCrit);
            // query
            List<?> vals = crit.list();
            if (vals.size() > 0) {
                // the product is already in DB
                GenericPointDataProductInfo dbProdInfo = (GenericPointDataProductInfo) vals
                        .get(0);
                // check to see if there are new parameters, and merge then to
                // parameter list in dbProdInfo
                boolean merged = mergeProductInfoParameterList(dbProdInfo, prod);
                if (merged == true) {
                    // if there are new parameters and product version number is
                    // 0, then update product to DB
                    if (prodVersion == 0) {
                        for (Parameter pm : dbProdInfo.getParameterLst()) {
                            if (lookupParameter(pm, true) == false) {
                                break;
                            }
                        }
                        sess.saveOrUpdate(dbProdInfo);
                        trans.commit();
                        // TBD...do we need clone it?
                        returnProdInfo = dbProdInfo;// .clone();
                    }
                    // prod version > 0, disallow update prod info
                } else {
                    // TBD...do we need clone it?
                    returnProdInfo = dbProdInfo;
                }

            } else if (createProd) {
                for (Parameter pm : prod.getParameterLst()) {
                    if (lookupParameter(pm, true) == false) {
                        status = false;
                        break;
                    }
                }
                if (status == true
                        && lookupMasterlevel(prod.getMasterLevel(), true) == false) {
                    status = false;
                }
                if (status) {
                    sess.saveOrUpdate(prod);
                    returnProdInfo = prod;
                    trans.commit();
                }
            } else
                status = false;
        } catch (Exception e) {
            logger.error(
                    "lookupGpdReportType: Error occurred looking up GenericPointDataReporttype["
                            + prod.getName() + "]", e);
            status = false;
            if (trans != null) {
                try {
                    trans.rollback();
                } catch (Exception e1) {
                    logger.error(
                            "lookupGpdReportType: Error occurred rolling back transaction",
                            e);
                }
            }
        } finally {
            if (sess != null) {
                try {
                    sess.close();
                } catch (Exception e) {
                    logger.error(
                            "lookupGpdReportType: Error occurred closing session",
                            e);
                }
            }
        }
        returnProdInfo.setMaxNumberOfLevel(maxnumLvl);
        return returnProdInfo;
    }

    /*
     * To create or update product meta data information
     */
    public GenericPointDataProductInfo updateProductInfo(
            GenericPointDataProductInfo prod) {
        Session sess = null;
        boolean status = true;
        Transaction trans = null;
        GenericPointDataProductInfo rval = null;
        try {
            sess = getSessionFactory().openSession();
            trans = sess.beginTransaction();

            for (Parameter pm : prod.getParameterLst()) {
                if (lookupParameter(pm, true) == false) {
                    status = false;
                    break;
                }
            }
            if (status == true
                    && lookupMasterlevel(prod.getMasterLevel(), true) == false) {
                status = false;
            }
            if (status) {
                sess.saveOrUpdate(prod);

                Criteria crit = sess
                        .createCriteria(GenericPointDataProductInfo.class);

                Criterion nameCrit = Restrictions.eq("name", prod.getName());
                crit.add(nameCrit);
                List<?> vals = crit.list();
                if (vals.size() > 0) {
                    rval = ((GenericPointDataProductInfo) vals.get(0)).clone();
                    System.out
                            .println("updateProductInfo: new parameter array size="
                                    + rval.getParameterLst().size());
                }

                trans.commit();
            }

        } catch (Exception e) {
            logger.error(
                    "updateProductInfo: Error occurred looking up product ["
                            + prod.getName() + "]", e);

            if (trans != null) {
                try {
                    trans.rollback();
                } catch (Exception e1) {
                    logger.error(
                            "updateProductInfo: Error occurred rolling back transaction",
                            e);
                }
            }
        } finally {
            if (sess != null) {
                try {
                    sess.close();
                } catch (Exception e) {
                    logger.error(
                            "updateProductInfo: Error occurred closing session",
                            e);
                }
            }
        }
        return rval;

    }

    /*
     * TBM...Chin delet this leter...not used public boolean
     * lookupGpdDataUri(String dataUri) { boolean status= true; Session sess =
     * null; Transaction trans = null; try { sess =
     * getSessionFactory().openSession(); trans = sess.beginTransaction();
     * 
     * Criteria crit = sess.createCriteria(GenericPointDataRecord.class);
     * 
     * Criterion nameCrit = Restrictions.eq("dataURI", dataUri);
     * crit.add(nameCrit); //querying... List<?> vals = crit.list();
     * 
     * if (vals.size() <= 0) { status = false; } } catch (Exception e) {
     * logger.error(
     * "lookupGpdLocation:Error occurred looking up lookupGpdDataUri[" + dataUri
     * + "]", e); status = false; if (trans != null) { try { trans.rollback(); }
     * catch (Exception e1) {
     * logger.error("lookupGpdDataUri: Error occurred rolling back transaction",
     * e); } } } finally { if (sess != null) { try { sess.close(); } catch
     * (Exception e) {
     * logger.error("lookupGpdDataUri: Error occurred closing session", e); } }
     * }
     * 
     * return status; }
     */
    public GenericPointDataProductInfo getGpdProdInfo(String prodName) {
        GenericPointDataProductInfo rval = null;
        if (prodName != null) {
            Session sess = null;
            sess = getSessionFactory().openSession();
            sess.beginTransaction();

            Criteria crit = sess
                    .createCriteria(GenericPointDataProductInfo.class);

            Criterion nameCrit = Restrictions.eq("name", prodName);
            crit.add(nameCrit);
            List<?> vals = crit.list();
            if (vals.size() > 0) {
                try {
                    // to avoid LazyInitializationException, we have to take
                    // care of Collection before
                    // closing session. Therefore, clone() it.
                    rval = ((GenericPointDataProductInfo) vals.get(0)).clone();
                } catch (CloneNotSupportedException e) {
                    e.printStackTrace();
                }
            }
            if (sess != null) {
                try {
                    sess.close();
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }
        }
        return rval;
    }

    /*
     * Chin:::: Get point data container for one or multiple stations. Based on
     * queryKey. If BY_STN_ID, then stnId required, and is querying one station
     * If BY_SLAT_SLON, then slat/slon required, and is querying one station If
     * BY_REPORT_NAME, then both not required, and is querying all stations that
     * meet prodInfo and refTime query constraints
     */
    private PointDataContainer getPointDataContainer(Date refTime,
            GenericPointDataQueryKey quertKey, String stnId, float slat,
            float slon, GenericPointDataProductInfo prodInfo, int productVersion)
            throws Exception {
        return (getPointDataContainer(refTime, null, quertKey, stnId, slat,
                slon, prodInfo, productVersion));
    }

    private PointDataContainer getPointDataContainer(Date refTime,
            Date rangeStartTime, GenericPointDataQueryKey quertKey,
            String stnId, float slat, float slon,
            GenericPointDataProductInfo prodInfo, int productVersion)
            throws Exception {
        String prodName = prodInfo.getName();

        PointDataContainer pdc = null;

        GenericPointDataQuery pdq = new GenericPointDataQuery("gpd");
        StringBuilder returnParametersString = new StringBuilder();
        /*
         * add return fields for both DB and HDF5
         */
        // 1st:: add return fields from HDF5. They are the parameter list
        // defined in a GPD report
        for (Parameter parm : prodInfo.getParameterLst()) {
            String parameter = parm.getAbbreviation();
            if (returnParametersString.length() > 0) {
                returnParametersString.append(",");
            }
            returnParametersString.append(parameter);
        }
        // also add the 3 HDF5 mandatory data sets
        returnParametersString.append(","
                + GenericPointDataConstants.HDF5_LEVEL_VALUE);
        returnParametersString.append(","
                + GenericPointDataConstants.HDF5_NUM_LEVEL);
        returnParametersString.append(","
                + GenericPointDataConstants.HDF5_STN_ID);
        // System.out.println("gpd dao hdf5 parameterlist="+returnParametersString.toString());

        // 2nd:: add return fields form DB. the parameter name need to be
        // defined in
        // gov.noaa.nws.ncep.edex.plugin.gpd/res/pointdata/gpddb.xmlquertKey
        // for example::
        returnParametersString.append(","
                + GenericPointDataConstants.DB_STN_CATALOGTYPE);
        returnParametersString.append("," + GenericPointDataConstants.DB_SLAT);
        returnParametersString.append("," + GenericPointDataConstants.DB_SLON);
        returnParametersString.append(","
                + GenericPointDataConstants.DB_UTILITY_FLAGS);
        returnParametersString.append(","
                + GenericPointDataConstants.DB_FORECAST_TIME);
        // parameters defined in
        // /gov.noaa.nws.ncep.edex.plugin.gpd/utility/common_static/base/path/gpdPathKeys.xml
        // AND those returned by dao.getKeysRequiredForFileName()
        // will be added automatically when calling PointDataQuery.execute()

        // PointDataQuery.setParameters() is to set return fields from both DB
        // and HDF5
        pdq.setParameters(returnParametersString.toString());

        // PointDataQuery.addParameter() is to add DB query constraints
        pdq.addParameter("productInfo.name", prodName, "=");
        if (quertKey == GenericPointDataQueryKey.BY_STN_ID)
            pdq.addParameter("location.stationId", stnId, "=");
        else if (quertKey == GenericPointDataQueryKey.BY_SLAT_SLON) {
            pdq.addParameter("slat", Float.toString(slat), "=");
            pdq.addParameter("slon", Float.toString(slon), "=");

        }

        String dateStr = dbRefTimeFormat.format(refTime);
        pdq.addParameter(GenericPointDataConstants.DB_REF_TIME, dateStr, "=");
        if (rangeStartTime != null) {
            String rangedateStr = dbRefTimeFormat.format(rangeStartTime);
            pdq.addParameter(GenericPointDataConstants.DB_RANGESTART_TIME,
                    rangedateStr, "=");
        }
        pdq.addParameter(GenericPointDataConstants.DB_PRODUCT_VERSION,
                Integer.toString(productVersion), "=");
        // System.out.println("requestig refTime = "+ dateStr);
        pdq.requestAllLevels();
        try {
            pdc = pdq.execute();
        } catch (StorageException e) {
            System.out.println("HDF5 query StorageException " + e);
        }

        return pdc;
    }

    /*
     * TBM...Chin delete this later...not used public
     * GenericPointDataProductContainer getGpdProduct(Date refTime, String
     * prodName,boolean useSpecifiedProductVersion, int productVersion)throws
     * Exception{ GenericPointDataProductInfo prodInfo = getGpdProdInfo(
     * prodName); if (prodInfo == null) {
     * System.out.println("report is not in DB"); return null; }
     * if(useSpecifiedProductVersion== false){ //find the latest version if
     * there is one. Otherwise, use user specified version number productVersion
     * = getGpdProductLatestVersion( refTime, prodName); } PointDataContainer
     * pdc = getPointDataContainer(
     * refTime,GenericPointDataQueryKey.BY_REPORT_NAME, null,0,0, prodInfo,
     * productVersion); if (pdc == null) { System.out.println("pdc is null");
     * return null; } System.out.println("pdc CurrentSz()="+pdc.getCurrentSz());
     * GenericPointDataProductContainer prodCon = new
     * GenericPointDataProductContainer(); prodCon.setProductInfo(prodInfo);
     * prodCon.setRefTime(refTime);
     * prodCon.setProductCorrectionVersion(productVersion); for (int i = 0; i <
     * pdc.getCurrentSz(); i++) { PointDataView pdv =pdc.readRandom(i);
     * System.out
     * .println("pdv#"+i+" *********************************************");
     * Set<String> parameters = new
     * HashSet<String>(pdv.getContainer().getParameters()); for(String parm:
     * parameters){ System.out.println("parm ="+parm); } String stnId= null; if
     * (parameters.contains(GenericPointDataConstants.HDF5_STN_ID)){ stnId =
     * pdv.getString(GenericPointDataConstants.HDF5_STN_ID);
     * System.out.println("stnid= "+ stnId); //stnId value is retrieved already,
     * so drop it here parameters.remove(GenericPointDataConstants.HDF5_STN_ID);
     * } else continue; //stnId not available, no need to continue on this PDV.
     * int numLevel=0; if
     * (parameters.contains(GenericPointDataConstants.HDF5_NUM_LEVEL)){ numLevel
     * = pdv.getInt(GenericPointDataConstants.HDF5_NUM_LEVEL);
     * System.out.println("numLevel= "+ numLevel); //numLevel value is retrieved
     * already, so drop it here
     * parameters.remove(GenericPointDataConstants.HDF5_NUM_LEVEL); } else
     * continue; //level number is 0, no need to continue on this PDV.
     * 
     * List<GenericPointDataLevel> levelList; if
     * (parameters.contains(GenericPointDataConstants.HDF5_LEVEL_VALUE)){
     * levelList = new ArrayList<GenericPointDataLevel>(numLevel); Number[] num
     * =
     * pdv.getNumberAllLevels(GenericPointDataConstants.HDF5_LEVEL_VALUE);//pdv
     * .getNumberAllLevels(parm,numLevel); for(Number n: num){
     * System.out.println("level value="+n.floatValue()); if(n.floatValue() ==
     * GenericPointDataConstants.GPD_INVALID_FLOAT_VALUE) //Not a valid level,
     * skip it continue; GenericPointDataLevel gpdLevel = new
     * GenericPointDataLevel(); gpdLevel.setLevelValue(n.floatValue());
     * levelList.add(gpdLevel); } //level value is retrieved already, so drop it
     * here parameters.remove(GenericPointDataConstants.HDF5_LEVEL_VALUE); }
     * else continue; //no level value, no need to continue on this PDV.
     * 
     * int stnCatalogType=ObStation.CAT_TYPE_MESONET; if
     * (parameters.contains(GenericPointDataConstants.DB_STN_CATALOGTYPE)){
     * stnCatalogType =
     * pdv.getInt(GenericPointDataConstants.DB_STN_CATALOGTYPE);
     * System.out.println("stnCatalogType= "+ stnCatalogType);
     * //DB_STN_CATALOGTYPE value is retrieved already, so drop it here
     * parameters.remove(GenericPointDataConstants.DB_STN_CATALOGTYPE); } float
     * slat = -9999; if
     * (parameters.contains(GenericPointDataConstants.DB_SLAT)){ slat =
     * pdv.getFloat(GenericPointDataConstants.DB_SLAT);
     * System.out.println("slat= "+ slat); //slat value is retrieved already, so
     * drop it here parameters.remove(GenericPointDataConstants.DB_SLAT); }
     * float slon = -9999; if
     * (parameters.contains(GenericPointDataConstants.DB_SLON)){ slon =
     * pdv.getFloat(GenericPointDataConstants.DB_SLON);
     * System.out.println("slon= "+ slon); //slon value is retrieved already, so
     * drop it here parameters.remove(GenericPointDataConstants.DB_SLON); }
     * //PDV id is not returned back to user, so drop it here
     * parameters.remove(GenericPointDataConstants.HDF5_PDV_ID);
     * 
     * GenericPointDataStationProduct stnPd= new
     * GenericPointDataStationProduct(); stnPd.setProductName(prodName);
     * stnPd.setRefTime(refTime); stnPd.setLevelLst(levelList);
     * stnPd.setProductVersion(productVersion);
     * stnPd.getLocation().setStationId(stnId);
     * stnPd.getLocation().setCatalogType(stnCatalogType);
     * stnPd.setNumLevel(numLevel); stnPd.setSlat(slat); stnPd.setSlon(slon);
     * for(String parm: parameters){ //these parameters are data parameters and
     * should be 2 dimensional float value per design //If a new "meta" data is
     * queried, then we should take care of that data specifically before here.
     * Number[] num = pdv.getNumberAllLevels(parm);//,numLevel);
     * System.out.println("parm ="+parm); for(int j=0; j< numLevel ; j++){
     * Number n = num[j]; System.out.println(" value="+n.floatValue());
     * GenericPointDataParameter gpdParm = new
     * GenericPointDataParameter(parm,n.floatValue());
     * levelList.get(j).getGpdParameters().add(gpdParm); } }
     * prodCon.getStnProdLst().add(stnPd); } return prodCon; }
     */
    public GenericPointDataProductContainer getGpdProduct(Date refTime,
            GenericPointDataQueryKey key, String stnId, float slat, float slon,
            String prodName, boolean useSpecifiedProductVersion,
            int productVersion) throws Exception {
        long t01 = System.currentTimeMillis();
        GenericPointDataProductInfo prodInfo = getGpdProdInfo(prodName);
        if (prodInfo == null) {
            System.out.println("report is not in DB");
            return null;
        }
        if (useSpecifiedProductVersion == false) {
            // find the latest version if there is one. Otherwise, use user
            // specified version number
            productVersion = getGpdProductLatestVersion(refTime, prodName);
            if (productVersion < 0)
                return null;
        }
        PointDataContainer pdc = getPointDataContainer(refTime, key, stnId,
                slat, slon, prodInfo, productVersion);
        if (pdc == null) {
            System.out.println("pdc is null");
            return null;
        }
        // System.out.println("pdc CurrentSz()="+pdc.getCurrentSz());
        GenericPointDataProductContainer prodCon = new GenericPointDataProductContainer();
        prodCon.setProductInfo(prodInfo);
        prodCon.setRefTime(refTime);
        prodCon.setProductCorrectionVersion(productVersion);
        for (int i = 0; i < pdc.getCurrentSz(); i++) {
            PointDataView pdv = pdc.readRandom(i);
            // System.out.println("pdv#"+i+" *********************************************");
            Set<String> parameters = new HashSet<String>(pdv.getContainer()
                    .getParameters());
            /*
             * for(String parm: parameters){ System.out.println("parm ="+parm);
             * }
             */
            int numLevel = 0;
            if (parameters.contains(GenericPointDataConstants.HDF5_NUM_LEVEL)) {
                numLevel = pdv.getInt(GenericPointDataConstants.HDF5_NUM_LEVEL);
                // System.out.println("numLevel= "+ numLevel);
                // numLevel value is retrieved already, so drop it here
                parameters.remove(GenericPointDataConstants.HDF5_NUM_LEVEL);
            } else
                continue; // level number is 0, no need to continue on this PDV.

            List<GenericPointDataLevel> levelList;
            if (parameters.contains(GenericPointDataConstants.HDF5_LEVEL_VALUE)) {
                levelList = new ArrayList<GenericPointDataLevel>(numLevel);
                if (numLevel > 1) {
                    Number[] num = pdv
                            .getNumberAllLevels(GenericPointDataConstants.HDF5_LEVEL_VALUE);// pdv.getNumberAllLevels(parm,numLevel);
                    int count = 0;
                    for (Number n : num) {
                        count++;
                        if (count > numLevel)
                            break;
                        // System.out.println("Level " +count+
                        // " value="+n.floatValue());
                        GenericPointDataLevel gpdLevel = new GenericPointDataLevel();
                        gpdLevel.setLevelValue(n.floatValue());
                        levelList.add(gpdLevel);
                    }
                } else {
                    GenericPointDataLevel gpdLevel = new GenericPointDataLevel();
                    gpdLevel.setLevelValue(pdv
                            .getFloat(GenericPointDataConstants.HDF5_LEVEL_VALUE));
                    levelList.add(gpdLevel);
                }
                // level value is retrieved already, so drop it here
                parameters.remove(GenericPointDataConstants.HDF5_LEVEL_VALUE);
            } else
                continue; // no level value, no need to continue on this PDV.

            int stnCatalogType = ObStation.CAT_TYPE_MESONET;
            if (parameters
                    .contains(GenericPointDataConstants.DB_STN_CATALOGTYPE)) {
                stnCatalogType = pdv
                        .getInt(GenericPointDataConstants.DB_STN_CATALOGTYPE);
                // System.out.println("stnCatalogType= "+ stnCatalogType);
                // DB_STN_CATALOGTYPE value is retrieved already, so drop it
                // here
                parameters.remove(GenericPointDataConstants.DB_STN_CATALOGTYPE);
            }
            String rtnstnId = stnId;
            if (parameters.contains(GenericPointDataConstants.HDF5_STN_ID)) {
                rtnstnId = pdv.getString(GenericPointDataConstants.HDF5_STN_ID);
                // System.out.println("stnId= "+ rtnstnId);
                // stnId is input parameter, can drop it here.
                parameters.remove(GenericPointDataConstants.HDF5_STN_ID);
            }
            float rtnslat = slat;
            if (parameters.contains(GenericPointDataConstants.DB_SLAT)) {
                rtnslat = pdv.getFloat(GenericPointDataConstants.DB_SLAT);
                // System.out.println("slat= "+ rtnslat);
                // slat value is retrieved already, so drop it here
                parameters.remove(GenericPointDataConstants.DB_SLAT);
            }
            float rtnslon = slon;
            if (parameters.contains(GenericPointDataConstants.DB_SLON)) {
                rtnslon = pdv.getFloat(GenericPointDataConstants.DB_SLON);
                // System.out.println("slon= "+ rtnslon);
                // slon value is retrieved already, so drop it here
                parameters.remove(GenericPointDataConstants.DB_SLON);
            }
            String utFlag = null;
            if (parameters.contains(GenericPointDataConstants.DB_UTILITY_FLAGS)) {
                utFlag = pdv
                        .getString(GenericPointDataConstants.DB_UTILITY_FLAGS);
                // System.out.println("utFlag= "+ utFlag);
                parameters.remove(GenericPointDataConstants.DB_UTILITY_FLAGS);
            }
            int forecastTime = 0;
            if (parameters.contains(GenericPointDataConstants.DB_FORECAST_TIME)) {
                forecastTime = pdv
                        .getInt(GenericPointDataConstants.DB_FORECAST_TIME);
                parameters.remove(GenericPointDataConstants.DB_FORECAST_TIME);
            }
            // PDV id is not returned back to user, so drop it here
            parameters.remove(GenericPointDataConstants.HDF5_PDV_ID);

            GenericPointDataStationProduct stnPd = new GenericPointDataStationProduct();
            stnPd.setProductName(prodName);
            stnPd.setRefTime(refTime);
            stnPd.setLevelLst(levelList);
            stnPd.setProductVersion(productVersion);
            stnPd.getLocation().setStationId(rtnstnId);
            stnPd.getLocation().setCatalogType(stnCatalogType);
            stnPd.setNumLevel(numLevel);
            stnPd.setSlat(rtnslat);
            stnPd.setSlon(rtnslon);
            stnPd.setForecastTime(forecastTime);
            stnPd.setUtilityFlag(utFlag);
            for (String parm : parameters) {
                if (numLevel > 1) {
                    // these parameters are data parameters and should be 2
                    // dimensional float value per design
                    // If a new "meta" data is queried, then we should take care
                    // of that data specifically before here.
                    Number[] num = pdv.getNumberAllLevels(parm);// ,numLevel);
                    // System.out.println("parm ="+parm);
                    for (int j = 0; j < numLevel; j++) {
                        Number n = num[j];
                        // System.out.println(" value="+n.floatValue());
                        GenericPointDataParameter gpdParm = new GenericPointDataParameter(
                                parm, n.floatValue());
                        levelList.get(j).getGpdParameters().add(gpdParm);
                    }
                } else {
                    // System.out.println("parm ="+parm);
                    GenericPointDataParameter gpdParm = new GenericPointDataParameter(
                            parm, pdv.getFloat(parm));
                    levelList.get(0).getGpdParameters().add(gpdParm);
                }
            }
            prodCon.getStnProdLst().add(stnPd);
        }

        long t02 = System.currentTimeMillis();
        System.out.println("ThrifClient: getGpdProduct()  took " + (t02 - t01)
                + " ms in total for query stn=" + stnId);
        return prodCon;
    }

    /*
	 * 
	 */
    public List<GenericPointDataStationProduct> getGpdStationProduct(
            List<Date> refTimeList, GenericPointDataQueryKey key, String stnId,
            float slat, float slon, String prodName) throws Exception {
        GenericPointDataProductInfo prodInfo = getGpdProdInfo(prodName);
        if (prodInfo == null) {
            System.out.println("product is not in DB");
            return null;
        }
        List<GenericPointDataStationProduct> stnProdList = new ArrayList<GenericPointDataStationProduct>();
        for (Date refTime : refTimeList) {
            int productVersion = getGpdProductLatestVersion(refTime, prodName);
            if (productVersion < 0)
                continue;
            PointDataContainer pdc = getPointDataContainer(refTime, key, stnId,
                    slat, slon, prodInfo, productVersion);
            if (pdc == null) {
                System.out.println("pdc is null");
                continue;
            }
            // System.out.println(refTime.toString()
            // +" pdc CurrentSz()="+pdc.getCurrentSz());

            for (int i = 0; i < pdc.getCurrentSz(); i++) {
                PointDataView pdv = pdc.readRandom(i);
                // System.out.println("pdv#"+i+" *********************************************");
                Set<String> parameters = new HashSet<String>(pdv.getContainer()
                        .getParameters());
                int numLevel = 0;
                if (parameters
                        .contains(GenericPointDataConstants.HDF5_NUM_LEVEL)) {
                    numLevel = pdv
                            .getInt(GenericPointDataConstants.HDF5_NUM_LEVEL);
                    // System.out.println("numLevel= "+ numLevel);
                    // numLevel value is retrieved already, so drop it here
                    parameters.remove(GenericPointDataConstants.HDF5_NUM_LEVEL);
                } else
                    continue; // level number is 0, no need to continue on this
                              // PDV.

                List<GenericPointDataLevel> levelList;
                if (parameters
                        .contains(GenericPointDataConstants.HDF5_LEVEL_VALUE)) {
                    levelList = new ArrayList<GenericPointDataLevel>(numLevel);
                    if (numLevel > 1) {
                        Number[] num = pdv
                                .getNumberAllLevels(GenericPointDataConstants.HDF5_LEVEL_VALUE);// pdv.getNumberAllLevels(parm,numLevel);
                        int count = 0;
                        for (Number n : num) {
                            count++;
                            if (count > numLevel)
                                break;
                            // System.out.println("Level " +count+
                            // " value="+n.floatValue());
                            GenericPointDataLevel gpdLevel = new GenericPointDataLevel();
                            gpdLevel.setLevelValue(n.floatValue());
                            levelList.add(gpdLevel);
                        }
                    } else {
                        GenericPointDataLevel gpdLevel = new GenericPointDataLevel();
                        gpdLevel.setLevelValue(pdv
                                .getFloat(GenericPointDataConstants.HDF5_LEVEL_VALUE));
                        levelList.add(gpdLevel);
                    }
                    // level value is retrieved already, so drop it here
                    parameters
                            .remove(GenericPointDataConstants.HDF5_LEVEL_VALUE);
                } else
                    continue; // no level value, no need to continue on this
                              // PDV.

                int stnCatalogType = ObStation.CAT_TYPE_MESONET;
                if (parameters
                        .contains(GenericPointDataConstants.DB_STN_CATALOGTYPE)) {
                    stnCatalogType = pdv
                            .getInt(GenericPointDataConstants.DB_STN_CATALOGTYPE);
                    // System.out.println("stnCatalogType= "+ stnCatalogType);
                    // DB_STN_CATALOGTYPE value is retrieved already, so drop it
                    // here
                    parameters
                            .remove(GenericPointDataConstants.DB_STN_CATALOGTYPE);
                }
                String rtnstnId = stnId;
                if (parameters.contains(GenericPointDataConstants.HDF5_STN_ID)) {
                    rtnstnId = pdv
                            .getString(GenericPointDataConstants.HDF5_STN_ID);
                    // System.out.println("stnId= "+ rtnstnId);
                    // stnId is input parameter, can drop it here.
                    parameters.remove(GenericPointDataConstants.HDF5_STN_ID);
                }
                float rtnslat = slat;
                if (parameters.contains(GenericPointDataConstants.DB_SLAT)) {
                    rtnslat = pdv.getFloat(GenericPointDataConstants.DB_SLAT);
                    // System.out.println("slat= "+ rtnslat);
                    // slat value is retrieved already, so drop it here
                    parameters.remove(GenericPointDataConstants.DB_SLAT);
                }
                float rtnslon = slon;
                if (parameters.contains(GenericPointDataConstants.DB_SLON)) {
                    rtnslon = pdv.getFloat(GenericPointDataConstants.DB_SLON);
                    // System.out.println("slon= "+ rtnslon);
                    // slon value is retrieved already, so drop it here
                    parameters.remove(GenericPointDataConstants.DB_SLON);
                }
                String utFlag = null;
                if (parameters
                        .contains(GenericPointDataConstants.DB_UTILITY_FLAGS)) {
                    utFlag = pdv
                            .getString(GenericPointDataConstants.DB_UTILITY_FLAGS);
                    System.out.println("utFlag= " + utFlag);
                    parameters
                            .remove(GenericPointDataConstants.DB_UTILITY_FLAGS);
                }
                int forecastTime = 0;
                if (parameters
                        .contains(GenericPointDataConstants.DB_FORECAST_TIME)) {
                    forecastTime = pdv
                            .getInt(GenericPointDataConstants.DB_FORECAST_TIME);
                    parameters
                            .remove(GenericPointDataConstants.DB_FORECAST_TIME);
                }
                // PDV id is not returned back to user, so drop it here
                parameters.remove(GenericPointDataConstants.HDF5_PDV_ID);

                GenericPointDataStationProduct stnPd = new GenericPointDataStationProduct();
                stnPd.setProductName(prodName);
                stnPd.setRefTime(refTime);
                stnPd.setLevelLst(levelList);
                stnPd.setProductVersion(productVersion);
                stnPd.getLocation().setStationId(rtnstnId);
                stnPd.getLocation().setCatalogType(stnCatalogType);
                stnPd.setNumLevel(numLevel);
                stnPd.setSlat(rtnslat);
                stnPd.setSlon(rtnslon);
                stnPd.setForecastTime(forecastTime);
                stnPd.setUtilityFlag(utFlag);
                for (String parm : parameters) {
                    if (numLevel > 1) {
                        // these parameters are data parameters and should be 2
                        // dimensional float value per design
                        // If a new "meta" data is queried, then we should take
                        // care of that data specifically before here.
                        Number[] num = pdv.getNumberAllLevels(parm);// ,numLevel);
                        // System.out.println("parm ="+parm);
                        for (int j = 0; j < numLevel; j++) {
                            Number n = num[j];
                            // System.out.println(" value="+n.floatValue());
                            GenericPointDataParameter gpdParm = new GenericPointDataParameter(
                                    parm, n.floatValue());
                            levelList.get(j).getGpdParameters().add(gpdParm);
                        }
                    } else {
                        GenericPointDataParameter gpdParm = new GenericPointDataParameter(
                                parm, pdv.getFloat(parm));
                        levelList.get(0).getGpdParameters().add(gpdParm);
                    }
                }
                stnProdList.add(stnPd);
            }
        }
        return stnProdList;
    }

    public List<GenericPointDataStationProduct> getGpdStationModelSndProduct(
            List<Date> rangeStartTimeList, Date referenceTime,
            GenericPointDataQueryKey key, String stnId, float slat, float slon,
            String prodName) throws Exception {
        long t01 = System.currentTimeMillis();
        GenericPointDataProductInfo prodInfo = getGpdProdInfo(prodName);
        if (prodInfo == null) {
            System.out.println("product is not in DB");
            return null;
        }
        int productVersion = getGpdProductLatestVersion(referenceTime, prodName);
        if (productVersion < 0) {
            System.out.println("product version not available");
            return null;
        }

        List<GenericPointDataStationProduct> stnProdList = new ArrayList<GenericPointDataStationProduct>();
        for (Date rangeStartTime : rangeStartTimeList) {

            PointDataContainer pdc = getPointDataContainer(referenceTime,
                    rangeStartTime, key, stnId, slat, slon, prodInfo,
                    productVersion);
            if (pdc == null) {
                System.out.println("pdc is null");
                continue;
            }
            // System.out.println(rangeStartTime.toString()
            // +" pdc CurrentSz()="+pdc.getCurrentSz());

            for (int i = 0; i < pdc.getCurrentSz(); i++) {
                PointDataView pdv = pdc.readRandom(i);
                // System.out.println("pdv#"+i+" *********************************************");
                Set<String> parameters = new HashSet<String>(pdv.getContainer()
                        .getParameters());
                int numLevel = 0;
                if (parameters
                        .contains(GenericPointDataConstants.HDF5_NUM_LEVEL)) {
                    numLevel = pdv
                            .getInt(GenericPointDataConstants.HDF5_NUM_LEVEL);
                    // System.out.println("numLevel= "+ numLevel);
                    // numLevel value is retrieved already, so drop it here
                    parameters.remove(GenericPointDataConstants.HDF5_NUM_LEVEL);
                } else
                    continue; // level number is 0, no need to continue on this
                              // PDV.

                List<GenericPointDataLevel> levelList;
                if (parameters
                        .contains(GenericPointDataConstants.HDF5_LEVEL_VALUE)) {
                    levelList = new ArrayList<GenericPointDataLevel>(numLevel);
                    if (numLevel > 1) {
                        Number[] num = pdv
                                .getNumberAllLevels(GenericPointDataConstants.HDF5_LEVEL_VALUE);// pdv.getNumberAllLevels(parm,numLevel);
                        int count = 0;
                        for (Number n : num) {
                            count++;
                            if (count > numLevel)
                                break;
                            // System.out.println("Level " +count+
                            // " value="+n.floatValue());
                            GenericPointDataLevel gpdLevel = new GenericPointDataLevel();
                            gpdLevel.setLevelValue(n.floatValue());
                            levelList.add(gpdLevel);
                        }
                    } else {
                        GenericPointDataLevel gpdLevel = new GenericPointDataLevel();
                        gpdLevel.setLevelValue(pdv
                                .getFloat(GenericPointDataConstants.HDF5_LEVEL_VALUE));
                        levelList.add(gpdLevel);
                    }
                    // level value is retrieved already, so drop it here
                    parameters
                            .remove(GenericPointDataConstants.HDF5_LEVEL_VALUE);
                } else
                    continue; // no level value, no need to continue on this
                              // PDV.

                int stnCatalogType = ObStation.CAT_TYPE_MESONET;
                if (parameters
                        .contains(GenericPointDataConstants.DB_STN_CATALOGTYPE)) {
                    stnCatalogType = pdv
                            .getInt(GenericPointDataConstants.DB_STN_CATALOGTYPE);
                    // System.out.println("stnCatalogType= "+ stnCatalogType);
                    // DB_STN_CATALOGTYPE value is retrieved already, so drop it
                    // here
                    parameters
                            .remove(GenericPointDataConstants.DB_STN_CATALOGTYPE);
                }
                String rtnstnId = stnId;
                if (parameters.contains(GenericPointDataConstants.HDF5_STN_ID)) {
                    rtnstnId = pdv
                            .getString(GenericPointDataConstants.HDF5_STN_ID);
                    // System.out.println("stnId= "+ rtnstnId);
                    // stnId is input parameter, can drop it here.
                    parameters.remove(GenericPointDataConstants.HDF5_STN_ID);
                }
                float rtnslat = slat;
                if (parameters.contains(GenericPointDataConstants.DB_SLAT)) {
                    rtnslat = pdv.getFloat(GenericPointDataConstants.DB_SLAT);
                    // System.out.println("slat= "+ rtnslat);
                    // slat value is retrieved already, so drop it here
                    parameters.remove(GenericPointDataConstants.DB_SLAT);
                }
                float rtnslon = slon;
                if (parameters.contains(GenericPointDataConstants.DB_SLON)) {
                    rtnslon = pdv.getFloat(GenericPointDataConstants.DB_SLON);
                    // System.out.println("slon= "+ rtnslon);
                    // slon value is retrieved already, so drop it here
                    parameters.remove(GenericPointDataConstants.DB_SLON);
                }
                String utFlag = null;
                if (parameters
                        .contains(GenericPointDataConstants.DB_UTILITY_FLAGS)) {
                    utFlag = pdv
                            .getString(GenericPointDataConstants.DB_UTILITY_FLAGS);
                    System.out.println("utFlag= " + utFlag);
                    parameters
                            .remove(GenericPointDataConstants.DB_UTILITY_FLAGS);
                }
                int forecastTime = 0;
                if (parameters
                        .contains(GenericPointDataConstants.DB_FORECAST_TIME)) {
                    forecastTime = pdv
                            .getInt(GenericPointDataConstants.DB_FORECAST_TIME);
                    parameters
                            .remove(GenericPointDataConstants.DB_FORECAST_TIME);
                }
                // PDV id is not returned back to user, so drop it here
                parameters.remove(GenericPointDataConstants.HDF5_PDV_ID);

                GenericPointDataStationProduct stnPd = new GenericPointDataStationProduct();
                stnPd.setProductName(prodName);
                stnPd.setRefTime(referenceTime);
                stnPd.setLevelLst(levelList);
                stnPd.setProductVersion(productVersion);
                stnPd.getLocation().setStationId(rtnstnId);
                stnPd.getLocation().setCatalogType(stnCatalogType);
                stnPd.setNumLevel(numLevel);
                stnPd.setSlat(rtnslat);
                stnPd.setSlon(rtnslon);
                stnPd.setForecastTime(forecastTime);
                stnPd.setUtilityFlag(utFlag);

                for (String parm : parameters) {
                    if (numLevel > 1) {
                        // these parameters are data parameters and should be 2
                        // dimensional float value per design
                        // If a new "meta" data is queried, then we should take
                        // care of that data specifically before here.
                        Number[] num = pdv.getNumberAllLevels(parm);// ,numLevel);
                        // System.out.println("parm ="+parm);
                        for (int j = 0; j < numLevel; j++) {
                            Number n = num[j];
                            // System.out.println(" value="+n.floatValue());
                            GenericPointDataParameter gpdParm = new GenericPointDataParameter(
                                    parm, n.floatValue());
                            levelList.get(j).getGpdParameters().add(gpdParm);
                        }
                    } else {
                        GenericPointDataParameter gpdParm = new GenericPointDataParameter(
                                parm, pdv.getFloat(parm));
                        levelList.get(0).getGpdParameters().add(gpdParm);
                    }
                }
                stnProdList.add(stnPd);
            }
        }
        long t02 = System.currentTimeMillis();
        System.out.println("ThrifClient: getGpdStationModelSndProduct()  took "
                + (t02 - t01) + " ms in total for query stn=" + stnId);

        return stnProdList;
    }

    /*
     * TBM...Chin delete this later...not used public
     * GenericPointDataStationProduct getGpdStationProduct(Date refTime,
     * GenericPointDataQueryKey key, String stnId, double slat, double slon,
     * String reportName,boolean useSpecifiedProductVersion, int
     * productVersion)throws Exception{ GenericPointDataProductInfo report =
     * getGpdProdInfo( reportName); if (report == null) {
     * System.out.println("report is not in DB"); return null; }
     * if(useSpecifiedProductVersion== false){ //find the latest version if
     * there is one. Otherwise, use user specified version number productVersion
     * = getGpdProductLatestVersion( refTime, reportName); } PointDataContainer
     * pdc; pdc = getPointDataContainer( refTime, key, stnId, slat, slon,
     * report, productVersion);
     * 
     * if (pdc == null) { System.out.println("pdc is null"); return null; }
     * //for a single station product query, the pdc.getCurrentSz() should
     * always be 1 System.out.println("pdc CurrentSz()="+pdc.getCurrentSz());
     * GenericPointDataStationProduct stnPd= null; for (int i = 0; i <
     * pdc.getCurrentSz(); i++) { PointDataView pdv =pdc.readRandom(i);
     * Set<String> parameters = pdv.getContainer().getParameters(); for(String
     * parm: parameters){ System.out.println("parm ="+parm); } int numLevel=0;
     * if (parameters.contains(GenericPointDataConstants.HDF5_NUM_LEVEL)){
     * numLevel = pdv.getInt(GenericPointDataConstants.HDF5_NUM_LEVEL);
     * System.out.println("numLevel= "+ numLevel); //numLevel value is retrieved
     * already, so drop it here
     * parameters.remove(GenericPointDataConstants.HDF5_NUM_LEVEL); } else
     * continue; //level number is 0, no need to continue on this PDV.
     * 
     * List<GenericPointDataLevel> levelList; if
     * (parameters.contains(GenericPointDataConstants.HDF5_LEVEL_VALUE)){
     * levelList = new ArrayList<GenericPointDataLevel>(numLevel); Number[] num
     * =
     * pdv.getNumberAllLevels(GenericPointDataConstants.HDF5_LEVEL_VALUE);//pdv
     * .getNumberAllLevels(parm,numLevel); for(Number n: num){
     * System.out.println("level value="+n.floatValue()); if(n.floatValue() ==
     * GenericPointDataConstants.GPD_INVALID_FLOAT_VALUE) //Not a valid level,
     * skip it continue; GenericPointDataLevel gpdLevel = new
     * GenericPointDataLevel(); gpdLevel.setLevelValue(n.floatValue());
     * levelList.add(gpdLevel); } //level value is retrieved already, so drop it
     * here parameters.remove(GenericPointDataConstants.HDF5_LEVEL_VALUE); }
     * else continue; //no level value, no need to continue on this PDV.
     * 
     * int stnCatalogType=ObStation.CAT_TYPE_MESONET; if
     * (parameters.contains(GenericPointDataConstants.DB_STN_CATALOGTYPE)){
     * stnCatalogType =
     * pdv.getInt(GenericPointDataConstants.DB_STN_CATALOGTYPE);
     * System.out.println("stnCatalogType= "+ stnCatalogType);
     * //DB_STN_CATALOGTYPE value is retrieved already, so drop it here
     * parameters.remove(GenericPointDataConstants.DB_STN_CATALOGTYPE); } String
     * rtnstnId = stnId; if
     * (parameters.contains(GenericPointDataConstants.HDF5_STN_ID)){ rtnstnId =
     * pdv.getString(GenericPointDataConstants.HDF5_STN_ID);
     * System.out.println("stnId= "+ rtnstnId); //stnId is input parameter, can
     * drop it here. parameters.remove(GenericPointDataConstants.HDF5_STN_ID); }
     * double rtnslat = slat; if
     * (parameters.contains(GenericPointDataConstants.DB_SLAT)){ rtnslat =
     * pdv.getFloat(GenericPointDataConstants.DB_SLAT);
     * System.out.println("slat= "+ rtnslat); //slat value is retrieved already,
     * so drop it here parameters.remove(GenericPointDataConstants.DB_SLAT); }
     * double rtnslon = slon; if
     * (parameters.contains(GenericPointDataConstants.DB_SLON)){ rtnslon =
     * pdv.getFloat(GenericPointDataConstants.DB_SLON);
     * System.out.println("slon= "+ rtnslon); //slon value is retrieved already,
     * so drop it here parameters.remove(GenericPointDataConstants.DB_SLON); }
     * //PDV_id is not returned back to user, so drop it here
     * parameters.remove(GenericPointDataConstants.HDF5_PDV_ID);
     * 
     * 
     * stnPd= new GenericPointDataStationProduct();
     * stnPd.setProductName(reportName); stnPd.setRefTime(refTime);
     * stnPd.setLevelLst(levelList); stnPd.setProductVersion(productVersion);
     * stnPd.setSlat(rtnslat); stnPd.setSlon(rtnslon);
     * stnPd.getLocation().setStationId(rtnstnId);
     * stnPd.getLocation().setCatalogType(stnCatalogType);
     * stnPd.setNumLevel(numLevel); for(String parm: parameters){ //these
     * parameters are data parameters and should be 2 dimensional float value
     * per design //If a new "meta" data is queried, then we should take care of
     * that data specifically before here. Number[] num =
     * pdv.getNumberAllLevels(parm);//,numLevel);
     * System.out.println("parm ="+parm); for(int j=0; j< numLevel ; j++){
     * Number n = num[j]; System.out.println(" value="+n.floatValue());
     * GenericPointDataParameter gpdParm = new
     * GenericPointDataParameter(parm,n.floatValue());
     * levelList.get(j).getGpdParameters().add(gpdParm); } } } return stnPd; }
     */
    public int getGpdProductLatestVersion(Date refTime, String prodName) {
        int latestProdVer = -1;
        Session sess = null;
        sess = getSessionFactory().openSession();
        sess.beginTransaction();

        Criteria crit = sess.createCriteria(GenericPointDataRecord.class);
        // System.out.println("getGpdProductLatestVersion input reportName="+
        // prodName);
        Criterion rptnameCrit = Restrictions.eq("productInfo.name", prodName);
        crit.add(rptnameCrit);
        Criterion reftimeCrit = Restrictions.eq("dataTime.refTime", refTime);
        crit.add(reftimeCrit);
        List<?> vals = crit.list();
        if (vals.size() > 0) {
            for (int i = 0; i < vals.size(); i++) {
                // to avoid LazyInitializationException, we have to take care of
                // Collection before
                // closing session.
                GenericPointDataRecord rec = (GenericPointDataRecord) vals
                        .get(i);
                int version = rec.getProductVersion();
                if (version > latestProdVer)
                    latestProdVer = version;
                // System.out.println("latestProdVer= "+ latestProdVer);

            }
        }
        if (sess != null) {
            try {
                sess.close();
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
        return latestProdVer;
    }

    /**
     * Chin note: copy from PluginDao, modified code to get GPD HDF5 file path
     * correctly.
     * 
     * Purges data from the database for this plugin with the given reference
     * time matching the given productKeys. If refTime is null, will purge all
     * data associated with the productKeys. Hdf5 must be purged separately as
     * most hdf5 files can't be purged with a single reference time. Use the
     * passed map to track what needs to be done with hdf5.
     * 
     * @param refTime
     *            The reftime to delete data for. A null will purge all data for
     *            the productKeys.
     * @param productKeys
     *            The product key/values to use as a constraint for deletions.
     *            Should be in key value pairs.
     * @param trackHdf5
     *            If true will use trackToUri to populate hdf5FileToUriPurged
     *            map.
     * @param trackToUri
     *            If true will track each URI that needs to be deleted from
     *            HDF5, if false will only track the hdf5 files that need to be
     *            deleted.
     * @param hdf5FileToUriPurged
     *            Map to be populated by purgeDataByRefTime of all the hdf5
     *            files that need to be updated. If trackToUri is true, each
     *            file will have the exact data URI's to be removed from each
     *            file. If trackToUri is false, the map will have a null entry
     *            for the list and only track the files.
     * @return Number of rows deleted from database.
     * @throws DataAccessLayerException
     */
    @Override
    @SuppressWarnings("unchecked")
    public int purgeDataByRefTime(Date refTime,
            Map<String, String> productKeys, boolean trackHdf5,
            boolean trackToUri, Map<String, List<String>> hdf5FileToUriPurged)
            throws DataAccessLayerException {

        int results = 0;
        PurgeLogger.logInfo("Enter purgeDataByRefTime refTime=" + refTime
                + " trackHdf5=" + trackHdf5 + " trackToUri=" + trackToUri,
                pluginName);
        DatabaseQuery dataQuery = new DatabaseQuery(this.daoClass);
        if ((hdf5FileToUriPurged != null)) {
            for (String key : hdf5FileToUriPurged.keySet()) {
                List<String> pairLst = hdf5FileToUriPurged.get(key);
                PurgeLogger.logInfo(
                        "starting purgeDataByRefTime hdf5FileToUriPurged map key="
                                + key, pluginName);
                if (pairLst != null) {
                    for (String val : pairLst) {
                        PurgeLogger.logInfo(
                                "starting purgeDataByRefTime hdf5FileToUriPurged map val="
                                        + val, pluginName);
                    }
                }
            }
        }
        if (refTime != null) {
            dataQuery.addQueryParam(PURGE_VERSION_FIELD, refTime);
        }

        if ((productKeys != null) && (productKeys.size() > 0)) {
            for (Map.Entry<String, String> pair : productKeys.entrySet()) {
                dataQuery.addQueryParam(pair.getKey(), pair.getValue());
                PurgeLogger.logInfo(" purgeDataByRefTime product map key="
                        + pair.getKey() + " value=" + pair.getValue(),
                        pluginName);
            }
        }

        List<PluginDataObject> pdos = null;

        dataQuery.setMaxResults(500);

        // fields for hdf5 purge
        String previousFile = null;
        StringBuilder pathBuilder = new StringBuilder();

        int loopCount = 0;
        do {
            pdos = (List<PluginDataObject>) this.queryByCriteria(dataQuery);
            if ((pdos != null) && !pdos.isEmpty()) {
                this.delete(pdos);

                if (trackHdf5 && (hdf5FileToUriPurged != null)) {
                    for (PluginDataObject pdo : pdos) {
                        pathBuilder.setLength(0);
                        GenericPointDataRecord rec = (GenericPointDataRecord) pdo;
                        String directory = PLUGIN_HDF5_DIR
                                + rec.getProductInfo().getName();
                        int forecasttime = rec.getDataTime().getFcstTime();
                        String dateStr = hdfFileDateFormat.format(refTime)
                                + "-f" + forecasttime;
                        String fileName = this.pluginName + "-"
                                + rec.getProductInfo().getName() + dateStr
                                + ".h5";
                        String file = directory + File.separator + fileName;
                        PurgeLogger.logInfo(++loopCount
                                + " purgeDataByRefTime file=" + file,
                                pluginName);
                        if (trackToUri) {
                            List<String> uriList = hdf5FileToUriPurged
                                    .get(file);
                            if (uriList == null) {
                                // sizing to 50 as most data types have numerous
                                // entries in a file
                                uriList = new ArrayList<String>(50);
                                hdf5FileToUriPurged.put(file, uriList);
                            }
                            uriList.add(file);
                        } else {
                            // only need to track file, tracking last file
                            // instead of constantly indexing hashMap
                            if (!file.equals(previousFile)) {
                                hdf5FileToUriPurged.put(file, null);
                                previousFile = file;
                            }
                        }
                    }
                }

                results += pdos.size();
            }

        } while ((pdos != null) && !pdos.isEmpty());
        if ((hdf5FileToUriPurged != null)) {
            for (String key : hdf5FileToUriPurged.keySet()) {
                List<String> pairLst = hdf5FileToUriPurged.get(key);
                PurgeLogger.logInfo(
                        "leaving purgeDataByRefTime hdf5FileToUriPurged map key="
                                + key, pluginName);
                if (pairLst != null) {
                    for (String val : pairLst) {
                        PurgeLogger.logInfo(
                                "leaving purgeDataByRefTime hdf5FileToUriPurged map val="
                                        + val, pluginName);
                    }
                }
            }
        }
        return results;
    }

    /*
     * Return distinct reference time lines for one product
     */
    public NcSoundingTimeLines getGpdProductTimeline(String prodName) {
        Object[] synopTimeAry = null;
        NcSoundingTimeLines tl = new NcSoundingTimeLines();
        String queryStr;
        queryStr = new String(
                "Select Distinct reftime FROM gpd where productinfo_name='"
                        + prodName + "' ORDER BY reftime DESC");
        synopTimeAry = (Object[]) executeSQLQuery(queryStr);
        tl.setTimeLines(synopTimeAry);
        return tl;
    }

    /*
     * Return distinct rangestart times for one product at one reference time.
     * Input reference time string format is "yyyy-mm-dd HH"
     */
    public NcSoundingTimeLines getGpdProductRangestartTimes(String prodName,
            String refTimeStr) {

        Object[] refTimeAry = null;
        NcSoundingTimeLines tl = new NcSoundingTimeLines();

        String queryStr = new String(
                "Select Distinct rangestart FROM gpd where productinfo_name='"
                        + prodName + "' AND reftime='" + refTimeStr + ":00:00'"
                        + " ORDER BY rangestart");
        refTimeAry = (Object[]) executeSQLQuery(queryStr);
        tl.setTimeLines(refTimeAry);

        return tl;
    }

    /*
     * Return distinct station id(s) for one product at one reference time
     */
    public NcSoundingStnInfoCollection getGpdStationInfoCollection(
            String selectedRefTime, String selectedRangeStartTime,
            String prodName) {
        NcSoundingStnInfoCollection stnInfoCol = new NcSoundingStnInfoCollection();
        List<NcSoundingStnInfo> stationInfoList = new ArrayList<NcSoundingStnInfo>();
        String queryStr;
        Object[] rtnobjArray;
        queryStr = new String(
                "Select Distinct slat, slon, id, location_gid, reftime, rangestart FROM gpd where reftime='"
                        + selectedRefTime
                        + "' AND rangestart='"
                        + selectedRangeStartTime
                        + "' AND productinfo_name='"
                        + prodName
                        + "' AND slat BETWEEN -89.9 AND 89.9 AND slon BETWEEN -179.9 AND 179.9");
        rtnobjArray = executeSQLQuery(queryStr);
        String stnId = "";
        Double slat, slon;
        Timestamp synoptictime = null, rsTime = null;
        for (int j = 0; j < rtnobjArray.length; j++) {
            Object[] objArray = (Object[]) rtnobjArray[j];
            // ids.add(((Integer)objArray[2]));
            // We save lat/lon as float in DB.
            // To make sure the double number get the same precision as the
            // float number saved in DB
            // we have to do the following conversion.
            slat = new Double(objArray[0].toString());
            slon = new Double(objArray[1].toString());
            stnId = (String) objArray[3];
            stnId = stnId.replace("1000-", "");
            synoptictime = (Timestamp) objArray[4];
            rsTime = (Timestamp) objArray[5];
            NcSoundingStnInfo stn = stnInfoCol.getNewStnInfo();
            stn.setStnId(stnId);
            stn.setStationLongitude(slon);
            stn.setStationLatitude(slat);
            stn.setSynopTime(synoptictime);
            stn.setRangeStartTime(rsTime);
            stationInfoList.add((NcSoundingStnInfo) stn);
        }
        NcSoundingStnInfo[] stationInfoAry = new NcSoundingStnInfo[stationInfoList
                .size()];
        stnInfoCol.setStationInfo(stationInfoList.toArray(stationInfoAry));
        // *System.out.println("stn size = "+
        // stnInfoCol.getStationInfo().length);
        return stnInfoCol;
    }

    public Object[] getGpdAvailProducts(GenericPointDataReqType reqType) {
        String queryStr;
        Object[] rtnobjArray;
        switch (reqType) {
        case GET_GPD_AVAILABLE_OBSERVED_SOUNDING_PRODUCTS:
            queryStr = new String(
                    "Select Distinct productinfo_name FROM gpd where productinfo_name IN (Select Distinct name FROM gpd_productinfo where maxnumberoflevel > 8) AND gpd.utilityflags = '[]'");
            break;
        case GET_GPD_AVAILABLE_MODEL_SOUNDING_PRODUCTS:
            queryStr = new String(
                    "Select Distinct productinfo_name FROM gpd where productinfo_name IN (Select Distinct name FROM gpd_productinfo where maxnumberoflevel > 8) AND gpd.utilityflags = '[FCST_USED]'");
            break;
        case GET_GPD_AVAILABLE_SURFACE_PRODUCTS:
            queryStr = new String(
                    "Select Distinct name FROM gpd_productinfo where maxnumberoflevel=1");
            break;
        case GET_GPD_ALL_AVAILABLE_PRODUCTS:
            queryStr = new String("Select Distinct name FROM gpd_productinfo");
            break;
        default:
            return null;
        }
        rtnobjArray = executeSQLQuery(queryStr);

        // List<String> prodList = new ArrayList<String>();
        // for (int j =0; j <rtnobjArray.length; j++){
        // Object[] objArray = (Object[] )rtnobjArray[j];
        // System.out.println("prod="+rtnobjArray[j]);
        // String prodName= (String)rtnobjArray[j];
        // prodList.add(prodName);
        // }
        return rtnobjArray;
    }
}
