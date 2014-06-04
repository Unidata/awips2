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
package com.raytheon.viz.hydrocommon.whfslib;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.common.dataplugin.shef.tables.Colorvalue;
import com.raytheon.uf.common.dataplugin.shef.tables.DAARadarResult;
import com.raytheon.uf.common.dataplugin.shef.tables.Hourlypc;
import com.raytheon.uf.common.dataplugin.shef.tables.Hourlypp;
import com.raytheon.uf.common.dataplugin.shef.tables.Ingestfilter;
import com.raytheon.uf.common.dataplugin.shef.tables.Rwbiasdyn;
import com.raytheon.uf.common.dataplugin.shef.tables.Rwbiasstat;
import com.raytheon.uf.common.dataplugin.shef.tables.Rwradarresult;
import com.raytheon.uf.common.dataplugin.shef.tables.Rwresult;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery.QueryLanguage;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydrocommon.HydroConstants;

public class IHFSDbGenerated {
    public static ArrayList<Colorvalue> GetColorValue(String where) {
        StringBuilder query = new StringBuilder("FROM ");
        query.append(Colorvalue.class.getName());
        // StringBuilder query = new StringBuilder("SELECT * FROM ColorValue");
        query.append(" ");
        query.append(where);

        ArrayList<Colorvalue> retVal = new ArrayList<Colorvalue>();
        try {
            List<Object[]> results = DirectDbQuery.executeQuery(query
                    .toString(), "ihfs", QueryLanguage.HQL);

            retVal.ensureCapacity(results.size());
            for (Object[] item : results) {
                retVal.add((Colorvalue) item[0]);
            }
        } catch (VizException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }

        return retVal;
    }

    public static ArrayList<Hourlypc> GetHourlyPC(String where) {
        StringBuilder query = new StringBuilder("FROM ");
        query.append(Hourlypc.class.getName());
        query.append(" ");
        query.append(where);

        ArrayList<Hourlypc> retVal = new ArrayList<Hourlypc>();
        try {
            List<Object[]> results = DirectDbQuery.executeQuery(query
                    .toString(), "ihfs", QueryLanguage.HQL);

            retVal.ensureCapacity(results.size());
            for (Object[] item : results) {
                retVal.add((Hourlypc) item[0]);
            }
        } catch (VizException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }

        return retVal;
    }

    public static ArrayList<Hourlypp> GetHourlyPP(String where) {
        StringBuilder query = new StringBuilder("FROM ");
        query.append(Hourlypp.class.getName());
        query.append(" ");
        query.append(where);

        ArrayList<Hourlypp> retVal = new ArrayList<Hourlypp>();
        try {
            List<Object[]> results = DirectDbQuery.executeQuery(query
                    .toString(), "ihfs", QueryLanguage.HQL);

            retVal.ensureCapacity(results.size());
            for (Object[] item : results) {
                retVal.add((Hourlypp) item[0]);
            }
        } catch (VizException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }

        return retVal;
    }

    public static List<Ingestfilter> GetIngestFilter(String where) {
        StringBuilder query = new StringBuilder("FROM ");
        query.append(Ingestfilter.class.getName());
        query.append(" ");
        query.append(where);

        ArrayList<Ingestfilter> retVal = new ArrayList<Ingestfilter>();
        try {
            List<Object[]> results = DirectDbQuery.executeQuery(query
                    .toString(), "ihfs", QueryLanguage.HQL);

            retVal.ensureCapacity(results.size());
            for (Object[] item : results) {
                retVal.add((Ingestfilter) item[0]);
            }
        } catch (VizException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }

        return retVal;
    }

    public static List<Rwresult> GetRWResult(String where) {
        StringBuilder query = new StringBuilder("FROM ");
        query.append(Rwresult.class.getName());
        query.append(" ");
        query.append(where);

        ArrayList<Rwresult> retVal = new ArrayList<Rwresult>();
        try {
            List<Object[]> results = DirectDbQuery.executeQuery(query
                    .toString(), "ihfs", QueryLanguage.HQL);

            retVal.ensureCapacity(results.size());
            for (Object[] item : results) {
                retVal.add((Rwresult) item[0]);
            }
        } catch (VizException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }

        return retVal;
    }

    public static int UpdateRWResult(Rwresult obj) {
        try {
            return DirectDbQuery.saveOrUpdate(obj, "ihfs");
        } catch (VizException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }

        return -1;
    }

    public static List<Rwbiasdyn> GetRWBiasDyn(String where) {
        StringBuilder query = new StringBuilder("FROM ");
        query.append(Rwbiasdyn.class.getName());
        query.append(" ");
        query.append(where);

        ArrayList<Rwbiasdyn> retVal = new ArrayList<Rwbiasdyn>();
        try {
            List<Object[]> results = DirectDbQuery.executeQuery(query
                    .toString(), "ihfs", QueryLanguage.HQL);

            retVal.ensureCapacity(results.size());
            for (Object[] item : results) {
                retVal.add((Rwbiasdyn) item[0]);
            }
        } catch (VizException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }

        return retVal;
    }

    public static String GetRadarLocOffice(String rid) throws VizException {
        String offc_id = "";
        String query = String.format(
                "select office_id from radarloc where radid ='%s'", rid);
        List<Object[]> rs = DirectDbQuery.executeQuery(query,
                HydroConstants.IHFS, QueryLanguage.SQL);

        for (int i = 0; i < rs.size(); i++) {
            Object[] oa = rs.get(i);
            offc_id = (String) oa[0];
        }
        return offc_id;
    }

    /**
     * This Queries the RWBiasStat table It only returns only one row.
     * 
     * @return int[]
     */
    public static ArrayList<Rwbiasstat> GetRWBiasstat(String where) {
        StringBuilder query = new StringBuilder("FROM ");
        query.append(Rwbiasstat.class.getName());
        query.append(" ");
        query.append(where);

        ArrayList<Rwbiasstat> retVal = new ArrayList<Rwbiasstat>();
        try {
            List<Object[]> results = DirectDbQuery.executeQuery(query
                    .toString(), "ihfs", QueryLanguage.HQL);

            retVal.ensureCapacity(results.size());
            for (Object[] item : results) {
                retVal.add((Rwbiasstat) item[0]);
            }
        } catch (VizException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }

        return retVal;
    }

    public static List<Rwradarresult> GetRWRadarResult(String where) {
        StringBuilder query = new StringBuilder("FROM ");
        query.append(Rwradarresult.class.getName());
        query.append(" ");
        query.append(where);

        ArrayList<Rwradarresult> retVal = new ArrayList<Rwradarresult>();
        try {
            List<Object[]> results = DirectDbQuery.executeQuery(query
                    .toString(), "ihfs", QueryLanguage.HQL);

            retVal.ensureCapacity(results.size());
            for (Object[] item : results) {
                retVal.add((Rwradarresult) item[0]);
            }
        } catch (VizException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }

        return retVal;
    }

    public static int UpdateRWRadarResult(Rwradarresult obj) {
        try {
            return DirectDbQuery.saveOrUpdate(obj, "ihfs");
        } catch (VizException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }

        return -1;
    }
    
    public static List<DAARadarResult> GetDAARadarResult(String where) {
        StringBuilder query = new StringBuilder("FROM ");
        query.append(DAARadarResult.class.getName());
        query.append(" ");
        query.append(where);
        
        ArrayList<DAARadarResult> retVal = new ArrayList<DAARadarResult>();
        try {
            List<Object[]> results = DirectDbQuery.executeQuery(query
                    .toString(), "ihfs", QueryLanguage.HQL);

            retVal.ensureCapacity(results.size());
            for (Object[] item : results) {
                retVal.add((DAARadarResult) item[0]);
            }
        } catch (VizException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }

        return retVal;
    }
    
    public static int UpdateDAARadarResult(DAARadarResult obj) {
        try {
            return DirectDbQuery.saveOrUpdate(obj, "ihfs");
        } catch (VizException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }

        return -1;
    }

}
