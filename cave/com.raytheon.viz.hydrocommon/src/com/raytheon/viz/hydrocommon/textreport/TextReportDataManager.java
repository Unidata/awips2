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
package com.raytheon.viz.hydrocommon.textreport;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;

import com.raytheon.viz.hydrocommon.datamanager.HydroDataManager;
import com.raytheon.viz.hydrocommon.textreport.TextReportData.Benchmark;
import com.raytheon.viz.hydrocommon.textreport.TextReportData.Contacts;
import com.raytheon.viz.hydrocommon.textreport.TextReportData.Crest;
import com.raytheon.viz.hydrocommon.textreport.TextReportData.Datum;
import com.raytheon.viz.hydrocommon.textreport.TextReportData.Flood;
import com.raytheon.viz.hydrocommon.textreport.TextReportData.Gage;
import com.raytheon.viz.hydrocommon.textreport.TextReportData.LowWater;
import com.raytheon.viz.hydrocommon.textreport.TextReportData.Pub;
import com.raytheon.viz.hydrocommon.textreport.TextReportData.StaffGageData;

/**
 * HydroBase Text Report data access. Writing individual inner classes because
 * of the Hibernate/Thrift issues/problems with the IHFS.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 18, 2009 2260       mpduff     Initial creation
 * Nov 09, 2010 5416       lbousaid   changed gageQuery
 * Dec 08, 2011 11728      lbousaidi  changed the routines that retrieve data
 * Apr 25, 2012 14499      wkwock     Refine format, query, etc
 * Nov 06, 2012 15454      wkwock     Fix query for get data from gage table
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class TextReportDataManager extends HydroDataManager {
	private static TextReportDataManager instance;

	private static final String LID_QUERY = "select lid from location order by lid";

	private HashMap<String, TextReportData> reportDataMap = new HashMap<String, TextReportData>();

	private HashMap<String, TextReportData.Stnclass> stnclassMap = new HashMap<String, TextReportData.Stnclass>();

	/**
	 * Private constructor.
	 */
	private TextReportDataManager() {
	}

	/**
	 * Get an instance of this class.
	 * 
	 * @return
	 */
	public static TextReportDataManager getInstance() {
		if (instance == null) {
			instance = new TextReportDataManager();
		}

		return instance;
	}

	/**
	 * Free the data memory.
	 */
	public void dispose() {
		reportDataMap = null;
		instance = null;
	}	

	/**
	 * Get all the data for the particular site.
	 * 
	 * @param lid
	 *            The location id
	 * @return The TextReportdata object
	 */
	
	private TextReportData getLocationList(String lid) {
		TextReportData data = new TextReportData();
		data.setLid(lid);

		String locationQuery = "select county, coe, cpm, detail, elev, hdatum, hsa, hu, lat, lon, lremark, lrevise, name, network, rb, rfc, sbd, sn, state, waro, wfo, wsfo, type, des, det, stntype, tzone from location where lid = '"
				+ lid + "'";
		ArrayList<Object[]> rs = runQuery(locationQuery);

		int i = 0;
		if ((rs != null) && (rs.size() > 0)) {
			Object[] oa = rs.get(0);
			data.getLocation().setCounty((String) oa[i]);
			data.getLocation().setCoe((String) oa[++i]);
			data.getLocation().setCpm((String) oa[++i]);
			data.getLocation().setDetail((String) oa[++i]);
			if (oa[++i] != null) {
				data.getLocation().setElev((Double) oa[i]);
			}
			data.getLocation().setHdatum((String) oa[++i]);
			data.getLocation().setHsa((String) oa[++i]);
			data.getLocation().setHu((String) oa[++i]);
			if (oa[++i] != null) {
				data.getLocation().setLat((Double) oa[i]);
			}
			if (oa[++i] != null) {
				data.getLocation().setLon((Double) oa[i]);
			}
			data.getLocation().setLremark((String) oa[++i]);
			data.getLocation().setLrevise((Date) oa[++i]);
			data.getLocation().setName((String) oa[++i]);
			data.getLocation().setNetwork((String) oa[++i]);
			data.getLocation().setRb((String) oa[++i]);
			data.getLocation().setRfc((String) oa[++i]);
			data.getLocation().setSbd((Date) oa[++i]);
			data.getLocation().setSn((String) oa[++i]);
			data.getLocation().setState((String) oa[++i]);
			data.getLocation().setWaro((String) oa[++i]);
			data.getLocation().setWfo((String) oa[++i]);
			data.getLocation().setWsfo((String) oa[++i]);
			data.getLocation().setType((String) oa[++i]);
			data.getLocation().setDes((String) oa[++i]);
			data.getLocation().setDet((String) oa[++i]);
			data.getLocation().setStntype((String) oa[++i]);
			data.getLocation().setTzone((String) oa[++i]);
		}

		return data;
	}
	
	public TextReportData getLocationData(String lid) {
        
        TextReportData data = getLocationList(lid);
        return data;
    }
	
	private TextReportData getStnclassList(String lid) {
		TextReportData data = new TextReportData();
		String stnclassQuery = "select disp_class, dcp, observer, telem_type from Stnclass where lid = '"
				+ lid + "'";
		ArrayList<Object[]> rs = runQuery(stnclassQuery);

		int i = 0;
		if ((rs != null) && (rs.size() > 0)) {
			Object[] oa = rs.get(0);
			data.getStnclass().setDisplayClass((String) oa[i]);
			data.getStnclass().setDcp((String) oa[++i]);
			data.getStnclass().setObserver((String) oa[++i]);
			data.getStnclass().setTelem_type((String) oa[++i]);
		}
		return data;
	}

    public TextReportData getStnclassData(String lid) {
        
        TextReportData data = getStnclassList(lid);
        return data;
    }
    
	private TextReportData getRiverProxList(String lid) {
		
		TextReportData data = new TextReportData();
	
		//get descrip data
		
		String proximityQuery = "select proximity, bed, reach, res, divert, ice, topo, remark from descrip where lid = '"
				+ lid + "'";
		ArrayList<Object[]> rs = runQuery(proximityQuery);

		int i = 0;
		if ((rs != null) && (rs.size() > 0)) {
		Object[] oa = rs.get(0);
			data.getDescrip().setProximity((String) oa[i]);
			data.getDescrip().setBed((String) oa[++i]);
			data.getDescrip().setReach((String) oa[++i]);
			data.getDescrip().setRes((String) oa[++i]);
			data.getDescrip().setDivert((String) oa[++i]);
			data.getDescrip().setIce((String) oa[++i]);
			data.getDescrip().setTopo((String) oa[++i]);
			data.getDescrip().setRemark((String) oa[++i]);
		}
		
	   //get RiverStat data
				
		String riverstatQuery = "select lat, lon, stream, da, fs, fq, mile, wstg, gsno, zd, bf, cb, pool, tide, por, remark, rrevise, rsource, vdatum, level, rated, action_flow from riverstat where lid = '"
				+ lid + "'";
		rs = runQuery(riverstatQuery);

		i = 0;
		if ((rs != null) && (rs.size() > 0)) {
			Object[] oa = rs.get(0);
			if (oa[i] != null) {
				data.getRiverstat().setLat((Double) oa[i]);
			}
			if (oa[++i] != null) {
				data.getRiverstat().setLon((Double) oa[i]);
			}
			data.getRiverstat().setStream((String) oa[++i]);
			if (oa[++i] != null) {
				data.getRiverstat().setDa((Double) oa[i]);
			}
			if (oa[++i] != null) {
				data.getRiverstat().setFs((Double) oa[i]);
			}
			if (oa[++i] != null) {
				data.getRiverstat().setFq((Double) oa[i]);
			}
			if (oa[++i] != null) {
				data.getRiverstat().setMile((Double) oa[i]);
			}
			if (oa[++i] != null) {
				data.getRiverstat().setWstg((Double) oa[i]);
			}
			data.getRiverstat().setGsno((String) oa[++i]);
			if (oa[++i] != null) {
				data.getRiverstat().setZd((Double) oa[i]);
			}
			if (oa[++i] != null) {
				data.getRiverstat().setBf((Double) oa[i]);
			}
			if (oa[++i] != null) {
				data.getRiverstat().setCb((Double) oa[i]);
			}
			if (oa[++i] != null) {
				data.getRiverstat().setPool((Double) oa[i]);
			}
			data.getRiverstat().setTide((String) oa[++i]);
			data.getRiverstat().setPor((String) oa[++i]);
			data.getRiverstat().setRemark((String) oa[++i]);
			data.getRiverstat().setRrevise((Date) oa[++i]);
			data.getRiverstat().setRsource((String) oa[++i]);
			data.getRiverstat().setVdatum((String) oa[++i]);
			data.getRiverstat().setLevel((String) oa[++i]);
			data.getRiverstat().setRated((String) oa[++i]);
			if (oa[++i] != null) {
				data.getRiverstat().setActionFlow((Double) oa[i]);
			}			
		}
		return data;

	}
	
	private TextReportData getObsList(String lid) {
		String observerQuery = "select dos, gn, a1, a2, a3, hphone, phone,firstname,lastname, recip, city, state, zip, comm, email, rprt, spons, ornr, rate, tsk from Observer where lid = '"
				+ lid + "'";
		ArrayList<Object[]> rs = runQuery(observerQuery);
		TextReportData data = new TextReportData();

		int i = 0;
		if ((rs != null) && (rs.size() > 0)) {
			Object[] oa = rs.get(0);
			data.getObserver().setDos((Date) oa[i]);
			data.getObserver().setGn((String) oa[++i]);
			data.getObserver().setA1((String) oa[++i]);
			data.getObserver().setA2((String) oa[++i]);
			data.getObserver().setA3((String) oa[++i]);
			data.getObserver().setHphone((String) oa[++i]);
			data.getObserver().setPhone((String) oa[++i]);
			data.getObserver().setFirstname((String) oa[++i]);
			data.getObserver().setLastname((String) oa[++i]);
			data.getObserver().setRecip((String) oa[++i]);
			data.getObserver().setCity((String) oa[++i]);
			data.getObserver().setState((String) oa[++i]);
			data.getObserver().setZip((String) oa[++i]);
			data.getObserver().setComm((String) oa[++i]);
			data.getObserver().setEmail((String) oa[++i]);
			data.getObserver().setRprt((String) oa[++i]);
			data.getObserver().setSponsor((String) oa[++i]);
			data.getObserver().setOrnr((String) oa[++i]);
			if (oa[++i] != null) {
				data.getObserver().setRate((Double) oa[i]);
			}
			data.getObserver().setTsk((String) oa[++i]);
		}
		return data;
	}
	
	private TextReportData getDcpTelemList(String lid) {
		TextReportData data = new TextReportData();
		String dcpQuery = "select goes, owner, rptime, rptfreq, criteria from dcp where lid = '"
				+ lid + "'";
		ArrayList<Object[]> rs = runQuery(dcpQuery);

		int i = 0;
		if ((rs != null) && (rs.size() > 0)) {
			Object[] oa = rs.get(0);
			data.getDcp().setGoes((String) oa[i]);
			data.getDcp().setOwner((String) oa[++i]);
			data.getDcp().setRptime((String) oa[++i]);
			data.getDcp().setRptfreq((String) oa[++i]);
			data.getDcp().setCriteria((String) oa[++i]);
		}
		
		String telemQuery = "select type, owner, phone, rptfreq, criteria, cost, payor from telem where lid = '"
				+ lid + "'";
	   rs = runQuery(telemQuery);

		i = 0;
		if ((rs != null) && (rs.size() > 0)) {
			Object[] oa = rs.get(0);
			data.getTelem().setType((String) oa[i]);
			data.getTelem().setOwner((String) oa[++i]);
			data.getTelem().setPhone((String) oa[++i]);
			data.getTelem().setRptFreq((String) oa[++i]);
			data.getTelem().setCriteria((String) oa[++i]);
			if (oa[++i] != null) {
				data.getTelem().setCost((Double) oa[i]);
			}
			data.getTelem().setPayor((String) oa[++i]);
		}
			return data;
	}
	
	public TextReportData getfloodcatData(String lid) {
		TextReportData data = new TextReportData();
		String floodcatQuery = "select minor_stage, moderate_stage, major_stage from floodcat where lid = '"
				+ lid + "'";
		ArrayList<Object[]> rs = runQuery(floodcatQuery);

		int i = 0;
		if ((rs != null) && (rs.size() > 0)) {
			Object[] oa = rs.get(0);
			if (oa[i] != null) {
				data.getFloodCat().setMinorStage((Double) oa[i]);
			}
			if (oa[++i] != null) {
				data.getFloodCat().setModerateStage((Double) oa[i]);
			}
			if (oa[++i] != null) {
				data.getFloodCat().setMajorStage((Double) oa[i]);
			}
		}
		return data;
	}

	public TextReportData getGageQueryList(String lid) {
		TextReportData data = new TextReportData();
		String gageQuery = "select gbegin, type, owner, remark, maint, gend from gage where lid = '"
				+ lid + "' and gend is null ORDER BY gbegin desc";
		ArrayList<Object[]> rs = runQuery(gageQuery);
		ArrayList<Gage> gageList = new ArrayList<Gage>();
		int i = 0;
		if ((rs != null) && (rs.size() > 0)) {
			for (Object[] oa : rs) {
				Gage g = data.new Gage();
				g.setBegin((Date) oa[i]);
				g.setType((String) oa[++i]);
				g.setOwner((String) oa[++i]);
				g.setRemark((String) oa[++i]);
				g.setMaint((String) oa[++i]);
				g.setEnd((Date) oa[++i]);
				gageList.add(g);
				i = 0;
			}

			data.setGageList(gageList);
		}
		return data;
	}
	
	public TextReportData getGageQueryData(String lid) {
        
        TextReportData data = getGageQueryList(lid);
        return data;
    }
	
	private TextReportData getRefList(String lid) {

		String referQuery = "select reference from refer where lid = '" + lid
				+ "' ORDER BY reference";
		ArrayList<Object[]> rs = runQuery(referQuery);
		TextReportData data = new TextReportData();

		ArrayList<String> list = new ArrayList<String>();

		if ((rs != null) && (rs.size() > 0)) {
			for (Object[] oa : rs) {
				list.add((String) oa[0]);
			}

			data.setRefer(list);
		}
		return data;
	}
	
    public TextReportData getReferenceData(String lid) {
        
        TextReportData data = getRefList(lid);
        return data;
    }
    
	public TextReportData getBenchmarkList(String lid) {
		String benchmarkQuery = "select bnum, elev, remark from benchmark where lid = '"
				+ lid + "' order by bnum";
		ArrayList<Object[]> rs = runQuery(benchmarkQuery);
		TextReportData data = new TextReportData();
		ArrayList<Benchmark> benchList = new ArrayList<Benchmark>();

		if ((rs != null) && (rs.size() > 0)) {
			for (Object[] oa : rs) {
				Benchmark bm = data.new Benchmark();
				bm.setBnum((String) oa[0]);
				if (oa[1] != null) {
					bm.setElev((Double) oa[1]);
				}
				bm.setRemark((String) oa[2]);
				benchList.add(bm);
			}

			data.setBenchmark(benchList);
		}
		return data;
	}

	private TextReportData getPubDatumList(String lid) {
		TextReportData data = new TextReportData();
		String pubQuery = "select pbegin, ppub, pend from pub where lid = '"
				+ lid + "'";
		ArrayList<Object[]> rs = runQuery(pubQuery);

		ArrayList<Pub> pubList = new ArrayList<Pub>();
		if ((rs != null) && (rs.size() > 0)) {
			for (Object[] oa : rs) {
				Pub pub = data.new Pub();
				if (oa[0] != null) {
					pub.setBegin((Date) oa[0]);
				}

				pub.setPpub((String) oa[1]);
				if (oa[2] != null) {
					pub.setEnd((Date) oa[2]);
				}
				pubList.add(pub);
			}

			data.setPubList(pubList);
		}
		
		String datumQuery = "select ddate, elev from datum where lid = '" + lid
				+ "' order by ddate asc";
		rs = runQuery(datumQuery);

		ArrayList<Datum> datumList = new ArrayList<Datum>();
		if ((rs != null) && (rs.size() > 0)) {
			for (Object[] oa : rs) {
				Datum datum = data.new Datum();
				if (oa[0] != null) {
					datum.setDate((Date) oa[0]);
				}
				if (oa[1] != null) {
					datum.setElevation((Double) oa[1]);
				}
				datumList.add(datum);
			}
			data.setDatumList(datumList);
		}
		return data;
	} 
	

	private TextReportData getCresList(String lid) {
		TextReportData data = new TextReportData();

		//String crestQuery = "select datcrst, cremark, hw, jam, olddatum, q, stage, suppress, timcrst, prelim from crest where lid = '"
		//		+ lid + "' order by datcrst";
		
		String where = "where lid = '" + lid + "' order by datcrst, timcrst";
		ArrayList<Crest> crestList = getCrestList(where);
		data.setCrestList(crestList);

		where = "where lid = '" + lid + "' order by stage";
		ArrayList<Flood> floodList = getFloodList(where);
		data.setFloodList(floodList);
		
		return data;
	}
   
	private TextReportData getLowWaterList(String lid) {
		TextReportData data = new TextReportData();
		String lwQuery = "select lwdat, q, stage, lwrem from lowwater where lid = '"
				+ lid + "' order by lwdat";
		ArrayList<Object[]> rs = runQuery(lwQuery);

		int i = 0;

		ArrayList<LowWater> lwList = new ArrayList<LowWater>();
		if ((rs != null) && (rs.size() > 0)) {
			for (Object[] oa : rs) {
				LowWater lw = data.new LowWater();
				lw.setDate((Date) oa[i]);
				if (oa[++i] != null) {
					lw.setQ((Integer) oa[i]);
				}
				if (oa[++i] != null) {
					lw.setStage((Double) oa[i]);
				}
				lw.setRemarks((String) oa[++i]);
				lwList.add(lw);
				i = 0;
			}
			data.setLowWaterList(lwList);
		}
		return data;
		
	}
	
	private TextReportData getStaffGageList(String lid) {
		TextReportData data = new TextReportData();
		
		/* Get the staff gage data */
		String crestWhere = " where lid = '"
				+ lid
				+ "' and ((suppress != 'X') or (suppress is null)) order by stage desc";
		String floodWhere = " where lid = '" + lid + "' order by stage desc";
		ArrayList<Crest> staffCrestList = getCrestList(crestWhere);
		ArrayList<Flood> staffFloodList = getFloodList(floodWhere);
		StaffGageData sgd = data.new StaffGageData();
		sgd.setCrestList(staffCrestList);
		sgd.setFloodList(staffFloodList);
		data.setStaffData(sgd);

		return data;
    }
	public TextReportData getStaffGageData(String lid) {
        
	      TextReportData data = getStaffGageList(lid);
	      return data;
    }
	
	private TextReportData getContactList(String lid) {
		TextReportData data = new TextReportData();
		String contactsQuery = "select contact, phone, email, remark, priority from contacts where lid = '"
				+ lid + "' order by priority";
		ArrayList<Object[]> rs = runQuery(contactsQuery);

		int i = 0;
		ArrayList<Contacts> contactList = new ArrayList<Contacts>();
		if ((rs != null) && (rs.size() > 0)) {
			for (Object[] oa : rs) {
				Contacts contacts = data.new Contacts();
				contacts.setContact((String) oa[i]);
				contacts.setPhone((String) oa[++i]);
				contacts.setEmail((String) oa[++i]);
				contacts.setRemark((String) oa[++i]);
				if (oa[++i] != null) {
					contacts.setPriority((Integer) oa[i]);
				}
				contactList.add(contacts);
				i = 0;
			}
			data.setContactList(contactList);
		}

		return data;
	}
	
	/**
	 * Get the list of Crest data.
	 * 
	 * @param where
	 *            The where clause for the query
	 * @return ArrayList of Crest objects
	 */
	private ArrayList<Crest> getCrestList(String where) {
		String query = "select datcrst, cremark, hw, jam, olddatum, q, stage, suppress, timcrst, prelim from crest "
				+ where;
		ArrayList<Object[]> rs = runQuery(query);
		TextReportData data = new TextReportData();

		int i = 0;

		ArrayList<Crest> crestList = new ArrayList<Crest>();
		if ((rs != null) && (rs.size() > 0)) {
			for (Object[] oa : rs) {
				Crest crest = data.new Crest();
				crest.setDatcrst((Date) oa[i]);
				crest.setCremark((String) oa[++i]);
				crest.setHw((String) oa[++i]);
				crest.setJam((String) oa[++i]);
				crest.setOldDatum((String) oa[++i]);
				if (oa[++i] != null) {
					crest.setQ((Integer) oa[i]);
				}
				if (oa[++i] != null) {
					crest.setStage((Double) oa[i]);
				}

				crest.setSuppress((String) oa[++i]);
				crest.setTimcrst((String) oa[++i]);
				crest.setPrelim((String) oa[++i]);

				crestList.add(crest);
				i = 0;
			}
		}

		return crestList;
	}
	
	 public TextReportData getCrestData(String lid) {
	        
         TextReportData data = getCresList(lid);
         return data;
	 }

	/**
	 * Get the list of Flood data.
	 * 
	 * @param where
	 *            The where clause for the query
	 * @return ArrayList of Flood objects
	 */
	private ArrayList<Flood> getFloodList(String where) {
		String query = "select stage, damage, dispstmt from flood " + where;
		ArrayList<Object[]> rs = runQuery(query);
		TextReportData data = new TextReportData();
		int i = 0;

		ArrayList<Flood> floodList = new ArrayList<Flood>();
		if ((rs != null) && (rs.size() > 0)) {
			for (Object[] oa : rs) {
				Flood flood = data.new Flood();
				if (oa[i] != null) {
					flood.setStage((Double) oa[i]);
				}
				flood.setDamage((String) oa[++i]);
				flood.setDispStmt((String) oa[++i]);
				floodList.add(flood);
				i = 0;
			}
		}

		return floodList;
	}

	/**
	 * Get the stnclass data.
	 * 
	 * @param lid
	 *            The location id
	 * @return The Stnclass data object
	 */
	public TextReportData.Stnclass getStnClass(String lid) {
		if (!stnclassMap.containsKey(lid)) {
			String query = "select disp_class, dcp, observer, telem_type from Stnclass where lid = '"
					+ lid + "'";
			TextReportData data = new TextReportData();
			TextReportData.Stnclass stnclass = data.new Stnclass();
			ArrayList<Object[]> rs = runQuery(query);

			if ((rs != null) && (rs.size() > 0)) {
				Object[] oa = rs.get(0);
				stnclass.setDisplayClass((String) oa[0]);
				stnclass.setDcp((String) oa[1]);
				stnclass.setObserver((String) oa[2]);
				stnclass.setTelem_type((String) oa[3]);
			}
			stnclassMap.put(lid, stnclass);
		}

		return stnclassMap.get(lid);
	}

	/**
	 * Get the list of all location ids ordered by lid
	 * 
	 * @return ArrayList of location ids
	 */
	public ArrayList<String> getLidList() {
		ArrayList<Object[]> rs = runQuery(LID_QUERY);

		ArrayList<String> lids = new ArrayList<String>();

		if ((rs != null) && (rs.size() > 0)) {
			for (Object[] oa : rs) {
				lids.add((String) oa[0]);
			}
		}

		return lids;
	}

	/**
	 * Get the data for the Service Backup Report.
	 * 
	 * @param sort
	 *            Which type of sorting to perform 0 = by lid; 1 = by wfo; 2 =
	 *            by hsa
	 * @return ServiceBackupData array
	 */
	public ServiceBackupData[] getSvcBkupData(int sort) {
		String orderby = null;
		if (sort == TextReportConstants.SERVBKUP_SORTBY_LID) {
			orderby = " order by lid ";
		} else if (sort == TextReportConstants.SERVBKUP_SORTBY_WFO) {
			orderby = " order by wfo, lid ";
		} else if (sort == TextReportConstants.SERVBKUP_SORTBY_HSA) {
			orderby = " order by hsa, lid ";
		}

		String query = "select lid, state, county, wfo, hsa from locview ";

		ArrayList<Object[]> rs = runQuery(query + orderby);
		ArrayList<ServiceBackupData> list = new ArrayList<ServiceBackupData>();

		if ((rs != null) && (rs.size() > 0)) {
			for (Object[] oa : rs) {
				ServiceBackupData data = new ServiceBackupData();
				data.setLid((String) oa[0]);
				data.setState((String) oa[1]);
				data.setCounty((String) oa[2]);
				data.setWfo((String) oa[3]);
				data.setHsa((String) oa[4]);
				list.add(data);
			}
		}
		return list.toArray(new ServiceBackupData[list.size()]);
	}

	/**
	 * Get the data for the station list report.
	 * 
	 * @param sort
	 *            Which type of sorting to perform 0 = lid; 1 = name; 2 =
	 *            county; 3 = basin; 4 = observer
	 * @return StationList data array
	 */
	public StationList[] getStnListData(int sort) {
		String orderby = null;

		switch (sort) {
		case TextReportConstants.STALIST_SORTBY_LID:
			orderby = " ORDER BY lid, name, lastname, rb, county, wfo ";
			break;
		case TextReportConstants.STALIST_SORTBY_NAME:
			orderby = " ORDER BY name, lid, lastname, rb, county, wfo ";
			break;
		case TextReportConstants.STALIST_SORTBY_COUNTY:
			orderby = " ORDER BY county, lid, name, lastname, rb, wfo ";
			break;
		case TextReportConstants.STALIST_SORTBY_BASIN:
			orderby = " ORDER BY rb, lid, name, lastname, county, wfo ";
			break;
		case TextReportConstants.STALIST_SORTBY_OBSERVER:
			orderby = " ORDER BY lastname, lid, name, rb, county, wfo ";
			break;
		}

		String query = "select lid, name, county, rb, wfo, lastname, hphone, ophone from stationlist";
		ArrayList<Object[]> rs = runQuery(query + orderby);
		ArrayList<StationList> dataList = new ArrayList<StationList>();
		int i = 0;
		if ((rs != null) && (rs.size() > 0)) {
			for (Object[] oa : rs) {
				StationList stn = new StationList();
				stn.setLid((String) oa[i]);
				stn.setName((String) oa[++i]);
				stn.setCounty((String) oa[++i]);
				stn.setRb((String) oa[++i]);
				stn.setWfo((String) oa[++i]);
				stn.setLastname((String) oa[++i]);
				stn.setHPhone((String) oa[++i]);
				stn.setOPhone((String) oa[++i]);
				dataList.add(stn);
				i = 0;
			}
		}

		return dataList.toArray(new StationList[dataList.size()]);
	}

	/**
	 * Get the hydrologist.
	 * 
	 * @return The hydrologist
	 */
	public String getHydrologist() {
		String hydrologist = null;

		final String query = "select focalpoint from admin";

		List<Object[]> rs = runQuery(query);

		if ((rs != null) && (rs.size() > 0)) {
			hydrologist = (String) rs.get(0)[0];
		}

		return hydrologist;
	}

	/**
	 * Get the highest crest.
	 * 
	 * @param query
	 *            The query to run
	 * @return The data
	 */
	public ArrayList<Object[]> getCrest(String query) {
		return runQuery(query);
	}
	
       
    /**
	 * Get the data for the report
	 * 
	 * @param lid
	 *           The site to get data for
	 * @param page
     *            which type of query to run          
	 * @return The data
	 */
    public TextReportData getDataForReports (String lid, int page) {
        
        TextReportData data = getDataList (lid, page);
        return data;
    }
	
	/**
     * Get the data list for the the report.
     * 
     * @param lid
     * 			 The site to get data for
     * @param page
     *           which table to query from  0 = descrip and RiverStat
     *           							1 = benchmark
     *           							2 = Dcp and Telem
     *           							3 = pub and Datum
     *           							4 = lowwater
     *           	    					5 = crest
     *           							6 = contacts
     *            							7 = Observer          
     */   
    public TextReportData getDataList (String lid, int page) {
    	TextReportData data = null;
        switch (page) {
        
        case 0:       
        	data = getRiverProxList(lid);            
            break;
        case 1: 
        	data = getBenchmarkList(lid);
            break;
        case 2:
        	data = getDcpTelemList(lid);
            break;
        case 3:
        	data = getPubDatumList(lid);
            break;        
        case 4:
            data = getLowWaterList(lid);
            break;
        case 5:  
        	data = getCresList(lid);
            break;        
        case 6:
        	data = getContactList(lid);        	
            break;   
        case 7:
        	data = getObsList(lid);        	
            break;
        }        
        return data;
       
    }

}
