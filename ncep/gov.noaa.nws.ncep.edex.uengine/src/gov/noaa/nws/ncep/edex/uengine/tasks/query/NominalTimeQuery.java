/*
 * Query
 *
 * Date created 06 Feb 2009
 *
 * This code has been developed by the SIB for use in the AWIPS2 system.
 *
 */

package gov.noaa.nws.ncep.edex.uengine.tasks.query;

import gov.noaa.nws.ncep.edex.uengine.utility.GempakConvert;

import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.ArrayList;
import java.util.Calendar;

import com.raytheon.edex.uengine.tasks.query.DbQuery;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.plugin.PluginFactory;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;

/**
 * NominalTimeQuery
 * 
 * Queries the database a specified data plugin times, converts the times to
 * nominal times and returns unique values.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date                 Ticket#         Engineer                Description
 * ------------         ----------      -----------             --------------------------
 * 03/18/2009                           mgamazaychikov          Initial Creation
 * </pre>
 * 
 * @author mgamazaychikov
 * @version 1
 */

public class NominalTimeQuery extends DbQuery {

	private String plugin;

	public NominalTimeQuery(String plugin) throws DataAccessLayerException,
			PluginException {
		super(PluginFactory.getInstance().getDatabase(plugin), PluginFactory
				.getInstance().getPluginRecordClass(plugin).getName());
		try {
			dao = new CoreDao(DaoConfig.forClass(className));
		} catch (Exception e) {
			throw new DataAccessLayerException(
					"Unable to instantiate data access object for: "
							+ className + " on database: " + database, e);
		}
		this.plugin = plugin;

	}

	@SuppressWarnings("unchecked")
	@Override
	public List<String> execute() throws Exception {
		if (dao == null) {
			dao = new CoreDao(DaoConfig.forClass(database, className));
		}

		List<String> resultsBack = new ArrayList();

		final long offset = 960000; // 16 minutes in milliseconds
		List<DataTime> timesToConsider, nomTimes;
		timesToConsider = new ArrayList<DataTime>();
		nomTimes = new ArrayList<DataTime>();

		/*
		 * Execute the query
		 */
		List<Object[]> results = (List<Object[]>) dao.queryByCriteria(query);

		/*
		 * Collect the times in the ArrayList timesToConsider
		 */
		for (int i = 0; i < results.size(); i++) {
			Object[] row = null;
			if (query.getReturnedFields().size() == 1) {
				row = new Object[1];
				row[0] = results.get(i);
			} else {
				row = results.get(i);
			}
			timesToConsider.add(new DataTime(row[0].toString()));
		}

		Iterator<DataTime> timesIterator = timesToConsider.iterator();

		/*
		 * Valid times between 45 minutes before the hour and 10 minutes after
		 * the hour add to ArraList of nominal times nomTimes.
		 */
		while (timesIterator.hasNext()) {
			DataTime dt = timesIterator.next();
			Date aDate = new Date(dt.getValidTime().getTimeInMillis());

			/*
			 * Create the calendar object.
			 */
			Calendar cdattim = Calendar.getInstance();
			cdattim.setTime(aDate);

			DataTime aNomDT = new DataTime();

			/*
			 * Modify fields of calendar object.
			 */
			if (cdattim.get(Calendar.MINUTE) > 45) {

				/*
				 * Add 16 minutes in milliseconds to time to make sure that all
				 * the calendar fields advance properly.
				 */
				long newTimeInMillis = dt.getValidTime().getTimeInMillis()
						+ offset;
				aDate = new Date(newTimeInMillis);

				/*
				 * Set the nominal time minute, second and millisecond fields to
				 * zero.
				 */
				cdattim.setTime(aDate);
				cdattim.set(Calendar.MINUTE, 0);
				cdattim.set(Calendar.SECOND, 0);
				cdattim.set(Calendar.MILLISECOND, 0);

				/*
				 * Construct nominal time object from the modified calendar
				 * object.
				 */
				aNomDT = new DataTime(new Date(cdattim.getTimeInMillis()));

				/*
				 * Add new nominal time to the ArrayList.
				 */
				if (!nomTimes.contains(aNomDT)) {
					nomTimes.add(aNomDT);
				}
			} else if (cdattim.get(Calendar.MINUTE) <= 10) {

				/*
				 * Set the nominal time minute, second and millisecond fields to
				 * zero.
				 */
				cdattim.set(Calendar.MINUTE, 0);
				cdattim.set(Calendar.SECOND, 0);
				cdattim.set(Calendar.MILLISECOND, 0);

				/*
				 * Construct nominal time object from the modified calendar
				 * object.
				 */
				aNomDT = new DataTime(new Date(cdattim.getTimeInMillis()));

				/*
				 * Add new nominal time to the ArrayList.
				 */
				if (!nomTimes.contains(aNomDT)) {
					nomTimes.add(aNomDT);
				}
			}

		}

		String strres = null;
		StringBuffer resultsBuf = new StringBuffer();
		GempakConvert converter = new GempakConvert();
		if (nomTimes == null || nomTimes.size() == 0) {
			/*
			 * Exit early if no results are found
			 */
			resultsBack.add("NominalTimeQuery returned no results");
			return resultsBack;
		} else if (nomTimes != null) {
			/*
			 * Convert the results to string buffer, then to string to prepare
			 * for the output
			 */
			for (int i = 0; i < nomTimes.size(); i++) {
				if (nomTimes.get(i) != null) {
					resultsBuf.append(converter.dbtimeToDattim(nomTimes.get(i)
							.toString()));
					resultsBuf.append("|");
				}
			}
		}
		strres = resultsBuf.toString();
		resultsBack.add(strres);
		return resultsBack;
	}

	public void setDistinctField(String distinctField) {
		query.addDistinctParameter(distinctField);
	}

	public String getPlugin() {
		return plugin;
	}

	public void setPlugin(String plugin) {
		this.plugin = plugin;
	}

}
