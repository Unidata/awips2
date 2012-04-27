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
package com.raytheon.uf.edex.plugin.qc.common;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.qc.internal.QCPaths;
import com.raytheon.uf.common.message.response.ResponseMessageCatalog;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.plugin.qc.dao.QCDao;
import com.raytheon.uf.edex.pointdata.PointDataQuery;

/**
 * A query task for accessing QC point data stored in netCDF files
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 12/04/09     3408       bphillip    Initial creation
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
public class QCDataQuery extends PointDataQuery {

	/** List of query parameters for "querying" the netCDF file */
	private List<String> queryParameters;

	/**
	 * Constructs a new QCDataQuery
	 * 
	 * @throws DataAccessLayerException
	 *             If errors occur while retrieving the data access object
	 * @throws PluginException
	 *             If the data access object is of the wrong type
	 */
	public QCDataQuery() throws DataAccessLayerException, PluginException {
		super("qc");
		queryParameters = new ArrayList<String>();
	}

	public void addParameter(String name, String value, String operand) {
		queryParameters.add(name + "  " + value + "  " + operand);
	}

	/**
	 * Executes the query and returns the data in a PointDataContainer
	 * 
	 * @return The data in a PointDataContainer
	 * @throws Exception
	 *             if errors occur while retrieving the data
	 */
	public PointDataContainer execute() throws Exception {
		List<PointDataContainer> containers = new ArrayList<PointDataContainer>();

		// Gets the available files for querying
		Map<String, File> pathMap = QCPaths.getPaths();
		for (File dir : pathMap.values()) {
			ArrayList<File> files = FileUtil.listFiles(dir, null, false);

			if (!files.isEmpty()) {
				String[] fileParams = ((QCDao) dao).getParameters(files.get(0));
				Set<String> attribSet = new HashSet<String>(
						Arrays.asList(attribs));
				attribSet.retainAll(Arrays.asList(fileParams));
				List<String> attributes = new ArrayList<String>();
				attributes.addAll(attribSet);
				for (File file : files) {
					PointDataContainer pdc = ((QCDao) dao).getPointData(file,
							attributes, queryParameters);
					if (pdc == null) {
						return null;
					}
					if (pdc.getAllocatedSz() != 0) {
						containers.add(pdc);
					}
				}
			}
		}

		if (containers.size() == 0)
			return null;

		PointDataContainer c0 = containers.get(0);

		for (int i = 1; i < containers.size(); i++) {
			c0.combine(containers.get(i));
		}
		return c0;
	}

	@Override
	public ResponseMessageCatalog getAvailableParameters() throws Exception {
		String[] p = new String[0];
		Map<String, List<Integer[]>> fnameMap = getRetrievalMap(1);

		if (fnameMap.size() > 0) {

			Iterator<String> str = fnameMap.keySet().iterator();
			p = ((QCDao) dao).getParameters(new File(str.next()));
		}
		ResponseMessageCatalog cat = new ResponseMessageCatalog();
		cat.setValues(p);

		return cat;
	}

	@SuppressWarnings("unchecked")
	private Map<String, List<Integer[]>> getRetrievalMap(int limit)
			throws Exception {

		String[] fnameKeys = dao.getKeysRequiredForFileName();
		tq.addReturnedField("pointDataView.curIdx", null);
		tq.addReturnedField("id", null);
		tq.setCount(limit);
		for (String fnameKey : fnameKeys) {
			tq.addReturnedField(fnameKey, null);
		}
		List<?> results = tq.execute();

		Map<String, List<Integer[]>> fnameMap = new HashMap<String, List<Integer[]>>();

		Map<String, Object> workingMap = new HashMap<String, Object>();
		for (Object o : results) {
			Object[] oArr = (Object[]) o;
			workingMap.clear();
			for (int i = 0; i < fnameKeys.length; i++) {
				workingMap.put(fnameKeys[i], oArr[i + 2]);
			}

			String fileName = dao.getPointDataFileName(workingMap);
			List<Integer[]> ints = fnameMap.get(fileName);
			if (ints == null) {
				ints = new ArrayList<Integer[]>(500);
				fnameMap.put(fileName, ints);
			}
			ints.add(new Integer[] { (Integer) oArr[0], (Integer) oArr[1] });
		}

		return fnameMap;
	}
}
