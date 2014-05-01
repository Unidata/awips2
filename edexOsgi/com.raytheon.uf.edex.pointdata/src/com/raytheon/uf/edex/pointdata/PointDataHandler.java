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
package com.raytheon.uf.edex.pointdata;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataquery.db.QueryParam.QueryOperand;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.pointdata.PointDataRequestMessage;
import com.raytheon.uf.common.pointdata.PointDataRequestMessageConstraint;
import com.raytheon.uf.common.pointdata.PointDataThriftContainer;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.edex.database.plugin.PluginDao;
import com.raytheon.uf.edex.database.plugin.PluginFactory;
import com.raytheon.uf.edex.database.query.DatabaseQuery;
import com.raytheon.uf.edex.pointdata.PointDataPluginDao.LevelRequest;

/**
 * Services PointDataRequestMessages
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 14, 2010            chammack     Initial creation
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class PointDataHandler implements
		IRequestHandler<PointDataRequestMessage> {

	private static Map<RequestConstraint.ConstraintType, QueryOperand> operandMap = new HashMap<ConstraintType, QueryOperand>();
	static {
		operandMap.put(ConstraintType.IN, QueryOperand.IN);
		operandMap.put(ConstraintType.EQUALS, QueryOperand.EQUALS);
		operandMap.put(ConstraintType.LESS_THAN, QueryOperand.LESSTHAN);
		operandMap.put(ConstraintType.LESS_THAN_EQUALS,
				QueryOperand.LESSTHANEQUALS);
		operandMap.put(ConstraintType.GREATER_THAN, QueryOperand.GREATERTHAN);
		operandMap.put(ConstraintType.GREATER_THAN_EQUALS,
				QueryOperand.GREATERTHANEQUALS);

	}

	@Override
	public Object handleRequest(PointDataRequestMessage request)
			throws Exception {

		long t0 = System.currentTimeMillis();
		try {
			if (request == null) {
				throw new IllegalArgumentException("Null request");
			}
			String plugin = request.getPluginName();

			if (plugin == null) {
				throw new IllegalArgumentException("Plugin name null");
			}

			PluginDao pd = PluginFactory.getInstance().getPluginDao(plugin);
			if (!(pd instanceof PointDataPluginDao<?>)) {
				throw new PluginException(plugin
						+ " DAO is not a point data DAO");
			}

			PointDataPluginDao<?> ppd = (PointDataPluginDao<?>) pd;

			PointDataRequestMessageConstraint[] constraints = request
					.getConstraints();

			Class<? extends PluginDataObject> pdo = PluginFactory.getInstance()
					.getPluginRecordClass(plugin);

			DatabaseQuery dq = new DatabaseQuery(pdo);

			for (PointDataRequestMessageConstraint c : constraints) {
				ConstraintType ct = ConstraintType.values()[c
						.getConstraintType()];
				QueryOperand qo = operandMap.get(ct);
				if (qo != null) {
					dq.addQueryParam(c.getParameter(), c.getValue(), qo);
				}
			}

			String[] fnameKeys = ppd.getKeysRequiredForFileName();
			dq.addReturnedField("pointDataView.curIdx", pdo.getName());
			dq.addReturnedField("id", pdo.getName());

			for (String fnameKey : fnameKeys) {
				dq.addReturnedField(fnameKey, pdo.getName());
			}
			List<?> results = null;
			try {
				results = pd.queryByCriteria(dq);
			} catch (Exception e) {
				e.printStackTrace();
				throw e;
			}

			Map<String, List<Integer[]>> fnameMap = new HashMap<String, List<Integer[]>>(
					64);

			Map<String, Object> workingMap = new HashMap<String, Object>();
			for (Object o : results) {
				Object[] oArr = (Object[]) o;
				workingMap.clear();
				for (int i = 0; i < fnameKeys.length; i++) {
					workingMap.put(fnameKeys[i], oArr[i + 2]);
				}

				String fileName = ppd.getPointDataFileName(workingMap);
				List<Integer[]> ints = fnameMap.get(fileName);
				if (ints == null) {
					ints = new ArrayList<Integer[]>(500);
					fnameMap.put(fileName, ints);
				}
				ints.add(new Integer[] { (Integer) oArr[0], (Integer) oArr[1] });
			}

			List<PointDataContainer> containers = new ArrayList<PointDataContainer>();

			if (fnameMap.size() == 0) {
				return new PointDataThriftContainer();
			}

			Set<String> attribSet = new HashSet<String>(Arrays.asList(request
					.getParameters()));

			for (String file : fnameMap.keySet()) {
				List<Integer[]> intList = fnameMap.get(file);
				int[] intArr = new int[intList.size()];
				int[] idArr = new int[intList.size()];
				for (int i = 0; i < intArr.length; i++) {
					intArr[i] = intList.get(i)[0];
					idArr[i] = intList.get(i)[1];
				}

				long tGPD = System.currentTimeMillis();
				PointDataContainer pdc = ppd.getPointData(new File(file),
						intArr, idArr,
						attribSet.toArray(new String[attribSet.size()]),
						LevelRequest.ALL);
				System.out.println("getPointData took: "
						+ (System.currentTimeMillis() - tGPD));

				containers.add(pdc);
			}

			if (containers.size() == 0)
				return new PointDataThriftContainer();

			PointDataContainer c0 = containers.get(0);

			for (int i = 1; i < containers.size(); i++) {
				c0.combine(containers.get(i));
			}

			return PointDataThriftContainer.from(c0);
		} finally {
			System.out.println("Total Query took: "
					+ (System.currentTimeMillis() - t0));
		}
	}
}
