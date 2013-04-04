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

package com.raytheon.uf.edex.plugin.qc.dao;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import jep.JepException;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.qc.QCRecord;
import com.raytheon.uf.common.dataplugin.qc.internal.QCPaths;
import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.common.datastorage.StorageStatus;
import com.raytheon.uf.common.datastorage.records.IntegerDataRecord;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.pointdata.PointDataDescription;
import com.raytheon.uf.common.python.thread.PythonScriptManager;
import com.raytheon.uf.common.python.thread.PythonScriptManager.ScriptRequest;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.query.DatabaseQuery;
import com.raytheon.uf.edex.pointdata.PointDataPluginDao;

/**
 * Data access object for retrieving QC mesonet data.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 12/04/2009   3408       bphillip   Initial creation
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
public class QCDao extends PointDataPluginDao<QCRecord> {

	/** Map of plugin names to point data descriptions */
	private static Map<String, PointDataDescription> pdds = new HashMap<String, PointDataDescription>();

	private static PythonScriptManager psm;

	private transient Log logger = LogFactory.getLog(getClass());

	static {
		pdds = QCPaths.getPointDataDescriptions();

		try {
			psm = new PythonScriptManager(
					QCPaths.getPythonScriptPath("qcNetCDF.py"),
					QCPaths.PYTHON_INCLUDE_PATH, 2);
		} catch (JepException e) {
			throw new RuntimeException(
					"Failed to initialize Qc NetCDF Python scripting.  Python QC NetCDF calls will Fail!");
		}
	}

	/**
	 * Constructs a new QC data access object
	 * 
	 * @param pluginName
	 *            "qc"
	 * @throws PluginException
	 *             If errors occur while constructing the data access object
	 */
	public QCDao(String pluginName) throws PluginException {
		super(pluginName);
	}

	/**
	 * Gets the point data from the specified netCDF file
	 * 
	 * @param file
	 *            The file to "query"
	 * @param attributes
	 *            The attributes to retrieve from the file
	 * @param queryParameters
	 *            The constraints used for retrieving the data
	 * @return PointDataContainer containing the requested attributes
	 */
	public PointDataContainer getPointData(File file, List<String> attributes,
			List<String> queryParameters) {
		PointDataContainer pc = new PointDataContainer();

		try {
			Map<String, Object> args = new HashMap<String, Object>();
			args.put("fileName", file.getAbsoluteFile());
			args.put("attributes", attributes);
			args.put("queryParameters", queryParameters);
			args.put("ptDataDescription", pdds.get(getQcType(file)));

			Object result = psm.callScript(new ScriptRequest("getDataSets",
					args));
			pc = (PointDataContainer) result;
		} catch (Exception e) {
			logger.error(
					"Failed to retrieve data from QC file: " + file + "\n", e);
		} catch (JepException e) {
			logger.error(
					"Failed to retrieve data from QC file: " + file + "\n", e);
		}
		return pc;
	}

	/*
	 * Jep uses the static return type of a method to determine what kind of
	 * Python object is used to represent a Java object. By using the methods of
	 * this class, the script can access the arrays as Java array objects.
	 */
	private class ScriptArgs {
		String fileName;

		String[] attributes;

		int[] indexes;

		PointDataDescription pdd;

		public ScriptArgs(String fileName, String[] attributes, int[] indexes,
				PointDataDescription pdd) {
			this.fileName = fileName;
			this.attributes = attributes;
			this.indexes = indexes;
			this.pdd = pdd;
		}

		@SuppressWarnings("unused")
		public String getFileName() {
			return fileName;
		}

		@SuppressWarnings("unused")
		public String[] getAttributes() {
			return attributes;
		}

		@SuppressWarnings("unused")
		public int[] getIndexes() {
			return indexes;
		}

		@SuppressWarnings("unused")
		public PointDataDescription getPdd() {
			return pdd;
		}
	}

	public PointDataContainer getPointData(File file, int[] indexes, int[] ids,
			String[] attributes, LevelRequest request) throws StorageException,
			FileNotFoundException {
		try {
			Map<String, Object> args = new HashMap<String, Object>();
			args.put("args", new ScriptArgs(file.getAbsolutePath(), attributes,
					indexes, pdds.get(getQcType(file))));
			Object result = psm.callScript(new ScriptRequest(Thread
					.currentThread().hashCode(), "getPointData", args));
			if (result != null
					&& (((PointDataContainer) result).getPointDataTypes() != null)) {
				// Because of the way qcNetCDF.py works, the result is
				// always the same size as indexes and id's.
				IntegerDataRecord idr = new IntegerDataRecord("id", "", ids);
				((PointDataContainer) result).add(idr, null);
			}
			return (PointDataContainer) result;
		} catch (Exception e) {
			throw new StorageException("Failed to retrieve data from QC file: "
					+ file + "\n" + e.getMessage(), null, new Exception(e));
		} catch (JepException e) {
			throw new StorageException("Failed to retrieve data from QC file: "
					+ file + "\n" + e.getMessage(), null, new Exception(e));
		}
	}

	@Override
	public String[] getKeysRequiredForFileName() {
		return new String[] { "ncSet", "qcType" };
	}

	@Override
	public String getPointDataFileName(QCRecord p) {
		return p.getNcSet();
	}

	public String getPointDataFileName(Map<String, Object> obj) {
		Object o = obj.get("ncSet");
		Object type = obj.get("qcType");
		if (o instanceof String && type instanceof String)
			return QCPaths.getBaseDir() + File.separator + (String) type
					+ File.separator + (String) o;
		else
			throw new IllegalArgumentException(
					"properties do not specify file name");
	}

	/**
	 * Gets the list of available parameters from the netCDF file
	 * 
	 * @param file
	 *            The file to examine
	 * @return The array of available parameters
	 */
	public String[] getParameters(File file) throws StorageException,
			FileNotFoundException {
		String[] retVal = null;

		try {
			Map<String, Object> args = new HashMap<String, Object>();
			args.put("fileName", file.getAbsoluteFile());
			Object result = psm.callScript(new ScriptRequest(Thread
					.currentThread().hashCode(), "getVars", args));
			if (result == null) {
				retVal = new String[0];
			} else {
				retVal = (String[]) result;
			}
		} catch (Exception e) {
			throw new StorageException(
					"Failed to retrieve parameters from QC file: " + file
							+ "\n" + e.getMessage(), null, new Exception(e));
		} catch (JepException e) {
			throw new StorageException(
					"Failed to retrieve parameters from QC file: " + file
							+ "\n" + e.getMessage(), null, new Exception(e));
		}
		return retVal;
	}

	@Override
	public QCRecord newObject() {
		return new QCRecord();
	}

	// -----------------------
	// Operations not supported
	// -----------------------

	public StorageStatus persistToHDF5(PluginDataObject... records)
			throws PluginException {
		throw new UnsupportedOperationException(
				"Operation not supported on the QC plugin");
	}

	/** Support function for the QCScanner */
	public int getMaxRecordIndex(String qcType, String setName)
			throws DataAccessLayerException {
		DatabaseQuery q = new DatabaseQuery(daoClass.getName());
		q.setMaxResults(1);
		q.addQueryParam("qcType", qcType);
		q.addQueryParam("ncSet", setName);
		q.addReturnedField("pointDataView.curIdx");
		q.addOrder("pointDataView.curIdx", false);

		List<?> results = queryByCriteria(q);
		if (results.size() > 0) {
			return (Integer) results.get(0);
		} else
			return -1;
	}

	private static String getQcType(File file) {
		String filePath = file.getPath();
		String[] fileElements = filePath.split(File.separator);
		final int TYPE_INDEX = fileElements.length - 2;
		return fileElements[TYPE_INDEX];
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.raytheon.uf.edex.pointdata.PointDataPluginDao#getPointDataDescription
	 * (java.util.Map)
	 */
	@Override
	public PointDataDescription getPointDataDescription(Map<String, Object> obj) {
		return pdds.get(obj.get("qcType").toString());
	}

}
