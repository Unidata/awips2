package gov.noaa.nws.ncep.edex.common.dao;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import org.hibernate.Query;
import org.hibernate.Session;
import org.hibernate.Transaction;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.persist.IPersistable;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.edex.core.hdf5.HDF5PluginFilenameFilter;
import com.raytheon.uf.edex.pointdata.PointDataPluginDao;
public abstract class NcepPointDataPluginDao<T extends PluginDataObject> extends PointDataPluginDao<T>{

	protected List<String> tableClassNameList= new ArrayList<String>();
	
	public List<String> getTableClassName() {
		return tableClassNameList;
	}

	public void setTableClassName(List<String> tableClassNamelst) {
		this.tableClassNameList = tableClassNamelst;
	}


	public NcepPointDataPluginDao(String pluginName) throws PluginException {
		super(pluginName);
		// TODO Auto-generated constructor stub
	}
	/**
	 * Purges all metadata  for the
	 * owning plugin from the database
	 * 
	 * @param date
	 *            The data cutoff date
	 * @return The number of items deleted
	 */
	protected int purgeAllTables() {
		int results = 0;
		logger.info("purging..."+ this.pluginName);
		if (this.daoClass == null) {
			logger.info("No record class specified for " + this.pluginName
					+ " plugin. EDEX will not purge data for "
					+ this.pluginName + " plugin");
		} else {
			Session s = this.getHibernateTemplate().getSessionFactory()
					.openSession();
			
			Transaction tx = s.beginTransaction();
			try {
				
				
				String queryString = "delete " + daoClass.getSimpleName()
						+ " x";
				Query query = s.createQuery(queryString);
				results = query.executeUpdate();
				tx.commit();
				
				s.flush();
			} catch (Throwable t) {
				// TODO
				t.printStackTrace();
				tx.rollback();
			} finally {
				if (s != null) {
					s.close();
				}
			}
		}
		return results;
	}
	@Override
	/**
	 * Purges all data associated with the owning plugin based on criteria
	 * specified by the owning plugin
	 * 
	 * @throws PluginException
	 *             If problems occur while interacting with the data stores
	 */
	public void purgeAllData() throws PluginException {
		purgeAllTables();
        List<File> files = FileUtil.listFiles(new File(PLUGIN_HDF5_DIR),
				new HDF5PluginFilenameFilter(pluginName), true);
		for (File file : files) {
			file.delete();
		}
	}

	@Override
	protected IDataStore populateDataStore(IDataStore dataStore,
			IPersistable obj) throws Exception {
		// TODO Auto-generated method stub
		return null;
	}

}
