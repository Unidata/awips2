package gov.noaa.nws.ncep.edex.plugin.nctext.common.dao;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;

import gov.noaa.nws.ncep.edex.plugin.nctext.common.statictable.NctextInputFileType;


public class NctextInputFileTypeDao extends CoreDao{
	   /**
     * Constructs a new 
     */
    public NctextInputFileTypeDao() {   	
        super(DaoConfig.forClass(NctextInputFileType.class));
    }
	/**
	 * Retrieves a NCTEXT input file Type using file extension as key
	 * 
	 * @param ext
	 *            input file extension
	 * @return A list of NCTEXT file type.  
	 */
	@SuppressWarnings("unchecked")
	public List<NctextInputFileType> queryByExt(String ext) {
    	List<String> fields = new ArrayList<String>();
    	List<Object> values = new ArrayList<Object>();
    	List<NctextInputFileType> lNctextInputFileType = null;
    	fields.add("fileExt");// the field name defined in NctextInputFileType
    	values.add(ext);
 
    	try {
    		lNctextInputFileType = (List<NctextInputFileType>) queryByCriteria(fields, values);
		} catch (DataAccessLayerException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

		return lNctextInputFileType;
    }

	/**
	 * 
	 * @param ext - input file extension
	 * @return nctext file type. Only 1st found one is returned.
	 * 
	 */
    public String getFiletypeType(String ext) {
    	
    	List<NctextInputFileType> fileType = null;
    	fileType = queryByExt(ext); 
    	if (fileType.isEmpty())
    		return("NA");
    	else
    		return (fileType.get(0).getFileType());
    	
    }

}
