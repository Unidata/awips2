package ohd.hseb.fcstservice;   

import ohd.hseb.util.gui.jtable.*;
import ohd.hseb.ihfsdb.generated.*;

public class ServiceTableViewViewJTableRowData extends AbstractJTableRowData
{
	private String lid;
	private String state;
	private String county;
	private String hsa;
	private String locName;
	private String stream;
	
	public ServiceTableViewViewJTableRowData()
	{
	}

	public void setServiceTableViewRecord(ServiceTableViewRecord serviceTableViewRecord)
	{
		setLid(serviceTableViewRecord.getLid());
		setState(serviceTableViewRecord.getState());
		setCounty(serviceTableViewRecord.getCounty());
		setHsa(serviceTableViewRecord.getHsa());
		setLocName(serviceTableViewRecord.getName());
		setStream(serviceTableViewRecord.getStream());
	}
	
	public ServiceTableViewViewJTableRowData(String missingRepresentation)
	{
		setMissingRepresentation(missingRepresentation);
	}
	
	public String toString()
	{
		String str = getLid()+" "+
			         getState()+" "+
	                 getCounty()+" "+
	                 getHsa();
		return str;
	}
	

	public int compare(String columnName, JTableRowData rowData) 
	{
		int ret = 0;
		ServiceTableViewViewJTableRowData serviceTableViewRecord = (ServiceTableViewViewJTableRowData) rowData;
		if(columnName.equals("Location ID"))
			ret = compareStrings(getLid(), serviceTableViewRecord.getLid());
		else if(columnName.equals("State"))
			ret = compareStrings(getState(), serviceTableViewRecord.getState());
		else if (columnName.equals("County"))
			ret = compareStrings(getCounty(), serviceTableViewRecord.getCounty());
		else if (columnName.equals("Hsa"))
			ret = compareStrings(getHsa(), serviceTableViewRecord.getHsa());
		else if (columnName.equals("Name"))
			ret = compareStrings(getLocName(), serviceTableViewRecord.getLocName());
		else if (columnName.equals("Stream"))
			ret = compareStrings(getStream(), serviceTableViewRecord.getStream());
		return ret;
	}
	public String getDataValue(String columnName) 
	{
		String dataValue = null;
		if(columnName.equals("Location ID"))
			dataValue = getStringValue(getLid());
		else if (columnName.equals("State"))
			dataValue = getStringValue(getState());
		else if (columnName.equals("County"))
			dataValue = getStringValue(getCounty());
		else if (columnName.equals("Hsa"))
			dataValue = getStringValue(getHsa());
		else if (columnName.equals("Name"))
			dataValue = getStringValue(getLocName());
		else if (columnName.equals("Stream"))
			dataValue = getStringValue(getStream());
		return dataValue;
	}

	public String getCounty() {
		return county;
	}

	public void setCounty(String county) {
		this.county = county;
	}

	public String getHsa() {
		return hsa;
	}

	public void setHsa(String hsa) {
		this.hsa = hsa;
	}

	public String getLid() {
		return lid;
	}

	public void setLid(String lid) {
		this.lid = lid;
	}

	public String getLocName() {
		return locName;
	}

	public void setLocName(String locName) {
		this.locName = locName;
	}

	public String getState() {
		return state;
	}

	public void setState(String state) {
		this.state = state;
	}

	public String getStream() {
		return stream;
	}

	public void setStream(String stream) {
		this.stream = stream;
	}
}
