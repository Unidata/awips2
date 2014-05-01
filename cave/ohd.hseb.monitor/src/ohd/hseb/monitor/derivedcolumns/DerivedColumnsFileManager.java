package ohd.hseb.monitor.derivedcolumns;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import ohd.hseb.util.SessionLogger;

public class DerivedColumnsFileManager 
{
	private String _derivedColumnsFile = null;
	private List _derivedColumnsList = null;
	private Map _derivedColumnMap = null;
	private SessionLogger _logger = null;
	
	public DerivedColumnsFileManager(String derivedColumnsFile, SessionLogger logger)
	{
		_derivedColumnsFile = derivedColumnsFile;
		_logger = logger;
	}
	
	
	private void printFileException(IOException exception)
	{
		_logger.log("RiverMonitorDerivedColumnsFileManager.printFileException(): ERROR = " + exception.getMessage());
		exception.printStackTrace();
		_logger.log("RiverMonitorDerivedColumnsFileManager.printFileException(): End of stack trace");
	}
	
    public List getDerivedColumns()
	{
        if(_derivedColumnsList == null)
            _derivedColumnsList = new ArrayList();
        else
            _derivedColumnsList.clear();
        
        if(_derivedColumnMap == null)
            _derivedColumnMap = new HashMap();
        else
            _derivedColumnMap.clear();
        
        Runtime.getRuntime().gc();
		File fileHandler = new File(_derivedColumnsFile);
		
		if(fileHandler.exists())
		{
			BufferedReader in = null;
			String line;
			
			try
			{
				in = new BufferedReader(new FileReader(fileHandler));
				while(( line = in.readLine()) != null)
				{
					String columnName;
					String returnType;
					String equationForValue;
					String equationForColor;
					
					if(line.length() != 0)
					{
						line = line.trim();
						if(line.length() != 0 && line.charAt(0) != '#')
						{
							String splitStr[] = line.split("\\|");
							columnName = splitStr[0];
							returnType = splitStr[1].trim();
							equationForValue = splitStr[2];
							equationForColor = splitStr[3];
							DerivedColumn derivedColumns = new DerivedColumn();
							derivedColumns.setColumnName(columnName);
							derivedColumns.setReturnType(returnType);
							derivedColumns.setEquationForCellValue(equationForValue);
							derivedColumns.setEquationForCellColor(equationForColor);
							_derivedColumnsList.add(derivedColumns);
							_derivedColumnMap.put(columnName, derivedColumns);
						}
					}
				}
			}
			catch(IOException exception)
			{
				printFileException(exception);
			}
		}
		else
		{
		    _logger.log("Derived columns file doesnt exist :[" + fileHandler.getAbsolutePath() + "]");
		}
		return _derivedColumnsList;
	}
	
	protected Map getDerivedColumnMap()
	{
		return _derivedColumnMap;
	}
	
	public String[] getDerivedColumnNames()
	{
		String derivedColumnNames[] = null;
		getDerivedColumns();
		if(_derivedColumnsList.size() != 0)
		{
			derivedColumnNames = new String[_derivedColumnsList.size()];
			for(int i=0; i < _derivedColumnsList.size(); i++)
			{
				DerivedColumn derivedColumns = (DerivedColumn) _derivedColumnsList.get(i);
				derivedColumnNames[i] = derivedColumns.getColumnName();
			}
		}
		return derivedColumnNames;
	}
	
	public String[] getAlignmentForDerivedColumns()
	{
		String alignmentForDerivedColumns[] = null;
		if(_derivedColumnsList.size() != 0)
		{
			alignmentForDerivedColumns = new String[_derivedColumnsList.size()];
			for(int i=0; i < _derivedColumnsList.size(); i++)
			{
				DerivedColumn derivedColumns = (DerivedColumn) _derivedColumnsList.get(i);
				if(derivedColumns.getReturnType().equalsIgnoreCase("string"))
				   alignmentForDerivedColumns[i] = "center";
				else
				   alignmentForDerivedColumns[i] = "right";
			}
		}
		return alignmentForDerivedColumns;
	}
	
}
